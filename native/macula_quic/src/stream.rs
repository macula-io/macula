use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, RwLock};

use rustler::{Encoder, Env, LocalPid, NifResult, ResourceArc, Term};
use tokio::sync::Notify;
use tokio::task::JoinHandle;

use crate::{atoms, connection::ConnectionResource, message, runtime};

/// Opaque stream handle exposed to Erlang via ResourceArc.
pub struct StreamResource {
    send: Mutex<Option<quinn::SendStream>>,
    recv: Mutex<Option<quinn::RecvStream>>,
    recv_task: Mutex<Option<JoinHandle<()>>>,
    pub conn: ResourceArc<ConnectionResource>,
    pub owner: RwLock<LocalPid>,
    pub active: AtomicBool,
    active_notify: Notify,
    pub closed: AtomicBool,
}

impl StreamResource {
    pub fn new(
        send: quinn::SendStream,
        recv: quinn::RecvStream,
        conn: ResourceArc<ConnectionResource>,
        owner: LocalPid,
    ) -> Self {
        Self {
            send: Mutex::new(Some(send)),
            recv: Mutex::new(Some(recv)),
            recv_task: Mutex::new(None),
            conn,
            owner: RwLock::new(owner),
            active: AtomicBool::new(false),
            active_notify: Notify::new(),
            closed: AtomicBool::new(false),
        }
    }

    /// Start the background read loop. Takes the recv stream from self.
    /// Must be called after the ResourceArc is created.
    pub fn start_recv_loop(self_arc: ResourceArc<Self>) {
        let mut recv_opt = self_arc.recv.lock().unwrap();
        let mut recv = match recv_opt.take() {
            Some(r) => r,
            None => return, // Already started or no recv stream
        };
        drop(recv_opt);
        let stream_arc = self_arc.clone();

        let handle = runtime::rt().spawn(async move {
            let mut buf = vec![0u8; 65536];
            loop {
                if stream_arc.closed.load(Ordering::Relaxed) {
                    break;
                }

                // Wait for active mode
                if !stream_arc.active.load(Ordering::Relaxed) {
                    stream_arc.active_notify.notified().await;
                    continue;
                }

                match recv.read(&mut buf).await {
                    Ok(Some(n)) => {
                        let data = buf[..n].to_vec();
                        let owner = *stream_arc.owner.read().unwrap();
                        message::send_data(&owner, data, stream_arc.clone());
                    }
                    Ok(None) => {
                        // Peer finished sending
                        let owner = *stream_arc.owner.read().unwrap();
                        message::send_event(
                            &owner,
                            atoms::peer_send_shutdown(),
                            stream_arc.clone(),
                            atoms::none(),
                        );
                        break;
                    }
                    Err(quinn::ReadError::Reset(code)) => {
                        // Peer called reset() on their send side (see
                        // `nif_reset_stream` below) — a deliberate,
                        // peer-visible abort with an application error
                        // code, distinct from every other read error
                        // (connection loss, zero-RTT rejection, ...),
                        // which stay collapsed into `none()` below.
                        let owner = *stream_arc.owner.read().unwrap();
                        message::send_event(
                            &owner,
                            atoms::stream_closed(),
                            stream_arc.clone(),
                            (atoms::reset(), code.into_inner()),
                        );
                        break;
                    }
                    Err(e) => {
                        let owner = *stream_arc.owner.read().unwrap();
                        message::send_event(
                            &owner,
                            atoms::stream_closed(),
                            stream_arc.clone(),
                            format!("{}", e),
                        );
                        break;
                    }
                }
            }
        });

        let mut task = self_arc.recv_task.lock().unwrap();
        *task = Some(handle);
    }

    /// Wake the recv loop when active mode is enabled.
    pub fn notify_active(&self) {
        self.active_notify.notify_one();
    }
}

impl Drop for StreamResource {
    fn drop(&mut self) {
        self.closed.store(true, Ordering::SeqCst);
        if let Some(task) = self.recv_task.lock().unwrap().take() {
            task.abort();
        }
    }
}

/// NIF: send(StreamRef, Data) -> ok | {error, Reason}
///
/// `write_all` awaits stream flow-control credit — network-IO bound, so
/// it belongs on a dirty-IO scheduler, not one of the scarce dirty-CPU
/// schedulers (see `nif_open_stream`).
#[rustler::nif(schedule = "DirtyIo")]
fn nif_send<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
    data: rustler::Binary<'a>,
) -> NifResult<Term<'a>> {
    if stream.closed.load(Ordering::Relaxed) {
        return Ok((atoms::error(), atoms::already_closed()).encode(env));
    }

    let bytes = data.as_slice().to_vec();
    let mut guard = stream.send.lock().unwrap();
    let send_stream = match guard.as_mut() {
        Some(s) => s,
        None => return Ok((atoms::error(), atoms::stream_finished()).encode(env)),
    };

    // Clone the send stream reference for the async block
    // Actually, we need to do the write inside block_on with a mutable ref
    let result = runtime::rt().block_on(async {
        send_stream
            .write_all(&bytes)
            .await
            .map_err(|e| format!("{}", e))
    });

    drop(guard); // release lock

    match result {
        Ok(()) => Ok(atoms::ok().encode(env)),
        Err(e) => Ok((atoms::error(), e).encode(env)),
    }
}

/// NIF: async_send(StreamRef, Data) -> ok | {error, Reason}
#[rustler::nif]
fn nif_async_send<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
    data: rustler::Binary<'a>,
) -> NifResult<Term<'a>> {
    if stream.closed.load(Ordering::Relaxed) {
        return Ok((atoms::error(), atoms::already_closed()).encode(env));
    }

    let bytes = data.as_slice().to_vec();

    // For async_send, we block briefly to queue the write (Quinn buffers internally).
    // This avoids the MutexGuard-across-await Send issue.
    let mut guard = stream.send.lock().unwrap();
    if let Some(send_stream) = guard.as_mut() {
        let _ = runtime::rt().block_on(send_stream.write_all(&bytes));
    }
    drop(guard);

    Ok(atoms::ok().encode(env))
}

/// Finish the send side, then await Quinn's own `stopped()` before this
/// value is allowed to drop.
///
/// Quinn's `Drop for SendStream` calls the underlying `finish()` a
/// SECOND time if the value drops while still mid-flight. If a peer
/// STOP_SENDING landed in that gap, Drop's second call sees
/// `FinishError::Stopped` and turns it into a `reset()` instead —
/// which discards locally-buffered, not-yet-transmitted data (see
/// `reset()`'s own doc). A legitimate in-flight reply that raced a
/// peer-side stop could be silently dropped this way. `finish()`'s own
/// doc recommends awaiting `stopped()` to observe this outcome; doing
/// so here means the eventual drop always lands on an already-terminal
/// stream state, never mid-flight. Returns the peer's STOP_SENDING
/// code if it stopped us, `None` if it cleanly acknowledged receipt.
async fn finish_and_await_stopped(mut send: quinn::SendStream) -> Option<quinn::VarInt> {
    let _ = send.finish();
    send.stopped().await.unwrap_or(None)
}

/// NIF: close_stream(StreamRef) -> ok
#[rustler::nif]
fn nif_close_stream<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
) -> NifResult<Term<'a>> {
    stream.closed.store(true, Ordering::SeqCst);
    if let Some(task) = stream.recv_task.lock().unwrap().take() {
        task.abort();
    }
    let mut guard = stream.send.lock().unwrap();
    if let Some(send_stream) = guard.take() {
        runtime::rt().spawn(finish_and_await_stopped(send_stream));
    }
    drop(guard);
    Ok(atoms::ok().encode(env))
}

/// NIF: reset_stream(StreamRef, ErrorCode) -> ok | {error, Reason}
///
/// Abruptly aborts OUR send side with a QUIC RESET_STREAM frame
/// carrying `ErrorCode` — genuinely peer-visible at the transport
/// level: the far end's `RecvStream::read` returns
/// `Err(ReadError::Reset(ErrorCode))` (see the recv loop above)
/// instead of the clean EOF `nif_close_stream`'s graceful `finish()`
/// produces. Local teardown (recv task abort, `closed` flag) mirrors
/// `nif_close_stream` exactly; only the send-side shutdown differs.
#[rustler::nif]
fn nif_reset_stream<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
    error_code: u64,
) -> NifResult<Term<'a>> {
    let code = match quinn::VarInt::from_u64(error_code) {
        Ok(c) => c,
        Err(_) => return Ok((atoms::error(), atoms::error_code_out_of_range()).encode(env)),
    };

    stream.closed.store(true, Ordering::SeqCst);
    if let Some(task) = stream.recv_task.lock().unwrap().take() {
        task.abort();
    }

    let mut guard = stream.send.lock().unwrap();
    let result = match guard.take() {
        Some(mut send_stream) => send_stream.reset(code).map_err(|e| format!("{}", e)),
        None => Ok(()), // already finished/reset — idempotent
    };
    drop(guard);

    match result {
        Ok(()) => Ok(atoms::ok().encode(env)),
        Err(e) => Ok((atoms::error(), e).encode(env)),
    }
}

/// NIF: setopt(StreamRef, active, true|false) -> ok
#[rustler::nif]
fn nif_setopt_active<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
    value: bool,
) -> NifResult<Term<'a>> {
    stream.active.store(value, Ordering::SeqCst);
    if value {
        stream.notify_active();
    }
    Ok(atoms::ok().encode(env))
}

/// NIF: controlling_process(StreamRef, NewPid) -> ok
#[rustler::nif]
fn nif_controlling_process<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
    new_owner: LocalPid,
) -> NifResult<Term<'a>> {
    let mut owner = stream.owner.write().unwrap();
    *owner = new_owner;
    Ok(atoms::ok().encode(env))
}
