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
    // TEMP DIAGNOSTIC (macula 10.5.3) — remove once the overlay_relay
    // WAN-only vanishing-frame incident is root-caused. See
    // CHANGELOG.md [10.5.3]. Immutable snapshot of `owner` at
    // construction time, purely so later code can answer "has ownership
    // of this stream ever been reassigned since it was created?" via
    // `LocalPid`'s `PartialEq` (it has no `Debug`, so this is compared,
    // not printed).
    pub birth_owner: LocalPid,
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
            birth_owner: owner,
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
        // TEMP DIAGNOSTIC (macula 10.5.1) — remove once the overlay_relay
        // WAN-only vanishing-frame incident is root-caused. See
        // CHANGELOG.md [10.5.1].
        let diag_id = recv.id();

        let handle = runtime::rt().spawn(async move {
            let mut buf = vec![0u8; 65536];
            eprintln!("[quic-diag] recv_loop stream={:?} started", diag_id);
            loop {
                if stream_arc.closed.load(Ordering::Relaxed) {
                    eprintln!("[quic-diag] recv_loop stream={:?} closed flag set, exiting", diag_id);
                    break;
                }

                // Wait for active mode
                if !stream_arc.active.load(Ordering::Relaxed) {
                    eprintln!("[quic-diag] recv_loop stream={:?} waiting for active", diag_id);
                    stream_arc.active_notify.notified().await;
                    eprintln!("[quic-diag] recv_loop stream={:?} woke from active wait", diag_id);
                    continue;
                }

                eprintln!("[quic-diag] recv_loop stream={:?} calling recv.read()", diag_id);
                match recv.read(&mut buf).await {
                    Ok(Some(n)) => {
                        let owner = *stream_arc.owner.read().unwrap();
                        // TEMP DIAGNOSTIC (macula 10.5.3) — remove once
                        // the overlay_relay WAN-only vanishing-frame
                        // incident is root-caused. See CHANGELOG.md
                        // [10.5.3]. Does THIS read's delivery target
                        // differ from the stream's original owner?
                        let owner_unchanged = owner == stream_arc.birth_owner;
                        eprintln!(
                            "[quic-diag] recv_loop stream={:?} read {} bytes owner_unchanged_since_birth={}",
                            diag_id, n, owner_unchanged
                        );
                        let data = buf[..n].to_vec();
                        message::send_data(&owner, data, stream_arc.clone());
                    }
                    Ok(None) => {
                        eprintln!("[quic-diag] recv_loop stream={:?} peer finished (EOF)", diag_id);
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
                        eprintln!("[quic-diag] recv_loop stream={:?} RESET code={:?}", diag_id, code);
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
                        eprintln!("[quic-diag] recv_loop stream={:?} ERROR {:?}", diag_id, e);
                        let owner = *stream_arc.owner.read().unwrap();
                        message::send_event(
                            &owner,
                            atoms::stream_closed(),
                            stream_arc.clone(),
                            atoms::none(), // simplified for now
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

    // TEMP DIAGNOSTIC (macula 10.5.1) — remove once the overlay_relay
    // WAN-only vanishing-frame incident is root-caused. See
    // CHANGELOG.md [10.5.1].
    let diag_id = send_stream.id();
    eprintln!(
        "[quic-diag] nif_send stream={:?} len={} calling write_all",
        diag_id,
        bytes.len()
    );

    // Clone the send stream reference for the async block
    // Actually, we need to do the write inside block_on with a mutable ref
    let result = runtime::rt().block_on(async {
        send_stream
            .write_all(&bytes)
            .await
            .map_err(|e| format!("{}", e))
    });

    eprintln!(
        "[quic-diag] nif_send stream={:?} len={} write_all result={:?}",
        diag_id,
        bytes.len(),
        result
    );

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
    // Finish the send stream gracefully
    let mut guard = stream.send.lock().unwrap();
    if let Some(mut send_stream) = guard.take() {
        let _ = send_stream.finish();
    }
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
    // TEMP DIAGNOSTIC (macula 10.5.1) — remove once the overlay_relay
    // WAN-only vanishing-frame incident is root-caused. See
    // CHANGELOG.md [10.5.1].
    eprintln!("[quic-diag] nif_setopt_active stream=<ptr {:p}> value={}", &*stream, value);
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
    // TEMP DIAGNOSTIC (macula 10.5.3) — remove once the overlay_relay
    // WAN-only vanishing-frame incident is root-caused. See
    // CHANGELOG.md [10.5.3].
    let actually_changed = *owner != new_owner;
    let was_birth_owner = *owner == stream.birth_owner;
    eprintln!(
        "[quic-diag] nif_controlling_process stream=<ptr {:p}> actually_changed={} was_birth_owner={}",
        &*stream, actually_changed, was_birth_owner
    );
    *owner = new_owner;
    Ok(atoms::ok().encode(env))
}
