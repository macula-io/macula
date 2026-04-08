use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, RwLock};

use rustler::{Encoder, Env, LocalPid, NifResult, ResourceArc, Term};
use tokio::sync::Notify;
use tokio::task::JoinHandle;

use crate::{atoms, connection::ConnectionResource, message, runtime};

/// Opaque stream handle exposed to Erlang via ResourceArc.
pub struct StreamResource {
    send: Mutex<Option<quinn::SendStream>>,
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
        let resource = Self {
            send: Mutex::new(Some(send)),
            recv_task: Mutex::new(None),
            conn,
            owner: RwLock::new(owner),
            active: AtomicBool::new(false),
            active_notify: Notify::new(),
            closed: AtomicBool::new(false),
        };
        // Note: recv task is NOT started here — it starts when active mode is enabled
        // The recv stream is moved into the task in start_recv_task
        // For now, we need to store it somewhere. We'll use a separate approach.
        // Actually, let's start the recv task immediately but have it wait on active_notify.
        resource
    }

    /// Start the background read loop. Must be called after construction
    /// with the recv stream.
    pub fn start_recv_loop(self_arc: ResourceArc<Self>, mut recv: quinn::RecvStream) {
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
                    Err(_e) => {
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
#[rustler::nif(schedule = "DirtyCpu")]
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
