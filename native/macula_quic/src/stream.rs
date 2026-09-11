use std::future::poll_fn;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Mutex, RwLock};
use std::task::{Context, Poll};
use std::time::Duration;

use quinn::VarInt;
use rustler::env::SavedTerm;
use rustler::{Encoder, Env, LocalPid, NifResult, OwnedEnv, ResourceArc, Term};
use tokio::sync::{mpsc, Notify, Semaphore};
use tokio::task::JoinHandle;
use tokio::time::Instant;

use crate::{atoms, connection::ConnectionResource, message, runtime};

/// Bytes async_send may queue on one stream beyond what the stream has
/// written. A call that would queue more returns `{error, busy}`.
const SEND_BUDGET_BYTES: usize = 1024 * 1024;

/// A process that got `{error, busy}` is told to retry once at least this
/// much of the budget is free again.
const SEND_READY_BYTES: usize = SEND_BUDGET_BYTES / 2;

/// Application error code of the reset that ends a closed stream whose queued
/// data could not be written within its linger bound.
const LINGER_RESET_CODE: u32 = 1;

/// Why a stream's writes stopped.
#[derive(Clone)]
enum Failure {
    /// reset_stream/2 dropped the queued data.
    Reset,
    /// The stream ended without writing the data: it was closed and its
    /// linger bound passed, or its writer is gone.
    Closed,
    /// A write failed, or the connection closed.
    Write(String),
}

impl Encoder for Failure {
    fn encode<'a>(&self, env: Env<'a>) -> Term<'a> {
        match self {
            Failure::Reset => atoms::reset().encode(env),
            Failure::Closed => atoms::closed().encode(env),
            Failure::Write(reason) => reason.encode(env),
        }
    }
}

/// Where a send/2 caller waits for `{quic, sent, Ref, Result}`. Dropped
/// without an answer, it answers `{error, closed}`, so a caller never waits
/// for a write that will not happen.
struct Reply {
    env: OwnedEnv,
    reference: Option<SavedTerm>,
    pid: LocalPid,
}

impl Reply {
    fn new(pid: LocalPid, reference: Term) -> Self {
        let env = OwnedEnv::new();
        let reference = Some(env.save(reference));
        Self { env, reference, pid }
    }

    /// Sends the answer, once. A thread the VM manages cannot send it, so on
    /// such a thread nothing is sent; the NIFs disarm a reply instead.
    fn answer(&mut self, result: Result<(), Failure>) {
        if rustler::thread::is_scheduler_thread() {
            return;
        }
        let Some(reference) = self.reference.take() else {
            return;
        };
        let pid = self.pid;
        let _ = self.env.send_and_clear(&pid, |env| {
            let reference = reference.load(env);
            match result {
                Ok(()) => (atoms::quic(), atoms::sent(), reference, atoms::ok()).encode(env),
                Err(failure) => {
                    (atoms::quic(), atoms::sent(), reference, (atoms::error(), failure)).encode(env)
                }
            }
        });
    }

    /// For a NIF that could not queue the data: its caller gets the error as
    /// the NIF's result instead.
    fn disarm(mut self) {
        self.reference = None;
    }
}

impl Drop for Reply {
    fn drop(&mut self) {
        self.answer(Err(Failure::Closed));
    }
}

/// Data for the writer task. `charged` bytes of the send budget return when
/// its write ends; `reply` is set for send/2.
struct Command {
    bytes: Vec<u8>,
    charged: usize,
    reply: Option<Reply>,
}

/// Opaque stream handle exposed to Erlang via ResourceArc.
pub struct StreamResource {
    /// The send half. The writer task polls it under this lock and never holds
    /// the lock across an await, so close_stream and reset_stream take it at
    /// once to finish or reset the stream.
    send: Mutex<Option<quinn::SendStream>>,
    recv: Mutex<Option<quinn::RecvStream>>,
    recv_task: Mutex<Option<JoinHandle<()>>>,
    /// Data for the writer task. Taken, and so dropped, when the stream is
    /// closed or reset: the task then ends after what is queued.
    commands: Mutex<Option<mpsc::UnboundedSender<Command>>>,
    /// The writer task's end of `commands`, until the task starts.
    command_receiver: Mutex<Option<mpsc::UnboundedReceiver<Command>>>,
    /// Bytes async_send may still queue.
    budget: Semaphore,
    /// Processes that got `{error, busy}`, each told once when to retry.
    waiting: Mutex<Vec<LocalPid>>,
    /// Data queued or being written.
    pending: AtomicUsize,
    /// Why writes stopped, once they have.
    failure: Mutex<Option<Failure>>,
    /// Set by a close while data is still queued: the writer task resets the
    /// stream if that data is not written by then.
    linger_deadline: Mutex<Option<Instant>>,
    /// Wakes the writer task after a close or a reset.
    writer_wake: Notify,
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
        let (commands, command_receiver) = mpsc::unbounded_channel();
        Self {
            send: Mutex::new(Some(send)),
            recv: Mutex::new(Some(recv)),
            recv_task: Mutex::new(None),
            commands: Mutex::new(Some(commands)),
            command_receiver: Mutex::new(Some(command_receiver)),
            budget: Semaphore::new(SEND_BUDGET_BYTES),
            waiting: Mutex::new(Vec::new()),
            pending: AtomicUsize::new(0),
            failure: Mutex::new(None),
            linger_deadline: Mutex::new(None),
            writer_wake: Notify::new(),
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
                        // Held until the message is sent, so a
                        // controlling_process that returns has no delivery
                        // to the former owner in flight.
                        let owner = stream_arc.owner.read().unwrap();
                        message::send_data(&owner, data, stream_arc.clone());
                    }
                    Ok(None) => {
                        // Peer finished sending
                        let owner = stream_arc.owner.read().unwrap();
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
                        let owner = stream_arc.owner.read().unwrap();
                        message::send_event(
                            &owner,
                            atoms::stream_closed(),
                            stream_arc.clone(),
                            (atoms::reset(), code.into_inner()),
                        );
                        break;
                    }
                    Err(e) => {
                        let owner = stream_arc.owner.read().unwrap();
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

    /// Start the writer task. Must be called after the ResourceArc is created.
    pub fn start_writer(self_arc: ResourceArc<Self>) {
        if let Some(commands) = self_arc.command_receiver.lock().unwrap().take() {
            runtime::rt().spawn(write_loop(self_arc.clone(), commands));
        }
    }

    /// Wake the recv loop when active mode is enabled.
    pub fn notify_active(&self) {
        self.active_notify.notify_one();
    }

    /// Why the stream takes no more data, if it does not: `already_closed`
    /// after close_stream or reset_stream, or the reason its writes stopped.
    fn refusal<'a>(&self, env: Env<'a>) -> Option<Term<'a>> {
        if self.closed.load(Ordering::SeqCst) {
            return Some((atoms::error(), atoms::already_closed()).encode(env));
        }
        self.failure()
            .map(|failure| (atoms::error(), failure).encode(env))
    }

    /// Hands data to the writer task. The caller has counted it as pending.
    /// When the stream stopped taking data meanwhile, undoes the count and
    /// the charge, and returns `{error, already_closed}`.
    fn queue<'a>(
        &self,
        env: Env<'a>,
        bytes: Vec<u8>,
        charged: usize,
        reply: Option<Reply>,
    ) -> Term<'a> {
        let command = Command { bytes, charged, reply };
        let refused = match self.commands.lock().unwrap().as_ref() {
            Some(sender) => sender.send(command).err().map(|refused| refused.0),
            None => Some(command),
        };
        match refused {
            None => atoms::ok().encode(env),
            Some(Command { charged, reply, .. }) => {
                if let Some(reply) = reply {
                    reply.disarm();
                }
                self.budget.add_permits(charged);
                self.pending.fetch_sub(1, Ordering::SeqCst);
                (atoms::error(), atoms::already_closed()).encode(env)
            }
        }
    }

    fn failure(&self) -> Option<Failure> {
        self.failure.lock().unwrap().clone()
    }

    /// Records why writes stopped, unless something already did, and returns
    /// the reason that stands.
    fn stop_writes(&self, failure: Failure) -> Failure {
        self.failure.lock().unwrap().get_or_insert(failure).clone()
    }

    /// Drops the command sender, so the writer task ends after what is queued.
    fn stop_taking_data(&self) {
        self.commands.lock().unwrap().take();
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

/// Writes a stream's queued data in order, until the stream is closed and its
/// queue written, or it is reset, lingers out, fails, or loses its
/// connection. Answers every send/2 caller whose data it held, and tells every
/// process waiting to retry when it may.
async fn write_loop(
    stream: ResourceArc<StreamResource>,
    mut commands: mpsc::UnboundedReceiver<Command>,
) {
    let connection = stream.conn.connection.clone();
    let ended = loop {
        if let Some(failure) = stream.failure() {
            break failure;
        }
        let command = tokio::select! {
            biased;
            _ = stream.writer_wake.notified() => continue,
            error = connection.closed() => break Failure::Write(error.to_string()),
            command = commands.recv() => command,
        };
        let Some(command) = command else {
            // Closed, and everything queued before the close is written.
            finish(&stream);
            break Failure::Closed;
        };
        if let Err(failure) = write_command(&stream, command).await {
            break failure;
        }
    };
    let failure = stream.stop_writes(ended);
    commands.close();
    while let Ok(Command { charged, mut reply, .. }) = commands.try_recv() {
        answer(&mut reply, Err(failure.clone()));
        written(&stream, charged);
    }
    tell_ready(&stream, std::mem::take(&mut *stream.waiting.lock().unwrap()));
}

/// Writes one command's data and answers its send/2 caller. Returns the
/// reason all writes stop, when this write ends them.
async fn write_command(stream: &ResourceArc<StreamResource>, command: Command) -> Result<(), Failure> {
    let Command { bytes, charged, mut reply } = command;
    let result = write_all(stream, &bytes).await;
    written(stream, charged);
    match result {
        Ok(()) => {
            answer(&mut reply, Ok(()));
            Ok(())
        }
        Err(failure) => {
            let failure = stream.stop_writes(failure);
            report_failed_write(stream, &failure);
            answer(&mut reply, Err(failure.clone()));
            Err(failure)
        }
    }
}

enum Step {
    Recheck,
    LingerPassed,
    Wrote(Result<usize, Failure>),
}

/// Writes all of `bytes`, waiting for flow-control credit as long as it takes.
/// Ends early when the stream is reset, when its send half is taken, or when
/// a close's linger deadline passes, which resets the stream.
async fn write_all(stream: &ResourceArc<StreamResource>, bytes: &[u8]) -> Result<(), Failure> {
    let mut offset = 0;
    while offset < bytes.len() {
        let deadline = *stream.linger_deadline.lock().unwrap();
        let step = tokio::select! {
            biased;
            _ = stream.writer_wake.notified() => Step::Recheck,
            _ = until(deadline) => Step::LingerPassed,
            wrote = poll_fn(|cx| poll_write(stream, cx, &bytes[offset..])) => Step::Wrote(wrote),
        };
        match step {
            Step::Recheck => {
                if let Some(failure) = stream.failure() {
                    return Err(failure);
                }
            }
            Step::LingerPassed => {
                reset_send_half(stream, LINGER_RESET_CODE);
                return Err(Failure::Closed);
            }
            Step::Wrote(Ok(n)) => offset += n,
            Step::Wrote(Err(failure)) => return Err(failure),
        }
    }
    Ok(())
}

fn poll_write(
    stream: &StreamResource,
    cx: &mut Context<'_>,
    bytes: &[u8],
) -> Poll<Result<usize, Failure>> {
    let mut send = stream.send.lock().unwrap();
    match send.as_mut() {
        Some(send_stream) => Pin::new(send_stream)
            .poll_write(cx, bytes)
            .map_err(|error| Failure::Write(error.to_string())),
        None => Poll::Ready(Err(stream.failure().unwrap_or(Failure::Closed))),
    }
}

async fn until(deadline: Option<Instant>) {
    match deadline {
        Some(deadline) => tokio::time::sleep_until(deadline).await,
        None => std::future::pending::<()>().await,
    }
}

fn finish(stream: &StreamResource) {
    if let Some(mut send_stream) = stream.send.lock().unwrap().take() {
        let _ = send_stream.finish();
    }
}

fn reset_send_half(stream: &StreamResource, code: u32) {
    if let Some(mut send_stream) = stream.send.lock().unwrap().take() {
        let _ = send_stream.reset(VarInt::from_u32(code));
    }
}

/// A data command's write ended: it stops counting as pending, its charge
/// returns to the budget, and waiting processes are told to retry once enough
/// of the budget is free.
fn written(stream: &ResourceArc<StreamResource>, charged: usize) {
    stream.pending.fetch_sub(1, Ordering::SeqCst);
    if charged == 0 {
        return;
    }
    let ready = {
        let mut waiting = stream.waiting.lock().unwrap();
        stream.budget.add_permits(charged);
        if stream.budget.available_permits() >= SEND_READY_BYTES {
            std::mem::take(&mut *waiting)
        } else {
            Vec::new()
        }
    };
    tell_ready(stream, ready);
}

fn tell_ready(stream: &ResourceArc<StreamResource>, pids: Vec<LocalPid>) {
    for pid in pids {
        message::send_event(&pid, atoms::send_ready(), stream.clone(), atoms::undefined());
    }
}

fn answer(reply: &mut Option<Reply>, result: Result<(), Failure>) {
    if let Some(reply) = reply.as_mut() {
        reply.answer(result);
    }
}

/// Tells the stream's owner, once, that a write failed.
fn report_failed_write(stream: &ResourceArc<StreamResource>, failure: &Failure) {
    if let Failure::Write(reason) = failure {
        let owner = stream.owner.read().unwrap();
        message::send_event(&owner, atoms::send_failed(), stream.clone(), reason.clone());
    }
}

/// NIF: send(StreamRef, Data, Ref) -> ok | {error, Reason}
///
/// Queues the data for the stream's writer task and returns at once. When
/// the write ends, the calling process gets `{quic, sent, Ref, Result}`.
/// macula_quic:send/2 waits for that message.
#[rustler::nif]
fn nif_send<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
    data: rustler::Binary<'a>,
    reference: Term<'a>,
) -> NifResult<Term<'a>> {
    stream.pending.fetch_add(1, Ordering::SeqCst);
    if let Some(refusal) = stream.refusal(env) {
        stream.pending.fetch_sub(1, Ordering::SeqCst);
        return Ok(refusal);
    }
    let reply = Reply::new(env.pid(), reference);
    Ok(stream.queue(env, data.as_slice().to_vec(), 0, Some(reply)))
}

/// NIF: async_send(StreamRef, Data) -> ok | {error, busy} | {error, Reason}
///
/// Queues the data and returns at once. When the stream already has
/// `SEND_BUDGET_BYTES` queued, queues nothing and returns `{error, busy}`;
/// the calling process later gets one `{quic, send_ready, StreamRef,
/// undefined}`, when it may retry.
#[rustler::nif]
fn nif_async_send<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
    data: rustler::Binary<'a>,
) -> NifResult<Term<'a>> {
    stream.pending.fetch_add(1, Ordering::SeqCst);
    if let Some(refusal) = stream.refusal(env) {
        stream.pending.fetch_sub(1, Ordering::SeqCst);
        return Ok(refusal);
    }
    let charged = data.len().min(SEND_BUDGET_BYTES);
    let mut waiting = stream.waiting.lock().unwrap();
    match stream.budget.try_acquire_many(charged as u32) {
        Ok(permit) => {
            permit.forget();
            drop(waiting);
            Ok(stream.queue(env, data.as_slice().to_vec(), charged, None))
        }
        Err(_) => {
            let caller = env.pid();
            if !waiting.contains(&caller) {
                waiting.push(caller);
            }
            drop(waiting);
            stream.pending.fetch_sub(1, Ordering::SeqCst);
            Ok((atoms::error(), atoms::busy()).encode(env))
        }
    }
}

/// NIF: close_stream(StreamRef, LingerMs) -> ok
///
/// Returns at once. With nothing queued it finishes the stream now: a QUIC
/// FIN, a clean EOF for the peer. Otherwise the writer task writes what is
/// queued and then finishes, or resets the stream with `LINGER_RESET_CODE`
/// when that takes longer than `LingerMs`.
#[rustler::nif]
fn nif_close_stream<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
    linger_ms: u64,
) -> NifResult<Term<'a>> {
    stream.closed.store(true, Ordering::SeqCst);
    if let Some(task) = stream.recv_task.lock().unwrap().take() {
        task.abort();
    }
    if stream.pending.load(Ordering::SeqCst) == 0 {
        finish(&stream);
    } else {
        *stream.linger_deadline.lock().unwrap() =
            Some(Instant::now() + Duration::from_millis(linger_ms));
    }
    stream.stop_taking_data();
    stream.writer_wake.notify_one();
    Ok(atoms::ok().encode(env))
}

/// NIF: reset_stream(StreamRef, ErrorCode) -> ok | {error, Reason}
///
/// Abruptly aborts OUR send side with a QUIC RESET_STREAM frame
/// carrying `ErrorCode` — genuinely peer-visible at the transport
/// level: the far end's `RecvStream::read` returns
/// `Err(ReadError::Reset(ErrorCode))` (see the recv loop above)
/// instead of the clean EOF `nif_close_stream`'s graceful `finish()`
/// produces. Returns at once: queued data is dropped, and a send/2
/// waiting for its write gets `{error, reset}`.
#[rustler::nif]
fn nif_reset_stream<'a>(
    env: Env<'a>,
    stream: ResourceArc<StreamResource>,
    error_code: u64,
) -> NifResult<Term<'a>> {
    let code = match VarInt::from_u64(error_code) {
        Ok(c) => c,
        Err(_) => return Ok((atoms::error(), atoms::error_code_out_of_range()).encode(env)),
    };

    stream.closed.store(true, Ordering::SeqCst);
    if let Some(task) = stream.recv_task.lock().unwrap().take() {
        task.abort();
    }

    let result = match stream.send.lock().unwrap().take() {
        Some(mut send_stream) => send_stream.reset(code).map_err(|e| format!("{}", e)),
        None => Ok(()), // already finished/reset — idempotent
    };
    stream.stop_writes(Failure::Reset);
    stream.stop_taking_data();
    stream.writer_wake.notify_one();

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
///
/// Takes the owner lock that every delivery to the owner holds while it
/// sends, so it returns only when no delivery to the former owner is in
/// flight.
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
