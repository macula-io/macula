use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use rustler::env::SavedTerm;
use rustler::{Binary, Encoder, Env, LocalPid, NifResult, OwnedEnv, ResourceArc, Term};
use tokio::task::{AbortHandle, JoinHandle};

use crate::{atoms, config, message, runtime, stream};

/// Application error code for a stream whose open was cancelled after the
/// peer allowed it: the stream is reset and stopped with this code.
const OPEN_CANCELLED_CODE: u32 = 0;

/// Opaque connection handle exposed to Erlang via ResourceArc.
pub struct ConnectionResource {
    pub connection: quinn::Connection,
    pub owner: RwLock<LocalPid>,
    stream_accept_task: Mutex<Option<JoinHandle<()>>>,
    pub closed: AtomicBool,
}

impl ConnectionResource {
    pub fn new(connection: quinn::Connection, owner: LocalPid) -> Self {
        Self {
            connection,
            owner: RwLock::new(owner),
            stream_accept_task: Mutex::new(None),
            closed: AtomicBool::new(false),
        }
    }

    pub fn set_stream_accept_task(&self, handle: JoinHandle<()>) {
        let mut task = self.stream_accept_task.lock().unwrap();
        *task = Some(handle);
    }
}

impl Drop for ConnectionResource {
    fn drop(&mut self) {
        self.closed.store(true, Ordering::SeqCst);
        if let Some(task) = self.stream_accept_task.lock().unwrap().take() {
            task.abort();
        }
        self.connection.close(0u32.into(), b"closed");
    }
}

/// Shared between a task that sends one result message and the handle that
/// can cancel it. The task sends its result only while `cancelled` is false,
/// and sets `delivered` once it has. A cancel reads `delivered` under the
/// same lock, so it knows whether a result is already in the owner's
/// mailbox.
#[derive(Default)]
struct DeliveryState {
    cancelled: bool,
    delivered: bool,
}

/// A task that sends one result to its owner, and the means to cancel it.
/// Dropping it cancels the task.
struct PendingResult {
    state: Arc<Mutex<DeliveryState>>,
    abort: AbortHandle,
}

impl PendingResult {
    /// Mark the task cancelled and abort it. Returns whether its result
    /// had already been sent.
    fn cancel(&self) -> bool {
        let mut state = self.state.lock().unwrap();
        state.cancelled = true;
        let delivered = state.delivered;
        drop(state);
        self.abort.abort();
        delivered
    }
}

impl Drop for PendingResult {
    fn drop(&mut self) {
        self.cancel();
    }
}

/// Opaque dial handle exposed to Erlang via ResourceArc. Dropping the last
/// reference, as when the owning process exits, cancels the dial.
pub struct DialResource(PendingResult);

/// Opaque stream open handle exposed to Erlang via ResourceArc. Dropping the
/// last reference, as when the owning process exits, cancels the open.
pub struct StreamOpenResource(PendingResult);

/// NIF: async_connect(Tag, Host, Port, Alpn, Verify, VerifyPubkey,
///                    IdleTimeoutMs, KeepAliveMs, TimeoutMs)
///   -> {ok, DialRef} | {error, Reason}
///
/// Starts a dial on the QUIC runtime and returns at once. The calling
/// process later receives `{quic, connected, Tag, ConnRef}` or
/// `{quic, connect_failed, Tag, Reason}`. One deadline covers resolution,
/// endpoint acquisition and the handshake.
///
/// `verify_pubkey` is a 32-byte Ed25519 pubkey to pin against the
/// leaf cert's SubjectPublicKeyInfo. An empty binary disables
/// pinning and falls back to `verify` semantics (system-CA or skip).
///
/// `verify_pubkey` is `Binary<'a>` rather than `Vec<u8>` because
/// rustler's `Vec<u8>` decoder requires a list term and rejects
/// Erlang binaries (which is how every caller passes pubkeys).
/// See cert.rs:nif_generate_self_signed_cert for the same pattern.
#[rustler::nif]
fn nif_async_connect<'a>(
    env: Env<'a>,
    tag: Term<'a>,
    host: String,
    port: u32,
    alpn: Vec<String>,
    verify: bool,
    verify_pubkey: Binary<'a>,
    idle_timeout_ms: u64,
    keep_alive_ms: u64,
    timeout_ms: u64,
) -> NifResult<Term<'a>> {
    let owner = env.pid();

    let pinned = if verify_pubkey.is_empty() {
        None
    } else {
        Some(verify_pubkey.as_slice().to_vec())
    };

    let client_config =
        config::build_client_config(&alpn, verify, pinned, idle_timeout_ms, keep_alive_ms)
            .map_err(|e| rustler::Error::Term(Box::new(e)))?;

    let reply_env = OwnedEnv::new();
    let saved_tag = reply_env.save(tag);
    let state = Arc::new(Mutex::new(DeliveryState::default()));
    let task_state = state.clone();

    let task = runtime::rt().spawn(async move {
        let result = dial(host, port, client_config, timeout_ms).await;
        deliver_dial_result(reply_env, saved_tag, owner, &task_state, result);
    });

    let resource = ResourceArc::new(DialResource(PendingResult {
        state,
        abort: task.abort_handle(),
    }));
    Ok((atoms::ok(), resource).encode(env))
}

/// NIF: cancel_connect(DialRef) -> delivered | cancelled
///
/// Cancels a dial. `delivered` means its result was sent before the cancel
/// and is in the owner's mailbox; `cancelled` means no result will be sent.
#[rustler::nif]
fn nif_cancel_connect<'a>(
    env: Env<'a>,
    dial: ResourceArc<DialResource>,
) -> NifResult<Term<'a>> {
    let outcome = if dial.0.cancel() {
        atoms::delivered()
    } else {
        atoms::cancelled()
    };
    Ok(outcome.encode(env))
}

async fn dial(
    host: String,
    port: u32,
    client_config: quinn::ClientConfig,
    timeout_ms: u64,
) -> Result<quinn::Connection, String> {
    // One deadline covers the WHOLE operation — DNS resolution,
    // endpoint acquisition and the CONNECT/HELLO handshake — so a
    // stall in any stage (a hung resolver, a black-holed handshake)
    // always ends within `timeout_ms`.
    let fut = async move {
        // Strip square brackets if the caller passed `[ipv6]` form
        // (used by the pubkey-pin path where the host string is a
        // synthetic `[ipv6]` derived from the target pubkey). The
        // bare IP works for both DNS resolution and SNI.
        let host_str: &str = host.trim_start_matches('[').trim_end_matches(']');

        // Two-arg lookup_host avoids the bracket+colon parsing the
        // single-string form requires for IPv6.
        let addrs: Vec<std::net::SocketAddr> =
            tokio::net::lookup_host((host_str, port as u16))
                .await
                .map_err(|e| format!("resolve {}:{}: {}", host_str, port, e))?
                .collect();

        let remote_addr = *addrs
            .first()
            .ok_or_else(|| format!("no addresses for {}:{}", host_str, port))?;

        // Shared client endpoint per address family (see
        // `runtime::client_endpoint`) — reused across all dials so we
        // don't leak a socket + driver task per connection.
        let endpoint = runtime::client_endpoint(remote_addr.is_ipv6())?;

        // Per-dial client config (verify / ALPN / pubkey-pin) via
        // `connect_with`; SNI = bare host string (rustls ServerName
        // accepts a literal IP address as a valid name).
        let connection = endpoint
            .connect_with(client_config, remote_addr, host_str)
            .map_err(|e| format!("connect: {}", e))?
            .await
            .map_err(|e| format!("handshake: {}", e))?;

        Ok::<quinn::Connection, String>(connection)
    };

    match tokio::time::timeout(std::time::Duration::from_millis(timeout_ms), fut).await {
        Ok(inner) => inner,
        Err(_) => Err("connection_timeout".to_string()),
    }
}

/// Send a dial's result to its owner, unless the dial was cancelled. The
/// state lock is held while sending, so a cancel either stops the send or
/// learns that it happened. A connection whose send is skipped is dropped
/// here, which closes it.
fn deliver_dial_result(
    mut reply_env: OwnedEnv,
    tag: SavedTerm,
    owner: LocalPid,
    state: &Mutex<DeliveryState>,
    result: Result<quinn::Connection, String>,
) {
    let mut state = state.lock().unwrap();
    if state.cancelled {
        return;
    }
    let _ = reply_env.send_and_clear(&owner, |env| {
        let tag = tag.load(env);
        match result {
            Ok(connection) => {
                let conn = ResourceArc::new(ConnectionResource::new(connection, owner));
                (atoms::quic(), atoms::connected(), tag, conn).encode(env)
            }
            Err(reason) => (atoms::quic(), atoms::connect_failed(), tag, reason).encode(env),
        }
    });
    state.delivered = true;
}

/// NIF: async_open_stream(ConnRef, Tag) -> {ok, OpenRef} | {error, already_closed}
///
/// Starts opening a bidirectional stream on the QUIC runtime and returns at
/// once. The calling process, which owns the stream once it is open, later
/// receives `{quic, stream_opened, Tag, StreamRef}` or
/// `{quic, stream_open_failed, Tag, Reason}`. The open waits for as long as
/// the peer allows no further stream, and fails when the connection ends.
#[rustler::nif]
fn nif_async_open_stream<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
    tag: Term<'a>,
) -> NifResult<Term<'a>> {
    if conn.closed.load(Ordering::Relaxed) {
        return Ok((atoms::error(), atoms::already_closed()).encode(env));
    }

    let owner = env.pid();
    let reply_env = OwnedEnv::new();
    let saved_tag = reply_env.save(tag);
    let state = Arc::new(Mutex::new(DeliveryState::default()));
    let task_state = state.clone();
    let task_conn = conn.clone();

    let task = runtime::rt().spawn(async move {
        let result = task_conn
            .connection
            .open_bi()
            .await
            .map_err(|e| format!("open_bi: {}", e));
        deliver_open_result(reply_env, saved_tag, owner, task_conn, &task_state, result);
    });

    let resource = ResourceArc::new(StreamOpenResource(PendingResult {
        state,
        abort: task.abort_handle(),
    }));
    Ok((atoms::ok(), resource).encode(env))
}

/// NIF: cancel_open_stream(OpenRef) -> delivered | cancelled
///
/// Cancels a stream open. `delivered` means its result was sent before the
/// cancel and is in the owner's mailbox; `cancelled` means no result will be
/// sent.
#[rustler::nif]
fn nif_cancel_open_stream<'a>(
    env: Env<'a>,
    opening: ResourceArc<StreamOpenResource>,
) -> NifResult<Term<'a>> {
    let outcome = if opening.0.cancel() {
        atoms::delivered()
    } else {
        atoms::cancelled()
    };
    Ok(outcome.encode(env))
}

/// Send an open's result to its owner, unless the open was cancelled. The
/// state lock is held while sending, so a cancel either stops the send or
/// learns that it happened. A stream whose send is skipped is reset and
/// stopped, so the peer does not keep it.
fn deliver_open_result(
    mut reply_env: OwnedEnv,
    tag: SavedTerm,
    owner: LocalPid,
    conn: ResourceArc<ConnectionResource>,
    state: &Mutex<DeliveryState>,
    result: Result<(quinn::SendStream, quinn::RecvStream), String>,
) {
    let mut state = state.lock().unwrap();
    if state.cancelled {
        if let Ok((mut send, mut recv)) = result {
            let _ = send.reset(OPEN_CANCELLED_CODE.into());
            let _ = recv.stop(OPEN_CANCELLED_CODE.into());
        }
        return;
    }
    let _ = reply_env.send_and_clear(&owner, |env| {
        let tag = tag.load(env);
        match result {
            Ok((send, recv)) => {
                let stream =
                    ResourceArc::new(stream::StreamResource::new(send, recv, conn, owner));
                stream::StreamResource::start_recv_loop(stream.clone());
                stream::StreamResource::start_writer(stream.clone());
                (atoms::quic(), atoms::stream_opened(), tag, stream).encode(env)
            }
            Err(reason) => (atoms::quic(), atoms::stream_open_failed(), tag, reason).encode(env),
        }
    });
    state.delivered = true;
}

/// NIF: close_connection(ConnRef) -> ok
#[rustler::nif]
fn nif_close_connection<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    conn.closed.store(true, Ordering::SeqCst);
    if let Some(task) = conn.stream_accept_task.lock().unwrap().take() {
        task.abort();
    }
    conn.connection.close(0u32.into(), b"closed");
    Ok(atoms::ok().encode(env))
}

/// NIF: async_accept_stream(ConnRef) -> ok
/// Starts stream accept loop. Delivers {quic, new_stream, StreamRef, Props}.
#[rustler::nif]
fn nif_async_accept_stream<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    let connection = conn.connection.clone();
    let conn_arc = conn.clone();

    let handle = runtime::rt().spawn(async move {
        loop {
            if conn_arc.closed.load(Ordering::Relaxed) {
                break;
            }

            match connection.accept_bi().await {
                Ok((send, recv)) => {
                    let owner = *conn_arc.owner.read().unwrap();
                    let stream_resource = ResourceArc::new(stream::StreamResource::new(
                        send,
                        recv,
                        conn_arc.clone(),
                        owner,
                    ));
                    stream::StreamResource::start_recv_loop(stream_resource.clone());
                    stream::StreamResource::start_writer(stream_resource.clone());
                    message::send_new_stream(&owner, stream_resource, conn_arc.clone(), 0);
                }
                Err(_) => break, // Connection closed
            }
        }
    });

    conn.set_stream_accept_task(handle);
    Ok(atoms::ok().encode(env))
}

/// NIF: controlling_process_conn(ConnRef, NewPid) -> ok
#[rustler::nif]
fn nif_controlling_process_conn<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
    new_owner: LocalPid,
) -> NifResult<Term<'a>> {
    let mut owner = conn.owner.write().unwrap();
    *owner = new_owner;
    Ok(atoms::ok().encode(env))
}

/// NIF: peername(ConnRef) -> {ok, {IP, Port}} | {error, Reason}
#[rustler::nif]
fn nif_peername<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    let addr = conn.connection.remote_address();
    let ip = addr.ip().to_string();
    let port = addr.port() as u32;
    Ok((atoms::ok(), (ip, port)).encode(env))
}

/// NIF: max_datagram_size(ConnRef) -> {ok, Bytes} | {error, already_closed}
///
/// Returns the current path MTU on this connection as tracked by
/// Quinn's path-state machine. Reflects DPLPMTUD probing (RFC 8899)
/// once the connection has been up long enough; before that, returns
/// Quinn's initial-MTU default (typically 1200 for IPv6).
///
/// Misnamed for historical reasons — semantics is path MTU in bytes,
/// not max QUIC datagram payload size. Phase 4.2.
#[rustler::nif]
fn nif_max_datagram_size<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    if conn.closed.load(Ordering::Relaxed) {
        return Ok((atoms::error(), atoms::already_closed()).encode(env));
    }
    let stats = conn.connection.stats();
    let mtu = stats.path.current_mtu as u64;
    Ok((atoms::ok(), mtu).encode(env))
}
