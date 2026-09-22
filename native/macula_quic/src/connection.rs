use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use rustler::env::SavedTerm;
use rustler::{Binary, Encoder, Env, LocalPid, NifResult, OwnedBinary, OwnedEnv, ResourceArc, Term};
use rustls::pki_types::CertificateDer;
use tokio::task::{AbortHandle, JoinHandle};

use crate::{atoms, config, error_codes, message, runtime, stream};

/// Opaque connection handle exposed to Erlang via ResourceArc.
pub struct ConnectionResource {
    pub connection: quinn::Connection,
    pub owner: RwLock<LocalPid>,
    stream_accept_task: Mutex<Option<JoinHandle<()>>>,
    pub closed: AtomicBool,
    /// The leaf certificate the other side sent in this connection's TLS
    /// handshake, as received. Only a dialed connection has one.
    peer_leaf: Option<Arc<[u8]>>,
    /// The leaf certificate this side sent in this connection's TLS
    /// handshake. Only an accepted connection has one.
    presented_leaf: Option<Arc<[u8]>>,
}

impl ConnectionResource {
    /// A connection this side dialed, with the leaf the other side sent.
    pub fn dialed(connection: quinn::Connection, owner: LocalPid) -> Self {
        let peer_leaf = received_leaf(&connection);
        Self::new(connection, owner, peer_leaf, None)
    }

    /// A connection a listener accepted, with the leaf of the certificate
    /// generation it was accepted with.
    pub fn accepted(
        connection: quinn::Connection,
        owner: LocalPid,
        presented_leaf: Arc<[u8]>,
    ) -> Self {
        Self::new(connection, owner, None, Some(presented_leaf))
    }

    fn new(
        connection: quinn::Connection,
        owner: LocalPid,
        peer_leaf: Option<Arc<[u8]>>,
        presented_leaf: Option<Arc<[u8]>>,
    ) -> Self {
        Self {
            connection,
            owner: RwLock::new(owner),
            stream_accept_task: Mutex::new(None),
            closed: AtomicBool::new(false),
            peer_leaf,
            presented_leaf,
        }
    }

    pub fn set_stream_accept_task(&self, handle: JoinHandle<()>) {
        let mut task = self.stream_accept_task.lock().unwrap();
        *task = Some(handle);
    }
}

/// The first certificate of the chain the other side sent in this
/// connection's TLS handshake, byte for byte, if it sent one. quinn's rustls
/// session hands the chain over as received, leaf first.
fn received_leaf(connection: &quinn::Connection) -> Option<Arc<[u8]>> {
    let chain = connection
        .peer_identity()?
        .downcast::<Vec<CertificateDer<'static>>>()
        .ok()?;
    chain.first().map(|leaf| Arc::from(&leaf[..]))
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

/// NIF: async_connect(Tag, Host, Port, Alpn, IdleTimeoutMs, KeepAliveMs,
///                    TimeoutMs)
///   -> {ok, DialRef} | {error, Reason}
///
/// Starts a dial on the QUIC runtime and returns at once. The calling
/// process later receives `{quic, connected, Tag, ConnRef}` or
/// `{quic, connect_failed, Tag, Reason}`. One deadline covers resolution,
/// endpoint acquisition and the handshake.
///
/// The station is verified in the one way there is
/// (`config::client_tls_config`): its handshake signature under the key of
/// the ML-DSA-87 certificate it presents. Who it is, the caller checks
/// against that certificate afterwards (`nif_peer_leaf`).
#[rustler::nif]
fn nif_async_connect<'a>(
    env: Env<'a>,
    tag: Term<'a>,
    host: String,
    port: u32,
    alpn: Vec<String>,
    idle_timeout_ms: u64,
    keep_alive_ms: u64,
    timeout_ms: u64,
) -> NifResult<Term<'a>> {
    let owner = env.pid();

    let client_config = config::build_client_config(&alpn, idle_timeout_ms, keep_alive_ms)
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
                let conn = ResourceArc::new(ConnectionResource::dialed(connection, owner));
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
            let _ = send.reset(error_codes::CANCELLED.into());
            let _ = recv.stop(error_codes::CANCELLED.into());
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
/// Closes with application error code 0 and the reason "closed".
#[rustler::nif]
fn nif_close_connection<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    close_with(&conn, 0u32.into(), b"closed");
    Ok(atoms::ok().encode(env))
}

/// NIF: close_connection(ConnRef, Code, Reason) -> ok | {error, error_code_out_of_range}
/// Closes with an application error code and reason, which the peer reads
/// from its own close reason. The code must fit a QUIC variable-length
/// integer.
#[rustler::nif]
fn nif_close_connection_with_code<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
    code: u64,
    reason: Binary<'a>,
) -> NifResult<Term<'a>> {
    let Ok(code) = quinn::VarInt::from_u64(code) else {
        return Ok((atoms::error(), atoms::error_code_out_of_range()).encode(env));
    };
    close_with(&conn, code, reason.as_slice());
    Ok(atoms::ok().encode(env))
}

fn close_with(conn: &ConnectionResource, code: quinn::VarInt, reason: &[u8]) {
    conn.closed.store(true, Ordering::SeqCst);
    if let Some(task) = conn.stream_accept_task.lock().unwrap().take() {
        task.abort();
    }
    conn.connection.close(code, reason);
}

/// NIF: close_reason(ConnRef) -> open | locally_closed | reset | timed_out
///   | version_mismatch | cids_exhausted
///   | {application_closed | transport_closed | transport_error, Code, Reason}
/// Why the connection closed, or `open` while it is open.
#[rustler::nif]
fn nif_close_reason<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    Ok(close_reason_term(env, conn.connection.close_reason()))
}

fn close_reason_term<'a>(env: Env<'a>, reason: Option<quinn::ConnectionError>) -> Term<'a> {
    use quinn::ConnectionError;
    match reason {
        None => atoms::open().encode(env),
        Some(ConnectionError::ApplicationClosed(close)) => (
            atoms::application_closed(),
            close.error_code.into_inner(),
            binary_term(env, &close.reason),
        )
            .encode(env),
        Some(ConnectionError::ConnectionClosed(close)) => (
            atoms::transport_closed(),
            u64::from(close.error_code),
            binary_term(env, &close.reason),
        )
            .encode(env),
        Some(ConnectionError::TransportError(error)) => (
            atoms::transport_error(),
            u64::from(error.code),
            binary_term(env, error.reason.as_bytes()),
        )
            .encode(env),
        Some(ConnectionError::LocallyClosed) => atoms::locally_closed().encode(env),
        Some(ConnectionError::Reset) => atoms::reset().encode(env),
        Some(ConnectionError::TimedOut) => atoms::timed_out().encode(env),
        Some(ConnectionError::VersionMismatch) => atoms::version_mismatch().encode(env),
        Some(ConnectionError::CidsExhausted) => atoms::cids_exhausted().encode(env),
    }
}

/// `bytes` copied into a binary made in `env`.
fn binary_term<'a>(env: Env<'a>, bytes: &[u8]) -> Term<'a> {
    let mut binary = rustler::NewBinary::new(env, bytes.len());
    binary.as_mut_slice().copy_from_slice(bytes);
    binary.into()
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
                    // Held until new_stream is sent, so a
                    // controlling_process_conn that returns has no notice to
                    // the former owner in flight.
                    let owner = conn_arc.owner.read().unwrap();
                    let stream_resource = ResourceArc::new(stream::StreamResource::new(
                        send,
                        recv,
                        conn_arc.clone(),
                        *owner,
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
///
/// Takes the owner lock that the stream accept loop holds while it sends a
/// new_stream notice, so it returns only when no notice to the former owner
/// is in flight.
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

/// NIF: lost_packets(ConnRef) -> {ok, Count} | {error, already_closed}
///
/// Packets this connection's congestion controller has declared lost,
/// from `quinn::Connection::stats().path.lost_packets`.
///
/// CUMULATIVE for the life of the connection and monotonically
/// non-decreasing. It never resets, so a caller wanting a rate reads it
/// twice and subtracts; reading it once tells you nothing about when the
/// losses happened. A connection that is replaced starts a new count,
/// so deltas are only meaningful within one ConnRef.
///
/// This exists to make a question answerable, not to answer it: a call
/// that stalls either coincides with a rise here or it does not, and
/// both outcomes are informative.
///
/// ⚠ NOT a general stats API, and deliberately not routed through
/// `macula_quic:getstat/2`. That function refuses with `not_implemented`
/// on purpose — see its comment — because a counter that always reads
/// zero makes "nothing is moving" indistinguishable from "nobody
/// implemented the counter". Surfacing ONE real field here does not
/// compromise that; filling the rest of `getstat/2`'s shape with zeros
/// would.
#[rustler::nif]
fn nif_lost_packets<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    if conn.closed.load(Ordering::Relaxed) {
        return Ok((atoms::error(), atoms::already_closed()).encode(env));
    }
    let stats = conn.connection.stats();
    Ok((atoms::ok(), stats.path.lost_packets).encode(env))
}

/// NIF: peer_leaf(ConnRef) -> {ok, Der} | {error, no_peer_leaf}
///
/// The leaf certificate the other side sent in this connection's TLS
/// handshake, exactly as received. A dialed connection has the station's;
/// an accepted connection has none, since clients send no certificate.
#[rustler::nif]
fn nif_peer_leaf<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    Ok(match conn.peer_leaf.as_deref() {
        Some(leaf) => (atoms::ok(), der_binary(env, leaf)).encode(env),
        None => (atoms::error(), atoms::no_peer_leaf()).encode(env),
    })
}

/// NIF: presented_leaf(ConnRef) -> {ok, Der} | {error, no_presented_leaf}
///
/// The leaf certificate this side sent in this connection's TLS handshake.
/// An accepted connection has the leaf of the certificate generation it was
/// accepted with, whatever its listener presents now; a dialed connection
/// has none.
#[rustler::nif]
fn nif_presented_leaf<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    Ok(match conn.presented_leaf.as_deref() {
        Some(leaf) => (atoms::ok(), der_binary(env, leaf)).encode(env),
        None => (atoms::error(), atoms::no_presented_leaf()).encode(env),
    })
}

fn der_binary<'a>(env: Env<'a>, der: &[u8]) -> Binary<'a> {
    let mut binary = OwnedBinary::new(der.len()).expect("allocate a certificate binary");
    binary.as_mut_slice().copy_from_slice(der);
    binary.release(env)
}
