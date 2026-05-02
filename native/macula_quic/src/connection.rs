use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, RwLock};

use rustler::{Binary, Encoder, Env, LocalPid, NifResult, ResourceArc, Term};
use tokio::task::JoinHandle;

use crate::{atoms, config, message, runtime, stream};

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

/// NIF: connect(Host, Port, Opts) -> {ok, ConnRef} | {error, Reason}
///
/// Blocks the dirty scheduler until handshake completes (up to timeout).
///
/// `verify_pubkey` is a 32-byte Ed25519 pubkey to pin against the
/// leaf cert's SubjectPublicKeyInfo. An empty binary disables
/// pinning and falls back to `verify` semantics (system-CA or skip).
///
/// `verify_pubkey` is `Binary<'a>` rather than `Vec<u8>` because
/// rustler's `Vec<u8>` decoder requires a list term and rejects
/// Erlang binaries (which is how every caller passes pubkeys).
/// See cert.rs:nif_generate_self_signed_cert for the same pattern.
#[rustler::nif(schedule = "DirtyCpu")]
fn nif_connect<'a>(
    env: Env<'a>,
    host: String,
    port: u32,
    alpn: Vec<String>,
    verify: bool,
    verify_pubkey: Binary<'a>,
    idle_timeout_ms: u64,
    keep_alive_ms: u64,
    timeout_ms: u64,
) -> NifResult<Term<'a>> {
    let caller = env.pid();

    let pinned = if verify_pubkey.is_empty() {
        None
    } else {
        Some(verify_pubkey.as_slice().to_vec())
    };

    let client_config =
        config::build_client_config(&alpn, verify, pinned, idle_timeout_ms, keep_alive_ms)
            .map_err(|e| rustler::Error::Term(Box::new(e)))?;

    let result = runtime::rt().block_on(async {
        // Strip square brackets if the caller passed `[ipv6]` form
        // (used by the pubkey-pin path where the host string is a
        // synthetic `[ipv6]` derived from the target pubkey). The
        // bare IP works for both DNS resolution and SNI.
        let host_str: &str = host
            .trim_start_matches('[')
            .trim_end_matches(']');

        // Two-arg lookup_host avoids the bracket+colon parsing the
        // single-string form requires for IPv6.
        let addrs: Vec<std::net::SocketAddr> = tokio::net::lookup_host((host_str, port as u16))
            .await
            .map_err(|e| format!("resolve {}:{}: {}", host_str, port, e))?
            .collect();

        let remote_addr = addrs
            .first()
            .ok_or_else(|| format!("no addresses for {}:{}", host_str, port))?;

        // Create client endpoint — match address family to remote
        let local_bind: std::net::SocketAddr = if remote_addr.is_ipv6() {
            "[::]:0".parse().unwrap()
        } else {
            "0.0.0.0:0".parse().unwrap()
        };
        let mut endpoint = quinn::Endpoint::client(local_bind)
            .map_err(|e| format!("client endpoint: {}", e))?;
        endpoint.set_default_client_config(client_config);

        // Connect with timeout. SNI = bare host string (rustls
        // ServerName accepts a literal IP address as a valid name).
        let connecting = endpoint
            .connect(*remote_addr, host_str)
            .map_err(|e| format!("connect: {}", e))?;

        let connection = tokio::time::timeout(
            std::time::Duration::from_millis(timeout_ms),
            connecting,
        )
        .await
        .map_err(|_| "connection_timeout".to_string())?
        .map_err(|e| format!("handshake: {}", e))?;

        Ok::<quinn::Connection, String>(connection)
    });

    match result {
        Ok(connection) => {
            let resource = ResourceArc::new(ConnectionResource::new(connection, caller));
            Ok((atoms::ok(), resource).encode(env))
        }
        Err(e) => Ok((atoms::error(), e).encode(env)),
    }
}

/// NIF: open_stream(ConnRef) -> {ok, StreamRef} | {error, Reason}
#[rustler::nif(schedule = "DirtyCpu")]
fn nif_open_stream<'a>(
    env: Env<'a>,
    conn: ResourceArc<ConnectionResource>,
) -> NifResult<Term<'a>> {
    if conn.closed.load(Ordering::Relaxed) {
        return Ok((atoms::error(), atoms::already_closed()).encode(env));
    }

    let caller = env.pid();
    let connection = conn.connection.clone();

    let result = runtime::rt().block_on(async {
        let (send, recv) = connection
            .open_bi()
            .await
            .map_err(|e| format!("open_bi: {}", e))?;
        Ok::<(quinn::SendStream, quinn::RecvStream), String>((send, recv))
    });

    match result {
        Ok((send, recv)) => {
            let resource = ResourceArc::new(stream::StreamResource::new(
                send, recv, conn.clone(), caller,
            ));
            stream::StreamResource::start_recv_loop(resource.clone());
            Ok((atoms::ok(), resource).encode(env))
        }
        Err(e) => Ok((atoms::error(), e).encode(env)),
    }
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
