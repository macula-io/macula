use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, RwLock};

use rustler::{Encoder, Env, LocalPid, NifResult, ResourceArc, Term};
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
#[rustler::nif(schedule = "DirtyCpu")]
fn nif_connect<'a>(
    env: Env<'a>,
    host: String,
    port: u32,
    alpn: Vec<String>,
    verify: bool,
    idle_timeout_ms: u64,
    keep_alive_ms: u64,
    timeout_ms: u64,
) -> NifResult<Term<'a>> {
    let caller = env.pid();

    let client_config = config::build_client_config(&alpn, verify, idle_timeout_ms, keep_alive_ms)
        .map_err(|e| rustler::Error::Term(Box::new(e)))?;

    let result = runtime::rt().block_on(async {
        // Resolve hostname
        let addr_str = format!("{}:{}", host, port);
        let addrs: Vec<std::net::SocketAddr> = tokio::net::lookup_host(&addr_str)
            .await
            .map_err(|e| format!("resolve {}: {}", addr_str, e))?
            .collect();

        let remote_addr = addrs
            .first()
            .ok_or_else(|| format!("no addresses for {}", addr_str))?;

        // Create client endpoint — match address family to remote
        let local_bind: std::net::SocketAddr = if remote_addr.is_ipv6() {
            "[::]:0".parse().unwrap()
        } else {
            "0.0.0.0:0".parse().unwrap()
        };
        let mut endpoint = quinn::Endpoint::client(local_bind)
            .map_err(|e| format!("client endpoint: {}", e))?;
        endpoint.set_default_client_config(client_config);

        // Connect with timeout
        let connecting = endpoint
            .connect(*remote_addr, &host)
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
