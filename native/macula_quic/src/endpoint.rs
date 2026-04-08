use std::net::{IpAddr, SocketAddr};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use rustler::{Encoder, Env, LocalPid, NifResult, ResourceArc, Term};
use tokio::task::JoinHandle;

use crate::{atoms, config, message, runtime};

/// Opaque listener handle exposed to Erlang via ResourceArc.
pub struct ListenerResource {
    pub endpoint: quinn::Endpoint,
    pub local_addr: SocketAddr,
    pub owner: RwLock<LocalPid>,
    accept_task: Mutex<Option<JoinHandle<()>>>,
    pub closed: AtomicBool,
}

impl ListenerResource {
    pub fn new(endpoint: quinn::Endpoint, local_addr: SocketAddr, owner: LocalPid) -> Self {
        Self {
            endpoint,
            local_addr,
            owner: RwLock::new(owner),
            accept_task: Mutex::new(None),
            closed: AtomicBool::new(false),
        }
    }

    pub fn set_accept_task(&self, handle: JoinHandle<()>) {
        let mut task = self.accept_task.lock().unwrap();
        *task = Some(handle);
    }
}

impl Drop for ListenerResource {
    fn drop(&mut self) {
        self.closed.store(true, Ordering::SeqCst);
        if let Some(task) = self.accept_task.lock().unwrap().take() {
            task.abort();
        }
        self.endpoint.close(0u32.into(), b"shutdown");
    }
}

/// NIF: listen(BindAddr, Port, Opts) -> {ok, ListenerRef} | {error, Reason}
#[rustler::nif(schedule = "DirtyCpu")]
fn nif_listen<'a>(
    env: Env<'a>,
    bind_addr: String,
    port: u32,
    certfile: String,
    keyfile: String,
    alpn: Vec<String>,
    idle_timeout_ms: u64,
    keep_alive_ms: u64,
    bidi_streams: u32,
    uni_streams: u32,
) -> NifResult<Term<'a>> {
    let caller = env.pid();

    let addr: IpAddr = bind_addr
        .parse()
        .map_err(|e| rustler::Error::Term(Box::new(format!("invalid bind_addr: {}", e))))?;

    let server_config = config::build_server_config(
        &certfile,
        &keyfile,
        &alpn,
        idle_timeout_ms,
        keep_alive_ms,
        bidi_streams,
        uni_streams,
    )
    .map_err(|e| rustler::Error::Term(Box::new(e)))?;

    let socket = config::create_bound_socket(addr, port as u16)
        .map_err(|e| rustler::Error::Term(Box::new(e)))?;

    // Quinn's TokioRuntime requires a tokio context (Handle::current()).
    // We enter the runtime context here since NIFs run on BEAM scheduler threads.
    let _guard = runtime::rt().enter();

    let endpoint = quinn::Endpoint::new(
        quinn::EndpointConfig::default(),
        Some(server_config),
        socket,
        Arc::new(quinn::TokioRuntime),
    )
    .map_err(|e| rustler::Error::Term(Box::new(format!("endpoint create: {}", e))))?;

    let local_addr = endpoint
        .local_addr()
        .map_err(|e| rustler::Error::Term(Box::new(format!("local_addr: {}", e))))?;

    let resource = ResourceArc::new(ListenerResource::new(endpoint, local_addr, caller));

    Ok((atoms::ok(), resource).encode(env))
}

/// NIF: close_listener(ListenerRef) -> ok
#[rustler::nif]
fn nif_close_listener<'a>(
    env: Env<'a>,
    listener: ResourceArc<ListenerResource>,
) -> NifResult<Term<'a>> {
    listener.closed.store(true, Ordering::SeqCst);
    if let Some(task) = listener.accept_task.lock().unwrap().take() {
        task.abort();
    }
    listener.endpoint.close(0u32.into(), b"shutdown");
    Ok(atoms::ok().encode(env))
}

/// NIF: async_accept(ListenerRef) -> ok
/// Starts the accept loop. Each new connection delivers {quic, new_conn, ConnRef, Info}.
#[rustler::nif]
fn nif_async_accept<'a>(
    env: Env<'a>,
    listener: ResourceArc<ListenerResource>,
) -> NifResult<Term<'a>> {
    let endpoint = listener.endpoint.clone();
    let listener_arc = listener.clone();

    let handle = runtime::rt().spawn(async move {
        loop {
            if listener_arc.closed.load(Ordering::Relaxed) {
                break;
            }

            match endpoint.accept().await {
                Some(incoming) => {
                    let listener_ref = listener_arc.clone();
                    // Spawn per-connection task for handshake
                    tokio::spawn(async move {
                        if listener_ref.closed.load(Ordering::Relaxed) {
                            return;
                        }
                        let remote_addr = incoming.remote_address().to_string();
                        match incoming.await {
                            Ok(connection) => {
                                let conn_resource = ResourceArc::new(
                                    crate::connection::ConnectionResource::new(
                                        connection,
                                        *listener_ref.owner.read().unwrap(),
                                    ),
                                );
                                let owner = *listener_ref.owner.read().unwrap();
                                message::send_new_conn(&owner, conn_resource, remote_addr);
                            }
                            Err(e) => {
                                eprintln!("[macula_quic] accept handshake failed: {}", e);
                            }
                        }
                    });
                }
                None => break, // Endpoint closed
            }
        }
    });

    listener.set_accept_task(handle);
    Ok(atoms::ok().encode(env))
}
