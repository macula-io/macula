use std::net::{IpAddr, SocketAddr};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use rustler::{Encoder, Env, LocalPid, NifResult, ResourceArc, Term};
use rustls::pki_types::CertificateDer;
use tokio::task::JoinHandle;

use crate::{atoms, config, connection, message, runtime};

/// One certificate generation of a listener: the server configuration
/// connections are accepted with, and the leaf certificate it presents.
pub struct CertificateGeneration {
    server_config: Arc<quinn::ServerConfig>,
    leaf: Arc<[u8]>,
}

impl CertificateGeneration {
    fn new(server_config: quinn::ServerConfig, leaf: CertificateDer<'static>) -> Self {
        Self {
            server_config: Arc::new(server_config),
            leaf: Arc::from(&leaf[..]),
        }
    }
}

/// Opaque listener handle exposed to Erlang via ResourceArc.
pub struct ListenerResource {
    pub endpoint: quinn::Endpoint,
    pub local_addr: SocketAddr,
    pub owner: RwLock<LocalPid>,
    accept_task: Mutex<Option<JoinHandle<()>>>,
    pub closed: AtomicBool,
    settings: config::ServerSettings,
    generation: RwLock<Arc<CertificateGeneration>>,
}

impl ListenerResource {
    pub fn new(
        endpoint: quinn::Endpoint,
        local_addr: SocketAddr,
        owner: LocalPid,
        settings: config::ServerSettings,
        generation: CertificateGeneration,
    ) -> Self {
        Self {
            endpoint,
            local_addr,
            owner: RwLock::new(owner),
            accept_task: Mutex::new(None),
            closed: AtomicBool::new(false),
            settings,
            generation: RwLock::new(Arc::new(generation)),
        }
    }

    pub fn set_accept_task(&self, handle: JoinHandle<()>) {
        let mut task = self.accept_task.lock().unwrap();
        *task = Some(handle);
    }

    /// The generation a connection accepted now is accepted with.
    fn current_generation(&self) -> Arc<CertificateGeneration> {
        self.generation.read().unwrap().clone()
    }

    /// Makes `generation` the one connections are accepted with from now on,
    /// and the endpoint's default server configuration.
    fn make_current(&self, generation: CertificateGeneration) {
        let mut current = self.generation.write().unwrap();
        self.endpoint
            .set_server_config(Some((*generation.server_config).clone()));
        *current = Arc::new(generation);
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
    stream_receive_window: u64,
    receive_window: u64,
) -> NifResult<Term<'a>> {
    let caller = env.pid();

    let addr: IpAddr = bind_addr
        .parse()
        .map_err(|e| rustler::Error::Term(Box::new(format!("invalid bind_addr: {}", e))))?;

    let settings = config::ServerSettings {
        alpn,
        idle_timeout_ms,
        keep_alive_ms,
        bidi_streams,
        uni_streams,
        stream_receive_window,
        receive_window,
    };
    let (server_config, leaf) = config::build_server_config(&certfile, &keyfile, &settings)
        .map_err(|e| rustler::Error::Term(Box::new(e)))?;
    let generation = CertificateGeneration::new(server_config, leaf);

    let socket = config::create_bound_socket(addr, port as u16)
        .map_err(|e| rustler::Error::Term(Box::new(e)))?;

    // Quinn's TokioRuntime requires a tokio context (Handle::current()).
    // We enter the runtime context here since NIFs run on BEAM scheduler threads.
    let _guard = runtime::rt().enter();

    let endpoint = quinn::Endpoint::new(
        quinn::EndpointConfig::default(),
        Some((*generation.server_config).clone()),
        socket,
        Arc::new(quinn::TokioRuntime),
    )
    .map_err(|e| rustler::Error::Term(Box::new(format!("endpoint create: {}", e))))?;

    let local_addr = endpoint
        .local_addr()
        .map_err(|e| rustler::Error::Term(Box::new(format!("local_addr: {}", e))))?;

    let resource = ResourceArc::new(ListenerResource::new(
        endpoint, local_addr, caller, settings, generation,
    ));

    Ok((atoms::ok(), resource).encode(env))
}

/// NIF: close_listener(ListenerRef) -> ok
#[rustler::nif]
fn nif_close_listener<'a>(
    env: Env<'a>,
    listener: ResourceArc<ListenerResource>,
) -> NifResult<Term<'a>> {
    // Marked closed under the owner lock that a handshake holds from its
    // closed check until it sends new_conn: no new_conn follows this call.
    {
        let _owner = listener.owner.write().unwrap();
        listener.closed.store(true, Ordering::SeqCst);
    }
    if let Some(task) = listener.accept_task.lock().unwrap().take() {
        task.abort();
    }
    listener.endpoint.close(0u32.into(), b"shutdown");
    Ok(atoms::ok().encode(env))
}

/// NIF: reload_certificate(ListenerRef, CertFile, KeyFile) -> ok | {error, Reason}
///
/// Builds the listener's next certificate generation from the files, with
/// the settings the listener was started with, and makes it current.
/// Connections accepted afterwards present its leaf; a connection accepted
/// earlier keeps the leaf it presented. On an error the current generation
/// stays. It reads and parses files, like `nif_listen`.
#[rustler::nif(schedule = "DirtyCpu")]
fn nif_reload_certificate<'a>(
    env: Env<'a>,
    listener: ResourceArc<ListenerResource>,
    certfile: String,
    keyfile: String,
) -> NifResult<Term<'a>> {
    match config::build_server_config(&certfile, &keyfile, &listener.settings) {
        Ok((server_config, leaf)) => {
            listener.make_current(CertificateGeneration::new(server_config, leaf));
            Ok(atoms::ok().encode(env))
        }
        Err(e) => Ok((atoms::error(), e).encode(env)),
    }
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
                // Spawn per-connection task for handshake
                Some(incoming) => {
                    tokio::spawn(accept_connection(listener_arc.clone(), incoming));
                }
                None => break, // Endpoint closed
            }
        }
    });

    listener.set_accept_task(handle);
    Ok(atoms::ok().encode(env))
}

/// Completes one incoming connection's handshake and hands the connection to
/// the listener's owner. The configuration it is accepted with and the leaf
/// it records come from one generation, so a reload racing this accept
/// cannot give the connection a leaf it did not present.
async fn accept_connection(listener: ResourceArc<ListenerResource>, incoming: quinn::Incoming) {
    if listener.closed.load(Ordering::Relaxed) {
        return;
    }
    let remote_addr = incoming.remote_address().to_string();
    let generation = listener.current_generation();
    let accepted = match incoming.accept_with(generation.server_config.clone()) {
        Ok(connecting) => connecting.await,
        Err(e) => Err(e),
    };
    match accepted {
        Ok(connection) => {
            // Held from the closed check until new_conn is sent. close_listener
            // marks the listener closed under the write lock, so no new_conn
            // follows a close_listener that returned.
            let owner = listener.owner.read().unwrap();
            if listener.closed.load(Ordering::SeqCst) {
                connection.close(0u32.into(), b"shutdown");
                return;
            }
            let conn_resource = ResourceArc::new(connection::ConnectionResource::accepted(
                connection,
                *owner,
                generation.leaf.clone(),
            ));
            message::send_new_conn(&owner, conn_resource, remote_addr);
        }
        Err(e) => {
            eprintln!("[macula_quic] accept handshake failed: {}", e);
        }
    }
}
