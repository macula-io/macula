use std::sync::OnceLock;
use tokio::runtime::Runtime;

static TOKIO_RT: OnceLock<Runtime> = OnceLock::new();

// Shared client endpoints, one per address family, reused across ALL
// outbound `connect` calls.
//
// Previously `nif_connect` built a fresh `quinn::Endpoint` per dial: each
// bound a new UDP socket and spawned a new endpoint-driver task on the
// shared runtime. In a long-lived client that reconnects continuously
// (the realm holds ~64 station-links across several subsystems, each
// re-dialing on churn) these per-connect endpoints accumulated hundreds
// of driver tasks + sockets on the 4-worker runtime, starving its
// reactor/timer until every subsequent dial hung and its timeout never
// fired. One shared endpoint per family fixes that: N connections
// multiplex over a single socket/driver, the normal quinn client shape.
static CLIENT_ENDPOINT_V6: OnceLock<quinn::Endpoint> = OnceLock::new();
static CLIENT_ENDPOINT_V4: OnceLock<quinn::Endpoint> = OnceLock::new();

/// Initialize the shared tokio runtime. Called once from NIF on_load.
pub fn init() -> bool {
    TOKIO_RT
        .get_or_init(|| {
            let worker_threads = std::env::var("MACULA_QUIC_WORKERS")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(4);

            tokio::runtime::Builder::new_multi_thread()
                .worker_threads(worker_threads)
                .enable_all()
                .thread_name("macula-quic")
                .build()
                .expect("failed to create tokio runtime for macula_quic")
        });
    true
}

/// Get the shared tokio runtime.
pub fn rt() -> &'static Runtime {
    TOKIO_RT.get().expect("macula_quic tokio runtime not initialized")
}

/// Get (or lazily create) the shared client endpoint for the given
/// address family. The returned handle is cheap to clone (Arc-backed).
///
/// Must be called from within the tokio runtime context (e.g. inside
/// `rt().block_on(...)`), because creating the endpoint spawns its driver
/// task on the ambient runtime.
pub fn client_endpoint(is_ipv6: bool) -> Result<quinn::Endpoint, String> {
    let cell = if is_ipv6 {
        &CLIENT_ENDPOINT_V6
    } else {
        &CLIENT_ENDPOINT_V4
    };
    if let Some(ep) = cell.get() {
        return Ok(ep.clone());
    }
    let bind: std::net::SocketAddr = if is_ipv6 {
        "[::]:0".parse().unwrap()
    } else {
        "0.0.0.0:0".parse().unwrap()
    };
    // On a rare init race the loser's freshly-bound endpoint is dropped
    // (socket closed) and the winner's is returned — harmless.
    let created = quinn::Endpoint::client(bind).map_err(|e| format!("client endpoint: {}", e))?;
    Ok(cell.get_or_init(|| created).clone())
}
