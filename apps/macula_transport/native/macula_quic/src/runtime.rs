use std::sync::OnceLock;
use tokio::runtime::Runtime;

static TOKIO_RT: OnceLock<Runtime> = OnceLock::new();

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
