mod atoms;
mod cert;
mod config;
mod connection;
mod endpoint;
mod message;
mod runtime;
mod stream;

use rustler::{Env, Term};

fn on_load(env: Env, _info: Term) -> bool {
    runtime::init();
    rustler::resource!(endpoint::ListenerResource, env);
    rustler::resource!(connection::ConnectionResource, env);
    rustler::resource!(stream::StreamResource, env);
    true
}

rustler::init!(
    "macula_quic",
    [
        // Listener
        endpoint::nif_listen,
        endpoint::nif_close_listener,
        endpoint::nif_async_accept,
        // Connection
        connection::nif_connect,
        connection::nif_open_stream,
        connection::nif_close_connection,
        connection::nif_async_accept_stream,
        connection::nif_controlling_process_conn,
        connection::nif_peername,
        connection::nif_max_datagram_size,
        // Stream
        stream::nif_send,
        stream::nif_async_send,
        stream::nif_close_stream,
        stream::nif_setopt_active,
        stream::nif_controlling_process,
        // Sovereign overlay (Yggdrasil) helpers
        cert::nif_generate_self_signed_cert,
    ],
    load = on_load
);
