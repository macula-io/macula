mod atoms;
mod cert;
mod config;
mod connection;
mod endpoint;
mod error_codes;
mod message;
mod runtime;
mod stream;

use rustler::{Env, Term};

fn on_load(env: Env, _info: Term) -> bool {
    runtime::init();
    rustler::resource!(endpoint::ListenerResource, env);
    rustler::resource!(connection::ConnectionResource, env);
    rustler::resource!(connection::DialResource, env);
    rustler::resource!(connection::StreamOpenResource, env);
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
        endpoint::nif_reload_certificate,
        // Connection
        connection::nif_async_connect,
        connection::nif_cancel_connect,
        connection::nif_async_open_stream,
        connection::nif_cancel_open_stream,
        connection::nif_close_connection,
        connection::nif_close_connection_with_code,
        connection::nif_close_reason,
        connection::nif_async_accept_stream,
        connection::nif_controlling_process_conn,
        connection::nif_peername,
        connection::nif_max_datagram_size,
        connection::nif_lost_packets,
        connection::nif_peer_leaf,
        connection::nif_presented_leaf,
        // Stream
        stream::nif_send,
        stream::nif_async_send,
        stream::nif_async_send_tagged,
        stream::nif_close_stream,
        stream::nif_reset_stream,
        stream::nif_setopt_active,
        stream::nif_controlling_process,
        // Self-signed pubkey-anchored cert helpers
        cert::nif_generate_self_signed_cert,
    ],
    load = on_load
);
