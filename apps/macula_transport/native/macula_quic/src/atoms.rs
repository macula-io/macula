rustler::atoms! {
    ok,
    error,

    // Top-level event atom
    quic,

    // Connection events
    new_conn,
    new_stream,

    // Stream events
    peer_send_shutdown,
    peer_send_aborted,
    send_shutdown_complete,
    stream_closed,

    // Connection lifecycle
    shutdown,
    closed,
    transport_shutdown,

    // Flow control (ignored by macula but delivered for compat)
    streams_available,
    peer_needs_streams,

    // Options
    active,

    // TLS modes
    none,      // verify: none
    peer,      // verify: peer

    // Error reasons
    timeout,
    closed_by_peer,
    connection_refused,
    nif_not_loaded,
    invalid_handle,
    already_closed,
    stream_finished,
    unknown_error,

    // Stat keys
    rtt,
    cwnd,
    send_count,
    recv_count,
    lost_count,
}
