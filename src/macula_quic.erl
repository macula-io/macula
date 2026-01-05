%%%-------------------------------------------------------------------
%%% @doc
%%% Main API module for Macula QUIC transport.
%%% Provides a simplified wrapper around the quicer library.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic).

-include_lib("kernel/include/logger.hrl").

-export([
    listen/2,
    connect/4,
    accept/2,
    accept_stream/2,
    open_stream/1,
    send/2,
    async_send/2,
    recv/2,
    close/1,
    peername/1
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start a QUIC listener on the specified port.
%% Options:
%%   {cert, CertFile} - Path to PEM certificate file
%%   {key, KeyFile} - Path to PEM private key file
%%   {alpn, [Protocol]} - List of ALPN protocols (e.g., ["macula"])
%%   {peer_unidi_stream_count, N} - Max unidirectional streams
%%   {peer_bidi_stream_count, N} - Max bidirectional streams
%%   {idle_timeout_ms, N} - Connection idle timeout in milliseconds
%%   {keep_alive_interval_ms, N} - Keep-alive PING interval in milliseconds
%% @end
-spec listen(inet:port_number(), list()) -> {ok, reference()} | {error, term()}.
listen(Port, Opts) ->
    %% Extract certificate files
    CertFile = proplists:get_value(cert, Opts),
    KeyFile = proplists:get_value(key, Opts),
    AlpnProtocols = proplists:get_value(alpn, Opts, ["macula"]),
    PeerUnidiStreamCount = proplists:get_value(peer_unidi_stream_count, Opts, 3),
    PeerBidiStreamCount = proplists:get_value(peer_bidi_stream_count, Opts, 100),

    %% Timeout and keep-alive configuration for mesh stability
    %% - idle_timeout_ms: How long connection can be idle before closing (60s default)
    %% - keep_alive_interval_ms: PING interval to keep connection alive (20s default)
    %% - Keep-alive MUST be < idle_timeout to prevent premature closure
    %% - 20s keep-alive stays ahead of typical NAT timeouts (20-30s)
    IdleTimeoutMs = proplists:get_value(idle_timeout_ms, Opts, 60000),
    KeepAliveIntervalMs = proplists:get_value(keep_alive_interval_ms, Opts, 20000),
    HandshakeIdleTimeoutMs = proplists:get_value(handshake_idle_timeout_ms, Opts, 30000),

    %% Build quicer listener options
    ListenerOpts = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        {alpn, AlpnProtocols},
        {peer_unidi_stream_count, PeerUnidiStreamCount},
        {peer_bidi_stream_count, PeerBidiStreamCount},
        {idle_timeout_ms, IdleTimeoutMs},
        {keep_alive_interval_ms, KeepAliveIntervalMs},
        {handshake_idle_timeout_ms, HandshakeIdleTimeoutMs}
    ],

    %% Start QUIC listener
    %% The calling process becomes the owner and will receive connection messages
    ?LOG_INFO("Starting listener on port ~p with idle_timeout=~pms, keep_alive=~pms",
              [Port, IdleTimeoutMs, KeepAliveIntervalMs]),
    quicer:listen(Port, ListenerOpts).

%% @doc Connect to a QUIC server.
%% Options:
%%   {alpn, [Protocol]} - List of ALPN protocols
%%   {verify, none | verify_peer} - Certificate verification mode
%%   {cacertfile, Path} - CA certificate bundle for verification (v0.16.3+)
%%   {depth, N} - Max certificate chain depth (v0.16.3+)
%%   {server_name_indication, Host} - SNI hostname (v0.16.3+)
%%   {idle_timeout_ms, N} - Connection idle timeout in milliseconds
%%   {keep_alive_interval_ms, N} - Keep-alive PING interval in milliseconds
%% @end
-spec connect(string() | inet:ip_address(), inet:port_number(), list(), timeout()) ->
    {ok, reference()} | {error, term()}.
connect(Host, Port, Opts, Timeout) ->
    %% Extract QUIC-specific options with defaults
    AlpnProtocols = proplists:get_value(alpn, Opts, ["macula"]),

    %% Timeout and keep-alive configuration for mesh stability
    %% CRITICAL: Both endpoints negotiate idle timeout - the SMALLER value wins
    %% So we must configure our client with proper values too
    IdleTimeoutMs = proplists:get_value(idle_timeout_ms, Opts, 60000),
    KeepAliveIntervalMs = proplists:get_value(keep_alive_interval_ms, Opts, 20000),
    HandshakeIdleTimeoutMs = proplists:get_value(handshake_idle_timeout_ms, Opts, 30000),

    %% Build base quicer options
    BaseOpts = [
        {alpn, AlpnProtocols},
        {idle_timeout_ms, IdleTimeoutMs},
        {keep_alive_interval_ms, KeepAliveIntervalMs},
        {handshake_idle_timeout_ms, HandshakeIdleTimeoutMs}
    ],

    %% Pass through ALL TLS options from macula_tls (v0.16.3+)
    %% This includes: verify, cacertfile, depth, server_name_indication, verify_fun
    TlsOptKeys = [verify, cacertfile, depth, server_name_indication, verify_fun, certfile, keyfile],
    TlsOpts = [{K, V} || K <- TlsOptKeys, {ok, V} <- [safe_get_value(K, Opts)]],

    QuicerOpts = BaseOpts ++ TlsOpts,

    %% Log connection attempt with TLS mode info
    VerifyMode = proplists:get_value(verify, Opts, none),
    CACertFile = proplists:get_value(cacertfile, Opts, undefined),
    ?LOG_INFO("Connecting to ~s:~p with idle_timeout=~pms, keep_alive=~pms, verify=~p, cacertfile=~p",
              [Host, Port, IdleTimeoutMs, KeepAliveIntervalMs, VerifyMode, CACertFile]),
    ?LOG_DEBUG("Full QuicerOpts: ~p", [QuicerOpts]),
    quicer:connect(Host, Port, QuicerOpts, Timeout).

%% @doc Safely get a value from proplist, returning {ok, Value} or error.
%% @private
-spec safe_get_value(atom(), list()) -> {ok, term()} | error.
safe_get_value(Key, Opts) ->
    case proplists:get_value(Key, Opts) of
        undefined -> error;
        Value -> {ok, Value}
    end.

%% @doc Accept an incoming connection on a listener.
%% After accepting, the connection needs handshake to complete.
-spec accept(reference(), timeout()) -> {ok, reference()} | {error, term()}.
accept(ListenerPid, Timeout) ->
    case quicer:accept(ListenerPid, [], Timeout) of
        {ok, Conn} ->
            %% Complete TLS handshake
            case quicer:handshake(Conn) of
                {ok, Conn} -> {ok, Conn};
                Error -> Error
            end;
        Error ->
            Error
    end.

%% @doc Accept an incoming stream on a connection.
-spec accept_stream(reference(), timeout()) -> {ok, reference()} | {error, term()}.
accept_stream(ConnPid, _Timeout) ->
    %% accept_stream/2 doesn't take timeout, it returns immediately
    quicer:accept_stream(ConnPid, []).

%% @doc Open a new bidirectional stream on a connection.
-spec open_stream(reference()) -> {ok, reference()} | {error, term()}.
open_stream(ConnPid) ->
    quicer:start_stream(ConnPid, []).

%% @doc Send data on a stream (blocking).
-spec send(reference(), iodata()) -> ok | {error, term()}.
send(StreamPid, Data) ->
    case quicer:send(StreamPid, Data) of
        {ok, _BytesSent} -> ok;
        Error -> Error
    end.

%% @doc Send data on a stream asynchronously (non-blocking).
%% This returns immediately without waiting for QUIC flow control.
-spec async_send(reference(), iodata()) -> ok | {error, term()}.
async_send(StreamPid, Data) ->
    case quicer:async_send(StreamPid, Data) of
        {ok, _BytesSent} -> ok;
        Error -> Error
    end.

%% @doc Receive data from a stream (blocking).
-spec recv(reference(), timeout()) -> {ok, binary()} | {error, term()}.
recv(StreamPid, Timeout) ->
    %% quicer sends data as messages, so we need to receive from mailbox
    receive
        {quic, Data, StreamPid, _Props} ->
            {ok, Data}
    after Timeout ->
        {error, timeout}
    end.

%% @doc Close a listener, connection, or stream.
-spec close(reference()) -> ok.
close(Pid) ->
    %% quicer uses different close functions based on resource type
    %% For simplicity, we try to close as stream first, then connection
    try
        quicer:close_stream(Pid)
    catch
        _:_ ->
            try
                quicer:close_connection(Pid)
            catch
                _:_ ->
                    quicer:close_listener(Pid)
            end
    end,
    ok.

%% @doc Get the peer's address from a stream or connection handle.
%% Returns {ok, {IP, Port}} on success or {error, Reason} on failure.
%% Works with both stream and connection handles.
-spec peername(term()) -> {ok, {inet:ip_address(), inet:port_number()}} | {error, term()}.
peername(Handle) ->
    %% quicer:peername/1 works on both stream and connection handles
    quicer:peername(Handle).
