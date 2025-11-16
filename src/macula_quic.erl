%%%-------------------------------------------------------------------
%%% @doc
%%% Main API module for Macula QUIC transport.
%%% Provides a simplified wrapper around the quicer library.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic).

-export([
    listen/2,
    connect/4,
    accept/2,
    accept_stream/2,
    open_stream/1,
    send/2,
    async_send/2,
    recv/2,
    close/1
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
%% @end
-spec listen(inet:port_number(), list()) -> {ok, pid()} | {error, term()}.
listen(Port, Opts) ->
    %% Extract certificate files
    CertFile = proplists:get_value(cert, Opts),
    KeyFile = proplists:get_value(key, Opts),
    AlpnProtocols = proplists:get_value(alpn, Opts, ["macula"]),
    PeerUnidiStreamCount = proplists:get_value(peer_unidi_stream_count, Opts, 3),
    PeerBidiStreamCount = proplists:get_value(peer_bidi_stream_count, Opts, 100),

    %% Build quicer listener options
    ListenerOpts = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        {alpn, AlpnProtocols},
        {peer_unidi_stream_count, PeerUnidiStreamCount},
        {peer_bidi_stream_count, PeerBidiStreamCount},
        {idle_timeout_ms, 0},  % Disable QUIC idle timeout (application handles keep-alive)
        {verify, none}  % Accept self-signed certificates (TODO: implement proper PKI/RPK)
    ],

    %% Start QUIC listener
    %% The calling process becomes the owner and will receive connection messages
    io:format("[QUIC] Starting listener on port ~p~n", [Port]),
    quicer:listen(Port, ListenerOpts).

%% @doc Connect to a QUIC server.
%% Options:
%%   {alpn, [Protocol]} - List of ALPN protocols
%%   {verify, none | verify_peer} - Certificate verification mode
%% @end
-spec connect(string() | inet:ip_address(), inet:port_number(), list(), timeout()) ->
    {ok, pid()} | {error, term()}.
connect(Host, Port, Opts, Timeout) ->
    %% Extract options
    AlpnProtocols = proplists:get_value(alpn, Opts, ["macula"]),
    Verify = proplists:get_value(verify, Opts, none),

    %% Build quicer options
    QuicerOpts = [
        {alpn, AlpnProtocols},
        {verify, Verify},
        {idle_timeout_ms, 0}  % Disable QUIC idle timeout (application handles keep-alive)
    ],

    %% Connect
    quicer:connect(Host, Port, QuicerOpts, Timeout).

%% @doc Accept an incoming connection on a listener.
%% After accepting, the connection needs handshake to complete.
-spec accept(pid(), timeout()) -> {ok, pid()} | {error, term()}.
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
-spec accept_stream(pid(), timeout()) -> {ok, pid()} | {error, term()}.
accept_stream(ConnPid, _Timeout) ->
    %% accept_stream/2 doesn't take timeout, it returns immediately
    quicer:accept_stream(ConnPid, []).

%% @doc Open a new bidirectional stream on a connection.
-spec open_stream(pid()) -> {ok, pid()} | {error, term()}.
open_stream(ConnPid) ->
    quicer:start_stream(ConnPid, []).

%% @doc Send data on a stream (blocking).
-spec send(pid(), binary()) -> ok | {error, term()}.
send(StreamPid, Data) ->
    case quicer:send(StreamPid, Data) of
        {ok, _BytesSent} -> ok;
        Error -> Error
    end.

%% @doc Send data on a stream asynchronously (non-blocking).
%% This returns immediately without waiting for QUIC flow control.
-spec async_send(pid(), binary()) -> ok | {error, term()}.
async_send(StreamPid, Data) ->
    case quicer:async_send(StreamPid, Data) of
        {ok, _BytesSent} -> ok;
        Error -> Error
    end.

%% @doc Receive data from a stream (blocking).
-spec recv(pid(), timeout()) -> {ok, binary()} | {error, term()}.
recv(StreamPid, Timeout) ->
    %% quicer sends data as messages, so we need to receive from mailbox
    receive
        {quic, Data, StreamPid, _Props} ->
            {ok, Data}
    after Timeout ->
        {error, timeout}
    end.

%% @doc Close a listener, connection, or stream.
-spec close(pid()) -> ok.
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
