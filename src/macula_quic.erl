%%%-------------------------------------------------------------------
%%% @doc Macula QUIC transport — Quinn-based Rust NIF.
%%%
%%% Provides QUIC listener, connection, and stream operations backed
%%% by Quinn (Rust) instead of MsQuic. Key improvement: listeners
%%% actually bind to specific IP addresses, enabling per-identity
%%% IPv6 binding for virtual relay identities.
%%%
%%% Active-mode messages delivered to owning process:
%%%   {quic, Data, StreamRef, Flags}       — stream data
%%%   {quic, new_conn, ConnRef, Info}      — new connection accepted
%%%   {quic, new_stream, StreamRef, Props} — new stream accepted
%%%   {quic, peer_send_shutdown, StreamRef, undefined}
%%%   {quic, stream_closed, StreamRef, Flags}
%%%   {quic, shutdown, Handle, Reason}
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic).

-include_lib("kernel/include/logger.hrl").

-on_load(init/0).

-export([
    %% Listener
    listen/2,
    listen/3,
    async_accept/1,
    async_accept/2,
    close_listener/1,

    %% Connection
    connect/4,
    open_stream/1,
    close_connection/1,
    async_accept_stream/1,
    async_accept_stream/2,
    handshake/1,
    peername/1,
    sockname/1,

    %% Stream
    send/2,
    async_send/2,
    close_stream/1,
    setopt/3,
    controlling_process/2,

    %% Compat: generic close (tries stream → conn → listener)
    close/1,

    %% Dist compat (synchronous accept, stream accept with opts)
    accept/2,
    accept_stream/3,
    open_stream/2,
    handoff_stream/3,

    %% Shutdown (maps to close with flags)
    async_shutdown_stream/3,
    async_shutdown_connection/3,

    %% Stats
    getstat/2
]).

%%%===================================================================
%%% NIF Loading
%%%===================================================================

init() ->
    PrivDir = code:priv_dir(macula),
    SoName = filename:join(PrivDir, "libmacula_quic"),
    case erlang:load_nif(SoName, 0) of
        ok ->
            ?LOG_INFO("[macula_quic] Quinn NIF loaded from ~s", [SoName]),
            ok;
        {error, {reload, _}} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING("[macula_quic] NIF load failed: ~p (path: ~s)", [Reason, SoName]),
            {error, Reason}
    end.

%%%===================================================================
%%% Listener API
%%%===================================================================

%% @doc Listen on a port or {Address, Port} tuple.
-spec listen(inet:port_number() | {string() | binary(), inet:port_number()}, list()) ->
    {ok, reference()} | {error, term()}.
listen({Address, Port}, Opts) ->
    listen(Address, Port, Opts);
listen(Port, Opts) when is_integer(Port) ->
    listen(<<"::">>, Port, Opts).

%% @doc Listen on a specific bind address and port.
%% BindAddr is a binary: "0.0.0.0", "192.168.1.1", "2600:3c0e::100", etc.
-spec listen(binary() | string(), inet:port_number(), list()) -> {ok, reference()} | {error, term()}.
listen(BindAddr, Port, Opts) when is_list(BindAddr) ->
    listen(list_to_binary(BindAddr), Port, Opts);
listen(BindAddr, Port, Opts) when is_binary(BindAddr) ->
    CertFile = to_binary(proplists:get_value(cert, Opts)),
    KeyFile = to_binary(proplists:get_value(key, Opts)),
    Alpn = [to_binary(A) || A <- proplists:get_value(alpn, Opts, ["macula"])],
    IdleTimeoutMs = proplists:get_value(idle_timeout_ms, Opts, 120000),
    KeepAliveMs = proplists:get_value(keep_alive_interval_ms, Opts, 30000),
    BidiStreams = proplists:get_value(peer_bidi_stream_count, Opts, 100),
    UniStreams = proplists:get_value(peer_unidi_stream_count, Opts, 3),
    ?LOG_INFO("Starting listener on ~s:~p with idle_timeout=~pms, keep_alive=~pms",
              [BindAddr, Port, IdleTimeoutMs, KeepAliveMs]),
    nif_listen(BindAddr, Port, CertFile, KeyFile, Alpn,
               IdleTimeoutMs, KeepAliveMs, BidiStreams, UniStreams).

%% @doc Start accepting connections on a listener.
%% Delivers {quic, new_conn, ConnRef, Info} to the calling process.
-spec async_accept(reference()) -> ok | {error, term()}.
async_accept(Listener) ->
    async_accept(Listener, #{}).

-spec async_accept(reference(), map()) -> ok | {error, term()}.
async_accept(Listener, _Opts) ->
    nif_async_accept(Listener).

%% @doc Close a listener.
-spec close_listener(reference()) -> ok.
close_listener(Listener) ->
    nif_close_listener(Listener).

%%%===================================================================
%%% Connection API
%%%===================================================================

%% @doc Connect to a remote QUIC server.
-spec connect(string() | binary(), inet:port_number(), list(), timeout()) ->
    {ok, reference()} | {error, term()}.
connect(Host, Port, Opts, Timeout) ->
    HostBin = to_binary(Host),
    Alpn = [to_binary(A) || A <- proplists:get_value(alpn, Opts, ["macula"])],
    Verify = proplists:get_value(verify, Opts, none) =/= none,
    IdleTimeoutMs = proplists:get_value(idle_timeout_ms, Opts, 60000),
    KeepAliveMs = proplists:get_value(keep_alive_interval_ms, Opts, 20000),
    nif_connect(HostBin, Port, Alpn, Verify, IdleTimeoutMs, KeepAliveMs, Timeout).

%% @doc Open a new bidirectional stream.
-spec open_stream(reference()) -> {ok, reference()} | {error, term()}.
open_stream(Conn) ->
    nif_open_stream(Conn).

%% @doc Close a connection.
-spec close_connection(reference()) -> ok.
close_connection(Conn) ->
    nif_close_connection(Conn).

%% @doc Start accepting streams on a connection.
%% Delivers {quic, new_stream, StreamRef, #{conn => ConnRef}} to the owning process.
-spec async_accept_stream(reference()) -> ok | {error, term()}.
async_accept_stream(Conn) ->
    async_accept_stream(Conn, #{}).

-spec async_accept_stream(reference(), map()) -> ok | {error, term()}.
async_accept_stream(Conn, _Opts) ->
    nif_async_accept_stream(Conn).

%% @doc Complete TLS handshake.
%% With Quinn, handshake completes during accept — this is a no-op for compat.
-spec handshake(reference()) -> ok | {ok, reference()} | {error, term()}.
handshake(Conn) ->
    {ok, Conn}.

%% @doc Get remote address of a connection.
-spec peername(reference()) -> {ok, {string(), inet:port_number()}} | {error, term()}.
peername(Conn) ->
    nif_peername(Conn).

%% @doc Get local address (liveness probe).
-spec sockname(reference()) -> {ok, {string(), inet:port_number()}} | {error, term()}.
sockname(_Conn) ->
    %% TODO: implement via nif_sockname
    {ok, {"0.0.0.0", 0}}.

%%%===================================================================
%%% Stream API
%%%===================================================================

%% @doc Send data on a stream (blocking).
-spec send(reference(), iodata()) -> ok | {error, term()}.
send(Stream, Data) ->
    nif_send(Stream, iolist_to_binary(Data)).

%% @doc Send data asynchronously.
-spec async_send(reference(), iodata()) -> ok | {error, term()}.
async_send(Stream, Data) ->
    nif_async_send(Stream, iolist_to_binary(Data)).

%% @doc Close a stream.
-spec close_stream(reference()) -> ok.
close_stream(Stream) ->
    nif_close_stream(Stream).

%% @doc Set active mode on a stream handle.
-spec setopt(reference(), active, boolean()) -> ok.
setopt(Stream, active, Value) ->
    nif_setopt_active(Stream, Value).

%% @doc Transfer ownership of a handle to another process.
%% Works with both stream and connection handles.
-spec controlling_process(reference(), pid()) -> ok | {error, term()}.
controlling_process(Handle, Pid) ->
    try nif_controlling_process(Handle, Pid)
    catch error:badarg ->
        nif_controlling_process_conn(Handle, Pid)
    end.

%%%===================================================================
%%% Compat API
%%%===================================================================

%% @doc Generic close — tries stream, then connection, then listener.
-spec close(reference()) -> ok.
close(Ref) ->
    close_as(Ref, [fun nif_close_stream/1,
                    fun nif_close_connection/1,
                    fun nif_close_listener/1]).

close_as(_Ref, []) -> ok;
close_as(Ref, [CloseFn | Rest]) ->
    try CloseFn(Ref) of ok -> ok
    catch _:_ -> close_as(Ref, Rest)
    end.

%% @doc Async shutdown stream (compat with quicer flags).
-spec async_shutdown_stream(reference(), integer(), integer()) -> ok.
async_shutdown_stream(Stream, _Flag, _Code) ->
    nif_close_stream(Stream).

%% @doc Async shutdown connection (compat with quicer flags).
-spec async_shutdown_connection(reference(), integer(), integer()) -> ok.
async_shutdown_connection(Conn, _Flag, _Code) ->
    nif_close_connection(Conn).

%% @doc Get connection stats (stub — returns empty for now).
-spec getstat(reference(), [atom()]) -> {ok, [{atom(), integer()}]} | {error, term()}.
getstat(_Conn, Stats) ->
    {ok, [{S, 0} || S <- Stats]}.

%%%===================================================================
%%% Dist Compat API
%%%===================================================================

%% @doc Synchronous accept (for macula_dist). Blocks until connection arrives.
-spec accept(reference(), map()) -> {ok, reference()} | {error, term()}.
accept(_Listener, _Opts) ->
    %% TODO: implement blocking accept for dist — for now returns error
    {error, not_implemented}.

%% @doc Accept stream with options and timeout (for macula_dist).
-spec accept_stream(reference(), map(), timeout()) -> {ok, reference()} | {error, term()}.
accept_stream(Conn, _Opts, _Timeout) ->
    async_accept_stream(Conn).

%% @doc Open stream with options map (for macula_dist).
-spec open_stream(reference(), map()) -> {ok, reference()} | {error, term()}.
open_stream(Conn, _Opts) ->
    open_stream(Conn).

%% @doc Hand off a stream to another process (for macula_dist).
-spec handoff_stream(reference(), pid(), map()) -> ok | {error, term()}.
handoff_stream(Stream, NewOwner, _Opts) ->
    controlling_process(Stream, NewOwner).

%%%===================================================================
%%% NIF Stubs
%%%===================================================================

nif_listen(_BindAddr, _Port, _CertFile, _KeyFile, _Alpn,
           _IdleTimeoutMs, _KeepAliveMs, _BidiStreams, _UniStreams) ->
    erlang:nif_error(nif_not_loaded).

nif_async_accept(_Listener) ->
    erlang:nif_error(nif_not_loaded).

nif_close_listener(_Listener) ->
    erlang:nif_error(nif_not_loaded).

nif_connect(_Host, _Port, _Alpn, _Verify, _IdleTimeoutMs, _KeepAliveMs, _TimeoutMs) ->
    erlang:nif_error(nif_not_loaded).

nif_open_stream(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_close_connection(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_async_accept_stream(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_peername(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_send(_Stream, _Data) ->
    erlang:nif_error(nif_not_loaded).

nif_async_send(_Stream, _Data) ->
    erlang:nif_error(nif_not_loaded).

nif_close_stream(_Stream) ->
    erlang:nif_error(nif_not_loaded).

nif_setopt_active(_Stream, _Value) ->
    erlang:nif_error(nif_not_loaded).

nif_controlling_process(_Handle, _Pid) ->
    erlang:nif_error(nif_not_loaded).

nif_controlling_process_conn(_Handle, _Pid) ->
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% Internal
%%%===================================================================

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A).
