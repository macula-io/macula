%% @doc Friendly wrapper around the Quinn QUIC NIF.
%%
%% The NIF (`macula_quic') exposes raw `nif_*' calls. This module:
%% <ul>
%%   <li>Validates inputs and applies sensible defaults.</li>
%%   <li>Returns `{ok, Handle} | {error, Reason}' uniformly.</li>
%%   <li>Documents the message protocol that NIF callbacks use to deliver
%%       events to controlling pids.</li>
%% </ul>
%%
%% == Message protocol ==
%%
%% NIF-driven events are delivered to the controlling pid as Erlang messages:
%% <ul>
%%   <li>`{quic, new_conn, ConnRef, #{remote_addr := binary()}}' — listener
%%       received an inbound connection (after `accept/1' was called).</li>
%%   <li>`{quic, new_stream, StreamRef, #{conn := ConnRef, flags := non_neg_integer()}}'
%%       — connection received an inbound stream.</li>
%%   <li>`{quic, Bin :: binary(), StreamRef, Flags :: non_neg_integer()}' —
%%       stream data; only when `setopt_active(Stream, true | once)' was called.</li>
%%   <li>`{quic, Event :: atom(), Handle, Detail}' — lifecycle events
%%       (`shutdown', `closed', `peer_send_shutdown', etc.).</li>
%% </ul>
-module(macula_transport).

-export([
    %% Listener
    listen/1,
    accept/1,
    close_listener/1,

    %% Outbound connect
    connect/1,
    close_connection/1,
    peername/1,
    controlling_process_conn/2,

    %% Streams
    open_stream/1,
    accept_stream/1,
    send/2,
    setopt_active/2,
    close_stream/1,
    controlling_process/2
]).

-export_type([
    listen_opts/0,
    connect_opts/0,
    listener/0,
    connection/0,
    stream/0,
    active/0
]).

-type listener()   :: reference().
-type connection() :: reference().
-type stream()     :: reference().
-type active()     :: true | false | once.

-type listen_opts() :: #{
    bind         := inet:ip_address() | string() | binary(),
    port         := inet:port_number(),
    certfile     := string() | binary(),
    keyfile      := string() | binary(),
    alpn         => [binary()],
    idle_ms      => non_neg_integer(),
    keep_alive_ms => non_neg_integer(),
    bidi_streams => non_neg_integer(),
    uni_streams  => non_neg_integer()
}.

-type connect_opts() :: #{
    host          := string() | binary(),
    port          := inet:port_number(),
    alpn          => [binary()],
    verify        => true | false,
    idle_ms       => non_neg_integer(),
    keep_alive_ms => non_neg_integer(),
    timeout_ms    => non_neg_integer()
}.

%% Defaults shared between listener + dialer.
-define(DEFAULT_ALPN, [<<"macula/2">>]).
-define(DEFAULT_IDLE_MS, 30_000).
-define(DEFAULT_KEEPALIVE_MS, 10_000).
-define(DEFAULT_BIDI, 1024).
-define(DEFAULT_UNI, 1024).
-define(DEFAULT_CONNECT_TIMEOUT_MS, 5_000).

%%------------------------------------------------------------------
%% Listener
%%------------------------------------------------------------------

-spec listen(listen_opts()) -> {ok, listener()} | {error, term()}.
listen(#{bind := Bind, port := Port, certfile := CertFile, keyfile := KeyFile} = Opts)
  when is_integer(Port), Port >= 0, Port =< 65535 ->
    Alpn        = to_alpn(maps:get(alpn, Opts, ?DEFAULT_ALPN)),
    IdleMs      = maps:get(idle_ms, Opts, ?DEFAULT_IDLE_MS),
    KeepAliveMs = maps:get(keep_alive_ms, Opts, ?DEFAULT_KEEPALIVE_MS),
    Bidi        = maps:get(bidi_streams, Opts, ?DEFAULT_BIDI),
    Uni         = maps:get(uni_streams, Opts, ?DEFAULT_UNI),
    macula_quic:nif_listen(
        to_str(Bind),
        Port,
        to_str(CertFile),
        to_str(KeyFile),
        Alpn,
        IdleMs,
        KeepAliveMs,
        Bidi,
        Uni
    ).

%% @doc Arm the listener for one async accept. The caller will receive
%% `{quic, new_conn, ConnRef, #{remote_addr := binary()}}' when a peer
%% connects. Call again from the message handler to keep accepting.
-spec accept(listener()) -> ok | {error, term()}.
accept(Listener) ->
    macula_quic:nif_async_accept(Listener).

-spec close_listener(listener()) -> ok | {error, term()}.
close_listener(Listener) ->
    macula_quic:nif_close_listener(Listener).

%%------------------------------------------------------------------
%% Outbound connect
%%------------------------------------------------------------------

-spec connect(connect_opts()) -> {ok, connection()} | {error, term()}.
connect(#{host := Host, port := Port} = Opts)
  when is_integer(Port), Port >= 0, Port =< 65535 ->
    Alpn        = to_alpn(maps:get(alpn, Opts, ?DEFAULT_ALPN)),
    Verify      = maps:get(verify, Opts, false),
    IdleMs      = maps:get(idle_ms, Opts, ?DEFAULT_IDLE_MS),
    KeepAliveMs = maps:get(keep_alive_ms, Opts, ?DEFAULT_KEEPALIVE_MS),
    TimeoutMs   = maps:get(timeout_ms, Opts, ?DEFAULT_CONNECT_TIMEOUT_MS),
    macula_quic:nif_connect(
        to_str(Host),
        Port,
        Alpn,
        Verify,
        IdleMs,
        KeepAliveMs,
        TimeoutMs
    ).

-spec close_connection(connection()) -> ok | {error, term()}.
close_connection(Conn) ->
    macula_quic:nif_close_connection(Conn).

-spec peername(connection()) -> {ok, binary()} | {error, term()}.
peername(Conn) ->
    macula_quic:nif_peername(Conn).

-spec controlling_process_conn(connection(), pid()) -> ok | {error, term()}.
controlling_process_conn(Conn, Pid) when is_pid(Pid) ->
    macula_quic:nif_controlling_process_conn(Conn, Pid).

%%------------------------------------------------------------------
%% Streams
%%------------------------------------------------------------------

-spec open_stream(connection()) -> {ok, stream()} | {error, term()}.
open_stream(Conn) ->
    macula_quic:nif_open_stream(Conn).

%% @doc Arm the connection for one async stream accept. Caller will receive
%% `{quic, new_stream, StreamRef, #{conn := ConnRef, flags := non_neg_integer()}}'.
-spec accept_stream(connection()) -> ok | {error, term()}.
accept_stream(Conn) ->
    macula_quic:nif_async_accept_stream(Conn).

-spec send(stream(), iodata()) -> ok | {error, term()}.
send(Stream, Data) ->
    macula_quic:nif_send(Stream, iolist_to_binary(Data)).

-spec setopt_active(stream(), active()) -> ok | {error, term()}.
setopt_active(Stream, Active) when Active =:= true; Active =:= false; Active =:= once ->
    macula_quic:nif_setopt_active(Stream, Active).

-spec close_stream(stream()) -> ok | {error, term()}.
close_stream(Stream) ->
    macula_quic:nif_close_stream(Stream).

-spec controlling_process(stream(), pid()) -> ok | {error, term()}.
controlling_process(Stream, Pid) when is_pid(Pid) ->
    macula_quic:nif_controlling_process(Stream, Pid).

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

to_str(B) when is_binary(B) -> binary_to_list(B);
to_str(L) when is_list(L) -> L;
to_str({A, B, C, D}) ->
    inet:ntoa({A, B, C, D});
to_str({A, B, C, D, E, F, G, H}) ->
    inet:ntoa({A, B, C, D, E, F, G, H}).

to_alpn(L) when is_list(L) ->
    [iolist_to_binary(A) || A <- L].
