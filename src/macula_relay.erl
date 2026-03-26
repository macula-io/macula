%%%-------------------------------------------------------------------
%%% @doc Macula Relay Server — hub-spoke message routing via pg + gproc.
%%%
%%% Accepts QUIC connections from nodes. Each connection gets a handler
%%% process. Pub/sub uses OTP pg groups. RPC uses gproc registry.
%%% Process death = automatic cleanup (no TTLs, no manual eviction).
%%%
%%% Uses async accept pattern (like macula_gateway_quic_server) —
%%% quicer:async_accept delivers {quic, new_conn, Conn, Info} messages.
%%%
%%% Start: `macula_relay:start_link(#{port => 4433}).'
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    listener :: reference() | undefined,
    port :: integer(),
    handlers :: [pid()]
}).

%%====================================================================
%% API
%%====================================================================

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    Port = maps:get(port, Opts, 4433),

    %% Ensure pg scope exists
    case pg:start(pg) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,

    %% Get TLS certs — check env vars first (production), then macula_tls (dev)
    {CertPath, KeyPath} = get_tls_paths(),
    ?LOG_INFO("[relay] TLS cert: ~s, key: ~s", [CertPath, KeyPath]),

    ListenOpts = [
        {cert, CertPath},
        {key, KeyPath},
        {alpn, ["macula"]},
        {peer_unidi_stream_count, 3},
        {peer_bidi_stream_count, 100},
        {idle_timeout_ms, 120000},
        {keep_alive_interval_ms, 30000}
    ],

    case macula_quic:listen(Port, ListenOpts) of
        {ok, Listener} ->
            ?LOG_INFO("[relay] Listening on port ~p", [Port]),
            %% Register for async connection events
            register_accept(Listener),
            {ok, #state{listener = Listener, port = Port, handlers = []}};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Async accept: quicer delivers new connections as messages
handle_info({quic, new_conn, Conn, ConnInfo}, #state{listener = Listener} = State) ->
    ?LOG_INFO("[relay] New connection: ~p", [ConnInfo]),
    complete_handshake(Conn),
    %% MUST re-register for next connection
    register_accept(Listener),
    {noreply, State};

%% Async stream: quicer delivers new streams as messages
handle_info({quic, new_stream, Stream, StreamProps}, State) ->
    ?LOG_INFO("[relay] New stream: ~p", [StreamProps]),
    %% Find which connection this stream belongs to
    Conn = case StreamProps of
        #{conn := C} -> C;
        _ -> get(pending_conn)
    end,
    case Conn of
        undefined ->
            ?LOG_WARNING("[relay] Stream arrived with no connection context"),
            {noreply, State};
        _ ->
            %% Start handler for this connection+stream pair
            {ok, Pid} = macula_relay_handler:start_link(Conn, Stream),
            ?LOG_INFO("[relay] Handler started: ~p", [Pid]),
            {noreply, State#state{handlers = [Pid | State#state.handlers]}}
    end;

%% QUIC data arriving on relay process (before handler takes over)
handle_info({quic, Data, _Stream, _Flags}, State) when is_binary(Data) ->
    ?LOG_DEBUG("[relay] Data on unowned stream (~p bytes)", [byte_size(Data)]),
    {noreply, State};

%% Handler process died — remove from handlers list
handle_info({'EXIT', Pid, Reason}, State) ->
    case lists:member(Pid, State#state.handlers) of
        true ->
            ?LOG_INFO("[relay] Handler ~p exited: ~p", [Pid, Reason]),
            {noreply, State#state{handlers = lists:delete(Pid, State#state.handlers)}};
        false ->
            {noreply, State}
    end;

handle_info(Info, State) ->
    ?LOG_DEBUG("[relay] Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{listener = Listener}) ->
    catch macula_quic:close(Listener),
    ok.

%%====================================================================
%% Internal
%%====================================================================

%% Register for async accept — quicer will send {quic, new_conn, ...}
register_accept(Listener) ->
    case quicer:async_accept(Listener, #{}) of
        {ok, _} ->
            ?LOG_INFO("[relay] Ready for connections"),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("[relay] async_accept failed: ~p", [Reason]),
            ok
    end.

%% Complete TLS handshake, then register for async stream accept
complete_handshake(Conn) ->
    case quicer:handshake(Conn) of
        ok ->
            accept_streams(Conn);
        {ok, _} ->
            accept_streams(Conn);
        {error, Reason} ->
            ?LOG_ERROR("[relay] Handshake failed: ~p", [Reason]),
            catch quicer:close_connection(Conn)
    end.

accept_streams(Conn) ->
    ?LOG_INFO("[relay] Handshake complete, accepting streams"),
    put(pending_conn, Conn),
    case quicer:async_accept_stream(Conn, #{}) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING("[relay] async_accept_stream failed: ~p", [Reason]),
            ok
    end.

%% @private Get TLS cert/key paths. Env vars take precedence (production),
%% then app config, then auto-generate for dev.
get_tls_paths() ->
    CertPath = case os:getenv("MACULA_TLS_CERTFILE") of
        false -> get_tls_path_from_config(cert);
        C -> C
    end,
    KeyPath = case os:getenv("MACULA_TLS_KEYFILE") of
        false -> get_tls_path_from_config(key);
        K -> K
    end,
    {CertPath, KeyPath}.

get_tls_path_from_config(cert) ->
    {CertPath, _} = macula_tls:get_cert_paths(),
    {ok, CertPath2, _, _} = macula_tls:ensure_cert_exists(CertPath, element(2, macula_tls:get_cert_paths())),
    CertPath2;
get_tls_path_from_config(key) ->
    {_, KeyPath} = macula_tls:get_cert_paths(),
    {ok, _, KeyPath2, _} = macula_tls:ensure_cert_exists(element(1, macula_tls:get_cert_paths()), KeyPath),
    KeyPath2.
