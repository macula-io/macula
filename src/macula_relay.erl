%%%-------------------------------------------------------------------
%%% @doc Macula Relay Server — hub-spoke message routing via pg + gproc.
%%%
%%% Accepts QUIC connections from nodes. Each connection gets a handler
%%% process. Pub/sub uses OTP pg groups. RPC uses gproc registry.
%%% Process death = automatic cleanup (no TTLs, no manual eviction).
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
        {idle_timeout_ms, 120000},
        {keep_alive_interval_ms, 30000}
    ],

    case macula_quic:listen(Port, ListenOpts) of
        {ok, Listener} ->
            ?LOG_INFO("[relay] Listening on port ~p", [Port]),
            %% Start accepting connections
            self() ! accept,
            {ok, #state{listener = Listener, port = Port, handlers = []}};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, #state{listener = Listener} = State) ->
    spawn_link(fun() -> accept_loop(Listener, self()) end),
    {noreply, State};

handle_info({new_connection, Conn}, State) ->
    {ok, Pid} = macula_relay_handler:start_link(Conn),
    ?LOG_INFO("[relay] New connection, handler ~p", [Pid]),
    {noreply, State#state{handlers = [Pid | State#state.handlers]}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener = Listener}) ->
    catch macula_quic:close(Listener),
    ok.

%%====================================================================
%% Internal
%%====================================================================

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

accept_loop(Listener, Parent) ->
    case macula_quic:accept(Listener, 60000) of
        {ok, Conn} ->
            Parent ! {new_connection, Conn},
            accept_loop(Listener, Parent);
        {error, timeout} ->
            accept_loop(Listener, Parent);
        {error, Reason} ->
            ?LOG_WARNING("[relay] Accept error: ~p", [Reason]),
            timer:sleep(1000),
            accept_loop(Listener, Parent)
    end.
