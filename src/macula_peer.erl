%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Peer - Mesh Participant API (v0.7.0+).
%%%
%%% This module provides the high-level API for mesh participants.
%%% It acts as a facade/coordinator, delegating to specialized child processes:
%%%   - macula_connection: QUIC transport layer (send/receive, encoding/decoding)
%%%   - macula_pubsub_handler: Pub/sub message routing
%%%   - macula_rpc_handler: RPC call/response handling
%%%   - macula_advertisement_manager: DHT service advertisements
%%%
%%% Renamed from macula_connection in v0.7.0 for clarity:
%%% - macula_peer = mesh participant (this module)
%%% - macula_connection = QUIC transport (low-level)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer).

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    stop/1,
    publish/3,
    publish/4,
    subscribe/3,
    unsubscribe/2,
    call/3,
    call/4,
    advertise/4,
    unadvertise/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

-record(state, {
    url :: binary(),
    realm :: binary(),
    node_id :: binary(),

    %% Supervision tree child PIDs
    supervisor_pid :: pid(),
    connection_manager_pid :: pid(),
    pubsub_handler_pid :: pid(),
    rpc_handler_pid :: pid(),
    advertisement_manager_pid :: pid()
}).

-define(CONNECT_RETRY_DELAY, 1000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start a client connection to a Macula mesh.
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Url, Opts) ->
    gen_server:start_link(?MODULE, {Url, Opts}, []).

%% @doc Stop the client connection.
-spec stop(pid()) -> ok.
stop(Client) ->
    gen_server:stop(Client).

%% @doc Publish an event through this client (no options).
-spec publish(pid(), binary(), map() | binary()) -> ok | {error, term()}.
publish(Client, Topic, Data) ->
    publish(Client, Topic, Data, #{}).

%% @doc Publish an event through this client with options.
-spec publish(pid(), binary(), map() | binary(), map()) -> ok | {error, term()}.
publish(Client, Topic, Data, Opts) ->
    gen_server:call(Client, {publish, Topic, Data, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Subscribe to a topic through this client.
-spec subscribe(pid(), binary(), fun((map()) -> ok)) ->
    {ok, reference()} | {error, term()}.
subscribe(Client, Topic, Callback) ->
    gen_server:call(Client, {subscribe, Topic, Callback}, ?DEFAULT_TIMEOUT).

%% @doc Unsubscribe from a topic.
-spec unsubscribe(pid(), reference()) -> ok | {error, term()}.
unsubscribe(Client, SubRef) ->
    gen_server:call(Client, {unsubscribe, SubRef}, ?DEFAULT_TIMEOUT).

%% @doc Make an RPC call through this client (default timeout).
-spec call(pid(), binary(), map() | list()) -> {ok, term()} | {error, term()}.
call(Client, Procedure, Args) ->
    call(Client, Procedure, Args, #{}).

%% @doc Make an RPC call through this client with options.
-spec call(pid(), binary(), map() | list(), map()) ->
    {ok, term()} | {error, term()}.
call(Client, Procedure, Args, Opts) ->
    Timeout = maps:get(timeout, Opts, ?CALL_TIMEOUT),
    gen_server:call(Client, {call, Procedure, Args, Opts}, Timeout + 1000).

%% @doc Advertise a service handler for a procedure.
%%
%% This makes the local handler available to other mesh nodes via DHT.
%% The handler will be periodically re-advertised based on TTL.
-spec advertise(pid(), binary(), fun((map()) -> {ok, term()} | {error, term()}), map()) ->
    ok | {error, term()}.
advertise(Client, Procedure, Handler, Opts) ->
    gen_server:call(Client, {advertise, Procedure, Handler, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Stop advertising a service.
%%
%% Removes the local handler and stops advertising to the DHT.
-spec unadvertise(pid(), binary()) -> ok | {error, term()}.
unadvertise(Client, Procedure) ->
    gen_server:call(Client, {unadvertise, Procedure}, ?DEFAULT_TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Url, Opts}) ->
    %% Parse URL to extract host and port
    {Host, Port} = macula_utils:parse_url(Url),

    %% Get realm (required)
    Realm = get_realm_from_opts(Opts),

    %% Generate or get node ID
    NodeId = maps:get(node_id, Opts, macula_utils:generate_node_id()),

    ?LOG_INFO("[Connection Facade] Starting supervision tree for ~s", [Url]),

    %% Prepare opts for handlers (includes node_id, realm, url)
    HandlerOpts = Opts#{
        node_id => NodeId,
        realm => Realm,
        url => Url,
        host => Host,
        port => Port
    },

    %% Start supervision tree
    {ok, SupPid} = macula_connection_sup:start_link(Url, HandlerOpts),

    %% Look up child PIDs from supervisor
    Children = supervisor:which_children(SupPid),
    ConnMgrPid = find_child_pid(Children, connection_manager),
    PubSubPid = find_child_pid(Children, pubsub_handler),
    RpcPid = find_child_pid(Children, rpc_handler),
    AdvMgrPid = find_child_pid(Children, advertisement_manager),

    ?LOG_INFO("[Connection Facade] Supervision tree started - ConnMgr: ~p, PubSub: ~p, RPC: ~p, AdvMgr: ~p",
              [ConnMgrPid, PubSubPid, RpcPid, AdvMgrPid]),

    %% Send connection_manager_pid to children that need it
    gen_server:cast(PubSubPid, {set_connection_manager_pid, ConnMgrPid}),
    gen_server:cast(RpcPid, {set_connection_manager_pid, ConnMgrPid}),
    gen_server:cast(AdvMgrPid, {set_connection_manager_pid, ConnMgrPid}),

    State = #state{
        url = Url,
        realm = Realm,
        node_id = NodeId,
        supervisor_pid = SupPid,
        connection_manager_pid = ConnMgrPid,
        pubsub_handler_pid = PubSubPid,
        rpc_handler_pid = RpcPid,
        advertisement_manager_pid = AdvMgrPid
    },

    {ok, State}.

%% @private
%% Delegate to pubsub_handler
handle_call({publish, Topic, Data, Opts}, _From, State) ->
    Result = macula_pubsub_handler:publish(State#state.pubsub_handler_pid, Topic, Data, Opts),
    {reply, Result, State};

%% Delegate to pubsub_handler
handle_call({subscribe, Topic, Callback}, _From, State) ->
    Result = macula_pubsub_handler:subscribe(State#state.pubsub_handler_pid, Topic, Callback),
    {reply, Result, State};

%% Delegate to pubsub_handler
handle_call({unsubscribe, SubRef}, _From, State) ->
    Result = macula_pubsub_handler:unsubscribe(State#state.pubsub_handler_pid, SubRef),
    {reply, Result, State};

%% Delegate to rpc_handler
handle_call({call, Procedure, Args, Opts}, _From, State) ->
    Result = macula_rpc_handler:call(State#state.rpc_handler_pid, Procedure, Args, Opts),
    {reply, Result, State};

%% Delegate to advertisement_manager
handle_call({advertise, Procedure, Handler, Opts}, _From, State) ->
    Result = macula_advertisement_manager:advertise_service(State#state.advertisement_manager_pid, Procedure, Handler, Opts),
    {reply, Result, State};

%% Delegate to advertisement_manager
handle_call({unadvertise, Procedure}, _From, State) ->
    Result = macula_advertisement_manager:unadvertise_service(State#state.advertisement_manager_pid, Procedure),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{supervisor_pid = SupPid}) ->
    ?LOG_INFO("[Connection Facade] Terminating, stopping supervision tree"),
    %% Stop the supervisor (will stop all children)
    case SupPid of
        Pid when is_pid(Pid) ->
            macula_connection_sup:stop(Pid);
        _ ->
            ok
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Extract and normalize realm from options (pattern matching on type).
-spec get_realm_from_opts(map()) -> binary().
get_realm_from_opts(Opts) ->
    normalize_realm(maps:get(realm, Opts, undefined)).

normalize_realm(undefined) ->
    error({missing_required_option, realm});
normalize_realm(Realm) when is_binary(Realm) ->
    Realm;
normalize_realm(Realm) when is_list(Realm) ->
    list_to_binary(Realm);
normalize_realm(Realm) when is_atom(Realm) ->
    atom_to_binary(Realm).

%% @doc Find child PID from supervisor children list.
-spec find_child_pid(list(), atom()) -> pid().
find_child_pid(Children, ChildId) ->
    extract_child_pid(lists:keyfind(ChildId, 1, Children), ChildId).

extract_child_pid({_, Pid, _Type, _Modules}, _ChildId) when is_pid(Pid) ->
    Pid;
extract_child_pid({_, undefined, _Type, _Modules}, ChildId) ->
    error({child_not_started, ChildId});
extract_child_pid(false, ChildId) ->
    error({child_not_found, ChildId}).
