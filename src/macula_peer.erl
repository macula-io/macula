%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Peer - Mesh Participant API (v0.7.0+).
%%%
%%% This module provides the high-level API for mesh participants.
%%% Use this module to connect to a Macula mesh and communicate via pub/sub or RPC.
%%%
%%% == Quick Start ==
%%%
%%% ```
%%% %% 1. Connect to a gateway
%%% {ok, Peer} = macula_peer:start_link(<<"https://gateway.example.com:9443">>, #{
%%%     realm => <<"com.example.app">>
%%% }).
%%%
%%% %% 2. Subscribe to events
%%% ok = macula_peer:subscribe(Peer, <<"sensor.temperature">>, self()).
%%%
%%% %% 3. Publish an event
%%% ok = macula_peer:publish(Peer, <<"sensor.temperature">>, #{
%%%     device_id => <<"sensor-001">>,
%%%     celsius => 21.5
%%% }).
%%%
%%% %% 4. Call a remote service
%%% {ok, Result} = macula_peer:call(Peer, <<"calculator.add">>, #{a => 5, b => 3}).
%%%
%%% %% 5. Advertise a service
%%% ok = macula_peer:advertise(Peer, <<"calculator.add">>, fun(#{a := A, b := B}) ->
%%%     #{result => A + B}
%%% end, #{ttl => 300}).
%%% '''
%%%
%%% == Architecture ==
%%%
%%% The peer acts as a facade/coordinator, delegating to specialized child processes:
%%%   - `macula_connection': QUIC transport layer (send/receive, encoding/decoding)
%%%   - `macula_pubsub_handler': Pub/sub message routing
%%%   - `macula_rpc_handler': RPC call/response handling
%%%   - `macula_advertisement_manager': DHT service advertisements
%%%
%%% Renamed from macula_connection in v0.7.0 for clarity:
%%%   - `macula_peer' = mesh participant (this module)
%%%   - `macula_connection' = QUIC transport (low-level)
%%%
%%% == Multi-Tenancy via Realms ==
%%%
%%% Realms provide logical isolation for different applications:
%%%
%%% ```
%%% %% App 1
%%% {ok, Peer1} = macula_peer:start_link(GatewayUrl, #{realm => <<"com.app1">>}).
%%%
%%% %% App 2 (completely isolated from App 1)
%%% {ok, Peer2} = macula_peer:start_link(GatewayUrl, #{realm => <<"com.app2">>}).
%%% '''
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
    discover_subscribers/2,
    call/3,
    call/4,
    call_to/5,
    advertise/4,
    unadvertise/2,
    get_node_id/1
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
%% This is fire-and-forget - returns ok immediately without blocking.
%% Use QoS 1 in Opts if you need delivery confirmation.
-spec publish(pid(), binary(), map() | binary(), map()) -> ok.
publish(Client, Topic, Data, Opts) ->
    gen_server:cast(Client, {publish, Topic, Data, Opts}),
    ok.

%% @doc Subscribe to a topic through this client.
-spec subscribe(pid(), binary(), fun((map()) -> ok)) ->
    {ok, reference()} | {error, term()}.
subscribe(Client, Topic, Callback) ->
    gen_server:call(Client, {subscribe, Topic, Callback}, ?DEFAULT_TIMEOUT).

%% @doc Unsubscribe from a topic.
-spec unsubscribe(pid(), reference()) -> ok | {error, term()}.
unsubscribe(Client, SubRef) ->
    gen_server:call(Client, {unsubscribe, SubRef}, ?DEFAULT_TIMEOUT).

%% @doc Discover subscribers to a topic via DHT query.
-spec discover_subscribers(pid(), binary()) ->
    {ok, [#{node_id := binary(), endpoint := binary()}]} | {error, term()}.
discover_subscribers(Client, Topic) ->
    gen_server:call(Client, {discover_subscribers, Topic}, ?DEFAULT_TIMEOUT).

%% @doc Get the node ID of this peer.
-spec get_node_id(pid()) -> {ok, binary()} | {error, term()}.
get_node_id(Client) ->
    gen_server:call(Client, get_node_id, ?DEFAULT_TIMEOUT).

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

%% @doc Make an RPC call to a specific target node.
%%
%% Unlike `call/4' which discovers any provider via DHT, this function
%% sends the RPC directly to the specified target node.
-spec call_to(pid(), binary(), binary(), map() | list(), map()) ->
    {ok, term()} | {error, term()}.
call_to(Client, TargetNodeId, Procedure, Args, Opts) ->
    Timeout = maps:get(timeout, Opts, ?CALL_TIMEOUT),
    gen_server:call(Client, {call_to, TargetNodeId, Procedure, Args, Opts}, Timeout + 1000).

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
    {ok, SupPid} = macula_peer_system:start_link(Url, HandlerOpts),

    %% Look up child PIDs from supervisor
    Children = supervisor:which_children(SupPid),
    ConnMgrPid = find_child_pid(Children, connection_manager),
    PubSubPid = find_child_pid(Children, pubsub_handler),
    RpcPid = find_child_pid(Children, rpc_handler),
    AdvMgrPid = find_child_pid(Children, advertisement_manager),

    ?LOG_INFO("[Connection Facade] Supervision tree started - ConnMgr: ~p, PubSub: ~p, RPC: ~p, AdvMgr: ~p",
              [ConnMgrPid, PubSubPid, RpcPid, AdvMgrPid]),

    %% Wait for QUIC connection to be established before returning
    %% This prevents race conditions where subscribe is called before connection is ready
    wait_for_connection(ConnMgrPid, 10000),
    ?LOG_INFO("[Connection Facade] QUIC connection ready"),

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
%% NOTE: publish is now handled via handle_cast (fire-and-forget semantics)

%% Delegate to pubsub_handler
handle_call({subscribe, Topic, Callback}, _From, State) ->
    Result = macula_pubsub_handler:subscribe(State#state.pubsub_handler_pid, Topic, Callback),
    {reply, Result, State};

%% Delegate to pubsub_handler
handle_call({unsubscribe, SubRef}, _From, State) ->
    Result = macula_pubsub_handler:unsubscribe(State#state.pubsub_handler_pid, SubRef),
    {reply, Result, State};

%% Discover subscribers via DHT
%% NOTE: This queries the local routing server which only has local subscriptions.
%% For full DHT discovery, peers should query the gateway via RPC.
handle_call({discover_subscribers, Topic}, _From, State) ->
    TopicKey = crypto:hash(sha256, Topic),

    Result = case whereis(macula_routing_server) of
        undefined ->
            logger:warning("[~s] Routing server not running, cannot discover subscribers",
                          [State#state.node_id]),
            {error, routing_server_not_running};
        RoutingServerPid ->
            case macula_routing_server:find_value(RoutingServerPid, TopicKey, 20) of
                {ok, Subscribers} when is_list(Subscribers) ->
                    logger:debug("[~s] Found ~p subscriber(s) for topic ~s in local DHT",
                                [State#state.node_id, length(Subscribers), Topic]),
                    {ok, Subscribers};
                {error, not_found} ->
                    logger:debug("[~s] No subscribers found for topic ~s in local DHT",
                                [State#state.node_id, Topic]),
                    {ok, []};
                {error, Reason} ->
                    logger:warning("[~s] Failed to discover subscribers for ~s: ~p",
                                  [State#state.node_id, Topic, Reason]),
                    {error, Reason}
            end
    end,
    {reply, Result, State};

%% Get node ID
handle_call(get_node_id, _From, State) ->
    {reply, {ok, State#state.node_id}, State};

%% Delegate to rpc_handler
handle_call({call, Procedure, Args, Opts}, _From, State) ->
    Result = macula_rpc_handler:call(State#state.rpc_handler_pid, Procedure, Args, Opts),
    {reply, Result, State};

%% Delegate to rpc_handler (targeted call)
handle_call({call_to, TargetNodeId, Procedure, Args, Opts}, _From, State) ->
    Result = macula_rpc_handler:call_to(State#state.rpc_handler_pid, TargetNodeId, Procedure, Args, Opts),
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
%% Async publish - fire-and-forget semantics
%% Handle both {publish, ...} (from macula_peer:publish/4) and
%% {publish_async, ...} (from macula:publish/4 facade)
handle_cast({publish, Topic, Data, Opts}, State) ->
    do_publish(Topic, Data, Opts, State);
handle_cast({publish_async, Topic, Data, Opts}, State) ->
    do_publish(Topic, Data, Opts, State);

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
            macula_peer_system:stop(Pid);
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

%% @doc Execute publish operation - shared by both {publish, ...} and {publish_async, ...}
-spec do_publish(binary(), map() | binary(), map(), #state{}) -> {noreply, #state{}}.
do_publish(Topic, Data, Opts, State) ->
    ?LOG_INFO("[Peer] publish received: topic=~s, pubsub_pid=~p",
              [Topic, State#state.pubsub_handler_pid]),
    %% Delegate to pubsub_handler (which is also async)
    macula_pubsub_handler:publish(State#state.pubsub_handler_pid, Topic, Data, Opts),
    {noreply, State}.

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

%% @doc Wait for connection manager to reach connected status.
%% Polls the connection status at 100ms intervals until connected or timeout.
%% Returns ok even on timeout to not block callers - messages may be queued.
-spec wait_for_connection(pid(), pos_integer()) -> ok.
wait_for_connection(ConnMgrPid, Timeout) when Timeout > 0 ->
    wait_for_connection_status(is_process_alive(ConnMgrPid), ConnMgrPid, Timeout);
wait_for_connection(_ConnMgrPid, _Timeout) ->
    %% Timeout reached - continue anyway, messages will be queued or dropped
    ?LOG_WARNING("[Connection Facade] Connection wait timed out, continuing anyway"),
    ok.

%% @private Process not alive - sleep and retry
wait_for_connection_status(false, ConnMgrPid, Timeout) ->
    timer:sleep(100),
    wait_for_connection(ConnMgrPid, Timeout - 100);
%% @private Process alive - check status
wait_for_connection_status(true, ConnMgrPid, Timeout) ->
    handle_connection_status(macula_connection:get_status(ConnMgrPid), ConnMgrPid, Timeout).

%% @private Connected - done
handle_connection_status(connected, _ConnMgrPid, _Timeout) ->
    ok;
%% @private Not connected - sleep and retry
handle_connection_status(_Other, ConnMgrPid, Timeout) ->
    timer:sleep(100),
    wait_for_connection(ConnMgrPid, Timeout - 100).
