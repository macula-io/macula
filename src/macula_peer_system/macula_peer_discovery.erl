%%%-------------------------------------------------------------------
%%% @doc Peer Discovery - DHT-based gateway discovery and P2P mesh formation.
%%%
%%% This module implements automatic peer discovery to enable true P2P mesh:
%%% 1. Gateways register themselves in the bootstrap DHT
%%% 2. Peers periodically query DHT to discover other gateways
%%% 3. Peers establish direct QUIC connections to discovered gateways
%%% 4. Cross-peer relay works automatically via these mesh connections
%%%
%%% Architecture:
%%%   Peer1 Gateway - Peer2 Gateway - Peer3 Gateway
%%%        |                  |                  |
%%%   Local Clients     Local Clients     Local Clients
%%%
%%% DHT Storage:
%%%   Key pattern: "peer.gateway." + NodeID
%%%   Value: map with node_id, host, port, realm fields
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer_discovery).
-behaviour(gen_server).

%% API
-export([start_link/1, register_gateway/0, discover_peers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    node_id :: binary(),
    host :: binary(),
    port :: integer(),
    realm :: binary(),
    discovery_interval :: integer(),  % milliseconds
    timer_ref :: reference() | undefined,
    bootstrap_peer_pid :: pid() | undefined  % PID of bootstrap peer connection for DHT queries
}).

%%==============================================================================
%% API
%%==============================================================================

%% @doc Start the peer discovery process
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Register this gateway in the bootstrap DHT
-spec register_gateway() -> ok | {error, term()}.
register_gateway() ->
    gen_server:call(?MODULE, register_gateway).

%% @doc Discover other peer gateways from DHT
-spec discover_peers() -> {ok, [map()]} | {error, term()}.
discover_peers() ->
    gen_server:call(?MODULE, discover_peers).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

init(Config) ->
    NodeID = maps:get(node_id, Config),
    Host = maps:get(host, Config, <<"localhost">>),
    Port = maps:get(port, Config),
    Realm = maps:get(realm, Config, <<"default">>),
    DiscoveryInterval = maps:get(discovery_interval, Config, 30000),  % 30s default

    io:format("[PeerDiscovery] Initializing for ~s:~p~n", [Host, Port]),

    State = #state{
        node_id = NodeID,
        host = Host,
        port = Port,
        realm = Realm,
        discovery_interval = DiscoveryInterval
    },

    %% If we're a gateway (no MACULA_BOOTSTRAP_PEERS), register the _dht.list_gateways service
    case os:getenv("MACULA_BOOTSTRAP_PEERS") of
        false ->
            %% We're the gateway - register RPC service for listing gateways
            register_dht_list_gateways_service();
        _ ->
            ok  % We're a peer, no need to register
    end,

    %% Register ourselves in DHT after short delay (let bootstrap connect first)
    timer:send_after(3000, self(), register_self),

    %% Start periodic peer discovery
    TimerRef = erlang:send_after(5000, self(), discover_and_connect),

    {ok, State#state{timer_ref = TimerRef}}.

handle_call(register_gateway, _From, State) ->
    Result = do_register_gateway(State),
    {reply, Result, State};

handle_call(discover_peers, _From, State) ->
    Result = do_discover_peers(State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(register_self, State) ->
    case do_register_gateway(State) of
        ok ->
            io:format("[PeerDiscovery] Successfully registered gateway in DHT~n");
        {error, Reason} ->
            io:format("[PeerDiscovery] Failed to register gateway: ~p~n", [Reason])
    end,
    {noreply, State};

handle_info(discover_and_connect, State) ->
    #state{discovery_interval = Interval} = State,

    %% Discover peers from DHT
    case do_discover_peers(State) of
        {ok, Peers} when length(Peers) > 0 ->
            io:format("[PeerDiscovery] Discovered ~p peer(s)~n", [length(Peers)]),
            lists:foreach(fun(Peer) ->
                connect_to_peer(Peer, State)
            end, Peers);
        {ok, []} ->
            ok;  % No peers discovered yet
        {error, Reason} ->
            io:format("[PeerDiscovery] Discovery failed: ~p~n", [Reason])
    end,

    %% Schedule next discovery
    TimerRef = erlang:send_after(Interval, self(), discover_and_connect),
    {noreply, State#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer_ref = TimerRef}) ->
    case TimerRef of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

%% @private Register this gateway in the DHT
do_register_gateway(State) ->
    #state{node_id = NodeID, host = Host, port = Port, realm = Realm} = State,

    Key = <<"peer.gateway.", NodeID/binary>>,
    Value = #{
        node_id => NodeID,
        host => Host,
        port => Port,
        realm => Realm,
        registered_at => erlang:system_time(second)
    },

    %% Store in local routing server's DHT
    case whereis(macula_routing_server) of
        undefined ->
            {error, routing_server_not_found};
        RoutingServer ->
            macula_routing_server:store_local(RoutingServer, Key, Value),
            ok
    end.

%% @private Discover peer gateways from DHT
do_discover_peers(State) ->
    #state{node_id = MyNodeID} = State,

    %% Check if we have bootstrap peers configured
    %% If yes, we're a peer and should query gateway's DHT via RPC
    %% If no, we're the gateway and should query our local DHT
    case os:getenv("MACULA_BOOTSTRAP_PEERS") of
        false ->
            %% We're the gateway - query local DHT
            case whereis(macula_routing_server) of
                undefined ->
                    {error, routing_server_not_found};
                RoutingServer ->
                    AllPeers = discover_all_gateway_peers(RoutingServer),
                    OtherPeers = lists:filter(fun(#{node_id := NodeID}) ->
                        NodeID =/= MyNodeID
                    end, AllPeers),
                    {ok, OtherPeers}
            end;
        _BootstrapPeers ->
            %% We're a peer - query gateway's DHT via RPC through bootstrap peer connection
            discover_peers_via_gateway(State)
    end.

%% @private Discover all registered gateways
%% Note: This is a naive implementation - we should use DHT prefix queries
discover_all_gateway_peers(RoutingServer) ->
    %% For now, we'll rely on the fact that keys are stored locally
    %% In a real implementation, we'd iterate through the DHT keyspace
    case macula_routing_server:get_all_keys(RoutingServer) of
        {ok, Keys} ->
            GatewayKeys = lists:filter(fun(Key) ->
                case Key of
                    <<"peer.gateway.", _/binary>> -> true;
                    _ -> false
                end
            end, Keys),

            lists:filtermap(fun(Key) ->
                case macula_routing_server:get_local(RoutingServer, Key) of
                    {ok, [Value|_]} -> {true, Value};  % Extract first element from list
                    {ok, []} -> false;  % Empty list
                    _ -> false
                end
            end, GatewayKeys);
        _ ->
            []
    end.

%% @private Connect to a discovered peer gateway
connect_to_peer(PeerInfo, State) ->
    #{node_id := PeerNodeID, host := Host, port := Port, realm := Realm} = PeerInfo,
    #state{node_id = MyNodeID} = State,

    %% Don't connect to ourselves
    case PeerNodeID of
        MyNodeID ->
            ok;
        _ ->
            %% Build peer URL
            PeerUrl = iolist_to_binary([<<"https://">>, Host, <<":">>, integer_to_binary(Port)]),

            %% Check if already connected
            case is_already_connected(PeerNodeID) of
                true ->
                    ok;  % Already connected
                false ->
                    io:format("[PeerDiscovery] Connecting to peer ~s at ~s~n",
                             [binary:encode_hex(PeerNodeID), PeerUrl]),

                    %% Start peer connection
                    case macula_peers_sup:start_peer(PeerUrl, #{realm => Realm}) of
                        {ok, _PeerPid} ->
                            io:format("[PeerDiscovery] Connected to peer ~s~n",
                                     [binary:encode_hex(PeerNodeID)]);
                        {error, {already_started, _}} ->
                            ok;  % Already connected
                        {error, Reason} ->
                            io:format("[PeerDiscovery] Failed to connect to ~s: ~p~n",
                                     [binary:encode_hex(PeerNodeID), Reason])
                    end
            end
    end.

%% @private Check if already connected to a peer
is_already_connected(_PeerNodeID) ->
    %% Check if we have an active peer connection to this node
    case whereis(macula_peers_sup) of
        undefined ->
            false;
        _ ->
            %% Query peer supervisor for active connections
            %% This is a simplified check - in production we'd have a proper registry
            Children = supervisor:which_children(macula_peers_sup),
            lists:any(fun({_Id, Pid, _Type, _Modules}) ->
                case Pid of
                    undefined -> false;
                    _ ->
                        %% Check if this peer process is for our target node
                        %% This is simplified - we'd need to query the peer for its node_id
                        false  % For now, allow reconnections
                end
            end, Children)
    end.

%% @private Discover peers by querying gateway's DHT via RPC
discover_peers_via_gateway(State) ->
    #state{node_id = MyNodeID} = State,

    %% Find the bootstrap peer connection (first child of macula_peers_sup)
    case whereis(macula_peers_sup) of
        undefined ->
            io:format("[PeerDiscovery] macula_peers_sup not found~n"),
            {error, peers_sup_not_found};
        _ ->
            Children = supervisor:which_children(macula_peers_sup),
            case Children of
                [] ->
                    io:format("[PeerDiscovery] No bootstrap peer connection yet~n"),
                    {ok, []};  % No bootstrap peer connected yet
                [{_Id, BootstrapSupPid, _Type, _Modules} | _] when is_pid(BootstrapSupPid) ->
                    %% Get the rpc_handler child from the peer system supervisor
                    case get_rpc_handler_pid(BootstrapSupPid) of
                        {ok, RpcHandlerPid} ->
                            %% Make RPC call to gateway asking for list of registered gateways
                            io:format("[PeerDiscovery] Querying gateway DHT via RPC...~n"),
                            case macula_rpc_handler:call(RpcHandlerPid, <<"_dht.list_gateways">>, #{}, #{}) of
                                {ok, #{<<"peers">> := PeersList}} ->
                                    io:format("[PeerDiscovery] Gateway returned ~p peer(s) from DHT~n",
                                             [length(PeersList)]),
                                    %% Filter out ourselves
                                    OtherPeers = lists:filter(fun(#{<<"node_id">> := NodeID}) ->
                                        NodeID =/= MyNodeID
                                    end, PeersList),
                                    {ok, OtherPeers};
                                {error, Reason} ->
                                    io:format("[PeerDiscovery] RPC to gateway failed: ~p~n", [Reason]),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            io:format("[PeerDiscovery] Failed to get RPC handler: ~p~n", [Reason]),
                            {error, Reason}
                    end;
                _ ->
                    io:format("[PeerDiscovery] Bootstrap peer not ready~n"),
                    {ok, []}
            end
    end.

%% @private Get the RPC handler PID from a peer system supervisor
get_rpc_handler_pid(PeerSupPid) when is_pid(PeerSupPid) ->
    Children = supervisor:which_children(PeerSupPid),
    case lists:keyfind(rpc_handler, 1, Children) of
        {rpc_handler, RpcHandlerPid, _Type, _Modules} when is_pid(RpcHandlerPid) ->
            {ok, RpcHandlerPid};
        _ ->
            {error, rpc_handler_not_found}
    end.

%% @private Register the _dht.list_gateways RPC service on the gateway
%%
%% This service queries the local DHT for all registered gateways and returns them.
%% Only called when we're the gateway (no MACULA_BOOTSTRAP_PEERS).
register_dht_list_gateways_service() ->
    io:format("[PeerDiscovery] Registering _dht.list_gateways RPC service~n"),

    %% Find our own RPC handler PID
    case whereis(macula_peers_sup) of
        undefined ->
            io:format("[PeerDiscovery] ERROR: macula_peers_sup not found, cannot register service~n"),
            {error, peers_sup_not_found};
        _ ->
            %% We're the gateway, so we're the first child of macula_peers_sup
            Children = supervisor:which_children(macula_peers_sup),
            case Children of
                [{_Id, PeerSupPid, _Type, _Modules} | _] when is_pid(PeerSupPid) ->
                    case get_rpc_handler_pid(PeerSupPid) of
                        {ok, RpcHandlerPid} ->
                            %% Create handler function that queries DHT
                            Handler = fun(_Args) ->
                                case whereis(macula_routing_server) of
                                    undefined ->
                                        {error, routing_server_not_found};
                                    RoutingServer ->
                                        %% Query all keys from DHT
                                        case macula_routing_server:get_all_keys(RoutingServer) of
                                            {ok, Keys} ->
                                                %% Filter for gateway keys
                                                GatewayKeys = lists:filter(fun(Key) ->
                                                    case Key of
                                                        <<"peer.gateway.", _/binary>> -> true;
                                                        _ -> false
                                                    end
                                                end, Keys),

                                                %% Get gateway info for each key
                                                Peers = lists:filtermap(fun(Key) ->
                                                    case macula_routing_server:get_local(RoutingServer, Key) of
                                                        {ok, [Value|_]} ->
                                                            %% Convert keys to binary for peer discovery
                                                            PeerMap = #{
                                                                <<"node_id">> => maps:get(node_id, Value),
                                                                <<"host">> => maps:get(host, Value),
                                                                <<"port">> => maps:get(port, Value),
                                                                <<"realm">> => maps:get(realm, Value)
                                                            },
                                                            {true, PeerMap};
                                                        _ ->
                                                            false
                                                    end
                                                end, GatewayKeys),

                                                io:format("[PeerDiscovery] _dht.list_gateways returning ~p gateway(s)~n",
                                                         [length(Peers)]),
                                                {ok, #{<<"peers">> => Peers}};
                                            {error, Reason} ->
                                                {error, Reason}
                                        end
                                end
                            end,

                            %% Register the handler
                            macula_rpc_handler:register_local_procedure(RpcHandlerPid, <<"_dht.list_gateways">>, Handler),
                            io:format("[PeerDiscovery] Successfully registered _dht.list_gateways~n"),
                            ok;
                        {error, Reason} ->
                            io:format("[PeerDiscovery] ERROR: Failed to get RPC handler: ~p~n", [Reason]),
                            {error, Reason}
                    end;
                _ ->
                    io:format("[PeerDiscovery] ERROR: No peer system supervisor found~n"),
                    {error, no_peer_system}
            end
    end.
