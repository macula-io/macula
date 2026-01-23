%%%-------------------------------------------------------------------
%%% @doc Macula Cluster Strategy for libcluster.
%%%
%%% This module implements a cluster formation strategy using Macula's
%%% decentralized discovery (DHT/mDNS) instead of EPMD. It can be used
%%% with libcluster in Elixir or standalone in Erlang.
%%%
%%% Integration: Works with libcluster (Elixir) or standalone (Erlang).
%%%
%%% Discovery Modes:
%%%   mdns - Local network discovery via mDNS (no bootstrap needed)
%%%   dht - Internet-scale discovery via Macula DHT
%%%   both - Try mDNS first, fall back to DHT
%%%
%%% Configuration: topology, realm, discovery_type
%%%
%%% @copyright 2025 Macula.io Apache-2.0
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_strategy).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    start_link/2,
    stop/1,
    list_connected/1
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

-define(POLL_INTERVAL, 5000).  % 5 seconds
-define(CONNECT_TIMEOUT, 5000).  % 5 seconds

-record(state, {
    %% Topology name (for libcluster compatibility)
    topology :: atom(),

    %% Configuration
    config :: map(),

    %% Connected nodes
    connected :: #{atom() => boolean()},

    %% Discovery subscription reference
    discovery_ref :: reference() | undefined,

    %% Poll timer
    poll_timer :: reference() | undefined,

    %% Callback module (for libcluster integration)
    callback_module :: module() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the cluster strategy with options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    Topology = maps:get(topology, Opts, macula_cluster),
    start_link(Topology, Opts).

%% @doc Start the cluster strategy with name and options.
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, Opts#{topology => Name}, []).

%% @doc Stop the cluster strategy.
-spec stop(atom() | pid()) -> ok.
stop(NameOrPid) ->
    gen_server:stop(NameOrPid).

%% @doc List currently connected nodes.
-spec list_connected(atom() | pid()) -> [atom()].
list_connected(NameOrPid) ->
    gen_server:call(NameOrPid, list_connected).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Opts) ->
    process_flag(trap_exit, true),

    Topology = maps:get(topology, Opts, macula_cluster),
    Config = maps:get(config, Opts, #{}),
    CallbackMod = maps:get(callback_module, Opts, undefined),

    %% Subscribe to node discovery events
    ok = macula_dist_discovery:subscribe(self()),

    %% Subscribe to Erlang node monitoring
    ok = net_kernel:monitor_nodes(true, [{node_type, all}]),

    %% Start polling for existing nodes
    PollTimer = erlang:send_after(?POLL_INTERVAL, self(), poll_nodes),

    State = #state{
        topology = Topology,
        config = Config,
        connected = #{},
        poll_timer = PollTimer,
        callback_module = CallbackMod
    },

    %% Log startup
    error_logger:info_msg(
        "macula_cluster_strategy: started for topology ~p~n",
        [Topology]
    ),

    {ok, State}.

%% @private
handle_call(list_connected, _From, State) ->
    Connected = maps:keys(maps:filter(fun(_, V) -> V end, State#state.connected)),
    {reply, Connected, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% Handle node discovered via Macula discovery
handle_info({node_discovered, _NodeName, IP, Port}, State) ->
    Node = make_node_name(Port, IP),
    NewState = maybe_connect_node(Node, State),
    {noreply, NewState};

%% Handle node lost via Macula discovery
handle_info({node_lost, NodeName}, State) ->
    Node = ensure_atom(NodeName),
    NewState = maybe_disconnect_node(Node, State),
    {noreply, NewState};

%% Handle Erlang node up event
handle_info({nodeup, Node, _Info}, State) ->
    error_logger:info_msg(
        "macula_cluster_strategy: node ~p joined cluster~n",
        [Node]
    ),
    Connected = maps:put(Node, true, State#state.connected),
    notify_callback(State, {nodeup, Node}),
    {noreply, State#state{connected = Connected}};

%% Handle Erlang node down event
handle_info({nodedown, Node, _Info}, State) ->
    error_logger:info_msg(
        "macula_cluster_strategy: node ~p left cluster~n",
        [Node]
    ),
    Connected = maps:put(Node, false, State#state.connected),
    notify_callback(State, {nodedown, Node}),
    {noreply, State#state{connected = Connected}};

%% Periodic polling for nodes
handle_info(poll_nodes, State) ->
    %% Query discovery for known nodes
    NewState = poll_discovered_nodes(State),

    %% Reschedule
    Timer = erlang:send_after(?POLL_INTERVAL, self(), poll_nodes),
    {noreply, NewState#state{poll_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Unsubscribe from discovery
    catch macula_dist_discovery:unsubscribe(self()),

    %% Stop node monitoring
    catch net_kernel:monitor_nodes(false),

    %% Cancel timer
    cancel_timer(State#state.poll_timer),

    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Try to connect to a discovered node
maybe_connect_node(Node, State) when Node =:= node() ->
    %% Don't connect to ourselves
    State;
maybe_connect_node(Node, State) ->
    case maps:get(Node, State#state.connected, false) of
        true ->
            %% Already connected
            State;
        false ->
            %% Try to connect
            error_logger:info_msg(
                "macula_cluster_strategy: attempting to connect to ~p~n",
                [Node]
            ),
            case net_kernel:connect_node(Node) of
                true ->
                    error_logger:info_msg(
                        "macula_cluster_strategy: connected to ~p~n",
                        [Node]
                    ),
                    Connected = maps:put(Node, true, State#state.connected),
                    notify_callback(State, {connected, Node}),
                    State#state{connected = Connected};
                false ->
                    error_logger:warning_msg(
                        "macula_cluster_strategy: failed to connect to ~p~n",
                        [Node]
                    ),
                    State;
                ignored ->
                    %% net_kernel not running
                    State
            end
    end.

%% @private Disconnect from a node that's no longer in discovery
maybe_disconnect_node(Node, State) ->
    case maps:get(Node, State#state.connected, false) of
        true ->
            error_logger:info_msg(
                "macula_cluster_strategy: disconnecting from ~p (no longer in discovery)~n",
                [Node]
            ),
            erlang:disconnect_node(Node),
            Connected = maps:put(Node, false, State#state.connected),
            notify_callback(State, {disconnected, Node}),
            State#state{connected = Connected};
        false ->
            State
    end.

%% @private Poll discovered nodes
poll_discovered_nodes(State) ->
    case macula_dist_discovery:list_nodes() of
        Nodes when is_list(Nodes) ->
            lists:foldl(
                fun(NodeName, AccState) ->
                    case macula_dist_discovery:lookup_node(NodeName) of
                        {ok, #{ip := IP, port := Port}} ->
                            Node = make_node_name(Port, IP),
                            maybe_connect_node(Node, AccState);
                        {error, _} ->
                            AccState
                    end
                end,
                State,
                Nodes
            );
        _ ->
            State
    end.

%% @private Make node name from port and IP
%% Format: port@ip (e.g., '4433@192.168.1.100')
make_node_name(Port, IP) when is_tuple(IP) ->
    make_node_name(Port, inet:ntoa(IP));
make_node_name(Port, IP) when is_list(IP) ->
    list_to_atom(integer_to_list(Port) ++ "@" ++ IP);
make_node_name(Port, IP) when is_binary(IP) ->
    make_node_name(Port, binary_to_list(IP)).

%% @private Ensure value is an atom
ensure_atom(Value) when is_atom(Value) -> Value;
ensure_atom(Value) when is_list(Value) -> list_to_atom(Value);
ensure_atom(Value) when is_binary(Value) -> binary_to_atom(Value, utf8).

%% @private Notify callback module (for libcluster integration)
notify_callback(#state{callback_module = undefined}, _Event) ->
    ok;
notify_callback(#state{callback_module = Mod, topology = Topology}, Event) ->
    _ = catch Mod:handle_event(Topology, Event),
    ok.

%% @private Cancel timer if defined
cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).
