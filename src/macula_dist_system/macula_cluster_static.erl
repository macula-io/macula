%%%-------------------------------------------------------------------
%%% @doc Macula Static Cluster Strategy.
%%%
%%% A simple cluster formation strategy that connects to a predefined
%%% list of nodes. Equivalent to libcluster's Cluster.Strategy.Epmd.
%%%
%%% == Configuration ==
%%%
%%% Start with a list of nodes to connect to:
%%%
%%% ```
%%% {ok, _Pid} = macula_cluster_static:start_link(#{
%%%     nodes => ['node1@host1', 'node2@host2', 'node3@host3'],
%%%     reconnect_interval => 5000  %% ms, default 5000
%%% }).
%%% '''
%%%
%%% Or from environment variables:
%%%
%%% ```
%%% %% CLUSTER_NODES=node1@host1,node2@host2,node3@host3
%%% {ok, _Pid} = macula_cluster_static:start_link(#{}).
%%% '''
%%%
%%% == Behavior ==
%%%
%%% - Attempts to connect to all configured nodes on startup
%%% - Monitors connected nodes for disconnect events
%%% - Automatically reconnects to disconnected nodes
%%% - Ignores self-connection attempts
%%% - Logs connection/disconnection events
%%%
%%% == Callbacks ==
%%%
%%% Register a callback module to receive cluster events:
%%%
%%% ```
%%% {ok, _Pid} = macula_cluster_static:start_link(#{
%%%     nodes => [...],
%%%     callback => self()  %% PID or {Module, Function}
%%% }).
%%% %% Receives: {macula_cluster, nodeup, Node}
%%% %% Receives: {macula_cluster, nodedown, Node}
%%% '''
%%%
%%% @copyright 2026 Macula.io Apache-2.0
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_static).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0,
    stop/1,
    get_nodes/0,
    get_nodes/1,
    get_connected/0,
    get_connected/1,
    add_node/1,
    add_node/2,
    remove_node/1,
    remove_node/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_RECONNECT_INTERVAL, 5000).

-record(state, {
    %% Configured nodes to connect to
    nodes :: [atom()],

    %% Currently connected nodes
    connected :: sets:set(atom()),

    %% Reconnect interval in milliseconds
    reconnect_interval :: pos_integer(),

    %% Reconnect timer reference
    reconnect_timer :: reference() | undefined,

    %% Callback for cluster events (pid or {M,F})
    callback :: pid() | {module(), atom()} | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the static cluster strategy with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the static cluster strategy with options.
%%
%% Options:
%% - nodes: List of node atoms to connect to
%% - reconnect_interval: Milliseconds between reconnect attempts (default 5000)
%% - callback: PID or {Module, Function} to receive cluster events
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Stop the static cluster strategy.
-spec stop() -> ok.
stop() ->
    stop(?SERVER).

%% @doc Stop a named static cluster strategy.
-spec stop(atom() | pid()) -> ok.
stop(NameOrPid) ->
    gen_server:stop(NameOrPid).

%% @doc Get the list of configured nodes.
-spec get_nodes() -> [atom()].
get_nodes() ->
    get_nodes(?SERVER).

%% @doc Get the list of configured nodes from a named instance.
-spec get_nodes(atom() | pid()) -> [atom()].
get_nodes(NameOrPid) ->
    gen_server:call(NameOrPid, get_nodes).

%% @doc Get the list of currently connected nodes.
-spec get_connected() -> [atom()].
get_connected() ->
    get_connected(?SERVER).

%% @doc Get the list of connected nodes from a named instance.
-spec get_connected(atom() | pid()) -> [atom()].
get_connected(NameOrPid) ->
    gen_server:call(NameOrPid, get_connected).

%% @doc Add a node to the cluster configuration.
-spec add_node(atom()) -> ok.
add_node(Node) ->
    add_node(?SERVER, Node).

%% @doc Add a node to a named instance.
-spec add_node(atom() | pid(), atom()) -> ok.
add_node(NameOrPid, Node) ->
    gen_server:call(NameOrPid, {add_node, Node}).

%% @doc Remove a node from the cluster configuration.
-spec remove_node(atom()) -> ok.
remove_node(Node) ->
    remove_node(?SERVER, Node).

%% @doc Remove a node from a named instance.
-spec remove_node(atom() | pid(), atom()) -> ok.
remove_node(NameOrPid, Node) ->
    gen_server:call(NameOrPid, {remove_node, Node}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Opts) ->
    process_flag(trap_exit, true),

    %% Get nodes from options or environment
    Nodes = resolve_nodes(Opts),
    ReconnectInterval = maps:get(reconnect_interval, Opts, ?DEFAULT_RECONNECT_INTERVAL),
    Callback = maps:get(callback, Opts, undefined),

    %% Subscribe to node events
    ok = net_kernel:monitor_nodes(true),

    State = #state{
        nodes = Nodes,
        connected = sets:new([{version, 2}]),
        reconnect_interval = ReconnectInterval,
        callback = Callback
    },

    ?LOG_INFO("[macula_cluster_static] Started with ~p configured node(s)",
              [length(Nodes)]),

    %% Attempt initial connections
    self() ! connect_all,

    {ok, State}.

%% @private
handle_call(get_nodes, _From, State) ->
    {reply, State#state.nodes, State};

handle_call(get_connected, _From, State) ->
    {reply, sets:to_list(State#state.connected), State};

handle_call({add_node, Node}, _From, State) ->
    NewNodes = lists:usort([Node | State#state.nodes]),
    %% Trigger connection attempt
    self() ! {connect_node, Node},
    {reply, ok, State#state{nodes = NewNodes}};

handle_call({remove_node, Node}, _From, State) ->
    NewNodes = lists:delete(Node, State#state.nodes),
    %% Disconnect if connected
    case sets:is_element(Node, State#state.connected) of
        true ->
            erlang:disconnect_node(Node);
        false ->
            ok
    end,
    NewConnected = sets:del_element(Node, State#state.connected),
    {reply, ok, State#state{nodes = NewNodes, connected = NewConnected}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% Initial connection attempt
handle_info(connect_all, State) ->
    NewState = connect_to_all_nodes(State),
    %% Schedule reconnection timer
    Timer = schedule_reconnect(State#state.reconnect_interval),
    {noreply, NewState#state{reconnect_timer = Timer}};

%% Connect to a specific node
handle_info({connect_node, Node}, State) ->
    NewState = try_connect(Node, State),
    {noreply, NewState};

%% Periodic reconnection
handle_info(reconnect, State) ->
    NewState = reconnect_disconnected(State),
    Timer = schedule_reconnect(State#state.reconnect_interval),
    {noreply, NewState#state{reconnect_timer = Timer}};

%% Node joined the cluster
handle_info({nodeup, Node}, State) ->
    case lists:member(Node, State#state.nodes) of
        true ->
            ?LOG_INFO("[macula_cluster_static] Node ~p connected", [Node]),
            NewConnected = sets:add_element(Node, State#state.connected),
            notify_callback(State#state.callback, nodeup, Node),
            {noreply, State#state{connected = NewConnected}};
        false ->
            %% Not a node we manage
            {noreply, State}
    end;

%% Node left the cluster
handle_info({nodedown, Node}, State) ->
    case lists:member(Node, State#state.nodes) of
        true ->
            ?LOG_WARNING("[macula_cluster_static] Node ~p disconnected", [Node]),
            NewConnected = sets:del_element(Node, State#state.connected),
            notify_callback(State#state.callback, nodedown, Node),
            {noreply, State#state{connected = NewConnected}};
        false ->
            %% Not a node we manage
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Cancel reconnect timer
    cancel_timer(State#state.reconnect_timer),
    %% Unsubscribe from node events
    catch net_kernel:monitor_nodes(false),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Resolve nodes from options or environment
-spec resolve_nodes(map()) -> [atom()].
resolve_nodes(Opts) ->
    case maps:get(nodes, Opts, undefined) of
        undefined ->
            parse_env_nodes();
        Nodes when is_list(Nodes) ->
            [ensure_atom(N) || N <- Nodes]
    end.

%% @private Parse nodes from CLUSTER_NODES environment variable
-spec parse_env_nodes() -> [atom()].
parse_env_nodes() ->
    case os:getenv("CLUSTER_NODES") of
        false ->
            [];
        "" ->
            [];
        NodesStr ->
            Nodes = string:tokens(NodesStr, ","),
            [ensure_atom(string:trim(N)) || N <- Nodes]
    end.

%% @private Connect to all configured nodes
-spec connect_to_all_nodes(#state{}) -> #state{}.
connect_to_all_nodes(State) ->
    lists:foldl(
        fun(Node, AccState) ->
            try_connect(Node, AccState)
        end,
        State,
        State#state.nodes
    ).

%% @private Reconnect to disconnected nodes
-spec reconnect_disconnected(#state{}) -> #state{}.
reconnect_disconnected(State) ->
    Disconnected = [N || N <- State#state.nodes,
                         not sets:is_element(N, State#state.connected)],
    lists:foldl(
        fun(Node, AccState) ->
            try_connect(Node, AccState)
        end,
        State,
        Disconnected
    ).

%% @private Try to connect to a single node
-spec try_connect(atom(), #state{}) -> #state{}.
try_connect(Node, State) when Node =:= node() ->
    %% Don't connect to ourselves
    State;
try_connect(Node, State) ->
    case sets:is_element(Node, State#state.connected) of
        true ->
            %% Already connected
            State;
        false ->
            ?LOG_DEBUG("[macula_cluster_static] Attempting connection to ~p", [Node]),
            case net_kernel:connect_node(Node) of
                true ->
                    ?LOG_INFO("[macula_cluster_static] Connected to ~p", [Node]),
                    NewConnected = sets:add_element(Node, State#state.connected),
                    notify_callback(State#state.callback, nodeup, Node),
                    State#state{connected = NewConnected};
                false ->
                    ?LOG_DEBUG("[macula_cluster_static] Failed to connect to ~p", [Node]),
                    State;
                ignored ->
                    %% net_kernel not running
                    ?LOG_WARNING("[macula_cluster_static] net_kernel not running, "
                                 "cannot connect to ~p", [Node]),
                    State
            end
    end.

%% @private Schedule reconnection timer
-spec schedule_reconnect(pos_integer()) -> reference().
schedule_reconnect(Interval) ->
    erlang:send_after(Interval, self(), reconnect).

%% @private Cancel timer if defined
-spec cancel_timer(reference() | undefined) -> ok.
cancel_timer(undefined) -> ok;
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    ok.

%% @private Ensure value is an atom
-spec ensure_atom(atom() | string() | binary()) -> atom().
ensure_atom(Value) when is_atom(Value) -> Value;
ensure_atom(Value) when is_list(Value) -> list_to_atom(Value);
ensure_atom(Value) when is_binary(Value) -> binary_to_atom(Value, utf8).

%% @private Notify callback of cluster event
-spec notify_callback(pid() | {module(), atom()} | undefined,
                      nodeup | nodedown, atom()) -> ok.
notify_callback(undefined, _Event, _Node) ->
    ok;
notify_callback(Pid, Event, Node) when is_pid(Pid) ->
    Pid ! {macula_cluster, Event, Node},
    ok;
notify_callback({Module, Function}, Event, Node) ->
    _ = catch Module:Function(Event, Node),
    ok.
