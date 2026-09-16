%%%-------------------------------------------------------------------
%%% @doc Cluster management utilities for Macula platform.
%%%
%%% This module provides cluster infrastructure functions that other
%%% applications (like bc_gitops) can delegate to when running on
%%% the Macula platform.
%%%
%%% == Distribution ==
%%%
%%% The `ensure_distributed/0' function ensures the node is running
%%% in distributed mode. If not already distributed, it starts
%%% distribution with a generated node name.
%%%
%%% == Cookie Management ==
%%%
%%% macula sets no distribution cookie, and reads or writes no cookie file. A
%%% node's cookie is its release's own configuration: `-setcookie', or the
%%% owner-only `.erlang.cookie' that OTP's auth reads in the node's HOME and
%%% creates there when it is missing. `get_cookie/0' returns a distributed
%%% node's cookie, and `set_cookie/1' changes it for the running node only.
%%%
%%% == Node Monitoring ==
%%%
%%% The `monitor_nodes/0' and `unmonitor_nodes/0' functions wrap
%%% `net_kernel:monitor_nodes/1' for subscribing to nodeup/nodedown
%%% messages.
%%%
%%% == bc_gitops Integration ==
%%%
%%% When bc_gitops is running on the Macula platform, it detects
%%% these exports and delegates clustering operations here. This
%%% allows Macula to own cluster infrastructure while bc_gitops
%%% remains usable standalone.
%%%
%%% @copyright 2026 Macula.io Apache-2.0
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster).

-include_lib("kernel/include/logger.hrl").

%% API - Distribution
-export([
    ensure_distributed/0,
    is_distributed/0
]).

%% API - Cookie Management
-export([
    get_cookie/0,
    set_cookie/1
]).

-deprecated([{get_cookie, 0, "call erlang:get_cookie/0 instead; removed in 11.0.0"},
             {set_cookie, 1, "call erlang:set_cookie/1 instead; removed in 11.0.0"}]).

%% API - Node Monitoring
-export([
    monitor_nodes/0,
    unmonitor_nodes/0
]).

%% API - Utilities
-export([
    get_hostname/0
]).

%% API - Auto-Clustering
-export([
    start_cluster/0,
    start_cluster/1,
    stop_cluster/0,
    nodes/0,
    is_clustered/0
]).

%%%===================================================================
%%% API - Distribution
%%%===================================================================

%% @doc Ensure this node is running in distributed mode.
%%
%% If the node is already distributed, returns `ok' immediately.
%% Otherwise, starts distribution with a generated node name in the
%% format `macula_host@hostname'.
%%
%% Examples:
%% ```
%% ok = macula_cluster:ensure_distributed().
%% '''
-spec ensure_distributed() -> ok | {error, term()}.
ensure_distributed() ->
    case is_distributed() of
        true ->
            ok;
        false ->
            start_distribution()
    end.

%% @doc Check if this node is running in distributed mode.
-spec is_distributed() -> boolean().
is_distributed() ->
    node() =/= nonode@nohost.

%%%===================================================================
%%% API - Cookie Management
%%%===================================================================

%% @doc The cookie of this node, which must be distributed.
%%
%% Returns what `erlang:get_cookie/0' returns, and raises `not_distributed' on
%% a node that is not distributed. macula sets no cookie and reads no cookie
%% file: a distributed node's cookie is its release's own configuration,
%% `-setcookie' or the owner-only `.erlang.cookie' that OTP's auth reads in
%% the node's HOME. Deprecated: call `erlang:get_cookie/0'. Removed in 11.0.0.
%%
%% Examples:
%% ```
%% Cookie = macula_cluster:get_cookie().
%% '''
-spec get_cookie() -> atom().
get_cookie() ->
    own_cookie(erlang:is_alive()).

%% @doc Set the cookie of this node, which must be distributed.
%%
%% Only the running node's cookie changes, and `not_distributed' is raised on
%% a node that is not distributed. No file is written, so a node that starts
%% again has its release's cookie. Deprecated: call `erlang:set_cookie/1'.
%% Removed in 11.0.0.
%%
%% Examples:
%% ```
%% ok = macula_cluster:set_cookie(my_secret_cookie).
%% ok = macula_cluster:set_cookie(&lt;&lt;"my_secret_cookie"&gt;&gt;).
%% '''
-spec set_cookie(atom() | binary()) -> ok.
set_cookie(Cookie) when is_binary(Cookie) ->
    set_cookie(binary_to_atom(Cookie, utf8));
set_cookie(Cookie) when is_atom(Cookie) ->
    node_cookie_set(erlang:is_alive(), Cookie).

%%%===================================================================
%%% API - Node Monitoring
%%%===================================================================

%% @doc Subscribe to node up/down events.
%%
%% After calling this function, the calling process will receive
%% `{nodeup, Node}' and `{nodedown, Node}' messages when nodes
%% join or leave the cluster.
%%
%% Examples:
%% ```
%% ok = macula_cluster:monitor_nodes().
%% receive
%%     {nodeup, Node} -> io:format("Node joined: ~p~n", [Node]);
%%     {nodedown, Node} -> io:format("Node left: ~p~n", [Node])
%% end.
%% '''
-spec monitor_nodes() -> ok.
monitor_nodes() ->
    ok = net_kernel:monitor_nodes(true),
    ok.

%% @doc Unsubscribe from node up/down events.
%%
%% Stops the calling process from receiving nodeup/nodedown messages.
-spec unmonitor_nodes() -> ok.
unmonitor_nodes() ->
    ok = net_kernel:monitor_nodes(false),
    ok.

%%%===================================================================
%%% API - Utilities
%%%===================================================================

%% @doc Get the short hostname of this machine.
%%
%% Examples:
%% ```
%% "myhost" = macula_cluster:get_hostname().
%% '''
-spec get_hostname() -> string().
get_hostname() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.

%%%===================================================================
%%% Internal Functions - Distribution
%%%===================================================================

%% Distribution starts without a cookie from macula: OTP's auth gives the node
%% its release's cookie, `-setcookie' or the owner-only `.erlang.cookie' in
%% the node's HOME, and creates that file when it is missing.
-spec start_distribution() -> ok | {error, term()}.
start_distribution() ->
    Hostname = get_hostname(),
    NodeName = list_to_atom("macula_host@" ++ Hostname),

    case net_kernel:start([NodeName, shortnames]) of
        {ok, _Pid} ->
            ?LOG_INFO("[macula_cluster] Started distribution as ~p", [NodeName]),
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("[macula_cluster] Failed to start distribution: ~p", [Reason]),
            {error, {distribution_failed, Reason}}
    end.

%%%===================================================================
%%% Internal Functions - The Node's Cookie
%%%===================================================================

own_cookie(true) -> erlang:get_cookie();
own_cookie(false) -> erlang:error(not_distributed).

node_cookie_set(true, Cookie) ->
    true = erlang:set_cookie(node(), Cookie),
    ok;
node_cookie_set(false, _Cookie) ->
    erlang:error(not_distributed).

%%%===================================================================
%%% API - Auto-Clustering
%%%===================================================================

%% @doc Start automatic cluster formation with default options.
%%
%% Uses the static strategy by default, reading nodes from:
%% 1. Application env: `{macula, [{cluster_nodes, [Node1, Node2, ...]}]}'
%% 2. Environment variable: `CLUSTER_NODES' (comma-separated)
%%
%% If no nodes are configured, starts the gossip strategy.
%%
%% Examples:
%% ```
%% %% With CLUSTER_NODES env var set
%% ok = macula_cluster:start_cluster().
%%
%% %% Or configure in sys.config
%% {macula, [{cluster_nodes, ['node1@host1', 'node2@host2']}]}
%% '''
-spec start_cluster() -> ok | {error, term()}.
start_cluster() ->
    start_cluster(#{}).

%% @doc Start automatic cluster formation with options.
%%
%% Options:
%% - strategy: `auto' (default), `gossip' or `static'. Any other value
%%   returns `{error, {unknown_strategy, Strategy}}' without starting
%%   distribution.
%% - nodes: List of node atoms (for static strategy)
%% - reconnect_interval: Milliseconds between reconnect attempts (default 5000)
%% - callback: PID or {Module, Function} to receive cluster events
%%
%% Gossip options (for gossip strategy):
%% - multicast_addr: IPv4 multicast address (default {230, 1, 1, 251})
%% - port: UDP port (default 45892)
%% - broadcast_interval: Milliseconds between broadcasts (default 1500)
%% - multicast_ttl: TTL for multicast packets (default 1 = same subnet)
%% - secret: Optional binary secret for HMAC authentication
%%
%% Strategy selection:
%% - `gossip': UDP multicast gossip for zero-config LAN (like libcluster Gossip)
%% - `static': Uses a known list of nodes (like libcluster Epmd strategy)
%% - `auto': Chooses strategy based on configuration
%%
%% Examples:
%% ```
%% %% Gossip strategy for zero-config LAN discovery (recommended)
%% ok = macula_cluster:start_cluster(#{
%%     strategy => gossip
%% }).
%%
%% %% Gossip with custom multicast group
%% ok = macula_cluster:start_cluster(#{
%%     strategy => gossip,
%%     multicast_addr => {239, 1, 1, 1},
%%     port => 9999,
%%     secret => &lt;&lt;"my-cluster-secret"&gt;&gt;
%% }).
%%
%% %% Static strategy with explicit nodes
%% ok = macula_cluster:start_cluster(#{
%%     strategy => static,
%%     nodes => ['node1@host1', 'node2@host2']
%% }).
%% '''
-spec start_cluster(map()) -> ok | {error, term()}.
start_cluster(Opts) ->
    start_known_strategy(resolve_strategy(Opts), Opts).

%% @doc Stop automatic cluster formation.
%%
%% Stops the cluster strategy process and disconnects from managed nodes.
-spec stop_cluster() -> ok.
stop_cluster() ->
    %% Try to stop gossip strategy
    try macula_cluster_gossip:stop() catch _:_ -> ok end,
    %% Try to stop static strategy
    try macula_cluster_static:stop() catch _:_ -> ok end,
    ok.

%% @doc Get list of connected cluster nodes.
%%
%% Returns all nodes connected to this node via Erlang distribution.
%%
%% Examples:
%% ```
%% Nodes = macula_cluster:nodes().
%% %% => ['node1@host1', 'node2@host2']
%% '''
-spec nodes() -> [atom()].
nodes() ->
    erlang:nodes().

%% @doc Check if auto-clustering is currently active.
%%
%% Returns `true' if the gossip or static strategy is running.
-spec is_clustered() -> boolean().
is_clustered() ->
    (whereis(macula_cluster_gossip) =/= undefined) orelse
    (whereis(macula_cluster_static) =/= undefined).

%%%===================================================================
%%% Internal Functions - Auto-Clustering
%%%===================================================================

%% @private Refuse an unknown strategy before distribution is started.
-spec start_known_strategy(term(), map()) -> ok | {error, term()}.
start_known_strategy(Strategy, Opts)
  when Strategy =:= gossip; Strategy =:= static; Strategy =:= auto ->
    start_distributed(ensure_distributed(), Strategy, Opts);
start_known_strategy(Strategy, _Opts) ->
    {error, {unknown_strategy, Strategy}}.

start_distributed(ok, Strategy, Opts) ->
    start_with_strategy(Strategy, Opts);
start_distributed({error, _} = Error, _Strategy, _Opts) ->
    Error.

start_with_strategy(gossip, Opts) ->
    start_gossip_strategy(Opts);
start_with_strategy(static, Opts) ->
    start_static_strategy(Opts);
start_with_strategy(auto, Opts) ->
    %% Auto-select: use static if nodes configured, else gossip
    start_auto_strategy(resolve_cluster_nodes(Opts), Opts).

start_auto_strategy([], Opts) ->
    %% No static nodes configured, use gossip for zero-config
    start_gossip_strategy(Opts);
start_auto_strategy(_Nodes, Opts) ->
    start_static_strategy(Opts).

%% @private Resolve which strategy to use
-spec resolve_strategy(map()) -> term().
resolve_strategy(Opts) ->
    resolve_strategy_opt(maps:get(strategy, Opts, undefined)).

resolve_strategy_opt(undefined) ->
    %% Check application env
    resolve_strategy_env(application:get_env(macula, cluster_strategy));
resolve_strategy_opt(Strategy) ->
    Strategy.

resolve_strategy_env({ok, Strategy}) -> Strategy;
resolve_strategy_env(undefined) -> auto.

%% @private Resolve cluster nodes from various sources
-spec resolve_cluster_nodes(map()) -> [atom()].
resolve_cluster_nodes(Opts) ->
    resolve_nodes_opt(maps:get(nodes, Opts, undefined)).

resolve_nodes_opt(undefined) ->
    %% Check application env
    resolve_nodes_env(application:get_env(macula, cluster_nodes));
resolve_nodes_opt(Nodes) when is_list(Nodes) ->
    Nodes.

resolve_nodes_env({ok, Nodes}) when is_list(Nodes) ->
    Nodes;
resolve_nodes_env(undefined) ->
    %% Fall back to env var (handled by macula_cluster_static)
    [].

%% @private Start the gossip cluster strategy
-spec start_gossip_strategy(map()) -> ok | {error, term()}.
start_gossip_strategy(Opts) ->
    start_gossip_running(whereis(macula_cluster_gossip), Opts).

start_gossip_running(undefined, Opts) ->
    gossip_start_result(macula_cluster_gossip:start_link(Opts));
start_gossip_running(_Pid, _Opts) ->
    ?LOG_INFO("[macula_cluster] Gossip strategy already running"),
    ok.

gossip_start_result({ok, _Pid}) ->
    ?LOG_INFO("[macula_cluster] Started gossip strategy (UDP multicast)"),
    ok;
gossip_start_result({error, Reason}) ->
    ?LOG_ERROR("[macula_cluster] Failed to start gossip strategy: ~p", [Reason]),
    {error, {gossip_strategy_failed, Reason}}.

%% @private Start the static cluster strategy
-spec start_static_strategy(map()) -> ok | {error, term()}.
start_static_strategy(Opts) ->
    Nodes = resolve_cluster_nodes(Opts),
    StrategyOpts = Opts#{nodes => Nodes},
    start_static_running(whereis(macula_cluster_static), StrategyOpts, Nodes).

start_static_running(undefined, StrategyOpts, Nodes) ->
    static_start_result(macula_cluster_static:start_link(StrategyOpts), Nodes);
start_static_running(_Pid, _StrategyOpts, _Nodes) ->
    ?LOG_INFO("[macula_cluster] Static strategy already running"),
    ok.

static_start_result({ok, _Pid}, Nodes) ->
    ?LOG_INFO("[macula_cluster] Started static strategy "
              "with ~p configured node(s)", [length(Nodes)]),
    ok;
static_start_result({error, Reason}, _Nodes) ->
    ?LOG_ERROR("[macula_cluster] Failed to start static strategy: ~p", [Reason]),
    {error, {static_strategy_failed, Reason}}.
