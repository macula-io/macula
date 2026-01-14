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
%%% Cookies are resolved in this priority order:
%%% 1. Application env: `{macula, [{cookie, CookieValue}]}'
%%% 2. Environment variable: `MACULA_COOKIE' or `RELEASE_COOKIE'
%%% 3. User's ~/.erlang.cookie file
%%% 4. Auto-generated (persisted to ~/.erlang.cookie)
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

%% Internal exports for testing
-export([
    resolve_cookie/0,
    read_cookie_file/0,
    cookie_file_path/0
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

%% @doc Get the Erlang cookie for the cluster.
%%
%% Resolves the cookie from various sources in priority order.
%% If no cookie is found, generates and persists a new one.
%%
%% Resolution Order:
%% 1. Application env: `{macula, [{cookie, CookieValue}]}'
%% 2. Environment variable: `MACULA_COOKIE' or `RELEASE_COOKIE'
%% 3. User's ~/.erlang.cookie file
%% 4. Auto-generated (persisted to ~/.erlang.cookie)
%%
%% Examples:
%% ```
%% Cookie = macula_cluster:get_cookie().
%% '''
-spec get_cookie() -> atom().
get_cookie() ->
    case resolve_cookie() of
        {ok, Cookie} ->
            Cookie;
        {error, not_found} ->
            %% Generate and persist a new cookie
            NewCookie = generate_cookie(),
            ok = persist_cookie(NewCookie),
            NewCookie
    end.

%% @doc Set the Erlang cookie for this node and persist it.
%%
%% Sets the cookie for the current node and attempts to persist
%% it to ~/.erlang.cookie for future sessions.
%%
%% Examples:
%% ```
%% ok = macula_cluster:set_cookie(my_secret_cookie).
%% ok = macula_cluster:set_cookie(<<"my_secret_cookie">>).
%% '''
-spec set_cookie(atom() | binary()) -> ok.
set_cookie(Cookie) when is_binary(Cookie) ->
    set_cookie(binary_to_atom(Cookie, utf8));
set_cookie(Cookie) when is_atom(Cookie) ->
    true = erlang:set_cookie(node(), Cookie),
    _ = persist_cookie(Cookie),
    ok.

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

-spec start_distribution() -> ok | {error, term()}.
start_distribution() ->
    Hostname = get_hostname(),
    NodeName = list_to_atom("macula_host@" ++ Hostname),

    case net_kernel:start([NodeName, shortnames]) of
        {ok, _Pid} ->
            ?LOG_INFO("[macula_cluster] Started distribution as ~p", [NodeName]),
            %% Set the cookie
            Cookie = get_cookie(),
            true = erlang:set_cookie(node(), Cookie),
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("[macula_cluster] Failed to start distribution: ~p", [Reason]),
            {error, {distribution_failed, Reason}}
    end.

%%%===================================================================
%%% Internal Functions - Cookie Resolution
%%%===================================================================

%% @doc Resolve the cookie from various sources.
-spec resolve_cookie() -> {ok, atom()} | {error, not_found}.
resolve_cookie() ->
    Sources = [
        fun get_cookie_from_app_env/0,
        fun get_cookie_from_env_var/0,
        fun read_cookie_file/0
    ],
    try_sources(Sources).

-spec try_sources([fun(() -> {ok, atom()} | {error, term()})]) ->
    {ok, atom()} | {error, not_found}.
try_sources([]) ->
    {error, not_found};
try_sources([Source | Rest]) ->
    case Source() of
        {ok, Cookie} -> {ok, Cookie};
        {error, _} -> try_sources(Rest)
    end.

-spec get_cookie_from_app_env() -> {ok, atom()} | {error, not_found}.
get_cookie_from_app_env() ->
    case application:get_env(macula, cookie) of
        {ok, Cookie} when is_atom(Cookie) ->
            {ok, Cookie};
        {ok, Cookie} when is_binary(Cookie) ->
            {ok, binary_to_atom(Cookie, utf8)};
        {ok, Cookie} when is_list(Cookie) ->
            {ok, list_to_atom(Cookie)};
        _ ->
            {error, not_found}
    end.

-spec get_cookie_from_env_var() -> {ok, atom()} | {error, not_found}.
get_cookie_from_env_var() ->
    EnvVars = ["MACULA_COOKIE", "RELEASE_COOKIE", "ERLANG_COOKIE"],
    get_first_env_var(EnvVars).

-spec get_first_env_var([string()]) -> {ok, atom()} | {error, not_found}.
get_first_env_var([]) ->
    {error, not_found};
get_first_env_var([Var | Rest]) ->
    case os:getenv(Var) of
        false -> get_first_env_var(Rest);
        "" -> get_first_env_var(Rest);
        Value -> {ok, list_to_atom(Value)}
    end.

%% @doc Read cookie from ~/.erlang.cookie file.
-spec read_cookie_file() -> {ok, atom()} | {error, term()}.
read_cookie_file() ->
    CookieFile = cookie_file_path(),
    case file:read_file(CookieFile) of
        {ok, Content} ->
            Cookie = string:trim(binary_to_list(Content)),
            case Cookie of
                "" -> {error, empty_cookie};
                _ -> {ok, list_to_atom(Cookie)}
            end;
        {error, Reason} ->
            {error, {cookie_file_read_failed, Reason}}
    end.

%% @doc Get the path to the cookie file.
-spec cookie_file_path() -> file:filename().
cookie_file_path() ->
    case os:getenv("HOME") of
        false -> "/tmp/.erlang.cookie";
        Home -> filename:join(Home, ".erlang.cookie")
    end.

%%%===================================================================
%%% Internal Functions - Cookie Generation
%%%===================================================================

-spec generate_cookie() -> atom().
generate_cookie() ->
    %% Generate a random 20-character cookie
    Bytes = crypto:strong_rand_bytes(15),
    Hex = binary:encode_hex(Bytes),
    binary_to_atom(Hex, utf8).

-spec persist_cookie(atom()) -> ok | {error, term()}.
persist_cookie(Cookie) ->
    CookieFile = cookie_file_path(),
    CookieStr = atom_to_list(Cookie) ++ "\n",
    case file:write_file(CookieFile, CookieStr) of
        ok ->
            %% Set restrictive permissions (owner read/write only)
            _ = file:change_mode(CookieFile, 8#600),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("[macula_cluster] Failed to persist cookie: ~p", [Reason]),
            {error, {cookie_persist_failed, Reason}}
    end.

%%%===================================================================
%%% API - Auto-Clustering
%%%===================================================================

%% @doc Start automatic cluster formation with default options.
%%
%% Uses the static strategy by default, reading nodes from:
%% 1. Application env: `{macula, [{cluster_nodes, [Node1, Node2, ...]}]}'
%% 2. Environment variable: `CLUSTER_NODES' (comma-separated)
%%
%% If no nodes are configured, starts the DHT-based discovery strategy.
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
%% - strategy: `gossip' (default), `static', `mdns', `dht', or `auto'
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
%% - `mdns': Uses mDNS for local network discovery
%% - `dht': Uses Macula's DHT for internet-scale discovery
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
%%     secret => <<"my-cluster-secret">>
%% }).
%%
%% %% Static strategy with explicit nodes
%% ok = macula_cluster:start_cluster(#{
%%     strategy => static,
%%     nodes => ['node1@host1', 'node2@host2']
%% }).
%%
%% %% mDNS strategy for local network
%% ok = macula_cluster:start_cluster(#{
%%     strategy => mdns
%% }).
%%
%% %% DHT strategy for internet-scale
%% ok = macula_cluster:start_cluster(#{
%%     strategy => dht
%% }).
%% '''
-spec start_cluster(map()) -> ok | {error, term()}.
start_cluster(Opts) ->
    %% Ensure we're in distributed mode first
    case ensure_distributed() of
        ok ->
            do_start_cluster(Opts);
        {error, _} = Error ->
            Error
    end.

%% @doc Stop automatic cluster formation.
%%
%% Stops the cluster strategy process and disconnects from managed nodes.
-spec stop_cluster() -> ok.
stop_cluster() ->
    %% Try to stop gossip strategy
    catch macula_cluster_gossip:stop(),
    %% Try to stop static strategy
    catch macula_cluster_static:stop(),
    %% Try to stop DHT-based strategy
    catch macula_cluster_strategy:stop(macula_cluster),
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
%% Returns `true' if any cluster strategy is running (gossip, static, or DHT).
-spec is_clustered() -> boolean().
is_clustered() ->
    (whereis(macula_cluster_gossip) =/= undefined) orelse
    (whereis(macula_cluster_static) =/= undefined) orelse
    (whereis(macula_cluster) =/= undefined).

%%%===================================================================
%%% Internal Functions - Auto-Clustering
%%%===================================================================

%% @private Start clustering with resolved strategy
-spec do_start_cluster(map()) -> ok | {error, term()}.
do_start_cluster(Opts) ->
    Strategy = resolve_strategy(Opts),
    case Strategy of
        gossip ->
            start_gossip_strategy(Opts);
        static ->
            start_static_strategy(Opts);
        mdns ->
            start_discovery_strategy(Opts#{discovery_type => mdns});
        dht ->
            start_discovery_strategy(Opts#{discovery_type => dht});
        auto ->
            %% Auto-select: use static if nodes configured, else gossip
            case resolve_cluster_nodes(Opts) of
                [] ->
                    %% No static nodes configured, use gossip for zero-config
                    start_gossip_strategy(Opts);
                _Nodes ->
                    start_static_strategy(Opts)
            end
    end.

%% @private Resolve which strategy to use
-spec resolve_strategy(map()) -> gossip | static | mdns | dht | auto.
resolve_strategy(Opts) ->
    case maps:get(strategy, Opts, undefined) of
        undefined ->
            %% Check application env
            case application:get_env(macula, cluster_strategy) of
                {ok, Strategy} -> Strategy;
                undefined -> auto
            end;
        Strategy ->
            Strategy
    end.

%% @private Resolve cluster nodes from various sources
-spec resolve_cluster_nodes(map()) -> [atom()].
resolve_cluster_nodes(Opts) ->
    case maps:get(nodes, Opts, undefined) of
        undefined ->
            %% Check application env
            case application:get_env(macula, cluster_nodes) of
                {ok, Nodes} when is_list(Nodes) ->
                    Nodes;
                undefined ->
                    %% Fall back to env var (handled by macula_cluster_static)
                    []
            end;
        Nodes when is_list(Nodes) ->
            Nodes
    end.

%% @private Start the gossip cluster strategy
-spec start_gossip_strategy(map()) -> ok | {error, term()}.
start_gossip_strategy(Opts) ->
    case whereis(macula_cluster_gossip) of
        undefined ->
            case macula_cluster_gossip:start_link(Opts) of
                {ok, _Pid} ->
                    ?LOG_INFO("[macula_cluster] Started gossip strategy (UDP multicast)"),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("[macula_cluster] Failed to start gossip strategy: ~p",
                               [Reason]),
                    {error, {gossip_strategy_failed, Reason}}
            end;
        _Pid ->
            ?LOG_INFO("[macula_cluster] Gossip strategy already running"),
            ok
    end.

%% @private Start the static cluster strategy
-spec start_static_strategy(map()) -> ok | {error, term()}.
start_static_strategy(Opts) ->
    Nodes = resolve_cluster_nodes(Opts),
    StrategyOpts = Opts#{nodes => Nodes},
    case whereis(macula_cluster_static) of
        undefined ->
            case macula_cluster_static:start_link(StrategyOpts) of
                {ok, _Pid} ->
                    ?LOG_INFO("[macula_cluster] Started static strategy "
                              "with ~p configured node(s)", [length(Nodes)]),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("[macula_cluster] Failed to start static strategy: ~p",
                               [Reason]),
                    {error, {static_strategy_failed, Reason}}
            end;
        _Pid ->
            ?LOG_INFO("[macula_cluster] Static strategy already running"),
            ok
    end.

%% @private Start the DHT/mDNS discovery strategy
-spec start_discovery_strategy(map()) -> ok | {error, term()}.
start_discovery_strategy(Opts) ->
    case whereis(macula_cluster) of
        undefined ->
            StrategyOpts = #{
                topology => macula_cluster,
                config => Opts
            },
            case macula_cluster_strategy:start_link(StrategyOpts) of
                {ok, _Pid} ->
                    DiscoveryType = maps:get(discovery_type, Opts, both),
                    ?LOG_INFO("[macula_cluster] Started discovery strategy (~p)",
                              [DiscoveryType]),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("[macula_cluster] Failed to start discovery strategy: ~p",
                               [Reason]),
                    {error, {discovery_strategy_failed, Reason}}
            end;
        _Pid ->
            ?LOG_INFO("[macula_cluster] Discovery strategy already running"),
            ok
    end.
