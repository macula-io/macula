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
%%% A node that is already distributed keeps its own cookie. On a node that
%%% is not, cookies are resolved in this priority order:
%%% 1. Application env: `{macula, [{cookie, CookieValue}]}'
%%% 2. Environment variable: `MACULA_COOKIE', `RELEASE_COOKIE' or `ERLANG_COOKIE'
%%% 3. The ~/.erlang.cookie file, which only its owner may be able to read
%%% 4. A new cookie, saved owner-only to ~/.erlang.cookie, only when that file
%%%    is missing
%%%
%%% There is no fallback when HOME is unset, and a cookie file that is there
%%% but cannot be used is refused, never replaced.
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
%% A node that is already distributed keeps the cookie it has, from its VM
%% arguments or the Erlang cookie file, and that cookie is returned. On a node
%% that is not distributed the cookie is resolved in this order:
%% 1. Application env: `{macula, [{cookie, CookieValue}]}'
%% 2. Environment variable: `MACULA_COOKIE', `RELEASE_COOKIE' or `ERLANG_COOKIE'
%% 3. The ~/.erlang.cookie file, which only its owner may be able to read
%% 4. A new cookie, saved to ~/.erlang.cookie, only when that file is missing
%%
%% Raises `{cookie_file_unavailable, home_not_set}' when HOME is unset, and
%% `{cookie_file_refused, Reason}' when the cookie file is there but cannot be
%% used. Such a file is never replaced.
%%
%% Examples:
%% ```
%% Cookie = macula_cluster:get_cookie().
%% '''
-spec get_cookie() -> atom().
get_cookie() ->
    cookie_from(own_cookie(erlang:is_alive())).

%% @doc Set the Erlang cookie for this node and persist it.
%%
%% Sets the cookie for the current node and attempts to persist
%% it to ~/.erlang.cookie for future sessions.
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

%% The cookie is resolved before distribution starts, while the node has no
%% cookie of its own, so a configured cookie applies to the node started here.
-spec start_distribution() -> ok | {error, term()}.
start_distribution() ->
    Hostname = get_hostname(),
    NodeName = list_to_atom("macula_host@" ++ Hostname),
    Cookie = get_cookie(),

    case net_kernel:start([NodeName, shortnames]) of
        {ok, _Pid} ->
            ?LOG_INFO("[macula_cluster] Started distribution as ~p", [NodeName]),
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

%% A distributed node's own cookie; `nocookie' means it has none.
own_cookie(true) -> known_cookie(erlang:get_cookie());
own_cookie(false) -> none.

known_cookie(nocookie) -> none;
known_cookie(Cookie) -> {ok, Cookie}.

cookie_from({ok, Cookie}) -> Cookie;
cookie_from(none) -> resolved(resolve_cookie()).

resolved({ok, Cookie}) -> Cookie;
resolved({error, not_found}) -> generated_and_saved();
resolved({error, home_not_set}) -> erlang:error({cookie_file_unavailable, home_not_set});
resolved({error, {cookie_file_refused, _} = Refusal}) -> erlang:error(Refusal).

%% Only a missing cookie file gets a new cookie.
generated_and_saved() ->
    Cookie = generate_cookie(),
    saved(persist_cookie(Cookie), Cookie).

saved(ok, Cookie) -> Cookie;
saved({error, Reason}, _Cookie) -> erlang:error(Reason).

%% @doc Resolve the cookie from the application env, the environment and the
%% cookie file. A missing cookie file is `{error, not_found}'. An unset HOME,
%% or a cookie file that cannot be used, is returned as its own error and
%% never skipped.
-spec resolve_cookie() ->
    {ok, atom()} | {error, not_found | home_not_set | {cookie_file_refused, term()}}.
resolve_cookie() ->
    configured_or_file(try_sources([fun get_cookie_from_app_env/0,
                                    fun get_cookie_from_env_var/0])).

configured_or_file({ok, _} = Found) -> Found;
configured_or_file({error, not_found}) -> file_resolution(read_cookie_file()).

file_resolution({error, enoent}) -> {error, not_found};
file_resolution(Result) -> Result.

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

%% @doc Read the cookie from ~/.erlang.cookie through
%% macula_owner_only_file:read/1, which accepts only a regular file its group
%% and others cannot access. A missing file is `{error, enoent}'; a file that
%% cannot be used is `{error, {cookie_file_refused, Reason}}'.
-spec read_cookie_file() ->
    {ok, atom()} | {error, enoent | home_not_set | {cookie_file_refused, term()}}.
read_cookie_file() ->
    cookie_from_path(cookie_file_path()).

cookie_from_path({ok, Path}) -> cookie_from_file(macula_owner_only_file:read(Path));
cookie_from_path({error, home_not_set} = Error) -> Error.

cookie_from_file({ok, Content}) ->
    cookie_value(string:trim(binary_to_list(Content)));
cookie_from_file({error, enoent}) ->
    {error, enoent};
cookie_from_file({error, Reason}) ->
    {error, {cookie_file_refused, Reason}}.

cookie_value("") -> {error, {cookie_file_refused, empty}};
cookie_value(Cookie) -> {ok, list_to_atom(Cookie)}.

%% @doc The cookie file in HOME. There is no fallback when HOME is unset.
-spec cookie_file_path() -> {ok, file:filename()} | {error, home_not_set}.
cookie_file_path() ->
    cookie_path_in(os:getenv("HOME")).

cookie_path_in(false) -> {error, home_not_set};
cookie_path_in("") -> {error, home_not_set};
cookie_path_in(Home) -> {ok, filename:join(Home, ".erlang.cookie")}.

%%%===================================================================
%%% Internal Functions - Cookie Generation
%%%===================================================================

-spec generate_cookie() -> atom().
generate_cookie() ->
    %% Generate a random 20-character cookie
    Bytes = crypto:strong_rand_bytes(15),
    Hex = binary:encode_hex(Bytes),
    binary_to_atom(Hex, utf8).

%% Saves through macula_owner_only_file:write/2: only the owner can read the
%% file, and a symlink at the path is replaced, never written through.
-spec persist_cookie(atom()) -> ok | {error, {cookie_persist_failed, term()}}.
persist_cookie(Cookie) ->
    persist_result(saved_to(cookie_file_path(), atom_to_list(Cookie) ++ "\n")).

saved_to({ok, Path}, Content) -> macula_owner_only_file:write(Path, Content);
saved_to({error, _} = Error, _Content) -> Error.

persist_result(ok) ->
    ok;
persist_result({error, Reason}) ->
    ?LOG_WARNING("[macula_cluster] Failed to persist cookie: ~p", [Reason]),
    {error, {cookie_persist_failed, Reason}}.

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
%% - strategy: `auto' (default), `gossip' or `static'. `dht' is not
%%   available: the call returns `{error, {strategy_unavailable, dht}}'.
%%   `mdns' needs a running `macula_dist_discovery' server; without one the
%%   call returns `{error, {strategy_unavailable, mdns}}'. Neither error
%%   starts distribution, and both values are removed in 11.0.0.
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
    start_available(strategy_availability(resolve_strategy(Opts)), Opts).

%% @doc Stop automatic cluster formation.
%%
%% Stops the cluster strategy process and disconnects from managed nodes.
-spec stop_cluster() -> ok.
stop_cluster() ->
    %% Try to stop gossip strategy
    try macula_cluster_gossip:stop() catch _:_ -> ok end,
    %% Try to stop static strategy
    try macula_cluster_static:stop() catch _:_ -> ok end,
    %% Try to stop the mdns discovery strategy
    try macula_cluster_strategy:stop(macula_cluster) catch _:_ -> ok end,
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
%% Returns `true' if any cluster strategy is running (gossip, static, or mdns).
-spec is_clustered() -> boolean().
is_clustered() ->
    (whereis(macula_cluster_gossip) =/= undefined) orelse
    (whereis(macula_cluster_static) =/= undefined) orelse
    (whereis(macula_cluster) =/= undefined).

%%%===================================================================
%%% Internal Functions - Auto-Clustering
%%%===================================================================

%% @private The dht strategy needs a DHT module that no macula release
%% provides, and the mdns strategy needs a running discovery server. An
%% unavailable strategy is refused before distribution is started.
strategy_availability(dht) ->
    {unavailable, dht};
strategy_availability(mdns) ->
    mdns_availability(whereis(macula_dist_discovery));
strategy_availability(_Strategy) ->
    available.

mdns_availability(undefined) -> {unavailable, mdns};
mdns_availability(_Pid) -> available.

start_available(available, Opts) ->
    start_distributed(ensure_distributed(), Opts);
start_available({unavailable, Strategy}, _Opts) ->
    {error, {strategy_unavailable, Strategy}}.

start_distributed(ok, Opts) ->
    do_start_cluster(Opts);
start_distributed({error, _} = Error, _Opts) ->
    Error.

%% @private Start clustering with resolved strategy
-spec do_start_cluster(map()) -> ok | {error, term()}.
do_start_cluster(Opts) ->
    start_with_strategy(resolve_strategy(Opts), Opts).

start_with_strategy(gossip, Opts) ->
    start_gossip_strategy(Opts);
start_with_strategy(static, Opts) ->
    start_static_strategy(Opts);
start_with_strategy(mdns, Opts) ->
    start_discovery_strategy(Opts#{discovery_type => mdns});
start_with_strategy(auto, Opts) ->
    %% Auto-select: use static if nodes configured, else gossip
    start_auto_strategy(resolve_cluster_nodes(Opts), Opts).

start_auto_strategy([], Opts) ->
    %% No static nodes configured, use gossip for zero-config
    start_gossip_strategy(Opts);
start_auto_strategy(_Nodes, Opts) ->
    start_static_strategy(Opts).

%% @private Resolve which strategy to use
-spec resolve_strategy(map()) -> gossip | static | mdns | dht | auto.
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

%% @private Start the mdns discovery strategy
-spec start_discovery_strategy(map()) -> ok | {error, term()}.
start_discovery_strategy(Opts) ->
    start_discovery_running(whereis(macula_cluster), Opts).

start_discovery_running(undefined, Opts) ->
    StrategyOpts = #{
        topology => macula_cluster,
        config => Opts
    },
    discovery_start_result(macula_cluster_strategy:start_link(StrategyOpts), Opts);
start_discovery_running(_Pid, _Opts) ->
    ?LOG_INFO("[macula_cluster] Discovery strategy already running"),
    ok.

discovery_start_result({ok, _Pid}, Opts) ->
    DiscoveryType = maps:get(discovery_type, Opts, both),
    ?LOG_INFO("[macula_cluster] Started discovery strategy (~p)", [DiscoveryType]),
    ok;
discovery_start_result({error, Reason}, _Opts) ->
    ?LOG_ERROR("[macula_cluster] Failed to start discovery strategy: ~p", [Reason]),
    {error, {discovery_strategy_failed, Reason}}.
