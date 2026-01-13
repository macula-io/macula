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
