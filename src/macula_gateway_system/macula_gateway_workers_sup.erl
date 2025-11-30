%%%-------------------------------------------------------------------
%%% @doc
%%% Gateway Workers Supervisor - supervises gateway worker processes.
%%%
%%% Supervision Strategy:
%%% - rest_for_one: If child N crashes, restart N and all children after N
%%% - Rationale: Only clients is foundational; pubsub/rpc/mesh depend
%%%   on it but are independent of each other. This strategy provides fault
%%%   isolation while maintaining consistency when clients restarts.
%%%
%%% Children (in dependency order):
%%% - macula_gateway_clients: Client tracking (foundational)
%%% - macula_gateway_pubsub: Pub/Sub message routing (depends on clients)
%%% - macula_gateway_rpc: RPC handler registration and routing (depends on clients)
%%% - macula_gateway_mesh: Mesh connection pooling and management (independent)
%%%
%%% Fault Isolation Examples:
%%% - mesh crash → only mesh restarts (0 clients disconnected)
%%% - rpc crash → rpc + mesh restart (0 clients disconnected)
%%% - pubsub crash → pubsub + rpc + mesh restart (0 clients disconnected)
%%% - clients crash → all restart (unavoidable - foundational)
%%%
%%% Extracted from macula_gateway.erl (Phase 6, 9)
%%% Renamed from macula_gateway_sup (Phase 2 QUIC refactoring)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_workers_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    get_clients/1,
    get_pubsub/1,
    get_rpc/1,
    get_mesh/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the gateway supervisor with configuration.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link(?MODULE, Config).

%% @doc Get the clients worker child PID.
-spec get_clients(pid()) -> {ok, pid()} | {error, not_found}.
get_clients(SupPid) ->
    get_child_pid(SupPid, macula_gateway_clients).

%% @doc Get the pubsub handler child PID.
-spec get_pubsub(pid()) -> {ok, pid()} | {error, not_found}.
get_pubsub(SupPid) ->
    get_child_pid(SupPid, macula_gateway_pubsub).

%% @doc Get the RPC handler child PID.
-spec get_rpc(pid()) -> {ok, pid()} | {error, not_found}.
get_rpc(SupPid) ->
    get_child_pid(SupPid, macula_gateway_rpc).

%% @doc Get the mesh connection manager child PID.
-spec get_mesh(pid()) -> {ok, pid()} | {error, not_found}.
get_mesh(SupPid) ->
    get_child_pid(SupPid, macula_gateway_mesh).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Config) ->
    ?LOG_INFO("Initializing gateway workers supervisor"),
    ?LOG_DEBUG("Config: ~p", [Config]),

    %% Supervision strategy: rest_for_one
    %% - If child N crashes, restart N and all children after N
    %% - Provides fault isolation while maintaining dependency consistency
    %% - Max 10 restarts in 60 seconds
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 60
    },

    ?LOG_DEBUG("Building child specifications..."),
    %% Child specifications
    Children = [
        %% Clients - tracks connected clients and streams
        #{
            id => macula_gateway_clients,
            start => {macula_gateway_clients, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_gateway_clients]
        },

        %% Subscriber Cache - caches DHT topic→subscribers lookups (5-10x latency improvement)
        #{
            id => macula_subscriber_cache,
            start => {macula_subscriber_cache, start_link, [#{ttl_ms => 5000}]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_subscriber_cache]
        },

        %% Direct Routing Table - caches NodeId→Endpoint for direct P2P routing (3-5x improvement)
        #{
            id => macula_direct_routing,
            start => {macula_direct_routing, start_link, [#{ttl_ms => 300000}]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_direct_routing]
        },

        %% Pub/Sub Handler - routes published messages to subscribers
        #{
            id => macula_gateway_pubsub,
            start => {macula_gateway_pubsub, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_gateway_pubsub]
        },

        %% RPC Handler - manages RPC handler registration and routing
        #{
            id => macula_gateway_rpc,
            start => {macula_gateway_rpc, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_gateway_rpc]
        },

        %% Mesh Connection Manager - pools QUIC connections to remote peers
        #{
            id => macula_gateway_mesh,
            start => {macula_gateway_mesh, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_gateway_mesh]
        },

        %% Peer Connection Pool - pools outbound QUIC connections (1.5-2x latency improvement)
        #{
            id => macula_peer_connection_pool,
            start => {macula_peer_connection_pool, start_link, [#{max_connections => 100, idle_timeout_ms => 60000}]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_peer_connection_pool]
        }
    ],

    ?LOG_DEBUG("Child specs built, returning from init"),
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Get a child process PID by child ID.
-spec get_child_pid(pid(), atom()) -> {ok, pid()} | {error, not_found}.
get_child_pid(SupPid, ChildId) ->
    case supervisor:which_children(SupPid) of
        Children when is_list(Children) ->
            extract_child_pid(lists:keyfind(ChildId, 1, Children));
        _ ->
            {error, not_found}
    end.

%% @doc Extract child PID from keyfind result.
extract_child_pid({_, ChildPid, _Type, _Modules}) when is_pid(ChildPid) ->
    {ok, ChildPid};
extract_child_pid(_) ->
    {error, not_found}.
