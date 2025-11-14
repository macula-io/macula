%%%-------------------------------------------------------------------
%%% @doc
%%% Gateway Supervisor - supervises all gateway worker processes.
%%%
%%% Supervision Strategy:
%%% - rest_for_one: If child N crashes, restart N and all children after N
%%% - Rationale: Only client_manager is foundational; pubsub/rpc/mesh depend
%%%   on it but are independent of each other. This strategy provides fault
%%%   isolation while maintaining consistency when client_manager restarts.
%%%
%%% Children (in dependency order):
%%% - macula_gateway_client_manager: Client lifecycle management (foundational)
%%% - macula_gateway_pubsub: Pub/Sub message routing (depends on client_manager)
%%% - macula_gateway_rpc: RPC handler registration and routing (depends on client_manager)
%%% - macula_gateway_mesh: Mesh connection pooling and management (independent)
%%%
%%% Fault Isolation Examples:
%%% - mesh crash → only mesh restarts (0 clients disconnected)
%%% - rpc crash → rpc + mesh restart (0 clients disconnected)
%%% - pubsub crash → pubsub + rpc + mesh restart (0 clients disconnected)
%%% - client_manager crash → all restart (unavoidable - foundational)
%%%
%%% Extracted from macula_gateway.erl (Phase 6, 9)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/1,
    get_client_manager/1,
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

%% @doc Get the client manager child PID.
-spec get_client_manager(pid()) -> {ok, pid()} | {error, not_found}.
get_client_manager(SupPid) ->
    get_child_pid(SupPid, macula_gateway_client_manager).

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
    %% Supervision strategy: rest_for_one
    %% - If child N crashes, restart N and all children after N
    %% - Provides fault isolation while maintaining dependency consistency
    %% - Max 10 restarts in 60 seconds
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 60
    },

    %% Child specifications
    Children = [
        %% Client Manager - tracks connected clients and streams
        #{
            id => macula_gateway_client_manager,
            start => {macula_gateway_client_manager, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_gateway_client_manager]
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
        }
    ],

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
