%%%-------------------------------------------------------------------
%%% @doc
%%% Supervisor for the connection subsystem.
%%%
%%% Supervision Strategy:
%%% - rest_for_one: If child N crashes, restart N and all children after N
%%% - Rationale: connection_manager is foundational; handlers depend on it
%%%   but are independent of each other. This provides fault isolation
%%%   while maintaining consistency when connection_manager restarts.
%%%
%%% Children (in dependency order):
%%% - macula_connection: QUIC connection lifecycle (foundational)
%%% - macula_pubsub_handler: Pub/sub operations (depends on connection_manager)
%%% - macula_rpc_handler: RPC operations (depends on connection_manager)
%%% - macula_advertisement_manager: DHT advertisements (depends on connection_manager)
%%%
%%% Fault Isolation:
%%% - advertisement_manager crash → only advertisement restarts
%%% - rpc_handler crash → rpc + advertisement restart
%%% - pubsub_handler crash → pubsub + rpc + advertisement restart
%%% - connection_manager crash → all restart (unavoidable - foundational)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_sup).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the connection supervisor with given URL and options.
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Url, Opts) ->
    supervisor:start_link(?MODULE, {Url, Opts}).

%% @doc Stop the connection supervisor and all children.
-spec stop(pid()) -> ok.
stop(Sup) when is_pid(Sup) ->
    exit(Sup, shutdown),
    ok.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @doc Initialize the supervisor with child specifications.
-spec init({binary(), map()}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({Url, Opts}) ->
    ?LOG_INFO("Starting connection supervisor for ~s", [Url]),

    %% Supervision strategy: rest_for_one
    %% If child N crashes, restart N and all children after N
    %% - connection_manager crash → all restart (they depend on it)
    %% - handler crash → only that handler and later ones restart (fault isolation)
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,  % Max 10 restarts (increased tolerance)
        period => 60      % Within 60 seconds (longer period)
    },

    %% Children in dependency order (connection_manager is foundational)
    ChildSpecs = [
        %% 1. Connection Manager (must start first)
        #{
            id => connection_manager,
            start => {macula_connection, start_link, [Url, Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_connection]
        },

        %% 2. PubSub Handler
        #{
            id => pubsub_handler,
            start => {macula_pubsub_handler, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_pubsub_handler]
        },

        %% 3. RPC Handler
        #{
            id => rpc_handler,
            start => {macula_rpc_handler, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_rpc_handler]
        },

        %% 4. Advertisement Manager
        #{
            id => advertisement_manager,
            start => {macula_advertisement_manager, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_advertisement_manager]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
