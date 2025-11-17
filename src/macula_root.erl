%%%-------------------------------------------------------------------
%% @doc Macula Application Root Supervisor.
%%
%% This is the top-level supervisor for the Macula application.
%% It manages core infrastructure and optionally starts gateway services.
%%
%% Supervision Hierarchy:
%% <pre>
%% macula_root (this module - application root)
%% ├── macula_routing_server (core DHT infrastructure)
%% └── macula_gateway_system (optional - gateway subsystem)
%%     ├── macula_gateway_health
%%     ├── macula_gateway_diagnostics
%%     ├── macula_gateway_quic_server
%%     ├── macula_gateway
%%     └── macula_gateway_workers_sup
%% </pre>
%%
%% Naming Convention (v0.7.10+):
%% - _root: Application root supervisor (one per application)
%% - _system: Subsystem root supervisors (gateway, peer, etc.)
%% - _sup: Worker supervisors (workers_sup)
%%
%% @end
%%%-------------------------------------------------------------------

-module(macula_root).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    %% Determine deployment mode
    Mode = application:get_env(macula, mode, gateway),
    validate_mode_config(Mode),
    io:format("Starting Macula in ~p mode~n", [Mode]),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },

    %% Build child specs based on mode
    CoreSpecs = get_core_child_specs(),
    BootstrapSpecs = maybe_start_bootstrap(Mode),
    GatewaySpecs = maybe_start_gateway(Mode),

    ChildSpecs = CoreSpecs ++ BootstrapSpecs ++ GatewaySpecs,

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

%% @private
%% @doc Get core infrastructure child specs (always started, even in embedded mode).
%% The routing server is essential for DHT operations in any mode.
get_core_child_specs() ->
    io:format("Starting Macula core infrastructure (routing server)~n"),
    [
        %% DHT routing server (essential for all DHT operations)
        #{
            id => macula_routing_server,
            start => {macula_routing_server, start_link, [get_node_id(), get_routing_config()]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_routing_server]
        }
    ].

%% @private
%% @doc Conditionally start bootstrap system based on mode.
%% Bootstrap system is started for: bootstrap, hybrid modes.
maybe_start_bootstrap(bootstrap) -> get_bootstrap_specs();
maybe_start_bootstrap(hybrid) -> get_bootstrap_specs();
maybe_start_bootstrap(_) -> [].

%% @private
%% @doc Get bootstrap system child specs.
get_bootstrap_specs() ->
    Realm = get_gateway_realm(),
    HealthInterval = application:get_env(macula, bootstrap_health_interval, 60000),

    io:format("Starting Bootstrap System (realm: ~s, health_interval: ~pms)~n", [Realm, HealthInterval]),

    [
        #{
            id => macula_bootstrap_system,
            start => {macula_bootstrap_system, start_link, [#{
                realm => Realm,
                health_check_interval => HealthInterval
            }]},
            restart => permanent,
            shutdown => infinity,  % supervisor shutdown
            type => supervisor,
            modules => [macula_bootstrap_system]
        }
    ].

%% @private
%% @doc Conditionally start gateway services based on mode and configuration.
%% Gateway required for: gateway, hybrid modes.
%% Can be overridden by: {start_gateway, false}.
maybe_start_gateway(Mode) ->
    %% Determine if gateway should start based on mode
    DefaultStartGateway = mode_requires_gateway(Mode),
    StartGateway = application:get_env(macula, start_gateway, DefaultStartGateway),

    case StartGateway of
        true ->
            %% Read gateway configuration from environment or config
            Port = get_gateway_port(),
            Realm = get_gateway_realm(),
            HealthPort = get_health_port(),

            io:format("Starting Macula Gateway on port ~p (realm: ~s)~n", [Port, Realm]),
            io:format("Starting health check server on port ~p~n", [HealthPort]),

            [
                %% Gateway system supervisor (manages all gateway components)
                #{
                    id => macula_gateway_system,
                    start => {macula_gateway_system, start_link, [[
                        {port, Port},
                        {realm, Realm},
                        {health_port, HealthPort}
                    ]]},
                    restart => permanent,
                    shutdown => infinity,
                    type => supervisor,
                    modules => [macula_gateway_system]
                }
            ];
        false ->
            io:format("Gateway disabled for ~p mode~n", [Mode]),
            []
    end.

%% @private
%% @doc Determine if mode requires gateway by default.
mode_requires_gateway(gateway) -> true;
mode_requires_gateway(hybrid) -> true;
mode_requires_gateway(bootstrap) -> false;  % Bootstrap-only, no gateway
mode_requires_gateway(edge) -> false.       % Pure P2P, no gateway

%% @private
%% @doc Validate mode configuration.
%% Crashes on invalid mode to prevent misconfiguration.
validate_mode_config(Mode) ->
    ValidModes = [bootstrap, edge, gateway, hybrid],
    case lists:member(Mode, ValidModes) of
        true -> ok;
        false ->
            io:format("ERROR: Invalid mode ~p. Valid modes: ~p~n", [Mode, ValidModes]),
            erlang:error({invalid_mode, Mode, ValidModes})
    end.

%% @private
%% @doc Get gateway port from environment variable or config.
get_gateway_port() ->
    case os:getenv("GATEWAY_PORT") of
        false ->
            application:get_env(macula, gateway_port, 9443);
        PortStr ->
            list_to_integer(PortStr)
    end.

%% @private
%% @doc Get gateway realm from environment variable or config.
get_gateway_realm() ->
    case os:getenv("MACULA_REALM") of
        false ->
            application:get_env(macula, gateway_realm, <<"com.example.realm">>);
        RealmStr ->
            list_to_binary(RealmStr)
    end.

%% @private
%% @doc Get health check port from environment variable or config.
get_health_port() ->
    case os:getenv("HEALTH_PORT") of
        false ->
            application:get_env(macula, health_port, 8080);
        PortStr ->
            list_to_integer(PortStr)
    end.

%% @private
%% @doc Get node ID for DHT routing (32-byte identifier).
get_node_id() ->
    %% Try environment variable first, then config, finally generate one
    case os:getenv("NODE_ID") of
        false ->
            case application:get_env(macula, node_id) of
                {ok, NodeId} when is_binary(NodeId), byte_size(NodeId) == 32 ->
                    NodeId;
                _ ->
                    %% Generate a random 32-byte node ID
                    crypto:strong_rand_bytes(32)
            end;
        NodeIdStr ->
            list_to_binary(NodeIdStr)
    end.

%% @private
%% @doc Get DHT routing configuration.
get_routing_config() ->
    #{
        k => application:get_env(macula, dht_k, 20),        %% Kademlia K parameter
        alpha => application:get_env(macula, dht_alpha, 3)  %% Concurrent queries
    }.
