%%%-------------------------------------------------------------------
%% @doc Macula Application Root Supervisor.
%%
%% This is the top-level supervisor for the Macula application.
%% It manages all Macula subsystems in an always-on architecture.
%%
%% Supervision Hierarchy (v0.8.5+):
%% <pre>
%% macula_root (this module - application root)
%% ├── macula_routing_server (core DHT infrastructure - always on)
%% ├── macula_bootstrap_system (bootstrap services - always on)
%% │   ├── macula_bootstrap_server
%% │   ├── macula_bootstrap_registry
%% │   └── macula_bootstrap_health
%% ├── macula_gateway_system (gateway services - always on)
%% │   ├── macula_gateway_health
%% │   ├── macula_gateway_diagnostics
%% │   ├── macula_gateway_quic_server
%% │   ├── macula_gateway
%% │   └── macula_gateway_workers_sup
%% └── macula_peers_sup (dynamic peer connections - always on)
%% </pre>
%%
%% Architecture Philosophy (v0.8.5):
%% - All nodes have ALL capabilities (no mode selection)
%% - Zero configuration required (TLS auto-generated)
%% - Simplified deployment (every node is bootstrap + gateway + peer)
%% - True P2P mesh (nodes connect on-demand based on capability discovery)
%%
%% Naming Convention (v0.7.10+):
%% - _root: Application root supervisor (one per application)
%% - _system: Subsystem root supervisors (gateway, peer, bootstrap, etc.)
%% - _sup: Worker supervisors (workers_sup, peers_sup)
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
    io:format("~n"),
    io:format("═══════════════════════════════════════════════════════════════~n"),
    io:format("  Starting Macula v0.8.5 (Always-On Architecture)~n"),
    io:format("  All capabilities enabled: Bootstrap + Gateway + Peer~n"),
    io:format("═══════════════════════════════════════════════════════════════~n"),
    io:format("~n"),

    %% Ensure TLS certificates exist (auto-generate if missing)
    {CertPath, KeyPath} = macula_tls:get_cert_paths(),
    {ok, _CertPath, _KeyPath, NodeID} = macula_tls:ensure_cert_exists(CertPath, KeyPath),

    io:format("✓ TLS Certificate: ~s~n", [CertPath]),
    io:format("✓ Private Key: ~s~n", [KeyPath]),
    io:format("✓ Node ID: ~s~n", [NodeID]),
    io:format("~n"),

    %% Get configuration
    Port = get_quic_port(),
    Realm = get_realm(),
    HealthPort = get_health_port(),
    HealthInterval = get_bootstrap_health_interval(),

    io:format("Configuration:~n"),
    io:format("  QUIC Port: ~p~n", [Port]),
    io:format("  Realm: ~s~n", [Realm]),
    io:format("  Health Port: ~p~n", [HealthPort]),
    io:format("  Bootstrap Health Interval: ~pms~n", [HealthInterval]),
    io:format("~n"),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },

    ChildSpecs = [
        %% 1. Core DHT routing (always on)
        get_routing_server_spec(NodeID),

        %% 2. Bootstrap system (always on)
        get_bootstrap_system_spec(Realm, HealthInterval),

        %% 3. Gateway system (always on)
        get_gateway_system_spec(Port, Realm, HealthPort),

        %% 4. Peer connections supervisor (always on)
        get_peers_sup_spec()
    ],

    io:format("Starting subsystems:~n"),
    io:format("  [1/4] Core DHT Routing~n"),
    io:format("  [2/4] Bootstrap System~n"),
    io:format("  [3/4] Gateway System~n"),
    io:format("  [4/4] Peers Supervisor~n"),
    io:format("~n"),

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

%% @private
%% @doc Get routing server child spec (core DHT infrastructure).
get_routing_server_spec(NodeID) ->
    #{
        id => macula_routing_server,
        start => {macula_routing_server, start_link, [NodeID, get_routing_config()]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_routing_server]
    }.

%% @private
%% @doc Get bootstrap system child spec.
get_bootstrap_system_spec(Realm, HealthInterval) ->
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
    }.

%% @private
%% @doc Get gateway system child spec.
get_gateway_system_spec(Port, Realm, HealthPort) ->
    #{
        id => macula_gateway_system,
        start => {macula_gateway_system, start_link, [[
            {port, Port},
            {realm, Realm},
            {health_port, HealthPort}
        ]]},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_gateway_system]
    }.

%% @private
%% @doc Get peers supervisor child spec (simple_one_for_one for dynamic peers).
get_peers_sup_spec() ->
    #{
        id => macula_peers_sup,
        start => {macula_peers_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_peers_sup]
    }.

%% @private
%% @doc Get QUIC port from environment variable or config.
%% Environment variable renamed from GATEWAY_PORT to MACULA_QUIC_PORT (v0.8.5).
get_quic_port() ->
    case os:getenv("MACULA_QUIC_PORT") of
        false ->
            %% Fallback to old GATEWAY_PORT for backward compatibility
            case os:getenv("GATEWAY_PORT") of
                false ->
                    application:get_env(macula, quic_port,
                        application:get_env(macula, gateway_port, 9443));
                PortStr ->
                    list_to_integer(PortStr)
            end;
        PortStr ->
            list_to_integer(PortStr)
    end.

%% @private
%% @doc Get realm from environment variable or config.
get_realm() ->
    case os:getenv("MACULA_REALM") of
        false ->
            application:get_env(macula, realm,
                application:get_env(macula, gateway_realm, <<"com.example.realm">>));
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
%% @doc Get bootstrap health check interval.
get_bootstrap_health_interval() ->
    application:get_env(macula, bootstrap_health_interval, 60000).

%% @private
%% @doc Get DHT routing configuration.
get_routing_config() ->
    #{
        k => application:get_env(macula, dht_k, 20),        %% Kademlia K parameter
        alpha => application:get_env(macula, dht_alpha, 3)  %% Concurrent queries
    }.
