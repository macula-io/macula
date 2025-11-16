%%%-------------------------------------------------------------------
%%% @doc
%%% Gateway Root Supervisor - top-level supervisor for gateway subsystem.
%%%
%%% Supervision Strategy:
%%% - rest_for_one: Dependency-based restart ordering
%%% - Child order reflects dependencies:
%%%   1. quic_server (owns QUIC listener, no dependencies)
%%%   2. gateway (depends on quic_server PID)
%%%   3. workers_sup (depends on gateway PID)
%%%
%%% Fault Isolation:
%%% - quic_server crash → restart quic_server, gateway, workers_sup
%%% - gateway crash → restart gateway, workers_sup (quic_server continues)
%%% - workers_sup crash → restart workers_sup only (quic_server and gateway continue)
%%%
%%% Architecture:
%%% ```
%%% macula_gateway_sup (this module)
%%% ├── macula_gateway_quic_server  - QUIC transport layer
%%% ├── macula_gateway              - Message routing coordinator
%%% └── macula_gateway_workers_sup  - Business logic workers
%%%     ├── macula_gateway_clients  - Client tracking
%%%     ├── macula_gateway_pubsub   - Pub/Sub routing
%%%     ├── macula_gateway_rpc      - RPC handling
%%%     └── macula_gateway_mesh     - Mesh connections
%%% ```
%%%
%%% Circular Dependency Resolution:
%%% - quic_server starts first (without gateway PID)
%%% - gateway starts second (receives quic_server PID)
%%% - Supervisor calls quic_server:set_gateway/1 to complete link
%%% - workers_sup starts last (receives gateway PID)
%%%
%%% Created during Phase 2 QUIC refactoring to enable proper OTP supervision.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the root gateway supervisor with configuration.
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Opts) when is_list(Opts) ->
    %% Called with proplist (legacy tests, application startup)
    Port = proplists:get_value(port, Opts, 9443),
    Realm = proplists:get_value(realm, Opts, <<"macula.default">>),
    CertFile = proplists:get_value(cert_file, Opts),
    KeyFile = proplists:get_value(key_file, Opts),
    init_supervisor(Port, Realm, CertFile, KeyFile);

init(Opts) when is_map(Opts) ->
    %% Called with map (new style)
    Port = maps:get(port, Opts, 9443),
    Realm = maps:get(realm, Opts, <<"macula.default">>),
    CertFile = maps:get(cert_file, Opts, undefined),
    KeyFile = maps:get(key_file, Opts, undefined),
    init_supervisor(Port, Realm, CertFile, KeyFile).

%% @private
%% @doc Initialize the supervisor with extracted configuration.
init_supervisor(Port, Realm, CertFile, KeyFile) ->

    io:format("[GatewaySup] Initializing gateway supervisor for realm ~s on port ~p~n",
              [Realm, Port]),

    %% Supervision strategy: rest_for_one
    %% - quic_server fails → restart quic_server, gateway, workers_sup
    %% - gateway fails → restart gateway, workers_sup
    %% - workers_sup fails → restart workers_sup only
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 60
    },

    %% Child 1: QUIC Server (starts without gateway PID)
    QuicServerSpec = #{
        id => macula_gateway_quic_server,
        start => {macula_gateway_quic_server, start_link, [[
            {port, Port},
            {realm, Realm},
            {cert_file, CertFile},
            {key_file, KeyFile}
            %% Note: NO gateway PID yet!
        ]]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_gateway_quic_server]
    },

    %% Child 2: Gateway (receives quic_server PID after it starts)
    GatewaySpec = #{
        id => macula_gateway,
        start => {macula_gateway, start_link, [[
            {port, Port},
            {realm, Realm}
            %% Note: Gateway will find quic_server via supervisor
        ]]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_gateway]
    },

    %% Child 3: Workers Supervisor (supervises business logic workers)
    WorkersSupSpec = #{
        id => macula_gateway_workers_sup,
        start => {macula_gateway_workers_sup, start_link, [#{
            port => Port,
            realm => Realm
        }]},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_gateway_workers_sup]
    },

    Children = [QuicServerSpec, GatewaySpec, WorkersSupSpec],

    {ok, {SupFlags, Children}}.
