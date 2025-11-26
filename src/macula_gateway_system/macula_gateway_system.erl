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
%%% <pre>
%%% macula_gateway_system (this module)
%%% ├── macula_gateway_health        - Health check HTTP server
%%% ├── macula_gateway_diagnostics   - Diagnostics service
%%% ├── macula_gateway_quic_server   - QUIC transport layer
%%% ├── macula_gateway               - Message routing coordinator
%%% └── macula_gateway_workers_sup   - Business logic workers
%%%     ├── macula_gateway_clients   - Client tracking
%%%     ├── macula_gateway_pubsub    - Pub/Sub routing
%%%     ├── macula_gateway_rpc       - RPC handling
%%%     └── macula_gateway_mesh      - Mesh connections
%%% </pre>
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
-module(macula_gateway_system).

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
    HealthPort = proplists:get_value(health_port, Opts, 8080),
    CertFile = get_cert_file(proplists:get_value(cert_file, Opts)),
    KeyFile = get_key_file(proplists:get_value(key_file, Opts)),
    init_supervisor(Port, Realm, HealthPort, CertFile, KeyFile);

init(Opts) when is_map(Opts) ->
    %% Called with map (new style)
    Port = maps:get(port, Opts, 9443),
    Realm = maps:get(realm, Opts, <<"macula.default">>),
    HealthPort = maps:get(health_port, Opts, 8080),
    CertFile = get_cert_file(maps:get(cert_file, Opts, undefined)),
    KeyFile = get_key_file(maps:get(key_file, Opts, undefined)),
    init_supervisor(Port, Realm, HealthPort, CertFile, KeyFile).

%% @private
%% @doc Initialize the supervisor with extracted configuration.
init_supervisor(Port, Realm, HealthPort, CertFile, KeyFile) ->

    io:format("[GatewaySup] Initializing gateway supervisor for realm ~s on port ~p~n",
              [Realm, Port]),
    io:format("[GatewaySup] Certificate file: ~p~n", [CertFile]),
    io:format("[GatewaySup] Key file: ~p~n", [KeyFile]),

    %% Compute node_id and url early so workers can use them
    NodeId = get_node_id(Realm, Port),
    Url = get_url(Port),
    io:format("[GatewaySup] NodeId: ~s~n", [binary:encode_hex(NodeId)]),
    io:format("[GatewaySup] Endpoint URL: ~s~n", [Url]),

    %% Supervision strategy: rest_for_one
    %% - health fails → restart health, diagnostics, quic_server, gateway, workers_sup
    %% - diagnostics fails → restart diagnostics, quic_server, gateway, workers_sup
    %% - quic_server fails → restart quic_server, gateway, workers_sup
    %% - gateway fails → restart gateway, workers_sup
    %% - workers_sup fails → restart workers_sup only
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 60
    },

    %% Child 1: Health Check Server
    HealthSpec = #{
        id => macula_gateway_health,
        start => {macula_gateway_health, start_link, [[{health_port, HealthPort}]]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_gateway_health]
    },

    %% Child 2: Diagnostics Service
    DiagnosticsSpec = #{
        id => macula_gateway_diagnostics,
        start => {macula_gateway_diagnostics, start_link, [[{realm, Realm}]]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [macula_gateway_diagnostics]
    },

    %% Child 3: QUIC Server (starts without gateway PID)
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

    %% Child 4: Gateway (receives quic_server PID after it starts)
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

    %% Child 5: Workers Supervisor (supervises business logic workers)
    WorkersSupSpec = #{
        id => macula_gateway_workers_sup,
        start => {macula_gateway_workers_sup, start_link, [#{
            port => Port,
            realm => Realm,
            node_id => NodeId,
            url => Url
        }]},
        restart => permanent,
        shutdown => infinity,  % supervisor shutdown
        type => supervisor,
        modules => [macula_gateway_workers_sup]
    },

    Children = [HealthSpec, DiagnosticsSpec, QuicServerSpec, GatewaySpec, WorkersSupSpec],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Private helper functions
%%%===================================================================

%% @private
%% @doc Get certificate file path from opts or OS environment variable.
%% Falls back to TLS_CERT_FILE environment variable when not provided in opts.
get_cert_file(undefined) ->
    case os:getenv("TLS_CERT_FILE") of
        false -> "/opt/macula/certs/cert.pem";  % Default fallback
        CertFile -> CertFile
    end;
get_cert_file(CertFile) when is_list(CertFile) ->
    CertFile.

%% @private
%% @doc Get key file path from opts or OS environment variable.
%% Falls back to TLS_KEY_FILE environment variable when not provided in opts.
get_key_file(undefined) ->
    case os:getenv("TLS_KEY_FILE") of
        false -> "/opt/macula/certs/key.pem";  % Default fallback
        KeyFile -> KeyFile
    end;
get_key_file(KeyFile) when is_list(KeyFile) ->
    KeyFile.

%% @private
%% @doc Get node ID from HOSTNAME env var (set by Docker) or generate from {Realm, Port}.
%% Returns a 32-byte binary (raw binary for Kademlia, never hex-encoded).
%% MUST match macula_gateway:get_node_id/2 exactly!
%%
%% Priority:
%% 1. NODE_NAME env var (explicit, highest priority)
%% 2. HOSTNAME env var (Docker sets this to container hostname - unique per container)
%% 3. Fallback to {Realm, Port} only (NO MAC - MAC is shared across Docker containers)
get_node_id(Realm, Port) ->
    case os:getenv("NODE_NAME") of
        false ->
            %% No NODE_NAME, try HOSTNAME (Docker sets this to container hostname)
            case os:getenv("HOSTNAME") of
                false ->
                    %% No HOSTNAME either, use {Realm, Port} as last resort
                    %% Note: This WILL collide if multiple nodes share same realm+port
                    io:format("[GatewaySup] WARNING: No HOSTNAME or NODE_NAME set, using realm+port only~n"),
                    io:format("[GatewaySup] This may cause node_id collisions in Docker!~n"),
                    crypto:hash(sha256, term_to_binary({Realm, Port}));
                Hostname when is_list(Hostname) ->
                    %% Use HOSTNAME from Docker - unique per container
                    io:format("[GatewaySup] Using HOSTNAME-based node ID: ~s, Realm=~s, Port=~p~n",
                             [Hostname, Realm, Port]),
                    crypto:hash(sha256, term_to_binary({Realm, list_to_binary(Hostname), Port}))
            end;
        NodeName when is_list(NodeName) ->
            %% Use NODE_NAME from environment - hash it to get 32-byte binary
            io:format("[GatewaySup] Using NODE_NAME from environment: ~s~n", [NodeName]),
            crypto:hash(sha256, list_to_binary(NodeName))
    end.

%% @private
%% @doc Get endpoint URL for this gateway.
%% Constructs "https://hostname:port" using HOSTNAME env var or "localhost".
get_url(Port) ->
    Hostname = case os:getenv("HOSTNAME") of
        false -> "localhost";
        Host when is_list(Host) -> Host
    end,
    iolist_to_binary([<<"https://">>, list_to_binary(Hostname), <<":">>, integer_to_binary(Port)]).
