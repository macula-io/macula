%%%-------------------------------------------------------------------
%% @doc macula top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(macula_sup).

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
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 5
    },

    %% Conditionally start gateway based on configuration
    ChildSpecs = maybe_start_gateway(),

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

%% @private
%% @doc Conditionally start the gateway based on configuration.
%% Set {start_gateway, false} in config to disable auto-start (for embedded use).
maybe_start_gateway() ->
    case application:get_env(macula, start_gateway, true) of
        true ->
            %% Read gateway configuration from environment or config
            Port = get_gateway_port(),
            Realm = get_gateway_realm(),
            HealthPort = get_health_port(),

            io:format("Starting Macula Gateway on port ~p (realm: ~s)~n", [Port, Realm]),
            io:format("Starting health check server on port ~p~n", [HealthPort]),

            [
                %% Health check server (must start first)
                #{
                    id => macula_gateway_health,
                    start => {macula_gateway_health, start_link, [[{health_port, HealthPort}]]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [macula_gateway_health]
                },
                %% Diagnostics service
                #{
                    id => macula_gateway_diagnostics,
                    start => {macula_gateway_diagnostics, start_link, [[{realm, Realm}]]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [macula_gateway_diagnostics]
                },
                %% Main gateway
                #{
                    id => macula_gateway,
                    start => {macula_gateway, start_link, [[{port, Port}, {realm, Realm}]]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [macula_gateway]
                }
            ];
        false ->
            io:format("Gateway auto-start disabled (embedded mode)~n"),
            []
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
    case os:getenv("GATEWAY_REALM") of
        false ->
            application:get_env(macula, gateway_realm, <<"be.cortexiq.energy">>);
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
