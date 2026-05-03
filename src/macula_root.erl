%%%-------------------------------------------------------------------
%% @doc Macula SDK Root Supervisor.
%%
%% Starts the SDK subsystems: MRI (resource identifiers), cert system
%% (identity), and distribution over mesh (optional).
%%
%% Server-side systems (gateway, DHT, RPC routing, PubSub routing,
%% peering, SWIM, etc.) live in macula-relay.
%%
%% @end
%%%-------------------------------------------------------------------
-module(macula_root).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Version = get_app_version(),

    ?LOG_INFO(""),
    ?LOG_INFO("═══════════════════════════════════════════════════════════════"),
    ?LOG_INFO("  Macula SDK ~s", [Version]),
    ?LOG_INFO("═══════════════════════════════════════════════════════════════"),
    ?LOG_INFO(""),

    SupFlags = #{strategy => one_for_one, intensity => 10, period => 5},

    ChildSpecs = observability_children() ++ [
        %% MRI Type Registry (type validation, custom type registration)
        #{
            id => macula_mri_registry,
            start => {macula_mri_registry, start_link, [[]]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% MRI ETS Storage (in-memory store/graph adapter)
        #{
            id => macula_mri_ets,
            start => {macula_mri_ets, start_link, [[]]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Local registry + dispatcher for streaming RPC (v1.5.0+).
        %% In-process pairing of client/server stream halves; the QUIC-
        %% backed cross-node path lands in Phase 2 of
        %% PLAN_MACULA_STREAMING.md.
        #{
            id => macula_stream_local,
            start => {macula_stream_local, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Distribution-over-mesh bridge supervisor.
        %% Started here (under the application supervisor) so it survives
        %% shell crashes and other transient process deaths in user code.
        #{
            id => macula_dist_bridge_sup,
            start => {macula_dist_bridge_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [macula_dist_bridge_sup]
        },

        %% Peering — per-peer connection state machines (CONNECT/HELLO
        %% handshake + frame send/receive). One macula_peering_conn
        %% gen_statem per peer, supervised by macula_peering_conn_sup
        %% under the macula_peering_sup top supervisor.
        #{
            id => macula_peering_sup,
            start => {macula_peering_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [macula_peering_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% @private
get_app_version() ->
    case application:get_key(macula, vsn) of
        {ok, Vsn} -> "v" ++ Vsn;
        undefined -> "v0.0.0"
    end.

%% Phase 4.1 — opt-in observability children. Read from application
%% env so embedded callers (Hecate, tests) can disable wholesale.
%% Defaults: metrics on, http off, packet_trace not started (it's a
%% pure module, no process needed).
observability_children() ->
    metrics_children(application:get_env(macula, metrics_enabled, true))
        ++ metrics_http_children(
              application:get_env(macula, metrics_http_enabled, false)).

metrics_children(false) -> [];
metrics_children(true) ->
    [#{
        id => macula_metrics,
        start => {macula_metrics, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    }].

metrics_http_children(false) -> [];
metrics_http_children(true) ->
    Port = application:get_env(macula, metrics_http_port, 9145),
    Bind = application:get_env(macula, metrics_http_bind, {127,0,0,1}),
    [#{
        id => macula_metrics_http,
        start => {macula_metrics_http, start_link, [#{port => Port, bind => Bind}]},
        restart => permanent,
        shutdown => 5000,
        type => worker
    }].
