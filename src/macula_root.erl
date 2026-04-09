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

    ChildSpecs = [
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
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% @private
get_app_version() ->
    case application:get_key(macula, vsn) of
        {ok, Vsn} -> "v" ++ Vsn;
        undefined -> "v0.0.0"
    end.
