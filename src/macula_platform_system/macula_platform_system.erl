%%%-------------------------------------------------------------------
%% @doc Macula Platform System Supervisor.
%%
%% This supervisor manages the platform layer services that provide
%% distributed coordination primitives for workloads.
%%
%% Platform Services:
%% - Leader Election (Raft-based via ra library)
%% - Shared State Management (CRDT-based replication)
%% - Coordination Primitives (locks, barriers, etc. - future)
%%
%% Architecture:
%% <pre>
%% macula_root
%% ├── [infrastructure: routing, bootstrap, gateway, peers]
%% └── macula_platform_system (this module)
%%     ├── macula_leader_election (Raft consensus)
%%     └── macula_shared_state (CRDT state)
%% </pre>
%%
%% @end
%%%-------------------------------------------------------------------

-module(macula_platform_system).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Config) ->
    io:format("[Platform] Starting Macula Platform System~n"),
    io:format("[Platform] Node ID: ~s~n", [maps:get(node_id, Config, <<"unknown">>)]),
    io:format("[Platform] Realm: ~s~n", [maps:get(realm, Config, <<"unknown">>)]),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    %% Platform services
    ChildSpecs = [
        %% Leader Election Service (Raft-based)
        #{
            id => macula_leader_election,
            start => {macula_leader_election, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_leader_election]
        }

        %% Shared State Service (CRDT-based) - TODO: Phase 2
        %% #{
        %%     id => macula_shared_state,
        %%     start => {macula_shared_state, start_link, [Config]},
        %%     restart => permanent,
        %%     shutdown => 5000,
        %%     type => worker,
        %%     modules => [macula_shared_state]
        %% }
    ],

    io:format("[Platform] Platform services initialized~n"),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
