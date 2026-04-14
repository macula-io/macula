%% @doc Macula SDK facade.
%%
%% Thin re-export layer over the SDK sub-apps. Consumers (daemons, stubs,
%% station implementations) import `macula' for the stable public surface.
%%
%% Phase 1 surface: `version/0' only. `advertise/3', `call/3', `subscribe/3',
%% `publish/3', `connect/2' land as their dependencies stabilise
%% (Phases 3–5 per PLAN_MACULA_V2_PART7).
-module(macula).

-export([version/0]).

-spec version() -> binary().
version() ->
    <<"2.0.0-alpha.1">>.
