%%%-------------------------------------------------------------------
%% @doc macula public API
%% @end
%%%-------------------------------------------------------------------

-module(macula_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = macula_identity:check_puzzle_difficulty(),
    ok = macula_peering_inflight:check_limits(),
    ok = macula_diagnostics:install_domain_filter(),
    macula_root:start_link().

stop(_State) ->
    ok.

%% internal functions
