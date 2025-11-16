%%%-------------------------------------------------------------------
%% @doc macula public API
%% @end
%%%-------------------------------------------------------------------

-module(macula_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    macula_root:start_link().

stop(_State) ->
    ok.

%% internal functions
