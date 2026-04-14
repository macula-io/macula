%% @doc Application callback for macula_peering.
-module(macula_peering_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    macula_peering_sup:start_link().

stop(_State) ->
    ok.
