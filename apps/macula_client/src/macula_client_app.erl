%%%-------------------------------------------------------------------
%%% @doc
%%% Macula SDK application behavior.
%%%
%%% Manages the SDK application lifecycle and supervision tree.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
start(_StartType, _StartArgs) ->
    macula_client_sup:start_link().

%% @private
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
