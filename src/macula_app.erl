%%%-------------------------------------------------------------------
%% @doc macula public API
%% @end
%%%-------------------------------------------------------------------

-module(macula_app).

-behaviour(application).

-export([start/2, stop/1]).

%% A node starts only with exactly one known crypto profile in its
%% environment. There is no default; see macula_crypto_profile.
start(_StartType, _StartArgs) ->
    start_with_profile(macula_crypto_profile:configured()).

stop(_State) ->
    ok.

%% internal functions

start_with_profile({ok, _Profile}) ->
    ok = macula_diagnostics:install_domain_filter(),
    macula_root:start_link();
start_with_profile({error, _Refusal} = Refused) ->
    Refused.
