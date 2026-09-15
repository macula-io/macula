%%%-------------------------------------------------------------------
%% @doc macula public API
%% @end
%%%-------------------------------------------------------------------

-module(macula_app).

-behaviour(application).

-export([start/2, stop/1]).

%% A node starts only with exactly one known crypto profile in its
%% environment, and with a puzzle difficulty in range. There is no default
%% profile; see macula_crypto_profile.
start(_StartType, _StartArgs) ->
    start_with_profile(macula_crypto_profile:configured()).

%% Stopping removes no logger filter: the key redaction filter stays, since a process that holds a key can outlive the
%% application, and the filter changes nothing but key material.
stop(_State) ->
    ok.

%% internal functions

start_with_profile({ok, _Profile}) ->
    ok = macula_identity:check_puzzle_difficulty(),
    ok = macula_diagnostics:install_domain_filter(),
    ok = macula_node_keys:install_log_redaction(),
    macula_root:start_link();
start_with_profile({error, _Refusal} = Refused) ->
    Refused.
