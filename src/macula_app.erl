%%%-------------------------------------------------------------------
%% @doc macula public API
%% @end
%%%-------------------------------------------------------------------

-module(macula_app).

-behaviour(application).

-export([start/2, stop/1]).

%% A node starts only with exactly one known crypto profile in its
%% environment, without the removed node_identity_path setting (macula#76),
%% and with a puzzle difficulty in range. There is no default
%% profile; see macula_crypto_profile.
start(_StartType, _StartArgs) ->
    start_with_profile(macula_crypto_profile:configured()).

%% Stopping removes no logger filter: the key redaction filter stays, since a process that holds a key can outlive the
%% application, and the filter changes nothing but key material.
stop(_State) ->
    ok.

%% internal functions

start_with_profile({ok, _Profile}) ->
    start_with_identity_env(macula_node_keys:identity_env_checked());
start_with_profile({error, _Refusal} = Refused) ->
    Refused.

%% The node_identity_path env macula#76 removed refuses the start, naming what replaces it, rather than being ignored
%% while the node quietly takes the account's shared identity.
start_with_identity_env(ok) ->
    ok = macula_node_keys:check_puzzle_difficulty(),
    ok = macula_diagnostics:install_domain_filter(),
    ok = macula_node_keys:install_log_redaction(),
    macula_root:start_link();
start_with_identity_env({error, _Refusal} = Refused) ->
    Refused.
