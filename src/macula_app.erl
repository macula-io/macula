%%%-------------------------------------------------------------------
%% @doc macula public API
%% @end
%%%-------------------------------------------------------------------

-module(macula_app).

-behaviour(application).

-export([start/2, stop/1]).

%% The primary logger filter that keeps private keys out of crash and diagnostics reports, see
%% macula_node_keys:redacted_log_event/2.
-define(KEY_REDACTION, macula_key_redaction).

%% A node starts only with exactly one known crypto profile in its
%% environment. There is no default; see macula_crypto_profile.
start(_StartType, _StartArgs) ->
    start_with_profile(macula_crypto_profile:configured()).

%% Stopping removes the application's own logger filter and no other.
stop(_State) ->
    _ = logger:remove_primary_filter(?KEY_REDACTION),
    ok.

%% internal functions

start_with_profile({ok, _Profile}) ->
    ok = macula_diagnostics:install_domain_filter(),
    ok = install_key_redaction(),
    macula_root:start_link();
start_with_profile({error, _Refusal} = Refused) ->
    Refused.

%% A filter an earlier start left behind is replaced, so the node holds one. Frames of this application's modules
%% show their arity only.
install_key_redaction() ->
    {ok, Modules} = application:get_key(macula, modules),
    _ = logger:remove_primary_filter(?KEY_REDACTION),
    Filter = {fun macula_node_keys:redacted_log_event/2, maps:from_keys(Modules, true)},
    logger:add_primary_filter(?KEY_REDACTION, Filter).
