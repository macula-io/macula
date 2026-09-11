%% @doc Foundation trust anchor: the key ids of the foundation keys a node trusts, and the check of a foundation record
%% against them.
%%
%% The Foundation signs Tier A seed lists, protocol parameters, realm trust lists and T3 attestations (Part 6 §9.14 to
%% §9.17). A foundation record carries one foundation signature in the signed-object format, by a key whose purpose is
%% foundation. A node trusts foundation keys by key id: SHA-256 over MACULA-KEY-ID-V1, the profile and the key as
%% carried (macula_node_keys:key_id/2). The trusted key ids are the root of trust for the Tier A bootstrap path
%% (Part 5 §4).
%%
%% == Configuration ==
%%
%% Production deployments set the trusted key ids in the macula application environment, under foundation_key_ids,
%% before any Tier A record is trusted. Replacing a key id takes a configuration or firmware update; records signed by
%% a retired key stay valid until they expire.
%%
%% == Placeholders ==
%%
%% Without that setting, key_ids/0 returns five deterministic placeholders, SHA-256 digests of fixed labels. No key
%% derives to a placeholder, so no record verifies as signed by one. Callers that must work only against configured
%% keys use live_key_ids/0, which returns the configured key ids or an empty list, never the placeholders.
%%
%% Reference: plans/PLAN_MACULA_V2_PART5_BOOTSTRAP.md §4, §12; plans/PLAN_MACULA_V2_PART6_PROTOCOL.md §9.14 to §9.17;
%% plans/PLAN_POST_QUANTUM_SECURITY.md.
-module(macula_foundation).

-export([
    key_ids/0,
    live_key_ids/0,
    is_foundation/1,
    verify_record/2,
    verify_record/3,
    placeholder_key_ids/0,
    placeholder_mode/0
]).

-export_type([key_id/0, verify_error/0]).

-type key_id() :: <<_:256>>.

-type verify_error() :: record_too_large | malformed | signature_invalid | alg_mismatch | not_yet_valid | expired
                      | key_id_mismatch | wrong_type | not_foundation_signed.

-define(ENV_KEY, foundation_key_ids).
-define(PLACEHOLDER_COUNT, 5).
-define(PLACEHOLDER_LABEL_PREFIX, "macula-v2-foundation-placeholder-").
-define(FOUNDATION_TYPES, [16#0D, 16#0E, 16#0F, 16#10]).

%%------------------------------------------------------------------
%% Trusted key ids
%%------------------------------------------------------------------

%% @doc The trusted foundation key ids: the configured ones when foundation_key_ids is set and not empty, the
%% placeholders otherwise, for development and tests only.
-spec key_ids() -> [key_id()].
key_ids() ->
    configured_or_placeholders(live_key_ids()).

configured_or_placeholders([]) -> placeholder_key_ids();
configured_or_placeholders(KeyIds) -> KeyIds.

%% @doc The configured foundation key ids, or an empty list; never the placeholders. Production bootstrap paths use it
%% to refuse to trust a record that only a placeholder would admit.
-spec live_key_ids() -> [key_id()].
live_key_ids() ->
    configured(application:get_env(macula, ?ENV_KEY)).

configured({ok, KeyIds}) when is_list(KeyIds) -> KeyIds;
configured(_Unset) -> [].

%% @doc Whether a value is one of the trusted foundation key ids.
-spec is_foundation(term()) -> boolean().
is_foundation(<<_:256>> = KeyId) ->
    lists:member(KeyId, key_ids());
is_foundation(_NotAKeyId) ->
    false.

%%------------------------------------------------------------------
%% Foundation records
%%------------------------------------------------------------------

%% @doc Verify a foundation record, as its wire form or its {key, tbs, signature} map, under the verifier's profile.
%% The record is accepted when it verifies (macula_record:verify/3), its type is a foundation type, and its signer's
%% key id is trusted. Refusals are returned in that order, never raised.
-spec verify_record(binary() | map(), macula_crypto_profile:profile()) ->
        {ok, macula_record:m_record()} | {error, verify_error()}.
verify_record(Signed, Profile) ->
    verify_record(Signed, Profile, erlang:system_time(millisecond)).

%% @doc As verify_record/2, at the verifier's clock Now, in milliseconds.
-spec verify_record(binary() | map(), macula_crypto_profile:profile(), integer()) ->
        {ok, macula_record:m_record()} | {error, verify_error()}.
verify_record(Signed, Profile, Now) ->
    foundation_record(macula_record:verify(Signed, Profile, Now)).

foundation_record({ok, #{type := Type} = Record}) ->
    typed(lists:member(Type, ?FOUNDATION_TYPES), Record);
foundation_record({error, _} = Refusal) ->
    Refusal.

typed(true, #{key_id := KeyId} = Record) -> signed_by_foundation(is_foundation(KeyId), Record);
typed(false, _Record) -> {error, wrong_type}.

signed_by_foundation(true, Record) -> {ok, Record};
signed_by_foundation(false, _Record) -> {error, not_foundation_signed}.

%%------------------------------------------------------------------
%% Placeholders
%%------------------------------------------------------------------

%% @doc The placeholder key ids: SHA-256 digests of fixed labels, which no key derives to.
-spec placeholder_key_ids() -> [key_id()].
placeholder_key_ids() ->
    [crypto:hash(sha256, [?PLACEHOLDER_LABEL_PREFIX, integer_to_list(I)]) || I <- lists:seq(1, ?PLACEHOLDER_COUNT)].

%% @doc Whether the node trusts the placeholder key ids, because no foundation key id is configured. Production code
%% refuses to bootstrap while this is true.
-spec placeholder_mode() -> boolean().
placeholder_mode() ->
    live_key_ids() =:= [].
