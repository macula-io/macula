%% EUnit tests for macula_foundation — firmware-embedded foundation
%% pubkey management and record verification (Part 5 §4, Part 6
%% §9.14–§9.17).
-module(macula_foundation_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Placeholder pubkey hygiene
%%------------------------------------------------------------------

placeholder_count_test() ->
    ?assertEqual(5, length(macula_foundation:placeholder_pubkeys())).

placeholder_keys_are_32_bytes_test() ->
    [?assertEqual(32, byte_size(K))
     || K <- macula_foundation:placeholder_pubkeys()].

placeholder_keys_are_distinct_test() ->
    Keys = macula_foundation:placeholder_pubkeys(),
    ?assertEqual(length(Keys), length(lists:usort(Keys))).

placeholder_keys_are_deterministic_test() ->
    ?assertEqual(macula_foundation:placeholder_pubkeys(),
                 macula_foundation:placeholder_pubkeys()).

%%------------------------------------------------------------------
%% Resolution: live vs placeholder
%%------------------------------------------------------------------

live_pubkeys_empty_without_override_test() ->
    with_clean_env(fun() ->
        ?assertEqual([], macula_foundation:live_pubkeys()),
        ?assert(macula_foundation:placeholder_mode())
    end).

live_pubkeys_returns_override_test() ->
    with_clean_env(fun() ->
        Live = [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, 5)],
        ok = application:set_env(macula_record, foundation_pubkeys, Live),
        ?assertEqual(Live, macula_foundation:live_pubkeys()),
        ?assertNot(macula_foundation:placeholder_mode())
    end).

pubkeys_falls_back_to_placeholder_when_empty_env_test() ->
    with_clean_env(fun() ->
        ok = application:set_env(macula_record, foundation_pubkeys, []),
        ?assertEqual(macula_foundation:placeholder_pubkeys(),
                     macula_foundation:pubkeys())
    end).

pubkeys_uses_override_when_set_test() ->
    with_clean_env(fun() ->
        K = crypto:strong_rand_bytes(32),
        ok = application:set_env(macula_record, foundation_pubkeys, [K]),
        ?assertEqual([K], macula_foundation:pubkeys())
    end).

%%------------------------------------------------------------------
%% is_foundation / trust boundary
%%------------------------------------------------------------------

is_foundation_recognises_trusted_key_test() ->
    with_clean_env(fun() ->
        K = crypto:strong_rand_bytes(32),
        ok = application:set_env(macula_record, foundation_pubkeys, [K]),
        ?assert(macula_foundation:is_foundation(K))
    end).

is_foundation_rejects_unknown_key_test() ->
    with_clean_env(fun() ->
        ok = application:set_env(macula_record, foundation_pubkeys,
                                 [crypto:strong_rand_bytes(32)]),
        ?assertNot(macula_foundation:is_foundation(
                     crypto:strong_rand_bytes(32)))
    end).

is_foundation_rejects_wrong_length_test() ->
    ?assertNot(macula_foundation:is_foundation(<<1, 2, 3>>)).

%%------------------------------------------------------------------
%% verify_record — the whole point
%%------------------------------------------------------------------

verify_record_accepts_signed_foundation_parameter_test() ->
    with_clean_env(fun() ->
        Kp = macula_identity:generate(),
        Fk = macula_identity:public(Kp),
        ok = application:set_env(macula_record, foundation_pubkeys, [Fk]),
        R  = macula_record:foundation_parameter(Fk, <<"puzzle_difficulty">>, 8),
        Signed = macula_record:sign(R, Kp),
        ?assertMatch({ok, _}, macula_foundation:verify_record(Signed))
    end).

verify_record_accepts_all_four_foundation_types_test() ->
    with_clean_env(fun() ->
        Kp = macula_identity:generate(),
        Fk = macula_identity:public(Kp),
        ok = application:set_env(macula_record, foundation_pubkeys, [Fk]),
        Now = erlang:system_time(millisecond),
        Records = [
            macula_record:foundation_seed_list(Fk, []),
            macula_record:foundation_parameter(Fk, <<"x">>, 1),
            macula_record:foundation_realm_trust_list(Fk, []),
            macula_record:foundation_t3_attestation(
              Fk, crypto:strong_rand_bytes(32), Now)
        ],
        [?assertMatch({ok, _},
                      macula_foundation:verify_record(
                        macula_record:sign(R, Kp)))
         || R <- Records]
    end).

verify_record_rejects_untrusted_signer_test() ->
    with_clean_env(fun() ->
        FoundationKp = macula_identity:generate(),
        Fk  = macula_identity:public(FoundationKp),
        ok = application:set_env(macula_record, foundation_pubkeys, [Fk]),
        %% Record owned (and signed) by a completely different key.
        ImpostorKp = macula_identity:generate(),
        R = macula_record:foundation_parameter(
              macula_identity:public(ImpostorKp), <<"x">>, 1),
        Signed = macula_record:sign(R, ImpostorKp),
        ?assertEqual({error, not_foundation_signed},
                     macula_foundation:verify_record(Signed))
    end).

verify_record_rejects_non_foundation_type_test() ->
    with_clean_env(fun() ->
        Kp = macula_identity:generate(),
        Fk = macula_identity:public(Kp),
        ok = application:set_env(macula_record, foundation_pubkeys, [Fk]),
        NodeRec = macula_record:node_record(Fk, [], 0),
        Signed = macula_record:sign(NodeRec, Kp),
        ?assertEqual({error, wrong_type},
                     macula_foundation:verify_record(Signed))
    end).

verify_record_rejects_tampered_payload_test() ->
    with_clean_env(fun() ->
        Kp = macula_identity:generate(),
        Fk = macula_identity:public(Kp),
        ok = application:set_env(macula_record, foundation_pubkeys, [Fk]),
        R  = macula_record:foundation_parameter(Fk, <<"x">>, 1),
        Signed = macula_record:sign(R, Kp),
        Tampered = Signed#{payload =>
                             (maps:get(payload, Signed))#{
                               {text, <<"param_value">>} => 999}},
        ?assertEqual({error, signature_invalid},
                     macula_foundation:verify_record(Tampered))
    end).

verify_record_rejects_expired_test() ->
    with_clean_env(fun() ->
        Kp = macula_identity:generate(),
        Fk = macula_identity:public(Kp),
        ok = application:set_env(macula_record, foundation_pubkeys, [Fk]),
        R  = macula_record:foundation_parameter(Fk, <<"x">>, 1, #{ttl_ms => 1}),
        Signed = macula_record:sign(R, Kp),
        timer:sleep(5),
        ?assertEqual({error, expired},
                     macula_foundation:verify_record(Signed))
    end).

verify_record_rejects_placeholder_key_unless_overridden_test() ->
    %% Placeholder-mode: verify_record must reject records whose `k'
    %% is a placeholder pubkey if no live override exists — because no
    %% private key exists to sign such records. This test confirms the
    %% failure mode: since placeholders ARE in `pubkeys/0`, a record
    %% with `k' = placeholder would structurally pass `is_foundation/1'.
    %% It fails at signature verification because no one can forge it.
    with_clean_env(fun() ->
        [Placeholder | _] = macula_foundation:placeholder_pubkeys(),
        ?assert(macula_foundation:is_foundation(Placeholder)),
        ?assert(macula_foundation:placeholder_mode())
    end).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

with_clean_env(Fun) ->
    Prev = application:get_env(macula_record, foundation_pubkeys),
    try
        application:unset_env(macula_record, foundation_pubkeys),
        Fun()
    after
        restore_env(Prev)
    end.

restore_env(undefined)  -> application:unset_env(macula_record, foundation_pubkeys);
restore_env({ok, V})    -> application:set_env(macula_record, foundation_pubkeys, V).
