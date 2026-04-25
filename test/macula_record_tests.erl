%% @doc Eunit tests for macula_record (build/sign/verify, content keys).
-module(macula_record_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TYPE_STATION, 16#02).
-define(TTL_MS, 600_000).

%%%===================================================================
%%% build / canonical_body
%%%===================================================================

build_returns_map_with_required_fields_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:build(?TYPE_STATION, <<"hello">>, Kp, ?TTL_MS),
    ?assertEqual(?TYPE_STATION, maps:get(type, R)),
    ?assertEqual(<<"hello">>,   maps:get(payload, R)),
    ?assertEqual(macula_identity:public(Kp), maps:get(pubkey, R)),
    ?assertEqual(?TTL_MS,        maps:get(ttl_ms, R)),
    ?assert(is_binary(maps:get(sig, R))),
    ?assertEqual(64, byte_size(maps:get(sig, R))).

canonical_body_is_deterministic_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:build(?TYPE_STATION, <<"x">>, Kp, ?TTL_MS),
    B1 = macula_record:canonical_body(R),
    B2 = macula_record:canonical_body(R),
    ?assertEqual(B1, B2).

canonical_body_changes_when_payload_changes_test() ->
    Kp = macula_identity:generate(),
    R1 = macula_record:build(?TYPE_STATION, <<"a">>, Kp, ?TTL_MS),
    R2 = macula_record:build(?TYPE_STATION, <<"b">>, Kp, ?TTL_MS),
    ?assertNotEqual(macula_record:canonical_body(R1),
                    macula_record:canonical_body(R2)).

%%%===================================================================
%%% verify
%%%===================================================================

verify_returns_true_for_valid_record_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:build(?TYPE_STATION, <<"valid">>, Kp, ?TTL_MS),
    ?assertEqual(true, macula_record:verify(R)).

verify_returns_false_when_payload_tampered_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:build(?TYPE_STATION, <<"original">>, Kp, ?TTL_MS),
    Tampered = R#{payload := <<"tampered">>},
    ?assertEqual(false, macula_record:verify(Tampered)).

verify_returns_false_when_pubkey_swapped_test() ->
    KpA = macula_identity:generate(),
    KpB = macula_identity:generate(),
    R   = macula_record:build(?TYPE_STATION, <<"x">>, KpA, ?TTL_MS),
    Swapped = R#{pubkey := macula_identity:public(KpB)},
    ?assertEqual(false, macula_record:verify(Swapped)).

verify_returns_false_when_ttl_modified_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:build(?TYPE_STATION, <<"x">>, Kp, ?TTL_MS),
    Modified = R#{ttl_ms := ?TTL_MS + 1},
    ?assertEqual(false, macula_record:verify(Modified)).

verify_returns_false_for_malformed_test() ->
    ?assertEqual(false, macula_record:verify(#{})),
    ?assertEqual(false, macula_record:verify(not_a_map)).

%%%===================================================================
%%% key_of — content addressing
%%%===================================================================

key_of_returns_32_bytes_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:build(?TYPE_STATION, <<"x">>, Kp, ?TTL_MS),
    K  = macula_record:key_of(R),
    ?assert(is_binary(K)),
    ?assertEqual(32, byte_size(K)).

key_of_is_deterministic_test() ->
    Kp = macula_identity:generate(),
    R  = macula_record:build(?TYPE_STATION, <<"x">>, Kp, ?TTL_MS),
    ?assertEqual(macula_record:key_of(R), macula_record:key_of(R)).

key_of_differs_for_different_payloads_test() ->
    Kp = macula_identity:generate(),
    R1 = macula_record:build(?TYPE_STATION, <<"a">>, Kp, ?TTL_MS),
    R2 = macula_record:build(?TYPE_STATION, <<"b">>, Kp, ?TTL_MS),
    ?assertNotEqual(macula_record:key_of(R1), macula_record:key_of(R2)).

key_of_differs_for_different_signers_test() ->
    %% Same logical content under two signers must yield distinct
    %% keys — the signature participates in the hash so signing
    %% identity is part of the content address.
    KpA = macula_identity:generate(),
    KpB = macula_identity:generate(),
    R1  = macula_record:build(?TYPE_STATION, <<"x">>, KpA, ?TTL_MS),
    R2  = macula_record:build(?TYPE_STATION, <<"x">>, KpB, ?TTL_MS),
    ?assertNotEqual(macula_record:key_of(R1), macula_record:key_of(R2)).

key_of_differs_for_different_ttl_test() ->
    Kp = macula_identity:generate(),
    R1 = macula_record:build(?TYPE_STATION, <<"x">>, Kp, 1_000),
    R2 = macula_record:build(?TYPE_STATION, <<"x">>, Kp, 2_000),
    ?assertNotEqual(macula_record:key_of(R1), macula_record:key_of(R2)).
