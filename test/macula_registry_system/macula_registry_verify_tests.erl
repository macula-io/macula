%%%-------------------------------------------------------------------
%%% @doc Registry Verify Tests
%%%
%%% Tests for macula_registry_verify Ed25519 signature operations.
%%% Covers keypair generation, signing, verification, key validation,
%%% encoding/decoding, and checksums.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_registry_verify_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Keypair generation tests
%%%===================================================================

generate_keypair_returns_valid_sizes_test() ->
    {PubKey, PrivKey} = macula_registry_verify:generate_keypair(),
    ?assertEqual(32, byte_size(PubKey)),
    ?assert(byte_size(PrivKey) =:= 32 orelse byte_size(PrivKey) =:= 64).

generate_keypair_unique_test() ->
    {Pub1, _Priv1} = macula_registry_verify:generate_keypair(),
    {Pub2, _Priv2} = macula_registry_verify:generate_keypair(),
    ?assertNotEqual(Pub1, Pub2).

%%%===================================================================
%%% Sign + verify roundtrip tests
%%%===================================================================

sign_verify_package_roundtrip_test() ->
    {PubKey, PrivKey} = macula_registry_verify:generate_keypair(),
    Manifest = <<"manifest data">>,
    Archive = <<"beam archive">>,
    {ok, Sig} = macula_registry_verify:sign_package(Manifest, Archive, PrivKey),
    ?assertEqual(ok, macula_registry_verify:verify_package(Manifest, Archive, Sig, PubKey)).

sign_data_verify_signature_roundtrip_test() ->
    {PubKey, PrivKey} = macula_registry_verify:generate_keypair(),
    Data = <<"arbitrary data to sign">>,
    Sig = macula_registry_verify:sign_data(Data, PrivKey),
    ?assertEqual(ok, macula_registry_verify:verify_signature(Data, Sig, PubKey)).

%%%===================================================================
%%% Verification failure tests
%%%===================================================================

verify_package_wrong_key_fails_test() ->
    {_Pub1, Priv1} = macula_registry_verify:generate_keypair(),
    {Pub2, _Priv2} = macula_registry_verify:generate_keypair(),
    Manifest = <<"manifest">>,
    Archive = <<"archive">>,
    {ok, Sig} = macula_registry_verify:sign_package(Manifest, Archive, Priv1),
    ?assertEqual({error, invalid_signature},
                 macula_registry_verify:verify_package(Manifest, Archive, Sig, Pub2)).

verify_package_tampered_manifest_fails_test() ->
    {PubKey, PrivKey} = macula_registry_verify:generate_keypair(),
    Manifest = <<"original manifest">>,
    Archive = <<"archive">>,
    {ok, Sig} = macula_registry_verify:sign_package(Manifest, Archive, PrivKey),
    ?assertEqual({error, invalid_signature},
                 macula_registry_verify:verify_package(<<"tampered">>, Archive, Sig, PubKey)).

verify_package_tampered_archive_fails_test() ->
    {PubKey, PrivKey} = macula_registry_verify:generate_keypair(),
    Manifest = <<"manifest">>,
    Archive = <<"original archive">>,
    {ok, Sig} = macula_registry_verify:sign_package(Manifest, Archive, PrivKey),
    ?assertEqual({error, invalid_signature},
                 macula_registry_verify:verify_package(Manifest, <<"tampered">>, Sig, PubKey)).

%%%===================================================================
%%% Different data produces different signatures
%%%===================================================================

different_data_different_signatures_test() ->
    {_PubKey, PrivKey} = macula_registry_verify:generate_keypair(),
    {ok, Sig1} = macula_registry_verify:sign_package(<<"m1">>, <<"a1">>, PrivKey),
    {ok, Sig2} = macula_registry_verify:sign_package(<<"m2">>, <<"a2">>, PrivKey),
    ?assertNotEqual(Sig1, Sig2).

deterministic_signatures_test() ->
    {_PubKey, PrivKey} = macula_registry_verify:generate_keypair(),
    Manifest = <<"same manifest">>,
    Archive = <<"same archive">>,
    {ok, Sig1} = macula_registry_verify:sign_package(Manifest, Archive, PrivKey),
    {ok, Sig2} = macula_registry_verify:sign_package(Manifest, Archive, PrivKey),
    ?assertEqual(Sig1, Sig2).

%%%===================================================================
%%% Key validation tests
%%%===================================================================

validate_public_key_correct_size_test() ->
    {PubKey, _PrivKey} = macula_registry_verify:generate_keypair(),
    ?assertEqual(ok, macula_registry_verify:validate_public_key(PubKey)).

validate_public_key_wrong_size_test() ->
    ?assertEqual({error, invalid_key}, macula_registry_verify:validate_public_key(<<"short">>)).

validate_public_key_not_binary_test() ->
    ?assertEqual({error, invalid_key}, macula_registry_verify:validate_public_key(12345)).

validate_private_key_correct_test() ->
    {_PubKey, PrivKey} = macula_registry_verify:generate_keypair(),
    ?assertEqual(ok, macula_registry_verify:validate_private_key(PrivKey)).

validate_private_key_wrong_size_test() ->
    ?assertEqual({error, invalid_key}, macula_registry_verify:validate_private_key(<<"bad">>)).

sign_package_invalid_key_test() ->
    ?assertMatch({error, _},
                 macula_registry_verify:sign_package(<<"m">>, <<"a">>, <<"bad">>)).

%%%===================================================================
%%% Encoding / decoding tests
%%%===================================================================

encode_decode_public_key_roundtrip_test() ->
    {PubKey, _PrivKey} = macula_registry_verify:generate_keypair(),
    Hex = macula_registry_verify:encode_public_key(PubKey),
    ?assert(is_binary(Hex)),
    ?assertEqual(64, byte_size(Hex)),
    {ok, Decoded} = macula_registry_verify:decode_public_key(Hex),
    ?assertEqual(PubKey, Decoded).

decode_public_key_invalid_hex_test() ->
    ?assertEqual({error, invalid_format},
                 macula_registry_verify:decode_public_key(<<"not hex!">>)).

decode_public_key_wrong_length_test() ->
    %% Valid hex but decodes to wrong key size (4 bytes, not 32)
    ?assertEqual({error, invalid_key},
                 macula_registry_verify:decode_public_key(<<"aabbccdd">>)).

%%%===================================================================
%%% Checksum tests
%%%===================================================================

compute_checksum_deterministic_test() ->
    Data = <<"some data">>,
    C1 = macula_registry_verify:compute_checksum(Data),
    C2 = macula_registry_verify:compute_checksum(Data),
    ?assertEqual(C1, C2),
    ?assertEqual(32, byte_size(C1)).

compute_checksum_different_data_test() ->
    C1 = macula_registry_verify:compute_checksum(<<"data1">>),
    C2 = macula_registry_verify:compute_checksum(<<"data2">>),
    ?assertNotEqual(C1, C2).
