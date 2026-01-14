%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_cert module
%%%
%%% Tests Ed25519 DID-anchored certificate generation and verification.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cert_tests).

-include_lib("eunit/include/eunit.hrl").
-include("macula_cert.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

%% Test fixture for realm keypair
realm_keypair() ->
    macula_cert:generate_keypair().

%% Test fixture for instance keypair
instance_keypair() ->
    macula_cert:generate_keypair().

%%%===================================================================
%%% Keypair Generation Tests
%%%===================================================================

generate_keypair_test() ->
    {PubKey, PrivKey} = macula_cert:generate_keypair(),
    ?assertEqual(32, byte_size(PubKey)),
    ?assert(byte_size(PrivKey) =:= 64 orelse byte_size(PrivKey) =:= 32).

generate_keypair_unique_test() ->
    {PubKey1, _} = macula_cert:generate_keypair(),
    {PubKey2, _} = macula_cert:generate_keypair(),
    ?assertNotEqual(PubKey1, PubKey2).

%%%===================================================================
%%% Realm Certificate Tests
%%%===================================================================

generate_realm_cert_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),

    ?assertEqual(RealmDID, Cert#macula_cert.subject_did),
    ?assertEqual(RealmDID, Cert#macula_cert.issuer_did),
    ?assertEqual(PubKey, Cert#macula_cert.public_key),
    ?assertEqual(64, byte_size(Cert#macula_cert.signature)),
    ?assertEqual(16, byte_size(Cert#macula_cert.serial)).

generate_realm_cert_custom_validity_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,
    ValidityDays = 30,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey, ValidityDays),

    Now = erlang:system_time(second),
    ExpectedExpiry = Now + (ValidityDays * 86400),
    %% Allow 1 second tolerance
    ?assert(abs(Cert#macula_cert.not_after - ExpectedExpiry) =< 1).

verify_self_signed_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),
    ?assertEqual(ok, macula_cert:verify_self_signed(Cert)).

verify_self_signed_tampered_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),
    %% Tamper with the certificate
    TamperedCert = Cert#macula_cert{subject_cn = <<"tampered">>},
    ?assertEqual({error, invalid_signature}, macula_cert:verify_self_signed(TamperedCert)).

verify_self_signed_wrong_key_test() ->
    {PubKey1, PrivKey1} = realm_keypair(),
    {PubKey2, _PrivKey2} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey1, PrivKey1),
    %% Replace public key with different key
    TamperedCert = Cert#macula_cert{public_key = PubKey2},
    ?assertEqual({error, invalid_signature}, macula_cert:verify_self_signed(TamperedCert)).

%%%===================================================================
%%% Instance Certificate Tests
%%%===================================================================

generate_instance_cert_test() ->
    {RealmPub, RealmPriv} = realm_keypair(),
    {InstPub, _InstPriv} = instance_keypair(),
    RealmDID = <<"did:macula:io.example">>,
    InstanceDID = <<"did:macula:io.example.app.node01">>,

    {ok, RealmCert} = macula_cert:generate_realm_cert(RealmDID, RealmPub, RealmPriv),
    {ok, InstCert} = macula_cert:generate_instance_cert(InstanceDID, InstPub, RealmCert, RealmPriv),

    ?assertEqual(InstanceDID, InstCert#macula_cert.subject_did),
    ?assertEqual(RealmDID, InstCert#macula_cert.issuer_did),
    ?assertEqual(InstPub, InstCert#macula_cert.public_key).

generate_instance_cert_wrong_namespace_test() ->
    {RealmPub, RealmPriv} = realm_keypair(),
    {InstPub, _InstPriv} = instance_keypair(),
    RealmDID = <<"did:macula:io.example">>,
    WrongDID = <<"did:macula:io.other.app">>,  %% Different namespace

    {ok, RealmCert} = macula_cert:generate_realm_cert(RealmDID, RealmPub, RealmPriv),
    Result = macula_cert:generate_instance_cert(WrongDID, InstPub, RealmCert, RealmPriv),

    ?assertMatch({error, {did_not_under_realm, _, _}}, Result).

verify_instance_cert_test() ->
    {RealmPub, RealmPriv} = realm_keypair(),
    {InstPub, _InstPriv} = instance_keypair(),
    RealmDID = <<"did:macula:io.example">>,
    InstanceDID = <<"did:macula:io.example.app.node01">>,

    {ok, RealmCert} = macula_cert:generate_realm_cert(RealmDID, RealmPub, RealmPriv),
    {ok, InstCert} = macula_cert:generate_instance_cert(InstanceDID, InstPub, RealmCert, RealmPriv),

    ?assertEqual(ok, macula_cert:verify_cert(InstCert, RealmCert)).

verify_instance_cert_wrong_issuer_test() ->
    {RealmPub1, RealmPriv1} = realm_keypair(),
    {RealmPub2, RealmPriv2} = realm_keypair(),
    {InstPub, _InstPriv} = instance_keypair(),

    RealmDID1 = <<"did:macula:io.example1">>,
    RealmDID2 = <<"did:macula:io.example2">>,
    InstanceDID = <<"did:macula:io.example1.app">>,

    {ok, RealmCert1} = macula_cert:generate_realm_cert(RealmDID1, RealmPub1, RealmPriv1),
    {ok, RealmCert2} = macula_cert:generate_realm_cert(RealmDID2, RealmPub2, RealmPriv2),
    {ok, InstCert} = macula_cert:generate_instance_cert(InstanceDID, InstPub, RealmCert1, RealmPriv1),

    %% Try to verify against wrong realm
    ?assertMatch({error, {issuer_mismatch, _, _}}, macula_cert:verify_cert(InstCert, RealmCert2)).

%%%===================================================================
%%% Validity Tests
%%%===================================================================

is_valid_now_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),
    ?assert(macula_cert:is_valid_now(Cert)).

is_expired_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),
    ?assertNot(macula_cert:is_expired(Cert)).

is_expired_manually_expired_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),
    %% Manually set expiry to the past
    ExpiredCert = Cert#macula_cert{not_after = erlang:system_time(second) - 3600},
    ?assert(macula_cert:is_expired(ExpiredCert)).

%%%===================================================================
%%% Encoding/Decoding Tests
%%%===================================================================

encode_decode_roundtrip_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),
    Encoded = macula_cert:encode(Cert),
    {ok, Decoded} = macula_cert:decode(Encoded),

    ?assertEqual(Cert#macula_cert.subject_did, Decoded#macula_cert.subject_did),
    ?assertEqual(Cert#macula_cert.public_key, Decoded#macula_cert.public_key),
    ?assertEqual(Cert#macula_cert.signature, Decoded#macula_cert.signature).

to_map_from_map_roundtrip_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),
    Map = macula_cert:to_map(Cert),
    {ok, Restored} = macula_cert:from_map(Map),

    ?assertEqual(Cert#macula_cert.subject_did, Restored#macula_cert.subject_did),
    ?assertEqual(Cert#macula_cert.serial, Restored#macula_cert.serial).

decode_invalid_binary_test() ->
    ?assertEqual({error, invalid_certificate_format}, macula_cert:decode(<<"garbage">>)).

from_map_invalid_test() ->
    ?assertEqual({error, invalid_map}, macula_cert:from_map(not_a_map)).

%%%===================================================================
%%% Utility Function Tests
%%%===================================================================

did_to_cn_test() ->
    ?assertEqual(<<"example.io">>, macula_cert:did_to_cn(<<"did:macula:io.example">>)),
    ?assertEqual(<<"app.example.io">>, macula_cert:did_to_cn(<<"did:macula:io.example.app">>)),
    ?assertEqual(<<"node01.app.example.io">>,
                 macula_cert:did_to_cn(<<"did:macula:io.example.app.node01">>)).

cn_to_did_test() ->
    ?assertEqual(<<"did:macula:io.example">>, macula_cert:cn_to_did(<<"example.io">>)),
    ?assertEqual(<<"did:macula:io.example.app">>, macula_cert:cn_to_did(<<"app.example.io">>)).

extract_realm_did_test() ->
    ?assertEqual(<<"did:macula:io.example">>,
                 macula_cert:extract_realm_did(<<"did:macula:io.example.app.node01">>)),
    ?assertEqual(<<"did:macula:io.example">>,
                 macula_cert:extract_realm_did(<<"did:macula:io.example">>)).

generate_serial_test() ->
    Serial1 = macula_cert:generate_serial(),
    Serial2 = macula_cert:generate_serial(),
    ?assertEqual(16, byte_size(Serial1)),
    ?assertNotEqual(Serial1, Serial2).

canonical_form_deterministic_test() ->
    {PubKey, PrivKey} = realm_keypair(),
    RealmDID = <<"did:macula:io.example">>,

    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),
    Canonical1 = macula_cert:canonical_form(Cert),
    Canonical2 = macula_cert:canonical_form(Cert),

    ?assertEqual(Canonical1, Canonical2).

%%%===================================================================
%%% Certificate Request Tests
%%%===================================================================

sign_cert_request_test() ->
    {RealmPub, RealmPriv} = realm_keypair(),
    {InstPub, _InstPriv} = instance_keypair(),
    RealmDID = <<"did:macula:io.example">>,
    InstanceDID = <<"did:macula:io.example.app">>,

    {ok, RealmCert} = macula_cert:generate_realm_cert(RealmDID, RealmPub, RealmPriv),

    Request = #macula_cert_request{
        subject_did = InstanceDID,
        subject_cn = <<"app.example.io">>,
        public_key = InstPub,
        validity_days = 30
    },

    {ok, InstCert} = macula_cert:sign_cert_request(Request, RealmCert, RealmPriv),
    ?assertEqual(InstanceDID, InstCert#macula_cert.subject_did),
    ?assertEqual(ok, macula_cert:verify_cert(InstCert, RealmCert)).
