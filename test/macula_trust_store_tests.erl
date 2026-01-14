%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_trust_store module
%%%
%%% Tests ETS-based trust store for realm certificates.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_trust_store_tests).

-include_lib("eunit/include/eunit.hrl").
-include("macula_cert.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Main test fixture using foreach for setup/cleanup
trust_store_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_add_trusted_realm/1,
      fun test_is_trusted_true/1,
      fun test_is_trusted_false/1,
      fun test_remove_trusted_realm/1,
      fun test_remove_nonexistent/1,
      fun test_list_trusted/1,
      fun test_verify_instance_cert/1,
      fun test_verify_unknown_issuer/1,
      fun test_clear_all/1,
      fun test_count/1,
      fun test_reject_invalid_cert/1,
      fun test_reject_mismatched_did/1,
      fun test_add_trusted_with_notes/1
     ]}.

setup() ->
    %% Start the trust store
    {ok, Pid} = macula_trust_store:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the trust store
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(Pid);
        false ->
            ok
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

create_realm_cert(RealmDID) ->
    {PubKey, PrivKey} = macula_cert:generate_keypair(),
    {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),
    {Cert, PrivKey}.

create_instance_cert(InstanceDID, RealmCert, RealmPrivKey) ->
    {InstPub, InstPriv} = macula_cert:generate_keypair(),
    {ok, InstCert} = macula_cert:generate_instance_cert(
        InstanceDID, InstPub, RealmCert, RealmPrivKey
    ),
    {InstCert, InstPriv}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_add_trusted_realm(_Pid) ->
    {"add and retrieve trusted realm", fun() ->
        RealmDID = <<"did:macula:io.example">>,
        {Cert, _PrivKey} = create_realm_cert(RealmDID),

        ?assertEqual(ok, macula_trust_store:add_trusted_realm(RealmDID, Cert)),

        {ok, Retrieved} = macula_trust_store:get_realm_cert(RealmDID),
        ?assertEqual(Cert#macula_cert.subject_did, Retrieved#macula_cert.subject_did),
        ?assertEqual(Cert#macula_cert.public_key, Retrieved#macula_cert.public_key)
    end}.

test_is_trusted_true(_Pid) ->
    {"is_trusted returns true for added realm", fun() ->
        RealmDID = <<"did:macula:io.trusted">>,
        {Cert, _PrivKey} = create_realm_cert(RealmDID),

        ok = macula_trust_store:add_trusted_realm(RealmDID, Cert),
        ?assert(macula_trust_store:is_trusted(RealmDID))
    end}.

test_is_trusted_false(_Pid) ->
    {"is_trusted returns false for unknown realm", fun() ->
        UnknownDID = <<"did:macula:io.unknown">>,
        ?assertNot(macula_trust_store:is_trusted(UnknownDID))
    end}.

test_remove_trusted_realm(_Pid) ->
    {"remove trusted realm", fun() ->
        RealmDID = <<"did:macula:io.toremove">>,
        {Cert, _PrivKey} = create_realm_cert(RealmDID),

        ok = macula_trust_store:add_trusted_realm(RealmDID, Cert),
        ?assert(macula_trust_store:is_trusted(RealmDID)),

        ok = macula_trust_store:remove_trusted_realm(RealmDID),
        ?assertNot(macula_trust_store:is_trusted(RealmDID))
    end}.

test_remove_nonexistent(_Pid) ->
    {"remove non-existent realm returns error", fun() ->
        NonexistentDID = <<"did:macula:io.nonexistent">>,
        ?assertEqual({error, not_found}, macula_trust_store:remove_trusted_realm(NonexistentDID))
    end}.

test_list_trusted(_Pid) ->
    {"list trusted realms", fun() ->
        RealmDID1 = <<"did:macula:io.realm1">>,
        RealmDID2 = <<"did:macula:io.realm2">>,
        {Cert1, _} = create_realm_cert(RealmDID1),
        {Cert2, _} = create_realm_cert(RealmDID2),

        ok = macula_trust_store:add_trusted_realm(RealmDID1, Cert1),
        ok = macula_trust_store:add_trusted_realm(RealmDID2, Cert2),

        Trusted = macula_trust_store:list_trusted(),
        ?assert(lists:member(RealmDID1, Trusted)),
        ?assert(lists:member(RealmDID2, Trusted))
    end}.

test_verify_instance_cert(_Pid) ->
    {"verify instance cert through trust store", fun() ->
        RealmDID = <<"did:macula:io.verifytest">>,
        InstanceDID = <<"did:macula:io.verifytest.app">>,

        {RealmCert, RealmPrivKey} = create_realm_cert(RealmDID),
        {InstCert, _InstPrivKey} = create_instance_cert(InstanceDID, RealmCert, RealmPrivKey),

        %% Add realm to trust store
        ok = macula_trust_store:add_trusted_realm(RealmDID, RealmCert),

        %% Verify instance through trust store
        ?assertEqual(ok, macula_trust_store:verify_instance_cert(InstCert))
    end}.

test_verify_unknown_issuer(_Pid) ->
    {"verify instance cert fails for unknown issuer", fun() ->
        RealmDID = <<"did:macula:io.unknownissuer">>,
        InstanceDID = <<"did:macula:io.unknownissuer.app">>,

        {RealmCert, RealmPrivKey} = create_realm_cert(RealmDID),
        {InstCert, _InstPrivKey} = create_instance_cert(InstanceDID, RealmCert, RealmPrivKey),

        %% DON'T add realm to trust store
        ?assertMatch({error, {issuer_not_trusted, _}},
                     macula_trust_store:verify_instance_cert(InstCert))
    end}.

test_clear_all(_Pid) ->
    {"clear all trusted realms", fun() ->
        RealmDID1 = <<"did:macula:io.clear1">>,
        RealmDID2 = <<"did:macula:io.clear2">>,
        {Cert1, _} = create_realm_cert(RealmDID1),
        {Cert2, _} = create_realm_cert(RealmDID2),

        ok = macula_trust_store:add_trusted_realm(RealmDID1, Cert1),
        ok = macula_trust_store:add_trusted_realm(RealmDID2, Cert2),
        ?assert(macula_trust_store:count() >= 2),

        ok = macula_trust_store:clear_all(),
        ?assertEqual(0, macula_trust_store:count())
    end}.

test_count(_Pid) ->
    {"count returns correct number", fun() ->
        %% Start fresh
        ok = macula_trust_store:clear_all(),
        ?assertEqual(0, macula_trust_store:count()),

        RealmDID = <<"did:macula:io.counttest">>,
        {Cert, _} = create_realm_cert(RealmDID),

        ok = macula_trust_store:add_trusted_realm(RealmDID, Cert),
        ?assertEqual(1, macula_trust_store:count())
    end}.

test_reject_invalid_cert(_Pid) ->
    {"reject invalid self-signed cert", fun() ->
        RealmDID = <<"did:macula:io.invalid">>,
        {PubKey, PrivKey} = macula_cert:generate_keypair(),
        {ok, Cert} = macula_cert:generate_realm_cert(RealmDID, PubKey, PrivKey),

        %% Tamper with the signature
        TamperedCert = Cert#macula_cert{signature = crypto:strong_rand_bytes(64)},

        ?assertEqual({error, invalid_signature},
                     macula_trust_store:add_trusted_realm(RealmDID, TamperedCert))
    end}.

test_reject_mismatched_did(_Pid) ->
    {"reject mismatched DID", fun() ->
        RealmDID1 = <<"did:macula:io.mismatch1">>,
        RealmDID2 = <<"did:macula:io.mismatch2">>,
        {Cert, _PrivKey} = create_realm_cert(RealmDID1),

        %% Try to add with wrong DID
        ?assertMatch({error, {did_mismatch, _, _}},
                     macula_trust_store:add_trusted_realm(RealmDID2, Cert))
    end}.

test_add_trusted_with_notes(_Pid) ->
    {"add trusted with notes", fun() ->
        RealmDID = <<"did:macula:io.withnotes">>,
        {Cert, _PrivKey} = create_realm_cert(RealmDID),
        Notes = <<"Verified via out-of-band channel on 2026-01-14">>,

        ok = macula_trust_store:add_trusted_realm(RealmDID, Cert, Notes),
        ?assert(macula_trust_store:is_trusted(RealmDID))
    end}.
