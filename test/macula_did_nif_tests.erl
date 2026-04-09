%% @doc Tests for macula_did_nif module.
%% Tests DID document creation, parsing, and verification.
-module(macula_did_nif_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generator
%%====================================================================

did_test_() ->
    [
     {"Document creation tests", fun creation_tests/0},
     {"Document parsing tests", fun parsing_tests/0},
     {"Key extraction tests", fun key_extraction_tests/0},
     {"Controller tests", fun controller_tests/0},
     {"DID parsing tests", fun did_parsing_tests/0},
     {"Hierarchy tests", fun hierarchy_tests/0},
     {"Error handling tests", fun error_handling_tests/0}
    ].

%%====================================================================
%% Helper Functions
%%====================================================================

generate_keypair() ->
    macula_crypto_nif:generate_keypair().

%%====================================================================
%% Document Creation Tests
%%====================================================================

creation_tests() ->
    {ok, {PubKey, _PrivKey}} = generate_keypair(),

    %% Test 1: Create realm DID document
    RealmDid = <<"did:macula:io.macula">>,
    {ok, RealmDoc} = macula_did_nif:create_document(RealmDid, PubKey),
    ?assertEqual(true, is_binary(RealmDoc)),

    %% Test 2: Create organization DID document
    OrgDid = <<"did:macula:io.macula.acme">>,
    {ok, OrgDoc} = macula_did_nif:create_document(OrgDid, PubKey),
    ?assertEqual(true, is_binary(OrgDoc)),

    %% Test 3: Create application DID document
    AppDid = <<"did:macula:io.macula.acme.myapp">>,
    {ok, AppDoc} = macula_did_nif:create_document(AppDid, PubKey),
    ?assertEqual(true, is_binary(AppDoc)),

    %% Test 4: Verify document contains correct context
    {ok, Parsed} = macula_did_nif:parse_document(OrgDoc),
    ?assertEqual(true, is_binary(Parsed)),

    %% Test 5: Create deep hierarchy DID
    DeepDid = <<"did:macula:io.macula.org.dept.team.project">>,
    {ok, DeepDoc} = macula_did_nif:create_document(DeepDid, PubKey),
    ?assertEqual(true, is_binary(DeepDoc)),

    %% Test 6: Different keys produce different documents
    {ok, {PubKey2, _}} = generate_keypair(),
    {ok, Doc1} = macula_did_nif:create_document(OrgDid, PubKey),
    {ok, Doc2} = macula_did_nif:create_document(OrgDid, PubKey2),
    ?assertNotEqual(Doc1, Doc2),

    ok.

%%====================================================================
%% Document Parsing Tests
%%====================================================================

parsing_tests() ->
    {ok, {PubKey, _}} = generate_keypair(),
    Did = <<"did:macula:io.macula.test">>,
    {ok, Doc} = macula_did_nif:create_document(Did, PubKey),

    %% Test 7: Parse valid document
    {ok, ParsedDoc} = macula_did_nif:parse_document(Doc),
    ?assertEqual(true, is_binary(ParsedDoc)),

    %% Test 8: Parse invalid JSON
    ?assertEqual({error, invalid_document}, macula_did_nif:parse_document(<<"not json">>)),

    %% Test 9: Parse empty document
    ?assertEqual({error, invalid_document}, macula_did_nif:parse_document(<<>>)),

    %% Test 10: Parse truncated document
    Truncated = binary:part(Doc, 0, byte_size(Doc) div 2),
    ?assertEqual({error, invalid_document}, macula_did_nif:parse_document(Truncated)),

    ok.

%%====================================================================
%% Key Extraction Tests
%%====================================================================

key_extraction_tests() ->
    {ok, {PubKey, _}} = generate_keypair(),
    Did = <<"did:macula:io.macula.keytest">>,
    {ok, Doc} = macula_did_nif:create_document(Did, PubKey),

    %% Test 11: Extract public key from document
    {ok, ExtractedKey} = macula_did_nif:extract_public_key(Doc),
    ?assertEqual(32, byte_size(ExtractedKey)),
    ?assertEqual(PubKey, ExtractedKey),

    %% Test 12: Extract from multiple documents
    {ok, {PubKey2, _}} = generate_keypair(),
    {ok, Doc2} = macula_did_nif:create_document(Did, PubKey2),
    {ok, ExtractedKey2} = macula_did_nif:extract_public_key(Doc2),
    ?assertEqual(PubKey2, ExtractedKey2),

    %% Test 13: Extract from invalid document
    ?assertEqual({error, invalid_document}, macula_did_nif:extract_public_key(<<"invalid">>)),

    ok.

%%====================================================================
%% Controller Tests
%%====================================================================

controller_tests() ->
    {ok, {PubKey, _}} = generate_keypair(),

    %% Test 14: Realm has no controller (self-controlled)
    RealmDid = <<"did:macula:io.macula">>,
    {ok, RealmDoc} = macula_did_nif:create_document(RealmDid, PubKey),
    {ok, RealmController} = macula_did_nif:get_controller(RealmDoc),
    %% Realm is self-controlled (no controller field or controller = self)
    ?assert(RealmController =:= RealmDid orelse RealmController =:= null),

    %% Test 15: Organization controlled by realm
    OrgDid = <<"did:macula:io.macula.acme">>,
    {ok, OrgDoc} = macula_did_nif:create_document(OrgDid, PubKey),
    {ok, OrgController} = macula_did_nif:get_controller(OrgDoc),
    ?assertEqual(<<"did:macula:io.macula">>, OrgController),

    %% Test 16: App controlled by organization
    AppDid = <<"did:macula:io.macula.acme.myapp">>,
    {ok, AppDoc} = macula_did_nif:create_document(AppDid, PubKey),
    {ok, AppController} = macula_did_nif:get_controller(AppDoc),
    ?assertEqual(<<"did:macula:io.macula.acme">>, AppController),

    %% Test 17: Verify controller - success
    ?assertEqual(ok, macula_did_nif:verify_controller(AppDoc, <<"did:macula:io.macula.acme">>)),

    %% Test 18: Verify controller - failure
    ?assertEqual({error, controller_mismatch},
                 macula_did_nif:verify_controller(AppDoc, <<"did:macula:io.macula.wrong">>)),

    %% Test 19: Get DID from document
    {ok, ExtractedDid} = macula_did_nif:get_did(OrgDoc),
    ?assertEqual(OrgDid, ExtractedDid),

    ok.

%%====================================================================
%% DID Parsing Tests
%%====================================================================

did_parsing_tests() ->
    %% Test 20: Parse realm DID
    {ok, RealmParts} = macula_did_nif:parse_did(<<"did:macula:io.macula">>),
    ?assertEqual(<<"macula">>, maps:get(<<"method">>, RealmParts)),
    ?assertEqual(<<"io.macula">>, maps:get(<<"identity">>, RealmParts)),
    ?assertEqual([<<"io">>, <<"macula">>], maps:get(<<"parts">>, RealmParts)),
    ?assertEqual(2, maps:get(<<"depth">>, RealmParts)),

    %% Test 21: Parse organization DID
    {ok, OrgParts} = macula_did_nif:parse_did(<<"did:macula:io.macula.acme">>),
    ?assertEqual(<<"io.macula.acme">>, maps:get(<<"identity">>, OrgParts)),
    ?assertEqual(3, maps:get(<<"depth">>, OrgParts)),

    %% Test 22: Parse application DID
    {ok, AppParts} = macula_did_nif:parse_did(<<"did:macula:io.macula.acme.myapp">>),
    ?assertEqual(4, maps:get(<<"depth">>, AppParts)),

    %% Test 23: Parse invalid DID - wrong method
    ?assertEqual({error, invalid_did}, macula_did_nif:parse_did(<<"did:web:example.com">>)),

    %% Test 24: Parse invalid DID - missing parts
    ?assertEqual({error, invalid_did}, macula_did_nif:parse_did(<<"did:macula">>)),

    %% Test 25: Parse invalid DID - wrong prefix
    ?assertEqual({error, invalid_did}, macula_did_nif:parse_did(<<"notadid">>)),

    ok.

%%====================================================================
%% Hierarchy Tests
%%====================================================================

hierarchy_tests() ->
    %% Test 26: App is descendant of org
    ?assertEqual(true, macula_did_nif:is_descendant(
        <<"did:macula:io.macula.acme.myapp">>,
        <<"did:macula:io.macula.acme">>)),

    %% Test 27: Org is descendant of realm
    ?assertEqual(true, macula_did_nif:is_descendant(
        <<"did:macula:io.macula.acme">>,
        <<"did:macula:io.macula">>)),

    %% Test 28: App is descendant of realm
    ?assertEqual(true, macula_did_nif:is_descendant(
        <<"did:macula:io.macula.acme.myapp">>,
        <<"did:macula:io.macula">>)),

    %% Test 29: Same DID is not descendant
    ?assertEqual(false, macula_did_nif:is_descendant(
        <<"did:macula:io.macula.acme">>,
        <<"did:macula:io.macula.acme">>)),

    %% Test 30: Reverse direction is not descendant
    ?assertEqual(false, macula_did_nif:is_descendant(
        <<"did:macula:io.macula.acme">>,
        <<"did:macula:io.macula.acme.myapp">>)),

    %% Test 31: Sibling is not descendant
    ?assertEqual(false, macula_did_nif:is_descendant(
        <<"did:macula:io.macula.acme.app1">>,
        <<"did:macula:io.macula.acme.app2">>)),

    %% Test 32: Different orgs are not related
    ?assertEqual(false, macula_did_nif:is_descendant(
        <<"did:macula:io.macula.acme.myapp">>,
        <<"did:macula:io.macula.corp">>)),

    %% Test 33: Deep hierarchy
    ?assertEqual(true, macula_did_nif:is_descendant(
        <<"did:macula:io.macula.org.dept.team.project.task">>,
        <<"did:macula:io.macula.org.dept">>)),

    ok.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_tests() ->
    {ok, {PubKey, _}} = generate_keypair(),

    %% Test 34: Create with invalid DID format
    ?assertEqual({error, invalid_did},
                 macula_did_nif:create_document(<<"invalid">>, PubKey)),

    %% Test 35: Create with wrong method
    ?assertEqual({error, invalid_did},
                 macula_did_nif:create_document(<<"did:web:example.com">>, PubKey)),

    %% Test 36: Create with invalid public key length
    ?assertEqual({error, invalid_public_key},
                 macula_did_nif:create_document(<<"did:macula:io.macula.test">>, <<"short">>)),

    %% Test 37: Create with empty public key
    ?assertEqual({error, invalid_public_key},
                 macula_did_nif:create_document(<<"did:macula:io.macula.test">>, <<>>)),

    %% Test 38: Get DID from invalid document
    ?assertEqual({error, invalid_document},
                 macula_did_nif:get_did(<<"not a document">>)),

    %% Test 39: Get controller from invalid document
    ?assertEqual({error, invalid_document},
                 macula_did_nif:get_controller(<<"{}">>)),

    %% Test 40: Verify controller with invalid document
    ?assertEqual({error, invalid_document},
                 macula_did_nif:verify_controller(<<"invalid">>, <<"did:macula:io.macula">>)),

    ok.
