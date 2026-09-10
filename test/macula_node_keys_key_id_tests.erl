%% EUnit tests for key ids (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Signed objects) and for realm, org and foundation
%% keys. A key that is not an identity key is named by SHA-256 over the label MACULA-KEY-ID-V1, a zero byte, the
%% length and name of the profile, and the key as carried; an identity key is named by its node_id.
-module(macula_node_keys_key_id_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).

%%------------------------------------------------------------------
%% Reference vectors, computed in Python (2026-09-11)
%%------------------------------------------------------------------

pq_pure_vector_test() ->
    ?assertEqual(hex(<<"8bb084a6409125fdd96c2da8c9a2d7accb4507d05f81e4c34424f35aa26c3290">>),
                 macula_node_keys:key_id(test_key(2592), pq_pure)).

pq_hybrid_vector_test() ->
    ?assertEqual(hex(<<"0f53c958ae40d81c719ec3d90475d12c254fc7bfdecb59cd7a273ed1fdfa0f81">>),
                 macula_node_keys:key_id(test_key(3118), pq_hybrid)).

profile_separates_key_ids_over_the_same_key_bytes_test() ->
    ?assertEqual(hex(<<"90792cb738943aed2706f7577b0e9d5e96c023dec6ebf6dca90b2221beee606b">>),
                 macula_node_keys:key_id(test_key(2592), pq_hybrid)).

label_separates_a_key_id_from_a_node_id_test() ->
    ?assertNotEqual(macula_node_keys:node_id(test_key(2592), pq_pure),
                    macula_node_keys:key_id(test_key(2592), pq_pure)).

unknown_profile_is_refused_test() ->
    ?assertError(function_clause, macula_node_keys:key_id(test_key(2592), rsa_only)).

%%------------------------------------------------------------------
%% Key ids of keys
%%------------------------------------------------------------------

identity_key_id_is_its_node_id_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    ?assertEqual(macula_node_keys:node_id(Key), {ok, macula_node_keys:key_id(Key)}).

other_purposes_take_the_key_id_of_their_carried_key_test_() ->
    [?_test(begin
         {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
         ?assertEqual(macula_node_keys:key_id(macula_node_keys:public_key(Key), pq_pure),
                      macula_node_keys:key_id(Key))
     end)
     || Purpose <- [connect, tls, realm, org, foundation]].

%%------------------------------------------------------------------
%% Realm, org and foundation keys
%%------------------------------------------------------------------

us_realm_org_and_foundation_keys_are_one_mldsa87_pair_test_() ->
    [?_assertMatch({ok, #{purpose := Purpose, profile := pq_pure, components := [#{algorithm := mldsa87}]}},
                   macula_node_keys:generate(Purpose, pq_pure))
     || Purpose <- [realm, org, foundation]].

eu_realm_org_and_foundation_keys_pair_mldsa87_with_rsa_pss_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        [?assertMatch({ok, #{components := [#{algorithm := mldsa87}, #{algorithm := rsa_pss}]}},
                      macula_node_keys:generate(Purpose, pq_hybrid))
         || Purpose <- [realm, org, foundation]]
    end}.

realm_org_and_foundation_keys_have_no_node_id_test_() ->
    [?_test(begin
         {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
         ?assertEqual({error, not_an_identity_key}, macula_node_keys:node_id(Key))
     end)
     || Purpose <- [realm, org, foundation]].

realm_org_and_foundation_keys_take_no_puzzle_test_() ->
    [?_assertEqual({error, not_an_identity_key},
                   macula_node_keys:generate(Purpose, pq_pure, #{puzzle_difficulty => 1}))
     || Purpose <- [realm, org, foundation]].

realm_org_and_foundation_keys_survive_save_and_load_test_() ->
    [?_test(begin
         {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
         ?assertEqual({ok, Key}, save_and_load(Key, Purpose, pq_pure))
     end)
     || Purpose <- [realm, org, foundation]].

a_realm_key_loaded_as_an_org_key_is_refused_test() ->
    {ok, Key} = macula_node_keys:generate(realm, pq_pure),
    ?assertEqual({error, {wrong_purpose, realm}}, save_and_load(Key, org, pq_pure)).

an_org_key_signs_and_verifies_like_an_identity_key_test() ->
    {ok, Key} = macula_node_keys:generate(org, pq_pure),
    Signature = macula_node_keys:sign(<<"message">>, Key),
    ?assert(macula_node_keys:verify(<<"message">>, Signature, macula_node_keys:public_key(Key), pq_pure)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

test_key(Length) ->
    << <<(I rem 256)>> || I <- lists:seq(0, Length - 1) >>.

hex(Hex) ->
    binary:decode_hex(Hex).

save_and_load(Key, Purpose, Profile) ->
    Path = mktmp("node.key"),
    ok = macula_node_keys:save(Path, Key),
    macula_node_keys:load(Path, Purpose, Profile).

mktmp(Name) ->
    Dir = filename:join([
        "/tmp",
        "macula_node_keys_key_id_tests",
        integer_to_list(erlang:unique_integer([positive]))
    ]),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    filename:join(Dir, Name).
