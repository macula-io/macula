%% EUnit tests for DHT storage keys (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Storage keys): a node record under the
%% signer's node_id, every other record under SHA-256("MACULA-PQ-STORAGE-KEY-V1" || 0x00 || type || fields), with a
%% 32-byte id as it is and any other field length-prefixed. The vectors were computed in Python (2026-09-11) with
%% realm id 0x11..., member 0x22..., org key id 0x44..., advertiser 0x55..., station 0x77... and content id
%% 02 55 88....
-module(macula_record_storage_key_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Reference vectors
%%------------------------------------------------------------------

procedure_key_vector_test() ->
    ?assertEqual(hex(<<"efbcd93463f2cd8c2c00fd481ef4f2ad2948af8476f505d4e3aeb13b9e69e2bc">>),
                 macula_record:procedure_key(fill(16#11), <<"acme/get_forecast_v1">>)).

content_key_vector_test() ->
    ?assertEqual(hex(<<"c3860b4b53a5ad2ab73ec3c26ec8c228e46f2f0c123732b257ea5c8b93351138">>),
                 macula_record:content_key(mcid())).

station_endpoint_key_vector_test() ->
    ?assertEqual(hex(<<"745798b5c27ad23602e034732f508ab43dd623371133fb5425ee4f600e09bc6c">>),
                 macula_record:station_endpoint_key(fill(16#77))).

org_directory_key_vector_test() ->
    ?assertEqual(hex(<<"a0c45a66de0f7000a76726e424add18ef32014cbd106e9c72e8c8425c1282924">>),
                 macula_record:org_directory_key(fill(16#11), <<"acme">>)).

procedure_delegation_key_vector_test() ->
    ?assertEqual(hex(<<"011574703bed4c79df51f4cc53afcebd79a4401518b830aa12587950d1edfda4">>),
                 macula_record:procedure_delegation_key(fill(16#44), fill(16#55))).

records_named_by_their_payload_match_the_vectors_test() ->
    RealmId = fill(16#11),
    Cases = [{"5ae659563813ed3f41b6ce4360c0397ddd46878fec0015beab4b2f7661a9fe29",
              macula_record:realm_directory(RealmId, <<"io.macula">>, fill(1))},
             {"d92cb1913d63777c2d7a4e7cb2177a978ecff041c00bb9567cf9fafd1e812341",
              macula_record:realm_stations(RealmId, [])},
             {"d93b63bd8a0c04442b094aa046be12eb4e72e34156913e2e413904c580bfdc3b",
              macula_record:realm_member_endorsement(RealmId, #{realm => RealmId, member_node => fill(16#22),
                                                                roles => []})},
             {"efbcd93463f2cd8c2c00fd481ef4f2ad2948af8476f505d4e3aeb13b9e69e2bc",
              macula_record:procedure_advertisement(fill(16#55), RealmId, <<"acme/get_forecast_v1">>, fill(16#77))},
             {"51c6fc4b520eed65bb556522be043366a888fb7c9a2d0915392378ca0adb8c31",
              macula_record:foundation_t3_attestation(fill(16#77), 1789000000000)},
             {"c3860b4b53a5ad2ab73ec3c26ec8c228e46f2f0c123732b257ea5c8b93351138",
              macula_record:content_announcement(fill(16#55), mcid(), #{realm_id => <<7:256>>, serving_station => <<9:256>>, procedure => <<"acme/content_v1_x">>})},
             {"a0c45a66de0f7000a76726e424add18ef32014cbd106e9c72e8c8425c1282924",
              macula_record:org_directory(RealmId, <<"acme">>, fill(16#44))}],
    [?assertEqual(hex(list_to_binary(Hex)), macula_record:storage_key(Record)) || {Hex, Record} <- Cases].

%% Records named by their signer, with key id 0x33 repeated. The vectors were computed in Python (2026-09-15) from the
%% design's formula, by a script that first reproduces procedure_key_vector_test's vector, and macula-go pins the same
%% rows, so a change of field order or prefix in either stack fails here or there. Each row names its vector.
records_named_by_their_signer_match_the_vectors_test() ->
    Signer = fill(16#33),
    Cases = [{"76f0c1daf5c481e36fe610398b395a8bca902297e292e1a21e1b161b01007c47",
              macula_record:foundation_seed_list([])},
             {"a5aad8cf95e5bbd30728b699bd8f1ed4434edc6b3cfa7fb508ec18a9a104cc29",
              macula_record:foundation_parameter(<<"max_hops">>, 8)},
             {"7c37bc0dac3384f930fe1c367743b0fb5ebedead7ac8d1e2590cdc94c92d3399",
              macula_record:foundation_realm_trust_list([])},
             {"a860117ee3bbb136d84a9638bb8221945de34588d8fbd2e67b7d9458b2aaffa5",
              macula_record:envelope(16#20, #{}, #{})},
             {"3ce72b62311244b1a9e502755545360a62bea31f6fc1e22aad680a257e657eab",
              macula_record:envelope(16#20, #{}, #{subject_id => <<"s1">>})}],
    [?assertEqual({Hex, hex(list_to_binary(Hex))}, {Hex, macula_record:storage_key(Record#{key_id => Signer})})
     || {Hex, Record} <- Cases].

%%------------------------------------------------------------------
%% Records named by their signer
%%------------------------------------------------------------------

a_node_record_is_stored_under_its_node_id_test() ->
    Id = key(identity),
    R = macula_record:sign(macula_record:node_record(macula_node_keys:key_id(Id), [], 0), Id),
    ?assertEqual(macula_node_keys:key_id(Id), macula_record:storage_key(R)).

a_station_endpoint_is_stored_under_its_station_endpoint_key_test() ->
    Id = key(identity),
    R = macula_record:sign(macula_record:station_endpoint(4433), Id),
    ?assertEqual(macula_record:station_endpoint_key(macula_node_keys:key_id(Id)), macula_record:storage_key(R)),
    ?assertNotEqual(macula_node_keys:key_id(Id), macula_record:storage_key(R)).

a_procedure_delegation_is_stored_under_its_org_key_id_and_advertiser_test() ->
    Org = key(org),
    OrgKeyId = macula_node_keys:key_id(Org),
    R = macula_record:sign(macula_record:procedure_delegation(OrgKeyId, fill(16#55)), Org),
    ?assertEqual(macula_record:procedure_delegation_key(OrgKeyId, fill(16#55)), macula_record:storage_key(R)).

foundation_records_are_stored_per_signer_and_type_test() ->
    Foundation = key(foundation),
    Other = key(foundation),
    Key = fun(R, K) -> macula_record:storage_key(macula_record:sign(R, K)) end,
    Seeds = Key(macula_record:foundation_seed_list([]), Foundation),
    ?assertEqual(Seeds, Key(macula_record:foundation_seed_list([#{node_id => fill(1), addresses => [], tier => 4}]),
                            Foundation)),
    ?assertNotEqual(Seeds, Key(macula_record:foundation_seed_list([]), Other)),
    ?assertNotEqual(Seeds, Key(macula_record:foundation_realm_trust_list([]), Foundation)),
    ?assertNotEqual(Key(macula_record:foundation_parameter(<<"max_hops">>, 8), Foundation),
                    Key(macula_record:foundation_parameter(<<"fanout">>, 8), Foundation)).

domain_records_are_stored_per_signer_and_subject_test() ->
    Realm = key(realm),
    Key = fun(Opts, K) ->
        macula_record:storage_key(macula_record:sign(macula_record:envelope(16#20, #{}, Opts), K))
    end,
    Bare = Key(#{}, Realm),
    ?assertEqual(Bare, Key(#{}, Realm)),
    ?assertNotEqual(Bare, Key(#{}, key(realm))),
    ?assertNotEqual(Bare, Key(#{subject_id => <<"s1">>}, Realm)),
    ?assertNotEqual(Key(#{subject_id => <<"s1">>}, Realm), Key(#{subject_id => <<"s2">>}, Realm)).

every_storage_key_is_32_bytes_test() ->
    Id = key(identity),
    ?assertEqual(32, byte_size(macula_record:storage_key(macula_record:sign(macula_record:station_endpoint(1), Id)))).

content_key_refuses_a_content_id_with_another_tag_test() ->
    ?assertError(function_clause, macula_record:content_key(<<1, 16#55, 0:256>>)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

hex(Hex) ->
    binary:decode_hex(Hex).

mcid() ->
    <<2, 16#55, (binary:copy(<<16#88>>, 48))/binary>>.

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
