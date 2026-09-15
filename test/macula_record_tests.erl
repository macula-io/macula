%% EUnit tests for macula_record in the signed-object format of DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md: records are
%% {key, tbs, signature} under MACULA-PQ-RECORD-V1, the signer's key arrives at sign/2, and verify/2,3 refuses what the
%% design refuses. Tombstones, advertisements, content announcements, storage keys and certificate chains have their own
%% test modules.
-module(macula_record_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(MINUTE, 60000).
-define(KIB, 1024).

%%------------------------------------------------------------------
%% Signing
%%------------------------------------------------------------------

a_signed_record_carries_key_key_id_alg_tbs_and_signature_test() ->
    Id = key(identity),
    NodeId = macula_node_keys:key_id(Id),
    R = macula_record:sign(macula_record:node_record(NodeId, [], 0), Id),
    ?assertEqual(macula_node_keys:public_key(Id), macula_record:key(R)),
    ?assertEqual(NodeId, macula_record:key_id(R)),
    ?assertEqual(<<"ML-DSA-87">>, maps:get(alg, R)),
    ?assertEqual(4627, byte_size(macula_record:signature(R))),
    ?assert(is_binary(maps:get(tbs, R))).

the_tbs_holds_exactly_the_record_fields_test() ->
    Id = key(identity),
    R = macula_record:sign(macula_record:node_record(macula_node_keys:key_id(Id), [], 0), Id),
    {ok, Fields} = macula_record_cbor:decode_strict(maps:get(tbs, R)),
    ?assertEqual([<<"alg">>, <<"created_at">>, <<"expires_at">>, <<"payload">>, <<"type">>, <<"version">>],
                 lists:sort([Name || {text, Name} <- maps:keys(Fields)])).

an_unsigned_record_has_no_key_and_no_signature_test() ->
    R = macula_record:node_record(fill(1), [], 0),
    ?assertNot(maps:is_key(key, R)),
    ?assertNot(maps:is_key(signature, R)),
    ?assertEqual(16, byte_size(macula_record:version(R))),
    ?assert(macula_record:expires_at(R) > macula_record:created_at(R)).

sign_refuses_a_key_whose_purpose_does_not_fit_the_type_test_() ->
    RealmId = fill(16#11),
    Realm = key(realm),
    [?_assertError({key_purpose_mismatch, _},
                   macula_record:sign(macula_record:node_record(macula_node_keys:key_id(Realm), [], 0), Realm)),
     ?_assertError({key_purpose_mismatch, _},
                   macula_record:sign(macula_record:realm_directory(RealmId, <<"io.macula">>, fill(2)), key(identity))),
     ?_assertError({key_purpose_mismatch, _},
                   macula_record:sign(macula_record:procedure_delegation(fill(3), fill(4)), key(realm))),
     ?_assertError({key_purpose_mismatch, _},
                   macula_record:sign(macula_record:foundation_realm_trust_list([], #{}), key(org))),
     ?_assertError({key_purpose_mismatch, _},
                   macula_record:sign(macula_record:envelope(16#20, #{}, #{}), key(connect)))].

sign_refuses_a_node_record_for_another_node_test() ->
    Id = key(identity),
    ?assertError({key_id_mismatch, _}, macula_record:sign(macula_record:node_record(fill(9), [], 0), Id)).

sign_refuses_a_record_larger_than_256_kib_test() ->
    Id = key(identity),
    Big = binary:copy(<<"x">>, 256 * ?KIB),
    Unsigned = macula_record:node_record(macula_node_keys:key_id(Id), [], 0, #{display_name => Big}),
    ?assertError({record_too_large, _}, macula_record:sign(Unsigned, Id)).

%%------------------------------------------------------------------
%% Verifying
%%------------------------------------------------------------------

a_record_verifies_from_its_wire_form_test() ->
    {Id, R} = signed_node_record(),
    {ok, V} = macula_record:verify(macula_record:encode(R), pq_pure),
    [?assertEqual(maps:get(F, R), maps:get(F, V))
     || F <- [type, version, created_at, expires_at, payload, key, key_id, alg, tbs, signature]],
    ?assertEqual(macula_node_keys:key_id(Id), macula_record:key_id(V)).

a_record_verifies_from_its_object_map_test() ->
    {_Id, #{key := K, tbs := T, signature := S} = R} = signed_node_record(),
    ?assertMatch({ok, _}, macula_record:verify(#{key => K, tbs => T, signature => S}, pq_pure)),
    ?assertEqual(macula_record:verify(macula_record:encode(R), pq_pure),
                 macula_record:verify(#{key => K, tbs => T, signature => S}, pq_pure)).

a_hybrid_record_verifies_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Id} = macula_node_keys:generate(identity, pq_hybrid),
        R = macula_record:sign(macula_record:node_record(macula_node_keys:key_id(Id), [], 0), Id),
        ?assertEqual(<<"ML-DSA-87-PS384">>, maps:get(alg, R)),
        ?assertMatch({ok, _}, macula_record:verify(macula_record:encode(R), pq_hybrid)),
        ?assertEqual({error, malformed}, macula_record:verify(macula_record:encode(R), pq_pure))
    end}.

a_record_under_another_profile_is_malformed_test() ->
    {_Id, R} = signed_node_record(),
    ?assertEqual({error, malformed}, macula_record:verify(macula_record:encode(R), pq_hybrid)).

a_record_signed_under_another_label_is_refused_test() ->
    Id = key(identity),
    Object = macula_signed_object:sign(<<"MACULA-PQ-REPLY-V1">>, node_fields(Id, now_ms()), Id),
    ?assertEqual({error, signature_invalid}, macula_record:verify(Object, pq_pure)).

a_tampered_tbs_is_refused_test() ->
    {_Id, #{tbs := <<Head:20/binary, Byte, Tail/binary>>} = R} = signed_node_record(),
    Object = object(R),
    ?assertEqual({error, signature_invalid},
                 macula_record:verify(Object#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}, pq_pure)).

a_record_under_another_key_is_refused_test() ->
    {_Id, R} = signed_node_record(),
    Object = object(R),
    ?assertEqual({error, signature_invalid},
                 macula_record:verify(Object#{key := macula_node_keys:public_key(key(identity))}, pq_pure)).

a_held_shape_is_malformed_test() ->
    {_Id, R} = signed_node_record(),
    ?assertEqual({error, malformed}, macula_record:verify(maps:remove(key, object(R)), pq_pure)).

garbage_is_malformed_test() ->
    ?assertEqual({error, malformed}, macula_record:verify(<<16#FF, 1, 2, 3>>, pq_pure)).

a_record_larger_than_256_kib_is_refused_before_any_other_check_test() ->
    Id = key(identity),
    Fields = node_fields(Id, now_ms()),
    Payload = maps:get({text, <<"payload">>}, Fields),
    Name = {text, binary:copy(<<"x">>, 256 * ?KIB)},
    Big = Fields#{{text, <<"payload">>} := Payload#{{text, <<"display_name">>} => Name}},
    Bytes = macula_signed_object:encode(macula_signed_object:sign(?LABEL, Big, Id)),
    ?assert(byte_size(Bytes) > 256 * ?KIB),
    ?assertEqual({error, record_too_large}, macula_record:verify(Bytes, pq_pure)),
    ?assertEqual({error, record_too_large}, macula_record:verify(<<Bytes/binary, 0>>, pq_pure)).

tbs_fields_the_design_does_not_allow_are_malformed_test_() ->
    Id = key(identity),
    Now = now_ms(),
    Fields = node_fields(Id, Now),
    Verify = fun(F) -> macula_record:verify(macula_signed_object:sign(?LABEL, F, Id), pq_pure) end,
    [?_assertMatch({ok, _}, Verify(Fields)),
     ?_assertEqual({error, malformed}, Verify(Fields#{{text, <<"extra">>} => 1})),
     ?_assertEqual({error, malformed}, Verify(maps:remove({text, <<"version">>}, Fields))),
     ?_assertEqual({error, malformed}, Verify(Fields#{{text, <<"version">>} := <<0:120>>})),
     ?_assertEqual({error, malformed}, Verify(Fields#{{text, <<"subject">>} => <<1, 2>>})),
     ?_assertEqual({error, malformed}, Verify(Fields#{{text, <<"type">>} := 16#07})),
     ?_assertEqual({error, malformed}, Verify(Fields#{{text, <<"payload">>} := [1]})),
     ?_assertEqual({error, malformed}, Verify(Fields#{{text, <<"created_at">>} := {text, <<"now">>}})),
     ?_assertEqual({error, malformed}, Verify(Fields#{{text, <<"type">>} := 256})),
     ?_assertEqual({error, malformed}, Verify(Fields#{{text, <<"created_at">>} := 1 bsl 53})),
     ?_assertEqual({error, malformed}, Verify(Fields#{{text, <<"expires_at">>} := 1 bsl 53}))].

a_node_record_naming_another_node_is_refused_test() ->
    Id = key(identity),
    Fields = node_fields(Id, now_ms()),
    Payload = maps:get({text, <<"payload">>}, Fields),
    Other = Fields#{{text, <<"payload">>} := Payload#{{text, <<"node_id">>} := fill(9)}},
    ?assertEqual({error, key_id_mismatch},
                 macula_record:verify(macula_signed_object:sign(?LABEL, Other, Id), pq_pure)).

the_clock_tolerance_is_five_minutes_test() ->
    {_Id, R} = signed_node_record(),
    Bytes = macula_record:encode(R),
    Created = macula_record:created_at(R),
    Expires = macula_record:expires_at(R),
    ?assertMatch({ok, _}, macula_record:verify(Bytes, pq_pure, Created - 4 * ?MINUTE)),
    ?assertEqual({error, not_yet_valid}, macula_record:verify(Bytes, pq_pure, Created - 6 * ?MINUTE)),
    ?assertMatch({ok, _}, macula_record:verify(Bytes, pq_pure, Expires + 4 * ?MINUTE)),
    ?assertEqual({error, expired}, macula_record:verify(Bytes, pq_pure, Expires + 6 * ?MINUTE)).

%%------------------------------------------------------------------
%% Key ids by record type
%%------------------------------------------------------------------

a_realm_record_is_named_by_the_realm_key_id_test() ->
    Realm = key(realm),
    R = macula_record:sign(macula_record:realm_directory(fill(16#11), <<"io.macula">>, fill(2)), Realm),
    ?assertEqual(macula_node_keys:key_id(Realm), macula_record:key_id(R)),
    {ok, V} = macula_record:verify(macula_record:encode(R), pq_pure),
    ?assertEqual(macula_node_keys:key_id(Realm), macula_record:key_id(V)).

a_domain_record_is_named_by_the_key_id_even_for_an_identity_key_test() ->
    Id = key(identity),
    R = macula_record:sign(macula_record:envelope(16#20, #{{text, <<"fact">>} => 1}, #{}), Id),
    KeyId = macula_node_keys:key_id(macula_node_keys:public_key(Id), pq_pure),
    ?assertEqual(KeyId, macula_record:key_id(R)),
    ?assertNotEqual(macula_node_keys:key_id(Id), KeyId),
    {ok, V} = macula_record:verify(macula_record:encode(R), pq_pure),
    ?assertEqual(KeyId, macula_record:key_id(V)).

%%------------------------------------------------------------------
%% Domain records
%%------------------------------------------------------------------

a_domain_record_carries_its_subject_in_tbs_test() ->
    Realm = key(realm),
    R = macula_record:sign(macula_record:envelope(16#21, #{}, #{subject_id => <<"station-1">>}), Realm),
    {ok, Fields} = macula_record_cbor:decode_strict(maps:get(tbs, R)),
    ?assertEqual(<<"station-1">>, maps:get({text, <<"subject">>}, Fields)),
    {ok, V} = macula_record:verify(macula_record:encode(R), pq_pure),
    ?assertEqual(<<"station-1">>, maps:get(subject, V)).

envelope_refuses_a_built_in_type_test() ->
    ?assertError(function_clause, macula_record:envelope(16#01, #{}, #{})).

%%------------------------------------------------------------------
%% Refresh
%%------------------------------------------------------------------

refresh_keeps_type_payload_and_ttl_and_takes_a_later_version_test() ->
    {Id, R} = signed_node_record(),
    timer:sleep(2),
    F = macula_record:refresh(R, Id),
    ?assertEqual(macula_record:type(R), macula_record:type(F)),
    ?assertEqual(macula_record:payload(R), macula_record:payload(F)),
    ?assertEqual(macula_record:expires_at(R) - macula_record:created_at(R),
                 macula_record:expires_at(F) - macula_record:created_at(F)),
    ?assert(macula_record:version(F) > macula_record:version(R)),
    ?assertMatch({ok, _}, macula_record:verify(macula_record:encode(F), pq_pure)).

%%------------------------------------------------------------------
%% Constructors and readers
%%------------------------------------------------------------------

node_record_payload_and_reader_test() ->
    Id = key(identity),
    NodeId = macula_node_keys:key_id(Id),
    Opts = #{station_id => fill(5), kind => <<"station">>, hostname => <<"beam00">>, lat => 50.8, lng => 4,
             peers => [fill(7), fill(6)], display_name => <<"Beam 00">>},
    R = macula_record:sign(macula_record:node_record(NodeId, [fill(16#11)], 3, Opts), Id),
    {ok, V} = macula_record:verify(macula_record:encode(R), pq_pure),
    ?assertMatch(#{node_id := NodeId, station_id := <<5:8, _/binary>>, realms := [<<16#11, _/binary>>],
                   capabilities := 3, kind := <<"station">>, hostname := <<"beam00">>, lat := 50.8, lng := 4,
                   display_name := <<"Beam 00">>}, macula_record:read_node_record(V)),
    ?assertEqual([fill(6), fill(7)], maps:get(peers, macula_record:read_node_record(V))).

realm_records_name_the_realm_by_realm_id_test() ->
    RealmId = fill(16#11),
    Member = macula_record:realm_member_endorsement(RealmId, #{realm => RealmId, member_node => fill(2),
                                                                roles => [<<"member">>]}),
    Org = macula_record:org_directory(RealmId, <<"acme">>, fill(4)),
    Stations = macula_record:realm_stations(RealmId, [#{station_id => fill(3), roles => [<<"seed">>]}]),
    Directory = macula_record:realm_directory(RealmId, <<"io.macula">>, fill(5)),
    [?assertEqual(RealmId, maps:get({text, <<"realm_id">>}, macula_record:payload(R)))
     || R <- [Member, Org, Stations, Directory]],
    ?assertEqual(#{realm_id => RealmId, org_name => <<"acme">>, org_key => fill(4)},
                 macula_record:read_org_directory(Org)).

%% A realm member endorsement's window, valid_from to valid_until, is at most 30 days: the builder refuses a longer one.
a_realm_member_endorsement_window_is_at_most_30_days_test() ->
    RealmId = fill(16#11),
    Spec = #{realm => RealmId, member_node => fill(2), roles => [<<"member">>]},
    From = now_ms(),
    Window = 30 * 24 * 60 * ?MINUTE,
    Endorse = fun(Until) -> macula_record:realm_member_endorsement(RealmId, Spec, #{valid_from => From,
                                                                                    valid_until => Until}) end,
    ?assertEqual(From + Window, maps:get({text, <<"valid_until">>}, macula_record:payload(Endorse(From + Window)))),
    ?assertError({badmatch, {error, endorsement_window_too_long}}, Endorse(From + Window + 1)).

procedure_delegation_names_org_key_and_advertiser_test() ->
    Org = key(org),
    OrgKeyId = macula_node_keys:key_id(Org),
    R = macula_record:sign(macula_record:procedure_delegation(OrgKeyId, fill(8)), Org),
    {ok, V} = macula_record:verify(macula_record:encode(R), pq_pure),
    ?assertEqual(#{org_key => OrgKeyId, advertiser => fill(8)}, macula_record:read_procedure_delegation(V)).

a_procedure_delegation_whose_org_key_is_not_its_signer_is_refused_test() ->
    Org = key(org),
    ?assertError({key_id_mismatch, _}, macula_record:sign(macula_record:procedure_delegation(fill(3), fill(8)), Org)).

station_endpoint_payload_and_reader_test() ->
    Id = key(identity),
    R = macula_record:sign(macula_record:station_endpoint(4433, #{host_advertised => [<<"beam00.lab">>]}), Id),
    {ok, V} = macula_record:verify(macula_record:encode(R), pq_pure),
    ?assertEqual(#{quic_port => 4433, host_advertised => [<<"beam00.lab">>]}, macula_record:read_station_endpoint(V)).

foundation_records_sign_with_a_foundation_key_test() ->
    Foundation = key(foundation),
    Records = [macula_record:foundation_seed_list([#{node_id => fill(1), addresses => [], tier => 3}], #{}),
               macula_record:foundation_parameter(<<"max_hops">>, 8, #{}),
               macula_record:foundation_realm_trust_list([fill(2)], #{}),
               macula_record:foundation_t3_attestation(fill(3), 1789000000000, #{})],
    [?assertMatch({ok, _}, macula_record:verify(macula_record:encode(macula_record:sign(R, Foundation)), pq_pure))
     || R <- Records].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

signed_node_record() ->
    Id = key(identity),
    {Id, macula_record:sign(macula_record:node_record(macula_node_keys:key_id(Id), [], 0), Id)}.

object(#{key := K, tbs := T, signature := S}) ->
    #{key => K, tbs => T, signature => S}.

%% The tbs fields of a node record, to sign by hand past what sign/2 would build.
node_fields(Id, Now) ->
    NodeId = macula_node_keys:key_id(Id),
    #{{text, <<"type">>} => 16#01,
      {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
      {text, <<"created_at">>} => Now,
      {text, <<"expires_at">>} => Now + 60 * ?MINUTE,
      {text, <<"payload">>} => #{{text, <<"node_id">>} => NodeId, {text, <<"station_id">>} => NodeId,
                                  {text, <<"realms">>} => [], {text, <<"capabilities">>} => 0}}.

now_ms() ->
    erlang:system_time(millisecond).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
