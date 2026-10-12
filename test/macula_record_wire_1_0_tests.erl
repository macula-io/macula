%% EUnit tests for records under wire 1.0 (macula#100, design macula#83 D4, D7, A3). Every record's tbs carries its
%% type's schema version and a lifetime class from a frozen table. A custodian (a station storing records) checks only
%% the envelope, the signature, the clock, the lifetime class and the slot, so it stores a type, schema, row or
%% content-id tag it does not know; a reader refuses those by name (docs/design/WIRE_1_0.md section 5). A built-in tag the custodian does not know is placed like a
%% domain type, under its signer's key id and its subject. Tag 0x14 is reserved for the successor record.
-module(macula_record_wire_1_0_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(STORAGE_KEY_LABEL, "MACULA-PQ-STORAGE-KEY-V1").
-define(MINUTE, 60_000).
-define(HOUR, 3_600_000).
-define(DAY, 86_400_000).

%%------------------------------------------------------------------
%% The tbs carries schema and lifetime class
%%------------------------------------------------------------------

every_built_in_type_signs_schema_1_and_its_own_lifetime_class_test() ->
    Id = key(identity),
    Realm = key(realm),
    Org = key(org),
    Foundation = key(foundation),
    NodeId = macula_node_keys:key_id(Id),
    OrgKeyId = macula_node_keys:key_id(Org),
    Signed = [{macula_record:node_record(NodeId, [], 0), Id, 3},
              {macula_record:station_endpoint(4433), Id, 0},
              {macula_record:procedure_advertisement(NodeId, <<1:256>>, <<"echo">>, [<<2:256>>]), Id, 0},
              {macula_record:realm_directory(<<1:256>>, <<"r">>, <<3:256>>), Realm, 5},
              {macula_record:realm_stations(<<1:256>>, []), Realm, 2},
              {macula_record:org_directory(<<1:256>>, <<"org">>, OrgKeyId), Realm, 2},
              {macula_record:procedure_delegation(OrgKeyId, NodeId), Org, 1},
              {macula_record:station_authorization(<<1:256>>, #{realm => <<1:256>>, station_node => NodeId,
                                                     hostname => <<"station.example">>, roles => [<<"relay">>]}),
               Realm, 1},
              {macula_record:foundation_seed_list([]), Foundation, 5},
              {macula_record:envelope(16#20, #{}, #{}), Id, 4}],
    [?assertEqual({1, Class, Type}, {schema(R), lifetime_class(R), Type})
     || {Unsigned, Key, Class} <- Signed,
        #{type := Type} = R <- [macula_record:sign(Unsigned, Key)]].

a_tombstone_names_the_lifetime_class_of_the_type_it_withdraws_test() ->
    Id = key(identity),
    Withdrawn = macula_record:sign(macula_record:station_endpoint(4433), Id),
    Tombstone = macula_record:sign(macula_record:tombstone(Withdrawn, shutdown), Id),
    ?assertEqual(0, lifetime_class(Tombstone)),
    ?assertMatch({ok, _}, macula_record:verify(macula_record:encode(Tombstone), pq_pure)).

a_record_without_schema_or_lifetime_class_is_malformed_test() ->
    Id = key(identity),
    NodeId = macula_node_keys:key_id(Id),
    Fields = fields(macula_record:node_record(NodeId, [], 0), Id),
    [?assertEqual({error, malformed}, verify(macula_signed_object:sign(?LABEL, maps:remove({text, Name}, Fields), Id)))
     || Name <- [<<"schema">>, <<"lifetime_class">>]].

an_extra_core_field_is_malformed_test() ->
    Id = key(identity),
    Fields = fields(macula_record:node_record(macula_node_keys:key_id(Id), [], 0), Id),
    Object = macula_signed_object:sign(?LABEL, Fields#{{text, <<"extra">>} => 1}, Id),
    ?assertEqual({error, malformed}, verify(Object)),
    ?assertEqual({error, malformed}, custodian(Object)).

%%------------------------------------------------------------------
%% Lifetime classes
%%------------------------------------------------------------------

%% WIRE_1_0.md 5.2: a known type naming another row is the reader's to refuse; the custodian bounds it by the row it
%% names.
a_known_type_naming_another_row_is_refused_by_a_reader_test() ->
    Id = key(identity),
    Fields = fields(macula_record:station_endpoint(4433), Id),
    Object = macula_signed_object:sign(?LABEL, Fields#{{text, <<"lifetime_class">>} => 3}, Id),
    ?assertEqual({error, lifetime_class_mismatch}, verify(Object)),
    ?assertMatch({ok, _}, custodian(Object)).

a_class_outside_the_table_is_refused_test() ->
    Id = key(identity),
    Object = unknown_tag(16#18, Id, #{{text, <<"lifetime_class">>} => 7}),
    ?assertEqual({error, lifetime_class_unknown}, custodian(Object)),
    ?assertEqual({error, lifetime_class_unknown}, verify(Object)).

an_unknown_tag_lives_at_most_its_class_maximum_test() ->
    Id = key(identity),
    Now = erlang:system_time(millisecond),
    AtBound = unknown_tag(16#18, Id, #{{text, <<"lifetime_class">>} => 0, {text, <<"created_at">>} => Now,
                                       {text, <<"expires_at">>} => Now + 5 * ?MINUTE}),
    Past = unknown_tag(16#18, Id, #{{text, <<"lifetime_class">>} => 0, {text, <<"created_at">>} => Now,
                                    {text, <<"expires_at">>} => Now + 5 * ?MINUTE + 1}),
    ?assertMatch({ok, _}, custodian(AtBound)),
    ?assertEqual({error, lifetime_too_long}, custodian(Past)).

the_lifetime_class_table_is_frozen_test() ->
    ?assertEqual(#{0 => 5 * ?MINUTE, 1 => 30 * ?MINUTE, 2 => 6 * ?HOUR, 3 => 48 * ?HOUR, 4 => 7 * ?DAY,
                   5 => 30 * ?DAY},
                 macula_record:lifetime_classes()).

%%------------------------------------------------------------------
%% Custodian stores what a reader does not know
%%------------------------------------------------------------------

an_unknown_built_in_tag_is_stored_under_its_signer_and_subject_test() ->
    Id = key(identity),
    Object = unknown_tag(16#18, Id, #{}),
    {ok, Record} = custodian(Object),
    KeyId = macula_node_keys:key_id(macula_node_keys:public_key(Id), pq_pure),
    ?assertEqual(KeyId, macula_record:key_id(Record)),
    ?assertEqual(crypto:hash(sha256, [<<?STORAGE_KEY_LABEL, 0:8, 16#18:8>>, KeyId, <<7:32>>, <<"subject">>]),
                 macula_record:storage_key(Record)),
    ?assertEqual({error, type_unknown}, verify(Object)).

an_unknown_built_in_tag_without_a_subject_is_refused_test() ->
    Id = key(identity),
    Fields = maps:remove({text, <<"subject">>}, unknown_fields(16#18, #{})),
    ?assertEqual({error, subject_missing}, custodian(macula_signed_object:sign(?LABEL, Fields, Id))).

an_unknown_schema_is_stored_and_refused_by_a_reader_test() ->
    Id = key(identity),
    Fields = fields(macula_record:node_record(macula_node_keys:key_id(Id), [], 0), Id),
    Object = macula_signed_object:sign(?LABEL, Fields#{{text, <<"schema">>} => 2}, Id),
    ?assertMatch({ok, _}, custodian(Object)),
    ?assertEqual({error, schema_unsupported}, verify(Object)).

an_unknown_content_id_tag_is_stored_and_refused_by_a_reader_test() ->
    Id = key(identity),
    NodeId = macula_node_keys:key_id(Id),
    Announcement = macula_record:content_announcement(NodeId, <<2, 0, 0:384>>,
                                                      #{realm_id => <<1:256>>, serving_station => <<2:256>>,
                                                        procedure => <<"content">>}),
    #{{text, <<"payload">>} := Payload} = Fields = fields(Announcement, Id),
    UnknownTag = <<3, 0, 0:384>>,
    Object = macula_signed_object:sign(?LABEL, Fields#{{text, <<"payload">>} := Payload#{{text, <<"mcid">>} := UnknownTag}},
                                       Id),
    {ok, Record} = custodian(Object),
    ?assertEqual(crypto:hash(sha256, [<<?STORAGE_KEY_LABEL, 0:8, 16#11:8>>, <<50:32>>, UnknownTag]),
                 macula_record:storage_key(Record)),
    ?assertEqual({error, mcid_tag_unknown}, verify(Object)).

the_successor_tag_is_reserved_test() ->
    Id = key(identity),
    ?assertEqual(16#14, macula_record:type_successor()),
    Object = unknown_tag(16#14, Id, #{{text, <<"lifetime_class">>} => 5}),
    ?assertMatch({ok, _}, custodian(Object)),
    ?assertEqual({error, type_unknown}, verify(Object)).

a_custodian_does_not_read_a_known_payload_past_its_slot_fields_test() ->
    Id = key(identity),
    Fields = fields(macula_record:node_record(macula_node_keys:key_id(Id), [], 0), Id),
    #{{text, <<"payload">>} := Payload} = Fields,
    %% The node_id names another node: a reader refuses the signer, a custodian stores under the signer's node_id.
    Object = macula_signed_object:sign(?LABEL, Fields#{{text, <<"payload">>} := Payload#{{text, <<"node_id">>} := <<9:256>>}},
                                       Id),
    ?assertEqual({error, key_id_mismatch}, verify(Object)),
    {ok, Record} = custodian(Object),
    ?assertEqual(macula_node_keys:key_id(Id), macula_record:storage_key(Record)).

a_known_type_missing_a_slot_field_is_malformed_for_the_custodian_test() ->
    Realm = key(realm),
    Fields = fields(macula_record:realm_stations(<<1:256>>, []), Realm),
    #{{text, <<"payload">>} := Payload} = Fields,
    Object = macula_signed_object:sign(?LABEL, Fields#{{text, <<"payload">>} := maps:remove({text, <<"realm_id">>}, Payload)},
                                       Realm),
    ?assertEqual({error, malformed}, custodian(Object)).

%%------------------------------------------------------------------
%% Check order
%%------------------------------------------------------------------

the_custodian_checks_in_the_pinned_order_test() ->
    Id = key(identity),
    Now = erlang:system_time(millisecond),
    Expired = #{{text, <<"created_at">>} => Now - 2 * ?DAY, {text, <<"expires_at">>} => Now - ?DAY},
    %% clock before class
    ?assertEqual({error, expired}, custodian(unknown_tag(16#18, Id, Expired#{{text, <<"lifetime_class">>} => 99}))),
    %% a row outside the table before the maximum
    Long = #{{text, <<"created_at">>} => Now, {text, <<"expires_at">>} => Now + 40 * ?DAY},
    ?assertEqual({error, lifetime_class_unknown},
                 custodian(unknown_tag(16#18, Id, Long#{{text, <<"lifetime_class">>} => 99}))),
    %% lifetime before slot
    NoSubject = maps:remove({text, <<"subject">>},
                            unknown_fields(16#18, Long#{{text, <<"lifetime_class">>} => 0})),
    ?assertEqual({error, lifetime_too_long}, custodian(macula_signed_object:sign(?LABEL, NoSubject, Id))),
    %% signature before everything in the tbs
    #{signature := <<B, Rest/binary>>} = Object = unknown_tag(16#18, Id, Expired),
    ?assertEqual({error, signature_invalid}, custodian(Object#{signature := <<(B bxor 1), Rest/binary>>})).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

verify(Object) ->
    macula_record:verify(macula_signed_object:encode(Object), pq_pure).

custodian(Object) ->
    macula_record:custodian_verify(macula_signed_object:encode(Object), pq_pure, erlang:system_time(millisecond)).

schema(#{tbs := Tbs}) ->
    {ok, #{{text, <<"schema">>} := Schema}} = macula_record_cbor:decode_strict(Tbs),
    Schema.

lifetime_class(#{tbs := Tbs}) ->
    {ok, #{{text, <<"lifetime_class">>} := Class}} = macula_record_cbor:decode_strict(Tbs),
    Class.

%% The tbs fields the signed record carries, as another stack would build them.
fields(Unsigned, Key) ->
    #{tbs := Tbs} = macula_record:sign(Unsigned, Key),
    {ok, Fields} = macula_record_cbor:decode_strict(Tbs),
    maps:remove({text, <<"alg">>}, Fields).

unknown_fields(Type, Overrides) ->
    Now = erlang:system_time(millisecond),
    maps:merge(#{{text, <<"type">>} => Type, {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
                 {text, <<"schema">>} => 1, {text, <<"lifetime_class">>} => 3, {text, <<"created_at">>} => Now,
                 {text, <<"expires_at">>} => Now + ?HOUR, {text, <<"payload">>} => #{},
                 {text, <<"subject">>} => <<"subject">>},
               Overrides).

unknown_tag(Type, Key, Overrides) ->
    macula_signed_object:sign(?LABEL, unknown_fields(Type, Overrides), Key).
