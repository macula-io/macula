%% EUnit tests for tombstones as DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md pins them: a tombstone takes the slot of the
%% record it withdraws, names that record's type, version and slot fields, gives one of three reasons, is signed by the
%% same key id, and expires no earlier than the withdrawn record.
-module(macula_record_tombstone_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(DAY, 86400000).

%%------------------------------------------------------------------
%% Construction
%%------------------------------------------------------------------

a_node_record_tombstone_takes_the_node_records_slot_test() ->
    {Id, Node} = signed_node(),
    Tomb = macula_record:sign(macula_record:tombstone(Node, shutdown), Id),
    ?assertEqual(16#0C, macula_record:type(Tomb)),
    ?assertEqual(macula_node_keys:key_id(Id), macula_record:storage_key(Tomb)),
    ?assertEqual(macula_record:storage_key(Node), macula_record:storage_key(Tomb)).

the_payload_names_the_withdrawn_type_version_and_reason_test() ->
    {_Id, Node} = signed_node(),
    ?assertEqual(#{{text, <<"withdrawn_type">>} => 16#01,
                   {text, <<"withdrawn_version">>} => macula_record:version(Node),
                   {text, <<"reason">>} => {text, <<"shutdown">>}},
                 macula_record:payload(macula_record:tombstone(Node, shutdown))).

detail_is_carried_only_when_given_test() ->
    {_Id, Node} = signed_node(),
    Tomb = macula_record:tombstone(Node, moved, #{detail => <<"to beam01">>}),
    ?assertEqual({text, <<"to beam01">>}, maps:get({text, <<"detail">>}, macula_record:payload(Tomb))).

only_shutdown_moved_and_revoked_are_reasons_test() ->
    {Id, Node} = signed_node(),
    [?assertMatch({ok, _}, verify(macula_record:sign(macula_record:tombstone(Node, Reason), Id)))
     || Reason <- [shutdown, moved, revoked]],
    ?assertError(function_clause, macula_record:tombstone(Node, retired)).

a_tombstone_expires_no_earlier_than_the_withdrawn_record_test() ->
    Id = key(identity),
    Unsigned = macula_record:node_record(macula_node_keys:key_id(Id), [], 0, #{ttl_ms => 2 * ?DAY}),
    Long = macula_record:sign(Unsigned, Id),
    ?assert(macula_record:expires_at(macula_record:tombstone(Long, shutdown)) >= macula_record:expires_at(Long)).

%% A tombstone lives until the record it withdraws has expired plus the five-minute clock tolerance, so no replica
%% serves the record again after the tombstone lapses. It signs within the withdrawn type's maximum plus twice that
%% tolerance, since the record it withdraws may be created up to the tolerance ahead: here a tombstone for a procedure
%% advertisement created four minutes ahead at its five-minute maximum signs and verifies, and one a millisecond past
%% that bound is refused.
a_tombstone_outlives_the_withdrawn_record_by_the_clock_tolerance_test() ->
    Minute = 60_000,
    Id = key(identity),
    Now = erlang:system_time(millisecond),
    Advertisement = macula_record:procedure_advertisement(macula_node_keys:key_id(Id), fill(16#11), <<"acme/echo_v1">>,
                                                          fill(16#77)),
    Withdrawn = macula_record:sign(Advertisement#{created_at := Now + 4 * Minute, expires_at := Now + 9 * Minute}, Id),
    #{created_at := Created} = Tomb = macula_record:tombstone(Withdrawn, shutdown),
    ?assertMatch({ok, _}, verify(Withdrawn)),
    ?assert(macula_record:expires_at(Tomb) >= macula_record:expires_at(Withdrawn) + 5 * Minute),
    Signed = macula_record:sign(Tomb, Id),
    ?assertEqual(macula_record:expires_at(Tomb), macula_record:expires_at(Signed)),
    ?assertMatch({ok, _}, verify(Signed)),
    ?assertError({lifetime_too_long, 16#0C}, macula_record:sign(Tomb#{expires_at := Created + 15 * Minute + 1}, Id)).

a_member_endorsement_tombstone_names_realm_and_member_and_the_realm_signs_it_test() ->
    Realm = key(realm),
    Endorsement = macula_record:sign(endorsement(), Realm),
    Tomb = macula_record:sign(macula_record:tombstone(Endorsement, revoked), Realm),
    Payload = macula_record:payload(Tomb),
    ?assertEqual(fill(16#11), maps:get({text, <<"realm_id">>}, Payload)),
    ?assertEqual(fill(2), maps:get({text, <<"member_node">>}, Payload)),
    ?assertEqual(macula_record:storage_key(Endorsement), macula_record:storage_key(Tomb)),
    ?assertEqual(macula_node_keys:key_id(Realm), macula_record:key_id(Tomb)),
    ?assertError({key_purpose_mismatch, _},
                 macula_record:sign(macula_record:tombstone(Endorsement, revoked), key(identity))).

tombstones_share_the_slot_and_key_id_of_what_they_withdraw_test() ->
    Realm = key(realm),
    Org = key(org),
    Foundation = key(foundation),
    Id = key(identity),
    OrgKeyId = macula_node_keys:key_id(Org),
    Withdrawn = [{macula_record:org_directory(fill(16#11), <<"acme">>, OrgKeyId), Realm},
                 {macula_record:procedure_delegation(OrgKeyId, fill(5)), Org},
                 {macula_record:foundation_seed_list([]), Foundation},
                 {macula_record:foundation_parameter(<<"max_hops">>, 8), Foundation},
                 {macula_record:foundation_t3_attestation(fill(3), 1789000000000), Foundation},
                 {macula_record:content_announcement(macula_node_keys:key_id(Id), mcid(), <<"quic://h:1">>), Id},
                 {macula_record:station_endpoint(4433), Id},
                 {macula_record:envelope(16#20, #{}, #{subject_id => <<"s1">>}), Realm},
                 {macula_record:envelope(16#21, #{}, #{}), Id}],
    [begin
         Signed = macula_record:sign(Unsigned, Key),
         Tomb = macula_record:sign(macula_record:tombstone(Signed, revoked), Key),
         {ok, Verified} = verify(Tomb),
         ?assertEqual(macula_record:storage_key(Signed), macula_record:storage_key(Verified)),
         ?assertEqual(macula_record:key_id(Signed), macula_record:key_id(Verified))
     end
     || {Unsigned, Key} <- Withdrawn].

read_tombstone_returns_the_typed_payload_test() ->
    Realm = key(realm),
    Endorsement = macula_record:sign(endorsement(), Realm),
    Unsigned = macula_record:tombstone(Endorsement, revoked, #{detail => <<"left">>}),
    {ok, Tomb} = verify(macula_record:sign(Unsigned, Realm)),
    ?assertEqual(#{withdrawn_type => 16#05, withdrawn_version => macula_record:version(Endorsement),
                   reason => <<"revoked">>, detail => <<"left">>, realm_id => fill(16#11), member_node => fill(2)},
                 macula_record:read_tombstone(Tomb)),
    {Id, Node} = signed_node(),
    ?assertEqual(#{withdrawn_type => 16#01, withdrawn_version => macula_record:version(Node),
                   reason => <<"shutdown">>, detail => undefined},
                 macula_record:read_tombstone(macula_record:sign(macula_record:tombstone(Node, shutdown), Id))).

%%------------------------------------------------------------------
%% What a verifier refuses
%%------------------------------------------------------------------

tombstone_payloads_the_design_does_not_allow_are_malformed_test() ->
    {Id, Node} = signed_node(),
    Good = node_tombstone_payload(Node),
    Verify = fun(Payload, Key) -> macula_record:verify(hand_signed(Payload, Key), pq_pure) end,
    ?assertMatch({ok, _}, Verify(Good, Id)),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"reason">>} := {text, <<"retired">>}}, Id)),
    ?assertEqual({error, malformed}, Verify(maps:remove({text, <<"withdrawn_version">>}, Good), Id)),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"withdrawn_version">>} := <<0:120>>}, Id)),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"replaced_at">>} => 1}, Id)),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"withdrawn_type">>} := 16#0C}, Id)),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"withdrawn_type">>} := 16#07}, Id)),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"detail">>} => <<"not text">>}, Id)),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"realm_id">>} => fill(16#11)}, Id)),
    Realm = key(realm),
    Member = #{{text, <<"withdrawn_type">>} => 16#05, {text, <<"withdrawn_version">>} => <<0:128>>,
               {text, <<"reason">>} => {text, <<"revoked">>}, {text, <<"realm_id">>} => fill(16#11)},
    ?assertEqual({error, malformed}, Verify(Member, Realm)),
    ?assertMatch({ok, _}, Verify(Member#{{text, <<"member_node">>} => fill(2)}, Realm)),
    ?assertEqual({error, malformed}, Verify(Member#{{text, <<"member_node">>} => <<1:248>>}, Realm)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

signed_node() ->
    Id = key(identity),
    {Id, macula_record:sign(macula_record:node_record(macula_node_keys:key_id(Id), [], 0), Id)}.

endorsement() ->
    macula_record:realm_member_endorsement(fill(16#11), #{realm => fill(16#11), member_node => fill(2), roles => []}).

node_tombstone_payload(Node) ->
    #{{text, <<"withdrawn_type">>} => 16#01, {text, <<"withdrawn_version">>} => macula_record:version(Node),
      {text, <<"reason">>} => {text, <<"shutdown">>}}.

%% A tombstone signed over hand-built tbs fields, to reach refusals tombstone/2,3 never builds.
hand_signed(Payload, Key) ->
    Now = erlang:system_time(millisecond),
    Fields = #{{text, <<"type">>} => 16#0C, {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
               {text, <<"created_at">>} => Now, {text, <<"expires_at">>} => Now + ?DAY,
               {text, <<"payload">>} => Payload},
    macula_signed_object:sign(?LABEL, Fields, Key).

verify(Record) ->
    macula_record:verify(macula_record:encode(Record), pq_pure).

mcid() ->
    <<2, 16#55, (binary:copy(<<16#88>>, 48))/binary>>.

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
