%% EUnit tests for the foundation realm trust list as DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md and D28 pin it: its
%% payload holds exactly `realms_trusted', an array of maps each with exactly a 32-byte `realm_id' and a 32-byte
%% `realm_key_id', and a station reads the pairs back as realm id to realm key id. The pairs side of
%% verify_authorization/3 is in macula_record_advertisement_tests.
-module(macula_record_trust_list_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(MINUTE, 60000).

a_trust_list_round_trips_its_pairs_test() ->
    Foundation = key(foundation),
    RealmId = fill(16#11),
    RealmKeyId = fill(16#12),
    R = macula_record:sign(macula_record:foundation_realm_trust_list(
                             [#{realm_id => RealmId, realm_key_id => RealmKeyId},
                              #{realm_id => fill(16#13), realm_key_id => fill(16#14)}]), Foundation),
    {ok, V} = macula_record:verify(macula_record:encode(R), pq_pure),
    ?assertEqual(#{RealmId => RealmKeyId, fill(16#13) => fill(16#14)},
                 macula_record:read_foundation_realm_trust_list(V)).

the_payload_holds_exactly_realms_trusted_test() ->
    R = macula_record:foundation_realm_trust_list([#{realm_id => fill(1), realm_key_id => fill(2)}]),
    ?assertEqual(#{{text, <<"realms_trusted">>} =>
                      [#{{text, <<"realm_id">>} => fill(1), {text, <<"realm_key_id">>} => fill(2)}]},
                 macula_record:payload(R)).

the_builder_refuses_entries_that_are_not_the_pair_test() ->
    ?assertError(function_clause, macula_record:foundation_realm_trust_list([#{realm_id => fill(1)}])),
    ?assertError(function_clause, macula_record:foundation_realm_trust_list(
                                    [#{realm_id => fill(1), realm_key_id => fill(2), extra => 1}])),
    ?assertError(function_clause, macula_record:foundation_realm_trust_list(
                                    [#{realm_id => <<1:248>>, realm_key_id => fill(2)}])),
    ?assertError(function_clause, macula_record:foundation_realm_trust_list(
                                    [#{realm_id => fill(1), realm_key_id => <<1:248>>}])),
    ?assertError(function_clause, macula_record:foundation_realm_trust_list([fill(1)])).

a_trust_list_payload_outside_the_pinned_shape_is_malformed_test() ->
    Foundation = key(foundation),
    Good = #{{text, <<"realms_trusted">>} =>
                 [#{{text, <<"realm_id">>} => fill(1), {text, <<"realm_key_id">>} => fill(2)}]},
    Verify = fun(Payload) -> macula_record:verify(hand_signed(Payload, Foundation), pq_pure) end,
    ?assertMatch({ok, _}, Verify(Good)),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"realms_revoked">>} => []})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"realms_trusted">>} => [fill(1)]})),
    ?assertEqual({error, malformed},
                 Verify(Good#{{text, <<"realms_trusted">>} => [#{{text, <<"realm_id">>} => fill(1)}]})),
    ?assertEqual({error, malformed},
                 Verify(Good#{{text, <<"realms_trusted">>} =>
                                  [#{{text, <<"realm_id">>} => fill(1), {text, <<"realm_key_id">>} => fill(2),
                                     {text, <<"extra">>} => 1}]})).

the_storage_key_derives_from_the_foundation_key_id_test() ->
    Foundation = key(foundation),
    R = macula_record:sign(macula_record:foundation_realm_trust_list([]), Foundation),
    ?assertEqual(macula_record:storage_key(R),
                 macula_record:foundation_realm_trust_list_key(macula_record:key_id(R))).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A foundation trust list whose payload holds Payload as written, put in by hand: the builder writes only the
%% pinned shape.
hand_signed(Payload, Key) ->
    Now = now_ms(),
    Fields = #{{text, <<"type">>} => 16#0F, {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
               {text, <<"created_at">>} => Now, {text, <<"expires_at">>} => Now + 5 * ?MINUTE,
               {text, <<"payload">>} => Payload},
    macula_signed_object:sign(?LABEL, Fields, Key).

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

now_ms() ->
    erlang:system_time(millisecond).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
