%% EUnit tests for station authorizations (macula-station#63 D1): a realm's statement, signed by the realm key, that
%% a node serves as one of its stations at a hostname, in roles, over a window of at most 30 minutes, optionally
%% carrying the hardware-attestation pair (attestation_key_id = the full 32-byte SHA-256 of the attestation public
%% key, attestation_required = 1). The realm signs it; the (realm_id, station_node) slot is what a tombstone
%% withdraws; and the co-presence bound holds at the builder and at verify alike, so a record that demands an
%% attestation but names no key is refused both ways.
-module(macula_record_station_authorization_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(MINUTE, 60000).

realm_id() -> fill(16#11).
station() -> fill(16#22).
hostname() -> <<"pq.station-be-bru.macula.io">>.

spec() ->
    #{realm => realm_id(), station_node => station(), hostname => hostname(), roles => [<<"station">>]}.

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

now_ms() ->
    erlang:system_time(millisecond).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).

verify(Record) ->
    macula_record:verify(macula_record:encode(Record), pq_pure).

%%------------------------------------------------------------------
%% The pinned payload
%%------------------------------------------------------------------

the_payload_holds_realm_station_hostname_roles_and_window_test() ->
    Now = now_ms(),
    R = macula_record:station_authorization(realm_id(), spec(), #{valid_from => Now, valid_until => Now + ?MINUTE}),
    ?assertEqual(#{{text, <<"realm_id">>}     => realm_id(),
                   {text, <<"station_node">>} => station(),
                   {text, <<"hostname">>}     => {text, hostname()},
                   {text, <<"roles">>}        => [{text, <<"station">>}],
                   {text, <<"valid_from">>}   => Now,
                   {text, <<"valid_until">>}  => Now + ?MINUTE},
                 macula_record:payload(R)).

the_attestation_pair_travels_only_when_carried_test() ->
    Now = now_ms(),
    Bare = macula_record:station_authorization(realm_id(), spec(), #{valid_from => Now, valid_until => Now + ?MINUTE}),
    P0 = macula_record:payload(Bare),
    ?assertNot(maps:is_key({text, <<"attestation_key_id">>}, P0)),
    ?assertNot(maps:is_key({text, <<"attestation_required">>}, P0)),
    Attestation = fill(16#AA),
    Pair = macula_record:station_authorization(realm_id(), spec(),
                                               #{valid_from => Now, valid_until => Now + ?MINUTE,
                                                 attestation_key_id => Attestation, attestation_required => 1}),
    P1 = macula_record:payload(Pair),
    ?assertEqual(Attestation, maps:get({text, <<"attestation_key_id">>}, P1)),
    ?assertEqual(1, maps:get({text, <<"attestation_required">>}, P1)).

read_station_authorization_returns_the_typed_payload_test() ->
    Now = now_ms(),
    Realm = key(realm),
    R = macula_record:station_authorization(realm_id(), spec(),
                                            #{valid_from => Now, valid_until => Now + ?MINUTE,
                                              attestation_key_id => fill(16#AB)}),
    {ok, V} = verify(macula_record:sign(R, Realm)),
    ?assertEqual(#{realm_id => realm_id(), station_node => station(), hostname => hostname(),
                   roles => [<<"station">>], valid_from => Now, valid_until => Now + ?MINUTE,
                   attestation_key_id => fill(16#AB)},
                 macula_record:read_station_authorization(V)),
    Bare = macula_record:station_authorization(realm_id(), spec(), #{valid_from => Now, valid_until => Now + ?MINUTE}),
    {ok, VB} = verify(macula_record:sign(Bare, Realm)),
    ?assertEqual(#{realm_id => realm_id(), station_node => station(), hostname => hostname(),
                   roles => [<<"station">>], valid_from => Now, valid_until => Now + ?MINUTE},
                 macula_record:read_station_authorization(VB)).

%%------------------------------------------------------------------
%% The window: at most 30 minutes, never reversed
%%------------------------------------------------------------------

the_maximum_window_is_thirty_minutes_test() ->
    ?assertEqual(30 * ?MINUTE, macula_record:max_station_authorization_window_ms()).

a_window_longer_than_thirty_minutes_is_refused_by_the_builder_test() ->
    Now = now_ms(),
    ?assertError({badmatch, {error, station_authorization_window_too_long}},
                 macula_record:station_authorization(realm_id(), spec(),
                                                     #{valid_from => Now, valid_until => Now + 31 * ?MINUTE})).

a_reversed_window_is_refused_by_the_builder_test() ->
    Now = now_ms(),
    ?assertError({badmatch, {error, station_authorization_window_reversed}},
                 macula_record:station_authorization(realm_id(), spec(),
                                                     #{valid_from => Now, valid_until => Now - 1})).

%%------------------------------------------------------------------
%% The co-presence bound at the builder (#62's pair)
%%------------------------------------------------------------------

a_demanded_attestation_without_a_key_id_is_refused_by_the_builder_test() ->
    ?assertError({badmatch, {error, station_authorization_attestation_bound}},
                 macula_record:station_authorization(realm_id(), spec(), #{attestation_required => 1})).

an_attestation_key_id_that_is_not_an_id_is_refused_by_the_builder_test() ->
    ?assertError({badmatch, {error, station_authorization_attestation_bound}},
                 macula_record:station_authorization(realm_id(), spec(), #{attestation_key_id => <<"short">>})).

%%------------------------------------------------------------------
%% The realm signs it; the record verifies and refuses what it must
%%------------------------------------------------------------------

the_realm_signs_it_and_an_identity_key_cannot_test() ->
    R = macula_record:station_authorization(realm_id(), spec()),
    ?assertError({key_purpose_mismatch, _}, macula_record:sign(R, key(identity))),
    Realm = key(realm),
    Signed = macula_record:sign(R, Realm),
    ?assertMatch({ok, _}, verify(Signed)),
    {ok, V} = verify(Signed),
    ?assertEqual(16#13, macula_record:type(V)),
    ?assertEqual(macula_node_keys:key_id(Realm), macula_record:key_id(V)).

station_authorization_payloads_the_design_does_not_allow_are_malformed_test() ->
    Realm = key(realm),
    Now = now_ms(),
    Good = #{{text, <<"realm_id">>}     => realm_id(),
             {text, <<"station_node">>} => station(),
             {text, <<"hostname">>}     => {text, hostname()},
             {text, <<"roles">>}        => [{text, <<"station">>}],
             {text, <<"valid_from">>}   => Now,
             {text, <<"valid_until">>}  => Now + ?MINUTE},
    Verify = fun(Payload) -> macula_record:verify(hand_signed(Payload, Realm), pq_pure) end,
    ?assertMatch({ok, _}, Verify(Good)),
    %% The co-presence bound is refused at verify: a demand without a key, a flag that is not 1, a key id that is not
    %% a 32-byte id — while a key id without the flag is a legitimate optional attestation.
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"attestation_required">>} => 1})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"attestation_required">>} => 0})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"attestation_key_id">>} => <<"short">>})),
    ?assertMatch({ok, _}, Verify(Good#{{text, <<"attestation_key_id">>} => fill(16#AB)})),
    %% The window and field shapes.
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"valid_until">>} := Now + 31 * ?MINUTE})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"valid_until">>} := Now - 1})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"hostname">>} => <<"not text">>})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"realm_id">>} := <<"short">>})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"roles">>} => [{text, <<>>}]})),
    ?assertEqual({error, malformed}, Verify(Good#{{text, <<"roles">>} => <<"station">>})).

%%------------------------------------------------------------------
%% The slot: per realm and station, and what a tombstone withdraws
%%------------------------------------------------------------------

the_storage_key_is_the_realm_and_station_slot_test() ->
    Realm = key(realm),
    Signed = macula_record:sign(macula_record:station_authorization(realm_id(), spec()), Realm),
    ?assertEqual(macula_record:station_authorization_key(realm_id(), station()), macula_record:storage_key(Signed)),
    ?assertNotEqual(macula_record:station_authorization_key(realm_id(), fill(16#23)),
                    macula_record:storage_key(Signed)),
    ?assertNotEqual(macula_record:station_authorization_key(fill(16#12), station()), macula_record:storage_key(Signed)).

a_realm_tombstone_lands_on_the_same_slot_test() ->
    Realm = key(realm),
    Signed = macula_record:sign(macula_record:station_authorization(realm_id(), spec()), Realm),
    Tomb = macula_record:sign(macula_record:tombstone(Signed, revoked), Realm),
    ?assertMatch({ok, _}, verify(Tomb)),
    {ok, V} = verify(Tomb),
    ?assertEqual(macula_record:storage_key(Signed), macula_record:storage_key(V)),
    ?assertEqual(macula_record:key_id(Signed), macula_record:key_id(V)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

hand_signed(Payload, Key) ->
    Now = now_ms(),
    Fields = #{{text, <<"type">>} => 16#13, {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
               {text, <<"created_at">>} => Now, {text, <<"expires_at">>} => Now + ?MINUTE,
               {text, <<"payload">>} => Payload},
    macula_signed_object:sign(?LABEL, Fields, Key).
