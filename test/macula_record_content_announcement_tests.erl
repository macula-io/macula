%% EUnit tests for content announcements in the signed-object format: the announcer signs its own announcement of a
%% tag 2 content id, naming where it is served (D27): the realm, the station it is reachable through and its content
%% procedure. The announcement is stored under the content id's storage key, so every announcer of one content id
%% shares a slot.
-module(macula_record_content_announcement_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(HOUR, 3600000).

the_payload_names_announcer_content_id_and_where_test() ->
    R = macula_record:content_announcement(fill(1), mcid(), where()),
    ?assertEqual(#{{text, <<"announcer_node">>} => fill(1), {text, <<"mcid">>} => mcid(),
                   {text, <<"realm_id">>} => fill(7), {text, <<"serving_station">>} => fill(9),
                   {text, <<"procedure">>} => {text, procedure()}},
                 macula_record:payload(R)).

metadata_is_carried_when_given_test() ->
    R = macula_record:content_announcement(fill(1), mcid(),
                                           (where())#{name => <<"report.pdf">>, size => 1048576, chunk_count => 4}),
    P = macula_record:payload(R),
    ?assertEqual({text, <<"report.pdf">>}, maps:get({text, <<"name">>}, P)),
    ?assertEqual(1048576, maps:get({text, <<"size">>}, P)),
    ?assertEqual(4, maps:get({text, <<"chunk_count">>}, P)).

the_constructor_refuses_a_content_id_that_is_not_tag_2_sha384_test() ->
    ?assertError(function_clause, macula_record:content_announcement(fill(1), <<2, 16#55, 0:256>>, where())),
    ?assertError(function_clause, macula_record:content_announcement(fill(1), <<1, 16#55, 0:384>>, where())).

%% Where it is served is not optional: an announcement without it names nowhere to fetch from.
the_constructor_requires_where_it_is_served_test() ->
    [?assertError(function_clause, macula_record:content_announcement(fill(1), mcid(), maps:without([K], where())))
     || K <- [realm_id, serving_station, procedure]],
    [?assertError(function_clause, macula_record:content_announcement(fill(1), mcid(), maps:merge(where(), Bad)))
     || Bad <- [#{realm_id => <<1, 2>>}, #{serving_station => <<1:8>>}, #{procedure => 42}]].

a_signed_announcement_verifies_test() ->
    Id = key(),
    Unsigned = macula_record:content_announcement(macula_node_keys:key_id(Id), mcid(), where()),
    R = macula_record:sign(Unsigned, Id),
    ?assertMatch({ok, _}, macula_record:verify(macula_record:encode(R), pq_pure)).

sign_refuses_an_announcement_for_another_announcer_test() ->
    ?assertError({key_id_mismatch, _},
                 macula_record:sign(macula_record:content_announcement(fill(9), mcid(), where()), key())).

a_verifier_refuses_a_content_id_that_is_not_tag_2_test() ->
    ?assertEqual({error, malformed}, verify_crafted((payload_of(key()))#{{text, <<"mcid">>} => <<1, 16#55, 0:384>>})).

%% A verifier refuses an announcement that does not say where its content is served, as an older one did.
a_verifier_refuses_an_announcement_without_where_test() ->
    Id = key(),
    [?assertEqual({error, malformed}, verify_crafted(maps:remove({text, Field}, payload_of(Id)), Id))
     || Field <- [<<"realm_id">>, <<"serving_station">>, <<"procedure">>]],
    ?assertEqual({error, malformed},
                 verify_crafted((payload_of(Id))#{{text, <<"procedure">>} => {text, <<>>}}, Id)).

every_announcer_of_one_content_id_shares_its_slot_test() ->
    Key = fun(Announcer) -> macula_record:storage_key(macula_record:content_announcement(Announcer, mcid(), where())) end,
    ?assertEqual(macula_record:content_key(mcid()), Key(fill(1))),
    ?assertEqual(Key(fill(1)), Key(fill(2))).

content_key_refuses_a_content_id_of_the_wrong_size_test() ->
    ?assertError(function_clause, macula_record:content_key(<<2, 16#55, 0:256>>)).

read_content_announcement_returns_the_typed_payload_test() ->
    Id = key(),
    NodeId = macula_node_keys:key_id(Id),
    Full = macula_record:content_announcement(NodeId, mcid(), (where())#{name => <<"a.bin">>, size => 10,
                                                                         chunk_count => 1}),
    {ok, V} = macula_record:verify(macula_record:encode(macula_record:sign(Full, Id)), pq_pure),
    ?assertEqual(#{announcer_node => NodeId, mcid => mcid(), realm_id => fill(7), serving_station => fill(9),
                   procedure => procedure(), name => <<"a.bin">>, size => 10, chunk_count => 1},
                 macula_record:read_content_announcement(V)),
    ?assertEqual(#{announcer_node => NodeId, mcid => mcid(), realm_id => fill(7), serving_station => fill(9),
                   procedure => procedure(), name => undefined, size => undefined, chunk_count => undefined},
                 macula_record:read_content_announcement(macula_record:content_announcement(NodeId, mcid(), where()))).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

where() ->
    #{realm_id => fill(7), serving_station => fill(9), procedure => procedure()}.

procedure() ->
    <<"acme/content_v1_", (binary:encode_hex(fill(1), lowercase))/binary>>.

payload_of(Id) ->
    #{{text, <<"announcer_node">>} => macula_node_keys:key_id(Id), {text, <<"mcid">>} => mcid(),
      {text, <<"realm_id">>} => fill(7), {text, <<"serving_station">>} => fill(9),
      {text, <<"procedure">>} => {text, procedure()}}.

verify_crafted(Payload) ->
    verify_crafted(Payload, key()).

%% An announcement signed by hand, around the constructor, so a verifier meets what the constructor refuses to build.
verify_crafted(Payload, Id) ->
    Now = erlang:system_time(millisecond),
    Signer = macula_node_keys:key_id(Id),
    Fields = #{{text, <<"type">>} => 16#11, {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
               {text, <<"created_at">>} => Now, {text, <<"expires_at">>} => Now + ?HOUR,
               {text, <<"payload">>} => Payload#{{text, <<"announcer_node">>} => Signer}},
    macula_record:verify(macula_signed_object:sign(?LABEL, Fields, Id), pq_pure).

key() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Key.

mcid() ->
    <<2, 16#55, (binary:copy(<<16#88>>, 48))/binary>>.

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
