%% EUnit tests for REGISTER_ACK (macula#12, wire 1.0): a station answers each ADVERTISE, UNADVERTISE, SUBSCRIBE and
%% UNSUBSCRIBE with one, naming the registration by kind, realm and name, and by the advertisement's record_version
%% or the subscriber, and saying whether it was accepted. A refusal carries a reason from a closed set; a reason this
%% node does not know reads as the generic refused, and a kind it does not know as unknown, each counted.
-module(macula_frame_register_ack_tests).

-include_lib("eunit/include/eunit.hrl").

register_ack_test_() ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(macula) end, fun(_) -> ok end,
     [{"an accepted advertise travels", fun an_accepted_advertise_travels/0},
      {"a refused subscribe travels with its reason", fun a_refused_subscribe_travels_with_its_reason/0},
      {"every reason of the set travels", fun every_reason_of_the_set_travels/0},
      {"an accepted ack carries no reason", fun an_accepted_ack_carries_no_reason/0},
      {"a refusal names its reason", fun a_refusal_names_its_reason/0},
      {"an advertise kind names a record version and a pubsub kind a subscriber",
       fun an_advertise_kind_names_a_record_version_and_a_pubsub_kind_a_subscriber/0},
      {"a name past 512 bytes is refused", fun a_name_past_512_bytes_is_refused/0},
      {"the name travels as text, and an empty one is refused", fun the_name_travels_as_text_and_an_empty_one_is_refused/0},
      {"an unknown reason reads as refused and is counted", fun an_unknown_reason_reads_as_refused_and_is_counted/0},
      {"an unknown kind reads as unknown and is counted", fun an_unknown_kind_reads_as_unknown_and_is_counted/0},
      {"accepted travels as 0 or 1", fun accepted_travels_as_0_or_1/0},
      {"a register ack is a control frame", fun a_register_ack_is_a_control_frame/0}]}.

an_accepted_advertise_travels() ->
    Ack = advertise_ack(#{accepted => true}),
    ?assertEqual(register_ack, macula_frame:frame_type(Ack)),
    ?assertMatch(#{kind := advertise, realm := <<7:256>>, name := <<"acme.count_v1">>, record_version := <<2:128>>,
                   accepted := true},
                 roundtrip(Ack)).

a_refused_subscribe_travels_with_its_reason() ->
    Ack = subscribe_ack(#{accepted => false, reason => too_many_realms}),
    ?assertMatch(#{kind := subscribe, subscriber := <<1:256>>, name := <<"acme.counted">>, accepted := false,
                   reason := too_many_realms},
                 roundtrip(Ack)).

every_reason_of_the_set_travels() ->
    Reasons = [too_many_providers, advertiser_quota, name_too_long, record_refused, not_authorized, too_many_realms,
               refused],
    [?assertMatch(#{reason := Reason}, roundtrip(advertise_ack(#{accepted => false, reason => Reason})))
     || Reason <- Reasons].

an_accepted_ack_carries_no_reason() ->
    ?assertError(function_clause, advertise_ack(#{accepted => true, reason => refused})),
    ?assertEqual({error, {invalid_frame, register_ack, reason}}, decode_map((advertise_ack(#{accepted => true}))#{reason => refused})).

a_refusal_names_its_reason() ->
    ?assertError(function_clause, advertise_ack(#{accepted => false})),
    ?assertError(function_clause, advertise_ack(#{accepted => false, reason => quota})),
    Refused = advertise_ack(#{accepted => false, reason => refused}),
    ?assertEqual({error, {invalid_frame, register_ack, reason}}, decode_map(maps:remove(reason, Refused))).

an_advertise_kind_names_a_record_version_and_a_pubsub_kind_a_subscriber() ->
    Ad = advertise_ack(#{accepted => true}),
    Sub = subscribe_ack(#{accepted => true}),
    ?assertError(function_clause, macula_frame:register_ack(maps:remove(record_version, spec(Ad)))),
    ?assertError(function_clause, macula_frame:register_ack((spec(Sub))#{record_version => <<2:128>>})),
    ?assertEqual({error, {invalid_frame, register_ack, record_version}}, decode_map((maps:remove(record_version, Ad))#{subscriber => <<1:256>>})),
    ?assertEqual({error, {invalid_frame, register_ack, subscriber}}, decode_map((maps:remove(subscriber, Sub))#{record_version => <<2:128>>})),
    ?assertMatch(#{kind := unadvertise, record_version := <<2:128>>},
                 roundtrip(macula_frame:register_ack((spec(Ad))#{kind => unadvertise}))),
    ?assertMatch(#{kind := unsubscribe, subscriber := <<1:256>>},
                 roundtrip(macula_frame:register_ack((spec(Sub))#{kind => unsubscribe}))).

a_name_past_512_bytes_is_refused() ->
    Ack = advertise_ack(#{accepted => true}),
    ?assertMatch(#{name := <<_:512/binary>>}, roundtrip(Ack#{name => binary:copy(<<"a">>, 512)})),
    ?assertError(function_clause, macula_frame:register_ack((spec(Ack))#{name => binary:copy(<<"a">>, 513)})),
    ?assertMatch({error, _}, decode_map(Ack#{name => binary:copy(<<"a">>, 513)})).

the_name_travels_as_text_and_an_empty_one_is_refused() ->
    Ack = advertise_ack(#{accepted => true}),
    <<_Length:32, Bytes/binary>> = macula_frame:encode(Ack),
    ?assertMatch({ok, #{{text, <<"name">>} := {text, <<"acme.count_v1">>}}}, macula_record_cbor:decode_strict(Bytes)),
    ?assertError(function_clause, macula_frame:register_ack((spec(Ack))#{name => <<>>})),
    ?assertEqual({error, {invalid_frame, register_ack, name}}, decode_map(Ack#{name => <<>>})).

an_unknown_reason_reads_as_refused_and_is_counted() ->
    Before = count({wire_enum_fallback, reason}),
    Ack = advertise_ack(#{accepted => false, reason => refused}),
    ?assertMatch({ok, #{accepted := false, reason := refused}, <<>>},
                 macula_frame:decode(with_text(Ack, <<"reason">>, <<"a_future_reason">>))),
    ?assertEqual(Before + 1, count({wire_enum_fallback, reason})).

an_unknown_kind_reads_as_unknown_and_is_counted() ->
    Before = count({wire_enum_fallback, kind}),
    Ack = advertise_ack(#{accepted => true}),
    ?assertMatch({ok, #{kind := unknown}, <<>>}, macula_frame:decode(with_text(Ack, <<"kind">>, <<"a_future_kind">>))),
    ?assertEqual(Before + 1, count({wire_enum_fallback, kind})).

accepted_travels_as_0_or_1() ->
    Ack = advertise_ack(#{accepted => true}),
    <<_Length:32, Bytes/binary>> = macula_frame:encode(Ack),
    ?assertMatch({ok, #{{text, <<"accepted">>} := 1}}, macula_record_cbor:decode_strict(Bytes)),
    [?assertEqual({error, bad_frame}, decode_map(Ack#{accepted => Value})) || Value <- [{text, <<"true">>}, 2]].

a_register_ack_is_a_control_frame() ->
    ?assert(macula_frame:control_frame(register_ack)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

advertise_ack(Extra) ->
    macula_frame:register_ack(maps:merge(#{kind => advertise, realm => <<7:256>>, name => <<"acme.count_v1">>,
                                           record_version => <<2:128>>}, Extra)).

subscribe_ack(Extra) ->
    macula_frame:register_ack(maps:merge(#{kind => subscribe, realm => <<7:256>>, name => <<"acme.counted">>,
                                           subscriber => <<1:256>>}, Extra)).

%% The spec a frame was built from.
spec(Frame) ->
    maps:with([kind, realm, name, record_version, subscriber, accepted, reason], Frame).

roundtrip(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

decode_map(Frame) ->
    macula_frame:decode(macula_frame:encode(Frame)).

%% The frame's bytes with one text field set to Value, as a newer peer would send it.
with_text(Frame, Field, Value) ->
    <<_Length:32, Bytes/binary>> = macula_frame:encode(Frame),
    {ok, Wire} = macula_record_cbor:decode_strict(Bytes),
    Body = macula_record_cbor:encode(Wire#{{text, Field} => {text, Value}}),
    <<(byte_size(Body)):32, Body/binary>>.

count(Key) ->
    maps:get(Key, macula_wire_tolerance:counters(), 0).
