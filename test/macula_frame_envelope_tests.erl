%% EUnit tests for the frame envelope (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Encoding and Peer-supplied maps, D26): a
%% frame decodes under the decoding rule; a frame type's own fields decode through a fixed table, so a frame type or a
%% field the table does not define, or an enum value it does not list, is refused; an envelope boolean travels as 0 or
%% 1; a GOODBYE reason is text of at most 256 bytes and a STORE_ACK carries no reason; payloads keep one key
%% form, the same on a fresh node as on a warm one; and records travel as their wire bytes.
-module(macula_frame_envelope_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Records travel as their wire bytes
%%------------------------------------------------------------------

records_travel_as_their_wire_bytes_test() ->
    Bytes = record_bytes(),
    Frames = [macula_frame:store(#{record => Bytes}),
              macula_frame:hyparview_join(#{realm => fill(1), new_member => fill(2), record => Bytes})],
    [?assertEqual(Bytes, maps:get(record, roundtrip(Frame))) || Frame <- Frames],
    ?assertEqual([Bytes, Bytes],
                 maps:get(records, roundtrip(macula_frame:value(#{key => fill(3), records => [Bytes, Bytes]})))),
    ?assertMatch({ok, _}, macula_record:verify(maps:get(record, roundtrip(hd(Frames))), pq_pure)).

a_record_that_is_not_bytes_is_refused_test() ->
    Map = #{type => 1, key => fill(1), payload => #{}},
    ?assertError(function_clause, macula_frame:store(#{record => Map})),
    ?assertError(function_clause, macula_frame:value(#{key => fill(3), records => [Map]})).

check_frame_judges_record_bytes_like_any_binary_test() ->
    TooBig = binary:copy(<<"x">>, 16#FFFFFF + 1),
    ?assertMatch({error, {unsupported_payload_type, payload_too_large, []}},
                 macula_frame:check_frame(#{frame_type => store, record => TooBig})).

%%------------------------------------------------------------------
%% The fixed table
%%------------------------------------------------------------------

a_frame_without_a_known_frame_type_is_refused_test() ->
    ?assertEqual({error, bad_frame}, decode_map(#{frame_type => not_a_frame_type, nonce => <<0:128>>})),
    ?assertEqual({error, bad_frame}, decode_map(#{payload => 1})).

a_field_its_frame_type_does_not_define_is_refused_test() ->
    Ping = macula_frame:ping(#{nonce => <<0:128>>}),
    ?assertMatch({ok, _, <<>>}, decode_map(Ping)),
    ?assertEqual({error, bad_frame}, decode_map(Ping#{payload => 1})).

an_enum_value_the_table_does_not_list_is_refused_test() ->
    Neighbour = macula_frame:hyparview_neighbor(#{realm => fill(1), priority => high}),
    ?assertEqual({error, bad_frame}, decode_map(Neighbour#{priority => medium})).

%% A value no code produces and reads is not in the table: EVENT delivered_via dht, and a MANIFEST_RES not_found.
values_without_a_producer_and_a_reader_are_refused_test() ->
    {ok, Publisher} = macula_node_keys:generate(identity, pq_pure),
    #{publication := Publication} =
        macula_frame:publish(#{realm => fill(1), topic => <<"t">>, seq => 0, published_at => 1, payload => 1},
                             Publisher),
    Event = macula_frame:event(#{publication => Publication, delivered_via => direct}),
    ?assertMatch({ok, #{delivered_via := direct}, <<>>}, decode_map(Event)),
    ?assertEqual({error, bad_frame}, decode_map(Event#{delivered_via => dht})),
    ManifestRes = macula_frame:manifest_res(#{mcid => <<2, 0, 0:384>>, manifest => #{}}),
    ?assertMatch({ok, #{manifest := #{}}, <<>>}, decode_map(ManifestRes)),
    ?assertEqual({error, bad_frame}, decode_map(ManifestRes#{manifest => not_found})).

%%------------------------------------------------------------------
%% Reasons and booleans
%%------------------------------------------------------------------

%% A GOODBYE reason is text for people, whether or not it is a name a stack knows.
a_goodbye_reason_is_free_text_test() ->
    Reasons = [draining, replaced_by_newer_handshake],
    [?assertEqual({text, atom_to_binary(Reason)},
                  maps:get(reason, roundtrip(macula_frame:goodbye(Reason, undefined)))) || Reason <- Reasons].

a_goodbye_reason_is_text_of_at_most_256_bytes_test() ->
    Goodbye = macula_frame:goodbye(draining, undefined),
    Within = binary:copy(<<"a">>, 256),
    ?assertMatch({ok, #{reason := {text, Within}}, <<>>}, decode_map(Goodbye#{reason => {text, Within}})),
    ?assertEqual({error, bad_frame}, decode_map(Goodbye#{reason => {text, <<Within/binary, "a">>}})),
    ?assertEqual({error, bad_frame}, decode_map(Goodbye#{reason => 7})),
    %% 129 two-byte characters: a valid atom, and 258 bytes of UTF-8.
    TooLong = binary_to_atom(binary:copy(<<16#C3, 16#A9>>, 129)),
    ?assertError(function_clause, macula_frame:goodbye(TooLong, undefined)).

a_store_ack_carries_no_reason_test() ->
    Refused = macula_frame:store_ack(#{key => fill(3), stored => false}),
    ?assertNot(maps:is_key(reason, roundtrip(Refused))),
    ?assertEqual({error, bad_frame}, decode_map(Refused#{reason => quota})).

an_envelope_boolean_travels_as_0_or_1_test() ->
    Stored = macula_frame:store_ack(#{key => fill(3), stored => true}),
    <<_Length:32, Bytes/binary>> = macula_frame:encode(Stored),
    ?assertMatch({ok, #{{text, <<"stored">>} := 1}}, macula_record_cbor:decode_strict(Bytes)),
    ?assertEqual(true, maps:get(stored, roundtrip(Stored))),
    ?assertEqual(false, maps:get(stored, roundtrip(macula_frame:store_ack(#{key => fill(3), stored => false})))).

an_envelope_boolean_other_than_0_or_1_is_refused_test() ->
    Stored = macula_frame:store_ack(#{key => fill(3), stored => true}),
    [?assertEqual({error, bad_frame}, decode_map(Stored#{stored => Value})) || Value <- [{text, <<"true">>}, 2]].

%%------------------------------------------------------------------
%% The decoding rule
%%------------------------------------------------------------------

a_duplicate_key_is_refused_test() ->
    FrameType = text(<<"frame_type">>),
    Bytes = <<16#A2, FrameType/binary, (text(<<"ping">>))/binary, FrameType/binary, (text(<<"pong">>))/binary>>,
    ?assertEqual({error, bad_frame}, macula_frame:decode(macula_frame:encode_bytes(Bytes))).

%% The frame codec refuses what the decoding rule refuses, wherever it sits in the frame; the shared vectors pin the
%% accepted side.
a_negative_integer_below_minus_2_pow_63_is_refused_test() ->
    ?assertEqual({error, bad_frame}, decode_ping_with(<<16#3B, (1 bsl 63):64>>)).

a_positive_integer_above_2_pow_63_minus_1_is_refused_test() ->
    ?assertEqual({error, bad_frame}, decode_ping_with(<<16#1B, (1 bsl 63):64>>)).

invalid_utf8_text_is_refused_test() ->
    ?assertEqual({error, bad_frame}, decode_ping_with(<<16#61, 16#FF>>)).

nesting_past_the_decoding_rule_is_refused_test() ->
    ?assertEqual({error, bad_frame}, decode_ping_with(<<(binary:copy(<<16#81>>, 64))/binary, 0>>)).

check_payload_refuses_what_the_decoding_rule_refuses_test() ->
    ?assertEqual(ok, macula_frame:check_payload(-(1 bsl 63))),
    ?assertMatch({error, {unsupported_payload_type, integer_out_of_range, []}},
                 macula_frame:check_payload(-(1 bsl 63) - 1)),
    ?assertEqual(ok, macula_frame:check_payload((1 bsl 63) - 1)),
    ?assertMatch({error, {unsupported_payload_type, integer_out_of_range, []}}, macula_frame:check_payload(1 bsl 63)),
    ?assertMatch({error, {unsupported_payload_type, invalid_text, []}}, macula_frame:check_payload({text, <<16#FF>>})),
    ?assertMatch({error, {unsupported_payload_type, invalid_text, []}}, macula_frame:check_payload(#{<<16#FF>> => 1})),
    ?assertEqual(ok, macula_frame:check_payload(nested(63))),
    ?assertMatch({error, {unsupported_payload_type, too_deep, _}}, macula_frame:check_payload(nested(64))).

%%------------------------------------------------------------------
%% One key form for payloads (D26)
%%------------------------------------------------------------------

a_decoded_payload_holds_no_atoms_test() ->
    Payload = #{station => {text, <<"station">>}, <<"kind">> => {text, <<"daemon">>}, list => [ok]},
    ?assertEqual(#{{text, <<"station">>} => {text, <<"station">>}, {text, <<"kind">>} => {text, <<"daemon">>},
                   {text, <<"list">>} => [{text, <<"ok">>}]},
                 request_payload(macula_frame:encode(signed_call(Payload)))).

%% The same frame decodes to the same payload map on a node that holds the payload's names as atoms and on one that
%% does not. The fresh node is checked first, so the test cannot pass by accident.
payload_maps_decode_the_same_on_a_fresh_node_and_a_warm_node_test_() ->
    {timeout, 60, fun fresh_and_warm_nodes_agree/0}.

fresh_and_warm_nodes_agree() ->
    Name = <<"d26_probe_field_name_2a">>,
    Payload = #{{text, Name} => {text, Name}, {text, <<"nested">>} => #{{text, Name} => 1}},
    Bin = macula_frame:encode(signed_call(Payload)),
    _ = binary_to_atom(Name),
    Paths = lists:append([["-pa", Path] || Path <- code:get_path()]),
    {ok, Peer, _Node} = peer:start_link(#{connection => standard_io, args => Paths}),
    try
        ?assertMatch({'EXIT', _}, catch peer:call(Peer, erlang, binary_to_existing_atom, [Name, utf8])),
        {ok, Fresh, <<>>} = peer:call(Peer, macula_frame, decode, [Bin]),
        {ok, #{payload := FreshPayload}} = peer:call(Peer, macula_frame, verify_request, [Fresh, pq_pure]),
        ?assertEqual(FreshPayload, request_payload(Bin)),
        ?assertEqual(Payload, request_payload(Bin))
    after
        peer:stop(Peer)
    end.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

record_bytes() ->
    {ok, Id} = macula_node_keys:generate(identity, pq_pure),
    macula_record:encode(macula_record:sign(macula_record:node_record(macula_node_keys:key_id(Id), [], 0), Id)).

roundtrip(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

decode_map(Frame) ->
    macula_frame:decode(macula_frame:encode(Frame)).

text(Bin) ->
    macula_record_cbor:encode({text, Bin}).

%% A PING whose nonce is the given CBOR item, as raw frame bytes through the frame codec.
decode_ping_with(Item) ->
    Head = <<16#A2, (text(<<"frame_type">>))/binary, (text(<<"ping">>))/binary, (text(<<"nonce">>))/binary>>,
    macula_frame:decode(macula_frame:encode_bytes(<<Head/binary, Item/binary>>)).

%% A CALL carrying the payload, signed by a fresh caller key.
signed_call(Payload) ->
    {ok, Caller} = macula_node_keys:generate(identity, pq_pure),
    macula_frame:call(#{request_id => <<1:128>>, realm => fill(1), procedure => <<"p">>, target => fill(2),
                        deadline => 1, payload => Payload}, Caller).

%% The payload of an encoded CALL as a receiver reads it: decoded, then verified.
request_payload(Bin) ->
    {ok, Frame, <<>>} = macula_frame:decode(Bin),
    {ok, #{payload := Payload}} = macula_frame:verify_request(Frame, pq_pure),
    Payload.

nested(0) -> 1;
nested(N) -> [nested(N - 1)].

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
