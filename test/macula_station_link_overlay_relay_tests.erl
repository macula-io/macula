%% EUnit tests for overlay frames a station relays between two nodes' links. The HyParView frames D17 leaves unsigned
%% reach the receiving link's overlay subscribers with the origin the station authenticated as their sender, whatever
%% the inner frame names; every other relayed frame type is still delivered only when its own signature verifies against
%% that origin.
%%
%% Two real links run here, and the test process is the peering connection of both, standing in for the station: it
%% takes the envelope link A sends, readdresses it to A's node_id as a station forwards it, and hands it to link B after
%% the wire codec. Each test runs in a process of its own.
-module(macula_station_link_overlay_relay_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<9:256>>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(NODE_IDENTITY_INDEX, macula_station_link:state_field_index(node_identity)).

%% A JOIN one node sends through its link reaches the other node's overlay subscriber, with the sending node's node_id
%% as its sender.
a_relayed_join_reaches_the_other_node_with_its_origin_as_sender_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {A, B, SubRef} = two_links_with_a_subscriber_on_b(),
         ANode = node_id(A),
         Join = macula_frame:hyparview_join(#{realm => ?REALM, new_member => ANode}),
         ok = macula_station_link:send_overlay_frame(A, node_id(B), Join),
         forward_to(B, ANode, sent_envelope(node_id(B))),
         ?assertMatch({SubRef, #{frame_type := hyparview_join, new_member := ANode}, #{sender := ANode}},
                      overlay_frame_within(1_000)),
         stop_links([A, B])
     end}}.

%% The sender comes from the station's envelope only: a JOIN naming another node still reaches the subscriber with the
%% origin as its sender.
the_sender_of_a_relayed_join_is_its_origin_whatever_the_join_names_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {A, B, SubRef} = two_links_with_a_subscriber_on_b(),
         ANode = node_id(A),
         Other = crypto:strong_rand_bytes(32),
         Join = macula_frame:hyparview_join(#{realm => ?REALM, new_member => Other}),
         ok = macula_station_link:send_overlay_frame(A, node_id(B), Join),
         forward_to(B, ANode, sent_envelope(node_id(B))),
         ?assertMatch({SubRef, #{new_member := Other}, #{sender := ANode}}, overlay_frame_within(1_000)),
         stop_links([A, B])
     end}}.

%% A relayed DISCONNECT and a relayed SHUFFLE_REPLY, which travel only through the relay, each reach the other node's
%% overlay subscriber with the origin as their sender.
a_relayed_disconnect_and_shuffle_reply_reach_the_other_node_with_the_origin_as_sender_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {A, B, SubRef} = two_links_with_a_subscriber_on_b(),
         ANode = node_id(A),
         Frames = [macula_frame:hyparview_disconnect(#{realm => ?REALM}),
                   macula_frame:hyparview_shuffle_reply(#{realm => ?REALM, peer_sample => [<<5:256>>]})],
         [begin
              ok = macula_station_link:send_overlay_frame(A, node_id(B), Frame),
              forward_to(B, ANode, sent_envelope(node_id(B))),
              ?assertMatch({SubRef, #{frame_type := Type}, #{sender := ANode}}, overlay_frame_within(1_000))
          end || #{frame_type := Type} = Frame <- Frames],
         stop_links([A, B])
     end}}.

%% A relayed frame of a type D17 signs is not delivered without a signature: the predicate opens only the overlay types.
an_unsigned_relayed_frame_of_another_type_is_not_delivered_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {A, B, _SubRef} = two_links_with_a_subscriber_on_b(),
         Ihave = macula_frame:plumtree_ihave(#{realm => ?REALM, msg_id => crypto:strong_rand_bytes(48), round => 0}),
         forward_to(B, node_id(A), macula_frame:overlay_relay(#{peer => node_id(B),
                                                                payload => macula_frame:encode(Ihave)})),
         ?assertEqual(none, overlay_frame_within(300)),
         ?assertEqual(#{unsigned => 1}, refused_relays(B)),
         stop_links([A, B])
     end}}.

%% A relayed frame of another type whose signature does not verify against the origin is refused, as before.
a_relayed_frame_of_another_type_with_an_invalid_signature_is_refused_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {A, B, _SubRef} = two_links_with_a_subscriber_on_b(),
         Ihave = macula_frame:plumtree_ihave(#{realm => ?REALM, msg_id => crypto:strong_rand_bytes(48), round => 0}),
         Forged = macula_frame:sign(Ihave, macula_identity:generate()),
         forward_to(B, node_id(A), macula_frame:overlay_relay(#{peer => node_id(B),
                                                                payload => macula_frame:encode(Forged)})),
         ?assertEqual(none, overlay_frame_within(300)),
         ?assertEqual(#{signature_invalid => 1}, refused_relays(B)),
         stop_links([A, B])
     end}}.

%% The link takes a relayed envelope only from its own connection, the one that verified it (in pq_hybrid, its neighbour
%% signature): an envelope from any other process reaches no subscriber.
a_relayed_envelope_from_another_process_reaches_no_subscriber_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {A, B, _SubRef} = two_links_with_a_subscriber_on_b(),
         Join = macula_frame:hyparview_join(#{realm => ?REALM, new_member => node_id(A)}),
         Envelope = received(macula_frame:overlay_relay(#{peer => node_id(A), payload => macula_frame:encode(Join)})),
         Stranger = spawn(fun() -> receive go -> ok end end),
         B ! {macula_peering, frame, Stranger, Envelope},
         ?assertEqual(none, overlay_frame_within(300)),
         Stranger ! go,
         stop_links([A, B])
     end}}.

%% A relayed payload that is not exactly one frame is dropped and counted by kind: an empty payload, a truncated JOIN,
%% bytes that do not decode, and a JOIN with a byte after it. The link carries on, and a JOIN relayed after them still
%% reaches the subscriber with its origin as sender.
a_relayed_payload_that_is_not_exactly_one_frame_is_dropped_and_the_link_carries_on_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {A, B, SubRef} = two_links_with_a_subscriber_on_b(),
         ANode = node_id(A),
         Join = macula_frame:encode(macula_frame:hyparview_join(#{realm => ?REALM, new_member => ANode})),
         Payloads = [<<>>, binary:part(Join, 0, byte_size(Join) - 1), <<3:32, 16#FF, 16#FF, 16#FF>>,
                     <<Join/binary, 0>>],
         _ = [forward_to(B, ANode, #{payload => Payload}) || Payload <- Payloads],
         ?assertEqual(none, overlay_frame_within(300)),
         forward_to(B, ANode, #{payload => Join}),
         ?assertMatch({SubRef, #{frame_type := hyparview_join, new_member := ANode}, #{sender := ANode}},
                      overlay_frame_within(1_000)),
         ?assert(is_process_alive(B)),
         ?assertEqual(#{truncated => 2, bad_frame => 1, trailing_bytes => 1}, refused_relays(B)),
         stop_links([A, B])
     end}}.

%% A relayed FORWARD_JOIN, one of the overlay types D17 leaves unsigned, reaches the other node's subscriber with the
%% origin as its sender.
a_relayed_forward_join_reaches_the_other_node_with_the_origin_as_sender_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {A, B, SubRef} = two_links_with_a_subscriber_on_b(),
         ANode = node_id(A),
         Forward = macula_frame:hyparview_forward_join(#{realm => ?REALM, new_member => <<5:256>>, ttl => 3,
                                                          arwl => 6, prwl => 3}),
         ok = macula_station_link:send_overlay_frame(A, node_id(B), Forward),
         forward_to(B, ANode, sent_envelope(node_id(B))),
         ?assertMatch({SubRef, #{frame_type := hyparview_forward_join, new_member := <<5:256>>}, #{sender := ANode}},
                      overlay_frame_within(1_000)),
         stop_links([A, B])
     end}}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% The relayed frames a link dropped, counted by kind.
refused_relays(Pid) ->
    macula_refusal_report:counts(element(macula_station_link:state_field_index(refused_relays), sys:get_state(Pid))).

two_links_with_a_subscriber_on_b() ->
    A = start_link_to_station(),
    B = start_link_to_station(),
    {ok, SubRef} = macula_station_link:overlay_subscribe(B, ?REALM, self()),
    {A, B, SubRef}.

with_link_keys(Opts) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, expected_node_id => <<1:256>>}.

%% A link that believes it is connected to a station, with this process as its peer.
start_link_to_station() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, StationKey} = macula_node_keys:generate(identity, Profile),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                                 connect_timeout_ms => 2000})),
    Peer = self(),
    Station = macula_node_keys:key_id(StationKey),
    _ = sys:replace_state(Pid, fun(S) -> setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Peer),
                                                    Station) end),
    Pid.

node_id(Pid) ->
    macula_node_keys:key_id(element(?NODE_IDENTITY_INDEX, sys:get_state(Pid))).

%% The overlay_relay envelope a link sent to `Target', as its station receives it.
sent_envelope(Target) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := overlay_relay} = Frame}} ->
            #{peer := Target} = Envelope = received(Frame),
            Envelope
    after 1_000 ->
        erlang:error(no_envelope_sent)
    end.

%% Forwards an envelope to link `To' as a station does: readdressed to the origin it authenticated.
forward_to(To, Origin, #{payload := Payload}) ->
    To ! {macula_peering, frame, self(), received(macula_frame:overlay_relay(#{peer => Origin, payload => Payload}))}.

received(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

overlay_frame_within(Ms) ->
    receive
        {macula_overlay_frame, SubRef, Frame, Meta} -> {SubRef, Frame, Meta}
    after Ms ->
        none
    end.

stop_links(Pids) ->
    [macula_station_link:stop(Pid) || Pid <- Pids],
    ok.
