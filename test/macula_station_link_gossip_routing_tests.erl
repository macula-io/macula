%% EUnit tests for how a station link routes a Plumtree GOSSIP, which names its realm only inside its signed
%% publication. A GOSSIP from the connected peer, and one a station relays, goes to the overlay subscribers of the realm
%% its publication claims, read without verifying, and the subscriber verifies the publication before acting on it. A
%% relayed delivery carries the relaying station as via. A GOSSIP whose publication names no realm, or names a realm
%% with no subscriber on the link, is counted and not delivered.
%%
%% A subscriber reports a refusal of what a delivered frame carries through overlay_frame_refused/3. The link reports it
%% to its connection, which charges it as macula_frame:charged_refusal/1 says, only for a frame that provably came from
%% its current peer: a Meta without via that names that peer as sender. Every other report, a relayed frame's, one whose
%% Meta lost its via, and one from before the link took another peer, is counted on the link and charges no one, and a
%% kind no rule classifies is counted as unknown_refusal.
%%
%% The test process is the peering connection of each link, standing in for the station. Each test runs in a process
%% of its own.
-module(macula_station_link_gossip_routing_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<9:256>>).
-define(OTHER_REALM, <<8:256>>).
-define(ORIGIN, <<5:256>>).
-define(PUBLICATION_LABEL, <<"MACULA-PQ-PUBLICATION-V1">>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(NODE_IDENTITY_INDEX, macula_station_link:state_field_index(node_identity)).

%% A GOSSIP one node sends through its link reaches the other node's subscriber of the realm its publication claims,
%% with the sending node as sender and the relaying station as via.
a_relayed_gossip_reaches_its_realm_subscriber_with_the_origin_as_sender_and_the_station_as_via_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {A, B, SubRef} = two_links_with_a_subscriber_on_b(),
         ANode = node_id(A),
         Gossip = gossip(publication(?REALM)),
         ok = macula_station_link:send_overlay_frame(A, node_id(B), Gossip),
         forward_to(B, ANode, sent_envelope(node_id(B))),
         ?assertEqual({SubRef, received(Gossip), #{sender => ANode, via => station_of(B)}},
                      overlay_frame_within(1_000)),
         stop_links([A, B])
     end}}.

%% A GOSSIP from the connected peer reaches the subscriber of the realm its publication claims, with that peer as
%% sender and no via.
a_direct_gossip_reaches_its_realm_subscriber_with_the_connected_peer_as_sender_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {B, SubRef} = link_with_a_subscriber(),
         Gossip = received(gossip(publication(?REALM))),
         B ! {macula_peering, frame, self(), Gossip},
         ?assertEqual({SubRef, Gossip, #{sender => station_of(B)}}, overlay_frame_within(1_000)),
         stop_links([B])
     end}}.

%% A relayed GOSSIP whose publication does not verify reaches the subscriber as it arrived, and Plumtree delivers
%% nothing from it. The subscriber's reports of refusals are counted on the link by kind name, and the station
%% connection that relayed the frame hears nothing of them, so nothing is charged to it.
a_relayed_gossip_that_does_not_verify_is_refused_by_plumtree_and_not_charged_to_the_station_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {B, SubRef} = link_with_a_subscriber(),
         Gossip = gossip(tampered(publication(?REALM))),
         forward_to(B, ?ORIGIN, #{payload => macula_frame:encode(Gossip)}),
         {SubRef, Arrived, Meta} = overlay_frame_within(1_000),
         ?assertEqual(received(Gossip), Arrived),
         ?assertEqual({[{refused, ?ORIGIN, signature_invalid}], []}, plumtree_outcome(?ORIGIN, Arrived)),
         ok = macula_station_link:overlay_frame_refused(B, Meta, signature_invalid),
         ok = macula_station_link:overlay_frame_refused(B, Meta, {expired, 700_000}),
         ?assertEqual(#{signature_invalid => 1, expired => 1}, refused_relays(B)),
         ?assertEqual(none, connection_report(0)),
         stop_links([B])
     end}}.

%% A GOSSIP from the connected peer whose publication does not verify reaches the subscriber as it arrived, Plumtree
%% delivers nothing from it, and the subscriber's report of that refusal reaches the connection, charged.
a_direct_gossip_that_does_not_verify_is_refused_by_plumtree_and_charged_to_the_connection_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {B, SubRef} = link_with_a_subscriber(),
         Peer = station_of(B),
         Gossip = received(gossip(tampered(publication(?REALM)))),
         B ! {macula_peering, frame, self(), Gossip},
         {SubRef, Gossip, Meta} = overlay_frame_within(1_000),
         ?assertEqual({[{refused, Peer, signature_invalid}], []}, plumtree_outcome(Peer, Gossip)),
         ok = macula_station_link:overlay_frame_refused(B, Meta, signature_invalid),
         ?assertEqual({signature_invalid, true}, connection_report(1_000)),
         ?assertEqual(#{}, refused_relays(B)),
         stop_links([B])
     end}}.

%% A report for a relayed frame whose Meta lost its via names the origin, not the connection's peer, as sender, so it
%% is counted on the link and the station connection hears nothing of it.
a_report_for_a_relayed_frame_whose_meta_lost_its_via_charges_no_one_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {B, SubRef} = link_with_a_subscriber(),
         forward_to(B, ?ORIGIN, #{payload => macula_frame:encode(gossip(tampered(publication(?REALM))))}),
         {SubRef, _Arrived, Meta} = overlay_frame_within(1_000),
         ok = macula_station_link:overlay_frame_refused(B, maps:remove(via, Meta), signature_invalid),
         ?assertEqual(#{signature_invalid => 1}, refused_relays(B)),
         ?assertEqual(none, connection_report(0)),
         stop_links([B])
     end}}.

%% A report for a frame from the connected peer that arrives after the link took another peer is counted on the link,
%% and the connection hears nothing of it.
a_report_from_before_the_link_took_another_peer_charges_no_one_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {B, SubRef} = link_with_a_subscriber(),
         Gossip = received(gossip(tampered(publication(?REALM)))),
         B ! {macula_peering, frame, self(), Gossip},
         {SubRef, Gossip, Meta} = overlay_frame_within(1_000),
         _ = sys:replace_state(B, fun(S) -> setelement(?PEER_NODE_ID_INDEX, S, <<6:256>>) end),
         ok = macula_station_link:overlay_frame_refused(B, Meta, signature_invalid),
         ?assertEqual(#{signature_invalid => 1}, refused_relays(B)),
         ?assertEqual(none, connection_report(0)),
         stop_links([B])
     end}}.

%% A report whose Meta names no current peer charges no one and is counted on the link: one made while the link is
%% still connecting, with no peer node_id yet and a Meta whose sender is undefined, one whose Meta has no sender, and
%% one whose Meta is not a map.
a_report_whose_meta_names_no_current_peer_charges_no_one_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {B, _SubRef} = link_with_a_subscriber(),
         _ = sys:replace_state(B, fun(S) -> setelement(?PEER_NODE_ID_INDEX, S, undefined) end),
         _ = [ok = macula_station_link:overlay_frame_refused(B, Meta, signature_invalid)
              || Meta <- [#{sender => undefined}, #{}, not_a_map]],
         ?assertEqual(#{signature_invalid => 3}, refused_relays(B)),
         ?assertEqual(none, connection_report(0)),
         ?assert(is_process_alive(B)),
         stop_links([B])
     end}}.

%% A report of a kind charged_refusal/1 does not classify is counted as unknown_refusal and charges no one, even for a
%% frame from the connected peer, and whatever its Meta, and the link stays up.
a_report_of_a_kind_no_rule_classifies_is_counted_as_unknown_refusal_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {B, SubRef} = link_with_a_subscriber(),
         Gossip = received(gossip(publication(?REALM))),
         B ! {macula_peering, frame, self(), Gossip},
         {SubRef, Gossip, Meta} = overlay_frame_within(1_000),
         Reports = [{Meta, Kind} || Kind <- [not_a_kind, {expired, soon}, {signature_invalid, 1}, "text", #{kind => 1}]]
                   ++ [{maps:remove(sender, Meta), not_a_kind}, {not_a_map, not_a_kind}],
         _ = [ok = macula_station_link:overlay_frame_refused(B, ReportMeta, Kind) || {ReportMeta, Kind} <- Reports],
         ?assertEqual(#{unknown_refusal => 7}, refused_relays(B)),
         ?assertEqual(none, connection_report(0)),
         ?assert(is_process_alive(B)),
         stop_links([B])
     end}}.

%% A GOSSIP whose publication names no realm, names a 31-byte realm, or has a tbs that is not CBOR reaches no
%% subscriber, and the link stays up. Relayed, each is counted on the link as no_realm. From the connected peer, each is
%% reported to the connection as malformed_frame, charged.
a_gossip_whose_publication_names_no_realm_is_counted_and_not_delivered_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {B, _SubRef} = link_with_a_subscriber(),
         Gossips = [gossip(signed(maps:remove({text, <<"realm">>}, tbs_fields(?REALM)))),
                    gossip(signed(tbs_fields(<<9:248>>))),
                    gossip(with_tbs(publication(?REALM), <<16#FF>>))],
         _ = [forward_to(B, ?ORIGIN, #{payload => macula_frame:encode(Gossip)}) || Gossip <- Gossips],
         ?assertEqual(none, overlay_frame_within(300)),
         ?assertEqual(#{no_realm => 3}, refused_relays(B)),
         _ = [B ! {macula_peering, frame, self(), received(Gossip)} || Gossip <- Gossips],
         ?assertEqual(none, overlay_frame_within(300)),
         ?assertEqual(lists:duplicate(3, {malformed_frame, true}), [connection_report(1_000) || _ <- Gossips]),
         ?assert(is_process_alive(B)),
         stop_links([B])
     end}}.

%% A GOSSIP for a realm with no subscriber on the link is counted and not delivered: relayed, on the link as
%% no_subscriber; from the connected peer, reported to the connection as no_subscriber, not charged.
a_gossip_for_a_realm_with_no_subscriber_is_counted_and_not_delivered_test_() ->
    {spawn, {timeout, 10,
     fun() ->
         {B, _SubRef} = link_with_a_subscriber(),
         Gossip = gossip(publication(?OTHER_REALM)),
         forward_to(B, ?ORIGIN, #{payload => macula_frame:encode(Gossip)}),
         ?assertEqual(none, overlay_frame_within(300)),
         ?assertEqual(#{no_subscriber => 1}, refused_relays(B)),
         B ! {macula_peering, frame, self(), received(Gossip)},
         ?assertEqual(none, overlay_frame_within(300)),
         ?assertEqual({no_subscriber, false}, connection_report(1_000)),
         stop_links([B])
     end}}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

two_links_with_a_subscriber_on_b() ->
    A = start_link_to_station(),
    {B, SubRef} = link_with_a_subscriber(),
    {A, B, SubRef}.

link_with_a_subscriber() ->
    B = start_link_to_station(),
    {ok, SubRef} = macula_station_link:overlay_subscribe(B, ?REALM, self()),
    {B, SubRef}.

%% The relayed frames and relayed-frame refusals a link counted, by kind.
refused_relays(Pid) ->
    macula_refusal_report:counts(element(macula_station_link:state_field_index(refused_relays), sys:get_state(Pid))).

%% A link's identity key, statement issuer, request admission and share.
with_link_keys(Opts) ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                            reply_bytes => 262144, reply_bytes_total => 16777216}),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
          share => {seed, {<<"127.0.0.1">>, 1}}, expected_node_id => <<1:256>>}.

%% A link that believes it is connected to a station, with this process as its peer.
start_link_to_station() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, StationKey} = macula_node_keys:generate(identity, profile()),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                                 connect_timeout_ms => 2000})),
    Peer = self(),
    Station = macula_node_keys:key_id(StationKey),
    _ = sys:replace_state(Pid, fun(S) -> setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Peer),
                                                    Station) end),
    Pid.

node_id(Pid) ->
    macula_node_keys:key_id(element(?NODE_IDENTITY_INDEX, sys:get_state(Pid))).

%% The station a link believes it is connected to: the node_id it holds as its peer's.
station_of(Pid) ->
    element(?PEER_NODE_ID_INDEX, sys:get_state(Pid)).

%% The overlay_relay envelope a link sent to `Target', as its station receives it.
sent_envelope(Target) ->
    receive
        {'$gen_cast', {send_frame, _, #{frame_type := overlay_relay} = Frame}} ->
            #{peer := Target} = Envelope = received(Frame),
            Envelope
    after 1_000 ->
        erlang:error(no_envelope_sent)
    end.

%% Forwards a payload to link `To' as a station does: in an envelope addressed to the origin it authenticated.
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

%% A refusal a link reported to its connection, this process, as its kind and whether it is charged.
connection_report(Ms) ->
    receive
        {'$gen_cast', {object_refused, Kind, Charged}} -> {Kind, Charged}
    after Ms ->
        none
    end.

%% What Plumtree makes of a GOSSIP from `From', its only peer: the actions and the deliveries.
plumtree_outcome(From, Frame) ->
    {ok, S0} = hecate_plumtree:new(<<99:256>>, ?REALM),
    Clocks = #{wall => erlang:system_time(millisecond), monotonic => 0},
    {_S, Actions, Deliveries} = hecate_plumtree:process(hecate_plumtree:add_peer(S0, From), From, Frame, Clocks),
    {Actions, Deliveries}.

stop_links(Pids) ->
    [macula_station_link:stop(Pid) || Pid <- Pids],
    ok.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

%% The publication a PUBLISH for `Realm' carries, signed by a fresh publisher in the configured profile.
publication(Realm) ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    #{publication := Publication} =
        macula_frame:publish(#{realm => Realm, topic => <<"news">>, seq => 1, payload => <<"p">>,
                               published_at => erlang:system_time(millisecond)}, Key),
    Publication.

%% The fields a publication's tbs holds, in their wire form, for a realm of any length.
tbs_fields(Realm) ->
    #{{text, <<"publisher">>} => <<1:256>>, {text, <<"realm">>} => Realm, {text, <<"topic">>} => {text, <<"news">>},
      {text, <<"seq">>} => 1, {text, <<"published_at">>} => erlang:system_time(millisecond),
      {text, <<"payload">>} => <<"p">>}.

signed(Fields) ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    macula_signed_object:sign(?PUBLICATION_LABEL, Fields, Key).

with_tbs(Publication, Tbs) ->
    Publication#{tbs := Tbs}.

tampered(#{signature := <<Byte, Rest/binary>>} = Publication) ->
    Publication#{signature := <<(Byte bxor 1), Rest/binary>>}.

gossip(Publication) ->
    macula_frame:plumtree_gossip(#{publication => Publication, round => 1}).
