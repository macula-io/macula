%% EUnit tests for `macula_station_link'.
%%
%% A live QUIC handshake against a real V2 station is exercised in
%% the hecate-station Common Test suites. These tests focus on the
%% bookkeeping the client owns end-to-end: seed parsing, subscriptions,
%% overlay frames, inbound calls and content streams. Outbound calls are in
%% macula_station_link_call_tests, liveness in
%% macula_station_link_liveness_tests, advertising in
%% macula_station_link_advertise_tests, and stream sessions in
%% macula_station_link_stream_open_tests,
%% macula_station_link_stream_serving_tests and
%% macula_station_link_stream_session_tests. Peering is exercised by injecting
%% synthetic frames as if `macula_peering' had delivered them.
-module(macula_station_link_tests).

-include_lib("eunit/include/eunit.hrl").

%% Realm-per-call: tests use the all-zeros realm tag (DHT-internal /
%% realm-agnostic) for every call/subscribe/publish.
-define(REALM, <<0:256>>).

%% Data a reason carries that must stay on this node.
-define(MARKER, <<"marker-3f9c-stays-on-this-node">>).
%% Well above what the log gets of a reason, far below a whole large one.
-define(LOGGED_BYTES, 8192).

%% The link's state fields these tests read or set, looked up by name in the
%% state record, so a field added to the record cannot shift them.
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(NODE_IDENTITY_INDEX, macula_station_link:state_field_index(node_identity)).

%%------------------------------------------------------------------
%% Seed parsing
%%------------------------------------------------------------------

seed_url_https_with_port_test() ->
    %% Drive parse_seed/1 through a start_link path that fails fast
    %% on the connect attempt (port 1 → connect refused). We only
    %% care that the URL parsed without crashing.
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => <<"https://localhost:4433">>,
        connect_timeout_ms => 2000
    })),
    ?assert(is_process_alive(Pid)),
    macula_station_link:stop(Pid),
    ok.

seed_map_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => #{host => <<"127.0.0.1">>, port => 65000},
        connect_timeout_ms => 2000
    })),
    ?assert(is_process_alive(Pid)),
    macula_station_link:stop(Pid),
    ok.

%%------------------------------------------------------------------
%% A CALL that never went out is reported as not sent
%%------------------------------------------------------------------

%% Only an error from before the CALL went out lets a pool try another link.
not_sent_is_true_only_for_errors_before_the_call_went_out_test() ->
    ?assert(macula_station_link:not_sent({error, not_connected})),
    ?assert(macula_station_link:not_sent({error, noproc})),
    ?assert(macula_station_link:not_sent({error, {refused, {unsupported_payload_type, pid, []}}})),
    ?assertNot(macula_station_link:not_sent({error, timeout})),
    ?assertNot(macula_station_link:not_sent({error, {disconnected, closed}})),
    ?assertNot(macula_station_link:not_sent({error, gone})),
    ?assertNot(macula_station_link:not_sent({error, {call_error, unknown_next_peer, undefined}})),
    ?assertNot(macula_station_link:not_sent({error, {call_error, <<"overloaded">>, undefined}})),
    ?assertNot(macula_station_link:not_sent({error, <<"not_connected">>})).

%% Start options with the keys a link starts with: a node identity key in
%% the node's profile, an issuer of its own for that key, owned by the
%% calling process, and the node_id its seed expects.
with_link_keys(Opts) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    %% A link also starts with a request admission and its share in it.
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
          share => {seed, {<<"127.0.0.1">>, 1}}, expected_node_id => <<1:256>>}.

%%------------------------------------------------------------------
%% subscribe/4 sends a SUBSCRIBE frame
%%------------------------------------------------------------------

subscribe_sends_frame_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         %% Force-inject both peer_pid (7) and peer_node_id (8) so the
         %% link skips the racy `{macula_peering, connected, _, _}'
         %% info-message dance — that handler is gated on
         %% `peer_pid = Pid', which is not yet set when the message
         %% arrives in this test setup, so it would silently fall
         %% through `handle_info(_Other, S)' and never set
         %% `peer_node_id'. Since 3.12.1 `{call, ...}' is gated on
         %% `peer_node_id' (matching `{publish, ...}'), the test now
         %% must set both fields atomically.
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         {ok, SubRef} = macula_station_link:subscribe(
                          Pid, ?REALM, <<"_mesh.station.announced_v1">>, self()),
         ?assert(is_reference(SubRef)),
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe,
                                          topic := <<"_mesh.station.announced_v1">>,
                                          realm := R}}} ->
                 ?assertEqual(?REALM, R)
         after 1_000 ->
             erlang:error(no_subscribe_frame)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% Inbound EVENT frame fans out to subscriber
%%------------------------------------------------------------------

event_frame_delivered_to_subscriber_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         %% Force-inject both peer_pid (7) and peer_node_id (8) so the
         %% link skips the racy `{macula_peering, connected, _, _}'
         %% info-message dance — that handler is gated on
         %% `peer_pid = Pid', which is not yet set when the message
         %% arrives in this test setup, so it would silently fall
         %% through `handle_info(_Other, S)' and never set
         %% `peer_node_id'. Since 3.12.1 `{call, ...}' is gated on
         %% `peer_node_id' (matching `{publish, ...}'), the test now
         %% must set both fields atomically.
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Topic = <<"_mesh.station.announced_v1">>,
         {ok, SubRef} = macula_station_link:subscribe(
                          Pid, ?REALM, Topic, self()),
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame)
         end,
         {Event, PublisherKey} = signed_event(?REALM, Topic, 42,
                                              #{hello => <<"world">>}),
         Pid ! {macula_peering, frame, FakePeer, Event},
         receive
             {macula_event, R, T, P, Meta} ->
                 ?assertEqual(SubRef, R),
                 ?assertEqual(Topic, T),
                 ?assertEqual(#{{text, <<"hello">>} => <<"world">>}, P),
                 ?assertEqual(?REALM, maps:get(realm, Meta)),
                 ?assertEqual(PublisherKey, maps:get(publisher, Meta)),
                 ?assertEqual(42, maps:get(seq, Meta))
         after 2_000 -> erlang:error(no_event_delivered)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%% A signed EVENT for (Realm, Topic) with Payload, plus the key id its
%% publication names as the publisher.
signed_event(Realm, Topic, Seq, Payload) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    #{publication := Publication} = macula_frame:publish(
        #{realm => Realm, topic => Topic, seq => Seq,
          published_at => erlang:system_time(millisecond), payload => Payload},
        Key),
    {macula_frame:event(#{publication => Publication, delivered_via => direct}),
     macula_node_keys:key_id(Key)}.

%%------------------------------------------------------------------
%% Inbound EVENT with publisher_sig — verified; lenient vs strict
%% (pubsub Phase 2 step 4)
%%------------------------------------------------------------------

event_publisher_sig_verify_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         application:unset_env(macula, pubsub_strict_publisher_sig),
         {ok, Profile} = macula_crypto_profile:configured(),
         {ok, PubKey} = macula_node_keys:generate(identity, Profile),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         {ok, PeerNodeId} = macula_node_keys:node_id(PubKey),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Topic = <<"io.macula/x/y/v1">>,
         {ok, SubRef} = macula_station_link:subscribe(Pid, ?REALM, Topic, self()),
         receive {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame) end,

         MkEvent = fun(Seq, Payload) ->
             #{publication := Publication} = macula_frame:publish(
                 #{realm => ?REALM, topic => Topic, seq => Seq,
                   published_at => erlang:system_time(millisecond), payload => Payload},
                 PubKey),
             macula_frame:event(#{publication => Publication, delivered_via => direct})
         end,


         %% 0. An EVENT without a publication has no fields to deliver
         %% and is dropped, strict or lenient: there is no unsigned
         %% publication on the wire any more.
         Pid ! {macula_peering, frame, FakePeer, #{frame_type => event, delivered_via => direct}},
         receive {macula_event, SubRef, _, _, _} ->
             erlang:error(unsigned_event_was_delivered)
         after 800 -> ok end,

         %% 1. A publication that verifies → delivered, Meta says `true'.
         Pid ! {macula_peering, frame, FakePeer, MkEvent(1, ok1)},
         receive {macula_event, SubRef, Topic, {text, <<"ok1">>}, Meta1} ->
             ?assertEqual(true, maps:get(publisher_verified, Meta1))
         after 2_000 -> erlang:error(valid_sig_event_not_delivered) end,

         %% 2. A tampered publication with the setting left at its
         %% default → NOT delivered. Strict is the default: a signature
         %% that is present but does not check out is dropped.
         Bad = tampered_publication(MkEvent(2, ok2), tampered),
         Pid ! {macula_peering, frame, FakePeer, Bad},
         receive {macula_event, SubRef, Topic, {text, <<"tampered">>}, _} ->
             erlang:error(default_bad_sig_event_was_delivered)
         after 800 -> ok end,

         %% 3. Tampered with lenient mode explicitly opted into →
         %% delivered, but Meta says `false', never `true' -- a
         %% subscriber must still be able to tell "signed, but the
         %% signature didn't check out" apart from a trustworthy fact.
         application:set_env(macula, pubsub_strict_publisher_sig, false),
         Bad3 = tampered_publication(MkEvent(3, ok3), tampered3),
         Pid ! {macula_peering, frame, FakePeer, Bad3},
         receive {macula_event, SubRef, Topic, {text, <<"tampered3">>}, Meta3} ->
             ?assertEqual(false, maps:get(publisher_verified, Meta3))
         after 2_000 -> erlang:error(lenient_bad_sig_event_not_delivered) end,
         application:unset_env(macula, pubsub_strict_publisher_sig),

         macula_station_link:stop(Pid),
         ok
     end}.

%% Rebuild an EVENT's publication with the payload swapped in its tbs
%% but the ORIGINAL signature kept: the publication then fails to
%% verify, exactly like a wire tamper.
tampered_publication(#{publication := #{key := Key, tbs := Tbs, signature := Signature}} = Event,
                     Payload) ->
    Fields0 = macula_record_cbor:decode(Tbs),
    Fields = Fields0#{{text, <<"payload">>} => Payload},
    Event#{publication := #{key => Key,
                            tbs => macula_record_cbor:encode(Fields),
                            signature => Signature}}.

%%------------------------------------------------------------------
%% EVENT for a different realm is NOT delivered (realm-scoped index)
%%------------------------------------------------------------------

event_in_other_realm_not_delivered_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         %% Force-inject both peer_pid (7) and peer_node_id (8) so the
         %% link skips the racy `{macula_peering, connected, _, _}'
         %% info-message dance — that handler is gated on
         %% `peer_pid = Pid', which is not yet set when the message
         %% arrives in this test setup, so it would silently fall
         %% through `handle_info(_Other, S)' and never set
         %% `peer_node_id'. Since 3.12.1 `{call, ...}' is gated on
         %% `peer_node_id' (matching `{publish, ...}'), the test now
         %% must set both fields atomically.
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Topic = <<"weather.measured_v1">>,
         RealmA = <<1:256>>,
         RealmB = <<2:256>>,
         {ok, _SubRef} = macula_station_link:subscribe(
                           Pid, RealmA, Topic, self()),
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame)
         end,
         {ok, Profile} = macula_crypto_profile:configured(),
         {ok, PublisherKey} = macula_node_keys:generate(identity, Profile),
         #{publication := Publication} = macula_frame:publish(
             #{realm => RealmB, topic => Topic, seq => 1,
               published_at => erlang:system_time(millisecond), payload => wrong_realm},
             PublisherKey),
         Pid ! {macula_peering, frame, FakePeer,
                macula_frame:event(#{publication => Publication, delivered_via => direct})},
         receive
             {macula_event, _, _, wrong_realm, _} ->
                 erlang:error(event_in_wrong_realm_delivered)
         after 200 -> ok
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% publish/4 sends a PUBLISH frame and increments seq
%%------------------------------------------------------------------

publish_sends_frame_and_increments_seq_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         {ok, Profile} = macula_crypto_profile:configured(),
         {ok, PeerKey} = macula_node_keys:generate(identity, Profile),
         {ok, PeerNodeId} = macula_node_keys:node_id(PeerKey),
         %% publish/4 requires full handshake (peer_node_id set). The
         %% connected info message races against attempt_connect setting
         %% peer_pid to a real worker; bypass by force-injecting both
         %% fields. peer_node_id is record element 8 (right after
         %% peer_pid at 7).
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Topic = <<"weather.measured_v1">>,
         ok = macula_station_link:publish(Pid, ?REALM, Topic,
                                           #{temp => 20}),
         Frame1 = receive
             {'$gen_cast', {send_frame, #{frame_type := publish} = F}} -> F
         after 1_000 -> erlang:error(no_publish_frame_1)
         end,
         Verified1 = verified_publish(Frame1),
         ?assertEqual(Topic, maps:get(topic, Verified1)),
         ?assertEqual(?REALM, maps:get(realm, Verified1)),
         ?assertEqual(0, maps:get(seq, Verified1)),
         ?assertEqual(#{{text, <<"temp">>} => 20}, maps:get(payload, Verified1)),
         ok = macula_station_link:publish(Pid, ?REALM, Topic,
                                           #{temp => 21}),
         Frame2 = receive
             {'$gen_cast', {send_frame, #{frame_type := publish} = F2}} -> F2
         after 1_000 -> erlang:error(no_publish_frame_2)
         end,
         ?assertEqual(1, maps:get(seq, verified_publish(Frame2))),
         macula_station_link:stop(Pid),
         ok
     end}.

%% A PUBLISH frame's fields are read from its verified publication —
%% the fields live inside the signed object, not on the frame.
verified_publish(Frame) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Verified} = macula_frame:verify_publication(Frame, Profile,
                                                     erlang:system_time(millisecond)),
    Verified.

%%------------------------------------------------------------------
%% publish/5 stamps the caller's seq verbatim and leaves the per-link
%% counter untouched — the pool owns the sequence so it survives a
%% link respawn (the reset-to-0 bug the pool-owned seq fixes).
%%------------------------------------------------------------------

publish5_uses_caller_seq_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         {ok, Profile} = macula_crypto_profile:configured(),
         {ok, PeerKey} = macula_node_keys:generate(identity, Profile),
         {ok, PeerNodeId} = macula_node_keys:node_id(PeerKey),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Topic = <<"weather.measured_v1">>,
         ok = macula_station_link:publish(Pid, ?REALM, Topic, #{n => 1}, 4242),
         F1 = receive
             {'$gen_cast', {send_frame, #{frame_type := publish} = A}} -> A
         after 1_000 -> erlang:error(no_publish_frame_1)
         end,
         ?assertEqual(4242, maps:get(seq, verified_publish(F1))),
         ok = macula_station_link:publish(Pid, ?REALM, Topic, #{n => 2}, 4243),
         F2 = receive
             {'$gen_cast', {send_frame, #{frame_type := publish} = B}} -> B
         after 1_000 -> erlang:error(no_publish_frame_2)
         end,
         ?assertEqual(4243, maps:get(seq, verified_publish(F2))),
         %% publish/5 must NOT advance the per-link counter: a later
         %% pool-less publish/4 still starts from 0.
         ok = macula_station_link:publish(Pid, ?REALM, Topic, #{n => 3}),
         F3 = receive
             {'$gen_cast', {send_frame, #{frame_type := publish} = C}} -> C
         after 1_000 -> erlang:error(no_publish_frame_3)
         end,
         ?assertEqual(0, maps:get(seq, verified_publish(F3))),
         macula_station_link:stop(Pid),
         ok
     end}.

publish5_not_connected_returns_error_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         R = macula_station_link:publish(Pid, ?REALM, <<"x">>, hello, 7),
         ?assertEqual({error, not_connected}, R),
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% publish/4 returns {error, not_connected} when peering is down
%%------------------------------------------------------------------

publish_not_connected_returns_error_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         %% Do NOT mark connected. Publish should error out.
         R = macula_station_link:publish(Pid, ?REALM,
                                          <<"x">>, hello),
         ?assertEqual({error, not_connected}, R),
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% unsubscribe/2 sends UNSUBSCRIBE frame and stops delivery
%%------------------------------------------------------------------

unsubscribe_sends_frame_and_clears_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         %% Force-inject both peer_pid (7) and peer_node_id (8) so the
         %% link skips the racy `{macula_peering, connected, _, _}'
         %% info-message dance — that handler is gated on
         %% `peer_pid = Pid', which is not yet set when the message
         %% arrives in this test setup, so it would silently fall
         %% through `handle_info(_Other, S)' and never set
         %% `peer_node_id'. Since 3.12.1 `{call, ...}' is gated on
         %% `peer_node_id' (matching `{publish, ...}'), the test now
         %% must set both fields atomically.
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Topic = <<"_mesh.station.announced_v1">>,
         {ok, SubRef} = macula_station_link:subscribe(
                          Pid, ?REALM, Topic, self()),
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame)
         end,
         ok = macula_station_link:unsubscribe(Pid, SubRef),
         receive
             {'$gen_cast', {send_frame, #{frame_type := unsubscribe,
                                          topic := T,
                                          realm := R}}} ->
                 ?assertEqual(Topic, T),
                 ?assertEqual(?REALM, R)
         after 1_000 ->
             erlang:error(no_unsubscribe_frame)
         end,
         {Event, _PublisherKey} = signed_event(?REALM, Topic, 1, post_unsubscribe),
         Pid ! {macula_peering, frame, FakePeer, Event},
         receive
             {macula_event, _, _, post_unsubscribe, _} ->
                 erlang:error(event_after_unsubscribe)
         after 200 -> ok
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% Subscriber pid death drops the subscription
%%------------------------------------------------------------------

subscriber_down_drops_subscription_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         %% Force-inject both peer_pid (7) and peer_node_id (8) so the
         %% link skips the racy `{macula_peering, connected, _, _}'
         %% info-message dance — that handler is gated on
         %% `peer_pid = Pid', which is not yet set when the message
         %% arrives in this test setup, so it would silently fall
         %% through `handle_info(_Other, S)' and never set
         %% `peer_node_id'. Since 3.12.1 `{call, ...}' is gated on
         %% `peer_node_id' (matching `{publish, ...}'), the test now
         %% must set both fields atomically.
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Test = self(),
         Sub = spawn(fun() ->
             {ok, R} = macula_station_link:subscribe(
                         Pid, ?REALM,
                         <<"_mesh.station.announced_v1">>, self()),
             Test ! {sub_started, self(), R}
         end),
         receive
             {sub_started, Sub, _SubRef} -> ok
         after 1_000 -> erlang:error(subscriber_did_not_subscribe)
         end,
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame)
         end,
         receive
             {'$gen_cast', {send_frame, #{frame_type := unsubscribe}}} -> ok
         after 1_000 -> erlang:error(no_cleanup_unsubscribe)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% Disconnect emits macula_event_gone to every subscriber
%%------------------------------------------------------------------

disconnect_notifies_subscribers_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         %% Earlier tests in this eunit process can leave their own
         %% macula_event_gone messages in the mailbox.
         flush_mailbox(),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         %% Force-inject both peer_pid (7) and peer_node_id (8) so the
         %% link skips the racy `{macula_peering, connected, _, _}'
         %% info-message dance — that handler is gated on
         %% `peer_pid = Pid', which is not yet set when the message
         %% arrives in this test setup, so it would silently fall
         %% through `handle_info(_Other, S)' and never set
         %% `peer_node_id'. Since 3.12.1 `{call, ...}' is gated on
         %% `peer_node_id' (matching `{publish, ...}'), the test now
         %% must set both fields atomically.
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         {ok, SubRef} = macula_station_link:subscribe(
                          Pid, ?REALM,
                          <<"_mesh.station.announced_v1">>, self()),
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame)
         end,
         Pid ! {macula_peering, disconnected, FakePeer, peer_closed},
         receive
             {macula_event_gone, SubRef, Reason} ->
                 ?assertEqual({disconnected, <<"peer_closed">>}, Reason)
         after 2_000 -> erlang:error(no_event_gone)
         end,
         ok
     end}.

%%------------------------------------------------------------------
%% Overlay-frame transport (overlay_subscribe/3, overlay_unsubscribe/2,
%% send_overlay_frame/2) — HyParView/Plumtree/future frame types the
%% built-in call/event handling doesn't recognise.
%%------------------------------------------------------------------

overlay_subscribe_delivers_matching_realm_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         {ok, SubRef} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
         Joiner = macula_identity:public(macula_identity:generate()),
         Frame = macula_frame:hyparview_join(#{realm => ?REALM, new_member => Joiner}),
         Pid ! {macula_peering, frame, FakePeer, Frame},
         receive
             {macula_overlay_frame, R, F, Meta} ->
                 ?assertEqual(SubRef, R),
                 ?assertEqual(Frame, F),
                 ?assertEqual(PeerNodeId, maps:get(sender, Meta))
         after 2_000 -> erlang:error(no_overlay_frame_delivered)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

overlay_frame_in_other_realm_not_delivered_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         {ok, _SubRef} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
         OtherRealm = crypto:strong_rand_bytes(32),
         Joiner = macula_identity:public(macula_identity:generate()),
         Frame = macula_frame:hyparview_join(#{realm => OtherRealm, new_member => Joiner}),
         Pid ! {macula_peering, frame, FakePeer, Frame},
         receive
             {macula_overlay_frame, _, _, _} -> erlang:error(unexpected_delivery)
         after 500 -> ok
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

send_overlay_frame_sends_on_wire_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Joiner = macula_identity:public(macula_identity:generate()),
         Frame = macula_frame:hyparview_join(#{realm => ?REALM, new_member => Joiner}),
         ?assertEqual(ok, macula_station_link:send_overlay_frame(Pid, Frame)),
         %% Other background traffic (e.g. a DHT call) may cast a
         %% send_frame to FakePeer first -- skip anything that isn't
         %% our own hyparview_join.
         ?assert(receive_frame_cast(Frame, 10)),
         macula_station_link:stop(Pid),
         ok
     end}.

receive_frame_cast(_Frame, 0) -> false;
receive_frame_cast(Frame, N) ->
    receive
        {'$gen_cast', {send_frame, F}} when F =:= Frame -> true;
        {'$gen_cast', {send_frame, _Other}} -> receive_frame_cast(Frame, N - 1)
    after 1_000 -> false
    end.

%%------------------------------------------------------------------
%% send_overlay_frame/3 + inbound overlay_relay envelope — Phase 3.5.
%%------------------------------------------------------------------

send_overlay_frame_3_wraps_target_in_relay_envelope_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Target = macula_identity:public(macula_identity:generate()),
         Frame = macula_frame:hyparview_disconnect(#{realm => ?REALM}),
         ?assertEqual(ok, macula_station_link:send_overlay_frame(Pid, Target, Frame)),
         Envelope = receive_relay_envelope(Target, 10),
         ?assert(Envelope =/= false),
         %% The connection signs the envelope for its neighbour as it
         %% sends it; the link hands it over without a signature of its own.
         ?assertNot(maps:is_key(signature, Envelope)),
         {ok, InnerDecoded, <<>>} =
             macula_frame:decode(maps:get(payload, Envelope)),
         ?assertEqual(Frame, InnerDecoded),
         macula_station_link:stop(Pid),
         ok
     end}.

receive_relay_envelope(_Target, 0) -> false;
receive_relay_envelope(Target, N) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := overlay_relay,
                                     peer := P} = F}} when P =:= Target ->
            F;
        {'$gen_cast', {send_frame, _Other}} -> receive_relay_envelope(Target, N - 1)
    after 1_000 -> false
    end.

%% The regression test for the sender-attribution bug found while
%% designing Phase 3.5: a relayed frame's `Meta.sender' must be the
%% ORIGINAL sender named in the `overlay_relay' envelope's `peer' field
%% (a genuine third-party HyParView peer), never this connection's own
%% `peer_node_id' (the station we're directly connected to) — those two
%% identities are deliberately different in this test to prove the fix
%% doesn't just happen to work when they coincide.
overlay_relay_delivers_with_envelope_origin_as_sender_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         StationNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, StationNodeId)
         end),
         {ok, SubRef} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
         OriginKp = macula_identity:generate(),
         Origin = macula_identity:public(OriginKp),
         ?assertNotEqual(Origin, StationNodeId),
         Joiner = macula_identity:public(macula_identity:generate()),
         Inner = macula_frame:sign(
                   macula_frame:hyparview_join(#{realm => ?REALM, new_member => Joiner}),
                   OriginKp),
         Envelope = macula_frame:overlay_relay(#{
             peer    => Origin,
             payload => macula_frame:encode(Inner)
         }),
         Pid ! {macula_peering, frame, FakePeer, Envelope},
         receive
             {macula_overlay_frame, R, F, Meta} ->
                 ?assertEqual(SubRef, R),
                 ?assertEqual(Inner, F),
                 ?assertEqual(Origin, maps:get(sender, Meta)),
                 ?assertNotEqual(StationNodeId, maps:get(sender, Meta))
         after 2_000 -> erlang:error(no_overlay_frame_delivered)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%% The inner frame of an `overlay_relay' envelope is delivered only when
%% its own signature verifies against the origin the envelope names. One
%% signed by any other key, or not signed at all, is dropped; a genuinely
%% signed one that follows is delivered.
overlay_relay_inner_frame_must_verify_against_origin_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {Pid, FakePeer, _StationNodeId} = start_connected_link(),
         {ok, SubRef} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
         OriginKp = macula_identity:generate(),
         Relay = fun(Inner) ->
                     Pid ! {macula_peering, frame, FakePeer,
                            macula_frame:overlay_relay(#{
                                peer    => macula_identity:public(OriginKp),
                                payload => macula_frame:encode(Inner)})}
                 end,
         Join = fun() ->
                    macula_frame:hyparview_join(#{
                        realm      => ?REALM,
                        new_member => macula_identity:public(macula_identity:generate())})
                end,
         Relay(macula_frame:sign(Join(), macula_identity:generate())),
         Relay(Join()),
         Genuine = macula_frame:sign(Join(), OriginKp),
         Relay(Genuine),
         receive
             {macula_overlay_frame, SubRef, First, _Meta} ->
                 ?assertEqual(Genuine, First)
         after 2_000 ->
             erlang:error(no_overlay_frame_delivered)
         end,
         receive
             {macula_overlay_frame, SubRef, Extra, _} ->
                 erlang:error({unverified_overlay_frame_delivered, Extra})
         after 300 ->
             ok
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

send_overlay_frame_not_connected_returns_error_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         %% Force peer_pid = undefined explicitly rather than relying
         %% on the real (127.0.0.1:1, nothing listening) dial staying
         %% pending for the length of this test — under a full-suite
         %% run sharing the same OTP node with hundreds of other
         %% QUIC-touching tests, that assumption is not always
         %% reliable even though the target never legitimately
         %% accepts a connection.
         _ = sys:replace_state(Pid, fun(S) ->
             setelement(?PEER_PID_INDEX, S, undefined)
         end),
         Joiner = macula_identity:public(macula_identity:generate()),
         Frame = macula_frame:hyparview_join(#{realm => ?REALM, new_member => Joiner}),
         ?assertEqual({error, not_connected},
                      macula_station_link:send_overlay_frame(Pid, Frame)),
         macula_station_link:stop(Pid),
         ok
     end}.

overlay_subscriber_down_drops_subscription_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Test = self(),
         Sub = spawn(fun() ->
             {ok, _R} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
             Test ! sub_started,
             receive stop -> ok end
         end),
         receive sub_started -> ok
         after 1_000 -> erlang:error(subscriber_did_not_subscribe)
         end,
         Mon = monitor(process, Sub),
         Sub ! stop,
         receive {'DOWN', Mon, process, Sub, _} -> ok
         after 1_000 -> erlang:error(subscriber_did_not_die)
         end,
         %% Give the link's own monitor a moment to process the DOWN
         %% before asserting the subscription is gone.
         timer:sleep(100),
         Joiner = macula_identity:public(macula_identity:generate()),
         Frame = macula_frame:hyparview_join(#{realm => ?REALM, new_member => Joiner}),
         Pid ! {macula_peering, frame, FakePeer, Frame},
         receive
             {macula_overlay_frame, _, _, _} -> erlang:error(delivered_after_subscriber_died)
         after 500 -> ok
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

disconnect_notifies_overlay_subscribers_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         {ok, SubRef} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
         Pid ! {macula_peering, disconnected, FakePeer, peer_closed},
         receive
             {macula_overlay_gone, R, Reason} ->
                 ?assertEqual(SubRef, R),
                 ?assertEqual({disconnected, <<"peer_closed">>}, Reason)
         after 2_000 -> erlang:error(no_overlay_gone)
         end,
         ok
     end}.

%%------------------------------------------------------------------
%% subscribe/4 before connect — frame drains on connected event
%%------------------------------------------------------------------

subscribe_before_connect_drains_on_connected_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         flush_send_frame_casts(),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         {ok, SubRef} = macula_station_link:subscribe(
                          Pid, ?REALM,
                          <<"_mesh.station.announced_v1">>, self()),
         ?assert(is_reference(SubRef)),
         receive
             {'$gen_cast', {send_frame, _}} ->
                 erlang:error(premature_send_frame)
         after 200 -> ok
         end,
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             setelement(?PEER_PID_INDEX, S, FakePeer)
         end),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe,
                                          topic := T}}} ->
                 ?assertEqual(<<"_mesh.station.announced_v1">>, T)
         after 1_000 ->
             erlang:error(subscribe_frame_not_drained)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% subscribe/4 mid-handshake (peer_pid set, peer_node_id not yet) --
%% regression for the bug fixed alongside this test: maybe_send_subscribe/3
%% used to gate on peer_pid alone instead of peer_node_id, so a
%% SUBSCRIBE frame sent in this exact window landed on the wire while the
%% peering statem was still in `handshaking' -- which has no clause for
%% `cast({send_frame, _})' and silently drops it via `drop_unexpected'.
%% Reproduced live: hecate-stations' own logs showed this exact frame
%% (topic _dht.records.N.stored) dropped on every reconnect.
%%------------------------------------------------------------------

subscribe_during_handshake_not_sent_early_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         flush_send_frame_casts(),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         %% peer_pid set (mirrors after_connect_request/2 right after
         %% macula_peering:connect/1 returns), peer_node_id deliberately
         %% left undefined (mirrors the statem still being in
         %% `handshaking' -- it is only set on the real `connected' event).
         _ = sys:replace_state(Pid, fun(S) ->
             setelement(?PEER_PID_INDEX, S, FakePeer)
         end),
         {ok, SubRef} = macula_station_link:subscribe(
                          Pid, ?REALM,
                          <<"_dht.records.1.stored">>, self()),
         ?assert(is_reference(SubRef)),
         receive
             {'$gen_cast', {send_frame, _}} ->
                 erlang:error(premature_send_frame)
         after 200 -> ok
         end,
         %% Handshake now genuinely completes -- drain must still deliver
         %% the frame that was correctly withheld above.
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe,
                                          topic := T}}} ->
                 ?assertEqual(<<"_dht.records.1.stored">>, T)
         after 1_000 ->
             erlang:error(subscribe_frame_not_drained)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%==================================================================
%% advertise/4 + inbound CALL dispatch
%%==================================================================

inbound_call_dispatches_to_handler_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Procedure = <<"_realm.membership.join_with_token_v1">>,
         %% Handler asserts on input + returns canonical reply.
         Handler = fun(#{token := <<"abc">>}) -> {ok, #{member_id => 42}} end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         flush_send_frame_casts(),
         CallId = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16>>,
         CallerKp = macula_identity:generate(),
         CallerPub = macula_identity:public(CallerKp),
         Pid ! {macula_peering, frame, FakePeer, macula_frame:sign(#{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => Procedure,
             payload     => #{token => <<"abc">>},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }, CallerKp)},
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := result,
                              call_id    := Id,
                              payload    := Payload}}} ->
                 ?assertEqual(CallId, Id),
                 ?assertEqual(#{member_id => 42}, Payload)
         after 1_000 ->
             erlang:error(no_result_frame_sent)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

inbound_call_threads_caller_into_payload_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Procedure = <<"_realm.membership.join_with_token_v1">>,
         Self = self(),
         %% The handler asserts what it actually received -- this is the
         %% behaviour under test, not the frame's own `caller' field.
         Handler = fun(Payload) ->
             Self ! {handler_saw, Payload},
             {ok, #{member_id => 42}}
         end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         flush_send_frame_casts(),
         CallId = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16>>,
         CallerKp = macula_identity:generate(),
         CallerPub = macula_identity:public(CallerKp),
         SpoofedCaller = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, macula_frame:sign(#{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => Procedure,
             %% Payload itself claims a DIFFERENT caller under the same
             %% key -- proving the wire-authenticated value wins, not
             %% whatever the payload body happens to say.
             payload     => #{token => <<"abc">>, caller => SpoofedCaller},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }, CallerKp)},
         receive
             {handler_saw, Payload} ->
                 ?assertEqual(CallerPub, maps:get(caller, Payload)),
                 ?assertNotEqual(SpoofedCaller, maps:get(caller, Payload)),
                 ?assertEqual(<<"abc">>, maps:get(token, Payload))
         after 1_000 ->
             erlang:error(handler_never_invoked)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

inbound_call_unknown_procedure_returns_error_frame_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         CallId = <<2:128>>,
         CallerKp = macula_identity:generate(),
         CallerPub = macula_identity:public(CallerKp),
         Pid ! {macula_peering, frame, FakePeer, macula_frame:sign(#{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => <<"_no.such.procedure">>,
             payload     => #{},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }, CallerKp)},
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := error,
                              call_id    := Id,
                              code       := Code}}} ->
                 ?assertEqual(CallId, Id),
                 ?assertEqual(16#01, Code)  %% unknown_next_peer
         after 1_000 ->
             erlang:error(no_error_frame_sent)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

inbound_call_handler_crash_returns_error_frame_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Procedure = <<"_test.crash">>,
         Handler = fun(_Args) -> error(deliberate) end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         flush_send_frame_casts(),
         CallId = <<3:128>>,
         CallerKp = macula_identity:generate(),
         CallerPub = macula_identity:public(CallerKp),
         Pid ! {macula_peering, frame, FakePeer, macula_frame:sign(#{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => Procedure,
             payload     => #{},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }, CallerKp)},
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := error,
                              call_id    := Id,
                              code       := Code}}} ->
                 ?assertEqual(CallId, Id),
                 ?assertEqual(16#02, Code)  %% temporary_relay_failure
         after 1_000 ->
             erlang:error(no_error_frame_sent)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

inbound_call_handler_error_tuple_emits_call_error_test_() ->
    %% Handler returning `{error, Reason}' MUST emit a BOLT#4
    %% `call_error' frame, NOT a `result' frame. RESULT payloads go
    %% through `macula_record_cbor:encode/1', which has no clause
    %% for raw tuples — sending `{error, _}' inside a RESULT crashes
    %% the peering gen_statem at frame-sign time and drops every
    %% other multiplexed RPC on the connection. Pre-4.1.1 this bit
    %% production every time `_dht.put_record' got a bad-signature
    %% record from the replication path.
    %%
    %% The error is funneled into `code = 0x0F unknown_error' with
    %% the reason's name in `detail'. Handlers that need a
    %% specific BOLT#4 code can crash with a tagged error or use
    %% the dedicated frame builders.
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Procedure = <<"_test.app_error">>,
         Handler = fun(_Args) -> {error, invalid_token} end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         flush_send_frame_casts(),
         CallId = <<4:128>>,
         CallerKp = macula_identity:generate(),
         CallerPub = macula_identity:public(CallerKp),
         Pid ! {macula_peering, frame, FakePeer, macula_frame:sign(#{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => Procedure,
             payload     => #{},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }, CallerKp)},
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := error,
                              call_id    := FrameCallId,
                              code       := Code,
                              detail     := Detail}}} ->
                 ?assertEqual(CallId, FrameCallId),
                 ?assertEqual(16#0F, Code),
                 ?assert(is_binary(Detail)),
                 ?assertNotEqual(nomatch, binary:match(Detail, <<"invalid_token">>))
         after 1_000 ->
             erlang:error(no_call_error_frame_sent)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% A handler's refusal reaches the caller who provoked it
%%------------------------------------------------------------------

%% The other half of the round trip. A binary reason must reach the wire
%% as itself: `~0p' would put <<"<<\"hold_full\">>">> in the frame, and
%% no caller can compare against a rendering of a binary.
binary_reason_crosses_the_wire_verbatim_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000
         })),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Procedure = <<"_test.refusal">>,
         Handler = fun(_Args) -> {error, <<"hold_full">>} end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         flush_send_frame_casts(),
         CallerKp = macula_identity:generate(),
         CallId = <<7:128>>,
         Pid ! {macula_peering, frame, FakePeer, macula_frame:sign(#{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => Procedure,
             payload     => #{},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => macula_identity:public(CallerKp)
         }, CallerKp)},
         receive
             {'$gen_cast', {send_frame, #{frame_type := error,
                                          detail     := Detail}}} ->
                 ?assertEqual(<<"hold_full">>, Detail)
         after 1_000 ->
             erlang:error(no_call_error_frame_sent)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% What a caller is told of a handler's error, and what stays here
%%------------------------------------------------------------------

%% A reason with data crosses as its name, and none of its terms leave
%% the node.
a_handler_error_with_data_is_told_by_its_name_only_test_() ->
    {timeout, 5,
     fun() ->
         Frame = error_frame_for_handler(fun(_Args) -> {error, {refused, ?MARKER}} end),
         ?assertMatch(#{code := 16#0F, detail := <<"refused">>}, Frame),
         ?assertEqual(nomatch, binary:match(term_to_binary(Frame), ?MARKER))
     end}.

%% A handler's own text crosses as at most 256 bytes of valid UTF-8.
a_handler_error_text_too_long_is_cut_on_a_character_boundary_test_() ->
    {timeout, 5,
     fun() ->
         Euro = <<16#20AC/utf8>>,
         Frame = error_frame_for_handler(fun(_Args) -> {error, binary:copy(Euro, 100)} end),
         #{detail := Detail} = Frame,
         ?assertEqual(<<(binary:copy(Euro, 84))/binary, "...">>, Detail),
         ?assert(is_binary(unicode:characters_to_binary(Detail)))
     end}.

%% A printable charlist is the handler's own text, as a binary is.
a_handler_error_charlist_crosses_as_text_test_() ->
    {timeout, 5,
     fun() ->
         Frame = error_frame_for_handler(fun(_Args) -> {error, "not found"} end),
         ?assertMatch(#{detail := <<"not found">>}, Frame)
     end}.

%% A reason that is neither text nor named sends no detail.
a_handler_error_list_of_pids_sends_no_detail_test_() ->
    {timeout, 5,
     fun() ->
         Frame = error_frame_for_handler(fun(_Args) -> {error, [self(), self()]} end),
         ?assertMatch(#{code := 16#0F, detail := undefined}, Frame)
     end}.

%% A crashing handler's caller gets no detail, and the node's log gets
%% the crash printed within bounds.
a_handler_crash_is_logged_within_bounds_test_() ->
    {timeout, 10,
     fun() ->
         Log = macula_test_log:capture(),
         try
             Frame = error_frame_for_handler(
                       fun(_Args) -> error({boom, lists:duplicate(10_000, ?MARKER)}) end),
             ?assertMatch(#{code := 16#02}, Frame),
             ?assertEqual(nomatch, binary:match(term_to_binary(Frame), ?MARKER)),
             Logged = macula_test_log:wait_text(<<"handler crashed">>, 1_000),
             ?assert(byte_size(Logged) < ?LOGGED_BYTES),
             ?assertNotEqual(nomatch, binary:match(Logged, <<"boom">>))
         after
             macula_test_log:release(Log)
         end
     end}.

%% The ERROR frame the link sends a caller whose CALL `Handler' answers.
error_frame_for_handler(Handler) ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000
    })),
    FakePeer = self(),
    PeerNodeId = macula_identity:public(macula_identity:generate()),
    _ = sys:replace_state(Pid, fun(S) ->
        S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
        setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
    end),
    Procedure = <<"_test.handler_answer">>,
    ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
    flush_send_frame_casts(),
    CallerKp = macula_identity:generate(),
    CallId = <<9:128>>,
    Pid ! {macula_peering, frame, FakePeer, macula_frame:sign(#{
        frame_type  => call,
        call_id     => CallId,
        realm       => ?REALM,
        procedure   => Procedure,
        payload     => #{},
        deadline_ms => erlang:system_time(millisecond) + 5_000,
        caller      => macula_identity:public(CallerKp)
    }, CallerKp)},
    Frame = receive
                {'$gen_cast', {send_frame, #{frame_type := error,
                                             call_id    := CallId} = Sent}} -> Sent
            after 1_000 ->
                erlang:error(no_call_error_frame_sent)
            end,
    macula_station_link:stop(Pid),
    Frame.

%%------------------------------------------------------------------
%% A reply completes a call only when its signature verifies
%%------------------------------------------------------------------

flush_send_frame_casts() ->
    receive
        {'$gen_cast', {send_frame, _}} -> flush_send_frame_casts()
    after 0 -> ok
    end.

flush_mailbox() ->
    receive _ -> flush_mailbox()
    after 0 -> ok
    end.

%%==================================================================
%% Content streams
%%==================================================================

%% Boilerplate: start a link, force-inject peer_pid + peer_node_id,
%% and mock `macula_peering' so dedicated-stream opens/sends are
%% observable without a real QUIC connection (session frames no
%% longer travel over the fake-peer-as-cast-target wire the way
%% ADVERTISE / CALL / EVENT still do — see
%% PLAN_PER_STREAM_QUIC_ISOLATION.md). `open_dedicated_stream/1`
%% hands back a fresh `make_ref/0' standing in for a QUIC stream
%% reference and notifies the test process; `send_on_stream/3`
%% captures what was written to it as `{sent_on_stream, Stream, Frame}'
%% instead of performing a real NIF send, and `close_dedicated_stream/1'
%% reports the close as `{closed_dedicated_stream, Stream}'.
setup_link_for_streams() ->
    {ok, _} = application:ensure_all_started(macula),
    meck:new(macula_peering, [passthrough]),
    Test = self(),
    meck:expect(macula_peering, open_dedicated_stream, fun(_ConnPid) ->
        Stream = make_ref(),
        Test ! {opened_dedicated_stream, Stream},
        {ok, Stream}
    end),
    meck:expect(macula_peering, send_on_stream, fun(Stream, Frame, _Id) ->
        Test ! {sent_on_stream, Stream, Frame},
        ok
    end),
    meck:expect(macula_peering, close_dedicated_stream, fun(Stream) ->
        Test ! {closed_dedicated_stream, Stream},
        ok
    end),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000
    })),
    FakePeer = self(),
    PeerNodeId = macula_identity:public(macula_identity:generate()),
    _ = sys:replace_state(Pid, fun(S) ->
        S2 = setelement(?PEER_PID_INDEX,     S, FakePeer),
        setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
    end),
    {Pid, FakePeer, PeerNodeId}.

%% Ends what a streams test leaves running, so no session outlives the test
%% that opened it. It stops the link when the test did not, and ends the
%% client streams the test owns: a stream lives until its owner ends, and this
%% module's tests share one long-lived process. It asserts that every stream
%% process the link started has ended, the sessions the link served included,
%% and drops the messages those endings sent this process.
teardown_link_for_streams(Link) ->
    ok = stop_link(Link),
    meck:unload(macula_peering),
    Started = macula_test_sessions:started_by(Link),
    ok = end_owned(Started, erlang:process_info(self(), monitored_by)),
    ?assertEqual([], macula_test_sessions:await_ended(Started)),
    flush_mailbox().

stop_link(Link) ->
    try macula_station_link:stop(Link) catch exit:noproc -> ok end.

%% A client stream monitors the process that owns it.
end_owned(Streams, {monitored_by, Monitoring}) ->
    _ = [exit(Stream, kill) || Stream <- Streams, lists:member(Stream, Monitoring)],
    ok.

-define(CONTENT_STREAM_BUFS_INDEX, macula_station_link:state_field_index(content_stream_bufs)).

%% A reply on a content stream whose bytes do not decode ends that stream
%% and fails the call waiting on it; the link lives on.
a_content_stream_reply_that_does_not_decode_fails_its_call_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, _FakePeer, _PeerNodeId} = setup_link_for_streams(),
         try
             Test = self(),
             {ok, Stream} = macula_station_link:open_content_stream(Pid),
             spawn_link(fun() ->
                 Test ! {content_call,
                         macula_station_link:call_on_stream(Pid, Stream, ?REALM,
                                                            <<"_content.get_block">>, #{}, 2_000)}
             end),
             receive
                 {sent_on_stream, Stream, #{frame_type := call}} -> ok
             after 1_000 ->
                 erlang:error(no_content_call_sent)
             end,
             Pid ! {quic, <<4:32, "junk">>, Stream, undefined},
             receive
                 {content_call, Result} -> ?assertEqual({error, {malformed, bad_frame}}, Result)
             after 1_000 ->
                 erlang:error(content_call_not_failed)
             end,
             ?assertNot(is_map_key(Stream, element(?CONTENT_STREAM_BUFS_INDEX, sys:get_state(Pid)))),
             macula_station_link:stop(Pid)
         after
             teardown_link_for_streams(Pid)
         end
     end}.

%% A reply on a content stream that decodes but lacks a field its type
%% requires ends that stream and fails the call waiting on it, naming the
%% frame type; the link lives on and opens the next content stream.
a_content_stream_reply_missing_a_required_field_fails_its_call_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, _FakePeer, _PeerNodeId} = setup_link_for_streams(),
         try
             Test = self(),
             {ok, Stream} = macula_station_link:open_content_stream(Pid),
             spawn_link(fun() ->
                 Test ! {content_call,
                         macula_station_link:call_on_stream(Pid, Stream, ?REALM,
                                                            <<"_content.get_block">>, #{}, 2_000)}
             end),
             CallId = receive
                          {sent_on_stream, Stream, #{frame_type := call, call_id := Id}} -> Id
                      after 1_000 ->
                          erlang:error(no_content_call_sent)
                      end,
             Pid ! {quic, macula_frame:encode(#{frame_type => result, call_id => CallId}),
                    Stream, undefined},
             receive
                 {content_call, Result} ->
                     ?assertMatch({error, {malformed, {invalid_frame, result, _}}}, Result)
             after 1_000 ->
                 erlang:error(content_call_not_failed)
             end,
             ?assertNot(is_map_key(Stream, element(?CONTENT_STREAM_BUFS_INDEX, sys:get_state(Pid)))),
             ?assertMatch({ok, _}, macula_station_link:open_content_stream(Pid)),
             macula_station_link:stop(Pid)
         after
             teardown_link_for_streams(Pid)
         end
     end}.

start_connected_link() ->
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000
    })),
    FakePeer = self(),
    PeerNodeId = macula_identity:public(macula_identity:generate()),
    _ = sys:replace_state(Pid, fun(S) ->
        S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
        setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
    end),
    {Pid, FakePeer, PeerNodeId}.

%%------------------------------------------------------------------
%% Inbound CALL handlers run off the link process.
%%
%% Found live 2026-09-02 on hecate-rag: a handler that made its own
%% mesh call through the pool waited 30 s and crashed, because the
%% link that had delivered the inbound CALL was still blocked running
%% that very handler and so could never read the RESULT of the
%% handler's outbound call (nor answer the pool's advertise/publish
%% calls, which timed out at 5 s meanwhile). Both tests below fail
%% on the inline implementation and pass once the handler runs in
%% its own process.
%%------------------------------------------------------------------

%% Start a link with a fake peer patched in and register `Handlers'
%% (a list of {Procedure, Fun}) on it. Returns
%% the link and the fake peer's key pair, which the tests sign CALLs with.
inbound_call_fixture(Handlers) ->
    inbound_call_fixture(Handlers, open).

%% Same, but with an explicit auth policy on every advertised procedure
%% instead of the `open' default -- for exercising `authorize_policy/2'.
inbound_call_fixture(Handlers, Policy) ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000
    })),
    FakePeer = self(),
    PeerKp = macula_identity:generate(),
    PeerNodeId = macula_identity:public(PeerKp),
    _ = sys:replace_state(Pid, fun(S) ->
        S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
        setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
    end),
    lists:foreach(
      fun({Proc, Fun}) ->
              ok = macula_station_link:advertise(Pid, ?REALM, Proc, Fun, Policy)
      end, Handlers),
    {Pid, PeerKp}.

inject_call(Pid, FakePeer, CallerKp, CallId, Proc) ->
    Pid ! {macula_peering, frame, FakePeer, macula_frame:sign(#{
        frame_type => call,
        call_id    => CallId,
        realm      => ?REALM,
        procedure  => Proc,
        payload    => #{},
        caller     => macula_identity:public(CallerKp)
    }, CallerKp)}.

%% Same as inject_call/5, carrying a `ucan_token' too -- for exercising
%% `{ucan_required, _}'/`{realm_member_required, _}' gates, which read
%% it straight off the Frame the same way `authorize/3' does.
inject_call_with_ucan(Pid, FakePeer, CallerKp, CallId, Proc, UcanToken) ->
    Pid ! {macula_peering, frame, FakePeer, macula_frame:sign(#{
        frame_type => call,
        call_id    => CallId,
        realm      => ?REALM,
        procedure  => Proc,
        payload    => #{},
        caller     => macula_identity:public(CallerKp),
        ucan_token => UcanToken
    }, CallerKp)}.

%% The error frame's own `frame_type' is `error' (`macula_frame:call_error/1'
%% builds it on `base(error, 0)'), not `call_error' -- every OTHER place in
%% this file that matches an error frame directly already uses `error'
%% (e.g. line ~1628). This helper's own `call_error' match never fired
%% until `realm_member_required_test_' below became the first test to
%% actually need `await_result/2' to recognize an authorization failure --
%% both existing callers only ever exercised its RESULT branch. Fixed
%% here rather than left broken now that something depends on it.
await_result(CallId, TimeoutMs) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := result,
                                     call_id    := CallId,
                                     payload    := Payload}}} ->
            {ok, Payload};
        {'$gen_cast', {send_frame, #{frame_type := error,
                                     call_id    := CallId} = Err}} ->
            {error, Err}
    after TimeoutMs ->
        timeout
    end.

connected_flag(true)  -> 1;
connected_flag(false) -> 0.

inbound_call_handler_calling_back_into_link_does_not_deadlock_test_() ->
    {timeout, 15,
     fun() ->
         %% The handler calls back into the link that is delivering
         %% the CALL -- what every hecate-om desk does when it
         %% publishes rpc.received_v1 or makes a mesh call of its own.
         %% The link is registered under a name because the handler
         %% is advertised before it can know the link's pid.
         Handler = fun(_Payload) ->
                       Link = whereis(link_under_inbound_call_test),
                       Up = macula_station_link:is_connected(Link),
                       {ok, #{connected => connected_flag(Up)}}
                   end,
         {Pid, CallerKp} = inbound_call_fixture([{<<"probe.callback">>, Handler}]),
         true = register(link_under_inbound_call_test, Pid),
         CallId = crypto:strong_rand_bytes(16),
         inject_call(Pid, self(), CallerKp, CallId, <<"probe.callback">>),
         %% Inline, is_connected/1's 1 s gen_server:call into the
         %% blocked link exits with timeout and the handler crash
         %% surfaces as call_error; off-process it is a RESULT that
         %% agrees with what the link says from outside.
         Result = await_result(CallId, 5_000),
         Expected = connected_flag(macula_station_link:is_connected(Pid)),
         ?assertEqual({ok, #{connected => Expected}}, Result),
         macula_station_link:stop(Pid),
         ok
     end}.

inbound_calls_are_served_concurrently_test_() ->
    {timeout, 15,
     fun() ->
         Slow = fun(_Payload) -> timer:sleep(1_500), {ok, #{who => 1}} end,
         Fast = fun(_Payload) -> {ok, #{who => 2}} end,
         {Pid, CallerKp} = inbound_call_fixture([{<<"probe.slow">>, Slow},
                                               {<<"probe.fast">>, Fast}]),
         SlowId = crypto:strong_rand_bytes(16),
         FastId = crypto:strong_rand_bytes(16),
         inject_call(Pid, self(), CallerKp, SlowId, <<"probe.slow">>),
         inject_call(Pid, self(), CallerKp, FastId, <<"probe.fast">>),
         %% Inline, the fast reply queues behind the slow handler and
         %% arrives after ~1.5 s; off-process it arrives at once.
         ?assertMatch({ok, #{who := 2}}, await_result(FastId, 500)),
         ?assertMatch({ok, #{who := 1}}, await_result(SlowId, 3_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% {realm_member_required, RealmDid, RequiredCan} -- gates a procedure on
%% membership in a realm AT A SPECIFIC TIER (any valid token signed by
%% the realm's own DID, audience-bound to the calling identity, carrying
%% the required capability) rather than one exact known identity. See
%% `macula_client:auth_policy()' and `authorize_policy/2' for the full
%% design reasoning, including why the tier check is mandatory.
%%------------------------------------------------------------------

%% Mints a real membership-shaped token: `RealmIdentity' signs it (the
%% realm's own keypair), naming `MemberPub' (hex-encoded, matching
%% macula-realm's own `RealmUcanIssuer.mint_membership/2' convention) as
%% audience, and carrying `Can' as its one capability (default the
%% citizen/human-confirmed tier -- see `mint_membership_ucan/4' for a
%% caller-chosen tier, used by the device-tier-bypass test below).
%% `ExpOverride' lets a test set `exp' explicitly (e.g. already-expired);
%% omitted keys keep the default (valid for an hour).
mint_membership_ucan(RealmIdentity, MemberPub, ExpOverride) ->
    mint_membership_ucan(RealmIdentity, MemberPub, ExpOverride,
                         <<"member/email-verified">>).

mint_membership_ucan(RealmIdentity, MemberPub, ExpOverride, Can) ->
    IssuerDid = binary:encode_hex(macula_identity:public(RealmIdentity), lowercase),
    AudienceDid = binary:encode_hex(MemberPub, lowercase),
    Cap = #{with => <<"mri:realm:test">>, can => Can},
    Opts = maps:merge(#{exp => erlang:system_time(second) + 3_600}, ExpOverride),
    {ok, Token} = macula_ucan_nif:create(IssuerDid, AudienceDid, [Cap],
                                        macula_identity:private(RealmIdentity), Opts),
    Token.

realm_member_required_test_() ->
    {timeout, 15,
     fun() ->
         UnauthorizedCode = macula_bolt4:code(unauthorized),
         RealmIdentity = macula_identity:generate(),
         RealmDid = macula_identity:public(RealmIdentity),
         Handler = fun(_Payload) -> {ok, #{admitted => true}} end,
         Policy = {realm_member_required, RealmDid, <<"member/email-verified">>},
         {Pid, CallerKp} = inbound_call_fixture([{<<"realm.only">>, Handler}], Policy),
         Caller = macula_identity:public(CallerKp),

         %% A genuine member: token signed by the realm, audience is
         %% the identity actually making the call, capability matches
         %% the tier this procedure actually requires.
         GoodToken = mint_membership_ucan(RealmIdentity, Caller, #{}),
         GoodId = crypto:strong_rand_bytes(16),
         inject_call_with_ucan(Pid, self(), CallerKp, GoodId, <<"realm.only">>, GoodToken),
         ?assertMatch({ok, #{admitted := true}}, await_result(GoodId, 2_000)),

         %% THE FABLE-FOUND GAP: a token that is entirely genuine --
         %% signed by the real realm, correctly audienced to this exact
         %% caller, not expired -- but minted at the weaker self-service
         %% device tier (`member/device-verified'), which any device can
         %% obtain with no human involved (macula-realm's
         %% `MembershipUcanRpcHandlers', gated only by proof of key
         %% possession plus a default-permissive admission list). Before
         %% the capability check this authorized identically to a real
         %% citizen -- signature and audience alone cannot tell the two
         %% tiers apart, since both are genuine grants from the same
         %% realm key. Must be refused for a procedure that requires the
         %% stronger tier.
         DeviceTierToken = mint_membership_ucan(RealmIdentity, Caller, #{},
                                                <<"member/device-verified">>),
         DeviceTierId = crypto:strong_rand_bytes(16),
         inject_call_with_ucan(Pid, self(), CallerKp, DeviceTierId, <<"realm.only">>,
                               DeviceTierToken),
         ?assertMatch({error, #{code := UnauthorizedCode}},
                      await_result(DeviceTierId, 2_000)),

         %% No token at all.
         NoTokenId = crypto:strong_rand_bytes(16),
         inject_call(Pid, self(), CallerKp, NoTokenId, <<"realm.only">>),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(NoTokenId, 2_000)),

         %% Token signed by a DIFFERENT key than the declared realm --
         %% a plausible-looking membership token that simply isn't from
         %% this realm at all.
         OtherRealmIdentity = macula_identity:generate(),
         WrongIssuerToken = mint_membership_ucan(OtherRealmIdentity, Caller, #{}),
         WrongIssuerId = crypto:strong_rand_bytes(16),
         inject_call_with_ucan(Pid, self(), CallerKp, WrongIssuerId, <<"realm.only">>, WrongIssuerToken),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(WrongIssuerId, 2_000)),

         %% Expired: genuinely signed by the realm, for this exact
         %% caller, but its own exp has already passed.
         ExpiredToken = mint_membership_ucan(RealmIdentity, Caller,
                                             #{exp => erlang:system_time(second) - 60}),
         ExpiredId = crypto:strong_rand_bytes(16),
         inject_call_with_ucan(Pid, self(), CallerKp, ExpiredId, <<"realm.only">>, ExpiredToken),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(ExpiredId, 2_000)),

         %% THE REPLAY CASE: a token that is completely genuine --
         %% signed by the real realm, not expired -- but minted for a
         %% DIFFERENT member than whoever is actually making this call.
         %% Whoever obtained a copy of another member's token cannot use
         %% it as their own: its audience is bound to `Caller', the
         %% wire-authenticated identity making THIS call, the same check
         %% `ucan_required' applies (see its own test below).
         RightfulOwner = macula_identity:public(macula_identity:generate()),
         StolenToken = mint_membership_ucan(RealmIdentity, RightfulOwner, #{}),
         StolenId = crypto:strong_rand_bytes(16),
         inject_call_with_ucan(Pid, self(), CallerKp, StolenId, <<"realm.only">>, StolenToken),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(StolenId, 2_000)),

         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% {ucan_required, Issuer} -- binds the token's audience to the caller
%%------------------------------------------------------------------

%% A token for a `ucan_required' procedure: `IssuerIdentity' signs it and
%% names `AudiencePub' (hex-encoded, the same convention as membership
%% tokens) as audience, with a plain `call' capability.
mint_ucan(IssuerIdentity, AudiencePub, ExpOverride) ->
    mint_membership_ucan(IssuerIdentity, AudiencePub, ExpOverride, <<"call">>).

%% `{ucan_required, Issuer}' serves a CALL only when its token is signed by
%% `Issuer', unexpired, and minted for the identity that signed this CALL. A
%% genuine token minted for anyone else is refused, like no token at all.
ucan_required_binds_the_token_audience_to_the_caller_test_() ->
    {timeout, 15,
     fun() ->
         UnauthorizedCode = macula_bolt4:code(unauthorized),
         IssuerIdentity = macula_identity:generate(),
         Handler = fun(_Payload) -> {ok, #{served => true}} end,
         Policy = {ucan_required, macula_identity:public(IssuerIdentity)},
         {Pid, CallerKp} = inbound_call_fixture([{<<"issuer.only">>, Handler}], Policy),
         Caller = macula_identity:public(CallerKp),

         OwnId = crypto:strong_rand_bytes(16),
         inject_call_with_ucan(Pid, self(), CallerKp, OwnId, <<"issuer.only">>,
                               mint_ucan(IssuerIdentity, Caller, #{})),
         ?assertMatch({ok, #{served := true}}, await_result(OwnId, 2_000)),

         Someone = macula_identity:public(macula_identity:generate()),
         ForSomeoneId = crypto:strong_rand_bytes(16),
         inject_call_with_ucan(Pid, self(), CallerKp, ForSomeoneId, <<"issuer.only">>,
                               mint_ucan(IssuerIdentity, Someone, #{})),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(ForSomeoneId, 2_000)),

         NoTokenId = crypto:strong_rand_bytes(16),
         inject_call(Pid, self(), CallerKp, NoTokenId, <<"issuer.only">>),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(NoTokenId, 2_000)),

         WrongIssuerId = crypto:strong_rand_bytes(16),
         inject_call_with_ucan(Pid, self(), CallerKp, WrongIssuerId, <<"issuer.only">>,
                               mint_ucan(macula_identity:generate(), Caller, #{})),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(WrongIssuerId, 2_000)),

         ExpiredId = crypto:strong_rand_bytes(16),
         inject_call_with_ucan(Pid, self(), CallerKp, ExpiredId, <<"issuer.only">>,
                               mint_ucan(IssuerIdentity, Caller,
                                         #{exp => erlang:system_time(second) - 60})),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(ExpiredId, 2_000)),

         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% An inbound CALL is served only when its signature verifies
%%------------------------------------------------------------------

%% A CALL is signed by the identity it names in `caller', and that field
%% is what `with_caller/2' hands the handler and what
%% `realm_member_required' binds a token's audience to. A CALL whose
%% signature does not verify against `caller' never runs the handler and
%% gets no reply; a genuinely signed CALL that follows is served.
call_that_does_not_verify_is_not_served_test_() ->
    {timeout, 10,
     fun() ->
         Test = self(),
         Handler = fun(#{tag := Tag}) ->
                       Test ! {handler_ran, Tag},
                       {ok, #{tag => Tag}}
                   end,
         {Pid, CallerKp} = inbound_call_fixture([{<<"probe.verify">>, Handler}]),
         Caller = macula_identity:public(CallerKp),
         Call = fun(Id, Tag) ->
                    #{frame_type => call, call_id => Id, realm => ?REALM,
                      procedure => <<"probe.verify">>, payload => #{tag => Tag},
                      caller => Caller}
                end,
         [ForgedId, UnsignedId, GenuineId] =
             [crypto:strong_rand_bytes(16) || _ <- [1, 2, 3]],
         %% Names `Caller' and is signed by another key.
         Pid ! {macula_peering, frame, Test,
                macula_frame:sign(Call(ForgedId, forged), macula_identity:generate())},
         %% Names `Caller' and carries no signature.
         Pid ! {macula_peering, frame, Test, Call(UnsignedId, unsigned)},
         Pid ! {macula_peering, frame, Test,
                macula_frame:sign(Call(GenuineId, genuine), CallerKp)},
         ?assertEqual({ok, #{tag => genuine}}, await_result(GenuineId, 2_000)),
         ?assertEqual(timeout, await_result(ForgedId, 300)),
         ?assertEqual(timeout, await_result(UnsignedId, 300)),
         receive
             {handler_ran, Ran} when Ran =/= genuine ->
                 erlang:error({handler_ran_for, Ran})
         after 300 ->
             ok
         end,
         macula_station_link:stop(Pid),
         ok
     end}.
