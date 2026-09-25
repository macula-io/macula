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

%% Realm-per-call: tests use the realm named "test" for every call, subscribe and publish. A realm id is SHA-256
%% over its name (D7), so a capability naming `mri:realm:test' is a capability in THIS realm, which is what a gated
%% procedure's check compares. The all-zero realm has no name, so no delegated grant could cover it.
-define(REALM_NAME, <<"test">>).
-define(REALM, crypto:hash(sha256, ?REALM_NAME)).
%% A gated procedure carries an org namespace, the text before its first slash (D25): without one no policy
%% authorizes it.
-define(GATED_PROCEDURE, <<"acme/realm.only">>).
-define(ISSUER_PROCEDURE, <<"acme/issuer.only">>).

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
%% What a call error says about trying the same request elsewhere
%%------------------------------------------------------------------

%% `candidate': nothing went out and what failed is THIS link or THIS station,
%% so another may well work.
a_failure_of_this_link_or_this_station_is_candidate_scoped_test() ->
    ?assertEqual(candidate, macula_station_link:failure_scope({error, not_connected})),
    ?assertEqual(candidate, macula_station_link:failure_scope({error, noproc})),
    ?assertEqual(candidate,
                 macula_station_link:failure_scope({error, {dial_refused, unusable_seed}})),
    ?assertEqual(candidate,
                 macula_station_link:failure_scope({error, {dial_refused, too_many_direct_links}})),
    ?assertEqual(candidate,
                 macula_station_link:failure_scope({error, {dial_refused, new_peer_budget_spent}})).

%% `request': nothing went out and what failed is the REQUEST, so every
%% candidate refuses it identically and there is nothing to gain by asking one
%% more. Both shapes are checked, since the scope has to hold for every reason
%% that reaches it and not merely for the one that is easiest to name.
a_failure_of_the_request_itself_is_request_scoped_test() ->
    ?assertEqual(request,
                 macula_station_link:failure_scope(
                   {error, {refused, {unsupported_payload_type, payload_too_large, []}}})),
    ?assertEqual(request,
                 macula_station_link:failure_scope({error, {refused, {text_too_long, procedure}}})),
    ?assertEqual(request,
                 macula_station_link:failure_scope({error, {open_too_large, 1048576}})).

%% `provider': it MAY have reached one, so it must never be sent elsewhere. A
%% timeout and a station's `unknown_next_peer' are here deliberately: neither
%% tells the caller whether a provider ran the call.
%%
%% The last two matter for a different reason. The function matches only terms
%% the link and the pool build, and a provider's code and detail are binaries,
%% so a provider that names itself `not_connected' in text cannot talk its way
%% into being retried.
a_failure_that_may_have_reached_a_provider_is_provider_scoped_test() ->
    ?assertEqual(provider, macula_station_link:failure_scope({error, timeout})),
    ?assertEqual(provider, macula_station_link:failure_scope({error, {disconnected, closed}})),
    ?assertEqual(provider, macula_station_link:failure_scope({error, gone})),
    ?assertEqual(provider,
                 macula_station_link:failure_scope(
                   {error, {call_error, unknown_next_peer, undefined}})),
    ?assertEqual(provider,
                 macula_station_link:failure_scope(
                   {error, {call_error, <<"overloaded">>, undefined}})),
    ?assertEqual(provider, macula_station_link:failure_scope({error, <<"not_connected">>})).

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
         PeerNodeId = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         {ok, SubRef} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
         Joiner = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         {ok, _SubRef} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
         OtherRealm = crypto:strong_rand_bytes(32),
         Joiner = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Joiner = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
         end),
         Target = macula_test_identity:node_id(),
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
         StationNodeId = macula_test_identity:node_id(),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_NODE_ID_INDEX, S2, StationNodeId)
         end),
         {ok, SubRef} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
         Origin = macula_test_identity:node_id(),
         ?assertNotEqual(Origin, StationNodeId),
         Joiner = macula_test_identity:node_id(),
         Inner = macula_frame:hyparview_join(#{realm => ?REALM, new_member => Joiner}),
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


start_connected_link() ->
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000
    })),
    FakePeer = self(),
    PeerNodeId = macula_test_identity:node_id(),
    _ = sys:replace_state(Pid, fun(S) ->
        S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
        setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
    end),
    {Pid, FakePeer, PeerNodeId}.

%% The inner frame of an `overlay_relay' envelope is taken as it is: the
%% station already authenticated the envelope's origin, so the link
%% verifies nothing itself and delivers each inner frame as sent. The
%% envelope's origin goes out as Meta.sender, the station as Meta.via.
overlay_relay_inner_frame_is_taken_as_sent_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {Pid, FakePeer, StationNodeId} = start_connected_link(),
         {ok, SubRef} = macula_station_link:overlay_subscribe(Pid, ?REALM, self()),
         Origin = macula_test_identity:node_id(),
         Relay = fun(Inner) ->
                     Pid ! {macula_peering, frame, FakePeer,
                            macula_frame:overlay_relay(#{
                                peer    => Origin,
                                payload => macula_frame:encode(Inner)})}
                 end,
         Join = fun() ->
                    macula_frame:hyparview_join(#{
                        realm      => ?REALM,
                        new_member => macula_test_identity:node_id()})
                end,
         %% Each frame is delivered as sent, in order.
         Frames = [Join(), Join(), Join()],
         [Relay(Frame) || Frame <- Frames],
         collect_relayed(SubRef, Frames, #{sender => Origin, via => StationNodeId}),
         macula_station_link:stop(Pid),
         ok
     end}.

collect_relayed(_SubRef, [], _Meta) ->
    ok;
collect_relayed(SubRef, [Expected | Rest], Meta) ->
    {ok, ExpectedDecoded, <<>>} = macula_frame:decode(macula_frame:encode(Expected)),
    receive
        {macula_overlay_frame, SubRef, Delivered, DeliveredMeta} ->
            ?assertEqual(ExpectedDecoded, Delivered),
            ?assertEqual(maps:get(sender, Meta), maps:get(sender, DeliveredMeta)),
            ?assertEqual(maps:get(via, Meta), maps:get(via, DeliveredMeta)),
            collect_relayed(SubRef, Rest, Meta)
    after 2_000 ->
        erlang:error({missing_overlay_frame, maps:get(frame_id, Expected)})
    end.

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
         Joiner = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
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
         Joiner = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
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
         PeerNodeId = macula_test_identity:node_id(),
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
         Handler = fun(#{{text, <<"token">>} := <<"abc">>}) -> {ok, #{member_id => 42}} end,
         {Pid, CallerKey} = inbound_call_fixture(
                              [{<<"_realm.membership.join_with_token_v1">>, Handler}]),
         CallFrame = inject_call_with_payload(Pid, self(), CallerKey, <<1:128>>,
                                               <<"_realm.membership.join_with_token_v1">>,
                                               #{token => <<"abc">>}),
         ?assertEqual({ok, #{{text, <<"member_id">>} => 42}},
                      await_result(CallFrame, 1_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

inbound_call_threads_caller_into_payload_test_() ->
    {timeout, 5,
     fun() ->
         Self = self(),
         %% The handler asserts what it actually received -- this is the
         %% behaviour under test, not the request's own `caller' field.
         Handler = fun(Payload) ->
             Self ! {handler_saw, Payload},
             {ok, #{member_id => 42}}
         end,
         {Pid, CallerKey} = inbound_call_fixture(
                              [{<<"_realm.membership.join_with_token_v1">>, Handler}]),
         Caller = macula_node_keys:key_id(CallerKey),
         {ok, Profile} = macula_crypto_profile:configured(),
         {ok, SpoofKey} = macula_node_keys:generate(identity, Profile),
         SpoofedCaller = macula_node_keys:key_id(SpoofKey),
         _ = inject_call_with_payload(Pid, self(), CallerKey, <<1:128>>,
                                      <<"_realm.membership.join_with_token_v1">>,
                                      #{token => <<"abc">>, caller => SpoofedCaller}),
         receive
             {handler_saw, Payload} ->
                 ?assertEqual(Caller, maps:get(caller, Payload)),
                 ?assertNotEqual(SpoofedCaller, maps:get(caller, Payload)),
                 ?assertEqual(<<"abc">>, maps:get({text, <<"token">>}, Payload))
         after 1_000 ->
             erlang:error(handler_never_invoked)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%% As inject_call/5, with an explicit payload -- for proving the
%% wire-authenticated caller wins over whatever the payload claims.
inject_call_with_payload(Pid, FakePeer, CallerKey, CallId, Proc, Payload) ->
    Frame = macula_frame:call(
              #{request_id => CallId, realm => ?REALM, procedure => Proc,
                target => link_node_id(Pid),
                deadline => erlang:system_time(millisecond) + 5_000,
                payload => Payload}, CallerKey),
    Pid ! {macula_peering, frame, FakePeer, Frame},
    Frame.

inbound_call_unknown_procedure_returns_error_frame_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, CallerKey} = inbound_call_fixture([]),
         CallFrame = inject_call(Pid, self(), CallerKey, <<2:128>>,
                                 <<"_no.such.procedure">>),
         ?assertMatch({error, #{code := <<"unknown_next_peer">>}},
                      await_result(CallFrame, 1_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

inbound_call_handler_crash_returns_error_frame_test_() ->
    {timeout, 5,
     fun() ->
         Handler = fun(_Args) -> error(deliberate) end,
         {Pid, CallerKey} = inbound_call_fixture([{<<"_test.crash">>, Handler}]),
         CallFrame = inject_call(Pid, self(), CallerKey, <<3:128>>, <<"_test.crash">>),
         ?assertMatch({error, #{code := <<"temporary_relay_failure">>}},
                      await_result(CallFrame, 1_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

inbound_call_handler_error_tuple_emits_call_error_test_() ->
    %% Handler returning `{error, Reason}' MUST emit a provider ERROR
    %% frame, NOT a `result' frame. RESULT payloads go through
    %% `macula_record_cbor:encode/1', which has no clause for raw
    %% tuples -- sending `{error, _}' inside a RESULT crashes the
    %% frame build and drops every other multiplexed RPC on the
    %% connection. Pre-4.1.1 this bit production every time
    %% `_dht.put_record' got a bad-signature record from the
    %% replication path.
    %%
    %% #28: the code is `handler_error', the one a caller unwraps to
    %% `{error, Detail}'. It was `unknown_error', so every handler's own
    %% reason reached a caller as an opaque provider error that looked like
    %% a platform fault.
    {timeout, 5,
     fun() ->
         Handler = fun(_Args) -> {error, invalid_token} end,
         {Pid, CallerKey} = inbound_call_fixture([{<<"_test.app_error">>, Handler}]),
         CallFrame = inject_call(Pid, self(), CallerKey, <<4:128>>, <<"_test.app_error">>),
         ?assertMatch({error, #{code := <<"handler_error">>, detail := <<"invalid_token">>}},
                      await_result(CallFrame, 1_000)),
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
         Handler = fun(_Args) -> {error, <<"hold_full">>} end,
         {Pid, CallerKey} = inbound_call_fixture([{<<"_test.refusal">>, Handler}]),
         CallFrame = inject_call(Pid, self(), CallerKey, <<7:128>>, <<"_test.refusal">>),
         ?assertMatch({error, #{code := <<"handler_error">>, detail := <<"hold_full">>}},
                      await_result(CallFrame, 1_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

flush_send_frame_casts() ->
    receive
        {'$gen_cast', {send_frame, _}} -> flush_send_frame_casts()
    after 0 -> ok
    end.

flush_mailbox() ->
    receive _ -> flush_mailbox()
    after 0 -> ok
    end.

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
%%------------------------------------------------------------------
%% A request's deadline and its one run (D7 check 2, D22's tolerance)
%%------------------------------------------------------------------

%% A provider judges a CALL by its signed deadline before it runs anything: a deadline further past than D22's five
%% minutes of clock tolerance is refused, and so is one more than ten minutes ahead, which would hold a request id
%% for longer than the window it is kept for.
a_call_outside_the_deadline_window_is_refused_test_() ->
    {timeout, 15,
     fun() ->
         Handler = fun(_Payload) -> {ok, #{served => true}} end,
         {Pid, CallerKp} = inbound_call_fixture([{<<"p">>, Handler}]),
         Now = erlang:system_time(millisecond),
         Expired = <<"expired">>,
         NotYetValid = <<"not_yet_valid">>,
         ?assertMatch({error, #{code := Expired}},
                      await_result(inject_call_with_deadline(Pid, CallerKp, Now - 6 * 60_000), 2_000)),
         ?assertMatch({error, #{code := NotYetValid}},
                      await_result(inject_call_with_deadline(Pid, CallerKp, Now + 11 * 60_000), 2_000)),
         %% Inside the window, on either side of now, the call is served.
         ?assertMatch({ok, _}, await_result(inject_call_with_deadline(Pid, CallerKp, Now - 60_000), 2_000)),
         ?assertMatch({ok, _}, await_result(inject_call_with_deadline(Pid, CallerKp, Now + 60_000), 2_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

%% A provider runs a request once. A copy of a CALL that has already been answered gets the reply it stored, so a
%% caller that retries a request whose reply was lost is answered rather than served twice; a different request
%% under a request id already held is refused. There is no nonce store for CALL and STREAM_OPEN: the signed
%% deadline and this window are what stop a replay (D7).
a_repeated_call_gets_its_stored_reply_and_a_reused_id_is_refused_test_() ->
    {timeout, 15,
     fun() ->
         Self = self(),
         Handler = fun(_Payload) -> Self ! ran, {ok, #{served => true}} end,
         {Pid, CallerKp} = inbound_call_fixture([{<<"p">>, Handler}]),
         CallId = crypto:strong_rand_bytes(16),

         First = inject_call(Pid, self(), CallerKp, CallId, <<"p">>),
         ?assertMatch({ok, #{{text, <<"served">>} := {text, <<"true">>}}}, await_result(First, 2_000)),
         ?assertEqual(ran, receive ran -> ran after 2_000 -> none end),

         %% The same request again, THE SAME BYTES as a retry sends: the stored reply, and the handler does not run
         %% a second time. A rebuilt frame would carry another deadline, so another request hash, which is a
         %% different request under a held id and is refused below.
         Pid ! {macula_peering, frame, self(), First},
         ?assertMatch({ok, #{{text, <<"served">>} := {text, <<"true">>}}}, await_result(First, 2_000)),
         ?assertEqual(none, receive ran -> ran after 500 -> none end),

         %% Another request under the same id is not that request: refused, not served.
         Reused = <<"request_id_reused">>,
         Other = inject_call_with_payload(Pid, self(), CallerKp, CallId, <<"p">>, #{other => 1}),
         ?assertMatch({error, #{code := Reused}}, await_result(Other, 2_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

%% The link's station's liveness probe, a `_macula.ping' CALL on the all-zero realm signed by that station and
%% targeted at this node, is answered as before (`unknown_next_peer') but takes no place in the admission: stations
%% probe every link every 30 s, and each probe held an entry for its deadline plus 5 minutes, so a station filled its
%% caller quota on a provider within hours (macula#37). A ping from any other caller, or targeted at another node, is
%% judged like any request and takes an entry, as does a CALL for an unknown procedure.
a_liveness_probe_is_answered_without_an_admission_entry_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, StationKey} = inbound_call_fixture([]),
         Admission = element(macula_station_link:state_field_index(admission), sys:get_state(Pid)),
         {ok, Profile} = macula_crypto_profile:configured(),
         {ok, OtherKey} = macula_node_keys:generate(identity, Profile),
         [begin
              Pid ! {macula_peering, frame, self(), Ping},
              ?assertMatch({error, #{code := <<"unknown_next_peer">>}}, await_result(Ping, 1_000))
          end || Ping <- [ping(StationKey, link_node_id(Pid)), ping(OtherKey, link_node_id(Pid))]],
         %% Its reply is signed by this node, not the one it names, so it does not verify as that node's reply; the
         %% link has judged it once it answers the next message.
         Pid ! {macula_peering, frame, self(), ping(StationKey, <<1:256>>)},
         _ = sys:get_state(Pid),
         %% The station's ping to this node on another realm, or for another procedure on the all-zero realm, is not
         %% its probe either.
         [begin
              Pid ! {macula_peering, frame, self(), Ping},
              ?assertMatch({error, #{code := <<"unknown_next_peer">>}}, await_result(Ping, 1_000))
          end || Ping <- [ping(StationKey, link_node_id(Pid), ?REALM, <<"_macula.ping">>),
                          ping(StationKey, link_node_id(Pid), <<0:256>>, <<"_macula.pong">>)]],
         Other = inject_call(Pid, self(), StationKey, crypto:strong_rand_bytes(16), <<"_no.such.procedure">>),
         ?assertMatch({error, #{code := <<"unknown_next_peer">>}}, await_result(Other, 1_000)),
         Later = erlang:system_time(millisecond) + 3_600_000,
         ?assertEqual(5, macula_request_admission:sweep(Admission, Later)),
         macula_station_link:stop(Pid),
         ok
     end}.

ping(CallerKey, Target) ->
    ping(CallerKey, Target, <<0:256>>, <<"_macula.ping">>).

ping(CallerKey, Target, Realm, Procedure) ->
    macula_frame:call(#{request_id => crypto:strong_rand_bytes(16), realm => Realm,
                        procedure => Procedure, target => Target,
                        deadline => erlang:system_time(millisecond) + 30_000, payload => #{}}, CallerKey).

%% A CALL with a deadline of the test's choosing.
inject_call_with_deadline(Pid, CallerKey, Deadline) ->
    Frame = macula_frame:call(
              #{request_id => crypto:strong_rand_bytes(16), realm => ?REALM, procedure => <<"p">>,
                target => link_node_id(Pid), deadline => Deadline, payload => #{}}, CallerKey),
    Pid ! {macula_peering, frame, self(), Frame},
    Frame.

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
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, PeerKey} = macula_node_keys:generate(identity, Profile),
    {ok, PeerNodeId} = macula_node_keys:node_id(PeerKey),
    _ = sys:replace_state(Pid, fun(S) ->
        S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
        setelement(?PEER_NODE_ID_INDEX, S2, PeerNodeId)
    end),
    lists:foreach(
      fun({Proc, Fun}) ->
              ok = macula_station_link:advertise(Pid, ?REALM, Proc, Fun, Policy)
      end, Handlers),
    {Pid, PeerKey}.

%% The link's own node id, as a request targets it.
link_node_id(Pid) ->
    Key = element(macula_station_link:state_field_index(node_identity), sys:get_state(Pid)),
    macula_node_keys:key_id(Key).

%% Inject a signed CALL for Proc from CallerKey, addressed to the link's
%% own node. Returns the frame, for await_result/2 to verify replies
%% against.
inject_call(Pid, FakePeer, CallerKey, CallId, Proc) ->
    Frame = macula_frame:call(
              #{request_id => CallId, realm => ?REALM, procedure => Proc,
                target => link_node_id(Pid),
                deadline => erlang:system_time(millisecond) + 5_000,
                payload => #{}}, CallerKey),
    Pid ! {macula_peering, frame, FakePeer, Frame},
    Frame.

%% Same as inject_call/5, carrying a `token' too -- for exercising
%% `{ucan_required, _}'/`{realm_member_required, _}' gates, which read
%% it straight off the verified request the same way `authorize/3' does.
inject_call_with_ucan(Pid, FakePeer, CallerKey, CallId, Proc, UcanToken) ->
    Frame = macula_frame:call(
              #{request_id => CallId, realm => ?REALM, procedure => Proc,
                target => link_node_id(Pid),
                deadline => erlang:system_time(millisecond) + 5_000,
                payload => #{}, token => UcanToken}, CallerKey),
    Pid ! {macula_peering, frame, FakePeer, Frame},
    Frame.

%% A request that does not verify has no Request to verify a reply
%% against; only silence is expected.
no_reply_in(TimeoutMs) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := Type}}} when Type =:= result; Type =:= error ->
            {error, {unexpected_reply, Type}}
    after TimeoutMs ->
        timeout
    end.

%% The reply to a CALL frame is a signed result or provider-error frame;
%% both verify against the request, and the verified fields come back
%% with their wire decoding (payloads text-keyed, codes as their text).
await_result(CallFrame, TimeoutMs) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Request} = macula_frame:verify_request(CallFrame, Profile),
    RequestId = maps:get(request_id, Request),
    await_reply_for(RequestId, Request, Profile, TimeoutMs).

%% Several calls may be in flight (ucan_required injects five): a reply
%% that verifies but names another request is skipped, not returned.
await_reply_for(RequestId, Request, Profile, TimeoutMs) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := Type} = Frame}}
          when Type =:= result; Type =:= error ->
            case macula_frame:claimed_reply_ids(Frame) of
                {ok, #{request_id := RequestId}} ->
                    {ok, Fields} = macula_frame:verify_reply(Frame, Request, Profile),
                    case Type of
                        result -> {ok, maps:get(payload, Fields)};
                        error  -> {error, Fields}
                    end;
                _Other ->
                    await_reply_for(RequestId, Request, Profile, TimeoutMs)
            end
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
         CallFrame = inject_call(Pid, self(), CallerKp, CallId, <<"probe.callback">>),
         %% Inline, is_connected/1's 1 s gen_server:call into the
         %% blocked link exits with timeout and the handler crash
         %% surfaces as a provider error; off-process it is a RESULT that
         %% agrees with what the link says from outside.
         Result = await_result(CallFrame, 5_000),
         Expected = connected_flag(macula_station_link:is_connected(Pid)),
         ?assertEqual({ok, #{{text, <<"connected">>} => Expected}}, Result),
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
         SlowFrame = inject_call(Pid, self(), CallerKp, SlowId, <<"probe.slow">>),
         FastFrame = inject_call(Pid, self(), CallerKp, FastId, <<"probe.fast">>),
         %% Inline, the fast reply queues behind the slow handler and
         %% arrives after ~1.5 s; off-process it arrives at once.
         ?assertMatch({ok, #{{text, <<"who">>} := 2}}, await_result(FastFrame, 500)),
         ?assertMatch({ok, #{{text, <<"who">>} := 1}}, await_result(SlowFrame, 3_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% {realm_member_required, RealmKeyId, RequiredCan} -- gates a procedure on
%% membership in a realm AT A SPECIFIC TIER (any valid token signed by
%% the realm's key, audience-bound to the calling node, carrying the
%% required capability) rather than one exact known node. See
%% `macula_client:auth_policy()' and macula_ucan for the full design
%% reasoning, including why the tier check is mandatory.
%%------------------------------------------------------------------

%% Mints a real membership-shaped token: `RealmKey' signs it (the realm's
%% own key), naming `Member' (a node_id) as audience, and carrying `Can'
%% as its one capability (default the citizen/human-confirmed tier -- see
%% `mint_membership_ucan/4' for a caller-chosen tier, used by the
%% device-tier-bypass test below). `ExpOverride' lets a test set `exp'
%% explicitly (e.g. already-expired); omitted keys keep the default (valid
%% for an hour).
realm_key() ->
    {ok, Key} = macula_node_keys:generate(realm, pq_pure),
    Key.

mint_membership_ucan(RealmKey, Member, ExpOverride) ->
    mint_membership_ucan(RealmKey, Member, ExpOverride, <<"member/email-verified">>).

mint_membership_ucan(IssuerKey, Member, ExpOverride, Can) ->
    Opts = maps:merge(#{exp => erlang:system_time(second) + 3_600}, ExpOverride),
    macula_ucan:create(IssuerKey, Member, [#{with => <<"mri:realm:test">>, can => Can}], Opts).

realm_member_required_test_() ->
    {timeout, 15,
     fun() ->
         UnauthorizedCode = <<"unauthorized">>,
         RealmIdentity = realm_key(),
         Handler = fun(_Payload) -> {ok, #{admitted => true}} end,
         Policy = {realm_member_required, macula_node_keys:key_id(RealmIdentity), <<"member/email-verified">>},
         {Pid, CallerKp} = inbound_call_fixture([{?GATED_PROCEDURE, Handler}], Policy),
         Caller = macula_node_keys:key_id(CallerKp),

         %% A genuine member: token signed by the realm, audience is
         %% the identity actually making the call, capability matches
         %% the tier this procedure actually requires.
         GoodToken = mint_membership_ucan(RealmIdentity, Caller, #{}),
         GoodId = crypto:strong_rand_bytes(16),
         GoodFrame = inject_call_with_ucan(Pid, self(), CallerKp, GoodId, ?GATED_PROCEDURE, GoodToken),
         ?assertMatch({ok, #{{text, <<"admitted">>} := {text, <<"true">>}}}, await_result(GoodFrame, 2_000)),

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
         DeviceTierFrame = inject_call_with_ucan(Pid, self(), CallerKp, DeviceTierId, ?GATED_PROCEDURE,
                                                 DeviceTierToken),
         ?assertMatch({error, #{code := UnauthorizedCode}},
                      await_result(DeviceTierFrame, 2_000)),

         %% No token at all.
         NoTokenId = crypto:strong_rand_bytes(16),
         NoTokenFrame = inject_call(Pid, self(), CallerKp, NoTokenId, ?GATED_PROCEDURE),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(NoTokenFrame, 2_000)),

         %% Token signed by a DIFFERENT key than the declared realm --
         %% a plausible-looking membership token that simply isn't from
         %% this realm at all.
         OtherRealmIdentity = realm_key(),
         WrongIssuerToken = mint_membership_ucan(OtherRealmIdentity, Caller, #{}),
         WrongIssuerId = crypto:strong_rand_bytes(16),
         WrongIssuerFrame = inject_call_with_ucan(Pid, self(), CallerKp, WrongIssuerId, ?GATED_PROCEDURE, WrongIssuerToken),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(WrongIssuerFrame, 2_000)),

         %% Expired: genuinely signed by the realm, for this exact
         %% caller, but its own exp has already passed.
         ExpiredToken = mint_membership_ucan(RealmIdentity, Caller,
                                             #{exp => erlang:system_time(second) - 60}),
         ExpiredId = crypto:strong_rand_bytes(16),
         ExpiredFrame = inject_call_with_ucan(Pid, self(), CallerKp, ExpiredId, ?GATED_PROCEDURE, ExpiredToken),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(ExpiredFrame, 2_000)),

         %% THE REPLAY CASE: a token that is completely genuine --
         %% signed by the real realm, not expired -- but minted for a
         %% DIFFERENT member than whoever is actually making this call.
         %% Whoever obtained a copy of another member's token cannot use
         %% it as their own: its audience is bound to `Caller', the
         %% wire-authenticated identity making THIS call, the same check
         %% `ucan_required' applies (see its own test below).
         RightfulOwner = macula_test_identity:node_id(),
         StolenToken = mint_membership_ucan(RealmIdentity, RightfulOwner, #{}),
         StolenId = crypto:strong_rand_bytes(16),
         StolenFrame = inject_call_with_ucan(Pid, self(), CallerKp, StolenId, ?GATED_PROCEDURE, StolenToken),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(StolenFrame, 2_000)),

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

%% A caller may present a capability it was delegated: its own token, and the tokens between it and the issuer the
%% policy trusts, in the request's `proofs'. The link hands the request's realm, procedure and proofs to
%% macula_ucan:authorize/3, which walks the chain (D7, WP 1.4 U2).
a_delegated_call_is_served_and_an_unreferenced_proof_is_malformed_test_() ->
    {timeout, 15,
     fun() ->
         RealmIdentity = realm_key(),
         {ok, OrgKey} = macula_node_keys:generate(org, pq_pure),
         OrgId = macula_node_keys:node_id(macula_node_keys:public_key(OrgKey), pq_pure),
         Handler = fun(_Payload) -> {ok, #{delegated => true}} end,
         Policy = {realm_member_required, macula_node_keys:key_id(RealmIdentity), <<"call">>},
         {Pid, CallerKp} = inbound_call_fixture([{?GATED_PROCEDURE, Handler}], Policy),
         Caller = macula_node_keys:key_id(CallerKp),

         %% The realm delegates its org to the org key, which delegates this one procedure to the caller.
         Proof = macula_ucan:create(RealmIdentity, OrgId,
                                    [#{with => <<"mri:org:test/acme">>, can => <<"call">>}],
                                    #{exp => erlang:system_time(second) + 3_600}),
         Token = macula_ucan:create(OrgKey, Caller,
                                    [#{with => <<"mri:proc:test/", (?GATED_PROCEDURE)/binary>>, can => <<"call">>}],
                                    #{exp => erlang:system_time(second) + 3_600,
                                      prf => [macula_ucan:proof_id(Proof)]}),

         Served = inject_call_with_chain(Pid, self(), CallerKp, crypto:strong_rand_bytes(16), Token, [Proof]),
         ?assertMatch({ok, #{{text, <<"delegated">>} := {text, <<"true">>}}}, await_result(Served, 2_000)),

         %% A proof nothing in the chain names is the caller sending what no check reads: malformed_frame, not
         %% unauthorized, since the request itself is wrong rather than the authority it claims (D7).
         Extra = macula_ucan:create(RealmIdentity, Caller, [#{with => <<"mri:realm:test">>, can => <<"call">>}],
                                    #{exp => erlang:system_time(second) + 3_600}),
         Unreferenced = inject_call_with_chain(Pid, self(), CallerKp, crypto:strong_rand_bytes(16), Token,
                                               [Proof, Extra]),
         MalformedCode = <<"malformed_frame">>,
         ?assertMatch({error, #{code := MalformedCode}}, await_result(Unreferenced, 2_000)),

         macula_station_link:stop(Pid),
         ok
     end}.

%% A CALL carrying a delegated token and the proofs that back it.
inject_call_with_chain(Pid, FakePeer, CallerKey, CallId, Token, Proofs) ->
    Frame = macula_frame:call(
              #{request_id => CallId, realm => ?REALM, procedure => ?GATED_PROCEDURE,
                target => link_node_id(Pid),
                deadline => erlang:system_time(millisecond) + 5_000,
                payload => #{}, token => Token, proofs => Proofs}, CallerKey),
    Pid ! {macula_peering, frame, FakePeer, Frame},
    Frame.

%% `{ucan_required, Issuer}' serves a CALL only when its token is signed by
%% `Issuer', unexpired, and minted for the identity that signed this CALL. A
%% genuine token minted for anyone else is refused, like no token at all.
ucan_required_binds_the_token_audience_to_the_caller_test_() ->
    {timeout, 15,
     fun() ->
         UnauthorizedCode = <<"unauthorized">>,
         IssuerIdentity = macula_test_identity:key(),
         Handler = fun(_Payload) -> {ok, #{served => true}} end,
         {ok, IssuerNodeId} = macula_node_keys:node_id(IssuerIdentity),
         Policy = {ucan_required, IssuerNodeId},
         {Pid, CallerKp} = inbound_call_fixture([{?ISSUER_PROCEDURE, Handler}], Policy),
         Caller = macula_node_keys:key_id(CallerKp),

         OwnId = crypto:strong_rand_bytes(16),
         OwnFrame = inject_call_with_ucan(Pid, self(), CallerKp, OwnId, ?ISSUER_PROCEDURE,
                                          mint_ucan(IssuerIdentity, Caller, #{})),
         ?assertMatch({ok, #{{text, <<"served">>} := {text, <<"true">>}}}, await_result(OwnFrame, 2_000)),

         Someone = macula_test_identity:node_id(),
         ForSomeoneId = crypto:strong_rand_bytes(16),
         ForSomeoneFrame = inject_call_with_ucan(Pid, self(), CallerKp, ForSomeoneId, ?ISSUER_PROCEDURE,
                                                 mint_ucan(IssuerIdentity, Someone, #{})),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(ForSomeoneFrame, 2_000)),

         NoTokenId = crypto:strong_rand_bytes(16),
         NoTokenFrame = inject_call(Pid, self(), CallerKp, NoTokenId, ?ISSUER_PROCEDURE),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(NoTokenFrame, 2_000)),

         WrongIssuerId = crypto:strong_rand_bytes(16),
         WrongIssuerFrame = inject_call_with_ucan(Pid, self(), CallerKp, WrongIssuerId, ?ISSUER_PROCEDURE,
                                                  mint_ucan(macula_test_identity:key(), Caller, #{})),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(WrongIssuerFrame, 2_000)),

         ExpiredId = crypto:strong_rand_bytes(16),
         ExpiredFrame = inject_call_with_ucan(Pid, self(), CallerKp, ExpiredId, ?ISSUER_PROCEDURE,
                                              mint_ucan(IssuerIdentity, Caller,
                                                        #{exp => erlang:system_time(second) - 60})),
         ?assertMatch({error, #{code := UnauthorizedCode}}, await_result(ExpiredFrame, 2_000)),

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
         Handler = fun(#{{text, <<"tag">>} := Tag}) ->
                       Test ! {handler_ran, Tag},
                       {ok, #{tag => Tag}}
                   end,
         {Pid, CallerKey} = inbound_call_fixture([{<<"probe.verify">>, Handler}]),
         GenuineId = crypto:strong_rand_bytes(16),
         %% A request whose signature does not verify: the same call as
         %% the genuine one, with one byte of its signature flipped.
         Genuine = inject_call_with_payload(Pid, Test, CallerKey, GenuineId,
                                             <<"probe.verify">>, #{tag => genuine}),
         #{request := #{key := Key, tbs := Tbs, signature := Sig}} = Genuine,
         [Flip | Rest] = binary_to_list(Sig),
         Forged = Genuine#{request := #{key => Key, tbs => Tbs,
                                        signature => list_to_binary([Flip bxor 1 | Rest])}},
         %% Not a request at all: no signed request field.
         Unsigned = #{version => 2, frame_type => call},
         Pid ! {macula_peering, frame, Test, Forged},
         Pid ! {macula_peering, frame, Test, Unsigned},
         %% Genuine was already injected by inject_call_with_payload.
         ?assertEqual({ok, #{{text, <<"tag">>} => {text, <<"genuine">>}}}, await_result(Genuine, 2_000)),
         ?assertEqual(timeout, no_reply_in(300)),
         ?assertEqual(timeout, no_reply_in(300)),
         receive
             {handler_ran, Ran} when Ran =/= {text, <<"genuine">>} ->
                 erlang:error({handler_ran_for, Ran})
         after 300 ->
             ok
         end,
         macula_station_link:stop(Pid),
         ok
     end}.
