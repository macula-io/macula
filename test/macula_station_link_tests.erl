%% EUnit tests for `macula_station_link'.
%%
%% A live QUIC handshake against a real V2 station is exercised in
%% the hecate-station Common Test suites. These tests focus on the
%% bookkeeping the client owns end-to-end: seed parsing, CALL frame
%% construction, RESULT/ERROR matching by `call_id', timeout fallback,
%% and disconnection cleanup. Peering is exercised by injecting
%% synthetic frames as if `macula_peering' had delivered them.
-module(macula_station_link_tests).

-include_lib("eunit/include/eunit.hrl").

%% Realm-per-call: tests use the all-zeros realm tag (DHT-internal /
%% realm-agnostic) for every call/subscribe/publish.
-define(REALM, <<0:256>>).

%% #state record layout after the realm-per-call refactor:
%%   1: tag (state)
%%   2: seed
%%   3: identity
%%   4: capabilities
%%   5: alpn
%%   6: connect_timeout_ms
%%   7: peer_pid           <-- patched by these tests via setelement
%%   8: peer_node_id
%%   9: pending
%%  10: subscriptions
%%  11: topic_index
%%  12: publish_seq
-define(PEER_PID_INDEX, 7).

%%------------------------------------------------------------------
%% Seed parsing
%%------------------------------------------------------------------

seed_url_https_with_port_test() ->
    %% Drive parse_seed/1 through a start_link path that fails fast
    %% on the connect attempt (port 1 → connect refused). We only
    %% care that the URL parsed without crashing.
    {ok, _} = application:ensure_all_started(macula),
    Identity = macula_identity:generate(),
    {ok, Pid} = macula_station_link:start_link(#{
        seed     => <<"https://localhost:4433">>,
        connect_timeout_ms => 2000,
        identity => Identity
    }),
    ?assert(is_process_alive(Pid)),
    macula_station_link:stop(Pid),
    ok.

seed_map_test() ->
    {ok, _} = application:ensure_all_started(macula),
    Identity = macula_identity:generate(),
    {ok, Pid} = macula_station_link:start_link(#{
        seed     => #{host => <<"127.0.0.1">>, port => 65000},
        connect_timeout_ms => 2000,
        identity => Identity
    }),
    ?assert(is_process_alive(Pid)),
    macula_station_link:stop(Pid),
    ok.

%%------------------------------------------------------------------
%% RESULT delivery + call_id matching (synthetic frame injection)
%%------------------------------------------------------------------

result_frame_resolves_pending_caller_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         CallerRef = make_ref(),
         Test = self(),
         spawn_link(fun() ->
             R = macula_station_link:call(Pid, ?REALM,
                                            <<"_dht.find_records_by_type">>,
                                            #{type => 1}, 1_000),
             Test ! {CallerRef, R}
         end),
         CallId = receive
             {'$gen_cast', {send_frame, #{frame_type := call,
                                          call_id    := Id}}} ->
                 Id
         after 1_000 ->
             erlang:error({no_send_frame_cast, Pid})
         end,
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type => result,
             call_id    => CallId,
             payload    => [#{tag => <<"hello">>}],
             responded_by => PeerNodeId
         }},
         receive
             {CallerRef, Reply} ->
                 ?assertEqual({ok, [#{tag => <<"hello">>}]}, Reply)
         after 2_000 ->
             erlang:error(no_caller_reply)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% ERROR frame surfaces as {error, {call_error, Code, Name}}
%%------------------------------------------------------------------

error_frame_surfaces_to_caller_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         CallerRef = make_ref(),
         Test = self(),
         spawn_link(fun() ->
             R = macula_station_link:call(Pid, ?REALM,
                                            <<"_dht.find_records_by_type">>,
                                            #{type => 1}, 1_000),
             Test ! {CallerRef, R}
         end),
         CallId = receive
             {'$gen_cast', {send_frame, #{frame_type := call,
                                          call_id    := Id}}} ->
                 Id
         after 1_000 ->
             erlang:error(no_send_frame_cast)
         end,
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type  => error,
             call_id     => CallId,
             code        => 16#01,
             name        => <<"unknown_next_peer">>,
             reported_by => PeerNodeId
         }},
         receive
             {CallerRef, Reply} ->
                 ?assertMatch({error, {call_error, 16#01, _}}, Reply)
         after 2_000 ->
             erlang:error(no_caller_reply)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% Disconnect drains pending callers
%%------------------------------------------------------------------

disconnect_fails_pending_callers_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         CallerRef = make_ref(),
         Test = self(),
         spawn_link(fun() ->
             R = macula_station_link:call(Pid, ?REALM,
                                            <<"_dht.find_records_by_type">>,
                                            #{type => 1}, 5_000),
             Test ! {CallerRef, R}
         end),
         receive
             {'$gen_cast', {send_frame, #{frame_type := call}}} -> ok
         after 1_000 ->
             erlang:error(no_send_frame_cast)
         end,
         Pid ! {macula_peering, disconnected, FakePeer, peer_closed},
         receive
             {CallerRef, Reply} ->
                 ?assertMatch({error, {disconnected, peer_closed}}, Reply)
         after 2_000 ->
             erlang:error(no_caller_reply)
         end,
         ok
     end}.

%%------------------------------------------------------------------
%% Timeout if no reply arrives
%%------------------------------------------------------------------

call_times_out_when_no_reply_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         R = macula_station_link:call(Pid, ?REALM,
                                        <<"_dht.find_records_by_type">>,
                                        #{type => 1}, 200),
         ?assertEqual({error, timeout}, R),
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% put_record/2 success path: classifies RESULT(ok) as ok
%%------------------------------------------------------------------

put_record_ok_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         CallerRef = make_ref(),
         Test = self(),
         Record = macula_record:sign(
                    macula_record:node_record(macula_identity:public(Identity),
                                              [], 0),
                    Identity),
         spawn_link(fun() ->
             R = macula_station_link:put_record(Pid, Record, 1_000),
             Test ! {CallerRef, R}
         end),
         CallId = receive
             {'$gen_cast', {send_frame, #{frame_type := call,
                                          procedure  := <<"_dht.put_record">>,
                                          call_id    := Id}}} ->
                 Id
         after 1_000 ->
             erlang:error(no_put_record_cast)
         end,
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type => result,
             call_id    => CallId,
             payload    => ok,
             responded_by => PeerNodeId
         }},
         receive
             {CallerRef, Reply} ->
                 ?assertEqual(ok, Reply)
         after 2_000 ->
             erlang:error(no_caller_reply)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% put_record/2 unexpected reply surfaces as {error, {unexpected_reply, _}}
%%------------------------------------------------------------------

put_record_unexpected_reply_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         CallerRef = make_ref(),
         Test = self(),
         Record = macula_record:sign(
                    macula_record:node_record(macula_identity:public(Identity),
                                              [], 0),
                    Identity),
         spawn_link(fun() ->
             R = macula_station_link:put_record(Pid, Record, 1_000),
             Test ! {CallerRef, R}
         end),
         CallId = receive
             {'$gen_cast', {send_frame, #{frame_type := call,
                                          procedure  := <<"_dht.put_record">>,
                                          call_id    := Id}}} -> Id
         after 1_000 -> erlang:error(no_put_record_cast)
         end,
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type   => result,
             call_id      => CallId,
             payload      => #{some => garbage},
             responded_by => PeerNodeId
         }},
         receive
             {CallerRef, Reply} ->
                 ?assertMatch({error, {unexpected_reply, _}}, Reply)
         after 2_000 -> erlang:error(no_reply)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% find_record/2 success path: classifies a signed record map
%%------------------------------------------------------------------

find_record_ok_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         CallerRef = make_ref(),
         Test = self(),
         Key = crypto:strong_rand_bytes(32),
         spawn_link(fun() ->
             R = macula_station_link:find_record(Pid, Key, 1_000),
             Test ! {CallerRef, R}
         end),
         CallId = receive
             {'$gen_cast', {send_frame, #{frame_type := call,
                                          procedure  := <<"_dht.find_record">>,
                                          call_id    := Id,
                                          payload    := #{key := K}}}}
               when K =:= Key -> Id
         after 1_000 ->
             erlang:error(no_find_record_cast)
         end,
         %% `signature' (not legacy `sig') matches the wire-canonical
         %% record shape — see commit 0b35f80 (4.2.2).
         FakeRecord = #{type => 1, payload => #{}, signature => <<>>},
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type   => result,
             call_id      => CallId,
             payload      => FakeRecord,
             responded_by => PeerNodeId
         }},
         receive
             {CallerRef, Reply} ->
                 ?assertEqual({ok, FakeRecord}, Reply)
         after 2_000 -> erlang:error(no_reply)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% find_record/2 surfaces RESULT(not_found) as {error, not_found}
%%------------------------------------------------------------------

find_record_not_found_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         CallerRef = make_ref(),
         Test = self(),
         Key = crypto:strong_rand_bytes(32),
         spawn_link(fun() ->
             R = macula_station_link:find_record(Pid, Key, 1_000),
             Test ! {CallerRef, R}
         end),
         CallId = receive
             {'$gen_cast', {send_frame, #{frame_type := call,
                                          procedure  := <<"_dht.find_record">>,
                                          call_id    := Id}}} -> Id
         after 1_000 -> erlang:error(no_find_record_cast)
         end,
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type   => result,
             call_id      => CallId,
             payload      => not_found,
             responded_by => PeerNodeId
         }},
         receive
             {CallerRef, Reply} ->
                 ?assertEqual({error, not_found}, Reply)
         after 2_000 -> erlang:error(no_reply)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% subscribe/4 sends a SUBSCRIBE frame
%%------------------------------------------------------------------

subscribe_sends_frame_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
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
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         Topic = <<"_mesh.station.announced_v1">>,
         {ok, SubRef} = macula_station_link:subscribe(
                          Pid, ?REALM, Topic, self()),
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame)
         end,
         PublisherKey = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type    => event,
             topic         => Topic,
             realm         => ?REALM,
             publisher     => PublisherKey,
             seq           => 42,
             payload       => #{hello => <<"world">>},
             delivered_via => direct
         }},
         receive
             {macula_event, R, T, P, Meta} ->
                 ?assertEqual(SubRef, R),
                 ?assertEqual(Topic, T),
                 ?assertEqual(#{hello => <<"world">>}, P),
                 ?assertEqual(?REALM, maps:get(realm, Meta)),
                 ?assertEqual(PublisherKey, maps:get(publisher, Meta)),
                 ?assertEqual(42, maps:get(seq, Meta))
         after 2_000 -> erlang:error(no_event_delivered)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% Inbound EVENT with publisher_sig — verified; lenient vs strict
%% (pubsub Phase 2 step 4)
%%------------------------------------------------------------------

event_publisher_sig_verify_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         application:unset_env(macula, pubsub_strict_publisher_sig),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => macula_identity:generate()
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         Topic = <<"io.macula/x/y/v1">>,
         {ok, SubRef} = macula_station_link:subscribe(Pid, ?REALM, Topic, self()),
         receive {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame) end,

         PubKp = macula_identity:generate(),
         MkEvent = fun(Seq, Payload) ->
             macula_frame:event(#{
                 topic => Topic, realm => ?REALM,
                 publisher => macula_identity:public(PubKp),
                 seq => Seq, payload => Payload, delivered_via => direct})
         end,

         %% 1. Valid publisher_sig → delivered.
         Pid ! {macula_peering, frame, FakePeer,
                macula_frame:sign_publisher(MkEvent(1, ok1), PubKp)},
         receive {macula_event, SubRef, Topic, ok1, _} -> ok
         after 2_000 -> erlang:error(valid_sig_event_not_delivered) end,

         %% 2. Tampered publisher_sig, lenient (default) → still delivered.
         Bad = (macula_frame:sign_publisher(MkEvent(2, ok2), PubKp))#{
                 payload => tampered},
         Pid ! {macula_peering, frame, FakePeer, Bad},
         receive {macula_event, SubRef, Topic, tampered, _} -> ok
         after 2_000 -> erlang:error(lenient_bad_sig_event_not_delivered) end,

         %% 3. Tampered publisher_sig, strict → NOT delivered.
         application:set_env(macula, pubsub_strict_publisher_sig, true),
         Bad3 = (macula_frame:sign_publisher(MkEvent(3, ok3), PubKp))#{
                  payload => tampered3},
         Pid ! {macula_peering, frame, FakePeer, Bad3},
         receive {macula_event, SubRef, Topic, tampered3, _} ->
             erlang:error(strict_bad_sig_event_was_delivered)
         after 800 -> ok end,
         application:unset_env(macula, pubsub_strict_publisher_sig),

         macula_station_link:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% EVENT for a different realm is NOT delivered (realm-scoped index)
%%------------------------------------------------------------------

event_in_other_realm_not_delivered_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
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
         PublisherKey = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type    => event,
             topic         => Topic,
             realm         => RealmB,
             publisher     => PublisherKey,
             seq           => 1,
             payload       => wrong_realm,
             delivered_via => direct
         }},
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
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         %% publish/4 requires full handshake (peer_node_id set). The
         %% connected info message races against attempt_connect setting
         %% peer_pid to a real worker; bypass by force-injecting both
         %% fields. peer_node_id is record element 8 (right after
         %% peer_pid at 7).
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         Topic = <<"weather.measured_v1">>,
         ok = macula_station_link:publish(Pid, ?REALM, Topic,
                                           #{temp => 20}),
         Frame1 = receive
             {'$gen_cast', {send_frame, #{frame_type := publish} = F}} -> F
         after 1_000 -> erlang:error(no_publish_frame_1)
         end,
         ?assertEqual(Topic, maps:get(topic, Frame1)),
         ?assertEqual(?REALM, maps:get(realm, Frame1)),
         ?assertEqual(0, maps:get(seq, Frame1)),
         ?assertEqual(#{temp => 20}, maps:get(payload, Frame1)),
         ok = macula_station_link:publish(Pid, ?REALM, Topic,
                                           #{temp => 21}),
         Frame2 = receive
             {'$gen_cast', {send_frame, #{frame_type := publish} = F2}} -> F2
         after 1_000 -> erlang:error(no_publish_frame_2)
         end,
         ?assertEqual(1, maps:get(seq, Frame2)),
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
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
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
         PublisherKey = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type    => event,
             topic         => Topic,
             realm         => ?REALM,
             publisher     => PublisherKey,
             seq           => 1,
             payload       => post_unsubscribe,
             delivered_via => direct
         }},
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
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
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
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
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
             {macula_event_gone, R, Reason} ->
                 ?assertEqual(SubRef, R),
                 ?assertMatch({disconnected, peer_closed}, Reason)
         after 2_000 -> erlang:error(no_event_gone)
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
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
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

%%==================================================================
%% advertise/4 + inbound CALL dispatch
%%==================================================================

advertise_sends_frame_when_connected_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         Procedure = <<"_realm.membership.join_with_token_v1">>,
         Handler = fun(_Args) -> {ok, #{joined => true}} end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         AdvertiserPub = macula_identity:public(Identity),
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := advertise,
                              realm      := R,
                              procedure  := P,
                              advertiser := A}}} ->
                 ?assertEqual(?REALM, R),
                 ?assertEqual(Procedure, P),
                 ?assertEqual(AdvertiserPub, A)
         after 1_000 ->
             erlang:error(advertise_frame_not_sent)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

advertise_before_connect_drains_on_connected_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
         Procedure = <<"_realm.membership.join_with_token_v1">>,
         Handler = fun(_Args) -> ok end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         %% Pre-connect: nothing on the wire yet.
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
             {'$gen_cast', {send_frame,
                            #{frame_type := advertise,
                              procedure  := P}}} ->
                 ?assertEqual(Procedure, P)
         after 1_000 ->
             erlang:error(advertise_frame_not_drained)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

inbound_call_dispatches_to_handler_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         Procedure = <<"_realm.membership.join_with_token_v1">>,
         %% Handler asserts on input + returns canonical reply.
         Handler = fun(#{token := <<"abc">>}) -> {ok, #{member_id => 42}} end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         flush_send_frame_casts(),
         CallId = <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16>>,
         CallerPub = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => Procedure,
             payload     => #{token => <<"abc">>},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }},
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

inbound_call_unknown_procedure_returns_error_frame_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         CallId = <<2:128>>,
         CallerPub = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => <<"_no.such.procedure">>,
             payload     => #{},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }},
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
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         Procedure = <<"_test.crash">>,
         Handler = fun(_Args) -> error(deliberate) end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         flush_send_frame_casts(),
         CallId = <<3:128>>,
         CallerPub = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => Procedure,
             payload     => #{},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }},
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
    %% the formatted Reason in `detail'. Handlers that need a
    %% specific BOLT#4 code can crash with a tagged error or use
    %% the dedicated frame builders.
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         Procedure = <<"_test.app_error">>,
         Handler = fun(_Args) -> {error, invalid_token} end,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure, Handler),
         flush_send_frame_casts(),
         CallId = <<4:128>>,
         CallerPub = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => Procedure,
             payload     => #{},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }},
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

unadvertise_clears_handler_and_sends_frame_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
             setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
         end),
         Procedure = <<"_test.unadv">>,
         ok = macula_station_link:advertise(Pid, ?REALM, Procedure,
                                            fun(_) -> ok end),
         flush_send_frame_casts(),
         ok = macula_station_link:unadvertise(Pid, ?REALM, Procedure),
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := unadvertise,
                              procedure  := P}}} ->
                 ?assertEqual(Procedure, P)
         after 1_000 ->
             erlang:error(no_unadvertise_frame_sent)
         end,
         %% After unadvertise, an inbound CALL must produce
         %% unknown_next_peer (handler was cleared).
         CallId = <<5:128>>,
         CallerPub = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type  => call,
             call_id     => CallId,
             realm       => ?REALM,
             procedure   => Procedure,
             payload     => #{},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }},
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := error,
                              code       := Code}}} ->
                 ?assertEqual(16#01, Code)
         after 1_000 ->
             erlang:error(no_error_frame_sent)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

flush_send_frame_casts() ->
    receive
        {'$gen_cast', {send_frame, _}} -> flush_send_frame_casts()
    after 0 -> ok
    end.

%%==================================================================
%% Streaming RPC (SDK 3.17+, A4)
%%==================================================================

%% Boilerplate: start a link, force-inject peer_pid + peer_node_id,
%% return {Pid, FakePeer, PeerNodeId}. The test pid acts as the wire
%% — outbound frames from the link arrive as `{'$gen_cast', {send_frame, _}}`
%% and inbound frames are synthesised as `{macula_peering, frame, ...}`.
setup_link_for_streams() ->
    {ok, _} = application:ensure_all_started(macula),
    Identity = macula_identity:generate(),
    {ok, Pid} = macula_station_link:start_link(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000,
        identity => Identity
    }),
    FakePeer = self(),
    PeerNodeId = macula_identity:public(macula_identity:generate()),
    _ = sys:replace_state(Pid, fun(S) ->
        S2 = setelement(?PEER_PID_INDEX,     S, FakePeer),
        setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
    end),
    {Pid, FakePeer, PeerNodeId}.

%% -- call_stream sends a STREAM_OPEN frame ------------------------

call_stream_emits_stream_open_frame_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, _Peer, _PeerNodeId} = setup_link_for_streams(),
         {ok, StreamPid} = macula_station_link:call_stream(
                             Pid, ?REALM, <<"foo.count">>,
                             #{n => 5}, #{mode => server_stream}),
         ?assert(is_pid(StreamPid)),
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := stream_open,
                              procedure  := P, mode := M,
                              args       := A,
                              stream_id  := Sid}}} ->
                 ?assertEqual(<<"foo.count">>, P),
                 ?assertEqual(server_stream,   M),
                 ?assertEqual(#{n => 5},       A),
                 ?assertEqual(16, byte_size(Sid))
         after 1_000 ->
             erlang:error(no_stream_open_frame)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%% -- call_stream gates on peer_node_id ----------------------------

call_stream_returns_not_connected_before_handshake_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             connect_timeout_ms => 2000,
             identity => Identity
         }),
         %% No peer_node_id injection — handshake "incomplete".
         ?assertEqual({error, not_connected},
                      macula_station_link:call_stream(
                        Pid, ?REALM, <<"foo">>, #{}, #{})),
         macula_station_link:stop(Pid),
         ok
     end}.

%% -- inbound STREAM_DATA delivers chunk to local stream pid -------

stream_data_delivers_chunk_to_stream_pid_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, FakePeer, _PeerNodeId} = setup_link_for_streams(),
         {ok, StreamPid} = macula_station_link:call_stream(
                             Pid, ?REALM, <<"foo">>, #{}, #{}),
         %% Drain the outbound STREAM_OPEN cast and capture the Sid.
         Sid = receive
             {'$gen_cast', {send_frame,
                            #{frame_type := stream_open,
                              stream_id  := S}}} -> S
         after 1_000 -> erlang:error(no_stream_open)
         end,
         %% Inject inbound STREAM_DATA.
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type => stream_data,
             stream_id  => Sid,
             seq        => 0,
             encoding   => raw,
             body       => <<"chunk-bytes">>
         }},
         %% recv unblocks with the chunk.
         ?assertEqual({chunk, <<"chunk-bytes">>},
                      macula_stream:recv(StreamPid, 1_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

%% -- inbound STREAM_REPLY surfaces terminal payload ---------------

stream_reply_surfaces_terminal_payload_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, FakePeer, PeerNodeId} = setup_link_for_streams(),
         {ok, StreamPid} = macula_station_link:call_stream(
                             Pid, ?REALM, <<"foo">>, #{}, #{mode => bidi}),
         Sid = receive
             {'$gen_cast', {send_frame,
                            #{frame_type := stream_open,
                              stream_id  := S}}} -> S
         after 1_000 -> erlang:error(no_stream_open)
         end,
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type   => stream_reply,
             stream_id    => Sid,
             payload      => #{count => 12},
             responded_by => PeerNodeId
         }},
         ?assertEqual({ok, #{count => 12}},
                      macula_stream:await_reply(StreamPid, 1_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

%% -- inbound STREAM_ERROR aborts local stream --------------------

stream_error_aborts_local_stream_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, FakePeer, _PeerNodeId} = setup_link_for_streams(),
         {ok, StreamPid} = macula_station_link:call_stream(
                             Pid, ?REALM, <<"foo">>, #{}, #{}),
         Sid = receive
             {'$gen_cast', {send_frame,
                            #{frame_type := stream_open,
                              stream_id  := S}}} -> S
         after 1_000 -> erlang:error(no_stream_open)
         end,
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type => stream_error,
             stream_id  => Sid,
             code       => <<"deadline_exceeded">>,
             message    => <<"server too slow">>
         }},
         %% recv waiters surface the error.
         ?assertEqual({error, {<<"deadline_exceeded">>, <<"server too slow">>}},
                      macula_stream:recv(StreamPid, 1_000)),
         macula_station_link:stop(Pid),
         ok
     end}.

%% -- inbound STREAM_OPEN unknown procedure → STREAM_ERROR --------

inbound_stream_open_unknown_procedure_returns_error_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, FakePeer, _PeerNodeId} = setup_link_for_streams(),
         CallerPub = macula_identity:public(macula_identity:generate()),
         Sid = crypto:strong_rand_bytes(16),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type  => stream_open,
             stream_id   => Sid,
             procedure   => <<"unknown.proc">>,
             realm       => ?REALM,
             mode        => server_stream,
             args        => #{},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }},
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := stream_error,
                              stream_id  := S2,
                              code       := Code}}} ->
                 ?assertEqual(Sid, S2),
                 ?assertEqual(<<"not_found">>, Code)
         after 1_000 ->
             erlang:error(no_stream_error_emitted)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%% -- advertise_stream emits an ADVERTISE frame -------------------

advertise_stream_emits_advertise_frame_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, _Peer, _PeerNodeId} = setup_link_for_streams(),
         Procedure = <<"foo.echo">>,
         Handler = fun(_Stream, _Args) -> ok end,
         ok = macula_station_link:advertise_stream(
                Pid, ?REALM, Procedure, server_stream, Handler),
         receive
             {'$gen_cast', {send_frame,
                            #{frame_type := advertise,
                              procedure  := P}}} ->
                 ?assertEqual(Procedure, P)
         after 1_000 ->
             erlang:error(no_advertise_frame_sent)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%% -- inbound STREAM_OPEN dispatches advertised handler -----------

inbound_stream_open_invokes_handler_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, FakePeer, _PeerNodeId} = setup_link_for_streams(),
         Test = self(),
         Procedure = <<"foo.invoked">>,
         Handler = fun(_Stream, Args) ->
             Test ! {handler_invoked, Args},
             ok
         end,
         ok = macula_station_link:advertise_stream(
                Pid, ?REALM, Procedure, server_stream, Handler),
         flush_send_frame_casts(),
         CallerPub = macula_identity:public(macula_identity:generate()),
         Sid = crypto:strong_rand_bytes(16),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type  => stream_open,
             stream_id   => Sid,
             procedure   => Procedure,
             realm       => ?REALM,
             mode        => server_stream,
             args        => #{n => 7},
             deadline_ms => erlang:system_time(millisecond) + 5_000,
             caller      => CallerPub
         }},
         receive
             {handler_invoked, Args} ->
                 ?assertEqual(#{n => 7}, Args)
         after 1_000 ->
             erlang:error(handler_not_invoked)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

%% -- disconnect aborts open streams -------------------------------

disconnect_aborts_open_streams_test_() ->
    {timeout, 5,
     fun() ->
         {Pid, FakePeer, _PeerNodeId} = setup_link_for_streams(),
         {ok, StreamPid} = macula_station_link:call_stream(
                             Pid, ?REALM, <<"foo">>, #{}, #{mode => bidi}),
         %% Drain the STREAM_OPEN cast.
         receive {'$gen_cast', {send_frame, _}} -> ok after 1_000 -> ok end,
         %% Synthesise a peering disconnect; the link's
         %% `fail_all_pending/2' aborts every open stream. The
         %% stream pid stays alive (its owner is still up) but
         %% surfaces a `disconnected' error to readers and reply
         %% waiters, then closes both sides.
         Pid ! {macula_peering, disconnected, FakePeer, peer_gone},
         %% Wait for the abort to settle by polling info/1 until
         %% the closed flags trip. Bounded retry so a real failure
         %% surfaces as a timeout.
         wait_until_closed(StreamPid, 20),
         ?assertMatch({error, {<<"disconnected">>, _}},
                      macula_stream:await_reply(StreamPid, 1_000)),
         ok
     end}.

wait_until_closed(_StreamPid, 0) ->
    erlang:error(stream_did_not_close);
wait_until_closed(StreamPid, N) ->
    case macula_stream:info(StreamPid) of
        #{closed_recv := true, closed_send := true} -> ok;
        _ ->
            timer:sleep(50),
            wait_until_closed(StreamPid, N - 1)
    end.

%%------------------------------------------------------------------
%% Liveness probe — bounded zombie-connection detection
%%
%% These tests drive the `liveness_tick' info message directly rather
%% than waiting `?LIVENESS_INTERVAL_MS' wallclock time. They cover:
%%   1. A probe CALL goes out on the first tick after connect.
%%   2. A matching ERROR reply clears the outstanding slot WITHOUT
%%      leaking to user `pending' machinery (i.e. no spurious
%%      caller-reply).
%%   3. After `?LIVENESS_MAX_MISSES' consecutive ticks with no reply,
%%      the link issues `macula_peering:close(PeerPid, ...)' which
%%      our FakePeer (the test pid) receives as a gen_statem cast.
%%------------------------------------------------------------------

liveness_tick_emits_probe_call_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {Pid, _FakePeer, _PeerNodeId} = start_connected_link(),
         %% Drive the tick.
         Pid ! liveness_tick,
         %% A CALL frame for `_macula.ping' must hit the test
         %% mailbox as a send_frame cast.
         CallId = receive
             {'$gen_cast', {send_frame,
                            #{frame_type := call,
                              procedure  := <<"_macula.ping">>,
                              call_id    := Id}}} -> Id
         after 1_000 ->
             erlang:error(no_probe_call_emitted)
         end,
         ?assertEqual(16, byte_size(CallId)),
         macula_station_link:stop(Pid),
         ok
     end}.

liveness_probe_reply_clears_outstanding_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {Pid, FakePeer, PeerNodeId} = start_connected_link(),
         Pid ! liveness_tick,
         CallId = receive
             {'$gen_cast', {send_frame, #{frame_type := call,
                                          call_id    := Id}}} -> Id
         after 1_000 -> erlang:error(no_probe_call_emitted)
         end,
         %% Synthesise an ERROR reply (`unknown_next_peer' shape) for
         %% the probe call_id — what a real station emits when it
         %% has no handler for `_macula.ping'.
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type   => error,
             call_id      => CallId,
             code         => 16#01,
             name         => unknown_next_peer,
             responded_by => PeerNodeId
         }},
         %% Drive another tick — outstanding must have been cleared
         %% (else this would be miss #1 → miss #2 → close).
         Pid ! liveness_tick,
         %% Should see a fresh probe go out, not a close.
         receive
             {'$gen_cast', {send_frame, #{frame_type := call,
                                          procedure  := <<"_macula.ping">>}}} ->
                 ok;
             {'$gen_statem', _, {close, _}} ->
                 erlang:error(unexpected_close_after_reply)
         after 1_000 ->
             erlang:error(no_second_probe)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

liveness_consecutive_misses_close_peer_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {Pid, _FakePeer, _PeerNodeId} = start_connected_link(),
         %% Tick #1: outstanding=undefined → sends probe, sets outstanding.
         Pid ! liveness_tick,
         _ = consume_probe(Pid),
         %% Tick #2: outstanding set (no reply received) → miss=1, then
         %% send another probe (still under ?LIVENESS_MAX_MISSES=2).
         Pid ! liveness_tick,
         _ = consume_probe(Pid),
         %% Tick #3: outstanding set again, miss=2 → triggers close.
         Pid ! liveness_tick,
         %% We expect a `{close, app_liveness_lost}' cast to our
         %% FakePeer (the test pid).
         receive
             {'$gen_statem', _, {close, app_liveness_lost}} -> ok;
             {'$gen_cast',   {close, app_liveness_lost}}    -> ok
         after 1_000 ->
             erlang:error(no_close_after_misses)
         end,
         macula_station_link:stop(Pid),
         ok
     end}.

start_connected_link() ->
    Identity = macula_identity:generate(),
    {ok, Pid} = macula_station_link:start_link(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000,
        identity => Identity
    }),
    FakePeer = self(),
    PeerNodeId = macula_identity:public(macula_identity:generate()),
    _ = sys:replace_state(Pid, fun(S) ->
        S2 = setelement(?PEER_PID_INDEX, S, FakePeer),
        setelement(?PEER_PID_INDEX + 1, S2, PeerNodeId)
    end),
    {Pid, FakePeer, PeerNodeId}.

consume_probe(_Pid) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := call,
                                     procedure  := <<"_macula.ping">>,
                                     call_id    := Id}}} -> Id
    after 1_000 ->
        erlang:error(no_probe_call_emitted)
    end.
