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
         FakeRecord = #{type => 1, payload => #{}, sig => <<>>},
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
%% EVENT for a different realm is NOT delivered (realm-scoped index)
%%------------------------------------------------------------------

event_in_other_realm_not_delivered_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
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

flush_send_frame_casts() ->
    receive
        {'$gen_cast', {send_frame, _}} -> flush_send_frame_casts()
    after 0 -> ok
    end.
