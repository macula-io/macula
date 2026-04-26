%% EUnit tests for `macula_station_client'.
%%
%% A live QUIC handshake against a real V2 station is exercised in
%% the hecate-station Common Test suites. These tests focus on the
%% bookkeeping the client owns end-to-end: seed parsing, CALL frame
%% construction, RESULT/ERROR matching by `call_id', timeout fallback,
%% and disconnection cleanup. Peering is exercised by injecting
%% synthetic frames as if `macula_peering' had delivered them.
-module(macula_station_client_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Seed parsing
%%------------------------------------------------------------------

seed_url_https_with_port_test() ->
    %% Drive parse_seed/1 through a start_link path that fails fast
    %% on the connect attempt (port 1 → connect refused). We only
    %% care that the URL parsed without crashing.
    {ok, _} = application:ensure_all_started(macula),
    Identity = macula_identity:generate(),
    {ok, Pid} = macula_station_client:start_link(#{
        seed     => <<"https://localhost:4433">>,
        identity => Identity
    }),
    ?assert(is_process_alive(Pid)),
    macula_station_client:stop(Pid),
    ok.

seed_map_test() ->
    {ok, _} = application:ensure_all_started(macula),
    Identity = macula_identity:generate(),
    {ok, Pid} = macula_station_client:start_link(#{
        seed     => #{host => <<"127.0.0.1">>, port => 65000},
        identity => Identity
    }),
    ?assert(is_process_alive(Pid)),
    macula_station_client:stop(Pid),
    ok.

%%------------------------------------------------------------------
%% RESULT delivery + call_id matching (synthetic frame injection)
%%------------------------------------------------------------------

result_frame_resolves_pending_caller_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         %% Force the gen_server into "connected" state by spoofing the
         %% peer_pid + peer_node_id via the public message-box. Easier
         %% than threading mock plumbing through start_link.
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         %% Inject the peer pid by mutating gen_server state through
         %% sys:replace_state — restricted to test code.
         _ = sys:replace_state(Pid, fun(S) ->
             %% #state record layout: tag, seed, identity, realm,
             %% capabilities, alpn, connect_timeout_ms, peer_pid (#8),
             %% peer_node_id, pending. Set peer_pid.
             setelement(8, S, FakePeer)
         end),
         %% Spawn caller. send_frame goes to FakePeer (us) — capture the
         %% CALL id then inject the matching RESULT.
         CallerRef = make_ref(),
         Test = self(),
         spawn_link(fun() ->
             R = macula_station_client:call(Pid,
                                            <<"_dht.find_records_by_type">>,
                                            #{type => 1}, 1_000),
             Test ! {CallerRef, R}
         end),
         %% Capture the cast that send_frame sends to FakePeer (us).
         CallId = receive
             {'$gen_cast', {send_frame, #{frame_type := call,
                                          call_id    := Id}}} ->
                 Id
         after 1_000 ->
             erlang:error({no_send_frame_cast, Pid})
         end,
         %% Reply with a RESULT frame (no signature needed — the client
         %% module does not verify).
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
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         CallerRef = make_ref(),
         Test = self(),
         spawn_link(fun() ->
             R = macula_station_client:call(Pid, <<"_dht.find_records_by_type">>,
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
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         CallerRef = make_ref(),
         Test = self(),
         spawn_link(fun() ->
             R = macula_station_client:call(Pid, <<"_dht.find_records_by_type">>,
                                            #{type => 1}, 5_000),
             Test ! {CallerRef, R}
         end),
         %% Consume the send_frame cast so the gen_server progresses.
         receive
             {'$gen_cast', {send_frame, #{frame_type := call}}} -> ok
         after 1_000 ->
             erlang:error(no_send_frame_cast)
         end,
         %% Now drop the peer.
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         %% Issue a call with a tiny deadline and never inject a reply.
         R = macula_station_client:call(Pid,
                                        <<"_dht.find_records_by_type">>,
                                        #{type => 1}, 200),
         ?assertEqual({error, timeout}, R),
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         CallerRef = make_ref(),
         Test = self(),
         %% A signed node_record makes a realistic put_record argument.
         Record = macula_record:sign(
                    macula_record:node_record(macula_identity:public(Identity),
                                              [], 0),
                    Identity),
         spawn_link(fun() ->
             R = macula_station_client:put_record(Pid, Record, 1_000),
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
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         CallerRef = make_ref(),
         Test = self(),
         Record = macula_record:sign(
                    macula_record:node_record(macula_identity:public(Identity),
                                              [], 0),
                    Identity),
         spawn_link(fun() ->
             R = macula_station_client:put_record(Pid, Record, 1_000),
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
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         CallerRef = make_ref(),
         Test = self(),
         Key = crypto:strong_rand_bytes(32),
         spawn_link(fun() ->
             R = macula_station_client:find_record(Pid, Key, 1_000),
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
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         CallerRef = make_ref(),
         Test = self(),
         Key = crypto:strong_rand_bytes(32),
         spawn_link(fun() ->
             R = macula_station_client:find_record(Pid, Key, 1_000),
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
         macula_station_client:stop(Pid),
         ok
     end}.

%%------------------------------------------------------------------
%% subscribe/3 sends a SUBSCRIBE frame
%%------------------------------------------------------------------

subscribe_sends_frame_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         {ok, SubRef} = macula_station_client:subscribe(
                          Pid, <<"_mesh.station.announced_v1">>, self()),
         ?assert(is_reference(SubRef)),
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe,
                                          topic := <<"_mesh.station.announced_v1">>}}} ->
                 ok
         after 1_000 ->
             erlang:error(no_subscribe_frame)
         end,
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         Topic = <<"_mesh.station.announced_v1">>,
         {ok, SubRef} = macula_station_client:subscribe(Pid, Topic, self()),
         %% Drain the outbound subscribe cast.
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame)
         end,
         %% Inject an EVENT frame.
         PublisherKey = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type    => event,
             topic         => Topic,
             realm         => <<0:256>>,
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
                 ?assertEqual(PublisherKey, maps:get(publisher, Meta)),
                 ?assertEqual(42, maps:get(seq, Meta))
         after 2_000 -> erlang:error(no_event_delivered)
         end,
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         Topic = <<"_mesh.station.announced_v1">>,
         {ok, SubRef} = macula_station_client:subscribe(Pid, Topic, self()),
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame)
         end,
         ok = macula_station_client:unsubscribe(Pid, SubRef),
         receive
             {'$gen_cast', {send_frame, #{frame_type := unsubscribe,
                                          topic := T}}} ->
                 ?assertEqual(Topic, T)
         after 1_000 ->
             erlang:error(no_unsubscribe_frame)
         end,
         %% After unsubscribe, EVENT frames must not be delivered.
         PublisherKey = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, frame, FakePeer, #{
             frame_type    => event,
             topic         => Topic,
             realm         => <<0:256>>,
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
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         %% Spawn a subscriber that exits immediately after subscribing
         %% from its own pid.
         Test = self(),
         Sub = spawn(fun() ->
             {ok, R} = macula_station_client:subscribe(
                         Pid, <<"_mesh.station.announced_v1">>, self()),
             Test ! {sub_started, self(), R}
         end),
         receive
             {sub_started, Sub, _SubRef} -> ok
         after 1_000 -> erlang:error(subscriber_did_not_subscribe)
         end,
         %% Drain the outbound subscribe cast.
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe}}} -> ok
         after 1_000 -> erlang:error(no_subscribe_frame)
         end,
         %% Subscriber has exited (returned from its fun). The client
         %% should observe DOWN and emit a best-effort UNSUBSCRIBE frame.
         receive
             {'$gen_cast', {send_frame, #{frame_type := unsubscribe}}} -> ok
         after 1_000 -> erlang:error(no_cleanup_unsubscribe)
         end,
         macula_station_client:stop(Pid),
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
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         {ok, SubRef} = macula_station_client:subscribe(
                          Pid, <<"_mesh.station.announced_v1">>, self()),
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
%% subscribe/3 before connect — frame drains on connected event
%%------------------------------------------------------------------

subscribe_before_connect_drains_on_connected_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         %% Drain any leftover send_frame casts from prior tests in
         %% this module — eunit may share the test process mailbox
         %% across cases.
         flush_send_frame_casts(),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_client:start_link(#{
             seed     => #{host => <<"127.0.0.1">>, port => 1},
             identity => Identity
         }),
         %% Subscribe BEFORE injecting the connected event. This used
         %% to return {error, not_connected}; in 3.10.2+ it stores
         %% the subscription and returns {ok, SubRef}.
         {ok, SubRef} = macula_station_client:subscribe(
                          Pid, <<"_mesh.station.announced_v1">>, self()),
         ?assert(is_reference(SubRef)),
         %% No frame on the wire yet — peer not connected.
         receive
             {'$gen_cast', {send_frame, _}} ->
                 erlang:error(premature_send_frame)
         after 200 -> ok
         end,
         %% Now mark connected; the drain handler must send the
         %% pending SUBSCRIBE frame.
         FakePeer = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) -> setelement(8, S, FakePeer) end),
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         receive
             {'$gen_cast', {send_frame, #{frame_type := subscribe,
                                          topic := T}}} ->
                 ?assertEqual(<<"_mesh.station.announced_v1">>, T)
         after 1_000 ->
             erlang:error(subscribe_frame_not_drained)
         end,
         macula_station_client:stop(Pid),
         ok
     end}.

flush_send_frame_casts() ->
    receive
        {'$gen_cast', {send_frame, _}} -> flush_send_frame_casts()
    after 0 -> ok
    end.
