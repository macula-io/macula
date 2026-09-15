%%%-------------------------------------------------------------------
%%% @doc Tests that macula_client:unsubscribe/2 takes a subscription off
%%% the wire, not only out of the pool's own bookkeeping.
%%%
%%% The pool subscribes once per (realm, topic) on each of its station
%%% links, and again on a link it respawns. When the last local subscriber
%%% of a (realm, topic) unsubscribes, each link that carried the SUBSCRIBE
%%% sends UNSUBSCRIBE; while another local subscriber remains, none does.
%%% A busy link does not hold up unsubscribe/2, and an UNSUBSCRIBE and a
%%% SUBSCRIBE that follow each other reach the link in that order.
%%%
%%% The pool's links are real macula_station_link workers against an
%%% unreachable seed. A fake peer planted in a link receives the frames the
%%% link sends, as in macula_station_link_tests.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_client_unsubscribe_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(SEED, #{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<1:256>>}).
-define(TOPIC, <<"unsub.test_v1">>).
%% The peer_pid and peer_node_id fields of macula_station_link's state, by name.
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(FRAME_MS, 1_000).
-define(QUIET_MS, 300).
%% Well under the 5 s a call to a busy link would wait.
-define(PROMPT_MICROS, 1_000_000).

unsubscribe_test_() ->
    {timeout, 60,
     {setup, fun setup/0, fun cleanup/1,
      [{"unsubscribing the only subscriber sends UNSUBSCRIBE on the link",
        {timeout, 10, fun only_subscriber_unsubscribes/0}},
       {"UNSUBSCRIBE goes out only when the last local subscriber of a topic leaves",
        {timeout, 10, fun last_subscriber_unsubscribes/0}},
       {"after a link respawn, UNSUBSCRIBE goes to the new link",
        {timeout, 15, fun respawned_link_unsubscribes/0}},
       {"a busy link does not hold up unsubscribe, and sends UNSUBSCRIBE once free",
        {timeout, 10, fun busy_link_does_not_block/0}},
       {"an unsubscribe and a new subscribe reach the link in that order, and its events reach the new subscriber",
        {timeout, 10, fun unsubscribe_then_subscribe_in_order/0}}]}}.

%%%===================================================================
%%% Scenarios
%%%===================================================================

only_subscriber_unsubscribes() ->
    with_pool(fun(Pool, _Link, Frames) ->
        {ok, Sub} = macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(), #{}),
        ?assertEqual(subscribe, next_frame(Frames)),
        ok = macula_client:unsubscribe(Pool, Sub),
        ?assertEqual({unsubscribe, ?REALM, ?TOPIC}, next_unsubscribe(Frames))
    end).

last_subscriber_unsubscribes() ->
    with_pool(fun(Pool, _Link, Frames) ->
        {ok, First} = macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(), #{}),
        {ok, Second} = macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(), #{}),
        ?assertEqual(subscribe, next_frame(Frames)),
        ok = macula_client:unsubscribe(Pool, First),
        ?assertEqual(none, next_frame(Frames, ?QUIET_MS)),
        ok = macula_client:unsubscribe(Pool, Second),
        ?assertEqual({unsubscribe, ?REALM, ?TOPIC}, next_unsubscribe(Frames))
    end).

respawned_link_unsubscribes() ->
    with_pool(fun(Pool, OldLink, _Frames) ->
        {ok, Sub} = macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(), #{}),
        Mon = erlang:monitor(process, OldLink),
        exit(OldLink, kill),
        receive
            {'DOWN', Mon, process, OldLink, _} -> ok
        after 2_000 ->
            erlang:error(link_did_not_die)
        end,
        NewLink = wait_for_new_link(Pool, OldLink, 30),
        {NewFrames, NewPeer} = plant_fake_peer(NewLink),
        ok = macula_client:unsubscribe(Pool, Sub),
        Seen = next_unsubscribe(NewFrames),
        exit(NewPeer, kill),
        ?assertEqual({unsubscribe, ?REALM, ?TOPIC}, Seen)
    end).

busy_link_does_not_block() ->
    with_pool(fun(Pool, Link, Frames) ->
        {ok, Sub} = macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(), #{}),
        ?assertEqual(subscribe, next_frame(Frames)),
        ok = sys:suspend(Link),
        {Micros, Result} = timer:tc(fun() -> catch macula_client:unsubscribe(Pool, Sub) end),
        ok = sys:resume(Link),
        ?assertEqual(ok, Result),
        ?assert(Micros < ?PROMPT_MICROS),
        ?assertEqual({unsubscribe, ?REALM, ?TOPIC}, next_unsubscribe(Frames))
    end).

unsubscribe_then_subscribe_in_order() ->
    with_pool(fun(Pool, Link, Frames) ->
        {ok, First} = macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(), #{}),
        ?assertEqual(subscribe, next_frame(Frames)),
        ok = macula_client:unsubscribe(Pool, First),
        {ok, Again} = macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(), #{}),
        ?assertEqual([unsubscribe, subscribe], [next_frame(Frames), next_frame(Frames)]),
        ?assertEqual({event, Again}, event_through(Link))
    end).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    ok.

cleanup(_) ->
    ok.

%% Runs Scenario with a pool of one link to an unreachable seed, that link,
%% and the tag of the frames a fake peer planted in the link passes on. The
%% fake peer goes before the pool closes, so the link never waits on it.
with_pool(Scenario) ->
    {ok, Pool} = macula_client:connect([?SEED], #{}),
    {ok, [#{pid := Link}]} = macula_client:links(Pool),
    {Frames, Peer} = plant_fake_peer(Link),
    try
        Scenario(Pool, Link, Frames)
    after
        exit(Peer, kill),
        _ = (catch macula_client:close(Pool))
    end.

%% Makes Link send its frames to a fake peer, which passes each one to the
%% test process tagged. Returns the tag and the fake peer.
plant_fake_peer(Link) ->
    Test = self(),
    Tag = make_ref(),
    Peer = spawn(fun() -> relay_frames(Test, Tag) end),
    PeerNodeId = macula_identity:public(macula_identity:generate()),
    _ = sys:replace_state(Link, fun(S) ->
            WithPeer = setelement(?PEER_PID_INDEX, S, Peer),
            setelement(?PEER_NODE_ID_INDEX, WithPeer, PeerNodeId)
        end),
    {Tag, Peer}.

relay_frames(Test, Tag) ->
    receive
        {'$gen_cast', {send_frame, Frame}} ->
            Test ! {Tag, Frame},
            relay_frames(Test, Tag)
    after 30_000 ->
        ok
    end.

%% The type of the next frame the fake peer passed on, or none.
next_frame(Tag) ->
    next_frame(Tag, ?FRAME_MS).

next_frame(Tag, Ms) ->
    receive
        {Tag, #{frame_type := Type}} -> Type
    after Ms ->
        none
    end.

%% The next frame the fake peer passed on, as {unsubscribe, Realm, Topic}
%% when it is an UNSUBSCRIBE.
next_unsubscribe(Tag) ->
    receive
        {Tag, #{frame_type := unsubscribe, realm := Realm, topic := Topic}} ->
            {unsubscribe, Realm, Topic};
        {Tag, #{frame_type := Other}} ->
            {other_frame, Other}
    after ?FRAME_MS ->
        none
    end.

%% Hands Link an EVENT for the test topic from its fake peer, as a station
%% sends one, and returns the local subscription the pool delivered it to,
%% as {event, SubRef}, or none.
event_through(Link) ->
    Peer = element(?PEER_PID_INDEX, sys:get_state(Link)),
    Link ! {macula_peering, frame, Peer,
            #{frame_type    => event,
              topic         => ?TOPIC,
              realm         => ?REALM,
              publisher     => macula_identity:public(macula_identity:generate()),
              seq           => 1,
              payload       => #{probe => true},
              delivered_via => direct}},
    receive
        {macula_event, SubRef, ?TOPIC, #{probe := true}, _Meta} -> {event, SubRef}
    after ?FRAME_MS ->
        none
    end.

wait_for_new_link(_Pool, _OldLink, 0) ->
    erlang:error(no_respawn_observed);
wait_for_new_link(Pool, OldLink, Tries) ->
    {ok, Links} = macula_client:links(Pool),
    new_link([P || #{pid := P} <- Links, is_pid(P), P =/= OldLink, is_process_alive(P)],
             Pool, OldLink, Tries).

new_link([New | _], _Pool, _OldLink, _Tries) ->
    New;
new_link([], Pool, OldLink, Tries) ->
    timer:sleep(200),
    wait_for_new_link(Pool, OldLink, Tries - 1).
