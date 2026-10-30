%% macula-station#129, phase 1: a subscriber is told when no station link
%% carries its subscription any more, and told the window it may have missed
%% events in once a station accepts it again. Never while another link still
%% carries it.
%%
%% Real macula_station_link workers against unreachable seeds, each told it is
%% connected to a station with this test process as its peer (as in
%% macula_client_register_ack_tests); the test sends the acks a station would,
%% and ends a link's connection as its peering process would.
-module(macula_client_delivery_gap_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_subscriber).
-export([init/1, handle_event/4, handle_interrupted/2, handle_gap/2]).

-define(REALM, <<7:256>>).
-define(TOPIC, <<"acme.counted">>).
-define(SEEDS, [#{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<1:256>>},
                #{host => <<"127.0.0.1">>, port => 2, expected_node_id => <<2:256>>}]).
-define(QUIET_MS, 300).

delivery_gap_test_() ->
    {foreach, fun pool/0, fun(Pool) -> ok = macula_client:close(Pool) end,
     [fun(Pool) -> {Name, {timeout, 15, fun() -> Case(Pool) end}} end
      || {Name, Case} <- [{"the last carrying link ending interrupts, and a station accepting again tells the gap",
                           fun the_last_link_ending_interrupts_then_a_gap_follows/1},
                          {"another link still carrying it tells nothing", fun another_carrying_link_tells_nothing/1},
                          {"both links ending interrupt once, and one gap follows",
                           fun both_links_ending_interrupt_once/1},
                          {"a refused re-subscription leaves it interrupted", fun a_refusal_leaves_it_interrupted/1},
                          {"an unsubscribed subscription gets no gap", fun an_unsubscribed_subscription_gets_no_gap/1},
                          {"a link that never carried it ending tells nothing",
                           fun a_link_that_never_carried_it_tells_nothing/1},
                          {"a callback subscription's notice options are called",
                           fun a_callback_subscription_is_told/1},
                          {"a notice option that is not a function is refused",
                           fun a_notice_option_that_is_not_a_function_is_refused/1},
                          {"a macula_subscriber module is told through its callbacks",
                           fun a_subscriber_module_is_told/1}]]}.

the_last_link_ending_interrupts_then_a_gap_follows(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    {ok, SubRef} = subscribed(Pool),
    Before = erlang:system_time(millisecond),
    accepted(Pool, Link1),
    ended(Link1),
    #{from_ms := From, reason := Reason} = told(macula_delivery_interrupted, SubRef),
    ?assert(From >= Before andalso From =< erlang:system_time(millisecond)),
    ?assertEqual(<<"app_liveness_lost">>, Reason),
    accepted(Pool, respawned(Pool, [Link1, Link2])),
    ?assertMatch(#{from_ms := From, reason := Reason}, told(macula_delivery_gap, SubRef)),
    nothing_told(SubRef).

another_carrying_link_tells_nothing(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    {ok, SubRef} = subscribed(Pool),
    accepted(Pool, Link1),
    accepted(Pool, Link2),
    ended(Link1),
    nothing_told(SubRef).

both_links_ending_interrupt_once(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    {ok, SubRef} = subscribed(Pool),
    accepted(Pool, Link1),
    accepted(Pool, Link2),
    ended(Link1),
    ended(Link2),
    #{from_ms := From} = told(macula_delivery_interrupted, SubRef),
    New1 = respawned(Pool, [Link1, Link2]),
    New2 = respawned(Pool, [Link1, Link2, New1]),
    accepted(Pool, New1),
    accepted(Pool, New2),
    ?assertMatch(#{from_ms := From, to_ms := _}, told(macula_delivery_gap, SubRef)),
    nothing_told(SubRef).

a_refusal_leaves_it_interrupted(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    {ok, SubRef} = subscribed(Pool),
    accepted(Pool, Link1),
    ended(Link1),
    _ = told(macula_delivery_interrupted, SubRef),
    acked(respawned(Pool, [Link1, Link2]), subscribe_ack(node_id(Pool), false, not_authorized)),
    nothing_told(SubRef).

an_unsubscribed_subscription_gets_no_gap(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    {ok, SubRef} = subscribed(Pool),
    accepted(Pool, Link1),
    ended(Link1),
    _ = told(macula_delivery_interrupted, SubRef),
    ok = macula_client:unsubscribe(Pool, SubRef),
    accepted(Pool, respawned(Pool, [Link1, Link2])),
    nothing_told(SubRef).

a_link_that_never_carried_it_tells_nothing(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    {ok, SubRef} = subscribed(Pool),
    accepted(Pool, Link1),
    ended(Link2),
    nothing_told(SubRef).

a_callback_subscription_is_told(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    Test = self(),
    {ok, _SubRef} = macula_pubsub:subscribe_callback(
                      Pool, ?REALM, ?TOPIC, fun(_Topic, _Payload, _Meta) -> ok end,
                      #{sources => open,
                        on_interrupted => fun(Info) -> Test ! {on_interrupted, Info} end,
                        on_gap => fun(Info) -> Test ! {on_gap, Info} end}),
    accepted(Pool, Link1),
    ended(Link1),
    ?assertMatch(#{from_ms := _, reason := _}, received(on_interrupted)),
    accepted(Pool, respawned(Pool, [Link1, Link2])),
    ?assertMatch(#{from_ms := _, to_ms := _, reason := _}, received(on_gap)).

a_notice_option_that_is_not_a_function_is_refused(Pool) ->
    ?assertEqual({error, {invalid_option, on_gap}},
                 macula_pubsub:subscribe_callback(Pool, ?REALM, ?TOPIC, fun(_, _, _) -> ok end,
                                                  #{sources => open, on_gap => not_a_fun})).

a_subscriber_module_is_told(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    {ok, Subscriber} = macula_subscriber:start_link(?MODULE, Pool, ?REALM, ?TOPIC, self(),
                                                    #{sources => open, subscribe => fun macula_client:subscribe/5}),
    accepted(Pool, Link1),
    ended(Link1),
    ?assertMatch(#{from_ms := _, reason := _}, received(handle_interrupted)),
    accepted(Pool, respawned(Pool, [Link1, Link2])),
    ?assertMatch(#{from_ms := _, to_ms := _, reason := _}, received(handle_gap)),
    unlink(Subscriber),
    exit(Subscriber, shutdown).

%%------------------------------------------------------------------
%% macula_subscriber callbacks: each notice goes to the test process.
%%------------------------------------------------------------------

init(Test) -> {ok, Test}.
handle_event(_Topic, _Payload, _Meta, Test) -> {noreply, Test}.
handle_interrupted(Info, Test) -> Test ! {handle_interrupted, Info}, {noreply, Test}.
handle_gap(Info, Test) -> Test ! {handle_gap, Info}, {noreply, Test}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

pool() ->
    ok = flushed(),
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Pool} = macula_client:connect(?SEEDS, #{node_identity => Key, register_ack_timeout_ms => 500}),
    Pool.

subscribed(Pool) ->
    macula_client:subscribe(Pool, ?REALM, ?TOPIC, self(), #{sources => open}).

%% Both links connected, each to its own station, with this process as their peer.
connected(Pool) ->
    {ok, Links} = macula_client:links(Pool),
    [{Pid, connect_to(Pid, <<(20 + N):256>>)} || {N, #{pid := Pid}} <- lists:enumerate(Links)].

connect_to(Pid, Station) ->
    Peer = self(),
    _ = sys:replace_state(Pid, fun(S) -> setelement(macula_station_link:state_field_index(peer_pid), S, Peer) end),
    Pid ! {macula_peering, connected, Peer, Station},
    Station = element(macula_station_link:state_field_index(peer_node_id), sys:get_state(Pid)),
    Station.

%% The link's connection ends as its peering process reports a liveness loss.
ended(Link) ->
    Mon = erlang:monitor(process, Link),
    Link ! {macula_peering, disconnected, self(), app_liveness_lost},
    receive {'DOWN', Mon, process, Link, _} -> ok after 5_000 -> error(link_not_ended) end.

%% A link the pool started after a link ended, none of `Known', connected.
respawned(Pool, Known) ->
    respawned(Pool, Known, 100).

respawned(_Pool, _Known, 0) ->
    error(no_respawn);
respawned(Pool, Known, N) ->
    {ok, Links} = macula_client:links(Pool),
    respawned_one([Pid || #{pid := Pid} <- Links, not lists:member(Pid, Known)], Pool, Known, N).

respawned_one([], Pool, Known, N) ->
    timer:sleep(50),
    respawned(Pool, Known, N - 1);
respawned_one([New | _], _Pool, _Known, _N) ->
    _ = connect_to(New, <<99:256>>),
    New.

accepted(Pool, Link) ->
    acked(Link, subscribe_ack(node_id(Pool), true, undefined)),
    _ = sys:get_state(Pool),
    ok.

subscribe_ack(Subscriber, Accepted, Reason) ->
    ack(#{kind => subscribe, name => ?TOPIC, subscriber => Subscriber}, Accepted, Reason).

ack(Named, true, undefined) ->
    macula_frame:register_ack(Named#{realm => ?REALM, accepted => true});
ack(Named, false, Reason) ->
    macula_frame:register_ack(Named#{realm => ?REALM, accepted => false, reason => Reason}).

%% The ack as the link's connection hands it over: decoded from its bytes, as a station sent it.
acked(Link, Ack) ->
    {ok, Frame, <<>>} = macula_frame:decode(macula_frame:encode(Ack)),
    Link ! {macula_peering, frame, self(), Frame},
    _ = sys:get_state(Link),
    ok.

node_id(Pool) ->
    element(macula_client:state_field_index(node_id), sys:get_state(Pool)).

told(Tag, SubRef) ->
    receive {Tag, SubRef, Info} -> Info after 5_000 -> error({not_told, Tag}) end.

received(Tag) ->
    receive {Tag, Info} -> Info after 5_000 -> error({not_received, Tag}) end.

nothing_told(SubRef) ->
    receive
        {macula_delivery_interrupted, SubRef, Info} -> error({told, interrupted, Info});
        {macula_delivery_gap, SubRef, Info} -> error({told, gap, Info})
    after ?QUIET_MS -> ok
    end.

%% An earlier test's links put from workers, so a put can still land after its pool closed: drained until the
%% mailbox has been quiet for 200 ms.
flushed() ->
    receive _ -> flushed() after 200 -> ok end.
