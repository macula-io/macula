%% macula#12: a station answers each registration with a REGISTER_ACK, and the pool waits for it. A registration is
%% acknowledged once one station it went to accepts it, refused once every one of them refused it, and unacknowledged
%% when none has answered in time. The pool remembers each registration's outcome, so an answer that comes before the
%% caller waits is not lost, and a refusal no caller waits for (a link's replay after a reconnect) is reported.
%%
%% Real macula_station_link workers against unreachable seeds, each told it is connected to a station with this test
%% process as its peer (as in macula_client_per_link_advertise_tests); the test sends the acks a station would.
-module(macula_client_register_ack_tests).

-include_lib("eunit/include/eunit.hrl").

-export([log/2]).

-define(REALM, <<7:256>>).
-define(PROCEDURE, <<"acme.count_v1">>).
-define(TOPIC, <<"acme.counted">>).
-define(SEEDS, [#{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<1:256>>},
                #{host => <<"127.0.0.1">>, port => 2, expected_node_id => <<2:256>>}]).
-define(HANDLER, register_ack_capture).

register_ack_test_() ->
    {foreach, fun pool/0, fun(Pool) -> ok = macula_client:close(Pool) end,
     [fun(Pool) -> {Name, {timeout, 10, fun() -> Case(Pool) end}} end
      || {Name, Case} <- [{"one station accepting acknowledges an advertisement", fun one_accepting_station_acknowledges/1},
                          {"one accepting station is enough when another refuses", fun one_accepting_station_is_enough/1},
                          {"every station refusing refuses with the first reason", fun every_station_refusing_refuses/1},
                          {"no answer in time is unacknowledged, naming a silent station",
                           fun no_answer_in_time_is_unacknowledged/1},
                          {"an answer before the wait is kept", fun an_answer_before_the_wait_is_kept/1},
                          {"a refused subscription is refused with its reason", fun a_refused_subscription_is_refused/1},
                          {"an ack for another subscriber is not this pool's", fun an_ack_for_another_subscriber_is_dropped/1},
                          {"an ack of an unknown kind is ignored", fun an_ack_of_an_unknown_kind_is_ignored/1},
                          {"an advertisement a link could not send is not sent", fun an_unsent_advertisement_is_not_sent/1},
                          {"a refusal nobody waits for is reported", fun a_refusal_nobody_waits_for_is_reported/1},
                          {"a registration the pool sends no frame for needs no ack",
                           fun a_registration_without_a_frame_needs_no_ack/1},
                          {"a refused publish is reported", fun a_refused_publish_is_reported/1},
                          {"a refused publish of another publisher is not this pool's",
                           fun a_refused_publish_of_another_publisher_is_dropped/1}]]}.

one_accepting_station_acknowledges(Pool) ->
    [{Link1, _}, _] = connected(Pool),
    ok = advertise(Pool),
    acked(Link1, advertise_ack(true, undefined)),
    ?assertEqual(ok, macula_client:await_registration(Pool, advertise, ?REALM, ?PROCEDURE)).

one_accepting_station_is_enough(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    ok = advertise(Pool),
    acked(Link1, advertise_ack(false, advertiser_quota)),
    acked(Link2, advertise_ack(true, undefined)),
    ?assertEqual(ok, macula_client:await_registration(Pool, advertise, ?REALM, ?PROCEDURE)).

every_station_refusing_refuses(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    ok = advertise(Pool),
    Waiter = waiting(Pool, advertise, ?PROCEDURE),
    acked(Link1, advertise_ack(false, too_many_providers)),
    acked(Link2, advertise_ack(false, name_too_long)),
    ?assertEqual({error, {refused, too_many_providers}}, answer(Waiter)).

no_answer_in_time_is_unacknowledged(Pool) ->
    [{Link1, _}, {_Link2, Station2}] = connected(Pool),
    ok = advertise(Pool),
    acked(Link1, advertise_ack(false, too_many_providers)),
    ?assertEqual({error, {unacknowledged, Station2}},
                 macula_client:await_registration(Pool, advertise, ?REALM, ?PROCEDURE)).

an_answer_before_the_wait_is_kept(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    ok = advertise(Pool),
    acked(Link1, advertise_ack(false, refused)),
    acked(Link2, advertise_ack(false, refused)),
    _ = sys:get_state(Pool),
    ?assertEqual({error, {refused, refused}}, macula_client:await_registration(Pool, advertise, ?REALM, ?PROCEDURE)).

a_refused_subscription_is_refused(Pool) ->
    [{Link1, _}, {Link2, _}] = connected(Pool),
    {ok, _SubRef} = macula_client:subscribe(Pool,  ?REALM,  ?TOPIC,  self(), #{sources => open}),
    Waiter = waiting(Pool, subscribe, ?TOPIC),
    Subscriber = node_id(Pool),
    acked(Link1, subscribe_ack(Subscriber, false, too_many_realms)),
    acked(Link2, subscribe_ack(Subscriber, false, refused)),
    ?assertEqual({error, {refused, too_many_realms}}, answer(Waiter)).

an_ack_for_another_subscriber_is_dropped(Pool) ->
    [{Link1, Station1}, {Link2, _}] = connected(Pool),
    {ok, _SubRef} = macula_client:subscribe(Pool,  ?REALM,  ?TOPIC,  self(), #{sources => open}),
    acked(Link1, subscribe_ack(<<99:256>>, true, undefined)),
    acked(Link2, subscribe_ack(node_id(Pool), false, refused)),
    ?assertEqual({error, {unacknowledged, Station1}},
                 macula_client:await_registration(Pool, subscribe, ?REALM, ?TOPIC)).

an_ack_of_an_unknown_kind_is_ignored(Pool) ->
    [{Link1, Station1}, {Link2, _}] = connected(Pool),
    ok = advertise(Pool),
    acked(Link1, (advertise_ack(true, undefined))#{kind => unknown}),
    acked(Link2, advertise_ack(false, refused)),
    ?assertEqual({error, {unacknowledged, Station1}},
                 macula_client:await_registration(Pool, advertise, ?REALM, ?PROCEDURE)).

an_unsent_advertisement_is_not_sent(Pool) ->
    _ = connected(Pool),
    Expired = (spec())#{not_after => erlang:system_time(millisecond) - 1},
    ok = macula_client:advertise(Pool, ?REALM, ?PROCEDURE, fun(_) -> {ok, counted} end, open, Expired),
    ?assertMatch({error, {not_sent, _}}, macula_client:await_registration(Pool, advertise, ?REALM, ?PROCEDURE)).

a_refusal_nobody_waits_for_is_reported(Pool) ->
    [{Link1, Station1}, {Link2, _}] = connected(Pool),
    ok = advertise(Pool),
    acked(Link2, advertise_ack(true, undefined)),
    ok = macula_client:await_registration(Pool, advertise, ?REALM, ?PROCEDURE),
    ok = capturing(),
    try
        acked(Link1, advertise_ack(false, advertiser_quota)),
        ?assertMatch(#{station := Station1, kind := advertise, realm := ?REALM, name := ?PROCEDURE,
                       reason := advertiser_quota},
                     captured(<<"_macula.client.registration_refused">>))
    after
        logger:remove_handler(?HANDLER)
    end.

a_registration_without_a_frame_needs_no_ack(Pool) ->
    _ = connected(Pool),
    ok = macula_client:advertise(Pool, ?REALM, ?PROCEDURE, fun(_) -> {ok, counted} end, open, undefined),
    ?assertEqual(ok, macula_client:await_registration(Pool, advertise, ?REALM, ?PROCEDURE)).

%% A station refusing a PUBLISH (macula#128) answers a send nobody waits on: the refusal is reported.
a_refused_publish_is_reported(Pool) ->
    [{Link1, Station1}, _] = connected(Pool),
    ok = capturing_afresh(),
    try
        acked(Link1, publish_ack(node_id(Pool), not_authorized)),
        ?assertMatch(#{station := Station1, kind := publish, realm := ?REALM, name := ?TOPIC,
                       reason := not_authorized},
                     captured(<<"_macula.client.registration_refused">>))
    after
        logger:remove_handler(?HANDLER)
    end.

a_refused_publish_of_another_publisher_is_dropped(Pool) ->
    [{Link1, _}, _] = connected(Pool),
    ok = capturing_afresh(),
    try
        acked(Link1, publish_ack(<<99:256>>, not_authorized)),
        _ = sys:get_state(Pool),
        receive {captured, <<"_macula.client.registration_refused">>, Props} -> error({reported, Props})
        after 300 -> ok
        end
    after
        logger:remove_handler(?HANDLER)
    end.

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

advertise(Pool) ->
    macula_client:advertise(Pool, ?REALM, ?PROCEDURE, fun(_) -> {ok, counted} end, open, spec()).

spec() ->
    #{authorization => #{org_directory => <<"org directory wire">>, procedure_delegation => <<"delegation wire">>},
      not_after => erlang:system_time(millisecond) + 3_600_000}.

advertise_ack(Accepted, Reason) ->
    ack(#{kind => advertise, name => ?PROCEDURE, record_version => <<2:128>>}, Accepted, Reason).

subscribe_ack(Subscriber, Accepted, Reason) ->
    ack(#{kind => subscribe, name => ?TOPIC, subscriber => Subscriber}, Accepted, Reason).

publish_ack(Publisher, Reason) ->
    ack(#{kind => publish, name => ?TOPIC, subscriber => Publisher}, false, Reason).

ack(Named, true, undefined) ->
    macula_frame:register_ack(Named#{realm => ?REALM, accepted => true});
ack(Named, false, Reason) ->
    macula_frame:register_ack(Named#{realm => ?REALM, accepted => false, reason => Reason}).

%% The ack as the link's connection hands it over: decoded from its bytes, as a station sent it.
acked(Link, Ack) ->
    {ok, Frame, <<>>} = decode(Ack),
    Link ! {macula_peering, frame, self(), Frame},
    _ = sys:get_state(Link),
    ok.

decode(#{kind := unknown} = Ack) ->
    {ok, Ack, <<>>};
decode(Ack) ->
    macula_frame:decode(macula_frame:encode(Ack)).

%% A caller already waiting when the acks come: its call has reached the pool once the pool has taken a request
%% sent after it.
waiting(Pool, Kind, Name) ->
    Test = self(),
    Waiter = spawn(fun() -> Test ! {answer, self(), macula_client:await_registration(Pool, Kind, ?REALM, Name)} end),
    ok = in_call(Waiter, 50),
    _ = sys:get_state(Pool),
    Waiter.

in_call(_Waiter, 0) ->
    error(waiter_not_calling);
in_call(Waiter, N) ->
    calling(erlang:process_info(Waiter, current_function), Waiter, N).

calling({current_function, {gen, do_call, 4}}, _Waiter, _N) -> ok;
calling(_Elsewhere, Waiter, N) -> timer:sleep(10), in_call(Waiter, N - 1).

answer(Waiter) ->
    receive {answer, Waiter, Answer} -> Answer after 5_000 -> error(no_answer) end.

node_id(Pool) ->
    element(macula_client:state_field_index(node_id), sys:get_state(Pool)).

capturing() ->
    logger:add_handler(?HANDLER, ?MODULE, #{level => all, config => #{test => self()}}).

%% Capturing with the refusal's log window closed, so an earlier test's report in the last 10 s holds nothing back.
capturing_afresh() ->
    true = ets:delete(macula_diagnostics_bound, {topic, <<"_macula.client.registration_refused">>}),
    capturing().

%% The capture handler's callback.
log(#{msg := {report, #{event := Topic, properties := Props}}}, #{config := #{test := Test}}) ->
    Test ! {captured, Topic, Props};
log(_Event, _Config) ->
    ok.

captured(Topic) ->
    receive {captured, Topic, Props} -> Props after 2_000 -> error({not_captured, Topic}) end.

%% An earlier test's links put from workers, so a put can still land after its pool closed: drained until the
%% mailbox has been quiet for 200 ms.
flushed() ->
    receive _ -> flushed() after 200 -> ok end.
