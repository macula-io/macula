%% EUnit tests for macula_realm_pubsub. The state verifies an EVENT's publication under its crypto profile before it matches
%% subscribers, and an EVENT carries its PUBLISH's publication bytes unchanged.
-module(macula_realm_pubsub_tests).

-include_lib("eunit/include/eunit.hrl").

%% A logger handler that forwards the test process's own events to it.
-export([log/2]).

%%---------------------------------------------------------------------
%% Construction + inspection
%%---------------------------------------------------------------------

new_starts_with_no_topics_test() ->
    R = realm(),
    S = new(R),
    ?assertEqual(R, macula_realm_pubsub:realm(S)),
    ?assertEqual(0, macula_realm_pubsub:topic_count(S)),
    ?assertEqual([], macula_realm_pubsub:topics(S)).

%%---------------------------------------------------------------------
%% Subscribe / unsubscribe basics
%%---------------------------------------------------------------------

subscribe_records_subscriber_test() ->
    Sub = id(1),
    S0 = new(realm()),
    S1 = macula_realm_pubsub:subscribe(S0, <<"news">>, Sub),
    ?assert(macula_realm_pubsub:is_subscribed(S1, <<"news">>, Sub)),
    ?assertEqual([Sub], macula_realm_pubsub:subscribers(S1, <<"news">>)),
    ?assertEqual([<<"news">>], macula_realm_pubsub:topics(S1)),
    ?assertEqual(1, macula_realm_pubsub:subscriber_count(S1)).

subscribe_idempotent_for_same_subscriber_test() ->
    Sub = id(1),
    S = macula_realm_pubsub:subscribe(macula_realm_pubsub:subscribe(new(realm()), <<"t">>, Sub), <<"t">>, Sub),
    ?assertEqual(1, length(macula_realm_pubsub:subscribers(S, <<"t">>))).

subscribe_supports_multiple_subscribers_per_topic_test() ->
    S = lists:foldl(fun(N, A) -> macula_realm_pubsub:subscribe(A, <<"t">>, id(N)) end, new(realm()), [1, 2, 3]),
    ?assertEqual(3, length(macula_realm_pubsub:subscribers(S, <<"t">>))).

unsubscribe_drops_subscriber_test() ->
    Sub = id(1),
    S0 = macula_realm_pubsub:subscribe(new(realm()), <<"t">>, Sub),
    S1 = macula_realm_pubsub:unsubscribe(S0, <<"t">>, Sub),
    ?assertNot(macula_realm_pubsub:is_subscribed(S1, <<"t">>, Sub)),
    %% Topic was the only subscriber: topic removed entirely.
    ?assertEqual(0, macula_realm_pubsub:topic_count(S1)).

unsubscribe_keeps_topic_for_other_subscribers_test() ->
    S0 = macula_realm_pubsub:subscribe(new(realm()), <<"t">>, id(1)),
    S1 = macula_realm_pubsub:subscribe(S0, <<"t">>, id(2)),
    S2 = macula_realm_pubsub:unsubscribe(S1, <<"t">>, id(1)),
    ?assertEqual([id(2)], macula_realm_pubsub:subscribers(S2, <<"t">>)).

unsubscribe_unknown_subscriber_is_noop_test() ->
    S = macula_realm_pubsub:unsubscribe(new(realm()), <<"t">>, id(99)),
    ?assertEqual(0, macula_realm_pubsub:topic_count(S)).

%%---------------------------------------------------------------------
%% purge_subscriber
%%---------------------------------------------------------------------

purge_subscriber_drops_topics_where_it_was_sole_subscriber_test() ->
    Sub = id(1),
    S0 = macula_realm_pubsub:subscribe(new(realm()), <<"a">>, Sub),
    S1 = macula_realm_pubsub:subscribe(S0, <<"b">>, Sub),
    S2 = macula_realm_pubsub:purge_subscriber(S1, Sub),
    ?assertEqual(0, macula_realm_pubsub:topic_count(S2)),
    ?assertEqual([], macula_realm_pubsub:topics(S2)).

purge_subscriber_keeps_topics_that_still_have_other_subscribers_test() ->
    S0 = macula_realm_pubsub:subscribe(new(realm()), <<"a">>, id(1)),
    S1 = macula_realm_pubsub:subscribe(S0, <<"a">>, id(2)),
    S2 = macula_realm_pubsub:purge_subscriber(S1, id(1)),
    ?assertEqual([<<"a">>], macula_realm_pubsub:topics(S2)),
    ?assertEqual([id(2)], macula_realm_pubsub:subscribers(S2, <<"a">>)).

purge_subscriber_only_touches_topics_the_subscriber_was_on_test() ->
    S0 = macula_realm_pubsub:subscribe(new(realm()), <<"a">>, id(1)),
    S1 = macula_realm_pubsub:subscribe(S0, <<"b">>, id(2)),
    S2 = macula_realm_pubsub:purge_subscriber(S1, id(1)),
    ?assertEqual([<<"b">>], macula_realm_pubsub:topics(S2)),
    ?assertEqual([id(2)], macula_realm_pubsub:subscribers(S2, <<"b">>)).

purge_subscriber_unknown_subscriber_is_noop_test() ->
    S0 = macula_realm_pubsub:subscribe(new(realm()), <<"a">>, id(1)),
    S1 = macula_realm_pubsub:purge_subscriber(S0, id(99)),
    ?assertEqual([<<"a">>], macula_realm_pubsub:topics(S1)).

%%---------------------------------------------------------------------
%% Event delivery
%%---------------------------------------------------------------------

deliver_event_returns_subscribers_for_matching_topic_test() ->
    R = realm(),
    S = macula_realm_pubsub:subscribe(new(R), <<"news">>, id(1)),
    ?assertEqual([id(1)], macula_realm_pubsub:deliver_event(S, event_frame(R, <<"news">>))).

deliver_event_returns_empty_when_no_subscribers_test() ->
    R = realm(),
    ?assertEqual([], macula_realm_pubsub:deliver_event(new(R), event_frame(R, <<"t">>))).

deliver_event_ignores_wrong_realm_test() ->
    R1 = realm(),
    R2 = realm(),
    S = macula_realm_pubsub:subscribe(new(R1), <<"t">>, id(1)),
    %% A publication for a different realm: defensive realm check.
    ?assertEqual([], macula_realm_pubsub:deliver_event(S, event_frame(R2, <<"t">>))).

deliver_event_ignores_a_publication_that_does_not_verify_test() ->
    R = realm(),
    S = macula_realm_pubsub:subscribe(new(R), <<"t">>, id(1)),
    #{publication := #{tbs := <<Head:20/binary, Byte, Tail/binary>>} = Publication} = Event = event_frame(R, <<"t">>),
    Tampered = Event#{publication := Publication#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}},
    ?assertEqual([], macula_realm_pubsub:deliver_event(S, Tampered)).

%%---------------------------------------------------------------------
%% build_event carries the publisher's publication
%%---------------------------------------------------------------------

build_event_carries_the_publication_bytes_unchanged_test() ->
    #{publication := Publication} = Publish = publish_frame(realm(), <<"t">>),
    F = macula_realm_pubsub:build_event(Publish, plumtree),
    ?assertEqual(event, macula_frame:frame_type(F)),
    ?assertEqual(plumtree, maps:get(delivered_via, F)),
    ?assertEqual(Publication, maps:get(publication, F)),
    ?assertNot(maps:is_key(signature, F)).

%%---------------------------------------------------------------------
%% Inbound dispatch via process/3
%%---------------------------------------------------------------------

process_subscribe_frame_records_test() ->
    R = realm(),
    Sub = id(7),
    {S1, []} = macula_realm_pubsub:process(new(R), Sub, subscribe_frame(R, <<"t">>, Sub)),
    ?assert(macula_realm_pubsub:is_subscribed(S1, <<"t">>, Sub)).

%% A subscription is recorded, not logged: this runs for every SUBSCRIBE a
%% station relays. A temporary per-topic info trace for the retired
%% mpong topics logged about 400 lines an hour at each
%% station, long after anything published there.
process_subscribe_logs_nothing_test() ->
    R = realm(),
    Sub = id(7),
    Logged = capture_logs(fun() ->
        macula_realm_pubsub:process(new(R), Sub,
                              subscribe_frame(R, <<"io.macula/beam-campus/demo/mpong/state_broadcast_v1">>, Sub))
    end),
    ?assertEqual([], Logged).

%% The capture sees only this test's own events: in a full eunit run another
%% test's process logs at the same time, and its line is not ours (#86).
capture_logs_ignores_another_process_test() ->
    Logged = capture_logs(fun() ->
        {Pid, Ref} = spawn_monitor(fun() -> logger:warning("a neighbouring test's warning") end),
        receive {'DOWN', Ref, process, Pid, _} -> ok end
    end),
    ?assertEqual([], Logged).

%% ...and still sees a line this process logs, so the assertion above is not vacuous.
capture_logs_sees_this_process_test() ->
    Logged = capture_logs(fun() -> logger:info("this test's line") end),
    ?assertEqual([{string, "this test's line"}], Logged).

%% process/3 is pure and runs in the test process, so an event logged by any
%% other process belongs to another test and is dropped.
log(#{meta := #{pid := Pid}} = Event, #{config := #{test := Pid}}) -> Pid ! {logged, Event};
log(_Event, _Config) -> ok.

capture_logs(Fun) ->
    #{level := Level} = logger:get_primary_config(),
    ok = logger:set_primary_config(level, all),
    ok = logger:add_handler(capture, ?MODULE, #{level => all, config => #{test => self()}}),
    try
        _ = Fun(),
        drain_logs()
    after
        logger:remove_handler(capture),
        logger:set_primary_config(level, Level)
    end.

drain_logs() ->
    receive {logged, #{msg := Msg}} -> [Msg | drain_logs()] after 100 -> [] end.

process_subscribe_for_wrong_realm_is_ignored_test() ->
    R1 = realm(),
    R2 = realm(),
    Sub = id(7),
    {S1, []} = macula_realm_pubsub:process(new(R1), Sub, subscribe_frame(R2, <<"t">>, Sub)),
    ?assertEqual(0, macula_realm_pubsub:topic_count(S1)).

process_unsubscribe_drops_subscription_test() ->
    R = realm(),
    Sub = id(7),
    S0 = macula_realm_pubsub:subscribe(new(R), <<"t">>, Sub),
    {S1, []} = macula_realm_pubsub:process(S0, Sub, unsubscribe_frame(R, <<"t">>, Sub)),
    ?assertNot(macula_realm_pubsub:is_subscribed(S1, <<"t">>, Sub)).

process_event_returns_local_subscribers_test() ->
    R = realm(),
    S0 = macula_realm_pubsub:subscribe(new(R), <<"t">>, id(1)),
    S1 = macula_realm_pubsub:subscribe(S0, <<"t">>, id(2)),
    {S1, Subs} = macula_realm_pubsub:process(S1, id(99), event_frame(R, <<"t">>)),
    ?assertEqual(lists:sort([id(1), id(2)]), lists:sort(Subs)).

%%---------------------------------------------------------------------
%% Wildcard subscriptions (2026-08-29, station-local)
%%---------------------------------------------------------------------

wildcard_subscriber_receives_every_matching_concrete_topic_test() ->
    Sub = id(1),
    S1 = macula_realm_pubsub:subscribe(new(realm()), <<"realm/*/app/domain/name_v1">>, Sub),
    ?assertEqual([Sub], macula_realm_pubsub:subscribers(S1, <<"realm/acme/app/domain/name_v1">>)),
    ?assertEqual([Sub], macula_realm_pubsub:subscribers(S1, <<"realm/contoso/app/domain/name_v1">>)),
    ?assertEqual([], macula_realm_pubsub:subscribers(S1, <<"realm/acme/app/domain/other_v1">>)).

exact_subscriber_unaffected_by_a_coexisting_pattern_test() ->
    ExactSub = id(1),
    PatternSub = id(2),
    S1 = macula_realm_pubsub:subscribe(new(realm()), <<"acme/svc.do">>, ExactSub),
    S2 = macula_realm_pubsub:subscribe(S1, <<"*/svc.do">>, PatternSub),
    Got = lists:sort(macula_realm_pubsub:subscribers(S2, <<"acme/svc.do">>)),
    ?assertEqual(lists:sort([ExactSub, PatternSub]), Got),
    %% A topic the exact subscription doesn't cover but the pattern does: only the pattern subscriber receives it.
    ?assertEqual([PatternSub], macula_realm_pubsub:subscribers(S2, <<"contoso/svc.do">>)).

pattern_is_not_exposed_via_topics_1_test() ->
    Sub = id(1),
    S1 = macula_realm_pubsub:subscribe(new(realm()), <<"acme/svc.do">>, Sub),
    S2 = macula_realm_pubsub:subscribe(S1, <<"*/svc.do">>, Sub),
    %% topics/1 feeds the Bloom-gossip re-subscription path: a wildcard pattern must never appear there (see moduledoc).
    ?assertEqual([<<"acme/svc.do">>], macula_realm_pubsub:topics(S2)),
    %% patterns/1 is the separate, purpose-built export for the _mesh.patterns gossip path instead.
    ?assertEqual([<<"*/svc.do">>], macula_realm_pubsub:patterns(S2)),
    %% Both ARE real, counted state.
    ?assertEqual(2, macula_realm_pubsub:topic_count(S2)),
    ?assertEqual(2, macula_realm_pubsub:subscriber_count(S2)).

is_subscribed_checks_the_literal_pattern_string_not_matched_concrete_topics_test() ->
    Sub = id(1),
    S1 = macula_realm_pubsub:subscribe(new(realm()), <<"*/svc.do">>, Sub),
    ?assert(macula_realm_pubsub:is_subscribed(S1, <<"*/svc.do">>, Sub)),
    %% Sub is not "subscribed" to a concrete topic the pattern merely matches: it never registered that literal string.
    ?assertNot(macula_realm_pubsub:is_subscribed(S1, <<"acme/svc.do">>, Sub)),
    %% But it WOULD receive a publish there: a different question.
    ?assertEqual([Sub], macula_realm_pubsub:subscribers(S1, <<"acme/svc.do">>)).

unsubscribe_from_a_pattern_drops_it_like_any_other_topic_test() ->
    Sub = id(1),
    S1 = macula_realm_pubsub:subscribe(new(realm()), <<"*/svc.do">>, Sub),
    S2 = macula_realm_pubsub:unsubscribe(S1, <<"*/svc.do">>, Sub),
    ?assertNot(macula_realm_pubsub:is_subscribed(S2, <<"*/svc.do">>, Sub)),
    ?assertEqual([], macula_realm_pubsub:subscribers(S2, <<"acme/svc.do">>)),
    ?assertEqual(0, macula_realm_pubsub:topic_count(S2)).

purge_subscriber_drops_patterns_too_test() ->
    Sub = id(1),
    S1 = macula_realm_pubsub:subscribe(new(realm()), <<"acme/svc.do">>, Sub),
    S2 = macula_realm_pubsub:subscribe(S1, <<"*/svc.do">>, Sub),
    S3 = macula_realm_pubsub:purge_subscriber(S2, Sub),
    ?assertEqual(0, macula_realm_pubsub:topic_count(S3)),
    ?assertEqual([], macula_realm_pubsub:subscribers(S3, <<"acme/svc.do">>)),
    ?assertEqual([], macula_realm_pubsub:subscribers(S3, <<"contoso/svc.do">>)).

%% deliver_event/2 (what a real inbound EVENT drives) routes through subscribers/2 unchanged: this proves the full
%% frame-shaped path, not just the pure subscribers/2 call above.
deliver_event_reaches_a_wildcard_subscriber_test() ->
    R = realm(),
    Sub = id(1),
    S1 = macula_realm_pubsub:subscribe(new(R), <<"*/svc.do">>, Sub),
    ?assertEqual([Sub], macula_realm_pubsub:deliver_event(S1, event_frame(R, <<"acme/svc.do">>))).

%%=====================================================================
%% Helpers
%%=====================================================================

realm() -> crypto:strong_rand_bytes(32).

id(N) -> <<N:256>>.

new(Realm) -> macula_realm_pubsub:new(Realm, pq_pure).

%% A PUBLISH signed by a fresh publisher in pq_pure.
publish_frame(Realm, Topic) ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    macula_frame:publish(#{realm => Realm, topic => Topic, seq => 0, published_at => erlang:system_time(millisecond),
                           payload => <<"data">>}, Key).

event_frame(Realm, Topic) ->
    #{publication := Publication} = publish_frame(Realm, Topic),
    macula_frame:event(#{publication => Publication, delivered_via => plumtree}).

subscribe_frame(Realm, Topic, Sub) ->
    macula_frame:subscribe(#{topic => Topic, realm => Realm, subscriber => Sub}).

unsubscribe_frame(Realm, Topic, Sub) ->
    macula_frame:unsubscribe(#{topic => Topic, realm => Realm, subscriber => Sub}).
