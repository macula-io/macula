%% EUnit tests for hecate_pubsub.
-module(hecate_pubsub_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Construction + inspection
%%---------------------------------------------------------------------

new_starts_with_no_topics_test() ->
    R = realm(),
    S = hecate_pubsub:new(R),
    ?assertEqual(R, hecate_pubsub:realm(S)),
    ?assertEqual(0, hecate_pubsub:topic_count(S)),
    ?assertEqual([], hecate_pubsub:topics(S)).

%%---------------------------------------------------------------------
%% Subscribe / unsubscribe basics
%%---------------------------------------------------------------------

subscribe_records_subscriber_test() ->
    Sub = id(1),
    S0 = hecate_pubsub:new(realm()),
    S1 = hecate_pubsub:subscribe(S0, <<"news">>, Sub),
    ?assert(hecate_pubsub:is_subscribed(S1, <<"news">>, Sub)),
    ?assertEqual([Sub], hecate_pubsub:subscribers(S1, <<"news">>)),
    ?assertEqual([<<"news">>], hecate_pubsub:topics(S1)),
    ?assertEqual(1, hecate_pubsub:subscriber_count(S1)).

subscribe_idempotent_for_same_subscriber_test() ->
    Sub = id(1),
    S = hecate_pubsub:subscribe(
          hecate_pubsub:subscribe(hecate_pubsub:new(realm()),
                                  <<"t">>, Sub),
          <<"t">>, Sub),
    ?assertEqual(1, length(hecate_pubsub:subscribers(S, <<"t">>))).

subscribe_supports_multiple_subscribers_per_topic_test() ->
    S = lists:foldl(fun(N, A) ->
                            hecate_pubsub:subscribe(A, <<"t">>, id(N))
                    end, hecate_pubsub:new(realm()), [1, 2, 3]),
    ?assertEqual(3, length(hecate_pubsub:subscribers(S, <<"t">>))).

unsubscribe_drops_subscriber_test() ->
    Sub = id(1),
    S0 = hecate_pubsub:subscribe(hecate_pubsub:new(realm()),
                                 <<"t">>, Sub),
    S1 = hecate_pubsub:unsubscribe(S0, <<"t">>, Sub),
    ?assertNot(hecate_pubsub:is_subscribed(S1, <<"t">>, Sub)),
    %% Topic was the only subscriber → topic removed entirely.
    ?assertEqual(0, hecate_pubsub:topic_count(S1)).

unsubscribe_keeps_topic_for_other_subscribers_test() ->
    S0 = hecate_pubsub:subscribe(hecate_pubsub:new(realm()),
                                 <<"t">>, id(1)),
    S1 = hecate_pubsub:subscribe(S0, <<"t">>, id(2)),
    S2 = hecate_pubsub:unsubscribe(S1, <<"t">>, id(1)),
    ?assertEqual([id(2)], hecate_pubsub:subscribers(S2, <<"t">>)).

unsubscribe_unknown_subscriber_is_noop_test() ->
    S = hecate_pubsub:unsubscribe(hecate_pubsub:new(realm()),
                                  <<"t">>, id(99)),
    ?assertEqual(0, hecate_pubsub:topic_count(S)).

%%---------------------------------------------------------------------
%% purge_subscriber
%%---------------------------------------------------------------------

purge_subscriber_drops_topics_where_it_was_sole_subscriber_test() ->
    Sub = id(1),
    S0 = hecate_pubsub:subscribe(hecate_pubsub:new(realm()), <<"a">>, Sub),
    S1 = hecate_pubsub:subscribe(S0, <<"b">>, Sub),
    S2 = hecate_pubsub:purge_subscriber(S1, Sub),
    ?assertEqual(0, hecate_pubsub:topic_count(S2)),
    ?assertEqual([], hecate_pubsub:topics(S2)).

purge_subscriber_keeps_topics_that_still_have_other_subscribers_test() ->
    S0 = hecate_pubsub:subscribe(hecate_pubsub:new(realm()), <<"a">>, id(1)),
    S1 = hecate_pubsub:subscribe(S0, <<"a">>, id(2)),
    S2 = hecate_pubsub:purge_subscriber(S1, id(1)),
    ?assertEqual([<<"a">>], hecate_pubsub:topics(S2)),
    ?assertEqual([id(2)], hecate_pubsub:subscribers(S2, <<"a">>)).

purge_subscriber_only_touches_topics_the_subscriber_was_on_test() ->
    S0 = hecate_pubsub:subscribe(hecate_pubsub:new(realm()), <<"a">>, id(1)),
    S1 = hecate_pubsub:subscribe(S0, <<"b">>, id(2)),
    S2 = hecate_pubsub:purge_subscriber(S1, id(1)),
    ?assertEqual([<<"b">>], hecate_pubsub:topics(S2)),
    ?assertEqual([id(2)], hecate_pubsub:subscribers(S2, <<"b">>)).

purge_subscriber_unknown_subscriber_is_noop_test() ->
    S0 = hecate_pubsub:subscribe(hecate_pubsub:new(realm()), <<"a">>, id(1)),
    S1 = hecate_pubsub:purge_subscriber(S0, id(99)),
    ?assertEqual([<<"a">>], hecate_pubsub:topics(S1)).

%%---------------------------------------------------------------------
%% Event delivery
%%---------------------------------------------------------------------

deliver_event_returns_subscribers_for_matching_topic_test() ->
    R = realm(),
    S = hecate_pubsub:subscribe(hecate_pubsub:new(R), <<"news">>,
                                 id(1)),
    Frame = event_frame(R, <<"news">>),
    ?assertEqual([id(1)], hecate_pubsub:deliver_event(S, Frame)).

deliver_event_returns_empty_when_no_subscribers_test() ->
    R = realm(),
    S = hecate_pubsub:new(R),
    ?assertEqual([], hecate_pubsub:deliver_event(S, event_frame(R, <<"t">>))).

deliver_event_ignores_wrong_realm_test() ->
    R1 = realm(),
    R2 = realm(),
    S = hecate_pubsub:subscribe(hecate_pubsub:new(R1), <<"t">>, id(1)),
    %% Event from a different realm — defensive realm check.
    ?assertEqual([], hecate_pubsub:deliver_event(S, event_frame(R2, <<"t">>))).

%%---------------------------------------------------------------------
%% build_event signs with publisher's identity
%%---------------------------------------------------------------------

build_event_signs_with_identity_test() ->
    R = realm(),
    Kp = macula_identity:generate(),
    PubId = macula_identity:public(Kp),
    State = hecate_pubsub:new(R),
    PubSpec = #{topic => <<"t">>,
                realm => R,
                publisher => PubId,
                seq => 0,
                payload => <<"data">>,
                published_at_ms => 1},
    F = hecate_pubsub:build_event(State, PubSpec, Kp),
    ?assertEqual(event,    macula_frame:frame_type(F)),
    ?assertEqual(plumtree, maps:get(delivered_via, F)),
    ?assertMatch({ok, _},  macula_frame:verify(F, PubId)).

%%---------------------------------------------------------------------
%% Inbound dispatch via process/3
%%---------------------------------------------------------------------

process_subscribe_frame_records_test() ->
    R = realm(),
    Sub = id(7),
    Frame = sign_subscribe(R, <<"t">>, Sub),
    {S1, []} = hecate_pubsub:process(hecate_pubsub:new(R), Sub, Frame),
    ?assert(hecate_pubsub:is_subscribed(S1, <<"t">>, Sub)).

process_subscribe_for_wrong_realm_is_ignored_test() ->
    R1 = realm(),
    R2 = realm(),
    Sub = id(7),
    Frame = sign_subscribe(R2, <<"t">>, Sub),
    {S1, []} = hecate_pubsub:process(hecate_pubsub:new(R1), Sub, Frame),
    ?assertEqual(0, hecate_pubsub:topic_count(S1)).

process_unsubscribe_drops_subscription_test() ->
    R = realm(),
    Sub = id(7),
    S0 = hecate_pubsub:subscribe(hecate_pubsub:new(R), <<"t">>, Sub),
    UnsubFrame = sign_unsubscribe(R, <<"t">>, Sub),
    {S1, []} = hecate_pubsub:process(S0, Sub, UnsubFrame),
    ?assertNot(hecate_pubsub:is_subscribed(S1, <<"t">>, Sub)).

process_event_returns_local_subscribers_test() ->
    R = realm(),
    S0 = hecate_pubsub:subscribe(hecate_pubsub:new(R), <<"t">>, id(1)),
    S1 = hecate_pubsub:subscribe(S0, <<"t">>, id(2)),
    EventFrame = event_frame(R, <<"t">>),
    {S1, Subs} = hecate_pubsub:process(S1, id(99), EventFrame),
    ?assertEqual(lists:sort([id(1), id(2)]), lists:sort(Subs)).

%%=====================================================================
%% Helpers
%%=====================================================================

%%---------------------------------------------------------------------
%% Wildcard subscriptions (2026-08-29, station-local)
%%---------------------------------------------------------------------

wildcard_subscriber_receives_every_matching_concrete_topic_test() ->
    Sub = id(1),
    S0 = hecate_pubsub:new(realm()),
    S1 = hecate_pubsub:subscribe(S0, <<"realm/*/app/domain/name_v1">>, Sub),
    ?assertEqual([Sub], hecate_pubsub:subscribers(
                          S1, <<"realm/acme/app/domain/name_v1">>)),
    ?assertEqual([Sub], hecate_pubsub:subscribers(
                          S1, <<"realm/contoso/app/domain/name_v1">>)),
    ?assertEqual([], hecate_pubsub:subscribers(
                       S1, <<"realm/acme/app/domain/other_v1">>)).

exact_subscriber_unaffected_by_a_coexisting_pattern_test() ->
    ExactSub = id(1),
    PatternSub = id(2),
    S0 = hecate_pubsub:new(realm()),
    S1 = hecate_pubsub:subscribe(S0, <<"acme/svc.do">>, ExactSub),
    S2 = hecate_pubsub:subscribe(S1, <<"*/svc.do">>, PatternSub),
    Got = lists:sort(hecate_pubsub:subscribers(S2, <<"acme/svc.do">>)),
    ?assertEqual(lists:sort([ExactSub, PatternSub]), Got),
    %% A topic the exact subscription doesn't cover but the pattern
    %% does -- only the pattern subscriber receives it.
    ?assertEqual([PatternSub],
                 hecate_pubsub:subscribers(S2, <<"contoso/svc.do">>)).

pattern_is_not_exposed_via_topics_1_test() ->
    Sub = id(1),
    S0 = hecate_pubsub:new(realm()),
    S1 = hecate_pubsub:subscribe(S0, <<"acme/svc.do">>, Sub),
    S2 = hecate_pubsub:subscribe(S1, <<"*/svc.do">>, Sub),
    %% topics/1 feeds cross-station gossip re-subscription -- a
    %% wildcard pattern must never appear there (see moduledoc).
    ?assertEqual([<<"acme/svc.do">>], hecate_pubsub:topics(S2)),
    %% But it IS real, counted state.
    ?assertEqual(2, hecate_pubsub:topic_count(S2)),
    ?assertEqual(2, hecate_pubsub:subscriber_count(S2)).

is_subscribed_checks_the_literal_pattern_string_not_matched_concrete_topics_test() ->
    Sub = id(1),
    S0 = hecate_pubsub:new(realm()),
    S1 = hecate_pubsub:subscribe(S0, <<"*/svc.do">>, Sub),
    ?assert(hecate_pubsub:is_subscribed(S1, <<"*/svc.do">>, Sub)),
    %% Sub is not "subscribed" to a concrete topic the pattern merely
    %% matches -- it never registered under that literal string.
    ?assertNot(hecate_pubsub:is_subscribed(S1, <<"acme/svc.do">>, Sub)),
    %% But it WOULD receive a publish there -- a different question.
    ?assertEqual([Sub], hecate_pubsub:subscribers(S1, <<"acme/svc.do">>)).

unsubscribe_from_a_pattern_drops_it_like_any_other_topic_test() ->
    Sub = id(1),
    S0 = hecate_pubsub:new(realm()),
    S1 = hecate_pubsub:subscribe(S0, <<"*/svc.do">>, Sub),
    S2 = hecate_pubsub:unsubscribe(S1, <<"*/svc.do">>, Sub),
    ?assertNot(hecate_pubsub:is_subscribed(S2, <<"*/svc.do">>, Sub)),
    ?assertEqual([], hecate_pubsub:subscribers(S2, <<"acme/svc.do">>)),
    ?assertEqual(0, hecate_pubsub:topic_count(S2)).

purge_subscriber_drops_patterns_too_test() ->
    Sub = id(1),
    S0 = hecate_pubsub:new(realm()),
    S1 = hecate_pubsub:subscribe(S0, <<"acme/svc.do">>, Sub),
    S2 = hecate_pubsub:subscribe(S1, <<"*/svc.do">>, Sub),
    S3 = hecate_pubsub:purge_subscriber(S2, Sub),
    ?assertEqual(0, hecate_pubsub:topic_count(S3)),
    ?assertEqual([], hecate_pubsub:subscribers(S3, <<"acme/svc.do">>)),
    ?assertEqual([], hecate_pubsub:subscribers(S3, <<"contoso/svc.do">>)).

%% deliver_event/2 (what a real inbound EVENT actually drives) routes
%% through subscribers/2 unchanged -- this proves the full frame-shaped
%% path, not just the pure subscribers/2 call above.
deliver_event_reaches_a_wildcard_subscriber_test() ->
    R = realm(),
    Sub = id(1),
    S0 = hecate_pubsub:new(R),
    S1 = hecate_pubsub:subscribe(S0, <<"*/svc.do">>, Sub),
    Frame = event_frame(R, <<"acme/svc.do">>),
    ?assertEqual([Sub], hecate_pubsub:deliver_event(S1, Frame)).

realm() -> crypto:strong_rand_bytes(32).

id(N) -> <<N:256>>.

event_frame(Realm, Topic) ->
    Kp = macula_identity:generate(),
    macula_frame:sign(macula_frame:event(#{
        topic         => Topic,
        realm         => Realm,
        publisher     => macula_identity:public(Kp),
        seq           => 0,
        payload       => <<"data">>,
        delivered_via => plumtree
    }), Kp).

sign_subscribe(Realm, Topic, Sub) ->
    Kp = macula_identity:generate(),
    macula_frame:sign(macula_frame:subscribe(#{
        topic      => Topic,
        realm      => Realm,
        subscriber => Sub
    }), Kp).

sign_unsubscribe(Realm, Topic, Sub) ->
    Kp = macula_identity:generate(),
    macula_frame:sign(macula_frame:unsubscribe(#{
        topic      => Topic,
        realm      => Realm,
        subscriber => Sub
    }), Kp).
