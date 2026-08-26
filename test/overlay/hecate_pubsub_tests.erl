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
