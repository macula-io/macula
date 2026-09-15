%% EUnit tests for hecate_pubsub_server. The server signs its own PUBLISH frames with a node identity key in the node's
%% configured crypto profile, and refuses to start with any other key; a relayed PUBLISH is verified once and its EVENT
%% carries the publication bytes unchanged; an inbound EVENT is verified before it matches subscribers.
-module(hecate_pubsub_server_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EU_TIMEOUT, 60).

%%---------------------------------------------------------------------
%% Setup helpers
%%---------------------------------------------------------------------

realm() -> crypto:strong_rand_bytes(32).
id(N)   -> <<N:256>>.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

%% A server draws each seq from the node's counter (macula_publication_seq), which runs under the macula application.
key() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    Key.

start() ->
    {ok, Pid} = hecate_pubsub_server:start_link(#{realm => realm(), identity => key()}),
    Pid.

stop_(Pid) ->
    hecate_pubsub_server:stop(Pid).

%%---------------------------------------------------------------------
%% Construction + inspection
%%---------------------------------------------------------------------

start_link_creates_empty_state_test() ->
    Pid = start(),
    ?assertEqual(0, hecate_pubsub_server:topic_count(Pid)),
    ?assertEqual([], hecate_pubsub_server:topics(Pid)),
    ?assertEqual(0, hecate_pubsub_server:subscriber_count(Pid)),
    stop_(Pid).

realm_returns_configured_realm_test() ->
    R = realm(),
    {ok, Pid} = hecate_pubsub_server:start_link(#{realm => R, identity => key()}),
    ?assertEqual(R, hecate_pubsub_server:realm(Pid)),
    hecate_pubsub_server:stop(Pid).

a_server_whose_key_is_in_another_profile_does_not_start_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        Configured = profile(),
        [Other] = macula_crypto_profile:profiles() -- [Configured],
        {ok, Key} = macula_node_keys:generate(identity, Other),
        ?assertEqual({error, {identity_profile_mismatch, Other, Configured}},
                     hecate_pubsub_server:start_link(#{realm => realm(), identity => Key}))
    end}.

a_server_whose_key_is_not_an_identity_key_does_not_start_test() ->
    {ok, Key} = macula_node_keys:generate(connect, profile()),
    ?assertEqual({error, {identity, not_an_identity_key}},
                 hecate_pubsub_server:start_link(#{realm => realm(), identity => Key})).

%%---------------------------------------------------------------------
%% Subscribe / unsubscribe
%%---------------------------------------------------------------------

subscribe_records_subscriber_test() ->
    Pid = start(),
    Sub = id(1),
    ok = hecate_pubsub_server:subscribe(Pid, <<"news">>, Sub),
    ?assert(hecate_pubsub_server:is_subscribed(Pid, <<"news">>, Sub)),
    ?assertEqual([Sub], hecate_pubsub_server:subscribers(Pid, <<"news">>)),
    ?assertEqual([<<"news">>], hecate_pubsub_server:topics(Pid)),
    ?assertEqual(1, hecate_pubsub_server:subscriber_count(Pid)),
    stop_(Pid).

subscribe_records_a_pattern_separately_from_topics_test() ->
    Pid = start(),
    Sub = id(1),
    ok = hecate_pubsub_server:subscribe(Pid, <<"acme/svc.do">>, Sub),
    ok = hecate_pubsub_server:subscribe(Pid, <<"*/svc.do">>, Sub),
    ?assertEqual([<<"acme/svc.do">>], hecate_pubsub_server:topics(Pid)),
    ?assertEqual([<<"*/svc.do">>], hecate_pubsub_server:patterns(Pid)),
    stop_(Pid).

subscribe_idempotent_test() ->
    Pid = start(),
    Sub = id(1),
    ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, Sub),
    ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, Sub),
    ?assertEqual(1, hecate_pubsub_server:subscriber_count(Pid)),
    stop_(Pid).

multiple_subscribers_per_topic_test() ->
    Pid = start(),
    [ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, id(N)) || N <- [1, 2, 3]],
    ?assertEqual(3, hecate_pubsub_server:subscriber_count(Pid)),
    ?assertEqual(3, length(hecate_pubsub_server:subscribers(Pid, <<"t">>))),
    stop_(Pid).

unsubscribe_drops_subscriber_test() ->
    Pid = start(),
    Sub = id(2),
    ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, Sub),
    ok = hecate_pubsub_server:unsubscribe(Pid, <<"t">>, Sub),
    ?assertNot(hecate_pubsub_server:is_subscribed(Pid, <<"t">>, Sub)),
    ?assertEqual(0, hecate_pubsub_server:subscriber_count(Pid)),
    stop_(Pid).

unsubscribe_unknown_topic_is_noop_test() ->
    Pid = start(),
    ok = hecate_pubsub_server:unsubscribe(Pid, <<"nope">>, id(1)),
    ?assertEqual(0, hecate_pubsub_server:topic_count(Pid)),
    stop_(Pid).

%%---------------------------------------------------------------------
%% purge_subscriber
%%---------------------------------------------------------------------

purge_subscriber_drops_topics_where_it_was_sole_subscriber_test() ->
    Pid = start(),
    Sub = id(1),
    ok = hecate_pubsub_server:subscribe(Pid, <<"a">>, Sub),
    ok = hecate_pubsub_server:subscribe(Pid, <<"b">>, Sub),
    ok = hecate_pubsub_server:purge_subscriber(Pid, Sub),
    ?assertEqual(0, hecate_pubsub_server:topic_count(Pid)),
    stop_(Pid).

purge_subscriber_keeps_topics_that_still_have_other_subscribers_test() ->
    Pid = start(),
    ok = hecate_pubsub_server:subscribe(Pid, <<"a">>, id(1)),
    ok = hecate_pubsub_server:subscribe(Pid, <<"a">>, id(2)),
    ok = hecate_pubsub_server:purge_subscriber(Pid, id(1)),
    ?assertEqual([<<"a">>], hecate_pubsub_server:topics(Pid)),
    ?assertEqual([id(2)], hecate_pubsub_server:subscribers(Pid, <<"a">>)),
    stop_(Pid).

%%---------------------------------------------------------------------
%% Publish
%%---------------------------------------------------------------------

publish_returns_local_matched_subscribers_test() ->
    Pid = start(),
    [ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, id(N)) || N <- [1, 2]],
    {Frame, Matched} = hecate_pubsub_server:publish(Pid, <<"t">>, <<"hello">>),
    ?assertEqual(event, macula_frame:frame_type(Frame)),
    ?assertEqual(lists:sort([id(1), id(2)]), lists:sort(Matched)),
    stop_(Pid).

publish_with_no_subscribers_returns_empty_match_test() ->
    Pid = start(),
    {_Frame, Matched} = hecate_pubsub_server:publish(Pid, <<"empty">>, <<"x">>),
    ?assertEqual([], Matched),
    stop_(Pid).

publish_increments_seq_test() ->
    Pid = start(),
    ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, id(1)),
    {F1, _} = hecate_pubsub_server:publish(Pid, <<"t">>, <<"a">>),
    {F2, _} = hecate_pubsub_server:publish(Pid, <<"t">>, <<"b">>),
    ?assertNotEqual(seq_of(F1), seq_of(F2)),
    stop_(Pid).

%% The seq is seeded from wall-clock microseconds at start, never from zero: the same convention macula_client's own
%% publish_seq follows, so a restarted server produces a large FORWARD jump that subscribers' macula_pubsub_order reads
%% as a new epoch. Seeding from zero rewinds the counter on every station restart, and every ordered/latest_only
%% subscriber then drops each fact as "past" until the counter climbs back over its old watermark: hecate-stations
%% went deaf for 10+ hours this way after a fleet rollout, 2026-09-02, with link, subscriptions and dedup all looking
%% healthy.
publish_seq_is_seeded_from_wall_clock_microseconds_test() ->
    Before = erlang:system_time(microsecond),
    Pid = start(),
    {F, _} = hecate_pubsub_server:publish(Pid, <<"t">>, <<"a">>),
    ?assert(seq_of(F) >= Before),
    stop_(Pid).

publish_signs_the_publication_with_the_server_identity_test() ->
    R = realm(),
    Key = key(),
    {ok, Pid} = hecate_pubsub_server:start_link(#{realm => R, identity => Key}),
    ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, id(1)),
    {Frame, _} = hecate_pubsub_server:publish(Pid, <<"t">>, <<"hello">>),
    Publisher = macula_node_keys:key_id(Key),
    ?assertMatch({ok, #{publisher := Publisher, realm := R, topic := <<"t">>, payload := <<"hello">>}},
                 verified(Frame)),
    ?assertNot(maps:is_key(signature, Frame)),
    hecate_pubsub_server:stop(Pid).

%%---------------------------------------------------------------------
%% deliver_event for inbound frames
%%---------------------------------------------------------------------

deliver_event_returns_subscribers_test() ->
    R = realm(),
    {ok, Pid} = hecate_pubsub_server:start_link(#{realm => R, identity => key()}),
    ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, id(1)),
    %% An event another publisher made, for the same realm.
    ?assertEqual([id(1)], hecate_pubsub_server:deliver_event(Pid, event_frame(R, <<"t">>))),
    hecate_pubsub_server:stop(Pid).

deliver_event_for_other_realm_returns_empty_test() ->
    R1 = realm(),
    R2 = realm(),
    {ok, Pid} = hecate_pubsub_server:start_link(#{realm => R1, identity => key()}),
    ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, id(1)),
    %% A publication for a different realm must NOT deliver.
    ?assertEqual([], hecate_pubsub_server:deliver_event(Pid, event_frame(R2, <<"t">>))),
    hecate_pubsub_server:stop(Pid).

deliver_event_for_a_publication_that_does_not_verify_returns_empty_test() ->
    R = realm(),
    {ok, Pid} = hecate_pubsub_server:start_link(#{realm => R, identity => key()}),
    ok = hecate_pubsub_server:subscribe(Pid, <<"t">>, id(1)),
    ?assertEqual([], hecate_pubsub_server:deliver_event(Pid, tampered(event_frame(R, <<"t">>)))),
    hecate_pubsub_server:stop(Pid).

%%---------------------------------------------------------------------
%% relay_publish: the publisher's publication goes end to end
%%---------------------------------------------------------------------

relay_publish_carries_the_publication_bytes_unchanged_test() ->
    R = realm(),
    Publisher = key(),
    {ok, Pid} = hecate_pubsub_server:start_link(#{realm => R, identity => key()}),
    ok = hecate_pubsub_server:subscribe(Pid, <<"io.macula/x/y/v1">>, id(3)),
    #{publication := Publication} = Publish = publish_frame(R, <<"io.macula/x/y/v1">>, Publisher),
    {EventFrame, Matched} = hecate_pubsub_server:relay_publish(Pid, Publish),
    ?assertEqual(event, macula_frame:frame_type(EventFrame)),
    ?assertEqual(Publication, maps:get(publication, EventFrame)),
    ?assertEqual(direct, maps:get(delivered_via, EventFrame)),
    %% No per-hop signature: the publisher's signature is verifiable end to end.
    ?assertNot(maps:is_key(signature, EventFrame)),
    PublisherId = macula_node_keys:key_id(Publisher),
    ?assertMatch({ok, #{publisher := PublisherId}}, verified(EventFrame)),
    ?assertEqual([id(3)], Matched),
    hecate_pubsub_server:stop(Pid).

relay_publish_refuses_a_publication_that_does_not_verify_test() ->
    R = realm(),
    {ok, Pid} = hecate_pubsub_server:start_link(#{realm => R, identity => key()}),
    Publish = tampered(publish_frame(R, <<"t">>, key())),
    ?assertEqual({error, signature_invalid}, hecate_pubsub_server:relay_publish(Pid, Publish)),
    hecate_pubsub_server:stop(Pid).

relay_publish_for_another_realm_is_refused_test() ->
    {ok, Pid} = hecate_pubsub_server:start_link(#{realm => realm(), identity => key()}),
    Publish = publish_frame(realm(), <<"t">>, key()),
    ?assertEqual({error, realm_mismatch}, hecate_pubsub_server:relay_publish(Pid, Publish)),
    hecate_pubsub_server:stop(Pid).

%%---------------------------------------------------------------------
%% Frame helpers
%%---------------------------------------------------------------------

publish_frame(Realm, Topic, Key) ->
    macula_frame:publish(#{realm => Realm, topic => Topic, seq => 9, published_at => erlang:system_time(millisecond),
                           payload => <<"x">>}, Key).

event_frame(Realm, Topic) ->
    #{publication := Publication} = publish_frame(Realm, Topic, key()),
    macula_frame:event(#{publication => Publication, delivered_via => plumtree}).

tampered(#{publication := #{tbs := <<Head:20/binary, Byte, Tail/binary>>} = Publication} = Frame) ->
    Frame#{publication := Publication#{tbs := <<Head/binary, (Byte bxor 1), Tail/binary>>}}.

verified(Frame) ->
    macula_frame:verify_publication(Frame, profile(), erlang:system_time(millisecond)).

seq_of(Frame) ->
    {ok, #{seq := Seq}} = verified(Frame),
    Seq.
