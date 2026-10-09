%%% @doc Wiring tests for delivery ordering through a live pool: inject
%%% out-of-order `macula_event' frames into a link-less pool and assert
%%% the subscriber sees each `delivery' mode's contract. The ordering
%%% logic itself is unit-tested in `macula_pubsub_order_tests'; this
%%% proves `macula_client' threads it, arms the flush timer, and reports
%%% the skip telemetry.
-module(macula_client_order_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(PUB, <<7:256>>).

%% Default mode is `ordered': a publisher's out-of-order arrivals are
%% delivered in seq order.
ordered_default_reorders_test_() ->
    {timeout, 5, fun() ->
        {Pool, Ref, Topic} = start(#{}, #{}),
        [inject(Pool, Topic, ?PUB, S) || S <- [1, 3, 2]],
        ?assertEqual([1, 2, 3], collect(Ref, Topic, 3, 2000)),
        stop(Pool, Ref)
    end}.

%% `latest_only' drops a stale (lower) seq arriving after a newer one.
latest_only_drops_stale_test_() ->
    {timeout, 5, fun() ->
        {Pool, Ref, Topic} = start(#{}, #{delivery => latest_only}),
        [inject(Pool, Topic, ?PUB, S) || S <- [1, 3, 2, 4]],
        %% 2 is stale after 3 and never delivered
        ?assertEqual([1, 3, 4], collect(Ref, Topic, 3, 2000)),
        stop(Pool, Ref)
    end}.

%% `as_arrives' delivers in raw arrival order (no reordering, no drops).
as_arrives_preserves_arrival_test_() ->
    {timeout, 5, fun() ->
        {Pool, Ref, Topic} = start(#{}, #{delivery => as_arrives}),
        [inject(Pool, Topic, ?PUB, S) || S <- [1, 3, 2]],
        ?assertEqual([1, 3, 2], collect(Ref, Topic, 3, 2000)),
        stop(Pool, Ref)
    end}.

%% A genuinely missing seq is skipped after the timeout, the buffered
%% tail releases, and the skip shows up in `status/1'.
ordered_skips_gap_after_timeout_test_() ->
    {timeout, 5, fun() ->
        {Pool, Ref, Topic} = start(#{order_timeout_ms => 50}, #{}),
        inject(Pool, Topic, ?PUB, 1),
        inject(Pool, Topic, ?PUB, 3),   %% 2 never arrives
        ?assertEqual([1], collect(Ref, Topic, 1, 1000)),
        %% the flush timer fires ~50ms later, skips 2, releases 3
        ?assertEqual([3], collect(Ref, Topic, 1, 1000)),
        {ok, St} = macula_client:status(Pool),
        ?assertEqual(1, maps:get(pubsub_gap_skips, St)),
        stop(Pool, Ref)
    end}.

%% A copy that arrives after its gap was skipped is delivered through the
%% pool too, flagged `late => true' in the meta, and both new counters
%% move: the skip and the late delivery are visible and the copy is not
%% mistaken for a duplicate.
ordered_late_copy_is_delivered_flagged_test_() ->
    {timeout, 5, fun() ->
        {Pool, Ref, Topic} = start(#{order_timeout_ms => 50}, #{}),
        inject(Pool, Topic, ?PUB, 1),
        inject(Pool, Topic, ?PUB, 3),   %% 2 never arrives yet
        ?assertEqual([1], collect(Ref, Topic, 1, 1000)),
        %% the flush fires ~50ms later, skips 2, releases 3
        ?assertEqual([3], collect(Ref, Topic, 1, 1000)),
        {ok, Marks} = macula_client:status(Pool),
        ?assertEqual(1, maps:get(pubsub_gap_skips, Marks)),
        %% 2's copy lands afterwards: delivered, flagged, counted late.
        inject(Pool, Topic, ?PUB, 2),
        {Late, LateMeta} = collect_flagged(Ref, Topic, 1000),
        ?assertEqual(2, Late),
        ?assertEqual(true, maps:get(late, LateMeta)),
        {ok, Marks2} = macula_client:status(Pool),
        ?assertEqual(1, maps:get(pubsub_late_delivered, Marks2)),
        ?assertEqual(0, maps:get(pubsub_past_dropped, Marks2)),
        %% A re-arrival the dedup no longer knows (same seq, other
        %% publication hash) is a true duplicate to the ordering: dropped
        %% and counted as a past-drop.
        inject(Pool, Topic, ?PUB, 1, crypto:hash(sha384, <<"past", ?PUB/binary, 1:64>>)),
        ?assertEqual(ok, collect_none(Ref, Topic, 200)),
        {ok, Marks3} = macula_client:status(Pool),
        ?assertEqual(1, maps:get(pubsub_past_dropped, Marks3)),
        stop(Pool, Ref)
    end}.

%% A pattern subscription's events keep their own topic however the
%% ordering releases them: at once, or by the flush after a gap
%% (macula#49). Before, a flushed event carried the pattern.
pattern_flushed_event_keeps_its_topic_test_() ->
    {timeout, 5, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        {ok, Pool} = macula_client:connect([], #{order_timeout_ms => 50}),
        Pattern = <<"order/*/wire_v1">>,
        Concrete = <<"order/east/wire_v1">>,
        {ok, Ref} = macula_client:subscribe(Pool, ?REALM, Pattern, self(), #{}),
        inject(Pool, Concrete, ?PUB, 1),
        inject(Pool, Concrete, ?PUB, 3),   %% 2 never arrives: 3 waits for the flush
        ?assertEqual([{Concrete, 1}, {Concrete, 3}], collect_topics(Ref, 2, 1000)),
        stop(Pool, Ref)
    end}.

%%%===================================================================
%%% helpers
%%%===================================================================

start(ConnOpts, SubOpts) ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], ConnOpts),
    Topic = <<"order.wire_v1">>,
    {ok, Ref} = macula_client:subscribe(Pool, ?REALM, Topic, self(), SubOpts),
    {Pool, Ref, Topic}.

stop(Pool, Ref) ->
    try macula_client:unsubscribe(Pool, Ref) catch _:_ -> ok end,
    ok = macula_client:close(Pool).

%% Each (publisher, seq) stands in for one verified publication, with its
%% own publication_hash and an expiry a minute from now.
inject(Pool, Topic, Pub, Seq) ->
    inject(Pool, Topic, Pub, Seq, crypto:hash(sha384, <<Pub/binary, Seq:64>>)).

%% As inject/4, with the publication hash given explicitly -- a second
%% arrival of the same seq under a different hash is what the dedup lets
%% through, and what `ordered' has to classify past vs late.
inject(Pool, Topic, Pub, Seq, Hash) ->
    Pool ! {macula_event, make_ref(), Topic, Seq,
            #{realm => ?REALM, publisher => Pub, seq => Seq,
              delivered_via => direct,
              publication_hash => Hash,
              expires_at => erlang:system_time(millisecond) + 60_000}},
    ok.

collect(_Ref, _Topic, 0, _Timeout) ->
    [];
collect(Ref, Topic, N, Timeout) ->
    receive
        {macula_event, Ref, Topic, Payload, _Meta} ->
            [Payload | collect(Ref, Topic, N - 1, Timeout)]
    after Timeout ->
        []
    end.

%% One event with its meta, or `timeout'.
collect_flagged(Ref, Topic, Timeout) ->
    receive
        {macula_event, Ref, Topic, Payload, Meta} -> {Payload, Meta}
    after Timeout ->
        timeout
    end.

%% Assert the subscriber sees nothing for `Timeout': `ok', or the
%% unexpected event as the return so an assertion shows it.
collect_none(Ref, Topic, Timeout) ->
    receive
        {macula_event, Ref, Topic, Payload, _Meta} -> {unexpected_event, Payload}
    after Timeout ->
        ok
    end.

collect_topics(_Ref, 0, _Timeout) ->
    [];
collect_topics(Ref, N, Timeout) ->
    receive
        {macula_event, Ref, Topic, Payload, _Meta} -> [{Topic, Payload} | collect_topics(Ref, N - 1, Timeout)]
    after Timeout ->
        []
    end.
