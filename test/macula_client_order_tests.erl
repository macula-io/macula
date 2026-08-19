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
    catch macula_client:unsubscribe(Pool, Ref),
    ok = macula_client:close(Pool).

inject(Pool, Topic, Pub, Seq) ->
    Pool ! {macula_event, make_ref(), Topic, Seq,
            #{realm => ?REALM, publisher => Pub, seq => Seq,
              delivered_via => direct}},
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
