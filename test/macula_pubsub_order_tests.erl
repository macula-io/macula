%%% @doc Tests for macula_pubsub_order — the per-subscription delivery
%%% ordering used to put a deduped-but-scrambled pubsub stream back into
%%% per-publisher order. The event carried is just the seq integer, so
%%% assertions read as the delivered order directly.
-module(macula_pubsub_order_tests).

-include_lib("eunit/include/eunit.hrl").

-define(P, <<"publisher-a">>).
-define(Q, <<"publisher-b">>).

%%%===================================================================
%%% ordered
%%%===================================================================

%% In-order arrivals pass straight through, in order.
ordered_in_order_test() ->
    S0 = macula_pubsub_order:new(ordered),
    {D, _} = run(S0, [{?P, 5, 0}, {?P, 6, 0}, {?P, 7, 0}]),
    ?assertEqual([5, 6, 7], D).

%% Out-of-order arrivals are buffered and released in seq order once the
%% gap fills. This is the whole bug the module fixes.
ordered_reorders_test() ->
    S0 = macula_pubsub_order:new(ordered),
    %% arrive 5, 8, 7, 6 -> deliver 5, 6, 7, 8
    {D, _} = run(S0, [{?P, 5, 0}, {?P, 8, 1}, {?P, 7, 2}, {?P, 6, 3}]),
    ?assertEqual([5, 6, 7, 8], D).

%% The first seq seen is the base — the module does not assume 0 (seq is
%% seeded from wall-clock µs).
ordered_first_seq_is_base_test() ->
    S0 = macula_pubsub_order:new(ordered),
    {D, _} = run(S0, [{?P, 1000, 0}, {?P, 1001, 0}]),
    ?assertEqual([1000, 1001], D).

%% A late duplicate / already-past seq is dropped, not re-delivered.
ordered_drops_past_test() ->
    S0 = macula_pubsub_order:new(ordered),
    {D, _} = run(S0, [{?P, 5, 0}, {?P, 6, 0}, {?P, 5, 0}, {?P, 6, 0}]),
    ?assertEqual([5, 6], D).

%% Two publishers are independent seq streams; interleaving one does not
%% stall the other.
ordered_independent_publishers_test() ->
    S0 = macula_pubsub_order:new(ordered),
    {D, _} = run(S0, [{?P, 5, 0}, {?Q, 100, 0}, {?P, 6, 0}, {?Q, 101, 0}]),
    ?assertEqual([5, 100, 6, 101], D).

%% A genuinely missing seq is skipped after the timeout: the buffered
%% tail releases and the skip is counted.
ordered_flush_skips_gap_test() ->
    S0 = macula_pubsub_order:new(ordered),
    %% 5 delivered; 7,8 buffered (arrivals 1,2); 6 never arrives
    {D1, S1} = run(S0, [{?P, 5, 0}, {?P, 7, 1}, {?P, 8, 2}]),
    ?assertEqual([5], D1),
    ?assertEqual(2, macula_pubsub_order:buffered(S1)),
    %% before timeout: nothing releases
    {D2, S2} = macula_pubsub_order:flush(S1, 50, 100),
    ?assertEqual([], D2),
    %% after timeout (now - oldest_arrival >= 100): skip gap 6, drain 7,8
    {D3, S3} = macula_pubsub_order:flush(S2, 200, 100),
    ?assertEqual([7, 8], D3),
    ?assertEqual(0, macula_pubsub_order:buffered(S3)),
    ?assertEqual(1, macula_pubsub_order:skips(S3)).

%% A large forward jump is a publisher restart (seq re-based to µs): the
%% old expected counter is abandoned rather than waited on.
ordered_epoch_jump_rebases_test() ->
    S0 = macula_pubsub_order:new(ordered),
    {D, S1} = run(S0, [{?P, 5, 0}, {?P, 6, 0}, {?P, 1700000000, 0},
                       {?P, 1700000001, 0}]),
    ?assertEqual([5, 6, 1700000000, 1700000001], D),
    ?assertEqual(0, macula_pubsub_order:skips(S1)).

%% When the reorder buffer exceeds its count cap, the head gap is
%% skipped early rather than held unbounded (memory guard for a
%% high-rate publisher gapping).
ordered_cap_skips_when_buffer_full_test() ->
    S0 = macula_pubsub_order:new(ordered, 3),
    %% 1 delivered; 3,4,5 buffered (gap at 2 fills the cap); 6 overflows
    {D, S1} = run(S0, [{?P, 1, 0}, {?P, 3, 0}, {?P, 4, 0},
                       {?P, 5, 0}, {?P, 6, 0}]),
    ?assertEqual([1, 3, 4, 5, 6], D),
    ?assertEqual(1, macula_pubsub_order:skips(S1)),
    ?assertEqual(0, macula_pubsub_order:buffered(S1)).

%% A large BACKWARD jump is also a publisher restart: one whose seq
%% counter re-seeded from zero instead of wall-clock microseconds
%% (macula-station's own hecate_pubsub_server did exactly this before
%% 10.17.0). Without this clause every fact after such a restart reads
%% as "past" and is silently dropped until the counter climbs back over
%% the old watermark -- which is how a live read model (hecate-stations,
%% 2026-09-02) went deaf for 10+ hours after a fleet rollout while its
%% link, subscriptions and dedup all looked healthy. Rebase, deliver,
%% count no skip.
ordered_rewound_epoch_rebases_test() ->
    S0 = macula_pubsub_order:new(ordered),
    {D, S1} = run(S0, [{?P, 589000, 0}, {?P, 589001, 0},
                       {?P, 40, 0}, {?P, 41, 0}]),
    ?assertEqual([589000, 589001, 40, 41], D),
    ?assertEqual(0, macula_pubsub_order:skips(S1)).

%% Whatever the old epoch still had buffered is released (in seq order)
%% ahead of the first fact of the new epoch -- same as the forward-jump
%% case, so a restart never strands a buffered tail.
ordered_rewound_epoch_releases_buffered_tail_test() ->
    S0 = macula_pubsub_order:new(ordered),
    %% 500005 delivered; 500008, 500007 buffered behind a gap at 500006;
    %% then the rewind lands
    {D, S1} = run(S0, [{?P, 500005, 0}, {?P, 500008, 1}, {?P, 500007, 2},
                       {?P, 40, 3}]),
    ?assertEqual([500005, 500007, 500008, 40], D),
    ?assertEqual(0, macula_pubsub_order:buffered(S1)).

%% A backward step within the epoch-jump threshold is still a late
%% duplicate / already-past seq, not a restart: dropped, as before.
ordered_small_backstep_is_still_past_test() ->
    S0 = macula_pubsub_order:new(ordered),
    {D, _} = run(S0, [{?P, 20000, 0}, {?P, 20001, 0}, {?P, 15000, 0}]),
    ?assertEqual([20000, 20001], D).

%%%===================================================================
%%% latest_only
%%%===================================================================

%% Delivers strictly increasing seqs; a stale (lower) seq arriving after
%% a newer one is dropped. No buffering, no skip.
latest_only_drops_stale_test() ->
    S0 = macula_pubsub_order:new(latest_only),
    {D, S1} = run(S0, [{?P, 5, 0}, {?P, 8, 0}, {?P, 6, 0}, {?P, 9, 0}]),
    ?assertEqual([5, 8, 9], D),
    ?assertEqual(0, macula_pubsub_order:buffered(S1)),
    ?assertEqual(0, macula_pubsub_order:skips(S1)).

%% A large backward jump is a publisher restart with a re-seeded-from-
%% zero counter (see ordered_rewound_epoch_rebases_test): accept it as
%% the new high-water mark instead of dropping every fact until the
%% counter climbs back. A small backstep after the rebase is still stale.
latest_only_rewound_epoch_rebases_test() ->
    S0 = macula_pubsub_order:new(latest_only),
    {D, S1} = run(S0, [{?P, 589000, 0}, {?P, 589001, 0},
                       {?P, 40, 0}, {?P, 39, 0}, {?P, 41, 0}]),
    ?assertEqual([589000, 589001, 40, 41], D),
    ?assertEqual(0, macula_pubsub_order:skips(S1)).

%%%===================================================================
%%% as_arrives
%%%===================================================================

%% Delivers everything immediately in arrival order — no reordering,
%% no dropping, no buffering.
as_arrives_passthrough_test() ->
    S0 = macula_pubsub_order:new(as_arrives),
    {D, S1} = run(S0, [{?P, 5, 0}, {?P, 3, 0}, {?P, 8, 0}, {?P, 3, 0}]),
    ?assertEqual([5, 3, 8, 3], D),
    ?assertEqual(0, macula_pubsub_order:buffered(S1)).

%%%===================================================================
%%% helper — feed a list of {Publisher, Seq, ArrivalMs}, event = Seq
%%%===================================================================

run(S, Events) ->
    lists:foldl(fun({Pub, Seq, Now}, {Acc, St}) ->
        {Out, St2} = macula_pubsub_order:offer(St, Pub, Seq, Seq, Now),
        {Acc ++ Out, St2}
    end, {[], S}, Events).
