%%%-------------------------------------------------------------------
%%% @doc Tests for macula_content_transfer's Phase 3 — multi-stream
%%% parallel chunk transfer (PLAN_PUSH_UPLOAD.md).
%%%
%%% Split out from `macula_content_transfer_tests' because this suite
%%% needs GENUINELY DISTINCT stream references per `open_content_stream'
%%% call (`fresh_stream_mock/0') to exercise concurrency at all — the
%%% Phase 1-2 suite's fixed-single-`Stream' mock is deliberately too
%%% simple for that and stays that way (those tests pin
%%% `stream_count => 1', where one shared stream ref is correct).
%%%
%%% The core thing every test here has to prove isn't "N streams get
%%% opened" (necessary but not sufficient) — it's that N chunk calls
%%% are GENUINELY in flight at once: collecting N `call_started' events
%%% from `blocking_call_on_stream/1' before releasing ANY of them only
%%% succeeds if the implementation actually dispatched them
%%% concurrently, not one at a time. A sequential implementation would
%%% time out waiting for the 2nd event.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_transfer_multi_stream_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    meck:new(macula_client, [passthrough]),
    meck:new(macula_station_link, [passthrough]),
    ok.

teardown(_) ->
    meck:unload(macula_station_link),
    meck:unload(macula_client),
    ok.

multi_stream_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun put_dispatches_chunks_concurrently_across_streams/0,
      fun get_reassembles_correctly_regardless_of_arrival_order/0,
      fun stream_count_is_capped_by_chunk_count/0,
      fun stream_open_failure_degrades_to_fewer_streams/0,
      fun single_failed_chunk_fails_whole_transfer_and_kills_other_lanes/0,
      fun cancel_mid_transfer_aborts_every_open_stream/0]}.

%%%===================================================================
%%% Tests
%%%===================================================================

%% The core proof: with 4 streams and exactly 4 chunks (one per lane),
%% all 4 chunk calls must already be blocked in flight SIMULTANEOUSLY —
%% none released yet — before this even attempts to collect them.
put_dispatches_chunks_concurrently_across_streams() ->
    Self = self(),
    N = 4,
    Bytes = chunked_bytes_for_n_chunks(N),
    {ok, ExpectedManifest, _Chunks} = macula_manifest:create(Bytes),
    ExpectedMcid = maps:get(mcid, ExpectedManifest),
    LinkPid = dummy_pid(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fresh_stream_mock()),
    meck:expect(macula_station_link, call_on_stream, blocking_call_on_stream(Self)),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), Bytes, #{stream_count => N}),

    Starts = [receive_call_started() || _ <- lists:seq(1, N)],
    [?assertEqual(<<"_content.put_block">>, Proc) || {_W, Proc, _Payload} <- Starts],
    %% N genuinely different chunks, not the same one N times.
    Mcids = [maps:get(mcid, Payload) || {_W, _Proc, Payload} <- Starts],
    ?assertEqual(N, length(lists:usort(Mcids))),

    [Worker ! {proceed, {ok, ok}} || {Worker, _, _} <- Starts],
    {WorkerM, <<"_content.put_manifest">>, _} = receive_call_started(),
    WorkerM ! {proceed, {ok, ok}},

    ?assertEqual({ok, ExpectedMcid}, macula_content_transfer:await(Pid)),
    ?assertEqual(N, meck:num_calls(macula_station_link, open_content_stream, '_')),
    ok = macula_content_transfer:cancel(Pid).

%% `acc' is keyed by chunk index, not arrival order — releasing chunks
%% in the REVERSE of whatever order their calls happened to start in
%% (itself scheduler-dependent, not something this test controls) must
%% still reassemble the exact original bytes.
get_reassembles_correctly_regardless_of_arrival_order() ->
    Self = self(),
    N = 4,
    OriginalBytes = chunked_bytes_for_n_chunks(N),
    {ok, Manifest, Chunks} = macula_manifest:create(OriginalBytes),
    Mcid = maps:get(mcid, Manifest),
    ChunkByMcid = chunk_mcid_map(Manifest, Chunks),
    LinkPid = dummy_pid(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fresh_stream_mock()),
    meck:expect(macula_station_link, call_on_stream, blocking_call_on_stream(Self)),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_get(dummy_pid(), Mcid, #{stream_count => N}),

    {WorkerM, <<"_content.get_manifest">>, #{mcid := Mcid}} = receive_call_started(),
    WorkerM ! {proceed, {ok, Manifest}},

    Starts = [receive_call_started() || _ <- lists:seq(1, N)],
    [Worker ! {proceed, {ok, maps:get(ChunkMcid, ChunkByMcid)}}
     || {Worker, _Proc, #{mcid := ChunkMcid}} <- lists:reverse(Starts)],

    ?assertEqual({ok, OriginalBytes}, macula_content_transfer:await(Pid)),
    ok = macula_content_transfer:cancel(Pid).

%% Only 2 chunks, but 5 streams requested — must never open more
%% streams than there is work to spread across them.
stream_count_is_capped_by_chunk_count() ->
    Self = self(),
    Bytes = chunked_bytes_for_n_chunks(2),
    LinkPid = dummy_pid(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fresh_stream_mock()),
    meck:expect(macula_station_link, call_on_stream, blocking_call_on_stream(Self)),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), Bytes, #{stream_count => 5}),

    Starts = [receive_call_started() || _ <- lists:seq(1, 2)],
    %% A 3rd chunk call starting would mean more streams/lanes exist
    %% than there is work for — exactly what the cap must prevent.
    assert_no_call_started(),
    [Worker ! {proceed, {ok, ok}} || {Worker, _, _} <- Starts],
    {WorkerM, <<"_content.put_manifest">>, _} = receive_call_started(),
    WorkerM ! {proceed, {ok, ok}},

    {ok, _Mcid} = macula_content_transfer:await(Pid),
    ?assertEqual(2, meck:num_calls(macula_station_link, open_content_stream, '_')),
    ok = macula_content_transfer:cancel(Pid).

%% Opening an extra stream is best-effort — the connect step's ONE
%% stream always succeeds (this repo's existing tests already cover
%% "no healthy link at all" failing the whole transfer), but if every
%% ADDITIONAL open fails, the transfer must still complete correctly,
%% just serialized onto the one stream it actually got.
stream_open_failure_degrades_to_fewer_streams() ->
    Self = self(),
    N = 3,
    Bytes = chunked_bytes_for_n_chunks(N),
    {ok, ExpectedManifest, _Chunks} = macula_manifest:create(Bytes),
    ExpectedMcid = maps:get(mcid, ExpectedManifest),
    LinkPid = dummy_pid(),
    OpenCount = counters:new(1, []),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) ->
        case counters:get(OpenCount, 1) of
            0 -> counters:add(OpenCount, 1, 1), {ok, make_ref()};
            _ -> {error, no_healthy_link}
        end
    end),
    meck:expect(macula_station_link, call_on_stream, blocking_call_on_stream(Self)),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), Bytes, #{stream_count => N}),

    %% Down to one real stream — chunks arrive strictly one at a time.
    {W0, <<"_content.put_block">>, _} = receive_call_started(),
    assert_no_call_started(),
    W0 ! {proceed, {ok, ok}},
    {W1, <<"_content.put_block">>, _} = receive_call_started(),
    assert_no_call_started(),
    W1 ! {proceed, {ok, ok}},
    {W2, <<"_content.put_block">>, _} = receive_call_started(),
    W2 ! {proceed, {ok, ok}},
    {WM, <<"_content.put_manifest">>, _} = receive_call_started(),
    WM ! {proceed, {ok, ok}},

    ?assertEqual({ok, ExpectedMcid}, macula_content_transfer:await(Pid)),
    ok = macula_content_transfer:cancel(Pid).

%% One lane's chunk genuinely fails (a real `{error, _}', not a crash)
%% — the whole transfer fails with it, and every OTHER lane's in-flight
%% worker is killed rather than left running to no purpose.
single_failed_chunk_fails_whole_transfer_and_kills_other_lanes() ->
    Self = self(),
    N = 3,
    Bytes = chunked_bytes_for_n_chunks(N),
    LinkPid = dummy_pid(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fresh_stream_mock()),
    meck:expect(macula_station_link, call_on_stream, blocking_call_on_stream(Self)),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), Bytes, #{stream_count => N}),

    Starts = [receive_call_started() || _ <- lists:seq(1, N)],
    [{FirstWorker, _, _} | Rest] = Starts,
    Monitors = [erlang:monitor(process, W) || {W, _, _} <- Rest],

    FirstWorker ! {proceed, {ok, hash_mismatch}},

    ?assertEqual({error, hash_mismatch}, macula_content_transfer:await(Pid)),
    [receive
         {'DOWN', Mon, process, _, _} -> ok
     after 2_000 ->
         erlang:error(lane_worker_not_killed)
     end || Mon <- Monitors],
    ok = macula_content_transfer:cancel(Pid).

%% Cancelling while N lanes each have a chunk in flight must reset
%% EVERY open stream, not just one.
cancel_mid_transfer_aborts_every_open_stream() ->
    Self = self(),
    N = 3,
    Bytes = chunked_bytes_for_n_chunks(N),
    LinkPid = dummy_pid(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fresh_stream_mock()),
    meck:expect(macula_station_link, call_on_stream, blocking_call_on_stream(Self)),
    meck:expect(macula_station_link, abort_content_stream, fun(_, _, _, _) -> ok end),

    {ok, Pid} = macula_content_transfer:start_put(dummy_pid(), Bytes, #{stream_count => N}),
    _Starts = [receive_call_started() || _ <- lists:seq(1, N)],

    ok = macula_content_transfer:cancel(Pid, 5, <<"multi-abort">>),

    AbortedStreams = [S || {_, {macula_station_link, abort_content_stream,
                                [_LinkPid, S, 5, <<"multi-abort">>]}, ok}
                           <- meck:history(macula_station_link)],
    ?assertEqual(N, length(lists:usort(AbortedStreams))).

%%%===================================================================
%%% Helpers
%%%===================================================================

dummy_pid() ->
    spawn(fun() -> receive stop -> ok end end).

chunked_bytes_for_n_chunks(N) ->
    crypto:strong_rand_bytes(N * macula_manifest:default_chunk_size()).

%% Unlike the Phase 1-2 suite's fixed single stream, every call here
%% must return a genuinely distinct reference — otherwise lane lookup
%% by stream (`lists:keyfind(Stream, #lane.stream, Lanes)') couldn't
%% tell one concurrent lane's stream from another's.
fresh_stream_mock() ->
    fun(_LinkPid) -> {ok, make_ref()} end.

%% A `call_on_stream' mock that blocks until the test explicitly
%% releases it with a chosen reply — deterministic synchronization:
%% collecting N `call_started' events before releasing any of them only
%% succeeds if N calls are genuinely in flight at once.
blocking_call_on_stream(Self) ->
    fun(_LinkPid, _Stream, _Realm, Proc, Payload, _Tmo) ->
        Self ! {call_started, self(), Proc, Payload},
        receive {proceed, Reply} -> Reply end
    end.

receive_call_started() ->
    receive
        {call_started, Worker, Proc, Payload} -> {Worker, Proc, Payload}
    after 2_000 ->
        erlang:error(no_call_started)
    end.

assert_no_call_started() ->
    receive
        {call_started, _, Proc, _} -> erlang:error({unexpected_call_started, Proc})
    after 300 ->
        ok
    end.

%% ChunkMcid => chunk bytes, for a get test's mocked `_content.get_block'
%% to answer with the right bytes regardless of fetch order.
chunk_mcid_map(Manifest, Chunks) ->
    Indices = lists:seq(0, length(Chunks) - 1),
    maps:from_list([begin
        {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, I, blake3),
        {ChunkMcid, C}
    end || {I, C} <- lists:zip(Indices, Chunks)]).
