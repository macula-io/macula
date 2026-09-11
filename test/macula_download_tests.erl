%%%-------------------------------------------------------------------
%%% @doc Tests for macula_download.
%%%
%%% Mocks at the `macula_client'/`macula_station_link' boundary — the
%%% same layer `macula_content_transfer_tests' mocks, since
%%% PLAN_PUSH_UPLOAD.md Phase 4 moved this module off a blocking
%%% `macula:get_content/2' call and onto `macula_content_transfer'
%%% directly. `Pool' must be a real pid here (not the placeholder atom
%%% `pool' the pre-Phase-4 version of this suite used) — it's now
%%% threaded all the way down to `macula_content_transfer:start_get/3',
%%% whose own guard requires one.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_download_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SINGLE_MCID, <<1, 16#55, 0:256>>).
-define(MANIFEST_MCID, <<1, 16#56, 0:256>>).

-behaviour(macula_download).
-export([init/1, handle_downloaded/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_downloaded(Result, Parent) ->
    Parent ! {downloaded, Result},
    {stop, normal, Parent}.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    meck:new(macula, [passthrough]),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    meck:new(macula_client, [passthrough]),
    meck:new(macula_station_link, [passthrough]),
    ok.

teardown(_) ->
    meck:unload(macula_station_link),
    meck:unload(macula_client),
    meck:unload(macula),
    ok.

handover_setup() ->
    ok = setup(),
    meck:new(macula_content_transfer, [passthrough]),
    meck:new(macula_direct_dial, [passthrough]),
    ok.

handover_teardown(Arg) ->
    meck:unload(macula_direct_dial),
    meck:unload(macula_content_transfer),
    teardown(Arg).

%%%===================================================================
%%% Tests
%%%===================================================================

download_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun single_block_get_reports_unchunked/0,
      fun manifest_get_reports_chunked/0,
      fun failure_still_announces_completion/0,
      fun cancel_before_get_resolves_announces_cancelled/0,
      fun cancel_reaches_the_real_content_transfer_not_just_the_local_worker/0,
      fun direct_dial_resolves_then_fetches_from_the_resolved_provider/0,
      fun a_malformed_mcid_is_rejected_before_anything_is_spawned/0,
      fun direct_dial_also_rejects_a_malformed_mcid/0]}.

%% A cancel that lands after the transfer has started, and before the
%% download holds the transfer's pid, still cancels the transfer. The start
%% is held at that point, in whichever process runs it, until the cancel
%% has reached the download.
handover_test_() ->
    {foreach, fun handover_setup/0, fun handover_teardown/1,
     [fun cancel_while_the_transfer_is_handed_over_still_reaches_it/0,
      fun cancel_while_a_direct_transfer_is_handed_over_still_reaches_it/0]}.

%% Regression: a share link or caller-supplied Mcid that doesn't carry
%% either codec byte macula:put_content/2 ever mints used to reach
%% macula_content_transfer's is_chunked/2, whose clauses assume that
%% shape and crash the spawned worker with a FunctionClauseError. This
%% pins the rejection at init/1, before the worker (or the started_v1
%% announce) ever exists.
a_malformed_mcid_is_rejected_before_anything_is_spawned() ->
    %% `init/1' returning `{stop, Reason}' acks this process cleanly
    %% with `{error, Reason}' AND, separately, an untrapped exit signal
    %% follows once the failed gen_server actually terminates — trap
    %% it like every other test here that calls start_link*/5.
    process_flag(trap_exit, true),
    Result = macula_download:start_link(?MODULE, dummy_pid(), <<0:256>>,
                                        <<"not-an-mcid">>, self()),
    ?assertEqual({error, invalid_mcid}, Result),
    ?assertEqual([], topics()).

direct_dial_also_rejects_a_malformed_mcid() ->
    process_flag(trap_exit, true),
    Result = macula_download:start_link_direct(?MODULE, dummy_pid(), <<0:256>>,
                                               <<"not-an-mcid">>, self()),
    ?assertEqual({error, invalid_mcid}, Result),
    ?assertEqual([], topics()).

single_block_get_reports_unchunked() ->
    process_flag(trap_exit, true),
    Bytes = <<"bytes">>,
    Hash = macula_blake3_nif:hash(Bytes),
    Mcid = <<1, 16#55, Hash/binary>>,
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream,
               fun(_, _, _, <<"_content.get_block">>, _, _) -> {ok, Bytes} end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, _Pid} = macula_download:start_link(?MODULE, dummy_pid(), <<0:256>>, Mcid, self()),
    ?assertEqual({downloaded, {ok, Bytes}}, wait_msg()),
    ?assertEqual([<<"sharing.get_started_v1">>, <<"sharing.get_completed_v1">>], topics()),
    ?assertMatch(#{outcome := completed, size := 5}, completed_payload()),
    ?assertMatch(#{mcid := Mcid, chunked := false}, started_payload()).

manifest_get_reports_chunked() ->
    process_flag(trap_exit, true),
    %% 3 chunks — macula_content_transfer's default stream_count (4,
    %% Phase 3) opens one dedicated stream per chunk here, so this
    %% mock must hand back a genuinely DISTINCT ref each call, same as
    %% macula_content_transfer_multi_stream_tests' fresh_stream_mock/0
    %% — a fixed shared ref would collide across lanes.
    OriginalBytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    {ok, Manifest, Chunks} = macula_manifest:create(OriginalBytes),
    Mcid = maps:get(mcid, Manifest),
    ChunkByMcid = chunk_mcid_map(Manifest, Chunks),
    LinkPid = dummy_pid(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, make_ref()} end),
    meck:expect(macula_station_link, call_on_stream, fun
        (_, _, _, <<"_content.get_manifest">>, _Payload, _) -> {ok, Manifest};
        (_, _, _, <<"_content.get_block">>, #{mcid := ChunkMcid}, _) ->
            {ok, maps:get(ChunkMcid, ChunkByMcid)}
    end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, _Pid} = macula_download:start_link(?MODULE, dummy_pid(), <<0:256>>, Mcid, self()),
    ?assertEqual({downloaded, {ok, OriginalBytes}}, wait_msg()),
    ?assertMatch(#{mcid := Mcid, chunked := true}, started_payload()).

failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {error, no_healthy_link} end),

    {ok, _Pid} = macula_download:start_link(?MODULE, dummy_pid(), <<0:256>>, ?SINGLE_MCID, self()),
    ?assertEqual({downloaded, {error, no_healthy_link}}, wait_msg()),
    ?assertMatch(#{outcome := failed, reason := no_healthy_link}, completed_payload()).

cancel_before_get_resolves_announces_cancelled() ->
    process_flag(trap_exit, true),
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream, fun(_, _, _, _, _, _) ->
        Self ! get_started,
        receive never -> ok after 5_000 -> ok end,
        {ok, <<"too late">>}
    end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),
    meck:expect(macula_station_link, abort_content_stream, fun(_, _, _, _) -> ok end),

    {ok, Pid} = macula_download:start_link(?MODULE, dummy_pid(), <<0:256>>, ?SINGLE_MCID, self()),
    ?assertEqual(get_started, wait_msg()),
    ok = macula_download:cancel(Pid),
    ?assertMatch(#{outcome := cancelled}, completed_payload()).

%% The actual point of Phase 4: `cancel/1' on the download must reach
%% all the way down to a real, peer-visible abort on the open content
%% stream — not just kill the download's own local proxy process and
%% leave the underlying macula_content_transfer running unnoticed
%% (which is exactly what happened before this phase: nothing links a
%% gen_server:call caller's death to the callee it was waiting on).
cancel_reaches_the_real_content_transfer_not_just_the_local_worker() ->
    process_flag(trap_exit, true),
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream, fun(_, _, _, _, _, _) ->
        Self ! get_started,
        receive never -> ok after 5_000 -> ok end,
        {ok, <<"too late">>}
    end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),
    meck:expect(macula_station_link, abort_content_stream, fun(_, _, _, _) -> ok end),

    {ok, Pid} = macula_download:start_link(?MODULE, dummy_pid(), <<0:256>>, ?SINGLE_MCID, self()),
    ?assertEqual(get_started, wait_msg()),
    ok = macula_download:cancel(Pid),
    ?assertEqual(1, meck:num_calls(macula_station_link, abort_content_stream,
                                   [LinkPid, Stream, '_', '_'])).

%% start_link_direct resolves the MCID's provider (a plain blocking
%% DHT lookup, unchanged in shape from before this phase) and THEN
%% fetches through macula_content_transfer:start_get_station/5,
%% dialing exactly that resolved provider.
direct_dial_resolves_then_fetches_from_the_resolved_provider() ->
    process_flag(trap_exit, true),
    Bytes = <<"direct fetch">>,
    Hash = macula_blake3_nif:hash(Bytes),
    Mcid = <<1, 16#55, Hash/binary>>,
    Node = crypto:strong_rand_bytes(32),
    Endpoint = <<"quic://provider.example:4433">>,
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:new(macula_direct_dial, [passthrough]),
    meck:expect(macula_direct_dial, resolve_content_provider,
               fun(_Pool, Mcid0) when Mcid0 =:= Mcid ->
                   {ok, #{announcer_node => Node, endpoint => Endpoint}}
               end),
    meck:expect(macula_client, ensure_content_link,
               fun(_Pool, Seed, _LinkOpts, _TimeoutMs) when Seed =:= Endpoint -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream,
               fun(_, _, _, <<"_content.get_block">>, _, _) -> {ok, Bytes} end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, _Pid} = macula_download:start_link_direct(?MODULE, dummy_pid(), <<0:256>>, Mcid, self()),
    ?assertEqual({downloaded, {ok, Bytes}}, wait_msg()),
    meck:unload(macula_direct_dial).

cancel_while_the_transfer_is_handed_over_still_reaches_it() ->
    process_flag(trap_exit, true),
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    expect_a_get_that_stays_open(Self, Stream),
    meck:expect(macula_content_transfer, start_get,
                fun(Pool, Mcid, Opts) ->
                    hold_handover(Self, meck:passthrough([Pool, Mcid, Opts]))
                end),

    {ok, Pid} = macula_download:start_link(?MODULE, dummy_pid(), <<0:256>>, ?SINGLE_MCID, self()),
    cancel_during_handover(Pid),
    ?assertEqual(1, meck:num_calls(macula_station_link, abort_content_stream,
                                   [LinkPid, Stream, '_', '_'])).

cancel_while_a_direct_transfer_is_handed_over_still_reaches_it() ->
    process_flag(trap_exit, true),
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    Endpoint = <<"quic://provider.example:4433">>,
    meck:expect(macula_direct_dial, resolve_content_provider,
               fun(_Pool, _Mcid) ->
                   {ok, #{announcer_node => crypto:strong_rand_bytes(32), endpoint => Endpoint}}
               end),
    meck:expect(macula_client, ensure_content_link,
               fun(_Pool, Seed, _LinkOpts, _TimeoutMs) when Seed =:= Endpoint -> {ok, LinkPid} end),
    expect_a_get_that_stays_open(Self, Stream),
    meck:expect(macula_content_transfer, start_get_station,
                fun(Pool, Station, Mcid, TimeoutMs, Opts) ->
                    hold_handover(Self, meck:passthrough([Pool, Station, Mcid, TimeoutMs, Opts]))
                end),

    {ok, Pid} = macula_download:start_link_direct(?MODULE, dummy_pid(), <<0:256>>, ?SINGLE_MCID, self()),
    cancel_during_handover(Pid),
    ?assertEqual(1, meck:num_calls(macula_station_link, abort_content_stream,
                                   [LinkPid, Stream, '_', '_'])).

%%%===================================================================
%%% Helpers
%%%===================================================================

dummy_pid() ->
    spawn(fun() -> receive stop -> ok end end).

topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, ok} <- meck:history(macula)].

started_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"sharing.get_started_v1">>],
    Payload.

completed_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"sharing.get_completed_v1">>],
    Payload.

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.

%% ChunkMcid => chunk bytes, for a get test's mocked `_content.get_block'
%% to answer with the right bytes regardless of fetch order.
chunk_mcid_map(Manifest, Chunks) ->
    Indices = lists:seq(0, length(Chunks) - 1),
    maps:from_list([begin
        {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, I, blake3),
        {ChunkMcid, C}
    end || {I, C} <- lists:zip(Indices, Chunks)]).

expect_a_get_that_stays_open(Self, Stream) ->
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream, fun(_, _, _, _, _, _) ->
        Self ! get_started,
        receive never -> ok after 5_000 -> ok end,
        {ok, <<"too late">>}
    end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),
    meck:expect(macula_station_link, abort_content_stream, fun(_, _, _, _) -> ok end).

%% Runs in whichever process starts the transfer: report, then keep the
%% start from returning until the test lets it.
hold_handover(Test, Started) ->
    Test ! {transfer_started, self()},
    receive release_handover -> Started end.

%% With the transfer started and its stream open, cancel the download, and let
%% the held start return only once the cancel has reached the download:
%% handled already, or queued behind the start.
cancel_during_handover(Pid) ->
    Self = self(),
    Holder = receive {transfer_started, H} -> H after 1_000 -> error(transfer_not_started) end,
    receive get_started -> ok after 1_000 -> error(stream_not_open) end,
    _ = spawn_link(fun() -> Self ! {cancelled, macula_download:cancel(Pid)} end),
    ok = wait_until_cancel_reached(Pid, 5_000),
    Holder ! release_handover,
    ?assertEqual({cancelled, ok},
                 receive {cancelled, _} = Cancelled -> Cancelled after 5_000 -> timeout end).

wait_until_cancel_reached(Pid, BudgetMs) ->
    cancel_reached(erlang:process_info(Pid, messages), Pid, BudgetMs).

cancel_reached(undefined, _Pid, _BudgetMs) ->
    ok;
cancel_reached({messages, Messages}, Pid, BudgetMs) ->
    stop_queued(lists:any(fun is_stop_request/1, Messages), Pid, BudgetMs).

stop_queued(true, _Pid, _BudgetMs) ->
    ok;
stop_queued(false, _Pid, BudgetMs) when BudgetMs =< 0 ->
    timeout;
stop_queued(false, Pid, BudgetMs) ->
    timer:sleep(1),
    wait_until_cancel_reached(Pid, BudgetMs - 1).

is_stop_request({system, _From, {terminate, _Reason}}) -> true;
is_stop_request(_Message) -> false.
