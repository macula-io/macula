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
      fun direct_dial_resolves_then_fetches_from_the_resolved_provider/0]}.

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
