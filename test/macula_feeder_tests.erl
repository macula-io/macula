%%%-------------------------------------------------------------------
%%% @doc Tests for macula_feeder.
%%%
%%% Mocks at the `macula_client'/`macula_station_link' boundary — the
%%% same layer `macula_content_transfer_tests' mocks, since
%%% PLAN_PUSH_UPLOAD.md Phase 4 moved this module off a blocking
%%% `macula:put_content/2' call and onto `macula_content_transfer'
%%% directly. `Pool' must be a real pid here (not the placeholder atom
%%% `pool' the pre-Phase-4 version of this suite used) — it's now
%%% threaded all the way down to `macula_content_transfer:start_put/3',
%%% whose own guard requires one.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_feeder_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SINGLE_CODEC, 16#55).

-behaviour(macula_feeder).
-export([init/1, handle_fed/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_fed(Result, Parent) ->
    Parent ! {fed, Result},
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

feeder_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun small_put_reports_unchunked/0,
      fun large_put_reports_chunked/0,
      fun failure_still_announces_completion/0,
      fun cancel_before_put_resolves_announces_cancelled/0,
      fun cancel_reaches_the_real_content_transfer_not_just_the_local_worker/0,
      fun direct_dial_resolves_then_puts_through_the_resolved_station/0]}.

small_put_reports_unchunked() ->
    process_flag(trap_exit, true),
    Bytes = <<"small">>,
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream,
               fun(_, _, _, <<"_content.put_block">>, _, _) -> {ok, ok} end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, _Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), <<0:256>>, Bytes, self()),
    Hash = macula_blake3_nif:hash(Bytes),
    ExpectedMcid = <<1, ?SINGLE_CODEC, Hash/binary>>,
    ?assertEqual({fed, {ok, ExpectedMcid}}, wait_msg()),
    ?assertEqual([<<"sharing.put_started_v1">>, <<"sharing.put_completed_v1">>], topics()),
    ?assertMatch(#{outcome := completed, mcid := ExpectedMcid, chunked := false},
                 completed_payload()).

large_put_reports_chunked() ->
    process_flag(trap_exit, true),
    %% 3 chunks — macula_content_transfer's default stream_count (4,
    %% Phase 3) opens one dedicated stream per chunk here, so this
    %% mock must hand back a genuinely DISTINCT ref each call, same as
    %% macula_content_transfer_multi_stream_tests' fresh_stream_mock/0
    %% — a fixed shared ref would collide across lanes.
    Bytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    {ok, ExpectedManifest, _Chunks} = macula_manifest:create(Bytes),
    ExpectedMcid = maps:get(mcid, ExpectedManifest),
    LinkPid = dummy_pid(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, make_ref()} end),
    meck:expect(macula_station_link, call_on_stream, fun(_, _, _, _, _, _) -> {ok, ok} end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, _Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), <<0:256>>, Bytes, self()),
    ?assertEqual({fed, {ok, ExpectedMcid}}, wait_msg()),
    ?assertMatch(#{outcome := completed, mcid := ExpectedMcid, chunked := true},
                 completed_payload()).

failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {error, no_healthy_link} end),

    {ok, _Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), <<0:256>>, <<"x">>, self()),
    ?assertEqual({fed, {error, no_healthy_link}}, wait_msg()),
    ?assertMatch(#{outcome := failed, reason := no_healthy_link}, completed_payload()).

cancel_before_put_resolves_announces_cancelled() ->
    process_flag(trap_exit, true),
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:expect(macula_client, pick_connected_link, fun(_Pool) -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream, fun(_, _, _, _, _, _) ->
        Self ! put_started,
        receive never -> ok after 5_000 -> ok end,
        {ok, ok}
    end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),
    meck:expect(macula_station_link, abort_content_stream, fun(_, _, _, _) -> ok end),

    {ok, Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), <<0:256>>, <<"x">>, self()),
    ?assertEqual(put_started, wait_msg()),
    ok = macula_feeder:cancel(Pid),
    ?assertMatch(#{outcome := cancelled}, completed_payload()).

%% The actual point of Phase 4: `cancel/1' on the feeder must reach
%% all the way down to a real, peer-visible abort on the open content
%% stream — not just kill the feeder's own local proxy process and
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
        Self ! put_started,
        receive never -> ok after 5_000 -> ok end,
        {ok, ok}
    end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),
    meck:expect(macula_station_link, abort_content_stream, fun(_, _, _, _) -> ok end),

    {ok, Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), <<0:256>>, <<"x">>, self()),
    ?assertEqual(put_started, wait_msg()),
    ok = macula_feeder:cancel(Pid),
    ?assertEqual(1, meck:num_calls(macula_station_link, abort_content_stream,
                                   [LinkPid, Stream, '_', '_'])).

%% start_link_direct resolves the station's endpoint (a plain blocking
%% DHT lookup, unchanged in shape from before this phase) and THEN
%% puts through macula_content_transfer:start_put_station/5, dialing
%% exactly that resolved endpoint.
direct_dial_resolves_then_puts_through_the_resolved_station() ->
    process_flag(trap_exit, true),
    Bytes = <<"direct">>,
    Station = crypto:strong_rand_bytes(32),
    DialUrl = <<"quic://station.example:4433">>,
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    meck:new(macula_direct_dial, [passthrough]),
    meck:expect(macula_direct_dial, resolve_station_endpoint,
               fun(_Pool, Station0) when Station0 =:= Station -> {ok, DialUrl} end),
    meck:expect(macula_client, ensure_content_link,
               fun(_Pool, Seed, _LinkOpts, _TimeoutMs) when Seed =:= DialUrl -> {ok, LinkPid} end),
    meck:expect(macula_station_link, open_content_stream, fun(_LinkPid) -> {ok, Stream} end),
    meck:expect(macula_station_link, call_on_stream,
               fun(_, _, _, <<"_content.put_block">>, _, _) -> {ok, ok} end),
    meck:expect(macula_station_link, close_content_stream, fun(_, _) -> ok end),

    {ok, _Pid} = macula_feeder:start_link_direct(?MODULE, dummy_pid(), Station,
                                                 <<0:256>>, Bytes, self()),
    Hash = macula_blake3_nif:hash(Bytes),
    ExpectedMcid = <<1, ?SINGLE_CODEC, Hash/binary>>,
    ?assertEqual({fed, {ok, ExpectedMcid}}, wait_msg()),
    meck:unload(macula_direct_dial).

%%%===================================================================
%%% Helpers
%%%===================================================================

dummy_pid() ->
    spawn(fun() -> receive stop -> ok end end).

topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, ok} <- meck:history(macula)].

completed_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"sharing.put_completed_v1">>],
    Payload.

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
