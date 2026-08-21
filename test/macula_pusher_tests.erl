%%%-------------------------------------------------------------------
%%% @doc Tests for macula_pusher.
%%%
%%% Mocks at the `macula'/`macula_direct_dial'/`macula_stream'
%%% boundary — the raw streaming primitives this module drives
%%% directly (`call_stream', `send', `close_send', `await_reply',
%%% `abort'), the same layer `macula_streamer_tests'/
%%% `macula_stream_sink_tests' mock.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pusher_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_pusher).
-export([init/1, handle_pushed/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_pushed(Result, Parent) ->
    Parent ! {pushed, Result},
    {stop, normal, Parent}.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    meck:new(macula, [passthrough]),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    meck:new(macula_stream, [passthrough]),
    ok.

teardown(_) ->
    catch meck:unload(macula_direct_dial),
    meck:unload(macula_stream),
    meck:unload(macula),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

pusher_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun small_push_delivers_the_verified_result/0,
      fun chunked_push_sends_every_chunk_in_order/0,
      fun send_failure_bails_without_awaiting_reply/0,
      fun open_failure_still_announces_completion/0,
      fun cancel_before_resolve_announces_cancelled/0,
      fun cancel_reaches_the_real_stream_not_just_the_local_worker/0,
      fun direct_dial_pushes_through_the_resolved_provider/0]}.

small_push_delivers_the_verified_result() ->
    process_flag(trap_exit, true),
    Bytes = <<"small">>,
    Stream = dummy_pid(),
    {ok, Manifest, _Chunks} = macula_manifest:create(Bytes),
    Mcid = maps:get(mcid, Manifest),
    meck:expect(macula, call_stream, fun(_Pool, _Realm, _Proc, _Args, _Opts) -> {ok, Stream} end),
    meck:expect(macula, send, fun(_Stream, _Chunk) -> ok end),
    meck:expect(macula, close_send, fun(_Stream) -> ok end),
    meck:expect(macula, await_reply, fun(_Stream) -> {ok, Mcid} end),

    {ok, _Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), <<0:256>>,
                                          <<"bulk.ingest">>, Bytes, self()),
    ?assertEqual({pushed, {ok, Mcid}}, wait_msg()),
    ?assertEqual([<<"sharing.push_started_v1">>, <<"sharing.push_completed_v1">>], topics()),
    ?assertMatch(#{outcome := completed, mcid := Mcid}, completed_payload()).

chunked_push_sends_every_chunk_in_order() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    {ok, Manifest, Chunks} = macula_manifest:create(Bytes),
    Mcid = maps:get(mcid, Manifest),
    Stream = dummy_pid(),
    meck:expect(macula, call_stream, fun(_Pool, _Realm, _Proc, _Args, _Opts) -> {ok, Stream} end),
    meck:expect(macula, send, fun(_Stream, _Chunk) -> ok end),
    meck:expect(macula, close_send, fun(_Stream) -> ok end),
    meck:expect(macula, await_reply, fun(_Stream) -> {ok, Mcid} end),

    {ok, _Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), <<0:256>>,
                                          <<"bulk.ingest">>, Bytes, self()),
    ?assertEqual({pushed, {ok, Mcid}}, wait_msg()),
    SentChunks = [C || {_, {macula, send, [_S, C]}, ok} <- meck:history(macula)],
    ?assertEqual(Chunks, SentChunks).

send_failure_bails_without_awaiting_reply() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    Stream = dummy_pid(),
    meck:expect(macula, call_stream, fun(_Pool, _Realm, _Proc, _Args, _Opts) -> {ok, Stream} end),
    meck:expect(macula, send, fun(_Stream, _Chunk) -> {error, send_closed} end),
    meck:expect(macula, close_send, fun(_Stream) -> ok end),
    meck:expect(macula, await_reply, fun(_Stream) -> ?assert(false) end),

    {ok, _Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), <<0:256>>,
                                          <<"bulk.ingest">>, Bytes, self()),
    ?assertEqual({pushed, {error, send_closed}}, wait_msg()),
    ?assertEqual(0, meck:num_calls(macula, close_send, ['_'])),
    ?assertEqual(0, meck:num_calls(macula, await_reply, ['_'])).

open_failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    meck:expect(macula, call_stream,
               fun(_Pool, _Realm, _Proc, _Args, _Opts) -> {error, no_healthy_link} end),

    {ok, _Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), <<0:256>>,
                                          <<"bulk.ingest">>, <<"x">>, self()),
    ?assertEqual({pushed, {error, no_healthy_link}}, wait_msg()),
    ?assertMatch(#{outcome := failed, reason := no_healthy_link}, completed_payload()).

cancel_before_resolve_announces_cancelled() ->
    process_flag(trap_exit, true),
    Self = self(),
    meck:expect(macula, call_stream, fun(_Pool, _Realm, _Proc, _Args, _Opts) ->
        Self ! resolving,
        receive never -> ok after 5_000 -> ok end,
        {ok, dummy_pid()}
    end),

    {ok, Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), <<0:256>>,
                                         <<"bulk.ingest">>, <<"x">>, self()),
    ?assertEqual(resolving, wait_msg()),
    ok = macula_pusher:cancel(Pid),
    ?assertMatch(#{outcome := cancelled}, completed_payload()).

%% The actual point of holding `stream' in state: `cancel/1' must
%% reach all the way down to a real, peer-visible abort on the open
%% stream — not just kill the pusher's own local proxy process and
%% leave the peer to infer cancellation from the connection going
%% away (`macula_stream''s owner-death path is silent — see the
%% module doc).
cancel_reaches_the_real_stream_not_just_the_local_worker() ->
    process_flag(trap_exit, true),
    Self = self(),
    Stream = dummy_pid(),
    meck:expect(macula, call_stream, fun(_Pool, _Realm, _Proc, _Args, _Opts) -> {ok, Stream} end),
    meck:expect(macula, send, fun(_Stream, _Chunk) ->
        Self ! sending,
        receive never -> ok after 5_000 -> ok end,
        ok
    end),
    meck:expect(macula_stream, abort, fun(_Stream, _Code, _Message) -> ok end),

    {ok, Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), <<0:256>>,
                                         <<"bulk.ingest">>, <<"x">>, self()),
    ?assertEqual(sending, wait_msg()),
    ok = macula_pusher:cancel(Pid),
    ?assertEqual(1, meck:num_calls(macula_stream, abort,
                                   [Stream, <<"cancelled">>, '_'])).

%% start_link_direct resolves+dials the procedure's provider through
%% `macula_direct_dial:call_stream/5' as one step (unlike content
%% sharing's lower-level primitives, there is no separate resolve step
%% for this module to drive itself — see the module doc).
direct_dial_pushes_through_the_resolved_provider() ->
    process_flag(trap_exit, true),
    Bytes = <<"direct">>,
    {ok, Manifest, _Chunks} = macula_manifest:create(Bytes),
    Mcid = maps:get(mcid, Manifest),
    Stream = dummy_pid(),
    meck:new(macula_direct_dial, [passthrough]),
    meck:expect(macula_direct_dial, call_stream,
               fun(_Pool, _Realm, <<"bulk.ingest">>, _Args, #{mode := client_stream}) ->
                   {ok, Stream}
               end),
    meck:expect(macula, send, fun(_Stream, _Chunk) -> ok end),
    meck:expect(macula, close_send, fun(_Stream) -> ok end),
    meck:expect(macula, await_reply, fun(_Stream) -> {ok, Mcid} end),

    {ok, _Pid} = macula_pusher:start_link_direct(?MODULE, dummy_pid(), <<0:256>>,
                                                 <<"bulk.ingest">>, Bytes, self()),
    ?assertEqual({pushed, {ok, Mcid}}, wait_msg()).

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
              T =:= <<"sharing.push_completed_v1">>],
    Payload.

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
