%%%-------------------------------------------------------------------
%%% @doc Tests for macula_streamer's optional `handle_eof/1' callback
%%% (PLAN_PUSH_UPLOAD.md Phase 6) — a `client_stream' provider's last
%%% chance to set the stream's terminal reply before it stops. Split
%%% into its own file/callback module for the same reason
%%% `macula_streamer_client_stream_tests' is separate from
%%% `macula_streamer_tests': it needs a Module that genuinely exports
%%% `handle_eof/1', which the other two files' callback modules
%%% deliberately do not (the export check is module-wide, so exporting
%%% it there would change EVERY test in those files, not just an eof
%%% one).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_streamer_eof_reply_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_streamer).
-export([init/1, handle_open/2, handle_chunk/2, handle_eof/1, terminate/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_open(_StreamArgs, Parent) -> {ok, Parent}.

%% Exported (even though unused by these tests) purely so
%% `maybe_spawn_reader/2' spawns a reader at all — otherwise `eof'
%% never gets fetched via `macula:recv/2' and `handle_eof/1' never
%% fires.
handle_chunk(_Data, Parent) -> {noreply, Parent}.

%% The reply this test wants `handle_eof/1' to return is stashed via
%% `persistent_term' before the stream ever opens — simpler than
%% cross-process message coordination for a value known upfront.
handle_eof(Parent) ->
    Result = persistent_term:get({?MODULE, reply}),
    Parent ! eof_reached,
    {reply, Result, Parent}.

terminate(Reason, Parent) ->
    Parent ! {terminated, Reason},
    ok.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    meck:new(macula, [passthrough]),
    meck:expect(macula, advertise_stream,
                fun(_Pool, _Realm, _Proc, _Mode, Handler) ->
                    persistent_term:put({?MODULE, handler}, Handler), ok
                end),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    meck:new(macula_stream, [passthrough]),
    meck:expect(macula_stream, close, fun(_Stream) -> ok end),
    meck:expect(macula_stream, abort, fun(_Stream, _Code, _Message) -> ok end),
    meck:expect(macula_stream, set_reply, fun(_Stream, _Value) -> ok end),
    meck:expect(macula_stream, set_error, fun(_Stream, _Reason) -> ok end),
    ok.

teardown(_) ->
    persistent_term:erase({?MODULE, handler}),
    persistent_term:erase({?MODULE, reply}),
    meck:unload(macula_stream),
    meck:unload(macula).

captured_handler() -> persistent_term:get({?MODULE, handler}).

stream_stub() -> receive stop -> ok end.

recv_returning(Results) ->
    Counter = atomics:new(1, []),
    meck:expect(macula, recv, fun(_Stream, _Timeout) ->
        N = atomics:add_get(Counter, 1, 1),
        lists:nth(N, Results)
    end).

open_and_wait_for_eof(Reply) ->
    persistent_term:put({?MODULE, reply}, Reply),
    recv_returning([eof]),
    {ok, _Sup} = macula_streamer:advertise(pool, <<0:256>>, <<"bulk.ingest">>,
                                           ?MODULE, self(), #{mode => client_stream}),
    Handler = captured_handler(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    ?assertEqual(eof_reached, wait_msg()),
    StreamPid.

%%%===================================================================
%%% Tests
%%%===================================================================

eof_reply_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun ok_reply_sets_reply_then_closes/0,
      fun error_reply_sets_error_then_closes/0]}.

ok_reply_sets_reply_then_closes() ->
    process_flag(trap_exit, true),
    StreamPid = open_and_wait_for_eof({ok, <<"the-mcid">>}),
    ?assertEqual({terminated, normal}, wait_msg()),
    ?assertEqual(1, meck:num_calls(macula_stream, set_reply, [StreamPid, <<"the-mcid">>])),
    ?assertEqual(0, meck:num_calls(macula_stream, set_error, ['_', '_'])),
    ?assertEqual(1, meck:num_calls(macula_stream, close, [StreamPid])).

error_reply_sets_error_then_closes() ->
    process_flag(trap_exit, true),
    StreamPid = open_and_wait_for_eof({error, root_hash_mismatch}),
    ?assertEqual({terminated, normal}, wait_msg()),
    ?assertEqual(1, meck:num_calls(macula_stream, set_error, [StreamPid, root_hash_mismatch])),
    ?assertEqual(0, meck:num_calls(macula_stream, set_reply, ['_', '_'])),
    ?assertEqual(1, meck:num_calls(macula_stream, close, [StreamPid])).

%%%===================================================================
%%% Helpers
%%%===================================================================

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
