%%%-------------------------------------------------------------------
%%% @doc Tests for macula_streamer's optional `handle_eof/1' callback
%%% (PLAN_PUSH_UPLOAD.md Phase 6), a `client_stream' provider's last
%%% chance to set the stream's terminal reply before it stops. Split
%%% into its own file/callback module for the same reason
%%% `macula_streamer_client_stream_tests' is separate from
%%% `macula_streamer_tests': it needs a Module that genuinely exports
%%% `handle_eof/1', which the other two files' callback modules
%%% deliberately do not (the export check is module-wide, so exporting
%%% it there would change EVERY test in those files, not just an eof
%%% one). Each streamer runs on the functions macula_scripted_stream
%%% gives, so no test replaces a module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_streamer_eof_reply_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_streamer).
-export([init/1, handle_open/2, handle_chunk/2, handle_eof/1, terminate/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

%% The state is the test process and the reply handle_eof/1 returns.
init({Parent, Reply}) -> {ok, {Parent, Reply}}.

handle_open(_StreamArgs, State) -> {ok, State}.

%% Exported (even though unused by these tests) purely so
%% `maybe_spawn_reader/3' spawns a reader at all; otherwise `eof'
%% never gets fetched via `recv/2' and `handle_eof/1' never fires.
handle_chunk(_Data, State) -> {noreply, State}.

handle_eof({Parent, Reply} = State) ->
    Parent ! eof_reached,
    {reply, Reply, State}.

terminate(Reason, {Parent, _Reply}) ->
    Parent ! {terminated, Reason},
    ok.

stream_stub() -> receive stop -> ok end.

open_and_wait_for_eof(Reply) ->
    Opts = (macula_scripted_stream:options([eof]))#{mode => client_stream},
    {ok, _Sup} = macula_streamer:advertise(pool, <<0:256>>, <<"bulk.ingest">>,
                                           ?MODULE, {self(), Reply}, Opts),
    [{_, client_stream, Handler, _}] = macula_scripted_stream:advertised(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    ?assertEqual(eof_reached, wait_msg()),
    StreamPid.

%%%===================================================================
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
eof_reply_test_() ->
    [{spawn, Test} || Test <- [fun ok_reply_sets_reply_then_closes/0,
                                fun error_reply_sets_error_then_closes/0]].

ok_reply_sets_reply_then_closes() ->
    process_flag(trap_exit, true),
    StreamPid = open_and_wait_for_eof({ok, <<"the-mcid">>}),
    ?assertEqual({terminated, normal}, wait_msg()),
    ?assertEqual([{set_reply, [StreamPid, <<"the-mcid">>]}, {close, [StreamPid]}],
                 macula_scripted_stream:calls()).

error_reply_sets_error_then_closes() ->
    process_flag(trap_exit, true),
    StreamPid = open_and_wait_for_eof({error, root_hash_mismatch}),
    ?assertEqual({terminated, normal}, wait_msg()),
    ?assertEqual([{set_error, [StreamPid, root_hash_mismatch]}, {close, [StreamPid]}],
                 macula_scripted_stream:calls()).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% The next message from the streamer's callbacks.
wait_msg() ->
    receive
        eof_reached = Msg -> Msg;
        {terminated, _} = Msg -> Msg
    after 1000 -> timeout
    end.
