%%%-------------------------------------------------------------------
%%% @doc Tests for macula_streamer's optional `client_stream' receive
%%% loop (`handle_chunk/2'). Split into its own file/callback module
%%% because it needs a Module that genuinely exports `handle_chunk/2'
%%% — unlike `macula_streamer_tests''s deliberately simpler
%%% `server_stream'-shaped callback module, which must NOT export it
%%% (`maybe_spawn_reader/2' gates the reader on
%%% `erlang:function_exported/3', module-wide — exporting it there
%%% would spawn a reader for every one of that file's tests too).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_streamer_client_stream_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_streamer).
-export([init/1, handle_open/2, handle_chunk/2, terminate/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_open(_StreamArgs, Parent) ->
    Parent ! {opened, self()},
    {ok, Parent}.

handle_chunk(Data, Parent) ->
    Parent ! {chunk_seen, Data},
    {noreply, Parent}.

%% `terminate/2' is exported purely as a synchronization signal — it
%% fires strictly after `finish_stream/2''s close/abort call in the
%% real module's own `terminate/2', so waiting for `{terminated, _}'
%% before asserting on `meck:num_calls' avoids a race against the
%% reader process's own concurrent, asynchronous eof/error delivery.
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
    ok.

teardown(_) ->
    persistent_term:erase({?MODULE, handler}),
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

%%%===================================================================
%%% Tests
%%%===================================================================

client_stream_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun pushed_chunks_reach_handle_chunk_then_eof_closes/0,
      fun recv_error_aborts_not_closes/0]}.

pushed_chunks_reach_handle_chunk_then_eof_closes() ->
    process_flag(trap_exit, true),
    recv_returning([{chunk, <<"a">>}, {chunk, <<"b">>}, eof]),
    {ok, _Sup} = macula_streamer:advertise(pool, <<0:256>>, <<"bulk.ingest">>,
                                           ?MODULE, self(), #{mode => client_stream}),
    Handler = captured_handler(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    ?assertMatch({opened, _}, wait_msg()),
    ?assertEqual({chunk_seen, <<"a">>}, wait_msg()),
    ?assertEqual({chunk_seen, <<"b">>}, wait_msg()),
    ?assertEqual({terminated, normal}, wait_msg()),
    ?assertEqual(1, meck:num_calls(macula_stream, close, [StreamPid])),
    ?assertEqual(0, meck:num_calls(macula_stream, abort, ['_', '_', '_'])).

recv_error_aborts_not_closes() ->
    process_flag(trap_exit, true),
    recv_returning([{error, boom}]),
    {ok, _Sup} = macula_streamer:advertise(pool, <<0:256>>, <<"bulk.ingest">>,
                                           ?MODULE, self(), #{mode => client_stream}),
    Handler = captured_handler(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    ?assertMatch({opened, _}, wait_msg()),
    ?assertEqual({terminated, boom}, wait_msg()),
    ?assertEqual(1, meck:num_calls(macula_stream, abort, [StreamPid, <<"cancelled">>, '_'])),
    ?assertEqual(0, meck:num_calls(macula_stream, close, [StreamPid])).

%%%===================================================================
%%% Helpers
%%%===================================================================

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
