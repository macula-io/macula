%%%-------------------------------------------------------------------
%%% @doc Tests for macula_streamer's optional `client_stream' receive
%%% loop (`handle_chunk/2'). Split into its own file/callback module
%%% because it needs a Module that genuinely exports `handle_chunk/2',
%%% unlike `macula_streamer_tests''s deliberately simpler
%%% `server_stream'-shaped callback module, which must NOT export it
%%% (`maybe_spawn_reader/3' gates the reader on
%%% `erlang:function_exported/3', module-wide: exporting it there
%%% would spawn a reader for every one of that file's tests too). Each
%%% streamer runs on the functions macula_scripted_stream gives, so no
%%% test replaces a module.
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

%% `terminate/2' is exported purely as a synchronization signal: it
%% fires strictly after the real module's own `terminate/2' closes or
%% aborts the stream, so those calls are in the mailbox once
%% `{terminated, _}' is.
terminate(Reason, Parent) ->
    Parent ! {terminated, Reason},
    ok.

stream_stub() -> receive stop -> ok end.

%%%===================================================================
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
client_stream_test_() ->
    [{spawn, Test} || Test <- [fun pushed_chunks_reach_handle_chunk_then_eof_closes/0,
                                fun recv_error_aborts_not_closes/0]].

pushed_chunks_reach_handle_chunk_then_eof_closes() ->
    process_flag(trap_exit, true),
    StreamPid = open_client_stream([{chunk, <<"a">>}, {chunk, <<"b">>}, eof]),
    ?assertMatch({opened, _}, wait_msg()),
    ?assertEqual({chunk_seen, <<"a">>}, wait_msg()),
    ?assertEqual({chunk_seen, <<"b">>}, wait_msg()),
    ?assertEqual({terminated, normal}, wait_msg()),
    ?assertEqual([{close, [StreamPid]}], macula_scripted_stream:calls()).

recv_error_aborts_not_closes() ->
    process_flag(trap_exit, true),
    StreamPid = open_client_stream([{error, boom}]),
    ?assertMatch({opened, _}, wait_msg()),
    ?assertEqual({terminated, boom}, wait_msg()),
    ?assertEqual([{abort, [StreamPid, <<"cancelled">>, <<"boom">>]}],
                 macula_scripted_stream:calls()).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Advertises this module as a client_stream provider whose recv/2
%% returns Results, and opens a stream on it.
open_client_stream(Results) ->
    Opts = (macula_scripted_stream:options(Results))#{mode => client_stream},
    {ok, _Sup} = macula_streamer:advertise(pool, <<0:256>>, <<"bulk.ingest">>,
                                           ?MODULE, self(), Opts),
    [{_, client_stream, Handler, _}] = macula_scripted_stream:advertised(),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    StreamPid.

%% The next message from the streamer's callbacks.
wait_msg() ->
    receive
        {opened, _} = Msg -> Msg;
        {chunk_seen, _} = Msg -> Msg;
        {terminated, _} = Msg -> Msg
    after 1000 -> timeout
    end.
