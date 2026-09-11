%%%-------------------------------------------------------------------
%%% @doc Tests for macula_pusher.
%%%
%%% Each pusher runs on the stream functions and fact publish its start
%%% options give, from macula_scripted_stream: the raw streaming
%%% primitives this module drives directly (`call_stream', `send',
%%% `close_send', `await_reply', `abort'), so no test replaces a module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pusher_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).

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
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
pusher_test_() ->
    [{spawn, Test}
     || Test <- [fun small_push_delivers_the_verified_result/0,
                 fun chunked_push_sends_every_chunk_in_order/0,
                 fun send_failure_bails_without_awaiting_reply/0,
                 fun open_failure_still_announces_completion/0,
                 fun cancel_before_resolve_announces_cancelled/0,
                 fun cancel_reaches_the_real_stream_not_just_the_local_worker/0,
                 fun direct_dial_pushes_through_the_resolved_provider/0,
                 fun stream_functions_of_another_shape_are_refused/0,
                 fun without_stream_io_a_pusher_opens_through_the_macula_facade/0]].

small_push_delivers_the_verified_result() ->
    process_flag(trap_exit, true),
    Bytes = <<"small">>,
    {ok, Manifest, _Chunks} = macula_manifest:create(Bytes),
    Mcid = maps:get(mcid, Manifest),
    Opts = with_io(opts(), await_reply, fun(_Stream) -> {ok, Mcid} end),
    {ok, _Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), ?REALM, <<"bulk.ingest">>,
                                          Bytes, self(), Opts),
    ?assertEqual({pushed, {ok, Mcid}}, wait_msg()),
    Published = macula_scripted_stream:published(),
    ?assertEqual([<<"sharing.push_started_v1">>, <<"sharing.push_completed_v1">>],
                 [Topic || {Topic, _} <- Published]),
    ?assertMatch([_, {_, #{outcome := completed, mcid := Mcid}}], Published).

chunked_push_sends_every_chunk_in_order() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    {ok, Manifest, Chunks} = macula_manifest:create(Bytes),
    Mcid = maps:get(mcid, Manifest),
    Opts = with_io(opts(), await_reply, fun(_Stream) -> {ok, Mcid} end),
    {ok, _Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), ?REALM, <<"bulk.ingest">>,
                                          Bytes, self(), Opts),
    ?assertEqual({pushed, {ok, Mcid}}, wait_msg()),
    ?assertEqual(Chunks,
                 [Chunk || {send, [_Stream, Chunk, raw]} <- macula_scripted_stream:calls()]).

send_failure_bails_without_awaiting_reply() ->
    process_flag(trap_exit, true),
    Bytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    Opts = with_io(opts(), send, fun(_Stream, _Chunk, _Encoding) -> {error, send_closed} end),
    {ok, _Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), ?REALM, <<"bulk.ingest">>,
                                          Bytes, self(), Opts),
    ?assertEqual({pushed, {error, send_closed}}, wait_msg()),
    ?assertEqual([], [Name || {Name, _} <- macula_scripted_stream:calls(),
                              Name =:= close_send orelse Name =:= await_reply]).

open_failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    Opts = with_io(opts(), call_stream,
                   fun(_Pool, _Realm, _Proc, _Args, _CallOpts) -> {error, no_healthy_link} end),
    {ok, _Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), ?REALM, <<"bulk.ingest">>,
                                          <<"x">>, self(), Opts),
    ?assertEqual({pushed, {error, no_healthy_link}}, wait_msg()),
    ?assertMatch([_, {_, #{outcome := failed, reason := no_healthy_link}}],
                 macula_scripted_stream:published()).

cancel_before_resolve_announces_cancelled() ->
    process_flag(trap_exit, true),
    Self = self(),
    Opts = with_io(opts(), call_stream, fun(_Pool, _Realm, _Proc, _Args, _CallOpts) ->
                                                Self ! resolving,
                                                receive never -> ok after 5_000 -> ok end,
                                                {ok, dummy_pid()}
                                        end),
    {ok, Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), ?REALM, <<"bulk.ingest">>,
                                         <<"x">>, self(), Opts),
    ?assertEqual(resolving, wait_msg()),
    ok = macula_pusher:cancel(Pid),
    ?assertMatch([_, {_, #{outcome := cancelled}}], macula_scripted_stream:published()).

%% The actual point of holding `stream' in state: `cancel/1' must
%% reach all the way down to a real, peer-visible abort on the open
%% stream, not just kill the pusher's own local proxy process and
%% leave the peer to infer cancellation from the connection going
%% away (`macula_stream''s owner-death path is silent, see the
%% module doc).
cancel_reaches_the_real_stream_not_just_the_local_worker() ->
    process_flag(trap_exit, true),
    Self = self(),
    Opts = with_io(opts(), send, fun(_Stream, _Chunk, _Encoding) ->
                                         Self ! sending,
                                         receive never -> ok after 5_000 -> ok end,
                                         ok
                                 end),
    {ok, Pid} = macula_pusher:start_link(?MODULE, dummy_pid(), ?REALM, <<"bulk.ingest">>,
                                         <<"x">>, self(), Opts),
    ?assertEqual(sending, wait_msg()),
    ok = macula_pusher:cancel(Pid),
    ?assertEqual([{abort, [Self, <<"cancelled">>, <<"push cancelled">>]}],
                 [Call || {abort, _} = Call <- macula_scripted_stream:calls()]).

%% start_link_direct opens with the call_stream it is given, for the
%% procedure, in client_stream mode, as it does with
%% `macula_direct_dial:call_stream/5' by default.
direct_dial_pushes_through_the_resolved_provider() ->
    process_flag(trap_exit, true),
    Bytes = <<"direct">>,
    {ok, Manifest, _Chunks} = macula_manifest:create(Bytes),
    Mcid = maps:get(mcid, Manifest),
    Opts = with_io(opts(), await_reply, fun(_Stream) -> {ok, Mcid} end),
    {ok, _Pid} = macula_pusher:start_link_direct(?MODULE, dummy_pid(), ?REALM, <<"bulk.ingest">>,
                                                 Bytes, self(), Opts),
    ?assertEqual({pushed, {ok, Mcid}}, wait_msg()),
    ?assertMatch([{call_stream, [_, _, <<"bulk.ingest">>, _, #{mode := client_stream}]} | _],
                 macula_scripted_stream:calls()).

%% Stream functions without one the pusher calls, of another arity, or a
%% fact publish of another arity are refused with function_clause, in the
%% caller, and nothing is announced.
stream_functions_of_another_shape_are_refused() ->
    #{stream_io := StreamIo} = Opts = opts(),
    Start = fun(Given) ->
                    macula_pusher:start_link(?MODULE, dummy_pid(), ?REALM, <<"bulk.ingest">>,
                                             <<"x">>, self(), Given)
            end,
    ?assertError(function_clause, Start(Opts#{stream_io := maps:remove(await_reply, StreamIo)})),
    ?assertError(function_clause, Start(with_io(Opts, send, fun(_Stream, _Chunk) -> ok end))),
    ?assertError(function_clause, Start(Opts#{fact_publish := fun(_, _, _) -> ok end})),
    ?assertEqual([], macula_scripted_stream:published()).

%% Without stream functions a pusher opens its stream with
%% macula:call_stream/5, whose guard refuses a pool that is not a process.
without_stream_io_a_pusher_opens_through_the_macula_facade() ->
    process_flag(trap_exit, true),
    {ok, Pid} = macula_pusher:start_link(?MODULE, pool, ?REALM, <<"bulk.ingest">>, <<"x">>,
                                         self(), maps:with([fact_publish], opts())),
    receive
        {'EXIT', Pid, Reason} ->
            ?assertMatch({worker_crashed, {function_clause, [{macula, call_stream, _, _} | _]}},
                         Reason)
    after 5000 ->
        error(pusher_did_not_stop)
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

%% The start options: the scripted stream functions and fact publish.
opts() ->
    maps:with([stream_io, fact_publish], macula_scripted_stream:options([])).

%% Opts with its stream function Key given as Fun.
with_io(#{stream_io := StreamIo} = Opts, Key, Fun) ->
    Opts#{stream_io := StreamIo#{Key => Fun}}.

dummy_pid() ->
    spawn(fun() -> receive stop -> ok end end).

%% The next message from the pusher's callback or a held function.
wait_msg() ->
    receive
        {pushed, _} = Msg -> Msg;
        resolving = Msg -> Msg;
        sending = Msg -> Msg
    after 1000 -> timeout
    end.
