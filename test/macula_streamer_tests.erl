%%%-------------------------------------------------------------------
%%% @doc Tests for macula_streamer.
%%%
%%% Each streamer runs on the functions its options give, from
%%% macula_scripted_stream, so no test replaces a module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_streamer_tests).

-include_lib("eunit/include/eunit.hrl").

%% Data a reason carries that must stay on this node.
-define(MARKER, <<"marker-3f9c-stays-on-this-node">>).
-define(REALM, <<0:256>>).

-behaviour(macula_streamer).
-export([init/1, handle_open/2, terminate/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_open(#{refuse := Reason}, Parent) ->
    {stop, Reason, Parent};
handle_open(StreamArgs, Parent) ->
    Parent ! {opened, StreamArgs, self()},
    {ok, Parent}.

terminate(Reason, Parent) ->
    Parent ! {terminated, Reason},
    ok.

stream_stub() -> receive stop -> ok end.

%%%===================================================================
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
streamer_test_() ->
    [{spawn, Test}
     || Test <- [fun opens_and_publishes_lifecycle/0,
                 fun send_and_close_drive_the_stream/0,
                 fun dead_stream_stops_the_streamer/0,
                 fun a_refused_open_tells_the_peer_its_reasons_name_only/0,
                 fun a_stream_ending_for_a_reason_with_data_tells_the_peer_its_name_only/0,
                 fun advertise_direct_forwards_mode_to_advertise_stream/0,
                 fun advertise_forwards_auth_to_advertise_stream/0,
                 fun the_advertisement_publish_gets_the_options_without_the_functions/0,
                 fun functions_of_another_shape_are_refused_before_anything_is_advertised/0,
                 fun without_functions_a_streamer_advertises_through_the_macula_facade/0,
                 fun reuse_sup_resends_advertise_without_a_new_supervisor/0,
                 fun reuse_sup_with_a_dead_pid_starts_a_fresh_supervisor/0]].

%% A station's wire-level registration for a procedure is tied to the
%% connection that sent it, and does not survive that connection being
%% replaced -- a periodic re-advertise is the only way to keep it
%% current. `reuse_sup' is what makes that safe: without it, every
%% re-advertise call starts a fresh factory supervisor, leaking one per
%% tick forever.
reuse_sup_resends_advertise_without_a_new_supervisor() ->
    Opts = macula_scripted_stream:options([]),
    {ok, Sup1} = macula_streamer:advertise(pool, ?REALM, <<"bulk.ingest">>, ?MODULE, self(), Opts),
    {ok, Sup2} = macula_streamer:advertise(pool, ?REALM, <<"bulk.ingest">>, ?MODULE, self(),
                                           Opts#{reuse_sup => Sup1}),
    ?assertEqual(Sup1, Sup2),
    ?assertMatch([{<<"bulk.ingest">>, _, _, _}, {<<"bulk.ingest">>, _, _, _}],
                 macula_scripted_stream:advertised()).

%% Regression test for the identical noproc-on-first-dispatch bug fixed
%% in `macula_response' (found live 2026-09-01 via hecate-rag): see that
%% module's test of the same name for the full incident.
reuse_sup_with_a_dead_pid_starts_a_fresh_supervisor() ->
    DeadPid = spawn(fun() -> ok end),
    wait_until_dead(DeadPid),
    Opts = (macula_scripted_stream:options([]))#{reuse_sup => DeadPid},
    {ok, Sup} = macula_streamer:advertise(pool, ?REALM, <<"bulk.ingest">>, ?MODULE, self(), Opts),
    ?assert(is_pid(Sup)),
    ?assertNotEqual(DeadPid, Sup),
    ?assert(erlang:is_process_alive(Sup)).

wait_until_dead(Pid) ->
    wait_until_dead(Pid, erlang:is_process_alive(Pid)).

wait_until_dead(_Pid, false) -> ok;
wait_until_dead(Pid, true) -> timer:sleep(1), wait_until_dead(Pid, erlang:is_process_alive(Pid)).

%% Regression test for a real bug found while building macula_upload
%% (PLAN_PUSH_UPLOAD.md Phase 6): `advertise_direct/7' used to call
%% `advertise/5' (the arity that always defaults `mode' to
%% `server_stream'), silently discarding whatever `mode' the caller
%% passed in `Opts': a `client_stream' provider that advertised
%% directly would have been served as `server_stream' instead, with no
%% error anywhere to say so.
advertise_direct_forwards_mode_to_advertise_stream() ->
    Identity = macula_identity:generate(),
    Opts = (macula_scripted_stream:options([]))#{mode => client_stream},
    {ok, _Sup} = macula_streamer:advertise_direct(pool, ?REALM, <<"bulk.ingest">>, ?MODULE,
                                                  self(), Identity, Opts),
    ?assertMatch([{<<"bulk.ingest">>, client_stream, _, _}], macula_scripted_stream:advertised()),
    ?assertMatch([{<<"bulk.ingest">>, Identity, _}],
                 macula_scripted_stream:advertisements_published()).

%% A streamer-built procedure is gated like any other: `auth' in `Opts'
%% reaches the advertise function as the procedure's policy, through
%% both `advertise/6' and `advertise_direct/7', and it gets none of the
%% other options.
advertise_forwards_auth_to_advertise_stream() ->
    Policy = {ucan_required, <<7:256>>},
    Opts = (macula_scripted_stream:options([]))#{auth => Policy},
    {ok, _} = macula_streamer:advertise(pool, ?REALM, <<"logs.gated">>, ?MODULE, self(), Opts),
    {ok, _} = macula_streamer:advertise_direct(pool, ?REALM, <<"logs.gated">>, ?MODULE, self(),
                                               macula_identity:generate(), Opts),
    Advertised = macula_scripted_stream:advertised(),
    ?assertEqual([#{auth => Policy}, #{auth => Policy}],
                 [AdvertiseOpts || {_, _, _, AdvertiseOpts} <- Advertised]).

%% advertise_direct/7 publishes its DHT record with the options, such as
%% cert_chain, and none of the functions; with no policy among them, the
%% advertise function gets no options at all.
the_advertisement_publish_gets_the_options_without_the_functions() ->
    Opts = (macula_scripted_stream:options([]))#{cert_chain => <<"chain">>},
    {ok, _} = macula_streamer:advertise_direct(pool, ?REALM, <<"logs.tail_v1">>, ?MODULE, self(),
                                               macula_identity:generate(), Opts),
    ?assertMatch([{_, _, _, AdvertiseOpts}] when map_size(AdvertiseOpts) =:= 0,
                 macula_scripted_stream:advertised()),
    [{_, _, Published}] = macula_scripted_stream:advertisements_published(),
    ?assertEqual(#{cert_chain => <<"chain">>}, Published).

%% A function of another arity, or stream functions without one the
%% streamer calls, are refused with function_clause, and nothing is
%% advertised for them.
functions_of_another_shape_are_refused_before_anything_is_advertised() ->
    #{stream_io := StreamIo} = Opts = macula_scripted_stream:options([]),
    Advertise = fun(Given) ->
                        macula_streamer:advertise(pool, ?REALM, <<"p">>, ?MODULE, self(), Given)
                end,
    {ok, _} = Advertise(Opts),
    ?assertMatch([{<<"p">>, _, _, _}], macula_scripted_stream:advertised()),
    Three = fun(_, _, _) -> ok end,
    Four = fun(_, _, _, _) -> ok end,
    Five = fun(_, _, _, _, _) -> ok end,
    ?assertError(function_clause, Advertise(Opts#{advertise_stream := Five})),
    ?assertError(function_clause, Advertise(Opts#{fact_publish := Three})),
    ?assertError(function_clause, Advertise(Opts#{stream_io := maps:remove(set_error, StreamIo)})),
    ?assertError(function_clause,
                 macula_streamer:advertise_direct(pool, ?REALM, <<"p">>, ?MODULE, self(),
                                                  macula_identity:generate(),
                                                  Opts#{publish_advertisement := Four})),
    ?assertEqual([], macula_scripted_stream:advertised()).

%% Without functions in its options a streamer advertises with
%% macula:advertise_stream/6, whose guard refuses a pool that is not a
%% process.
without_functions_a_streamer_advertises_through_the_macula_facade() ->
    ?assertMatch({error, function_clause, [{macula, advertise_stream, _, _} | _]},
                 try macula_streamer:advertise(pool, ?REALM, <<"p">>, ?MODULE, self()) of
                     Result -> Result
                 catch
                     Class:Reason:Stack -> {Class, Reason, Stack}
                 end).

opens_and_publishes_lifecycle() ->
    process_flag(trap_exit, true),
    Handler = advertised_handler(macula_scripted_stream:options([])),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{topic => <<"t">>}),
    ?assertMatch({opened, #{topic := <<"t">>}, _}, wait_msg()),
    StreamPid ! stop,
    ?assertMatch({terminated, _}, wait_msg()),
    ?assertEqual([<<"streaming.started_v1">>, <<"streaming.completed_v1">>],
                 [Topic || {Topic, _} <- macula_scripted_stream:published()]),
    ?assertEqual([{close, [StreamPid]}], macula_scripted_stream:calls()).

send_and_close_drive_the_stream() ->
    process_flag(trap_exit, true),
    Handler = advertised_handler(macula_scripted_stream:options([])),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    {opened, _, StreamerPid} = wait_msg(),
    ?assertEqual(ok, macula_streamer:send(StreamerPid, <<"line\n">>)),
    ?assertEqual(ok, macula_streamer:close(StreamerPid)),
    ?assertEqual([{send, [StreamPid, <<"line\n">>, raw]}, {close_send, [StreamPid]}],
                 macula_scripted_stream:calls()).

dead_stream_stops_the_streamer() ->
    process_flag(trap_exit, true),
    Handler = advertised_handler(macula_scripted_stream:options([])),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    {opened, _, StreamerPid} = wait_msg(),
    Ref = monitor(process, StreamerPid),
    exit(StreamPid, boom),
    receive
        {'DOWN', Ref, process, StreamerPid, _} -> ok
    after 1000 -> ?assert(false)
    end,
    ?assertMatch({terminated, _}, wait_msg()),
    ?assertMatch([{<<"streaming.started_v1">>, _},
                  {<<"streaming.completed_v1">>, #{outcome := failed}}],
                 macula_scripted_stream:published()),
    ?assertEqual([{abort, [StreamPid, <<"cancelled">>, <<"boom">>]}],
                 macula_scripted_stream:calls()).

%% A refused open tells the peer the reason's name, and none of its terms.
a_refused_open_tells_the_peer_its_reasons_name_only() ->
    process_flag(trap_exit, true),
    Handler = advertised_handler(macula_scripted_stream:options([])),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{refuse => {refused, ?MARKER}}),
    ?assertEqual([{abort, [StreamPid, <<"cancelled">>, <<"refused">>]}],
                 macula_scripted_stream:calls()).

%% A stream that ends for a reason with data tells the peer the reason's
%% name, and none of its terms.
a_stream_ending_for_a_reason_with_data_tells_the_peer_its_name_only() ->
    process_flag(trap_exit, true),
    Handler = advertised_handler(macula_scripted_stream:options([])),
    StreamPid = spawn(fun stream_stub/0),
    ok = Handler(StreamPid, #{}),
    {opened, _, StreamerPid} = wait_msg(),
    Ref = monitor(process, StreamerPid),
    exit(StreamPid, {boom, ?MARKER}),
    receive
        {'DOWN', Ref, process, StreamerPid, _} -> ok
    after 1000 -> ?assert(false)
    end,
    ?assertMatch({terminated, _}, wait_msg()),
    ?assertEqual([{abort, [StreamPid, <<"cancelled">>, <<"boom">>]}],
                 macula_scripted_stream:calls()).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% Advertises this module with Opts and returns the handler the
%% advertise function got.
advertised_handler(Opts) ->
    {ok, _Sup} = macula_streamer:advertise(pool, ?REALM, <<"logs.tail_v1">>, ?MODULE, self(),
                                           Opts),
    [{<<"logs.tail_v1">>, _, Handler, _}] = macula_scripted_stream:advertised(),
    Handler.

%% The next message from the streamer's callbacks.
wait_msg() ->
    receive
        {opened, _, _} = Msg -> Msg;
        {terminated, _} = Msg -> Msg
    after 1000 -> timeout
    end.
