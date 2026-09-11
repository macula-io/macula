%%%-------------------------------------------------------------------
%%% @doc Tests for macula_feeder.
%%%
%%% Each feeder runs on functions its start options give: scripted link
%%% functions (macula_scripted_link), which it passes on to the real
%%% macula_content_transfer, the same layer macula_content_transfer_tests
%%% scripts, since PLAN_PUSH_UPLOAD.md Phase 4 moved this module onto
%%% macula_content_transfer directly; a scripted fact publish; and, where
%%% a test holds or redirects a step, its own transfer or resolve
%%% function. No test replaces a module. `Pool' must be a real pid here:
%%% it's threaded all the way down to `macula_content_transfer:start_put/3',
%%% whose own guard requires one.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_feeder_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SINGLE_CODEC, 16#55).
-define(REALM, <<0:256>>).

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
%%% Tests
%%%===================================================================

%% The application runs the share registry; each test runs in a process
%% of its own.
feeder_test_() ->
    {setup, fun start_macula/0,
     [{spawn, Test}
      || Test <- [fun small_put_reports_unchunked/0,
                  fun large_put_reports_chunked/0,
                  fun failure_still_announces_completion/0,
                  fun cancel_before_put_resolves_announces_cancelled/0,
                  fun cancel_reaches_the_real_content_transfer_not_just_the_local_worker/0,
                  fun direct_dial_resolves_then_puts_through_the_resolved_station/0,
                  fun functions_of_another_shape_are_refused/0]]}.

%% A cancel that lands after the transfer has started, and before the
%% feeder holds the transfer's pid, still cancels the transfer. The start
%% is held at that point, in whichever process runs it, until the cancel
%% has reached the feeder.
handover_test_() ->
    {setup, fun start_macula/0,
     [{spawn, Test}
      || Test <- [fun cancel_while_the_transfer_is_handed_over_still_reaches_it/0,
                  fun cancel_while_a_direct_transfer_is_handed_over_still_reaches_it/0]]}.

small_put_reports_unchunked() ->
    process_flag(trap_exit, true),
    Bytes = <<"small">>,
    LinkIo = (macula_scripted_link:link_io())#{
               call_on_stream := fun(_, _, _, <<"_content.put_block">>, _, _) -> {ok, ok} end},

    {ok, _Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), ?REALM, Bytes, self(),
                                          opts(#{link_io => LinkIo})),
    Hash = macula_blake3_nif:hash(Bytes),
    ExpectedMcid = <<1, ?SINGLE_CODEC, Hash/binary>>,
    ?assertEqual({fed, {ok, ExpectedMcid}}, wait_msg()),
    Published = macula_scripted_stream:published(),
    ?assertEqual([<<"sharing.put_started_v1">>, <<"sharing.put_completed_v1">>],
                 [Topic || {Topic, _} <- Published]),
    ?assertMatch([_, {_, #{outcome := completed, mcid := ExpectedMcid, chunked := false}}],
                 Published).

large_put_reports_chunked() ->
    process_flag(trap_exit, true),
    %% 3 chunks: macula_content_transfer's default stream_count (4, Phase
    %% 3) opens one dedicated stream per chunk here, and the scripted
    %% open_content_stream/1 hands back a distinct reference each call, as
    %% the lanes need.
    Bytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    {ok, ExpectedManifest, _Chunks} = macula_manifest:create(Bytes),
    ExpectedMcid = maps:get(mcid, ExpectedManifest),

    {ok, _Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), ?REALM, Bytes, self(),
                                          opts(#{link_io => macula_scripted_link:link_io()})),
    ?assertEqual({fed, {ok, ExpectedMcid}}, wait_msg()),
    ?assertMatch([_, {_, #{outcome := completed, mcid := ExpectedMcid, chunked := true}}],
                 macula_scripted_stream:published()).

failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    LinkIo = (macula_scripted_link:link_io())#{
               pick_connected_link := fun(_Pool) -> {error, no_healthy_link} end},

    {ok, _Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), ?REALM, <<"x">>, self(),
                                          opts(#{link_io => LinkIo})),
    ?assertEqual({fed, {error, no_healthy_link}}, wait_msg()),
    ?assertMatch([_, {_, #{outcome := failed, reason := no_healthy_link}}],
                 macula_scripted_stream:published()).

cancel_before_put_resolves_announces_cancelled() ->
    process_flag(trap_exit, true),
    LinkIo = open_put_link_io(self(), dummy_pid(), make_ref()),

    {ok, Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), ?REALM, <<"x">>, self(),
                                         opts(#{link_io => LinkIo})),
    ?assertEqual(put_started, wait_msg()),
    ok = macula_feeder:cancel(Pid),
    ?assertMatch([_, {_, #{outcome := cancelled}}], macula_scripted_stream:published()).

%% The actual point of Phase 4: `cancel/1' on the feeder must reach
%% all the way down to a real, peer-visible abort on the open content
%% stream, not just kill the feeder's own local proxy process and
%% leave the underlying macula_content_transfer running unnoticed
%% (which is exactly what happened before this phase: nothing links a
%% gen_server:call caller's death to the callee it was waiting on).
cancel_reaches_the_real_content_transfer_not_just_the_local_worker() ->
    process_flag(trap_exit, true),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    LinkIo = open_put_link_io(self(), LinkPid, Stream),

    {ok, Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), ?REALM, <<"x">>, self(),
                                         opts(#{link_io => LinkIo})),
    ?assertEqual(put_started, wait_msg()),
    ok = macula_feeder:cancel(Pid),
    ?assertMatch([[LinkPid, Stream, _, _]], aborts()).

%% start_link_direct resolves the station's endpoint (a plain blocking
%% lookup, unchanged in shape from before this phase) and THEN puts
%% through macula_content_transfer:start_put_station/5, dialing exactly
%% that resolved endpoint.
direct_dial_resolves_then_puts_through_the_resolved_station() ->
    process_flag(trap_exit, true),
    Bytes = <<"direct">>,
    Station = crypto:strong_rand_bytes(32),
    DialUrl = <<"quic://station.example:4433">>,
    LinkPid = dummy_pid(),
    LinkIo = (macula_scripted_link:link_io())#{
               ensure_content_link := link_to(DialUrl, LinkPid),
               call_on_stream := fun(_, _, _, <<"_content.put_block">>, _, _) -> {ok, ok} end},
    Resolve = fun(_Pool, Station0) when Station0 =:= Station -> {ok, DialUrl} end,

    Opts = opts(#{link_io => LinkIo, resolve_station_endpoint => Resolve}),
    {ok, _Pid} = macula_feeder:start_link_direct(?MODULE, dummy_pid(), Station, ?REALM, Bytes,
                                                 self(), Opts),
    Hash = macula_blake3_nif:hash(Bytes),
    ExpectedMcid = <<1, ?SINGLE_CODEC, Hash/binary>>,
    ?assertEqual({fed, {ok, ExpectedMcid}}, wait_msg()).

%% Transfer functions without one the feeder calls or of another arity,
%% and a resolve or fact publish of another arity, are refused with
%% function_clause, in the caller, and nothing is announced.
functions_of_another_shape_are_refused() ->
    Start = fun(Opts) ->
                    macula_feeder:start_link(?MODULE, dummy_pid(), ?REALM, <<"x">>, self(), Opts)
            end,
    Transfer = default_transfer_io(),
    ?assertError(function_clause, Start(opts(#{transfer_io => maps:remove(cancel, Transfer)}))),
    ?assertError(function_clause,
                 Start(opts(#{transfer_io => Transfer#{await := fun(_, _) -> ok end}}))),
    ?assertError(function_clause, Start(opts(#{resolve_station_endpoint => fun(_) -> ok end}))),
    ?assertError(function_clause, Start(opts(#{fact_publish => fun(_, _, _) -> ok end}))),
    ?assertEqual([], macula_scripted_stream:published()).

cancel_while_the_transfer_is_handed_over_still_reaches_it() ->
    process_flag(trap_exit, true),
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    Held = fun(Pool, Bytes, TransferOpts) ->
                   hold_handover(Self, macula_content_transfer:start_put(Pool, Bytes, TransferOpts))
           end,
    Opts = opts(#{link_io => open_put_link_io(Self, LinkPid, Stream),
                  transfer_io => (default_transfer_io())#{start_put := Held}}),

    {ok, Pid} = macula_feeder:start_link(?MODULE, dummy_pid(), ?REALM, <<"x">>, self(), Opts),
    cancel_during_handover(Pid),
    ?assertMatch([[LinkPid, Stream, _, _]], aborts()).

cancel_while_a_direct_transfer_is_handed_over_still_reaches_it() ->
    process_flag(trap_exit, true),
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    Station = crypto:strong_rand_bytes(32),
    DialUrl = <<"quic://station.example:4433">>,
    LinkIo = (open_put_link_io(Self, LinkPid, Stream))#{
               ensure_content_link := link_to(DialUrl, LinkPid)},
    Held = fun(Pool, Dial, Bytes, TimeoutMs, TransferOpts) ->
                   hold_handover(Self, macula_content_transfer:start_put_station(
                                         Pool, Dial, Bytes, TimeoutMs, TransferOpts))
           end,
    Resolve = fun(_Pool, Station0) when Station0 =:= Station -> {ok, DialUrl} end,
    Opts = opts(#{link_io => LinkIo, resolve_station_endpoint => Resolve,
                  transfer_io => (default_transfer_io())#{start_put_station := Held}}),

    {ok, Pid} = macula_feeder:start_link_direct(?MODULE, dummy_pid(), Station, ?REALM, <<"x">>,
                                                self(), Opts),
    cancel_during_handover(Pid),
    ?assertMatch([[LinkPid, Stream, _, _]], aborts()).

%%%===================================================================
%%% Helpers
%%%===================================================================

start_macula() ->
    {ok, _} = application:ensure_all_started(macula),
    ok.

dummy_pid() ->
    spawn(fun() -> receive stop -> ok end end).

%% Start options: a scripted fact publish, with Extra over it.
opts(Extra) ->
    FactPublish = maps:get(fact_publish, macula_scripted_stream:options([])),
    maps:merge(#{fact_publish => FactPublish}, Extra).

%% The transfer functions a feeder runs on by default.
default_transfer_io() ->
    #{start_put => fun macula_content_transfer:start_put/3,
      start_put_station => fun macula_content_transfer:start_put_station/5,
      await => fun macula_content_transfer:await/1,
      cancel => fun macula_content_transfer:cancel/1}.

%% An ensure_content_link/4 that dials only Seed, as LinkPid.
link_to(Seed, LinkPid) ->
    fun(_Pool, Dialed, _LinkOpts, _TimeoutMs) when Dialed =:= Seed -> {ok, LinkPid} end.

%% The abort calls made so far, as argument lists.
aborts() ->
    [Args || {abort_content_stream, Args} <- macula_scripted_link:calls()].

%% The next message from the feeder's callback or a held put.
wait_msg() ->
    receive
        {fed, _} = Msg -> Msg;
        put_started = Msg -> Msg
    after 1000 -> timeout
    end.

%% Link functions on LinkPid and Stream whose put stays open: the test
%% gets put_started, and the call does not return within the test.
open_put_link_io(Self, LinkPid, Stream) ->
    (macula_scripted_link:link_io())#{
      pick_connected_link := fun(_Pool) -> {ok, LinkPid} end,
      open_content_stream := fun(_LinkPid) -> {ok, Stream} end,
      call_on_stream := fun(_, _, _, _, _, _) ->
                                Self ! put_started,
                                receive never -> ok after 5_000 -> ok end,
                                {ok, ok}
                        end}.

%% Runs in whichever process starts the transfer: report, then keep the
%% start from returning until the test lets it.
hold_handover(Test, Started) ->
    Test ! {transfer_started, self()},
    receive release_handover -> Started end.

%% With the transfer started and its stream open, cancel the feeder, and let
%% the held start return only once the cancel has reached the feeder:
%% handled already, or queued behind the start.
cancel_during_handover(Pid) ->
    Self = self(),
    Holder = receive {transfer_started, H} -> H after 1_000 -> error(transfer_not_started) end,
    receive put_started -> ok after 1_000 -> error(stream_not_open) end,
    _ = spawn_link(fun() -> Self ! {cancelled, macula_feeder:cancel(Pid)} end),
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
