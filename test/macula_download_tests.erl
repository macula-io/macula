%%%-------------------------------------------------------------------
%%% @doc Tests for macula_download.
%%%
%%% Each download runs on functions its start options give: scripted link
%%% functions (macula_scripted_link), which it passes on to the real
%%% macula_content_transfer, the same layer macula_content_transfer_tests
%%% scripts, since PLAN_PUSH_UPLOAD.md Phase 4 moved this module onto
%%% macula_content_transfer directly; a scripted fact publish; and, where
%%% a test holds or redirects a step, its own transfer or fetch function.
%%% No test replaces a module. `Pool' must be a real pid here: it's
%%% threaded all the way down to `macula_content_transfer:start_get/3',
%%% whose own guard requires one.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_download_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SINGLE_MCID, <<1, 16#55, 0:256>>).
-define(MANIFEST_MCID, <<1, 16#56, 0:256>>).
-define(REALM, <<0:256>>).

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
%%% Tests
%%%===================================================================

%% The application runs the share registry; each test runs in a process
%% of its own.
download_test_() ->
    {setup, fun start_macula/0,
     [{spawn, Test}
      || Test <- [fun single_block_get_reports_unchunked/0,
                  fun manifest_get_reports_chunked/0,
                  fun failure_still_announces_completion/0,
                  fun cancel_before_get_resolves_announces_cancelled/0,
                  fun cancel_reaches_the_real_content_transfer_not_just_the_local_worker/0,
                  fun direct_dial_resolves_then_fetches_from_the_resolved_provider/0,
                  fun a_malformed_mcid_is_rejected_before_anything_is_spawned/0,
                  fun direct_dial_also_rejects_a_malformed_mcid/0,
                  fun functions_of_another_shape_are_refused/0]]}.

%% A cancel that lands after the transfer has started, and before the
%% download holds the transfer's pid, still cancels the transfer. The start
%% is held at that point, in whichever process runs it, until the cancel
%% has reached the download.
handover_test_() ->
    {setup, fun start_macula/0,
     [{spawn, Test}
      || Test <- [fun cancel_while_the_transfer_is_handed_over_still_reaches_it/0,
                  fun cancel_while_a_direct_transfer_is_handed_over_still_reaches_it/0]]}.

%% Regression: a share link or caller-supplied Mcid that doesn't carry
%% either codec byte macula:put_content/2 ever mints used to reach
%% macula_content_transfer's is_chunked/2, whose clauses assume that
%% shape and crash the spawned worker with a FunctionClauseError. This
%% pins the rejection at init/1, before the worker (or the started_v1
%% announce) ever exists.
a_malformed_mcid_is_rejected_before_anything_is_spawned() ->
    %% `init/1' returning `{stop, Reason}' acks this process cleanly
    %% with `{error, Reason}' AND, separately, an untrapped exit signal
    %% follows once the failed gen_server actually terminates: trap
    %% it like every other test here that calls start_link*.
    process_flag(trap_exit, true),
    Result = macula_download:start_link(?MODULE, dummy_pid(), ?REALM, <<"not-an-mcid">>, self(),
                                        opts(#{})),
    ?assertEqual({error, invalid_mcid}, Result),
    ?assertEqual([], macula_scripted_stream:published()).

direct_dial_also_rejects_a_malformed_mcid() ->
    process_flag(trap_exit, true),
    Result = macula_download:start_link_direct(?MODULE, dummy_pid(), ?REALM, <<"not-an-mcid">>,
                                               self(), opts(#{})),
    ?assertEqual({error, invalid_mcid}, Result),
    ?assertEqual([], macula_scripted_stream:published()).

single_block_get_reports_unchunked() ->
    process_flag(trap_exit, true),
    Bytes = <<"bytes">>,
    Hash = macula_blake3_nif:hash(Bytes),
    Mcid = <<1, 16#55, Hash/binary>>,
    LinkIo = (macula_scripted_link:link_io())#{
               call_on_stream := fun(_, _, _, <<"_content.get_block">>, _, _) -> {ok, Bytes} end},

    {ok, _Pid} = macula_download:start_link(?MODULE, dummy_pid(), ?REALM, Mcid, self(),
                                            opts(#{link_io => LinkIo})),
    ?assertEqual({downloaded, {ok, Bytes}}, wait_msg()),
    Published = macula_scripted_stream:published(),
    ?assertEqual([<<"sharing.get_started_v1">>, <<"sharing.get_completed_v1">>],
                 [Topic || {Topic, _} <- Published]),
    ?assertMatch([{_, #{mcid := Mcid, chunked := false}}, {_, #{outcome := completed, size := 5}}],
                 Published).

manifest_get_reports_chunked() ->
    process_flag(trap_exit, true),
    %% 3 chunks: macula_content_transfer's default stream_count (4, Phase
    %% 3) opens one dedicated stream per chunk here, and the scripted
    %% open_content_stream/1 hands back a distinct reference each call, as
    %% the lanes need.
    OriginalBytes = crypto:strong_rand_bytes(3 * macula_manifest:default_chunk_size()),
    {ok, Manifest, Chunks} = macula_manifest:create(OriginalBytes),
    Mcid = maps:get(mcid, Manifest),
    ChunkByMcid = chunk_mcid_map(Manifest, Chunks),
    LinkIo = (macula_scripted_link:link_io())#{
               call_on_stream := fun
                                     (_, _, _, <<"_content.get_manifest">>, _Payload, _) ->
                                         {ok, Manifest};
                                     (_, _, _, <<"_content.get_block">>, #{mcid := ChunkMcid}, _) ->
                                         {ok, maps:get(ChunkMcid, ChunkByMcid)}
                                 end},

    {ok, _Pid} = macula_download:start_link(?MODULE, dummy_pid(), ?REALM, Mcid, self(),
                                            opts(#{link_io => LinkIo})),
    ?assertEqual({downloaded, {ok, OriginalBytes}}, wait_msg()),
    ?assertMatch([{_, #{mcid := Mcid, chunked := true}} | _], macula_scripted_stream:published()).

failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    LinkIo = (macula_scripted_link:link_io())#{
               pick_connected_link := fun(_Pool) -> {error, no_healthy_link} end},

    {ok, _Pid} = macula_download:start_link(?MODULE, dummy_pid(), ?REALM, ?SINGLE_MCID, self(),
                                            opts(#{link_io => LinkIo})),
    ?assertEqual({downloaded, {error, no_healthy_link}}, wait_msg()),
    ?assertMatch([_, {_, #{outcome := failed, reason := no_healthy_link}}],
                 macula_scripted_stream:published()).

cancel_before_get_resolves_announces_cancelled() ->
    process_flag(trap_exit, true),
    LinkIo = open_get_link_io(self(), dummy_pid(), make_ref()),

    {ok, Pid} = macula_download:start_link(?MODULE, dummy_pid(), ?REALM, ?SINGLE_MCID, self(),
                                           opts(#{link_io => LinkIo})),
    ?assertEqual(get_started, wait_msg()),
    ok = macula_download:cancel(Pid),
    ?assertMatch([_, {_, #{outcome := cancelled}}], macula_scripted_stream:published()).

%% The actual point of Phase 4: `cancel/1' on the download must reach
%% all the way down to a real, peer-visible abort on the open content
%% stream, not just kill the download's own local proxy process and
%% leave the underlying macula_content_transfer running unnoticed
%% (which is exactly what happened before this phase: nothing links a
%% gen_server:call caller's death to the callee it was waiting on).
cancel_reaches_the_real_content_transfer_not_just_the_local_worker() ->
    process_flag(trap_exit, true),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    LinkIo = open_get_link_io(self(), LinkPid, Stream),

    {ok, Pid} = macula_download:start_link(?MODULE, dummy_pid(), ?REALM, ?SINGLE_MCID, self(),
                                           opts(#{link_io => LinkIo})),
    ?assertEqual(get_started, wait_msg()),
    ok = macula_download:cancel(Pid),
    ?assertMatch([[LinkPid, Stream, _, _]], aborts()).

%% start_link_direct has its fetch function choose the MCID's provider
%% and THEN fetches through macula_content_transfer:start_get_station/5,
%% dialing exactly the endpoint the fetch function gave.
direct_dial_resolves_then_fetches_from_the_resolved_provider() ->
    process_flag(trap_exit, true),
    Bytes = <<"direct fetch">>,
    Hash = macula_blake3_nif:hash(Bytes),
    Mcid = <<1, 16#55, Hash/binary>>,
    Endpoint = <<"quic://provider.example:4433">>,
    LinkPid = dummy_pid(),
    LinkIo = (macula_scripted_link:link_io())#{
               ensure_content_link := link_to(Endpoint, LinkPid),
               call_on_stream := fun(_, _, _, <<"_content.get_block">>, _, _) -> {ok, Bytes} end},

    {ok, _Pid} = macula_download:start_link_direct(?MODULE, dummy_pid(), ?REALM, Mcid, self(),
                                                   opts(#{link_io => LinkIo,
                                                          fetch_content => fetch_from(Endpoint)})),
    ?assertEqual({downloaded, {ok, Bytes}}, wait_msg()).

%% Transfer functions without one the download calls or of another
%% arity, and a fetch or fact publish of another arity, are refused with
%% function_clause, in the caller, and nothing is announced.
functions_of_another_shape_are_refused() ->
    Start = fun(Opts) ->
                    macula_download:start_link(?MODULE, dummy_pid(), ?REALM, ?SINGLE_MCID, self(),
                                               Opts)
            end,
    Transfer = default_transfer_io(),
    ?assertError(function_clause, Start(opts(#{transfer_io => maps:remove(cancel, Transfer)}))),
    ?assertError(function_clause,
                 Start(opts(#{transfer_io => Transfer#{await := fun(_, _) -> ok end}}))),
    ?assertError(function_clause, Start(opts(#{fetch_content => fun(_, _, _) -> ok end}))),
    ?assertError(function_clause, Start(opts(#{fact_publish => fun(_, _, _) -> ok end}))),
    ?assertEqual([], macula_scripted_stream:published()).

cancel_while_the_transfer_is_handed_over_still_reaches_it() ->
    process_flag(trap_exit, true),
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    Held = fun(Pool, Mcid, TransferOpts) ->
                   hold_handover(Self, macula_content_transfer:start_get(Pool, Mcid, TransferOpts))
           end,
    Opts = opts(#{link_io => open_get_link_io(Self, LinkPid, Stream),
                  transfer_io => (default_transfer_io())#{start_get := Held}}),

    {ok, Pid} = macula_download:start_link(?MODULE, dummy_pid(), ?REALM, ?SINGLE_MCID, self(),
                                           Opts),
    cancel_during_handover(Pid),
    ?assertMatch([[LinkPid, Stream, _, _]], aborts()).

cancel_while_a_direct_transfer_is_handed_over_still_reaches_it() ->
    process_flag(trap_exit, true),
    Self = self(),
    LinkPid = dummy_pid(),
    Stream = make_ref(),
    Endpoint = <<"quic://provider.example:4433">>,
    LinkIo = (open_get_link_io(Self, LinkPid, Stream))#{
               ensure_content_link := link_to(Endpoint, LinkPid)},
    Held = fun(Pool, Station, Mcid, TimeoutMs, TransferOpts) ->
                   hold_handover(Self, macula_content_transfer:start_get_station(
                                         Pool, Station, Mcid, TimeoutMs, TransferOpts))
           end,
    Opts = opts(#{link_io => LinkIo, fetch_content => fetch_from(Endpoint),
                  transfer_io => (default_transfer_io())#{start_get_station := Held}}),

    {ok, Pid} = macula_download:start_link_direct(?MODULE, dummy_pid(), ?REALM, ?SINGLE_MCID,
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

%% The transfer functions a download runs on by default.
default_transfer_io() ->
    #{start_get => fun macula_content_transfer:start_get/3,
      start_get_station => fun macula_content_transfer:start_get_station/5,
      await => fun macula_content_transfer:await/1,
      cancel => fun macula_content_transfer:cancel/1}.

%% A fetch function that chooses Endpoint as the provider and fetches
%% from it, as macula_direct_dial:fetch_content/4 does for a provider it
%% resolved.
fetch_from(Endpoint) ->
    fun(_Pool, _Mcid, _TimeoutMs, Fetch) ->
            Fetch(Endpoint, #{verify => none}, 1_000, 30_000)
    end.

%% An ensure_content_link/4 that dials only Seed, as LinkPid.
link_to(Seed, LinkPid) ->
    fun(_Pool, Dialed, _LinkOpts, _TimeoutMs) when Dialed =:= Seed -> {ok, LinkPid} end.

%% The abort calls made so far, as argument lists.
aborts() ->
    [Args || {abort_content_stream, Args} <- macula_scripted_link:calls()].

%% The next message from the download's callback or a held get.
wait_msg() ->
    receive
        {downloaded, _} = Msg -> Msg;
        get_started = Msg -> Msg
    after 1000 -> timeout
    end.

%% ChunkMcid => chunk bytes, for a get test's scripted `_content.get_block'
%% to answer with the right bytes regardless of fetch order.
chunk_mcid_map(Manifest, Chunks) ->
    Indices = lists:seq(0, length(Chunks) - 1),
    maps:from_list([begin
        {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, I, blake3),
        {ChunkMcid, C}
    end || {I, C} <- lists:zip(Indices, Chunks)]).

%% Link functions on LinkPid and Stream whose get stays open: the test
%% gets get_started, and the call does not return within the test.
open_get_link_io(Self, LinkPid, Stream) ->
    (macula_scripted_link:link_io())#{
      pick_connected_link := fun(_Pool) -> {ok, LinkPid} end,
      open_content_stream := fun(_LinkPid) -> {ok, Stream} end,
      call_on_stream := fun(_, _, _, _, _, _) ->
                                Self ! get_started,
                                receive never -> ok after 5_000 -> ok end,
                                {ok, <<"too late">>}
                        end}.

%% Runs in whichever process starts the transfer: report, then keep the
%% start from returning until the test lets it.
hold_handover(Test, Started) ->
    Test ! {transfer_started, self()},
    receive release_handover -> Started end.

%% With the transfer started and its stream open, cancel the download, and let
%% the held start return only once the cancel has reached the download:
%% handled already, or queued behind the start.
cancel_during_handover(Pid) ->
    Self = self(),
    Holder = receive {transfer_started, H} -> H after 1_000 -> error(transfer_not_started) end,
    receive get_started -> ok after 1_000 -> error(stream_not_open) end,
    _ = spawn_link(fun() -> Self ! {cancelled, macula_download:cancel(Pid)} end),
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
