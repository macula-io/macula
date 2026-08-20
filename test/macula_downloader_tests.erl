%%%-------------------------------------------------------------------
%%% @doc Tests for macula_downloader.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_downloader_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SINGLE_MCID, <<1, 16#55, 0:256>>).
-define(MANIFEST_MCID, <<1, 16#56, 0:256>>).

-behaviour(macula_downloader).
-export([init/1, handle_downloaded/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_downloaded(Result, Parent) ->
    Parent ! {downloaded, Result},
    {stop, normal, Parent}.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    meck:new(macula, [passthrough]),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    ok.

teardown(_) ->
    meck:unload(macula).

%%%===================================================================
%%% Tests
%%%===================================================================

downloader_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun single_block_get_reports_unchunked/0,
      fun manifest_get_reports_chunked/0,
      fun failure_still_announces_completion/0,
      fun cancel_before_get_resolves_announces_cancelled/0]}.

single_block_get_reports_unchunked() ->
    process_flag(trap_exit, true),
    meck:expect(macula, get_content, fun(_Pool, _Mcid) -> {ok, <<"bytes">>} end),
    {ok, _Pid} = macula_downloader:start_link(?MODULE, pool, <<0:256>>, ?SINGLE_MCID, self()),
    ?assertEqual({downloaded, {ok, <<"bytes">>}}, wait_msg()),
    ?assertEqual([<<"sharing.get_started_v1">>, <<"sharing.get_completed_v1">>], topics()),
    ?assertMatch(#{outcome := completed, size := 5}, completed_payload()),
    ?assertMatch(#{mcid := ?SINGLE_MCID, chunked := false}, started_payload()).

manifest_get_reports_chunked() ->
    process_flag(trap_exit, true),
    meck:expect(macula, get_content, fun(_Pool, _Mcid) -> {ok, <<"bytes">>} end),
    {ok, _Pid} = macula_downloader:start_link(?MODULE, pool, <<0:256>>, ?MANIFEST_MCID, self()),
    ?assertEqual({downloaded, {ok, <<"bytes">>}}, wait_msg()),
    ?assertMatch(#{mcid := ?MANIFEST_MCID, chunked := true}, started_payload()).

failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    meck:expect(macula, get_content, fun(_Pool, _Mcid) -> {error, not_found} end),
    {ok, _Pid} = macula_downloader:start_link(?MODULE, pool, <<0:256>>, ?SINGLE_MCID, self()),
    ?assertEqual({downloaded, {error, not_found}}, wait_msg()),
    ?assertMatch(#{outcome := failed, reason := not_found}, completed_payload()).

cancel_before_get_resolves_announces_cancelled() ->
    process_flag(trap_exit, true),
    Self = self(),
    meck:expect(macula, get_content, fun(_Pool, _Mcid) ->
        Self ! get_started,
        receive never -> ok after 5_000 -> ok end,
        {ok, too_late}
    end),
    {ok, Pid} = macula_downloader:start_link(?MODULE, pool, <<0:256>>, ?SINGLE_MCID, self()),
    ?assertEqual(get_started, wait_msg()),
    ok = macula_downloader:cancel(Pid),
    ?assertMatch(#{outcome := cancelled}, completed_payload()).

%%%===================================================================
%%% Helpers
%%%===================================================================

topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, ok} <- meck:history(macula)].

started_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"sharing.get_started_v1">>],
    Payload.

completed_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"sharing.get_completed_v1">>],
    Payload.

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
