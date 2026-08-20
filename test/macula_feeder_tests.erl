%%%-------------------------------------------------------------------
%%% @doc Tests for macula_feeder.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_feeder_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SINGLE_MCID, <<1, 16#55, 0:256>>).
-define(MANIFEST_MCID, <<1, 16#56, 0:256>>).

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

feeder_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun small_put_reports_unchunked/0,
      fun large_put_reports_chunked/0,
      fun failure_still_announces_completion/0,
      fun cancel_before_put_resolves_announces_cancelled/0]}.

small_put_reports_unchunked() ->
    process_flag(trap_exit, true),
    meck:expect(macula, put_content, fun(_Pool, _Bytes) -> {ok, ?SINGLE_MCID} end),
    {ok, _Pid} = macula_feeder:start_link(?MODULE, pool, <<0:256>>, <<"small">>, self()),
    ?assertEqual({fed, {ok, ?SINGLE_MCID}}, wait_msg()),
    ?assertEqual([<<"sharing.put_started_v1">>, <<"sharing.put_completed_v1">>], topics()),
    ?assertMatch(#{outcome := completed, mcid := ?SINGLE_MCID, chunked := false},
                 completed_payload()).

large_put_reports_chunked() ->
    process_flag(trap_exit, true),
    meck:expect(macula, put_content, fun(_Pool, _Bytes) -> {ok, ?MANIFEST_MCID} end),
    {ok, _Pid} = macula_feeder:start_link(?MODULE, pool, <<0:256>>, <<"big">>, self()),
    ?assertEqual({fed, {ok, ?MANIFEST_MCID}}, wait_msg()),
    ?assertMatch(#{outcome := completed, mcid := ?MANIFEST_MCID, chunked := true},
                 completed_payload()).

failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    meck:expect(macula, put_content, fun(_Pool, _Bytes) -> {error, no_healthy_link} end),
    {ok, _Pid} = macula_feeder:start_link(?MODULE, pool, <<0:256>>, <<"x">>, self()),
    ?assertEqual({fed, {error, no_healthy_link}}, wait_msg()),
    ?assertMatch(#{outcome := failed, reason := no_healthy_link}, completed_payload()).

cancel_before_put_resolves_announces_cancelled() ->
    process_flag(trap_exit, true),
    Self = self(),
    meck:expect(macula, put_content, fun(_Pool, _Bytes) ->
        Self ! put_started,
        receive never -> ok after 5_000 -> ok end,
        {ok, too_late}
    end),
    {ok, Pid} = macula_feeder:start_link(?MODULE, pool, <<0:256>>, <<"x">>, self()),
    ?assertEqual(put_started, wait_msg()),
    ok = macula_feeder:cancel(Pid),
    ?assertMatch(#{outcome := cancelled}, completed_payload()).

%%%===================================================================
%%% Helpers
%%%===================================================================

topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, ok} <- meck:history(macula)].

completed_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"sharing.put_completed_v1">>],
    Payload.

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
