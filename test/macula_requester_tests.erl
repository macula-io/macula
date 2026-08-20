%%%-------------------------------------------------------------------
%%% @doc Tests for macula_requester.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_requester_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_requester).
-export([init/1, handle_reply/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_reply(Result, Parent) ->
    Parent ! {reply_seen, Result},
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

requester_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun delivers_reply_and_publishes_lifecycle/0,
      fun surfaces_call_error/0,
      fun cancel_before_reply_announces_cancelled/0]}.

delivers_reply_and_publishes_lifecycle() ->
    process_flag(trap_exit, true),
    meck:expect(macula, call, fun(_Pool, _Realm, _Proc, _Payload, _Timeout) ->
        {ok, #{result => 5}}
    end),
    {ok, _Pid} = macula_requester:start_link(?MODULE, pool, <<0:256>>,
        <<"math.add_v1">>, #{a => 2, b => 3}, 5_000, self()),
    ?assertEqual({reply_seen, {ok, #{result => 5}}}, wait_msg()),
    ?assertEqual([<<"rpc.sent_v1">>, <<"rpc.completed_v1">>], topics()).

surfaces_call_error() ->
    process_flag(trap_exit, true),
    meck:expect(macula, call, fun(_Pool, _Realm, _Proc, _Payload, _Timeout) ->
        {error, no_healthy_link}
    end),
    {ok, _Pid} = macula_requester:start_link(?MODULE, pool, <<0:256>>,
        <<"math.add_v1">>, #{}, 5_000, self()),
    ?assertEqual({reply_seen, {error, no_healthy_link}}, wait_msg()),
    ?assertMatch(#{outcome := failed, reason := no_healthy_link}, completed_payload()).

cancel_before_reply_announces_cancelled() ->
    process_flag(trap_exit, true),
    Self = self(),
    meck:expect(macula, call, fun(_Pool, _Realm, _Proc, _Payload, _Timeout) ->
        Self ! call_started,
        receive never -> ok after 5_000 -> ok end,
        {ok, too_late}
    end),
    {ok, Pid} = macula_requester:start_link(?MODULE, pool, <<0:256>>,
        <<"math.add_v1">>, #{}, 5_000, self()),
    ?assertEqual(call_started, wait_msg()),
    ok = macula_requester:cancel(Pid),
    ?assertMatch(#{outcome := cancelled}, completed_payload()).

%%%===================================================================
%%% Helpers
%%%===================================================================

topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, ok} <- meck:history(macula)].

completed_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, ok}] =
        [E || {_, {macula, publish, [_, _, T, _]}, ok} = E <- meck:history(macula),
              T =:= <<"rpc.completed_v1">>],
    Payload.

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
