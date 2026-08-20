%%%-------------------------------------------------------------------
%%% @doc Tests for macula_publisher.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_publisher_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_publisher).
-export([init/1, handle_published/2]).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_published(Result, Parent) ->
    Parent ! {published, Result},
    {stop, normal, Parent}.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    meck:new(macula, [passthrough]),
    ok.

teardown(_) ->
    meck:unload(macula).

%%%===================================================================
%%% Tests
%%%===================================================================

publisher_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun successful_publish_reports_completed/0,
      fun failure_still_announces_completion/0,
      fun cancel_before_publish_resolves_announces_cancelled/0]}.

successful_publish_reports_completed() ->
    process_flag(trap_exit, true),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) -> ok end),
    {ok, _Pid} = macula_publisher:start_link(?MODULE, pool, <<0:256>>,
                                             <<"t">>, #{x => 1}, self()),
    ?assertEqual({published, ok}, wait_msg()),
    ?assertEqual([<<"pubsub.publish_started_v1">>, <<"pubsub.publish_completed_v1">>],
                 topics()),
    ?assertMatch(#{outcome := completed}, completed_payload()).

failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    meck:expect(macula, publish, fun(_Pool, _Realm, _Topic, _Payload) ->
        {error, no_healthy_link}
    end),
    {ok, _Pid} = macula_publisher:start_link(?MODULE, pool, <<0:256>>,
                                             <<"t">>, #{x => 1}, self()),
    ?assertEqual({published, {error, no_healthy_link}}, wait_msg()),
    ?assertMatch(#{outcome := failed, reason := no_healthy_link}, completed_payload()).

%% The mesh-fact announcements (`pubsub.publish_started_v1' /
%% `pubsub.publish_completed_v1') go through the SAME `macula:publish/4'
%% as the user's own real publish -- unlike `macula_feeder', whose
%% transfer rides a different mocked function (`put_content') from its
%% announcements. The mock must let mesh facts through immediately and
%% only block the user's own topic, or the STARTED announcement fired
%% synchronously inside `init/1' stalls `start_link/6' itself.
cancel_before_publish_resolves_announces_cancelled() ->
    process_flag(trap_exit, true),
    Self = self(),
    meck:expect(macula, publish, fun
        (_Pool, _Realm, <<"pubsub.", _/binary>>, _Payload) -> ok;
        (_Pool, _Realm, _Topic, _Payload) ->
            Self ! publish_started,
            receive never -> ok after 5_000 -> ok end,
            ok
    end),
    {ok, Pid} = macula_publisher:start_link(?MODULE, pool, <<0:256>>,
                                            <<"t">>, #{x => 1}, self()),
    ?assertEqual(publish_started, wait_msg()),
    ok = macula_publisher:cancel(Pid),
    ?assertMatch(#{outcome := cancelled}, completed_payload()).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% The mocked `macula:publish/4' fires once for the harness's own probe
%% payload AND once each for the started/completed mesh facts, so
%% `topics/0' filters to just the mesh-fact topics for the assertion
%% above to read cleanly.
topics() ->
    [T || {_, {macula, publish, [_Pool, _Realm, T, _Payload]}, _Ret} <- meck:history(macula),
          T =:= <<"pubsub.publish_started_v1">> orelse
          T =:= <<"pubsub.publish_completed_v1">>].

completed_payload() ->
    [{_, {macula, publish, [_, _, _, Payload]}, _Ret}] =
        [E || {_, {macula, publish, [_, _, T, _]}, _Ret} = E <- meck:history(macula),
              T =:= <<"pubsub.publish_completed_v1">>],
    Payload.

wait_msg() ->
    receive
        Msg -> Msg
    after 1000 -> timeout
    end.
