%%%-------------------------------------------------------------------
%%% @doc Tests for macula_subscriber.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_subscriber_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_subscriber).
-export([init/1, handle_event/4, terminate/2]).

%%%===================================================================
%%% Test callback module (this module doubles as the subscriber under test)
%%%===================================================================

init(Parent) -> {ok, #{parent => Parent}}.

handle_event(Topic, Payload, Meta, #{parent := Parent} = State) ->
    Parent ! {seen, Topic, Payload, Meta},
    case Payload of
        #{stop := true} -> {stop, normal, State};
        _ -> {noreply, State}
    end.

terminate(Reason, #{parent := Parent}) ->
    Parent ! {terminated, Reason},
    ok.

%%%===================================================================
%%% Fixtures
%%%===================================================================

setup() ->
    %% start_link/5 links the caller. Tests whose subscriber exits with
    %% a non-normal reason set trap_exit themselves, as the first thing
    %% in the test body — eunit's {foreach, ...} runs each test fun in
    %% a freshly spawned process (via eunit_proc:with_timeout), separate
    %% from whatever process ran this setup/0, so a flag set here never
    %% reaches the process the test actually executes in.
    meck:new(macula, [passthrough]),
    meck:expect(macula, subscribe,
                fun(_Pool, _Realm, _Topic, _Sub, _Opts) -> {ok, test_subref} end),
    ok.

teardown(_) ->
    meck:unload(macula).

%%%===================================================================
%%% Tests
%%%===================================================================

subscriber_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun receives_events_and_threads_state/0,
      fun stops_on_macula_event_gone/0,
      fun callback_can_stop_itself/0,
      fun init_stop_propagates/0]}.

receives_events_and_threads_state() ->
    {ok, Pid} = macula_subscriber:start_link(?MODULE, pool, <<0:256>>, <<"t">>, self()),
    Pid ! {macula_event, test_subref, <<"t">>, #{n => 1}, #{seq => 1}},
    receive
        {seen, <<"t">>, #{n := 1}, #{seq := 1}} -> ok
    after 1000 -> ?assert(false)
    end,
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid).

stops_on_macula_event_gone() ->
    process_flag(trap_exit, true),
    {ok, Pid} = macula_subscriber:start_link(?MODULE, pool, <<0:256>>, <<"t">>, self()),
    Ref = monitor(process, Pid),
    Pid ! {macula_event_gone, test_subref, pool_closed},
    receive
        {'DOWN', Ref, process, Pid, pool_closed} -> ok
    after 1000 -> ?assert(false)
    end,
    receive
        {terminated, pool_closed} -> ok
    after 1000 -> ?assert(false)
    end.

callback_can_stop_itself() ->
    {ok, Pid} = macula_subscriber:start_link(?MODULE, pool, <<0:256>>, <<"t">>, self()),
    Ref = monitor(process, Pid),
    Pid ! {macula_event, test_subref, <<"t">>, #{stop => true}, #{}},
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    after 1000 -> ?assert(false)
    end.

init_stop_propagates() ->
    process_flag(trap_exit, true),
    meck:expect(macula, subscribe,
                fun(_Pool, _Realm, _Topic, _Sub, _Opts) -> {error, no_healthy_link} end),
    ?assertEqual({error, no_healthy_link},
                 macula_subscriber:start_link(?MODULE, pool, <<0:256>>, <<"t">>, self())).
