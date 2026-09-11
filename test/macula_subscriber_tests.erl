%%%-------------------------------------------------------------------
%%% @doc Tests for macula_subscriber. Each subscriber subscribes with a
%%% function the test gives it, so no test replaces the macula module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_subscriber_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_subscriber).
-export([init/1, handle_event/4, terminate/2]).

-define(REALM, <<0:256>>).
-define(TOPIC, <<"t">>).

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
%%% Subscribe functions
%%%===================================================================

%% A subscribe function that grants test_subref and tells Test what it
%% was called with.
subscribed(Test) ->
    fun(Pool, Realm, Topic, Subscriber, Opts) ->
            Test ! {subscribed, Pool, Realm, Topic, Subscriber, Opts},
            {ok, test_subref}
    end.

%% start_link/6 links the caller, so a test whose subscriber exits with
%% a non-normal reason sets trap_exit itself, first thing in the test.
start_subscriber(Opts) ->
    macula_subscriber:start_link(?MODULE, pool, ?REALM, ?TOPIC, self(),
                                 Opts#{subscribe => subscribed(self())}).

%%%===================================================================
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
subscriber_test_() ->
    [{spawn, Test}
     || Test <- [fun receives_events_and_threads_state/0,
                 fun stops_on_macula_event_gone/0,
                 fun callback_can_stop_itself/0,
                 fun init_stop_propagates/0,
                 fun the_subscribe_function_gets_the_other_options/0,
                 fun without_a_subscribe_function_it_subscribes_through_macula/0,
                 fun a_subscribe_option_that_is_not_an_arity_5_fun_is_refused/0]].

receives_events_and_threads_state() ->
    {ok, Pid} = start_subscriber(#{}),
    Pid ! {macula_event, test_subref, <<"t">>, #{n => 1}, #{seq => 1}},
    receive
        {seen, <<"t">>, #{n := 1}, #{seq := 1}} -> ok
    after 1000 -> ?assert(false)
    end,
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid).

stops_on_macula_event_gone() ->
    process_flag(trap_exit, true),
    {ok, Pid} = start_subscriber(#{}),
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
    {ok, Pid} = start_subscriber(#{}),
    Ref = monitor(process, Pid),
    Pid ! {macula_event, test_subref, <<"t">>, #{stop => true}, #{}},
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    after 1000 -> ?assert(false)
    end.

init_stop_propagates() ->
    process_flag(trap_exit, true),
    Refusing = fun(_Pool, _Realm, _Topic, _Subscriber, _Opts) -> {error, no_healthy_link} end,
    ?assertEqual({error, no_healthy_link},
                 macula_subscriber:start_link(?MODULE, pool, ?REALM, ?TOPIC, self(),
                                              #{subscribe => Refusing})).

the_subscribe_function_gets_the_other_options() ->
    {ok, Pid} = start_subscriber(#{delivery => at_least_once}),
    Subscribed = receive
                     {subscribed, _, _, _, _, _} = Call -> Call
                 after 1000 ->
                     nothing_subscribed
                 end,
    ?assertEqual({subscribed, pool, ?REALM, ?TOPIC, Pid, #{delivery => at_least_once}},
                 Subscribed),
    ok = gen_server:stop(Pid).

%% Without a subscribe function the subscriber subscribes with
%% macula:subscribe/5, which passes a pool that is not a process on to
%% macula_pubsub:subscribe/5, whose guard refuses it.
without_a_subscribe_function_it_subscribes_through_macula() ->
    process_flag(trap_exit, true),
    ?assertMatch({error, {function_clause, [{macula_pubsub, subscribe, _, _} | _]}},
                 macula_subscriber:start_link(?MODULE, pool, ?REALM, ?TOPIC, self())).

a_subscribe_option_that_is_not_an_arity_5_fun_is_refused() ->
    ?assertError(function_clause,
                 macula_subscriber:start_link(?MODULE, pool, ?REALM, ?TOPIC, self(),
                                              #{subscribe => fun(_Pool) -> ok end})).
