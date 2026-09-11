%%%-------------------------------------------------------------------
%%% @doc Tests for macula_publisher. Each publisher publishes and
%%% announces with functions the test gives it, so no test replaces the
%%% macula module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_publisher_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_publisher).
-export([init/1, handle_published/2]).

-define(REALM, <<0:256>>).
-define(TOPIC, <<"t">>).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_published(Result, Parent) ->
    Parent ! {published, Result},
    {stop, normal, Parent}.

%%%===================================================================
%%% Publish functions
%%%===================================================================

%% A publish function that answers Result.
answering(Result) ->
    fun(_Pool, _Realm, _Topic, _Payload) -> Result end.

%% A fact publish function that sends Test each fact.
facts_to(Test) ->
    fun(_Pool, _Realm, Topic, Payload) ->
            Test ! {fact, Topic, Payload},
            ok
    end.

start_publisher(Publish) ->
    macula_publisher:start_link(?MODULE, pool, ?REALM, ?TOPIC, #{x => 1}, self(),
                                #{publish => Publish, fact_publish => facts_to(self())}).

%%%===================================================================
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
publisher_test_() ->
    [{spawn, Test}
     || Test <- [fun successful_publish_reports_completed/0,
                 fun failure_still_announces_completion/0,
                 fun cancel_before_publish_resolves_announces_cancelled/0,
                 fun without_publish_functions_it_announces_through_macula/0,
                 fun a_publish_option_that_is_not_an_arity_4_fun_is_refused/0]].

successful_publish_reports_completed() ->
    process_flag(trap_exit, true),
    {ok, _Pid} = start_publisher(answering(ok)),
    ?assertMatch({fact, <<"pubsub.publish_started_v1">>, #{topic := ?TOPIC}}, next_fact()),
    ?assertMatch({fact, <<"pubsub.publish_completed_v1">>, #{outcome := completed}},
                 next_fact()),
    ?assertEqual({published, ok}, wait_published()).

failure_still_announces_completion() ->
    process_flag(trap_exit, true),
    {ok, _Pid} = start_publisher(answering({error, no_healthy_link})),
    ?assertMatch({fact, <<"pubsub.publish_started_v1">>, _}, next_fact()),
    ?assertMatch({fact, <<"pubsub.publish_completed_v1">>,
                  #{outcome := failed, reason := no_healthy_link}}, next_fact()),
    ?assertEqual({published, {error, no_healthy_link}}, wait_published()).

cancel_before_publish_resolves_announces_cancelled() ->
    process_flag(trap_exit, true),
    Test = self(),
    Blocking = fun(_Pool, _Realm, _Topic, _Payload) ->
                       Test ! publish_started,
                       receive never -> ok after 5_000 -> ok end
               end,
    {ok, Pid} = start_publisher(Blocking),
    ?assertEqual(ok, receive publish_started -> ok after 1000 -> not_started end),
    ok = macula_publisher:cancel(Pid),
    ?assertMatch({fact, <<"pubsub.publish_started_v1">>, _}, next_fact()),
    ?assertMatch({fact, <<"pubsub.publish_completed_v1">>, #{outcome := cancelled}},
                 next_fact()).

%% Without publish functions the publisher announces with
%% macula:publish/4, which passes a pool that is not a process on to
%% macula_pubsub:publish/5, whose guard refuses it.
without_publish_functions_it_announces_through_macula() ->
    process_flag(trap_exit, true),
    ?assertMatch({error, {function_clause, [{macula_pubsub, publish, _, _} | _]}},
                 macula_publisher:start_link(?MODULE, pool, ?REALM, ?TOPIC, #{x => 1}, self())).

a_publish_option_that_is_not_an_arity_4_fun_is_refused() ->
    ?assertError(function_clause,
                 macula_publisher:start_link(?MODULE, pool, ?REALM, ?TOPIC, #{x => 1}, self(),
                                             #{fact_publish => fun(_Pool) -> ok end})).

%%%===================================================================
%%% Helpers
%%%===================================================================

next_fact() ->
    receive
        {fact, _, _} = Fact -> Fact
    after 1000 ->
        no_fact
    end.

wait_published() ->
    receive
        {published, _} = Published -> Published
    after 1000 ->
        timeout
    end.
