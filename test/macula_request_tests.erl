%%%-------------------------------------------------------------------
%%% @doc Tests for macula_request. Each request calls and announces with
%%% functions the test gives it, so no test replaces the macula module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_request_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_request).
-export([init/1, handle_reply/2]).

-define(REALM, <<0:256>>).
-define(PROCEDURE, <<"math.add_v1">>).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(Parent) -> {ok, Parent}.

handle_reply(Result, Parent) ->
    Parent ! {reply_seen, Result},
    {stop, normal, Parent}.

%%%===================================================================
%%% Call and publish functions
%%%===================================================================

%% A call function that answers Result.
answering(Result) ->
    fun(_Pool, _Realm, _Procedure, _Payload, _TimeoutMs) -> Result end.

%% A fact publish function that sends Test each fact.
facts_to(Test) ->
    fun(_Pool, _Realm, Topic, Payload) ->
            Test ! {fact, Topic, Payload},
            ok
    end.

start_request(Call, Payload) ->
    macula_request:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, Payload, 5_000, self(),
                              #{call => Call, fact_publish => facts_to(self())}).

%%%===================================================================
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
request_test_() ->
    [{spawn, Test}
     || Test <- [fun delivers_reply_and_publishes_lifecycle/0,
                 fun surfaces_call_error/0,
                 fun cancel_before_reply_announces_cancelled/0,
                 fun a_direct_request_calls_with_the_other_options/0,
                 fun without_a_call_function_it_calls_through_macula/0,
                 fun a_call_option_that_is_not_an_arity_5_fun_is_refused/0,
                 fun a_timeout_outside_its_bounds_is_refused_where_the_request_starts/0]].

delivers_reply_and_publishes_lifecycle() ->
    process_flag(trap_exit, true),
    {ok, _Pid} = start_request(answering({ok, #{result => 5}}), #{a => 2, b => 3}),
    ?assertEqual({reply_seen, {ok, #{result => 5}}}, wait_reply()),
    ?assertMatch({fact, <<"rpc.sent_v1">>, _}, next_fact()),
    ?assertMatch({fact, <<"rpc.completed_v1">>, #{outcome := completed}}, next_fact()).

surfaces_call_error() ->
    process_flag(trap_exit, true),
    {ok, _Pid} = start_request(answering({error, no_healthy_link}), #{}),
    ?assertEqual({reply_seen, {error, no_healthy_link}}, wait_reply()),
    ?assertMatch({fact, <<"rpc.sent_v1">>, _}, next_fact()),
    ?assertMatch({fact, <<"rpc.completed_v1">>, #{outcome := failed, reason := no_healthy_link}},
                 next_fact()).

cancel_before_reply_announces_cancelled() ->
    process_flag(trap_exit, true),
    Test = self(),
    Blocking = fun(_Pool, _Realm, _Procedure, _Payload, _TimeoutMs) ->
                       Test ! call_started,
                       receive never -> ok after 5_000 -> ok end,
                       {ok, too_late}
               end,
    {ok, Pid} = start_request(Blocking, #{}),
    ?assertEqual(ok, receive call_started -> ok after 1000 -> not_started end),
    ok = macula_request:cancel(Pid),
    ?assertMatch({fact, <<"rpc.sent_v1">>, _}, next_fact()),
    ?assertMatch({fact, <<"rpc.completed_v1">>, #{outcome := cancelled}}, next_fact()).

a_direct_request_calls_with_the_other_options() ->
    process_flag(trap_exit, true),
    Test = self(),
    DirectCall = fun(Pool, Realm, Procedure, Payload, TimeoutMs, Opts) ->
                         Test ! {direct_call, Pool, Realm, Procedure, Payload, TimeoutMs, Opts},
                         {ok, #{result => 5}}
                 end,
    Opts = #{direct_call => DirectCall, fact_publish => facts_to(self()),
             realm_trust => #{realm_key => <<"key">>}},
    {ok, _Pid} = macula_request:start_link_direct(?MODULE, pool, ?REALM, ?PROCEDURE, #{a => 2},
                                                  5_000, self(), Opts),
    ?assertEqual({reply_seen, {ok, #{result => 5}}}, wait_reply()),
    Called = receive
                 {direct_call, _, _, _, _, _, _} = Call -> Call
             after 1000 ->
                 not_called
             end,
    ?assertEqual({direct_call, pool, ?REALM, ?PROCEDURE, #{a => 2}, 5_000,
                  #{realm_trust => #{realm_key => <<"key">>}}}, Called).

%% Without a call function the request's worker calls macula:call/5, which
%% resolves the procedure through macula_direct_dial and passes a pool that
%% is not a process on to macula:find_records/3, whose guard refuses it, and
%% the worker's crash stops the request.
without_a_call_function_it_calls_through_macula() ->
    process_flag(trap_exit, true),
    {ok, Pid} = macula_request:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, #{}, 5_000, self(),
                                          #{fact_publish => facts_to(self())}),
    Reason = receive
                 {'EXIT', Pid, Exit} -> Exit
             after 5000 ->
                 no_exit
             end,
    ?assertMatch({worker_crashed, {function_clause, [{macula, find_records, _, _} | _]}}, Reason).

%% A request's timeout is a positive number of milliseconds up to ten minutes, the bound its call has; anything else is
%% refused where the request starts, not in its worker.
a_timeout_outside_its_bounds_is_refused_where_the_request_starts() ->
    Facts = facts_to(self()),
    DirectCall = fun(_Pool, _Realm, _Procedure, _Payload, _TimeoutMs, _Opts) -> {ok, 1} end,
    [begin
         ?assertError(function_clause,
                      macula_request:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, #{}, Timeout, self(),
                                                #{call => answering({ok, 1}), fact_publish => Facts})),
         ?assertError(function_clause,
                      macula_request:start_link_direct(?MODULE, pool, ?REALM, ?PROCEDURE, #{}, Timeout, self(),
                                                       #{direct_call => DirectCall, fact_publish => Facts}))
     end || Timeout <- [infinity, 0, 600_001]].

a_call_option_that_is_not_an_arity_5_fun_is_refused() ->
    ?assertError(function_clause,
                 macula_request:start_link(?MODULE, pool, ?REALM, ?PROCEDURE, #{}, 5_000, self(),
                                           #{call => fun(_Pool) -> ok end})).

%%%===================================================================
%%% Helpers
%%%===================================================================

next_fact() ->
    receive
        {fact, _, _} = Fact -> Fact
    after 1000 ->
        no_fact
    end.

wait_reply() ->
    receive
        {reply_seen, _} = Reply -> Reply
    after 1000 ->
        timeout
    end.
