%%%-------------------------------------------------------------------
%%% @doc Tests for macula_response. Each response advertises, publishes
%%% its DHT record and announces with functions the test gives it, so no
%%% test replaces a module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_response_tests).

-include_lib("eunit/include/eunit.hrl").

-behaviour(macula_response).
-export([init/1, handle_request/2]).

-define(REALM, <<0:256>>).
-define(PROCEDURE, <<"math.add_v1">>).

%%%===================================================================
%%% Test callback module
%%%===================================================================

init(fail) -> {stop, init_failed};
init(_Args) -> {ok, #{}}.

handle_request(#{a := A, b := B}, State) ->
    {reply, #{result => A + B}, State};
handle_request(#{boom := true}, _State) ->
    error(boom);
handle_request(bad, State) ->
    {error, invalid_payload, State}.

%%%===================================================================
%%% Advertise and publish functions
%%%===================================================================

%% An advertise function that sends Test the handler and options it gets
%% and answers Result.
advertising_to(Test, Result) ->
    fun(_Pool, _Realm, _Procedure, Handler, Opts) ->
            Test ! {advertised, Handler, Opts},
            Result
    end.

%% A fact publish function that sends Test each fact.
facts_to(Test) ->
    fun(_Pool, _Realm, Topic, Payload) ->
            Test ! {fact, Topic, Payload},
            ok
    end.

functions(Test) ->
    #{advertise => advertising_to(Test, ok), fact_publish => facts_to(Test)}.

advertise(Opts) ->
    macula_response:advertise(pool, ?REALM, ?PROCEDURE, ?MODULE, [], Opts).

%%%===================================================================
%%% Tests
%%%===================================================================

%% Each test runs in a process of its own.
response_test_() ->
    [{spawn, Test}
     || Test <- [fun replies_and_publishes_lifecycle/0,
                 fun error_reply_is_surfaced/0,
                 fun crash_propagates_to_caller/0,
                 fun advertise_failure_is_surfaced/0,
                 fun advertise_direct_forwards_opts_to_advertise/0,
                 fun reuse_sup_resends_advertise_without_a_new_supervisor/0,
                 fun reuse_sup_with_a_dead_pid_starts_a_fresh_supervisor/0,
                 fun without_an_advertise_function_it_advertises_through_macula/0,
                 fun an_advertise_option_that_is_not_an_arity_5_fun_is_refused/0]].

%% A station's wire-level registration for a procedure is tied to the
%% connection that sent it, and does not survive that connection being
%% replaced -- a periodic re-advertise is the only way to keep it
%% current. `reuse_sup' is what makes that safe: without it, every
%% re-advertise call starts a fresh factory supervisor, leaking one per
%% tick forever.
reuse_sup_resends_advertise_without_a_new_supervisor() ->
    {ok, Sup1} = advertise(functions(self())),
    {ok, Sup2} = advertise((functions(self()))#{reuse_sup => Sup1}),
    ?assertEqual(Sup1, Sup2),
    ?assertMatch({_, #{}}, next_advertised()),
    ?assertMatch({_, #{reuse_sup := Sup1}}, next_advertised()).

%% Regression test for the noproc-on-first-dispatch bug (found live
%% 2026-09-01 via hecate-rag): a caller that reuses a `reuse_sup' pid
%% across republish ticks can find that pid already dead -- e.g. the
%% caller itself crashed between ticks and, being linked to the sup it
%% started, took it down too. Reusing a dead pid unconditionally used to
%% hand `dispatch/7' a `Sup' that would `noproc' on its very first
%% `supervisor:start_child', silently breaking the procedure until a
%% later re-advertise happened to land.
reuse_sup_with_a_dead_pid_starts_a_fresh_supervisor() ->
    DeadPid = spawn(fun() -> ok end),
    wait_until_dead(DeadPid),
    {ok, Sup} = advertise((functions(self()))#{reuse_sup => DeadPid}),
    ?assert(is_pid(Sup)),
    ?assertNotEqual(DeadPid, Sup),
    ?assert(erlang:is_process_alive(Sup)).

wait_until_dead(Pid) ->
    wait_until_dead(Pid, erlang:is_process_alive(Pid)).

wait_until_dead(_Pid, false) -> ok;
wait_until_dead(Pid, true) -> timer:sleep(1), wait_until_dead(Pid, erlang:is_process_alive(Pid)).

%% advertise_direct/7 passes its options on to advertise/6, so `announce'
%% and `auth' apply to a direct-dial advertised procedure too, and both
%% the advertise and the DHT record publish get the options without the
%% three function options.
advertise_direct_forwards_opts_to_advertise() ->
    Test = self(),
    PublishAdvertisement = fun(_Pool, _Realm, _Procedure, _Identity, Opts) ->
                                   Test ! {advertisement_published, Opts},
                                   ok
                           end,
    Opts = (functions(Test))#{announce => false, publish_advertisement => PublishAdvertisement},
    {ok, _Sup} = macula_response:advertise_direct(pool, ?REALM, ?PROCEDURE, ?MODULE, [],
                                                  macula_identity:generate(), Opts),
    {_Handler, Advertised} = next_advertised(),
    ?assertEqual(#{announce => false}, Advertised),
    Published = receive
                    {advertisement_published, PublishedOpts} -> PublishedOpts
                after 1000 ->
                    not_published
                end,
    ?assertEqual(#{announce => false}, Published).

replies_and_publishes_lifecycle() ->
    {ok, _Sup} = advertise(functions(self())),
    {Handler, _} = next_advertised(),
    ?assertEqual({ok, #{result => 5}}, Handler(#{a => 2, b => 3})),
    ?assertMatch({fact, <<"rpc.received_v1">>, _}, next_fact()),
    ?assertMatch({fact, <<"rpc.replied_v1">>, #{outcome := replied}}, next_fact()).

error_reply_is_surfaced() ->
    {ok, _Sup} = advertise(functions(self())),
    {Handler, _} = next_advertised(),
    ?assertEqual({error, invalid_payload}, Handler(bad)),
    ?assertMatch({fact, <<"rpc.received_v1">>, _}, next_fact()),
    ?assertMatch({fact, <<"rpc.replied_v1">>, #{outcome := failed, reason := invalid_payload}},
                 next_fact()).

crash_propagates_to_caller() ->
    {ok, _Sup} = advertise(functions(self())),
    {Handler, _} = next_advertised(),
    ?assertExit(_, Handler(#{boom => true})).

advertise_failure_is_surfaced() ->
    Failing = #{advertise => advertising_to(self(), {error, no_healthy_link}),
                fact_publish => facts_to(self())},
    ?assertEqual({error, no_healthy_link}, advertise(Failing)).

%% Without an advertise function the response advertises with
%% macula:advertise/5, whose guard refuses a pool that is not a process.
without_an_advertise_function_it_advertises_through_macula() ->
    Stack = try macula_response:advertise(pool, ?REALM, ?PROCEDURE, ?MODULE, []) of
                Returned -> {returned, Returned}
            catch
                error:function_clause:Trace -> Trace
            end,
    ?assertMatch([{macula, advertise, _, _} | _], Stack).

an_advertise_option_that_is_not_an_arity_5_fun_is_refused() ->
    ?assertError(function_clause, advertise(#{advertise => fun(_Pool) -> ok end})).

%%%===================================================================
%%% Helpers
%%%===================================================================

next_advertised() ->
    receive
        {advertised, Handler, Opts} -> {Handler, Opts}
    after 1000 ->
        error(nothing_advertised)
    end.

next_fact() ->
    receive
        {fact, _, _} = Fact -> Fact
    after 1000 ->
        no_fact
    end.
