%%%-------------------------------------------------------------------
%%% @doc Tests for macula_lifetime_announcer. Each announcer runs for a
%%% wrapper process the test starts, on a publish function the test
%%% gives, so no test replaces a module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_lifetime_announcer_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(STARTED, <<"example.started_v1">>).
-define(ENDED, <<"example.completed_v1">>).
%% How long a start or a hand-over may take while publishing never returns.
-define(BOUND_MS, 1000).

%% Each test runs in a process of its own.
announcer_test_() ->
    [{spawn, Test}
     || Test <- [fun the_start_fact_goes_out_first_and_the_handed_end_after_it/0,
                 fun a_wrapper_that_goes_down_first_has_its_end_announced_with_the_reason/0,
                 fun a_publish_that_fails_is_logged_and_the_end_still_goes_out/0,
                 fun a_publish_that_never_returns_holds_neither_start_nor_hand_over/0,
                 fun without_announcing_nothing_starts/0]].

the_start_fact_goes_out_first_and_the_handed_end_after_it() ->
    {Wrapper, Announcer} = start_wrapper(facts(publishing_to(self()))),
    ?assertEqual({?STARTED, #{id => 1, size => 3}}, next_published()),
    Wrapper ! {announce_end, #{id => 1, outcome => completed}},
    ?assertEqual({?ENDED, #{id => 1, outcome => completed}}, next_published()),
    wait_down(Announcer),
    Wrapper ! stop.

a_wrapper_that_goes_down_first_has_its_end_announced_with_the_reason() ->
    {Wrapper, Announcer} = start_wrapper(facts(publishing_to(self()))),
    ?assertMatch({?STARTED, _}, next_published()),
    exit(Wrapper, kill),
    ?assertEqual({?ENDED, #{id => 1, outcome => failed, reason => killed}}, next_published()),
    wait_down(Announcer).

a_publish_that_fails_is_logged_and_the_end_still_goes_out() ->
    Test = self(),
    Failing = fun(_Pool, _Realm, Topic, Payload) ->
                      Test ! {published, Topic, Payload},
                      exit({noproc, {gen_server, call, [pool, publish]}})
              end,
    {Wrapper, Announcer} = start_wrapper(facts(Failing)),
    ?assertMatch({?STARTED, _}, next_published()),
    Wrapper ! {announce_end, #{id => 1, outcome => completed}},
    ?assertEqual({?ENDED, #{id => 1, outcome => completed}}, next_published()),
    wait_down(Announcer),
    ?assert(is_process_alive(Wrapper)),
    Wrapper ! stop.

a_publish_that_never_returns_holds_neither_start_nor_hand_over() ->
    Test = self(),
    Held = fun(_Pool, _Realm, Topic, _Payload) ->
                   Test ! {publish_held, Topic, self()},
                   receive release -> ok end
           end,
    {StartMs, {Wrapper, Announcer}} = timer:tc(fun() -> start_wrapper(facts(Held)) end,
                                               millisecond),
    Wrapper ! {announce_end, #{id => 1, outcome => completed}},
    Handed = receive
                 {handed, Wrapper} -> handed
             after ?BOUND_MS ->
                 not_handed_within_bound
             end,
    Announcer = release(?STARTED),
    Announcer = release(?ENDED),
    wait_down(Announcer),
    Wrapper ! stop,
    ?assertEqual(handed, Handed),
    ?assert(StartMs < ?BOUND_MS).

without_announcing_nothing_starts() ->
    ?assertEqual(undefined, macula_lifetime_announcer:start(false, facts(publishing_to(self())))),
    ?assertEqual(ok, macula_lifetime_announcer:announce_end(undefined, #{id => 1})).

%%%===================================================================
%%% Helpers
%%%===================================================================

facts(Publish) ->
    #{publish => Publish, pool => pool, realm => ?REALM,
      started => {?STARTED, #{id => 1, size => 3}},
      ended => {?ENDED, #{id => 1}}}.

%% A publish function that sends the test each fact it publishes.
publishing_to(Test) ->
    fun(_Pool, _Realm, Topic, Payload) ->
            Test ! {published, Topic, Payload},
            ok
    end.

%% A wrapper process that starts its announcer, hands over the end
%% payload it is sent, and waits to be told to stop.
start_wrapper(Facts) ->
    Test = self(),
    Wrapper = spawn(fun() -> wrapper(Test, Facts) end),
    receive
        {announcer, Wrapper, Announcer} -> {Wrapper, Announcer}
    after 5000 ->
        error(no_announcer)
    end.

wrapper(Test, Facts) ->
    Announcer = macula_lifetime_announcer:start(true, Facts),
    Test ! {announcer, self(), Announcer},
    receive
        {announce_end, Payload} ->
            ok = macula_lifetime_announcer:announce_end(Announcer, Payload),
            Test ! {handed, self()}
    end,
    receive
        stop -> ok
    end.

next_published() ->
    receive
        {published, Topic, Payload} -> {Topic, Payload}
    after 5000 ->
        error(nothing_published)
    end.

release(Topic) ->
    receive
        {publish_held, Topic, Announcer} ->
            Announcer ! release,
            Announcer
    after 5000 ->
        error({publish_not_held, Topic})
    end.

wait_down(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 ->
        error({did_not_exit, Pid})
    end.
