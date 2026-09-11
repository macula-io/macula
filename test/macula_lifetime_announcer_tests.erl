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
%% Data a reason carries that must stay on this node.
-define(MARKER, <<"marker-3f9c-stays-on-this-node">>).
%% Well above what the log gets of a reason, far below a whole large one.
-define(LOGGED_BYTES, 8192).

%% Each test runs in a process of its own.
announcer_test_() ->
    [{spawn, Test}
     || Test <- [fun the_start_fact_goes_out_first_and_the_handed_end_after_it/0,
                 fun a_handed_end_and_an_exit_at_once_publish_one_end_fact/0,
                 fun a_wrapper_that_goes_down_first_has_its_end_announced_with_the_reason/0,
                 fun a_handed_end_names_its_reason_and_carries_none_of_its_terms/0,
                 fun a_wrapper_down_for_a_reason_with_data_ends_with_the_reasons_name/0,
                 fun a_publish_that_fails_is_logged_and_the_end_still_goes_out/0,
                 fun a_publish_that_never_returns_holds_neither_start_nor_hand_over/0,
                 fun without_announcing_nothing_starts/0,
                 fun a_reason_that_is_more_than_a_name_is_logged_within_bounds/0,
                 fun a_publish_that_fails_for_a_large_reason_is_logged_within_bounds/0]].

the_start_fact_goes_out_first_and_the_handed_end_after_it() ->
    {Wrapper, Announcer} = start_wrapper(facts(publishing_to(self()))),
    ?assertEqual({?STARTED, #{id => 1, size => 3}}, next_published()),
    Wrapper ! {announce_end, #{id => 1, outcome => completed}},
    ?assertEqual({?ENDED, #{id => 1, outcome => completed}}, next_published()),
    wait_down(Announcer),
    no_more_published(),
    Wrapper ! stop.

a_handed_end_and_an_exit_at_once_publish_one_end_fact() ->
    {Wrapper, Announcer} = start_wrapper(facts(publishing_to(self()))),
    ?assertMatch({?STARTED, _}, next_published()),
    Wrapper ! {announce_end_and_exit, #{id => 1, outcome => completed}},
    ?assertEqual({?ENDED, #{id => 1, outcome => completed}}, next_published()),
    wait_down(Announcer),
    no_more_published().

a_wrapper_that_goes_down_first_has_its_end_announced_with_the_reason() ->
    {Wrapper, Announcer} = start_wrapper(facts(publishing_to(self()))),
    ?assertMatch({?STARTED, _}, next_published()),
    exit(Wrapper, kill),
    ?assertEqual({?ENDED, #{id => 1, outcome => failed, reason => <<"killed">>}},
                 next_published()),
    wait_down(Announcer),
    no_more_published().

a_handed_end_names_its_reason_and_carries_none_of_its_terms() ->
    {Wrapper, Announcer} = start_wrapper(facts(publishing_to(self()))),
    ?assertMatch({?STARTED, _}, next_published()),
    Wrapper ! {announce_end, #{id => 1, outcome => failed, reason => {badmatch, ?MARKER}}},
    {?ENDED, End} = next_published(),
    ?assertEqual(#{id => 1, outcome => failed, reason => <<"badmatch">>}, End),
    ?assertEqual(nomatch, binary:match(term_to_binary(End), ?MARKER)),
    wait_down(Announcer),
    no_more_published(),
    Wrapper ! stop.

a_wrapper_down_for_a_reason_with_data_ends_with_the_reasons_name() ->
    {Wrapper, Announcer} = start_wrapper(facts(publishing_to(self()))),
    ?assertMatch({?STARTED, _}, next_published()),
    Wrapper ! {exit, {{badmatch, ?MARKER}, [{a_module, a_function, [?MARKER], []}]}},
    {?ENDED, End} = next_published(),
    ?assertEqual(#{id => 1, outcome => failed, reason => <<"badmatch">>}, End),
    ?assertEqual(nomatch, binary:match(term_to_binary(End), ?MARKER)),
    wait_down(Announcer),
    no_more_published().

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
    no_more_published(),
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

a_reason_that_is_more_than_a_name_is_logged_within_bounds() ->
    Log = macula_test_log:capture(),
    {Wrapper, Announcer} = start_wrapper(facts(publishing_to(self()))),
    ?assertMatch({?STARTED, _}, next_published()),
    Wrapper ! {exit, {badmatch, lists:duplicate(10000, ?MARKER)}},
    ?assertMatch({?ENDED, #{reason := <<"badmatch">>}}, next_published()),
    wait_down(Announcer),
    Logged = macula_test_log:wait_text(<<"ends for">>, 1000),
    ok = macula_test_log:release(Log),
    ?assert(byte_size(Logged) < ?LOGGED_BYTES),
    ?assertNotEqual(nomatch, binary:match(Logged, <<"badmatch">>)).

a_publish_that_fails_for_a_large_reason_is_logged_within_bounds() ->
    Log = macula_test_log:capture(),
    Test = self(),
    Failing = fun(_Pool, _Realm, Topic, Payload) ->
                      Test ! {published, Topic, Payload},
                      exit({noproc, lists:duplicate(10000, ?MARKER)})
              end,
    {Wrapper, Announcer} = start_wrapper(facts(Failing)),
    ?assertMatch({?STARTED, _}, next_published()),
    Logged = macula_test_log:wait_text(<<"not published">>, 1000),
    Wrapper ! stop,
    ?assertMatch({?ENDED, _}, next_published()),
    wait_down(Announcer),
    ok = macula_test_log:release(Log),
    ?assert(byte_size(Logged) < ?LOGGED_BYTES).

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

%% A wrapper process that starts its announcer and then does what the
%% test tells it: hand over an end payload, hand one over and exit at
%% once, exit for a reason, or stop.
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
    wrapper_loop(Test, Announcer).

wrapper_loop(Test, Announcer) ->
    receive
        {announce_end, Payload} ->
            ok = macula_lifetime_announcer:announce_end(Announcer, Payload),
            Test ! {handed, self()},
            wrapper_loop(Test, Announcer);
        {announce_end_and_exit, Payload} ->
            ok = macula_lifetime_announcer:announce_end(Announcer, Payload),
            exit(normal);
        {exit, Reason} ->
            exit(Reason);
        stop ->
            ok
    end.

next_published() ->
    receive
        {published, Topic, Payload} -> {Topic, Payload}
    after 5000 ->
        error(nothing_published)
    end.

%% Once the announcer is down, every fact it published is in the mailbox.
no_more_published() ->
    receive
        {published, _, _} = Extra -> error({published_more, Extra})
    after 0 ->
        ok
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
