%% EUnit smoke tests for the `macula_pubsub' slice module. The pool
%% delegation is exercised via `macula_client_tests'; these cases
%% verify input guards + the round-trip via the slice surface.
-module(macula_pubsub_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).

publish_rejects_short_realm_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    %% Realm not 32 bytes → function_clause via guard.
    ?assertError(function_clause,
        macula_pubsub:publish(Pool, <<0:128>>, <<"x.v1">>, hello)),
    ?assertError(function_clause,
        macula_pubsub:publish(Pool, <<0:128>>, <<"x.v1">>, hello, #{})),
    ok = macula_client:close(Pool),
    ok.

subscribe_rejects_short_realm_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    ?assertError(function_clause,
        macula_pubsub:subscribe(Pool, <<0:128>>, <<"x.v1">>, self())),
    ?assertError(function_clause,
        macula_pubsub:subscribe(Pool, <<0:128>>, <<"x.v1">>, self(), #{})),
    ok = macula_client:close(Pool),
    ok.

round_trip_through_slice_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"slice.smoke_v1">>,
         {ok, SubRef} = macula_pubsub:subscribe(Pool, ?REALM,
                                                Topic, self()),
         Pool ! {macula_event, make_ref(), Topic, world,
                 #{realm => ?REALM, publisher => <<1:256>>,
                   seq => 1, delivered_via => direct}},
         receive
             {macula_event, SubRef, Topic, world, _} -> ok
         after 2_000 -> erlang:error(no_event)
         end,
         ok = macula_pubsub:unsubscribe(Pool, SubRef),
         ok = macula_client:close(Pool),
         ok
     end}.

publish_no_links_returns_transient_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         R = macula_pubsub:publish(Pool, ?REALM, <<"x.v1">>, hello),
         ?assertEqual({error, {transient, no_healthy_station}}, R),
         ok = macula_client:close(Pool),
         ok
     end}.

%%------------------------------------------------------------------
%% subscribe_callback/4
%%------------------------------------------------------------------

subscribe_callback_invokes_fun_per_event_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"cb.smoke_v1">>,
         Self = self(),
         CB = fun(T, P, M) -> Self ! {got, T, P, M} end,
         {ok, SubRef} = macula_pubsub:subscribe_callback(
                           Pool, ?REALM, Topic, CB),
         Pool ! {macula_event, make_ref(), Topic, hello,
                 #{realm => ?REALM, publisher => <<2:256>>,
                   seq => 1, delivered_via => direct}},
         receive
             {got, Topic, hello, #{publisher := <<2:256>>}} -> ok
         after 2_000 -> erlang:error(no_callback)
         end,
         ok = macula_pubsub:unsubscribe(Pool, SubRef),
         ok = macula_client:close(Pool)
     end}.

subscribe_callback_swallows_callback_crash_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Topic = <<"cb.crash_v1">>,
         Self = self(),
         %% First event: crash. Second event: succeeds. Receiver
         %% must survive the crash and deliver event 2.
         Counter = counters:new(1, []),
         CB = fun(T, P, M) ->
             N = counters:get(Counter, 1),
             counters:add(Counter, 1, 1),
             case N of
                 0 -> error(deliberate_crash);
                 _ -> Self ! {got, T, P, M}
             end
         end,
         {ok, SubRef} = macula_pubsub:subscribe_callback(
                           Pool, ?REALM, Topic, CB),
         Pool ! {macula_event, make_ref(), Topic, first,
                 #{realm => ?REALM, publisher => <<3:256>>,
                   seq => 1, delivered_via => direct}},
         Pool ! {macula_event, make_ref(), Topic, second,
                 #{realm => ?REALM, publisher => <<3:256>>,
                   seq => 2, delivered_via => direct}},
         receive
             {got, Topic, second, _} -> ok
         after 2_000 -> erlang:error(receiver_died_on_crash)
         end,
         ok = macula_pubsub:unsubscribe(Pool, SubRef),
         ok = macula_client:close(Pool)
     end}.

subscribe_callback_rejects_wrong_arity_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([], #{}),
    Two = fun(_, _) -> ok end,
    ?assertError(function_clause,
                 macula_pubsub:subscribe_callback(Pool, ?REALM,
                                                   <<"a.v1">>, Two)),
    ok = macula_client:close(Pool).

subscribe_callback_caller_death_cleans_up_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula_client:connect([], #{}),
         Self = self(),
         %% Run subscribe_callback in a child; capture its SubRef via
         %% message; kill the child; assert pool's subscription
         %% bookkeeping drops the entry.
         Child = spawn(fun() ->
             {ok, SR} = macula_pubsub:subscribe_callback(
                          Pool, ?REALM, <<"cb.death_v1">>,
                          fun(_, _, _) -> ok end),
             Self ! {sub, self(), SR},
             receive _ -> ok end %% wait forever; we'll be killed
         end),
         SubRef = receive {sub, Child, SR} -> SR
                  after 2_000 -> erlang:error(no_subref) end,
         {ok, S0} = macula_client:status(Pool),
         ?assertEqual(1, maps:get(subscriptions, S0)),
         exit(Child, kill),
         %% Wait for the pool to process the subscriber-DOWN.
         wait_subscriptions_eq(Pool, 0, 50),
         {ok, S1} = macula_client:status(Pool),
         ?assertEqual(0, maps:get(subscriptions, S1)),
         _ = SubRef,
         ok = macula_client:close(Pool)
     end}.

wait_subscriptions_eq(_Pool, _Target, 0) ->
    erlang:error(subscriptions_did_not_drop);
wait_subscriptions_eq(Pool, Target, N) ->
    {ok, S} = macula_client:status(Pool),
    case maps:get(subscriptions, S) of
        Target -> ok;
        _ -> timer:sleep(50), wait_subscriptions_eq(Pool, Target, N - 1)
    end.
