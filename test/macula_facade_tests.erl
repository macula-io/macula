%% EUnit smoke tests for the V2 surface re-exports on the `macula'
%% facade. The pool's behaviour is covered by `macula_client_tests';
%% these cases verify the facade routes to the right implementation.
-module(macula_facade_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).

connect_returns_pool_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula:connect([], #{}),
    ?assert(is_process_alive(Pool)),
    ok = macula:close(Pool),
    ok.

child_spec_returns_supervisor_map_test() ->
    Spec = macula:child_spec(my_pool, [], #{}),
    ?assertMatch(#{id := my_pool,
                   start := {macula_client, connect, [[], #{}]},
                   restart := permanent,
                   type := worker}, Spec),
    ok.

publish_subscribe_unsubscribe_round_trip_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula:connect([], #{}),
         Topic = <<"facade.smoke_v1">>,
         {ok, SubRef} = macula:subscribe(Pool, ?REALM, Topic, self()),
         %% Inject a synthetic event at the pool — the facade
         %% subscribe routed through to macula_client which keeps
         %% the same fan-out semantics.
         Pool ! {macula_event, make_ref(), Topic, hello,
                 #{realm => ?REALM, publisher => <<1:256>>,
                   seq => 1, delivered_via => direct}},
         receive
             {macula_event, R, T, P, _} ->
                 ?assertEqual(SubRef, R),
                 ?assertEqual(Topic, T),
                 ?assertEqual(hello, P)
         after 2_000 -> erlang:error(no_event)
         end,
         ok = macula:unsubscribe(Pool, SubRef),
         ok = macula:close(Pool),
         ok
     end}.

publish_with_no_links_returns_transient_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         {ok, Pool} = macula:connect([], #{}),
         R1 = macula:publish(Pool, ?REALM, <<"x.v1">>, hello),
         R2 = macula:publish(Pool, ?REALM, <<"x.v1">>, hello, #{}),
         ?assertEqual({error, {transient, no_healthy_station}}, R1),
         ?assertEqual({error, {transient, no_healthy_station}}, R2),
         ok = macula:close(Pool),
         ok
     end}.
