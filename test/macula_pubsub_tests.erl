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
