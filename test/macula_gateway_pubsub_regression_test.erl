-module(macula_gateway_pubsub_regression_test).
-include_lib("eunit/include/eunit.hrl").

%%% @doc Regression test for v0.7.7 issue where get_subscribers returns empty
%%% even though streams have successfully subscribed.

subscribe_then_get_subscribers_test() ->
    %% Start pubsub module
    {ok, PubSub} = macula_gateway_pubsub:start_link(#{}),

    %% Create mock stream PIDs (use self() for simplicity)
    Stream1 = spawn(fun() -> timer:sleep(5000) end),
    Stream2 = spawn(fun() -> timer:sleep(5000) end),
    Stream3 = spawn(fun() -> timer:sleep(5000) end),

    Topic = <<"arcade.matchmaking.snake">>,

    %% Subscribe all three streams
    ok = macula_gateway_pubsub:subscribe(PubSub, Stream1, Topic),
    ok = macula_gateway_pubsub:subscribe(PubSub, Stream2, Topic),
    ok = macula_gateway_pubsub:subscribe(PubSub, Stream3, Topic),

    io:format("~n[TEST] Subscribed 3 streams to topic: ~p~n", [Topic]),

    %% Get subscribers - this should return all 3 streams
    {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(PubSub, Topic),

    io:format("[TEST] get_subscribers returned: ~p subscribers~n", [length(Subscribers)]),
    io:format("[TEST] Subscribers: ~p~n", [Subscribers]),

    %% Verify we got all 3 subscribers
    ?assertEqual(3, length(Subscribers)),
    ?assert(lists:member(Stream1, Subscribers)),
    ?assert(lists:member(Stream2, Subscribers)),
    ?assert(lists:member(Stream3, Subscribers)),

    %% Cleanup
    macula_gateway_pubsub:stop(PubSub),
    exit(Stream1, kill),
    exit(Stream2, kill),
    exit(Stream3, kill),
    ok.
