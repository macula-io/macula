%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for macula_gateway
%%% Tests gateway functionality including client connections, pub/sub routing,
%%% and RPC handling.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_gateway_start/1,
    test_gateway_stats/1,
    test_client_connection_tracking/1,
    test_subscribe_routing/1,
    test_publish_routing/1,
    test_multiple_subscribers/1,
    test_unsubscribe/1,
    test_client_disconnect_cleanup/1,
    test_rpc_registration/1,
    test_rpc_call_routing/1,
    test_rpc_no_handler/1,
    test_realm_validation/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        test_gateway_start,
        test_gateway_stats,
        test_client_connection_tracking,
        test_subscribe_routing,
        test_publish_routing,
        test_multiple_subscribers,
        test_unsubscribe,
        test_client_disconnect_cleanup,
        test_rpc_registration,
        test_rpc_call_routing,
        test_rpc_no_handler,
        test_realm_validation
    ].

init_per_suite(Config) ->
    %% Ensure required applications are started
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Start a fresh gateway for each test
    Port = 9443 + rand:uniform(1000),  % Random port to avoid conflicts
    Realm = <<"test.realm">>,
    {ok, Gateway} = macula_gateway:start_link([
        {port, Port},
        {realm, Realm}
    ]),

    %% Give gateway time to start listener
    timer:sleep(100),

    [{gateway, Gateway}, {port, Port}, {realm, Realm} | Config].

end_per_testcase(_TestCase, Config) ->
    Gateway = ?config(gateway, Config),
    macula_gateway:stop(Gateway),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_gateway_start(Config) ->
    Gateway = ?config(gateway, Config),
    Port = ?config(port, Config),
    Realm = ?config(realm, Config),

    %% Gateway should be alive
    ?assert(is_process_alive(Gateway)),

    %% Should have correct initial stats
    Stats = macula_gateway:get_stats(Gateway),
    ?assertEqual(Port, maps:get(port, Stats)),
    ?assertEqual(Realm, maps:get(realm, Stats)),
    ?assertEqual(0, maps:get(clients, Stats)),
    ?assertEqual(0, maps:get(subscriptions, Stats)),
    ?assertEqual(0, maps:get(registrations, Stats)),

    ok.

test_gateway_stats(Config) ->
    Gateway = ?config(gateway, Config),

    %% Get initial stats
    Stats1 = macula_gateway:get_stats(Gateway),
    ?assertEqual(0, maps:get(clients, Stats1)),

    %% Simulate client connection
    ClientInfo = #{
        realm => <<"test.realm">>,
        node_id => <<"client1">>,
        capabilities => [publisher, subscriber]
    },
    Gateway ! {client_connected, self(), ClientInfo},

    %% Give it time to process
    timer:sleep(50),

    %% Stats should reflect new client
    Stats2 = macula_gateway:get_stats(Gateway),
    ?assertEqual(1, maps:get(clients, Stats2)),

    ok.

test_client_connection_tracking(Config) ->
    Gateway = ?config(gateway, Config),

    %% Register two clients
    Client1 = spawn(fun() -> receive stop -> ok end end),
    Client2 = spawn(fun() -> receive stop -> ok end end),

    ClientInfo1 = #{
        realm => <<"test.realm">>,
        node_id => <<"client1">>,
        capabilities => [publisher]
    },
    ClientInfo2 = #{
        realm => <<"test.realm">>,
        node_id => <<"client2">>,
        capabilities => [subscriber]
    },

    Gateway ! {client_connected, Client1, ClientInfo1},
    Gateway ! {client_connected, Client2, ClientInfo2},

    timer:sleep(50),

    %% Check stats
    Stats = macula_gateway:get_stats(Gateway),
    ?assertEqual(2, maps:get(clients, Stats)),

    %% Cleanup
    Client1 ! stop,
    Client2 ! stop,

    ok.

test_subscribe_routing(Config) ->
    Gateway = ?config(gateway, Config),
    Topic = <<"energy.home.measured">>,

    %% Create subscriber client
    TestPid = self(),
    Subscriber = spawn_link(fun() ->
        Gateway ! {subscribe, self(), Topic},
        receive
            {subscribed, RecvTopic} ->
                TestPid ! {subscription_confirmed, RecvTopic}
        after 1000 ->
            TestPid ! subscription_timeout
        end,
        %% Keep alive
        receive stop -> ok end
    end),

    %% Wait for subscription confirmation
    receive
        {subscription_confirmed, Topic} -> ok;
        subscription_timeout -> ct:fail("Subscription timed out")
    after 1500 ->
        ct:fail("No subscription response")
    end,

    %% Cleanup
    Subscriber ! stop,

    ok.

test_publish_routing(Config) ->
    Gateway = ?config(gateway, Config),
    Topic = <<"energy.home.measured">>,
    Payload = #{power => 1500, timestamp => 123456789},

    %% Create subscriber
    TestPid = self(),
    Subscriber = spawn_link(fun() ->
        Gateway ! {subscribe, self(), Topic},
        receive {subscribed, _} -> ok after 1000 -> ok end,
        TestPid ! subscriber_ready,
        receive
            {event, RecvTopic, RecvPayload} ->
                TestPid ! {event_received, RecvTopic, RecvPayload}
        after 2000 ->
            TestPid ! event_timeout
        end,
        receive stop -> ok end
    end),

    %% Wait for subscriber to be ready
    receive subscriber_ready -> ok after 1000 -> ok end,
    timer:sleep(50),

    %% Create publisher
    Publisher = spawn_link(fun() ->
        Gateway ! {publish, self(), Topic, Payload},
        receive stop -> ok end
    end),

    %% Subscriber should receive event
    receive
        {event_received, Topic, Payload} -> ok;
        event_timeout -> ct:fail("Event not received")
    after 2500 ->
        ct:fail("No event received")
    end,

    %% Cleanup
    Subscriber ! stop,
    Publisher ! stop,

    ok.

test_multiple_subscribers(Config) ->
    Gateway = ?config(gateway, Config),
    Topic = <<"test.topic">>,
    Payload = #{data => <<"hello">>},

    TestPid = self(),

    %% Create 3 subscribers
    Subscribers = [
        spawn_link(fun() ->
            Gateway ! {subscribe, self(), Topic},
            receive {subscribed, _} -> ok after 1000 -> ok end,
            receive
                {event, _, RecvPayload} ->
                    TestPid ! {sub_received, self(), RecvPayload}
            after 2000 ->
                TestPid ! {sub_timeout, self()}
            end,
            receive stop -> ok end
        end)
        || _ <- lists:seq(1, 3)
    ],

    %% Wait for subscriptions
    timer:sleep(100),

    %% Publish event
    Gateway ! {publish, self(), Topic, Payload},

    %% All 3 should receive
    ReceivedCount = lists:foldl(fun(_Sub, Acc) ->
        receive
            {sub_received, _Pid, Payload} -> Acc + 1;
            {sub_timeout, _Pid} -> Acc
        after 2500 ->
            Acc
        end
    end, 0, Subscribers),

    ?assertEqual(3, ReceivedCount),

    %% Cleanup
    [Sub ! stop || Sub <- Subscribers],

    ok.

test_unsubscribe(Config) ->
    Gateway = ?config(gateway, Config),
    Topic = <<"test.topic">>,

    TestPid = self(),
    Subscriber = spawn_link(fun() ->
        %% Subscribe
        Gateway ! {subscribe, self(), Topic},
        receive {subscribed, _} -> ok after 1000 -> ok end,
        TestPid ! subscribed,

        %% Unsubscribe
        receive do_unsubscribe -> ok end,
        Gateway ! {unsubscribe, self(), Topic},
        receive {unsubscribed, _} -> ok after 1000 -> ok end,
        TestPid ! unsubscribed,

        %% Should NOT receive events now
        receive
            {event, _, _} -> TestPid ! unexpected_event
        after 500 ->
            TestPid ! no_event
        end,
        receive stop -> ok end
    end),

    %% Wait for subscription
    receive subscribed -> ok after 1500 -> ok end,
    timer:sleep(50),

    %% Unsubscribe
    Subscriber ! do_unsubscribe,
    receive unsubscribed -> ok after 1500 -> ok end,
    timer:sleep(50),

    %% Publish - subscriber should NOT receive
    Gateway ! {publish, self(), Topic, #{data => <<"test">>}},

    receive
        no_event -> ok;  % Expected
        unexpected_event -> ct:fail("Received event after unsubscribe")
    after 1000 ->
        ct:fail("No response from subscriber")
    end,

    %% Cleanup
    Subscriber ! stop,

    ok.

test_client_disconnect_cleanup(Config) ->
    Gateway = ?config(gateway, Config),
    Topic = <<"test.topic">>,
    Procedure = <<"test.procedure">>,

    %% Create client that subscribes and registers
    Client = spawn_link(fun() ->
        Gateway ! {subscribe, self(), Topic},
        Gateway ! {register, self(), Procedure},
        receive stop -> ok end
    end),

    timer:sleep(100),

    %% Verify client is tracked
    Stats1 = macula_gateway:get_stats(Gateway),
    ?assertEqual(1, maps:get(clients, Stats1)),

    %% Kill client
    exit(Client, kill),
    timer:sleep(100),

    %% Gateway should have cleaned up
    Stats2 = macula_gateway:get_stats(Gateway),
    ?assertEqual(0, maps:get(clients, Stats2)),

    ok.

test_rpc_registration(Config) ->
    Gateway = ?config(gateway, Config),
    Procedure = <<"calculate.sum">>,

    TestPid = self(),
    Handler = spawn_link(fun() ->
        Gateway ! {register, self(), Procedure},
        receive
            {registered, RecvProc} ->
                TestPid ! {registration_confirmed, RecvProc}
        after 1000 ->
            TestPid ! registration_timeout
        end,
        receive stop -> ok end
    end),

    %% Wait for registration
    receive
        {registration_confirmed, Procedure} -> ok;
        registration_timeout -> ct:fail("Registration timed out")
    after 1500 ->
        ct:fail("No registration response")
    end,

    %% Check stats
    Stats = macula_gateway:get_stats(Gateway),
    ?assertEqual(1, maps:get(registrations, Stats)),

    %% Cleanup
    Handler ! stop,

    ok.

test_rpc_call_routing(Config) ->
    Gateway = ?config(gateway, Config),
    Procedure = <<"calculate.sum">>,
    CallId = <<"call-123">>,
    Args = #{a => 5, b => 3},

    TestPid = self(),

    %% Create handler
    Handler = spawn_link(fun() ->
        Gateway ! {register, self(), Procedure},
        receive {registered, _} -> ok after 1000 -> ok end,
        TestPid ! handler_ready,
        receive
            {invoke, CallerPid, RecvCallId, RecvProc, RecvArgs} ->
                TestPid ! {invoke_received, CallerPid, RecvCallId, RecvProc, RecvArgs}
        after 2000 ->
            TestPid ! invoke_timeout
        end,
        receive stop -> ok end
    end),

    %% Wait for handler
    receive handler_ready -> ok after 1500 -> ok end,
    timer:sleep(50),

    %% Create caller
    Caller = spawn_link(fun() ->
        Gateway ! {call, self(), CallId, Procedure, Args},
        receive stop -> ok end
    end),

    %% Handler should receive invocation
    receive
        {invoke_received, CallerPid, CallId, Procedure, Args} ->
            ?assertEqual(Caller, CallerPid);
        invoke_timeout ->
            ct:fail("Invocation not received")
    after 2500 ->
        ct:fail("No invocation received")
    end,

    %% Cleanup
    Handler ! stop,
    Caller ! stop,

    ok.

test_rpc_no_handler(Config) ->
    Gateway = ?config(gateway, Config),
    Procedure = <<"nonexistent.procedure">>,
    CallId = <<"call-456">>,

    TestPid = self(),
    Caller = spawn_link(fun() ->
        Gateway ! {call, self(), CallId, Procedure, #{}},
        receive
            {call_error, RecvCallId, Error} ->
                TestPid ! {error_received, RecvCallId, Error}
        after 1000 ->
            TestPid ! error_timeout
        end,
        receive stop -> ok end
    end),

    %% Caller should receive error
    receive
        {error_received, CallId, Error} ->
            ?assertEqual(<<"wamp.error.no_such_procedure">>, Error);
        error_timeout ->
            ct:fail("Error not received")
    after 1500 ->
        ct:fail("No error received")
    end,

    %% Cleanup
    Caller ! stop,

    ok.

test_realm_validation(Config) ->
    %% This test would require mocking QUIC connection
    %% and is better suited for integration testing
    %% For now, we just verify the concept

    Gateway = ?config(gateway, Config),
    CorrectRealm = ?config(realm, Config),

    %% Verify gateway has correct realm
    Stats = macula_gateway:get_stats(Gateway),
    ?assertEqual(CorrectRealm, maps:get(realm, Stats)),

    ok.
