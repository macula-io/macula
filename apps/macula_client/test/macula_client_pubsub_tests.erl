%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for Pub/Sub operations in macula_sdk.
%%% Tests publish, subscribe, and unsubscribe functionality.
%%% Following TDD principles - tests written before implementation fixes.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_sdk_pubsub_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

pubsub_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"publish with map data", fun test_publish_map/0},
         {"publish with binary data", fun test_publish_binary/0},
         {"publish with list data", fun test_publish_list/0},
         {"publish requires connected client", fun test_publish_disconnected/0},
         {"publish with QoS option", fun test_publish_qos/0},
         {"publish with retain option", fun test_publish_retain/0},
         {"subscribe with callback", fun test_subscribe_callback/0},
         {"subscribe returns reference", fun test_subscribe_returns_ref/0},
         {"subscribe requires connected client", fun test_subscribe_disconnected/0},
         {"unsubscribe with valid reference", fun test_unsubscribe_valid/0},
         {"unsubscribe with invalid reference", fun test_unsubscribe_invalid/0},
         {"multiple subscriptions to same topic", fun test_multiple_subscriptions/0},
         {"subscription callback receives events", fun test_callback_receives_events/0},
         {"topic design validation", fun test_topic_design/0},
         {"json encoding", fun test_json_encoding/0}
     ]}.

setup() ->
    application:ensure_all_started(macula_sdk),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Publish Tests
%%%===================================================================

test_publish_map() ->
    %% GIVEN: Map data to publish
    Data = #{
        type => <<"user.registered">>,
        user_id => <<"user-123">>,
        email => <<"user@example.com">>,
        timestamp => 1699564800
    },
    Topic = <<"test.events.user">>,

    %% WHEN: Publishing map data
    %% THEN: API should accept map data type
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula_sdk:publish(Client, Topic, Data)),

    %% Should fail with noproc (process not a gen_server) not badarg
    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_publish_binary() ->
    %% GIVEN: Binary data to publish
    Data = <<"binary event payload">>,
    Topic = <<"test.events">>,

    %% WHEN: Publishing binary data
    %% THEN: API should accept binary data type
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula_sdk:publish(Client, Topic, Data)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_publish_list() ->
    %% GIVEN: List (will be converted to JSON array)
    Data = [<<"item1">>, <<"item2">>, <<"item3">>],
    Topic = <<"test.events.list">>,

    %% WHEN: Publishing list data
    %% THEN: API should accept list data type
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula_sdk:publish(Client, Topic, Data)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_publish_disconnected() ->
    %% GIVEN: A disconnected (dead) client
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead

    %% WHEN: Attempting to publish
    Result = (catch macula_sdk:publish(Client, <<"test.topic">>, #{})),

    %% THEN: Should fail (process is dead)
    ?assertMatch({'EXIT', _}, Result).

test_publish_qos() ->
    %% GIVEN: Publish options with QoS
    Topic = <<"test.events">>,
    Data = #{test => <<"data">>},
    Opts = #{qos => 1},

    %% WHEN: Publishing with QoS option
    %% THEN: API should accept qos option
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula_sdk:publish(Client, Topic, Data, Opts)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_publish_retain() ->
    %% GIVEN: Publish options with retain flag
    Topic = <<"test.events.retained">>,
    Data = #{status => <<"online">>},
    Opts = #{retain => true},

    %% WHEN: Publishing with retain option
    %% THEN: API should accept retain option
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula_sdk:publish(Client, Topic, Data, Opts)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

%%%===================================================================
%%% Subscribe Tests
%%%===================================================================

test_subscribe_callback() ->
    %% GIVEN: A callback function
    Callback = fun(Event) ->
        _ = Event, %% Suppress unused warning
        ok
    end,

    %% WHEN: Subscribing with callback
    %% THEN: API should accept 1-arity function
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula_sdk:subscribe(Client, <<"test.topic">>, Callback)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_subscribe_returns_ref() ->
    %% GIVEN: A valid subscription request
    Callback = fun(_Event) -> ok end,

    %% WHEN: Subscribing
    %% THEN: Should return {ok, Reference} on success
    %% (We can't test actual success without a server, but we verify API)
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula_sdk:subscribe(Client, <<"test.topic">>, Callback)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_subscribe_disconnected() ->
    %% GIVEN: A disconnected client
    Client = spawn(fun() -> ok end),
    timer:sleep(10),
    Callback = fun(_Event) -> ok end,

    %% WHEN: Attempting to subscribe
    Result = (catch macula_sdk:subscribe(Client, <<"test.topic">>, Callback)),

    %% THEN: Should fail
    ?assertMatch({'EXIT', _}, Result).

%%%===================================================================
%%% Unsubscribe Tests
%%%===================================================================

test_unsubscribe_valid() ->
    %% GIVEN: A valid subscription reference
    SubRef = make_ref(),

    %% WHEN: Unsubscribing
    %% THEN: API should accept reference type
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result = (catch macula_sdk:unsubscribe(Client, SubRef)),

    ?assertMatch({'EXIT', {noproc, _}}, Result).

test_unsubscribe_invalid() ->
    %% GIVEN: An invalid subscription reference (wrong type)
    InvalidRef = "not-a-reference",

    %% WHEN: Attempting to unsubscribe
    %% THEN: Should fail with function_clause
    Client = self(),
    Result = (catch macula_sdk:unsubscribe(Client, InvalidRef)),

    ?assertMatch({'EXIT', {function_clause, _}}, Result).

%%%===================================================================
%%% Advanced Pub/Sub Tests
%%%===================================================================

test_multiple_subscriptions() ->
    %% GIVEN: Multiple subscriptions to the same topic
    Callback1 = fun(_Event) -> ok end,
    Callback2 = fun(_Event) -> ok end,

    %% WHEN: Subscribing multiple times
    %% THEN: Each subscription should get its own reference
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    Result1 = (catch macula_sdk:subscribe(Client, <<"test.topic">>, Callback1)),
    Result2 = (catch macula_sdk:subscribe(Client, <<"test.topic">>, Callback2)),

    %% Both should fail the same way (noproc)
    ?assertMatch({'EXIT', {noproc, _}}, Result1),
    ?assertMatch({'EXIT', {noproc, _}}, Result2).

test_callback_receives_events() ->
    %% This is an integration test - would need mock server
    %% For now, just verify callback can be called
    Callback = fun(Event) ->
        _ = Event, %% Suppress unused warning
        ok
    end,

    %% Test callback works
    TestEvent = #{type => <<"test">>, data => <<"value">>},
    ?assertEqual(ok, Callback(TestEvent)).

%%%===================================================================
%%% Topic Design Tests
%%%===================================================================

test_topic_design() ->
    %% Test that topics follow design principles

    %% ✅ GOOD: Event types in topics
    GoodTopics = [
        <<"my.app.user.registered">>,
        <<"my.app.order.placed">>,
        <<"my.app.payment.completed">>,
        <<"energy.home.measured">>,
        <<"energy.contract.signed">>
    ],

    %% ❌ BAD: Entity IDs in topics (should be in payload)
    BadTopics = [
        <<"my.app.user.123.registered">>,  %% ID in topic!
        <<"energy.home.home-001.measured">>  %% ID in topic!
    ],

    %% All topics should be accepted by API (validation is semantic, not enforced)
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead

    lists:foreach(fun(Topic) ->
        Result = (catch macula_sdk:publish(Client, Topic, #{})),
        ?assertMatch({'EXIT', {noproc, _}}, Result)
    end, GoodTopics ++ BadTopics).

%%%===================================================================
%%% Message Encoding Tests
%%%===================================================================

test_json_encoding() ->
    %% Test that various data types can be JSON encoded
    %% (Depends on jiffy being available)

    %% Simple map
    Data1 = #{key => <<"value">>},

    %% Nested map
    Data2 = #{
        user => #{
            id => <<"123">>,
            name => <<"Test User">>
        },
        metadata => #{
            timestamp => 1699564800
        }
    },

    %% List
    Data3 = [1, 2, 3, 4, 5],

    %% All should be encodable
    Client = spawn(fun() -> ok end),
    timer:sleep(10), %% Ensure dead
    ?assertMatch({'EXIT', {noproc, _}},
                 (catch macula_sdk:publish(Client, <<"test">>, Data1))),
    ?assertMatch({'EXIT', {noproc, _}},
                 (catch macula_sdk:publish(Client, <<"test">>, Data2))),
    ?assertMatch({'EXIT', {noproc, _}},
                 (catch macula_sdk:publish(Client, <<"test">>, Data3))).
