%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_gateway_pubsub module.
%%% Tests pub/sub message routing - Phase 3 of gateway refactoring.
%%%
%%% TDD Approach:
%%% 1. Write failing tests first
%%% 2. Implement minimal functionality
%%% 3. Make tests pass incrementally
%%% 4. Refactor for idiomatic Erlang
%%%
%%% Responsibilities:
%%% - Subscribe/unsubscribe to topics
%%% - Route published messages to subscribers
%%% - Support wildcard topics (* and **)
%%% - Track bidirectional mapping (topic ↔ stream)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_pubsub_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    {ok, Pid} = macula_gateway_pubsub:start_link(#{}),
    Pid.

cleanup(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> macula_gateway_pubsub:stop(Pid);
        false -> ok
    end.

%%%===================================================================
%%% Basic API Tests
%%%===================================================================

module_exports_test() ->
    Exports = macula_gateway_pubsub:module_info(exports),

    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({stop, 1}, Exports)),
    ?assert(lists:member({subscribe, 3}, Exports)),
    ?assert(lists:member({unsubscribe, 3}, Exports)),
    ?assert(lists:member({publish, 3}, Exports)),
    ?assert(lists:member({get_subscribers, 2}, Exports)),
    ?assert(lists:member({get_stream_topics, 2}, Exports)).

gen_server_callbacks_test() ->
    Exports = macula_gateway_pubsub:module_info(exports),

    ?assert(lists:member({init, 1}, Exports)),
    ?assert(lists:member({handle_call, 3}, Exports)),
    ?assert(lists:member({handle_cast, 2}, Exports)),
    ?assert(lists:member({handle_info, 2}, Exports)),
    ?assert(lists:member({terminate, 2}, Exports)).

%%%===================================================================
%%% Startup/Shutdown Tests
%%%===================================================================

start_link_test() ->
    {ok, Pid} = macula_gateway_pubsub:start_link(#{}),
    ?assert(erlang:is_process_alive(Pid)),
    macula_gateway_pubsub:stop(Pid).

stop_test() ->
    {ok, Pid} = macula_gateway_pubsub:start_link(#{}),
    ok = macula_gateway_pubsub:stop(Pid),
    timer:sleep(50),
    ?assertNot(erlang:is_process_alive(Pid)).

%%%===================================================================
%%% Subscribe Tests
%%%===================================================================

subscribe_adds_to_topic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Stream = spawn(fun() -> timer:sleep(1000) end),
        Topic = <<"test.topic">>,

        ok = macula_gateway_pubsub:subscribe(Pid, Stream, Topic),

        {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(Pid, Topic),
        [?_assert(lists:member(Stream, Subscribers))]
     end}.

subscribe_multiple_topics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Stream = spawn(fun() -> timer:sleep(1000) end),
        Topics = [<<"topic1">>, <<"topic2">>, <<"topic3">>],

        [macula_gateway_pubsub:subscribe(Pid, Stream, T) || T <- Topics],

        {ok, StreamTopics} = macula_gateway_pubsub:get_stream_topics(Pid, Stream),
        [
            ?_assertEqual(3, length(StreamTopics)),
            ?_assert(lists:member(<<"topic1">>, StreamTopics)),
            ?_assert(lists:member(<<"topic2">>, StreamTopics)),
            ?_assert(lists:member(<<"topic3">>, StreamTopics))
        ]
     end}.

subscribe_duplicate_idempotent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Stream = spawn(fun() -> timer:sleep(1000) end),
        Topic = <<"test.topic">>,

        ok = macula_gateway_pubsub:subscribe(Pid, Stream, Topic),
        ok = macula_gateway_pubsub:subscribe(Pid, Stream, Topic),

        {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(Pid, Topic),
        %% Should only appear once
        [?_assertEqual(1, length(Subscribers))]
     end}.

subscribe_multiple_streams_same_topic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Stream1 = spawn(fun() -> timer:sleep(1000) end),
        Stream2 = spawn(fun() -> timer:sleep(1000) end),
        Topic = <<"test.topic">>,

        ok = macula_gateway_pubsub:subscribe(Pid, Stream1, Topic),
        ok = macula_gateway_pubsub:subscribe(Pid, Stream2, Topic),

        {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(Pid, Topic),
        [
            ?_assertEqual(2, length(Subscribers)),
            ?_assert(lists:member(Stream1, Subscribers)),
            ?_assert(lists:member(Stream2, Subscribers))
        ]
     end}.

%%%===================================================================
%%% Unsubscribe Tests
%%%===================================================================

unsubscribe_removes_from_topic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Stream = spawn(fun() -> timer:sleep(1000) end),
        Topic = <<"test.topic">>,

        macula_gateway_pubsub:subscribe(Pid, Stream, Topic),
        ok = macula_gateway_pubsub:unsubscribe(Pid, Stream, Topic),

        {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(Pid, Topic),
        [?_assertEqual([], Subscribers)]
     end}.

unsubscribe_preserves_other_subscriptions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Stream = spawn(fun() -> timer:sleep(1000) end),
        Topic1 = <<"topic1">>,
        Topic2 = <<"topic2">>,

        macula_gateway_pubsub:subscribe(Pid, Stream, Topic1),
        macula_gateway_pubsub:subscribe(Pid, Stream, Topic2),
        ok = macula_gateway_pubsub:unsubscribe(Pid, Stream, Topic1),

        {ok, StreamTopics} = macula_gateway_pubsub:get_stream_topics(Pid, Stream),
        [
            ?_assertEqual(1, length(StreamTopics)),
            ?_assert(lists:member(Topic2, StreamTopics))
        ]
     end}.

unsubscribe_idempotent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Stream = spawn(fun() -> timer:sleep(1000) end),
        Topic = <<"test.topic">>,

        ok = macula_gateway_pubsub:unsubscribe(Pid, Stream, Topic),
        ok = macula_gateway_pubsub:unsubscribe(Pid, Stream, Topic),

        [?_assertEqual(ok, ok)] % Should not crash
     end}.

%%%===================================================================
%%% Publish Tests
%%%===================================================================

publish_routes_to_subscribers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        %% Create subscriber that collects messages
        Parent = self(),
        Stream = spawn(fun() ->
            receive
                {publish, Topic, Payload} ->
                    Parent ! {received, Topic, Payload}
            after 1000 ->
                Parent ! timeout
            end
        end),

        Topic = <<"test.topic">>,
        Payload = #{data => <<"hello">>},

        macula_gateway_pubsub:subscribe(Pid, Stream, Topic),
        ok = macula_gateway_pubsub:publish(Pid, Topic, Payload),

        %% Wait for message
        receive
            {received, RecvTopic, RecvPayload} ->
                [
                    ?_assertEqual(Topic, RecvTopic),
                    ?_assertEqual(Payload, RecvPayload)
                ]
        after 2000 ->
            [?_assert(false)] % Timeout
        end
     end}.

publish_routes_to_multiple_subscribers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Parent = self(),
        MakeSubscriber = fun(Id) ->
            spawn(fun() ->
                receive
                    {publish, Topic, Payload} ->
                        Parent ! {received, Id, Topic, Payload}
                after 1000 ->
                    Parent ! {timeout, Id}
                end
            end)
        end,

        Stream1 = MakeSubscriber(1),
        Stream2 = MakeSubscriber(2),
        Topic = <<"test.topic">>,
        Payload = #{data => <<"hello">>},

        macula_gateway_pubsub:subscribe(Pid, Stream1, Topic),
        macula_gateway_pubsub:subscribe(Pid, Stream2, Topic),
        ok = macula_gateway_pubsub:publish(Pid, Topic, Payload),

        %% Collect messages
        Received = [receive
            {received, Id, T, P} -> {Id, T, P}
        after 2000 ->
            timeout
        end || _ <- [1, 2]],

        [
            ?_assertEqual(2, length([R || R <- Received, R =/= timeout])),
            ?_assert(lists:member({1, Topic, Payload}, Received)),
            ?_assert(lists:member({2, Topic, Payload}, Received))
        ]
     end}.

publish_to_no_subscribers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Topic = <<"empty.topic">>,
        Payload = #{data => <<"hello">>},

        %% Should not crash
        [?_assertEqual(ok, macula_gateway_pubsub:publish(Pid, Topic, Payload))]
     end}.

%%%===================================================================
%%% Wildcard Subscribe Tests
%%%===================================================================

subscribe_wildcard_single_level_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Parent = self(),
        Stream = spawn(fun() ->
            receive
                {publish, Topic, Payload} ->
                    Parent ! {received, Topic, Payload}
            after 1000 ->
                Parent ! timeout
            end
        end),

        %% Subscribe with wildcard
        Pattern = <<"test.*.event">>,
        macula_gateway_pubsub:subscribe(Pid, Stream, Pattern),

        %% Publish matching topic
        Topic = <<"test.foo.event">>,
        Payload = #{data => <<"hello">>},
        ok = macula_gateway_pubsub:publish(Pid, Topic, Payload),

        receive
            {received, RecvTopic, RecvPayload} ->
                [
                    ?_assertEqual(Topic, RecvTopic),
                    ?_assertEqual(Payload, RecvPayload)
                ]
        after 2000 ->
            [?_assert(false)]
        end
     end}.

subscribe_wildcard_multi_level_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Parent = self(),
        Stream = spawn(fun() ->
            receive
                {publish, Topic, Payload} ->
                    Parent ! {received, Topic, Payload}
            after 1000 ->
                Parent ! timeout
            end
        end),

        %% Subscribe with multi-level wildcard
        Pattern = <<"test.**.event">>,
        macula_gateway_pubsub:subscribe(Pid, Stream, Pattern),

        %% Publish matching topic (multiple levels)
        Topic = <<"test.foo.bar.baz.event">>,
        Payload = #{data => <<"hello">>},
        ok = macula_gateway_pubsub:publish(Pid, Topic, Payload),

        receive
            {received, RecvTopic, RecvPayload} ->
                [
                    ?_assertEqual(Topic, RecvTopic),
                    ?_assertEqual(Payload, RecvPayload)
                ]
        after 2000 ->
            [?_assert(false)]
        end
     end}.

%%%===================================================================
%%% Query Tests
%%%===================================================================

get_subscribers_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(Pid, <<"unknown">>),
        [?_assertEqual([], Subscribers)]
     end}.

get_stream_topics_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Stream = spawn(fun() -> ok end),
        {ok, Topics} = macula_gateway_pubsub:get_stream_topics(Pid, Stream),
        [?_assertEqual([], Topics)]
     end}.

%%%===================================================================
%%% Edge Cases
%%%===================================================================

concurrent_subscriptions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        %% Create many streams subscribing concurrently
        Streams = [spawn(fun() -> timer:sleep(1000) end) || _ <- lists:seq(1, 20)],
        Topic = <<"test.topic">>,

        %% Subscribe all concurrently
        [macula_gateway_pubsub:subscribe(Pid, S, Topic) || S <- Streams],

        {ok, Subscribers} = macula_gateway_pubsub:get_subscribers(Pid, Topic),
        [?_assertEqual(20, length(Subscribers))]
     end}.

stream_death_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Stream = spawn(fun() -> ok end),
        Topic = <<"test.topic">>,

        macula_gateway_pubsub:subscribe(Pid, Stream, Topic),
        timer:sleep(100), %% Let stream die

        %% Publish should not crash (dead stream ignored)
        [?_assertEqual(ok, macula_gateway_pubsub:publish(Pid, Topic, #{data => <<"test">>}))]
     end}.
