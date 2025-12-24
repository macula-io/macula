%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_handler module.
%%%
%%% Tests pub/sub operations without requiring actual QUIC connections.
%%% Focuses on:
%%% - Subscription management (subscribe/unsubscribe)
%%% - State management (subscriptions, pending acknowledgments)
%%% - Message routing (incoming publishes)
%%% - Connection manager PID handling
%%% - Error handling
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

pubsub_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Lifecycle Tests
      {"handler starts successfully", fun handler_starts_successfully_test/0},
      {"handler accepts custom topic config", fun handler_accepts_custom_topic_config_test/0},
      {"handler can be stopped", fun handler_can_be_stopped_test/0},
      %% Connection Manager PID Tests
      {"handler starts without connection manager pid", fun handler_starts_without_connection_manager_pid_test/0},
      {"handler accepts connection manager pid", fun handler_accepts_connection_manager_pid_test/0},
      %% Subscribe Tests
      {"subscribe returns reference", fun subscribe_returns_reference_test/0},
      {"subscribe with binary topic", fun subscribe_with_binary_topic_test/0},
      {"subscribe with string topic", fun subscribe_with_string_topic_test/0},
      {"subscribe with atom topic", fun subscribe_with_atom_topic_test/0},
      {"multiple subscriptions", fun multiple_subscriptions_test/0},
      %% Unsubscribe Tests
      {"unsubscribe removes subscription", fun unsubscribe_removes_subscription_test/0},
      {"unsubscribe invalid ref returns error", fun unsubscribe_invalid_ref_returns_error_test/0},
      {"unsubscribe twice returns error", fun unsubscribe_twice_returns_error_test/0},
      %% Publish Tests
      {"publish without connection manager", fun publish_without_connection_manager_test/0},
      {"publish with binary data", fun publish_with_binary_data_test/0},
      {"publish with map data", fun publish_with_map_data_test/0},
      {"publish with qos 0", fun publish_with_qos_0_test/0},
      {"publish with qos 1", fun publish_with_qos_1_test/0},
      {"publish with retain flag", fun publish_with_retain_flag_test/0},
      %% Incoming Publish Tests
      {"handle incoming publish with binary topic", fun handle_incoming_publish_with_binary_topic_test/0},
      {"handle incoming publish no matching subscription", fun handle_incoming_publish_no_matching_subscription_test/0},
      %% State Management Tests
      {"handler tracks multiple subscriptions", fun handler_tracks_multiple_subscriptions_test/0},
      {"handler survives invalid publish data", fun handler_survives_invalid_publish_data_test/0},
      {"handler handles callback exception", fun handler_handles_callback_exception_test/0}
     ]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(gproc),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Lifecycle Tests
%%%===================================================================

handler_starts_successfully_test() ->
    Opts = #{
        node_id => <<"test_node_123">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4000">>
    },

    Result = macula_pubsub_handler:start_link(Opts),

    ?assertMatch({ok, _Pid}, Result),

    case Result of
        {ok, Pid} ->
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid);
        _ ->
            ok
    end.

handler_accepts_custom_topic_config_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4001">>,
        topic_separator => <<"/">>,
        topic_wildcard_single => <<"+">>,
        topic_wildcard_multi => <<"#">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handler_can_be_stopped_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4002">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    timer:sleep(50),

    ?assertNot(is_process_alive(Pid)).

%%%===================================================================
%%% Connection Manager PID Tests
%%%===================================================================

handler_starts_without_connection_manager_pid_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4003">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    %% Should start even without connection_manager_pid (set to undefined initially)
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handler_accepts_connection_manager_pid_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4004">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    %% Mock connection manager PID
    MockConnMgrPid = spawn(fun() -> timer:sleep(1000) end),

    %% Send set_connection_manager_pid message
    gen_server:cast(Pid, {set_connection_manager_pid, MockConnMgrPid}),

    %% Handler should still be alive
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid),
    exit(MockConnMgrPid, kill).

%%%===================================================================
%%% Subscribe Tests
%%%===================================================================

subscribe_returns_reference_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4005">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = <<"test.topic">>,
    Callback = fun(_Msg) -> ok end,

    Result = macula_pubsub_handler:subscribe(Pid, Topic, Callback),

    ?assertMatch({ok, _Ref}, Result),

    case Result of
        {ok, Ref} ->
            ?assert(is_reference(Ref));
        _ ->
            ok
    end,

    gen_server:stop(Pid).

subscribe_with_binary_topic_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4006">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = <<"test.binary.topic">>,
    Callback = fun(_) -> ok end,

    {ok, _Ref} = macula_pubsub_handler:subscribe(Pid, Topic, Callback),

    gen_server:stop(Pid).

subscribe_with_string_topic_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4007">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = "test.string.topic",
    Callback = fun(_) -> ok end,

    {ok, _Ref} = macula_pubsub_handler:subscribe(Pid, Topic, Callback),

    gen_server:stop(Pid).

subscribe_with_atom_topic_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4008">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = test_atom_topic,
    Callback = fun(_) -> ok end,

    {ok, _Ref} = macula_pubsub_handler:subscribe(Pid, Topic, Callback),

    gen_server:stop(Pid).

multiple_subscriptions_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4009">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    {ok, Ref1} = macula_pubsub_handler:subscribe(Pid, <<"topic1">>, fun(_) -> ok end),
    {ok, Ref2} = macula_pubsub_handler:subscribe(Pid, <<"topic2">>, fun(_) -> ok end),
    {ok, Ref3} = macula_pubsub_handler:subscribe(Pid, <<"topic3">>, fun(_) -> ok end),

    %% All references should be unique
    ?assertNotEqual(Ref1, Ref2),
    ?assertNotEqual(Ref2, Ref3),
    ?assertNotEqual(Ref1, Ref3),

    gen_server:stop(Pid).

%%%===================================================================
%%% Unsubscribe Tests
%%%===================================================================

unsubscribe_removes_subscription_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4010">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    {ok, SubRef} = macula_pubsub_handler:subscribe(Pid, <<"test.topic">>, fun(_) -> ok end),

    %% Unsubscribe should succeed
    Result = macula_pubsub_handler:unsubscribe(Pid, SubRef),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

unsubscribe_invalid_ref_returns_error_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4011">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    %% Try to unsubscribe with invalid reference
    InvalidRef = make_ref(),
    Result = macula_pubsub_handler:unsubscribe(Pid, InvalidRef),

    %% Should return error
    ?assertMatch({error, _}, Result),

    gen_server:stop(Pid).

unsubscribe_twice_returns_error_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4012">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    {ok, SubRef} = macula_pubsub_handler:subscribe(Pid, <<"test.topic">>, fun(_) -> ok end),

    %% First unsubscribe should succeed
    ok = macula_pubsub_handler:unsubscribe(Pid, SubRef),

    %% Second unsubscribe should fail
    Result = macula_pubsub_handler:unsubscribe(Pid, SubRef),
    ?assertMatch({error, _}, Result),

    gen_server:stop(Pid).

%%%===================================================================
%%% Publish Tests (without connection manager)
%%%===================================================================

publish_without_connection_manager_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4013">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = <<"test.topic">>,
    Data = #{message => <<"hello">>},

    %% Should return ok even without connection manager (queues internally)
    Result = macula_pubsub_handler:publish(Pid, Topic, Data, #{}),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

publish_with_binary_data_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4014">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = <<"test.topic">>,
    Data = <<"binary data">>,

    Result = macula_pubsub_handler:publish(Pid, Topic, Data, #{}),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

publish_with_map_data_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4015">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = <<"test.topic">>,
    Data = #{key1 => <<"value1">>, key2 => 123},

    Result = macula_pubsub_handler:publish(Pid, Topic, Data, #{}),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

publish_with_qos_0_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4016">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = <<"test.topic">>,
    Data = #{message => <<"qos 0">>},
    PublishOpts = #{qos => 0},

    Result = macula_pubsub_handler:publish(Pid, Topic, Data, PublishOpts),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

publish_with_qos_1_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4017">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = <<"test.topic">>,
    Data = #{message => <<"qos 1">>},
    PublishOpts = #{qos => 1},

    Result = macula_pubsub_handler:publish(Pid, Topic, Data, PublishOpts),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

publish_with_retain_flag_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4018">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    Topic = <<"test.topic">>,
    Data = #{message => <<"retained">>},
    PublishOpts = #{retain => true},

    Result = macula_pubsub_handler:publish(Pid, Topic, Data, PublishOpts),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

%%%===================================================================
%%% Incoming Publish Tests
%%%===================================================================

handle_incoming_publish_with_binary_topic_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4019">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    %% Subscribe to topic
    ReceivedRef = make_ref(),
    Self = self(),
    Callback = fun(Msg) ->
        Self ! {received, ReceivedRef, Msg}
    end,

    {ok, _SubRef} = macula_pubsub_handler:subscribe(Pid, <<"test.topic">>, Callback),

    %% Simulate incoming publish
    IncomingMsg = #{
        topic => <<"test.topic">>,
        payload => <<"test payload">>,
        qos => 0
    },

    ok = macula_pubsub_handler:handle_incoming_publish(Pid, IncomingMsg),

    %% Wait for callback
    %% Callback receives message with matched_pattern added
    receive
        {received, ReceivedRef, Msg} ->
            ?assertEqual(<<"test.topic">>, maps:get(topic, Msg)),
            ?assertEqual(<<"test payload">>, maps:get(payload, Msg)),
            ?assertEqual(<<"test.topic">>, maps:get(matched_pattern, Msg))
    after 500 ->
        ?assert(false, "Callback not invoked")
    end,

    gen_server:stop(Pid).

handle_incoming_publish_no_matching_subscription_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4020">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    %% Don't subscribe to anything

    %% Simulate incoming publish
    IncomingMsg = #{
        topic => <<"unsubscribed.topic">>,
        payload => <<"test payload">>,
        qos => 0
    },

    %% Should not crash, just ignore
    ok = macula_pubsub_handler:handle_incoming_publish(Pid, IncomingMsg),

    timer:sleep(100),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% State Management Tests
%%%===================================================================

handler_tracks_multiple_subscriptions_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4021">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    %% Create multiple subscriptions
    {ok, _Ref1} = macula_pubsub_handler:subscribe(Pid, <<"topic.1">>, fun(_) -> ok end),
    {ok, _Ref2} = macula_pubsub_handler:subscribe(Pid, <<"topic.2">>, fun(_) -> ok end),
    {ok, _Ref3} = macula_pubsub_handler:subscribe(Pid, <<"topic.3">>, fun(_) -> ok end),

    %% Handler should still be alive
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

handler_survives_invalid_publish_data_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4022">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    %% Try to publish with invalid data types
    Topic = <<"test.topic">>,

    %% These should all work (handler is flexible with data types)
    ok = macula_pubsub_handler:publish(Pid, Topic, <<"binary">>, #{}),
    ok = macula_pubsub_handler:publish(Pid, Topic, #{map => <<"data">>}, #{}),
    ok = macula_pubsub_handler:publish(Pid, Topic, [list, data], #{}),

    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handler_handles_callback_exception_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:4023">>
    },

    {ok, Pid} = macula_pubsub_handler:start_link(Opts),

    %% Subscribe with a callback that crashes
    BadCallback = fun(_Msg) ->
        error(intentional_crash)
    end,

    {ok, _SubRef} = macula_pubsub_handler:subscribe(Pid, <<"test.topic">>, BadCallback),

    %% Send incoming publish
    IncomingMsg = #{
        topic => <<"test.topic">>,
        payload => <<"test">>,
        qos => 0
    },

    ok = macula_pubsub_handler:handle_incoming_publish(Pid, IncomingMsg),

    %% Handler should survive callback crash
    timer:sleep(100),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).
