%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for NATS-style async RPC operations in macula_rpc_handler.
%%% Tests asynchronous request/reply pattern with callbacks.
%%% Following TDD principles.
%%%
%%% Note: These tests focus on unit testing local handler execution
%%% and protocol message types. Integration tests for DHT discovery
%%% and P2P delivery are done via Docker tests.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_async_rpc_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

async_rpc_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         %% Local handler tests (no network required)
         {"request/4 with local handler returns ID", fun test_request_local_returns_id/0},
         {"request/4 with callback option", fun test_request_with_callback/0},
         {"local handler receives args", fun test_local_handler_receives_args/0},
         {"local handler error propagates", fun test_local_handler_error/0},
         {"register_local_procedure/3 works", fun test_register_local_procedure/0},
         {"get_service_registry returns registry", fun test_get_service_registry/0},
         {"multiple local handlers", fun test_multiple_local_handlers/0},

         %% Protocol type tests
         {"protocol message types defined", fun test_protocol_types/0},
         {"message validation for rpc_request", fun test_rpc_request_validation/0},
         {"message validation for rpc_reply", fun test_rpc_reply_validation/0},
         {"rpc_request with binary keys validates", fun test_rpc_request_binary_keys/0},
         {"rpc_reply with binary keys validates", fun test_rpc_reply_binary_keys/0},

         %% Async reply handling (mocked)
         {"handle_async_reply to unknown request", fun test_handle_reply_unknown/0},
         {"unique request IDs generation", fun test_unique_request_ids/0},

         %% Async DHT discovery tests
         {"async request returns ID on cache miss", fun test_async_dht_discovery_returns_id/0},
         {"multiple requests queue during discovery", fun test_async_dht_discovery_queues_requests/0},
         {"discovery timeout notifies all callbacks", fun test_async_dht_discovery_timeout/0},

         %% Pull-based service discovery tests
         {"service_interests option in init", fun test_service_interests_init/0},
         {"normalize_service_interests handles binary list", fun test_normalize_interests_binary_list/0},
         {"normalize_service_interests handles atom list", fun test_normalize_interests_atom_list/0},
         {"get_service_interests returns configured interests", fun test_get_service_interests/0},
         {"prefetch_services adds to interests", fun test_prefetch_services_adds_interests/0}
     ]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(gproc),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Local Handler Tests
%%%===================================================================

test_request_local_returns_id() ->
    %% GIVEN: An RPC handler with local handler registered
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% Register a local handler
    TestHandler = fun(_Args) -> {ok, #{<<"result">> => <<"test">>}} end,
    macula_rpc_handler:register_local_procedure(RpcPid, <<"local.test">>, TestHandler),
    timer:sleep(10),

    %% WHEN: Making async RPC request to local handler
    Callback = fun(_Result) -> ok end,
    Result = macula_rpc_handler:request(RpcPid, <<"local.test">>, #{}, #{callback => Callback}),

    %% THEN: Should return {ok, RequestId} immediately
    ?assertMatch({ok, _RequestId}, Result),

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

test_request_with_callback() ->
    %% GIVEN: An RPC handler with local handler
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% Register handler that returns expected result
    ExpectedResult = #{<<"status">> => <<"success">>, <<"value">> => 42},
    TestHandler = fun(_Args) -> {ok, ExpectedResult} end,
    macula_rpc_handler:register_local_procedure(RpcPid, <<"callback.test">>, TestHandler),
    timer:sleep(10),

    %% WHEN: Making async RPC request with callback
    Self = self(),
    Callback = fun(Result) -> Self ! {callback_result, Result} end,
    {ok, _RequestId} = macula_rpc_handler:request(
        RpcPid, <<"callback.test">>, #{},
        #{callback => Callback}
    ),

    %% THEN: Callback should be invoked with result
    receive
        {callback_result, {ok, Res}} ->
            ?assertEqual(ExpectedResult, Res)
    after 1000 ->
        ?assert(false, "Callback not invoked within timeout")
    end,

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

test_local_handler_receives_args() ->
    %% GIVEN: An RPC handler with echo handler
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% Register echo handler
    Self = self(),
    EchoHandler = fun(Args) ->
        Self ! {handler_received, Args},
        {ok, #{<<"echoed">> => Args}}
    end,
    macula_rpc_handler:register_local_procedure(RpcPid, <<"echo.handler">>, EchoHandler),
    timer:sleep(10),

    %% WHEN: Making request with specific args
    InputArgs = #{<<"input">> => <<"test_value">>, <<"number">> => 123},
    Callback = fun(_) -> ok end,
    {ok, _} = macula_rpc_handler:request(RpcPid, <<"echo.handler">>, InputArgs, #{callback => Callback}),

    %% THEN: Handler should receive the args
    receive
        {handler_received, ReceivedArgs} ->
            ?assertEqual(InputArgs, ReceivedArgs)
    after 1000 ->
        ?assert(false, "Handler did not receive args")
    end,

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

test_local_handler_error() ->
    %% GIVEN: An RPC handler with error-returning handler
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% Register handler that returns error
    ErrorHandler = fun(_Args) -> {error, #{<<"code">> => <<"FAILED">>, <<"message">> => <<"Test error">>}} end,
    macula_rpc_handler:register_local_procedure(RpcPid, <<"error.handler">>, ErrorHandler),
    timer:sleep(10),

    %% WHEN: Making request
    Self = self(),
    Callback = fun(Result) -> Self ! {error_result, Result} end,
    {ok, _} = macula_rpc_handler:request(RpcPid, <<"error.handler">>, #{}, #{callback => Callback}),

    %% THEN: Callback should receive error
    receive
        {error_result, {error, ErrDetails}} ->
            ?assertMatch(#{<<"code">> := <<"FAILED">>}, ErrDetails)
    after 1000 ->
        ?assert(false, "Error callback not invoked")
    end,

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

test_register_local_procedure() ->
    %% GIVEN: An RPC handler
    {ok, RpcPid} = start_test_rpc_handler(),

    %% WHEN: Registering local procedure
    Handler = fun(_Args) -> {ok, <<"result">>} end,
    ok = macula_rpc_handler:register_local_procedure(RpcPid, <<"my.local.proc">>, Handler),
    timer:sleep(10),

    %% THEN: Service registry should contain the handler
    Registry = macula_rpc_handler:get_service_registry(RpcPid),
    LocalResult = macula_service_registry:get_local_handler(Registry, <<"my.local.proc">>),
    ?assertMatch({ok, _}, LocalResult),

    %% Cleanup
    stop_test_rpc_handler(RpcPid).

test_get_service_registry() ->
    %% GIVEN: An RPC handler
    {ok, RpcPid} = start_test_rpc_handler(),

    %% WHEN: Getting service registry
    Registry = macula_rpc_handler:get_service_registry(RpcPid),

    %% THEN: Should return a valid registry
    ?assert(is_map(Registry) orelse is_tuple(Registry)),

    %% Cleanup
    stop_test_rpc_handler(RpcPid).

test_multiple_local_handlers() ->
    %% GIVEN: An RPC handler
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% Register multiple handlers
    Handler1 = fun(_) -> {ok, #{<<"handler">> => 1}} end,
    Handler2 = fun(_) -> {ok, #{<<"handler">> => 2}} end,
    Handler3 = fun(_) -> {ok, #{<<"handler">> => 3}} end,

    macula_rpc_handler:register_local_procedure(RpcPid, <<"proc.1">>, Handler1),
    macula_rpc_handler:register_local_procedure(RpcPid, <<"proc.2">>, Handler2),
    macula_rpc_handler:register_local_procedure(RpcPid, <<"proc.3">>, Handler3),
    timer:sleep(10),

    %% WHEN: Calling each handler
    Self = self(),
    lists:foreach(fun(N) ->
        Proc = <<"proc.", (integer_to_binary(N))/binary>>,
        Callback = fun(Result) -> Self ! {result, N, Result} end,
        {ok, _} = macula_rpc_handler:request(RpcPid, Proc, #{}, #{callback => Callback})
    end, [1, 2, 3]),

    %% THEN: Each should return correct handler ID
    lists:foreach(fun(N) ->
        receive
            {result, N, {ok, #{<<"handler">> := N}}} -> ok
        after 500 ->
            ?assert(false, io_lib:format("Handler ~p did not respond", [N]))
        end
    end, [1, 2, 3]),

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

%%%===================================================================
%%% Protocol Type Tests
%%%===================================================================

test_protocol_types() ->
    %% GIVEN: Protocol type definitions
    %% WHEN: Getting type IDs
    RequestTypeId = macula_protocol_types:message_type_id(rpc_request),
    ReplyTypeId = macula_protocol_types:message_type_id(rpc_reply),

    %% THEN: Type IDs should be defined correctly
    ?assertEqual(16#24, RequestTypeId),
    ?assertEqual(16#25, ReplyTypeId),

    %% And reverse lookup should work
    ?assertEqual({ok, rpc_request}, macula_protocol_types:message_type_name(16#24)),
    ?assertEqual({ok, rpc_reply}, macula_protocol_types:message_type_name(16#25)).

test_rpc_request_validation() ->
    %% GIVEN: Valid rpc_request message with atom keys
    ValidMsg = #{
        request_id => <<"req-123">>,
        procedure => <<"test.proc">>,
        args => #{},
        from_node => <<"node-456">>
    },

    %% WHEN: Encoding the message
    Result = (catch macula_protocol_encoder:encode(rpc_request, ValidMsg)),

    %% THEN: Should succeed (return binary)
    ?assert(is_binary(Result)).

test_rpc_reply_validation() ->
    %% GIVEN: Valid rpc_reply message with atom keys
    ValidMsg = #{
        request_id => <<"req-123">>,
        from_node => <<"node-456">>,
        result => #{<<"value">> => 42}
    },

    %% WHEN: Encoding the message
    Result = (catch macula_protocol_encoder:encode(rpc_reply, ValidMsg)),

    %% THEN: Should succeed (return binary)
    ?assert(is_binary(Result)).

test_rpc_request_binary_keys() ->
    %% GIVEN: Valid rpc_request message with binary keys (from MessagePack decode)
    ValidMsg = #{
        <<"request_id">> => <<"req-123">>,
        <<"procedure">> => <<"test.proc">>,
        <<"args">> => #{},
        <<"from_node">> => <<"node-456">>
    },

    %% WHEN: Encoding the message
    Result = (catch macula_protocol_encoder:encode(rpc_request, ValidMsg)),

    %% THEN: Should succeed (return binary)
    ?assert(is_binary(Result)).

test_rpc_reply_binary_keys() ->
    %% GIVEN: Valid rpc_reply message with binary keys (from MessagePack decode)
    ValidMsg = #{
        <<"request_id">> => <<"req-123">>,
        <<"from_node">> => <<"node-456">>,
        <<"result">> => #{<<"value">> => 42}
    },

    %% WHEN: Encoding the message
    Result = (catch macula_protocol_encoder:encode(rpc_reply, ValidMsg)),

    %% THEN: Should succeed (return binary)
    ?assert(is_binary(Result)).

%%%===================================================================
%%% Async Reply Handling Tests
%%%===================================================================

test_handle_reply_unknown() ->
    %% GIVEN: An RPC handler with no pending requests
    {ok, RpcPid} = start_test_rpc_handler(),

    %% WHEN: Receiving a reply for unknown request
    ReplyMsg = #{
        <<"request_id">> => <<"nonexistent-req-id">>,
        <<"result">> => #{<<"value">> => 42},
        <<"from_node">> => <<"provider-node">>
    },

    %% THEN: Should not crash (handle gracefully)
    ok = macula_rpc_handler:handle_async_reply(RpcPid, ReplyMsg),
    timer:sleep(50),

    %% Process should still be alive
    ?assert(is_process_alive(RpcPid)),

    %% Cleanup
    stop_test_rpc_handler(RpcPid).

test_unique_request_ids() ->
    %% GIVEN: An RPC handler with local handlers
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% Register local handler
    TestHandler = fun(_) -> {ok, #{<<"ok">> => true}} end,
    macula_rpc_handler:register_local_procedure(RpcPid, <<"unique.test">>, TestHandler),
    timer:sleep(10),

    %% WHEN: Making many async RPC requests
    Callback = fun(_) -> ok end,
    Ids = [begin
        {ok, Id} = macula_rpc_handler:request(RpcPid, <<"unique.test">>, #{}, #{callback => Callback}),
        Id
    end || _ <- lists:seq(1, 50)],

    %% THEN: All IDs should be unique
    UniqueIds = lists:usort(Ids),
    ?assertEqual(length(Ids), length(UniqueIds)),

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

%%%===================================================================
%%% Async DHT Discovery Tests
%%%===================================================================

test_async_dht_discovery_returns_id() ->
    %% GIVEN: An RPC handler with mock connection manager that accepts messages
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_with_find_value_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% WHEN: Making async RPC request to unknown procedure (cache miss)
    Self = self(),
    Callback = fun(Result) -> Self ! {callback_result, Result} end,
    Result = macula_rpc_handler:request(
        RpcPid, <<"unknown.procedure">>, #{},
        #{callback => Callback, dht_timeout => 500}
    ),

    %% THEN: Should return {ok, RequestId} immediately (discovery starts)
    ?assertMatch({ok, _RequestId}, Result),

    %% Wait for timeout (we don't simulate DHT response)
    receive
        {callback_result, {error, dht_discovery_timeout}} -> ok
    after 1000 ->
        ?assert(false, "Expected dht_discovery_timeout callback")
    end,

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

test_async_dht_discovery_queues_requests() ->
    %% GIVEN: An RPC handler with mock connection manager
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_with_find_value_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% WHEN: Making multiple async RPC requests to same procedure (cache miss)
    Self = self(),
    Results = [begin
        Callback = fun(Res) -> Self ! {callback_result, N, Res} end,
        macula_rpc_handler:request(
            RpcPid, <<"same.procedure">>, #{n => N},
            #{callback => Callback, dht_timeout => 500}
        )
    end || N <- [1, 2, 3]],

    %% THEN: All should return {ok, RequestId} immediately
    lists:foreach(fun(Result) ->
        ?assertMatch({ok, _RequestId}, Result)
    end, Results),

    %% All should eventually get timeout (no DHT response simulated)
    lists:foreach(fun(N) ->
        receive
            {callback_result, N, {error, dht_discovery_timeout}} -> ok
        after 1000 ->
            ?assert(false, io_lib:format("Expected timeout for request ~p", [N]))
        end
    end, [1, 2, 3]),

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

test_async_dht_discovery_timeout() ->
    %% GIVEN: An RPC handler with mock connection manager
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_with_find_value_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% WHEN: Making async RPC request with short timeout
    Self = self(),
    Callback = fun(Result) -> Self ! {callback_result, Result} end,
    {ok, _RequestId} = macula_rpc_handler:request(
        RpcPid, <<"timeout.test">>, #{},
        #{callback => Callback, dht_timeout => 100}
    ),

    %% THEN: Should receive timeout error via callback
    receive
        {callback_result, {error, dht_discovery_timeout}} -> ok
    after 500 ->
        ?assert(false, "Expected dht_discovery_timeout callback")
    end,

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

%%%===================================================================
%%% Pull-based Service Discovery Tests
%%%===================================================================

test_service_interests_init() ->
    %% GIVEN: Service interests configured in opts
    PeerId = erlang:unique_integer([monotonic, positive]),
    Opts = #{
        node_id => <<"test-node-", (integer_to_binary(PeerId))/binary>>,
        realm => <<"test-realm">>,
        peer_id => PeerId,
        service_interests => [<<"ping.handler">>, <<"user.service">>]
    },

    %% WHEN: Starting RPC handler with service_interests
    {ok, RpcPid} = macula_rpc_handler:start_link(Opts),
    timer:sleep(10),

    %% THEN: Service interests should be stored
    Interests = macula_rpc_handler:get_service_interests(RpcPid),
    ?assertEqual([<<"ping.handler">>, <<"user.service">>], lists:sort(Interests)),

    %% Cleanup
    stop_test_rpc_handler(RpcPid).

test_normalize_interests_binary_list() ->
    %% GIVEN: Service interests as binary list in opts
    PeerId = erlang:unique_integer([monotonic, positive]),
    Opts = #{
        node_id => <<"test-node-", (integer_to_binary(PeerId))/binary>>,
        realm => <<"test-realm">>,
        peer_id => PeerId,
        service_interests => [<<"service.a">>, <<"service.b">>, <<"service.c">>]
    },

    %% WHEN: Starting RPC handler
    {ok, RpcPid} = macula_rpc_handler:start_link(Opts),
    timer:sleep(10),

    %% THEN: All binaries should be normalized
    Interests = macula_rpc_handler:get_service_interests(RpcPid),
    ?assertEqual(3, length(Interests)),
    ?assert(lists:all(fun is_binary/1, Interests)),

    %% Cleanup
    stop_test_rpc_handler(RpcPid).

test_normalize_interests_atom_list() ->
    %% GIVEN: Service interests as atom list in opts
    PeerId = erlang:unique_integer([monotonic, positive]),
    Opts = #{
        node_id => <<"test-node-", (integer_to_binary(PeerId))/binary>>,
        realm => <<"test-realm">>,
        peer_id => PeerId,
        service_interests => [ping_handler, user_service]
    },

    %% WHEN: Starting RPC handler
    {ok, RpcPid} = macula_rpc_handler:start_link(Opts),
    timer:sleep(10),

    %% THEN: Atoms should be converted to binaries
    Interests = macula_rpc_handler:get_service_interests(RpcPid),
    ?assertEqual(2, length(Interests)),
    ?assert(lists:member(<<"ping_handler">>, Interests)),
    ?assert(lists:member(<<"user_service">>, Interests)),

    %% Cleanup
    stop_test_rpc_handler(RpcPid).

test_get_service_interests() ->
    %% GIVEN: RPC handler with no service interests
    {ok, RpcPid} = start_test_rpc_handler(),

    %% WHEN: Getting service interests
    Interests = macula_rpc_handler:get_service_interests(RpcPid),

    %% THEN: Should return empty list
    ?assertEqual([], Interests),

    %% Cleanup
    stop_test_rpc_handler(RpcPid).

test_prefetch_services_adds_interests() ->
    %% GIVEN: RPC handler with mock connection manager
    {ok, RpcPid} = start_test_rpc_handler(),

    MockConnMgr = spawn(fun() -> mock_conn_mgr_with_find_value_loop() end),
    gen_server:cast(RpcPid, {set_connection_manager_pid, MockConnMgr}),
    timer:sleep(10),

    %% Initial interests should be empty
    ?assertEqual([], macula_rpc_handler:get_service_interests(RpcPid)),

    %% WHEN: Prefetching services dynamically
    macula_rpc_handler:prefetch_services(RpcPid, [<<"new.service">>, <<"another.service">>]),
    timer:sleep(50),

    %% THEN: Service interests should be updated
    Interests = macula_rpc_handler:get_service_interests(RpcPid),
    ?assertEqual(2, length(Interests)),
    ?assert(lists:member(<<"new.service">>, Interests)),
    ?assert(lists:member(<<"another.service">>, Interests)),

    %% Cleanup
    stop_test_rpc_handler(RpcPid),
    exit(MockConnMgr, kill).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

start_test_rpc_handler() ->
    %% Generate unique peer ID to avoid gproc conflicts
    PeerId = erlang:unique_integer([monotonic, positive]),
    Opts = #{
        node_id => <<"test-node-", (integer_to_binary(PeerId))/binary>>,
        realm => <<"test-realm">>,
        peer_id => PeerId
    },
    macula_rpc_handler:start_link(Opts).

stop_test_rpc_handler(Pid) ->
    catch gen_server:stop(Pid, normal, 100).

mock_conn_mgr_loop() ->
    receive
        {send_message, _Type, _Msg} ->
            mock_conn_mgr_loop();
        stop ->
            ok;
        _ ->
            mock_conn_mgr_loop()
    after 10000 ->
        ok
    end.

%% Mock connection manager that accepts find_value messages for DHT discovery tests
mock_conn_mgr_with_find_value_loop() ->
    receive
        {'$gen_call', From, {send_message, find_value, _Msg}} ->
            gen_server:reply(From, ok),
            mock_conn_mgr_with_find_value_loop();
        {'$gen_call', From, {send_message, _Type, _Msg}} ->
            gen_server:reply(From, ok),
            mock_conn_mgr_with_find_value_loop();
        stop ->
            ok;
        _ ->
            mock_conn_mgr_with_find_value_loop()
    after 10000 ->
        ok
    end.
