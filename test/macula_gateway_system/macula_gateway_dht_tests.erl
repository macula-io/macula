%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_gateway_dht module.
%%% Tests DHT query handling (delegation to routing server).
%%%
%%% TDD Approach:
%%% 1. Write failing tests first
%%% 2. Implement minimal functionality
%%% 3. Make tests pass incrementally
%%% 4. Refactor for idiomatic Erlang
%%%
%%% Responsibilities:
%%% - Forward DHT STORE messages to routing server
%%% - Forward DHT FIND_VALUE messages to routing server, send replies
%%% - Forward DHT FIND_NODE messages to routing server, send replies
%%% - Handle DHT queries from process messages
%%% - Encode replies using protocol encoder
%%% - Handle errors gracefully
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_dht_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% No setup needed - functional module (stateless)

%%%===================================================================
%%% Basic API Tests
%%%===================================================================

module_exports_test() ->
    Exports = macula_gateway_dht:module_info(exports),

    ?assert(lists:member({handle_store, 2}, Exports)),
    ?assert(lists:member({handle_find_value, 2}, Exports)),
    ?assert(lists:member({handle_find_node, 2}, Exports)),
    ?assert(lists:member({handle_query, 3}, Exports)).

%%%===================================================================
%%% STORE Message Tests
%%%===================================================================

handle_store_basic_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create mock stream
        Stream = spawn(fun() -> receive _ -> ok end end),

        %% Create STORE message
        StoreMsg = #{
            type => store,
            key => <<"test_key">>,
            value => <<"test_value">>,
            ttl => 3600
        },

        %% Call handler (will fail until implemented)
        Result = macula_gateway_dht:handle_store(Stream, StoreMsg),

        %% Should return ok (STORE doesn't send reply)
        [?_assertMatch(ok, Result)]
     end}.

handle_store_with_routing_server_down_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create mock stream
        Stream = spawn(fun() -> receive _ -> ok end end),

        %% Create STORE message
        StoreMsg = #{
            type => store,
            key => <<"test_key">>,
            value => <<"test_value">>
        },

        %% Call handler when routing server is down
        %% Handler is async (fire-and-forget) - returns ok immediately
        Result = macula_gateway_dht:handle_store(Stream, StoreMsg),

        %% Handler returns ok regardless of routing server state
        %% (async message to routing server may fail silently)
        [?_assertEqual(ok, Result)]
     end}.

%%%===================================================================
%%% FIND_VALUE Message Tests
%%%===================================================================

handle_find_value_basic_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create mock stream that receives messages
        Parent = self(),
        Stream = spawn(fun() ->
            receive
                Message -> Parent ! {stream_received, Message}
            end
        end),

        %% Create FIND_VALUE message
        FindValueMsg = #{
            type => find_value,
            key => <<"test_key">>,
            sender => crypto:strong_rand_bytes(32)
        },

        %% Call handler
        Result = macula_gateway_dht:handle_find_value(Stream, FindValueMsg),

        %% Should return ok (reply sent over stream)
        [?_assertMatch(ok, Result)]
     end}.

handle_find_value_with_error_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create invalid stream (dead process)
        Stream = spawn(fun() -> ok end),
        timer:sleep(50),  % Let process die

        %% Create FIND_VALUE message
        FindValueMsg = #{
            type => find_value,
            key => <<"test_key">>
        },

        %% Call handler
        Result = macula_gateway_dht:handle_find_value(Stream, FindValueMsg),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

%%%===================================================================
%%% FIND_NODE Message Tests
%%%===================================================================

handle_find_node_basic_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create mock stream that receives messages
        Parent = self(),
        Stream = spawn(fun() ->
            receive
                Message -> Parent ! {stream_received, Message}
            end
        end),

        %% Create FIND_NODE message
        FindNodeMsg = #{
            type => find_node,
            target => crypto:strong_rand_bytes(32),
            sender => crypto:strong_rand_bytes(32)
        },

        %% Call handler
        Result = macula_gateway_dht:handle_find_node(Stream, FindNodeMsg),

        %% Should return ok (reply sent over stream)
        [?_assertMatch(ok, Result)]
     end}.

handle_find_node_with_error_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create invalid stream (dead process)
        Stream = spawn(fun() -> ok end),
        timer:sleep(50),  % Let process die

        %% Create FIND_NODE message
        FindNodeMsg = #{
            type => find_node,
            target => crypto:strong_rand_bytes(32)
        },

        %% Call handler
        Result = macula_gateway_dht:handle_find_node(Stream, FindNodeMsg),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

%%%===================================================================
%%% Process Query Tests
%%%===================================================================

handle_query_find_node_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create receiver process
        Parent = self(),
        ReceiverPid = spawn(fun() ->
            receive
                {dht_reply, _Reply} -> Parent ! {received_reply, ok}
            after 1000 -> Parent ! {received_reply, timeout}
            end
        end),

        %% Create encoded FIND_NODE query
        FindNodeMsg = #{
            type => find_node,
            target => crypto:strong_rand_bytes(32),
            sender => crypto:strong_rand_bytes(32)
        },
        QueryData = macula_protocol_encoder:encode(find_node, FindNodeMsg),

        %% Call handler
        Result = macula_gateway_dht:handle_query(ReceiverPid, find_node, QueryData),

        %% Should return ok
        [?_assertMatch(ok, Result)]
     end}.

handle_query_find_value_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create receiver process
        Parent = self(),
        ReceiverPid = spawn(fun() ->
            receive
                {dht_reply, _Reply} -> Parent ! {received_reply, ok}
            after 1000 -> Parent ! {received_reply, timeout}
            end
        end),

        %% Create encoded FIND_VALUE query
        FindValueMsg = #{
            type => find_value,
            key => <<"test_key">>,
            sender => crypto:strong_rand_bytes(32)
        },
        QueryData = macula_protocol_encoder:encode(find_value, FindValueMsg),

        %% Call handler
        Result = macula_gateway_dht:handle_query(ReceiverPid, find_value, QueryData),

        %% Should return ok
        [?_assertMatch(ok, Result)]
     end}.

handle_query_store_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create receiver process
        Parent = self(),
        ReceiverPid = spawn(fun() ->
            receive
                {dht_reply, _Reply} -> Parent ! {received_reply, ok}
            after 1000 -> Parent ! {received_reply, timeout}
            end
        end),

        %% Create encoded STORE query
        StoreMsg = #{
            type => store,
            key => <<"test_key">>,
            value => <<"test_value">>,
            ttl => 3600
        },
        QueryData = macula_protocol_encoder:encode(store, StoreMsg),

        %% Call handler
        Result = macula_gateway_dht:handle_query(ReceiverPid, store, QueryData),

        %% Should return ok
        [?_assertMatch(ok, Result)]
     end}.

handle_query_invalid_data_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create receiver process
        Parent = self(),
        ReceiverPid = spawn(fun() ->
            receive
                {dht_reply, _Reply} -> Parent ! {received_reply, ok}
            after 1000 -> Parent ! {received_reply, timeout}
            end
        end),

        %% Create invalid query data
        InvalidData = <<"invalid binary data">>,

        %% Call handler
        Result = macula_gateway_dht:handle_query(ReceiverPid, find_node, InvalidData),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

handle_query_unknown_type_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create receiver process
        Parent = self(),
        ReceiverPid = spawn(fun() ->
            receive
                {dht_reply, _Reply} -> Parent ! {received_reply, ok}
            after 1000 -> Parent ! {received_reply, timeout}
            end
        end),

        %% Create query with unknown type
        UnknownMsg = #{type => unknown, data => <<"test">>},
        QueryData = macula_protocol_encoder:encode(reply, UnknownMsg),

        %% Call handler
        Result = macula_gateway_dht:handle_query(ReceiverPid, unknown, QueryData),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

handle_store_with_malformed_message_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create mock stream
        Stream = spawn(fun() -> receive _ -> ok end end),

        %% Create malformed message (missing required fields)
        MalformedMsg = #{invalid => field},

        %% Call handler
        Result = macula_gateway_dht:handle_store(Stream, MalformedMsg),

        %% Handler is lenient - returns ok for any input (fire-and-forget)
        [?_assertEqual(ok, Result)]
     end}.

handle_find_value_with_malformed_message_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create mock stream
        Stream = spawn(fun() -> receive _ -> ok end end),

        %% Create malformed message
        MalformedMsg = #{invalid => field},

        %% Call handler - will fail trying to send response, but returns ok
        %% The handler is lenient and continues processing
        Result = macula_gateway_dht:handle_find_value(Stream, MalformedMsg),

        %% Handler returns ok (lenient error handling - logs warning internally)
        [?_assertEqual(ok, Result)]
     end}.

handle_find_node_with_malformed_message_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Create mock stream
        Stream = spawn(fun() -> receive _ -> ok end end),

        %% Create malformed message
        MalformedMsg = #{invalid => field},

        %% Call handler
        Result = macula_gateway_dht:handle_find_node(Stream, MalformedMsg),

        %% Handler returns ok (lenient error handling)
        [?_assertEqual(ok, Result)]
     end}.

%%%===================================================================
%%% Integration Tests (require routing server)
%%%===================================================================

%% These tests require macula_routing_server to be running
%% They are skipped by default and should be run in integration environment

%% integration_store_test_() ->
%%     {setup,
%%      fun() ->
%%          %% Start routing server
%%          macula_routing_server:start_link(#{node_id => crypto:strong_rand_bytes(32)})
%%      end,
%%      fun({ok, Pid}) ->
%%          macula_routing_server:stop(Pid)
%%      end,
%%      fun(_) ->
%%          Stream = spawn(fun() -> receive _ -> ok end end),
%%          StoreMsg = #{type => store, key => <<"k">>, value => <<"v">>, ttl => 3600},
%%          Result = macula_gateway_dht:handle_store(Stream, StoreMsg),
%%          [?_assertEqual(ok, Result)]
%%      end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================
%% (None currently needed)
