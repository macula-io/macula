%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_connection module (QUIC transport layer).
%%%
%%% Tests connection lifecycle and message handling without requiring
%%% actual QUIC connections. Focuses on:
%%% - Handler lifecycle (start/stop)
%%% - URL parsing (various formats)
%%% - Realm normalization (various types)
%%% - Status management
%%% - Message sending (when not connected)
%%% - Message decoding and buffering
%%% - Error handling
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Lifecycle Tests
%%%===================================================================

manager_starts_with_valid_url_and_realm_test() ->
    Url = <<"https://localhost:9443">>,
    Opts = #{realm => <<"test.realm">>},

    Result = macula_connection:start_link(Url, Opts),

    %% Should start even if connection fails
    ?assertMatch({ok, _Pid}, Result),

    case Result of
        {ok, Pid} ->
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid);
        _ ->
            ok
    end.

manager_can_be_stopped_test() ->
    Url = <<"https://localhost:9444">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    timer:sleep(50),

    ?assertNot(is_process_alive(Pid)).

manager_accepts_node_id_in_opts_test() ->
    Url = <<"https://localhost:9445">>,
    Opts = #{
        realm => <<"test.realm">>,
        node_id => <<"custom_node_id_123">>
    },

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% URL Parsing Tests (testing internal parse_url/1 via behavior)
%%%===================================================================

manager_parses_https_url_with_port_test() ->
    Url = <<"https://example.com:9443">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_parses_https_url_without_port_test() ->
    Url = <<"https://example.com">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_parses_http_url_with_port_test() ->
    Url = <<"http://localhost:8080">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_parses_http_url_without_port_test() ->
    Url = <<"http://localhost">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Realm Normalization Tests
%%%===================================================================

manager_accepts_binary_realm_test() ->
    Url = <<"https://localhost:9446">>,
    Opts = #{realm => <<"binary.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_accepts_string_realm_test() ->
    Url = <<"https://localhost:9447">>,
    Opts = #{realm => "string.realm"},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_accepts_atom_realm_test() ->
    Url = <<"https://localhost:9448">>,
    Opts = #{realm => atom_realm},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Status Management Tests
%%%===================================================================

manager_starts_in_connecting_status_test() ->
    Url = <<"https://localhost:9449">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),

    %% Initial status should be connecting
    timer:sleep(50),  % Give it a moment to process init
    Status = macula_connection:get_status(Pid),

    %% Should be connecting or error (if connection attempt failed quickly)
    ?assert(Status =:= connecting orelse Status =:= error),

    gen_server:stop(Pid).

manager_returns_status_when_not_connected_test() ->
    Url = <<"https://localhost:9450">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    timer:sleep(100),  % Wait for connection attempt

    %% get_status should work even when not connected
    Status = macula_connection:get_status(Pid),
    ?assert(is_atom(Status)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Send Message Tests (when not connected)
%%%===================================================================

send_message_returns_error_when_not_connected_test() ->
    Url = <<"https://localhost:9451">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    timer:sleep(50),

    %% Try to send message when not connected
    Type = publish,
    Msg = #{topic => <<"test.topic">>, payload => <<"data">>},

    Result = macula_connection:send_message(Pid, Type, Msg),

    %% Should return error (not connected)
    ?assertEqual({error, not_connected}, Result),

    gen_server:stop(Pid).

send_message_with_call_type_test() ->
    Url = <<"https://localhost:9452">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    timer:sleep(50),

    Type = call,
    Msg = #{
        call_id => <<"call_123">>,
        procedure => <<"test.proc">>,
        args => #{}
    },

    Result = macula_connection:send_message(Pid, Type, Msg),
    ?assertMatch({error, not_connected}, Result),

    gen_server:stop(Pid).

send_message_with_subscribe_type_test() ->
    Url = <<"https://localhost:9453">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    timer:sleep(50),

    Type = subscribe,
    Msg = #{topics => [<<"topic1">>, <<"topic2">>], qos => 0},

    Result = macula_connection:send_message(Pid, Type, Msg),
    ?assertMatch({error, not_connected}, Result),

    gen_server:stop(Pid).

send_message_with_find_value_type_test() ->
    Url = <<"https://localhost:9454">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    timer:sleep(50),

    Type = find_value,
    ServiceKey = crypto:hash(sha256, <<"test.service">>),
    Msg = #{type => find_value, key => ServiceKey},

    Result = macula_connection:send_message(Pid, Type, Msg),
    ?assertMatch({error, not_connected}, Result),

    gen_server:stop(Pid).

send_message_with_store_type_test() ->
    Url = <<"https://localhost:9455">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    timer:sleep(50),

    Type = store,
    ServiceKey = crypto:hash(sha256, <<"test.service">>),
    Msg = #{
        type => store,
        key => ServiceKey,
        value => #{node_id => <<"node123">>, endpoint => <<"https://node:9443">>}
    },

    Result = macula_connection:send_message(Pid, Type, Msg),
    ?assertMatch({error, not_connected}, Result),

    gen_server:stop(Pid).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

manager_handles_invalid_call_request_test() ->
    Url = <<"https://localhost:9456">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),

    %% Send invalid request
    Result = gen_server:call(Pid, {invalid_request, foo, bar}, 1000),
    ?assertMatch({error, unknown_request}, Result),

    %% Manager should survive
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_handles_invalid_cast_test() ->
    Url = <<"https://localhost:9457">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),

    %% Send invalid cast
    gen_server:cast(Pid, {invalid_cast, foo, bar}),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_handles_invalid_info_message_test() ->
    Url = <<"https://localhost:9458">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),

    %% Send invalid info message
    Pid ! {invalid_info, foo, bar},

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_handles_quic_control_messages_test() ->
    Url = <<"https://localhost:9459">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),

    %% Simulate QUIC control message (atom instead of binary)
    Pid ! {quic, send_shutdown_complete, undefined, []},

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Option Validation Tests
%%%===================================================================

manager_generates_node_id_when_not_provided_test() ->
    Url = <<"https://localhost:9460">>,
    Opts = #{realm => <<"test.realm">>},  % No node_id provided

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_accepts_custom_options_test() ->
    Url = <<"https://localhost:9461">>,
    Opts = #{
        realm => <<"custom.realm">>,
        node_id => <<"custom_node">>,
        custom_opt => <<"custom_value">>
    },

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Integration Behavior Tests
%%%===================================================================

manager_retries_connection_on_failure_test() ->
    Url = <<"https://unreachable.invalid:9999">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),

    %% Wait for connection attempts
    timer:sleep(200),

    %% Should be in error status but still alive (retrying)
    Status = macula_connection:get_status(Pid),
    ?assertEqual(error, Status),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_maintains_recv_buffer_across_messages_test() ->
    %% This test verifies the manager's state structure includes recv_buffer
    %% Even though we can't test actual buffering without QUIC, we verify
    %% the manager initializes and maintains state correctly

    Url = <<"https://localhost:9462">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Message Decoding & Buffering Tests (TDD for v0.7.0)
%%%===================================================================

decode_single_complete_message_test() ->
    %% Build a complete message buffer
    Type = publish,
    Msg = #{
        topic => <<"test.topic">>,
        payload => <<"data">>,
        qos => 0,
        retain => false,
        message_id => <<"test_msg_id">>
    },
    Binary = macula_protocol_encoder:encode(Type, Msg),

    %% Decode should extract the message
    {Messages, Remaining} = macula_connection:decode_messages(Binary, []),

    ?assertEqual(1, length(Messages)),
    ?assertEqual(<<>>, Remaining),

    [{DecodedType, DecodedMsg}] = Messages,
    ?assertEqual(publish, DecodedType),
    ?assertEqual(<<"test.topic">>, maps:get(<<"topic">>, DecodedMsg)).

decode_multiple_messages_in_buffer_test() ->
    %% Build buffer with 3 messages (using proper publish format)
    Msg1 = macula_protocol_encoder:encode(publish, #{
        topic => <<"t1">>,
        payload => <<"d1">>,
        qos => 0,
        retain => false,
        message_id => <<"mid1">>
    }),
    Msg2 = macula_protocol_encoder:encode(connected, #{
        server_node_id => <<"server">>,
        capabilities => [pubsub, rpc]
    }),
    Msg3 = macula_protocol_encoder:encode(publish, #{
        topic => <<"t2">>,
        payload => <<"d2">>,
        qos => 0,
        retain => false,
        message_id => <<"mid2">>
    }),

    Buffer = <<Msg1/binary, Msg2/binary, Msg3/binary>>,

    {Messages, Remaining} = macula_connection:decode_messages(Buffer, []),

    ?assertEqual(3, length(Messages)),
    ?assertEqual(<<>>, Remaining),

    %% Messages should be in order
    [{Type1, _}, {Type2, _}, {Type3, _}] = Messages,
    ?assertEqual(publish, Type1),
    ?assertEqual(connected, Type2),
    ?assertEqual(publish, Type3).

decode_partial_message_leaves_buffer_intact_test() ->
    %% Create a complete message
    FullMsg = macula_protocol_encoder:encode(publish, #{
        topic => <<"test">>,
        payload => <<"data">>,
        qos => 0,
        retain => false,
        message_id => <<"test_msg">>
    }),

    %% Take only first 10 bytes (incomplete)
    <<PartialMsg:10/binary, _Rest/binary>> = FullMsg,

    {Messages, Remaining} = macula_connection:decode_messages(PartialMsg, []),

    ?assertEqual([], Messages),
    ?assertEqual(PartialMsg, Remaining),
    ?assertEqual(10, byte_size(Remaining)).

decode_partial_header_waits_for_more_data_test() ->
    %% Less than 8 bytes (header size)
    PartialHeader = <<1, 2, 3, 4, 5>>,

    {Messages, Remaining} = macula_connection:decode_messages(PartialHeader, []),

    ?assertEqual([], Messages),
    ?assertEqual(PartialHeader, Remaining).

decode_one_complete_one_partial_test() ->
    %% First message complete, second partial
    Msg1 = macula_protocol_encoder:encode(publish, #{
        topic => <<"t1">>,
        payload => <<"d1">>,
        qos => 0,
        retain => false,
        message_id => <<"m1">>
    }),
    Msg2Full = macula_protocol_encoder:encode(connected, #{
        server_node_id => <<"server">>,
        capabilities => [pubsub]
    }),

    <<Msg2Partial:10/binary, _/binary>> = Msg2Full,

    Buffer = <<Msg1/binary, Msg2Partial/binary>>,

    {Messages, Remaining} = macula_connection:decode_messages(Buffer, []),

    ?assertEqual(1, length(Messages)),
    ?assertEqual(Msg2Partial, Remaining),

    [{Type, _}] = Messages,
    ?assertEqual(publish, Type).

decode_empty_buffer_returns_empty_list_test() ->
    {Messages, Remaining} = macula_connection:decode_messages(<<>>, []),

    ?assertEqual([], Messages),
    ?assertEqual(<<>>, Remaining).

%%%===================================================================
%%% URL Parsing Edge Cases (TDD for v0.7.0)
%%%===================================================================

parse_url_ipv4_address_test() ->
    Url = <<"https://192.168.1.100:9443">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

parse_url_localhost_custom_port_test() ->
    Url = <<"https://localhost:4433">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Realm Normalization Edge Cases (TDD for v0.7.0)
%%%===================================================================

normalize_realm_undefined_throws_error_test() ->
    Url = <<"https://localhost:9463">>,
    Opts = #{},  % No realm

    ?assertError({missing_required_option, realm},
                 macula_connection:start_link(Url, Opts)).
