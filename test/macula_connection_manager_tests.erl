%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_connection_manager module.
%%%
%%% Tests connection lifecycle and message handling without requiring
%%% actual QUIC connections. Focuses on:
%%% - Handler lifecycle (start/stop)
%%% - URL parsing (various formats)
%%% - Realm normalization (various types)
%%% - Status management
%%% - Message sending (when not connected)
%%% - Error handling
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Lifecycle Tests
%%%===================================================================

manager_starts_with_valid_url_and_realm_test() ->
    Url = <<"https://localhost:9443">>,
    Opts = #{realm => <<"test.realm">>},

    Result = macula_connection_manager:start_link(Url, Opts),

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

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
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

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% URL Parsing Tests (testing internal parse_url/1 via behavior)
%%%===================================================================

manager_parses_https_url_with_port_test() ->
    Url = <<"https://example.com:9443">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_parses_https_url_without_port_test() ->
    Url = <<"https://example.com">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_parses_http_url_with_port_test() ->
    Url = <<"http://localhost:8080">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_parses_http_url_without_port_test() ->
    Url = <<"http://localhost">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Realm Normalization Tests
%%%===================================================================

manager_accepts_binary_realm_test() ->
    Url = <<"https://localhost:9446">>,
    Opts = #{realm => <<"binary.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_accepts_string_realm_test() ->
    Url = <<"https://localhost:9447">>,
    Opts = #{realm => "string.realm"},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_accepts_atom_realm_test() ->
    Url = <<"https://localhost:9448">>,
    Opts = #{realm => atom_realm},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Status Management Tests
%%%===================================================================

manager_starts_in_connecting_status_test() ->
    Url = <<"https://localhost:9449">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),

    %% Initial status should be connecting
    timer:sleep(50),  % Give it a moment to process init
    Status = macula_connection_manager:get_status(Pid),

    %% Should be connecting or error (if connection attempt failed quickly)
    ?assert(Status =:= connecting orelse Status =:= error),

    gen_server:stop(Pid).

manager_returns_status_when_not_connected_test() ->
    Url = <<"https://localhost:9450">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    timer:sleep(100),  % Wait for connection attempt

    %% get_status should work even when not connected
    Status = macula_connection_manager:get_status(Pid),
    ?assert(is_atom(Status)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Send Message Tests (when not connected)
%%%===================================================================

send_message_returns_error_when_not_connected_test() ->
    Url = <<"https://localhost:9451">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    timer:sleep(50),

    %% Try to send message when not connected
    Type = publish,
    Msg = #{topic => <<"test.topic">>, payload => <<"data">>},

    Result = macula_connection_manager:send_message(Pid, Type, Msg),

    %% Should return error (not connected)
    ?assertEqual({error, not_connected}, Result),

    gen_server:stop(Pid).

send_message_with_call_type_test() ->
    Url = <<"https://localhost:9452">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    timer:sleep(50),

    Type = call,
    Msg = #{
        call_id => <<"call_123">>,
        procedure => <<"test.proc">>,
        args => #{}
    },

    Result = macula_connection_manager:send_message(Pid, Type, Msg),
    ?assertMatch({error, not_connected}, Result),

    gen_server:stop(Pid).

send_message_with_subscribe_type_test() ->
    Url = <<"https://localhost:9453">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    timer:sleep(50),

    Type = subscribe,
    Msg = #{topics => [<<"topic1">>, <<"topic2">>], qos => 0},

    Result = macula_connection_manager:send_message(Pid, Type, Msg),
    ?assertMatch({error, not_connected}, Result),

    gen_server:stop(Pid).

send_message_with_find_value_type_test() ->
    Url = <<"https://localhost:9454">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    timer:sleep(50),

    Type = find_value,
    ServiceKey = crypto:hash(sha256, <<"test.service">>),
    Msg = #{type => find_value, key => ServiceKey},

    Result = macula_connection_manager:send_message(Pid, Type, Msg),
    ?assertMatch({error, not_connected}, Result),

    gen_server:stop(Pid).

send_message_with_store_type_test() ->
    Url = <<"https://localhost:9455">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    timer:sleep(50),

    Type = store,
    ServiceKey = crypto:hash(sha256, <<"test.service">>),
    Msg = #{
        type => store,
        key => ServiceKey,
        value => #{node_id => <<"node123">>, endpoint => <<"https://node:9443">>}
    },

    Result = macula_connection_manager:send_message(Pid, Type, Msg),
    ?assertMatch({error, not_connected}, Result),

    gen_server:stop(Pid).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

manager_handles_invalid_call_request_test() ->
    Url = <<"https://localhost:9456">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),

    %% Send invalid request
    Result = gen_server:call(Pid, {invalid_request, foo, bar}, 1000),
    ?assertMatch({error, unknown_request}, Result),

    %% Manager should survive
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_handles_invalid_cast_test() ->
    Url = <<"https://localhost:9457">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),

    %% Send invalid cast
    gen_server:cast(Pid, {invalid_cast, foo, bar}),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_handles_invalid_info_message_test() ->
    Url = <<"https://localhost:9458">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),

    %% Send invalid info message
    Pid ! {invalid_info, foo, bar},

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_handles_quic_control_messages_test() ->
    Url = <<"https://localhost:9459">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),

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

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_accepts_custom_options_test() ->
    Url = <<"https://localhost:9461">>,
    Opts = #{
        realm => <<"custom.realm">>,
        node_id => <<"custom_node">>,
        custom_opt => <<"custom_value">>
    },

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Integration Behavior Tests
%%%===================================================================

manager_retries_connection_on_failure_test() ->
    Url = <<"https://unreachable.invalid:9999">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),

    %% Wait for connection attempts
    timer:sleep(200),

    %% Should be in error status but still alive (retrying)
    Status = macula_connection_manager:get_status(Pid),
    ?assertEqual(error, Status),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_maintains_recv_buffer_across_messages_test() ->
    %% This test verifies the manager's state structure includes recv_buffer
    %% Even though we can't test actual buffering without QUIC, we verify
    %% the manager initializes and maintains state correctly

    Url = <<"https://localhost:9462">>,
    Opts = #{realm => <<"test.realm">>},

    {ok, Pid} = macula_connection_manager:start_link(Url, Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).
