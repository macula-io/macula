%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_connection_dispatch.
%%%
%%% Tests pure functional parts: message routing, peer normalization,
%%% map field access, mesh lifecycle interception, JSON decoding,
%%% and handler execution result handling.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_dispatch_tests).

-include_lib("eunit/include/eunit.hrl").
-include("macula_connection.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

-define(TEST_REALM, <<"test-realm">>).

make_node_id() ->
    crypto:strong_rand_bytes(32).

make_state() ->
    make_state(#{}).

make_state(Overrides) ->
    NodeId = maps:get(node_id, Overrides, make_node_id()),
    #state{
        url = maps:get(url, Overrides, <<"https://localhost:9443">>),
        opts = maps:get(opts, Overrides, #{}),
        node_id = NodeId,
        realm = maps:get(realm, Overrides, ?TEST_REALM),
        peer_id = maps:get(peer_id, Overrides, erlang:unique_integer([positive])),
        connection = maps:get(connection, Overrides, undefined),
        stream = maps:get(stream, Overrides, undefined),
        status = maps:get(status, Overrides, connected)
    }.

ensure_gproc() ->
    case whereis(gproc) of
        undefined -> application:ensure_all_started(gproc);
        _ -> ok
    end.

ensure_pg() ->
    case whereis(pg) of
        undefined -> pg:start_link(pg);
        _ -> ok
    end.

%%%===================================================================
%%% normalize_peer_node_id tests
%%%===================================================================

normalize_32_byte_raw_test() ->
    Raw = crypto:strong_rand_bytes(32),
    ?assertEqual(Raw, macula_connection_dispatch:normalize_peer_node_id(Raw)).

normalize_64_byte_hex_test() ->
    Raw = crypto:strong_rand_bytes(32),
    Hex = binary:encode_hex(Raw),
    ?assertEqual(64, byte_size(Hex)),
    ?assertEqual(Raw, macula_connection_dispatch:normalize_peer_node_id(Hex)).

normalize_other_size_passthrough_test() ->
    Short = <<"abc">>,
    ?assertEqual(Short, macula_connection_dispatch:normalize_peer_node_id(Short)).

%%%===================================================================
%%% get_map_field tests
%%%===================================================================

get_map_field_atom_key_test() ->
    Map = #{name => <<"alice">>},
    ?assertEqual(<<"alice">>, macula_connection_dispatch:get_map_field(Map, name, <<"name">>)).

get_map_field_binary_key_test() ->
    Map = #{<<"name">> => <<"bob">>},
    ?assertEqual(<<"bob">>, macula_connection_dispatch:get_map_field(Map, name, <<"name">>)).

get_map_field_atom_takes_precedence_test() ->
    Map = #{name => <<"atom_val">>, <<"name">> => <<"bin_val">>},
    ?assertEqual(<<"atom_val">>, macula_connection_dispatch:get_map_field(Map, name, <<"name">>)).

get_map_field_missing_returns_undefined_test() ->
    Map = #{other => 1},
    ?assertEqual(undefined, macula_connection_dispatch:get_map_field(Map, name, <<"name">>)).

get_map_field_4_arity_default_test() ->
    Map = #{},
    ?assertEqual(<<"fallback">>,
                 macula_connection_dispatch:get_map_field(Map, name, <<"name">>, <<"fallback">>)).

get_map_field_4_arity_found_test() ->
    Map = #{<<"endpoint">> => <<"quic://host:9443">>},
    ?assertEqual(<<"quic://host:9443">>,
                 macula_connection_dispatch:get_map_field(Map, endpoint, <<"endpoint">>, <<"default">>)).

%%%===================================================================
%%% handle_mesh_lifecycle_publish tests (via process_message)
%%%===================================================================

mesh_peer_connected_handled_test() ->
    ensure_gproc(),
    State = make_state(),
    Msg = #{<<"topic">> => <<"_mesh.peer.connected">>,
            <<"payload">> => #{<<"node_id">> => <<"abc">>}},
    ResultState = macula_connection_dispatch:process_message({publish, Msg}, State),
    receive
        {mesh_peer_connected, Payload} ->
            ?assertEqual(#{<<"node_id">> => <<"abc">>}, Payload)
    after 100 ->
        ?assert(false, "Expected mesh_peer_connected message")
    end,
    ?assertEqual(State, ResultState).

mesh_peer_disconnected_handled_test() ->
    ensure_gproc(),
    State = make_state(),
    Msg = #{<<"topic">> => <<"_mesh.peer.disconnected">>,
            <<"payload">> => #{<<"node_id">> => <<"def">>}},
    ResultState = macula_connection_dispatch:process_message({publish, Msg}, State),
    receive
        {mesh_peer_disconnected, Payload} ->
            ?assertEqual(#{<<"node_id">> => <<"def">>}, Payload)
    after 100 ->
        ?assert(false, "Expected mesh_peer_disconnected message")
    end,
    ?assertEqual(State, ResultState).

mesh_lifecycle_uses_full_msg_when_no_payload_test() ->
    ensure_gproc(),
    State = make_state(),
    Msg = #{<<"topic">> => <<"_mesh.peer.connected">>,
            <<"node_id">> => <<"ghi">>},
    _ = macula_connection_dispatch:process_message({publish, Msg}, State),
    receive
        {mesh_peer_connected, Payload} ->
            ?assertEqual(Msg, Payload)
    after 100 ->
        ?assert(false, "Expected mesh_peer_connected message")
    end.

%%%===================================================================
%%% process_message: connected (passthrough)
%%%===================================================================

process_message_connected_returns_state_test() ->
    State = make_state(),
    Msg = #{<<"server">> => <<"relay-de-munich">>},
    ?assertEqual(State, macula_connection_dispatch:process_message({connected, Msg}, State)).

%%%===================================================================
%%% process_message: unknown type
%%%===================================================================

process_message_unknown_type_returns_state_test() ->
    State = make_state(),
    ?assertEqual(State, macula_connection_dispatch:process_message({bizarre_type, #{}}, State)).

%%%===================================================================
%%% process_message: publish passthrough (no handler registered)
%%%===================================================================

publish_passthrough_no_handler_test() ->
    ensure_gproc(),
    State = make_state(),
    Msg = #{<<"topic">> => <<"app.events.something">>},
    ResultState = macula_connection_dispatch:process_message({publish, Msg}, State),
    ?assertEqual(State, ResultState).

%%%===================================================================
%%% process_message: reply with no rpc handler
%%%===================================================================

reply_no_rpc_handler_test() ->
    ensure_gproc(),
    State = make_state(),
    Msg = #{<<"call_id">> => <<"req-1">>, <<"result">> => <<"ok">>},
    ResultState = macula_connection_dispatch:process_message({reply, Msg}, State),
    ?assertEqual(State, ResultState).

%%%===================================================================
%%% process_message: rpc_reply with no handler
%%%===================================================================

rpc_reply_no_handler_test() ->
    ensure_gproc(),
    State = make_state(),
    Msg = #{<<"request_id">> => <<"req-42">>, <<"result">> => <<"pong">>},
    ResultState = macula_connection_dispatch:process_message({rpc_reply, Msg}, State),
    ?assertEqual(State, ResultState).

%%%===================================================================
%%% process_message: find_value_reply with no handler
%%%===================================================================

find_value_reply_no_handler_test() ->
    ensure_gproc(),
    State = make_state(),
    Msg = #{<<"key">> => <<"some_key">>, <<"value">> => <<"some_val">>},
    ResultState = macula_connection_dispatch:process_message({find_value_reply, Msg}, State),
    ?assertEqual(State, ResultState).

%%%===================================================================
%%% process_message: find_node_reply (no routing server)
%%%===================================================================

find_node_reply_no_routing_server_test() ->
    State = make_state(),
    Msg = #{<<"nodes">> => [#{<<"node_id">> => crypto:strong_rand_bytes(32),
                              <<"endpoint">> => <<"quic://1.2.3.4:9443">>}]},
    ResultState = macula_connection_dispatch:process_message({find_node_reply, Msg}, State),
    ?assertEqual(State, ResultState).

%%%===================================================================
%%% handle_execution_result tests
%%%===================================================================

handle_execution_result_ok_test() ->
    ?assertEqual({ok, <<"done">>},
                 macula_connection_dispatch:handle_execution_result({ok, <<"done">>})).

handle_execution_result_error_test() ->
    ?assertEqual({error, <<"nope">>},
                 macula_connection_dispatch:handle_execution_result({error, <<"nope">>})).

handle_execution_result_exit_test() ->
    Result = macula_connection_dispatch:handle_execution_result({'EXIT', badarg}),
    ?assertMatch({error, _}, Result).

handle_execution_result_arbitrary_value_test() ->
    %% Non-EXIT values pass through unchanged
    ?assertEqual(42, macula_connection_dispatch:handle_execution_result(42)).

%%%===================================================================
%%% safe_decode_json tests
%%%===================================================================

safe_decode_json_valid_binary_test() ->
    Json = <<"{\"key\":\"value\"}">>,
    Result = macula_connection_dispatch:safe_decode_json(Json),
    ?assertEqual(#{<<"key">> => <<"value">>}, Result).

safe_decode_json_invalid_binary_test() ->
    %% Invalid JSON returns the original binary
    Invalid = <<"not-json{{{">>,
    Result = macula_connection_dispatch:safe_decode_json(Invalid),
    ?assertEqual(Invalid, Result).

safe_decode_json_map_passthrough_test() ->
    %% Maps decode to themselves (json:decode on a map returns it)
    Map = #{<<"already">> => <<"decoded">>},
    Result = macula_connection_dispatch:safe_decode_json(Map),
    ?assertEqual(Map, Result).

%%%===================================================================
%%% extract_peer_info tests
%%%===================================================================

extract_peer_info_skips_self_test() ->
    MyId = make_node_id(),
    NodeInfo = #{node_id => MyId, endpoint => <<"quic://1.2.3.4:9443">>},
    ?assertEqual(false, macula_connection_dispatch:extract_peer_info(NodeInfo, MyId)).

extract_peer_info_returns_peer_test() ->
    MyId = make_node_id(),
    PeerId = make_node_id(),
    NodeInfo = #{node_id => PeerId, endpoint => <<"quic://5.6.7.8:9443">>},
    {true, Info} = macula_connection_dispatch:extract_peer_info(NodeInfo, MyId),
    ?assertEqual(PeerId, maps:get(node_id, Info)),
    ?assertEqual(<<"quic://5.6.7.8:9443">>, maps:get(endpoint, Info)).

extract_peer_info_hex_node_id_test() ->
    MyId = make_node_id(),
    PeerId = make_node_id(),
    HexPeerId = binary:encode_hex(PeerId),
    NodeInfo = #{<<"node_id">> => HexPeerId, <<"endpoint">> => <<"quic://10.0.0.1:9443">>},
    {true, Info} = macula_connection_dispatch:extract_peer_info(NodeInfo, MyId),
    ?assertEqual(PeerId, maps:get(node_id, Info)).

extract_peer_info_skips_hex_self_test() ->
    MyId = make_node_id(),
    HexMyId = binary:encode_hex(MyId),
    NodeInfo = #{<<"node_id">> => HexMyId, <<"endpoint">> => <<"quic://1.2.3.4:9443">>},
    ?assertEqual(false, macula_connection_dispatch:extract_peer_info(NodeInfo, MyId)).

extract_peer_info_undefined_node_id_test() ->
    MyId = make_node_id(),
    NodeInfo = #{<<"endpoint">> => <<"quic://1.2.3.4:9443">>},
    ?assertEqual(false, macula_connection_dispatch:extract_peer_info(NodeInfo, MyId)).

extract_peer_info_non_map_test() ->
    MyId = make_node_id(),
    ?assertEqual(false, macula_connection_dispatch:extract_peer_info(not_a_map, MyId)).

extract_peer_info_uses_address_fallback_test() ->
    MyId = make_node_id(),
    PeerId = make_node_id(),
    NodeInfo = #{node_id => PeerId, address => <<"quic://addr:9443">>},
    {true, Info} = macula_connection_dispatch:extract_peer_info(NodeInfo, MyId),
    ?assertEqual(<<"quic://addr:9443">>, maps:get(endpoint, Info)).

%%%===================================================================
%%% make_peer_info tests
%%%===================================================================

make_peer_info_undefined_node_id_test() ->
    ?assertEqual(false, macula_connection_dispatch:make_peer_info(undefined, <<"ep">>, <<"myid">>)).

%%%===================================================================
%%% notify_mesh_lifecycle_observers tests
%%%===================================================================

notify_mesh_lifecycle_observers_test() ->
    ensure_pg(),
    ok = pg:join(pg, macula_mesh_lifecycle, self()),
    PeerInfo = #{node_id => <<"test_node">>},
    ok = macula_connection_dispatch:notify_mesh_lifecycle_observers(mesh_peer_connected, PeerInfo),
    receive
        {macula_mesh_event, mesh_peer_connected, ReceivedInfo} ->
            ?assertEqual(PeerInfo, ReceivedInfo)
    after 200 ->
        ?assert(false, "Expected macula_mesh_event message")
    end,
    pg:leave(pg, macula_mesh_lifecycle, self()).

%%%===================================================================
%%% remove_peer_from_routing_table tests
%%%===================================================================

remove_peer_undefined_test() ->
    ?assertEqual(ok, macula_connection_dispatch:remove_peer_from_routing_table(undefined)).

remove_peer_no_routing_server_test() ->
    ?assertEqual(ok, macula_connection_dispatch:remove_peer_from_routing_table(make_node_id())).
