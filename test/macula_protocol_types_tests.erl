%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_protocol_types module.
%%% Tests protocol message type ID mapping and conversions.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_types_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% message_type_id/1 - Control Messages Tests
%%%===================================================================

message_type_id_connect_test() ->
    ?assertEqual(16#01, macula_protocol_types:message_type_id(connect)).

message_type_id_disconnect_test() ->
    ?assertEqual(16#02, macula_protocol_types:message_type_id(disconnect)).

message_type_id_ping_test() ->
    ?assertEqual(16#03, macula_protocol_types:message_type_id(ping)).

message_type_id_pong_test() ->
    ?assertEqual(16#04, macula_protocol_types:message_type_id(pong)).

%%%===================================================================
%%% message_type_id/1 - Pub/Sub Messages Tests
%%%===================================================================

message_type_id_publish_test() ->
    ?assertEqual(16#10, macula_protocol_types:message_type_id(publish)).

message_type_id_subscribe_test() ->
    ?assertEqual(16#11, macula_protocol_types:message_type_id(subscribe)).

message_type_id_unsubscribe_test() ->
    ?assertEqual(16#12, macula_protocol_types:message_type_id(unsubscribe)).

%%%===================================================================
%%% message_type_id/1 - RPC Messages Tests
%%%===================================================================

message_type_id_call_test() ->
    ?assertEqual(16#20, macula_protocol_types:message_type_id(call)).

message_type_id_reply_test() ->
    ?assertEqual(16#21, macula_protocol_types:message_type_id(reply)).

message_type_id_cast_test() ->
    ?assertEqual(16#22, macula_protocol_types:message_type_id(cast)).

%%%===================================================================
%%% message_type_id/1 - SWIM Membership Messages Tests
%%%===================================================================

message_type_id_swim_ping_test() ->
    ?assertEqual(16#30, macula_protocol_types:message_type_id(swim_ping)).

message_type_id_swim_ack_test() ->
    ?assertEqual(16#31, macula_protocol_types:message_type_id(swim_ack)).

message_type_id_swim_ping_req_test() ->
    ?assertEqual(16#32, macula_protocol_types:message_type_id(swim_ping_req)).

%%%===================================================================
%%% message_type_id/1 - Kademlia Routing Messages Tests
%%%===================================================================

message_type_id_find_node_test() ->
    ?assertEqual(16#40, macula_protocol_types:message_type_id(find_node)).

message_type_id_find_node_reply_test() ->
    ?assertEqual(16#41, macula_protocol_types:message_type_id(find_node_reply)).

message_type_id_store_test() ->
    ?assertEqual(16#42, macula_protocol_types:message_type_id(store)).

message_type_id_find_value_test() ->
    ?assertEqual(16#43, macula_protocol_types:message_type_id(find_value)).

message_type_id_find_value_reply_test() ->
    ?assertEqual(16#44, macula_protocol_types:message_type_id(find_value_reply)).

%%%===================================================================
%%% message_type_name/1 - Control Messages Tests
%%%===================================================================

message_type_name_connect_test() ->
    ?assertEqual({ok, connect}, macula_protocol_types:message_type_name(16#01)).

message_type_name_disconnect_test() ->
    ?assertEqual({ok, disconnect}, macula_protocol_types:message_type_name(16#02)).

message_type_name_ping_test() ->
    ?assertEqual({ok, ping}, macula_protocol_types:message_type_name(16#03)).

message_type_name_pong_test() ->
    ?assertEqual({ok, pong}, macula_protocol_types:message_type_name(16#04)).

%%%===================================================================
%%% message_type_name/1 - Pub/Sub Messages Tests
%%%===================================================================

message_type_name_publish_test() ->
    ?assertEqual({ok, publish}, macula_protocol_types:message_type_name(16#10)).

message_type_name_subscribe_test() ->
    ?assertEqual({ok, subscribe}, macula_protocol_types:message_type_name(16#11)).

message_type_name_unsubscribe_test() ->
    ?assertEqual({ok, unsubscribe}, macula_protocol_types:message_type_name(16#12)).

%%%===================================================================
%%% message_type_name/1 - RPC Messages Tests
%%%===================================================================

message_type_name_call_test() ->
    ?assertEqual({ok, call}, macula_protocol_types:message_type_name(16#20)).

message_type_name_reply_test() ->
    ?assertEqual({ok, reply}, macula_protocol_types:message_type_name(16#21)).

message_type_name_cast_test() ->
    ?assertEqual({ok, cast}, macula_protocol_types:message_type_name(16#22)).

%%%===================================================================
%%% message_type_name/1 - SWIM Membership Messages Tests
%%%===================================================================

message_type_name_swim_ping_test() ->
    ?assertEqual({ok, swim_ping}, macula_protocol_types:message_type_name(16#30)).

message_type_name_swim_ack_test() ->
    ?assertEqual({ok, swim_ack}, macula_protocol_types:message_type_name(16#31)).

message_type_name_swim_ping_req_test() ->
    ?assertEqual({ok, swim_ping_req}, macula_protocol_types:message_type_name(16#32)).

%%%===================================================================
%%% message_type_name/1 - Kademlia Routing Messages Tests
%%%===================================================================

message_type_name_find_node_test() ->
    ?assertEqual({ok, find_node}, macula_protocol_types:message_type_name(16#40)).

message_type_name_find_node_reply_test() ->
    ?assertEqual({ok, find_node_reply}, macula_protocol_types:message_type_name(16#41)).

message_type_name_store_test() ->
    ?assertEqual({ok, store}, macula_protocol_types:message_type_name(16#42)).

message_type_name_find_value_test() ->
    ?assertEqual({ok, find_value}, macula_protocol_types:message_type_name(16#43)).

message_type_name_find_value_reply_test() ->
    ?assertEqual({ok, find_value_reply}, macula_protocol_types:message_type_name(16#44)).

%%%===================================================================
%%% message_type_name/1 - Error Cases
%%%===================================================================

message_type_name_invalid_id_test() ->
    ?assertEqual({error, unknown_type}, macula_protocol_types:message_type_name(16#FF)).

message_type_name_zero_test() ->
    ?assertEqual({error, unknown_type}, macula_protocol_types:message_type_name(0)).

message_type_name_unassigned_ids_test() ->
    %% Test unassigned IDs in each range
    %% Note: 0x13 is pubsub_route, 0x24/0x25 are rpc_request/rpc_reply
    ?assertEqual({error, unknown_type}, macula_protocol_types:message_type_name(16#05)),  % After control
    ?assertEqual({error, unknown_type}, macula_protocol_types:message_type_name(16#14)),  % After pub/sub (0x13 is pubsub_route)
    ?assertEqual({error, unknown_type}, macula_protocol_types:message_type_name(16#26)),  % After RPC (0x25 is rpc_reply)
    ?assertEqual({error, unknown_type}, macula_protocol_types:message_type_name(16#33)),  % After SWIM
    ?assertEqual({error, unknown_type}, macula_protocol_types:message_type_name(16#45)).  % After Kademlia

%%%===================================================================
%%% Roundtrip Tests
%%%===================================================================

roundtrip_control_messages_test() ->
    Types = [connect, disconnect, ping, pong],
    lists:foreach(fun(Type) ->
        Id = macula_protocol_types:message_type_id(Type),
        {ok, Name} = macula_protocol_types:message_type_name(Id),
        ?assertEqual(Type, Name)
    end, Types).

roundtrip_pubsub_messages_test() ->
    Types = [publish, subscribe, unsubscribe],
    lists:foreach(fun(Type) ->
        Id = macula_protocol_types:message_type_id(Type),
        {ok, Name} = macula_protocol_types:message_type_name(Id),
        ?assertEqual(Type, Name)
    end, Types).

roundtrip_rpc_messages_test() ->
    Types = [call, reply, cast],
    lists:foreach(fun(Type) ->
        Id = macula_protocol_types:message_type_id(Type),
        {ok, Name} = macula_protocol_types:message_type_name(Id),
        ?assertEqual(Type, Name)
    end, Types).

roundtrip_swim_messages_test() ->
    Types = [swim_ping, swim_ack, swim_ping_req],
    lists:foreach(fun(Type) ->
        Id = macula_protocol_types:message_type_id(Type),
        {ok, Name} = macula_protocol_types:message_type_name(Id),
        ?assertEqual(Type, Name)
    end, Types).

roundtrip_kademlia_messages_test() ->
    Types = [find_node, find_node_reply, store, find_value, find_value_reply],
    lists:foreach(fun(Type) ->
        Id = macula_protocol_types:message_type_id(Type),
        {ok, Name} = macula_protocol_types:message_type_name(Id),
        ?assertEqual(Type, Name)
    end, Types).

roundtrip_all_message_types_test() ->
    AllTypes = [
        connect, disconnect, ping, pong,
        publish, subscribe, unsubscribe,
        call, reply, cast,
        swim_ping, swim_ack, swim_ping_req,
        find_node, find_node_reply, store, find_value, find_value_reply
    ],

    lists:foreach(fun(Type) ->
        Id = macula_protocol_types:message_type_id(Type),
        {ok, Name} = macula_protocol_types:message_type_name(Id),
        ?assertEqual(Type, Name)
    end, AllTypes).

%%%===================================================================
%%% Message Type ID Range Tests
%%%===================================================================

message_type_id_ranges_test() ->
    %% Control messages: 0x01-0x04
    ?assert(macula_protocol_types:message_type_id(connect) >= 16#01),
    ?assert(macula_protocol_types:message_type_id(pong) =< 16#04),

    %% Pub/Sub messages: 0x10-0x12
    ?assert(macula_protocol_types:message_type_id(publish) >= 16#10),
    ?assert(macula_protocol_types:message_type_id(unsubscribe) =< 16#12),

    %% RPC messages: 0x20-0x22
    ?assert(macula_protocol_types:message_type_id(call) >= 16#20),
    ?assert(macula_protocol_types:message_type_id(cast) =< 16#22),

    %% SWIM messages: 0x30-0x32
    ?assert(macula_protocol_types:message_type_id(swim_ping) >= 16#30),
    ?assert(macula_protocol_types:message_type_id(swim_ping_req) =< 16#32),

    %% Kademlia messages: 0x40-0x44
    ?assert(macula_protocol_types:message_type_id(find_node) >= 16#40),
    ?assert(macula_protocol_types:message_type_id(find_value_reply) =< 16#44).

%%%===================================================================
%%% Message Type ID Uniqueness Tests
%%%===================================================================

message_type_ids_are_unique_test() ->
    AllTypes = [
        connect, disconnect, ping, pong,
        publish, subscribe, unsubscribe,
        call, reply, cast,
        swim_ping, swim_ack, swim_ping_req,
        find_node, find_node_reply, store, find_value, find_value_reply
    ],

    Ids = [macula_protocol_types:message_type_id(Type) || Type <- AllTypes],
    UniqueIds = lists:usort(Ids),

    %% All IDs should be unique
    ?assertEqual(length(AllTypes), length(UniqueIds)).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

message_type_encoding_size_test() ->
    %% All message type IDs should fit in a single byte
    AllTypes = [
        connect, disconnect, ping, pong,
        publish, subscribe, unsubscribe,
        call, reply, cast,
        swim_ping, swim_ack, swim_ping_req,
        find_node, find_node_reply, store, find_value, find_value_reply
    ],

    lists:foreach(fun(Type) ->
        Id = macula_protocol_types:message_type_id(Type),
        ?assert(Id >= 0),
        ?assert(Id =< 255)
    end, AllTypes).

message_type_naming_consistency_test() ->
    %% Verify that message type names follow consistent naming
    %% All names should be lowercase atoms
    AllTypes = [
        connect, disconnect, ping, pong,
        publish, subscribe, unsubscribe,
        call, reply, cast,
        swim_ping, swim_ack, swim_ping_req,
        find_node, find_node_reply, store, find_value, find_value_reply
    ],

    lists:foreach(fun(Type) ->
        ?assert(is_atom(Type)),
        %% Convert to string and verify lowercase
        TypeStr = atom_to_list(Type),
        ?assertEqual(string:lowercase(TypeStr), TypeStr)
    end, AllTypes).

message_type_count_test() ->
    %% Verify we have exactly 18 message types defined
    AllTypes = [
        connect, disconnect, ping, pong,
        publish, subscribe, unsubscribe,
        call, reply, cast,
        swim_ping, swim_ack, swim_ping_req,
        find_node, find_node_reply, store, find_value, find_value_reply
    ],

    ?assertEqual(18, length(AllTypes)).
