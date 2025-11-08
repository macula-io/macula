%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_protocol_types module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_types_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Message Type ID Tests
%%%===================================================================

%% Test: All message types have unique IDs
message_type_ids_unique_test() ->
    Types = [connect, disconnect, ping, pong,
             publish, subscribe, unsubscribe,
             call, reply, cast,
             swim_ping, swim_ack, swim_ping_req,
             find_node, find_node_reply, store, find_value],
    Ids = [macula_protocol_types:message_type_id(T) || T <- Types],
    ?assertEqual(length(Ids), length(lists:usort(Ids))).

%% Test: message_type_id returns integers in valid range
message_type_id_range_test() ->
    Types = [connect, ping, publish, swim_ping, find_node],
    lists:foreach(
        fun(Type) ->
            Id = macula_protocol_types:message_type_id(Type),
            ?assert(is_integer(Id)),
            ?assert(Id >= 1 andalso Id =< 255)
        end,
        Types
    ).

%% Test: Specific message type IDs
connect_message_type_id_test() ->
    ?assertEqual(16#01, macula_protocol_types:message_type_id(connect)).

publish_message_type_id_test() ->
    ?assertEqual(16#10, macula_protocol_types:message_type_id(publish)).

swim_ping_message_type_id_test() ->
    ?assertEqual(16#30, macula_protocol_types:message_type_id(swim_ping)).

%%%===================================================================
%%% Message Type Name Tests
%%%===================================================================

%% Test: Round-trip type ID to name
message_type_roundtrip_test() ->
    Types = [connect, ping, publish, subscribe],
    lists:foreach(
        fun(Type) ->
            Id = macula_protocol_types:message_type_id(Type),
            {ok, Name} = macula_protocol_types:message_type_name(Id),
            ?assertEqual(Type, Name)
        end,
        Types
    ).

%% Test: Unknown type ID returns error
unknown_message_type_test() ->
    ?assertEqual({error, unknown_type},
                 macula_protocol_types:message_type_name(255)).

%% Test: message_type_name for all valid types
all_message_types_have_names_test() ->
    Types = [connect, disconnect, ping, pong,
             publish, subscribe, unsubscribe,
             call, reply, cast,
             swim_ping, swim_ack, swim_ping_req,
             find_node, find_node_reply, store, find_value],
    lists:foreach(
        fun(Type) ->
            Id = macula_protocol_types:message_type_id(Type),
            Result = macula_protocol_types:message_type_name(Id),
            ?assertMatch({ok, _}, Result)
        end,
        Types
    ).
