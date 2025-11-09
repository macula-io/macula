%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_id module.
%%% Tests written FIRST (TDD red phase).
%%% ID generation utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_id_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Node ID Generation Tests
%%%===================================================================

%% Test: node_id generates 32-byte random ID
node_id_test() ->
    NodeId = macula_id:node_id(),

    ?assert(is_binary(NodeId)),
    ?assertEqual(32, byte_size(NodeId)).

%% Test: node_id generates unique IDs
node_id_unique_test() ->
    NodeId1 = macula_id:node_id(),
    NodeId2 = macula_id:node_id(),

    ?assertNotEqual(NodeId1, NodeId2).

%%%===================================================================
%%% Message ID Generation Tests
%%%===================================================================

%% Test: message_id generates 16-byte random ID
message_id_test() ->
    MessageId = macula_id:message_id(),

    ?assert(is_binary(MessageId)),
    ?assertEqual(16, byte_size(MessageId)).

%% Test: message_id generates unique IDs
message_id_unique_test() ->
    MessageId1 = macula_id:message_id(),
    MessageId2 = macula_id:message_id(),

    ?assertNotEqual(MessageId1, MessageId2).

%%%===================================================================
%%% Session ID Generation Tests
%%%===================================================================

%% Test: session_id generates 16-byte random ID
session_id_test() ->
    SessionId = macula_id:session_id(),

    ?assert(is_binary(SessionId)),
    ?assertEqual(16, byte_size(SessionId)).

%% Test: session_id generates unique IDs
session_id_unique_test() ->
    SessionId1 = macula_id:session_id(),
    SessionId2 = macula_id:session_id(),

    ?assertNotEqual(SessionId1, SessionId2).

%%%===================================================================
%%% Hash ID Generation Tests
%%%===================================================================

%% Test: hash_id generates deterministic 32-byte hash
hash_id_test() ->
    Data = <<"test data">>,
    HashId1 = macula_id:hash_id(Data),
    HashId2 = macula_id:hash_id(Data),

    ?assert(is_binary(HashId1)),
    ?assertEqual(32, byte_size(HashId1)),
    ?assertEqual(HashId1, HashId2).  % Deterministic

%% Test: hash_id produces different hashes for different data
hash_id_different_data_test() ->
    HashId1 = macula_id:hash_id(<<"data1">>),
    HashId2 = macula_id:hash_id(<<"data2">>),

    ?assertNotEqual(HashId1, HashId2).

%%%===================================================================
%%% UUID String Conversion Tests
%%%===================================================================

%% Test: to_uuid converts binary to UUID string
to_uuid_test() ->
    Id = <<16#12,16#34,16#56,16#78,16#90,16#ab,16#cd,16#ef,
           16#12,16#34,16#56,16#78,16#90,16#ab,16#cd,16#ef>>,

    Uuid = macula_id:to_uuid(Id),

    ?assert(is_binary(Uuid)),
    ?assertEqual(36, byte_size(Uuid)),  % UUID format: 8-4-4-4-12
    ?assertEqual(<<"12345678-90ab-cdef-1234-567890abcdef">>, Uuid).

%% Test: from_uuid converts UUID string to binary
from_uuid_test() ->
    Uuid = <<"12345678-90ab-cdef-1234-567890abcdef">>,

    {ok, Id} = macula_id:from_uuid(Uuid),

    Expected = <<16#12,16#34,16#56,16#78,16#90,16#ab,16#cd,16#ef,
                 16#12,16#34,16#56,16#78,16#90,16#ab,16#cd,16#ef>>,
    ?assertEqual(Expected, Id).

%% Test: to_uuid and from_uuid are inverse operations (16-byte IDs)
uuid_round_trip_test() ->
    OriginalId = macula_id:message_id(),  % 16 bytes

    Uuid = macula_id:to_uuid(OriginalId),
    {ok, DecodedId} = macula_id:from_uuid(Uuid),

    ?assertEqual(OriginalId, DecodedId).

%% Test: from_uuid rejects invalid UUID format
from_uuid_invalid_test() ->
    ?assertEqual({error, invalid_uuid}, macula_id:from_uuid(<<"invalid">>)),
    ?assertEqual({error, invalid_uuid}, macula_id:from_uuid(<<"12345678-90ab-cdef-1234-567890abcdefXX">>)).

%%%===================================================================
%%% Hex String Conversion Tests
%%%===================================================================

%% Test: to_hex converts binary to hex string
to_hex_test() ->
    Id = <<16#de, 16#ad, 16#be, 16#ef>>,

    Hex = macula_id:to_hex(Id),

    ?assertEqual(<<"deadbeef">>, Hex).

%% Test: from_hex converts hex string to binary
from_hex_test() ->
    Hex = <<"deadbeef">>,

    {ok, Id} = macula_id:from_hex(Hex),

    ?assertEqual(<<16#de, 16#ad, 16#be, 16#ef>>, Id).

%% Test: to_hex and from_hex are inverse operations
hex_round_trip_test() ->
    OriginalId = macula_id:node_id(),

    Hex = macula_id:to_hex(OriginalId),
    {ok, DecodedId} = macula_id:from_hex(Hex),

    ?assertEqual(OriginalId, DecodedId).

%% Test: from_hex rejects invalid hex
from_hex_invalid_test() ->
    ?assertEqual({error, invalid_hex}, macula_id:from_hex(<<"xyz">>)),
    ?assertEqual({error, invalid_hex}, macula_id:from_hex(<<"g0">>)).

%% Test: to_hex produces lowercase
to_hex_lowercase_test() ->
    Id = <<16#AB, 16#CD, 16#EF>>,

    Hex = macula_id:to_hex(Id),

    ?assertEqual(<<"abcdef">>, Hex).
