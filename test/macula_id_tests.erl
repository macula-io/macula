%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_id module.
%%% Tests ID generation and conversion utilities with actual code execution.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_id_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% ID Generation Tests
%%%===================================================================

node_id_generates_32_bytes_test() ->
    Id = macula_id:node_id(),
    ?assert(is_binary(Id)),
    ?assertEqual(32, byte_size(Id)).

node_id_generates_unique_ids_test() ->
    Id1 = macula_id:node_id(),
    Id2 = macula_id:node_id(),
    ?assertNotEqual(Id1, Id2).

message_id_generates_16_bytes_test() ->
    Id = macula_id:message_id(),
    ?assert(is_binary(Id)),
    ?assertEqual(16, byte_size(Id)).

message_id_generates_unique_ids_test() ->
    Id1 = macula_id:message_id(),
    Id2 = macula_id:message_id(),
    ?assertNotEqual(Id1, Id2).

session_id_generates_16_bytes_test() ->
    Id = macula_id:session_id(),
    ?assert(is_binary(Id)),
    ?assertEqual(16, byte_size(Id)).

session_id_generates_unique_ids_test() ->
    Id1 = macula_id:session_id(),
    Id2 = macula_id:session_id(),
    ?assertNotEqual(Id1, Id2).

%%%===================================================================
%%% Hash ID Tests
%%%===================================================================

hash_id_generates_32_bytes_test() ->
    Data = <<"test data">>,
    Hash = macula_id:hash_id(Data),
    ?assert(is_binary(Hash)),
    ?assertEqual(32, byte_size(Hash)).

hash_id_is_deterministic_test() ->
    Data = <<"test data">>,
    Hash1 = macula_id:hash_id(Data),
    Hash2 = macula_id:hash_id(Data),
    ?assertEqual(Hash1, Hash2).

hash_id_different_data_different_hash_test() ->
    Data1 = <<"test data 1">>,
    Data2 = <<"test data 2">>,
    Hash1 = macula_id:hash_id(Data1),
    Hash2 = macula_id:hash_id(Data2),
    ?assertNotEqual(Hash1, Hash2).

hash_id_empty_data_test() ->
    Data = <<>>,
    Hash = macula_id:hash_id(Data),
    ?assert(is_binary(Hash)),
    ?assertEqual(32, byte_size(Hash)).

%%%===================================================================
%%% UUID Conversion Tests
%%%===================================================================

to_uuid_16_bytes_test() ->
    %% Create 16-byte binary
    Id = <<16#12345678:32, 16#90ab:16, 16#cdef:16, 16#1234:16, 16#567890abcdef:48>>,
    Uuid = macula_id:to_uuid(Id),
    ?assert(is_binary(Uuid)),
    ?assertEqual(36, byte_size(Uuid)),
    ?assertEqual(<<"12345678-90ab-cdef-1234-567890abcdef">>, Uuid).

to_uuid_32_bytes_uses_first_16_test() ->
    %% Create 32-byte binary (first 16 bytes same as above, rest ignored)
    Id = <<16#12345678:32, 16#90ab:16, 16#cdef:16, 16#1234:16, 16#567890abcdef:48,
           0:128>>,  %% Extra 16 bytes
    Uuid = macula_id:to_uuid(Id),
    ?assertEqual(<<"12345678-90ab-cdef-1234-567890abcdef">>, Uuid).

to_uuid_format_test() ->
    Id = macula_id:message_id(),
    Uuid = macula_id:to_uuid(Id),
    %% Check UUID format: 8-4-4-4-12 with dashes
    ?assertEqual(36, byte_size(Uuid)),
    <<Part1:8/binary, $-, Part2:4/binary, $-, Part3:4/binary, $-,
      Part4:4/binary, $-, Part5:12/binary>> = Uuid,
    %% Verify each part is hex
    ?assert(is_hex(Part1)),
    ?assert(is_hex(Part2)),
    ?assert(is_hex(Part3)),
    ?assert(is_hex(Part4)),
    ?assert(is_hex(Part5)).

from_uuid_valid_uuid_test() ->
    Uuid = <<"12345678-90ab-cdef-1234-567890abcdef">>,
    Id = macula_id:from_uuid(Uuid),
    ?assert(is_binary(Id)),
    ?assertEqual(16, byte_size(Id)),
    ?assertEqual(<<16#12345678:32, 16#90ab:16, 16#cdef:16, 16#1234:16,
                   16#567890abcdef:48>>, Id).

from_uuid_roundtrip_test() ->
    OriginalId = <<16#12345678:32, 16#90ab:16, 16#cdef:16, 16#1234:16,
                   16#567890abcdef:48>>,
    Uuid = macula_id:to_uuid(OriginalId),
    ConvertedId = macula_id:from_uuid(Uuid),
    ?assertEqual(OriginalId, ConvertedId).

from_uuid_random_roundtrip_test() ->
    OriginalId = macula_id:message_id(),
    Uuid = macula_id:to_uuid(OriginalId),
    ConvertedId = macula_id:from_uuid(Uuid),
    ?assertEqual(OriginalId, ConvertedId).

from_uuid_invalid_length_test() ->
    Uuid = <<"too-short">>,
    ?assertException(error, function_clause, macula_id:from_uuid(Uuid)).

from_uuid_invalid_format_test() ->
    %% Wrong format (no dashes)
    Uuid = <<"1234567890abcdef1234567890abcdef">>,
    ?assertException(error, function_clause, macula_id:from_uuid(Uuid)).

from_uuid_invalid_hex_test() ->
    %% Invalid hex character 'g'
    Uuid = <<"12345678-90ab-cdef-1234-567890abcdeg">>,
    ?assertException(error, badarg, macula_id:from_uuid(Uuid)).

from_uuid_wrong_dash_positions_test() ->
    %% Dashes in wrong positions
    Uuid = <<"1234-5678-90ab-cdef-1234567890abcdef">>,
    ?assertException(error, {badmatch, _}, macula_id:from_uuid(Uuid)).

%%%===================================================================
%%% Hex Conversion Tests
%%%===================================================================

to_hex_empty_binary_test() ->
    Binary = <<>>,
    Hex = macula_id:to_hex(Binary),
    ?assertEqual(<<>>, Hex).

to_hex_single_byte_test() ->
    Binary = <<16#ab>>,
    Hex = macula_id:to_hex(Binary),
    ?assertEqual(<<"ab">>, Hex).

to_hex_multiple_bytes_test() ->
    Binary = <<16#ab, 16#cd, 16#ef>>,
    Hex = macula_id:to_hex(Binary),
    ?assertEqual(<<"abcdef">>, Hex).

to_hex_with_leading_zeros_test() ->
    Binary = <<16#00, 16#01, 16#0f>>,
    Hex = macula_id:to_hex(Binary),
    ?assertEqual(<<"00010f">>, Hex).

to_hex_all_zeros_test() ->
    Binary = <<0, 0, 0, 0>>,
    Hex = macula_id:to_hex(Binary),
    ?assertEqual(<<"00000000">>, Hex).

to_hex_all_ones_test() ->
    Binary = <<16#ff, 16#ff, 16#ff>>,
    Hex = macula_id:to_hex(Binary),
    ?assertEqual(<<"ffffff">>, Hex).

from_hex_empty_string_test() ->
    Hex = <<>>,
    Binary = macula_id:from_hex(Hex),
    ?assertEqual(<<>>, Binary).

from_hex_single_byte_test() ->
    Hex = <<"ab">>,
    Binary = macula_id:from_hex(Hex),
    ?assertEqual(<<16#ab>>, Binary).

from_hex_multiple_bytes_test() ->
    Hex = <<"abcdef">>,
    Binary = macula_id:from_hex(Hex),
    ?assertEqual(<<16#ab, 16#cd, 16#ef>>, Binary).

from_hex_with_leading_zeros_test() ->
    Hex = <<"00010f">>,
    Binary = macula_id:from_hex(Hex),
    ?assertEqual(<<16#00, 16#01, 16#0f>>, Binary).

from_hex_uppercase_test() ->
    Hex = <<"ABCDEF">>,
    Binary = macula_id:from_hex(Hex),
    ?assertEqual(<<16#ab, 16#cd, 16#ef>>, Binary).

from_hex_mixed_case_test() ->
    Hex = <<"AbCdEf">>,
    Binary = macula_id:from_hex(Hex),
    ?assertEqual(<<16#ab, 16#cd, 16#ef>>, Binary).

from_hex_roundtrip_test() ->
    OriginalBinary = <<16#ab, 16#cd, 16#ef, 16#12, 16#34>>,
    Hex = macula_id:to_hex(OriginalBinary),
    ConvertedBinary = macula_id:from_hex(Hex),
    ?assertEqual(OriginalBinary, ConvertedBinary).

from_hex_random_roundtrip_test() ->
    OriginalBinary = macula_id:message_id(),
    Hex = macula_id:to_hex(OriginalBinary),
    ConvertedBinary = macula_id:from_hex(Hex),
    ?assertEqual(OriginalBinary, ConvertedBinary).

from_hex_odd_length_test() ->
    %% Odd number of hex digits
    Hex = <<"abc">>,
    ?assertException(error, function_clause, macula_id:from_hex(Hex)).

from_hex_invalid_character_test() ->
    %% Invalid hex character 'g' - crashes with function_clause
    Hex = <<"abcdefg">>,
    ?assertException(error, function_clause, macula_id:from_hex(Hex)).

from_hex_invalid_character_space_test() ->
    Hex = <<"ab cd">>,
    ?assertException(error, function_clause, macula_id:from_hex(Hex)).

from_hex_not_binary_test() ->
    ?assertException(error, function_clause, macula_id:from_hex("not a binary")).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

full_uuid_roundtrip_with_node_id_test() ->
    %% Test full workflow with 32-byte node ID
    NodeId = macula_id:node_id(),
    Uuid = macula_id:to_uuid(NodeId),
    RecoveredId = macula_id:from_uuid(Uuid),
    %% Node ID is 32 bytes, UUID only uses first 16
    ?assertEqual(binary:part(NodeId, 0, 16), RecoveredId).

full_hex_roundtrip_with_hash_test() ->
    Data = <<"test data for hash">>,
    Hash = macula_id:hash_id(Data),
    Hex = macula_id:to_hex(Hash),
    RecoveredHash = macula_id:from_hex(Hex),
    ?assertEqual(Hash, RecoveredHash).

multiple_id_types_unique_test() ->
    NodeId = macula_id:node_id(),
    MessageId = macula_id:message_id(),
    SessionId = macula_id:session_id(),

    %% All should be different
    ?assertNotEqual(binary:part(NodeId, 0, 16), MessageId),
    ?assertNotEqual(binary:part(NodeId, 0, 16), SessionId),
    ?assertNotEqual(MessageId, SessionId).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Check if binary contains only hex characters (0-9, a-f, A-F)
is_hex(Binary) when is_binary(Binary) ->
    is_hex(Binary, true).

is_hex(<<>>, Acc) ->
    Acc;
is_hex(<<C, Rest/binary>>, Acc) when (C >= $0 andalso C =< $9);
                                      (C >= $a andalso C =< $f);
                                      (C >= $A andalso C =< $F) ->
    is_hex(Rest, Acc);
is_hex(_, _) ->
    false.
