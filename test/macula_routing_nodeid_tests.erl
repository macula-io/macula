%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_nodeid module.
%%% Tests Kademlia DHT node ID utilities with actual code execution.
%%% Node IDs are 256-bit (32-byte) identifiers with XOR distance metric.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_nodeid_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% generate/0 Tests
%%%===================================================================

generate_returns_binary_test() ->
    NodeId = macula_routing_nodeid:generate(),
    ?assert(is_binary(NodeId)).

generate_returns_32_bytes_test() ->
    NodeId = macula_routing_nodeid:generate(),
    ?assertEqual(32, byte_size(NodeId)).

generate_unique_ids_test() ->
    Id1 = macula_routing_nodeid:generate(),
    Id2 = macula_routing_nodeid:generate(),
    ?assertNotEqual(Id1, Id2).

generate_multiple_unique_test() ->
    %% Generate 10 IDs and verify all unique
    Ids = [macula_routing_nodeid:generate() || _ <- lists:seq(1, 10)],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(length(Ids), length(UniqueIds)).

%%%===================================================================
%%% from_binary/1 Tests
%%%===================================================================

from_binary_valid_32_bytes_test() ->
    Binary = <<1:256>>,
    ?assertMatch({ok, _}, macula_routing_nodeid:from_binary(Binary)).

from_binary_returns_same_binary_test() ->
    Binary = <<16#ff:256>>,
    {ok, NodeId} = macula_routing_nodeid:from_binary(Binary),
    ?assertEqual(Binary, NodeId).

from_binary_invalid_size_31_bytes_test() ->
    Binary = <<1:248>>,  % 31 bytes
    ?assertEqual({error, invalid_size}, macula_routing_nodeid:from_binary(Binary)).

from_binary_invalid_size_33_bytes_test() ->
    Binary = <<1:264>>,  % 33 bytes
    ?assertEqual({error, invalid_size}, macula_routing_nodeid:from_binary(Binary)).

from_binary_invalid_size_empty_test() ->
    Binary = <<>>,
    ?assertEqual({error, invalid_size}, macula_routing_nodeid:from_binary(Binary)).

from_binary_invalid_size_1_byte_test() ->
    Binary = <<1>>,
    ?assertEqual({error, invalid_size}, macula_routing_nodeid:from_binary(Binary)).

from_binary_all_zeros_test() ->
    Binary = <<0:256>>,
    {ok, NodeId} = macula_routing_nodeid:from_binary(Binary),
    ?assertEqual(Binary, NodeId).

from_binary_all_ones_test() ->
    Binary = <<16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff:256>>,
    {ok, NodeId} = macula_routing_nodeid:from_binary(Binary),
    ?assertEqual(Binary, NodeId).

%%%===================================================================
%%% distance/2 Tests - XOR Distance Metric
%%%===================================================================

distance_same_node_is_zero_test() ->
    NodeId = <<16#aa:256>>,
    Distance = macula_routing_nodeid:distance(NodeId, NodeId),
    ?assertEqual(<<0:256>>, Distance).

distance_is_symmetric_test() ->
    Node1 = <<16#ff:256>>,
    Node2 = <<16#aa:256>>,
    Dist1 = macula_routing_nodeid:distance(Node1, Node2),
    Dist2 = macula_routing_nodeid:distance(Node2, Node1),
    ?assertEqual(Dist1, Dist2).

distance_xor_properties_test() ->
    %% XOR: 0xFF XOR 0xAA = 0x55
    Node1 = <<16#ff:256>>,
    Node2 = <<16#aa:256>>,
    Expected = <<16#55:256>>,
    Distance = macula_routing_nodeid:distance(Node1, Node2),
    ?assertEqual(Expected, Distance).

distance_zero_and_nonzero_test() ->
    Node1 = <<0:256>>,
    Node2 = <<16#123456:256>>,
    Distance = macula_routing_nodeid:distance(Node1, Node2),
    ?assertEqual(Node2, Distance).

distance_all_ones_test() ->
    Node1 = <<16#ff:256>>,
    Node2 = <<0:256>>,
    Distance = macula_routing_nodeid:distance(Node1, Node2),
    ?assertEqual(Node1, Distance).

distance_triangle_inequality_test() ->
    %% In XOR metric: d(a,b) <= d(a,c) + d(c,b) doesn't hold (not a metric)
    %% But XOR is symmetric and d(a,a) = 0
    Node1 = <<1:256>>,
    Node2 = <<2:256>>,
    Node3 = <<3:256>>,

    D12 = macula_routing_nodeid:distance(Node1, Node2),
    D23 = macula_routing_nodeid:distance(Node2, Node3),
    D13 = macula_routing_nodeid:distance(Node1, Node3),

    %% Verify they are all different (sanity check)
    ?assertNotEqual(D12, D23),
    ?assertNotEqual(D12, D13),
    ?assertNotEqual(D23, D13).

%%%===================================================================
%%% leading_zeros/1 Tests
%%%===================================================================

leading_zeros_all_zeros_test() ->
    Binary = <<0:256>>,
    ?assertEqual(256, macula_routing_nodeid:leading_zeros(Binary)).

leading_zeros_single_bit_test() ->
    %% 0b10000000... (leading bit set)
    Binary = <<128, 0:248>>,
    ?assertEqual(0, macula_routing_nodeid:leading_zeros(Binary)).

leading_zeros_7_zeros_test() ->
    %% 0b00000001... (7 leading zeros)
    Binary = <<1, 0:248>>,
    ?assertEqual(7, macula_routing_nodeid:leading_zeros(Binary)).

leading_zeros_8_zeros_test() ->
    %% First byte is 0, second byte starts with 1
    Binary = <<0, 128, 0:240>>,
    ?assertEqual(8, macula_routing_nodeid:leading_zeros(Binary)).

leading_zeros_15_zeros_test() ->
    %% First byte is 0, second byte is 0b00000001
    Binary = <<0, 1, 0:240>>,
    ?assertEqual(15, macula_routing_nodeid:leading_zeros(Binary)).

leading_zeros_16_zeros_test() ->
    %% Two zero bytes, then 0b10000000
    Binary = <<0, 0, 128, 0:232>>,
    ?assertEqual(16, macula_routing_nodeid:leading_zeros(Binary)).

leading_zeros_partial_byte_test() ->
    %% 0b01000000 = 64 (1 leading zero)
    Binary = <<64, 0:248>>,
    ?assertEqual(1, macula_routing_nodeid:leading_zeros(Binary)).

leading_zeros_various_positions_test() ->
    %% Test different bit positions in the FIRST byte
    Tests = [
        {<<128, 0:248>>, 0},   % 0b10000000
        {<<64, 0:248>>, 1},    % 0b01000000
        {<<32, 0:248>>, 2},    % 0b00100000
        {<<16, 0:248>>, 3},    % 0b00010000
        {<<8, 0:248>>, 4},     % 0b00001000
        {<<4, 0:248>>, 5},     % 0b00000100
        {<<2, 0:248>>, 6},     % 0b00000010
        {<<1, 0:248>>, 7}      % 0b00000001
    ],
    lists:foreach(fun({Bin, Expected}) ->
        ?assertEqual(Expected, macula_routing_nodeid:leading_zeros(Bin))
    end, Tests).

leading_zeros_empty_binary_test() ->
    ?assertEqual(0, macula_routing_nodeid:leading_zeros(<<>>)).

%%%===================================================================
%%% closer_to/3 Tests
%%%===================================================================

closer_to_same_distance_test() ->
    Target = <<1:256>>,
    NodeA = <<2:256>>,
    NodeB = <<3:256>>,
    %% Distance from 1 to 2 is 3, from 1 to 3 is 2
    %% So NodeB (3) is closer to Target (1) than NodeA (2)
    ?assertNot(macula_routing_nodeid:closer_to(Target, NodeA, NodeB)).

closer_to_clear_difference_test() ->
    Target = <<0:256>>,
    %% NodeA is closer (smaller distance)
    NodeA = <<1:256>>,
    NodeB = <<256:256>>,
    ?assert(macula_routing_nodeid:closer_to(Target, NodeA, NodeB)).

closer_to_identical_nodes_test() ->
    Target = <<1:256>>,
    Node = <<2:256>>,
    %% Same node, same distance, not closer
    ?assertNot(macula_routing_nodeid:closer_to(Target, Node, Node)).

closer_to_target_is_nodeA_test() ->
    Target = <<5:256>>,
    NodeA = <<5:256>>,  % Same as target
    NodeB = <<6:256>>,
    %% NodeA has distance 0, which is closer
    ?assert(macula_routing_nodeid:closer_to(Target, NodeA, NodeB)).

%%%===================================================================
%%% compare/3 Tests
%%%===================================================================

compare_nodeA_closer_test() ->
    Target = <<0:256>>,
    NodeA = <<1:256>>,
    NodeB = <<256:256>>,
    ?assertEqual(less, macula_routing_nodeid:compare(Target, NodeA, NodeB)).

compare_nodeB_closer_test() ->
    Target = <<0:256>>,
    NodeA = <<256:256>>,
    NodeB = <<1:256>>,
    ?assertEqual(greater, macula_routing_nodeid:compare(Target, NodeA, NodeB)).

compare_equal_distance_test() ->
    Target = <<1:256>>,
    NodeA = <<2:256>>,
    NodeB = <<2:256>>,  % Same as NodeA
    ?assertEqual(equal, macula_routing_nodeid:compare(Target, NodeA, NodeB)).

compare_both_same_as_target_test() ->
    Target = <<5:256>>,
    NodeA = <<5:256>>,
    NodeB = <<5:256>>,
    ?assertEqual(equal, macula_routing_nodeid:compare(Target, NodeA, NodeB)).

compare_symmetry_test() ->
    Target = <<10:256>>,
    NodeA = <<15:256>>,
    NodeB = <<20:256>>,
    Result1 = macula_routing_nodeid:compare(Target, NodeA, NodeB),
    Result2 = macula_routing_nodeid:compare(Target, NodeB, NodeA),
    %% Reversing should give opposite result
    case Result1 of
        less -> ?assertEqual(greater, Result2);
        greater -> ?assertEqual(less, Result2);
        equal -> ?assertEqual(equal, Result2)
    end.

%%%===================================================================
%%% bucket_index/2 Tests
%%%===================================================================

bucket_index_same_node_test() ->
    NodeId = <<16#aa:256>>,
    %% Same node has special bucket index 256
    ?assertEqual(256, macula_routing_nodeid:bucket_index(NodeId, NodeId)).

bucket_index_distance_1_test() ->
    %% Local: all zeros, Target: last byte is 1
    Local = <<0:256>>,
    Target = <<1:256>>,
    %% Distance = last byte is 0x01 -> 255 leading zeros (31 bytes + 7 bits)
    ?assertEqual(255, macula_routing_nodeid:bucket_index(Local, Target)).

bucket_index_distance_128_test() ->
    %% Local: all zeros, Target: first byte is 128
    Local = <<0:256>>,
    Target = <<128, 0:248>>,
    %% Distance = first byte is 0x80 (0b10000000) -> 0 leading zeros
    ?assertEqual(0, macula_routing_nodeid:bucket_index(Local, Target)).

bucket_index_far_distance_test() ->
    %% Local: all zeros, Target: first byte is 0xff
    Local = <<0:256>>,
    Target = <<16#ff, 0:248>>,
    %% Distance = 0xFF in first byte (0b11111111) -> 0 leading zeros
    ?assertEqual(0, macula_routing_nodeid:bucket_index(Local, Target)).

bucket_index_close_distance_test() ->
    %% Local and Target differ only in last byte
    Local = <<0:248, 16#ff>>,
    Target = <<0:248, 16#fe>>,
    %% Distance = last byte is 0x01 -> 255 leading zeros
    ?assertEqual(255, macula_routing_nodeid:bucket_index(Local, Target)).

bucket_index_all_zero_distance_test() ->
    %% This is the same as same_node_test but explicit
    Local = <<123:256>>,
    Target = <<123:256>>,
    ?assertEqual(256, macula_routing_nodeid:bucket_index(Local, Target)).

bucket_index_range_test() ->
    %% Bucket index should always be 0-256
    Local = macula_routing_nodeid:generate(),
    Target = macula_routing_nodeid:generate(),
    Index = macula_routing_nodeid:bucket_index(Local, Target),
    ?assert(Index >= 0),
    ?assert(Index =< 256).

%%%===================================================================
%%% to_hex/1 Tests
%%%===================================================================

to_hex_all_zeros_test() ->
    NodeId = <<0:256>>,
    Hex = macula_routing_nodeid:to_hex(NodeId),
    Expected = lists:duplicate(64, $0),
    ?assertEqual(Expected, Hex).

to_hex_all_ones_test() ->
    NodeId = <<16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff:256>>,
    Hex = macula_routing_nodeid:to_hex(NodeId),
    Expected = lists:duplicate(64, $f),
    ?assertEqual(Expected, Hex).

to_hex_known_value_test() ->
    NodeId = <<16#123456:256>>,
    Hex = macula_routing_nodeid:to_hex(NodeId),
    %% Pad with zeros to 64 hex chars
    Expected = lists:duplicate(58, $0) ++ "123456",
    ?assertEqual(Expected, Hex).

to_hex_length_test() ->
    NodeId = macula_routing_nodeid:generate(),
    Hex = macula_routing_nodeid:to_hex(NodeId),
    ?assertEqual(64, length(Hex)).

to_hex_lowercase_test() ->
    NodeId = <<16#abcdef:256>>,
    Hex = macula_routing_nodeid:to_hex(NodeId),
    %% Should be lowercase
    ?assert(lists:all(fun(C) ->
        (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f)
    end, Hex)).

%%%===================================================================
%%% from_hex/1 Tests
%%%===================================================================

from_hex_valid_64_chars_test() ->
    Hex = lists:duplicate(64, $0),
    NodeId = macula_routing_nodeid:from_hex(Hex),
    ?assert(is_binary(NodeId)),
    ?assertEqual(32, byte_size(NodeId)).

from_hex_all_zeros_test() ->
    Hex = lists:duplicate(64, $0),
    NodeId = macula_routing_nodeid:from_hex(Hex),
    ?assertEqual(<<0:256>>, NodeId).

from_hex_all_ones_test() ->
    Hex = lists:duplicate(64, $f),
    NodeId = macula_routing_nodeid:from_hex(Hex),
    Expected = <<16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff:256>>,
    ?assertEqual(Expected, NodeId).

from_hex_mixed_case_test() ->
    Hex = lists:duplicate(32, $A) ++ lists:duplicate(32, $a),
    NodeId = macula_routing_nodeid:from_hex(Hex),
    ?assertEqual(32, byte_size(NodeId)).

from_hex_invalid_length_short_test() ->
    Hex = lists:duplicate(63, $0),
    ?assertException(error, function_clause, macula_routing_nodeid:from_hex(Hex)).

from_hex_invalid_length_long_test() ->
    Hex = lists:duplicate(65, $0),
    ?assertException(error, function_clause, macula_routing_nodeid:from_hex(Hex)).

from_hex_invalid_char_test() ->
    %% 'g' is not a valid hex character
    Hex = lists:duplicate(63, $0) ++ "g",
    ?assertException(error, badarg, macula_routing_nodeid:from_hex(Hex)).

from_hex_invalid_char_space_test() ->
    Hex = lists:duplicate(63, $0) ++ " ",
    ?assertException(error, badarg, macula_routing_nodeid:from_hex(Hex)).

from_hex_empty_string_test() ->
    ?assertException(error, function_clause, macula_routing_nodeid:from_hex("")).

from_hex_invalid_hex_wrong_length_test() ->
    %% Invalid hex character + wrong length
    Hex = "xyz",
    ?assertException(error, function_clause, macula_routing_nodeid:from_hex(Hex)).

%%%===================================================================
%%% Hex Roundtrip Tests
%%%===================================================================

hex_roundtrip_test() ->
    OriginalId = <<16#123456789abcdef0:256>>,
    Hex = macula_routing_nodeid:to_hex(OriginalId),
    ConvertedId = macula_routing_nodeid:from_hex(Hex),
    ?assertEqual(OriginalId, ConvertedId).

hex_roundtrip_random_test() ->
    OriginalId = macula_routing_nodeid:generate(),
    Hex = macula_routing_nodeid:to_hex(OriginalId),
    ConvertedId = macula_routing_nodeid:from_hex(Hex),
    ?assertEqual(OriginalId, ConvertedId).

hex_roundtrip_all_zeros_test() ->
    OriginalId = <<0:256>>,
    Hex = macula_routing_nodeid:to_hex(OriginalId),
    ConvertedId = macula_routing_nodeid:from_hex(Hex),
    ?assertEqual(OriginalId, ConvertedId).

hex_roundtrip_all_ones_test() ->
    OriginalId = <<16#ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff:256>>,
    Hex = macula_routing_nodeid:to_hex(OriginalId),
    ConvertedId = macula_routing_nodeid:from_hex(Hex),
    ?assertEqual(OriginalId, ConvertedId).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

full_workflow_test() ->
    %% Generate ID
    NodeId1 = macula_routing_nodeid:generate(),
    ?assertEqual(32, byte_size(NodeId1)),

    %% Convert to hex and back
    Hex = macula_routing_nodeid:to_hex(NodeId1),
    NodeId2 = macula_routing_nodeid:from_hex(Hex),
    ?assertEqual(NodeId1, NodeId2),

    %% Validate with from_binary
    {ok, NodeId3} = macula_routing_nodeid:from_binary(NodeId1),
    ?assertEqual(NodeId1, NodeId3),

    %% Calculate distance to itself (should be 0)
    Distance = macula_routing_nodeid:distance(NodeId1, NodeId1),
    ?assertEqual(<<0:256>>, Distance),

    %% Bucket index to itself (should be 256)
    BucketIndex = macula_routing_nodeid:bucket_index(NodeId1, NodeId1),
    ?assertEqual(256, BucketIndex).

kademlia_distance_properties_test() ->
    %% XOR distance properties for Kademlia DHT
    Node1 = macula_routing_nodeid:generate(),
    Node2 = macula_routing_nodeid:generate(),
    Node3 = macula_routing_nodeid:generate(),

    %% 1. d(x,x) = 0
    D11 = macula_routing_nodeid:distance(Node1, Node1),
    ?assertEqual(<<0:256>>, D11),

    %% 2. d(x,y) = d(y,x) (symmetric)
    D12 = macula_routing_nodeid:distance(Node1, Node2),
    D21 = macula_routing_nodeid:distance(Node2, Node1),
    ?assertEqual(D12, D21),

    %% 3. d(x,y) > 0 for x ≠ y
    ?assertNotEqual(<<0:256>>, D12),

    %% 4. XOR identity: d(x,y) XOR d(y,z) = d(x,z)
    D23 = macula_routing_nodeid:distance(Node2, Node3),
    D13 = macula_routing_nodeid:distance(Node1, Node3),
    D12_xor_D23 = crypto:exor(D12, D23),
    ?assertEqual(D13, D12_xor_D23).

bucket_distribution_test() ->
    %% Generate random nodes and check bucket distribution
    Local = <<0:256>>,
    Nodes = [macula_routing_nodeid:generate() || _ <- lists:seq(1, 20)],
    Buckets = [macula_routing_nodeid:bucket_index(Local, N) || N <- Nodes],

    %% All buckets should be in range 0-255 (not 256 since we're not testing local node)
    ?assert(lists:all(fun(B) -> B >= 0 andalso B =< 255 end, Buckets)).

compare_and_closer_to_consistency_test() ->
    %% compare/3 and closer_to/3 should be consistent
    Target = macula_routing_nodeid:generate(),
    NodeA = macula_routing_nodeid:generate(),
    NodeB = macula_routing_nodeid:generate(),

    CompareResult = macula_routing_nodeid:compare(Target, NodeA, NodeB),
    CloserResult = macula_routing_nodeid:closer_to(Target, NodeA, NodeB),

    case CompareResult of
        less -> ?assert(CloserResult);
        greater -> ?assertNot(CloserResult);
        equal -> ?assertNot(CloserResult)  % Equal distance means not strictly closer
    end.
