%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_nodeid module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_nodeid_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Node ID Generation Tests
%%%===================================================================

%% Test: generate creates 256-bit (32 byte) node ID
generate_creates_256bit_id_test() ->
    NodeId = macula_routing_nodeid:generate(),
    ?assertEqual(32, byte_size(NodeId)).

%% Test: generate creates unique IDs
generate_creates_unique_ids_test() ->
    Id1 = macula_routing_nodeid:generate(),
    Id2 = macula_routing_nodeid:generate(),
    ?assertNotEqual(Id1, Id2).

%% Test: from_binary accepts 32-byte binary
from_binary_accepts_valid_test() ->
    ValidId = <<1:256>>,
    ?assertEqual({ok, ValidId}, macula_routing_nodeid:from_binary(ValidId)).

%% Test: from_binary rejects wrong size
from_binary_rejects_invalid_size_test() ->
    InvalidId = <<1:128>>,  % Only 16 bytes
    ?assertEqual({error, invalid_size}, macula_routing_nodeid:from_binary(InvalidId)).

%%%===================================================================
%%% XOR Distance Tests
%%%===================================================================

%% Test: distance between same IDs is zero
distance_same_is_zero_test() ->
    Id = <<1:256>>,
    Distance = macula_routing_nodeid:distance(Id, Id),
    ?assertEqual(<<0:256>>, Distance).

%% Test: distance is symmetric (A^B = B^A)
distance_is_symmetric_test() ->
    Id1 = <<1:256>>,
    Id2 = <<2:256>>,
    Dist1 = macula_routing_nodeid:distance(Id1, Id2),
    Dist2 = macula_routing_nodeid:distance(Id2, Id1),
    ?assertEqual(Dist1, Dist2).

%% Test: XOR distance calculation correctness
distance_xor_correct_test() ->
    Id1 = <<0:248, 5:8>>,   % Binary: ...00000101
    Id2 = <<0:248, 3:8>>,   % Binary: ...00000011
    Expected = <<0:248, 6:8>>,  % 5 XOR 3 = 6 (...00000110)
    Distance = macula_routing_nodeid:distance(Id1, Id2),
    ?assertEqual(Expected, Distance).

%% Test: distance triangle inequality holds (d(A,C) <= d(A,B) + d(B,C))
%% This is a property of XOR metric
distance_triangle_inequality_test() ->
    Id1 = <<1:256>>,
    Id2 = <<2:256>>,
    Id3 = <<3:256>>,

    D12 = macula_routing_nodeid:distance(Id1, Id2),
    D23 = macula_routing_nodeid:distance(Id2, Id3),
    D13 = macula_routing_nodeid:distance(Id1, Id3),

    %% In XOR metric, triangle inequality is strict equality for d(A,B) XOR d(B,C) = d(A,C)
    ?assertEqual(D13, macula_routing_nodeid:distance(D12, D23)).

%%%===================================================================
%%% Leading Zeros Tests
%%%===================================================================

%% Test: leading_zeros counts correctly for all zeros
leading_zeros_all_zeros_test() ->
    AllZeros = <<0:256>>,
    ?assertEqual(256, macula_routing_nodeid:leading_zeros(AllZeros)).

%% Test: leading_zeros counts correctly for no leading zeros
leading_zeros_none_test() ->
    NoLeading = <<1:1, 0:255>>,  % MSB is 1
    ?assertEqual(0, macula_routing_nodeid:leading_zeros(NoLeading)).

%% Test: leading_zeros counts correctly for some leading zeros
leading_zeros_some_test() ->
    SomeLeading = <<0:5, 1:1, 0:250>>,  % 5 leading zeros, then 1
    ?assertEqual(5, macula_routing_nodeid:leading_zeros(SomeLeading)).

%% Test: leading_zeros for 1 in last position
leading_zeros_last_bit_test() ->
    LastBit = <<0:255, 1:1>>,
    ?assertEqual(255, macula_routing_nodeid:leading_zeros(LastBit)).

%%%===================================================================
%%% Comparison Tests
%%%===================================================================

%% Test: closer_to prefers closer node
closer_to_prefers_closer_test() ->
    Target = <<100:256>>,
    NodeA = <<101:256>>,  % Distance: 1
    NodeB = <<110:256>>,  % Distance: 10
    ?assert(macula_routing_nodeid:closer_to(Target, NodeA, NodeB)).

%% Test: closer_to rejects farther node
closer_to_rejects_farther_test() ->
    Target = <<100:256>>,
    NodeA = <<110:256>>,  % Distance: 10
    NodeB = <<101:256>>,  % Distance: 1
    ?assertNot(macula_routing_nodeid:closer_to(Target, NodeA, NodeB)).

%% Test: closer_to handles equal distance (returns false by convention)
closer_to_equal_distance_test() ->
    Target = <<100:256>>,
    NodeA = <<101:256>>,
    NodeB = <<101:256>>,  % Same distance
    ?assertNot(macula_routing_nodeid:closer_to(Target, NodeA, NodeB)).

%% Test: compare returns correct ordering
compare_returns_ordering_test() ->
    Target = <<100:256>>,
    NodeA = <<101:256>>,
    NodeB = <<110:256>>,

    ?assertEqual(less, macula_routing_nodeid:compare(Target, NodeA, NodeB)),
    ?assertEqual(greater, macula_routing_nodeid:compare(Target, NodeB, NodeA)),
    ?assertEqual(equal, macula_routing_nodeid:compare(Target, NodeA, NodeA)).

%%%===================================================================
%%% Bucket Index Tests
%%%===================================================================

%% Test: bucket_index for same ID is 256 (special case)
bucket_index_same_test() ->
    Id = <<1:256>>,
    ?assertEqual(256, macula_routing_nodeid:bucket_index(Id, Id)).

%% Test: bucket_index for distance with no leading zeros is 0
bucket_index_no_leading_zeros_test() ->
    Id1 = <<0:255, 0:1>>,
    Id2 = <<1:1, 0:255>>,  % MSB differs
    ?assertEqual(0, macula_routing_nodeid:bucket_index(Id1, Id2)).

%% Test: bucket_index for distance with some leading zeros
bucket_index_some_leading_zeros_test() ->
    Id1 = <<0:256>>,
    Id2 = <<0:10, 1:1, 0:245>>,  % 10 leading zeros
    ?assertEqual(10, macula_routing_nodeid:bucket_index(Id1, Id2)).

%% Test: bucket_index is symmetric
bucket_index_is_symmetric_test() ->
    Id1 = <<1:256>>,
    Id2 = <<2:256>>,
    Index1 = macula_routing_nodeid:bucket_index(Id1, Id2),
    Index2 = macula_routing_nodeid:bucket_index(Id2, Id1),
    ?assertEqual(Index1, Index2).

%%%===================================================================
%%% Serialization Tests
%%%===================================================================

%% Test: to_hex and from_hex roundtrip
hex_roundtrip_test() ->
    NodeId = <<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
               17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32>>,
    Hex = macula_routing_nodeid:to_hex(NodeId),
    ?assertEqual({ok, NodeId}, macula_routing_nodeid:from_hex(Hex)).

%% Test: to_hex produces correct format
to_hex_format_test() ->
    NodeId = <<0:256>>,
    Hex = macula_routing_nodeid:to_hex(NodeId),
    ?assertEqual(64, length(Hex)),  % 32 bytes = 64 hex chars
    ?assert(lists:all(fun(C) -> (C >= $0 andalso C =< $9) orelse
                                 (C >= $a andalso C =< $f) end, Hex)).

%% Test: from_hex rejects invalid hex
from_hex_rejects_invalid_test() ->
    InvalidHex = "zzzz",
    ?assertEqual({error, invalid_hex}, macula_routing_nodeid:from_hex(InvalidHex)).

%% Test: from_hex rejects wrong length
from_hex_rejects_wrong_length_test() ->
    TooShort = "0102",  % Only 2 hex chars
    ?assertEqual({error, invalid_length}, macula_routing_nodeid:from_hex(TooShort)).
