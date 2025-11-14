%%%-------------------------------------------------------------------
%%% @doc
%%% Node ID utilities for Kademlia DHT.
%%% 256-bit node identifiers with XOR distance metric.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_nodeid).

%% API
-export([
    generate/0,
    from_binary/1,
    normalize/1,
    distance/2,
    leading_zeros/1,
    closer_to/3,
    compare/3,
    bucket_index/2,
    to_hex/1,
    from_hex/1
]).

%% Types
-type node_id() :: binary().  % 32 bytes (256 bits)
-export_type([node_id/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Generate a random 256-bit node ID.
-spec generate() -> node_id().
generate() ->
    crypto:strong_rand_bytes(32).

%% @doc Create node ID from binary (validates size).
-spec from_binary(binary()) -> {ok, node_id()} | {error, invalid_size}.
from_binary(Binary) when byte_size(Binary) =:= 32 ->
    {ok, Binary};
from_binary(_) ->
    {error, invalid_size}.

%% @doc Normalize any binary to a 32-byte node ID.
%% If already 32 bytes, returns as-is. Otherwise, hashes with SHA-256.
-spec normalize(binary()) -> node_id().
normalize(Binary) when byte_size(Binary) =:= 32 ->
    Binary;
normalize(Binary) when is_binary(Binary) ->
    crypto:hash(sha256, Binary).

%% @doc Calculate XOR distance between two node IDs.
%% Normalizes inputs to 32 bytes if needed.
-spec distance(binary(), binary()) -> binary().
distance(NodeId1, NodeId2) ->
    crypto:exor(normalize(NodeId1), normalize(NodeId2)).

%% @doc Count leading zero bits in binary.
-spec leading_zeros(binary()) -> 0..256.
leading_zeros(Binary) ->
    leading_zeros(Binary, 0).

leading_zeros(<<>>, Acc) ->
    Acc;
leading_zeros(<<0:8, Rest/binary>>, Acc) ->
    leading_zeros(Rest, Acc + 8);
leading_zeros(<<Byte:8, _/binary>>, Acc) ->
    %% Count leading zeros in this byte
    Acc + leading_zeros_byte(Byte).

%% @doc Count leading zeros in a single byte.
-spec leading_zeros_byte(0..255) -> 0..8.
leading_zeros_byte(0) -> 8;
leading_zeros_byte(Byte) when Byte >= 128 -> 0;  % 1xxxxxxx
leading_zeros_byte(Byte) when Byte >= 64 -> 1;   % 01xxxxxx
leading_zeros_byte(Byte) when Byte >= 32 -> 2;   % 001xxxxx
leading_zeros_byte(Byte) when Byte >= 16 -> 3;   % 0001xxxx
leading_zeros_byte(Byte) when Byte >= 8 -> 4;    % 00001xxx
leading_zeros_byte(Byte) when Byte >= 4 -> 5;    % 000001xx
leading_zeros_byte(Byte) when Byte >= 2 -> 6;    % 0000001x
leading_zeros_byte(1) -> 7;                      % 00000001
leading_zeros_byte(0) -> 8.                      % 00000000

%% @doc Check if NodeA is closer to Target than NodeB.
%% Normalizes inputs to 32 bytes if needed.
-spec closer_to(binary(), binary(), binary()) -> boolean().
closer_to(Target, NodeA, NodeB) ->
    DistA = distance(Target, NodeA),
    DistB = distance(Target, NodeB),
    DistA < DistB.

%% @doc Compare distances of NodeA and NodeB to Target.
%% Returns: less (A closer), equal (same distance), greater (B closer).
%% Normalizes inputs to 32 bytes if needed.
-spec compare(binary(), binary(), binary()) -> less | equal | greater.
compare(Target, NodeA, NodeB) ->
    DistA = distance(Target, NodeA),
    DistB = distance(Target, NodeB),
    if
        DistA < DistB -> less;
        DistA > DistB -> greater;
        true -> equal
    end.

%% @doc Calculate bucket index for a node relative to local node.
%% Returns leading zero count of XOR distance (0..255).
%% Special case: distance 0 (same node) returns 256.
%% Normalizes inputs to 32 bytes if needed.
-spec bucket_index(binary(), binary()) -> 0..256.
bucket_index(LocalNodeId, TargetNodeId) ->
    Distance = distance(LocalNodeId, TargetNodeId),
    case Distance of
        <<0:256>> -> 256;  % Same node (special case)
        _ -> leading_zeros(Distance)
    end.

%% @doc Convert node ID to hex string.
-spec to_hex(node_id()) -> string().
to_hex(NodeId) ->
    binary:bin_to_list(binary:encode_hex(NodeId, lowercase)).

%% @doc Parse node ID from hex string.
%% Crashes on invalid hex or wrong length - exposes bugs in validation logic.
-spec from_hex(string()) -> node_id().
from_hex(HexString) when length(HexString) =:= 64 ->
    %% Decode hex (let it crash on invalid characters)
    binary:decode_hex(list_to_binary(HexString)).
