%%%-------------------------------------------------------------------
%%% @doc
%%% ID generation utilities for Macula.
%%% Provides functions for generating various types of IDs.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_id).

%% API
-export([
    node_id/0,
    message_id/0,
    session_id/0,
    hash_id/1,
    to_uuid/1,
    from_uuid/1,
    to_hex/1,
    from_hex/1
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Generate 256-bit (32-byte) random node ID.
-spec node_id() -> binary().
node_id() ->
    crypto:strong_rand_bytes(32).

%% @doc Generate 128-bit (16-byte) random message ID.
-spec message_id() -> binary().
message_id() ->
    crypto:strong_rand_bytes(16).

%% @doc Generate 128-bit (16-byte) random session ID.
-spec session_id() -> binary().
session_id() ->
    crypto:strong_rand_bytes(16).

%% @doc Generate deterministic 256-bit hash ID from data.
-spec hash_id(binary()) -> binary().
hash_id(Data) when is_binary(Data) ->
    crypto:hash(sha256, Data).

%% @doc Convert 16-byte or 32-byte binary ID to UUID string format.
%% For 16-byte: 8-4-4-4-12 (e.g., "12345678-90ab-cdef-1234-567890abcdef")
%% For 32-byte: Uses first 16 bytes
-spec to_uuid(binary()) -> binary().
to_uuid(<<A:32, B:16, C:16, D:16, E:48, _Rest/binary>>) ->
    %% Handles both 16-byte and 32-byte inputs (uses first 16 bytes)
    list_to_binary(
        io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                      [A, B, C, D, E])
    ).

%% @doc Convert UUID string to 16-byte binary ID.
%% Crashes on invalid UUID format - exposes bugs in validation logic.
-spec from_uuid(binary()) -> binary().
from_uuid(Uuid) when is_binary(Uuid), byte_size(Uuid) =:= 36 ->
    %% Parse UUID format: 8-4-4-4-12 (let it crash on invalid format)
    <<A:8/binary, $-, B:4/binary, $-, C:4/binary, $-,
      D:4/binary, $-, E:12/binary>> = Uuid,

    %% Convert hex strings to integers (let it crash on invalid hex)
    AInt = binary_to_integer(A, 16),
    BInt = binary_to_integer(B, 16),
    CInt = binary_to_integer(C, 16),
    DInt = binary_to_integer(D, 16),
    EInt = binary_to_integer(E, 16),

    %% Pack into binary
    <<AInt:32, BInt:16, CInt:16, DInt:16, EInt:48>>.

%% @doc Convert binary to lowercase hex string.
-spec to_hex(binary()) -> binary().
to_hex(Binary) when is_binary(Binary) ->
    list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= Binary]).

%% @doc Convert hex string to binary.
%% Crashes on invalid hex - exposes bugs in validation logic.
-spec from_hex(binary()) -> binary().
from_hex(Hex) when is_binary(Hex), byte_size(Hex) rem 2 =:= 0 ->
    %% Parse hex string (let it crash on invalid hex characters)
    Bytes = [binary_to_integer(<<H, L>>, 16) || <<H, L>> <= Hex],
    list_to_binary(Bytes).
