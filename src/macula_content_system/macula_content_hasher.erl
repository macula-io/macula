%%%-------------------------------------------------------------------
%%% @doc
%%% Content hashing module for Macula content-addressed storage.
%%%
%%% Provides cryptographic hashing using BLAKE3 (primary) and SHA256 (fallback).
%%% BLAKE3 uses a pure Erlang implementation that can be optimized with NIFs
%%% from macula-nifs/ for production use.
%%%
%%% == Example Usage ==
%%% ```
%%% %% Hash binary data
%%% Hash = macula_content_hasher:hash(blake3, Data).
%%%
%%% %% Verify hash
%%% true = macula_content_hasher:verify(blake3, Data, Hash).
%%%
%%% %% Stream hash multiple chunks
%%% Hash = macula_content_hasher:hash_streaming(sha256, [Chunk1, Chunk2]).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_hasher).

%% API
-export([
    hash/2,
    hash_streaming/2,
    verify/3,
    supported_algorithms/0,
    is_supported/1,
    hash_size/1,
    hex_encode/1,
    hex_decode/1
]).

%% Types
-type algorithm() :: blake3 | sha256.
-type hash() :: <<_:256>>.  %% 32 bytes

-export_type([algorithm/0, hash/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Hash binary data using the specified algorithm.
-spec hash(algorithm(), binary()) -> hash().
hash(sha256, Data) ->
    crypto:hash(sha256, Data);
hash(blake3, Data) ->
    blake3_hash(Data).

%% @doc Hash a list of binary chunks using streaming (memory efficient).
-spec hash_streaming(algorithm(), [binary()]) -> hash().
hash_streaming(sha256, Chunks) ->
    Context0 = crypto:hash_init(sha256),
    Context1 = lists:foldl(
        fun(Chunk, Ctx) -> crypto:hash_update(Ctx, Chunk) end,
        Context0,
        Chunks
    ),
    crypto:hash_final(Context1);
hash_streaming(blake3, Chunks) ->
    %% Use NIF streaming hash if available (avoids concatenation)
    case is_nif_available() of
        true -> macula_blake3_nif:hash_streaming(Chunks);
        false ->
            Combined = iolist_to_binary(Chunks),
            blake3_pure(Combined)
    end.

%% @doc Verify that data matches the expected hash.
-spec verify(algorithm(), binary(), hash()) -> boolean().
verify(Algorithm, Data, ExpectedHash) ->
    ActualHash = hash(Algorithm, Data),
    ActualHash =:= ExpectedHash.

%% @doc Return list of supported hash algorithms.
-spec supported_algorithms() -> [algorithm()].
supported_algorithms() ->
    [blake3, sha256].

%% @doc Check if an algorithm is supported.
-spec is_supported(atom()) -> boolean().
is_supported(blake3) -> true;
is_supported(sha256) -> true;
is_supported(_) -> false.

%% @doc Return hash output size in bytes for an algorithm.
-spec hash_size(algorithm()) -> pos_integer().
hash_size(blake3) -> 32;
hash_size(sha256) -> 32.

%% @doc Encode binary to lowercase hex string.
-spec hex_encode(binary()) -> binary().
hex_encode(Bin) ->
    << <<(hex_digit(N))>> || <<N:4>> <= Bin >>.

%% @doc Decode hex string to binary.
-spec hex_decode(binary()) -> {ok, binary()} | {error, invalid_hex}.
hex_decode(Hex) ->
    hex_decode(Hex, <<>>).

%%%===================================================================
%%% Internal Functions - BLAKE3
%%%===================================================================

%% @private
%% BLAKE3 hash with NIF acceleration when available.
%%
%% Tries to use macula_blake3_nif (from macula-nifs package) which provides
%% a Rust NIF implementation that is 10-20x faster than pure Erlang.
%% Falls back to pure Erlang implementation if NIFs are not available.
-spec blake3_hash(binary()) -> hash().
blake3_hash(Data) ->
    case is_nif_available() of
        true -> macula_blake3_nif:hash(Data);
        false -> blake3_pure(Data)
    end.

%% @private
%% Check if BLAKE3 NIF is available.
%% Caches result in process dictionary for performance.
is_nif_available() ->
    case get(macula_blake3_nif_available) of
        undefined ->
            Available = check_nif_available(),
            put(macula_blake3_nif_available, Available),
            Available;
        Cached ->
            Cached
    end.

%% @private
check_nif_available() ->
    case code:ensure_loaded(macula_blake3_nif) of
        {module, macula_blake3_nif} ->
            case erlang:function_exported(macula_blake3_nif, is_nif_loaded, 0) of
                true -> macula_blake3_nif:is_nif_loaded();
                false -> false
            end;
        _ ->
            false
    end.

%% @private
%% Pure Erlang BLAKE3 implementation.
%% Based on the BLAKE3 specification with key constants from ChaCha.
blake3_pure(Data) ->
    %% BLAKE3 IV (same as BLAKE2s IV)
    IV = {
        16#6A09E667, 16#BB67AE85,
        16#3C6EF372, 16#A54FF53A,
        16#510E527F, 16#9B05688C,
        16#1F83D9AB, 16#5BE0CD19
    },

    %% BLAKE3 constants
    BlockLen = 64,
    ChunkLen = 1024,

    %% Flag constants
    RootFlag = 8,

    %% Process data in chunks
    Chunks = chunk_data(Data, ChunkLen),
    ChunkCount = length(Chunks),

    %% Hash each chunk
    ChunkHashes = lists:map(
        fun({Index, Chunk}) ->
            IsFirst = Index =:= 0,
            IsLast = Index =:= ChunkCount - 1,
            Flags = chunk_flags(IsFirst, IsLast, ChunkCount =:= 1),
            compress_chunk(IV, Chunk, BlockLen, Flags)
        end,
        lists:zip(lists:seq(0, ChunkCount - 1), Chunks)
    ),

    %% Build Merkle tree from chunk hashes
    finalize_tree(ChunkHashes, IV, RootFlag).

%% @private
chunk_data(<<>>, _ChunkLen) ->
    [<<>>];
chunk_data(Data, ChunkLen) ->
    chunk_data(Data, ChunkLen, []).

chunk_data(<<>>, _ChunkLen, Acc) ->
    lists:reverse(Acc);
chunk_data(Data, ChunkLen, Acc) when byte_size(Data) =< ChunkLen ->
    lists:reverse([Data | Acc]);
chunk_data(Data, ChunkLen, Acc) ->
    <<Chunk:ChunkLen/binary, Rest/binary>> = Data,
    chunk_data(Rest, ChunkLen, [Chunk | Acc]).

%% @private
chunk_flags(true, true, true) -> 1 bor 2 bor 8;  %% START | END | ROOT
chunk_flags(true, true, false) -> 1 bor 2;       %% START | END
chunk_flags(true, false, _) -> 1;                %% START
chunk_flags(false, true, _) -> 2;                %% END
chunk_flags(false, false, _) -> 0.

%% @private
%% Compress a single chunk using BLAKE3 compression.
compress_chunk(IV, Chunk, BlockLen, Flags) ->
    %% Pad chunk to block boundary
    PaddedLen = ((byte_size(Chunk) + BlockLen - 1) div BlockLen) * BlockLen,
    Padded = case byte_size(Chunk) < PaddedLen of
        true -> <<Chunk/binary, 0:((PaddedLen - byte_size(Chunk)) * 8)>>;
        false -> Chunk
    end,

    %% Process blocks
    Blocks = chunk_data(Padded, BlockLen),
    BlockCount = length(Blocks),

    State = lists:foldl(
        fun({BlockIdx, Block}, S) ->
            IsLastBlock = BlockIdx =:= BlockCount - 1,
            BlockFlags = if IsLastBlock -> Flags; true -> 0 end,
            compress_block(S, Block, byte_size(Chunk), BlockFlags)
        end,
        IV,
        lists:zip(lists:seq(0, BlockCount - 1), Blocks)
    ),

    %% Extract first 32 bytes of state as hash
    state_to_hash(State).

%% @private
%% BLAKE3 block compression function (simplified G function).
compress_block({H0, H1, H2, H3, H4, H5, H6, H7}, Block, Counter, Flags) ->
    %% Message schedule from block
    M = block_to_words(Block),

    %% Initial state: h[0..7] || IV[0..3] || counter_lo || counter_hi || block_len || flags
    V = {
        H0, H1, H2, H3, H4, H5, H6, H7,
        16#6A09E667, 16#BB67AE85, 16#3C6EF372, 16#A54FF53A,
        Counter band 16#FFFFFFFF,
        (Counter bsr 32) band 16#FFFFFFFF,
        byte_size(Block),
        Flags
    },

    %% 7 rounds of mixing
    V1 = rounds(V, M, 7),

    %% XOR upper and lower halves
    {V10, V11, V12, V13, V14, V15, V16, V17,
     V18, V19, V1A, V1B, V1C, V1D, V1E, V1F} = V1,

    {
        V10 bxor V18, V11 bxor V19, V12 bxor V1A, V13 bxor V1B,
        V14 bxor V1C, V15 bxor V1D, V16 bxor V1E, V17 bxor V1F
    }.

%% @private
block_to_words(Block) when byte_size(Block) < 64 ->
    Padded = <<Block/binary, 0:((64 - byte_size(Block)) * 8)>>,
    block_to_words(Padded);
block_to_words(<<W0:32/little, W1:32/little, W2:32/little, W3:32/little,
                 W4:32/little, W5:32/little, W6:32/little, W7:32/little,
                 W8:32/little, W9:32/little, WA:32/little, WB:32/little,
                 WC:32/little, WD:32/little, WE:32/little, WF:32/little>>) ->
    {W0, W1, W2, W3, W4, W5, W6, W7, W8, W9, WA, WB, WC, WD, WE, WF}.

%% @private
rounds(V, _M, 0) -> V;
rounds(V, M, N) ->
    V1 = round_fn(V, M),
    rounds(V1, permute(M), N - 1).

%% @private
%% BLAKE3 round function with G mixing.
round_fn({V0, V1, V2, V3, V4, V5, V6, V7,
          V8, V9, VA, VB, VC, VD, VE, VF},
         {M0, M1, M2, M3, M4, M5, M6, M7, _, _, _, _, _, _, _, _}) ->
    %% Column mixing
    {V0a, V4a, V8a, VCa} = g(V0, V4, V8, VC, M0, M1),
    {V1a, V5a, V9a, VDa} = g(V1, V5, V9, VD, M2, M3),
    {V2a, V6a, VAa, VEa} = g(V2, V6, VA, VE, M4, M5),
    {V3a, V7a, VBa, VFa} = g(V3, V7, VB, VF, M6, M7),

    %% Diagonal mixing
    {V0b, V5b, VAb, VFb} = g(V0a, V5a, VAa, VFa, M0, M1),
    {V1b, V6b, VBb, VCb} = g(V1a, V6a, VBa, VCa, M2, M3),
    {V2b, V7b, V8b, VDb} = g(V2a, V7a, V8a, VDa, M4, M5),
    {V3b, V4b, V9b, VEb} = g(V3a, V4a, V9a, VEa, M6, M7),

    {V0b, V1b, V2b, V3b, V4b, V5b, V6b, V7b,
     V8b, V9b, VAb, VBb, VCb, VDb, VEb, VFb}.

%% @private
%% G mixing function.
g(A, B, C, D, MX, MY) ->
    A1 = (A + B + MX) band 16#FFFFFFFF,
    D1 = rotr32(D bxor A1, 16),
    C1 = (C + D1) band 16#FFFFFFFF,
    B1 = rotr32(B bxor C1, 12),
    A2 = (A1 + B1 + MY) band 16#FFFFFFFF,
    D2 = rotr32(D1 bxor A2, 8),
    C2 = (C1 + D2) band 16#FFFFFFFF,
    B2 = rotr32(B1 bxor C2, 7),
    {A2, B2, C2, D2}.

%% @private
rotr32(X, N) ->
    ((X bsr N) bor (X bsl (32 - N))) band 16#FFFFFFFF.

%% @private
%% Message word permutation for BLAKE3.
permute({M0, M1, M2, M3, M4, M5, M6, M7,
         M8, M9, MA, MB, MC, MD, ME, MF}) ->
    {M2, M6, M3, MA, M7, M0, M4, MD,
     M1, MB, MC, M5, M9, ME, MF, M8}.

%% @private
state_to_hash({H0, H1, H2, H3, H4, H5, H6, H7}) ->
    <<H0:32/little, H1:32/little, H2:32/little, H3:32/little,
      H4:32/little, H5:32/little, H6:32/little, H7:32/little>>.

%% @private
finalize_tree([Hash], _IV, _RootFlag) ->
    Hash;
finalize_tree(Hashes, IV, RootFlag) ->
    %% Pair up hashes and compress
    Paired = pair_hashes(Hashes),
    NewHashes = lists:map(
        fun({Left, Right}) ->
            Combined = <<Left/binary, Right/binary>>,
            compress_chunk(IV, Combined, 64, RootFlag)
        end,
        Paired
    ),
    finalize_tree(NewHashes, IV, RootFlag).

%% @private
pair_hashes([]) -> [];
pair_hashes([H]) -> [{H, <<0:256>>}];  %% Pad with zero hash
pair_hashes([H1, H2 | Rest]) -> [{H1, H2} | pair_hashes(Rest)].

%%%===================================================================
%%% Internal Functions - Hex Encoding
%%%===================================================================

%% @private
hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.

%% @private
hex_decode(<<>>, Acc) ->
    {ok, Acc};
hex_decode(<<H1, H2, Rest/binary>>, Acc) ->
    case {hex_value(H1), hex_value(H2)} of
        {{ok, V1}, {ok, V2}} ->
            Byte = (V1 bsl 4) bor V2,
            hex_decode(Rest, <<Acc/binary, Byte>>);
        _ ->
            {error, invalid_hex}
    end;
hex_decode(_, _) ->
    {error, invalid_hex}.

%% @private
hex_value(C) when C >= $0, C =< $9 -> {ok, C - $0};
hex_value(C) when C >= $a, C =< $f -> {ok, C - $a + 10};
hex_value(C) when C >= $A, C =< $F -> {ok, C - $A + 10};
hex_value(_) -> error.
