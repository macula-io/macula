%% @doc Fixed-size chunking, Merkle-root computation, and manifest
%% construction for content larger than one storage block.
%%
%% Mirrors macula-station's `macula_manifest' /
%% `macula_content_chunker' / `macula_content_hasher' BYTE-FOR-BYTE:
%% same MCID format, same default chunk size (256 KiB), same Merkle
%% fold, same canonical-CBOR MCID derivation, same manifest wire shape.
%% This is deliberate, not incidental — the SDK puts a manifest via the
%% station's existing (unmodified) `_content.put_manifest' /
%% `_content.get_manifest' RPCs, so the two sides must agree on the
%% algorithm bit-for-bit. Both use the same BLAKE3 NIF
%% (`macula_blake3_nif', SDK-owned; the station calls it too) and the
%% same deterministic CBOR encoder (`macula_record_cbor', SDK-owned;
%% the station's manifest module calls it directly), so this is a
%% faithful client-side port, not a re-derivation.
%%
%% MCID format (34 bytes): `<<Version:8, Codec:8, Hash:32/binary>>'.
%% `?CODEC_RAW' (16#55) addresses a single chunk (or a whole blob that
%% fits in one chunk — see the module doc on `macula:put_content/2').
%% `?CODEC_MANIFEST' (16#56) addresses a manifest describing many
%% chunks.
-module(macula_manifest).

-export([
    default_chunk_size/0,
    create/1, create/2,
    chunk_mcid/3,
    verify/2,
    from_wire/1
]).

-export_type([manifest/0, chunk_info/0, algorithm/0]).

-type algorithm() :: blake3 | sha256.
-type mcid() :: <<_:272>>.

-type chunk_info() :: #{
    index  := non_neg_integer(),
    offset := non_neg_integer(),
    size   := pos_integer(),
    hash   := binary()
}.

-type manifest() :: #{
    mcid           := mcid(),
    version        := pos_integer(),
    name           := binary(),
    size           := non_neg_integer(),
    created        := non_neg_integer(),
    chunk_size     := pos_integer(),
    chunk_count    := non_neg_integer(),
    hash_algorithm := algorithm(),
    root_hash      := binary(),
    chunks         := [chunk_info()]
}.

-define(VERSION,        1).
-define(CODEC_RAW,      16#55).
-define(CODEC_MANIFEST, 16#56).
-define(DEFAULT_CHUNK_SIZE, 262144).

%%====================================================================
%% API
%%====================================================================

-spec default_chunk_size() -> pos_integer().
default_chunk_size() -> ?DEFAULT_CHUNK_SIZE.

-spec create(binary()) -> {ok, manifest(), [binary()]}.
create(Data) -> create(Data, #{}).

%% @doc Split `Data' into fixed-size chunks and build its manifest.
%% Returns the manifest and the chunk bytes in order (index 0 first),
%% so a caller can upload each chunk (via `_content.put_block') and
%% then the manifest (via `_content.put_manifest'). Options:
%% <ul>
%%   <li>`name' — content name (default `<<"unnamed">>')</li>
%%   <li>`chunk_size' — bytes per chunk (default `default_chunk_size/0')</li>
%%   <li>`hash_algorithm' — `blake3' | `sha256' (default `blake3')</li>
%% </ul>
-spec create(binary(), map()) -> {ok, manifest(), [binary()]}.
create(Data, Opts) when is_binary(Data), is_map(Opts) ->
    Name      = maps:get(name, Opts, <<"unnamed">>),
    ChunkSize = maps:get(chunk_size, Opts, ?DEFAULT_CHUNK_SIZE),
    Algorithm = maps:get(hash_algorithm, Opts, blake3),

    Chunks     = do_chunk(Data, ChunkSize, []),
    ChunkInfos = chunk_infos(Chunks, Algorithm),
    RootHash   = root_hash_for(ChunkInfos, Algorithm),

    Body = #{
        version        => ?VERSION,
        name           => Name,
        size           => byte_size(Data),
        created        => erlang:system_time(second),
        chunk_size     => ChunkSize,
        chunk_count    => length(ChunkInfos),
        hash_algorithm => Algorithm,
        root_hash      => RootHash,
        chunks         => ChunkInfos
    },
    {ok, Body#{mcid => compute_mcid(Body, Algorithm)}, Chunks}.

%% @doc The MCID a chunk at `Index' is stored/fetched under. The
%% station derives this same value independently when serving the
%% chunk, so both sides agree on its address without exchanging it.
%%
%% `Algorithm' is accepted but unused: the chunk's hash was already
%% computed (with whichever algorithm `create/2' was given) and stored
%% in `Chunks', so there's nothing left to derive here. KNOWN GAP
%% (2026-09-05, will not be fixed without a design decision): per-chunk
%% fetch verification (`macula_content_transfer:verify_block_hash/2') is
%% hardcoded to blake3 regardless of this field, so a manifest created
%% with `hash_algorithm => sha256' produces chunk MCIDs that can never
%% actually verify on fetch. `hash_algorithm' today only affects the
%% manifest's own root-hash Merkle step (`verify/2'). Same gap
%% independently confirmed in macula-io/macula-rust's `content.rs'
%% (`block_mcid' is likewise hardcoded to blake3). Left as-is: nothing
%% exercises sha256 in practice, and whether per-chunk sha256
%% verification was ever meant to work is an open question, not a bug
%% with an obvious fix.
-spec chunk_mcid(manifest(), non_neg_integer(), algorithm()) ->
        {ok, mcid()} | {error, invalid_index}.
chunk_mcid(#{chunks := Chunks}, Index, _Algorithm)
  when Index >= 0, Index < length(Chunks) ->
    #{hash := Hash} = lists:nth(Index + 1, Chunks),
    {ok, make_mcid(?CODEC_RAW, Hash)};
chunk_mcid(#{chunks := _}, _Index, _Algorithm) ->
    {error, invalid_index}.

%% @doc Verify reassembled `Data' against `Manifest': size, then a
%% fresh Merkle root over `Data' re-chunked the same way.
-spec verify(manifest(), binary()) ->
        ok | {error, size_mismatch | root_hash_mismatch}.
verify(#{size := ExpectedSize} = Manifest, Data) when is_binary(Data) ->
    verify_size(byte_size(Data) =:= ExpectedSize, Manifest, Data).

verify_size(false, _Manifest, _Data) ->
    {error, size_mismatch};
verify_size(true, #{chunk_size := CS, hash_algorithm := Alg,
                    root_hash := Expected}, Data) ->
    Chunks = do_chunk(Data, CS, []),
    Actual = root_hash_for(chunk_infos(Chunks, Alg), Alg),
    root_hash_result(Actual =:= Expected).

root_hash_result(true)  -> ok;
root_hash_result(false) -> {error, root_hash_mismatch}.

%% @doc Read a manifest as it arrives over `_content.get_manifest': the
%% station stores + returns the map exactly as its RPC layer decoded
%% it, with no dedicated re-encode/decode round trip on either side
%% — so the shape depends on the general CALL-result codec, not the
%% canonical `{text,_}' record shape. Robust to atom
%% keys (the happy path — the field names are atoms defined in this
%% module, so `binary_to_existing_atom' in the frame decoder resolves
%% them) and to binary-string keys (a defensive fallback, mirroring
%% `macula_record:payload_field/2').
-spec from_wire(map()) -> {ok, manifest()} | {error, invalid_manifest}.
from_wire(M) when is_map(M) ->
    from_wire_result(field(M, mcid), field(M, chunks), M).

from_wire_result(undefined, _Chunks, _M) ->
    {error, invalid_manifest};
from_wire_result(_Mcid, undefined, _M) ->
    {error, invalid_manifest};
from_wire_result(MCID, Chunks, M) when is_list(Chunks) ->
    {ok, #{mcid           => MCID,
           version        => field_default(M, version, 1),
           name           => field_default(M, name, <<"unnamed">>),
           size           => field_default(M, size, 0),
           created        => field_default(M, created, 0),
           chunk_size     => field_default(M, chunk_size, ?DEFAULT_CHUNK_SIZE),
           chunk_count    => field_default(M, chunk_count, 0),
           hash_algorithm => to_algorithm(field_default(M, hash_algorithm, blake3)),
           root_hash      => field_default(M, root_hash, <<>>),
           chunks         => [chunk_info_from_wire(C) || C <- Chunks]}}.

chunk_info_from_wire(C) when is_map(C) ->
    #{index  => field_default(C, index, 0),
      offset => field_default(C, offset, 0),
      size   => field_default(C, size, 0),
      hash   => field_default(C, hash, <<>>)}.

to_algorithm(<<"blake3">>) -> blake3;
to_algorithm(<<"sha256">>) -> sha256;
to_algorithm(blake3)       -> blake3;
to_algorithm(sha256)       -> sha256;
to_algorithm(_)            -> blake3.

%% A field, robust to atom keys (the happy path) or binary-string keys
%% (defensive fallback).
field(M, Key) when is_atom(Key) ->
    field_try([Key, atom_to_binary(Key)], M).

field_default(M, Key, Default) ->
    field_or_default(field(M, Key), Default).

field_or_default(undefined, Default) -> Default;
field_or_default(Value, _Default)    -> Value.

field_try([K | Ks], M) ->
    field_try_step(maps:find(K, M), Ks, M);
field_try([], _M) ->
    undefined.

field_try_step({ok, V}, _Ks, _M) -> V;
field_try_step(error, Ks, M)     -> field_try(Ks, M).

%%====================================================================
%% Internal — chunking (mirrors macula_content_chunker:chunk/2)
%%====================================================================

do_chunk(<<>>, _ChunkSize, []) ->
    [];
do_chunk(Data, ChunkSize, Acc) when byte_size(Data) =< ChunkSize ->
    lists:reverse([Data | Acc]);
do_chunk(Data, ChunkSize, Acc) ->
    <<Chunk:ChunkSize/binary, Rest/binary>> = Data,
    do_chunk(Rest, ChunkSize, [Chunk | Acc]).

chunk_infos(Chunks, Algorithm) ->
    {Infos, _} = lists:foldl(
        fun(C, {Acc, {Idx, Off}}) ->
            Sz = byte_size(C),
            H  = hash(Algorithm, C),
            {[#{index => Idx, offset => Off, size => Sz, hash => H} | Acc],
             {Idx + 1, Off + Sz}}
        end, {[], {0, 0}}, Chunks),
    lists:reverse(Infos).

%%====================================================================
%% Internal — Merkle root (mirrors macula_content_chunker:merkle_root/2)
%%====================================================================

root_hash_for([], Algorithm) ->
    hash(Algorithm, <<>>);
root_hash_for(Infos, Algorithm) ->
    Hashes = [maps:get(hash, I) || I <- Infos],
    fold_pairs(Hashes, Algorithm).

fold_pairs([H], _Algorithm) ->
    H;
fold_pairs(Hashes, Algorithm) ->
    fold_pairs(combine(Hashes, Algorithm, []), Algorithm).

combine([], _Algorithm, Acc) ->
    lists:reverse(Acc);
combine([Last], Algorithm, Acc) ->
    %% Odd count — pair the last hash with itself (V1 convention).
    H = hash(Algorithm, <<Last/binary, Last/binary>>),
    lists:reverse([H | Acc]);
combine([L, R | Rest], Algorithm, Acc) ->
    H = hash(Algorithm, <<L/binary, R/binary>>),
    combine(Rest, Algorithm, [H | Acc]).

%%====================================================================
%% Internal — hashing (mirrors macula_content_hasher:hash/2)
%%====================================================================

hash(blake3, Data) -> macula_blake3_nif:hash(Data);
hash(sha256, Data) -> crypto:hash(sha256, Data).

%%====================================================================
%% Internal — MCID (mirrors macula_manifest:compute_mcid/2)
%%====================================================================

compute_mcid(Body, Algorithm) ->
    %% Deterministic canonical encoding — excludes `created' (timestamp)
    %% and `chunks' (already rolled up into root_hash), matching the
    %% station's exact field set and key order.
    Canonical = #{
        {text, <<"name">>}           => {text, maps:get(name, Body)},
        {text, <<"size">>}           => maps:get(size, Body),
        {text, <<"chunk_size">>}     => maps:get(chunk_size, Body),
        {text, <<"chunk_count">>}    => maps:get(chunk_count, Body),
        {text, <<"hash_algorithm">>} => {text, atom_to_binary(Algorithm)},
        {text, <<"root_hash">>}      => maps:get(root_hash, Body)
    },
    Bytes = macula_cbor_nif:pack_deterministic(Canonical),
    make_mcid(?CODEC_MANIFEST, hash(Algorithm, Bytes)).

make_mcid(Codec, Hash) ->
    <<?VERSION:8, Codec:8, Hash/binary>>.
