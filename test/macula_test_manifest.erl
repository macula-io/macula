%% Test support for manifest checks: a manifest made to match the MCID it names, the way any peer can make one. The
%% MCID is recomputed from the manifest's own canonical fields and nothing else, so a peer who writes those fields
%% also gets a matching MCID for them.
-module(macula_test_manifest).

-export([with_matching_mcid/1, with_matching_mcid/2]).

-define(MCID_VERSION, 1).
-define(MANIFEST_CODEC, 16#56).

%% @doc Manifest with its mcid recomputed from its canonical fields (name, size, chunk_size, chunk_count, blake3 and
%% root_hash), whatever those fields now say.
-spec with_matching_mcid(map()) -> map().
with_matching_mcid(Manifest) ->
    with_matching_mcid(Manifest, blake3).

%% @doc Manifest with its mcid recomputed from its canonical fields the way a manifest hashed with Algorithm has it
%% computed: the algorithm's name among the fields, and the algorithm over their encoding.
-spec with_matching_mcid(map(), blake3 | sha256) -> map().
with_matching_mcid(#{name := Name, size := Size, chunk_size := ChunkSize, chunk_count := ChunkCount,
                     root_hash := RootHash} = Manifest, Algorithm) ->
    Canonical = #{{text, <<"name">>}           => {text, Name},
                  {text, <<"size">>}           => Size,
                  {text, <<"chunk_size">>}     => ChunkSize,
                  {text, <<"chunk_count">>}    => ChunkCount,
                  {text, <<"hash_algorithm">>} => {text, atom_to_binary(Algorithm)},
                  {text, <<"root_hash">>}      => RootHash},
    Hash = hash(Algorithm, macula_cbor_nif:pack_deterministic(Canonical)),
    Manifest#{mcid := <<?MCID_VERSION, ?MANIFEST_CODEC, Hash/binary>>}.

hash(blake3, Bytes) -> macula_blake3_nif:hash(Bytes);
hash(sha256, Bytes) -> crypto:hash(sha256, Bytes).
