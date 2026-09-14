%% Test support for manifest checks: a manifest made to match the MCID it names, the way any peer can make one. The
%% MCID is recomputed from the manifest's own canonical fields and nothing else, so a peer who writes those fields
%% also gets a matching MCID for them.
-module(macula_test_manifest).

-export([with_matching_mcid/1]).

-define(MCID_VERSION, 1).
-define(MANIFEST_CODEC, 16#56).

%% @doc Manifest with its mcid recomputed from its canonical fields (name, size, chunk_size, chunk_count, blake3 and
%% root_hash), whatever those fields now say.
-spec with_matching_mcid(map()) -> map().
with_matching_mcid(#{name := Name, size := Size, chunk_size := ChunkSize, chunk_count := ChunkCount,
                     root_hash := RootHash} = Manifest) ->
    Canonical = #{{text, <<"name">>}           => {text, Name},
                  {text, <<"size">>}           => Size,
                  {text, <<"chunk_size">>}     => ChunkSize,
                  {text, <<"chunk_count">>}    => ChunkCount,
                  {text, <<"hash_algorithm">>} => {text, <<"blake3">>},
                  {text, <<"root_hash">>}      => RootHash},
    Hash = macula_blake3_nif:hash(macula_cbor_nif:pack_deterministic(Canonical)),
    Manifest#{mcid := <<?MCID_VERSION, ?MANIFEST_CODEC, Hash/binary>>}.
