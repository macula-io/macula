%% @doc The content a node shares, held by the node itself (D27: a station keeps no content; the sharing node keeps it
%% and serves it). A pure value: the owner keeps it and threads it through.
%%
%% Bytes of at most one chunk (`macula_manifest:default_chunk_size/0', 256 KiB) are one raw block, under the content id
%% `<<2, 16#55, SHA-384(Bytes)>>'. Larger bytes are chunked by `macula_manifest:create/2', and the manifest's content id
%% is the root. A fetcher asks for a root (`root/2') and then for each chunk of a manifest (`chunk/2'); a raw root is
%% not a chunk, so nothing is served but what a root names. A chunk two roots share is counted, and kept until neither
%% needs it.
-module(macula_content_store).

-export([new/0, added/3, root/2, chunk/2, removed/2, roots/1]).

-export_type([store/0]).

-define(TAG_SHA384, 2).
-define(CODEC_RAW, 16#55).

-record(store, {
    roots = #{}  :: #{macula:mcid() => {block, binary()} | {manifest, macula_manifest:manifest()}},
    chunks = #{} :: #{macula:mcid() => {binary(), pos_integer()}}
}).

-opaque store() :: #store{}.

%% @doc An empty store.
-spec new() -> store().
new() ->
    #store{}.

%% @doc The store with `Bytes' added, and their root content id. `Opts' may name the content (`name'), which a
%% manifest carries; bytes already held under the same root are not added twice.
-spec added(binary(), map(), store()) -> {macula:mcid(), store()}.
added(Bytes, Opts, Store) when is_binary(Bytes), is_map(Opts) ->
    root_added(byte_size(Bytes) =< macula_manifest:default_chunk_size(), Bytes, Opts, Store).

root_added(true, Bytes, _Opts, #store{roots = Roots} = S) ->
    MCID = raw_mcid(Bytes),
    {MCID, S#store{roots = Roots#{MCID => {block, Bytes}}}};
root_added(false, Bytes, Opts, S) ->
    {ok, #{mcid := MCID} = Manifest, Chunks} = macula_manifest:create(Bytes, maps:with([name], Opts)),
    {MCID, manifest_added(is_map_key(MCID, S#store.roots), MCID, Manifest, Chunks, S)}.

manifest_added(true, _MCID, _Manifest, _Chunks, S) ->
    S;
manifest_added(false, MCID, Manifest, Chunks, #store{roots = Roots, chunks = Held} = S) ->
    S#store{roots = Roots#{MCID => {manifest, Manifest}},
            chunks = lists:foldl(fun chunk_counted/2, Held, [{raw_mcid(C), C} || C <- Chunks])}.

chunk_counted({ChunkMCID, Bytes}, Held) ->
    maps:update_with(ChunkMCID, fun({B, N}) -> {B, N + 1} end, {Bytes, 1}, Held).

%% @doc A root the store holds: a raw block's bytes, or a manifest.
-spec root(macula:mcid(), store()) -> {block, binary()} | {manifest, macula_manifest:manifest()} | not_found.
root(MCID, #store{roots = Roots}) ->
    maps:get(MCID, Roots, not_found).

%% @doc A chunk of a held manifest.
-spec chunk(macula:mcid(), store()) -> {ok, binary()} | not_found.
chunk(MCID, #store{chunks = Held}) ->
    chunk_found(maps:find(MCID, Held)).

chunk_found({ok, {Bytes, _Count}}) -> {ok, Bytes};
chunk_found(error) -> not_found.

%% @doc The store without the root `MCID', and without the chunks no other root needs.
-spec removed(macula:mcid(), store()) -> store().
removed(MCID, #store{roots = Roots} = S) ->
    root_removed(maps:take(MCID, Roots), S).

root_removed({{block, _Bytes}, Roots}, S) ->
    S#store{roots = Roots};
root_removed({{manifest, Manifest}, Roots}, #store{chunks = Held} = S) ->
    S#store{roots = Roots, chunks = lists:foldl(fun chunk_released/2, Held, chunk_mcids(Manifest))};
root_removed(error, S) ->
    S.

chunk_released(ChunkMCID, Held) ->
    released_count(maps:find(ChunkMCID, Held), ChunkMCID, Held).

released_count({ok, {_Bytes, 1}}, ChunkMCID, Held) -> maps:remove(ChunkMCID, Held);
released_count({ok, {Bytes, N}}, ChunkMCID, Held) -> Held#{ChunkMCID => {Bytes, N - 1}};
released_count(error, _ChunkMCID, Held) -> Held.

%% @doc The roots the store holds.
-spec roots(store()) -> [macula:mcid()].
roots(#store{roots = Roots}) ->
    maps:keys(Roots).

chunk_mcids(#{chunk_count := Count} = Manifest) ->
    [begin {ok, C} = macula_manifest:chunk_mcid(Manifest, I), C end || I <- lists:seq(0, Count - 1)].

raw_mcid(Bytes) ->
    <<?TAG_SHA384, ?CODEC_RAW, (crypto:hash(sha384, Bytes))/binary>>.
