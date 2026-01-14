%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_content_store module.
%%% Tests block storage, manifest storage, and maintenance operations.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_store_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    %% Create temp directory for tests
    TempDir = "/tmp/macula_content_store_test_" ++
              integer_to_list(erlang:system_time(microsecond)),
    ok = filelib:ensure_dir(TempDir ++ "/"),
    application:set_env(macula, content_store_dir, TempDir),
    {ok, Pid} = macula_content_store:start_link(#{base_dir => TempDir}),
    {Pid, TempDir}.

cleanup({Pid, TempDir}) ->
    gen_server:stop(Pid),
    %% Clean up temp directory
    os:cmd("rm -rf " ++ TempDir),
    ok.

%%%===================================================================
%%% Block Operations Tests
%%%===================================================================

block_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun put_and_get_block/1,
      fun get_nonexistent_block/1,
      fun has_block_true/1,
      fun has_block_false/1,
      fun delete_block/1,
      fun block_verification_on_get/1,
      fun put_block_duplicate/1
     ]}.

put_and_get_block({_Pid, _Dir}) ->
    fun() ->
        Data = <<"test block data">>,
        MCID = compute_mcid(Data),
        ?assertEqual(ok, macula_content_store:put_block(MCID, Data)),
        ?assertEqual({ok, Data}, macula_content_store:get_block(MCID))
    end.

get_nonexistent_block({_Pid, _Dir}) ->
    fun() ->
        FakeMCID = <<1, 16#55, 0:256>>,
        ?assertEqual({error, not_found}, macula_content_store:get_block(FakeMCID))
    end.

has_block_true({_Pid, _Dir}) ->
    fun() ->
        Data = <<"block exists test">>,
        MCID = compute_mcid(Data),
        ok = macula_content_store:put_block(MCID, Data),
        ?assert(macula_content_store:has_block(MCID))
    end.

has_block_false({_Pid, _Dir}) ->
    fun() ->
        FakeMCID = <<1, 16#55, 0:256>>,
        ?assertNot(macula_content_store:has_block(FakeMCID))
    end.

delete_block({_Pid, _Dir}) ->
    fun() ->
        Data = <<"delete test data">>,
        MCID = compute_mcid(Data),
        ok = macula_content_store:put_block(MCID, Data),
        ?assert(macula_content_store:has_block(MCID)),
        ?assertEqual(ok, macula_content_store:delete_block(MCID)),
        ?assertNot(macula_content_store:has_block(MCID))
    end.

block_verification_on_get({_Pid, _Dir}) ->
    fun() ->
        %% Store a block, then corrupt the file
        Data = <<"original data">>,
        MCID = compute_mcid(Data),
        ok = macula_content_store:put_block(MCID, Data),
        %% Corrupt the file by writing different data directly
        Path = macula_content_store:block_path(MCID),
        ok = file:write_file(Path, <<"corrupted">>),
        %% Get should detect corruption
        ?assertEqual({error, hash_mismatch}, macula_content_store:get_block(MCID))
    end.

put_block_duplicate({_Pid, _Dir}) ->
    fun() ->
        Data = <<"duplicate test">>,
        MCID = compute_mcid(Data),
        ok = macula_content_store:put_block(MCID, Data),
        %% Putting same block again should succeed (idempotent)
        ?assertEqual(ok, macula_content_store:put_block(MCID, Data))
    end.

%%%===================================================================
%%% Manifest Operations Tests
%%%===================================================================

manifest_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun put_and_get_manifest/1,
      fun get_nonexistent_manifest/1,
      fun list_manifests_empty/1,
      fun list_manifests_multiple/1,
      fun delete_manifest/1,
      fun manifest_persists_metadata/1
     ]}.

put_and_get_manifest({_Pid, _Dir}) ->
    fun() ->
        Data = crypto:strong_rand_bytes(5000),
        {ok, Manifest} = macula_content_manifest:create(Data, #{
            name => <<"test.tar.gz">>,
            chunk_size => 1024
        }),
        MCID = maps:get(mcid, Manifest),
        ok = macula_content_store:put_manifest(Manifest),
        {ok, Retrieved} = macula_content_store:get_manifest(MCID),
        ?assertEqual(MCID, maps:get(mcid, Retrieved)),
        ?assertEqual(maps:get(name, Manifest), maps:get(name, Retrieved)),
        ?assertEqual(maps:get(size, Manifest), maps:get(size, Retrieved))
    end.

get_nonexistent_manifest({_Pid, _Dir}) ->
    fun() ->
        FakeMCID = <<1, 16#56, 0:256>>,
        ?assertEqual({error, not_found}, macula_content_store:get_manifest(FakeMCID))
    end.

list_manifests_empty({_Pid, _Dir}) ->
    fun() ->
        ?assertEqual([], macula_content_store:list_manifests())
    end.

list_manifests_multiple({_Pid, _Dir}) ->
    fun() ->
        Data1 = <<"data1">>,
        Data2 = <<"data2">>,
        {ok, M1} = macula_content_manifest:create(Data1, #{name => <<"file1">>}),
        {ok, M2} = macula_content_manifest:create(Data2, #{name => <<"file2">>}),
        ok = macula_content_store:put_manifest(M1),
        ok = macula_content_store:put_manifest(M2),
        List = macula_content_store:list_manifests(),
        ?assertEqual(2, length(List)),
        ?assert(lists:member(maps:get(mcid, M1), List)),
        ?assert(lists:member(maps:get(mcid, M2), List))
    end.

delete_manifest({_Pid, _Dir}) ->
    fun() ->
        Data = <<"manifest delete test">>,
        {ok, Manifest} = macula_content_manifest:create(Data, #{}),
        MCID = maps:get(mcid, Manifest),
        ok = macula_content_store:put_manifest(Manifest),
        ?assertMatch({ok, _}, macula_content_store:get_manifest(MCID)),
        ok = macula_content_store:delete_manifest(MCID),
        ?assertEqual({error, not_found}, macula_content_store:get_manifest(MCID))
    end.

manifest_persists_metadata({_Pid, _Dir}) ->
    fun() ->
        Data = crypto:strong_rand_bytes(3000),
        {ok, Manifest} = macula_content_manifest:create(Data, #{
            name => <<"my_app-1.0.0.tar.gz">>,
            chunk_size => 512,
            hash_algorithm => sha256
        }),
        MCID = maps:get(mcid, Manifest),
        ok = macula_content_store:put_manifest(Manifest),
        {ok, Retrieved} = macula_content_store:get_manifest(MCID),
        ?assertEqual(<<"my_app-1.0.0.tar.gz">>, maps:get(name, Retrieved)),
        ?assertEqual(512, maps:get(chunk_size, Retrieved)),
        ?assertEqual(sha256, maps:get(hash_algorithm, Retrieved))
    end.

%%%===================================================================
%%% Maintenance Tests
%%%===================================================================

maintenance_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun stats_empty/1,
      fun stats_with_blocks/1,
      fun stats_with_manifests/1,
      fun verify_integrity_clean/1,
      fun verify_integrity_corrupted/1,
      fun gc_orphaned_blocks/1
     ]}.

stats_empty({_Pid, _Dir}) ->
    fun() ->
        Stats = macula_content_store:stats(),
        ?assertEqual(0, maps:get(block_count, Stats)),
        ?assertEqual(0, maps:get(manifest_count, Stats)),
        ?assertEqual(0, maps:get(total_size, Stats))
    end.

stats_with_blocks({_Pid, _Dir}) ->
    fun() ->
        Data1 = <<"block one">>,
        Data2 = <<"block two is longer">>,
        ok = macula_content_store:put_block(compute_mcid(Data1), Data1),
        ok = macula_content_store:put_block(compute_mcid(Data2), Data2),
        Stats = macula_content_store:stats(),
        ?assertEqual(2, maps:get(block_count, Stats)),
        ?assertEqual(byte_size(Data1) + byte_size(Data2), maps:get(total_size, Stats))
    end.

stats_with_manifests({_Pid, _Dir}) ->
    fun() ->
        Data = <<"test manifest">>,
        {ok, Manifest} = macula_content_manifest:create(Data, #{}),
        ok = macula_content_store:put_manifest(Manifest),
        Stats = macula_content_store:stats(),
        ?assertEqual(1, maps:get(manifest_count, Stats))
    end.

verify_integrity_clean({_Pid, _Dir}) ->
    fun() ->
        Data1 = <<"integrity test 1">>,
        Data2 = <<"integrity test 2">>,
        ok = macula_content_store:put_block(compute_mcid(Data1), Data1),
        ok = macula_content_store:put_block(compute_mcid(Data2), Data2),
        ?assertEqual({ok, 2}, macula_content_store:verify_integrity())
    end.

verify_integrity_corrupted({_Pid, _Dir}) ->
    fun() ->
        Data = <<"original integrity">>,
        MCID = compute_mcid(Data),
        ok = macula_content_store:put_block(MCID, Data),
        %% Corrupt the file
        Path = macula_content_store:block_path(MCID),
        ok = file:write_file(Path, <<"corrupted!">>),
        {error, {corrupted, [MCID]}} = macula_content_store:verify_integrity()
    end.

gc_orphaned_blocks({_Pid, _Dir}) ->
    fun() ->
        %% Create blocks that are referenced by a manifest
        Data = crypto:strong_rand_bytes(3000),
        {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
        Chunks = maps:get(chunks, Manifest),
        %% Store manifest and its chunks
        ok = macula_content_store:put_manifest(Manifest),
        {ok, ChunkBins} = macula_content_chunker:chunk(Data, 1024),
        lists:foreach(fun({ChunkInfo, ChunkData}) ->
            ChunkMCID = <<1, 16#55, (maps:get(hash, ChunkInfo))/binary>>,
            macula_content_store:put_block(ChunkMCID, ChunkData)
        end, lists:zip(Chunks, ChunkBins)),
        %% Add an orphaned block (not referenced by any manifest)
        OrphanData = <<"orphan block">>,
        OrphanMCID = compute_mcid(OrphanData),
        ok = macula_content_store:put_block(OrphanMCID, OrphanData),
        %% Run GC
        {ok, #{removed := 1}} = macula_content_store:gc(),
        %% Orphan should be gone, referenced blocks remain
        ?assertNot(macula_content_store:has_block(OrphanMCID)),
        lists:foreach(fun(ChunkInfo) ->
            ChunkMCID = <<1, 16#55, (maps:get(hash, ChunkInfo))/binary>>,
            ?assert(macula_content_store:has_block(ChunkMCID))
        end, Chunks)
    end.

%%%===================================================================
%%% Storage Path Tests
%%%===================================================================

path_test_() ->
    [
     {"block path uses sharding", fun block_path_sharding/0},
     {"manifest path uses sharding", fun manifest_path_sharding/0}
    ].

block_path_sharding() ->
    %% Hash starting with "5d" should go to blocks/5d/ directory
    Hash = <<16#5d, 16#41, 16#40, 0:232>>,
    MCID = <<1, 16#55, Hash/binary>>,
    Path = macula_content_store:block_path(MCID),
    ?assert(binary:match(list_to_binary(Path), <<"blocks/5d/">>) =/= nomatch).

manifest_path_sharding() ->
    %% Hash starting with "a1" should go to manifests/a1/ directory
    Hash = <<16#a1, 16#b2, 16#c3, 0:232>>,
    MCID = <<1, 16#56, Hash/binary>>,
    Path = macula_content_store:manifest_path(MCID),
    ?assert(binary:match(list_to_binary(Path), <<"manifests/a1/">>) =/= nomatch).

%%%===================================================================
%%% Store Content Tests (combined block + manifest)
%%%===================================================================

store_content_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun store_and_retrieve_full_content/1,
      fun store_large_content/1
     ]}.

store_and_retrieve_full_content({_Pid, _Dir}) ->
    fun() ->
        %% Create content, store all chunks and manifest
        Data = crypto:strong_rand_bytes(5000),
        {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 1024}),
        %% Store manifest
        ok = macula_content_store:put_manifest(Manifest),
        %% Store all chunks
        {ok, ChunkBins} = macula_content_chunker:chunk(Data, 1024),
        lists:foreach(fun({ChunkInfo, ChunkData}) ->
            ChunkMCID = <<1, 16#55, (maps:get(hash, ChunkInfo))/binary>>,
            macula_content_store:put_block(ChunkMCID, ChunkData)
        end, lists:zip(maps:get(chunks, Manifest), ChunkBins)),
        %% Retrieve all chunks and reassemble
        Retrieved = lists:map(fun(ChunkInfo) ->
            ChunkMCID = <<1, 16#55, (maps:get(hash, ChunkInfo))/binary>>,
            {ok, ChunkData} = macula_content_store:get_block(ChunkMCID),
            ChunkData
        end, maps:get(chunks, Manifest)),
        Reassembled = macula_content_chunker:reassemble(Retrieved),
        ?assertEqual(Data, Reassembled)
    end.

store_large_content({_Pid, _Dir}) ->
    fun() ->
        %% Test with 1MB content
        Data = crypto:strong_rand_bytes(1024 * 1024),
        {ok, Manifest} = macula_content_manifest:create(Data, #{chunk_size => 262144}),
        ?assertEqual(4, maps:get(chunk_count, Manifest)),
        ok = macula_content_store:put_manifest(Manifest),
        ?assertMatch({ok, _}, macula_content_store:get_manifest(maps:get(mcid, Manifest)))
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

compute_mcid(Data) ->
    Hash = macula_content_hasher:hash(blake3, Data),
    <<1, 16#55, Hash/binary>>.
