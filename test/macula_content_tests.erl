%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_content public API facade.
%%%
%%% Tests the high-level content operations:
%%% - Publishing content
%%% - Fetching content
%%% - Manifest operations
%%% - Provider discovery
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Descriptions
%%%===================================================================

macula_content_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% MCID operations
      {"MCID from binary creates valid identifier", fun mcid_from_binary/0},
      {"MCID to string produces readable format", fun mcid_to_string/0},
      {"MCID from string parses correctly", fun mcid_from_string/0},
      {"MCID roundtrip preserves data", fun mcid_roundtrip/0},

      %% Manifest operations
      {"Create manifest from binary", fun create_manifest_from_binary/0},
      {"Create manifest with options", fun create_manifest_with_options/0},
      {"Verify manifest against original data", fun verify_manifest/0},

      %% Chunk operations
      {"Get chunk info from manifest", fun get_chunk_info/0},
      {"List missing chunks", fun list_missing_chunks/0},

      %% Status and stats
      {"Get content system status", fun get_status/0},
      {"Check if content is local", fun is_local_check/0}
     ]}.

%%%===================================================================
%%% Setup and Cleanup
%%%===================================================================

setup() ->
    %% Create temp directory for tests
    TempDir = create_temp_dir(),
    {ok, _} = macula_content_store:start_link(#{store_path => TempDir}),
    TempDir.

cleanup(TempDir) ->
    macula_content_store:stop(),
    cleanup_temp_dir(TempDir),
    ok.

create_temp_dir() ->
    Rand = integer_to_list(erlang:unique_integer([positive])),
    Dir = filename:join(["/tmp", "macula_content_test_" ++ Rand]),
    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
    Dir.

cleanup_temp_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            os:cmd("rm -rf " ++ Dir);
        false ->
            ok
    end.

%%%===================================================================
%%% Test Helpers
%%%===================================================================

test_binary() ->
    <<"Hello, Macula Content System! This is test data for publishing.">>.

test_binary_large() ->
    %% 1MB of data
    binary:copy(<<"X">>, 1024 * 1024).

%%%===================================================================
%%% MCID Operation Tests
%%%===================================================================

mcid_from_binary() ->
    Data = test_binary(),
    MCID = macula_content:mcid(Data),
    ?assertEqual(34, byte_size(MCID)),
    %% Version byte
    <<Version:8, _/binary>> = MCID,
    ?assertEqual(1, Version).

mcid_to_string() ->
    Data = test_binary(),
    MCID = macula_content:mcid(Data),
    Str = macula_content:mcid_to_string(MCID),
    ?assert(is_binary(Str)),
    ?assertMatch(<<"mcid1-", _/binary>>, Str).

mcid_from_string() ->
    Data = test_binary(),
    MCID = macula_content:mcid(Data),
    Str = macula_content:mcid_to_string(MCID),
    {ok, ParsedMCID} = macula_content:mcid_from_string(Str),
    ?assertEqual(MCID, ParsedMCID).

mcid_roundtrip() ->
    Data = test_binary(),
    MCID1 = macula_content:mcid(Data),
    Str = macula_content:mcid_to_string(MCID1),
    {ok, MCID2} = macula_content:mcid_from_string(Str),
    ?assertEqual(MCID1, MCID2).

%%%===================================================================
%%% Manifest Operation Tests
%%%===================================================================

create_manifest_from_binary() ->
    Data = test_binary(),
    {ok, Manifest} = macula_content:create_manifest(Data),
    ?assert(maps:is_key(mcid, Manifest)),
    ?assertEqual(byte_size(Data), maps:get(size, Manifest)).

create_manifest_with_options() ->
    Data = test_binary(),
    Opts = #{
        name => <<"test_app.tar.gz">>,
        chunk_size => 1024,
        hash_algorithm => sha256
    },
    {ok, Manifest} = macula_content:create_manifest(Data, Opts),
    ?assertEqual(<<"test_app.tar.gz">>, maps:get(name, Manifest)),
    ?assertEqual(sha256, maps:get(hash_algorithm, Manifest)).

verify_manifest() ->
    Data = test_binary(),
    {ok, Manifest} = macula_content:create_manifest(Data),
    ?assertEqual(true, macula_content:verify_manifest(Manifest, Data)),
    %% Different data should fail
    ?assertEqual(false, macula_content:verify_manifest(Manifest, <<"different">>)).

%%%===================================================================
%%% Chunk Operation Tests
%%%===================================================================

get_chunk_info() ->
    Data = test_binary_large(),  % 1MB
    {ok, Manifest} = macula_content:create_manifest(Data, #{chunk_size => 262144}),
    Chunks = macula_content:get_chunks(Manifest),
    ?assert(length(Chunks) >= 4),  % 1MB / 256KB = 4 chunks
    %% Each chunk has required fields
    [First | _] = Chunks,
    ?assert(maps:is_key(index, First)),
    ?assert(maps:is_key(offset, First)),
    ?assert(maps:is_key(size, First)),
    ?assert(maps:is_key(hash, First)).

list_missing_chunks() ->
    Data = test_binary_large(),
    {ok, Manifest} = macula_content:create_manifest(Data, #{chunk_size => 262144}),

    %% Initially all chunks are "missing" (not stored locally)
    Missing = macula_content:missing_chunks(Manifest),
    TotalChunks = length(macula_content:get_chunks(Manifest)),
    ?assertEqual(TotalChunks, length(Missing)).

%%%===================================================================
%%% Status and Stats Tests
%%%===================================================================

get_status() ->
    Status = macula_content:status(),
    ?assert(is_map(Status)),
    ?assert(maps:is_key(enabled, Status)).

is_local_check() ->
    Data = test_binary(),
    MCID = macula_content:mcid(Data),

    %% Not stored yet
    ?assertEqual(false, macula_content:is_local(MCID)),

    %% Store the data
    {ok, _Manifest} = macula_content:store(Data),

    %% Now it should be local
    ?assertEqual(true, macula_content:is_local(MCID)).
