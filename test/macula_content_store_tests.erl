%% EUnit tests for `macula_content_store': the content a node shares, held by the node itself (D27). Bytes of at most
%% one chunk are one raw block under their SHA-384 content id; larger bytes are chunked under a manifest, whose content
%% id is the root. A chunk two roots share is kept until neither needs it.
-module(macula_content_store_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CHUNK, 262144).

small_bytes_are_one_raw_block_test() ->
    Bytes = <<"hello, mesh">>,
    {MCID, Store} = macula_content_store:added(Bytes, #{}, macula_content_store:new()),
    ?assertEqual(<<2, 16#55, (crypto:hash(sha384, Bytes))/binary>>, MCID),
    ?assertEqual({block, Bytes}, macula_content_store:root(MCID, Store)).

large_bytes_are_chunked_under_a_manifest_test() ->
    Bytes = crypto:strong_rand_bytes(?CHUNK * 2 + 17),
    {MCID, Store} = macula_content_store:added(Bytes, #{name => <<"big.bin">>}, macula_content_store:new()),
    ?assertMatch(<<2, 16#56, _:48/binary>>, MCID),
    {manifest, Manifest} = macula_content_store:root(MCID, Store),
    ?assertEqual(ok, macula_manifest:verify_mcid(Manifest, MCID)),
    ?assertEqual(3, maps:get(chunk_count, Manifest)),
    Chunks = [begin {ok, C} = macula_manifest:chunk_mcid(Manifest, I),
                    {ok, B} = macula_content_store:chunk(C, Store), B end || I <- [0, 1, 2]],
    ?assertEqual(Bytes, iolist_to_binary(Chunks)).

an_unknown_content_id_is_not_found_test() ->
    Store = macula_content_store:new(),
    ?assertEqual(not_found, macula_content_store:root(<<2, 16#55, 0:384>>, Store)),
    ?assertEqual(not_found, macula_content_store:chunk(<<2, 16#55, 0:384>>, Store)).

%% A raw root is not a chunk of anything: asking for it as a chunk finds nothing, so a fetcher cannot read around the
%% root it was given.
a_raw_root_is_not_served_as_a_chunk_test() ->
    {MCID, Store} = macula_content_store:added(<<"x">>, #{}, macula_content_store:new()),
    ?assertEqual(not_found, macula_content_store:chunk(MCID, Store)).

removing_a_root_drops_its_chunks_test() ->
    Bytes = crypto:strong_rand_bytes(?CHUNK + 1),
    {MCID, S1} = macula_content_store:added(Bytes, #{}, macula_content_store:new()),
    {manifest, Manifest} = macula_content_store:root(MCID, S1),
    {ok, C0} = macula_manifest:chunk_mcid(Manifest, 0),
    S2 = macula_content_store:removed(MCID, S1),
    ?assertEqual(not_found, macula_content_store:root(MCID, S2)),
    ?assertEqual(not_found, macula_content_store:chunk(C0, S2)),
    ?assertEqual([], macula_content_store:roots(S2)).

%% Two roots sharing a chunk (the same first 256 KiB): removing one keeps the chunk for the other.
a_shared_chunk_outlives_one_of_its_roots_test() ->
    Head = crypto:strong_rand_bytes(?CHUNK),
    {A, S1} = macula_content_store:added(<<Head/binary, "tail-a">>, #{}, macula_content_store:new()),
    {B, S2} = macula_content_store:added(<<Head/binary, "tail-b">>, #{}, S1),
    {manifest, MA} = macula_content_store:root(A, S2),
    {ok, Shared} = macula_manifest:chunk_mcid(MA, 0),
    S3 = macula_content_store:removed(A, S2),
    ?assertEqual({ok, Head}, macula_content_store:chunk(Shared, S3)),
    ?assertMatch({manifest, _}, macula_content_store:root(B, S3)),
    ?assertEqual(not_found, macula_content_store:chunk(Shared, macula_content_store:removed(B, S3))).

%% Sharing the same bytes twice is one root, and one removal takes it away.
the_same_bytes_twice_are_one_root_test() ->
    Bytes = crypto:strong_rand_bytes(?CHUNK + 5),
    {M, S1} = macula_content_store:added(Bytes, #{}, macula_content_store:new()),
    {M, S2} = macula_content_store:added(Bytes, #{}, S1),
    ?assertEqual([M], macula_content_store:roots(S2)),
    ?assertEqual(not_found, macula_content_store:root(M, macula_content_store:removed(M, S2))).

removing_an_unknown_root_changes_nothing_test() ->
    {M, S1} = macula_content_store:added(<<"x">>, #{}, macula_content_store:new()),
    ?assertEqual(S1, macula_content_store:removed(<<2, 16#55, 0:384>>, S1)),
    ?assertEqual([M], macula_content_store:roots(S1)).
