%% @doc Unit tests for macula_did_cache module.
-module(macula_did_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

setup() ->
    %% Clear cache before each test
    macula_did_cache:clear(),
    ok.

cleanup(_) ->
    %% Clear cache after each test
    macula_did_cache:clear(),
    ok.

%%====================================================================
%% Test Generators
%%====================================================================

did_cache_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"get_or_parse returns parsed DID", fun get_or_parse_valid_did/0},
         {"get_or_parse caches result", fun get_or_parse_caches_result/0},
         {"get_or_parse returns error for invalid DID", fun get_or_parse_invalid_did/0},
         {"get_or_parse returns error for non-binary", fun get_or_parse_non_binary/0},
         {"invalidate removes DID from cache", fun invalidate_removes_did/0},
         {"invalidate handles non-cached DID", fun invalidate_non_cached/0},
         {"invalidate handles non-binary", fun invalidate_non_binary/0},
         {"clear removes all cached DIDs", fun clear_removes_all/0},
         {"cache_size returns correct count", fun cache_size_returns_count/0},
         {"cache_key generates correct format", fun cache_key_format/0},
         {"parse various DID formats", fun parse_various_formats/0},
         {"performance: cached lookup is fast", fun performance_cached_lookup/0}
     ]
    }.

%%====================================================================
%% Basic Functionality Tests
%%====================================================================

get_or_parse_valid_did() ->
    DID = <<"did:macula:io.macula.rgfaber">>,
    {ok, Parsed} = macula_did_cache:get_or_parse(DID),

    ?assertEqual(<<"macula">>, maps:get(<<"method">>, Parsed)),
    ?assertEqual(<<"io.macula.rgfaber">>, maps:get(<<"identity">>, Parsed)),
    ?assertEqual([<<"io">>, <<"macula">>, <<"rgfaber">>], maps:get(<<"parts">>, Parsed)),
    ?assertEqual(3, maps:get(<<"depth">>, Parsed)).

get_or_parse_caches_result() ->
    DID = <<"did:macula:io.macula.example">>,

    %% First call parses and caches
    {ok, Parsed1} = macula_did_cache:get_or_parse(DID),

    %% Second call should return same result (from cache)
    {ok, Parsed2} = macula_did_cache:get_or_parse(DID),

    ?assertEqual(Parsed1, Parsed2),

    %% Verify it's actually in persistent_term
    Key = macula_did_cache:cache_key(DID),
    Cached = persistent_term:get(Key, not_found),
    ?assertEqual(Parsed1, Cached).

get_or_parse_invalid_did() ->
    %% Wrong prefix
    ?assertEqual({error, invalid_did}, macula_did_cache:get_or_parse(<<"not:a:did">>)),

    %% Missing identity
    ?assertEqual({error, invalid_did}, macula_did_cache:get_or_parse(<<"did:macula">>)),

    %% Wrong method
    ?assertEqual({error, invalid_did}, macula_did_cache:get_or_parse(<<"did:key:abc">>)),

    %% Empty string
    ?assertEqual({error, invalid_did}, macula_did_cache:get_or_parse(<<"">>)).

get_or_parse_non_binary() ->
    ?assertEqual({error, invalid_did}, macula_did_cache:get_or_parse(123)),
    ?assertEqual({error, invalid_did}, macula_did_cache:get_or_parse(atom)),
    ?assertEqual({error, invalid_did}, macula_did_cache:get_or_parse({tuple})),
    ?assertEqual({error, invalid_did}, macula_did_cache:get_or_parse([list])).

%%====================================================================
%% Invalidation Tests
%%====================================================================

invalidate_removes_did() ->
    DID = <<"did:macula:io.macula.invalidate.test">>,

    %% Cache the DID
    {ok, _Parsed} = macula_did_cache:get_or_parse(DID),
    ?assertEqual(1, macula_did_cache:cache_size()),

    %% Invalidate
    ok = macula_did_cache:invalidate(DID),

    %% Verify removed
    Key = macula_did_cache:cache_key(DID),
    ?assertEqual(undefined, persistent_term:get(Key, undefined)),
    ?assertEqual(0, macula_did_cache:cache_size()).

invalidate_non_cached() ->
    DID = <<"did:macula:io.macula.never.cached">>,

    %% Should not error
    ?assertEqual(ok, macula_did_cache:invalidate(DID)).

invalidate_non_binary() ->
    ?assertEqual(ok, macula_did_cache:invalidate(123)),
    ?assertEqual(ok, macula_did_cache:invalidate(undefined)).

%%====================================================================
%% Clear Tests
%%====================================================================

clear_removes_all() ->
    %% Cache multiple DIDs
    DIDs = [
        <<"did:macula:io.macula.one">>,
        <<"did:macula:io.macula.two">>,
        <<"did:macula:io.macula.three">>
    ],
    lists:foreach(fun(D) -> {ok, _} = macula_did_cache:get_or_parse(D) end, DIDs),

    ?assertEqual(3, macula_did_cache:cache_size()),

    %% Clear all
    ok = macula_did_cache:clear(),

    ?assertEqual(0, macula_did_cache:cache_size()).

%%====================================================================
%% Cache Size Tests
%%====================================================================

cache_size_returns_count() ->
    ?assertEqual(0, macula_did_cache:cache_size()),

    {ok, _} = macula_did_cache:get_or_parse(<<"did:macula:one">>),
    ?assertEqual(1, macula_did_cache:cache_size()),

    {ok, _} = macula_did_cache:get_or_parse(<<"did:macula:two">>),
    ?assertEqual(2, macula_did_cache:cache_size()),

    %% Same DID doesn't increase count
    {ok, _} = macula_did_cache:get_or_parse(<<"did:macula:one">>),
    ?assertEqual(2, macula_did_cache:cache_size()).

%%====================================================================
%% Cache Key Tests
%%====================================================================

cache_key_format() ->
    DID = <<"did:macula:io.macula.test">>,
    Key = macula_did_cache:cache_key(DID),

    ?assertEqual({macula_did_cache, DID}, Key).

%%====================================================================
%% Various DID Format Tests
%%====================================================================

parse_various_formats() ->
    %% Short identity
    {ok, P1} = macula_did_cache:get_or_parse(<<"did:macula:test">>),
    ?assertEqual(<<"test">>, maps:get(<<"identity">>, P1)),
    ?assertEqual([<<"test">>], maps:get(<<"parts">>, P1)),
    ?assertEqual(1, maps:get(<<"depth">>, P1)),

    %% Deep identity
    {ok, P2} = macula_did_cache:get_or_parse(<<"did:macula:io.macula.ibm.watson.api">>),
    ?assertEqual(<<"io.macula.ibm.watson.api">>, maps:get(<<"identity">>, P2)),
    ?assertEqual([<<"io">>, <<"macula">>, <<"ibm">>, <<"watson">>, <<"api">>],
                 maps:get(<<"parts">>, P2)),
    ?assertEqual(5, maps:get(<<"depth">>, P2)),

    %% Realm-level identity
    {ok, P3} = macula_did_cache:get_or_parse(<<"did:macula:io.macula">>),
    ?assertEqual(<<"io.macula">>, maps:get(<<"identity">>, P3)),
    ?assertEqual(2, maps:get(<<"depth">>, P3)).

%%====================================================================
%% Performance Tests
%%====================================================================

performance_cached_lookup() ->
    DID = <<"did:macula:io.macula.performance.test">>,

    %% First call to cache
    {ok, _} = macula_did_cache:get_or_parse(DID),

    %% Measure cached lookups (should be very fast)
    Iterations = 10000,
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            {ok, _} = macula_did_cache:get_or_parse(DID)
        end, lists:seq(1, Iterations))
    end),

    %% 10k lookups should take less than 100ms (10 microseconds each)
    %% persistent_term is O(1) and extremely fast
    TimePerLookup = Time / Iterations,
    ?assert(TimePerLookup < 100), %% Less than 100 microseconds per lookup

    %% Log for visibility
    io:format("~n  Cached lookup time: ~.2f μs per lookup~n", [TimePerLookup]).
