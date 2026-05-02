%%%-------------------------------------------------------------------
%%% @doc Tests for macula_cache_route (Phase 2 §4.3).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cache_route_tests).

-include_lib("eunit/include/eunit.hrl").

addr(N) -> <<16#fd, N:120>>.

entry(ExpiresAtMs) ->
    #{station_pubkey => binary:copy(<<0>>, 32),
      host           => <<"127.0.0.1">>,
      port           => 4400,
      expires_at     => ExpiresAtMs}.

future() -> erlang:system_time(millisecond) + 60_000.
past()   -> erlang:system_time(millisecond) - 1.

%% =============================================================================
%% Tests — wrapped in inorder because the ETS table is named.
%% =============================================================================

cache_route_test_() ->
    {inorder, [
        ?_test(empty_lookup_returns_miss()),
        ?_test(insert_then_lookup_returns_entry()),
        ?_test(invalidate_removes_entry()),
        ?_test(expired_entry_is_evicted_on_lookup()),
        ?_test(sweep_removes_only_expired_entries()),
        ?_test(insert_overwrites_existing()),
        ?_test(size_reflects_entries())
    ]}.

cleanup() ->
    catch macula_cache_route:stop(),
    case ets:info(macula_cache_route_table) of
        undefined -> ok;
        _ -> ets:delete(macula_cache_route_table)
    end.

empty_lookup_returns_miss() ->
    cleanup(),
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 5_000}),
    ?assertEqual(miss, macula_cache_route:lookup(addr(1))),
    cleanup().

insert_then_lookup_returns_entry() ->
    cleanup(),
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 5_000}),
    E = entry(future()),
    ok = macula_cache_route:insert(addr(2), E),
    ?assertEqual({ok, E}, macula_cache_route:lookup(addr(2))),
    cleanup().

invalidate_removes_entry() ->
    cleanup(),
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 5_000}),
    ok = macula_cache_route:insert(addr(3), entry(future())),
    ok = macula_cache_route:invalidate(addr(3)),
    ?assertEqual(miss, macula_cache_route:lookup(addr(3))),
    cleanup().

expired_entry_is_evicted_on_lookup() ->
    cleanup(),
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 5_000}),
    ok = macula_cache_route:insert(addr(4), entry(past())),
    ?assertEqual(expired, macula_cache_route:lookup(addr(4))),
    %% Subsequent lookup must see the eviction.
    ?assertEqual(miss, macula_cache_route:lookup(addr(4))),
    cleanup().

sweep_removes_only_expired_entries() ->
    cleanup(),
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 60_000}),
    ok = macula_cache_route:insert(addr(5), entry(past())),
    ok = macula_cache_route:insert(addr(6), entry(future())),
    ok = macula_cache_route:insert(addr(7), entry(past())),
    ok = macula_cache_route:sweep(),
    ?assertEqual(miss, macula_cache_route:lookup(addr(5))),
    ?assertMatch({ok, _}, macula_cache_route:lookup(addr(6))),
    ?assertEqual(miss, macula_cache_route:lookup(addr(7))),
    cleanup().

insert_overwrites_existing() ->
    cleanup(),
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 60_000}),
    E1 = (entry(future()))#{port => 4400},
    E2 = (entry(future()))#{port => 5500},
    ok = macula_cache_route:insert(addr(8), E1),
    ok = macula_cache_route:insert(addr(8), E2),
    {ok, Got} = macula_cache_route:lookup(addr(8)),
    ?assertEqual(5500, maps:get(port, Got)),
    cleanup().

size_reflects_entries() ->
    cleanup(),
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 60_000}),
    ?assertEqual(0, macula_cache_route:size()),
    ok = macula_cache_route:insert(addr(9), entry(future())),
    ok = macula_cache_route:insert(addr(10), entry(future())),
    ?assertEqual(2, macula_cache_route:size()),
    cleanup().
