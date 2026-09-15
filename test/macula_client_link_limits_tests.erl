%% EUnit tests for the bounds on the links a pool holds and dials: a limit on its configured seeds, a limit on its
%% direct-dial links, and a budget of new peers per window that its configured seeds never spend. Refused dials are
%% counted in the pool's status. Links dial unreachable seeds (127.0.0.1, low ports), so every link stays
%% disconnected and a fresh direct dial ends in not_connected once its dial timeout passes.
-module(macula_client_link_limits_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(DIAL_MS, 300).

link_limits_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(ok) -> ok end,
     [{spawn, Test}
      || Test <- [fun a_pool_with_more_seeds_than_its_limit_does_not_start/0,
                  fun a_fresh_direct_dial_past_the_direct_link_limit_is_refused/0,
                  fun a_fresh_direct_dial_past_the_new_peer_budget_is_refused/0,
                  fun configured_seeds_never_spend_the_new_peer_budget/0,
                  fun discovery_past_the_new_peer_budget_defers_its_additions/0,
                  fun refused_dials_are_counted_in_the_pool_status/0]]}.

a_pool_with_more_seeds_than_its_limit_does_not_start() ->
    ?assertEqual({error, {too_many_seeds, 3, 2}},
                 macula_client:connect([seed(1), seed(2), seed(3)], #{max_seeds => 2})).

%% A station already linked is reused whatever the limit; only a fresh dial is refused.
a_fresh_direct_dial_past_the_direct_link_limit_is_refused() ->
    {ok, Pool} = macula_client:connect([], #{max_direct_links => 1}),
    ?assertEqual({error, not_connected}, call_station(Pool, seed(1))),
    ?assertEqual({error, too_many_direct_links}, call_station(Pool, seed(2))),
    ?assertEqual({error, not_connected}, call_station(Pool, seed(1))),
    ok = macula_client:close(Pool).

a_fresh_direct_dial_past_the_new_peer_budget_is_refused() ->
    {ok, Pool} = macula_client:connect([], #{new_peer_budget => 1}),
    ?assertEqual({error, not_connected}, call_station(Pool, seed(1))),
    ?assertEqual({error, new_peer_budget_spent}, call_station(Pool, seed(2))),
    ok = macula_client:close(Pool).

%% The pool's configured seeds sit outside the budget: a pool with two of them still has its one new peer to dial.
configured_seeds_never_spend_the_new_peer_budget() ->
    {ok, Pool} = macula_client:connect([seed(1), seed(2)], #{new_peer_budget => 1}),
    ?assertEqual({error, not_connected}, call_station(Pool, seed(3))),
    ?assertEqual({error, new_peer_budget_spent}, call_station(Pool, seed(4))),
    ok = macula_client:close(Pool).

%% Discovered stations past the budget are not linked now, and the deferral is counted like a refused dial; a later
%% discovery run offers them again.
discovery_past_the_new_peer_budget_defers_its_additions() ->
    {ok, Pool} = macula_client:connect([], #{station_discovery => #{enabled => true, max_links => 5},
                                             new_peer_budget => 1}),
    ok = gen_server:cast(Pool, {discovered_stations, [{seed(5), undefined}, {seed(6), undefined}]}),
    {ok, #{refused_dials := Refused}} = macula_client:status(Pool),
    ?assertEqual(#{new_peer_budget_spent => 1}, Refused),
    ok = macula_client:close(Pool).

refused_dials_are_counted_in_the_pool_status() ->
    {ok, Pool} = macula_client:connect([], #{max_direct_links => 1}),
    {error, not_connected} = call_station(Pool, seed(1)),
    {error, too_many_direct_links} = call_station(Pool, seed(2)),
    {error, too_many_direct_links} = call_station(Pool, seed(3)),
    {ok, #{refused_dials := Refused}} = macula_client:status(Pool),
    ?assertEqual(#{too_many_direct_links => 2}, Refused),
    ok = macula_client:close(Pool).

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

call_station(Pool, Seed) ->
    macula_client:call_station(Pool, Seed, ?REALM, <<"x.y">>, #{}, ?DIAL_MS).

seed(Port) ->
    #{host => <<"127.0.0.1">>, port => Port}.
