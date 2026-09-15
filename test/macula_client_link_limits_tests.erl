%% EUnit tests for the bounds on the links a pool holds and dials: a limit on its configured seeds, a limit on its
%% direct-dial links, and a budget of new peers per window that its configured seeds never spend. Refused dials are
%% counted in the pool's status. Links dial unreachable seeds (127.0.0.1, low ports), each naming the node_id it
%% expects as a link requires, so every link starts, stays disconnected, and a fresh direct dial ends in not_connected
%% once its dial timeout passes.
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
                  fun a_link_limit_outside_its_range_does_not_start_the_pool/0,
                  fun a_fresh_direct_dial_past_the_direct_link_limit_is_refused/0,
                  fun a_fresh_direct_dial_past_the_new_peer_budget_is_refused/0,
                  fun configured_seeds_never_spend_the_new_peer_budget/0,
                  fun discovery_past_the_new_peer_budget_defers_its_additions/0,
                  fun refused_dials_are_counted_in_the_pool_status/0]]}.

a_pool_with_more_seeds_than_its_limit_does_not_start() ->
    ?assertEqual({error, {too_many_seeds, 3, 2}},
                 macula_client:connect([seed(1), seed(2), seed(3)], #{max_seeds => 2})).

%% Each link limit is an integer from 1 to its cap. Anything else, an atom included, does not start the pool, so no
%% bound is silently lifted.
a_link_limit_outside_its_range_does_not_start_the_pool() ->
    [?assertEqual({error, {invalid_link_limit, Key, Value}}, macula_client:connect([seed(1)], #{Key => Value}))
     || {Key, Cap} <- [{max_seeds, 64}, {max_direct_links, 64}, {new_peer_budget, 256}],
        Value <- [many, 0, -1, Cap + 1]],
    [?assertEqual({error, {invalid_link_limit, max_links, Value}},
                  macula_client:connect([], #{station_discovery => #{enabled => true, max_links => Value}}))
     || Value <- [many, 0, 65]].

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
%% One peer per station, however its seed is spelled
%%---------------------------------------------------------------------

%% Scheme, host case, a trailing dot, a charlist or a binary host: one station, one peer.
a_station_is_one_peer_however_its_seed_is_spelled_test() ->
    Spellings = [<<"https://Station.Example.:4433">>, <<"quic://station.example:4433">>, "quic://STATION.example:4433",
                 #{host => "station.example", port => 4433}, #{host => <<"Station.Example.">>, port => 4433}],
    ?assertEqual(1, length(lists:usort([macula_client:seed_peer(Seed) || Seed <- Spellings]))).

%% An IP literal is one peer in any of its textual forms, and an IPv4 address mapped into IPv6 is that IPv4 address.
an_ip_literal_is_one_peer_in_any_of_its_forms_test() ->
    V6 = [<<"quic://[::1]:4433">>, <<"quic://[0:0:0:0:0:0:0:1]:4433">>, #{host => "::1", port => 4433},
          #{host => <<"[0::1]">>, port => 4433}],
    V4 = [<<"quic://127.0.0.1:4433">>, <<"quic://[::ffff:127.0.0.1]:4433">>, #{host => "127.0.0.1", port => 4433}],
    ?assertEqual(1, length(lists:usort([macula_client:seed_peer(Seed) || Seed <- V6]))),
    ?assertEqual(1, length(lists:usort([macula_client:seed_peer(Seed) || Seed <- V4]))),
    ?assertNotEqual(macula_client:seed_peer(hd(V6)), macula_client:seed_peer(hd(V4))).

a_different_port_or_host_is_another_peer_test() ->
    ?assertNotEqual(macula_client:seed_peer(<<"quic://station.example:4433">>),
                    macula_client:seed_peer(<<"quic://station.example:4434">>)),
    ?assertNotEqual(macula_client:seed_peer(<<"quic://station.example:4433">>),
                    macula_client:seed_peer(<<"quic://other.example:4433">>)).

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

call_station(Pool, Seed) ->
    macula_client:call_station(Pool, Seed, ?REALM, <<"x.y">>, #{}, ?DIAL_MS).

seed(Port) ->
    #{host => <<"127.0.0.1">>, port => Port, expected_node_id => <<Port:256>>}.
