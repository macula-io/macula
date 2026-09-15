%% EUnit tests for the bounds on the links a pool holds and dials: a limit on its configured seeds, a limit on its
%% direct-dial links, and a budget of new peers per window that its configured seeds never spend. Refused dials are
%% counted in the pool's status. A pool given a seed that names no node_id it expects does not start. Links dial
%% unreachable seeds (127.0.0.1, low ports), each naming the node_id it expects as a link requires, so every link
%% starts, stays disconnected, and a fresh direct dial ends in not_connected once its dial timeout passes.
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
                  fun a_pool_given_a_seed_that_names_no_node_id_does_not_start/0,
                  fun a_link_limit_outside_its_range_does_not_start_the_pool/0,
                  fun a_fresh_direct_dial_past_the_direct_link_limit_is_refused/0,
                  fun a_fresh_direct_dial_past_the_new_peer_budget_is_refused/0,
                  fun configured_seeds_never_spend_the_new_peer_budget/0,
                  fun discovery_past_the_new_peer_budget_defers_its_additions/0,
                  fun refused_dials_are_counted_in_the_pool_status/0,
                  fun a_direct_dial_to_a_seed_the_pool_cannot_dial_is_refused/0,
                  fun discovered_seeds_the_pool_cannot_dial_are_refused/0]]}.

a_pool_with_more_seeds_than_its_limit_does_not_start() ->
    ?assertEqual({error, {too_many_seeds, 3, 2}},
                 macula_client:connect([seed(1), seed(2), seed(3)], #{max_seeds => 2})).

%% A pool starts only on seeds that each name the node_id they expect, in the seed or in the pool's expected_node_id
%% option. A seed that names none, or whose own expected_node_id is not 32 bytes, stops the start by name, and no
%% statement issuer starts. Seeds pinned either way, and a pool with no seeds, start.
a_pool_given_a_seed_that_names_no_node_id_does_not_start() ->
    Issuers = fun() -> proplists:get_value(active, supervisor:count_children(macula_statement_issuer_sup)) end,
    Before = Issuers(),
    Refused = [{[<<"quic://127.0.0.1:1">>], #{}},
               {[#{host => <<"127.0.0.1">>, port => 1}], #{}},
               {[seed(1), #{host => <<"127.0.0.1">>, port => 2}], #{}},
               {[(seed(1))#{expected_node_id => <<1:248>>}], #{expected_node_id => <<1:256>>}}],
    [?assertEqual({error, {seeds, expected_node_id_required}}, macula_client:connect(Seeds, Opts))
     || {Seeds, Opts} <- Refused],
    ?assert(Issuers() =< Before),
    Started = [{[seed(1)], #{}}, {[<<"quic://127.0.0.1:1">>], #{expected_node_id => <<1:256>>}}, {[], #{}}],
    [begin {ok, Pool} = macula_client:connect(Seeds, Opts), ok = macula_client:close(Pool) end
     || {Seeds, Opts} <- Started].

%% Each link limit is an integer from 1 to its cap. Anything else, an atom included, does not start the pool, so no
%% bound is silently lifted.
a_link_limit_outside_its_range_does_not_start_the_pool() ->
    [?assertEqual({error, {invalid_link_limit, Key, Value}}, macula_client:connect([seed(1)], #{Key => Value}))
     || {Key, Cap} <- [{max_seeds, 64}, {max_direct_links, 64}, {new_peer_budget, 256}],
        Value <- [many, 0, -1, Cap + 1]],
    [?assertEqual({error, {invalid_link_limit, max_links, Value}},
                  macula_client:connect([], #{station_discovery => #{enabled => true, max_links => Value}}))
     || Value <- [many, 0, -1, 65]].

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

%% A seed with no text host, or no port from 1 to 65535, can never be dialed: a direct dial to one is refused, counted,
%% and starts no link.
a_direct_dial_to_a_seed_the_pool_cannot_dial_is_refused() ->
    {ok, Pool} = macula_client:connect([], #{}),
    ?assertEqual({error, unusable_seed}, call_station(Pool, #{host => {not_a, host}, port => 4433})),
    ?assertEqual({error, unusable_seed}, call_station(Pool, #{host => <<"127.0.0.1">>, port => 0})),
    {ok, #{refused_dials := Refused}} = macula_client:status(Pool),
    ?assertEqual({#{unusable_seed => 2}, {ok, []}}, {Refused, macula_client:links(Pool)}),
    ok = macula_client:close(Pool).

%% Discovered stations whose seeds cannot be dialed are refused and counted where they enter the pool, which starts no
%% link for them and keeps serving.
discovered_seeds_the_pool_cannot_dial_are_refused() ->
    {ok, Pool} = macula_client:connect([], #{station_discovery => #{enabled => true, max_links => 5}}),
    Unusable = [#{host => {not_a, host}, port => 4433, expected_node_id => <<1:256>>},
                #{host => [16#110000], port => 4433, expected_node_id => <<2:256>>},
                #{host => <<"127.0.0.1">>, port => 0, expected_node_id => <<3:256>>}],
    ok = gen_server:cast(Pool, {discovered_stations, [{Seed, undefined} || Seed <- Unusable]}),
    {ok, #{refused_dials := Refused}} = macula_client:status(Pool),
    ?assertEqual({#{unusable_seed => 3}, {ok, []}}, {Refused, macula_client:links(Pool)}),
    ok = macula_client:close(Pool).

%%---------------------------------------------------------------------
%% One peer per station, however its seed is spelled
%%---------------------------------------------------------------------

%% Scheme, the case of ASCII letters, a charlist or a binary host: one station, one peer.
a_station_is_one_peer_however_its_seed_is_spelled_test() ->
    Spellings = [<<"https://Station.Example:4433">>, <<"quic://station.example:4433">>, "quic://STATION.example:4433",
                 #{host => "station.example", port => 4433}, #{host => <<"Station.Example">>, port => 4433}],
    ?assertEqual(1, length(lists:usort([macula_client:seed_peer(Seed) || Seed <- Spellings]))).

%% Only ASCII letters fold. A name with a trailing dot, and a name whose non-ASCII letter lowercases to an ASCII one,
%% are other names to DNS, so each is another peer: two peers counted as one would let more new peers through than
%% the budget allows.
a_trailing_dot_or_a_non_ascii_letter_names_another_peer_test() ->
    ?assertNotEqual(macula_client:seed_peer(#{host => <<"station.example">>, port => 4433}),
                    macula_client:seed_peer(#{host => <<"station.example.">>, port => 4433})),
    ?assertNotEqual(macula_client:seed_peer(#{host => <<"k.example">>, port => 4433}),
                    macula_client:seed_peer(#{host => <<16#212A/utf8, ".example">>, port => 4433})).

%% A host that is not text, or not valid text, still gives its seed a peer, without raising: a malformed seed from
%% discovery or a direct dial must not stop the pool.
a_malformed_host_never_raises_test() ->
    [?assertMatch({seed, _}, macula_client:seed_peer(#{host => Host, port => 4433}))
     || Host <- [<<255>>, {not_a, host}, [16#110000], [not_a_char]]].

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
    macula_client:call_station(Pool, Seed, <<2:256>>, ?REALM, <<"x.y">>, #{}, ?DIAL_MS).

seed(Port) ->
    #{host => <<"127.0.0.1">>, port => Port, expected_node_id => <<Port:256>>}.
