%% EUnit tests for the pool's keys, identity migration step 3 on the pool side. A pool holds, in the node's crypto
%% profile, one node identity key that every link shares and one CONNECT key of its own, and starts every link with the
%% link start options profile, node_identity and connect_key. The tests stop at macula_station_link:start_link/1: what a
%% link does with those options is the connection side's.
-module(macula_client_pool_keys_tests).

-include_lib("eunit/include/eunit.hrl").

%% A pq_hybrid key carries an RSA-4096 half, which takes up to about a second to generate.
-define(EU_TIMEOUT, 120).
-define(SEED1, #{host => <<"127.0.0.1">>, port => 1}).
-define(SEED2, #{host => <<"127.0.0.1">>, port => 2}).

%%------------------------------------------------------------------
%% The link start options
%%------------------------------------------------------------------

link_start_options_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun started_links/0, fun stop_links/1, fun link_cases/1}}.

link_cases(#{profile := Profile, links := [#{opts := First}, #{opts := Second}]}) ->
    [{"every link starts with the node's crypto profile",
      ?_assertEqual([Profile, Profile], [maps:get(profile, Opts, undefined) || Opts <- [First, Second]])},
     {"every link starts with an identity key in that profile",
      ?_assertMatch(#{node_identity := #{purpose := identity, profile := Profile}}, First)},
     {"every link starts with a CONNECT key in that profile",
      ?_assertMatch(#{connect_key := #{purpose := connect, profile := Profile}}, First)},
     {"the links share one identity key and one CONNECT key", ?_test(assert_shared_keys(First, Second))},
     {"the CONNECT key shares no half with the identity key", ?_test(assert_separate_keys(First))},
     {"no link starts with a classical identity",
      ?_assertEqual([false, false], [maps:is_key(identity, Opts) || Opts <- [First, Second]])}].

assert_shared_keys(First, Second) ->
    ?assertMatch(#{node_identity := _, connect_key := _}, First),
    ?assertEqual(maps:with([node_identity, connect_key], First), maps:with([node_identity, connect_key], Second)).

assert_separate_keys(#{node_identity := #{components := IdentityHalves},
                       connect_key := #{components := ConnectHalves}}) ->
    ConnectPublics = [Public || #{public := Public} <- ConnectHalves],
    ?assertEqual([], [Public || #{public := Public} <- IdentityHalves, lists:member(Public, ConnectPublics)]).

%%------------------------------------------------------------------
%% The node identity key
%%------------------------------------------------------------------

a_generated_identity_key_meets_the_puzzle_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        {ok, Pool} = macula_client:connect([], #{}),
        {ok, #{self_node_id := NodeId}} = macula_client:status(Pool),
        ok = macula_client:close(Pool),
        ?assert(macula_node_keys:puzzle_solved(NodeId, macula_node_keys:puzzle_difficulty()))
    end}.

a_supplied_identity_key_is_used_as_it_is_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        {ok, #{self_node_id := SelfNodeId}} = macula_client:status(Pool),
        ok = macula_client:close(Pool),
        ?assertEqual(NodeId, SelfNodeId)
    end}.

an_identity_key_in_another_profile_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        [Other] = macula_crypto_profile:profiles() -- [Profile],
        {ok, Key} = macula_node_keys:generate(identity, Other),
        ?assertEqual({error, {node_identity, {wrong_profile, Other}}},
                     macula_client:connect([], #{node_identity => Key}))
    end}.

a_key_of_another_purpose_is_refused_as_the_identity_key_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(connect, Profile),
        ?assertEqual({error, {node_identity, not_an_identity_key}},
                     macula_client:connect([], #{node_identity => Key}))
    end}.

%%------------------------------------------------------------------
%% The node's crypto profile
%%------------------------------------------------------------------

a_node_without_a_crypto_profile_starts_no_pool_test_() ->
    {setup, fun profile/0, fun restore_profile/1,
     fun(_) ->
         ?_test(begin
                    ok = application:unset_env(macula, crypto_profile),
                    ?assertEqual({error, crypto_profile_missing}, macula_client:connect([], #{}))
                end)
     end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

profile() ->
    {ok, _} = application:ensure_all_started(macula),
    macula_crypto_profile:configured().

restore_profile({ok, Profile}) ->
    ok = application:set_env(macula, crypto_profile, Profile).

%% A pool of two seeds whose links are stand-ins, with the options each link was started with.
started_links() ->
    {ok, Profile} = profile(),
    Test = self(),
    ok = meck:new(macula_station_link, [passthrough]),
    ok = meck:expect(macula_station_link, start_link, fun(Opts) -> link_started(Test, Opts) end),
    ok = meck:expect(macula_station_link, stop, fun(Link) -> Link ! stop, ok end),
    {ok, Pool} = macula_client:connect([?SEED1, ?SEED2], #{}),
    #{profile => Profile, pool => Pool, links => [started_link(), started_link()]}.

stop_links(#{pool := Pool, links := Links}) ->
    ok = macula_client:close(Pool),
    lists:foreach(fun(#{link := Link}) -> Link ! stop end, Links),
    meck:unload(macula_station_link).

%% Stands in for a link: reports the options it was started with, and waits to be stopped.
link_started(Test, Opts) ->
    Link = spawn(fun() -> receive stop -> ok end end),
    Test ! {link_started, Opts, Link},
    {ok, Link}.

started_link() ->
    receive
        {link_started, Opts, Link} -> #{opts => Opts, link => Link}
    after 60000 ->
        erlang:error(no_link_started)
    end.
