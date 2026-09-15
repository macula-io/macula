%% EUnit tests for the pool's keys, identity migration step 3 on the pool side. A pool holds, in the node's crypto
%% profile, one node identity key that every link shares, and a statement issuer of its own under
%% macula_statement_issuer_sup that holds the pool's CONNECT key. Every link holds that key, that profile and that
%% issuer, and no classical identity. The issuer ends with its pool, and a pool whose issuer ends starts a new one and
%% respawns its links with it. The links are real: their state is read by field name, and their seeds name node_ids
%% nothing answers for, so nothing connects.
-module(macula_client_pool_keys_tests).

-include_lib("eunit/include/eunit.hrl").

%% A pq_hybrid key carries an RSA-4096 half, which takes up to about a second to generate.
-define(EU_TIMEOUT, 120).
%% The issuer restart backoff reaches 5 s, and a link respawns a second after it ends.
-define(RESPAWN_MS, 15_000).

%%------------------------------------------------------------------
%% What every link holds
%%------------------------------------------------------------------

link_keys_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun started_pool/0, fun stop_pool/1, fun link_cases/1}}.

link_cases(#{profile := Profile, pool := Pool}) ->
    [{"every link holds the node's crypto profile",
      ?_assertEqual([Profile, Profile], [held(Link, profile) || Link <- links(Pool)])},
     {"every link holds an identity key in that profile",
      ?_assertMatch([#{purpose := identity, profile := Profile}, #{purpose := identity, profile := Profile}],
                    [held(Link, node_identity) || Link <- links(Pool)])},
     {"every link holds a statement issuer under macula_statement_issuer_sup",
      ?_test([?assert(lists:member(held(Link, issuer), issuers())) || Link <- links(Pool)])},
     {"the links share one identity key and one issuer",
      ?_test(begin
                 [First, Second] = links(Pool),
                 ?assertEqual({held(First, node_identity), held(First, issuer)},
                              {held(Second, node_identity), held(Second, issuer)})
             end)},
     {"the issuer's CONNECT key is in that profile and shares no half with the identity key",
      ?_test(assert_separate_keys(Profile, hd(links(Pool))))},
     {"no link holds a classical identity",
      ?_assertError(function_clause, macula_station_link:state_field_index(identity))}].

assert_separate_keys(Profile, Link) ->
    #{components := IdentityHalves} = held(Link, node_identity),
    #{connect_key := #{purpose := connect, profile := Profile, components := ConnectHalves}} =
        macula_statement_issuer:connect_material(held(Link, issuer)),
    ConnectPublics = [Public || #{public := Public} <- ConnectHalves],
    ?assertEqual([], [Public || #{public := Public} <- IdentityHalves, lists:member(Public, ConnectPublics)]).

%%------------------------------------------------------------------
%% The pool's issuer
%%------------------------------------------------------------------

an_issuer_ends_with_its_pool_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Before = issuers(),
        {ok, Pool} = macula_client:connect([], #{}),
        [Issuer] = issuers() -- Before,
        Ref = erlang:monitor(process, Issuer),
        ok = macula_client:close(Pool),
        ?assertEqual(normal, receive {'DOWN', Ref, process, Issuer, Reason} -> Reason after 5000 -> still_running end)
    end}.

two_pools_have_two_issuers_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Before = issuers(),
        {ok, Pool1} = macula_client:connect([], #{}),
        {ok, Pool2} = macula_client:connect([], #{}),
        New = issuers() -- Before,
        ok = macula_client:close(Pool1),
        ok = macula_client:close(Pool2),
        ?assertEqual(2, length(New))
    end}.

%% The pool starts a new issuer once its issuer ends, and only then its links again: every link that runs
%% afterwards holds the new issuer, and none holds the one that ended.
a_pool_whose_issuer_ends_respawns_its_links_with_a_new_issuer_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        {ok, Pool} = macula_client:connect([seed(1)], #{}),
        [Link] = links(Pool),
        Ended = held(Link, issuer),
        exit(Ended, kill),
        Respawned = respawned(Pool, Link, erlang:monotonic_time(millisecond) + ?RESPAWN_MS),
        New = held(Respawned, issuer),
        ok = macula_client:close(Pool),
        ?assertNotEqual(Ended, New),
        ?assert(lists:member(New, issuers()) orelse not is_process_alive(New)),
        ?assertNot(is_process_alive(Ended))
    end}.

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

%% The statement issuers running under macula_statement_issuer_sup.
issuers() ->
    [Pid || {_Id, Pid, _Type, _Modules} <- supervisor:which_children(macula_statement_issuer_sup), is_pid(Pid)].

%% A pool of two links whose seeds name node_ids nothing answers for.
started_pool() ->
    {ok, Profile} = profile(),
    {ok, Pool} = macula_client:connect([seed(1), seed(2)], #{}),
    #{profile => Profile, pool => Pool}.

stop_pool(#{pool := Pool}) ->
    ok = macula_client:close(Pool).

seed(Port) ->
    #{host => <<"127.0.0.1">>, port => Port, expected_node_id => crypto:strong_rand_bytes(32)}.

%% The pids of the pool's links that run.
links(Pool) ->
    {ok, Infos} = macula_client:links(Pool),
    [Pid || #{pid := Pid} <- Infos, is_pid(Pid)].

%% A field of a link's state, read by name.
held(Link, Field) ->
    element(macula_station_link:state_field_index(Field), sys:get_state(Link)).

%% The pool's link that runs in place of Ended, once one does, before Deadline.
respawned(Pool, Ended, Deadline) ->
    respawned_in_time(erlang:monotonic_time(millisecond) < Deadline, links(Pool) -- [Ended], Pool, Ended, Deadline).

respawned_in_time(_InTime, [Link], _Pool, _Ended, _Deadline) ->
    Link;
respawned_in_time(true, [], Pool, Ended, Deadline) ->
    receive after 100 -> ok end,
    respawned(Pool, Ended, Deadline);
respawned_in_time(false, Links, _Pool, _Ended, _Deadline) ->
    erlang:error({no_respawned_link, Links}).
