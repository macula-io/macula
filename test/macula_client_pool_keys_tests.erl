%% EUnit tests for the pool's keys, identity migration step 3 on the pool side. A pool holds, in the node's crypto
%% profile, one node identity key that every link shares, and a statement issuer of its own under
%% macula_statement_issuer_sup that holds the pool's CONNECT key. Every link holds that key, that profile and that
%% issuer, and no classical identity. The issuer ends with its pool, and a pool whose issuer ends starts a new one and
%% respawns its links with it. The links are real: their state is read by field name, and their seeds name node_ids
%% nothing answers for, so nothing connects.
-module(macula_client_pool_keys_tests).

-include_lib("eunit/include/eunit.hrl").

%% The key loader a child spec in these tests names, and the logger handler callback that captures reports.
-export([child_key/0, log/2]).

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
      ?_assertError(function_clause, macula_station_link:state_field_index(identity))},
     {"no value in a link's state, at any depth, is a classical key pair",
      ?_assertEqual([], [Pair || Link <- links(Pool), Pair <- key_pairs(sys:get_state(Link))])}].

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
        Seen = {New =/= Ended, lists:member(New, issuers()), is_process_alive(Ended)},
        ok = macula_client:close(Pool),
        ?assertEqual({true, true, false}, Seen)
    end}.

%% While the pool's issuer cannot start again, a link start waits for the next one and counts once however long it
%% waits, and the link then starts with the issuer that runs.
a_held_link_start_counts_once_and_starts_with_the_next_issuer_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Gate = ets:new(issuer_gate, [public]),
        true = ets:insert(Gate, {open, true}),
        Start = fun(Identity, Owner) -> gated_start(ets:lookup_element(Gate, open, 2), Identity, Owner) end,
        {ok, Pool} = macula_client:connect([seed(1)], #{issuer_start => Start}),
        [Link] = links(Pool),
        true = ets:insert(Gate, {open, false}),
        exit(held(Link, issuer), kill),
        Waited = until(fun() -> waits_for_issuer(Pool) =:= 1 end, ?RESPAWN_MS),
        receive after 2_000 -> ok end,
        Held = {Waited, waits_for_issuer(Pool), links(Pool)},
        true = ets:insert(Gate, {open, true}),
        Respawned = respawned(Pool, Link, erlang:monotonic_time(millisecond) + ?RESPAWN_MS),
        Resumed = {lists:member(held(Respawned, issuer), issuers()), waits_for_issuer(Pool)},
        ok = macula_client:close(Pool),
        ?assertEqual({ok, 1, []}, Held),
        ?assertEqual({true, 1}, Resumed)
    end}.

%% A pool whose issuer ends starts a new one and counts the restart in its status. The issuer_down log line is bounded
%% to one for each window, node-wide, so the count and not the line is what a test reads.
an_issuer_restart_is_counted_in_the_status_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Before = issuers(),
        {ok, Pool} = macula_client:connect([], #{}),
        {ok, #{issuer_restarts := Initially, issuer_losses := InitiallyLost}} = macula_client:status(Pool),
        [Issuer] = issuers() -- Before,
        exit(Issuer, kill),
        Restarted = until(fun() -> issuer_restarts(Pool) =:= 1 end, ?RESPAWN_MS),
        {ok, #{issuer_losses := Lost}} = macula_client:status(Pool),
        Running = issuers() -- Before,
        ok = macula_client:close(Pool),
        ?assertEqual({0, 0, ok, 1}, {Initially, InitiallyLost, Restarted, Lost}),
        ?assertMatch([New] when New =/= Issuer, Running)
    end}.

%% A restart_issuer the pool receives while its issuer runs starts no second issuer.
a_restart_while_the_issuer_runs_starts_no_issuer_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Before = issuers(),
        {ok, Pool} = macula_client:connect([], #{}),
        Started = issuers() -- Before,
        Pool ! restart_issuer,
        _ = sys:get_state(Pool),
        After = issuers() -- Before,
        ok = macula_client:close(Pool),
        ?assertEqual(Started, After)
    end}.

the_issuer_restart_backoff_doubles_from_100_ms_to_5_s_and_resets_after_a_minute_test() ->
    Backoffs = lists:foldl(fun(_, [Last | _] = Acc) -> [macula_client:next_issuer_backoff(Last) | Acc] end,
                           [100], lists:seq(1, 7)),
    ?assertEqual([100, 200, 400, 800, 1600, 3200, 5000, 5000], lists:reverse(Backoffs)),
    ?assertEqual(5000, macula_client:issuer_restart_delay(59_999, 5000)),
    ?assertEqual(100, macula_client:issuer_restart_delay(60_000, 5000)).

%%------------------------------------------------------------------
%% A child spec loads the key
%%------------------------------------------------------------------

%% A child spec holds how to load the node identity key, never the key: a supervisor keeps the spec for its child's
%% life, and a function holding the key would keep the key there too.
a_child_spec_holds_a_loader_and_not_the_key_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        ok = persistent_term:put({?MODULE, child_key}, Key),
        Spec = macula_client:child_spec(pool, [], #{node_identity => {?MODULE, child_key, []}}),
        #{start := {Module, Function, Args}} = Spec,
        {ok, Pool} = apply(Module, Function, Args),
        {ok, #{self_node_id := NodeId}} = macula_client:status(Pool),
        ok = macula_client:close(Pool),
        _ = persistent_term:erase({?MODULE, child_key}),
        ?assertEqual({ok, NodeId}, macula_node_keys:node_id(Key)),
        ?assertEqual([], [Private || #{private := Private} <- maps:get(components, Key),
                                      binary:match(term_to_binary(Spec), Private) =/= nomatch])
    end}.

%% A child spec given the key, or a function that can hold the key, is refused: only a loader names how to get it.
a_child_spec_given_a_key_or_a_function_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        ?assertError({node_identity, loader_required}, macula_client:child_spec(pool, [], #{node_identity => Key})),
        ?assertError({node_identity, loader_required},
                     macula_client:child_spec(pool, [], #{node_identity => fun() -> Key end}))
    end}.

%% A loader that raises refuses the pool by name. The start leaves no key in its result or in any report, even when
%% the loader's error carries one.
a_loader_that_raises_refuses_the_pool_by_name_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        process_flag(trap_exit, true),
        Test = self(),
        Loader = {erlang, error, [{no_key_here, Key}]},
        Events = captured(fun() -> Test ! {started, catch macula_client:connect([], #{node_identity => Loader})} end),
        Started = receive {started, Result} -> Result after 0 -> no_result end,
        ?assertEqual({error, {node_identity, loader_failed}}, Started),
        ?assertEqual([], [Private || #{private := Private} <- maps:get(components, Key),
                                      binary:match(term_to_binary({Started, Events}), Private) =/= nomatch])
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

%% An issuer start that goes to macula_statement_issuer_sup while the gate is open, and is refused while it is shut.
gated_start(true, Identity, Owner) -> macula_statement_issuer_sup:start_issuer(Identity, Owner);
gated_start(false, _Identity, _Owner) -> {error, gate_shut}.

%% The issuers a pool has started after its first, as its status counts them.
issuer_restarts(Pool) ->
    {ok, #{issuer_restarts := Restarts}} = macula_client:status(Pool),
    Restarts.

%% The link starts of a pool that waited for an issuer, as its status counts them.
waits_for_issuer(Pool) ->
    {ok, #{refused_dials := Refused}} = macula_client:status(Pool),
    maps:get(link_start_waits_for_issuer, Refused, 0).

%% ok once Check holds, checked every 50 ms, or {error, timeout} after TimeoutMs.
until(Check, TimeoutMs) ->
    until_by(Check, erlang:monotonic_time(millisecond) + TimeoutMs).

until_by(Check, Deadline) ->
    checked(Check(), erlang:monotonic_time(millisecond) >= Deadline, Check, Deadline).

checked(true, _Late, _Check, _Deadline) -> ok;
checked(false, true, _Check, _Deadline) -> {error, timeout};
checked(false, false, Check, Deadline) -> receive after 50 -> ok end, until_by(Check, Deadline).

%% Every classical key pair in a term, at any depth: a map of exactly a public and a private half.
key_pairs(#{public := _, private := _} = Pair) when map_size(Pair) =:= 2 -> [Pair];
key_pairs(Map) when is_map(Map) -> key_pairs(maps:values(Map));
key_pairs([Head | Tail]) -> key_pairs(Head) ++ key_pairs(Tail);
key_pairs(Tuple) when is_tuple(Tuple) -> key_pairs(tuple_to_list(Tuple));
key_pairs(_Other) -> [].

%% The log events that running Act leaves, caught by a logger handler added for the run.
captured(Act) ->
    Handler = list_to_atom("macula_pool_keys_capture_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Handler, ?MODULE, #{config => #{test => self()}, level => all, filter_default => log}),
    _ = Act(),
    receive after 500 -> ok end,
    ok = logger:remove_handler(Handler),
    drained([]).

log(Event, #{config := #{test := Test}}) ->
    Test ! {captured, Event}.

drained(Events) ->
    receive
        {captured, Event} -> drained([Event | Events])
    after 0 ->
        lists:reverse(Events)
    end.

%% The loader the child spec test names: the key it stored.
child_key() ->
    {ok, persistent_term:get({?MODULE, child_key})}.
