%% EUnit tests for macula_peer_versions: which handshake version a client dials a node with, and what the node-wide
%% handshake counters say (docs/design/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md sections 3, 4 and 6).
-module(macula_peer_versions_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NODE, <<1:256>>).
-define(OTHER, <<2:256>>).
-define(T0, 1789000000000).
-define(MINUTE, 60000).

peer_versions_test_() ->
    {foreach,
     fun() -> own_table() end,
     fun(Table) -> give_back(Table) end,
     [{"a node never seen is dialled with version 5", fun never_seen_is_v5/0},
      {"after a fallback a node is dialled with version 4 for 10 minutes, then 5 again", fun fallback_cache/0},
      {"a node seen on version 5 is never dialled with version 4", fun seen_v5_never_v4/0},
      {"a node seen on version 5 refuses a fallback, until it is forgotten", fun downgrade_refused_until_forgotten/0},
      {"fallbacks are counted per node, and warned from the second, at most once a minute per node",
       fun fallback_warnings/0},
      {"a refused downgrade is warned per node with its count, at most once a minute", fun downgrade_warnings/0},
      {"the counters count what happened, node-wide", fun counters/0}]}.

%% The downgrade memory lasts the run, not a process (macula#50): the table belongs to macula_peering_sup, which
%% holds every connection that reads it, so a child of it that dies takes none of the memory with it.
memory_outlives_children_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula) end,
     fun(_) -> ok end,
     fun() ->
         Node = <<3:256>>,
         ok = macula_peer_versions:completed_v5(Node),
         Sup = whereis(macula_peering_sup),
         Workers = [Pid || {_, Pid, worker, _} <- supervisor:which_children(Sup), is_pid(Pid)],
         [ok = killed(Sup, Pid) || Pid <- Workers],
         ?assert(macula_peer_versions:seen_v5(Node)),
         ?assertEqual(5, macula_peer_versions:dial_version(Node, ?T0)),
         ?assertEqual(Sup, ets:info(macula_peer_versions, owner)),
         ok = macula_peering:forget_v5_peer(Node)
     end}.

%% Pid, a child of Sup, is killed and Sup has started its successor.
killed(Sup, Pid) ->
    exit(Pid, kill),
    ok = wait_down(Pid),
    restarted_after(Sup, Pid, 50).

restarted_after(_Sup, _Pid, 0) ->
    error(not_restarted);
restarted_after(Sup, Pid, Tries) ->
    Children = [P || {_, P, _, _} <- supervisor:which_children(Sup)],
    successor(lists:all(fun is_pid/1, Children) andalso not lists:member(Pid, Children), Sup, Pid, Tries).

successor(true, _Sup, _Pid, _Tries) -> ok;
successor(false, Sup, Pid, Tries) ->
    timer:sleep(20),
    restarted_after(Sup, Pid, Tries - 1).

never_seen_is_v5() ->
    ?assertEqual(5, macula_peer_versions:dial_version(?NODE, ?T0)).

fallback_cache() ->
    ?assertEqual({fall_back, 1}, macula_peer_versions:unsupported_version(?NODE, ?T0)),
    ?assertEqual(4, macula_peer_versions:dial_version(?NODE, ?T0 + 1)),
    ?assertEqual(4, macula_peer_versions:dial_version(?NODE, ?T0 + 10 * ?MINUTE - 1)),
    ?assertEqual(5, macula_peer_versions:dial_version(?NODE, ?T0 + 10 * ?MINUTE)),
    ?assertEqual(5, macula_peer_versions:dial_version(?OTHER, ?T0 + 1)).

seen_v5_never_v4() ->
    ok = macula_peer_versions:completed_v5(?NODE),
    ?assertEqual(5, macula_peer_versions:dial_version(?NODE, ?T0)),
    ?assert(macula_peer_versions:seen_v5(?NODE)),
    ?assertNot(macula_peer_versions:seen_v5(?OTHER)).

downgrade_refused_until_forgotten() ->
    ok = macula_peer_versions:completed_v5(?NODE),
    ?assertEqual(downgrade_refused, macula_peer_versions:unsupported_version(?NODE, ?T0)),
    ?assertEqual(5, macula_peer_versions:dial_version(?NODE, ?T0 + 1)),
    ok = macula_peering:forget_v5_peer(?NODE),
    ?assertNot(macula_peer_versions:seen_v5(?NODE)),
    ?assertEqual({fall_back, 1}, macula_peer_versions:unsupported_version(?NODE, ?T0 + 2)),
    ?assertEqual(4, macula_peer_versions:dial_version(?NODE, ?T0 + 3)).

fallback_warnings() ->
    ?assertEqual({fall_back, 1}, macula_peer_versions:unsupported_version(?NODE, ?T0)),
    ?assertEqual(no_warning, macula_peer_versions:fallback_warning(?NODE, ?T0)),
    ?assertEqual({fall_back, 2}, macula_peer_versions:unsupported_version(?NODE, ?T0 + 1)),
    ?assertEqual({warn, 2}, macula_peer_versions:fallback_warning(?NODE, ?T0 + 1)),
    ?assertEqual({fall_back, 3}, macula_peer_versions:unsupported_version(?NODE, ?T0 + 2)),
    ?assertEqual(no_warning, macula_peer_versions:fallback_warning(?NODE, ?T0 + 2)),
    ?assertEqual({warn, 3}, macula_peer_versions:fallback_warning(?NODE, ?T0 + 1 + ?MINUTE)),
    ?assertEqual({fall_back, 1}, macula_peer_versions:unsupported_version(?OTHER, ?T0 + 3)),
    ?assertEqual(no_warning, macula_peer_versions:fallback_warning(?OTHER, ?T0 + 3)).

downgrade_warnings() ->
    ok = macula_peer_versions:completed_v5(?NODE),
    downgrade_refused = macula_peer_versions:unsupported_version(?NODE, ?T0),
    ?assertEqual({warn, 1}, macula_peer_versions:downgrade_warning(?NODE, ?T0)),
    downgrade_refused = macula_peer_versions:unsupported_version(?NODE, ?T0 + 1),
    ?assertEqual(no_warning, macula_peer_versions:downgrade_warning(?NODE, ?T0 + 1)),
    ?assertEqual({warn, 2}, macula_peer_versions:downgrade_warning(?NODE, ?T0 + ?MINUTE)),
    ok = macula_peer_versions:completed_v5(?OTHER),
    downgrade_refused = macula_peer_versions:unsupported_version(?OTHER, ?T0 + 2),
    ?assertEqual({warn, 1}, macula_peer_versions:downgrade_warning(?OTHER, ?T0 + 2)).

counters() ->
    Zero = macula_peer_versions:counters(),
    ?assertEqual(0, maps:get(v5_connections, Zero)),
    ok = macula_peer_versions:count(v5_connections),
    ok = macula_peer_versions:count(v5_connections),
    ok = macula_peer_versions:count(session_proof_rate),
    {fall_back, 1} = macula_peer_versions:unsupported_version(?NODE, ?T0),
    ok = macula_peer_versions:completed_v5(?OTHER),
    downgrade_refused = macula_peer_versions:unsupported_version(?OTHER, ?T0),
    Counted = macula_peering:handshake_counters(),
    ?assertEqual(2, maps:get(v5_connections, Counted)),
    ?assertEqual(1, maps:get(session_proof_rate, Counted)),
    ?assertEqual(1, maps:get(v4_fallbacks, Counted)),
    ?assertEqual(1, maps:get(v5_downgrade_refused, Counted)),
    ?assertEqual(lists:sort([v4_connections, v5_connections, v4_control_frames, v4_fallbacks, v5_downgrade_refused,
                             v4_hello_to_v5_connect, session_proof_invalid, session_proof_missing,
                             session_proof_rate, exporter_unavailable]),
                 lists:sort(maps:keys(Counted))),
    ?assertError(function_clause, macula_peer_versions:count(not_a_counter)).

%% An empty table for each test. When the macula application runs, macula_peering_sup owns it and the test empties it
%% before and after; otherwise a process of the test's own holds one for the test.
own_table() ->
    own_table(ets:whereis(macula_peer_versions)).

own_table(undefined) ->
    Parent = self(),
    Holder = spawn(fun() -> ok = macula_peer_versions:new_table(), Parent ! {held, self()}, receive stop -> ok end end),
    receive {held, Holder} -> {held, Holder} after 5000 -> error(no_table) end;
own_table(_Supervised) ->
    true = ets:delete_all_objects(macula_peer_versions),
    supervised.

give_back({held, Holder}) ->
    exit(Holder, kill),
    wait_down(Holder);
give_back(supervised) ->
    true = ets:delete_all_objects(macula_peer_versions),
    ok.

wait_down(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _} -> ok after 5000 -> error(not_down) end.
