%% EUnit tests for macula_peer_versions: the version floor each peer is held to, in memory and through a store, and
%% what the node-wide handshake counters say (docs/design/WIRE_1_0.md sections 4.6 and 8).
-module(macula_peer_versions_tests).

-include_lib("eunit/include/eunit.hrl").

%% The store these tests hand macula_peer_versions.
-export([floor/2, raise/3]).

-define(NODE, <<1:256>>).
-define(OTHER, <<2:256>>).

peer_versions_test_() ->
    {foreach,
     fun() -> own_table() end,
     fun(Table) -> give_back(Table) end,
     [{"a node never completed with has no floor", fun no_floor/0},
      {"a completion raises the floor for the run, per node", fun raised_for_the_run/0},
      {"the floor rises only upward", fun only_upward/0},
      {"a store's floor is read, and a raise reaches the store", fun store_read_and_raised/0},
      {"the floor held for the run is read before the store", fun memory_before_store/0},
      {"a store that fails keeps the floor in memory and counts version_floor_unpersisted",
       fun failed_store_counted/0},
      {"the counters count what happened, node-wide", fun counters/0}]}.

%% The floor lasts the run, not a process (macula#50): the table belongs to macula_peering_sup, which holds every
%% connection that reads it, so a child of it that dies takes none of the memory with it.
memory_outlives_children_test_() ->
    {setup,
     fun() -> {ok, _} = application:ensure_all_started(macula) end,
     fun(_) -> ok end,
     fun() ->
         Node = <<3:256>>,
         ok = macula_peer_versions:raise(Node, 5, none),
         Sup = whereis(macula_peering_sup),
         Workers = [Pid || {_, Pid, worker, _} <- supervisor:which_children(Sup), is_pid(Pid)],
         [ok = killed(Sup, Pid) || Pid <- Workers],
         ?assertEqual(5, macula_peer_versions:floor(Node, none)),
         ?assertEqual(Sup, ets:info(macula_peer_versions, owner))
     end}.

no_floor() ->
    ?assertEqual(none, macula_peer_versions:floor(?NODE, none)).

raised_for_the_run() ->
    ok = macula_peer_versions:raise(?NODE, 5, none),
    ?assertEqual(5, macula_peer_versions:floor(?NODE, none)),
    ?assertEqual(none, macula_peer_versions:floor(?OTHER, none)).

only_upward() ->
    ok = macula_peer_versions:raise(?NODE, 6, none),
    ok = macula_peer_versions:raise(?NODE, 5, none),
    ?assertEqual(6, macula_peer_versions:floor(?NODE, none)).

store_read_and_raised() ->
    ?assertEqual(6, macula_peer_versions:floor(?NODE, store(6, ok))),
    ok = macula_peer_versions:raise(?OTHER, 5, store(none, ok)),
    ?assertEqual({raised, ?OTHER, 5}, receive {raised, _, _} = R -> R after 0 -> none end),
    %% A raise below the store's floor reaches neither.
    ok = macula_peer_versions:raise(?NODE, 5, store(6, ok)),
    ?assertEqual(none, receive {raised, _, _} = R2 -> R2 after 0 -> none end).

memory_before_store() ->
    ok = macula_peer_versions:raise(?NODE, 5, none),
    ?assertEqual(5, macula_peer_versions:floor(?NODE, store(7, ok))).

failed_store_counted() ->
    Before = maps:get(version_floor_unpersisted, macula_peer_versions:counters()),
    ok = macula_peer_versions:raise(?NODE, 5, store(none, {error, read_only})),
    ?assertEqual(5, macula_peer_versions:floor(?NODE, none)),
    ?assertEqual(Before + 1, maps:get(version_floor_unpersisted, macula_peer_versions:counters())).

counters() ->
    Zero = macula_peer_versions:counters(),
    ?assertEqual(0, lists:sum(maps:values(Zero))),
    ok = macula_peer_versions:count(downgrade_detected),
    ok = macula_peer_versions:count(downgrade_detected),
    ok = macula_peer_versions:count(v5_connections),
    #{downgrade_detected := 2, v5_connections := 1, negotiation_mismatch := 0} = macula_peer_versions:counters(),
    ?assertError(_, macula_peer_versions:count(v4_connections)).

%% A store answering Floor, and telling this process of each raise.
store(Floor, Answer) ->
    {?MODULE, #{test => self(), floor => Floor, raise => Answer}}.

floor(#{floor := Floor}, _NodeId) -> Floor.

raise(#{test := Test, raise := Answer}, NodeId, Version) ->
    Test ! {raised, NodeId, Version},
    Answer.

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
