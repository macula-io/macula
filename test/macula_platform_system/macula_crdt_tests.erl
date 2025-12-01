%% @doc Unit tests for macula_crdt (CRDT implementations)
-module(macula_crdt_tests).
-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% LWW-Register Tests
%%------------------------------------------------------------------------------

lww_register_test_() ->
    [
     ?_test(test_new_lww_register_empty()),
     ?_test(test_new_lww_register_with_value()),
     ?_test(test_lww_set_updates_value()),
     ?_test(test_lww_get_returns_value()),
     ?_test(test_lww_timestamp_returns_timestamp()),
     ?_test(test_lww_value_alias_for_get()),
     ?_test(test_lww_merge_keeps_higher_timestamp()),
     ?_test(test_lww_merge_keeps_lower_timestamp()),
     ?_test(test_lww_merge_breaks_ties_by_node()),
     ?_test(test_lww_merge_idempotent()),
     ?_test(test_lww_merge_commutative()),
     ?_test(test_lww_merge_associative()),
     ?_test(test_lww_concurrent_updates()),
     ?_test(test_lww_sequential_updates())
    ].

%%------------------------------------------------------------------------------
%% OR-Set Tests
%%------------------------------------------------------------------------------

or_set_test_() ->
    [
     ?_test(test_new_or_set_empty()),
     ?_test(test_or_add_single_element()),
     ?_test(test_or_add_multiple_elements()),
     ?_test(test_or_add_duplicate_element()),
     ?_test(test_or_remove_element()),
     ?_test(test_or_remove_nonexistent()),
     ?_test(test_or_contains()),
     ?_test(test_or_elements()),
     ?_test(test_or_size()),
     ?_test(test_or_merge_disjoint()),
     ?_test(test_or_merge_overlapping()),
     ?_test(test_or_merge_with_removes()),
     ?_test(test_or_merge_commutative()),
     ?_test(test_or_merge_associative()),
     ?_test(test_or_merge_idempotent()),
     ?_test(test_or_add_after_remove()),
     ?_test(test_or_concurrent_add_remove())
    ].

%%------------------------------------------------------------------------------
%% G-Counter Tests
%%------------------------------------------------------------------------------

gcounter_test_() ->
    [
     ?_test(test_new_gcounter_empty()),
     ?_test(test_gcounter_increment_by_one()),
     ?_test(test_gcounter_increment_by_n()),
     ?_test(test_gcounter_value()),
     ?_test(test_gcounter_multiple_increments()),
     ?_test(test_gcounter_merge()),
     ?_test(test_gcounter_merge_commutative()),
     ?_test(test_gcounter_merge_associative()),
     ?_test(test_gcounter_merge_idempotent())
    ].

%%------------------------------------------------------------------------------
%% PN-Counter Tests
%%------------------------------------------------------------------------------

pncounter_test_() ->
    [
     ?_test(test_new_pncounter_empty()),
     ?_test(test_pncounter_increment()),
     ?_test(test_pncounter_decrement()),
     ?_test(test_pncounter_increment_decrement()),
     ?_test(test_pncounter_negative_value()),
     ?_test(test_pncounter_merge()),
     ?_test(test_pncounter_merge_commutative()),
     ?_test(test_pncounter_merge_associative())
    ].

%%------------------------------------------------------------------------------
%% Basic LWW-Register Tests
%%------------------------------------------------------------------------------

test_new_lww_register_empty() ->
    Register = macula_crdt:new_lww_register(),

    %% Verify initial state
    ?assertEqual(undefined, macula_crdt:lww_get(Register)),
    ?assertEqual(0, macula_crdt:lww_timestamp(Register)),
    ?assertEqual(node(), maps:get(node, Register)),
    ok.

test_new_lww_register_with_value() ->
    Value = {foo, bar, 123},
    Register = macula_crdt:new_lww_register(Value),

    %% Verify value is set
    ?assertEqual(Value, macula_crdt:lww_get(Register)),

    %% Timestamp should be non-zero
    ?assert(macula_crdt:lww_timestamp(Register) > 0),
    ok.

test_lww_set_updates_value() ->
    R1 = macula_crdt:new_lww_register(),
    NewValue = "hello world",
    Timestamp = erlang:system_time(microsecond),

    R2 = macula_crdt:lww_set(R1, NewValue, Timestamp),

    %% Verify value and timestamp updated
    ?assertEqual(NewValue, macula_crdt:lww_get(R2)),
    ?assertEqual(Timestamp, macula_crdt:lww_timestamp(R2)),
    ok.

test_lww_get_returns_value() ->
    Value = [1, 2, 3],
    Register = macula_crdt:new_lww_register(Value),

    ?assertEqual(Value, macula_crdt:lww_get(Register)),
    ok.

test_lww_timestamp_returns_timestamp() ->
    Register = macula_crdt:new_lww_register(foo),
    Timestamp = macula_crdt:lww_timestamp(Register),

    ?assert(is_integer(Timestamp)),
    ?assert(Timestamp > 0),
    ok.

test_lww_value_alias_for_get() ->
    Value = #{key => value},
    Register = macula_crdt:new_lww_register(Value),

    %% lww_value is an alias for lww_get
    ?assertEqual(macula_crdt:lww_get(Register),
                 macula_crdt:lww_value(Register)),
    ok.

%%------------------------------------------------------------------------------
%% LWW-Register Merge Tests
%%------------------------------------------------------------------------------

test_lww_merge_keeps_higher_timestamp() ->
    R1 = macula_crdt:lww_set(macula_crdt:new_lww_register(), value1, 100),
    R2 = macula_crdt:lww_set(macula_crdt:new_lww_register(), value2, 200),

    %% R2 has higher timestamp, so merge should keep value2
    Merged = macula_crdt:lww_merge(R1, R2),

    ?assertEqual(value2, macula_crdt:lww_get(Merged)),
    ?assertEqual(200, macula_crdt:lww_timestamp(Merged)),
    ok.

test_lww_merge_keeps_lower_timestamp() ->
    R1 = macula_crdt:lww_set(macula_crdt:new_lww_register(), value1, 300),
    R2 = macula_crdt:lww_set(macula_crdt:new_lww_register(), value2, 100),

    %% R1 has higher timestamp, so merge should keep value1
    Merged = macula_crdt:lww_merge(R1, R2),

    ?assertEqual(value1, macula_crdt:lww_get(Merged)),
    ?assertEqual(300, macula_crdt:lww_timestamp(Merged)),
    ok.

test_lww_merge_breaks_ties_by_node() ->
    %% Same timestamp, different nodes
    R1 = #{value => value1, timestamp => 100, node => 'node_a@host'},
    R2 = #{value => value2, timestamp => 100, node => 'node_z@host'},

    %% node_a < node_z lexicographically, so should keep value1
    Merged = macula_crdt:lww_merge(R1, R2),

    ?assertEqual(value1, macula_crdt:lww_get(Merged)),
    ?assertEqual('node_a@host', maps:get(node, Merged)),
    ok.

test_lww_merge_idempotent() ->
    R1 = macula_crdt:new_lww_register(value1),

    %% Merging with itself should return same value
    Merged = macula_crdt:lww_merge(R1, R1),

    ?assertEqual(macula_crdt:lww_get(R1), macula_crdt:lww_get(Merged)),
    ok.

test_lww_merge_commutative() ->
    R1 = macula_crdt:lww_set(macula_crdt:new_lww_register(), value1, 100),
    R2 = macula_crdt:lww_set(macula_crdt:new_lww_register(), value2, 200),

    %% merge(R1, R2) == merge(R2, R1)
    Merged1 = macula_crdt:lww_merge(R1, R2),
    Merged2 = macula_crdt:lww_merge(R2, R1),

    ?assertEqual(macula_crdt:lww_get(Merged1), macula_crdt:lww_get(Merged2)),
    ?assertEqual(macula_crdt:lww_timestamp(Merged1),
                 macula_crdt:lww_timestamp(Merged2)),
    ok.

test_lww_merge_associative() ->
    R1 = macula_crdt:lww_set(macula_crdt:new_lww_register(), value1, 100),
    R2 = macula_crdt:lww_set(macula_crdt:new_lww_register(), value2, 200),
    R3 = macula_crdt:lww_set(macula_crdt:new_lww_register(), value3, 300),

    %% merge(merge(R1, R2), R3) == merge(R1, merge(R2, R3))
    Merged1 = macula_crdt:lww_merge(macula_crdt:lww_merge(R1, R2), R3),
    Merged2 = macula_crdt:lww_merge(R1, macula_crdt:lww_merge(R2, R3)),

    ?assertEqual(macula_crdt:lww_get(Merged1), macula_crdt:lww_get(Merged2)),
    ?assertEqual(macula_crdt:lww_timestamp(Merged1),
                 macula_crdt:lww_timestamp(Merged2)),
    ok.

%%------------------------------------------------------------------------------
%% Real-World Scenario Tests
%%------------------------------------------------------------------------------

test_lww_concurrent_updates() ->
    %% Simulate concurrent updates on different nodes
    %% Node A updates at T=100
    NodeA = #{value => value_from_node_a, timestamp => 100, node => 'node_a@host'},

    %% Node B updates at T=150
    NodeB = #{value => value_from_node_b, timestamp => 150, node => 'node_b@host'},

    %% Merge should resolve to Node B (higher timestamp)
    Merged = macula_crdt:lww_merge(NodeA, NodeB),

    ?assertEqual(value_from_node_b, macula_crdt:lww_get(Merged)),
    ok.

test_lww_sequential_updates() ->
    %% Simulate sequential updates
    R1 = macula_crdt:new_lww_register(v1),
    T1 = macula_crdt:lww_timestamp(R1),

    timer:sleep(1), % Ensure different timestamp

    R2 = macula_crdt:lww_set(R1, v2, erlang:system_time(microsecond)),
    T2 = macula_crdt:lww_timestamp(R2),

    timer:sleep(1),

    R3 = macula_crdt:lww_set(R2, v3, erlang:system_time(microsecond)),
    T3 = macula_crdt:lww_timestamp(R3),

    %% Timestamps should be increasing
    ?assert(T1 < T2),
    ?assert(T2 < T3),

    %% Final value should be v3
    ?assertEqual(v3, macula_crdt:lww_get(R3)),
    ok.

%%------------------------------------------------------------------------------
%% OR-Set Tests Implementation
%%------------------------------------------------------------------------------

test_new_or_set_empty() ->
    Set = macula_crdt:new_or_set(),
    ?assertEqual([], macula_crdt:or_elements(Set)),
    ?assertEqual(0, macula_crdt:or_size(Set)),
    ok.

test_or_add_single_element() ->
    Set = macula_crdt:new_or_set(),
    Set1 = macula_crdt:or_add(Set, foo),
    ?assert(macula_crdt:or_contains(Set1, foo)),
    ?assertEqual(1, macula_crdt:or_size(Set1)),
    ok.

test_or_add_multiple_elements() ->
    Set = macula_crdt:new_or_set(),
    Set1 = macula_crdt:or_add(Set, a),
    Set2 = macula_crdt:or_add(Set1, b),
    Set3 = macula_crdt:or_add(Set2, c),
    ?assert(macula_crdt:or_contains(Set3, a)),
    ?assert(macula_crdt:or_contains(Set3, b)),
    ?assert(macula_crdt:or_contains(Set3, c)),
    ?assertEqual(3, macula_crdt:or_size(Set3)),
    ok.

test_or_add_duplicate_element() ->
    %% Adding same element multiple times should still result in element being present
    Set = macula_crdt:new_or_set(),
    Set1 = macula_crdt:or_add(Set, foo),
    Set2 = macula_crdt:or_add(Set1, foo),
    Set3 = macula_crdt:or_add(Set2, foo),
    ?assert(macula_crdt:or_contains(Set3, foo)),
    %% Element appears once in the list
    Elements = macula_crdt:or_elements(Set3),
    ?assertEqual(1, length([E || E <- Elements, E =:= foo])),
    ok.

test_or_remove_element() ->
    Set = macula_crdt:new_or_set(),
    Set1 = macula_crdt:or_add(Set, foo),
    Set2 = macula_crdt:or_add(Set1, bar),
    ?assert(macula_crdt:or_contains(Set2, foo)),
    ?assert(macula_crdt:or_contains(Set2, bar)),

    %% Remove foo
    Set3 = macula_crdt:or_remove(Set2, foo),
    ?assertNot(macula_crdt:or_contains(Set3, foo)),
    ?assert(macula_crdt:or_contains(Set3, bar)),
    ?assertEqual(1, macula_crdt:or_size(Set3)),
    ok.

test_or_remove_nonexistent() ->
    Set = macula_crdt:new_or_set(),
    Set1 = macula_crdt:or_add(Set, foo),

    %% Removing nonexistent element should be no-op
    Set2 = macula_crdt:or_remove(Set1, bar),
    ?assert(macula_crdt:or_contains(Set2, foo)),
    ?assertEqual(1, macula_crdt:or_size(Set2)),
    ok.

test_or_contains() ->
    Set = macula_crdt:new_or_set(),
    ?assertNot(macula_crdt:or_contains(Set, foo)),

    Set1 = macula_crdt:or_add(Set, foo),
    ?assert(macula_crdt:or_contains(Set1, foo)),
    ?assertNot(macula_crdt:or_contains(Set1, bar)),
    ok.

test_or_elements() ->
    Set = macula_crdt:new_or_set(),
    Set1 = macula_crdt:or_add(Set, a),
    Set2 = macula_crdt:or_add(Set1, b),
    Set3 = macula_crdt:or_add(Set2, c),

    Elements = lists:sort(macula_crdt:or_elements(Set3)),
    ?assertEqual([a, b, c], Elements),
    ok.

test_or_size() ->
    Set = macula_crdt:new_or_set(),
    ?assertEqual(0, macula_crdt:or_size(Set)),

    Set1 = macula_crdt:or_add(Set, a),
    ?assertEqual(1, macula_crdt:or_size(Set1)),

    Set2 = macula_crdt:or_add(Set1, b),
    ?assertEqual(2, macula_crdt:or_size(Set2)),

    Set3 = macula_crdt:or_remove(Set2, a),
    ?assertEqual(1, macula_crdt:or_size(Set3)),
    ok.

test_or_merge_disjoint() ->
    Set1 = macula_crdt:or_add(macula_crdt:new_or_set(), a),
    Set2 = macula_crdt:or_add(macula_crdt:new_or_set(), b),

    Merged = macula_crdt:or_merge(Set1, Set2),
    ?assert(macula_crdt:or_contains(Merged, a)),
    ?assert(macula_crdt:or_contains(Merged, b)),
    ?assertEqual(2, macula_crdt:or_size(Merged)),
    ok.

test_or_merge_overlapping() ->
    Set1 = macula_crdt:or_add(macula_crdt:or_add(macula_crdt:new_or_set(), a), b),
    Set2 = macula_crdt:or_add(macula_crdt:or_add(macula_crdt:new_or_set(), b), c),

    Merged = macula_crdt:or_merge(Set1, Set2),
    Elements = lists:sort(macula_crdt:or_elements(Merged)),
    ?assertEqual([a, b, c], Elements),
    ok.

test_or_merge_with_removes() ->
    %% Set1: add a, add b
    Set1 = macula_crdt:or_add(macula_crdt:or_add(macula_crdt:new_or_set(), a), b),

    %% Set2: add a, remove a
    Set2_0 = macula_crdt:or_add(macula_crdt:new_or_set(), a),
    Set2 = macula_crdt:or_remove(Set2_0, a),

    %% After merge, 'a' should NOT be present (tombstone wins)
    %% because Set2's remove tombstones Set2's add tag
    %% But Set1's add tag for 'a' is different, so 'a' should be present
    Merged = macula_crdt:or_merge(Set1, Set2),
    %% Set1's tag for 'a' is not tombstoned by Set2's remove
    ?assert(macula_crdt:or_contains(Merged, a)),
    ?assert(macula_crdt:or_contains(Merged, b)),
    ok.

test_or_merge_commutative() ->
    Set1 = macula_crdt:or_add(macula_crdt:new_or_set(), a),
    Set2 = macula_crdt:or_add(macula_crdt:new_or_set(), b),

    Merged1 = macula_crdt:or_merge(Set1, Set2),
    Merged2 = macula_crdt:or_merge(Set2, Set1),

    ?assertEqual(lists:sort(macula_crdt:or_elements(Merged1)),
                 lists:sort(macula_crdt:or_elements(Merged2))),
    ok.

test_or_merge_associative() ->
    Set1 = macula_crdt:or_add(macula_crdt:new_or_set(), a),
    Set2 = macula_crdt:or_add(macula_crdt:new_or_set(), b),
    Set3 = macula_crdt:or_add(macula_crdt:new_or_set(), c),

    Merged1 = macula_crdt:or_merge(macula_crdt:or_merge(Set1, Set2), Set3),
    Merged2 = macula_crdt:or_merge(Set1, macula_crdt:or_merge(Set2, Set3)),

    ?assertEqual(lists:sort(macula_crdt:or_elements(Merged1)),
                 lists:sort(macula_crdt:or_elements(Merged2))),
    ok.

test_or_merge_idempotent() ->
    Set = macula_crdt:or_add(macula_crdt:or_add(macula_crdt:new_or_set(), a), b),

    Merged = macula_crdt:or_merge(Set, Set),

    ?assertEqual(lists:sort(macula_crdt:or_elements(Set)),
                 lists:sort(macula_crdt:or_elements(Merged))),
    ok.

test_or_add_after_remove() ->
    %% Add element, remove it, add it again
    Set = macula_crdt:new_or_set(),
    Set1 = macula_crdt:or_add(Set, foo),
    Set2 = macula_crdt:or_remove(Set1, foo),
    ?assertNot(macula_crdt:or_contains(Set2, foo)),

    %% Add again - should succeed (new tag)
    Set3 = macula_crdt:or_add(Set2, foo),
    ?assert(macula_crdt:or_contains(Set3, foo)),
    ok.

test_or_concurrent_add_remove() ->
    %% Simulate concurrent operations on different replicas
    %% Replica A: add x
    SetA = macula_crdt:or_add(macula_crdt:new_or_set(), x),

    %% Replica B starts from same state, adds x, then removes x
    SetB_0 = macula_crdt:or_add(macula_crdt:new_or_set(), x),
    SetB = macula_crdt:or_remove(SetB_0, x),

    %% Merge: A's add should survive (add-wins semantics)
    %% because A's tag is different from B's tombstoned tag
    Merged = macula_crdt:or_merge(SetA, SetB),
    ?assert(macula_crdt:or_contains(Merged, x)),
    ok.

%%------------------------------------------------------------------------------
%% G-Counter Tests Implementation
%%------------------------------------------------------------------------------

test_new_gcounter_empty() ->
    Counter = macula_crdt:new_gcounter(),
    ?assertEqual(0, macula_crdt:gcounter_value(Counter)),
    ok.

test_gcounter_increment_by_one() ->
    Counter = macula_crdt:new_gcounter(),
    Counter1 = macula_crdt:gcounter_increment(Counter),
    ?assertEqual(1, macula_crdt:gcounter_value(Counter1)),
    ok.

test_gcounter_increment_by_n() ->
    Counter = macula_crdt:new_gcounter(),
    Counter1 = macula_crdt:gcounter_increment(Counter, 5),
    ?assertEqual(5, macula_crdt:gcounter_value(Counter1)),
    ok.

test_gcounter_value() ->
    Counter = macula_crdt:new_gcounter(),
    Counter1 = macula_crdt:gcounter_increment(Counter, 3),
    Counter2 = macula_crdt:gcounter_increment(Counter1, 7),
    ?assertEqual(10, macula_crdt:gcounter_value(Counter2)),
    ok.

test_gcounter_multiple_increments() ->
    Counter = macula_crdt:new_gcounter(),
    Counter1 = macula_crdt:gcounter_increment(Counter),
    Counter2 = macula_crdt:gcounter_increment(Counter1),
    Counter3 = macula_crdt:gcounter_increment(Counter2),
    Counter4 = macula_crdt:gcounter_increment(Counter3),
    Counter5 = macula_crdt:gcounter_increment(Counter4),
    ?assertEqual(5, macula_crdt:gcounter_value(Counter5)),
    ok.

test_gcounter_merge() ->
    %% Simulate counters from different nodes
    %% Node A incremented by 3
    CounterA = #{nodeA => 3},
    %% Node B incremented by 5
    CounterB = #{nodeB => 5},

    Merged = macula_crdt:gcounter_merge(CounterA, CounterB),
    ?assertEqual(8, macula_crdt:gcounter_value(Merged)),
    ok.

test_gcounter_merge_commutative() ->
    CounterA = #{nodeA => 3},
    CounterB = #{nodeB => 5},

    Merged1 = macula_crdt:gcounter_merge(CounterA, CounterB),
    Merged2 = macula_crdt:gcounter_merge(CounterB, CounterA),

    ?assertEqual(macula_crdt:gcounter_value(Merged1),
                 macula_crdt:gcounter_value(Merged2)),
    ok.

test_gcounter_merge_associative() ->
    CounterA = #{nodeA => 3},
    CounterB = #{nodeB => 5},
    CounterC = #{nodeC => 7},

    Merged1 = macula_crdt:gcounter_merge(
                macula_crdt:gcounter_merge(CounterA, CounterB), CounterC),
    Merged2 = macula_crdt:gcounter_merge(
                CounterA, macula_crdt:gcounter_merge(CounterB, CounterC)),

    ?assertEqual(macula_crdt:gcounter_value(Merged1),
                 macula_crdt:gcounter_value(Merged2)),
    ok.

test_gcounter_merge_idempotent() ->
    Counter = #{nodeA => 5, nodeB => 3},

    Merged = macula_crdt:gcounter_merge(Counter, Counter),

    ?assertEqual(macula_crdt:gcounter_value(Counter),
                 macula_crdt:gcounter_value(Merged)),
    ok.

%%------------------------------------------------------------------------------
%% PN-Counter Tests Implementation
%%------------------------------------------------------------------------------

test_new_pncounter_empty() ->
    Counter = macula_crdt:new_pncounter(),
    ?assertEqual(0, macula_crdt:pncounter_value(Counter)),
    ok.

test_pncounter_increment() ->
    Counter = macula_crdt:new_pncounter(),
    Counter1 = macula_crdt:pncounter_increment(Counter),
    ?assertEqual(1, macula_crdt:pncounter_value(Counter1)),

    Counter2 = macula_crdt:pncounter_increment(Counter1, 5),
    ?assertEqual(6, macula_crdt:pncounter_value(Counter2)),
    ok.

test_pncounter_decrement() ->
    Counter = macula_crdt:new_pncounter(),
    Counter1 = macula_crdt:pncounter_increment(Counter, 10),
    Counter2 = macula_crdt:pncounter_decrement(Counter1),
    ?assertEqual(9, macula_crdt:pncounter_value(Counter2)),

    Counter3 = macula_crdt:pncounter_decrement(Counter2, 3),
    ?assertEqual(6, macula_crdt:pncounter_value(Counter3)),
    ok.

test_pncounter_increment_decrement() ->
    Counter = macula_crdt:new_pncounter(),
    Counter1 = macula_crdt:pncounter_increment(Counter, 5),
    Counter2 = macula_crdt:pncounter_decrement(Counter1, 2),
    Counter3 = macula_crdt:pncounter_increment(Counter2, 3),
    Counter4 = macula_crdt:pncounter_decrement(Counter3, 1),

    %% 5 - 2 + 3 - 1 = 5
    ?assertEqual(5, macula_crdt:pncounter_value(Counter4)),
    ok.

test_pncounter_negative_value() ->
    Counter = macula_crdt:new_pncounter(),
    Counter1 = macula_crdt:pncounter_decrement(Counter, 5),
    ?assertEqual(-5, macula_crdt:pncounter_value(Counter1)),
    ok.

test_pncounter_merge() ->
    %% Node A: +10
    CounterA = macula_crdt:pncounter_increment(macula_crdt:new_pncounter(), 10),
    %% Node B: -3
    CounterB = macula_crdt:pncounter_decrement(macula_crdt:new_pncounter(), 3),

    Merged = macula_crdt:pncounter_merge(CounterA, CounterB),
    %% 10 - 3 = 7
    ?assertEqual(7, macula_crdt:pncounter_value(Merged)),
    ok.

test_pncounter_merge_commutative() ->
    CounterA = macula_crdt:pncounter_increment(macula_crdt:new_pncounter(), 5),
    CounterB = macula_crdt:pncounter_decrement(macula_crdt:new_pncounter(), 2),

    Merged1 = macula_crdt:pncounter_merge(CounterA, CounterB),
    Merged2 = macula_crdt:pncounter_merge(CounterB, CounterA),

    ?assertEqual(macula_crdt:pncounter_value(Merged1),
                 macula_crdt:pncounter_value(Merged2)),
    ok.

test_pncounter_merge_associative() ->
    CounterA = macula_crdt:pncounter_increment(macula_crdt:new_pncounter(), 5),
    CounterB = macula_crdt:pncounter_decrement(macula_crdt:new_pncounter(), 2),
    CounterC = macula_crdt:pncounter_increment(macula_crdt:new_pncounter(), 3),

    Merged1 = macula_crdt:pncounter_merge(
                macula_crdt:pncounter_merge(CounterA, CounterB), CounterC),
    Merged2 = macula_crdt:pncounter_merge(
                CounterA, macula_crdt:pncounter_merge(CounterB, CounterC)),

    ?assertEqual(macula_crdt:pncounter_value(Merged1),
                 macula_crdt:pncounter_value(Merged2)),
    ok.
