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
