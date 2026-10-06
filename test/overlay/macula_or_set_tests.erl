%% EUnit tests for macula_or_set.
-module(macula_or_set_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Construction
%%---------------------------------------------------------------------

new_is_empty_test() ->
    S = macula_or_set:new(),
    ?assert(macula_or_set:is_empty(S)),
    ?assertEqual(0, macula_or_set:size(S)),
    ?assertEqual([], macula_or_set:members(S)),
    ?assertEqual(0, macula_or_set:tombstone_count(S)).

%%---------------------------------------------------------------------
%% Add / contains / members
%%---------------------------------------------------------------------

add_inserts_element_test() ->
    {S1, _Delta} = macula_or_set:add(macula_or_set:new(), alice),
    ?assert(macula_or_set:contains(S1, alice)),
    ?assertEqual([alice], macula_or_set:members(S1)).

add_returns_delta_with_fresh_tag_test() ->
    {_S1, {add, alice, T1}} = macula_or_set:add(macula_or_set:new(), alice),
    {_S2, {add, alice, T2}} = macula_or_set:add(macula_or_set:new(), alice),
    ?assertEqual(16, byte_size(T1)),
    ?assertEqual(16, byte_size(T2)),
    ?assertNotEqual(T1, T2).

add_same_element_twice_keeps_one_member_two_tags_test() ->
    {S1, _} = macula_or_set:add(macula_or_set:new(), alice),
    {S2, _} = macula_or_set:add(S1, alice),
    ?assertEqual([alice], macula_or_set:members(S2)),
    ?assertEqual(2, length(macula_or_set:tags_for(S2, alice))).

%%---------------------------------------------------------------------
%% Remove
%%---------------------------------------------------------------------

remove_drops_element_test() ->
    {S1, _} = macula_or_set:add(macula_or_set:new(), alice),
    {S2, _} = macula_or_set:remove(S1, alice),
    ?assertNot(macula_or_set:contains(S2, alice)),
    ?assertEqual([], macula_or_set:members(S2)).

remove_tombstones_observed_tags_test() ->
    {S1, {add, _, T}} = macula_or_set:add(macula_or_set:new(), alice),
    {S2, {remove, [T2]}} = macula_or_set:remove(S1, alice),
    ?assertEqual(T, T2),
    ?assertEqual(1, macula_or_set:tombstone_count(S2)).

remove_unknown_element_is_noop_with_empty_delta_test() ->
    {S1, {remove, []}} =
        macula_or_set:remove(macula_or_set:new(), nobody),
    ?assertEqual(0, macula_or_set:tombstone_count(S1)).

%%---------------------------------------------------------------------
%% Concurrent add + remove (the OR-Set hallmark)
%%---------------------------------------------------------------------

concurrent_add_and_remove_keeps_element_test() ->
    %% Replica A adds alice. Replica B has not seen the add and
    %% issues a remove (no tags to tombstone). Merging keeps alice.
    {A1, _} = macula_or_set:add(macula_or_set:new(), alice),
    {B1, _} = macula_or_set:remove(macula_or_set:new(), alice),
    Merged = macula_or_set:merge(A1, B1),
    ?assert(macula_or_set:contains(Merged, alice)).

later_remove_after_observed_add_drops_element_test() ->
    %% A adds → broadcasts to B → both have alice. B then removes
    %% (tombstones the observed tag). Merging drops alice.
    {A1, Delta} = macula_or_set:add(macula_or_set:new(), alice),
    B1 = macula_or_set:apply_delta(macula_or_set:new(), Delta),
    {B2, _} = macula_or_set:remove(B1, alice),
    Merged = macula_or_set:merge(A1, B2),
    ?assertNot(macula_or_set:contains(Merged, alice)).

readd_after_remove_yields_element_present_test() ->
    %% Add → remove → add → contains.
    {S1, _} = macula_or_set:add(macula_or_set:new(), alice),
    {S2, _} = macula_or_set:remove(S1, alice),
    {S3, _} = macula_or_set:add(S2, alice),
    ?assert(macula_or_set:contains(S3, alice)).

two_replicas_each_add_then_one_removes_keeps_other_test() ->
    %% A adds with tag Ta; B independently adds with tag Tb. Merge
    %% has both tags. A removes (tombstones Ta only). B re-merges
    %% with A; alice remains because Tb is still live.
    {A1, _} = macula_or_set:add(macula_or_set:new(), alice),
    {B1, _} = macula_or_set:add(macula_or_set:new(), alice),
    AB = macula_or_set:merge(A1, B1),
    ?assert(macula_or_set:contains(AB, alice)),
    {A2, _} = macula_or_set:remove(A1, alice),
    Merged = macula_or_set:merge(A2, B1),
    ?assert(macula_or_set:contains(Merged, alice)).

%%---------------------------------------------------------------------
%% Merge basics
%%---------------------------------------------------------------------

merge_is_commutative_test() ->
    {A1, _} = macula_or_set:add(macula_or_set:new(), alice),
    {B1, _} = macula_or_set:add(macula_or_set:new(), bob),
    ?assertEqual(macula_or_set:merge(A1, B1),
                 macula_or_set:merge(B1, A1)).

merge_is_idempotent_test() ->
    {S, _} = macula_or_set:add(macula_or_set:new(), alice),
    Once  = macula_or_set:merge(S, S),
    Twice = macula_or_set:merge(Once, Once),
    ?assertEqual(Once, Twice).

merge_is_associative_test() ->
    {A, _} = macula_or_set:add(macula_or_set:new(), x),
    {B, _} = macula_or_set:add(macula_or_set:new(), y),
    {C, _} = macula_or_set:add(macula_or_set:new(), z),
    Left  = macula_or_set:merge(macula_or_set:merge(A, B), C),
    Right = macula_or_set:merge(A, macula_or_set:merge(B, C)),
    ?assertEqual(Left, Right).

%%---------------------------------------------------------------------
%% Delta application
%%---------------------------------------------------------------------

apply_add_delta_brings_element_in_test() ->
    {_, Delta} = macula_or_set:add(macula_or_set:new(), alice),
    S = macula_or_set:apply_delta(macula_or_set:new(), Delta),
    ?assert(macula_or_set:contains(S, alice)).

apply_remove_delta_drops_element_test() ->
    {S1, AddD}    = macula_or_set:add(macula_or_set:new(), alice),
    {_, RemD}     = macula_or_set:remove(S1, alice),
    Replica = macula_or_set:apply_delta(macula_or_set:new(), AddD),
    Final   = macula_or_set:apply_delta(Replica, RemD),
    ?assertNot(macula_or_set:contains(Final, alice)).

apply_delta_is_idempotent_test() ->
    {_, AddD} = macula_or_set:add(macula_or_set:new(), alice),
    Once  = macula_or_set:apply_delta(macula_or_set:new(), AddD),
    Twice = macula_or_set:apply_delta(Once, AddD),
    ?assertEqual(Once, Twice).

delayed_add_after_remove_does_not_resurrect_element_test() ->
    %% A adds (broadcasts AddDelta). B applies. B removes
    %% (broadcasts RemDelta). A applies the remove. Then a delayed
    %% AddDelta arrives at A again — it must NOT resurrect alice.
    {A1, AddD} = macula_or_set:add(macula_or_set:new(), alice),
    {_,  RemD} = macula_or_set:remove(A1, alice),
    %% Re-apply remove to a fresh replica, then re-apply the add.
    R0 = macula_or_set:apply_delta(macula_or_set:new(), AddD),
    R1 = macula_or_set:apply_delta(R0, RemD),
    R2 = macula_or_set:apply_delta(R1, AddD),
    ?assertNot(macula_or_set:contains(R2, alice)).

%%---------------------------------------------------------------------
%% Inspection helpers
%%---------------------------------------------------------------------

size_reflects_distinct_members_test() ->
    {S1, _} = macula_or_set:add(macula_or_set:new(), alice),
    {S2, _} = macula_or_set:add(S1, bob),
    {S3, _} = macula_or_set:add(S2, alice),
    ?assertEqual(2, macula_or_set:size(S3)).

tags_for_unknown_returns_empty_test() ->
    ?assertEqual([], macula_or_set:tags_for(macula_or_set:new(),
                                            stranger)).
