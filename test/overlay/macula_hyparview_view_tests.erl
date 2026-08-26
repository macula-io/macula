%% EUnit tests for macula_hyparview_view.
-module(macula_hyparview_view_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Construction
%%---------------------------------------------------------------------

new_default_caps_test() ->
    V = macula_hyparview_view:new(id(0)),
    ?assertEqual(5,  macula_hyparview_view:active_cap(V)),
    ?assertEqual(20, macula_hyparview_view:passive_cap(V)),
    ?assertEqual(0,  macula_hyparview_view:active_size(V)),
    ?assertEqual(0,  macula_hyparview_view:passive_size(V)),
    ?assertEqual(id(0), macula_hyparview_view:self_id(V)).

new_custom_caps_test() ->
    V = macula_hyparview_view:new(id(0), #{active_cap => 8,
                                         passive_cap => 32}),
    ?assertEqual(8,  macula_hyparview_view:active_cap(V)),
    ?assertEqual(32, macula_hyparview_view:passive_cap(V)).

new_zero_active_cap_rejected_test() ->
    ?assertError(function_clause,
                 macula_hyparview_view:new(id(0), #{active_cap => 0})).

new_passive_cap_below_active_rejected_test() ->
    ?assertError(function_clause,
                 macula_hyparview_view:new(id(0), #{active_cap => 5,
                                                  passive_cap => 3})).

%%---------------------------------------------------------------------
%% Active view basics
%%---------------------------------------------------------------------

add_active_first_peer_test() ->
    V = macula_hyparview_view:add_active(macula_hyparview_view:new(id(0)),
                                       id(1)),
    ?assert(macula_hyparview_view:is_active(id(1), V)),
    ?assertEqual(1, macula_hyparview_view:active_size(V)).

add_active_self_is_noop_test() ->
    V = macula_hyparview_view:add_active(macula_hyparview_view:new(id(0)),
                                       id(0)),
    ?assertEqual(0, macula_hyparview_view:active_size(V)).

add_active_idempotent_test() ->
    V0 = macula_hyparview_view:add_active(macula_hyparview_view:new(id(0)),
                                        id(1)),
    V1 = macula_hyparview_view:add_active(V0, id(1)),
    ?assertEqual(1, macula_hyparview_view:active_size(V1)).

%%---------------------------------------------------------------------
%% Active eviction → passive
%%---------------------------------------------------------------------

add_active_at_capacity_demotes_random_to_passive_test() ->
    V0 = macula_hyparview_view:new(id(0), #{active_cap => 2,
                                          passive_cap => 4}),
    V1 = macula_hyparview_view:add_active(V0, id(1)),
    V2 = macula_hyparview_view:add_active(V1, id(2)),
    %% At capacity. Adding a third forces an eviction.
    V3 = macula_hyparview_view:add_active(V2, id(3)),
    ?assertEqual(2, macula_hyparview_view:active_size(V3)),
    ?assertEqual(1, macula_hyparview_view:passive_size(V3)),
    %% The new peer is in active.
    ?assert(macula_hyparview_view:is_active(id(3), V3)),
    %% The total set across both views still has all three peers.
    Combined = sets:from_list(macula_hyparview_view:active(V3) ++
                              macula_hyparview_view:passive(V3)),
    ?assertEqual(sets:from_list([id(1), id(2), id(3)]), Combined).

%%---------------------------------------------------------------------
%% Passive view basics
%%---------------------------------------------------------------------

add_passive_first_peer_test() ->
    V = macula_hyparview_view:add_passive(macula_hyparview_view:new(id(0)),
                                        id(1)),
    ?assert(macula_hyparview_view:is_passive(id(1), V)),
    ?assertEqual(1, macula_hyparview_view:passive_size(V)).

add_passive_self_is_noop_test() ->
    V = macula_hyparview_view:add_passive(macula_hyparview_view:new(id(0)),
                                        id(0)),
    ?assertEqual(0, macula_hyparview_view:passive_size(V)).

add_passive_for_active_peer_is_noop_test() ->
    V0 = macula_hyparview_view:add_active(macula_hyparview_view:new(id(0)),
                                        id(1)),
    V1 = macula_hyparview_view:add_passive(V0, id(1)),
    ?assertEqual(1, macula_hyparview_view:active_size(V1)),
    ?assertEqual(0, macula_hyparview_view:passive_size(V1)).

add_passive_at_capacity_evicts_random_test() ->
    V0 = macula_hyparview_view:new(id(0), #{active_cap => 1,
                                          passive_cap => 2}),
    V1 = macula_hyparview_view:add_passive(V0, id(10)),
    V2 = macula_hyparview_view:add_passive(V1, id(11)),
    V3 = macula_hyparview_view:add_passive(V2, id(12)),
    ?assertEqual(2, macula_hyparview_view:passive_size(V3)),
    ?assert(macula_hyparview_view:is_passive(id(12), V3)).

%%---------------------------------------------------------------------
%% Promotion
%%---------------------------------------------------------------------

promote_passive_to_active_test() ->
    V0 = macula_hyparview_view:add_passive(macula_hyparview_view:new(id(0)),
                                         id(1)),
    V1 = macula_hyparview_view:promote(V0, id(1)),
    ?assert(macula_hyparview_view:is_active(id(1), V1)),
    ?assertNot(macula_hyparview_view:is_passive(id(1), V1)).

add_active_for_passive_peer_promotes_test() ->
    V0 = macula_hyparview_view:add_passive(macula_hyparview_view:new(id(0)),
                                         id(7)),
    V1 = macula_hyparview_view:add_active(V0, id(7)),
    ?assert(macula_hyparview_view:is_active(id(7), V1)),
    ?assertNot(macula_hyparview_view:is_passive(id(7), V1)).

%%---------------------------------------------------------------------
%% Demotion
%%---------------------------------------------------------------------

demote_moves_active_to_passive_test() ->
    V0 = macula_hyparview_view:add_active(macula_hyparview_view:new(id(0)),
                                        id(5)),
    V1 = macula_hyparview_view:demote(V0, id(5)),
    ?assertNot(macula_hyparview_view:is_active(id(5), V1)),
    ?assert(macula_hyparview_view:is_passive(id(5), V1)).

demote_unknown_peer_is_noop_test() ->
    V = macula_hyparview_view:demote(macula_hyparview_view:new(id(0)),
                                   id(99)),
    ?assertEqual(0, macula_hyparview_view:active_size(V)),
    ?assertEqual(0, macula_hyparview_view:passive_size(V)).

%%---------------------------------------------------------------------
%% Removal
%%---------------------------------------------------------------------

remove_active_drops_peer_test() ->
    V0 = macula_hyparview_view:add_active(macula_hyparview_view:new(id(0)),
                                        id(1)),
    V1 = macula_hyparview_view:remove_active(V0, id(1)),
    ?assertNot(macula_hyparview_view:is_active(id(1), V1)).

remove_passive_drops_peer_test() ->
    V0 = macula_hyparview_view:add_passive(macula_hyparview_view:new(id(0)),
                                         id(2)),
    V1 = macula_hyparview_view:remove_passive(V0, id(2)),
    ?assertNot(macula_hyparview_view:is_passive(id(2), V1)).

%%---------------------------------------------------------------------
%% Sampling
%%---------------------------------------------------------------------

random_active_on_empty_returns_empty_test() ->
    ?assertEqual(empty,
                 macula_hyparview_view:random_active(
                   macula_hyparview_view:new(id(0)))).

random_active_returns_member_test() ->
    V = lists:foldl(fun(N, A) ->
                            macula_hyparview_view:add_active(A, id(N))
                    end,
                    macula_hyparview_view:new(id(0), #{active_cap => 5,
                                                     passive_cap => 20}),
                    [1, 2, 3]),
    {ok, P} = macula_hyparview_view:random_active(V),
    ?assert(lists:member(P, [id(1), id(2), id(3)])).

random_subset_zero_returns_empty_test() ->
    V = macula_hyparview_view:add_active(macula_hyparview_view:new(id(0)),
                                       id(1)),
    ?assertEqual([], macula_hyparview_view:random_active_subset(V, 0)).

random_subset_caps_at_size_test() ->
    V = lists:foldl(fun(N, A) ->
                            macula_hyparview_view:add_passive(A, id(N))
                    end, macula_hyparview_view:new(id(0)),
                    [1, 2, 3]),
    Sample = macula_hyparview_view:random_passive_subset(V, 10),
    ?assertEqual(3, length(Sample)).

%%---------------------------------------------------------------------
%% Shuffle merge
%%---------------------------------------------------------------------

merge_shuffle_adds_new_peers_to_passive_test() ->
    V0 = macula_hyparview_view:new(id(0), #{active_cap => 2,
                                          passive_cap => 10}),
    V1 = macula_hyparview_view:merge_shuffle(V0,
                                           [id(1), id(2), id(3)]),
    ?assertEqual(3, macula_hyparview_view:passive_size(V1)).

merge_shuffle_filters_self_and_known_test() ->
    V0 = macula_hyparview_view:add_active(macula_hyparview_view:new(id(0)),
                                        id(1)),
    V1 = macula_hyparview_view:merge_shuffle(V0,
                                           [id(0), id(1), id(2)]),
    %% Self filtered, known active filtered, only id(2) admitted.
    ?assertEqual(1, macula_hyparview_view:passive_size(V1)),
    ?assert(macula_hyparview_view:is_passive(id(2), V1)),
    ?assertNot(macula_hyparview_view:is_passive(id(1), V1)).

%%---------------------------------------------------------------------
%% Counts helper
%%---------------------------------------------------------------------

counts_reflect_view_state_test() ->
    V0 = macula_hyparview_view:add_active(macula_hyparview_view:new(id(0)),
                                        id(1)),
    V1 = macula_hyparview_view:add_passive(V0, id(2)),
    ?assertEqual(#{active => 1, passive => 1},
                 macula_hyparview_view:counts(V1)).

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

id(N) -> <<N:256>>.
