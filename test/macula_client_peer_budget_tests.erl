%% EUnit tests for `macula_client_peer_budget'. A pool links to at most `budget' new peers per window: a peer counts
%% once per window, its count ends when the window does, and an exempt peer (a configured seed) never spends the budget.
-module(macula_client_peer_budget_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1789000000000).
-define(MINUTE, 60000).
-define(WINDOW, 15 * ?MINUTE).

a_new_peer_within_the_budget_is_allowed_test() ->
    ?assertMatch({ok, _}, macula_client_peer_budget:spend(budget(2), peer(1), ?NOW)).

a_spent_budget_refuses_another_new_peer_test() ->
    {ok, B1} = macula_client_peer_budget:spend(budget(2), peer(1), ?NOW),
    {ok, B2} = macula_client_peer_budget:spend(B1, peer(2), ?NOW + 1),
    ?assertMatch({spent, _}, macula_client_peer_budget:spend(B2, peer(3), ?NOW + 2)).

a_refused_peer_is_not_counted_test() ->
    {ok, B1} = macula_client_peer_budget:spend(budget(1), peer(1), ?NOW),
    {spent, B2} = macula_client_peer_budget:spend(B1, peer(2), ?NOW + 1),
    ?assertEqual(1, macula_client_peer_budget:counted(B2, ?NOW + 1)),
    ?assertMatch({ok, _}, macula_client_peer_budget:spend(B2, peer(1), ?NOW + 2)).

%% A peer already counted spends nothing more in its window, and seeing it again does not extend its window.
a_peer_counts_once_per_window_test() ->
    {ok, B1} = macula_client_peer_budget:spend(budget(1), peer(1), ?NOW),
    {ok, B2} = macula_client_peer_budget:spend(B1, peer(1), ?NOW + ?MINUTE),
    ?assertMatch({spent, _}, macula_client_peer_budget:spend(B2, peer(2), ?NOW + ?WINDOW - 1)),
    ?assertMatch({ok, _}, macula_client_peer_budget:spend(B2, peer(2), ?NOW + ?WINDOW)).

a_peer_counted_again_after_its_window_spends_again_test() ->
    {ok, B1} = macula_client_peer_budget:spend(budget(1), peer(1), ?NOW),
    {ok, B2} = macula_client_peer_budget:spend(B1, peer(1), ?NOW + ?WINDOW),
    ?assertMatch({spent, _}, macula_client_peer_budget:spend(B2, peer(2), ?NOW + ?WINDOW + 1)).

%% A configured seed sits outside the budget: it is allowed when the budget is spent, and it takes no count.
an_exempt_peer_never_spends_the_budget_test() ->
    Budget = macula_client_peer_budget:new(#{budget => 1, window_ms => ?WINDOW, exempt => [seed(1)]}),
    {ok, B1} = macula_client_peer_budget:spend(Budget, peer(1), ?NOW),
    {ok, B2} = macula_client_peer_budget:spend(B1, seed(1), ?NOW + 1),
    ?assertEqual(1, macula_client_peer_budget:counted(B2, ?NOW + 1)),
    ?assertMatch({spent, _}, macula_client_peer_budget:spend(B2, seed(2), ?NOW + 2)).

%% New peers arriving exactly at the budget's rate, one every window divided by the budget, are all allowed, and the
%% peers counted in any window never exceed the budget.
new_peers_at_the_highest_allowed_rate_stay_within_the_budget_test() ->
    Budget = 4,
    Step = ?WINDOW div Budget,
    Final = lists:foldl(
              fun(N, B) ->
                      Now = ?NOW + N * Step,
                      {ok, Next} = macula_client_peer_budget:spend(B, peer(N), Now),
                      ?assert(macula_client_peer_budget:counted(Next, Now) =< Budget),
                      Next
              end, budget(Budget), lists:seq(0, 3 * Budget)),
    ?assertEqual(Budget, macula_client_peer_budget:counted(Final, ?NOW + 3 * Budget * Step)).

a_new_peer_faster_than_the_rate_is_refused_until_a_count_ends_test() ->
    Full = lists:foldl(fun(N, B) ->
                               {ok, Next} = macula_client_peer_budget:spend(B, peer(N), ?NOW + N),
                               Next
                       end, budget(4), [1, 2, 3, 4]),
    ?assertMatch({spent, _}, macula_client_peer_budget:spend(Full, peer(5), ?NOW + 5)),
    ?assertMatch({ok, _}, macula_client_peer_budget:spend(Full, peer(5), ?NOW + 1 + ?WINDOW)).

a_budget_or_window_that_is_not_positive_is_refused_test() ->
    ?assertError(function_clause, macula_client_peer_budget:new(#{budget => 0, window_ms => ?WINDOW})),
    ?assertError(function_clause, macula_client_peer_budget:new(#{budget => 1, window_ms => 0})).

%%---------------------------------------------------------------------
%% Helpers
%%---------------------------------------------------------------------

budget(Budget) ->
    macula_client_peer_budget:new(#{budget => Budget, window_ms => ?WINDOW}).

peer(N) -> {node_id, <<N:256>>}.

seed(N) -> {seed, #{host => <<"station", (integer_to_binary(N))/binary, ".example">>, port => 4433}}.
