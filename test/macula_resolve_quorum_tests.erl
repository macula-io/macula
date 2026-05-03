%%%-------------------------------------------------------------------
%%% @doc Phase 4.4 §4 acceptance — resolve_quorum eclipse mitigation.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_resolve_quorum_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEY, <<0:256>>).

quorum_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [
        fun three_of_three_consistent_returns_record/1,
        fun two_of_three_majority_wins/1,
        fun no_majority_flags_eclipse/1,
        fun all_errors_insufficient_responses/1,
        fun single_endpoint_passthrough/1,
        fun timeout_under_threshold_insufficient/1
     ]}.

setup() ->
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = macula_metrics:start_link(#{install_default_gauges => false}),
    macula_metrics:reset_all(),
    ok.

teardown(_) ->
    case whereis(macula_metrics) of
        undefined -> ok;
        _ -> macula_metrics:stop()
    end,
    ok.

%% --- §4 #1 ---------------------------------------------------------

three_of_three_consistent_returns_record(_) ->
    fun() ->
        Record = make_record(<<"sig-A">>),
        Fns = [const_fn({ok, Record}) || _ <- lists:seq(1, 3)],
        Wrapped = macula_resolve_quorum:wrap(Fns, #{threshold => 2}),
        ?assertEqual({ok, Record}, Wrapped(?KEY)),
        ?assertEqual(1, counter(<<"consistent">>))
    end.

%% --- §4 #2 ---------------------------------------------------------

two_of_three_majority_wins(_) ->
    fun() ->
        RecMajority = make_record(<<"sig-majority">>),
        RecAttacker = make_record(<<"sig-attacker">>),
        Fns = [
            const_fn({ok, RecMajority}),
            const_fn({ok, RecMajority}),
            const_fn({ok, RecAttacker})
        ],
        Wrapped = macula_resolve_quorum:wrap(Fns, #{threshold => 2}),
        ?assertEqual({ok, RecMajority}, Wrapped(?KEY)),
        ?assertEqual(1, counter(<<"consistent">>))
    end.

%% --- §4 #3 ---------------------------------------------------------

no_majority_flags_eclipse(_) ->
    fun() ->
        Fns = [
            const_fn({ok, make_record(<<"sig-A">>)}),
            const_fn({ok, make_record(<<"sig-B">>)}),
            const_fn({ok, make_record(<<"sig-C">>)})
        ],
        Wrapped = macula_resolve_quorum:wrap(Fns, #{threshold => 2}),
        ?assertEqual({error, eclipse_suspected}, Wrapped(?KEY)),
        ?assertEqual(1, counter(<<"disagreement">>))
    end.

%% --- §4 #4 ---------------------------------------------------------

all_errors_insufficient_responses(_) ->
    fun() ->
        Fns = [const_fn({error, not_found}) || _ <- lists:seq(1, 3)],
        Wrapped = macula_resolve_quorum:wrap(Fns, #{threshold => 2}),
        ?assertEqual({error, insufficient_responses}, Wrapped(?KEY)),
        ?assertEqual(1, counter(<<"insufficient_responses">>))
    end.

%% --- §4 #5 ---------------------------------------------------------

single_endpoint_passthrough(_) ->
    fun() ->
        Record = make_record(<<"sig-only">>),
        Wrapped = macula_resolve_quorum:wrap([const_fn({ok, Record})], #{}),
        ?assertEqual({ok, Record}, Wrapped(?KEY)),
        ?assertEqual(1, counter(<<"single_endpoint">>))
    end.

%% --- §4 #6 (covered by counter() assertions above; explicit timeout) ---

timeout_under_threshold_insufficient(_) ->
    fun() ->
        SlowFn = fun(_K) -> timer:sleep(2000), {ok, make_record(<<"slow">>)} end,
        Fns = [SlowFn, SlowFn, SlowFn],
        Wrapped = macula_resolve_quorum:wrap(
                    Fns, #{threshold => 2, timeout_ms => 50}),
        ?assertEqual({error, insufficient_responses}, Wrapped(?KEY)),
        ?assertEqual(1, counter(<<"insufficient_responses">>))
    end.

%% --- helpers --------------------------------------------------------

const_fn(Result) ->
    fun(_Key) -> Result end.

%% Build a minimal "record" map — only `signature' field matters for
%% the quorum logic. Other fields are placeholders.
make_record(Sig) when is_binary(Sig) ->
    #{type => 16#13, signature => Sig, payload => #{}, key => <<>>,
      issued_at => 0, expires_at => 0}.

counter(Outcome) ->
    Snap = macula_metrics:gather(),
    case [M || #{name := N} = M <- Snap,
               N =:= <<"macula_net_resolve_quorum_total">>] of
        [M] ->
            Samples = maps:get(samples, M),
            Hits = [V || #{labels := L, value := V} <- Samples,
                         proplists:get_value(outcome, L) =:= Outcome],
            sum(Hits);
        _ -> 0
    end.

sum([]) -> 0;
sum(L)  -> lists:sum(L).
