%% EUnit tests for macula_session_proof_rate: how many session proofs a station signs, per client node and in total
%% (plans/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md section 3, "Signing cost as an attack surface").
-module(macula_session_proof_rate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(T0, 1789000020000).
-define(NODE, <<1:256>>).

session_proof_rate_test_() ->
    {foreach,
     fun() -> {ok, Pid} = macula_session_proof_rate:start_link(), Pid end,
     fun(Pid) -> unlink(Pid), exit(Pid, shutdown), wait_down(Pid) end,
     [{"one client node gets 30 session proofs a minute, then a refusal until the next minute",
       fun per_node_minute/0},
      {"all clients together get 30 session proofs a second, then a refusal until the next second",
       fun total_second/0},
      {"windows that ended are purged", fun purged/0}]}.

per_node_minute() ->
    Allowed = [macula_session_proof_rate:allow(?NODE, ?T0 + I * 1000) || I <- lists:seq(0, 29)],
    ?assertEqual(lists:duplicate(30, ok), Allowed),
    ?assertEqual({error, session_proof_rate}, macula_session_proof_rate:allow(?NODE, ?T0 + 30000)),
    ?assertEqual(ok, macula_session_proof_rate:allow(<<2:256>>, ?T0 + 30001)),
    ?assertEqual(ok, macula_session_proof_rate:allow(?NODE, ?T0 + 60000)).

total_second() ->
    Allowed = [macula_session_proof_rate:allow(<<I:256>>, ?T0 + 500) || I <- lists:seq(1, 30)],
    ?assertEqual(lists:duplicate(30, ok), Allowed),
    ?assertEqual({error, session_proof_rate}, macula_session_proof_rate:allow(<<31:256>>, ?T0 + 999)),
    ?assertEqual(ok, macula_session_proof_rate:allow(<<31:256>>, ?T0 + 1000)).

purged() ->
    ok = macula_session_proof_rate:allow(?NODE, ?T0),
    ?assert(macula_session_proof_rate:windows() > 0),
    ok = macula_session_proof_rate:purge(?T0 + 2 * 60000),
    ?assertEqual(0, macula_session_proof_rate:windows()).

wait_down(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _} -> ok after 5000 -> error(not_down) end.
