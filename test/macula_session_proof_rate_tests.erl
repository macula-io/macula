%% EUnit tests for macula_session_proof_rate: how many session proofs a station signs, per client node and in total
%% (plans/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md section 3, "Signing cost as an attack surface").
-module(macula_session_proof_rate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(T0, 1789000020000).
-define(NODE, <<1:256>>).

session_proof_rate_test_() ->
    {foreach,
     fun() -> own_process() end,
     fun(Pid) -> give_back(Pid) end,
     [{"one client node gets 30 session proofs a minute, then a refusal until the next minute",
       fun per_node_minute/0},
      {"all clients together get 30 session proofs a second, then a refusal until the next second",
       fun total_second/0},
      {"windows that ended are purged", fun purged/0},
      {"a refusal on the total spends none of the client's own budget", fun total_refusal_spends_no_node_budget/0},
      {"the defaults are the limits in force", fun default_limits/0}]}.

%% The limits come from the macula application environment, read once at start.
configured_test_() ->
    [{"configured limits replace the defaults", fun configured_limits/0},
     {"a limit below 1, or not an integer, refuses to start and names itself", fun invalid_limits/0}].

per_node_minute() ->
    Allowed = [macula_session_proof_rate:allow(?NODE, ?T0 + I * 1000) || I <- lists:seq(0, 29)],
    ?assertEqual(lists:duplicate(30, ok), Allowed),
    ?assertEqual({error, {session_proof_rate, per_node_per_minute}},
                 macula_session_proof_rate:allow(?NODE, ?T0 + 30000)),
    ?assertEqual(ok, macula_session_proof_rate:allow(<<2:256>>, ?T0 + 30001)),
    ?assertEqual(ok, macula_session_proof_rate:allow(?NODE, ?T0 + 60000)).

total_second() ->
    Allowed = [macula_session_proof_rate:allow(<<I:256>>, ?T0 + 500) || I <- lists:seq(1, 30)],
    ?assertEqual(lists:duplicate(30, ok), Allowed),
    ?assertEqual({error, {session_proof_rate, per_second}}, macula_session_proof_rate:allow(<<31:256>>, ?T0 + 999)),
    ?assertEqual(ok, macula_session_proof_rate:allow(<<31:256>>, ?T0 + 1000)).

total_refusal_spends_no_node_budget() ->
    [ok = macula_session_proof_rate:allow(<<(100 + I):256>>, ?T0 + 500) || I <- lists:seq(1, 30)],
    [{error, {session_proof_rate, per_second}} = macula_session_proof_rate:allow(?NODE, ?T0 + 600)
     || _ <- lists:seq(1, 30)],
    Refused = [{I, Refusal} || I <- lists:seq(0, 29),
                               Refusal <- [macula_session_proof_rate:allow(?NODE, ?T0 + 1000 + I * 1000)],
                               Refusal =/= ok],
    ?assertEqual([], Refused).

purged() ->
    ok = macula_session_proof_rate:allow(?NODE, ?T0),
    ?assert(macula_session_proof_rate:windows() > 0),
    ok = macula_session_proof_rate:purge(?T0 + 2 * 60000),
    ?assertEqual(0, macula_session_proof_rate:windows()).

default_limits() ->
    ?assertEqual(#{per_node_per_minute => 30, per_second => 30}, macula_peering:session_proof_limits()).

configured_limits() ->
    with_env([{session_proofs_per_node_per_minute, 2}, {session_proofs_per_second, 3}],
             fun() ->
                 ?assertEqual(#{per_node_per_minute => 2, per_second => 3}, macula_session_proof_rate:limits()),
                 ?assertEqual(ok, macula_session_proof_rate:allow(?NODE, ?T0)),
                 ?assertEqual(ok, macula_session_proof_rate:allow(?NODE, ?T0 + 1000)),
                 ?assertEqual({error, {session_proof_rate, per_node_per_minute}},
                              macula_session_proof_rate:allow(?NODE, ?T0 + 2000))
             end).

invalid_limits() ->
    process_flag(trap_exit, true),
    [?assertEqual({error, {invalid_limit, Name, Value}}, started_with([{Name, Value}]))
     || {Name, Value} <- [{session_proofs_per_second, 0}, {session_proofs_per_node_per_minute, -1},
                          {session_proofs_per_second, <<"30">>}]],
    process_flag(trap_exit, false).

with_env(Env, Test) ->
    {ok, Pid} = started_with(Env),
    try Test() after unset(Env), give_back(Pid) end.

started_with(Env) ->
    [ok = application:set_env(macula, Name, Value) || {Name, Value} <- Env],
    ok = supervised(fun supervisor:terminate_child/2),
    Started = macula_session_proof_rate:start_link(),
    unset_on_error(Started, Env).

unset_on_error({ok, _} = Started, _Env) -> Started;
unset_on_error(Error, Env) ->
    unset(Env),
    ok = supervised(fun(Sup, Id) -> restarted(supervisor:restart_child(Sup, Id)) end),
    Error.

unset(Env) ->
    [ok = application:unset_env(macula, Name) || {Name, _} <- Env],
    ok.

%% A fresh process of this test's own, whether or not the macula application runs one under macula_peering_sup:
%% the supervised one is stopped for the test and started again after it.
own_process() ->
    ok = supervised(fun supervisor:terminate_child/2),
    {ok, Pid} = macula_session_proof_rate:start_link(),
    Pid.

give_back(Pid) ->
    unlink(Pid),
    exit(Pid, shutdown),
    ok = wait_down(Pid),
    supervised(fun(Sup, Id) -> restarted(supervisor:restart_child(Sup, Id)) end).

supervised(Action) ->
    supervised(whereis(macula_peering_sup), Action).

supervised(undefined, _Action) -> ok;
supervised(Sup, Action) -> Action(Sup, macula_session_proof_rate).

restarted({ok, _Pid}) -> ok.

wait_down(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _} -> ok after 5000 -> error(not_down) end.
