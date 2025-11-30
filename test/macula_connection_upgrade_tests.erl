%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_connection_upgrade module.
%%%
%%% Tests cover:
%%% - Relay registration and unregistration
%%% - Upgrade statistics tracking
%%% - Upgrade with non-existent relay
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection_upgrade_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

connection_upgrade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Start and stop upgrade manager", fun start_stop_test/0},
      {"Get empty stats", fun empty_stats_test/0},
      {"Register and unregister relay", fun register_unregister_test/0},
      {"Upgrade non-existent relay", fun upgrade_nonexistent_test/0},
      {"Stats track attempts", fun stats_tracking_test/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    case whereis(macula_connection_upgrade) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

start_stop_test() ->
    {ok, Pid} = macula_connection_upgrade:start_link(),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

empty_stats_test() ->
    {ok, Pid} = macula_connection_upgrade:start_link(),
    try
        Stats = macula_connection_upgrade:get_upgrade_stats(),
        ?assertEqual(0, maps:get(upgrades_attempted, Stats)),
        ?assertEqual(0, maps:get(upgrades_succeeded, Stats)),
        ?assertEqual(0, maps:get(upgrades_failed, Stats)),
        ?assertEqual(0, maps:get(active_relays, Stats))
    after
        gen_server:stop(Pid)
    end.

register_unregister_test() ->
    {ok, Pid} = macula_connection_upgrade:start_link(),
    try
        PeerId = <<"peer-123">>,
        RelayConn = make_ref(),
        Endpoint = {{192, 168, 1, 1}, 4433},

        %% Register a relay
        ok = macula_connection_upgrade:register_relay(PeerId, RelayConn, Endpoint),
        timer:sleep(10), % Allow async cast to process

        Stats1 = macula_connection_upgrade:get_upgrade_stats(),
        ?assertEqual(1, maps:get(active_relays, Stats1)),

        %% Unregister the relay
        ok = macula_connection_upgrade:unregister_relay(PeerId),
        timer:sleep(10),

        Stats2 = macula_connection_upgrade:get_upgrade_stats(),
        ?assertEqual(0, maps:get(active_relays, Stats2))
    after
        gen_server:stop(Pid)
    end.

upgrade_nonexistent_test() ->
    {ok, Pid} = macula_connection_upgrade:start_link(),
    try
        PeerId = <<"unknown-peer">>,
        RelayConn = make_ref(),
        DirectConn = make_ref(),

        %% Try to upgrade without registering first
        Result = macula_connection_upgrade:upgrade_to_direct(PeerId, RelayConn, DirectConn),
        ?assertEqual({error, no_relay}, Result)
    after
        gen_server:stop(Pid)
    end.

stats_tracking_test() ->
    {ok, Pid} = macula_connection_upgrade:start_link(),
    try
        PeerId = <<"peer-456">>,
        RelayConn = make_ref(),
        DirectConn = make_ref(),

        %% Attempt upgrade without registration (will fail)
        _Result = macula_connection_upgrade:upgrade_to_direct(PeerId, RelayConn, DirectConn),

        %% Check stats incremented
        Stats = macula_connection_upgrade:get_upgrade_stats(),
        ?assertEqual(1, maps:get(upgrades_attempted, Stats))
    after
        gen_server:stop(Pid)
    end.
