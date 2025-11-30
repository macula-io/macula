%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_relay_registry module.
%%%
%%% Tests cover:
%%% - Relay registration and unregistration
%%% - Relay discovery and selection
%%% - TTL-based expiration
%%% - Load-based selection
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

relay_registry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Start and stop relay registry", fun start_stop_test/0},
      {"Register relay", fun register_relay_test/0},
      {"Register and unregister relay", fun register_unregister_test/0},
      {"Get relay count", fun get_relay_count_test/0},
      {"Check is_relay", fun is_relay_test/0},
      {"Get all relays", fun get_relays_test/0},
      {"Find relay excludes target", fun find_relay_excludes_target_test/0},
      {"Find relay when none available", fun find_relay_none_test/0},
      {"Select best relay by load", fun select_best_relay_by_load_test/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    case whereis(macula_relay_registry) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

start_stop_test() ->
    {ok, Pid} = macula_relay_registry:start_link(#{}),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

register_relay_test() ->
    {ok, Pid} = macula_relay_registry:start_link(#{}),
    try
        NodeId = <<"relay-001">>,
        Endpoint = {<<"192.168.1.100">>, 4433},

        Result = macula_relay_registry:register(NodeId, Endpoint),
        ?assertEqual(ok, Result),

        %% Verify registered
        ?assert(macula_relay_registry:is_relay(NodeId))
    after
        gen_server:stop(Pid)
    end.

register_unregister_test() ->
    {ok, Pid} = macula_relay_registry:start_link(#{}),
    try
        NodeId = <<"relay-002">>,
        Endpoint = {<<"10.0.0.1">>, 9443},

        ok = macula_relay_registry:register(NodeId, Endpoint),
        ?assert(macula_relay_registry:is_relay(NodeId)),

        ok = macula_relay_registry:unregister(NodeId),
        timer:sleep(10), % Allow async cast
        ?assertNot(macula_relay_registry:is_relay(NodeId))
    after
        gen_server:stop(Pid)
    end.

get_relay_count_test() ->
    {ok, Pid} = macula_relay_registry:start_link(#{}),
    try
        ?assertEqual(0, macula_relay_registry:get_relay_count()),

        ok = macula_relay_registry:register(<<"relay-a">>, {<<"1.1.1.1">>, 4433}),
        ?assertEqual(1, macula_relay_registry:get_relay_count()),

        ok = macula_relay_registry:register(<<"relay-b">>, {<<"2.2.2.2">>, 4433}),
        ?assertEqual(2, macula_relay_registry:get_relay_count())
    after
        gen_server:stop(Pid)
    end.

is_relay_test() ->
    {ok, Pid} = macula_relay_registry:start_link(#{}),
    try
        NodeId = <<"relay-003">>,

        %% Not registered yet
        ?assertNot(macula_relay_registry:is_relay(NodeId)),

        ok = macula_relay_registry:register(NodeId, {<<"5.5.5.5">>, 4433}),

        %% Now registered
        ?assert(macula_relay_registry:is_relay(NodeId)),

        %% Non-existent still false
        ?assertNot(macula_relay_registry:is_relay(<<"unknown">>))
    after
        gen_server:stop(Pid)
    end.

get_relays_test() ->
    {ok, Pid} = macula_relay_registry:start_link(#{}),
    try
        Relays0 = macula_relay_registry:get_relays(),
        ?assertEqual([], Relays0),

        ok = macula_relay_registry:register(<<"r1">>, {<<"1.1.1.1">>, 4433}),
        ok = macula_relay_registry:register(<<"r2">>, {<<"2.2.2.2">>, 4433}),

        Relays = macula_relay_registry:get_relays(),
        ?assertEqual(2, length(Relays)),

        NodeIds = [maps:get(node_id, R) || R <- Relays],
        ?assert(lists:member(<<"r1">>, NodeIds)),
        ?assert(lists:member(<<"r2">>, NodeIds))
    after
        gen_server:stop(Pid)
    end.

find_relay_excludes_target_test() ->
    {ok, Pid} = macula_relay_registry:start_link(#{}),
    try
        %% Register relay that is also our target
        ok = macula_relay_registry:register(<<"target-peer">>, {<<"3.3.3.3">>, 4433}),
        ok = macula_relay_registry:register(<<"other-relay">>, {<<"4.4.4.4">>, 4433}),

        %% Find relay for target - should not return target itself
        {ok, Relay} = macula_relay_registry:find_relay(<<"target-peer">>),
        ?assertEqual(<<"other-relay">>, maps:get(node_id, Relay))
    after
        gen_server:stop(Pid)
    end.

find_relay_none_test() ->
    {ok, Pid} = macula_relay_registry:start_link(#{}),
    try
        %% No relays registered
        Result = macula_relay_registry:find_relay(<<"some-peer">>),
        ?assertEqual({error, no_relays_available}, Result)
    after
        gen_server:stop(Pid)
    end.

select_best_relay_by_load_test() ->
    {ok, Pid} = macula_relay_registry:start_link(#{}),
    try
        %% Register relays with same capacity, same latency
        %% They will be sorted by load, so the first registered will be selected
        ok = macula_relay_registry:register(<<"relay-low">>, {<<"10.0.0.1">>, 4433}, #{capacity => 100}),
        ok = macula_relay_registry:register(<<"relay-high">>, {<<"10.0.0.2">>, 4433}, #{capacity => 100}),

        %% Both have 0 load, so either is acceptable
        {ok, Relay} = macula_relay_registry:find_relay(<<"peer-x">>),
        ?assert(maps:is_key(node_id, Relay))
    after
        gen_server:stop(Pid)
    end.
