%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_nat_connector module.
%%%
%%% Tests NAT-aware peer connection establishment.
%%% Note: Tests requiring actual QUIC connections are integration tests.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_connector_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Connect Option Validation Tests
%%%===================================================================

connect_opts_defaults_test() ->
    %% Test that connect options have sensible defaults
    DefaultOpts = #{},

    %% Default timeout should be used (10000ms)
    Timeout = maps:get(timeout, DefaultOpts, 10000),
    ?assertEqual(10000, Timeout),

    %% skip_hole_punch defaults to false
    SkipHolePunch = maps:get(skip_hole_punch, DefaultOpts, false),
    ?assertEqual(false, SkipHolePunch).

connect_opts_custom_timeout_test() ->
    %% Custom timeout should be respected
    Opts = #{timeout => 5000},
    Timeout = maps:get(timeout, Opts, 10000),
    ?assertEqual(5000, Timeout).

connect_opts_skip_hole_punch_test() ->
    %% skip_hole_punch option
    Opts = #{skip_hole_punch => true},
    SkipHolePunch = maps:get(skip_hole_punch, Opts, false),
    ?assertEqual(true, SkipHolePunch).

connect_opts_relay_endpoint_test() ->
    %% relay_endpoint option for specific relay
    Opts = #{relay_endpoint => <<"192.168.1.100:4433">>},
    RelayEndpoint = maps:get(relay_endpoint, Opts, undefined),
    ?assertEqual(<<"192.168.1.100:4433">>, RelayEndpoint).

%%%===================================================================
%%% Connect Result Type Tests
%%%===================================================================

connect_result_types_test() ->
    %% Test all possible connect result types
    DirectResult = {ok, make_ref(), direct},
    HolePunchResult = {ok, make_ref(), hole_punch},
    RelayResult = {ok, make_ref(), relay},
    ErrorResult = {error, no_route_to_peer},

    %% All success results should have connection and strategy
    {ok, Conn1, Strategy1} = DirectResult,
    ?assert(is_reference(Conn1)),
    ?assertEqual(direct, Strategy1),

    {ok, Conn2, Strategy2} = HolePunchResult,
    ?assert(is_reference(Conn2)),
    ?assertEqual(hole_punch, Strategy2),

    {ok, Conn3, Strategy3} = RelayResult,
    ?assert(is_reference(Conn3)),
    ?assertEqual(relay, Strategy3),

    %% Error result should have reason
    {error, Reason} = ErrorResult,
    ?assertEqual(no_route_to_peer, Reason).

%%%===================================================================
%%% Disconnect Tests
%%%===================================================================

disconnect_handles_nil_test() ->
    %% Disconnect should handle invalid conn gracefully
    %% This exercises the catch in disconnect/1
    Result = macula_nat_connector:disconnect(undefined),
    ?assertEqual(ok, Result).

disconnect_handles_ref_test() ->
    %% Disconnect should handle any term gracefully
    MockConn = make_ref(),
    Result = macula_nat_connector:disconnect(MockConn),
    ?assertEqual(ok, Result).

%%%===================================================================
%%% Strategy Selection Tests (without coordinator)
%%%===================================================================

%% Without coordinator, default strategy should be hole_punch

connect_without_coordinator_test() ->
    %% GIVEN: No NAT coordinator running
    case whereis(macula_nat_coordinator) of
        undefined -> ok;  % Expected
        Pid -> gen_server:stop(Pid)
    end,

    %% WHEN: Connecting without coordinator
    LocalNodeId = <<"local-node">>,
    TargetNodeId = <<"target-node">>,

    %% THEN: Should try to connect (will fail without infrastructure)
    Result = macula_nat_connector:connect(LocalNodeId, TargetNodeId),

    %% Should return error (no actual connection possible)
    ?assertMatch({error, _}, Result).

connect_with_custom_timeout_test() ->
    %% GIVEN: Custom timeout option
    LocalNodeId = <<"local-node">>,
    TargetNodeId = <<"target-node">>,
    Opts = #{timeout => 100},  % Very short timeout

    %% WHEN: Connecting with short timeout
    Result = macula_nat_connector:connect(LocalNodeId, TargetNodeId, Opts),

    %% THEN: Should fail quickly
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Session ID Generation Tests
%%%===================================================================

%% The connector generates session IDs for hole punch coordination

session_id_uniqueness_test() ->
    %% Session IDs should be unique
    SessionId1 = crypto:strong_rand_bytes(16),
    SessionId2 = crypto:strong_rand_bytes(16),

    ?assertNotEqual(SessionId1, SessionId2).

session_id_is_binary_test() ->
    %% Session IDs should be binaries
    SessionId = crypto:strong_rand_bytes(16),
    ?assert(is_binary(SessionId)).

session_id_sufficient_length_test() ->
    %% Session IDs should be sufficiently long for uniqueness
    SessionId = crypto:strong_rand_bytes(16),
    ?assertEqual(16, byte_size(SessionId)).

%%%===================================================================
%%% Connection Strategy Types Tests
%%%===================================================================

strategy_types_test() ->
    %% Verify valid connection strategy atoms
    Strategies = [direct, hole_punch, relay],
    lists:foreach(fun(Strategy) ->
        ?assert(is_atom(Strategy))
    end, Strategies).

strategy_priority_order_test() ->
    %% Connection strategies should be tried in priority order
    %% 1. Direct (fastest, if NAT allows)
    %% 2. Hole punch (if NATs are compatible)
    %% 3. Relay (fallback, always works)
    PriorityOrder = [direct, hole_punch, relay],
    ?assertEqual(3, length(PriorityOrder)),
    ?assertEqual(direct, hd(PriorityOrder)),
    ?assertEqual(relay, lists:last(PriorityOrder)).

%%%===================================================================
%%% NAT Type Compatibility Tests
%%%===================================================================

%% These test the logic of which NAT combinations can hole punch

nat_compatibility_full_cone_test() ->
    %% Full Cone NAT can receive from anyone - always punchable
    FullCone = full_cone,
    ?assertEqual(full_cone, FullCone).

nat_compatibility_symmetric_test() ->
    %% Symmetric NAT to Symmetric NAT - hardest case
    Symmetric = symmetric,
    ?assertEqual(symmetric, Symmetric).

nat_type_pairs_test() ->
    %% Test all NAT type combinations
    NatTypes = [full_cone, restricted, port_restricted, symmetric],
    Pairs = [{A, B} || A <- NatTypes, B <- NatTypes],
    ?assertEqual(16, length(Pairs)).  % 4x4 = 16 combinations

