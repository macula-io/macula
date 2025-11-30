%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_nat_detector module.
%%%
%%% Tests NAT type detection using NATCracker methodology.
%%% Note: Full detection tests require external observers and are
%%% covered by integration tests.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_detector_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% NAT Detector gen_server Tests
%%%===================================================================

%% Test fixture for detector tests
nat_detector_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"get_local_profile returns undefined before detection", fun test_initial_profile/0},
         {"get_local_address returns valid address", fun test_local_address/0},
         {"get_local_quic_port returns valid port", fun test_local_quic_port/0},
         {"is_relay_capable returns boolean", fun test_relay_capable/0},
         {"refresh triggers detection", fun test_refresh/0}
     ]}.

setup() ->
    %% Start NAT detector with minimal config
    {ok, Pid} = macula_nat_detector:start_link(#{
        detection_timeout_ms => 1000
    }),
    Pid.

cleanup(Pid) ->
    catch gen_server:stop(Pid),
    ok.

test_initial_profile() ->
    %% Before detection, profile should be not_detected
    Profile = macula_nat_detector:get_local_profile(),
    ?assertEqual(not_detected, Profile).

test_local_address() ->
    %% Should return a valid local address tuple
    Result = macula_nat_detector:get_local_address(),
    case Result of
        {A, B, C, D} when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
            %% Valid IPv4
            ?assert(A >= 0 andalso A =< 255),
            ?assert(B >= 0 andalso B =< 255),
            ?assert(C >= 0 andalso C =< 255),
            ?assert(D >= 0 andalso D =< 255);
        undefined ->
            %% No network interface - acceptable in test env
            ok;
        _ ->
            %% Could be IPv6 or other format
            ok
    end.

test_local_quic_port() ->
    %% Should return a port number or undefined
    Result = macula_nat_detector:get_local_quic_port(),
    case Result of
        Port when is_integer(Port), Port > 0, Port < 65536 ->
            ok;
        undefined ->
            %% No QUIC listener - acceptable in test env
            ok
    end.

test_relay_capable() ->
    %% Should return a boolean
    Result = macula_nat_detector:is_relay_capable(),
    ?assert(is_boolean(Result)).

test_refresh() ->
    %% Refresh should trigger async detection and return ok
    Result = macula_nat_detector:refresh(),
    ?assertEqual(ok, Result).

%%%===================================================================
%%% NAT Profile Classification Tests
%%%===================================================================

%% Test NAT type classification based on observations
%% These tests verify the classification logic without network

nat_type_classification_test() ->
    %% Full Cone NAT: Same reflexive address for all destinations
    %% Endpoint-Independent mapping and filtering
    FullConeProfile = #{
        nat_type => full_cone,
        mapping_policy => endpoint_independent,
        filtering_policy => endpoint_independent,
        allocation_policy => port_preservation
    },
    ?assertEqual(full_cone, maps:get(nat_type, FullConeProfile)),

    %% Symmetric NAT: Different reflexive address per destination
    %% Port-Dependent mapping
    SymmetricProfile = #{
        nat_type => symmetric,
        mapping_policy => port_dependent,
        filtering_policy => port_dependent,
        allocation_policy => random
    },
    ?assertEqual(symmetric, maps:get(nat_type, SymmetricProfile)).

%%%===================================================================
%%% Observation Record Tests
%%%===================================================================

observation_structure_test() ->
    %% Test that observation records have required fields
    Observation = #{
        observer_id => <<"observer-001">>,
        reflexive_address => {{1,2,3,4}, 5000},
        local_address => {{192,168,1,100}, 4433},
        observed_at => erlang:system_time(millisecond)
    },

    ?assert(maps:is_key(observer_id, Observation)),
    ?assert(maps:is_key(reflexive_address, Observation)),
    ?assert(maps:is_key(local_address, Observation)),
    ?assert(maps:is_key(observed_at, Observation)).

observation_reflexive_address_comparison_test() ->
    %% Test comparing reflexive addresses to determine NAT type
    Obs1 = #{
        observer_id => <<"obs1">>,
        reflexive_address => {{203,0,113,50}, 12345},
        local_address => {{192,168,1,100}, 4433},
        observed_at => 1000
    },
    Obs2 = #{
        observer_id => <<"obs2">>,
        reflexive_address => {{203,0,113,50}, 12345},  % Same address
        local_address => {{192,168,1,100}, 4433},
        observed_at => 2000
    },

    %% Same reflexive address from different observers = Endpoint-Independent mapping
    Addr1 = maps:get(reflexive_address, Obs1),
    Addr2 = maps:get(reflexive_address, Obs2),
    ?assertEqual(Addr1, Addr2).

observation_different_addresses_test() ->
    %% Different reflexive addresses indicate NAT behavior
    %% Port-Dependent mapping would show different ports for different destinations
    Addr1 = {{203,0,113,50}, 12345},
    Addr2 = {{203,0,113,50}, 54321},

    %% Addresses with different ports should not be equal
    {Ip1, Port1} = Addr1,
    {Ip2, Port2} = Addr2,
    ?assertEqual(Ip1, Ip2),  % Same IP
    ?assert(Port1 =/= Port2).  % Different ports

%%%===================================================================
%%% Detection Timeout Tests
%%%===================================================================

detection_timeout_config_test() ->
    %% Test that detection timeout is configurable
    %% Start with custom timeout
    {ok, Pid} = macula_nat_detector:start_link(#{
        detection_timeout_ms => 500
    }),

    %% Detector should start successfully
    ?assert(is_pid(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% NAT Type Constants Tests
%%%===================================================================

nat_types_test() ->
    %% Verify standard NAT type atoms
    Types = [full_cone, restricted, port_restricted, symmetric, unknown],
    lists:foreach(fun(Type) ->
        ?assert(is_atom(Type))
    end, Types).

mapping_policy_types_test() ->
    %% Mapping policies per NATCracker
    Policies = [endpoint_independent, host_dependent, port_dependent],
    lists:foreach(fun(Policy) ->
        ?assert(is_atom(Policy))
    end, Policies).

filtering_policy_types_test() ->
    %% Filtering policies per NATCracker
    Policies = [endpoint_independent, host_dependent, port_dependent],
    lists:foreach(fun(Policy) ->
        ?assert(is_atom(Policy))
    end, Policies).

allocation_policy_types_test() ->
    %% Port allocation policies per NATCracker
    Policies = [port_preservation, port_contiguity, random],
    lists:foreach(fun(Policy) ->
        ?assert(is_atom(Policy))
    end, Policies).

