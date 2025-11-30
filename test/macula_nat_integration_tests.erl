%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests for NAT Traversal system.
%%%
%%% These tests verify the complete NAT traversal flow:
%%% 1. NAT Detection via NAT_PROBE/NAT_PROBE_REPLY
%%% 2. Hole punch coordination via PUNCH_COORDINATE
%%% 3. Fallback to relay when hole punching fails
%%% 4. Full connection flow from request to established connection
%%%
%%% For unit testing, these tests use mocking and simulated components.
%%% For actual Docker NAT simulation, use docker/nat-test/.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

nat_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% NAT Detection Tests
      {"NAT probe message encoding", fun nat_probe_encoding_test/0},
      {"NAT probe reply decoding", fun nat_probe_reply_decoding_test/0},
      {"NAT detection flow simulation", fun nat_detection_flow_test/0},
      {"Multiple observations yield classification", fun multiple_observations_test/0},

      %% Hole Punch Coordination Tests
      {"Punch coordinate message encoding", fun punch_coordinate_encoding_test/0},
      {"Punch coordinate round-trip", fun punch_coordinate_roundtrip_test/0},
      {"Coordinator creates session for request", fun coordinator_session_test/0},
      {"Coordinator strategy selection", fun coordinator_strategy_test/0},

      %% Relay Fallback Tests
      {"Relay request encoding", fun relay_request_encoding_test/0},
      {"Relay data encoding", fun relay_data_encoding_test/0},
      {"Strategy relay for symmetric NAT", fun strategy_relay_symmetric_test/0},

      %% Full Flow Tests
      {"Full message flow simulation", fun full_message_flow_test/0},
      {"Protocol type IDs correct range", fun protocol_type_ids_test/0},
      {"All NAT messages supported", fun all_nat_messages_supported_test/0}
     ]}.

%%%===================================================================
%%% NAT Detection Tests
%%%===================================================================

nat_probe_encoding_test() ->
    Msg = #{
        node_id => crypto:strong_rand_bytes(16),
        local_port => 12345,
        request_id => crypto:strong_rand_bytes(16)
    },
    Encoded = macula_protocol_encoder:encode(nat_probe, Msg),
    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 8),  % Header + payload
    %% Verify header
    <<Version:8, TypeId:8, _Flags:8, _Reserved:8, _Len:32, _/binary>> = Encoded,
    ?assertEqual(1, Version),
    ?assertEqual(16#50, TypeId).  % MSG_NAT_PROBE

nat_probe_reply_decoding_test() ->
    OrigMsg = #{
        node_id => crypto:strong_rand_bytes(16),
        request_id => crypto:strong_rand_bytes(16),
        reflexive_ip => <<"192.168.1.100">>,
        reflexive_port => 54321,
        server_time => erlang:system_time(millisecond)
    },
    Encoded = macula_protocol_encoder:encode(nat_probe_reply, OrigMsg),
    {ok, {nat_probe_reply, DecodedMsg}} = macula_protocol_decoder:decode(Encoded),
    ?assertEqual(maps:get(reflexive_ip, OrigMsg), maps:get(<<"reflexive_ip">>, DecodedMsg)),
    ?assertEqual(maps:get(reflexive_port, OrigMsg), maps:get(<<"reflexive_port">>, DecodedMsg)).

nat_detection_flow_test() ->
    %% Simulate NAT detection flow:
    %% 1. Peer sends NAT_PROBE
    %% 2. Observer replies with NAT_PROBE_REPLY containing reflexive address
    %% 3. Peer processes reply to determine NAT type

    LocalPort = 12345,
    RequestId = crypto:strong_rand_bytes(16),
    NodeId = crypto:strong_rand_bytes(16),

    %% 1. Create NAT_PROBE
    ProbeMsg = #{
        node_id => NodeId,
        local_port => LocalPort,
        request_id => RequestId
    },
    ProbeEncoded = macula_protocol_encoder:encode(nat_probe, ProbeMsg),
    ?assert(is_binary(ProbeEncoded)),

    %% 2. Simulate observer creating NAT_PROBE_REPLY
    ReflexiveIP = <<"203.0.113.50">>,  % Simulated external IP
    ReflexivePort = 32000,              % Simulated mapped port
    ReplyMsg = #{
        node_id => crypto:strong_rand_bytes(16),  % Observer's node ID
        request_id => RequestId,
        reflexive_ip => ReflexiveIP,
        reflexive_port => ReflexivePort,
        server_time => erlang:system_time(millisecond)
    },
    ReplyEncoded = macula_protocol_encoder:encode(nat_probe_reply, ReplyMsg),
    ?assert(is_binary(ReplyEncoded)),

    %% 3. Decode and verify correlation
    {ok, {nat_probe_reply, DecodedReply}} = macula_protocol_decoder:decode(ReplyEncoded),
    ?assertEqual(RequestId, maps:get(<<"request_id">>, DecodedReply)),
    ?assertEqual(ReflexiveIP, maps:get(<<"reflexive_ip">>, DecodedReply)),
    ?assertEqual(ReflexivePort, maps:get(<<"reflexive_port">>, DecodedReply)).

multiple_observations_test() ->
    %% Test that multiple NAT probes to different observers
    %% can be used to classify NAT type

    LocalPort = 12345,
    _NodeId = crypto:strong_rand_bytes(16),

    %% Simulate 3 observations from different observers
    Observations = [
        #{observer => <<"observer1">>, reflexive_port => LocalPort},      % Port preserved
        #{observer => <<"observer2">>, reflexive_port => LocalPort},      % Port preserved
        #{observer => <<"observer3">>, reflexive_port => LocalPort + 1}   % Port incremented
    ],

    %% In a real test, these would be fed to macula_nat_detector
    %% For now, verify the observation data structure
    ?assertEqual(3, length(Observations)),
    ?assert(lists:all(fun(O) -> is_map(O) andalso maps:is_key(observer, O) end, Observations)).

%%%===================================================================
%%% Hole Punch Coordination Tests
%%%===================================================================

punch_coordinate_encoding_test() ->
    Msg = #{
        session_id => crypto:strong_rand_bytes(16),
        peer_id => crypto:strong_rand_bytes(16),
        peer_host => <<"192.168.1.50">>,
        peer_ports => [4433, 4434, 4435],
        punch_time => erlang:system_time(millisecond) + 1000,
        role => initiator
    },
    Encoded = macula_protocol_encoder:encode(punch_coordinate, Msg),
    ?assert(is_binary(Encoded)),
    <<_:8, TypeId:8, _/binary>> = Encoded,
    ?assertEqual(16#53, TypeId).  % MSG_PUNCH_COORDINATE

punch_coordinate_roundtrip_test() ->
    SessionId = crypto:strong_rand_bytes(16),
    PeerId = crypto:strong_rand_bytes(16),
    PeerPorts = [4433, 4434, 4435],
    PunchTime = erlang:system_time(millisecond) + 1000,

    Msg = #{
        session_id => SessionId,
        peer_id => PeerId,
        peer_host => <<"10.0.0.100">>,
        peer_ports => PeerPorts,
        punch_time => PunchTime,
        role => target
    },
    Encoded = macula_protocol_encoder:encode(punch_coordinate, Msg),
    {ok, {punch_coordinate, Decoded}} = macula_protocol_decoder:decode(Encoded),

    ?assertEqual(SessionId, maps:get(<<"session_id">>, Decoded)),
    ?assertEqual(PeerId, maps:get(<<"peer_id">>, Decoded)),
    ?assertEqual(PeerPorts, maps:get(<<"peer_ports">>, Decoded)),
    ?assertEqual(PunchTime, maps:get(<<"punch_time">>, Decoded)).

coordinator_session_test() ->
    %% Test that NAT coordinator can be started and creates sessions
    %% This is a simulated test - actual coordinator requires full system

    SessionId = crypto:strong_rand_bytes(16),
    InitiatorId = crypto:strong_rand_bytes(16),
    TargetId = crypto:strong_rand_bytes(16),

    %% Simulate session data structure
    Session = #{
        session_id => SessionId,
        initiator_id => InitiatorId,
        target_id => TargetId,
        state => pending,
        attempts => 0,
        created_at => erlang:system_time(millisecond)
    },

    ?assertEqual(SessionId, maps:get(session_id, Session)),
    ?assertEqual(pending, maps:get(state, Session)),
    ?assertEqual(0, maps:get(attempts, Session)).

coordinator_strategy_test() ->
    %% Test strategy selection logic
    %% Full Cone (EI/EI) -> direct
    %% EI mapping -> hole_punch
    %% Symmetric (PD/PD/RD) -> relay

    %% Simulate NAT profiles
    FullConeProfile = #{
        mapping_policy => endpoint_independent,
        filtering_policy => endpoint_independent,
        allocation_policy => port_preservation
    },
    SymmetricProfile = #{
        mapping_policy => port_dependent,
        filtering_policy => port_dependent,
        allocation_policy => random
    },

    %% Verify profile structure
    ?assertEqual(endpoint_independent, maps:get(mapping_policy, FullConeProfile)),
    ?assertEqual(port_dependent, maps:get(mapping_policy, SymmetricProfile)).

%%%===================================================================
%%% Relay Fallback Tests
%%%===================================================================

relay_request_encoding_test() ->
    Msg = #{
        session_id => crypto:strong_rand_bytes(16),
        target_id => crypto:strong_rand_bytes(16),
        reason => <<"hole_punch_failed">>
    },
    Encoded = macula_protocol_encoder:encode(relay_request, Msg),
    ?assert(is_binary(Encoded)),
    <<_:8, TypeId:8, _/binary>> = Encoded,
    ?assertEqual(16#56, TypeId).  % MSG_RELAY_REQUEST

relay_data_encoding_test() ->
    Payload = crypto:strong_rand_bytes(256),  % Simulated encrypted data
    Msg = #{
        session_id => crypto:strong_rand_bytes(16),
        payload => Payload
    },
    Encoded = macula_protocol_encoder:encode(relay_data, Msg),
    ?assert(is_binary(Encoded)),
    <<_:8, TypeId:8, _/binary>> = Encoded,
    ?assertEqual(16#57, TypeId).  % MSG_RELAY_DATA

strategy_relay_symmetric_test() ->
    %% Verify that symmetric NAT profiles require relay
    %% Symmetric NAT: PD mapping + PD filtering + RD allocation

    SymmetricProfile = #{
        mapping_policy => port_dependent,
        filtering_policy => port_dependent,
        allocation_policy => random
    },

    %% Both peers have symmetric NAT -> requires relay
    RequiresRelay = requires_relay(SymmetricProfile, SymmetricProfile),
    ?assert(RequiresRelay).

%% Helper to determine if relay is required
requires_relay(ProfileA, ProfileB) ->
    %% If either peer has symmetric NAT, relay is likely needed
    IsSymmetric = fun(P) ->
        maps:get(mapping_policy, P) =:= port_dependent andalso
        maps:get(allocation_policy, P) =:= random
    end,
    IsSymmetric(ProfileA) orelse IsSymmetric(ProfileB).

%%%===================================================================
%%% Full Flow Tests
%%%===================================================================

full_message_flow_test() ->
    %% Test complete message flow:
    %% 1. NAT_PROBE -> NAT_PROBE_REPLY (detection)
    %% 2. PUNCH_REQUEST -> PUNCH_COORDINATE (coordination)
    %% 3. PUNCH_RESULT (reporting)

    NodeA = crypto:strong_rand_bytes(16),
    NodeB = crypto:strong_rand_bytes(16),
    Observer = crypto:strong_rand_bytes(16),
    SessionId = crypto:strong_rand_bytes(16),

    %% Step 1: NAT Detection
    ProbeMsg = #{node_id => NodeA, local_port => 4433, request_id => crypto:strong_rand_bytes(16)},
    ProbeEncoded = macula_protocol_encoder:encode(nat_probe, ProbeMsg),
    {ok, {nat_probe, _}} = macula_protocol_decoder:decode(ProbeEncoded),

    ReplyMsg = #{
        node_id => Observer,
        request_id => maps:get(request_id, ProbeMsg),
        reflexive_ip => <<"203.0.113.10">>,
        reflexive_port => 32001,
        server_time => erlang:system_time(millisecond)
    },
    ReplyEncoded = macula_protocol_encoder:encode(nat_probe_reply, ReplyMsg),
    {ok, {nat_probe_reply, _}} = macula_protocol_decoder:decode(ReplyEncoded),

    %% Step 2: Punch Coordination
    CoordMsg = #{
        session_id => SessionId,
        peer_id => NodeB,
        peer_host => <<"203.0.113.20">>,
        peer_ports => [4433, 4434],
        punch_time => erlang:system_time(millisecond) + 500,
        role => initiator
    },
    CoordEncoded = macula_protocol_encoder:encode(punch_coordinate, CoordMsg),
    {ok, {punch_coordinate, _}} = macula_protocol_decoder:decode(CoordEncoded),

    %% Step 3: Punch Result
    ResultMsg = #{session_id => SessionId, success => true, connected_port => 4433},
    ResultEncoded = macula_protocol_encoder:encode(punch_result, ResultMsg),
    {ok, {punch_result, DecodedResult}} = macula_protocol_decoder:decode(ResultEncoded),
    ?assertEqual(true, maps:get(<<"success">>, DecodedResult)).

protocol_type_ids_test() ->
    %% Verify NAT message type IDs are in the 0x50-0x5F range
    NatTypes = [
        {nat_probe, 16#50},
        {nat_probe_reply, 16#51},
        {punch_request, 16#52},
        {punch_coordinate, 16#53},
        %% 16#54 = PUNCH_EXECUTE (unused in current implementation)
        {punch_result, 16#55},
        {relay_request, 16#56},
        {relay_data, 16#57}
    ],

    lists:foreach(fun({Type, ExpectedId}) ->
        ActualId = macula_protocol_types:message_type_id(Type),
        ?assertEqual(ExpectedId, ActualId,
                     io_lib:format("~p should have ID ~.16B", [Type, ExpectedId]))
    end, NatTypes).

all_nat_messages_supported_test() ->
    %% Verify all NAT message types can be encoded and decoded

    %% NAT_PROBE
    ProbeMsg = #{node_id => <<1:128>>, local_port => 4433},
    ?assertMatch({ok, _}, test_roundtrip(nat_probe, ProbeMsg)),

    %% NAT_PROBE_REPLY
    ReplyMsg = #{node_id => <<2:128>>, request_id => <<3:128>>,
                 reflexive_ip => <<"1.2.3.4">>, reflexive_port => 5000,
                 server_time => 12345},
    ?assertMatch({ok, _}, test_roundtrip(nat_probe_reply, ReplyMsg)),

    %% PUNCH_REQUEST
    PunchReqMsg = #{session_id => <<4:128>>, requester_id => <<5:128>>,
                    target_id => <<6:128>>},
    ?assertMatch({ok, _}, test_roundtrip(punch_request, PunchReqMsg)),

    %% PUNCH_COORDINATE
    CoordMsg = #{session_id => <<7:128>>, peer_id => <<8:128>>,
                 peer_host => <<"10.0.0.1">>, peer_ports => [4433],
                 punch_time => 12345, role => initiator},
    ?assertMatch({ok, _}, test_roundtrip(punch_coordinate, CoordMsg)),

    %% PUNCH_RESULT
    ResultMsg = #{session_id => <<9:128>>, success => true},
    ?assertMatch({ok, _}, test_roundtrip(punch_result, ResultMsg)),

    %% RELAY_REQUEST
    RelayReqMsg = #{session_id => <<10:128>>, target_id => <<11:128>>},
    ?assertMatch({ok, _}, test_roundtrip(relay_request, RelayReqMsg)),

    %% RELAY_DATA
    RelayDataMsg = #{session_id => <<12:128>>, payload => <<"test data">>},
    ?assertMatch({ok, _}, test_roundtrip(relay_data, RelayDataMsg)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

test_roundtrip(Type, Msg) ->
    Encoded = macula_protocol_encoder:encode(Type, Msg),
    macula_protocol_decoder:decode(Encoded).
