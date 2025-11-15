-module(dht_address_serialization_test).
-include_lib("eunit/include/eunit.hrl").

%% This test reproduces the bug where Address tuples cannot be serialized
address_tuple_serialization_bug_test() ->
    %% This is what currently happens - address is stored as tuple
    NodeInfo = #{
        node_id => crypto:strong_rand_bytes(32),
        address => {{127,0,0,1}, 9443}  % PROBLEM: tuple, not binary!
    },

    %% Try to encode node info (this is what encode_node_info does)
    NodeInfoForTransmission = #{
        node_id => maps:get(node_id, NodeInfo),
        address => maps:get(address, NodeInfo)
    },

    %% Try to pack with msgpack (this should fail with current code)
    Result = msgpack:pack(NodeInfoForTransmission, [{map_format, map}]),

    %% This test documents the bug - msgpack returns error for tuples
    ?assertMatch({error, {badarg, _}}, Result).

%% This test shows what SHOULD happen - address stored as binary
address_binary_serialization_works_test() ->
    %% This is what SHOULD happen - address is stored as binary string
    NodeInfo = #{
        node_id => crypto:strong_rand_bytes(32),
        address => <<"https://arcade-gateway:4433">>  % CORRECT: binary string
    },

    %% Try to encode node info
    NodeInfoForTransmission = #{
        node_id => maps:get(node_id, NodeInfo),
        address => maps:get(address, NodeInfo)
    },

    %% Try to pack with msgpack (returns binary directly, not {ok, Binary})
    Result = msgpack:pack(NodeInfoForTransmission, [{map_format, map}]),

    %% This should work fine - msgpack returns binary for valid data
    ?assert(is_binary(Result)).
