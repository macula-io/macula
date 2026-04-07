%%% @doc Tests for Kademlia bootstrap lookup (FIND_NODE for self after connect).
%%%
%%% Tests the peer discovery mechanism added in v0.20.14:
%%% - After connecting, peer sends FIND_NODE for its own node_id
%%% - Gateway responds with known peers from its routing table
%%% - Peer adds discovered peers to its local routing table
%%% @end
-module(macula_bootstrap_lookup_tests).

-include_lib("eunit/include/eunit.hrl").

%%==============================================================================
%% Helper: generate a random 32-byte node_id
%%==============================================================================

random_node_id() ->
    crypto:strong_rand_bytes(32).

%%==============================================================================
%% extract_peer_info/2 tests
%%==============================================================================

extract_peer_info_with_atom_keys_test() ->
    MyId = random_node_id(),
    PeerId = random_node_id(),
    NodeInfo = #{node_id => PeerId, endpoint => <<"192.168.1.1:9443">>},
    {true, Info} = macula_connection_dispatch:extract_peer_info(NodeInfo, MyId),
    ?assertEqual(PeerId, maps:get(node_id, Info)),
    ?assertEqual(<<"192.168.1.1:9443">>, maps:get(endpoint, Info)).

extract_peer_info_with_binary_keys_test() ->
    MyId = random_node_id(),
    PeerId = random_node_id(),
    NodeInfo = #{<<"node_id">> => PeerId, <<"endpoint">> => <<"10.0.0.1:4433">>},
    {true, Info} = macula_connection_dispatch:extract_peer_info(NodeInfo, MyId),
    ?assertEqual(PeerId, maps:get(node_id, Info)),
    ?assertEqual(<<"10.0.0.1:4433">>, maps:get(endpoint, Info)).

extract_peer_info_skips_self_test() ->
    MyId = random_node_id(),
    NodeInfo = #{node_id => MyId, endpoint => <<"127.0.0.1:9443">>},
    ?assertEqual(false, macula_connection_dispatch:extract_peer_info(NodeInfo, MyId)).

extract_peer_info_skips_undefined_node_id_test() ->
    MyId = random_node_id(),
    NodeInfo = #{endpoint => <<"10.0.0.1:4433">>},
    ?assertEqual(false, macula_connection_dispatch:extract_peer_info(NodeInfo, MyId)).

extract_peer_info_skips_non_map_test() ->
    MyId = random_node_id(),
    ?assertEqual(false, macula_connection_dispatch:extract_peer_info(not_a_map, MyId)).

extract_peer_info_uses_address_fallback_test() ->
    MyId = random_node_id(),
    PeerId = random_node_id(),
    NodeInfo = #{node_id => PeerId, address => <<"192.168.1.2:9450">>},
    {true, Info} = macula_connection_dispatch:extract_peer_info(NodeInfo, MyId),
    ?assertEqual(<<"192.168.1.2:9450">>, maps:get(endpoint, Info)).

%%==============================================================================
%% make_peer_info/3 tests
%%==============================================================================

make_peer_info_valid_test() ->
    PeerId = random_node_id(),
    {true, Info} = macula_connection_dispatch:make_peer_info(PeerId, <<"10.0.0.1:9443">>, random_node_id()),
    ?assertEqual(PeerId, maps:get(node_id, Info)),
    ?assertEqual(<<"10.0.0.1:9443">>, maps:get(address, Info)),
    ?assertEqual(<<"10.0.0.1:9443">>, maps:get(endpoint, Info)).

make_peer_info_undefined_id_test() ->
    ?assertEqual(false, macula_connection_dispatch:make_peer_info(undefined, <<"10.0.0.1:9443">>, random_node_id())).

make_peer_info_self_id_test() ->
    MyId = random_node_id(),
    ?assertEqual(false, macula_connection_dispatch:make_peer_info(MyId, <<"10.0.0.1:9443">>, MyId)).

%%==============================================================================
%% get_map_field/3 tests
%%==============================================================================

get_map_field_atom_key_test() ->
    Map = #{name => <<"alice">>},
    ?assertEqual(<<"alice">>, macula_connection_dispatch:get_map_field(Map, name, <<"name">>)).

get_map_field_binary_key_test() ->
    Map = #{<<"name">> => <<"bob">>},
    ?assertEqual(<<"bob">>, macula_connection_dispatch:get_map_field(Map, name, <<"name">>)).

get_map_field_atom_key_priority_test() ->
    Map = #{name => <<"atom_val">>, <<"name">> => <<"bin_val">>},
    ?assertEqual(<<"atom_val">>, macula_connection_dispatch:get_map_field(Map, name, <<"name">>)).

get_map_field_missing_returns_undefined_test() ->
    Map = #{other => <<"value">>},
    ?assertEqual(undefined, macula_connection_dispatch:get_map_field(Map, name, <<"name">>)).

get_map_field_with_default_test() ->
    Map = #{},
    ?assertEqual(<<"fallback">>, macula_connection_dispatch:get_map_field(Map, name, <<"name">>, <<"fallback">>)).

get_map_field_with_default_not_needed_test() ->
    Map = #{name => <<"found">>},
    ?assertEqual(<<"found">>, macula_connection_dispatch:get_map_field(Map, name, <<"name">>, <<"fallback">>)).
