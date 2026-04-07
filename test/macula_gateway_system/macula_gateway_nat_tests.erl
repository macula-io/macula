%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for untested code paths in macula_gateway:
%%% - NAT probe helpers (format_ip_address, get_peer_address)
%%% - Punch coordinate helpers (normalize_role, ensure_port_list, get_punch_field)
%%% - Request forwarding decision (should_forward_request)
%%% - Endpoint building (get_advertise_port, get_gateway_hostname, build_gateway_endpoint)
%%% - Supervisor child lookup (find_sibling_in_children, find_gateway_system_in_children)
%%% - Peer formatting (format_peer_for_pong)
%%% - Stale stream cleanup (cleanup_stale_stream)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_nat_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% format_ip_address/1 Tests
%%%===================================================================

format_ipv4_address_test() ->
    Result = macula_gateway:format_ip_address({192, 168, 1, 1}),
    ?assertEqual(<<"192.168.1.1">>, Result).

format_ipv4_loopback_test() ->
    Result = macula_gateway:format_ip_address({127, 0, 0, 1}),
    ?assertEqual(<<"127.0.0.1">>, Result).

format_ipv6_address_test() ->
    Result = macula_gateway:format_ip_address({0, 0, 0, 0, 0, 16#FFFF, 16#C0A8, 16#0101}),
    ?assert(is_binary(Result)),
    %% Should be a colon-separated hex string
    ?assertMatch(<<_/binary>>, Result),
    %% Verify it contains colons (IPv6 format)
    ?assert(binary:match(Result, <<":">>) =/= nomatch).

format_ipv6_all_zeros_test() ->
    Result = macula_gateway:format_ip_address({0, 0, 0, 0, 0, 0, 0, 0}),
    ?assertEqual(<<"0000:0000:0000:0000:0000:0000:0000:0000">>, Result).

format_ipv6_full_test() ->
    Result = macula_gateway:format_ip_address({16#2001, 16#0DB8, 16#85A3, 0, 0, 16#8A2E, 16#0370, 16#7334}),
    ?assertEqual(<<"2001:0DB8:85A3:0000:0000:8A2E:0370:7334">>, Result).

%%%===================================================================
%%% normalize_role/1 Tests
%%%===================================================================

normalize_role_binary_initiator_test() ->
    ?assertEqual(initiator, macula_gateway:normalize_role(<<"initiator">>)).

normalize_role_binary_target_test() ->
    ?assertEqual(target, macula_gateway:normalize_role(<<"target">>)).

normalize_role_atom_initiator_test() ->
    ?assertEqual(initiator, macula_gateway:normalize_role(initiator)).

normalize_role_atom_target_test() ->
    ?assertEqual(target, macula_gateway:normalize_role(target)).

normalize_role_unknown_defaults_to_initiator_test() ->
    ?assertEqual(initiator, macula_gateway:normalize_role(<<"unknown">>)).

normalize_role_integer_defaults_to_initiator_test() ->
    ?assertEqual(initiator, macula_gateway:normalize_role(42)).

%%%===================================================================
%%% ensure_port_list/1 Tests
%%%===================================================================

ensure_port_list_already_list_test() ->
    ?assertEqual([4433, 4434], macula_gateway:ensure_port_list([4433, 4434])).

ensure_port_list_single_integer_test() ->
    ?assertEqual([9443], macula_gateway:ensure_port_list(9443)).

ensure_port_list_empty_list_test() ->
    ?assertEqual([], macula_gateway:ensure_port_list([])).

ensure_port_list_invalid_returns_empty_test() ->
    ?assertEqual([], macula_gateway:ensure_port_list(<<"not_a_port">>)).

ensure_port_list_undefined_returns_empty_test() ->
    ?assertEqual([], macula_gateway:ensure_port_list(undefined)).

%%%===================================================================
%%% get_punch_field/3 Tests
%%%===================================================================

get_punch_field_atom_key_test() ->
    Msg = #{session_id => <<"abc123">>},
    ?assertEqual(<<"abc123">>, macula_gateway:get_punch_field(Msg, session_id, <<"session_id">>)).

get_punch_field_binary_key_test() ->
    Msg = #{<<"session_id">> => <<"abc123">>},
    ?assertEqual(<<"abc123">>, macula_gateway:get_punch_field(Msg, session_id, <<"session_id">>)).

get_punch_field_atom_key_takes_priority_test() ->
    Msg = #{session_id => <<"atom_val">>, <<"session_id">> => <<"binary_val">>},
    ?assertEqual(<<"atom_val">>, macula_gateway:get_punch_field(Msg, session_id, <<"session_id">>)).

get_punch_field_missing_returns_undefined_test() ->
    Msg = #{},
    ?assertEqual(undefined, macula_gateway:get_punch_field(Msg, session_id, <<"session_id">>)).

%%%===================================================================
%%% should_forward_request/2 Tests
%%%===================================================================

should_forward_undefined_target_test() ->
    LocalNodeId = crypto:strong_rand_bytes(32),
    ?assertEqual(false, macula_gateway:should_forward_request(undefined, LocalNodeId)).

should_forward_same_node_test() ->
    NodeId = crypto:strong_rand_bytes(32),
    ?assertEqual(false, macula_gateway:should_forward_request(NodeId, NodeId)).

should_forward_different_node_test() ->
    LocalNodeId = crypto:strong_rand_bytes(32),
    RemoteNodeId = crypto:strong_rand_bytes(32),
    ?assertEqual(true, macula_gateway:should_forward_request(RemoteNodeId, LocalNodeId)).

%%%===================================================================
%%% get_advertise_port/1 Tests
%%%===================================================================

get_advertise_port_no_env_test() ->
    os:unsetenv("MACULA_ADVERTISE_PORT"),
    ?assertEqual(9443, macula_gateway:get_advertise_port(9443)).

get_advertise_port_with_env_test() ->
    os:putenv("MACULA_ADVERTISE_PORT", "443"),
    Result = macula_gateway:get_advertise_port(9443),
    os:unsetenv("MACULA_ADVERTISE_PORT"),
    ?assertEqual(443, Result).

%%%===================================================================
%%% get_gateway_hostname/0 Tests
%%%===================================================================

get_gateway_hostname_no_env_test() ->
    OldMacula = os:getenv("MACULA_HOSTNAME"),
    OldHostname = os:getenv("HOSTNAME"),
    os:unsetenv("MACULA_HOSTNAME"),
    os:unsetenv("HOSTNAME"),
    Result = macula_gateway:get_gateway_hostname(),
    %% Restore
    restore_env("MACULA_HOSTNAME", OldMacula),
    restore_env("HOSTNAME", OldHostname),
    ?assertEqual(<<"localhost">>, Result).

get_gateway_hostname_with_hostname_env_test() ->
    OldMacula = os:getenv("MACULA_HOSTNAME"),
    OldHostname = os:getenv("HOSTNAME"),
    os:unsetenv("MACULA_HOSTNAME"),
    os:putenv("HOSTNAME", "mycontainer"),
    Result = macula_gateway:get_gateway_hostname(),
    %% Restore
    restore_env("MACULA_HOSTNAME", OldMacula),
    restore_env("HOSTNAME", OldHostname),
    ?assertEqual(<<"mycontainer">>, Result).

get_gateway_hostname_macula_hostname_takes_priority_test() ->
    OldMacula = os:getenv("MACULA_HOSTNAME"),
    OldHostname = os:getenv("HOSTNAME"),
    os:putenv("MACULA_HOSTNAME", "gateway.macula.io"),
    os:putenv("HOSTNAME", "mycontainer"),
    Result = macula_gateway:get_gateway_hostname(),
    %% Restore
    restore_env("MACULA_HOSTNAME", OldMacula),
    restore_env("HOSTNAME", OldHostname),
    ?assertEqual(<<"gateway.macula.io">>, Result).

%%%===================================================================
%%% get_peer_address/1 Tests
%%%===================================================================

get_peer_address_from_process_dict_test() ->
    %% Simulate cached address in process dictionary
    put(current_peer_addr, {ok, {{192,168,1,100}, 4433}}),
    Result = macula_gateway:get_peer_address(fake_stream),
    erase(current_peer_addr),
    ?assertEqual({ok, {{192,168,1,100}, 4433}}, Result).

get_peer_address_no_cache_falls_through_test() ->
    %% No cached address - will try macula_quic:peername which will fail
    %% on a fake stream, but we verify the fallback path is taken
    erase(current_peer_addr),
    Result = macula_gateway:get_peer_address(fake_stream),
    %% Should return an error since fake_stream is not a real QUIC stream
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% find_sibling_in_children/2 Tests
%%%===================================================================

find_sibling_found_test() ->
    FakePid = spawn(fun() -> receive stop -> ok end end),
    Children = [
        {some_module, FakePid, worker, [some_module]},
        {other_module, spawn(fun() -> ok end), worker, [other_module]}
    ],
    Result = macula_gateway:find_sibling_in_children(Children, some_module),
    FakePid ! stop,
    ?assertEqual({ok, FakePid}, Result).

find_sibling_not_found_test() ->
    Children = [
        {some_module, spawn(fun() -> ok end), worker, [some_module]}
    ],
    Result = macula_gateway:find_sibling_in_children(Children, missing_module),
    ?assertEqual({error, {not_found, missing_module}}, Result).

find_sibling_empty_children_test() ->
    Result = macula_gateway:find_sibling_in_children([], any_module),
    ?assertEqual({error, {not_found, any_module}}, Result).

%%%===================================================================
%%% find_gateway_system_in_children/1 Tests
%%%===================================================================

find_gateway_system_found_test() ->
    FakePid = spawn(fun() -> receive stop -> ok end end),
    Children = [
        {macula_gateway_system, FakePid, supervisor, [macula_gateway_system]}
    ],
    Result = macula_gateway:find_gateway_system_in_children(Children),
    FakePid ! stop,
    ?assertEqual({ok, FakePid}, Result).

find_gateway_system_not_found_test() ->
    Children = [
        {macula_routing_system, spawn(fun() -> ok end), supervisor, [macula_routing_system]}
    ],
    Result = macula_gateway:find_gateway_system_in_children(Children),
    ?assertEqual({error, gateway_system_not_found}, Result).

find_gateway_system_empty_test() ->
    Result = macula_gateway:find_gateway_system_in_children([]),
    ?assertEqual({error, gateway_system_not_found}, Result).

%%%===================================================================
%%% format_peer_for_pong/1 Tests
%%%===================================================================

format_peer_for_pong_with_endpoint_test() ->
    NodeId = crypto:strong_rand_bytes(32),
    PeerInfo = #{node_id => NodeId, endpoint => <<"https://peer1:9443">>},
    Result = macula_gateway:format_peer_for_pong(PeerInfo),
    ?assertEqual(binary:encode_hex(NodeId), maps:get(<<"node_id">>, Result)),
    ?assertEqual(<<"https://peer1:9443">>, maps:get(<<"endpoint">>, Result)).

format_peer_for_pong_with_address_key_test() ->
    NodeId = crypto:strong_rand_bytes(32),
    PeerInfo = #{node_id => NodeId, address => <<"https://peer2:4433">>},
    Result = macula_gateway:format_peer_for_pong(PeerInfo),
    ?assertEqual(binary:encode_hex(NodeId), maps:get(<<"node_id">>, Result)),
    ?assertEqual(<<"https://peer2:4433">>, maps:get(<<"endpoint">>, Result)).

format_peer_for_pong_no_endpoint_test() ->
    NodeId = crypto:strong_rand_bytes(32),
    PeerInfo = #{node_id => NodeId},
    Result = macula_gateway:format_peer_for_pong(PeerInfo),
    ?assertEqual(binary:encode_hex(NodeId), maps:get(<<"node_id">>, Result)),
    ?assertEqual(undefined, maps:get(<<"endpoint">>, Result)).

format_peer_for_pong_invalid_map_test() ->
    Result = macula_gateway:format_peer_for_pong(#{}),
    ?assertEqual(#{}, Result).

%%%===================================================================
%%% cleanup_stale_stream/4 Tests
%%%===================================================================

cleanup_stale_stream_undefined_client_mgr_test() ->
    %% Should be a no-op when client manager is undefined
    ?assertEqual(ok, macula_gateway:cleanup_stale_stream(undefined, <<"node">>, <<"hex">>, closed)).

cleanup_stale_stream_non_connection_error_test() ->
    %% Non-connection errors should not trigger cleanup
    FakeMgr = spawn(fun() -> receive _ -> ok end end),
    ?assertEqual(ok, macula_gateway:cleanup_stale_stream(FakeMgr, <<"hex">>, <<"node">>, timeout)),
    FakeMgr ! stop.

%%%===================================================================
%%% build_gateway_endpoint/1 Tests
%%%===================================================================

build_gateway_endpoint_test() ->
    OldMacula = os:getenv("MACULA_HOSTNAME"),
    OldHostname = os:getenv("HOSTNAME"),
    OldPort = os:getenv("MACULA_ADVERTISE_PORT"),
    os:putenv("MACULA_HOSTNAME", "relay.macula.io"),
    os:unsetenv("MACULA_ADVERTISE_PORT"),
    %% Build a fake state record with port field
    %% The record is {state, Port, Realm, NodeId, ParentSup, Listener, Supervisor,
    %%                ClientManager, PubSub, Rpc, Mesh, ClientStreams}
    State = {state, 9443, <<"test.realm">>, undefined, undefined, undefined, undefined,
             undefined, undefined, undefined, undefined, #{}},
    Result = macula_gateway:build_gateway_endpoint(State),
    %% Restore
    restore_env("MACULA_HOSTNAME", OldMacula),
    restore_env("HOSTNAME", OldHostname),
    restore_env("MACULA_ADVERTISE_PORT", OldPort),
    ?assertEqual(<<"https://relay.macula.io:9443">>, Result).

build_gateway_endpoint_with_advertise_port_test() ->
    OldMacula = os:getenv("MACULA_HOSTNAME"),
    OldPort = os:getenv("MACULA_ADVERTISE_PORT"),
    os:putenv("MACULA_HOSTNAME", "relay.macula.io"),
    os:putenv("MACULA_ADVERTISE_PORT", "443"),
    State = {state, 9443, <<"test.realm">>, undefined, undefined, undefined, undefined,
             undefined, undefined, undefined, undefined, #{}},
    Result = macula_gateway:build_gateway_endpoint(State),
    %% Restore
    restore_env("MACULA_HOSTNAME", OldMacula),
    restore_env("MACULA_ADVERTISE_PORT", OldPort),
    ?assertEqual(<<"https://relay.macula.io:443">>, Result).

%%%===================================================================
%%% Helpers
%%%===================================================================

restore_env(Key, false) ->
    os:unsetenv(Key);
restore_env(Key, Value) when is_list(Value) ->
    os:putenv(Key, Value).
