%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_nat_detector module.
%%%
%%% Tests the pure functional parts of NAT type detection:
%%% - NAT type classification logic (mapping, filtering, allocation)
%%% - Address comparison and parsing helpers
%%% - Port mapping analysis
%%% - IP address classification (public, private, loopback)
%%% - Message field extraction
%%% - State initialization and gen_server lifecycle
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_detector_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Mapping Policy Classification Tests
%%%===================================================================

mapping_policy_endpoint_independent_test() ->
    %% Same IP and same port from two observers => EI
    IP = {203, 0, 113, 50},
    Port = 12345,
    ?assertEqual(ei, macula_nat_detector:classify_mapping_policy(IP, Port, IP, Port)).

mapping_policy_host_dependent_test() ->
    %% Same IP but different ports => HD
    IP = {203, 0, 113, 50},
    ?assertEqual(hd, macula_nat_detector:classify_mapping_policy(IP, 12345, IP, 54321)).

mapping_policy_port_dependent_test() ->
    %% Different IPs => PD (symmetric NAT)
    ?assertEqual(pd, macula_nat_detector:classify_mapping_policy(
        {203, 0, 113, 50}, 12345,
        {198, 51, 100, 1}, 54321)).

mapping_policy_different_ip_same_port_test() ->
    %% Different IPs, same port => still PD
    ?assertEqual(pd, macula_nat_detector:classify_mapping_policy(
        {203, 0, 113, 50}, 5000,
        {198, 51, 100, 1}, 5000)).

%%%===================================================================
%%% Allocation Policy Classification Tests
%%%===================================================================

allocation_policy_port_preservation_test() ->
    %% All four ports identical => PP (port preservation)
    ?assertEqual(pp, macula_nat_detector:classify_allocation_policy(5000, 5000, 5000, 5000)).

allocation_policy_port_contiguity_test() ->
    %% External ports close together (delta < 10) => PC
    ?assertEqual(pc, macula_nat_detector:classify_allocation_policy(5000, 5001, 6000, 5005)).

allocation_policy_random_test() ->
    %% External ports far apart (delta >= 10) => RD
    ?assertEqual(rd, macula_nat_detector:classify_allocation_policy(5000, 10000, 6000, 40000)).

allocation_policy_contiguity_boundary_test() ->
    %% Delta of exactly 9 => PC (just under threshold)
    ?assertEqual(pc, macula_nat_detector:classify_allocation_policy(5000, 10000, 6000, 10009)).

allocation_policy_contiguity_boundary_at_10_test() ->
    %% Delta of exactly 10 => RD (at threshold)
    ?assertEqual(rd, macula_nat_detector:classify_allocation_policy(5000, 10000, 6000, 10010)).

%%%===================================================================
%%% Port Contiguity Classification Tests
%%%===================================================================

port_contiguity_zero_delta_test() ->
    ?assertEqual(pc, macula_nat_detector:classify_port_contiguity(0)).

port_contiguity_small_delta_test() ->
    ?assertEqual(pc, macula_nat_detector:classify_port_contiguity(5)).

port_contiguity_large_delta_test() ->
    ?assertEqual(rd, macula_nat_detector:classify_port_contiguity(100)).

%%%===================================================================
%%% Filtering Policy Inference Tests
%%%===================================================================

infer_filtering_from_ei_mapping_test() ->
    %% EI mapping => conservative PD filtering
    ?assertEqual(pd, macula_nat_detector:infer_filtering_policy(ei)).

infer_filtering_from_hd_mapping_test() ->
    ?assertEqual(pd, macula_nat_detector:infer_filtering_policy(hd)).

infer_filtering_from_pd_mapping_test() ->
    ?assertEqual(pd, macula_nat_detector:infer_filtering_policy(pd)).

%%%===================================================================
%%% Port Preservation Policy Tests
%%%===================================================================

port_preserved_test() ->
    ?assertEqual(pp, macula_nat_detector:port_preservation_policy(5000, 5000)).

port_not_preserved_test() ->
    %% Different external and local port => RD (can't determine with one sample)
    ?assertEqual(rd, macula_nat_detector:port_preservation_policy(12345, 5000)).

%%%===================================================================
%%% Reflexive Address Parsing Tests
%%%===================================================================

parse_reflexive_address_tuple_test() ->
    %% When IP is already a tuple, return as-is
    IP = {203, 0, 113, 50},
    ?assertEqual({IP, 5000}, macula_nat_detector:parse_reflexive_address(IP, 5000)).

parse_reflexive_address_binary_valid_test() ->
    %% Parse binary IP string
    ?assertEqual({{192, 168, 1, 1}, 8080},
                 macula_nat_detector:parse_reflexive_address(<<"192.168.1.1">>, 8080)).

parse_reflexive_address_binary_invalid_test() ->
    %% Invalid IP string => fallback to {0,0,0,0}
    ?assertEqual({{0, 0, 0, 0}, 9999},
                 macula_nat_detector:parse_reflexive_address(<<"not.an.ip.address">>, 9999)).

parse_reflexive_address_ipv6_binary_test() ->
    %% IPv6 address in binary form
    ?assertEqual({{0, 0, 0, 0, 0, 0, 0, 1}, 443},
                 macula_nat_detector:parse_reflexive_address(<<"::1">>, 443)).

%%%===================================================================
%%% Message Field Extraction Tests
%%%===================================================================

get_msg_field_atom_key_test() ->
    Map = #{reflexive_ip => <<"1.2.3.4">>},
    ?assertEqual(<<"1.2.3.4">>,
                 macula_nat_detector:get_msg_field(Map, reflexive_ip, <<"reflexive_ip">>)).

get_msg_field_binary_key_test() ->
    %% msgpack often returns binary keys
    Map = #{<<"reflexive_ip">> => <<"1.2.3.4">>},
    ?assertEqual(<<"1.2.3.4">>,
                 macula_nat_detector:get_msg_field(Map, reflexive_ip, <<"reflexive_ip">>)).

get_msg_field_missing_key_test() ->
    Map = #{other_field => <<"value">>},
    ?assertEqual(undefined,
                 macula_nat_detector:get_msg_field(Map, reflexive_ip, <<"reflexive_ip">>)).

get_msg_field_atom_takes_precedence_test() ->
    %% When both keys exist, atom key wins
    Map = #{reflexive_ip => <<"atom_value">>, <<"reflexive_ip">> => <<"binary_value">>},
    ?assertEqual(<<"atom_value">>,
                 macula_nat_detector:get_msg_field(Map, reflexive_ip, <<"reflexive_ip">>)).

%%%===================================================================
%%% Observer ID Normalization Tests
%%%===================================================================

ensure_observer_id_undefined_test() ->
    %% undefined observer ID falls back to endpoint
    ?assertEqual(<<"quic://1.2.3.4:9443">>,
                 macula_nat_detector:ensure_observer_id(undefined, <<"quic://1.2.3.4:9443">>)).

ensure_observer_id_valid_binary_test() ->
    ?assertEqual(<<"node-abc">>,
                 macula_nat_detector:ensure_observer_id(<<"node-abc">>, <<"quic://1.2.3.4:9443">>)).

ensure_observer_id_non_binary_test() ->
    %% Non-binary ID falls back to endpoint
    ?assertEqual(<<"quic://1.2.3.4:9443">>,
                 macula_nat_detector:ensure_observer_id(12345, <<"quic://1.2.3.4:9443">>)).

%%%===================================================================
%%% IP Address Classification Tests
%%%===================================================================

is_public_ip_test() ->
    ?assertEqual(true,  macula_nat_detector:is_public_ip({8, 8, 8, 8})),
    ?assertEqual(true,  macula_nat_detector:is_public_ip({203, 0, 113, 1})),
    ?assertEqual(true,  macula_nat_detector:is_public_ip({1, 1, 1, 1})).

is_private_ip_class_a_test() ->
    ?assertEqual(false, macula_nat_detector:is_public_ip({10, 0, 0, 1})),
    ?assertEqual(false, macula_nat_detector:is_public_ip({10, 255, 255, 255})).

is_private_ip_class_b_test() ->
    ?assertEqual(false, macula_nat_detector:is_public_ip({172, 16, 0, 1})),
    ?assertEqual(false, macula_nat_detector:is_public_ip({172, 31, 255, 255})),
    %% 172.15.x.x is public
    ?assertEqual(true,  macula_nat_detector:is_public_ip({172, 15, 0, 1})),
    %% 172.32.x.x is public
    ?assertEqual(true,  macula_nat_detector:is_public_ip({172, 32, 0, 1})).

is_private_ip_class_c_test() ->
    ?assertEqual(false, macula_nat_detector:is_public_ip({192, 168, 0, 1})),
    ?assertEqual(false, macula_nat_detector:is_public_ip({192, 168, 255, 255})).

is_loopback_ip_test() ->
    ?assertEqual(false, macula_nat_detector:is_public_ip({127, 0, 0, 1})),
    ?assertEqual(false, macula_nat_detector:is_public_ip({127, 255, 255, 255})).

is_link_local_ip_test() ->
    ?assertEqual(false, macula_nat_detector:is_public_ip({169, 254, 0, 1})),
    ?assertEqual(false, macula_nat_detector:is_public_ip({169, 254, 255, 255})).

is_unspecified_ip_test() ->
    ?assertEqual(false, macula_nat_detector:is_public_ip({0, 0, 0, 0})).

is_public_ip_non_ipv4_test() ->
    %% Non-IPv4 returns false
    ?assertEqual(false, macula_nat_detector:is_public_ip(not_an_ip)).

%%%===================================================================
%%% Private IP Helper Tests
%%%===================================================================

is_private_ip_helper_test() ->
    ?assertEqual(true,  macula_nat_detector:is_private_ip(10, 0, 0)),
    ?assertEqual(true,  macula_nat_detector:is_private_ip(172, 16, 0)),
    ?assertEqual(true,  macula_nat_detector:is_private_ip(172, 31, 0)),
    ?assertEqual(false, macula_nat_detector:is_private_ip(172, 15, 0)),
    ?assertEqual(false, macula_nat_detector:is_private_ip(172, 32, 0)),
    ?assertEqual(true,  macula_nat_detector:is_private_ip(192, 168, 0)),
    ?assertEqual(false, macula_nat_detector:is_private_ip(8, 8, 8)).

is_loopback_helper_test() ->
    ?assertEqual(true,  macula_nat_detector:is_loopback(127)),
    ?assertEqual(false, macula_nat_detector:is_loopback(128)),
    ?assertEqual(false, macula_nat_detector:is_loopback(10)).

%%%===================================================================
%%% IP Selection Tests
%%%===================================================================

select_best_ip_empty_test() ->
    ?assertEqual({0, 0, 0, 0}, macula_nat_detector:select_best_ip([])).

select_best_ip_prefers_non_loopback_test() ->
    IPs = [
        {{127, 0, 0, 1}, false, true},
        {{192, 168, 1, 100}, true, false}
    ],
    ?assertEqual({192, 168, 1, 100}, macula_nat_detector:select_best_ip(IPs)).

select_best_ip_prefers_private_test() ->
    IPs = [
        {{8, 8, 8, 8}, false, false},
        {{192, 168, 1, 100}, true, false}
    ],
    ?assertEqual({192, 168, 1, 100}, macula_nat_detector:select_best_ip(IPs)).

select_best_ip_loopback_only_test() ->
    IPs = [{{127, 0, 0, 1}, false, true}],
    ?assertEqual({127, 0, 0, 1}, macula_nat_detector:select_best_ip(IPs)).

%%%===================================================================
%%% Environment Variable Parsing Tests
%%%===================================================================

parse_float_env_unset_test() ->
    %% Use a variable name that won't exist
    os:unsetenv("MACULA_TEST_FLOAT_PARSE"),
    ?assertEqual(undefined, macula_nat_detector:parse_float_env("MACULA_TEST_FLOAT_PARSE")).

parse_float_env_valid_float_test() ->
    os:putenv("MACULA_TEST_FLOAT_PARSE", "52.3702"),
    ?assertEqual(52.3702, macula_nat_detector:parse_float_env("MACULA_TEST_FLOAT_PARSE")),
    os:unsetenv("MACULA_TEST_FLOAT_PARSE").

parse_float_env_integer_value_test() ->
    %% Integer string should be converted to float
    os:putenv("MACULA_TEST_FLOAT_PARSE", "42"),
    ?assertEqual(42.0, macula_nat_detector:parse_float_env("MACULA_TEST_FLOAT_PARSE")),
    os:unsetenv("MACULA_TEST_FLOAT_PARSE").

parse_float_env_empty_string_test() ->
    os:putenv("MACULA_TEST_FLOAT_PARSE", ""),
    ?assertEqual(undefined, macula_nat_detector:parse_float_env("MACULA_TEST_FLOAT_PARSE")),
    os:unsetenv("MACULA_TEST_FLOAT_PARSE").

parse_float_env_invalid_test() ->
    os:putenv("MACULA_TEST_FLOAT_PARSE", "not_a_number"),
    ?assertEqual(undefined, macula_nat_detector:parse_float_env("MACULA_TEST_FLOAT_PARSE")),
    os:unsetenv("MACULA_TEST_FLOAT_PARSE").

get_binary_env_unset_test() ->
    os:unsetenv("MACULA_TEST_BINARY_ENV"),
    ?assertEqual(undefined, macula_nat_detector:get_binary_env("MACULA_TEST_BINARY_ENV")).

get_binary_env_valid_test() ->
    os:putenv("MACULA_TEST_BINARY_ENV", "Amsterdam, NL"),
    ?assertEqual(<<"Amsterdam, NL">>, macula_nat_detector:get_binary_env("MACULA_TEST_BINARY_ENV")),
    os:unsetenv("MACULA_TEST_BINARY_ENV").

get_binary_env_empty_test() ->
    os:putenv("MACULA_TEST_BINARY_ENV", ""),
    ?assertEqual(undefined, macula_nat_detector:get_binary_env("MACULA_TEST_BINARY_ENV")),
    os:unsetenv("MACULA_TEST_BINARY_ENV").

%%%===================================================================
%%% Create Profile Tests
%%%===================================================================

create_profile_test_() ->
    {setup,
     fun() ->
         %% Ensure no geo env vars set to get predictable profiles
         os:unsetenv("MACULA_LATITUDE"),
         os:unsetenv("MACULA_LONGITUDE"),
         os:unsetenv("MACULA_LOCATION")
     end,
     fun(_) -> ok end,
     [
         {"full cone profile", fun create_profile_full_cone/0},
         {"symmetric NAT profile", fun create_profile_symmetric/0},
         {"common consumer NAT profile", fun create_profile_common_consumer/0}
     ]}.

create_profile_full_cone() ->
    %% EI mapping + EI filtering => can receive unsolicited, no relay needed
    Profile = macula_nat_detector:create_profile(ei, ei, pp),
    ?assertEqual(ei, maps:get(mapping_policy, Profile)),
    ?assertEqual(ei, maps:get(filtering_policy, Profile)),
    ?assertEqual(pp, maps:get(allocation_policy, Profile)),
    ?assertEqual(true, maps:get(can_receive_unsolicited, Profile)),
    ?assertEqual(false, maps:get(requires_relay, Profile)).

create_profile_symmetric() ->
    %% PD mapping + RD allocation => requires relay
    Profile = macula_nat_detector:create_profile(pd, pd, rd),
    ?assertEqual(pd, maps:get(mapping_policy, Profile)),
    ?assertEqual(pd, maps:get(filtering_policy, Profile)),
    ?assertEqual(rd, maps:get(allocation_policy, Profile)),
    ?assertEqual(false, maps:get(can_receive_unsolicited, Profile)),
    ?assertEqual(true, maps:get(requires_relay, Profile)).

create_profile_common_consumer() ->
    %% EI mapping + PD filtering + PP allocation (37% of consumer NATs per NATCracker)
    Profile = macula_nat_detector:create_profile(ei, pd, pp),
    ?assertEqual(ei, maps:get(mapping_policy, Profile)),
    ?assertEqual(pd, maps:get(filtering_policy, Profile)),
    ?assertEqual(pp, maps:get(allocation_policy, Profile)),
    ?assertEqual(false, maps:get(can_receive_unsolicited, Profile)),
    ?assertEqual(false, maps:get(requires_relay, Profile)),
    %% Profile should have required fields
    ?assert(maps:is_key(node_id, Profile)),
    ?assert(maps:is_key(detected_at, Profile)),
    ?assert(maps:is_key(ttl_seconds, Profile)),
    ?assertEqual(300, maps:get(ttl_seconds, Profile)).

create_profile_with_geo_env_test() ->
    %% Set geo environment variables and verify they appear in profile
    os:putenv("MACULA_LATITUDE", "52.3702"),
    os:putenv("MACULA_LONGITUDE", "4.8952"),
    os:putenv("MACULA_LOCATION", "Amsterdam, NL"),
    Profile = macula_nat_detector:create_profile(ei, pd, pp),
    ?assertEqual(52.3702, maps:get(latitude, Profile)),
    ?assertEqual(4.8952, maps:get(longitude, Profile)),
    ?assertEqual(<<"Amsterdam, NL">>, maps:get(location_label, Profile)),
    os:unsetenv("MACULA_LATITUDE"),
    os:unsetenv("MACULA_LONGITUDE"),
    os:unsetenv("MACULA_LOCATION").

create_profile_without_geo_env_test() ->
    %% Without geo env vars, profile should not have geo fields
    os:unsetenv("MACULA_LATITUDE"),
    os:unsetenv("MACULA_LONGITUDE"),
    os:unsetenv("MACULA_LOCATION"),
    Profile = macula_nat_detector:create_profile(ei, pd, pp),
    ?assertEqual(false, maps:is_key(latitude, Profile)),
    ?assertEqual(false, maps:is_key(longitude, Profile)),
    ?assertEqual(false, maps:is_key(location_label, Profile)).

%%%===================================================================
%%% Gen Server Lifecycle Tests
%%%===================================================================

gen_server_lifecycle_test_() ->
    {foreach,
     fun start_detector/0,
     fun stop_detector/1,
     [
         {"initial profile is not_detected", fun test_initial_profile_not_detected/0},
         {"get_local_address returns IP and port", fun test_get_local_address_shape/0},
         {"get_local_quic_port returns valid port", fun test_get_local_quic_port_valid/0},
         {"refresh returns ok", fun test_refresh_returns_ok/0},
         {"unknown call returns error", fun test_unknown_call/0}
     ]}.

start_detector() ->
    {ok, Pid} = macula_nat_detector:start_link(#{detection_timeout_ms => 500}),
    Pid.

stop_detector(Pid) ->
    catch gen_server:stop(Pid),
    ok.

test_initial_profile_not_detected() ->
    ?assertEqual(not_detected, macula_nat_detector:get_local_profile()).

test_get_local_address_shape() ->
    {IP, Port} = macula_nat_detector:get_local_address(),
    ?assert(is_tuple(IP)),
    ?assert(is_integer(Port)),
    ?assert(Port > 0).

test_get_local_quic_port_valid() ->
    Port = macula_nat_detector:get_local_quic_port(),
    ?assert(is_integer(Port)),
    ?assert(Port > 0),
    ?assert(Port < 65536).

test_refresh_returns_ok() ->
    ?assertEqual(ok, macula_nat_detector:refresh()).

test_unknown_call() ->
    ?assertEqual({error, unknown_request},
                 gen_server:call(macula_nat_detector, some_unknown_request)).

%%%===================================================================
%%% Extract IPv4 Addresses Tests
%%%===================================================================

extract_ipv4_addresses_empty_test() ->
    ?assertEqual([], macula_nat_detector:extract_ipv4_addresses([])).

extract_ipv4_addresses_with_interfaces_test() ->
    %% Simulate interface data similar to inet:getifaddrs/0
    Interfaces = [
        {"lo", [{addr, {127, 0, 0, 1}}, {netmask, {255, 0, 0, 0}}]},
        {"eth0", [{addr, {192, 168, 1, 100}}, {netmask, {255, 255, 255, 0}}]}
    ],
    Result = macula_nat_detector:extract_ipv4_addresses(Interfaces),
    ?assertEqual(2, length(Result)),
    %% Each entry should be {IP, IsPrivate, IsLoopback}
    lists:foreach(fun({IP, IsPrivate, IsLoopback}) ->
        ?assert(is_tuple(IP)),
        ?assert(is_boolean(IsPrivate)),
        ?assert(is_boolean(IsLoopback))
    end, Result).

extract_ipv4_addresses_skips_ipv6_test() ->
    Interfaces = [
        {"eth0", [{addr, {0, 0, 0, 0, 0, 0, 0, 1}}]}
    ],
    ?assertEqual([], macula_nat_detector:extract_ipv4_addresses(Interfaces)).
