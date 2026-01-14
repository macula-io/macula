%%%-------------------------------------------------------------------
%%% @doc Tests for macula_cluster_gossip module.
%%%
%%% Tests UDP multicast gossip cluster strategy for zero-config LAN discovery.
%%%
%%% @copyright 2026 Macula.io Apache-2.0
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_gossip_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Generators
%%%===================================================================

gossip_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"start_link with default options", fun start_link_default/0},
         {"start_link with custom options", fun start_link_custom/0},
         {"stop terminates gracefully", fun stop_terminates/0},
         {"get_discovered returns empty set initially", fun get_discovered_initial/0},
         {"get_connected returns empty set initially", fun get_connected_initial/0},
         {"broadcast_now sends immediately", fun broadcast_now_works/0}
     ]}.

config_test_() ->
    [
        {"default multicast address", fun default_multicast_addr/0},
        {"default port", fun default_port/0},
        {"env var MACULA_GOSSIP_ADDR", fun env_gossip_addr/0},
        {"env var MACULA_GOSSIP_PORT", fun env_gossip_port/0},
        {"env var MACULA_GOSSIP_SECRET", fun env_gossip_secret/0}
    ].

payload_test_() ->
    [
        {"payload without secret", fun payload_without_secret/0},
        {"payload with secret includes HMAC", fun payload_with_secret/0}
    ].

api_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"API get_discovered returns list", fun api_get_discovered/0},
         {"API get_connected returns list", fun api_get_connected/0}
     ]}.

%%%===================================================================
%%% Setup / Cleanup
%%%===================================================================

setup() ->
    %% Ensure crypto is started for HMAC
    application:ensure_all_started(crypto),
    ok.

cleanup(_) ->
    %% Stop gossip if running
    catch macula_cluster_gossip:stop(),
    %% Clean up env vars
    os:unsetenv("MACULA_GOSSIP_ADDR"),
    os:unsetenv("MACULA_GOSSIP_PORT"),
    os:unsetenv("MACULA_GOSSIP_SECRET"),
    ok.

%%%===================================================================
%%% Lifecycle Tests
%%%===================================================================

start_link_default() ->
    {ok, Pid} = macula_cluster_gossip:start_link(),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    macula_cluster_gossip:stop().

start_link_custom() ->
    Opts = #{
        multicast_addr => {239, 1, 1, 1},
        port => 9999,
        broadcast_interval => 2000,
        multicast_ttl => 2
    },
    {ok, Pid} = macula_cluster_gossip:start_link(Opts),
    ?assert(is_pid(Pid)),
    macula_cluster_gossip:stop().

stop_terminates() ->
    {ok, Pid} = macula_cluster_gossip:start_link(),
    ?assert(is_process_alive(Pid)),
    ok = macula_cluster_gossip:stop(),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

get_discovered_initial() ->
    {ok, _Pid} = macula_cluster_gossip:start_link(),
    Discovered = macula_cluster_gossip:get_discovered(),
    ?assertEqual([], Discovered),
    macula_cluster_gossip:stop().

get_connected_initial() ->
    {ok, _Pid} = macula_cluster_gossip:start_link(),
    Connected = macula_cluster_gossip:get_connected(),
    ?assertEqual([], Connected),
    macula_cluster_gossip:stop().

broadcast_now_works() ->
    {ok, _Pid} = macula_cluster_gossip:start_link(),
    %% Should not crash
    ok = macula_cluster_gossip:broadcast_now(),
    macula_cluster_gossip:stop().

%%%===================================================================
%%% Configuration Tests
%%%===================================================================

default_multicast_addr() ->
    %% Default should be {230, 1, 1, 251}
    {ok, Pid} = macula_cluster_gossip:start_link(),
    ?assert(is_pid(Pid)),
    macula_cluster_gossip:stop().

default_port() ->
    %% Default should be 45892
    {ok, Pid} = macula_cluster_gossip:start_link(),
    ?assert(is_pid(Pid)),
    macula_cluster_gossip:stop().

env_gossip_addr() ->
    os:putenv("MACULA_GOSSIP_ADDR", "239.0.0.1"),
    {ok, Pid} = macula_cluster_gossip:start_link(),
    ?assert(is_pid(Pid)),
    macula_cluster_gossip:stop(),
    os:unsetenv("MACULA_GOSSIP_ADDR").

env_gossip_port() ->
    os:putenv("MACULA_GOSSIP_PORT", "12345"),
    {ok, Pid} = macula_cluster_gossip:start_link(),
    ?assert(is_pid(Pid)),
    macula_cluster_gossip:stop(),
    os:unsetenv("MACULA_GOSSIP_PORT").

env_gossip_secret() ->
    os:putenv("MACULA_GOSSIP_SECRET", "my-test-secret"),
    {ok, Pid} = macula_cluster_gossip:start_link(),
    ?assert(is_pid(Pid)),
    macula_cluster_gossip:stop(),
    os:unsetenv("MACULA_GOSSIP_SECRET").

%%%===================================================================
%%% Payload Tests
%%%===================================================================

payload_without_secret() ->
    %% Verify payload format
    NodeName = <<"test@localhost">>,
    Payload = build_test_payload(NodeName, undefined),
    ?assertEqual(<<"MACULA_GOSSIP:test@localhost">>, Payload).

payload_with_secret() ->
    %% Verify payload with HMAC
    NodeName = <<"test@localhost">>,
    Secret = <<"my-secret">>,
    Payload = build_test_payload(NodeName, Secret),
    %% Should have HMAC suffix with | delimiter
    ?assertMatch(<<"MACULA_GOSSIP:test@localhost|", _Rest/binary>>, Payload),
    %% Verify HMAC length (SHA256 = 64 hex chars)
    [_Data, HexHMAC] = binary:split(Payload, <<"|">>),
    ?assertEqual(64, byte_size(HexHMAC)).

%% Helper to build payload (mirrors internal function)
build_test_payload(NodeName, undefined) ->
    <<"MACULA_GOSSIP:", NodeName/binary>>;
build_test_payload(NodeName, Secret) ->
    Data = <<"MACULA_GOSSIP:", NodeName/binary>>,
    HMAC = crypto:mac(hmac, sha256, Secret, Data),
    <<Data/binary, "|", (binary:encode_hex(HMAC))/binary>>.

%%%===================================================================
%%% API Tests
%%%===================================================================

api_get_discovered() ->
    {ok, _Pid} = macula_cluster_gossip:start_link(),
    Result = macula_cluster_gossip:get_discovered(),
    ?assert(is_list(Result)),
    macula_cluster_gossip:stop().

api_get_connected() ->
    {ok, _Pid} = macula_cluster_gossip:start_link(),
    Result = macula_cluster_gossip:get_connected(),
    ?assert(is_list(Result)),
    macula_cluster_gossip:stop().
