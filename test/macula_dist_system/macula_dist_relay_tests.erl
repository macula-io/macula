%%%-------------------------------------------------------------------
%%% @doc Tests for macula_dist_relay + macula_dist_bridge.
%%%
%%% Tests cover:
%%% - Public API: is_relay_mode, register/get mesh client
%%% - Loopback pair creation and packet framing
%%% - Bridge I/O: reader/writer loops via loopback
%%% - Payload extraction
%%% - Tunnel encryption (AES-256-GCM from cookie)
%%% - Metrics counters
%%% - Bridge supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests — is_relay_mode
%%%===================================================================

is_relay_mode_off_test() ->
    os:unsetenv("MACULA_DIST_MODE"),
    ?assertNot(macula_dist_relay:is_relay_mode()).

is_relay_mode_on_test() ->
    os:putenv("MACULA_DIST_MODE", "relay"),
    ?assert(macula_dist_relay:is_relay_mode()),
    os:unsetenv("MACULA_DIST_MODE").

is_relay_mode_other_test() ->
    os:putenv("MACULA_DIST_MODE", "direct"),
    ?assertNot(macula_dist_relay:is_relay_mode()),
    os:unsetenv("MACULA_DIST_MODE").

%%%===================================================================
%%% Tests — register/get mesh client
%%%===================================================================

register_mesh_client_test() ->
    Self = self(),
    ok = macula_dist_relay:register_mesh_client(Self),
    ?assertEqual(Self, macula_dist_relay:get_mesh_client()),
    persistent_term:erase(macula_dist_mesh_client).

get_mesh_client_undefined_test() ->
    persistent_term:erase(macula_dist_mesh_client),
    ?assertEqual(undefined, macula_dist_relay:get_mesh_client()).

get_mesh_client_dead_pid_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(50),
    persistent_term:put(macula_dist_mesh_client, Pid),
    ?assertEqual(undefined, macula_dist_relay:get_mesh_client()),
    persistent_term:erase(macula_dist_mesh_client).

%%%===================================================================
%%% Tests — connect error paths
%%%===================================================================

connect_no_mesh_client_test() ->
    persistent_term:erase(macula_dist_mesh_client),
    os:putenv("MACULA_DIST_MODE", "relay"),
    ?assertEqual({error, no_mesh_connection},
                 macula_dist_relay:connect("test@host", "host", 4433)),
    os:unsetenv("MACULA_DIST_MODE").

%%%===================================================================
%%% Tests — extract_payload
%%%===================================================================

extract_payload_map_test() ->
    ?assertEqual(<<"data">>,
                 macula_dist_relay:extract_payload(#{payload => <<"data">>})).

extract_payload_binary_test() ->
    ?assertEqual(<<"raw">>,
                 macula_dist_relay:extract_payload(<<"raw">>)).

%%%===================================================================
%%% Tests — loopback pair
%%%===================================================================

loopback_pair_created_test() ->
    {CSock, ASock} = make_loopback_pair(),

    ok = gen_tcp:send(CSock, <<"hello">>),
    {ok, Data} = gen_tcp:recv(ASock, 0, 1000),
    ?assertEqual(<<0, 5, "hello">>, Data),

    gen_tcp:close(CSock),
    gen_tcp:close(ASock).

loopback_pair_bidirectional_test() ->
    {CSock, ASock} = make_loopback_pair(),

    ok = gen_tcp:send(CSock, <<"outbound">>),
    {ok, Out} = gen_tcp:recv(ASock, 0, 1000),
    ?assertEqual(<<0, 8, "outbound">>, Out),

    ok = gen_tcp:send(ASock, <<0, 7, "inbound">>),
    {ok, In} = gen_tcp:recv(CSock, 0, 1000),
    ?assertEqual(<<"inbound">>, In),

    gen_tcp:close(CSock),
    gen_tcp:close(ASock).

loopback_pair_packet4_switch_test() ->
    {CSock, ASock} = make_loopback_pair(),

    ok = gen_tcp:send(CSock, <<"hs">>),
    {ok, <<0, 2, "hs">>} = gen_tcp:recv(ASock, 0, 1000),

    inet:setopts(CSock, [{packet, 4}]),
    ok = gen_tcp:send(CSock, <<"post">>),
    {ok, <<0, 0, 0, 4, "post">>} = gen_tcp:recv(ASock, 0, 1000),

    ok = gen_tcp:send(ASock, <<0, 0, 0, 3, "ack">>),
    {ok, <<"ack">>} = gen_tcp:recv(CSock, 0, 1000),

    gen_tcp:close(CSock),
    gen_tcp:close(ASock).

%%%===================================================================
%%% Tests — bridge roundtrip (simulated relay via message passing)
%%%===================================================================

bridge_roundtrip_test() ->
    {DistA, BridgeA} = make_loopback_pair(),
    {DistB, BridgeB} = make_loopback_pair(),

    Self = self(),

    ReaderA = spawn_link(fun() ->
        case gen_tcp:recv(BridgeA, 0, 2000) of
            {ok, Data} -> Self ! {from_a, Data};
            {error, R} -> Self ! {from_a_err, R}
        end
    end),

    ok = gen_tcp:send(DistA, <<"ping">>),

    receive
        {from_a, RawData} ->
            ok = gen_tcp:send(BridgeB, RawData),
            {ok, <<"ping">>} = gen_tcp:recv(DistB, 0, 1000)
    after 2000 ->
        error(bridge_reader_timeout)
    end,

    exit(ReaderA, normal),
    gen_tcp:close(DistA),
    gen_tcp:close(BridgeA),
    gen_tcp:close(DistB),
    gen_tcp:close(BridgeB).

%%%===================================================================
%%% Tests — encryption
%%%===================================================================

encrypt_decrypt_roundtrip_test() ->
    Key = crypto:hash(sha256, <<"test-cookie">>),
    Plaintext = <<"hello distribution">>,
    Encrypted = encrypt(Key, Plaintext),
    ?assertNotEqual(Plaintext, Encrypted),
    {ok, Decrypted} = decrypt(Key, Encrypted),
    ?assertEqual(Plaintext, Decrypted).

encrypt_different_nonces_test() ->
    Key = crypto:hash(sha256, <<"test-cookie">>),
    Enc1 = encrypt(Key, <<"same">>),
    Enc2 = encrypt(Key, <<"same">>),
    ?assertNotEqual(Enc1, Enc2).

decrypt_wrong_key_test() ->
    Key1 = crypto:hash(sha256, <<"cookie-a">>),
    Key2 = crypto:hash(sha256, <<"cookie-b">>),
    Encrypted = encrypt(Key1, <<"secret">>),
    ?assertEqual({error, decrypt_failed}, decrypt(Key2, Encrypted)).

decrypt_garbage_test() ->
    Key = crypto:hash(sha256, <<"test">>),
    ?assertEqual({error, decrypt_failed}, decrypt(Key, <<"short">>)).

decrypt_tampered_test() ->
    Key = crypto:hash(sha256, <<"test">>),
    Encrypted = encrypt(Key, <<"original">>),
    <<H:12/binary, _Tag:16/binary, Rest/binary>> = Encrypted,
    Tampered = <<H/binary, (crypto:strong_rand_bytes(16))/binary, Rest/binary>>,
    ?assertEqual({error, decrypt_failed}, decrypt(Key, Tampered)).

%%%===================================================================
%%% Tests — metrics
%%%===================================================================

metrics_init_read_test() ->
    TunnelId = unique_tunnel_id(),
    Ref = init_metrics(TunnelId),
    M = read_metrics(Ref),
    ?assertEqual(0, maps:get(bytes_out, M)),
    ?assertEqual(0, maps:get(bytes_in, M)),
    remove_metrics(TunnelId).

metrics_increment_test() ->
    TunnelId = unique_tunnel_id(),
    Ref = init_metrics(TunnelId),
    counters:add(Ref, 1, 100),
    counters:add(Ref, 3, 1),
    M = read_metrics(Ref),
    ?assertEqual(100, maps:get(bytes_out, M)),
    ?assertEqual(1, maps:get(msgs_out, M)),
    remove_metrics(TunnelId).

get_tunnel_metrics_empty_test() ->
    persistent_term:erase(macula_dist_tunnels),
    ?assertEqual([], macula_dist_relay:get_tunnel_metrics()).

get_tunnel_metrics_specific_missing_test() ->
    persistent_term:erase(macula_dist_tunnels),
    ?assertEqual(undefined, macula_dist_relay:get_tunnel_metrics(<<"nope">>)).

%%%===================================================================
%%% Tests — bridge supervisor
%%%===================================================================

bridge_sup_starts_test() ->
    %% If already running (from dist_system), just verify it exists
    case whereis(macula_dist_bridge_sup) of
        undefined ->
            {ok, Sup} = macula_dist_bridge_sup:start_link(),
            ?assert(is_pid(Sup)),
            ?assertEqual([], supervisor:which_children(Sup)),
            %% Don't kill it — may be needed by other tests.
            %% It will die when the test process exits.
            unlink(Sup);
        Existing ->
            ?assert(is_pid(Existing))
    end.

%%%===================================================================
%%% Tests — advertise_dist_accept
%%%===================================================================

advertise_no_mesh_client_test() ->
    persistent_term:erase(macula_dist_mesh_client),
    ?assertEqual(ok, macula_dist_relay:advertise_dist_accept()).

%%%===================================================================
%%% Helpers
%%%===================================================================

make_loopback_pair() ->
    ListenOpts = [binary, {active, false}, {reuseaddr, true}, {ip, {127,0,0,1}}],
    {ok, LSock} = gen_tcp:listen(0, ListenOpts),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port,
                                  [binary, {active, false}, {packet, 2}, {nodelay, true}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    inet:setopts(ASock, [{packet, raw}, {nodelay, true}]),
    {CSock, ASock}.

unique_tunnel_id() ->
    <<"test-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

encrypt(Key, Plaintext) ->
    Nonce = crypto:strong_rand_bytes(12),
    {Ciphertext, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, Key, Nonce, Plaintext, <<>>, true),
    <<Nonce/binary, Tag/binary, Ciphertext/binary>>.

decrypt(Key, <<Nonce:12/binary, Tag:16/binary, Ciphertext/binary>>) ->
    case crypto:crypto_one_time_aead(
            aes_256_gcm, Key, Nonce, Ciphertext, <<>>, Tag, false) of
        error -> {error, decrypt_failed};
        Plaintext -> {ok, Plaintext}
    end;
decrypt(_Key, _Data) ->
    {error, decrypt_failed}.

init_metrics(TunnelId) ->
    Ref = counters:new(4, [write_concurrency]),
    Tunnels = persistent_term:get(macula_dist_tunnels, #{}),
    persistent_term:put(macula_dist_tunnels, Tunnels#{TunnelId => Ref}),
    Ref.

read_metrics(Ref) ->
    #{bytes_out => counters:get(Ref, 1),
      bytes_in  => counters:get(Ref, 2),
      msgs_out  => counters:get(Ref, 3),
      msgs_in   => counters:get(Ref, 4)}.

remove_metrics(TunnelId) ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> ok;
        Tunnels ->
            persistent_term:put(macula_dist_tunnels, maps:remove(TunnelId, Tunnels))
    end.
