%%%-------------------------------------------------------------------
%%% @doc Tests for macula_dist_pool + macula_dist_bridge.
%%%
%%% Tests cover:
%%% - Public API: is_relay_mode, register/get mesh pool
%%% - Loopback pair creation and packet framing
%%% - Bridge I/O: reader/writer loops via loopback
%%% - Tunnel encryption (AES-256-GCM from cookie)
%%% - Metrics counters
%%% - Bridge supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_pool_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests — is_relay_mode
%%%===================================================================

is_relay_mode_off_test() ->
    os:unsetenv("MACULA_DIST_MODE"),
    ?assertNot(macula_dist_pool:is_relay_mode()).

is_relay_mode_on_test() ->
    os:putenv("MACULA_DIST_MODE", "relay"),
    ?assert(macula_dist_pool:is_relay_mode()),
    os:unsetenv("MACULA_DIST_MODE").

is_relay_mode_other_test() ->
    os:putenv("MACULA_DIST_MODE", "direct"),
    ?assertNot(macula_dist_pool:is_relay_mode()),
    os:unsetenv("MACULA_DIST_MODE").

%%%===================================================================
%%% Tests — register/get mesh pool
%%%===================================================================

register_mesh_pool_test() ->
    Self = self(),
    ok = macula_dist_pool:register_mesh_pool(Self),
    ?assertEqual(Self, macula_dist_pool:get_mesh_pool()),
    persistent_term:erase(macula_dist_mesh_pool).

get_mesh_pool_undefined_test() ->
    persistent_term:erase(macula_dist_mesh_pool),
    ?assertEqual(undefined, macula_dist_pool:get_mesh_pool()).

get_mesh_pool_dead_pid_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(50),
    persistent_term:put(macula_dist_mesh_pool, Pid),
    ?assertEqual(undefined, macula_dist_pool:get_mesh_pool()),
    persistent_term:erase(macula_dist_mesh_pool).

%%%===================================================================
%%% Tests — connect error paths
%%%===================================================================

connect_no_mesh_pool_test() ->
    persistent_term:erase(macula_dist_mesh_pool),
    os:putenv("MACULA_DIST_MODE", "relay"),
    ?assertEqual({error, no_mesh_connection},
                 macula_dist_pool:connect("test@host", "host", 4433)),
    os:unsetenv("MACULA_DIST_MODE").

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
    ?assertEqual([], macula_dist_pool:get_tunnel_metrics()).

get_tunnel_metrics_specific_missing_test() ->
    persistent_term:erase(macula_dist_tunnels),
    ?assertEqual(undefined, macula_dist_pool:get_tunnel_metrics(<<"nope">>)).

%%%===================================================================
%%% Tests — bridge supervisor
%%%===================================================================

bridge_sup_starts_test() ->
    bridge_sup_runs(whereis(macula_dist_bridge_sup)).

%% A running macula application already owns the supervisor. Otherwise
%% start one, check it, and stop it again: a registered supervisor left
%% behind stops a later test from starting the macula application.
bridge_sup_runs(undefined) ->
    {ok, Sup} = macula_dist_bridge_sup:start_link(),
    ?assertEqual([], supervisor:which_children(Sup)),
    ok = gen_server:stop(Sup),
    ?assertEqual(undefined, whereis(macula_dist_bridge_sup));
bridge_sup_runs(Existing) ->
    ?assert(is_pid(Existing)).

%%%===================================================================
%%% Tests — advertise_dist_accept
%%%===================================================================

advertise_no_mesh_pool_test() ->
    persistent_term:erase(macula_dist_mesh_pool),
    ?assertEqual(ok, macula_dist_pool:advertise_dist_accept()).

%%%===================================================================
%%% Tests — tunnel RPC payloads as the frame decoder delivers them
%%%===================================================================

%% The accepting side's handler builds its reply with binary keys. After
%% the RESULT frame is encoded and decoded, the connecting side must still
%% find the tunnel id, in whatever key form the decode produced.
tunnel_reply_reads_the_tunnel_id_of_a_decoded_result_test() ->
    Reply = decoded_payload(#{<<"tunnel_id">> => <<"t-1">>,
                              <<"send_topic">> => <<"_dist.data.t-1.in">>,
                              <<"recv_topic">> => <<"_dist.data.t-1.out">>}),
    ?assertEqual({tunnel, <<"t-1">>}, macula_dist_pool:tunnel_reply(Reply)).

tunnel_reply_reads_the_error_of_a_decoded_result_test() ->
    Reply = decoded_payload(#{<<"error">> => <<"no_mesh_pool">>}),
    ?assertEqual({tunnel_error, <<"no_mesh_pool">>},
                 macula_dist_pool:tunnel_reply(Reply)).

tunnel_reply_without_tunnel_id_or_error_is_unexpected_test() ->
    Reply = decoded_payload(#{<<"other">> => 1}),
    ?assertEqual(unexpected, macula_dist_pool:tunnel_reply(Reply)).

tunnel_request_reads_from_node_of_decoded_arguments_test() ->
    Args = decoded_payload(#{<<"from_node">> => <<"a@host">>,
                             <<"target_node">> => <<"b@host">>}),
    ?assertEqual(<<"a@host">>, macula_dist_pool:tunnel_request_from_node(Args)).

tunnel_request_without_from_node_reads_empty_test() ->
    ?assertEqual(<<>>, macula_dist_pool:tunnel_request_from_node(decoded_payload(#{}))).

%% In a node that has not created the field atoms, the same payloads
%% decode with {text, Name} keys. The node is checked first, so the test
%% cannot pass on a node that already has them.
tunnel_payloads_decoded_in_a_fresh_node_test_() ->
    {timeout, 60, fun fresh_node_reads_tunnel_payloads/0}.

fresh_node_reads_tunnel_payloads() ->
    Bin = result_frame(#{<<"tunnel_id">> => <<"t-1">>,
                         <<"from_node">> => <<"a@host">>}),
    Paths = lists:append([["-pa", P] || P <- code:get_path()]),
    {ok, Peer, _Node} = peer:start_link(#{connection => standard_io, args => Paths}),
    try
        ?assertMatch({'EXIT', _}, catch peer:call(Peer, erlang, binary_to_existing_atom,
                                                  [<<"from_node">>, utf8])),
        {ok, Decoded, <<>>} = peer:call(Peer, macula_frame, decode, [Bin]),
        Wire = maps:get(payload, Decoded),
        ?assert(maps:is_key({text, <<"from_node">>}, Wire)),
        ?assert(maps:is_key({text, <<"tunnel_id">>}, Wire)),
        ?assertEqual({tunnel, <<"t-1">>},
                     peer:call(Peer, macula_dist_pool, tunnel_reply, [Wire])),
        ?assertEqual(<<"a@host">>,
                     peer:call(Peer, macula_dist_pool, tunnel_request_from_node, [Wire]))
    after
        peer:stop(Peer)
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

result_frame(Payload) ->
    iolist_to_binary(macula_frame:encode(macula_frame:result(#{call_id => <<0:128>>,
                                                               payload => Payload,
                                                               responded_by => <<0:256>>}))).

decoded_payload(Payload) ->
    {ok, Decoded, <<>>} = macula_frame:decode(result_frame(Payload)),
    maps:get(payload, Decoded).

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
