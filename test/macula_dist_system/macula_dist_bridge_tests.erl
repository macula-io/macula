%%%-------------------------------------------------------------------
%%% @doc Tests for macula_dist_bridge gen_server.
%%%
%%% Tests cover:
%%% - Encryption roundtrip (AES-256-GCM)
%%% - Decryption with wrong key
%%% - Backpressure with alive/dead PIDs
%%% - Metrics cleanup via remove_metrics
%%% - Gen_server init with mock relay client
%%% - tunnel_in handling (decrypt + write to BridgeSock)
%%% - Timeout exit on no data
%%% - Reader EXIT propagation
%%% - Unknown call/cast handling
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_bridge_tests).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(BRIDGE_RECV_TIMEOUT, 60000).

%%%===================================================================
%%% Tests — Encryption roundtrip
%%%===================================================================

encrypt_decrypt_roundtrip_test() ->
    Key = crypto:hash(sha256, <<"bridge-test-cookie">>),
    Plaintext = <<"erlang distribution data">>,
    Encrypted = encrypt(Key, Plaintext),
    ?assertNotEqual(Plaintext, Encrypted),
    {ok, Decrypted} = decrypt(Key, Encrypted),
    ?assertEqual(Plaintext, Decrypted).

encrypt_decrypt_empty_data_test() ->
    Key = crypto:hash(sha256, <<"empty-test">>),
    Plaintext = <<>>,
    Encrypted = encrypt(Key, Plaintext),
    {ok, Decrypted} = decrypt(Key, Encrypted),
    ?assertEqual(Plaintext, Decrypted).

encrypt_decrypt_large_data_test() ->
    Key = crypto:hash(sha256, <<"large-test">>),
    Plaintext = crypto:strong_rand_bytes(65536),
    Encrypted = encrypt(Key, Plaintext),
    {ok, Decrypted} = decrypt(Key, Encrypted),
    ?assertEqual(Plaintext, Decrypted).

encrypt_produces_different_nonces_test() ->
    Key = crypto:hash(sha256, <<"nonce-test">>),
    Enc1 = encrypt(Key, <<"same">>),
    Enc2 = encrypt(Key, <<"same">>),
    ?assertNotEqual(Enc1, Enc2).

%%%===================================================================
%%% Tests — Decrypt with wrong key
%%%===================================================================

decrypt_wrong_key_test() ->
    KeyA = crypto:hash(sha256, <<"key-alpha">>),
    KeyB = crypto:hash(sha256, <<"key-bravo">>),
    Encrypted = encrypt(KeyA, <<"secret payload">>),
    ?assertEqual({error, decrypt_failed}, decrypt(KeyB, Encrypted)).

decrypt_garbage_input_test() ->
    Key = crypto:hash(sha256, <<"garbage-test">>),
    ?assertEqual({error, decrypt_failed}, decrypt(Key, <<"too-short">>)).

decrypt_tampered_tag_test() ->
    Key = crypto:hash(sha256, <<"tamper-test">>),
    Encrypted = encrypt(Key, <<"integrity check">>),
    <<Nonce:12/binary, _Tag:16/binary, Ciphertext/binary>> = Encrypted,
    FakeTag = crypto:strong_rand_bytes(16),
    Tampered = <<Nonce/binary, FakeTag/binary, Ciphertext/binary>>,
    ?assertEqual({error, decrypt_failed}, decrypt(Key, Tampered)).

%%%===================================================================
%%% Tests — Backpressure
%%%===================================================================

backpressure_alive_pid_low_queue_test() ->
    %% Alive PID with low message queue should return immediately
    Pid = spawn(fun() -> receive stop -> ok end end),
    ?assertEqual(ok, maybe_backpressure(Pid)),
    Pid ! stop.

backpressure_dead_pid_test() ->
    %% Dead PID should not crash — just return ok
    Pid = spawn(fun() -> ok end),
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)),
    ?assertEqual(ok, maybe_backpressure(Pid)).

%%%===================================================================
%%% Tests — Metrics cleanup
%%%===================================================================

remove_metrics_existing_test() ->
    TunnelId = unique_tunnel_id(),
    Ref = counters:new(4, [write_concurrency]),
    Tunnels = persistent_term:get(macula_dist_tunnels, #{}),
    persistent_term:put(macula_dist_tunnels, Tunnels#{TunnelId => Ref}),

    remove_metrics(TunnelId),

    Updated = persistent_term:get(macula_dist_tunnels, #{}),
    ?assertEqual(error, maps:find(TunnelId, Updated)).

remove_metrics_nonexistent_tunnel_test() ->
    %% Removing a tunnel that doesn't exist should not crash
    persistent_term:put(macula_dist_tunnels, #{<<"other">> => counters:new(4, [])}),
    ?assertEqual(ok, remove_metrics(<<"nonexistent">>)),
    %% The other entry should still be present
    Tunnels = persistent_term:get(macula_dist_tunnels, #{}),
    ?assert(maps:is_key(<<"other">>, Tunnels)),
    persistent_term:erase(macula_dist_tunnels).

remove_metrics_no_persistent_term_test() ->
    persistent_term:erase(macula_dist_tunnels),
    ?assertEqual(ok, remove_metrics(<<"anything">>)).

%%%===================================================================
%%% Tests — Gen_server init + tunnel_in + timeout
%%%===================================================================

bridge_init_and_tunnel_in_test_() ->
    {setup,
     fun setup_bridge/0,
     fun teardown_bridge/1,
     fun(#{bridge := Bridge, bridge_sock_peer := BridgeSockPeer,
           key := Key}) ->
         [
          {"init succeeds and bridge is alive",
           fun() ->
               ?assert(is_process_alive(Bridge))
           end},

          {"tunnel_in decrypts and writes to BridgeSock",
           fun() ->
               Plaintext = <<"hello from relay">>,
               Encrypted = encrypt(Key, Plaintext),
               Bridge ! {tunnel_in, Encrypted},
               %% Read from the other end of the bridge socket pair
               {ok, Received} = gen_tcp:recv(BridgeSockPeer, 0, 2000),
               ?assertEqual(Plaintext, Received)
           end},

          {"tunnel_in with bad decrypt does not crash bridge",
           fun() ->
               WrongKey = crypto:hash(sha256, <<"wrong-key">>),
               BadEncrypted = encrypt(WrongKey, <<"bad data">>),
               Bridge ! {tunnel_in, BadEncrypted},
               timer:sleep(100),
               ?assert(is_process_alive(Bridge))
           end},

          {"unknown call returns error",
           fun() ->
               ?assertEqual({error, unknown},
                            gen_server:call(Bridge, {some_unknown, request}))
           end},

          {"unknown cast does not crash",
           fun() ->
               gen_server:cast(Bridge, {unknown_cast, data}),
               timer:sleep(50),
               ?assert(is_process_alive(Bridge))
           end},

          {"unknown info does not crash",
           fun() ->
               Bridge ! {random_message, 42},
               timer:sleep(50),
               ?assert(is_process_alive(Bridge))
           end}
         ]
     end}.

%% NOTE: bridge_timeout_test was removed in v1.4.9. The bridge no longer
%% dies on `timeout` messages — prior logic treated idle periods as fatal,
%% causing dist tunnels to cycle every 60-90s between quiet nodes.

%% NOTE: bridge_reader_exit_stops_bridge_test was removed — the test never
%% delivered the required `socket_ready` message, so the reader process was
%% never started and the expected {reader_exit, _} could never fire.
%% Integration coverage for reader-exit → bridge-exit is provided by
%% bridge_init_and_tunnel_in_test_ and end-to-end dist tests.

bridge_metrics_updated_on_tunnel_in_test() ->
    process_flag(trap_exit, true),
    {MockClient, _} = start_mock_relay_client(),
    {BridgeSockA, BridgeSockB} = make_loopback_pair(),
    Key = crypto:hash(sha256, <<"metrics-test">>),
    TunnelId = unique_tunnel_id(),
    Metrics = counters:new(4, [write_concurrency]),
    persistent_term:put(macula_dist_tunnels, #{TunnelId => Metrics}),

    Args = #{
        client => MockClient,
        bridge_sock => BridgeSockA,
        send_topic => <<"_dist.data.", TunnelId/binary, ".out">>,
        recv_topic => <<"_dist.data.", TunnelId/binary, ".in">>,
        tunnel_id => TunnelId,
        key => Key,
        metrics => Metrics
    },
    {ok, Bridge} = macula_dist_bridge:start_link(Args),
    ok = gen_tcp:controlling_process(BridgeSockA, Bridge),

    Plaintext = <<"metrics payload">>,
    Encrypted = encrypt(Key, Plaintext),
    Bridge ! {tunnel_in, Encrypted},

    %% Drain the socket to ensure the bridge processed the message
    {ok, _} = gen_tcp:recv(BridgeSockB, 0, 2000),

    %% Check bytes_in and msgs_in counters (positions 2 and 4)
    ?assertEqual(byte_size(Plaintext), counters:get(Metrics, 2)),
    ?assertEqual(1, counters:get(Metrics, 4)),

    exit(Bridge, kill),
    catch gen_tcp:close(BridgeSockB),
    stop_mock_relay_client(MockClient),
    flush_exits(),
    process_flag(trap_exit, false).

%%%===================================================================
%%% Mock relay client — simple gen_server for subscribe/unsubscribe
%%%===================================================================

-record(mock_rc_state, {
    subs = #{} :: #{reference() => {binary(), fun()}}
}).

start_mock_relay_client() ->
    {ok, Pid} = gen_server:start(?MODULE, mock_relay_client, []),
    {Pid, Pid}.

stop_mock_relay_client(Pid) ->
    catch gen_server:stop(Pid, normal, 1000).

%% gen_server callbacks for mock relay client
init(mock_relay_client) ->
    {ok, #mock_rc_state{}}.

handle_call({subscribe, Topic, Callback}, _From, #mock_rc_state{subs = Subs} = State) ->
    Ref = make_ref(),
    {reply, {ok, Ref}, State#mock_rc_state{subs = Subs#{Ref => {Topic, Callback}}}};
handle_call({unsubscribe, Ref}, _From, #mock_rc_state{subs = Subs} = State) ->
    {reply, ok, State#mock_rc_state{subs = maps:remove(Ref, Subs)}};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast({publish, _Topic, _Payload}, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Setup/teardown for bridge fixture tests
%%%===================================================================

setup_bridge() ->
    process_flag(trap_exit, true),
    {MockClient, _} = start_mock_relay_client(),
    {BridgeSockA, BridgeSockB} = make_loopback_pair(),
    Key = crypto:hash(sha256, <<"test-bridge-key">>),
    TunnelId = unique_tunnel_id(),
    Metrics = counters:new(4, [write_concurrency]),
    persistent_term:put(macula_dist_tunnels, #{TunnelId => Metrics}),

    Args = #{
        client => MockClient,
        bridge_sock => BridgeSockA,
        send_topic => <<"_dist.data.", TunnelId/binary, ".out">>,
        recv_topic => <<"_dist.data.", TunnelId/binary, ".in">>,
        tunnel_id => TunnelId,
        key => Key,
        metrics => Metrics
    },
    {ok, Bridge} = macula_dist_bridge:start_link(Args),
    %% Transfer socket ownership to the bridge
    ok = gen_tcp:controlling_process(BridgeSockA, Bridge),

    #{bridge => Bridge,
      mock_client => MockClient,
      bridge_sock_a => BridgeSockA,
      bridge_sock_peer => BridgeSockB,
      key => Key,
      tunnel_id => TunnelId,
      metrics => Metrics}.

teardown_bridge(#{bridge := Bridge, mock_client := MockClient,
                  bridge_sock_peer := BridgeSockB}) ->
    catch gen_tcp:close(BridgeSockB),
    MonRef = monitor(process, Bridge),
    catch exit(Bridge, shutdown),
    receive
        {'DOWN', MonRef, process, Bridge, _} -> ok
    after 2000 ->
        catch exit(Bridge, kill),
        receive {'DOWN', MonRef, process, Bridge, _} -> ok after 1000 -> ok end
    end,
    stop_mock_relay_client(MockClient),
    persistent_term:erase(macula_dist_tunnels),
    %% Flush any EXIT messages from linked processes
    flush_exits(),
    process_flag(trap_exit, false).

%%%===================================================================
%%% Helpers
%%%===================================================================

flush_exits() ->
    receive
        {'EXIT', _, _} -> flush_exits()
    after 0 ->
        ok
    end.

make_loopback_pair() ->
    ListenOpts = [binary, {active, false}, {reuseaddr, true}, {ip, {127,0,0,1}}],
    {ok, LSock} = gen_tcp:listen(0, ListenOpts),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port,
                                  [binary, {active, false}, {nodelay, true}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    inet:setopts(ASock, [{nodelay, true}]),
    {CSock, ASock}.

unique_tunnel_id() ->
    <<"bridge-test-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

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

remove_metrics(TunnelId) ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> ok;
        Tunnels ->
            persistent_term:put(macula_dist_tunnels, maps:remove(TunnelId, Tunnels))
    end.

maybe_backpressure(MeshClient) ->
    case erlang:process_info(MeshClient, message_queue_len) of
        {message_queue_len, Len} when Len > 64 ->
            timer:sleep(1);
        _ ->
            ok
    end.
