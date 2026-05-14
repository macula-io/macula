%%%-------------------------------------------------------------------
%%% @doc End-to-end V2 peering tests against a real Quinn QUIC pair.
%%%
%%% Drives an actual CONNECT/HELLO exchange between two
%%% `macula_peering_conn' workers (one client-role, one server-role)
%%% over a loopback Quinn listener. Pins the public contract:
%%% notification message shapes (`connected', `handshake_complete'
%%% 4-tuple, `disconnected'), peer_node_id propagation through
%%% `absorb_peer_info', state-machine progression, and graceful
%%% close behaviour.
%%%
%%% Why end-to-end and not state-machine-direct: the
%%% `awaiting_start' → `handshaking' transition calls
%%% `macula_quic:async_accept_stream/1' on the worker's `quic_conn',
%%% and the `handshaking' state reads bytes off a real QUIC stream.
%%% Faking those NIF boundaries adds more surface than running
%%% Quinn against itself on loopback.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_handshake_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Fixture
%%====================================================================

handshake_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"client + server reach connected; both fire connected notification",
            fun() -> client_and_server_both_connect(Ctx) end},
           {"accept_owner receives 4-tuple handshake_complete with peer_node_id",
            fun() -> accept_owner_receives_4_tuple(Ctx) end},
           {"controlling_pid's connected notification carries peer_node_id",
            fun() -> connected_notification_carries_peer_node_id(Ctx) end},
           {"absorb_peer_info populates realms + capabilities from frame",
            fun() -> absorb_peer_info_populates_fields(Ctx) end},
           {"close on a connected worker transitions through draining",
            %% DRAIN_TIMEOUT_MS is 5s; the disconnected fires shortly
            %% after that. Override the eunit default 5s per-test cap.
            {timeout, 15, fun() -> close_drains_then_disconnects(Ctx) end}},
           {"undefined accept_owner: handshake completes without notification",
            fun() -> handshake_complete_skipped_when_accept_owner_undefined(Ctx) end}]
      end}}.

%%====================================================================
%% Setup / cleanup
%%====================================================================

%% Setup builds the cert/key pair only. The QUIC listener itself is
%% created INSIDE each test body, because `nif_listen' captures the
%% calling pid as the listener owner — and that pid is where
%% `{quic, new_conn, ...}' is delivered. Setup runs in a different
%% process from the test bodies under eunit's `{setup, ...}` shape,
%% so a setup-owned listener can never deliver inbound conns to the
%% test that asserts on them.
setup() ->
    {ok, _} = application:ensure_all_started(macula),
    {Pub, Priv} = ephemeral_keypair(),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            Pub, Priv, [<<"localhost">>, <<"127.0.0.1">>]),
    Tmp  = lists:flatten(io_lib:format("/tmp/macula-peering-handshake-~p",
                                       [erlang:unique_integer([positive])])),
    Cert = Tmp ++ ".crt",
    Key  = Tmp ++ ".key",
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key,  KeyPem),
    #{cert => Cert, key => Key}.

cleanup(#{cert := Cert, key := Key}) ->
    file:delete(Cert),
    file:delete(Key),
    drain_quic_messages(),
    ok.

start_listener(#{cert := Cert, key := Key}) ->
    Port = pick_free_port(),
    {ok, Listener} = macula_quic:listen(
        <<"127.0.0.1">>, Port,
        [{cert, Cert}, {key, Key},
         {alpn, [<<"macula">>]},
         {idle_timeout_ms, 30000},
         {keep_alive_interval_ms, 5000}]),
    ok = macula_quic:async_accept(Listener),
    {Listener, Port}.

stop_listener(Listener) ->
    catch macula_quic:close_listener(Listener),
    ok.

%%====================================================================
%% Test bodies
%%====================================================================

client_and_server_both_connect(Ctx) ->
    {ClientPid, ServerPid, _, Listener} = handshake_pair(Ctx, []),
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    cleanup_pair(ClientPid, ServerPid, Listener).

accept_owner_receives_4_tuple(Ctx) ->
    %% accept_owner = self() (default for this helper). The server
    %% worker must send `{macula_peering, handshake_complete, ServerPid, PeerNodeId}'.
    {ClientPid, ServerPid, ClientId, Listener} = handshake_pair(Ctx, []),
    %% Drain `connected' notifications first so we deterministically
    %% match on the 4-tuple shape.
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    receive
        {macula_peering, handshake_complete, ServerPid, PeerNodeId} ->
            ?assertEqual(macula_identity:public(ClientId), PeerNodeId)
    after 5_000 ->
        ?assert(false)
    end,
    cleanup_pair(ClientPid, ServerPid, Listener).

connected_notification_carries_peer_node_id(Ctx) ->
    {ClientPid, ServerPid, ClientId, Listener} = handshake_pair(Ctx, []),
    ClientPub = macula_identity:public(ClientId),
    %% Server-side connected: peer_node_id should be the CLIENT's pubkey.
    {macula_peering, connected, ServerPid, ServerObservedPeerId} =
        recv_event(connected, ServerPid, 5_000),
    ?assertEqual(ClientPub, ServerObservedPeerId),
    %% Client-side connected: peer_node_id should be the SERVER's pubkey
    %% (whatever identity the server worker advertises).
    {macula_peering, connected, ClientPid, ClientObservedPeerId} =
        recv_event(connected, ClientPid, 5_000),
    ?assert(is_binary(ClientObservedPeerId)),
    ?assertEqual(32, byte_size(ClientObservedPeerId)),
    %% Symmetric: the two pubkeys are the two distinct identities used.
    ?assertNotEqual(ServerObservedPeerId, ClientObservedPeerId),
    cleanup_pair(ClientPid, ServerPid, Listener).

absorb_peer_info_populates_fields(Ctx) ->
    %% Spawn a client that advertises a specific realms list. The
    %% server-side worker's data record (after handshake) must hold
    %% that realms list in `peer_realms'.
    Realm = crypto:strong_rand_bytes(32),
    {ClientPid, ServerPid, _, Listener} = handshake_pair(Ctx, [{client_realms, [Realm]}]),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    %% Inspect server-side data via sys:get_state. Search the tuple
    %% for the peer_realms slot by shape rather than a hard-coded
    %% offset — the #data record has grown (dht_recipient,
    %% pubsub_recipient, timing_enabled, peer_capabilities ...) so a
    %% fixed index ages badly. peer_realms is a *list* (and the only
    %% list-of-binaries slot in the record), so structural match is
    %% safe.
    {connected, Data} = sys:get_state(ServerPid, 1_000),
    ?assert(tuple_has_list(Data, [Realm])),
    cleanup_pair(ClientPid, ServerPid, Listener).

%% True iff any element of `Tuple' equals `Expected'.
tuple_has_list(Tuple, Expected) when is_tuple(Tuple) ->
    lists:any(
      fun(I) -> element(I, Tuple) =:= Expected end,
      lists:seq(1, tuple_size(Tuple))).

close_drains_then_disconnects(Ctx) ->
    {ClientPid, ServerPid, _, Listener} = handshake_pair(Ctx, []),
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    Mon = erlang:monitor(process, ClientPid),
    macula_peering:close(ClientPid, operator_stop),
    %% Drain phase fires `{disconnected, drained}' after the
    %% `?DRAIN_TIMEOUT_MS' (5s) drain window. The user-supplied
    %% close reason (`operator_stop') travels in the GOODBYE frame
    %% to the peer, not in this local notification — that's a
    %% deliberate split: peer-visible vs locally-visible reasons.
    receive
        {macula_peering, disconnected, ClientPid, drained} -> ok
    after 8_000 ->
        ?assert(false)
    end,
    %% Worker exits :normal once draining completes.
    receive
        {'DOWN', Mon, process, ClientPid, normal} -> ok
    after 2_000 ->
        ?assert(false)
    end,
    cleanup_pair(undefined, ServerPid, Listener).

handshake_complete_skipped_when_accept_owner_undefined(Ctx) ->
    %% Server worker spawned WITHOUT accept_owner. `notify_handshake_complete'
    %% should be a no-op — `connected' still fires to controlling_pid, but
    %% no `handshake_complete' message is sent anywhere.
    {ClientPid, ServerPid, _, Listener} = handshake_pair(Ctx, [no_accept_owner]),
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    %% Verify no handshake_complete arrives — give it a beat to be sure.
    receive
        {macula_peering, handshake_complete, _, _} ->
            ?assert(false);
        {macula_peering, handshake_complete, _} ->
            ?assert(false)
    after 200 ->
        ok
    end,
    cleanup_pair(ClientPid, ServerPid, Listener).

%%====================================================================
%% Helpers
%%====================================================================

%% Drive a full client+server handshake on the loopback listener.
%% Returns `{ClientPid, ServerPid, ClientIdentity}' once both workers
%% have been spawned and ownership transferred. Caller drains the
%% notifications for whatever it's asserting on.
%% Returns `{ClientPid, ServerPid, ClientIdentity, Listener}'. The
%% listener stays alive for the duration of the test — closing it
%% cascades to every live connection (Quinn `Endpoint::close'
%% doesn't spare in-flight peers), which would race against the
%% `connected'/`handshake_complete' notifications the test asserts on.
%% `cleanup_pair/3' drops it once the assertions are done.
handshake_pair(Ctx, Opts) ->
    {Listener, Port} = start_listener(Ctx),
    {ClientPid, ServerPid, ClientId} = do_handshake_pair(Port, Opts),
    {ClientPid, ServerPid, ClientId, Listener}.

do_handshake_pair(Port, Opts) ->
    Self = self(),
    ClientId  = macula_identity:generate(),
    ServerId  = macula_identity:generate(),
    ClientRealms = proplists:get_value(client_realms, Opts, []),
    ServerRealms = proplists:get_value(server_realms, Opts, []),
    AcceptOwner = case lists:member(no_accept_owner, Opts) of
                       true  -> undefined;
                       false -> Self
                   end,

    %% Client side: spawn a peering worker that will dial loopback.
    {ok, ClientPid} = macula_peering:connect(#{
        identity        => ClientId,
        realms          => ClientRealms,
        capabilities    => 0,
        controlling_pid => Self,
        target          => #{host => "127.0.0.1", port => Port,
                             timeout_ms => 5_000}
    }),

    %% Server side: wait for the listener to deliver the new conn,
    %% then hand off to a server-role peering worker.
    ServerConn = receive
        {quic, new_conn, C, _Info} -> C
    after 5_000 ->
        erlang:error(no_inbound_conn)
    end,
    ServerOpts0 = #{
        identity        => ServerId,
        realms          => ServerRealms,
        capabilities    => 0,
        controlling_pid => Self
    },
    ServerOpts = case AcceptOwner of
                     undefined -> ServerOpts0;
                     Pid       -> ServerOpts0#{accept_owner => Pid}
                 end,
    {ok, ServerPid} = macula_peering:accept(ServerConn, ServerOpts),
    {ClientPid, ServerPid, ClientId}.

cleanup_pair(ClientPid, ServerPid, Listener) ->
    [catch macula_peering:close(P, test_cleanup)
     || P <- [ClientPid, ServerPid], is_pid(P)],
    stop_listener(Listener),
    drain_peering_messages().

%% Selectively receive `{macula_peering, connected, Pid, _}' for a
%% specific worker pid; useful when both client and server fire
%% `connected' notifications and the test cares about ordering.
recv_event(EventTag, Pid, Timeout) ->
    receive
        {macula_peering, EventTag, Pid, _} = Msg -> Msg
    after Timeout ->
        erlang:error({event_not_received, EventTag, Pid})
    end.

%% Match a notification with a wildcard final element; '_' in the
%% pattern stands in for "don't care".
expect_message({Tag, Event, Pid, '_'}, Timeout) ->
    receive
        {Tag, Event, Pid, _} -> ok
    after Timeout ->
        erlang:error({timeout_waiting_for, Tag, Event, Pid})
    end.

drain_peering_messages() ->
    receive
        {macula_peering, _, _, _} -> drain_peering_messages();
        {macula_peering, _, _}    -> drain_peering_messages()
    after 0 ->
        ok
    end.

drain_quic_messages() ->
    receive
        {quic, _, _, _} -> drain_quic_messages()
    after 0 -> ok
    end.

ephemeral_keypair() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {iolist_to_binary(Pub), iolist_to_binary(Priv)}.

pick_free_port() ->
    {ok, S} = gen_udp:open(0, [binary, {ip, {127,0,0,1}}]),
    {ok, P} = inet:port(S),
    gen_udp:close(S),
    P.
