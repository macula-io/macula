%%%-------------------------------------------------------------------
%%% @doc End-to-end peering tests against a real Quinn QUIC pair.
%%%
%%% Drives the post-quantum handshake of
%%% plans/DESIGN_PQ_HANDSHAKE_FRAMES.md (opener, challenge, CONNECT,
%%% HELLO) between two `macula_peering_conn' workers (one client-role,
%%% one server-role) over a loopback Quinn listener, whose self-signed
%%% leaf the station's statement issuer binds. Pins the public contract:
%%% notification message shapes (`connected', `handshake_complete'
%%% 4-tuple, `disconnected'), the peer's node_id and `peer_identity/1',
%%% the station's puzzle modes, status frames at every reissue and the
%%% peer's statement timer, binding expiry, neighbour signatures in
%%% pq_hybrid, local close reasons,
%%% state-machine progression, and graceful close behaviour.
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

-export([log/2]).

%% The test clock: issuers start at T0, and connections stand a minute
%% after it unless a test says otherwise.
-define(T0, 1789000000000).
-define(MINUTE, 60_000).
-define(HOUR, 3_600_000).
-define(DAY, 86_400_000).
-define(STATION_CAPABILITIES, 5).
-define(CLIENT_CAPABILITIES, 3).
-define(UNSOLVED, <<"_macula.peering.puzzle_unsolved">>).

%%====================================================================
%% Fixture
%%====================================================================

handshake_test_() ->
    {timeout, 600,
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
           {"peer_identity reports what the handshake verified of each side",
            fun() -> peer_identity_reports_what_the_handshake_verified(Ctx) end},
           {"close on a connected worker transitions through draining",
            %% DRAIN_TIMEOUT_MS is 5s; the disconnected fires shortly
            %% after that. Override the eunit default 5s per-test cap.
            {timeout, 15, fun() -> close_drains_then_disconnects(Ctx) end}},
           {"dedicated_streams_idle ends the drain early instead of waiting the full timeout",
            fun() -> dedicated_streams_idle_ends_drain_early(Ctx) end},
           {"reject on a connected worker terminates immediately, no draining",
            fun() -> reject_terminates_immediately(Ctx) end},
           {"peer's graceful control-stream half-close drains, doesn't stop immediately",
            %% Same DRAIN_TIMEOUT_MS budget as close_drains_then_disconnects.
            {timeout, 15, fun() -> peer_send_shutdown_drains_not_stops(Ctx) end}},
           {"peer closing (reject) notifies and terminates the surviving side",
            fun() -> peer_closing_notifies_the_surviving_side(Ctx) end},
           {"undefined accept_owner: handshake completes without notification",
            fun() -> handshake_complete_skipped_when_accept_owner_undefined(Ctx) end},
           {"close sends a GOODBYE the peer reads",
            fun() -> close_sends_a_goodbye_the_peer_reads(Ctx) end},
           {"a dial to another node_id closes with peer_identity_mismatch",
            {timeout, 30, fun() -> a_dial_to_another_node_id_closes_with_peer_identity_mismatch(Ctx) end}},
           {"a connection without what its role needs does not start",
            {timeout, 30, fun() -> a_connection_without_what_its_role_needs_does_not_start(Ctx) end}},
           {"a first frame other than an opener closes with unexpected_frame",
            {timeout, 30, fun() -> a_first_frame_other_than_an_opener_closes_with_unexpected_frame(Ctx) end}},
           {"under enforce a solved puzzle connects and an unsolved one is refused",
            {timeout, 30, fun() -> under_enforce_a_solved_puzzle_connects_and_an_unsolved_one_is_refused(Ctx) end}},
           {"under log_only an unsolved puzzle connects and is reported",
            {timeout, 30, fun() -> under_log_only_an_unsolved_puzzle_connects_and_is_reported(Ctx) end}},
           {"a statement that lapses closes the connection with status_expired",
            {timeout, 30, fun() -> a_statement_that_lapses_closes_the_connection_with_status_expired(Ctx) end}},
           {"status frames at each reissue keep the connection open",
            {timeout, 30, fun() -> status_frames_at_each_reissue_keep_the_connection_open(Ctx) end}},
           {"a status frame that fails its checks closes with that check's reason",
            {timeout, 30, fun() -> a_status_frame_that_fails_its_checks_closes_with_that_reason(Ctx) end}},
           {"a binding at its not_after closes the connection with binding_expired",
            {timeout, 30,
             fun() -> a_binding_at_its_not_after_closes_the_connection_with_binding_expired(Ctx) end}},
           {"control frames verify both ways around a status frame in pq_hybrid",
            {timeout, 120,
             fun() -> control_frames_verify_both_ways_around_a_status_frame_in_pq_hybrid(Ctx) end}},
           {"a control frame signed by another key closes the connection in pq_hybrid",
            {timeout, 120,
             fun() -> a_control_frame_signed_by_another_key_closes_the_connection_in_pq_hybrid(Ctx) end}},
           {"a control frame without a neighbour signature closes the connection in pq_hybrid",
            {timeout, 120,
             fun() ->
                 a_control_frame_without_a_neighbour_signature_closes_the_connection_in_pq_hybrid(Ctx)
             end}},
           {"an overlay relay goes out neighbour-signed and verifies in pq_hybrid",
            {timeout, 120,
             fun() -> an_overlay_relay_goes_out_neighbour_signed_and_verifies_in_pq_hybrid(Ctx) end}},
           {"a frame with a field its table does not name closes with malformed_frame",
            {timeout, 30, fun() -> a_frame_with_a_field_its_table_does_not_name_closes_with_malformed_frame(Ctx) end}},
           {"an application frame on the control stream is delivered",
            {timeout, 30, fun() -> an_application_frame_on_the_control_stream_is_delivered(Ctx) end}},
           {"a handshake frame after HELLO closes with malformed_frame",
            {timeout, 30, fun() -> a_handshake_frame_after_hello_closes_with_malformed_frame(Ctx) end}},
           {"a handshake frame that is not CBOR closes with malformed_frame",
            {timeout, 30, fun() -> a_handshake_frame_that_is_not_cbor_closes_with_malformed_frame(Ctx) end}},
           {"a handshake length header above the frame cap closes from the header",
            {timeout, 30, fun() -> a_handshake_length_header_above_the_frame_cap_closes_from_the_header(Ctx) end}},
           {"a handshake frame of exactly 64 KiB is read, and one byte more closes from the header",
            {timeout, 30, fun() -> a_handshake_frame_of_64_kib_is_read_and_one_byte_more_closes(Ctx) end}},
           {"bytes that are not CBOR on the control stream close with malformed_frame",
            {timeout, 30, fun() -> bytes_that_are_not_cbor_on_the_control_stream_close_with_malformed_frame(Ctx) end}},
           {"a control stream length header above the frame cap closes from the header",
            {timeout, 30, fun() -> a_control_stream_length_header_above_the_frame_cap_closes_from_the_header(Ctx) end}},
           {"a control stream frame over the element budget closes with malformed_frame",
            {timeout, 30, fun() -> a_control_stream_frame_over_the_element_budget_closes_with_malformed_frame(Ctx) end}},
           {"a CALL, a PUBLISH and a STREAM_OPEN on the control stream are delivered",
            {timeout, 30, fun() -> a_call_a_publish_and_a_stream_open_on_the_control_stream_are_delivered(Ctx) end}},
           {"a CALL without its request closes with malformed_frame",
            {timeout, 30, fun() -> a_call_without_its_request_closes_with_malformed_frame(Ctx) end}},
           {"a frame of a type this node does not know closes with malformed_frame",
            {timeout, 30, fun() -> a_frame_of_a_type_this_node_does_not_know_closes_with_malformed_frame(Ctx) end}},
           {"a STORE whose record bytes are not CBOR reaches its recipient and the connection serves on",
            {timeout, 30, fun() -> a_store_whose_record_is_not_cbor_reaches_its_recipient_and_the_connection_serves_on(Ctx) end}}]
      end}}.

%%====================================================================
%% Setup / cleanup
%%====================================================================

%% Setup builds the cert/key pair only, as files and DER. The QUIC listener itself is
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
    Dir  = macula_test_tmp:dir("macula-peering-handshake"),
    Cert = filename:join(Dir, "listener.crt"),
    Key  = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key,  KeyPem),
    [{'Certificate', Der, not_encrypted}] = public_key:pem_decode(CertPem),
    #{dir => Dir, cert => Cert, key => Key, der => Der}.

cleanup(#{dir := Dir}) ->
    ok = file:del_dir_r(Dir),
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
    try macula_quic:close_listener(Listener) catch _:_ -> ok end,
    ok.

%%====================================================================
%% Test bodies
%%====================================================================

client_and_server_both_connect(Ctx) ->
    {ClientPid, ServerPid, _, World} = handshake_pair(Ctx, []),
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    cleanup_pair(ClientPid, ServerPid, World).

accept_owner_receives_4_tuple(Ctx) ->
    %% accept_owner = self() (default for this helper). The server
    %% worker must send `{macula_peering, handshake_complete, ServerPid, PeerNodeId}'.
    {ClientPid, ServerPid, ClientId, World} = handshake_pair(Ctx, []),
    %% Drain `connected' notifications first so we deterministically
    %% match on the 4-tuple shape.
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    receive
        {macula_peering, handshake_complete, ServerPid, PeerNodeId} ->
            ?assertEqual(node_id(ClientId), PeerNodeId)
    after 5_000 ->
        ?assert(false)
    end,
    cleanup_pair(ClientPid, ServerPid, World).

connected_notification_carries_peer_node_id(Ctx) ->
    {ClientPid, ServerPid, ClientId, World} = handshake_pair(Ctx, []),
    ClientNodeId = node_id(ClientId),
    %% Server-side connected: peer_node_id should be the CLIENT's node_id.
    {macula_peering, connected, ServerPid, ServerObservedPeerId} =
        recv_event(connected, ServerPid, 5_000),
    ?assertEqual(ClientNodeId, ServerObservedPeerId),
    %% Client-side connected: peer_node_id should be the SERVER's node_id
    %% (whatever identity the server worker advertises).
    {macula_peering, connected, ClientPid, ClientObservedPeerId} =
        recv_event(connected, ClientPid, 5_000),
    ?assert(is_binary(ClientObservedPeerId)),
    ?assertEqual(32, byte_size(ClientObservedPeerId)),
    %% Symmetric: the two node_ids are the two distinct identities used.
    ?assertNotEqual(ServerObservedPeerId, ClientObservedPeerId),
    cleanup_pair(ClientPid, ServerPid, World).

close_drains_then_disconnects(Ctx) ->
    {ClientPid, ServerPid, _, World} = handshake_pair(Ctx, []),
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
    cleanup_pair(undefined, ServerPid, World).

%% macula-io/macula#9, part 2: `controlling_pid' (e.g. macula-station's
%% peer_observer, which tracks dedicated/bidi streams per connection)
%% can end the drain early by casting `dedicated_streams_idle' once it
%% knows none remain for this connection, instead of the caller always
%% waiting out the full `?DRAIN_TIMEOUT_MS' (5s, per
%% `close_drains_then_disconnects' above). Also pins the new
%% `{macula_peering, draining, Pid, Reason}' notification `close/2'
%% fires on entering draining -- the signal `controlling_pid' actually
%% needs to know it's worth watching this connection's own stream
%% count at all.
dedicated_streams_idle_ends_drain_early(Ctx) ->
    {ClientPid, ServerPid, _, World} = handshake_pair(Ctx, []),
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    Mon = erlang:monitor(process, ClientPid),
    macula_peering:close(ClientPid, operator_stop),
    receive
        {macula_peering, draining, ClientPid, operator_stop} -> ok
    after 1_000 ->
        erlang:error(no_draining_notification)
    end,
    gen_statem:cast(ClientPid, dedicated_streams_idle),
    %% Must conclude well within the 5s DRAIN_TIMEOUT_MS -- proves this
    %% actually ended the drain early rather than coincidentally timing
    %% out at the same moment.
    receive
        {macula_peering, disconnected, ClientPid, drained} -> ok
    after 1_000 ->
        erlang:error(drain_did_not_end_early)
    end,
    receive
        {'DOWN', Mon, process, ClientPid, normal} -> ok
    after 500 ->
        ?assert(false)
    end,
    cleanup_pair(undefined, ServerPid, World).

%% Real bug this pins (macula-io/macula#9, Fable's review, 2026-09-06):
%% a peer that gracefully half-closes JUST its control stream — real
%% SDKs do exactly this in their own graceful close (e.g. macula-go's
%% Session.Close: send GOODBYE, finish the control stream's send side,
%% THEN drain briefly before fully closing the whole connection) — used
%% to be treated by the surviving side as an immediate `{stop, normal,
%% Data}', asymmetric with this side's OWN `close/2' path (which
%% already drains, see `close_drains_then_disconnects' above). An
%% in-flight dedicated/bidi stream on the same connection would be
%% killed mid-write by that abrupt teardown.
%%
%% Closes the CLIENT's control stream directly via `macula_quic:
%% close_stream/1' (bypassing `macula_peering_conn''s own `close/2',
%% which closes the whole connection after draining, not just the
%% control stream) to reproduce exactly the narrow signal a real SDK's
%% graceful close sends first. The client gen_statem process itself is
%% left in an artificial half-manipulated state by this — the point is
%% the SERVER's reaction to what arrives on the wire, not the client.
peer_send_shutdown_drains_not_stops(Ctx) ->
    {ClientPid, ServerPid, _, World} = handshake_pair(Ctx, []),
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),

    {connected, ClientData} = sys:get_state(ClientPid, 1_000),
    close_control_stream(ClientData),

    Mon = erlang:monitor(process, ServerPid),
    %% Must NOT stop immediately -- the old behaviour was an abrupt
    %% {stop, normal, Data} right here.
    receive
        {'DOWN', Mon, process, ServerPid, _Reason} ->
            erlang:error(stopped_immediately_instead_of_draining)
    after 500 ->
        ok
    end,
    ?assertMatch({draining, _}, sys:get_state(ServerPid, 1_000)),

    %% Still finishes the drain and disconnects afterward, exactly
    %% like the initiator-side path.
    receive
        {macula_peering, disconnected, ServerPid, drained} -> ok
    after 8_000 ->
        ?assert(false)
    end,
    receive
        {'DOWN', Mon, process, ServerPid, normal} -> ok
    after 2_000 ->
        ?assert(false)
    end,
    cleanup_pair(ClientPid, undefined, World).

%% `#data' has two `reference()' slots (`quic_conn', `quic_stream') —
%% distinguishing them by declared type alone is ambiguous, so try
%% `close_stream/1' on every reference found and let the NIF's own
%% resource-type check reject the wrong one (a connection resource
%% doesn't decode as a stream resource; the mismatch is caught).
%% Structural, like `tuple_has_list/2' above, rather than a hard-coded
%% index that ages badly as the record grows.
close_control_stream(Data) ->
    Refs = [E || E <- tuple_to_list(Data), is_reference(E)],
    lists:foreach(fun(Ref) -> catch macula_quic:close_stream(Ref) end, Refs).

%% The whole point of `reject/2': unlike `close/2' above, both the
%% `disconnected' notification and worker exit must land almost
%% immediately, never anywhere near `?DRAIN_TIMEOUT_MS' (5s) — proving
%% there is no `draining' state in between. Also confirms the REAL
%% rejection reason (`puzzle_invalid') travels in the local
%% notification, unlike `close/2''s drain path where the local
%% notification reason is always the fixed atom `drained' regardless
%% of what the caller originally passed.
reject_terminates_immediately(Ctx) ->
    {ClientPid, ServerPid, _, World} = handshake_pair(Ctx, []),
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    Mon = erlang:monitor(process, ClientPid),
    macula_peering:reject(ClientPid, puzzle_invalid),
    receive
        {macula_peering, disconnected, ClientPid, puzzle_invalid} -> ok
    after 500 ->
        ?assert(false)
    end,
    receive
        {'DOWN', Mon, process, ClientPid, normal} -> ok
    after 500 ->
        ?assert(false)
    end,
    cleanup_pair(undefined, ServerPid, World).

%% Real bug this pins: when one side's connection dies, the events the
%% NIF actually delivers to the OTHER side are `{quic, stream_closed,
%% Stream, Detail}' / `{quic, peer_send_shutdown, Stream, Detail}' —
%% `{quic, closed, Conn, Detail}' is never sent by anything (verified
%% directly in `native/macula_quic/src/*.rs': the atom exists, nothing
%% calls `send_event' with it). Before this fix, `handshaking' and
%% `connected' had a clause ONLY for the atom that never arrives, so
%% the surviving side's worker fell through to `drop_unexpected',
%% logged "unexpected", and sat there indefinitely — never notifying
%% its own `controlling_pid', never terminating — a zombie connection
%% that looks alive from the BEAM side while its transport is
%% genuinely gone. Reproduced live against the real fleet while
%% verifying `reject/2' (10.9.0): the puzzle-invalid CLIENT correctly
%% saw the SDK's own logging fire `unexpected state=handshaking
%% event={quic, stream_closed, ...}' and simply never disconnected.
peer_closing_notifies_the_surviving_side(Ctx) ->
    {ClientPid, ServerPid, _, World} = handshake_pair(Ctx, []),
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    Mon = erlang:monitor(process, ServerPid),
    %% Reject the CLIENT — the SERVER did not initiate this and has no
    %% a priori reason to expect it, exactly like a real transport
    %% failure or a peer that vanished. `reject/2', not `close/2': an
    %% immediate `{stop, normal, Data}' with no GOODBYE frame is the
    %% scenario that actually exercises the missing-event-clause bug —
    %% `close/2''s GOODBYE is itself an application-level frame the
    %% peer would see as ordinary stream data, not a transport-level
    %% closure.
    macula_peering:reject(ClientPid, puzzle_invalid),
    receive
        {macula_peering, disconnected, ServerPid, _Reason} -> ok
    after 2_000 ->
        ?assert(false)
    end,
    receive
        {'DOWN', Mon, process, ServerPid, normal} -> ok
    after 500 ->
        ?assert(false)
    end,
    cleanup_pair(undefined, undefined, World).

handshake_complete_skipped_when_accept_owner_undefined(Ctx) ->
    %% Server worker spawned WITHOUT accept_owner. `notify_handshake_complete'
    %% should be a no-op — `connected' still fires to controlling_pid, but
    %% no `handshake_complete' message is sent anywhere.
    {ClientPid, ServerPid, _, World} = handshake_pair(Ctx, [no_accept_owner]),
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
    cleanup_pair(ClientPid, ServerPid, World).

%%====================================================================
%% Helpers
%%====================================================================

%% Drive a full client+server handshake on the loopback listener.
%% Returns `{ClientPid, ServerPid, ClientIdentity, World}' once both
%% workers have been spawned and ownership transferred: the client's
%% identity key, and the world of `world/2' (the listener, each side's
%% statement issuer and their clock). Caller drains the notifications
%% for whatever it's asserting on. The listener stays alive for the
%% duration of the test: closing it cascades to every live connection
%% (Quinn `Endpoint::close' doesn't spare in-flight peers), which would
%% race against the `connected'/`handshake_complete' notifications the
%% test asserts on. `cleanup_pair/3' drops the world once the
%% assertions are done.
handshake_pair(Ctx, Opts) ->
    #{client_key := ClientKey} = World = world(Ctx, #{}),
    {ClientPid, ServerPid} = connect(World, (accept_owner(Opts))#{mode => off}),
    {ClientPid, ServerPid, ClientKey, World}.

cleanup_pair(ClientPid, ServerPid, World) ->
    [try macula_peering:close(P, test_cleanup) catch _:_ -> ok end
     || P <- [ClientPid, ServerPid], is_pid(P)],
    forget_world(World),
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
        {macula_peering, _, _}    -> drain_peering_messages();
        {macula_tls_rotation_due, _Issuer} -> drain_peering_messages();
        {puzzle_unsolved, _NodeId} -> drain_peering_messages();
        {quic, _, _, _}           -> drain_peering_messages()
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

%%====================================================================
%% The post-quantum handshake: identities, puzzle, status and binding
%%====================================================================

%% What the handshake verified of each side, as `peer_identity/1'
%% reports it: the peer's node_id, its identity key as carried, the
%% profile and the capabilities its CONNECT or HELLO carried.
peer_identity_reports_what_the_handshake_verified(Ctx) ->
    #{station_key := StationKey, client_key := ClientKey} = World = world(Ctx, #{}),
    {Client, Station} = connect(World, #{mode => off}),
    StationId = node_id(StationKey),
    ClientId = node_id(ClientKey),
    ?assertEqual({StationId, ClientId}, {await(Client, connected), await(Station, connected)}),
    ?assertEqual({ok, #{node_id => StationId, identity_key => macula_node_keys:public_key(StationKey),
                        profile => pq_pure, capabilities => ?STATION_CAPABILITIES}},
                 macula_peering:peer_identity(Client)),
    ?assertEqual({ok, #{node_id => ClientId, identity_key => macula_node_keys:public_key(ClientKey),
                        profile => pq_pure, capabilities => ?CLIENT_CAPABILITIES}},
                 macula_peering:peer_identity(Station)),
    finish(World, [Client, Station]).

%% `close/2' sends GOODBYE on the control stream, and the peer reads it
%% as a frame of the open connection.
close_sends_a_goodbye_the_peer_reads(Ctx) ->
    {ClientPid, ServerPid, _, World} = handshake_pair(Ctx, [no_accept_owner]),
    expect_message({macula_peering, connected, ClientPid, '_'}, 5_000),
    expect_message({macula_peering, connected, ServerPid, '_'}, 5_000),
    macula_peering:close(ClientPid, operator_stop),
    receive
        {macula_peering, frame, ServerPid, Frame} -> ?assertMatch(#{frame_type := goodbye}, Frame)
    after 5_000 ->
        erlang:error(no_goodbye)
    end,
    cleanup_pair(ClientPid, ServerPid, World).

a_dial_to_another_node_id_closes_with_peer_identity_mismatch(Ctx) ->
    #{station_key := StationKey} = World = world(Ctx, #{}),
    Other = node_id(identity()),
    {Client, Station} = connect(World, #{mode => off, expected => Other}),
    ?assertEqual({peer_identity_mismatch, #{expected => Other, derived => node_id(StationKey)}}, ended(Client)),
    finish(World, [Client, Station]).

a_connection_without_what_its_role_needs_does_not_start(_Ctx) ->
    {ok, ConnectKey} = macula_node_keys:generate(connect, pq_pure),
    Target = #{host => <<"127.0.0.1">>, port => 1},
    Client = #{identity => identity(), issuer => self(), capabilities => 0, controlling_pid => self(),
               target => Target},
    Station = #{role => server, identity => identity(), issuer => self(), capabilities => 0,
                controlling_pid => self(), quic_conn => make_ref()},
    ?assertEqual({error, {target, expected_node_id_required}}, macula_peering:connect(Client)),
    ?assertEqual({error, {identity, not_an_identity_key}},
                 macula_peering:connect(Client#{identity := ConnectKey,
                                                target := Target#{expected_node_id => <<0:256>>}})),
    ?assertEqual({error, {puzzle, mode_required}}, macula_peering_conn_sup:start_conn(Station)).

a_first_frame_other_than_an_opener_closes_with_unexpected_frame(Ctx) ->
    #{port := Port} = World = world(Ctx, #{}),
    {ok, Raw} = macula_quic:connect(<<"127.0.0.1">>, Port, [{verify, none}, {alpn, [<<"macula">>]}], 5_000),
    Station = accept_one(station_opts(World, #{mode => off})),
    {ok, Stream} = macula_quic:open_stream(Raw),
    NotAnOpener = macula_handshake:status(#{tbs => <<"a tbs">>, signature => <<"a signature">>}),
    ok = macula_quic:send(Stream, macula_frame:encode_bytes(NotAnOpener)),
    ?assertEqual(unexpected_frame, ended(Station)),
    ok = macula_quic:close_connection(Raw),
    finish(World, [Station]).

under_enforce_a_solved_puzzle_connects_and_an_unsolved_one_is_refused(Ctx) ->
    Solved = solved_identity(),
    SolvedWorld = world(Ctx, #{client_key => Solved}),
    {Client1, Station1} = connect(SolvedWorld, #{mode => enforce}),
    ?assertEqual(node_id(Solved), await(Station1, connected)),
    finish(SolvedWorld, [Client1, Station1]),
    UnsolvedWorld = world(Ctx, #{client_key => unsolved_identity()}),
    {Client2, Station2} = connect(UnsolvedWorld, #{mode => enforce}),
    ClientEnded = ended(Client2),
    StationEnded = ended(Station2),
    ?assertEqual({{refused, puzzle_invalid}, puzzle_invalid}, {ClientEnded, StationEnded}),
    finish(UnsolvedWorld, [Client2, Station2]).

under_log_only_an_unsolved_puzzle_connects_and_is_reported(Ctx) ->
    Unsolved = unsolved_identity(),
    World = world(Ctx, #{client_key => Unsolved}),
    Handler = capture_diagnostics(),
    {Client, Station} = connect(World, #{mode => log_only}),
    Connected = await(Station, connected),
    Reported = reported_unsolved(),
    ok = logger:remove_handler(Handler),
    ?assertEqual(node_id(Unsolved), Connected),
    ?assertEqual(binary:encode_hex(node_id(Unsolved), lowercase), Reported),
    finish(World, [Client, Station]).

%% The station's clock stands a second short of the tolerance past the
%% expiry of the client's statement, issued at T0 for an hour.
a_statement_that_lapses_closes_the_connection_with_status_expired(Ctx) ->
    World = world(Ctx, #{}),
    {Client, Station} = connect(World, #{mode => off, station_clock => ?T0 + ?HOUR + 5 * ?MINUTE - 1_000}),
    _ = await(Station, connected),
    ?assertEqual(status_expired, ended(Station)),
    finish(World, [Client, Station]).

%% The station's clock stands 3 seconds short of that tolerance, but the
%% client's issuer reissues its statement right after the handshake,
%% and the client sends it as a status frame, which moves the station's
%% timer past the end of the test.
status_frames_at_each_reissue_keep_the_connection_open(Ctx) ->
    #{issuer_tab := Tab, client_issuer := ClientIssuer} = World = world(Ctx, #{}),
    {Client, Station} = connect(World, #{mode => off, station_clock => ?T0 + ?HOUR + 5 * ?MINUTE - 3_000}),
    _ = {await(Client, connected), await(Station, connected)},
    set_time(Tab, ?T0 + 15 * ?MINUTE),
    ok = macula_statement_issuer:tick(ClientIssuer),
    ?assertEqual(open, still_open(Station, 4_500)),
    finish(World, [Client, Station]).

a_status_frame_that_fails_its_checks_closes_with_that_reason(Ctx) ->
    #{client_issuer := ClientIssuer} = World = world(Ctx, #{}),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    #{connect_binding := #{tbs := Tbs}, connect_status := #{signature := Signature} = Statement} =
        macula_statement_issuer:connect_material(ClientIssuer),
    Client ! {macula_statement, ClientIssuer, crypto:hash(sha384, Tbs), Statement#{signature := flip(Signature)}},
    ?assertEqual(status_signature_invalid, ended(Station)),
    finish(World, [Client, Station]).

%% The station's TLS binding, bound at T0, ends 7 days later. Its issuer
%% states it again 2 minutes before that, and the client's clock stands
%% a second before the end.
a_binding_at_its_not_after_closes_the_connection_with_binding_expired(Ctx) ->
    #{issuer_tab := Tab, station_issuer := StationIssuer} = World = world(Ctx, #{}),
    set_time(Tab, ?T0 + 7 * ?DAY - 2 * ?MINUTE),
    ok = macula_statement_issuer:tick(StationIssuer),
    {Client, Station} = connect(World, #{mode => off, client_clock => ?T0 + 7 * ?DAY - 1_000}),
    _ = await(Client, connected),
    ?assertEqual(binding_expired, ended(Client)),
    finish(World, [Client, Station]).

%%====================================================================
%% The world: a listener, each side's identity key and issuer, a clock
%%====================================================================

%% A station and a client, each with its identity key and a statement
%% issuer on one test clock, and a listener presenting the setup's leaf,
%% which the station's issuer binds. The keys are in pq_pure unless the
%% test names another profile.
world(#{der := Der} = Ctx, Options) ->
    Profile = maps:get(profile, Options, pq_pure),
    {Tab, Clock} = clock(?T0),
    StationKey = identity(Profile),
    ClientKey = maps:get(client_key, Options, identity(Profile)),
    {ok, TlsKey} = macula_node_keys:generate(tls, Profile),
    StationIssuer = issuer(StationKey, Clock),
    ok = macula_statement_issuer:register_tls_leaf(StationIssuer, Der, TlsKey),
    {Listener, Port} = start_listener(Ctx),
    #{issuer_tab => Tab, station_key => StationKey, client_key => ClientKey, station_issuer => StationIssuer,
      client_issuer => issuer(ClientKey, Clock), listener => Listener, port => Port}.

%% A client connection dialing the listener, and the station connection
%% for what the listener accepts. The clocks of both connections stand
%% still, a minute after T0 unless the test says otherwise.
connect(#{client_key := ClientKey, client_issuer := ClientIssuer, station_key := StationKey, port := Port} = World,
        Options) ->
    Target = #{host => <<"127.0.0.1">>, port => Port, timeout_ms => 5_000,
               expected_node_id => maps:get(expected, Options, node_id(StationKey))},
    {ok, Client} = macula_peering:connect(#{identity => ClientKey, issuer => ClientIssuer,
                                            capabilities => ?CLIENT_CAPABILITIES, controlling_pid => self(),
                                            clock => fixed(maps:get(client_clock, Options, ?T0 + ?MINUTE)),
                                            target => Target}),
    {Client, accept_one(station_opts(World, Options))}.

station_opts(#{station_key := StationKey, station_issuer := StationIssuer}, Options) ->
    Opts = #{identity => StationKey, issuer => StationIssuer, capabilities => ?STATION_CAPABILITIES,
             controlling_pid => self(), puzzle => #{mode => maps:get(mode, Options)},
             clock => fixed(maps:get(station_clock, Options, ?T0 + ?MINUTE))},
    maps:merge(Opts, maps:with([accept_owner], Options)).

accept_one(StationOpts) ->
    receive
        {quic, new_conn, Conn, _Info} ->
            {ok, Station} = macula_peering:accept(Conn, StationOpts),
            Station
    after 5_000 ->
        erlang:error(no_inbound_conn)
    end.

accept_owner(Opts) ->
    accept_owner_opt(lists:member(no_accept_owner, Opts)).

accept_owner_opt(true)  -> #{};
accept_owner_opt(false) -> #{accept_owner => self()}.

%% Both connections end at once, with no drain, before the world goes.
finish(World, Conns) ->
    _ = [macula_peering:reject(Conn, test_finished) || Conn <- Conns],
    _ = [wait_down(Conn) || Conn <- Conns],
    cleanup_pair(undefined, undefined, World).

forget_world(#{listener := Listener, station_issuer := StationIssuer, client_issuer := ClientIssuer,
               issuer_tab := Tab}) ->
    stop_listener(Listener),
    ok = gen_server:stop(StationIssuer),
    ok = gen_server:stop(ClientIssuer),
    true = ets:delete(Tab),
    ok.

identity() ->
    identity(pq_pure).

identity(Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Key.

node_id(Key) ->
    {ok, NodeId} = macula_node_keys:node_id(Key),
    NodeId.

solved_identity() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure,
                                          #{puzzle_difficulty => macula_node_keys:puzzle_difficulty()}),
    Key.

%% An identity key whose node_id does not meet the puzzle.
unsolved_identity() ->
    Key = identity(),
    unsolved(macula_node_keys:puzzle_solved(node_id(Key), macula_node_keys:puzzle_difficulty()), Key).

unsolved(false, Key) -> Key;
unsolved(true, _Key) -> unsolved_identity().

issuer(Key, Clock) ->
    {ok, Issuer} = macula_statement_issuer:start_link(#{identity => fun() -> Key end, owner => self(), clock => Clock}),
    Issuer.

clock(Start) ->
    Tab = ets:new(handshake_test_clock, [public, set]),
    set_time(Tab, Start),
    {Tab, fun() -> ets:lookup_element(Tab, now, 2) end}.

set_time(Tab, Ms) ->
    true = ets:insert(Tab, {now, Ms}).

fixed(Ms) ->
    fun() -> Ms end.

await(Pid, Event) ->
    receive
        {macula_peering, Event, Pid, Detail} -> Detail;
        {macula_peering, disconnected, Pid, Reason} -> erlang:error({disconnected, Reason})
    after 5_000 ->
        erlang:error({no_event, Event})
    end.

ended(Pid) ->
    receive
        {macula_peering, disconnected, Pid, Reason} -> Reason
    after 6_000 ->
        still_running
    end.

still_open(Pid, Ms) ->
    receive
        {macula_peering, disconnected, Pid, Reason} -> {closed, Reason}
    after Ms ->
        open
    end.

wait_down(Pid) ->
    Monitor = erlang:monitor(process, Pid),
    receive
        {'DOWN', Monitor, process, Pid, _Reason} -> ok
    after 3_000 ->
        erlang:error({still_alive, Pid})
    end.

capture_diagnostics() ->
    Handler = list_to_atom("peering_handshake_test_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Handler, ?MODULE, #{config => #{test => self()}, level => all, filter_default => log}),
    Handler.

%% The logger handler that forwards the station's report of an unsolved
%% puzzle to the test process.
log(#{msg := {report, #{event := ?UNSOLVED, properties := #{node_id := NodeId}}}}, #{config := #{test := Test}}) ->
    Test ! {puzzle_unsolved, NodeId};
log(_Event, _Config) ->
    ok.

reported_unsolved() ->
    receive
        {puzzle_unsolved, NodeId} -> NodeId
    after 2_000 ->
        none
    end.

flip(<<Head:20/binary, Byte, Tail/binary>>) ->
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

%%====================================================================
%% Neighbour signatures in pq_hybrid (D17)
%%====================================================================

%% Both sides send control frames both ways, and each issuer's reissued
%% statement goes out as a status frame between two rounds of them. The
%% clocks of both connections stand 3 seconds short of the tolerance past
%% the statements from the handshake, so each side stays open past that
%% only by reading the other's status frame, and a control frame whose
%% neighbour signature or seq failed would close the connection.
control_frames_verify_both_ways_around_a_status_frame_in_pq_hybrid(Ctx) ->
    #{issuer_tab := Tab, client_issuer := ClientIssuer, station_issuer := StationIssuer} = World =
        world(Ctx, #{profile => pq_hybrid}),
    Lapse = ?T0 + ?HOUR + 5 * ?MINUTE - 3_000,
    {Client, Station} = connect(World, #{mode => off, client_clock => Lapse, station_clock => Lapse}),
    _ = {await(Client, connected), await(Station, connected)},
    ok = sent(Client, Station, [ping() || _ <- lists:seq(1, 3)]),
    ok = sent(Station, Client, [ping() || _ <- lists:seq(1, 3)]),
    set_time(Tab, ?T0 + 15 * ?MINUTE),
    ok = macula_statement_issuer:tick(ClientIssuer),
    ok = macula_statement_issuer:tick(StationIssuer),
    ok = sent(Client, Station, [ping() || _ <- lists:seq(1, 3)]),
    ok = sent(Station, Client, [ping() || _ <- lists:seq(1, 3)]),
    ?assertEqual({open, open}, {still_open(Client, 4_500), still_open(Station, 0)}),
    finish(World, [Client, Station]).

%% A control frame signed with a key other than the peer's identity key
%% closes the connection with signature_invalid.
a_control_frame_signed_by_another_key_closes_the_connection_in_pq_hybrid(Ctx) ->
    #{station_key := StationKey} = World = world(Ctx, #{profile => pq_hybrid}),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    Forged = macula_frame:sign_neighbour(ping(), StationKey,
                                         #{connection => crypto:hash(sha384, <<"a challenge">>), seq => 0}),
    ok = on_control_stream(Client, macula_frame:encode(Forged)),
    ?assertEqual(signature_invalid, ended(Station)),
    finish(World, [Client, Station]).

%% In pq_hybrid a control frame without a neighbour signature closes the
%% connection with malformed_frame.
a_control_frame_without_a_neighbour_signature_closes_the_connection_in_pq_hybrid(Ctx) ->
    World = world(Ctx, #{profile => pq_hybrid}),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    ok = on_control_stream(Client, macula_frame:encode(ping())),
    ?assertEqual(malformed_frame, ended(Station)),
    finish(World, [Client, Station]).

%% An OVERLAY_RELAY is a control frame: sent on an open pq_hybrid connection
%% with no signature of its own, it goes out neighbour-signed, and the other
%% side verifies it and delivers the frame as its producer built it. A control
%% frame without a neighbour signature would have closed that side instead.
an_overlay_relay_goes_out_neighbour_signed_and_verifies_in_pq_hybrid(Ctx) ->
    World = world(Ctx, #{profile => pq_hybrid}),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    Relay = macula_frame:overlay_relay(#{peer => <<9:256>>, payload => macula_frame:encode(ping())}),
    ok = sent(Client, Station, [Relay]),
    ?assertEqual({open, open}, {still_open(Client, 500), still_open(Station, 0)}),
    finish(World, [Client, Station]).

ping() ->
    macula_frame:ping(#{nonce => crypto:strong_rand_bytes(16)}).

%% A frame as a peer reads it: encoded and decoded.
wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

%% Frames sent through one connection reach the controlling process of
%% the other, in order, as their producer built them.
sent(From, To, Frames) ->
    _ = [ok = macula_peering:send_frame(From, Frame) || Frame <- Frames],
    ?assertEqual([wire(Frame) || Frame <- Frames], [frame_from(To) || _ <- Frames]),
    ok.

frame_from(Pid) ->
    receive
        {macula_peering, frame, Pid, Frame} -> Frame;
        {macula_peering, disconnected, Pid, Reason} -> erlang:error({disconnected, Reason})
    after 5_000 ->
        erlang:error(no_frame)
    end.

%% Bytes written onto a connection's control stream from outside the
%% connection, as a peer breaking the rules would write them. The stream
%% is one of the two references in the connection's state, and the
%% connection resource refuses the send.
on_control_stream(Conn, Bytes) ->
    {connected, Data} = sys:get_state(Conn, 1_000),
    Results = [catch macula_quic:send(Ref, Bytes) || Ref <- tuple_to_list(Data), is_reference(Ref)],
    ?assert(lists:member(ok, Results)),
    ok.

%%====================================================================
%% Frames on the open connection that are not application frames
%%====================================================================

%% A version-2 frame with a field its table does not name closes the
%% connection with malformed_frame.
a_frame_with_a_field_its_table_does_not_name_closes_with_malformed_frame(Ctx) ->
    World = world(Ctx, #{}),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    <<_Length:32, Bytes/binary>> = macula_frame:encode(ping()),
    ok = on_control_stream(Client, macula_frame:encode_bytes(binary:replace(Bytes, <<"nonce">>, <<"nonzz">>))),
    ?assertEqual(malformed_frame, ended(Station)),
    finish(World, [Client, Station]).

%% An application frame written onto the control stream reaches the
%% controlling process as it was built: the connection leaves application
%% frame types to their owners.
an_application_frame_on_the_control_stream_is_delivered(Ctx) ->
    World = world(Ctx, #{}),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    Ping = ping(),
    ok = on_control_stream(Client, macula_frame:encode(Ping)),
    ?assertEqual(wire(Ping), frame_from(Station)),
    ?assertEqual(open, still_open(Station, 500)),
    finish(World, [Client, Station]).

%% A handshake frame after HELLO closes the connection with
%% malformed_frame: here a second accepted HELLO.
a_handshake_frame_after_hello_closes_with_malformed_frame(Ctx) ->
    World = world(Ctx, #{}),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    Hello = macula_record_cbor:encode(#{{text, <<"version">>} => 3, {text, <<"frame_type">>} => {text, <<"hello">>},
                                        {text, <<"accepted">>} => 1, {text, <<"capabilities">>} => 0}),
    ok = on_control_stream(Client, macula_frame:encode_bytes(Hello)),
    ?assertEqual(malformed_frame, ended(Station)),
    finish(World, [Client, Station]).

%%====================================================================
%% Bytes a peer sends that do not make a frame, and frames with fields
%% their type refuses: a refused frame from the peer on the other end of
%% the connection ends the connection, named and counted once
%%====================================================================

%% The frame cap in macula_frame, 16 MiB, and the handshake frame cap in
%% macula_peering_conn, 64 KiB.
-define(FRAME_CAP, 16#FFFFFF).
-define(HANDSHAKE_CAP, 64 * 1024).
-define(NOT_CBOR, <<10:32/big, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>).

a_handshake_frame_that_is_not_cbor_closes_with_malformed_frame(Ctx) ->
    with_raw_peer(Ctx, fun(Station, Stream) ->
        ok = macula_quic:send(Stream, ?NOT_CBOR),
        _ = macula_quic:send(Stream, binary:copy(<<0>>, 65_536)),
        ?assertEqual(malformed_frame, ended(Station))
    end).

a_handshake_length_header_above_the_frame_cap_closes_from_the_header(Ctx) ->
    with_raw_peer(Ctx, fun(Station, Stream) ->
        ok = macula_quic:send(Stream, <<(?FRAME_CAP + 1):32/big>>),
        ?assertEqual(malformed_frame, ended(Station))
    end).

%% A status frame is no opener, so a station that reads one of exactly
%% 64 KiB closes with unexpected_frame. With one byte more, its length
%% header alone closes the connection.
a_handshake_frame_of_64_kib_is_read_and_one_byte_more_closes(Ctx) ->
    AtCap = status_frame_of_size(?HANDSHAKE_CAP),
    ?assertEqual(?HANDSHAKE_CAP, byte_size(AtCap)),
    with_raw_peer(Ctx, fun(Station, Stream) ->
        ok = macula_quic:send(Stream, macula_frame:encode_bytes(AtCap)),
        ?assertEqual(unexpected_frame, ended(Station))
    end),
    with_raw_peer(Ctx, fun(Station, Stream) ->
        ok = macula_quic:send(Stream, <<(?HANDSHAKE_CAP + 1):32/big>>),
        ?assertEqual(malformed_frame, ended(Station))
    end).

bytes_that_are_not_cbor_on_the_control_stream_close_with_malformed_frame(Ctx) ->
    on_open_connection(Ctx, fun(Client, Station) ->
        ok = on_control_stream(Client, ?NOT_CBOR),
        ?assertEqual(malformed_frame, ended(Station))
    end).

a_control_stream_length_header_above_the_frame_cap_closes_from_the_header(Ctx) ->
    on_open_connection(Ctx, fun(Client, Station) ->
        ok = on_control_stream(Client, <<(?FRAME_CAP + 1):32/big>>),
        ?assertEqual(malformed_frame, ended(Station))
    end).

%% An array of 131,072 items, the element budget, is one item over it with
%% the array itself.
a_control_stream_frame_over_the_element_budget_closes_with_malformed_frame(Ctx) ->
    Budget = 131_072,
    OverBudget = <<16#9A, Budget:32/big, (binary:copy(<<0>>, Budget))/binary>>,
    on_open_connection(Ctx, fun(Client, Station) ->
        ok = on_control_stream(Client, macula_frame:encode_bytes(OverBudget)),
        ?assertEqual(malformed_frame, ended(Station))
    end).

a_call_a_publish_and_a_stream_open_on_the_control_stream_are_delivered(Ctx) ->
    Key = identity(),
    Spec = #{request_id => <<1:128>>, realm => <<1:256>>, procedure => <<"io.macula.test.echo">>,
             target => node_id(identity()), deadline => erlang:system_time(millisecond) + 60_000, payload => #{}},
    Frames = [macula_frame:call(Spec, Key),
              macula_frame:publish(#{realm => <<1:256>>, topic => <<"t">>, seq => 0,
                                     published_at => erlang:system_time(millisecond), payload => 1}, Key),
              macula_frame:stream_open(Spec#{request_id => <<2:128>>, mode => bidi}, Key)],
    on_open_connection(Ctx, fun(Client, Station) ->
        _ = [ok = on_control_stream(Client, macula_frame:encode(Frame)) || Frame <- Frames],
        ?assertEqual([wire(Frame) || Frame <- Frames], [frame_from(Station) || _ <- Frames]),
        ?assertEqual(open, still_open(Station, 500))
    end).

a_call_without_its_request_closes_with_malformed_frame(Ctx) ->
    Call = macula_record_cbor:encode(#{{text, <<"version">>} => macula_frame:version(ping()),
                                       {text, <<"frame_type">>} => {text, <<"call">>}}),
    on_open_connection(Ctx, fun(Client, Station) ->
        ok = on_control_stream(Client, macula_frame:encode_bytes(Call)),
        ?assertEqual(malformed_frame, ended(Station))
    end).

%% A type name this node does not know, whether sent as text or as bytes.
a_frame_of_a_type_this_node_does_not_know_closes_with_malformed_frame(Ctx) ->
    Version = macula_frame:version(ping()),
    Frames = [#{{text, <<"version">>} => Version, {text, <<"frame_type">>} => {text, <<"zz_future_frame">>}},
              #{{text, <<"version">>} => Version, {text, <<"frame_type">>} => <<"zz_future_frame">>}],
    [on_open_connection(Ctx, fun(Client, Station) ->
         ok = on_control_stream(Client, macula_frame:encode_bytes(macula_record_cbor:encode(Frame))),
         ?assertEqual(malformed_frame, ended(Station))
     end) || Frame <- Frames],
    ok.

%% A record is an object carried for others: the connection passes the
%% STORE to its recipient, which checks the record, and reads the next
%% frame.
a_store_whose_record_is_not_cbor_reaches_its_recipient_and_the_connection_serves_on(Ctx) ->
    Store = macula_frame:store(#{record => <<255, 255, 255, 255>>}),
    Ping = ping(),
    on_open_connection(Ctx, fun(Client, Station) ->
        ok = on_control_stream(Client, macula_frame:encode(Store)),
        ok = on_control_stream(Client, macula_frame:encode(Ping)),
        ?assertEqual([wire(Store), wire(Ping)], [frame_from(Station), frame_from(Station)]),
        ?assertEqual(open, still_open(Station, 500))
    end).

%% Runs Scenario with a station connection accepted from a raw QUIC peer
%% that has not sent an opener, and that peer's open stream.
with_raw_peer(Ctx, Scenario) ->
    #{port := Port} = World = world(Ctx, #{}),
    {ok, Raw} = macula_quic:connect(<<"127.0.0.1">>, Port, [{verify, none}, {alpn, [<<"macula">>]}], 5_000),
    Station = accept_one(station_opts(World, #{mode => off})),
    {ok, Stream} = macula_quic:open_stream(Raw),
    try
        Scenario(Station, Stream)
    after
        _ = (catch macula_quic:close_connection(Raw)),
        finish(World, [Station])
    end.

%% Runs Scenario on a connected client and station.
on_open_connection(Ctx, Scenario) ->
    World = world(Ctx, #{}),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    try
        Scenario(Client, Station)
    after
        finish(World, [Client, Station])
    end.

%% A status frame whose CBOR bytes are exactly Bytes long: its tbs makes up
%% the size, with a byte string header of three bytes from 256 bytes on.
status_frame_of_size(Bytes) ->
    Status = fun(Size) -> macula_handshake:status(#{tbs => binary:copy(<<0>>, Size), signature => <<"s">>}) end,
    Fixed = byte_size(Status(256)) - 256,
    Status(Bytes - Fixed).
