%% @doc Per-peer connection state machine.
%%
%% Implements the lifecycle from `Part 4 §10' simplified for Phase 1:
%% no REFRESH phase, no RECONNECTING — just enough to exchange signed
%% CONNECT/HELLO frames and drain on GOODBYE.
%%
%% State graph:
%% <pre>
%%   client: connecting → handshaking → connected → draining → (terminate)
%%   server: awaiting_start → handshaking → connected → draining → (terminate)
%% </pre>
-module(macula_peering_conn).
-behaviour(gen_statem).

-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([connecting/3, awaiting_start/3, handshaking/3, connected/3, draining/3]).

-export_type([opts/0, connect_opts/0]).

-type connect_opts() :: #{
    host        := binary() | string(),
    port        := inet:port_number(),
    alpn        => [binary()],
    timeout_ms  => timeout(),
    %% TLS trust mode for the QUIC dial. `webpki' (the default since
    %% 5.0.0) validates the server cert against webpki roots + the
    %% dialed hostname. `none' skips TLS verification — development /
    %% self-signed labs only; every such dial logs a warning.
    %% Ignored when `expected_node_id' is set (pubkey pin wins).
    verify      => webpki | none,
    %% The peer's Ed25519 pubkey, when the dialer knows who it is
    %% dialing (DHT node records, pre-shared relay identities). Two
    %% enforcement points: (a) the QUIC dial pins the server cert's
    %% SPKI to this key (no CA needed — station self-signed certs
    %% wrap the node identity key), and (b) the HELLO handshake is
    %% rejected unless the peer's verified `node_id' equals this key.
    %% Without it, the handshake only proves the peer holds the key
    %% for whatever identity IT claims (self-asserted).
    expected_node_id => macula_identity:pubkey(),
    _           => _
}.

-type opts() :: #{
    role            := client | server,
    identity        := macula_identity:key_pair(),
    realms          := [macula_identity:pubkey()],
    capabilities    := non_neg_integer(),
    controlling_pid := pid(),
    target          => connect_opts(),
    quic_conn       => reference(),
    %% Optional pid notified once when the worker completes the
    %% CONNECT/HELLO handshake and transitions to `connected'. Sent
    %% as `{macula_peering, handshake_complete, self(), PeerNodeId}'
    %% where `PeerNodeId' is the verified peer Ed25519 pubkey from
    %% the inbound CONNECT/HELLO frame. Used by accept-side listeners
    %% that (a) cap concurrent *handshaking* workers and need to
    %% release a slot the moment a worker is verified-and-connected,
    %% and (b) dedupe duplicate dials from the same peer identity by
    %% closing prior workers for the same `PeerNodeId'. Distinct from
    %% `controlling_pid', which receives the peer-node-id-bearing
    %% `connected' / `frame' / `disconnected' stream.
    accept_owner    => pid(),
    %% Optional pid that receives DHT-class frames (`ping', `pong',
    %% `find_node', `nodes', `find_value', `value', `store',
    %% `store_ack', `replicate', `replicate_ack') directly, bypassing
    %% `controlling_pid'. Sent as
    %%     `{macula_peering, dht_frame, self(), PeerNodeId, Frame}'.
    %% Stations set this to their `macula_dht' pid so DHT traffic
    %% (which under load is 85%+ of all inbound frames — `_dht.put_record'
    %% replication chatter) does not queue behind handler-dispatch and
    %% sub/pub work in the observer's gen_server mailbox. When
    %% unset (the default), DHT frames flow through `controlling_pid'
    %% in the legacy `{macula_peering, frame, ...}' form. The peer's
    %% verified `PeerNodeId' is included so the recipient does not have
    %% to walk frame internals to decide routing.
    dht_recipient   => pid(),
    %% Optional pid that receives pubsub-class frames (`subscribe',
    %% `unsubscribe', `publish', `event') directly, bypassing
    %% `controlling_pid'. Sent as
    %%     `{macula_peering, pubsub_frame, self(), PeerNodeId, Frame}'.
    %% Mirrors `dht_recipient' for the pubsub category. After DHT was
    %% bypassed (4.4.3), inbound EVENT became the dominant work on
    %% station observers — multi-publisher cases fire bursts of
    %% Ed25519-verify-per-event work that backs up the same gen_server
    %% mailbox that handles handler dispatch and ADVERTISE / SUBSCRIBE
    %% propagation. Stations on macula >= 4.4.4 set this to a dedicated
    %% pubsub frame dispatcher.
    pubsub_recipient => pid(),
    %% When true, every inbound-frame notification carries an extra
    %% `RecvAtUs :: integer()' element captured the moment the frame
    %% finished decoding (just before dispatch to the recipient). The
    %% controlling_pid then receives the 5-tuple
    %%   `{macula_peering, frame, ConnPid, Frame, RecvAtUs}'
    %% and the dht/pubsub bypass paths receive the 6-tuple
    %%   `{macula_peering, dht_frame, ConnPid, NodeId, Frame, RecvAtUs}'
    %%   `{macula_peering, pubsub_frame, ConnPid, NodeId, Frame, RecvAtUs}'
    %% RecvAtUs is `erlang:monotonic_time(microsecond)' on the receiving
    %% BEAM. Stations subtract from their own monotonic clock to compute
    %% mailbox wait time at the recipient. Defaults to false; recipients
    %% MUST keep the legacy 4-/5-tuple match clause to remain compatible
    %% with peers that have not opted in (cross-version rollout window).
    timing_enabled  => boolean()
}.

-record(data, {
    role             :: client | server,
    identity         :: macula_identity:key_pair(),
    node_id          :: macula_identity:pubkey(),
    realms           :: [macula_identity:pubkey()],
    capabilities     :: non_neg_integer(),
    controlling_pid  :: pid(),
    accept_owner     :: undefined | pid(),
    dht_recipient    :: undefined | pid(),
    pubsub_recipient :: undefined | pid(),
    timing_enabled   :: boolean(),
    target           :: undefined | connect_opts(),
    %% Pinned peer identity from the target's `expected_node_id'.
    %% `undefined' on the server role and on dials where the peer
    %% identity is not known up front (first-contact bootstrap).
    expected_node_id :: undefined | macula_identity:pubkey(),
    quic_conn        :: undefined | reference(),
    quic_stream      :: undefined | reference(),
    peer_node_id      :: undefined | macula_identity:pubkey(),
    peer_station_id   :: undefined | macula_identity:pubkey(),
    peer_realms       :: [macula_identity:pubkey()],
    %% Counterpart's capabilities bitmask as carried in CONNECT
    %% (server-side absorb) or HELLO (client-side absorb). Captured
    %% in `absorb_peer_info/2'. Stays `undefined' until handshake.
    peer_capabilities :: undefined | non_neg_integer(),
    buf               :: binary()
}).

-define(DRAIN_TIMEOUT_MS, 5_000).
%% Maximum time the `handshaking' state may take before the worker
%% gives up. CONNECT/HELLO is sub-second on a healthy peer; 30s is
%% generous. Drains workers stuck because the peer speaks the wrong
%% protocol (e.g. V1 frames against a V2 station) — without this the
%% sup accumulates stuck workers indefinitely. See PLAN_FLYING_RESTART.
-define(HANDSHAKE_TIMEOUT_MS, 30_000).

%%------------------------------------------------------------------
%% Lifecycle
%%------------------------------------------------------------------

-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_statem:start_link(?MODULE, Opts, []).

callback_mode() ->
    [state_functions, state_enter].

init(#{role := Role, identity := Identity, controlling_pid := Pid} = Opts)
  when Role =:= client; Role =:= server ->
    Data = #data{
        role             = Role,
        identity         = Identity,
        node_id          = macula_identity:public(Identity),
        realms           = maps:get(realms, Opts, []),
        capabilities     = maps:get(capabilities, Opts, 0),
        controlling_pid  = Pid,
        accept_owner     = maps:get(accept_owner, Opts, undefined),
        timing_enabled   = maps:get(timing_enabled, Opts, false),
        dht_recipient    = maps:get(dht_recipient, Opts, undefined),
        pubsub_recipient = maps:get(pubsub_recipient, Opts, undefined),
        target           = maps:get(target, Opts, undefined),
        expected_node_id = maps:get(expected_node_id,
                                    maps:get(target, Opts, #{}), undefined),
        quic_conn        = maps:get(quic_conn, Opts, undefined),
        quic_stream      = undefined,
        peer_node_id     = undefined,
        peer_station_id  = undefined,
        peer_realms      = [],
        buf              = <<>>
    },
    {ok, initial_state(Role), Data}.

initial_state(client) -> connecting;
initial_state(server) -> awaiting_start.

terminate(_Reason, _State, Data) ->
    _ = close_quic(Data),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%------------------------------------------------------------------
%% State: connecting (client only)
%%------------------------------------------------------------------

connecting(enter, _Old, Data) ->
    self() ! attempt_connect,
    {keep_state, Data};
connecting(info, attempt_connect, #data{target = Target} = Data) ->
    after_connect(do_connect(Target), Data);
connecting(cast, {close, Reason}, Data) ->
    notify(disconnected, Reason, Data),
    {stop, normal, Data};
connecting(EventType, Event, Data) ->
    drop_unexpected(EventType, Event, connecting, Data).

after_connect({ok, Conn}, Data) ->
    ok = macula_quic:controlling_process(Conn, self()),
    {next_state, handshaking, Data#data{quic_conn = Conn}};
after_connect(Other, Data) ->
    notify(disconnected, {connect_failed, Other}, Data),
    {stop, normal, Data}.

%%------------------------------------------------------------------
%% State: awaiting_start (server only — wait for ownership transfer)
%%------------------------------------------------------------------

awaiting_start(enter, _Old, Data) ->
    {keep_state, Data};
awaiting_start(cast, start_handshake, Data) ->
    {next_state, handshaking, Data};
awaiting_start(cast, {close, Reason}, Data) ->
    notify(disconnected, Reason, Data),
    {stop, normal, Data};
%% QUIC events that race the `start_handshake' cast must NOT be
%% dropped. `macula_peering:accept/2' transfers conn ownership before
%% it casts `start_handshake', and the QUIC NIF redelivers any
%% buffered `{quic, new_stream, ...}' / `{quic, Bin, Stream, _Flags}'
%% events to the new owner. If those land in the worker's mailbox
%% before the cast does, the old `drop_unexpected/4' clause sent them
%% to the floor and the worker stayed in `handshaking' forever with
%% an empty buffer — the peer's CONNECT frame never reached
%% `consume_handshake/2'. Live-verified across the production
%% Leuven fleet (every station had multiple stuck workers; vaartkom
%% specifically lost its inbound from centrum because of this). The
%% `[postpone]' action defers the message so it is re-delivered after
%% `start_handshake' transitions us into `handshaking', where the
%% real handler consumes it.
awaiting_start(info, {quic, _, _, _}, _Data) ->
    {keep_state_and_data, [postpone]};
awaiting_start(EventType, Event, Data) ->
    drop_unexpected(EventType, Event, awaiting_start, Data).

%%------------------------------------------------------------------
%% State: handshaking
%%------------------------------------------------------------------

handshaking(enter, _Old, #data{role = client, quic_conn = Conn} = Data) ->
    on_handshake_enter_client(macula_quic:open_stream(Conn), Data);
handshaking(enter, _Old, #data{role = server, quic_conn = Conn} = Data) ->
    ok = macula_quic:async_accept_stream(Conn),
    {keep_state, Data, [handshake_state_timeout()]};
handshaking(info, {quic, new_stream, Stream, _Info}, Data) ->
    %% Take ownership of the stream so subsequent `{quic, Bin, ...}'
    %% events route to us. The Quinn NIF stamps the stream's owner at
    %% creation time using whatever owns the conn AT THAT MOMENT —
    %% which on the server-side accept path can still be the listener
    %% (the conn ownership transfer happens just after Quinn's accept
    %% loop has already emitted `new_stream'). Without this call,
    %% future inbound bytes go to the listener's mailbox and get
    %% dropped by its wildcard `handle_info/2'. setopt(active, true)
    %% on its own does NOT change ownership; it only enables active
    %% delivery to the current owner.
    _ = macula_quic:controlling_process(Stream, self()),
    ok = macula_quic:setopt(Stream, active, true),
    {keep_state, Data#data{quic_stream = Stream}};
handshaking(info, {quic, Bin, Stream, _Flags},
            #data{quic_stream = Stream, buf = Buf} = Data) when is_binary(Bin) ->
    consume_handshake(<<Buf/binary, Bin/binary>>, Data);
handshaking(info, {quic, closed, _Conn, _Detail}, Data) ->
    notify(disconnected, closed_during_handshake, Data),
    {stop, normal, Data};
handshaking(cast, {close, Reason}, Data) ->
    notify(disconnected, Reason, Data),
    {stop, normal, Data};
%% No CONNECT/HELLO completed within the timeout window. Most common
%% cause: peer is speaking a different protocol version (e.g. V1
%% frames at a V2 station) — bytes accumulate in `buf' but never form
%% a valid frame. Surface a structured diagnostic and exit so the sup
%% does not retain the worker forever.
handshaking(state_timeout, handshake_timeout,
            #data{role = Role, buf = Buf, quic_stream = Stream} = Data) ->
    macula_diagnostics:event(<<"_macula.peering.handshake_timeout">>, #{
        role         => Role,
        buf_size     => byte_size(Buf),
        has_stream   => Stream =/= undefined,
        timeout_ms   => ?HANDSHAKE_TIMEOUT_MS
    }),
    notify(disconnected, handshake_timeout, Data),
    {stop, normal, Data};
handshaking(EventType, Event, Data) ->
    drop_unexpected(EventType, Event, handshaking, Data).

handshake_state_timeout() ->
    {state_timeout, ?HANDSHAKE_TIMEOUT_MS, handshake_timeout}.

on_handshake_enter_client({ok, Stream}, Data) ->
    %% setopt/send can both fail if the QUIC connection died between
    %% nif_connect returning {ok, Conn} and us getting here (peer
    %% closed, network drop, server rejected with a CONNECTION_CLOSE
    %% frame after the TLS handshake but before we open a stream).
    %% Prior to 3.15.3 the `ok = ...` matches turned every such
    %% race into a crash; now we surface a structured disconnect
    %% and let the caller schedule a reconnect.
    case macula_quic:setopt(Stream, active, true) of
        ok ->
            case send_connect(Stream, Data) of
                ok ->
                    {keep_state,
                     Data#data{quic_stream = Stream},
                     [handshake_state_timeout()]};
                {error, _} = SendErr ->
                    notify(disconnected, {send_connect_failed, SendErr}, Data),
                    {stop, normal, Data}
            end;
        {error, _} = SetoptErr ->
            notify(disconnected, {setopt_failed, SetoptErr}, Data),
            {stop, normal, Data}
    end;
on_handshake_enter_client(Err, Data) ->
    notify(disconnected, {open_stream_failed, Err}, Data),
    {stop, normal, Data}.

consume_handshake(Buf, Data) ->
    {Frames, Tail} = macula_frame:parse_stream(Buf),
    handle_handshake_frames(Frames, Data#data{buf = Tail}).

handle_handshake_frames([], Data) ->
    {keep_state, Data};
handle_handshake_frames([#{frame_type := connect} = F | _], Data) ->
    process_connect(F, Data);
handle_handshake_frames([#{frame_type := hello} = F | _], Data) ->
    process_hello(F, Data);
handle_handshake_frames([_Other | Rest], Data) ->
    handle_handshake_frames(Rest, Data).

%% Server side: peer's CONNECT
process_connect(#{node_id := PeerNodeId} = Frame,
                #data{role = server, quic_stream = Stream} = Data) ->
    on_connect_verified(macula_frame:verify(Frame, PeerNodeId), Frame, Stream, Data);
process_connect(_Frame, Data) ->
    notify(disconnected, unexpected_connect_on_client, Data),
    {stop, normal, Data}.

on_connect_verified({ok, _Verified}, Frame, Stream, Data) ->
    NewData = absorb_peer_info(Frame, Data),
    on_send_hello(send_hello(Stream, NewData), NewData);
on_connect_verified({error, R}, _Frame, _Stream, Data) ->
    notify(disconnected, {connect_verify_failed, R}, Data),
    {stop, normal, Data}.

%% Server-side handshake completion. `send_hello' wraps
%% `macula_quic:send', which returns `{error, _}' when the underlying
%% QUIC stream has been closed by the peer between our CONNECT-verify
%% and our HELLO write — a real race during teardown bursts (peer's
%% pool closed mid-handshake; many simultaneous closes during e2e
%% suite end_per_suite). Pre-fix: `ok = send_hello(...)' badmatched
%% the error and the peering_conn worker crashed. Under load that
%% tripped the supervisor's restart-intensity threshold and forced a
%% whole-station restart. Now mirrors the client-side
%% `send_connect' handling — emit a structured disconnect notify
%% and stop normally so the supervisor can clean up without
%% counting it as a crash.
on_send_hello(ok, NewData) ->
    transition_to_connected(NewData);
on_send_hello({error, _} = SendErr, NewData) ->
    notify(disconnected, {send_hello_failed, SendErr}, NewData),
    {stop, normal, NewData}.

%% Client side: peer's HELLO
process_hello(#{node_id := PeerNodeId} = Frame, #data{role = client} = Data) ->
    on_hello_verified(macula_frame:verify(Frame, PeerNodeId), Frame, Data);
process_hello(_Frame, Data) ->
    notify(disconnected, unexpected_hello_on_server, Data),
    {stop, normal, Data}.

on_hello_verified({ok, _Verified}, #{accepted := true} = Frame, Data) ->
    on_peer_identity_bound(bind_peer_identity(maps:get(node_id, Frame), Data),
                           Frame, Data);
on_hello_verified({ok, _Verified}, #{accepted := false} = Frame, Data) ->
    notify(disconnected, {refused, maps:get(refusal_code, Frame, undefined)}, Data),
    {stop, normal, Data};
on_hello_verified({error, R}, _Frame, Data) ->
    notify(disconnected, {hello_verify_failed, R}, Data),
    {stop, normal, Data}.

%% The frame-signature check above only proves the peer holds the key
%% for whatever `node_id' IT claims (self-asserted). When the dialer
%% pinned an `expected_node_id' in the target, require the verified
%% identity to match it — otherwise a redirected/intercepted dial
%% completes the handshake under the interceptor's own identity.
bind_peer_identity(_PeerNodeId, #data{expected_node_id = undefined}) ->
    ok;
bind_peer_identity(PeerNodeId, #data{expected_node_id = PeerNodeId}) ->
    ok;
bind_peer_identity(PeerNodeId, #data{expected_node_id = Expected}) ->
    {error, {peer_identity_mismatch, Expected, PeerNodeId}}.

on_peer_identity_bound(ok, Frame, Data) ->
    transition_to_connected(absorb_peer_info(Frame, Data));
on_peer_identity_bound({error, Mismatch}, _Frame, Data) ->
    macula_diagnostics:event(<<"_macula.peering.identity_mismatch">>, #{
        role => Data#data.role
    }),
    notify(disconnected, Mismatch, Data),
    {stop, normal, Data}.

absorb_peer_info(Frame, Data) ->
    Data#data{
        peer_node_id      = maps:get(node_id, Frame),
        peer_station_id   = maps:get(station_id, Frame),
        peer_realms       = maps:get(realms, Frame, []),
        peer_capabilities = maps:get(capabilities, Frame, 0)
    }.

transition_to_connected(Data) ->
    notify(connected, Data#data.peer_node_id, Data),
    notify_handshake_complete(Data),
    {next_state, connected, Data}.

notify_handshake_complete(#data{accept_owner = undefined}) ->
    ok;
notify_handshake_complete(#data{accept_owner = Pid, peer_node_id = NodeId})
        when is_pid(Pid) ->
    Pid ! {macula_peering, handshake_complete, self(), NodeId},
    ok.

%%------------------------------------------------------------------
%% State: connected
%%------------------------------------------------------------------

connected(enter, _Old, Data) ->
    {keep_state, Data};
connected(info, {quic, Bin, Stream, _Flags},
          #data{quic_stream = Stream, buf = Buf} = Data) when is_binary(Bin) ->
    {Frames, Tail} = macula_frame:parse_stream(<<Buf/binary, Bin/binary>>),
    [route_frame(F, Data) || F <- Frames],
    {keep_state, Data#data{buf = Tail}};
connected(info, {quic, closed, _Conn, _Detail}, Data) ->
    notify(disconnected, peer_closed, Data),
    {stop, normal, Data};
connected(cast, {close, Reason}, Data) ->
    _ = send_goodbye(Data#data.quic_stream, Reason, Data),
    {next_state, draining, Data};
connected(cast, {send_frame, Frame}, Data) ->
    %% Coalesce: drain any other queued `{send_frame, _}' casts and
    %% emit them in a single NIF write. Cuts per-NIF overhead +
    %% gen_statem reduction-counter cost when many EVENT/PUBLISH
    %% frames burst together (pubsub flood, DHT batch put). The
    %% Quinn stream still handles MTU-level packetisation; this is
    %% purely an Erlang-side amortization.
    Frames = drain_send_frames([Frame]),
    _ = send_application_frames(Frames, Data),
    {keep_state, Data};
connected({call, From}, peer_capabilities, Data) ->
    {keep_state, Data,
     [{reply, From, {ok, Data#data.peer_capabilities}}]};
connected(EventType, Event, Data) ->
    drop_unexpected(EventType, Event, connected, Data).

%%------------------------------------------------------------------
%% State: draining
%%------------------------------------------------------------------

draining(enter, _Old, Data) ->
    {keep_state, Data, [{state_timeout, ?DRAIN_TIMEOUT_MS, drain_done}]};
draining(state_timeout, drain_done, Data) ->
    notify(disconnected, drained, Data),
    _ = close_quic(Data),
    {stop, normal, Data};
draining(info, {quic, closed, _Conn, _Detail}, Data) ->
    notify(disconnected, peer_closed_during_drain, Data),
    {stop, normal, Data};
draining(info, {quic, _, _, _}, Data) ->
    %% Ignore late inbound during drain.
    {keep_state, Data};
draining(cast, {close, _Reason}, Data) ->
    %% Already draining — idempotent.
    {keep_state, Data};
draining(EventType, Event, Data) ->
    drop_unexpected(EventType, Event, draining, Data).

%%------------------------------------------------------------------
%% Frame send helpers
%%------------------------------------------------------------------

send_connect(Stream, Data) ->
    Frame = macula_frame:connect(#{
        node_id          => Data#data.node_id,
        station_id       => Data#data.node_id,
        realms           => Data#data.realms,
        capabilities     => Data#data.capabilities,
        puzzle_evidence  => macula_identity:puzzle_evidence(Data#data.node_id)
    }),
    Signed = macula_frame:sign(Frame, Data#data.identity),
    macula_quic:send(Stream, macula_frame:encode(Signed)).

send_hello(Stream, Data) ->
    Frame = macula_frame:hello(#{
        node_id                 => Data#data.node_id,
        station_id              => Data#data.node_id,
        realms                  => Data#data.realms,
        capabilities            => Data#data.capabilities,
        accepted                => true,
        negotiated_capabilities => Data#data.capabilities
    }),
    Signed = macula_frame:sign(Frame, Data#data.identity),
    macula_quic:send(Stream, macula_frame:encode(Signed)).

send_goodbye(undefined, _Reason, _Data) ->
    ok;
send_goodbye(Stream, Reason, Data) ->
    Frame = macula_frame:goodbye(Reason, undefined),
    Signed = macula_frame:sign(Frame, Data#data.identity),
    macula_quic:send(Stream, macula_frame:encode(Signed)).

send_application_frame(_Frame, #data{quic_stream = undefined}) ->
    ok;
send_application_frame(Frame, #data{quic_stream = Stream, identity = Id}) ->
    Signed = ensure_signed(Frame, Id),
    macula_quic:send(Stream, macula_frame:encode(Signed)).

%% Encode N frames into one iolist, sign each, push as a single NIF
%% call. Skips work entirely when the stream isn't yet up.
send_application_frames(_Frames, #data{quic_stream = undefined}) ->
    ok;
send_application_frames([Frame], Data) ->
    %% Single-frame fast path — avoid the iolist accumulation cost.
    send_application_frame(Frame, Data);
send_application_frames(Frames, #data{quic_stream = Stream, identity = Id}) ->
    Encoded = [macula_frame:encode(ensure_signed(F, Id)) || F <- Frames],
    macula_quic:send(Stream, Encoded).

%% Drain queued send_frame casts. Capped at ?MAX_BATCH frames per
%% pass so a runaway producer can't park us in the receive forever.
%% A `cast' arrives in the gen_statem mailbox as
%% `{'$gen_cast', {send_frame, F}}'. We pattern-match that exact
%% shape so unrelated mailbox traffic stays untouched.
-define(MAX_BATCH, 64).
drain_send_frames(Acc) ->
    drain_send_frames(Acc, ?MAX_BATCH - 1).

drain_send_frames(Acc, 0) ->
    lists:reverse(Acc);
drain_send_frames(Acc, N) ->
    receive
        {'$gen_cast', {send_frame, F}} ->
            drain_send_frames([F | Acc], N - 1)
    after 0 ->
        lists:reverse(Acc)
    end.

ensure_signed(#{signature := _} = Frame, _Id) -> Frame;
ensure_signed(Frame, Id) -> macula_frame:sign(Frame, Id).

close_quic(#data{quic_conn = undefined}) ->
    ok;
close_quic(#data{quic_conn = Conn}) ->
    catch macula_quic:close_connection(Conn),
    ok.

%%------------------------------------------------------------------
%% Outbound dial — unpack peering's option-map into macula_quic's
%% positional API.
%%------------------------------------------------------------------

do_connect(#{host := Host, port := Port} = Target) ->
    Timeout = maps:get(timeout_ms, Target, 30_000),
    Alpn = maps:get(alpn, Target, [<<"macula">>]),
    macula_quic:connect(Host, Port,
                        [{alpn, Alpn} | dial_trust_opts(Target)], Timeout).

%% TLS trust for the dial. A known peer identity pins the server
%% cert's Ed25519 SPKI (strongest — no CA involved); otherwise the
%% `verify' mode flows through, defaulting to webpki inside
%% `macula_quic:connect/4'. `{verify, none}' must be an explicit
%% caller choice and is warned about at the macula_quic layer.
dial_trust_opts(#{expected_node_id := NodeId}) when is_binary(NodeId),
                                                    byte_size(NodeId) =:= 32 ->
    [{verify_pubkey, NodeId}];
dial_trust_opts(#{verify := Mode}) ->
    [{verify, Mode}];
dial_trust_opts(_Target) ->
    [].

%%------------------------------------------------------------------
%% Notifications
%%------------------------------------------------------------------

notify(Event, Detail, #data{controlling_pid = Pid}) ->
    Pid ! {macula_peering, Event, self(), Detail},
    ok.

%% Inbound-frame router. Category-bypass: DHT-class frames go to
%% `dht_recipient' if set; pubsub-class frames go to `pubsub_recipient'
%% if set; everything else (and any bypass with the recipient unset)
%% flows through `controlling_pid' in the legacy form. See the
%% `dht_recipient' / `pubsub_recipient' field docs on `opts()' for why.
%%
%% When `timing_enabled' is true on this conn, the recipient receives
%% an extra trailing `RecvAtUs' element holding
%% `erlang:monotonic_time(microsecond)' captured here, so recipients
%% can compute mailbox wait at the receiving gen_server.
route_frame(Frame, #data{peer_node_id = NodeId} = Data)
        when is_binary(NodeId) ->
    route_by_category(category(Frame), Frame, NodeId, Data);
route_frame(Frame, Data) ->
    %% No verified peer node id yet (handshake edge), or it's a frame
    %% type we don't classify. Fall back to controlling_pid.
    notify_frame(Frame, Data).

route_by_category(dht, Frame, NodeId,
                  #data{dht_recipient = Pid,
                        timing_enabled = Timing}) when is_pid(Pid) ->
    notify_bypass(Pid, dht_frame, NodeId, Frame, Timing),
    ok;
route_by_category(pubsub, Frame, NodeId,
                  #data{pubsub_recipient = Pid,
                        timing_enabled = Timing}) when is_pid(Pid) ->
    notify_bypass(Pid, pubsub_frame, NodeId, Frame, Timing),
    ok;
route_by_category(_, Frame, _NodeId, Data) ->
    notify_frame(Frame, Data).

notify_frame(Frame, #data{controlling_pid = Pid, timing_enabled = false}) ->
    Pid ! {macula_peering, frame, self(), Frame},
    ok;
notify_frame(Frame, #data{controlling_pid = Pid, timing_enabled = true}) ->
    T = erlang:monotonic_time(microsecond),
    Pid ! {macula_peering, frame, self(), Frame, T},
    ok.

notify_bypass(Pid, Tag, NodeId, Frame, false) ->
    Pid ! {macula_peering, Tag, self(), NodeId, Frame},
    ok;
notify_bypass(Pid, Tag, NodeId, Frame, true) ->
    T = erlang:monotonic_time(microsecond),
    Pid ! {macula_peering, Tag, self(), NodeId, Frame, T},
    ok.

%% Mirror macula-station's `macula_station_peer_observer:classify/1' —
%% any frame type added to a category on one side must be added on the
%% other or frames will leak through the legacy controlling_pid path.
category(Frame) ->
    classify(macula_frame:frame_type(Frame)).

classify(ping)           -> dht;
classify(pong)           -> dht;
classify(find_node)      -> dht;
classify(nodes)          -> dht;
classify(find_value)     -> dht;
classify(value)          -> dht;
classify(store)          -> dht;
classify(store_ack)      -> dht;
classify(replicate)      -> dht;
classify(replicate_ack)  -> dht;
classify(subscribe)      -> pubsub;
classify(unsubscribe)    -> pubsub;
classify(publish)        -> pubsub;
classify(event)          -> pubsub;
classify(_)              -> other.

drop_unexpected({call, From}, Event, State, Data) ->
    %% Synchronous call into a state that doesn't handle it. Reply
    %% so the caller fails fast (e.g. `peer_capabilities/1' before
    %% handshake completes) instead of blocking until its own
    %% timeout — which would surface as `{timeout, ...}' to user
    %% code and require defensive try/catch wrappers everywhere.
    macula_diagnostics:event(<<"_macula.peering.unexpected">>, #{
        state      => State,
        event_type => call,
        event      => safe_event(Event)
    }),
    {keep_state, Data, [{reply, From, not_connected}]};
drop_unexpected(EventType, Event, State, Data) ->
    macula_diagnostics:event(<<"_macula.peering.unexpected">>, #{
        state      => State,
        event_type => EventType,
        event      => safe_event(Event)
    }),
    {keep_state, Data}.

%% Truncate large/binary events for safer log emission.
safe_event(Bin) when is_binary(Bin), byte_size(Bin) > 64 ->
    {truncated, byte_size(Bin)};
safe_event(Other) ->
    Other.
