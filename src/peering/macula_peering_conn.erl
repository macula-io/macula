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
    _           => _
}.

-type opts() :: #{
    role            := client | server,
    identity        := macula_identity:key_pair(),
    realms          := [macula_identity:pubkey()],
    capabilities    := non_neg_integer(),
    controlling_pid := pid(),
    target          => connect_opts(),
    quic_conn       => reference()
}.

-record(data, {
    role            :: client | server,
    identity        :: macula_identity:key_pair(),
    node_id         :: macula_identity:pubkey(),
    realms          :: [macula_identity:pubkey()],
    capabilities    :: non_neg_integer(),
    controlling_pid :: pid(),
    target          :: undefined | connect_opts(),
    quic_conn       :: undefined | reference(),
    quic_stream     :: undefined | reference(),
    peer_node_id    :: undefined | macula_identity:pubkey(),
    peer_station_id :: undefined | macula_identity:pubkey(),
    peer_realms     :: [macula_identity:pubkey()],
    buf             :: binary()
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
        role            = Role,
        identity        = Identity,
        node_id         = macula_identity:public(Identity),
        realms          = maps:get(realms, Opts, []),
        capabilities    = maps:get(capabilities, Opts, 0),
        controlling_pid = Pid,
        target          = maps:get(target, Opts, undefined),
        quic_conn       = maps:get(quic_conn, Opts, undefined),
        quic_stream     = undefined,
        peer_node_id    = undefined,
        peer_station_id = undefined,
        peer_realms     = [],
        buf             = <<>>
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
    ok = send_hello(Stream, NewData),
    transition_to_connected(NewData);
on_connect_verified({error, R}, _Frame, _Stream, Data) ->
    notify(disconnected, {connect_verify_failed, R}, Data),
    {stop, normal, Data}.

%% Client side: peer's HELLO
process_hello(#{node_id := PeerNodeId} = Frame, #data{role = client} = Data) ->
    on_hello_verified(macula_frame:verify(Frame, PeerNodeId), Frame, Data);
process_hello(_Frame, Data) ->
    notify(disconnected, unexpected_hello_on_server, Data),
    {stop, normal, Data}.

on_hello_verified({ok, _Verified}, #{accepted := true} = Frame, Data) ->
    transition_to_connected(absorb_peer_info(Frame, Data));
on_hello_verified({ok, _Verified}, #{accepted := false} = Frame, Data) ->
    notify(disconnected, {refused, maps:get(refusal_code, Frame, undefined)}, Data),
    {stop, normal, Data};
on_hello_verified({error, R}, _Frame, Data) ->
    notify(disconnected, {hello_verify_failed, R}, Data),
    {stop, normal, Data}.

absorb_peer_info(Frame, Data) ->
    Data#data{
        peer_node_id    = maps:get(node_id, Frame),
        peer_station_id = maps:get(station_id, Frame),
        peer_realms     = maps:get(realms, Frame, [])
    }.

transition_to_connected(Data) ->
    notify(connected, Data#data.peer_node_id, Data),
    {next_state, connected, Data}.

%%------------------------------------------------------------------
%% State: connected
%%------------------------------------------------------------------

connected(enter, _Old, Data) ->
    {keep_state, Data};
connected(info, {quic, Bin, Stream, _Flags},
          #data{quic_stream = Stream, buf = Buf} = Data) when is_binary(Bin) ->
    {Frames, Tail} = macula_frame:parse_stream(<<Buf/binary, Bin/binary>>),
    [notify(frame, F, Data) || F <- Frames],
    {keep_state, Data#data{buf = Tail}};
connected(info, {quic, closed, _Conn, _Detail}, Data) ->
    notify(disconnected, peer_closed, Data),
    {stop, normal, Data};
connected(cast, {close, Reason}, Data) ->
    _ = send_goodbye(Data#data.quic_stream, Reason, Data),
    {next_state, draining, Data};
connected(cast, {send_frame, Frame}, Data) ->
    _ = send_application_frame(Frame, Data),
    {keep_state, Data};
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

%% Sovereign-overlay path: target carries a pinned Ed25519 pubkey
%% (32 raw bytes). Derive the Yggdrasil IPv6 from the pubkey and
%% pass the pubkey through to `macula_quic' as `verify_pubkey' so
%% the leaf cert SPKI is pin-checked at handshake time. No DNS, no
%% CA chain. See PLAN_SOVEREIGN_OVERLAY_PHASE1 §4.4.
do_connect(#{pubkey := Pk, port := Port} = Target)
        when is_binary(Pk), byte_size(Pk) =:= 32 ->
    Timeout = maps:get(timeout_ms, Target, 30_000),
    Alpn = maps:get(alpn, Target, [<<"macula">>]),
    macula_quic:connect({pubkey, Pk}, Port, [{alpn, Alpn}], Timeout);
do_connect(#{host := Host, port := Port} = Target) ->
    Timeout = maps:get(timeout_ms, Target, 30_000),
    Alpn = maps:get(alpn, Target, [<<"macula">>]),
    macula_quic:connect(Host, Port, [{alpn, Alpn}], Timeout).

%%------------------------------------------------------------------
%% Notifications
%%------------------------------------------------------------------

notify(Event, Detail, #data{controlling_pid = Pid}) ->
    Pid ! {macula_peering, Event, self(), Detail},
    ok.

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
