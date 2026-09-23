%% @doc Per-peer connection state machine.
%%
%% Implements the lifecycle from `Part 4 §10' simplified for Phase 1:
%% no REFRESH phase and no RECONNECTING. The handshake is the post-quantum
%% one of plans/DESIGN_PQ_HANDSHAKE_FRAMES.md, built and checked by
%% `macula_handshake': the client sends an opener, the station a
%% challenge, the client CONNECT and the station HELLO. After HELLO each
%% side sends a status frame at every reissue of its statement, and the
%% connection closes when the peer's statement lapses (`status_expired')
%% or the peer's binding reaches its not_after (`binding_expired'). Close
%% reasons are local: the controlling process and diagnostics hear them,
%% the peer does not.
%% In pq_hybrid every control frame on the open connection carries a
%% neighbour signature for the connection and its seq in that direction.
%%
%% State graph:
%% <pre>
%%   client: connecting → handshaking → connected → draining → (terminate)
%%   server: awaiting_start → handshaking → connected → draining → (terminate)
%% </pre>
-module(macula_peering_conn).
-behaviour(gen_statem).

-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4, format_status/1]).
-export([connecting/3, awaiting_start/3, handshaking/3, connected/3, draining/3]).

-export_type([opts/0, connect_opts/0]).

-ifdef(TEST).
%% Exports for unit tests: private helpers that are otherwise unreachable.
%% `start_dial/1' is here so a test can watch one real dial decide what it
%% verifies, without standing up a whole connection to do it.
-export([
    resolve_recipient/1,
    start_dial/1
]).
%% The position of a field in the data tuple, read from the record itself, so
%% a test can inspect one by NAME. `quic_stream' is the control stream, and
%% which stream that is cannot be seen from outside the connection: a test
%% that it is not replaced mid-handshake has nowhere else to look. Naming the
%% field means a field added to the record cannot shift the test onto another
%% one. Same seam `macula_station_link:state_field_index/1' already provides.
-export([state_field_index/1]).
-endif.

-type connect_opts() :: #{
    host             := binary() | string(),
    port             := inet:port_number(),
    alpn             => [binary()],
    timeout_ms       => timeout(),
    %% The station's node_id, derived from its identity key (D5). A dial
    %% without one does not start, and the handshake closes with
    %% `{peer_identity_mismatch, #{expected, derived}}' when the station's
    %% challenge derives to another node_id.
    expected_node_id := <<_:256>>,
    %% ⚠ `verify' is refused: a dial trusts the station in one way only,
    %% its handshake signature under its ML-DSA-87 certificate's key
    %% (`macula_quic:connect/4'), and the handshake above names the peer.
    _                => _
}.

-type opts() :: #{
    role            := client | server,
    %% The node's identity key in its crypto profile: its node_id and its
    %% key as carried go into the handshake.
    identity        := macula_node_keys:node_key(),
    %% The node's `macula_statement_issuer': CONNECT material for a
    %% client, TLS material for the leaf a station presents, and the
    %% statements a connection sends as status frames.
    issuer          := pid(),
    capabilities    := non_neg_integer(),
    controlling_pid := pid(),
    target          => connect_opts(),
    quic_conn       => reference(),
    %% A station's puzzle mode for the client's derived node_id, at the
    %% difficulty of `macula_node_keys:puzzle_difficulty/0'. Required on
    %% the station role.
    puzzle          => #{mode := macula_handshake:puzzle_mode()},
    %% Wall-clock milliseconds, for tests.
    clock           => fun(() -> integer()),
    %% Optional pid notified once when the worker completes the
    %% handshake and transitions to `connected'. Sent
    %% as `{macula_peering, handshake_complete, self(), PeerNodeId}'
    %% where `PeerNodeId' is the peer's node_id, derived from the
    %% identity key the handshake verified. Used by accept-side listeners
    %% that (a) cap concurrent *handshaking* workers and need to
    %% release a slot the moment a worker is verified-and-connected,
    %% and (b) dedupe duplicate dials from the same peer identity by
    %% closing prior workers for the same `PeerNodeId'. Distinct from
    %% `controlling_pid', which receives the peer-node-id-bearing
    %% `connected' / `frame' / `disconnected' stream.
    accept_owner    => pid(),
    %% Optional pid that receives DHT-class frames (`ping', `pong',
    %% `find_node', `nodes', `find_value', `value', `store',
    %% `store_ack') directly, bypassing
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
    %%
    %% Prefer a REGISTERED NAME over a raw pid. A name is re-resolved
    %% on every frame, so a recipient crash-restart is transparent: the
    %% supervisor re-registers the name to the new pid and the next
    %% frame lands there. A raw pid is captured once for the life of
    %% this connection and cannot follow a restart.
    dht_recipient   => pid() | atom(),
    %% Optional pid that receives pubsub-class frames (`subscribe',
    %% `unsubscribe', `publish', `event') directly, bypassing
    %% `controlling_pid'. Sent as
    %%     `{macula_peering, pubsub_frame, self(), PeerNodeId, Frame}'.
    %% Mirrors `dht_recipient' for the pubsub category. After DHT was
    %% bypassed (4.4.3), inbound EVENT became the dominant work on
    %% station observers — multi-publisher cases fire bursts of
    %% a signature check per event, ML-DSA-87 since 12.0.0 and slower
    %% than the Ed25519 this was written for, backing up the gen_server
    %% mailbox that handles handler dispatch and ADVERTISE / SUBSCRIBE
    %% propagation. Stations on macula >= 4.4.4 set this to a dedicated
    %% pubsub frame dispatcher.
    %%
    %% As with `dht_recipient', prefer a registered name — see the
    %% note there. Stations pass
    %% `macula_station_route_pubsub_frames' so that a recipient
    %% restart does not silently strand every pre-existing peering
    %% connection on a dead pid.
    pubsub_recipient => pid() | atom(),
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
    timing_enabled  => boolean(),
    %% App-level liveness probe: this conn sends a `_macula.ping' CALL
    %% on the all-zero realm to the peer every `liveness_interval_ms'
    %% and accepts ANY verified reply — a RESULT, a provider ERROR or
    %% the peer's own relay ERROR — naming the probe's request_id as
    %% proof of life. `liveness_max_misses' unanswered probes in a row
    %% close the connection as `peer_liveness_lost'. Unset
    %% `liveness_interval_ms' (the default) disables the probe: a link
    %% that already probes at the application layer (the SDK pool
    %% link's own `_macula.ping') opts out; stations opt in on both
    %% their listener-accepted and outbound conns, so a peer VM that
    %% dies outright (the dead-but-healthy class — keep-alives defeat
    %% the QUIC idle timer for its full 300s) is reaped within
    %% interval × max_misses. A CALL is understood by every released
    %% peer, so a mixed-version fleet answers the probe safely.
    liveness_interval_ms => pos_integer(),
    liveness_max_misses  => pos_integer()
}.

-record(data, {
    role             :: client | server,
    identity         :: macula_node_keys:node_key(),
    profile          :: macula_crypto_profile:profile(),
    node_id          :: <<_:256>>,
    issuer           :: pid(),
    capabilities     :: non_neg_integer(),
    controlling_pid  :: pid(),
    accept_owner     :: undefined | pid(),
    dht_recipient    :: undefined | pid() | atom(),
    pubsub_recipient :: undefined | pid() | atom(),
    timing_enabled   :: boolean(),
    target           :: undefined | connect_opts(),
    %% The station's node_id a client dialed, from its target.
    expected_node_id :: undefined | <<_:256>>,
    %% A station's puzzle mode.
    puzzle           :: undefined | macula_handshake:puzzle_mode(),
    clock            :: fun(() -> integer()),
    quic_conn        :: undefined | reference(),
    quic_stream      :: undefined | reference(),
    %% While `connecting' (client role): the dial in progress, the tag
    %% on its result message, and the monitor on `controlling_pid'.
    dial             :: undefined | macula_quic:dial(),
    dial_tag         :: undefined | reference(),
    owner_mon        :: undefined | reference(),
    %% During the handshake: the frame this side accepts next, the leaf
    %% and challenge bytes the checks run over, and a station's refusal
    %% while its refused HELLO reaches the client.
    expect           :: undefined | opener | challenge | connect | hello,
    leaf             :: undefined | binary(),
    challenge        :: undefined | binary(),
    refusal          :: undefined | term(),
    %% The binding this side presented, whose statements it sends as
    %% status frames.
    own_binding_hash :: undefined | <<_:384>>,
    %% The connection hash neighbour signatures carry (the SHA-384 of the
    %% challenge frame's bytes), and the seq of the next neighbour-signed
    %% frame in each direction.
    connection       :: undefined | <<_:384>>,
    sent_seq = 0     :: non_neg_integer(),
    received_seq = 0 :: non_neg_integer(),
    %% What the handshake verified of the peer.
    peer              :: undefined | macula_handshake:station() | macula_handshake:client(),
    peer_node_id      :: undefined | <<_:256>>,
    %% Counterpart's capabilities bitmask as carried in CONNECT (station
    %% side) or HELLO (client side). Stays `undefined' until the
    %% handshake completes.
    peer_capabilities :: undefined | non_neg_integer(),
    buf               :: binary(),
    %% Dedicated stream opens waiting for the peer, by the tag on their
    %% result message: the process that asked, its reference, and the
    %% open's handle.
    openings = #{}    :: #{reference() =>
                               {pid(), reference(), macula_quic:stream_opening()}},
    %% Refused objects reported on this connection, by kind, and how
    %% many of them were charged (D28).
    refusals = #{}    :: #{atom() => pos_integer()},
    charged = 0       :: non_neg_integer(),
    %% App-level liveness probe state (see the opts doc). The
    %% outstanding probe's request_id + the request as a verifier
    %% reads it, and the consecutive unanswered-probe count.
    liveness_interval_ms :: undefined | pos_integer(),
    liveness_max_misses  = 2 :: pos_integer(),
    liveness_outstanding :: undefined | {<<_:128>>, macula_frame:verified_request()},
    liveness_misses      = 0 :: non_neg_integer()
}).

-define(DRAIN_TIMEOUT_MS, 5_000).
%% How long an open of a dedicated stream may wait for the peer to allow
%% another stream before it fails with `timeout'.
-define(DEDICATED_STREAM_OPEN_TIMEOUT_MS, 10_000).
%% Added to a dial's own timeout before the connecting state gives up on
%% a dial that has not reported.
-define(DIAL_DEADLINE_GRACE_MS, 1_000).
%% Maximum time the `handshaking' state may take before the worker
%% gives up. CONNECT/HELLO is sub-second on a healthy peer; 30s is
%% generous. Drains workers stuck because the peer speaks the wrong
%% protocol (e.g. V1 frames against a V2 station) — without this the
%% sup accumulates stuck workers indefinitely. See PLAN_FLYING_RESTART.
-define(HANDSHAKE_TIMEOUT_MS, 30_000).

%% App-level liveness probe: the procedure the peer answers (or
%% answers `unknown_next_peer' for — either signed reply is proof of
%% life) and the realm it is probed on, mirroring the SDK pool link's
%% own probe (`macula_station_link').
-define(LIVENESS_PROCEDURE, <<"_macula.ping">>).
-define(LIVENESS_REALM, <<0:256>>).
%% The largest frame the handshake reads, 64 KiB. The post-quantum opener,
%% challenge, CONNECT and HELLO stay under 20 KB. A length header above it
%% ends the connection as soon as the header arrives, before the frame's
%% bytes are buffered or decoded.
-define(HANDSHAKE_FRAME_BYTES, 64 * 1024).
%% A peer's status statement is accepted up to 5 minutes past its
%% expiry (D22), so the connection waits that long before it closes.
-define(STATUS_GRACE_MS, 5 * 60000).
%% How long a station that refused CONNECT waits for its HELLO to reach
%% the client before it closes.
-define(REFUSAL_LINGER_MS, 2_000).

%%------------------------------------------------------------------
%% Lifecycle
%%------------------------------------------------------------------

-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_statem:start_link(?MODULE, Opts, []).

callback_mode() ->
    [state_functions, state_enter].

init(#{role := Role, identity := Identity, issuer := Issuer, controlling_pid := Pid} = Opts)
  when (Role =:= client orelse Role =:= server), is_pid(Issuer), is_pid(Pid) ->
    started(identity_key(Identity), role_ready(Role, Opts), Opts).

%% A connection starts only with an identity key; a dial only with the
%% node_id it expects, and a station only with its puzzle mode.
identity_key(#{purpose := identity, profile := Profile} = Identity) -> {ok, Identity, Profile};
identity_key(_NotAnIdentityKey) -> {error, {identity, not_an_identity_key}}.

role_ready(client, #{target := #{verify := _}}) -> {error, {target, {verify, one_verification_mode}}};
role_ready(client, #{target := #{expected_node_id := <<_:256>>}}) -> ok;
role_ready(client, _Opts) -> {error, {target, expected_node_id_required}};
role_ready(server, #{puzzle := #{mode := Mode}}) when Mode =:= off; Mode =:= log_only; Mode =:= enforce -> ok;
role_ready(server, _Opts) -> {error, {puzzle, mode_required}}.

started({ok, Identity, Profile}, ok, #{role := Role} = Opts) ->
    {ok, NodeId} = macula_node_keys:node_id(Identity),
    Data = #data{
        role             = Role,
        identity         = Identity,
        profile          = Profile,
        node_id          = NodeId,
        issuer           = maps:get(issuer, Opts),
        capabilities     = maps:get(capabilities, Opts, 0),
        controlling_pid  = maps:get(controlling_pid, Opts),
        accept_owner     = maps:get(accept_owner, Opts, undefined),
        timing_enabled   = maps:get(timing_enabled, Opts, false),
        dht_recipient    = maps:get(dht_recipient, Opts, undefined),
        pubsub_recipient = maps:get(pubsub_recipient, Opts, undefined),
        target           = maps:get(target, Opts, undefined),
        expected_node_id = maps:get(expected_node_id, maps:get(target, Opts, #{}), undefined),
        puzzle           = maps:get(mode, maps:get(puzzle, Opts, #{}), undefined),
        clock            = maps:get(clock, Opts, fun wall_clock_ms/0),
        quic_conn        = maps:get(quic_conn, Opts, undefined),
        buf              = <<>>,
        liveness_interval_ms = maps:get(liveness_interval_ms, Opts, undefined),
        liveness_max_misses  = maps:get(liveness_max_misses, Opts, 2)
    },
    {ok, initial_state(Role), Data};
started({error, Refusal}, _RoleReady, _Opts) ->
    {stop, Refusal};
started(_Identity, {error, Refusal}, _Opts) ->
    {stop, Refusal}.

initial_state(client) -> connecting;
initial_state(server) -> awaiting_start.

terminate(_Reason, _State, Data) ->
    ok = fail_waiting_opens(Data),
    _ = close_quic(Data),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% Status output and crash reports show this process's keys with their private halves redacted.
format_status(Status) ->
    macula_node_keys:redacted(Status).

%%------------------------------------------------------------------
%% State: connecting (client only)
%%------------------------------------------------------------------

connecting(enter, _Old, Data) ->
    self() ! attempt_connect,
    {keep_state, Data};
%% The dial runs on the QUIC runtime, so this worker stays free for close,
%% reject and its controlling process's exit while the dial waits.
connecting(info, attempt_connect,
           #data{target = Target, controlling_pid = Owner} = Data) ->
    dial_started(start_dial(Target),
                 Data#data{owner_mon = erlang:monitor(process, Owner)});
connecting(info, {quic, connected, Tag, Conn}, #data{dial_tag = Tag} = Data) ->
    after_connect({ok, Conn}, dial_ended(Data));
connecting(info, {quic, connect_failed, Tag, Reason},
           #data{dial_tag = Tag} = Data) ->
    after_connect({error, Reason}, dial_ended(Data));
connecting(info, {'DOWN', Mon, process, _Owner, _Reason},
           #data{owner_mon = Mon} = Data) ->
    {stop, normal, cancel_dial(Data)};
connecting(state_timeout, dial_deadline, Data) ->
    after_connect({error, <<"connection_timeout">>}, cancel_dial(Data));
connecting(cast, {close, Reason}, Data) ->
    notify(disconnected, Reason, Data),
    {stop, normal, cancel_dial(Data)};
%% `reject/2' behaves exactly like `close/2' here: nothing has been
%% established yet, so there is no legitimate session to distinguish
%% "graceful" from "immediate" for.
connecting(cast, {reject, Reason}, Data) ->
    notify(disconnected, Reason, Data),
    {stop, normal, cancel_dial(Data)};
connecting(cast, {open_dedicated_stream, Owner, Ref}, _Data) ->
    refuse_dedicated_open(Owner, Ref);
connecting(EventType, Event, Data) ->
    other_event(EventType, Event, connecting, Data).

dial_started({ok, Dial}, #data{target = Target} = Data) ->
    {keep_state, Data#data{dial = Dial, dial_tag = macula_quic:dial_tag(Dial)},
     [{state_timeout, dial_timeout(Target) + ?DIAL_DEADLINE_GRACE_MS,
       dial_deadline}]};
dial_started(Error, Data) ->
    after_connect(Error, cancel_dial(Data)).

%% The dial reported, or is being given up: forget it and stop watching
%% the controlling process.
dial_ended(Data) ->
    stop_watching_owner(Data#data{dial = undefined, dial_tag = undefined}).

cancel_dial(#data{dial = undefined} = Data) ->
    stop_watching_owner(Data);
cancel_dial(#data{dial = Dial} = Data) ->
    ok = macula_quic:cancel_connect(Dial),
    dial_ended(Data).

stop_watching_owner(#data{owner_mon = undefined} = Data) ->
    Data;
stop_watching_owner(#data{owner_mon = Mon} = Data) ->
    true = erlang:demonitor(Mon, [flush]),
    Data#data{owner_mon = undefined}.

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
%% See `connecting/3''s matching clause — same reasoning.
awaiting_start(cast, {reject, Reason}, Data) ->
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
awaiting_start(cast, {open_dedicated_stream, Owner, Ref}, _Data) ->
    refuse_dedicated_open(Owner, Ref);
awaiting_start(EventType, Event, Data) ->
    other_event(EventType, Event, awaiting_start, Data).

%%------------------------------------------------------------------
%% State: handshaking
%%------------------------------------------------------------------

handshaking(enter, _Old, #data{role = client, quic_conn = Conn} = Data) ->
    %% Start the accept loop on this side too, not just server's —
    %% `?nif_async_accept_stream' is what turns a peer's later
    %% `open_bi()' into a `{quic, new_stream, ...}' event HERE. QUIC
    %% streams can be opened by either endpoint after the handshake
    %% regardless of who dialed; a dedicated stream opened BY THE
    %% SERVER side toward a peer that only ever dialed out (the
    %% common case: a daemon that dials a station, then the station
    %% relays a streaming session back toward it) would sit
    %% unaccepted forever without this. Before dedicated streams
    %% existed this was fine — the client role never needed to accept
    %% anything, since it always opened the one stream it used itself.
    ok = macula_quic:async_accept_stream(Conn),
    on_handshake_enter_client(macula_quic:open_stream(Conn), Data);
handshaking(enter, _Old, #data{role = server, quic_conn = Conn} = Data) ->
    ok = macula_quic:async_accept_stream(Conn),
    {keep_state, Data#data{expect = opener}, [handshake_state_timeout()]};
%% THE CONTROL STREAM IS THE ONE THE CLIENT OPENED, and this is the only
%% clause that may ever set it. Both halves of the guard are load-bearing and
%% neither alone is enough: the role check alone would still let a SECOND
%% inbound stream replace the first on a server, and the `undefined' check
%% alone would still let a client adopt one if a later change left
%% `quic_stream' unset on entry (macula#23).
handshaking(info, {quic, new_stream, Stream, _Info},
            #data{role = server, quic_stream = undefined} = Data) ->
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
%% ANY OTHER inbound stream during the handshake is closed, not adopted: a
%% client's, which already holds the stream it opened itself, and a second
%% one on a server. Before this clause existed the adopting one above matched
%% every inbound stream, so the peer's stream became the control stream, the
%% peer's bytes were read as handshake frames, the frame check refused them,
%% and THE CONNECTION DIED — a peer could end any handshake by opening a
%% stream and writing one byte into it.
%%
%% Closed rather than merely ignored, because a stream nobody will ever read
%% is one the peer may keep writing into. Ownership is taken first so the
%% close is ours to make and no `{quic, ...}' event for it is left routing to
%% the listener.
%%
%% ⚠ The client accepts inbound streams during the handshake ON PURPOSE (see
%% `handshaking(enter, ...)'): a dedicated stream opened by the far side must
%% not sit unaccepted. Accepting one is right; adopting it as the CONTROL
%% stream was the defect. A peer wanting a dedicated stream opens it once the
%% handshake is done, where `connected(info, {quic, new_stream, ...})' hands
%% it to the controlling process.
handshaking(info, {quic, new_stream, Stream, _Info}, Data) ->
    _ = macula_quic:controlling_process(Stream, self()),
    _ = macula_quic:close_stream(Stream),
    macula_diagnostics:event(<<"_macula.peering.handshake_stream_refused">>,
                             #{role => Data#data.role, conn => self()}),
    {keep_state, Data};
%% A station that refused CONNECT reads nothing more while its refused
%% HELLO reaches the client, and closes with its refusal once the client
%% closes or the linger ends.
handshaking(info, {quic, Bin, Stream, _Flags}, #data{quic_stream = Stream, refusal = Refusal} = Data)
  when is_binary(Bin), Refusal =/= undefined ->
    {keep_state, Data};
handshaking(info, {quic, Closed, Stream, _Detail}, #data{quic_stream = Stream, refusal = Refusal} = Data)
  when (Closed =:= stream_closed orelse Closed =:= peer_send_shutdown), Refusal =/= undefined ->
    closed(Refusal, Data);
handshaking(state_timeout, refusal_sent, #data{refusal = Refusal} = Data) ->
    closed(Refusal, Data);
handshaking(info, {quic, Bin, Stream, _Flags},
            #data{quic_stream = Stream, buf = Buf} = Data) when is_binary(Bin) ->
    consume_handshake(<<Buf/binary, Bin/binary>>, Data);
%% `{quic, closed, Conn, Detail}' is NEVER actually sent by the NIF —
%% `native/macula_quic/src/atoms.rs' defines the atom but nothing calls
%% `send_event' with it (verified directly in source: the connection's
%% own `closed' field is a purely-local `AtomicBool', never surfaced as
%% an event). What the recv loop ACTUALLY sends when the control
%% stream dies for ANY reason (peer reset, connection loss, timeout —
%% every non-Reset read error, collapsed by `stream.rs''s own
%% "simplified for now" catch-all) is `{quic, stream_closed, Stream,
%% Detail}'; a clean peer-initiated half-close on the same stream comes
%% as `{quic, peer_send_shutdown, Stream, none}'. A clause that can
%% never match is not a safety net — it is a connection that, if its
%% transport dies during handshake, sits in this state forever: no
%% `disconnected' notification, no termination, indistinguishable from
%% healthy to `controlling_pid'/`accept_owner' until some unrelated
%% higher-layer liveness probe eventually notices. This is the same
%% failure class `project_station_dead_but_healthy_milan' documented at
%% the station-transport level; the fix here is at the connection
%% state-machine level, one layer down.
handshaking(info, {quic, stream_closed, Stream, Detail},
            #data{quic_stream = Stream} = Data) ->
    notify(disconnected, {closed_during_handshake, Detail}, Data),
    {stop, normal, Data};
handshaking(info, {quic, peer_send_shutdown, Stream, _Detail},
            #data{quic_stream = Stream} = Data) ->
    notify(disconnected, closed_during_handshake, Data),
    {stop, normal, Data};
%% A failed CONNECT or HELLO write leaves the control stream as unusable
%% as a closed one.
handshaking(info, {quic, send_failed, Stream, Reason},
            #data{quic_stream = Stream} = Data) ->
    notify(disconnected, {closed_during_handshake, {send_failed, Reason}}, Data),
    {stop, normal, Data};
handshaking(cast, {close, Reason}, Data) ->
    notify(disconnected, Reason, Data),
    {stop, normal, Data};
%% See `connecting/3''s matching clause — same reasoning. This is the
%% state `macula_station_listener:reject_handshake/3' expects a
%% puzzle-invalid peer to still be in, though the notify/transition
%% race with `transition_to_connected/1' (see `connected/3''s own
%% `{reject, Reason}' clause) means it can just as easily already be
%% `connected' by the time the reject cast arrives — both are covered.
handshaking(cast, {reject, Reason}, Data) ->
    notify(disconnected, Reason, Data),
    {stop, normal, Data};
%% No CONNECT/HELLO completed within the timeout window: the peer sent
%% no complete frame, or only frames other than CONNECT and HELLO. A
%% peer whose bytes do not decode as frames (e.g. V1 frames at a V2
%% station) is disconnected as malformed before this. Surface a
%% structured diagnostic and exit so the sup does not retain the worker
%% forever.
handshaking(state_timeout, handshake_timeout,
            #data{role = Role, buf = Buf, quic_stream = Stream} = Data) ->
    macula_diagnostics:bounded_event(info, <<"_macula.peering.handshake_timeout">>, #{
        role         => Role,
        buf_size     => byte_size(Buf),
        has_stream   => Stream =/= undefined,
        timeout_ms   => ?HANDSHAKE_TIMEOUT_MS
    }),
    notify(disconnected, handshake_timeout, Data),
    {stop, normal, Data};
handshaking(cast, {open_dedicated_stream, Owner, Ref}, _Data) ->
    refuse_dedicated_open(Owner, Ref);
handshaking(EventType, Event, Data) ->
    other_event(EventType, Event, handshaking, Data).

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
    handshake_setopt(macula_quic:setopt(Stream, active, true), Stream, Data);
on_handshake_enter_client(Err, Data) ->
    notify(disconnected, {open_stream_failed, Err}, Data),
    {stop, normal, Data}.

handshake_setopt(ok, Stream, Data) ->
    handshake_send(send_handshake_bytes(Stream, macula_handshake:opener()), Stream, Data);
handshake_setopt({error, _} = SetoptErr, _Stream, Data) ->
    notify(disconnected, {setopt_failed, SetoptErr}, Data),
    {stop, normal, Data}.

handshake_send(ok, Stream, Data) ->
    {keep_state, Data#data{quic_stream = Stream, expect = challenge}, [handshake_state_timeout()]};
handshake_send({error, _} = SendErr, _Stream, Data) ->
    notify(disconnected, {send_opener_failed, SendErr}, Data),
    {stop, normal, Data}.

%% Frames come off the control stream by their length prefix, as the
%% bytes the checks hash, and each one is checked in order against the
%% one frame this side accepts next. Until HELLO, a frame that arrives
%% before its turn closes the handshake with `unexpected_frame'. A length
%% header above the handshake's 64 KiB cap closes it as `malformed_frame'
%% from the header alone, before the frame's bytes are buffered.
consume_handshake(Buf, Data) ->
    handshake_frames(macula_frame:parse_stream_bytes(Buf, ?HANDSHAKE_FRAME_BYTES), Data).

handshake_frames({ok, Frames, Tail}, Data) ->
    handshake_step(Frames, Data#data{buf = Tail});
handshake_frames({error, frame_too_large}, Data) ->
    closed(malformed_frame, Data).

handshake_step([], Data) ->
    {keep_state, Data};
handshake_step([Bytes | Rest], #data{expect = opener} = Data) ->
    opened(macula_handshake:read_opener(Bytes), Rest, Data);
handshake_step([Bytes | Rest], #data{expect = challenge} = Data) ->
    challenged(Rest, Bytes, Data);
handshake_step([Bytes | Rest], #data{expect = connect} = Data) ->
    connect_checked(Rest, Bytes, Data);
handshake_step([Bytes | Rest], #data{expect = hello} = Data) ->
    hello_read(macula_handshake:read_hello(Bytes), Rest, Data).

%% Station: the opener, answered with a challenge over the leaf this
%% connection presented.
opened(ok, [], #data{quic_conn = Conn} = Data) ->
    challenge_sent(station_material(macula_quic:presented_leaf(Conn), Data), Data);
opened(ok, _Early, Data) ->
    closed(unexpected_frame, Data);
opened({error, Reason}, _Rest, Data) ->
    closed(Reason, Data).

station_material({ok, Leaf}, #data{issuer = Issuer} = Data) ->
    tls_material(macula_statement_issuer:tls_material(Issuer, crypto:hash(sha384, Leaf)), Leaf, Data);
station_material({error, _} = NoLeaf, _Data) ->
    NoLeaf.

tls_material({ok, #{tls_binding := Binding, tls_status := Status}}, Leaf,
             #data{identity = Identity, profile = Profile}) ->
    Material = #{profile => Profile, identity_key => macula_node_keys:public_key(Identity), tls_binding => Binding,
                 tls_status => Status},
    {ok, Leaf, Binding, macula_handshake:challenge(Material)};
tls_material({error, _} = Unknown, _Leaf, _Data) ->
    Unknown.

challenge_sent({ok, Leaf, Binding, Challenge}, #data{quic_stream = Stream} = Data) ->
    Sent = Data#data{leaf = Leaf, challenge = Challenge, own_binding_hash = binding_hash(Binding),
                     connection = crypto:hash(sha384, Challenge), expect = connect},
    handshake_written(send_handshake_bytes(Stream, Challenge), Sent);
challenge_sent({error, Reason}, Data) ->
    closed(Reason, Data).

%% Client: the challenge, answered with CONNECT once every check on it
%% passes against the leaf this dial received.
challenged([], Challenge, #data{quic_conn = Conn} = Data) ->
    connect_sent(client_session(macula_quic:peer_leaf(Conn), Data), Challenge, Data);
challenged(_Early, _Challenge, Data) ->
    closed(unexpected_frame, Data).

client_session({ok, Leaf}, #data{issuer = Issuer, identity = Identity, profile = Profile,
                                 expected_node_id = Expected, capabilities = Capabilities} = Data) ->
    #{connect_key := Key, connect_binding := Binding, connect_status := Status} =
        macula_statement_issuer:connect_material(Issuer),
    {ok, #{profile => Profile, expected_node_id => Expected, leaf => Leaf,
           identity_key => macula_node_keys:public_key(Identity), connect_key => Key, connect_binding => Binding,
           connect_status => Status, capabilities => Capabilities, now => now_ms(Data)}};
client_session({error, _} = NoLeaf, _Data) ->
    NoLeaf.

connect_sent({ok, #{connect_binding := Binding} = Session}, Challenge, Data) ->
    answered(macula_handshake:answer_challenge(Challenge, Session), binding_hash(Binding),
             Data#data{connection = crypto:hash(sha384, Challenge)});
connect_sent({error, Reason}, _Challenge, Data) ->
    closed(Reason, Data).

answered({ok, Connect, Station}, OwnHash, #data{quic_stream = Stream} = Data) ->
    Sent = with_peer(Station, Data#data{own_binding_hash = OwnHash, expect = hello}),
    handshake_written(send_handshake_bytes(Stream, Connect), Sent);
answered({error, Reason}, _OwnHash, Data) ->
    closed(Reason, Data).

%% Client: HELLO. Frames that follow it in the same read are the
%% station's first frames on the open connection.
hello_read({ok, #{capabilities := Capabilities}}, Rest, Data) ->
    after_hello(transition_to_connected(Data#data{peer_capabilities = Capabilities}), Rest);
hello_read({error, Reason}, _Rest, Data) ->
    closed(Reason, Data).

after_hello({next_state, connected, #data{buf = Buf} = Data, Actions}, Rest) ->
    Early = iolist_to_binary([macula_frame:encode_bytes(Bytes) || Bytes <- Rest]),
    {next_state, connected, Data#data{buf = <<Early/binary, Buf/binary>>},
     Actions ++ [{next_event, internal, drain_buffer}]};
after_hello(Closed, _Rest) ->
    Closed.

%% Station: CONNECT, answered with HELLO. A refused CONNECT gets a HELLO
%% with one coarse refusal code, and the station closes with its own
%% reason once that HELLO has had time to arrive.
connect_checked([], Connect, #data{challenge = Challenge, leaf = Leaf, profile = Profile, puzzle = Mode,
                                   capabilities = Capabilities} = Data) ->
    Session = #{profile => Profile, challenge => Challenge, leaf => Leaf, capabilities => Capabilities,
                now => now_ms(Data), puzzle => #{difficulty => macula_node_keys:puzzle_difficulty(), mode => Mode}},
    connect_verdict(macula_handshake:accept_connect(Connect, Session), Data);
connect_checked(_Early, _Connect, Data) ->
    closed(unexpected_frame, Data).

connect_verdict({accepted, #{capabilities := Capabilities} = Client, Hello}, #data{quic_stream = Stream} = Data) ->
    Accepted = puzzle_reported(Client, with_peer(Client, Data#data{peer_capabilities = Capabilities})),
    hello_sent(send_handshake_bytes(Stream, Hello), Accepted);
connect_verdict({refused, Reason, Hello}, #data{quic_stream = Stream} = Data) ->
    _ = send_handshake_bytes(Stream, Hello),
    {keep_state, Data#data{refusal = Reason, expect = undefined},
     [{state_timeout, ?REFUSAL_LINGER_MS, refusal_sent}]}.

hello_sent(ok, Data) ->
    transition_to_connected(Data);
hello_sent({error, _} = SendErr, Data) ->
    notify(disconnected, {send_hello_failed, SendErr}, Data),
    {stop, normal, Data}.

%% Under log_only an unsolved puzzle is accepted and reported, at most
%% once per 10 seconds on this node with a count of the rest.
puzzle_reported(#{puzzle := unsolved, node_id := NodeId}, #data{puzzle = log_only} = Data) ->
    ok = macula_diagnostics:bounded_event(warning, <<"_macula.peering.puzzle_unsolved">>,
                                          #{node_id => binary:encode_hex(NodeId, lowercase)}),
    Data;
puzzle_reported(_Client, Data) ->
    Data.

handshake_written(ok, Data) ->
    {keep_state, Data};
handshake_written({error, _} = SendErr, Data) ->
    notify(disconnected, {handshake_send_failed, SendErr}, Data),
    {stop, normal, Data}.

with_peer(#{node_id := NodeId} = Peer, Data) ->
    Data#data{peer = Peer, peer_node_id = NodeId}.

binding_hash(#{tbs := Tbs}) ->
    crypto:hash(sha384, Tbs).

%% A handshake or an open connection that ends for a local reason: the
%% controlling process and diagnostics hear it, the peer does not. The
%% diagnostics line is bounded per node, with a count of the rest.
closed(Reason, #data{role = Role, quic_conn = Conn} = Data) ->
    ok = macula_diagnostics:bounded_event(info, <<"_macula.peering.closed">>,
                                          peer_named(Conn, #{role => Role, reason => Reason})),
    notify(disconnected, Reason, Data),
    {stop, normal, Data}.

%% The address this connection talks to, as `peer'. A connection closed at its
%% first frame, as a version refusal is, has no peer identity yet (that arrives
%% in CONNECT), but its QUIC connection knows the remote address, and without
%% it a station refusing every peer logs the same line naming nobody (macula#24).
peer_named(Conn, Props) when is_reference(Conn) ->
    with_address(macula_quic:peername(Conn), Props);
peer_named(_NoConn, Props) ->
    Props.

with_address({ok, {Host, Port}}, Props) ->
    Props#{peer => iolist_to_binary(address_text(Host, Port))};
with_address({error, _}, Props) ->
    Props.

%% An IPv6 host is bracketed, so the port stays readable after its colons.
address_text(Host, Port) when is_binary(Host) ->
    bracketed(binary:match(Host, <<":">>) =/= nomatch, Host, integer_to_list(Port)).

bracketed(true, Host, Port) -> ["[", Host, "]:", Port];
bracketed(false, Host, Port) -> [Host, ":", Port].

%% After HELLO the peer hears this side's statements as status frames,
%% and the connection ends when the peer's statement lapses or its
%% binding reaches its not_after.
transition_to_connected(#data{issuer = Issuer, own_binding_hash = Hash} = Data) ->
    subscribed(macula_statement_issuer:subscribe(Issuer, Hash), Data).

%% The issuer forgets a binding only once its not_after has passed.
subscribed(ok, #data{peer = Peer} = Data) ->
    notify(connected, Data#data.peer_node_id, Data),
    notify_handshake_complete(Data),
    {next_state, connected, Data#data{expect = undefined, leaf = undefined, challenge = undefined},
     lifecycle_timers(Peer, Data)};
subscribed({error, unknown_binding}, Data) ->
    closed(binding_expired, Data).

lifecycle_timers(#{status_expires_at := StatusExpiresAt, binding_not_after := NotAfter}, Data) ->
    [status_timer(StatusExpiresAt, Data),
     {{timeout, binding_expired}, max(0, NotAfter - now_ms(Data)), binding_expired}].

status_timer(ExpiresAt, Data) ->
    {{timeout, status_expired}, max(0, ExpiresAt + ?STATUS_GRACE_MS - now_ms(Data)), status_expired}.

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
    {keep_state, Data, liveness_tick_actions(Data)};
connected(state_timeout, liveness_tick, #data{liveness_interval_ms = undefined} = Data) ->
    {keep_state, Data};
connected(state_timeout, liveness_tick, #data{liveness_outstanding = undefined} = Data) ->
    {keep_state, send_liveness_probe(Data), [liveness_tick_action(Data)]};
connected(state_timeout, liveness_tick,
          #data{liveness_outstanding = _Probe, liveness_misses = Misses,
                liveness_max_misses = Max} = Data) ->
    case Misses + 1 >= Max of
        true ->
            closed(peer_liveness_lost, Data#data{liveness_misses = Misses + 1});
        false ->
            {keep_state, send_liveness_probe(Data#data{liveness_misses = Misses + 1}),
             [liveness_tick_action(Data)]}
    end;
connected(info, {quic, Bin, Stream, _Flags},
          #data{quic_stream = Stream, buf = Buf} = Data) when is_binary(Bin) ->
    open_frames(macula_frame:parse_stream_bytes(<<Buf/binary, Bin/binary>>), Data);
connected(internal, drain_buffer, #data{buf = Buf} = Data) ->
    open_frames(macula_frame:parse_stream_bytes(Buf), Data);
%% Peer opened a new stream on this connection, outside the control
%% stream — a dedicated stream for a streaming RPC session or a
%% content transfer (see PLAN_PER_STREAM_QUIC_ISOLATION.md). This
%% connection process is not the intended long-term owner: it exists
%% only to take custody long enough to hand the stream to
%% `controlling_pid' (`macula_station_link' on the SDK side,
%% `macula_station_peer_observer' on the station side), which reads
%% the stream's own first frame to learn what it's for. Mirrors the
%% ownership-transfer pattern already used for the handshake stream
%% in `handshaking/3', just handing off to a third party instead of
%% keeping it for `self()'.
%%
%% ⚠ NOTIFY BEFORE ENABLING ACTIVE MODE — not the other way around.
%% Every new stream resource is created passive
%% (`StreamResource::active: AtomicBool::new(false)'; its recv loop
%% literally blocks on a `Notify' until `setopt(active, true)' wakes
%% it — see native/macula_quic/src/stream.rs), so nothing can be
%% delivered to `Pid' before that NIF call runs. `Pid ! Msg' is a
%% direct send from THIS process and lands in `Pid''s mailbox
%% synchronously; `setopt/2' triggers an independent async Rust task
%% that starts delivering the moment it runs. Enabling active mode
%% BEFORE sending the notification (the order this used to be in) left
%% a real window: the peer sends its first frame the instant IT
%% finishes opening the stream, so on a fast/local path the NIF's recv
%% task can start delivering `{quic, Bin, Stream, Flags}' to `Pid'
%% before `Pid' has any idea `Stream' exists — every consumer's
%% dedicated-stream handling keys off already knowing the stream
%% (`stream_bufs' / `content_stream_bufs', seeded only by THIS
%% notification), so that data had nowhere to land and was silently
%% dropped by whichever catch-all `Pid' happened to have. Reordering
%% closes the window structurally: passive mode guarantees zero
%% delivery until `setopt' below runs, and by then the notification is
%% already sitting in `Pid''s mailbox. Found via
%% macula-station's `stream_relays_through_outbound_dialled_hop' CT
%% case, ~40% flaky on localhost (near-zero RTT maximizes the race);
%% same root cause as the live `stream_order_and_eof' timeout on the
%% production fleet.
connected(info, {quic, new_stream, Stream, _Info},
          #data{controlling_pid = Pid} = Data) ->
    _ = macula_quic:controlling_process(Stream, Pid),
    Pid ! {macula_peering, new_dedicated_stream, self(), Stream},
    ok = macula_quic:setopt(Stream, active, true),
    {keep_state, Data};
%% Open a dedicated QUIC stream for `Owner' without waiting on the peer:
%% the open runs on the QUIC runtime, so this connection keeps serving
%% while the peer allows no further stream. The opened stream goes
%% straight to `Owner' (no custody window, unlike the inbound case
%% above) with `{macula_peering, dedicated_stream_opened, Ref, Stream}';
%% a failed or expired open is reported as
%% `{macula_peering, dedicated_stream_open_failed, Ref, Reason}'. `Owner'
%% drives the stream directly via `macula_quic:send/2' /
%% `macula_peering:send_on_stream/3' and receives its
%% `{quic, Bin, Stream, Flags}' events straight into its own mailbox.
connected(cast, {open_dedicated_stream, Owner, Ref},
          #data{quic_conn = Conn} = Data) ->
    dedicated_open_started(macula_quic:async_open_stream(Conn), Owner, Ref, Data);
connected(info, {quic, stream_opened, Tag, Stream},
          #data{openings = Openings} = Data) when is_map_key(Tag, Openings) ->
    dedicated_stream_opened(Tag, Stream, Data);
connected(info, {quic, stream_open_failed, Tag, Reason},
          #data{openings = Openings} = Data) when is_map_key(Tag, Openings) ->
    dedicated_open_failed(Tag, Reason, Data);
connected({timeout, {dedicated_stream_open, Tag}}, expired, Data) ->
    dedicated_open_expired(Tag, Data);
%% `{quic, closed, Conn, Detail}' is NEVER actually sent — see
%% `handshaking/3''s matching comment for the full explanation. The
%% events that actually arrive when the control stream dies are
%% `stream_closed'/`peer_send_shutdown', handled below; a connection
%% stuck here would otherwise sit `connected' forever after its
%% transport genuinely died.
connected(info, {quic, stream_closed, Stream, Detail},
          #data{quic_stream = Stream} = Data) ->
    notify(disconnected, {peer_closed, Detail}, Data),
    {stop, normal, Data};
%% A failed write leaves the control stream as unusable as a closed one.
connected(info, {quic, send_failed, Stream, Reason},
          #data{quic_stream = Stream} = Data) ->
    notify(disconnected, {send_failed, Reason}, Data),
    {stop, normal, Data};
%% A graceful `peer_send_shutdown' on the control stream means the
%% peer is done sending on it -- their side of the session is over --
%% but says nothing about a dedicated/bidi stream still actively
%% finishing on this SAME connection (client_stream/bidi RPC runs on
%% its own stream, independent of the control stream). Treat it like
%% OUR OWN `cast, {close, Reason}' just below: drain for up to
%% `?DRAIN_TIMEOUT_MS' instead of tearing the whole connection down
%% immediately.
%%
%% `notify(draining, ...)' here (and in `cast, {close, Reason}' below)
%% is deliberately a DIFFERENT event than `disconnected' -- its job is
%% only to tell `controlling_pid' "you may want to hold off on
%% `dedicated_streams_idle' until you've checked your own bookkeeping
%% for this connection", not to signal the connection is gone.
%% `disconnected' itself still only fires once draining actually
%% concludes (`finish_draining/1'), so `controlling_pid' never tears
%% down routing for a stream that may still legitimately finish.
%%
%% Was: `{stop, normal, Data}' unconditionally here -- asymmetric with
%% the graceful `cast, {close, Reason}' path (which already drains)
%% for what is, from the wire's perspective, the exact same kind of
%% event just initiated by the other side. The abrupt CONNECTION_CLOSE
%% that produced could kill an in-flight dedicated-stream reply
%% mid-write. Found by Fable's review, macula-io/macula#9.
%%
%% `stream_closed' just above is deliberately NOT changed the same
%% way: it signals an error/reset on the control stream, not a clean
%% peer GOODBYE, and draining a connection whose control stream just
%% errored has no clear benefit.
connected(info, {quic, peer_send_shutdown, Stream, _Detail},
          #data{quic_stream = Stream} = Data) ->
    notify(draining, peer_closed, Data),
    {next_state, draining, Data};
connected(cast, {close, Reason}, Data) ->
    {_Sent, Signed} = send_goodbye(Data#data.quic_stream, Reason, Data),
    notify(draining, Reason, Signed),
    {next_state, draining, Signed};
%% `reject/2''s reason for existing: `close/2' transitions through
%% `draining' for up to `?DRAIN_TIMEOUT_MS' (5s), during which further
%% inbound data is silently accepted and discarded by design
%% (`draining/3''s "ignore late inbound during drain" clause) — correct
%% for a peer whose session was genuinely trusted and is simply ending,
%% wrong for a peer that just failed an admission check (e.g. an
%% S/Kademlia puzzle, see `macula_station_listener:reject_handshake/3')
%% and was never trusted in the first place. No GOODBYE either — a
%% rejected peer has no session to say goodbye to.
connected(cast, {reject, Reason}, Data) ->
    notify(disconnected, Reason, Data),
    {stop, normal, Data};
connected(cast, {send_frame, Frame}, Data) ->
    %% Coalesce: drain any other queued `{send_frame, _}' casts and
    %% emit them in a single NIF write. Cuts per-NIF overhead +
    %% gen_statem reduction-counter cost when many EVENT/PUBLISH
    %% frames burst together (pubsub flood, DHT batch put). The
    %% Quinn stream still handles MTU-level packetisation; this is
    %% purely an Erlang-side amortization.
    Frames = drain_send_frames([Frame]),
    {_Sent, Signed} = send_application_frames(Frames, Data),
    {keep_state, Signed};
%% The issuer's statement for the binding this side presented goes to
%% the peer as a status frame.
connected(info, {macula_statement, Issuer, Hash, Statement},
          #data{issuer = Issuer, own_binding_hash = Hash, quic_stream = Stream} = Data) ->
    _ = send_handshake_bytes(Stream, macula_handshake:status(Statement)),
    {keep_state, Data};
connected({timeout, status_expired}, status_expired, Data) ->
    closed(status_expired, Data);
connected({timeout, binding_expired}, binding_expired, Data) ->
    closed(binding_expired, Data);
connected({call, From}, peer_identity,
          #data{profile = Profile, peer = #{node_id := NodeId, identity_key := Key},
                peer_capabilities = Capabilities} = Data) ->
    Identity = #{node_id => NodeId, identity_key => Key, profile => Profile, capabilities => Capabilities},
    {keep_state, Data, [{reply, From, {ok, Identity}}]};
connected({call, From}, peer_capabilities, Data) ->
    {keep_state, Data,
     [{reply, From, {ok, Data#data.peer_capabilities}}]};
connected(EventType, Event, Data) ->
    other_event(EventType, Event, connected, Data).


%% On the open connection every frame is an application frame or a
%% status frame, told apart by its frame_type. A handshake frame, a frame
%% macula_frame refuses and bytes that are not CBOR close the connection
%% with malformed_frame, as any envelope refusal does, and a status frame
%% that fails its checks closes it with that check's reason.
open_frames({ok, Frames, Tail}, Data) ->
    open_frame(Frames, Data#data{buf = Tail}, []);
open_frames({error, frame_too_large}, Data) ->
    closed(malformed_frame, Data).

open_frame([], Data, Actions) ->
    {keep_state, Data, lists:reverse(Actions)};
open_frame([Bytes | Rest], Data, Actions) ->
    routed(macula_record_cbor:decode_strict(Bytes), Rest, Data, Actions).

%% Each frame is decoded once, and its frame_type routes it.
routed({ok, Wire}, Rest, Data, Actions) ->
    kind_read(macula_handshake:open_frame_kind(Wire), Wire, Rest, Data, Actions);
routed({error, _NotCbor}, _Rest, Data, _Actions) ->
    closed(malformed_frame, Data).

kind_read(status, Wire, Rest, #data{profile = Profile, peer = Peer} = Data, Actions) ->
    Reader = #{profile => Profile, identity_key => maps:get(identity_key, Peer), binding => peer_binding(Peer),
               now => now_ms(Data)},
    status_read(macula_handshake:read_status_wire(Wire, Reader), Rest, Data, Actions);
kind_read(handshake, _Wire, _Rest, Data, _Actions) ->
    closed(malformed_frame, Data);
kind_read(other, Wire, Rest, Data, Actions) ->
    open_frame_read(macula_frame:read_wire(Wire), Rest, Data, Actions).

open_frame_read({ok, Frame}, Rest, Data, Actions) ->
    neighbour_read(macula_frame:verify_neighbour(Frame, neighbour_reader(Data)), Frame, Rest, Data, Actions);
open_frame_read({error, bad_frame}, _Rest, Data, _Actions) ->
    closed(malformed_frame, Data).

status_read({ok, ExpiresAt}, Rest, Data, Actions) ->
    open_frame(Rest, Data, [status_timer(ExpiresAt, Data) | Actions]);
status_read({error, Reason}, _Rest, Data, _Actions) ->
    closed(Reason, Data).

%% In pq_hybrid a control frame comes back as the frame its neighbour
%% tbs holds, for this connection and the next seq from the peer, and a
%% refused one closes the connection with the refusal.
neighbour_read({ok, Opened}, Frame, Rest, Data, Actions) ->
    ok = route_frame(Opened, Data),
    open_frame(Rest, received(Frame, maybe_liveness_reply(Opened, Data)), Actions);
neighbour_read({error, Reason}, _Frame, _Rest, Data, _Actions) ->
    closed(Reason, Data).

neighbour_reader(#data{profile = Profile, peer = #{identity_key := PeerKey}, connection = Connection,
                       received_seq = Seq}) ->
    #{profile => Profile, peer_key => PeerKey, connection => Connection, seq => Seq}.

received(#{frame_type := Type}, #data{profile = Profile, received_seq = Seq} = Data) ->
    counted(macula_frame:neighbour_signed(Profile, Type), Data#data{received_seq = Seq + 1}, Data).

counted(true, Counted, _Data) -> Counted;
counted(false, _Counted, Data) -> Data.

peer_binding(#{tls_binding := Binding}) -> Binding;
peer_binding(#{connect_binding := Binding}) -> Binding.

%%------------------------------------------------------------------
%% State: draining
%%------------------------------------------------------------------

draining(enter, _Old, Data) ->
    {keep_state, Data, [{state_timeout, ?DRAIN_TIMEOUT_MS, drain_done}]};
draining(state_timeout, drain_done, Data) ->
    finish_draining(Data);
%% `controlling_pid' (e.g. macula-station's peer_observer, which
%% already tracks exactly which dedicated streams belong to which
%% connection) sends this once it has confirmed none remain for this
%% connection -- lets a connection with no in-flight dedicated/bidi
%% work close as soon as that's known, instead of always waiting out
%% the full `?DRAIN_TIMEOUT_MS'. A `controlling_pid' that never sends
%% this (doesn't track streams, or simply hasn't been updated) changes
%% nothing -- `state_timeout'/`drain_done' above still fires
%% unconditionally as the backstop. macula-io/macula#9.
draining(cast, dedicated_streams_idle, Data) ->
    finish_draining(Data);
%% `{quic, closed, Conn, Detail}' is NEVER actually sent — see
%% `handshaking/3''s matching comment. Replaced with the events that
%% actually arrive; unlike `handshaking'/`connected' this was not a
%% "stuck forever" bug (the `state_timeout' below already terminates
%% the drain unconditionally), just a missed opportunity to end the
%% drain the instant the transport confirms the peer is gone instead
%% of always waiting out the full `?DRAIN_TIMEOUT_MS'.
draining(info, {quic, stream_closed, Stream, Detail},
         #data{quic_stream = Stream} = Data) ->
    notify(disconnected, {peer_closed_during_drain, Detail}, Data),
    {stop, normal, Data};
draining(info, {quic, peer_send_shutdown, Stream, _Detail},
         #data{quic_stream = Stream} = Data) ->
    notify(disconnected, peer_closed_during_drain, Data),
    {stop, normal, Data};
%% A failed write (the GOODBYE, or a frame sent before it) ends the drain
%% as a closed control stream does.
draining(info, {quic, send_failed, Stream, Reason},
         #data{quic_stream = Stream} = Data) ->
    notify(disconnected, {send_failed_during_drain, Reason}, Data),
    {stop, normal, Data};
%% An open started while connected still ends during the drain: a stream
%% the peer allows now is handed over as usual. New opens are refused.
draining(info, {quic, stream_opened, Tag, Stream},
         #data{openings = Openings} = Data) when is_map_key(Tag, Openings) ->
    dedicated_stream_opened(Tag, Stream, Data);
draining(info, {quic, stream_open_failed, Tag, Reason},
         #data{openings = Openings} = Data) when is_map_key(Tag, Openings) ->
    dedicated_open_failed(Tag, Reason, Data);
draining({timeout, {dedicated_stream_open, Tag}}, expired, Data) ->
    dedicated_open_expired(Tag, Data);
draining(cast, {open_dedicated_stream, Owner, Ref}, _Data) ->
    refuse_dedicated_open(Owner, Ref);
draining(info, {quic, _, _, _}, Data) ->
    %% Ignore late inbound during drain.
    {keep_state, Data};
draining(cast, {close, _Reason}, Data) ->
    %% Already draining — idempotent.
    {keep_state, Data};
%% A `reject/2' arriving while already draining (e.g. a caller closed
%% gracefully first, then a separate admission check failed) ends the
%% remaining drain window immediately rather than idling it out.
draining(cast, {reject, Reason}, Data) ->
    notify(disconnected, Reason, Data),
    {stop, normal, Data};
%% A lifecycle timer or a statement that arrives while draining changes
%% nothing: the connection is ending.
draining({timeout, _Lifecycle}, _Expired, Data) ->
    {keep_state, Data};
draining(info, {macula_statement, _Issuer, _Hash, _Statement}, Data) ->
    {keep_state, Data};
draining(EventType, Event, Data) ->
    other_event(EventType, Event, draining, Data).

%% Shared terminal action for `draining' concluding normally, whether
%% via the `state_timeout' backstop or an early `dedicated_streams_idle'
%% signal from `controlling_pid'. `close_quic' here is technically
%% redundant with `terminate/3''s own unconditional call once `{stop,
%% normal, Data}' lands -- matches the pre-existing `drain_done' shape,
%% harmless (`close_quic' is idempotent).
finish_draining(Data) ->
    notify(disconnected, drained, Data),
    _ = close_quic(Data),
    {stop, normal, Data}.

%%------------------------------------------------------------------
%% Dedicated stream open
%%------------------------------------------------------------------

dedicated_open_started({ok, Opening}, Owner, Ref, #data{openings = Openings} = Data) ->
    Tag = macula_quic:stream_open_tag(Opening),
    {keep_state, Data#data{openings = Openings#{Tag => {Owner, Ref, Opening}}},
     [{{timeout, {dedicated_stream_open, Tag}}, ?DEDICATED_STREAM_OPEN_TIMEOUT_MS, expired}]};
dedicated_open_started({error, Reason}, Owner, Ref, _Data) ->
    Owner ! {macula_peering, dedicated_stream_open_failed, Ref, Reason},
    keep_state_and_data.

dedicated_stream_opened(Tag, Stream, #data{openings = Openings} = Data) ->
    {{Owner, Ref, _Opening}, Rest} = maps:take(Tag, Openings),
    ok = macula_quic:controlling_process(Stream, Owner),
    ok = macula_quic:setopt(Stream, active, true),
    Owner ! {macula_peering, dedicated_stream_opened, Ref, Stream},
    {keep_state, Data#data{openings = Rest}, [{{timeout, {dedicated_stream_open, Tag}}, cancel}]}.

dedicated_open_failed(Tag, Reason, #data{openings = Openings} = Data) ->
    {{Owner, Ref, _Opening}, Rest} = maps:take(Tag, Openings),
    Owner ! {macula_peering, dedicated_stream_open_failed, Ref, Reason},
    {keep_state, Data#data{openings = Rest}, [{{timeout, {dedicated_stream_open, Tag}}, cancel}]}.

%% The peer allowed no stream within the bound. Ending the open takes out
%% a result already delivered to this process, so no stream from it
%% reaches `Owner' later.
dedicated_open_expired(Tag, #data{openings = Openings} = Data) ->
    {{Owner, Ref, Opening}, Rest} = maps:take(Tag, Openings),
    ok = macula_quic:cancel_open_stream(Opening),
    Owner ! {macula_peering, dedicated_stream_open_failed, Ref, timeout},
    {keep_state, Data#data{openings = Rest}}.

refuse_dedicated_open(Owner, Ref) ->
    Owner ! {macula_peering, dedicated_stream_open_failed, Ref, not_connected},
    keep_state_and_data.

%% A connection that ends fails every open still waiting for the peer.
fail_waiting_opens(#data{openings = Openings}) ->
    maps:foreach(fun(_Tag, {Owner, Ref, Opening}) ->
                         ok = macula_quic:cancel_open_stream(Opening),
                         Owner ! {macula_peering, dedicated_stream_open_failed, Ref, closed}
                 end, Openings).

%%------------------------------------------------------------------
%% Frame send helpers
%%------------------------------------------------------------------

send_handshake_bytes(Stream, Bytes) ->
    macula_quic:send(Stream, macula_frame:encode_bytes(Bytes)).

send_goodbye(undefined, _Reason, Data) ->
    {ok, Data};
send_goodbye(Stream, Reason, Data) ->
    send_encoded(encode_or_drop(macula_frame:goodbye(Reason, undefined), Data), Stream).

send_application_frame(_Frame, #data{quic_stream = undefined} = Data) ->
    {ok, Data};
send_application_frame(Frame, #data{quic_stream = Stream} = Data) ->
    send_encoded(encode_or_drop(Frame, Data), Stream).

%% Encode N frames into one iolist and push them as a single NIF call.
%% Their producers sign what they carry, and in pq_hybrid this
%% connection neighbour-signs the control frames among them, in order.
%% Returns the send result with the seq moved on. Skips work entirely
%% when the stream isn't yet up.
send_application_frames(_Frames, #data{quic_stream = undefined} = Data) ->
    {ok, Data};
send_application_frames([Frame], Data) ->
    %% Single-frame fast path — avoid the iolist accumulation cost.
    send_application_frame(Frame, Data);
send_application_frames(Frames, #data{quic_stream = Stream} = Data) ->
    {Encoded, Signed} = lists:foldl(fun encode_next/2, {[], Data}, Frames),
    {macula_quic:send(Stream, lists:reverse(Encoded)), Signed}.

%%------------------------------------------------------------------
%% App-level liveness probe
%%------------------------------------------------------------------

%% The `connected' state's `state_timeout' action, armed only when the
%% probe is enabled. Re-armed after every tick.
liveness_tick_actions(#data{liveness_interval_ms = undefined}) ->
    [];
liveness_tick_actions(Data) ->
    [liveness_tick_action(Data)].

liveness_tick_action(#data{liveness_interval_ms = Ms}) ->
    {state_timeout, Ms, liveness_tick}.

%% Send a `_macula.ping' CALL to the peer and hold the request a
%% verified reply must name. The peer answers any verified CALL — a
%% station answers an unknown procedure with a signed
%% `unknown_next_peer' relay error, a pool link with a provider error —
%% so any verified reply proves the peer's application layer is alive,
%% which the transport's keep-alive ACKs cannot.
send_liveness_probe(#data{identity = Kp, profile = Profile,
                          peer_node_id = Peer,
                          liveness_interval_ms = Ms} = Data) ->
    RequestId = crypto:strong_rand_bytes(16),
    Probe = macula_frame:call(#{
        request_id => RequestId,
        realm      => ?LIVENESS_REALM,
        procedure  => ?LIVENESS_PROCEDURE,
        target     => Peer,
        deadline   => now_ms(Data) + Ms,
        payload    => #{}}, Kp),
    {ok, Request} = macula_frame:verify_request(Probe, Profile),
    ProbeData = Data#data{liveness_outstanding = {RequestId, Request}},
    {_Sent, Data1} = send_application_frames([Probe], ProbeData),
    Data1.

%% A frame naming the outstanding probe's request_id: verified against
%% the request, it proves the peer is alive and clears the probe
%% (fresh probe on the next tick); anything else leaves the probe
%% outstanding, so the tick counts the miss. The frame is still routed
%% to the controlling pid as any other frame would be.
maybe_liveness_reply(Frame, #data{liveness_outstanding = {RequestId, Request},
                                  profile = Profile,
                                  peer_node_id = Station} = Data) ->
    case macula_frame:claimed_reply_ids(Frame) of
        {ok, #{request_id := RequestId, request_hash := _}} ->
            liveness_reply_verdict(reply_kind(Frame), Frame,
                                   Request, Profile, Station, Data);
        _Other ->
            Data
    end;
maybe_liveness_reply(_Frame, Data) ->
    Data.

reply_kind(#{frame_type := result}) -> result;
reply_kind(#{frame_type := error, relay_error := _}) -> relay_error;
reply_kind(#{frame_type := error, reply := _}) -> error;
reply_kind(_Frame) -> not_a_reply.

liveness_reply_verdict(result, Frame, Request, Profile, _Station, Data) ->
    cleared(macula_frame:verify_reply(Frame, Request, Profile), Data);
liveness_reply_verdict(relay_error, Frame, Request, Profile, Station, Data) ->
    cleared(macula_frame:verify_relay_error(Frame, Request, Profile, Station),
            Data);
liveness_reply_verdict(error, Frame, Request, Profile, _Station, Data) ->
    cleared(macula_frame:verify_reply(Frame, Request, Profile), Data);
liveness_reply_verdict(not_a_reply, _Frame, _Request, _Profile, _Station, Data) ->
    Data.

cleared({ok, _Verified}, Data) ->
    Data#data{liveness_outstanding = undefined, liveness_misses = 0};
cleared({error, _Refusal}, Data) ->
    Data.

encode_next(Frame, {Encoded, Data}) ->
    kept(encode_or_drop(Frame, Data), Encoded).

kept({{true, Bytes}, Data}, Encoded) -> {[Bytes | Encoded], Data};
kept({false, Data}, Encoded)         -> {Encoded, Data}.

send_encoded({{true, Bytes}, Data}, Stream) -> {macula_quic:send(Stream, Bytes), Data};
send_encoded({false, Data}, _Stream)        -> {ok, Data}.

%% @private Encode one frame, or drop it loudly.
%%
%% A frame that cannot be encoded is the SENDER's bug, but the process
%% that dies for it is this connection — shared by every producer on the
%% link — and `drain_send_frames/1' means it takes up to ?MAX_BATCH
%% innocent frames from unrelated producers down with it. Let-it-crash
%% is the wrong instinct in exactly this shape, where the process that
%% crashes is not the one that was guilty.
%%
%% This is the backstop, not the guard. `macula_peering:send_frame/2'
%% rejects unencodable frames synchronously, where the caller can be
%% told. What reaches here is what the checker cannot know without
%% encoding — chiefly total frame size — and the honest response to that
%% is to lose one frame with a loud log rather than a whole connection.
encode_or_drop(Frame, Data) ->
    try encoded(neighboured(Frame, Data))
    catch Class:Reason ->
        logger:error("[macula_peering_conn] dropped unencodable ~p frame: ~p:~p",
                     [maps:get(frame_type, Frame, unknown), Class, Reason]),
        {false, Data}
    end.

%% In pq_hybrid a control frame goes out neighbour-signed with this
%% side's identity key, for this connection and the next seq. A frame
%% that cannot be signed or encoded takes no seq.
neighboured(#{frame_type := Type} = Frame, #data{profile = Profile} = Data) ->
    neighbour_signed(macula_frame:neighbour_signed(Profile, Type), Frame, Data).

neighbour_signed(true, Frame, #data{identity = Identity, connection = Connection, sent_seq = Seq} = Data) ->
    {macula_frame:sign_neighbour(Frame, Identity, #{connection => Connection, seq => Seq}),
     Data#data{sent_seq = Seq + 1}};
neighbour_signed(false, Frame, Data) ->
    {Frame, Data}.

encoded({Frame, Data}) ->
    {{true, macula_frame:encode(Frame)}, Data}.

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

close_quic(#data{quic_conn = undefined}) ->
    ok;
close_quic(#data{quic_conn = Conn}) ->
    try macula_quic:close_connection(Conn) catch _:_ -> ok end,
    ok.

%%------------------------------------------------------------------
%% Outbound dial — unpack peering's option-map into macula_quic's
%% positional API.
%%------------------------------------------------------------------

%% The station is named by the handshake: its challenge must derive to
%% the node_id dialed, over the leaf this dial received.
start_dial(#{host := Host, port := Port} = Target) ->
    macula_quic:async_connect(Host, Port, dial_opts(Target), dial_timeout(Target)).

%% The QUIC options for one dial, from the target the caller built. There
%% is no TLS policy to choose: `macula_quic' trusts the station by its
%% handshake signature under its certificate's key, and the handshake in
%% this module binds the connection to the node_id dialed.
dial_opts(Target) ->
    [{alpn, maps:get(alpn, Target, [<<"macula">>])}].

dial_timeout(Target) ->
    maps:get(timeout_ms, Target, 30_000).

%%------------------------------------------------------------------
%% Clock
%%------------------------------------------------------------------

now_ms(#data{clock = Clock}) ->
    Clock().

wall_clock_ms() ->
    erlang:system_time(millisecond).

%%------------------------------------------------------------------
%% Notifications
%%------------------------------------------------------------------

notify(Event, Detail, #data{controlling_pid = Pid}) ->
    Pid ! {macula_peering, Event, self(), Detail},
    ok.

%% Inbound-frame router. Category-bypass: DHT-class frames go to
%% `dht_recipient' if set; pubsub-class frames go to `pubsub_recipient'
%% if set; everything else (and any bypass whose recipient is unset,
%% unregistered or dead) flows through `controlling_pid' in the
%% legacy form. Recipients are resolved per frame — see
%% `resolve_recipient/1'. See the
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

route_by_category(dht, Frame, NodeId, #data{dht_recipient = R} = Data) ->
    bypass_or_legacy(resolve_recipient(R), dht_frame, Frame, NodeId, Data);
route_by_category(pubsub, Frame, NodeId, #data{pubsub_recipient = R} = Data) ->
    bypass_or_legacy(resolve_recipient(R), pubsub_frame, Frame, NodeId, Data);
route_by_category(_, Frame, _NodeId, Data) ->
    notify_frame(Frame, Data).

%% Resolve a bypass recipient on EVERY frame rather than trusting the
%% value captured at `init/1'.
%%
%% A registered name is re-resolved per frame, so a recipient
%% crash-restart is transparent: the supervisor re-registers the name
%% to the new pid and the next frame lands there. A raw pid cannot be
%% re-resolved, so it is liveness-checked instead — before this,
%% `is_pid/1' alone guarded the bypass, and `is_pid/1' is true for a
%% DEAD pid. A recipient restart therefore stranded every
%% already-established connection: frames were posted to a dead pid
%% and silently vanished, with no error at either end, for the whole
%% remaining life of the connection.
%%
%% `is_process_alive/1' raises `badarg' for a remote pid, so locality
%% is a guard rather than a check. Recipients are same-BEAM by design;
%% a remote one is passed through untested.
resolve_recipient(undefined) ->
    undefined;
resolve_recipient(Name) when is_atom(Name) ->
    erlang:whereis(Name);
resolve_recipient(Pid) when is_pid(Pid), node(Pid) =:= node() ->
    live_pid(Pid, erlang:is_process_alive(Pid));
resolve_recipient(Pid) when is_pid(Pid) ->
    Pid.

live_pid(Pid, true)   -> Pid;
live_pid(_Pid, false) -> undefined.

%% No live bypass recipient — fall back to `controlling_pid' in the
%% legacy form. That is the documented pre-4.4.3/4.4.4 route and the
%% consumer still handles every frame category there, so an absent or
%% restarting recipient degrades to the slower path instead of
%% dropping traffic on the floor.
bypass_or_legacy(undefined, _Tag, Frame, _NodeId, Data) ->
    notify_frame(Frame, Data);
bypass_or_legacy(Pid, Tag, Frame, NodeId,
                 #data{timing_enabled = Timing}) ->
    notify_bypass(Pid, Tag, NodeId, Frame, Timing),
    ok.

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
classify(subscribe)      -> pubsub;
classify(unsubscribe)    -> pubsub;
classify(publish)        -> pubsub;
classify(event)          -> pubsub;
classify(_)              -> other.

%% Refusal reports and the refusal count are taken in every state;
%% anything else a state does not handle is dropped.
other_event(cast, {object_refused, Kind, Charged}, _State, #data{refusals = Refusals, charged = Count} = Data) ->
    {keep_state, Data#data{refusals = maps:update_with(Kind, fun(N) -> N + 1 end, 1, Refusals),
                           charged = Count + charge(Charged)}};
other_event({call, From}, refusals, _State, #data{refusals = Refusals, charged = Count}) ->
    {keep_state_and_data, [{reply, From, #{counts => Refusals, charged => Count}}]};
other_event(EventType, Event, State, Data) ->
    drop_unexpected(EventType, Event, State, Data).

charge(true) -> 1;
charge(false) -> 0.

drop_unexpected({call, From}, Event, State, Data) ->
    %% Synchronous call into a state that doesn't handle it. Reply
    %% so the caller fails fast (e.g. `peer_capabilities/1' before
    %% handshake completes) instead of blocking until its own
    %% timeout — which would surface as `{timeout, ...}' to user
    %% code and require defensive try/catch wrappers everywhere.
    macula_diagnostics:event(warning, <<"_macula.peering.unexpected_event">>, #{
        state      => State,
        event_type => call,
        event      => safe_event(Event)
    }),
    {keep_state, Data, [{reply, From, not_connected}]};
drop_unexpected(EventType, Event, State, Data) ->
    macula_diagnostics:event(warning, <<"_macula.peering.unexpected_event">>, #{
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

-ifdef(TEST).
state_field_index(Field) ->
    field_index(Field, record_info(fields, data), 2).

field_index(Field, [Field | _Rest], Index) -> Index;
field_index(Field, [_Other | Rest], Index) -> field_index(Field, Rest, Index + 1).
-endif.
