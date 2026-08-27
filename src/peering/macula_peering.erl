%% @doc Macula peering — connection state machine API.
%%
%% Each peer connection is one `macula_peering_conn' gen_statem under the
%% `macula_peering_conn_sup' simple_one_for_one supervisor.
%%
%% Two entry points:
%% <ul>
%%   <li>`connect/1' — outbound dial; worker drives the QUIC connect.</li>
%%   <li>`accept/2' — inbound; caller transfers ownership of an
%%       already-established `macula_quic' connection (a `reference()')
%%       to a new worker.</li>
%% </ul>
%%
%% The caller passes a `controlling_pid' in opts; that pid receives
%% peering events as messages:
%% <ul>
%%   <li>`{macula_peering, connected, ConnPid, PeerNodeId}'</li>
%%   <li>`{macula_peering, frame, ConnPid, Frame}' (post-handshake)</li>
%%   <li>`{macula_peering, disconnected, ConnPid, Reason}'</li>
%% </ul>
%%
%% An optional `accept_owner' pid in opts receives a single
%% `{macula_peering, handshake_complete, ConnPid, PeerNodeId}'
%% message the moment the worker transitions from `handshaking' to
%% `connected'. Used by accept-side listeners that (a) cap concurrent
%% handshaking workers separately from healthy connected peers, and
%% (b) dedupe duplicate dials from the same peer identity by closing
%% prior workers for the same `PeerNodeId'.
-module(macula_peering).

-export([
    connect/1,
    accept/2,
    close/1, close/2,
    reject/2,
    send_frame/2,
    peer_capabilities/1,
    open_dedicated_stream/1,
    send_on_stream/3
]).

%% Capability bit asserting the peer is a relay-station (i.e. it
%% advertises on behalf of others via gossip). Daemons MUST leave this
%% unset. Stations set this on outbound dial and inbound accept so the
%% counterpart can tell direct daemon ADVERTISEs apart from station
%% gossip relays.
-define(CAP_STATION, 16#0000_0000_0000_0001).

-type opts() :: macula_peering_conn:opts().
-export_type([opts/0]).

%%------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------

%% @doc Outbound connect. Spawns a worker that opens a QUIC connection to
%% `target' and runs the CONNECT/HELLO handshake.
-spec connect(opts()) -> {ok, pid()} | {error, term()}.
connect(Opts) ->
    macula_peering_conn_sup:start_conn(Opts#{role => client}).

%% @doc Inbound accept. Caller currently owns `Conn' (e.g. it's the listener
%% owner that just received `{quic, new_conn, Conn, _}'). The transfer of
%% ownership and the handshake start are sequenced atomically.
-spec accept(reference(), opts()) ->
    {ok, pid()} | {error, term()}.
accept(Conn, Opts) ->
    start_server_worker(Conn, Opts#{role => server, quic_conn => Conn}).

start_server_worker(Conn, Opts) ->
    handle_started(macula_peering_conn_sup:start_conn(Opts), Conn).

handle_started({ok, Pid}, Conn) ->
    ok = macula_quic:controlling_process(Conn, Pid),
    ok = gen_statem:cast(Pid, start_handshake),
    {ok, Pid};
handle_started(Err, _Conn) ->
    Err.

%% @doc Initiate a graceful close (sends GOODBYE, drains 5s, terminates).
%% For a peer that was never trusted in the first place (failed an
%% admission check rather than ending a legitimate session), use
%% `reject/2' instead — see its own doc for why.
-spec close(pid()) -> ok.
close(Pid) ->
    close(Pid, operator_stop).

-spec close(pid(), atom()) -> ok.
close(Pid, Reason) ->
    gen_statem:cast(Pid, {close, Reason}).

%% @doc Terminate a connection immediately, with no GOODBYE and no
%% drain window — for a peer that failed an admission check (e.g. an
%% S/Kademlia identity puzzle) rather than one ending a legitimate
%% session. `close/2' transitions through `draining' for up to 5s
%% (`?DRAIN_TIMEOUT_MS'), during which any further inbound data is
%% silently accepted and discarded by design (`draining' state's
%% "ignore late inbound during drain" clause) — correct for a
%% genuinely-trusted peer whose last few in-flight frames shouldn't
%% cause spurious errors, but for a peer that was never admitted at
%% all, those same 5 seconds are pure exposure: the connection went
%% from "no verdict yet" to "should already be gone" the instant the
%% admission check failed, so there is no legitimate traffic left to
%% drain gracefully. `reject/2' skips `draining' and terminates the
%% state machine directly.
-spec reject(pid(), atom()) -> ok.
reject(Pid, Reason) ->
    gen_statem:cast(Pid, {reject, Reason}).

%% @doc Send a frame through the peer connection. Signs the frame with
%% the local identity if it isn't already signed.
%%
%% The send is a cast, so encoding happens later, inside the shared
%% connection process. This is therefore the LAST synchronous point at
%% which a caller can be told its frame is unsendable, and every
%% producer — pubsub, RPC calls and results, streaming, advertise,
%% content — funnels through here. Guarding one verb upstream (publish)
%% left the other five able to kill the connection, so the check lives
%% here, where it covers all of them at one seam.
%%
%% Returns `{error, {unsupported_payload_type, Type, Path}}' without
%% casting when the frame cannot be encoded. Callers that ignore the
%% return at least no longer take the connection down; callers that
%% check get a structured reason and a path to the offending value.
-spec send_frame(pid(), macula_frame:frame()) -> ok | {error, term()}.
send_frame(Pid, Frame) when is_map(Frame) ->
    cast_checked(macula_frame:check_frame(Frame), Pid, Frame).

cast_checked(ok, Pid, Frame) ->
    gen_statem:cast(Pid, {send_frame, Frame});
cast_checked({error, Reason} = Rejected, _Pid, Frame) ->
    logger:error("[macula_peering] refused unsendable ~p frame: ~ts",
                 [maps:get(frame_type, Frame, unknown),
                  macula_frame:explain(Reason)]),
    Rejected.

%% @doc Open a QUIC stream on this connection dedicated to one
%% session (a streaming RPC call, a content transfer) instead of
%% sharing the connection's control stream. Ownership transfers to
%% the calling process immediately: it drives the stream directly via
%% `send_on_stream/3' and `macula_quic:*', and receives the stream's
%% `{quic, Bin, Stream, Flags}' events straight into its own mailbox
%% — the peering connection process is not in this stream's path at
%% all once this call returns. See PLAN_PER_STREAM_QUIC_ISOLATION.md.
-spec open_dedicated_stream(pid()) -> {ok, reference()} | {error, term()}.
open_dedicated_stream(Pid) ->
    gen_statem:call(Pid, {open_dedicated_stream, self()}, 10_000).

%% @doc Encode, sign, and write one frame directly onto a dedicated
%% stream obtained from `open_dedicated_stream/1' — no peering
%% connection process involved, unlike `send_frame/2'. `Identity' is
%% the caller's own key pair (the connection process is not asked to
%% sign on the caller's behalf here, since it is not a party to this
%% stream). Synchronous: the underlying NIF write can block briefly
%% on QUIC flow-control credit, same as any other `macula_quic:send/2'
%% call.
-spec send_on_stream(reference(), macula_frame:frame(),
                     macula_identity:key_pair()) -> ok | {error, term()}.
send_on_stream(Stream, Frame, Identity) when is_map(Frame) ->
    send_checked_on_stream(macula_frame:check_frame(Frame), Stream, Frame, Identity).

send_checked_on_stream(ok, Stream, Frame, Identity) ->
    macula_quic:send(Stream, macula_frame:encode(ensure_signed(Frame, Identity)));
send_checked_on_stream({error, Reason} = Rejected, _Stream, Frame, _Identity) ->
    logger:error("[macula_peering] refused unsendable ~p frame: ~ts",
                 [maps:get(frame_type, Frame, unknown),
                  macula_frame:explain(Reason)]),
    Rejected.

ensure_signed(#{signature := _} = Frame, _Id) -> Frame;
ensure_signed(Frame, Id) -> macula_frame:sign(Frame, Id).

%% @doc Read the peer's capabilities bitmask as observed in their
%% CONNECT/HELLO frame. Returns `{ok, NegotiatedCaps}' once the
%% handshake has completed and `{error, not_connected}' otherwise.
%%
%% Used by relays to tell direct daemon ADVERTISEs from station-to-
%% station gossip relays at frame-dispatch time (see `?CAP_STATION').
%% Daemons send `0'; relay stations OR-in `?CAP_STATION'. Pre-version
%% peers that don't set the bit are treated as daemons by callers,
%% which matches their actual role.
-spec peer_capabilities(pid()) ->
    {ok, non_neg_integer()} | {error, not_connected}.
peer_capabilities(Pid) when is_pid(Pid) ->
    try gen_statem:call(Pid, peer_capabilities, 1_000) of
        {ok, _Caps} = Ok -> Ok;
        not_connected   -> {error, not_connected}
    catch _:_ -> {error, not_connected}
    end.
