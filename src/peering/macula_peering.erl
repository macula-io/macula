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
%% `PeerNodeId' is the peer's node_id, derived from the identity key the
%% handshake verified (plans/DESIGN_PQ_HANDSHAKE_FRAMES.md).
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
    peer_identity/1,
    open_dedicated_stream/1,
    async_open_dedicated_stream/1,
    send_on_stream/2,
    async_send_on_stream/2,
    async_send_on_stream/3,
    relay_on_stream/2,
    async_relay_on_stream/2,
    async_relay_on_stream/3,
    close_dedicated_stream/1,
    object_refused/2,
    refusals/1
]).

%% Exports with no caller inside macula yet: macula-station's observer relays
%% stream frames with them, so a peer that stops reading never holds it.
-ignore_xref([{macula_peering, async_send_on_stream, 2}]).
-ignore_xref([{macula_peering, async_send_on_stream, 3}]).

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
%% `target' and runs the post-quantum handshake.
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

%% @doc Send a frame through the peer connection. The frame goes out as
%% its producer built it, and in pq_hybrid the connection adds a
%% neighbour signature to a control frame.
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

%% Stamped with the microsecond it was queued, so the connection can report
%% how long it waited (`macula_peering_conn''s `frame_observer').
cast_checked(ok, Pid, Frame) ->
    gen_statem:cast(Pid, {send_frame, erlang:monotonic_time(microsecond), Frame});
cast_checked({error, Reason} = Rejected, _Pid, Frame) ->
    logger:error("[macula_peering] refused unsendable ~p frame: ~ts",
                 [maps:get(frame_type, Frame, unknown),
                  macula_frame:explain(Reason)]),
    Rejected.

%% @doc Open a QUIC stream on this connection dedicated to one
%% session (a streaming RPC call, a content transfer) instead of
%% sharing the connection's control stream, waiting for it in the
%% calling process. Ownership transfers to the calling process: it
%% drives the stream directly via `send_on_stream/2' and
%% `macula_quic:*', and receives the stream's `{quic, Bin, Stream, Flags}'
%% events straight into its own mailbox — the peering connection
%% process is not in this stream's path at all once this call returns.
%% See PLAN_PER_STREAM_QUIC_ISOLATION.md.
%%
%% Returns `{error, timeout}' when the peer allows no further stream
%% within 10 s, and `{error, closed}' when the connection ends first. A
%% process that must keep serving while it waits uses
%% `async_open_dedicated_stream/1' instead.
-spec open_dedicated_stream(pid()) -> {ok, reference()} | {error, term()}.
open_dedicated_stream(Pid) ->
    Mon = erlang:monitor(process, Pid),
    Ref = async_open_dedicated_stream(Pid),
    receive
        {macula_peering, dedicated_stream_opened, Ref, Stream} ->
            true = erlang:demonitor(Mon, [flush]),
            {ok, Stream};
        {macula_peering, dedicated_stream_open_failed, Ref, Reason} ->
            true = erlang:demonitor(Mon, [flush]),
            {error, Reason};
        {'DOWN', Mon, process, Pid, _Reason} ->
            {error, closed}
    end.

%% @doc Start opening a dedicated stream, as `open_dedicated_stream/1'
%% does, and return at once with a reference. The calling process later
%% receives `{macula_peering, dedicated_stream_opened, Ref, Stream}', and
%% owns `Stream' in active mode, or
%% `{macula_peering, dedicated_stream_open_failed, Ref, Reason}', where
%% `Reason' is `timeout' when the peer allows no further stream within
%% 10 s, `not_connected' when the connection is not connected, `closed'
%% when it ends first, or the open's error. A connection process that has
%% already exited sends nothing, so monitor it to learn that.
-spec async_open_dedicated_stream(pid()) -> reference().
async_open_dedicated_stream(Pid) ->
    Ref = make_ref(),
    ok = gen_statem:cast(Pid, {open_dedicated_stream, self(), Ref}),
    Ref.

%% @doc Write one frame's bytes directly onto a dedicated stream
%% obtained from `open_dedicated_stream/1', with no peering connection
%% process involved, unlike `send_frame/2'. The stream's writer builds,
%% signs and encodes the frame itself; this writes the bytes it is
%% given and nothing else. Synchronous: waits in the calling process until
%% the bytes are written, for as long as the peer withholds QUIC flow-control
%% credit, as `macula_quic:send/2' does. A process that must not wait on a
%% peer uses `async_send_on_stream/2' instead.
-spec send_on_stream(reference(), binary()) -> ok | {error, term()}.
send_on_stream(Stream, Bytes) when is_binary(Bytes) ->
    macula_quic:send(Stream, Bytes).

%% @doc `send_on_stream/2' without waiting: the bytes are queued with
%% `macula_quic:async_send/2', so a peer that stops reading cannot hold the
%% calling process. Returns `ok' once they are queued. When the stream
%% already holds 1 MiB unwritten, queues nothing and returns
%% `{error, busy}', and the calling process later gets
%% `{quic, send_ready, Stream, undefined}' when it may send again. A relay
%% passes a frame on as the bytes it received.
-spec async_send_on_stream(reference(), binary()) -> ok | {error, term()}.
async_send_on_stream(Stream, Bytes) when is_binary(Bytes) ->
    macula_quic:async_send(Stream, Bytes).

%% @doc `async_send_on_stream/2' for bytes whose end the calling process
%% hears about. For bytes it queued, that process gets exactly one of
%% `{quic, send_complete, Stream, Tag}', once they are written, and
%% `{quic, send_incomplete, Stream, {Tag, Reason}}', when the stream is
%% reset, closed or fails first. `Tag' is the caller's own term, copied into
%% that message, so keep it small.
-spec async_send_on_stream(reference(), binary(), term()) -> ok | {error, term()}.
async_send_on_stream(Stream, Bytes, Tag) when is_binary(Bytes) ->
    macula_quic:async_send(Stream, Bytes, Tag).

%% @doc Write a frame a relay received onto a dedicated stream, as the bytes
%% it received: a unit `macula_frame:parse_for_relay/2' accepted, and nothing
%% else, so a relay never writes bytes its reader did not accept. Synchronous,
%% as `send_on_stream/2'.
-spec relay_on_stream(reference(), macula_frame:received_frame()) -> ok | {error, term()}.
relay_on_stream(Stream, Received) ->
    macula_quic:send(Stream, macula_frame:relayed_bytes(Received)).

%% @doc `relay_on_stream/2' without waiting, with the busy and send_ready
%% behaviour of `async_send_on_stream/2'.
-spec async_relay_on_stream(reference(), macula_frame:received_frame()) -> ok | {error, term()}.
async_relay_on_stream(Stream, Received) ->
    macula_quic:async_send(Stream, macula_frame:relayed_bytes(Received)).

%% @doc `async_relay_on_stream/2' for a frame whose end the calling process
%% hears about, with the notices of `async_send_on_stream/3'.
-spec async_relay_on_stream(reference(), macula_frame:received_frame(), term()) -> ok | {error, term()}.
async_relay_on_stream(Stream, Received, Tag) ->
    macula_quic:async_send(Stream, macula_frame:relayed_bytes(Received), Tag).

%% @doc Report an object a connection carried that its receiver
%% refused, by the kind of refusal. The connection counts refusals by
%% kind, and counts the ones `macula_frame:charged_refusal/1' charges.
%% A kind outside that classification is refused here, where it is
%% reported.
-spec object_refused(pid(), atom()) -> ok.
object_refused(Conn, Kind) when is_pid(Conn) ->
    gen_statem:cast(Conn, {object_refused, Kind, macula_frame:charged_refusal(Kind)}).

%% @doc The refusals reported on a connection: a count per kind, and
%% how many of them were charged.
-spec refusals(pid()) -> #{counts := #{atom() => pos_integer()}, charged := non_neg_integer()}.
refusals(Conn) when is_pid(Conn) ->
    gen_statem:call(Conn, refusals).

%% @doc Close a dedicated stream, one obtained from `open_dedicated_stream/1'
%% or one the peer opened, gracefully: data already written still goes out,
%% and then the stream ends, as `macula_quic:close_stream/1' does.
-spec close_dedicated_stream(reference()) -> ok.
close_dedicated_stream(Stream) ->
    macula_quic:close_stream(Stream).

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

%% @doc What the handshake verified of the peer: its node_id, its identity
%% key as carried, the profile and its capabilities. Returns
%% `{error, not_connected}' until the handshake has completed.
-spec peer_identity(pid()) ->
    {ok, #{node_id := <<_:256>>, identity_key := binary(), profile := macula_crypto_profile:profile(),
           capabilities := non_neg_integer()}}
  | {error, not_connected}.
peer_identity(Pid) when is_pid(Pid) ->
    try gen_statem:call(Pid, peer_identity, 1_000) of
        {ok, _Identity} = Ok -> Ok;
        not_connected        -> {error, not_connected}
    catch _:_ -> {error, not_connected}
    end.
