%%%-------------------------------------------------------------------
%%% @doc Macula streaming RPC — single-stream state machine.
%%%
%%% Owns one streaming RPC's state. Each `call_stream', `open_stream',
%%% or server-side handler invocation gets its own `macula_stream'
%%% gen_server. The state machine itself is carrier-agnostic; the
%%% peer shape (`{local, _}' or `{remote_via_link, _, _}') decides
%%% how chunks reach the wire.
%%%
%%% Two carriers route through `forward_to_peer/2':
%%% <ul>
%%%   <li>`{local, Pid}' — in-process pairing for unit tests and
%%%       `macula_stream_local' dispatch.</li>
%%%   <li>`{remote_via_link, Link, Sid}' — V2 wire format via
%%%       `macula_station_link' (CBOR `macula_frame:stream_*' frames
%%%       over a peering connection). The stream builds, signs and
%%%       writes its own frames on the dedicated QUIC stream the link
%%%       hands it; while that stream takes no more data the frames wait
%%%       here in order, and `send/2,3' callers wait for theirs.</li>
%%% </ul>
%%%
%%% Renamed from `macula_stream_v1' in 3.17.0; the V1 mesh_client
%%% carrier (`{remote, _, _}') was retired alongside the rest of the
%%% V1 surface in the same release. The module now spans the LOCAL
%%% carrier and the V2 station_link carrier only.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream).

-behaviour(gen_server).

%% Public API
-export([
    start_link/1,
    pair/2,
    attach_to_link/4,
    send/2,
    send/3,
    recv/1,
    recv/2,
    close/1,
    close_send/1,
    await_reply/1,
    await_reply/2,
    set_reply/2,
    set_error/2,
    abort/3,
    info/1
]).

%% Peer-to-peer protocol — drives inbound deliveries from carrier
%% modules (`macula_stream_local' for LOCAL pairs, `macula_station_link'
%% for V2 station-link pairs).
-export([
    deliver_chunk/3,
    deliver_end/2,
    deliver_error/3,
    deliver_reply/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    format_status/1
]).

-type role() :: client | server.
-type mode() :: server_stream | client_stream | bidi.
-type encoding() :: raw | msgpack.
-type chunk() :: binary() | {raw, binary()} | {term, term()}.
-type stream_id() :: binary().
-type result() :: {ok, term()} | {error, term()}.

%% Peer shape:
%%   undefined                — unpaired
%%   {local, Pid}             — in-process pairing
%%   {remote_via_link, L, Sid}— V2 station_link carrier: deliveries
%%                              encoded as `macula_frame:stream_*'
%%                              frames and shipped through the
%%                              station_link's peering connection.
-type peer() :: undefined
              | {local, pid()}
              | {remote_via_link, pid(), stream_id()}.

-export_type([role/0, mode/0, encoding/0, chunk/0, stream_id/0, result/0,
              peer/0]).

-record(state, {
    id              :: stream_id(),
    role            :: role(),
    mode            :: mode(),
    owner           :: pid(),
    owner_ref       :: reference(),
    peer            :: peer(),
    %% Recv side: inbound chunks queued, waiting recv/2 callers, eof flag
    inbox = queue:new() :: queue:queue({encoding(), term()}),
    waiters = queue:new() :: queue:queue({{pid(), reference()}, reference()}),
    closed_recv = false :: boolean(),
    %% Send side
    closed_send = false :: boolean(),
    seq_out = 0 :: non_neg_integer(),
    seq_in  = 0 :: non_neg_integer(),
    %% Terminal reply (for client-stream / bidi)
    reply = undefined :: undefined | result(),
    reply_waiters = [] :: [{pid(), reference()}],
    %% `{remote_via_link, _, _}' carrier: the node key that signs this
    %% session's frames (`redacted' only in status output), the dedicated
    %% QUIC stream once the link hands it over, the encoded frames that
    %% stream has not taken yet (each with whether it is the session's
    %% last frame, and the `send' caller waiting on it), and the transport
    %% error that ended the session.
    signer = undefined :: undefined | redacted | macula_identity:key_pair(),
    link_stream = undefined :: undefined | reference(),
    held = queue:new() :: queue:queue({binary(), boolean(), undefined | gen_server:from()}),
    transport_error = undefined :: undefined | {error, {transport, term()}}
}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Start a stream gen_server.
%%
%% Required opts: id, role, mode, owner.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Pair two stream processes as peers (Phase 1 local dispatch).
-spec pair(pid(), pid()) -> ok.
pair(A, B) when is_pid(A), is_pid(B) ->
    ok = gen_server:call(A, {pair_local, B}),
    ok = gen_server:call(B, {pair_local, A}),
    ok.

%% @doc Attach a V2 `macula_station_link' peer to this stream. The
%% stream signs its outbound V2 `macula_frame:stream_*' frames with
%% `Signer' and writes them on the dedicated QUIC stream the link hands
%% over as `{dedicated_stream, StreamId, QuicStream}'; frames sent before
%% that wait in order. Inbound STREAM_* frames are decoded by the link
%% and forwarded into this stream via the deliver_chunk / end / error /
%% reply casts below.
-spec attach_to_link(pid(), pid(), stream_id(), macula_identity:key_pair()) -> ok.
attach_to_link(StreamPid, LinkPid, StreamId, Signer)
  when is_pid(StreamPid), is_pid(LinkPid), is_binary(StreamId) ->
    gen_server:call(StreamPid, {pair_via_link, LinkPid, StreamId, Signer}).

%% @doc Send a binary chunk on the stream. On a stream carried by a
%% `macula_station_link' the call returns once the dedicated QUIC stream
%% has taken the chunk, and waits, with no timeout, while that stream
%% takes no more data.
-spec send(pid(), binary()) -> ok | {error, term()}.
send(Pid, Bin) when is_binary(Bin) ->
    send(Pid, Bin, raw).

-spec send(pid(), binary() | term(), encoding()) -> ok | {error, term()}.
send(Pid, Body, raw) when is_binary(Body) ->
    gen_server:call(Pid, {send, raw, Body}, infinity);
send(Pid, Body, msgpack) ->
    gen_server:call(Pid, {send, msgpack, Body}, infinity).

%% @doc Receive the next chunk (blocks indefinitely).
-spec recv(pid()) -> {chunk, binary()}
                   | {data, term()}
                   | eof
                   | {error, term()}.
recv(Pid) ->
    recv(Pid, infinity).

-spec recv(pid(), timeout()) -> {chunk, binary()}
                              | {data, term()}
                              | eof
                              | {error, term()}.
recv(Pid, Timeout) ->
    %% Long timeouts allowed because the wait is on inbound network
    %% data, not on the gen_server's processing time.
    GsTimeout = case Timeout of
                    infinity -> infinity;
                    N when is_integer(N) -> N + 100
                end,
    gen_server:call(Pid, {recv, Timeout}, GsTimeout).

%% @doc Half-close the write side. Recv side stays open.
-spec close_send(pid()) -> ok.
close_send(Pid) ->
    gen_server:call(Pid, close_send).

%% @doc Close both sides. Idempotent.
-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:call(Pid, close).

%% @doc Wait for the terminal reply (client-stream / bidi).
-spec await_reply(pid()) -> result().
await_reply(Pid) ->
    await_reply(Pid, infinity).

-spec await_reply(pid(), timeout()) -> result() | {error, timeout}.
await_reply(Pid, Timeout) ->
    GsTimeout = case Timeout of
                    infinity -> infinity;
                    N when is_integer(N) -> N + 100
                end,
    gen_server:call(Pid, {await_reply, Timeout}, GsTimeout).

%% @doc Server-side: emit the terminal reply.
-spec set_reply(pid(), term()) -> ok.
set_reply(Pid, Result) ->
    gen_server:call(Pid, {set_reply, {ok, Result}}).

%% @doc Server-side: emit a terminal error as the reply value.
-spec set_error(pid(), term()) -> ok.
set_error(Pid, Reason) ->
    gen_server:call(Pid, {set_reply, {error, Reason}}).

%% @doc Abort the stream with a STREAM_ERROR frame. Both sides close;
%% any pending recv/await_reply waiters receive {error, {Code, Message}}.
-spec abort(pid(), binary(), binary()) -> ok.
abort(Pid, Code, Message) when is_binary(Code), is_binary(Message) ->
    gen_server:call(Pid, {abort, Code, Message}).

%% @doc Inspect stream state (debugging).
-spec info(pid()) -> map().
info(Pid) ->
    gen_server:call(Pid, info).

%%%===================================================================
%%% Peer-to-peer protocol
%%%===================================================================

%% @doc Deliver a chunk frame from the peer.
-spec deliver_chunk(pid(), encoding(), term()) -> ok.
deliver_chunk(Pid, Encoding, Body) ->
    gen_server:cast(Pid, {peer_chunk, Encoding, Body}).

%% @doc Deliver a STREAM_END frame from the peer.
-spec deliver_end(pid(), send | both) -> ok.
deliver_end(Pid, Role) ->
    gen_server:cast(Pid, {peer_end, Role}).

%% @doc Deliver a STREAM_ERROR frame from the peer.
-spec deliver_error(pid(), binary(), binary()) -> ok.
deliver_error(Pid, Code, Message) ->
    gen_server:cast(Pid, {peer_error, Code, Message}).

%% @doc Deliver a STREAM_REPLY frame from the peer.
-spec deliver_reply(pid(), result()) -> ok.
deliver_reply(Pid, Result) ->
    gen_server:cast(Pid, {peer_reply, Result}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Id = maps:get(id, Opts),
    Role = maps:get(role, Opts),
    Mode = maps:get(mode, Opts),
    Owner = maps:get(owner, Opts),
    OwnerRef = erlang:monitor(process, Owner),
    {ok, #state{
        id = Id,
        role = Role,
        mode = Mode,
        owner = Owner,
        owner_ref = OwnerRef
    }}.

%% --- pair --------------------------------------------------------------

handle_call({pair_local, Peer}, _From, State) ->
    _ = erlang:monitor(process, Peer),
    {reply, ok, State#state{peer = {local, Peer}}};
handle_call({pair_via_link, LinkPid, StreamId, Signer}, _From, State) ->
    _ = erlang:monitor(process, LinkPid),
    {reply, ok, State#state{peer = {remote_via_link, LinkPid, StreamId}, signer = Signer}};

%% --- send --------------------------------------------------------------

handle_call({send, _Encoding, _Body}, _From, #state{closed_send = true} = State) ->
    {reply, {error, send_closed}, State};
handle_call({send, Encoding, Body}, From, State) ->
    sent(forward_to_peer(State, {chunk, Encoding, Body}, From));

%% --- recv --------------------------------------------------------------

handle_call({recv, Timeout}, From, State) ->
    handle_recv(From, Timeout, State);

%% --- close_send --------------------------------------------------------

handle_call(close_send, _From, State) ->
    State1 = case State#state.closed_send of
                 true -> State;
                 false ->
                     {_, Forwarded} = forward_to_peer(State, {end_stream, send}, undefined),
                     Forwarded#state{closed_send = true}
             end,
    {reply, ok, State1};

%% --- close -------------------------------------------------------------

handle_call(close, _From, State) ->
    {_, Forwarded} = forward_to_peer(State, {end_stream, both}, undefined),
    State1 = Forwarded#state{closed_send = true, closed_recv = true},
    State2 = drain_waiters(eof, State1),
    {reply, ok, State2};

%% --- await_reply -------------------------------------------------------

handle_call({await_reply, _Timeout}, _From, #state{reply = {ok, _} = R} = State) ->
    {reply, R, State};
handle_call({await_reply, _Timeout}, _From, #state{reply = {error, _} = R} = State) ->
    {reply, R, State};
handle_call({await_reply, Timeout}, From, State) ->
    Ref = case Timeout of
              infinity -> undefined;
              N -> erlang:send_after(N, self(), {reply_timeout, From})
          end,
    Waiters = [{From, Ref} | State#state.reply_waiters],
    {noreply, State#state{reply_waiters = Waiters}};

%% --- set_reply ---------------------------------------------------------

handle_call({set_reply, Result}, _From, State) ->
    State1 = case State#state.reply of
                 undefined ->
                     {_, Forwarded} = forward_to_peer(State, {reply, Result}, undefined),
                     Forwarded#state{reply = Result};
                 _ ->
                     State
             end,
    {reply, ok, State1};

handle_call({abort, Code, Message}, _From, State) ->
    Err = {error, {Code, Message}},
    {_, Forwarded} = forward_to_peer(State, {error, Code, Message}, undefined),
    State1 = Forwarded#state{closed_recv = true, closed_send = true,
                         reply = case State#state.reply of
                                     undefined -> Err;
                                     R -> R
                                 end},
    State2 = drain_waiters(Err, State1),
    State3 = settle_reply_waiters_with(Err, State2),
    {reply, ok, State3};

%% --- info --------------------------------------------------------------

handle_call(info, _From, State) ->
    Map = #{
        id => State#state.id,
        role => State#state.role,
        mode => State#state.mode,
        peer => State#state.peer,
        inbox_size => queue:len(State#state.inbox),
        waiters => queue:len(State#state.waiters),
        closed_recv => State#state.closed_recv,
        closed_send => State#state.closed_send,
        seq_out => State#state.seq_out,
        seq_in => State#state.seq_in,
        reply => State#state.reply,
        held_frames => queue:len(State#state.held)
    },
    {reply, Map, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown}, State}.

%% --- peer-delivered events --------------------------------------------

handle_cast({peer_chunk, _Encoding, _Body}, #state{closed_recv = true} = State) ->
    {noreply, State};
handle_cast({peer_chunk, Encoding, Body}, State) ->
    State1 = enqueue_or_deliver(Encoding, Body, State),
    {noreply, State1#state{seq_in = State1#state.seq_in + 1}};

handle_cast({peer_end, send}, State) ->
    %% Peer half-closed: no more inbound data
    State1 = State#state{closed_recv = true},
    State2 = drain_waiters(eof, State1),
    {noreply, State2};
handle_cast({peer_end, both}, State) ->
    State1 = State#state{closed_recv = true, closed_send = true},
    State2 = drain_waiters(eof, State1),
    State3 = settle_reply_waiters_with({error, peer_closed}, State2),
    {noreply, State3};

handle_cast({peer_error, Code, Message}, State) ->
    Err = {error, {Code, Message}},
    State1 = State#state{closed_recv = true, closed_send = true},
    State2 = drain_waiters(Err, State1),
    State3 = settle_reply_waiters_with(Err, State2),
    {noreply, State3};

handle_cast({peer_reply, Result}, State) ->
    State1 = State#state{reply = Result},
    State2 = settle_reply_waiters_with(Result, State1),
    {noreply, State2};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --- info / monitors / timers -----------------------------------------

handle_info({recv_timeout, From}, State) ->
    %% Drop this waiter and reply timeout — only if it's still queued
    {Replied, NewQ} = drop_waiter_and_reply(From, {error, timeout}, State#state.waiters),
    case Replied of
        true -> ok;
        false -> ok       % already served
    end,
    {noreply, State#state{waiters = NewQ}};

handle_info({reply_timeout, From}, State) ->
    NewWaiters = lists:filter(
                   fun({F, _Ref}) when F =:= From ->
                       gen_server:reply(F, {error, timeout}),
                       false;
                      (_) -> true
                   end, State#state.reply_waiters),
    {noreply, State#state{reply_waiters = NewWaiters}};

%% The link hands over this session's dedicated QUIC stream: held frames
%% go out on it, oldest first.
handle_info({dedicated_stream, Sid, Stream}, #state{id = Sid} = State) ->
    {noreply, flush_held(State#state{link_stream = Stream})};

%% The dedicated stream takes data again after answering busy.
handle_info({quic, send_ready, Stream, undefined}, #state{link_stream = Stream} = State) ->
    {noreply, flush_held(State)};

%% A write of this session's bytes failed, or its dedicated stream could
%% not be opened. The session ends once, with the transport error.
handle_info({stream_write_failed, Sid, Reason}, #state{id = Sid} = State) ->
    {noreply, write_failed({error, {transport, Reason}}, State)};

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    IsOwner = Ref =:= State#state.owner_ref andalso
              Pid =:= State#state.owner,
    handle_down(IsOwner, Pid, State);

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

%% The signer is the node's private key: status output and crash reports
%% show it redacted, in the state and in the call that attached it.
format_status(Status) ->
    maps:map(fun redact_signer/2, Status).

redact_signer(state, #state{signer = Signer} = State) when Signer =/= undefined ->
    State#state{signer = redacted};
redact_signer(message, {'$gen_call', From, {pair_via_link, Link, Sid, _Signer}}) ->
    {'$gen_call', From, {pair_via_link, Link, Sid, redacted}};
redact_signer(_Key, Value) ->
    Value.

%%%===================================================================
%%% Internal helpers
%%%===================================================================

%% @private `ok': the peer has the chunk, or the QUIC stream took it;
%% `held': the caller is answered once the stream takes it.
sent({ok, State}) ->
    {reply, ok, State#state{seq_out = State#state.seq_out + 1}};
sent({held, State}) ->
    {noreply, State#state{seq_out = State#state.seq_out + 1}};
sent({{error, _} = Error, State}) ->
    {reply, Error, State}.

%% @private Dispatch a stream-level action to the peer, and return how it
%% went with the new state.
%%
%% Peer-shape-aware:
%%   {local, Pid}             — in-process pair; cast the symmetric
%%                              deliver_* helper directly.
%%   {remote_via_link, L, Sid}— build and sign the `macula_frame:stream_*'
%%                              frame and write it on the dedicated QUIC
%%                              stream, or hold it in order.
%%
%% Action shapes:
%%   {chunk, Encoding, Body}
%%   {end_stream, send | both}
%%   {error, Code, Message}
%%   {reply, Result}
%%
%% Results: `ok' (the peer has it, or the QUIC stream took it), `held' (it
%% waits here, and `From', when defined, is answered once the stream takes
%% it), or `{error, Reason}'.
forward_to_peer(#state{peer = undefined} = S, _Action, _From) ->
    {{error, no_peer}, S};
forward_to_peer(#state{peer = {local, Pid}} = S, Action, _From) ->
    {deliver_locally(Pid, Action), S};
forward_to_peer(#state{transport_error = {error, _} = Error} = S, _Action, _From) ->
    {Error, S};
forward_to_peer(#state{peer = {remote_via_link, _Link, Sid}} = S, Action, From) ->
    write_frame(link_frame(Sid, Action, S), last_frame(Action), From, S).

deliver_locally(Pid, {chunk, Encoding, Body}) -> deliver_chunk(Pid, Encoding, Body);
deliver_locally(Pid, {end_stream, Role})      -> deliver_end(Pid, Role);
deliver_locally(Pid, {error, Code, Message})  -> deliver_error(Pid, Code, Message);
deliver_locally(Pid, {reply, Result})         -> deliver_reply(Pid, Result).

%% @private The V2 frame for an action. STREAM_DATA / END / ERROR name
%% the signer so every station on the path can verify them; STREAM_REPLY
%% names it as `responded_by'.
link_frame(Sid, {chunk, Encoding, Body}, #state{seq_out = Seq, signer = Signer}) ->
    macula_frame:stream_data(#{stream_id => Sid, seq => Seq, encoding => Encoding,
                               body => Body, signer => macula_identity:public(Signer)});
link_frame(Sid, {end_stream, Role}, #state{signer = Signer}) ->
    macula_frame:stream_end(#{stream_id => Sid, role => Role,
                              signer => macula_identity:public(Signer)});
link_frame(Sid, {error, Code, Message}, #state{signer = Signer}) ->
    macula_frame:stream_error(#{stream_id => Sid, code => Code, message => Message,
                                signer => macula_identity:public(Signer)});
link_frame(Sid, {reply, Result}, #state{signer = Signer}) ->
    macula_frame:stream_reply(#{stream_id => Sid, payload => reply_payload(Result),
                                responded_by => macula_identity:public(Signer)}).

reply_payload({ok, Value}) -> Value;
reply_payload({error, _Reason} = Error) -> Error.

%% @private The session's last frame from this side: a full STREAM_END, a
%% STREAM_ERROR or a STREAM_REPLY.
last_frame({end_stream, both}) -> true;
last_frame({error, _Code, _Message}) -> true;
last_frame({reply, _Result}) -> true;
last_frame(_Action) -> false.

write_frame(Frame, Last, From, S) ->
    encoded(macula_frame:check_frame(Frame), Frame, Last, From, S).

encoded(ok, Frame, Last, From, #state{signer = Signer} = S) ->
    queue_bytes(macula_frame:encode(macula_frame:sign(Frame, Signer)), Last, From, S);
encoded({error, _} = Refused, _Frame, _Last, _From, S) ->
    {Refused, S}.

%% @private A frame goes straight to the QUIC stream once the stream is
%% handed over and nothing is held before it; otherwise, or when the stream
%% answers busy, it waits in `held', in order.
queue_bytes(Bytes, Last, From, #state{link_stream = Stream, held = Held} = S) ->
    written_now(Stream =/= undefined andalso queue:is_empty(Held), Bytes, Last, From, S).

written_now(true, Bytes, Last, From, #state{link_stream = Stream} = S) ->
    taken_now(macula_quic:async_send(Stream, Bytes), Bytes, Last, From, S);
written_now(false, Bytes, Last, From, S) ->
    {held, hold(Bytes, Last, From, S)}.

taken_now(ok, _Bytes, Last, _From, S) ->
    {ok, after_written(Last, S)};
taken_now({error, busy}, Bytes, Last, From, S) ->
    {held, hold(Bytes, Last, From, S)};
taken_now({error, already_closed}, _Bytes, _Last, _From, S) ->
    {{error, send_closed}, closed_under_the_session(S)};
taken_now({error, Reason}, _Bytes, _Last, _From, S) ->
    Error = {error, {transport, Reason}},
    {Error, write_failed(Error, S)}.

hold(Bytes, Last, From, #state{held = Held} = S) ->
    S#state{held = queue:in({Bytes, Last, From}, Held)}.

%% @private Send held frames, oldest first, until the stream answers busy.
flush_held(#state{link_stream = Stream, held = Held} = S) ->
    flush_next(queue:peek(Held), Stream, S).

flush_next(empty, _Stream, S) ->
    S;
flush_next({value, {Bytes, Last, From}}, Stream, S) ->
    flush_taken(macula_quic:async_send(Stream, Bytes), Last, From, S).

flush_taken(ok, Last, From, #state{held = Held} = S) ->
    ok = answer(From, ok),
    flush_held(after_written(Last, S#state{held = queue:drop(Held)}));
flush_taken({error, busy}, _Last, _From, S) ->
    S;
flush_taken({error, already_closed}, _Last, _From, S) ->
    closed_under_the_session(S);
flush_taken({error, Reason}, _Last, _From, S) ->
    write_failed({error, {transport, Reason}}, S).

%% @private Once the QUIC stream took the session's last frame, the link
%% may close that stream gracefully.
after_written(true, #state{peer = {remote_via_link, Link, Sid}} = S) ->
    ok = macula_station_link:stream_finished(Link, Sid),
    S;
after_written(false, S) ->
    S.

%% @private The link closed or reset the stream first: held and later send
%% callers are answered `{error, send_closed}'.
closed_under_the_session(#state{held = Held} = S) ->
    ok = answer_held(Held, {error, send_closed}),
    S#state{held = queue:new(), closed_send = true}.

%% @private A write failed, or the stream could not be opened: the session
%% ends once, with the transport error, for its held send callers, its
%% readers and its reply waiters. A later failure changes nothing.
write_failed(Error, #state{transport_error = undefined, held = Held} = S) ->
    ok = answer_held(Held, Error),
    S1 = S#state{transport_error = Error, held = queue:new(),
                 closed_recv = true, closed_send = true,
                 reply = first_reply(S#state.reply, Error)},
    settle_reply_waiters_with(Error, drain_waiters(Error, S1));
write_failed(_Error, S) ->
    S.

first_reply(undefined, Error) -> Error;
first_reply(Reply, _Error) -> Reply.

answer_held(Held, Reply) ->
    lists:foreach(fun({_Bytes, _Last, From}) -> answer(From, Reply) end, queue:to_list(Held)).

answer(undefined, _Reply) -> ok;
answer(From, Reply) -> gen_server:reply(From, Reply).

%% @private Owner DOWN → stop. Otherwise check whether the dead pid
%% was our peer (or our peer's mesh_client for remote peers) and, if
%% so, surface as a stream error to any local readers / reply waiters.
handle_down(true, _Pid, State) ->
    {stop, normal, State};
handle_down(false, Pid, #state{peer = {local, Pid}} = State) ->
    propagate_peer_down(State);
handle_down(false, Pid, #state{peer = {remote_via_link, Pid, _Sid}} = State) ->
    propagate_peer_down(State);
handle_down(false, _Pid, State) ->
    {noreply, State}.

propagate_peer_down(State) ->
    Err = {error, peer_down},
    ok = answer_held(State#state.held, Err),
    State1 = State#state{closed_recv = true, closed_send = true,
                         peer = undefined, held = queue:new()},
    State2 = drain_waiters(Err, State1),
    State3 = settle_reply_waiters_with(Err, State2),
    {noreply, State3}.

%% @doc Either deliver a chunk to a waiting recv/2 caller or queue it.
enqueue_or_deliver(Encoding, Body, #state{waiters = W0} = State) ->
    case queue:out(W0) of
        {{value, {From, Ref}}, W1} ->
            cancel_timer(Ref),
            gen_server:reply(From, chunk_to_recv_result(Encoding, Body)),
            State#state{waiters = W1};
        {empty, _} ->
            Inbox = queue:in({Encoding, Body}, State#state.inbox),
            State#state{inbox = Inbox}
    end.

handle_recv(From, _Timeout, #state{inbox = Inbox} = State) ->
    case queue:out(Inbox) of
        {{value, {Encoding, Body}}, Rest} ->
            {reply, chunk_to_recv_result(Encoding, Body), State#state{inbox = Rest}};
        {empty, _} when State#state.closed_recv ->
            {reply, eof, State};
        {empty, _} ->
            queue_waiter(From, _Timeout, State)
    end.

queue_waiter(From, Timeout, State) ->
    Ref = case Timeout of
              infinity -> undefined;
              0 -> immediate;
              N when is_integer(N) -> erlang:send_after(N, self(), {recv_timeout, From})
          end,
    case Ref of
        immediate ->
            {reply, {error, would_block}, State};
        _ ->
            Waiters = queue:in({From, Ref}, State#state.waiters),
            {noreply, State#state{waiters = Waiters}}
    end.

chunk_to_recv_result(raw, Body) -> {chunk, Body};
chunk_to_recv_result(msgpack, Body) -> {data, Body};
chunk_to_recv_result(Other, Body) -> {data, {Other, Body}}.

drain_waiters(Reply, State) ->
    drain_waiters(Reply, State#state.waiters, State).

drain_waiters(Reply, Q, State) ->
    case queue:out(Q) of
        {{value, {From, Ref}}, Rest} ->
            cancel_timer(Ref),
            gen_server:reply(From, Reply),
            drain_waiters(Reply, Rest, State#state{waiters = Rest});
        {empty, _} ->
            State#state{waiters = queue:new()}
    end.

settle_reply_waiters_with(Result, State) ->
    lists:foreach(
      fun({From, Ref}) ->
              cancel_timer(Ref),
              gen_server:reply(From, Result)
      end, State#state.reply_waiters),
    State#state{reply_waiters = []}.

drop_waiter_and_reply(From, Reply, Q) ->
    %% Walk the queue once, dropping the matching waiter and replying.
    L = queue:to_list(Q),
    {Match, Rest} = lists:partition(fun({F, _R}) -> F =:= From end, L),
    case Match of
        [{F, _Ref}] ->
            gen_server:reply(F, Reply),
            {true, queue:from_list(Rest)};
        _ ->
            {false, Q}
    end.

cancel_timer(undefined) -> ok;
cancel_timer(immediate) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref), ok.
