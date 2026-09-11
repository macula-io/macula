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
%%%   <li>`{remote_via_link, Link, Sid}': frames over a peering
%%%       connection through `macula_station_link'. The stream signs
%%%       and numbers its own frames with the node identity key it is
%%%       started with, from 0 across STREAM_DATA, STREAM_END,
%%%       STREAM_ERROR and STREAM_REPLY, and hands the link their
%%%       bytes. It verifies each frame from the peer against its
%%%       STREAM_OPEN before the frame takes effect, and reports a
%%%       refused frame to its peering connection.</li>
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
    attach_to_link/3,
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
    deliver_reply/2,
    deliver_frame/2
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
%%   {remote_via_link, L, Sid}: the station_link carrier. The stream
%%                              signs its frames and hands the link
%%                              their bytes.
-type peer() :: undefined
              | {local, pid()}
              | {remote_via_link, pid(), stream_id()}.

-export_type([role/0, mode/0, encoding/0, chunk/0, stream_id/0, result/0,
              peer/0]).

%% A STREAM_ERROR message is text for people of at most 256 bytes, as a
%% GOODBYE reason is.
-define(MAX_ERROR_TEXT_BYTES, 256).

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
    %% A link-carried stream: the node identity key it signs with, its
    %% verified STREAM_OPEN, the peering connection it reports refused
    %% frames to, the crypto profile, and what it has verified of the
    %% peer's frames so far.
    key      :: macula_node_keys:node_key() | undefined,
    open     :: macula_frame:verified_request() | undefined,
    conn     :: pid() | undefined,
    profile  :: macula_crypto_profile:profile() | undefined,
    verifier :: macula_frame:stream_state() | undefined
}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Start a stream gen_server.
%%
%% Required opts: id, role, mode, owner. A stream carried by a
%% `macula_station_link' also takes `key' (the node identity key it
%% signs with), `open' (the verified STREAM_OPEN), `conn' (the peering
%% connection that carries it) and `profile'.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Pair two stream processes as peers (Phase 1 local dispatch).
-spec pair(pid(), pid()) -> ok.
pair(A, B) when is_pid(A), is_pid(B) ->
    ok = gen_server:call(A, {pair_local, B}),
    ok = gen_server:call(B, {pair_local, A}),
    ok.

%% @doc Attach a `macula_station_link' peer to this stream. The stream
%% hands the link the bytes of each frame it signs, and the link
%% forwards the peer's STREAM_* frames into it through
%% `deliver_frame/2'.
-spec attach_to_link(pid(), pid(), stream_id()) -> ok.
attach_to_link(StreamPid, LinkPid, StreamId)
  when is_pid(StreamPid), is_pid(LinkPid), is_binary(StreamId) ->
    gen_server:call(StreamPid, {pair_via_link, LinkPid, StreamId}).

%% @doc Send a binary chunk on the stream.
-spec send(pid(), binary()) -> ok | {error, term()}.
send(Pid, Bin) when is_binary(Bin) ->
    send(Pid, Bin, raw).

-spec send(pid(), binary() | term(), encoding()) -> ok | {error, term()}.
send(Pid, Body, raw) when is_binary(Body) ->
    gen_server:call(Pid, {send, raw, Body});
send(Pid, Body, msgpack) ->
    gen_server:call(Pid, {send, msgpack, Body}).

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

%% @doc Deliver a STREAM_DATA, STREAM_END, STREAM_ERROR or STREAM_REPLY
%% frame of a link-carried stream from the peer. The stream verifies it
%% against its STREAM_OPEN before it takes effect. A refused frame is
%% dropped and reported to the stream's peering connection, and the
%% stream carries on.
-spec deliver_frame(pid(), macula_frame:frame()) -> ok.
deliver_frame(Pid, Frame) when is_map(Frame) ->
    gen_server:cast(Pid, {peer_frame, Frame}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Id = maps:get(id, Opts),
    Role = maps:get(role, Opts),
    Mode = maps:get(mode, Opts),
    Owner = maps:get(owner, Opts),
    OwnerRef = erlang:monitor(process, Owner),
    {ok, carried(Opts, #state{
        id = Id,
        role = Role,
        mode = Mode,
        owner = Owner,
        owner_ref = OwnerRef
    })}.

%% --- pair --------------------------------------------------------------

handle_call({pair_local, Peer}, _From, State) ->
    _ = erlang:monitor(process, Peer),
    {reply, ok, State#state{peer = {local, Peer}}};
handle_call({pair_via_link, LinkPid, StreamId}, _From, State) ->
    _ = erlang:monitor(process, LinkPid),
    {reply, ok, State#state{peer = {remote_via_link, LinkPid, StreamId}}};

%% --- send --------------------------------------------------------------

handle_call({send, _Encoding, _Body}, _From, #state{closed_send = true} = State) ->
    {reply, {error, send_closed}, State};
handle_call({send, Encoding, Body}, _From, State) ->
    {Sent, State1} = forward_to_peer(State, {chunk, Encoding, Body}),
    {reply, Sent, State1};

%% --- recv --------------------------------------------------------------

handle_call({recv, Timeout}, From, State) ->
    handle_recv(From, Timeout, State);

%% --- close_send --------------------------------------------------------

handle_call(close_send, _From, #state{closed_send = true} = State) ->
    {reply, ok, State};
handle_call(close_send, _From, State) ->
    {_Sent, State1} = forward_to_peer(State, {end_stream, send}),
    {reply, ok, State1#state{closed_send = true}};

%% --- close -------------------------------------------------------------

handle_call(close, _From, State) ->
    {_Sent, State0} = forward_to_peer(State, {end_stream, both}),
    State1 = State0#state{closed_send = true, closed_recv = true},
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

handle_call({set_reply, Result}, _From, #state{reply = undefined} = State) ->
    replied(forward_to_peer(State, {reply, Result}), Result);
handle_call({set_reply, _Result}, _From, State) ->
    {reply, ok, State};

handle_call({abort, Code, Message}, _From, State) ->
    Err = {error, {Code, Message}},
    {_Sent, State0} = forward_to_peer(State, {error, Code, Message}),
    State1 = State0#state{closed_recv = true, closed_send = true,
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
        reply => State#state.reply
    },
    {reply, Map, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown}, State}.

%% --- peer-delivered events --------------------------------------------

handle_cast({peer_chunk, Encoding, Body}, State) ->
    {noreply, chunk_arrived(Encoding, Body, State)};
handle_cast({peer_end, Role}, State) when Role =:= send; Role =:= both ->
    {noreply, end_arrived(Role, State)};
handle_cast({peer_error, Code, Message}, State) ->
    {noreply, error_arrived(Code, Message, State)};
handle_cast({peer_reply, Result}, State) ->
    {noreply, reply_arrived(Result, State)};
handle_cast({peer_frame, Frame}, #state{role = Role, verifier = Verifier, profile = Profile} = State) ->
    {noreply, verified_frame(peer_verified(Role, Frame, Verifier, Profile), State)};

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

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    IsOwner = Ref =:= State#state.owner_ref andalso
              Pid =:= State#state.owner,
    handle_down(IsOwner, Pid, State);

%% A write of this stream's bytes failed on its link. The stream ends
%% here with a transport failure, which its readers and reply waiters
%% receive. It is not a refusal, so its connection hears nothing.
handle_info({stream_write_failed, Sid, Reason}, #state{id = Sid} = State) ->
    {noreply, write_failed({error, {transport, Reason}}, State)};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

%% A link-carried stream holds the node identity key: status output and
%% crash reports show it redacted.
format_status(Status) ->
    macula_node_keys:redacted(Status).

%%%===================================================================
%%% Internal helpers
%%%===================================================================

%% @private Dispatch a stream-level action to the peer, returning what
%% the send gave and the stream's next state.
%%
%% Peer-shape-aware:
%%   {local, Pid}: an in-process pair; cast the symmetric deliver_*
%%                 helper directly.
%%   {remote_via_link, L, Sid}: sign the frame with the stream's own key
%%                 and number, and hand the link its bytes. A side sends
%%                 nothing after its own STREAM_END.
%%
%% Action shapes:
%%   {chunk, Encoding, Body}
%%   {end_stream, send | both}
%%   {error, Code, Message}
%%   {reply, Result}
forward_to_peer(#state{peer = undefined} = S, _Action) ->
    {{error, no_peer}, S};
forward_to_peer(#state{peer = {local, Pid}} = S, {chunk, Encoding, Body}) ->
    {deliver_chunk(Pid, Encoding, Body), S#state{seq_out = S#state.seq_out + 1}};
forward_to_peer(#state{peer = {local, Pid}} = S, {end_stream, Role}) ->
    {deliver_end(Pid, Role), S};
forward_to_peer(#state{peer = {local, Pid}} = S, {error, Code, Message}) ->
    {deliver_error(Pid, Code, Message), S};
forward_to_peer(#state{peer = {local, Pid}} = S, {reply, Result}) ->
    {deliver_reply(Pid, Result), S};
forward_to_peer(#state{peer = {remote_via_link, _Link, _Sid}, closed_send = true} = S, _Action) ->
    {{error, send_closed}, S};
forward_to_peer(#state{peer = {remote_via_link, Link, Sid}, role = Role, mode = Mode} = S, Action) ->
    #{frame_type := Type} = Spec = frame_spec(Action, S#state.seq_out),
    sent_via_link(allowed(Role, Mode, Type), encodable(Spec), Spec, Link, Sid, S).

%% @private The frame a link-carried stream sends for an action, with its
%% next sequence number. An error reply travels as STREAM_ERROR with code
%% `error'.
frame_spec({chunk, Encoding, Body}, Seq) ->
    #{frame_type => stream_data, seq => Seq, encoding => Encoding, body => Body};
frame_spec({end_stream, Role}, Seq) ->
    #{frame_type => stream_end, seq => Seq, role => Role};
frame_spec({error, Code, Message}, Seq) ->
    #{frame_type => stream_error, seq => Seq, code => Code, message => error_text(Message)};
frame_spec({reply, {ok, Value}}, Seq) ->
    #{frame_type => stream_reply, seq => Seq, payload => Value};
frame_spec({reply, {error, Reason}}, Seq) ->
    #{frame_type => stream_error, seq => Seq, code => <<"error">>, message => error_text(Reason)}.

%% @private A reason as STREAM_ERROR message text: a binary, or an atom's
%% name, that is valid UTF-8 of at most 256 bytes. Anything else sends an
%% empty message, so no other term is rendered onto the wire.
error_text(Reason) when is_atom(Reason) ->
    error_text(atom_to_binary(Reason));
error_text(Reason) when is_binary(Reason), byte_size(Reason) =< ?MAX_ERROR_TEXT_BYTES ->
    valid_text(unicode:characters_to_binary(Reason));
error_text(_Reason) ->
    <<>>.

valid_text(Text) when is_binary(Text) -> Text;
valid_text(_Invalid) -> <<>>.

%% @private A caller sends no STREAM_REPLY, and no STREAM_DATA in a
%% server_stream.
allowed(client, _Mode, stream_reply) -> false;
allowed(client, server_stream, stream_data) -> false;
allowed(_Role, _Mode, _Type) -> true.

encodable(#{encoding := msgpack, body := Body}) -> macula_frame:check_payload(Body);
encodable(#{payload := Payload}) -> macula_frame:check_payload(Payload);
encodable(_Spec) -> ok.

sent_via_link(false, _Encodable, _Spec, _Link, _Sid, S) ->
    {{error, not_allowed}, S};
sent_via_link(true, {error, _} = Unsendable, _Spec, _Link, _Sid, S) ->
    {Unsendable, S};
sent_via_link(true, ok, Spec, Link, Sid, #state{seq_out = Seq} = S) ->
    Bytes = macula_frame:encode(signed_frame(Spec, S)),
    {macula_station_link:send_stream_bytes(Link, Sid, Bytes, last_frame(Spec)), S#state{seq_out = Seq + 1}}.

signed_frame(Spec, #state{role = server, key = Key, open = Open}) ->
    macula_frame:provider_stream(Spec, Key, Open);
signed_frame(Spec, #state{role = client, key = Key, open = Open}) ->
    macula_frame:caller_stream(Spec, Key, Open).

%% @private The last frame from a side, after which its link forgets the
%% stream.
last_frame(#{frame_type := stream_end, role := both}) -> true;
last_frame(#{frame_type := stream_error}) -> true;
last_frame(#{frame_type := stream_reply}) -> true;
last_frame(_Spec) -> false.

%% @private A reply the side may not send is refused and not recorded.
replied({{error, not_allowed} = Refused, State}, _Result) ->
    {reply, Refused, State};
replied({_Sent, State}, Result) ->
    {reply, ok, State#state{reply = Result}}.

%% @private A stream started with the key and STREAM_OPEN of a
%% link-carried stream signs, numbers and verifies its frames; a local
%% pair has none of these.
carried(#{key := Key, open := Open, conn := Conn, profile := Profile}, State) ->
    State#state{key = Key, open = Open, conn = Conn, profile = Profile, verifier = macula_frame:open_stream(Open)};
carried(_LocalPair, State) ->
    State.

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
    State1 = State#state{closed_recv = true, closed_send = true,
                         peer = undefined},
    State2 = drain_waiters(Err, State1),
    State3 = settle_reply_waiters_with(Err, State2),
    {noreply, State3}.

%% @private The effects of the peer's frames, the same for both carriers.
chunk_arrived(_Encoding, _Body, #state{closed_recv = true} = State) ->
    State;
chunk_arrived(Encoding, Body, State) ->
    State1 = enqueue_or_deliver(Encoding, Body, State),
    State1#state{seq_in = State1#state.seq_in + 1}.

%% Peer half-closed: no more inbound data.
end_arrived(send, State) ->
    drain_waiters(eof, State#state{closed_recv = true});
end_arrived(both, State) ->
    State1 = drain_waiters(eof, State#state{closed_recv = true, closed_send = true}),
    settle_reply_waiters_with({error, peer_closed}, State1).

error_arrived(Code, Message, State) ->
    ended_with({error, {Code, Message}}, State).

reply_arrived(Result, State) ->
    settle_reply_waiters_with(Result, State#state{reply = Result}).

%% Both sides closed, with every reader and reply waiter answered with Err.
ended_with(Err, State) ->
    State1 = drain_waiters(Err, State#state{closed_recv = true, closed_send = true}),
    settle_reply_waiters_with(Err, State1).

write_failed(Err, #state{reply = Reply} = State) ->
    ended_with(Err, State#state{peer = undefined, reply = first_result(Reply, Err)}).

first_result(undefined, Err) -> Err;
first_result(Reply, _Err) -> Reply.

%% @private A link-carried stream's caller side verifies the provider's
%% frames, and its provider side the caller's.
peer_verified(client, Frame, Verifier, Profile) ->
    macula_frame:verify_provider_stream(Frame, Verifier, Profile);
peer_verified(server, Frame, Verifier, Profile) ->
    macula_frame:verify_caller_stream(Frame, Verifier, Profile).

verified_frame({ok, Fields, Verifier}, State) ->
    peer_event(Fields, State#state{verifier = Verifier});
verified_frame({error, Refusal}, #state{conn = Conn} = State) ->
    ok = macula_peering:object_refused(Conn, Refusal),
    State.

peer_event(#{frame_type := stream_data, encoding := Encoding, body := Body}, State) ->
    chunk_arrived(Encoding, Body, State);
peer_event(#{frame_type := stream_end, role := Role}, State) ->
    end_arrived(Role, State);
peer_event(#{frame_type := stream_error, code := Code, message := Message}, State) ->
    error_arrived(Code, Message, State);
peer_event(#{frame_type := stream_reply, payload := Payload}, State) ->
    reply_arrived({ok, Payload}, State).

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
