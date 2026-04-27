%%%-------------------------------------------------------------------
%%% @doc Macula streaming RPC — single-stream state machine.
%%%
%%% Owns one streaming RPC's state. Each call_stream, open_stream,
%%% or server-side handler invocation gets its own macula_stream
%%% gen_server. Phase 1 only supports the LOCAL dispatch path: the
%%% client-side and server-side macula_stream processes live in the
%%% same BEAM and are paired with pair/2. Phase 2 swaps the local
%%% peer for a QUIC stream owned by macula_mesh_client; the public
%%% API stays unchanged.
%%%
%%% See PLAN_MACULA_STREAMING.md (macula-architecture/plans).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_v1).

-behaviour(gen_server).

%% Public API
-export([
    start_link/1,
    pair/2,
    attach_remote/3,
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

%% Peer-to-peer protocol (also used by macula_mesh_client when bridging
%% to QUIC in Phase 2)
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
    terminate/2
]).

-type role() :: client | server.
-type mode() :: server_stream | client_stream | bidi.
-type encoding() :: raw | msgpack.
-type chunk() :: binary() | {raw, binary()} | {term, term()}.
-type stream_id() :: binary().
-type result() :: {ok, term()} | {error, term()}.

%% Peer shape (Phase 2+):
%%   undefined             — unpaired
%%   {local, Pid}          — Phase 1 in-process pairing
%%   {remote, Client, Sid} — Phase 2 QUIC: peer lives on another node,
%%                           deliveries are encoded as STREAM_* frames
%%                           and sent out the mesh_client's QUIC stream.
-type peer() :: undefined
              | {local, pid()}
              | {remote, pid(), stream_id()}.

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
    reply_waiters = [] :: [{pid(), reference()}]
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

%% @doc Attach a remote (QUIC) peer to this stream. Used by
%% macula_mesh_client to wire a stream to its QUIC-carrier: all
%% deliveries out of this stream will be encoded as STREAM_* frames
%% and sent through Client's relay connection, tagged with
%% StreamId. Deliveries INTO this stream still arrive via the
%% deliver_chunk/end/error/reply casts below — mesh_client decodes
%% the incoming frames and forwards them to the local stream pid it
%% tracks for this StreamId.
-spec attach_remote(pid(), pid(), stream_id()) -> ok.
attach_remote(StreamPid, ClientPid, StreamId)
  when is_pid(StreamPid), is_pid(ClientPid), is_binary(StreamId) ->
    gen_server:call(StreamPid, {pair_remote, ClientPid, StreamId}).

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
handle_call({pair_remote, ClientPid, StreamId}, _From, State) ->
    _ = erlang:monitor(process, ClientPid),
    {reply, ok, State#state{peer = {remote, ClientPid, StreamId}}};

%% --- send --------------------------------------------------------------

handle_call({send, _Encoding, _Body}, _From, #state{closed_send = true} = State) ->
    {reply, {error, send_closed}, State};
handle_call({send, Encoding, Body}, _From, State) ->
    case forward_to_peer(State, {chunk, Encoding, Body}) of
        ok ->
            {reply, ok, State#state{seq_out = State#state.seq_out + 1}};
        {error, _} = Err ->
            {reply, Err, State}
    end;

%% --- recv --------------------------------------------------------------

handle_call({recv, Timeout}, From, State) ->
    handle_recv(From, Timeout, State);

%% --- close_send --------------------------------------------------------

handle_call(close_send, _From, State) ->
    State1 = case State#state.closed_send of
                 true -> State;
                 false ->
                     _ = forward_to_peer(State, {end_stream, send}),
                     State#state{closed_send = true}
             end,
    {reply, ok, State1};

%% --- close -------------------------------------------------------------

handle_call(close, _From, State) ->
    _ = forward_to_peer(State, {end_stream, both}),
    State1 = State#state{closed_send = true, closed_recv = true},
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
                     _ = forward_to_peer(State, {reply, Result}),
                     State#state{reply = Result};
                 _ ->
                     State
             end,
    {reply, ok, State1};

handle_call({abort, Code, Message}, _From, State) ->
    Err = {error, {Code, Message}},
    _ = forward_to_peer(State, {error, Code, Message}),
    State1 = State#state{closed_recv = true, closed_send = true,
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

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    IsOwner = Ref =:= State#state.owner_ref andalso
              Pid =:= State#state.owner,
    handle_down(IsOwner, Pid, State);

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

%%%===================================================================
%%% Internal helpers
%%%===================================================================

%% @private Dispatch a stream-level action to the peer.
%%
%% Peer-shape-aware:
%%   {local, Pid}          — in-process pair; cast the symmetric
%%                           deliver_* helper directly.
%%   {remote, Client, Sid} — hand off to macula_mesh_client which
%%                           encodes the action as a STREAM_* frame
%%                           and sends it out the QUIC stream.
%%
%% Action shapes:
%%   {chunk, Encoding, Body}
%%   {end_stream, send | both}
%%   {error, Code, Message}
%%   {reply, Result}
forward_to_peer(#state{peer = undefined}, _Action) ->
    {error, no_peer};
forward_to_peer(#state{peer = {local, Pid}}, {chunk, Encoding, Body}) ->
    deliver_chunk(Pid, Encoding, Body);
forward_to_peer(#state{peer = {local, Pid}}, {end_stream, Role}) ->
    deliver_end(Pid, Role);
forward_to_peer(#state{peer = {local, Pid}}, {error, Code, Message}) ->
    deliver_error(Pid, Code, Message);
forward_to_peer(#state{peer = {local, Pid}}, {reply, Result}) ->
    deliver_reply(Pid, Result);
forward_to_peer(#state{peer = {remote, Client, Sid}} = S, Action) ->
    send_remote(Client, Sid, Action, S#state.seq_out).

send_remote(Client, Sid, {chunk, Encoding, Body}, Seq) ->
    macula_mesh_client:send_stream_frame(Client, stream_data, #{
        stream_id => Sid,
        seq => Seq,
        body => Body,
        encoding => Encoding
    });
send_remote(Client, Sid, {end_stream, Role}, _Seq) ->
    macula_mesh_client:send_stream_frame(Client, stream_end, #{
        stream_id => Sid,
        role => Role
    });
send_remote(Client, Sid, {error, Code, Message}, _Seq) ->
    macula_mesh_client:send_stream_frame(Client, stream_error, #{
        stream_id => Sid,
        code => Code,
        message => Message
    });
send_remote(Client, Sid, {reply, {ok, Value}}, _Seq) ->
    macula_mesh_client:send_stream_frame(Client, stream_reply, #{
        stream_id => Sid,
        result => Value
    });
send_remote(Client, Sid, {reply, {error, Reason}}, _Seq) ->
    macula_mesh_client:send_stream_frame(Client, stream_reply, #{
        stream_id => Sid,
        error => #{code => <<"error">>,
                   message => iolist_to_binary(io_lib:format("~p", [Reason]))}
    }).

%% @private Owner DOWN → stop. Otherwise check whether the dead pid
%% was our peer (or our peer's mesh_client for remote peers) and, if
%% so, surface as a stream error to any local readers / reply waiters.
handle_down(true, _Pid, State) ->
    {stop, normal, State};
handle_down(false, Pid, #state{peer = {local, Pid}} = State) ->
    propagate_peer_down(State);
handle_down(false, Pid, #state{peer = {remote, Pid, _Sid}} = State) ->
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
