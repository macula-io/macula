%%%-------------------------------------------------------------------
%%% @doc Macula streaming RPC — single-stream state machine.
%%%
%%% Owns one streaming RPC's state. Each `call_stream`, `open_stream`,
%%% or server-side handler invocation gets its own `macula_stream`
%%% gen_server. Phase 1 only supports the LOCAL dispatch path: the
%%% client-side and server-side `macula_stream` processes live in the
%%% same BEAM and are paired with `pair/2`. Phase 2 swaps the local
%%% peer for a QUIC stream owned by `macula_mesh_client`; the public
%%% API stays unchanged.
%%%
%%% See `PLAN_MACULA_STREAMING.md` (macula-architecture/plans).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream).

-behaviour(gen_server).

%% Public API
-export([
    start_link/1,
    pair/2,
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

-export_type([role/0, mode/0, encoding/0, chunk/0, stream_id/0, result/0]).

-record(state, {
    id              :: stream_id(),
    role            :: role(),
    mode            :: mode(),
    owner           :: pid(),
    owner_ref       :: reference(),
    peer            :: pid() | undefined,
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
%% Required opts: `id`, `role`, `mode`, `owner`.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Pair two stream processes as peers (local dispatch only).
-spec pair(pid(), pid()) -> ok.
pair(A, B) when is_pid(A), is_pid(B) ->
    ok = gen_server:call(A, {pair, B}),
    ok = gen_server:call(B, {pair, A}),
    ok.

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
%% any pending recv/await_reply waiters receive `{error, {Code, Message}}`.
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

handle_call({pair, Peer}, _From, State) ->
    _ = erlang:monitor(process, Peer),
    {reply, ok, State#state{peer = Peer}};

%% --- send --------------------------------------------------------------

handle_call({send, _Encoding, _Body}, _From, #state{closed_send = true}) ->
    {reply, {error, send_closed}, #state{}};
handle_call({send, Encoding, Body}, _From, State) ->
    case forward_to_peer(State, fun(Peer) ->
            macula_stream:deliver_chunk(Peer, Encoding, Body)
         end) of
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
                     _ = forward_to_peer(State, fun(Peer) ->
                             macula_stream:deliver_end(Peer, send)
                         end),
                     State#state{closed_send = true}
             end,
    {reply, ok, State1};

%% --- close -------------------------------------------------------------

handle_call(close, _From, State) ->
    State1 = case State#state.closed_send of
                 true ->
                     _ = forward_to_peer(State, fun(Peer) ->
                             macula_stream:deliver_end(Peer, both)
                         end),
                     State;
                 false ->
                     _ = forward_to_peer(State, fun(Peer) ->
                             macula_stream:deliver_end(Peer, both)
                         end),
                     State#state{closed_send = true}
             end,
    State2 = State1#state{closed_recv = true},
    State3 = drain_waiters(eof, State2),
    {reply, ok, State3};

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
                     _ = forward_to_peer(State, fun(Peer) ->
                             macula_stream:deliver_reply(Peer, Result)
                         end),
                     State#state{reply = Result};
                 _ ->
                     State
             end,
    {reply, ok, State1};

handle_call({abort, Code, Message}, _From, State) ->
    Err = {error, {Code, Message}},
    _ = forward_to_peer(State, fun(Peer) ->
            macula_stream:deliver_error(Peer, Code, Message)
        end),
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
    case {Ref, Pid} of
        {OwnerRef, OwnerPid} when OwnerRef =:= State#state.owner_ref,
                                  OwnerPid =:= State#state.owner ->
            {stop, normal, State};
        _ when Pid =:= State#state.peer ->
            %% Peer died — surface as a stream error to local readers
            Err = {error, peer_down},
            State1 = State#state{closed_recv = true, closed_send = true,
                                 peer = undefined},
            State2 = drain_waiters(Err, State1),
            State3 = settle_reply_waiters_with(Err, State2),
            {noreply, State3};
        _ -> {noreply, State}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

%%%===================================================================
%%% Internal helpers
%%%===================================================================

forward_to_peer(#state{peer = undefined}, _Fun) -> {error, no_peer};
forward_to_peer(#state{peer = Peer}, Fun) when is_pid(Peer) -> Fun(Peer).

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
