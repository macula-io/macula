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
%%%       over a peering connection).</li>
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
    controlling_process/2,
    info/1
]).

%% The check of the stream functions a supervised wrapper is given.
-export([stream_io/2]).

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
    terminate/2
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

%% The functions a supervised stream wrapper, such as `macula_stream_sink',
%% opens, reads, writes and ends its stream with, by key. Each wrapper
%% calls some of them; `stream_io/2' checks the ones it is given.
-type stream_io() :: #{call_stream => fun((macula:pool(), macula:realm(), macula:procedure(),
                                           term(), map()) -> {ok, pid()} | {error, term()}),
                       recv => fun((pid(), timeout()) ->
                                      {chunk, binary()} | {data, term()} | eof | {error, term()}),
                       send => fun((pid(), binary() | term(), encoding()) -> ok | {error, term()}),
                       close_send => fun((pid()) -> term()),
                       close => fun((pid()) -> term()),
                       close_stream => fun((pid()) -> term()),
                       abort => fun((pid(), binary(), binary()) -> term()),
                       set_reply => fun((pid(), term()) -> term()),
                       set_error => fun((pid(), term()) -> term()),
                       await_reply => fun((pid()) -> result()),
                       controlling_process => fun((pid(), pid()) -> ok | {error, not_owner})}.

-export_type([stream_io/0]).

%% How a session ended, as the owner is told it.
-type ended() :: closed | peer_down | {error, {binary(), binary()}}.

%% The bytes of chunks no reader has taken a stream keeps by default, the
%% same as a QUIC stream's default receive window.
-define(MAX_INBOX_BYTES, 16#1000000).

-record(state, {
    id              :: stream_id(),
    role            :: role(),
    mode            :: mode(),
    owner           :: pid(),
    owner_ref       :: reference(),
    peer            :: peer(),
    %% Recv side: inbound chunks queued, waiting recv/2 callers, eof flag
    inbox = queue:new() :: queue:queue({encoding(), term()}),
    %% The bytes of the queued chunks, and the most it may reach
    inbox_bytes = 0 :: non_neg_integer(),
    max_inbox_bytes = ?MAX_INBOX_BYTES :: pos_integer(),
    waiters = queue:new() :: queue:queue({{pid(), reference()}, reference()}),
    closed_recv = false :: boolean(),
    %% Send side
    closed_send = false :: boolean(),
    seq_out = 0 :: non_neg_integer(),
    seq_in  = 0 :: non_neg_integer(),
    %% Terminal reply (for client-stream / bidi)
    reply = undefined :: undefined | result(),
    reply_waiters = [] :: [{pid(), reference()}],
    %% How the session ended, once it has and the owner has been told
    ended = undefined :: undefined | ended()
}).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Start a stream gen_server.
%%
%% Required opts: id, role, mode, owner. Optional: max_inbox_bytes, the
%% bytes of chunks no reader has taken that the stream keeps; a chunk past
%% them ends the session (default 16 MiB).
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
%% station_link carries deliveries as V2 `macula_frame:stream_*'
%% frames over its peering connection (one per pool seed); inbound
%% STREAM_* frames are decoded by the link and forwarded into this
%% stream via the deliver_chunk / end / error / reply casts below.
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

%% @doc Hand the stream to `NewOwner'. A stream ends when its owner ends;
%% after this it ends when `NewOwner' does, and `NewOwner' is told when the
%% session ends, as `{macula_stream, ended, Stream, How}', or at once if it
%% already has. Only the stream's current owner can hand it over; any other
%% caller gets `{error, not_owner}' and the stream stays with its owner.
-spec controlling_process(pid(), pid()) -> ok | {error, not_owner}.
controlling_process(Pid, NewOwner) when is_pid(Pid), is_pid(NewOwner) ->
    gen_server:call(Pid, {controlling_process, NewOwner}).

%% @doc Inspect stream state (debugging).
-spec info(pid()) -> map().
info(Pid) ->
    gen_server:call(Pid, info).

%% @doc The stream functions a wrapper runs on. `Defaults' are the
%% functions the wrapper calls, by key, and `Given' the `stream_io' its
%% caller gave, or `undefined' for none, which gives `Defaults'. A given
%% set has every key in `Defaults', each function at the arity its key
%% takes, and may carry other `stream_io()' functions; any other is
%% refused with `function_clause', in the calling process.
-spec stream_io(stream_io(), stream_io() | undefined) -> stream_io().
stream_io(Defaults, undefined) when is_map(Defaults) ->
    Defaults;
stream_io(Defaults, Given) when is_map(Defaults), is_map(Given) ->
    ok = maps:foreach(fun stream_function/2, Given),
    ok = lists:foreach(fun(Key) -> given_key(Key, Given) end, maps:keys(Defaults)),
    Given.

stream_function(call_stream, Fun) when is_function(Fun, 5) -> ok;
stream_function(recv, Fun) when is_function(Fun, 2) -> ok;
stream_function(send, Fun) when is_function(Fun, 3) -> ok;
stream_function(close_send, Fun) when is_function(Fun, 1) -> ok;
stream_function(close, Fun) when is_function(Fun, 1) -> ok;
stream_function(close_stream, Fun) when is_function(Fun, 1) -> ok;
stream_function(abort, Fun) when is_function(Fun, 3) -> ok;
stream_function(set_reply, Fun) when is_function(Fun, 2) -> ok;
stream_function(set_error, Fun) when is_function(Fun, 2) -> ok;
stream_function(await_reply, Fun) when is_function(Fun, 1) -> ok;
stream_function(controlling_process, Fun) when is_function(Fun, 2) -> ok.

given_key(Key, Given) when is_map_key(Key, Given) -> ok.

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
        owner_ref = OwnerRef,
        max_inbox_bytes = maps:get(max_inbox_bytes, Opts, ?MAX_INBOX_BYTES)
    }}.

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
handle_call({send, Encoding, Body}, _From, #state{role = Role, mode = Mode} = State) ->
    send_chunk(may_send(Role, Mode), Encoding, Body, State);

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
    {reply, ok, ended_when_both_closed(State1)};

%% --- close -------------------------------------------------------------

handle_call(close, _From, State) ->
    _ = forward_to_peer(State, {end_stream, both}),
    State1 = State#state{closed_send = true, closed_recv = true},
    State2 = drain_waiters(eof, State1),
    {reply, ok, session_ended(closed, State2)};

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
    {reply, ok, abort_session(Code, Message, State)};

%% --- controlling_process -----------------------------------------------

%% Only the owner hands the stream over.
handle_call({controlling_process, NewOwner}, {Owner, _Tag}, #state{owner = Owner} = State) ->
    {reply, ok, hand_over(NewOwner, State)};
handle_call({controlling_process, _NewOwner}, _From, State) ->
    {reply, {error, not_owner}, State};

%% --- info --------------------------------------------------------------

handle_call(info, _From, State) ->
    Map = #{
        id => State#state.id,
        role => State#state.role,
        mode => State#state.mode,
        peer => State#state.peer,
        inbox_size => queue:len(State#state.inbox),
        inbox_bytes => State#state.inbox_bytes,
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
handle_cast({peer_chunk, Encoding, Body}, #state{role = Role, mode = Mode} = State) ->
    {noreply, take_chunk(peer_may_send(Role, Mode), Encoding, Body, State)};

handle_cast({peer_end, send}, State) ->
    %% Peer half-closed: no more inbound data
    State1 = State#state{closed_recv = true},
    State2 = drain_waiters(eof, State1),
    {noreply, ended_when_both_closed(State2)};
%% A session that ended keeps how it ended as its reply, unless a reply is
%% already set, so an await_reply called later returns it at once.
handle_cast({peer_end, both}, State) ->
    State1 = State#state{closed_recv = true, closed_send = true,
                         reply = first_reply(State#state.reply, {error, peer_closed})},
    State2 = drain_waiters(eof, State1),
    State3 = settle_reply_waiters_with({error, peer_closed}, State2),
    {noreply, session_ended(closed, State3)};

handle_cast({peer_error, Code, Message}, State) ->
    Err = {error, {Code, Message}},
    State1 = State#state{closed_recv = true, closed_send = true,
                         reply = first_reply(State#state.reply, Err)},
    State2 = drain_waiters(Err, State1),
    State3 = settle_reply_waiters_with(Err, State2),
    {noreply, session_ended(Err, State3)};

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
%%   {local, Pid}             — in-process pair; cast the symmetric
%%                              deliver_* helper directly.
%%   {remote_via_link, L, Sid}— hand off to `macula_station_link'
%%                              which signs and sends a
%%                              `macula_frame:stream_*' frame.
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
forward_to_peer(#state{peer = {remote_via_link, Link, Sid}} = S, Action) ->
    send_via_link(Link, Sid, Action, S#state.seq_out).

%% @private V2 carrier: hand off to `macula_station_link' which signs
%% and ships a `macula_frame:stream_*' frame through its peering
%% connection. Action shapes mirror `send_remote/4'; the link
%% translates them to V2 frame specs internally.
send_via_link(Link, Sid, {chunk, Encoding, Body}, Seq) ->
    macula_station_link:send_stream_frame(Link, stream_data, #{
        stream_id => Sid,
        seq       => Seq,
        encoding  => Encoding,
        body      => Body
    });
send_via_link(Link, Sid, {end_stream, Role}, _Seq) ->
    macula_station_link:send_stream_frame(Link, stream_end, #{
        stream_id => Sid,
        role      => Role
    });
send_via_link(Link, Sid, {error, Code, Message}, _Seq) ->
    macula_station_link:send_stream_frame(Link, stream_error, #{
        stream_id => Sid,
        code      => Code,
        message   => Message
    });
send_via_link(Link, Sid, {reply, {ok, Value}}, _Seq) ->
    macula_station_link:send_stream_frame(Link, stream_reply, #{
        stream_id => Sid,
        payload   => Value
    });
send_via_link(Link, Sid, {reply, {error, _Reason} = Err}, _Seq) ->
    macula_station_link:send_stream_frame(Link, stream_reply, #{
        stream_id => Sid,
        payload   => Err
    }).

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
                         peer = undefined,
                         reply = first_reply(State#state.reply, Err)},
    State2 = drain_waiters(Err, State1),
    State3 = settle_reply_waiters_with(Err, State2),
    {noreply, session_ended(peer_down, State3)}.

%% @private In server_stream only the server sends chunks, and in client_stream
%% only the client does; bidi takes them both ways.
may_send(client, server_stream) -> false;
may_send(server, client_stream) -> false;
may_send(_Role, _Mode)          -> true.

peer_may_send(Role, Mode) ->
    may_send(peer_role(Role), Mode).

peer_role(client) -> server;
peer_role(server) -> client.

%% @private A chunk this side's mode lets it send goes to the peer. Any other
%% send is refused, and nothing reaches the peer.
send_chunk(true, Encoding, Body, State) ->
    case forward_to_peer(State, {chunk, Encoding, Body}) of
        ok ->
            {reply, ok, State#state{seq_out = State#state.seq_out + 1}};
        {error, _} = Err ->
            {reply, Err, State}
    end;
send_chunk(false, _Encoding, _Body, #state{mode = Mode} = State) ->
    {reply, {error, {send_not_allowed, Mode}}, State}.

%% @private A chunk the mode lets the peer send is delivered or queued. Any
%% other chunk ends the session with a stream protocol error, and nothing of it
%% is kept.
take_chunk(true, Encoding, Body, State) ->
    State1 = enqueue_or_deliver(Encoding, Body, State),
    State1#state{seq_in = State1#state.seq_in + 1};
take_chunk(false, _Encoding, _Body, State) ->
    abort_session(<<"stream_protocol_error">>,
                  <<"the peer sent a chunk its stream mode does not allow">>, State).

%% @private End the session with an error: the peer is sent it, both directions
%% close, waiting readers and reply waiters get it, a reply already set stays,
%% and the owner is told.
abort_session(Code, Message, State) ->
    Err = {error, {Code, Message}},
    _ = forward_to_peer(State, {error, Code, Message}),
    State1 = State#state{closed_recv = true, closed_send = true,
                         reply = first_reply(State#state.reply, Err)},
    State2 = drain_waiters(Err, State1),
    State3 = settle_reply_waiters_with(Err, State2),
    session_ended(Err, State3).

first_reply(undefined, Err) -> Err;
first_reply(Reply, _Err)    -> Reply.

%% @private A session has ended once both of its directions are closed.
ended_when_both_closed(#state{closed_recv = true, closed_send = true} = State) ->
    session_ended(closed, State);
ended_when_both_closed(State) ->
    State.

%% @private The owner is told once how the session ended: `closed',
%% `{error, {Code, Message}}' or `peer_down'. The stream itself stays
%% until its owner ends.
session_ended(_How, #state{ended = Ended} = State) when Ended =/= undefined ->
    State;
session_ended(How, #state{owner = Owner} = State) ->
    Owner ! {macula_stream, ended, self(), How},
    State#state{ended = How}.

%% @private The new owner is monitored before the old one is let go, so the
%% stream always has an owner it ends with; a new owner that is already gone
%% ends the stream at once. A new owner is told at once if the session has
%% already ended.
hand_over(NewOwner, #state{owner_ref = OldRef, ended = Ended} = State) ->
    NewRef = erlang:monitor(process, NewOwner),
    true = erlang:demonitor(OldRef, [flush]),
    ok = tell_new_owner(Ended, NewOwner),
    State#state{owner = NewOwner, owner_ref = NewRef}.

tell_new_owner(undefined, _NewOwner) ->
    ok;
tell_new_owner(How, NewOwner) ->
    NewOwner ! {macula_stream, ended, self(), How},
    ok.

%% @doc Either deliver a chunk to a waiting recv/2 caller or queue it.
enqueue_or_deliver(Encoding, Body, #state{waiters = W0} = State) ->
    case queue:out(W0) of
        {{value, {From, Ref}}, W1} ->
            cancel_timer(Ref),
            gen_server:reply(From, chunk_to_recv_result(Encoding, Body)),
            State#state{waiters = W1};
        {empty, _} ->
            Kept = kept(Encoding, Body),
            enqueue(chunk_bytes(Encoding, Kept), Encoding, Kept, State)
    end.

%% A chunk that would take the memory the queued chunks hold past the
%% stream's bound, or a served stream's caller or node past its budget for
%% unread bytes, ends the session with resource_exhausted, and nothing of it
%% is kept: the receiving side has no room, which a peer that keeps to the
%% protocol cannot see coming.
enqueue(Bytes, _Encoding, _Kept, #state{inbox_bytes = Queued, max_inbox_bytes = Max} = State)
  when Queued + Bytes > Max ->
    abort_session(<<"resource_exhausted">>,
                  <<"the stream keeps no more unread bytes">>, State);
enqueue(Bytes, Encoding, Kept, State) ->
    queue_charged(charge_budget(Bytes, State), Bytes, Encoding, Kept, State).

queue_charged(ok, Bytes, Encoding, Kept, #state{inbox = Inbox, inbox_bytes = Queued} = State) ->
    State#state{inbox = queue:in({Encoding, Kept}, Inbox), inbox_bytes = Queued + Bytes};
queue_charged({error, _Refused}, _Bytes, _Encoding, _Kept, State) ->
    abort_session(<<"resource_exhausted">>,
                  <<"the node keeps no more unread bytes for this session">>, State).

%% A served stream on a station link charges what it keeps unread to its
%% caller and the node (`macula_stream_sessions'), and gives it back when a
%% reader takes it or the stream ends; any other stream keeps only its own
%% bound.
charge_budget(Bytes, #state{role = server, peer = {remote_via_link, _Link, _Sid}}) ->
    macula_stream_sessions:charge(self(), Bytes);
charge_budget(_Bytes, _State) ->
    ok.

release_budget(Bytes, #state{role = server, peer = {remote_via_link, _Link, _Sid}}) ->
    macula_stream_sessions:release(self(), Bytes);
release_budget(_Bytes, _State) ->
    ok.

%% A chunk as the inbox keeps it: a copy, so a body that is part of the frame
%% it arrived in keeps none of the rest of that frame.
kept(raw, Body) when is_binary(Body) -> binary:copy(Body);
kept(_Encoding, Body)                -> binary_to_term(term_to_binary(Body)).

%% The memory a queued chunk holds: its term on the heap, with the tuple and
%% the queue cell that hold it (two words), and every binary it keeps off the
%% heap. An empty chunk still holds its cell and tuple.
chunk_bytes(Encoding, Kept) ->
    (erts_debug:flat_size({Encoding, Kept}) + 2) * erlang:system_info(wordsize)
        + off_heap_bytes(Kept).

%% Binaries over 64 bytes live off the heap; smaller ones are counted in the
%% heap size.
off_heap_bytes(Bin) when is_binary(Bin), byte_size(Bin) > 64 -> byte_size(Bin);
off_heap_bytes(Tuple) when is_tuple(Tuple) -> off_heap_bytes(tuple_to_list(Tuple));
off_heap_bytes(Map) when is_map(Map) -> off_heap_bytes(maps:to_list(Map));
off_heap_bytes([Head | Tail]) -> off_heap_bytes(Head) + off_heap_bytes(Tail);
off_heap_bytes(_Other) -> 0.

handle_recv(From, _Timeout, #state{inbox = Inbox} = State) ->
    case queue:out(Inbox) of
        {{value, {Encoding, Body}}, Rest} ->
            take_queued(Encoding, Body, Rest, State);
        {empty, _} when State#state.closed_recv ->
            {reply, eof, State};
        {empty, _} ->
            queue_waiter(From, _Timeout, State)
    end.

%% A reader takes a queued chunk: its bytes leave the stream's count, and a
%% served stream gives them back to its caller's and the node's budget.
take_queued(Encoding, Body, Rest, #state{inbox_bytes = Queued} = State) ->
    Bytes = chunk_bytes(Encoding, Body),
    ok = release_budget(Bytes, State),
    {reply, chunk_to_recv_result(Encoding, Body), State#state{inbox = Rest, inbox_bytes = Queued - Bytes}}.

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
