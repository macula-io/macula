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
-type ended() :: closed | peer_down | {error, {binary(), binary()}} | {error, {transport, term()}}.

%% The bytes of chunks no reader has taken a stream keeps by default, the
%% same as a QUIC stream's default receive window.
-define(MAX_INBOX_BYTES, 16#1000000).

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
    ended = undefined :: undefined | ended(),
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
%% Required opts: id, role, mode, owner. Optional: max_inbox_bytes, the
%% bytes of chunks no reader has taken that the stream keeps; a chunk past
%% them ends the session (default 16 MiB). A stream carried by a
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
%% A code a STREAM_ERROR cannot carry, over 64 bytes or not UTF-8, is
%% refused by name before the stream is called, and the stream goes on.
-spec abort(pid(), binary(), binary()) -> ok | {error, {text_too_long | invalid_text, code}}.
abort(Pid, Code, Message) when is_binary(Code), is_binary(Message) ->
    aborted(macula_frame:text_checked(code, Code), Pid, Code, Message).

aborted(ok, Pid, Code, Message) -> gen_server:call(Pid, {abort, Code, Message});
aborted({error, _} = Refused, _Pid, _Code, _Message) -> Refused.

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

%% @doc Deliver a STREAM_DATA, STREAM_END, STREAM_ERROR or STREAM_REPLY
%% frame of a link-carried stream from the peer. The stream verifies it
%% against its STREAM_OPEN before it takes effect. A refused frame is
%% dropped and reported to the stream's peering connection, and the
%% stream carries on. A frame of a type that belongs on the control
%% stream rejects the connection with malformed_frame and ends the
%% stream, in either profile.
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
        owner_ref = OwnerRef,
        max_inbox_bytes = maps:get(max_inbox_bytes, Opts, ?MAX_INBOX_BYTES)
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
handle_call({send, Encoding, Body}, _From, #state{role = Role, mode = Mode} = State) ->
    send_chunk(may_send(Role, Mode), Encoding, Body, State);

%% --- recv --------------------------------------------------------------

handle_call({recv, Timeout}, From, State) ->
    handle_recv(From, Timeout, State);

%% --- close_send --------------------------------------------------------

handle_call(close_send, _From, #state{closed_send = true} = State) ->
    {reply, ok, State};
handle_call(close_send, _From, State) ->
    {_Sent, State1} = forward_to_peer(State, {end_stream, send}),
    {reply, ok, ended_when_both_closed(State1#state{closed_send = true})};

%% --- close -------------------------------------------------------------

handle_call(close, _From, State) ->
    {_Sent, State0} = forward_to_peer(State, {end_stream, both}),
    State1 = State0#state{closed_send = true, closed_recv = true},
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

handle_call({set_reply, Result}, _From, #state{reply = undefined} = State) ->
    replied(forward_to_peer(State, {reply, Result}), Result);
handle_call({set_reply, _Result}, _From, State) ->
    {reply, ok, State};

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

handle_cast({peer_chunk, Encoding, Body}, State) ->
    {noreply, chunk_arrived(Encoding, Body, State)};
handle_cast({peer_end, Role}, State) when Role =:= send; Role =:= both ->
    {noreply, end_arrived(Role, State)};

handle_cast({peer_error, Code, Message}, State) ->
    {noreply, error_arrived(Code, Message, State)};

handle_cast({peer_reply, Result}, State) ->
    {noreply, reply_arrived(Result, State)};
handle_cast({peer_frame, #{frame_type := Type} = Frame}, State) ->
    {noreply, peer_frame(macula_frame:control_frame(Type), Frame, State)};
handle_cast({peer_frame, Frame}, State) ->
    {noreply, peer_frame(false, Frame, State)};

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
    {noreply, transport_failed({error, {transport, Reason}}, State)};

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
    {Sent, State1} = forward_to_peer(State, {chunk, Encoding, Body}),
    {reply, Sent, State1};
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
    {_Sent, State0} = forward_to_peer(State, {error, Code, Message}),
    State1 = State0#state{closed_recv = true, closed_send = true,
                          reply = first_reply(State0#state.reply, Err)},
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

%% @private The effects of the peer's frames, the same for both carriers. A
%% chunk after the receive side closed is dropped, and any other goes through
%% the mode's direction check (`take_chunk/4').
chunk_arrived(_Encoding, _Body, #state{closed_recv = true} = State) ->
    State;
chunk_arrived(Encoding, Body, #state{role = Role, mode = Mode} = State) ->
    take_chunk(peer_may_send(Role, Mode), Encoding, Body, State).

%% Peer half-closed: no more inbound data, and the session has ended once both
%% directions are closed. A full close keeps how the session ended as its
%% reply, unless a reply is already set, so an await_reply called later
%% returns it at once.
end_arrived(send, State) ->
    ended_when_both_closed(drain_waiters(eof, State#state{closed_recv = true}));
end_arrived(both, #state{reply = Reply} = State) ->
    State1 = drain_waiters(eof, State#state{closed_recv = true, closed_send = true,
                                             reply = first_reply(Reply, {error, peer_closed})}),
    session_ended(closed, settle_reply_waiters_with({error, peer_closed}, State1)).

%% The peer's STREAM_ERROR ends the session: how it ended stays as the reply,
%% unless one is already set, and the owner is told.
error_arrived(Code, Message, #state{reply = Reply} = State) ->
    Err = {error, {Code, Message}},
    session_ended(Err, ended_with(Err, State#state{reply = first_reply(Reply, Err)})).

reply_arrived(Result, State) ->
    settle_reply_waiters_with(Result, State#state{reply = Result}).

%% Both sides closed, with every reader and reply waiter answered with Err.
ended_with(Err, State) ->
    State1 = drain_waiters(Err, State#state{closed_recv = true, closed_send = true}),
    settle_reply_waiters_with(Err, State1).

%% A transport failure ends the session too, and the owner is told.
transport_failed(Err, #state{reply = Reply} = State) ->
    session_ended(Err, ended_with(Err, State#state{peer = undefined, reply = first_reply(Reply, Err)})).

%% @private A frame of a type that belongs on the control stream has no place
%% on a dedicated stream, in either profile. It is the connection peer's
%% doing, so the connection is rejected with malformed_frame, and this
%% stream ends with that transport failure. Any other frame is verified.
peer_frame(true, _Frame, #state{conn = Conn} = State) ->
    ok = macula_peering:reject(Conn, malformed_frame),
    transport_failed({error, {transport, malformed_frame}}, State);
peer_frame(false, Frame, #state{role = Role, verifier = Verifier, profile = Profile} = State) ->
    verified_frame(peer_verified(Role, Frame, Verifier, Profile), State).

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
