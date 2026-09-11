%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised streaming RPC consumers.
%%%
%%% `call_stream/5' hands back a raw stream pid; a real consumer has to
%%% hand-write a `recv/2' loop around it — the provider side already
%%% gets this for free via `advertise_stream/5''s callback handler, this
%%% is the missing consumer-side half. `macula_stream_sink' opens the
%%% stream for you, drives the `recv/2' loop in a linked reader process
%%% (so a slow or stuck `recv' never blocks your gen_server's own
%%% mailbox), and calls `Module:handle_chunk/2' once per item against
%%% state your module owns, `Module:handle_close/2' when the stream ends
%%% or errors.
%%%
%%% This is the general-purpose RPC streaming feature (`call_stream/5',
%%% e.g. a `logs.tail_v1'-style procedure) — unrelated to content
%%% sharing's own chunked-transfer protocol; see `macula_feeder' /
%%% `macula_download' for that.
%%%
%%% Publishes `streaming.started_v1' / `streaming.completed_v1' mesh
%%% facts around the stream's lifetime, from the consumer's own
%%% perspective — the provider side (`macula_streamer') publishes its
%%% own copy from its perspective; the two are not deduplicated,
%%% mirroring how `macula_feeder' / `macula_download' each announce
%%% their own side of a content transfer.
%%%
%%% A process of the sink's own publishes these facts, in order, so a
%%% pool that is gone or slow never fails or holds up the sink or its
%%% stream; a publish that fails is logged. A sink killed before it could
%%% hand over its stream's end has `streaming.completed_v1' published
%%% for it, with outcome `failed' and the reason it went down for.
%%% An end fact and an abort message name their reason, `killed' or
%%% `timeout' for example, and carry none of the reason's terms, which
%%% go to the local log only.
%%%
%%% == Cancel ==
%%%
%%% Stopping this gen_server for any non-`normal' reason (a `recv'
%%% error, the reader crashing, `Module:handle_chunk/2' returning a
%%% non-normal stop) sends the provider an explicit `macula:abort/3'
%%% STREAM_ERROR instead of an ordinary close — the provider learns
%%% the consumer cancelled or failed rather than mistaking it for a
%%% clean end-of-stream. A `normal' stop closes both sides cleanly
%%% instead, same as before.
%%%
%%% == Direct-dial ==
%%%
%%% `start_link/5,6' opens through the pool's existing links — the same
%%% gossip-propagated routing `call_stream/5' always used.
%%% `start_link_direct/5,6' is the direct-dial counterpart: it resolves
%%% the procedure's `procedure_advertisement' from the DHT (published by
%%% `macula_streamer:advertise_direct/6,7' on the provider side) and
%%% opens the stream there directly, in one hop, instead of depending on
%%% advertise-gossip having propagated a route between arbitrary
%%% stations. Requires the provider to have advertised via
%%% `advertise_direct/6,7', not plain `advertise/5,6'. See
%%% `macula_direct_dial''s module doc, "Trust model".
%%%
%%% == Stream I/O ==
%%%
%%% A sink opens, reads, ends and announces its stream through five
%%% functions: `call_stream/5', `recv/2', `close_stream/1', `abort/3'
%%% and `publish/4'. They are the `macula' facade's by default, and a
%%% direct-dial sink dials with `macula_direct_dial:call_stream/5'.
%%% `start_link/7' and `start_link_direct/7' take a `stream_io' start
%%% option of five other functions at the same arities, all five given,
%%% to run a sink on something else, such as a test's scripted stream.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(log_tailer).
%%% -behaviour(macula_stream_sink).
%%% -export([init/1, handle_chunk/2, handle_close/2]).
%%%
%%% init(_Args) -> {ok, []}.
%%%
%%% handle_chunk(Line, Lines) ->
%%%     io:format("~s", [Line]),
%%%     {noreply, [Line | Lines]}.
%%%
%%% handle_close(_Reason, _Lines) -> ok.
%%% '''
%%%
%%% ```
%%% {ok, Pid} = macula_stream_sink:start_link(log_tailer, Pool, Realm,
%%%     <<"logs.tail_v1">>, []).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_sink).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/5, start_link/6, start_link/7]).
-export([start_link_direct/5, start_link_direct/6, start_link_direct/7]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export_type([stream_io/0, start_opts/0]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_chunk(Chunk :: term(), State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback handle_close(Reason :: normal | term(), State :: term()) -> any().

-optional_callbacks([handle_close/2]).

-define(RECV_TIMEOUT, 30_000).
-define(STREAMING_STARTED, <<"streaming.started_v1">>).
-define(STREAMING_COMPLETED, <<"streaming.completed_v1">>).
-define(CANCEL_CODE, <<"cancelled">>).
%% The longest reason name an end fact or an abort message carries.
-define(REASON_NAME_BYTES, 64).

-type stream_io() :: #{call_stream := fun((macula:pool(), macula:realm(), macula:procedure(),
                                          term(), map()) ->
                                             {ok, macula:stream()} | {error, term()}),
                       recv := fun((macula:stream(), timeout()) ->
                                      {chunk, binary()} | {data, term()} | eof | {error, term()}),
                       close_stream := fun((macula:stream()) -> term()),
                       abort := fun((macula:stream(), binary(), binary()) -> term()),
                       publish := fun((macula:pool(), macula:realm(), macula:topic(), term()) ->
                                         term())}.
-type start_opts() :: #{stream_io => stream_io()}.

-record(kstate, {
    io        :: stream_io(),
    module    :: module(),
    pool      :: macula:pool(),
    realm     :: macula:realm(),
    announcer :: pid(),
    stream_id :: binary(),
    stream    :: pid(),
    reader    :: pid(),
    user      :: term()
}).

%% @doc Start a sink. Opens a stream to `Procedure' on `(Realm)' via
%% `Pool' and passes `Args' to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:procedure(),
                  term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Procedure, Args) ->
    start_link(Module, Pool, Realm, Procedure, Args, #{}).

%% @doc As `start_link/5', with `CallArgs' passed to `call_stream/5' as
%% the RPC argument payload.
-spec start_link(module(), macula:pool(), macula:realm(), macula:procedure(),
                  term(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Procedure, Args, CallArgs) ->
    start_link(Module, Pool, Realm, Procedure, Args, CallArgs, #{}).

%% @doc As `start_link/6', with start options: `stream_io' gives the
%% five functions the sink runs its stream on (see "Stream I/O" above).
-spec start_link(module(), macula:pool(), macula:realm(), macula:procedure(),
                  term(), term(), start_opts()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Procedure, Args, CallArgs, Opts) when is_map(Opts) ->
    start(stream_io(pooled, Opts), {Module, Pool, Realm, Procedure, Args, CallArgs}).

%% @doc As `start_link/5', but resolves and dials the procedure's
%% provider directly instead of routing through the pool's existing
%% links. See the "Direct-dial" section above.
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:procedure(), term()) ->
    {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Procedure, Args) ->
    start_link_direct(Module, Pool, Realm, Procedure, Args, undefined).

%% @doc As `start_link_direct/5', with `CallArgs' passed to
%% `macula_direct_dial:call_stream/5' as the RPC argument payload.
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:procedure(), term(), term()) ->
    {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Procedure, Args, CallArgs) ->
    start_link_direct(Module, Pool, Realm, Procedure, Args, CallArgs, #{}).

%% @doc As `start_link_direct/6', with start options: `stream_io' gives
%% the five functions the sink runs its stream on (see "Stream I/O"
%% above).
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:procedure(), term(), term(), start_opts()) ->
    {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Procedure, Args, CallArgs, Opts) when is_map(Opts) ->
    start(stream_io(direct, Opts), {Module, Pool, Realm, Procedure, Args, CallArgs}).

%% A sink starts on a stream_io of exactly the five functions at their
%% arities; any other is refused with function_clause, in the caller.
start(#{call_stream := CallStream, recv := Recv, close_stream := CloseStream,
        abort := Abort, publish := Publish} = StreamIo, Start)
  when map_size(StreamIo) =:= 5, is_function(CallStream, 5), is_function(Recv, 2),
       is_function(CloseStream, 1), is_function(Abort, 3), is_function(Publish, 4) ->
    gen_server:start_link(?MODULE, {StreamIo, Start}, []).

stream_io(DialMode, Opts) ->
    maps:get(stream_io, Opts, default_stream_io(DialMode)).

default_stream_io(DialMode) ->
    #{call_stream => dial(DialMode),
      recv => fun macula:recv/2,
      close_stream => fun macula:close_stream/1,
      abort => fun macula:abort/3,
      publish => fun macula:publish/4}.

dial(pooled) -> fun macula:call_stream/5;
dial(direct) -> fun macula_direct_dial:call_stream/5.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({StreamIo, {Module, Pool, Realm, Procedure, InitArgs, CallArgs}}) ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            open_stream(StreamIo, Module, Pool, Realm, Procedure, CallArgs,
                        UserState);
        {stop, Reason} ->
            {stop, Reason}
    end.

open_stream(#{call_stream := CallStream, recv := Recv} = StreamIo, Module, Pool, Realm,
            Procedure, CallArgs, UserState) ->
    case CallStream(Pool, Realm, Procedure, CallArgs, #{}) of
        {ok, Stream} ->
            StreamId = crypto:strong_rand_bytes(16),
            Announcer = start_announcer(StreamIo, Pool, Realm, StreamId),
            Reader = spawn_reader(Recv, Stream),
            {ok, #kstate{io = StreamIo, module = Module, pool = Pool, realm = Realm,
                         announcer = Announcer, stream_id = StreamId,
                         stream = Stream, reader = Reader, user = UserState}};
        {error, Reason} ->
            {stop, Reason}
    end.

spawn_reader(Recv, Stream) ->
    Parent = self(),
    spawn_link(fun() -> reader_loop(Parent, Recv, Stream) end).

reader_loop(Parent, Recv, Stream) ->
    dispatch_recv(Recv(Stream, ?RECV_TIMEOUT), Parent, Recv, Stream).

dispatch_recv({chunk, Data}, Parent, Recv, Stream) ->
    Parent ! {stream_item, Data}, reader_loop(Parent, Recv, Stream);
dispatch_recv({data, Data}, Parent, Recv, Stream) ->
    Parent ! {stream_item, Data}, reader_loop(Parent, Recv, Stream);
dispatch_recv(eof, Parent, _Recv, _Stream) ->
    Parent ! stream_eof;
dispatch_recv({error, Reason}, Parent, _Recv, _Stream) ->
    Parent ! {stream_error, Reason}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({stream_item, Data}, #kstate{module = Module, user = User} = State) ->
    deliver(Module:handle_chunk(Data, User), State);
handle_info(stream_eof, State) ->
    {stop, normal, State};
handle_info({stream_error, Reason}, State) ->
    {stop, Reason, State};
handle_info({'EXIT', Reader, Reason}, #kstate{reader = Reader} = State)
        when Reason =/= normal ->
    {stop, {reader_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

deliver({noreply, NewUser}, State) ->
    {noreply, State#kstate{user = NewUser}};
deliver({stop, Reason, NewUser}, State) ->
    {stop, Reason, State#kstate{user = NewUser}}.

%% @private
terminate(Reason, #kstate{io = StreamIo, module = Module, announcer = Announcer,
                          stream_id = StreamId, stream = Stream, reader = Reader,
                          user = User}) ->
    %% A `normal'-reason exit does not propagate across a link to a
    %% non-trapping process, so a clean stop (eof, or the callback
    %% returning {stop, normal, _}) would otherwise leave the reader
    %% looping on `recv/2' forever against a stream nobody is reading
    %% for anymore. Stop it unconditionally, and wait until it has
    %% exited: a kill arrives asynchronously, so without the wait the
    %% sink could be gone while its reader still calls `recv/2'.
    stop_reader(Reader),
    finish_stream(StreamIo, Reason, Stream),
    Announcer ! {stream_ended, outcome_fields(#{stream_id => StreamId}, Reason)},
    maybe_close(Module, Reason, User).

stop_reader(Reader) ->
    Ref = monitor(process, Reader),
    unlink(Reader),
    exit(Reader, kill),
    receive
        {'DOWN', Ref, process, Reader, _} -> ok
    end.

%% @private A `normal' reason (eof, or the callback choosing to stop
%% cleanly) closes both sides. Anything else sends the provider an
%% explicit abort instead of an ordinary close, so it learns this was
%% a cancellation/failure rather than a clean end-of-stream. `Stream'
%% may already be dead by the time this runs (e.g. `{stream_error,_}'
%% means the provider already tore it down) — harmless, caught below.
finish_stream(#{close_stream := CloseStream}, normal, Stream) ->
    try CloseStream(Stream) catch _:_ -> ok end;
finish_stream(#{abort := Abort}, Reason, Stream) ->
    try Abort(Stream, ?CANCEL_CODE, reason_text(Reason)) catch _:_ -> ok end.

outcome_fields(Base, normal) -> Base#{outcome => completed};
outcome_fields(Base, Reason) -> Base#{outcome => failed, reason => Reason}.

maybe_close(Module, Reason, User) ->
    case erlang:function_exported(Module, handle_close, 2) of
        true -> Module:handle_close(Reason, User);
        false -> ok
    end.

%% The sink's facts go out through an announcer of its own, so a publish
%% never fails, holds up or ends the sink's work on its stream. The
%% announcer publishes them in the order it gets them, logs a publish
%% that fails and goes on, and never touches the stream. A sink that
%% goes down before it hands over its stream's end, killed before
%% terminate/2, has that end announced for it, with the reason it went
%% down for. The announcer ends with the last fact it publishes. The
%% sink waits until the announcer monitors it, so that reason is always
%% the sink's own.
start_announcer(#{publish := Publish}, Pool, Realm, StreamId) ->
    Sink = self(),
    Announcer = spawn(fun() -> announce(Sink, Publish, Pool, Realm, StreamId) end),
    receive
        {announcer_watching, Announcer} -> Announcer
    end.

announce(Sink, Publish, Pool, Realm, StreamId) ->
    SinkRef = monitor(process, Sink),
    Sink ! {announcer_watching, self()},
    publish(Publish, Pool, Realm, ?STREAMING_STARTED, #{stream_id => StreamId}),
    receive
        {stream_ended, Outcome} ->
            publish(Publish, Pool, Realm, ?STREAMING_COMPLETED, Outcome);
        {'DOWN', SinkRef, process, Sink, Reason} ->
            publish(Publish, Pool, Realm, ?STREAMING_COMPLETED,
                    outcome_fields(#{stream_id => StreamId}, Reason))
    end.

publish(Publish, Pool, Realm, Topic, #{stream_id := StreamId} = Payload) ->
    Fact = with_reason_name(Topic, StreamId, Payload),
    log_unpublished(Topic, StreamId, try_publish(Publish, Pool, Realm, Topic, Fact)).

%% A fact carries its reason's name and none of the reason's terms. A
%% reason that is more than a name goes to the local log whole.
with_reason_name(_Topic, _StreamId, #{reason := Reason} = Payload) when is_atom(Reason) ->
    Payload#{reason := reason_text(Reason)};
with_reason_name(Topic, StreamId, #{reason := Reason} = Payload) ->
    ?LOG_NOTICE("[macula_stream_sink] ~ts for stream ~ts ends for ~p",
                [Topic, binary:encode_hex(StreamId), Reason]),
    Payload#{reason := reason_text(Reason)};
with_reason_name(_Topic, _StreamId, Payload) ->
    Payload.

%% The name a reason goes by in an end fact or an abort message: the atom
%% at its head, such as killed, shutdown, timeout or badmatch, looking
%% through {error, Reason} and at most three tuples deep, or crashed when
%% there is no such atom or its name is longer than ?REASON_NAME_BYTES.
reason_text(Reason) ->
    name_text(reason_name(Reason, 3)).

reason_name(Name, _Depth) when is_atom(Name) ->
    Name;
reason_name({error, Reason}, Depth) when Depth > 0 ->
    reason_name(Reason, Depth - 1);
reason_name(Reason, Depth) when is_tuple(Reason), tuple_size(Reason) > 0, Depth > 0 ->
    reason_name(element(1, Reason), Depth - 1);
reason_name(_Reason, _Depth) ->
    crashed.

name_text(Name) ->
    within_name_bytes(atom_to_binary(Name, utf8)).

within_name_bytes(Text) when byte_size(Text) =< ?REASON_NAME_BYTES -> Text;
within_name_bytes(_Text) -> <<"crashed">>.

try_publish(Publish, Pool, Realm, Topic, Payload) ->
    try
        Publish(Pool, Realm, Topic, Payload)
    catch
        Class:Reason -> {Class, Reason}
    end.

log_unpublished(_Topic, _StreamId, ok) ->
    ok;
log_unpublished(Topic, StreamId, Failure) ->
    ?LOG_WARNING("[macula_stream_sink] ~ts for stream ~ts not published: ~p",
                 [Topic, binary:encode_hex(StreamId), Failure]).
