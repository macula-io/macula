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

-export([start_link/5, start_link/6]).
-export([start_link_direct/5, start_link_direct/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

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

-record(kstate, {
    module    :: module(),
    pool      :: macula:pool(),
    realm     :: macula:realm(),
    announce  :: boolean(),
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
    gen_server:start_link(?MODULE,
        {pooled, Module, Pool, Realm, Procedure, Args, CallArgs}, []).

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
    gen_server:start_link(?MODULE,
        {direct, Module, Pool, Realm, Procedure, Args, CallArgs}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({DialMode, Module, Pool, Realm, Procedure, InitArgs, CallArgs}) ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            open_stream(DialMode, Module, Pool, Realm, Procedure, CallArgs,
                       UserState);
        {stop, Reason} ->
            {stop, Reason}
    end.

open_stream(DialMode, Module, Pool, Realm, Procedure, CallArgs, UserState) ->
    case dial_stream(DialMode, Pool, Realm, Procedure, CallArgs) of
        {ok, Stream} ->
            Reader = spawn_reader(Stream),
            StreamId = crypto:strong_rand_bytes(16),
            publish(true, Pool, Realm, ?STREAMING_STARTED,
                    #{stream_id => StreamId}),
            {ok, #kstate{module = Module, pool = Pool, realm = Realm,
                        announce = true, stream_id = StreamId,
                        stream = Stream, reader = Reader, user = UserState}};
        {error, Reason} ->
            {stop, Reason}
    end.

dial_stream(pooled, Pool, Realm, Procedure, CallArgs) ->
    macula:call_stream(Pool, Realm, Procedure, CallArgs, #{});
dial_stream(direct, Pool, Realm, Procedure, CallArgs) ->
    macula_direct_dial:call_stream(Pool, Realm, Procedure, CallArgs, #{}).

spawn_reader(Stream) ->
    Parent = self(),
    spawn_link(fun() -> reader_loop(Parent, Stream) end).

reader_loop(Parent, Stream) ->
    dispatch_recv(macula:recv(Stream, ?RECV_TIMEOUT), Parent, Stream).

dispatch_recv({chunk, Data}, Parent, Stream) ->
    Parent ! {stream_item, Data}, reader_loop(Parent, Stream);
dispatch_recv({data, Data}, Parent, Stream) ->
    Parent ! {stream_item, Data}, reader_loop(Parent, Stream);
dispatch_recv(eof, Parent, _Stream) ->
    Parent ! stream_eof;
dispatch_recv({error, Reason}, Parent, _Stream) ->
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
terminate(Reason, #kstate{module = Module, pool = Pool, realm = Realm,
                          announce = Announce, stream_id = StreamId,
                          stream = Stream, reader = Reader, user = User}) ->
    %% A `normal'-reason exit does not propagate across a link to a
    %% non-trapping process, so a clean stop (eof, or the callback
    %% returning {stop, normal, _}) would otherwise leave the reader
    %% looping on `recv/2' forever against a stream nobody is reading
    %% for anymore. Stop it unconditionally.
    unlink(Reader),
    exit(Reader, kill),
    finish_stream(Reason, Stream),
    publish(Announce, Pool, Realm, ?STREAMING_COMPLETED,
            outcome_fields(#{stream_id => StreamId}, Reason)),
    maybe_close(Module, Reason, User).

%% @private A `normal' reason (eof, or the callback choosing to stop
%% cleanly) closes both sides. Anything else sends the provider an
%% explicit abort instead of an ordinary close, so it learns this was
%% a cancellation/failure rather than a clean end-of-stream. `Stream'
%% may already be dead by the time this runs (e.g. `{stream_error,_}'
%% means the provider already tore it down) — harmless, caught below.
finish_stream(normal, Stream) ->
    try macula:close_stream(Stream) catch _:_ -> ok end;
finish_stream(Reason, Stream) ->
    Message = iolist_to_binary(io_lib:format("~p", [Reason])),
    try macula:abort(Stream, ?CANCEL_CODE, Message) catch _:_ -> ok end.

outcome_fields(Base, normal) -> Base#{outcome => completed};
outcome_fields(Base, Reason) -> Base#{outcome => failed, reason => Reason}.

maybe_close(Module, Reason, User) ->
    case erlang:function_exported(Module, handle_close, 2) of
        true -> Module:handle_close(Reason, User);
        false -> ok
    end.

publish(false, _, _, _, _) -> ok;
publish(true, Pool, Realm, Topic, Payload) ->
    _ = macula:publish(Pool, Realm, Topic, Payload), ok.
