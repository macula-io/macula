%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised streaming RPC providers.
%%%
%%% `advertise_stream/5' on the raw SDK takes a bare handler fun
%%% invoked as `Handler(StreamPid, Args)' in a transient process
%%% spawned per inbound STREAM_OPEN (see the internal
%%% macula_station_link advertise_stream/5 — "this link spawns a
%%% server-side macula_stream and dispatches Handler(StreamPid, Args)
%%% in a transient process"). This is the provider-side counterpart to
%%% `macula_stream_sink': each inbound stream starts one supervised
%%% `macula_streamer' child (under a `simple_one_for_one' factory this
%%% module owns), threading state through `Module:init/1' and
%%% `Module:handle_open/2', and publishing `streaming.started_v1' /
%%% `streaming.completed_v1' mesh facts around the stream's lifetime.
%%%
%%% Sending is push-based and driven from outside the callback: once
%%% `Module:handle_open/2' has done whatever registration it needs
%%% (e.g. stashing `self()' in a registry keyed by some connection id),
%%% any process holding this streamer's pid can call `send/2,3' /
%%% `close/1' on it. This module does not prescribe the discovery
%%% mechanism.
%%%
%%% For `client_stream' mode — a consumer pushing chunks INTO the
%%% provider, e.g. a batch upload — export the optional
%%% `handle_chunk/2' callback (mirroring `macula_stream_sink''s
%%% consumer-side callback exactly) and this module drives the same
%%% linked-reader `recv/2' loop for you, on the provider side. A
%%% `server_stream'-mode module that doesn't export it is unaffected.
%%%
%%% A `client_stream' provider that also needs to hand the consumer a
%%% terminal result (not just accept chunks) exports the optional
%%% `handle_eof/1' callback: called once, when the consumer's own
%%% `close_send/1' surfaces here as end-of-stream, in place of the
%%% default unconditional `{stop, normal, State}'. Returning
%%% `{reply, Result, NewState}' sets the stream's terminal reply
%%% (`macula_stream:set_reply/2' for `{ok, Value}', `set_error/2' for
%%% `{error, Reason}') so the consumer's own `macula:await_reply/1,2'
%%% unblocks with it, before stopping. A module that doesn't export
%%% `handle_eof/1' keeps the exact prior behavior — no reply is ever
%%% set, eof just stops the stream.
%%%
%%% This is the general-purpose RPC streaming feature (`call_stream/5',
%%% `advertise_stream/5', e.g. a `logs.tail_v1'-style procedure) —
%%% unrelated to content sharing's own chunked-transfer protocol; see
%%% `macula_feeder' / `macula_download' for that.
%%%
%%% == Cancel ==
%%%
%%% Stopping this gen_server for any non-`normal' reason (a crash, the
%%% underlying stream dying, `Module:handle_open/2'/`handle_chunk/2'
%%% returning a non-normal stop) sends the peer an explicit
%%% `macula_stream:abort/3' STREAM_ERROR, not just a graceful close —
%%% the peer learns the transfer was cancelled/failed instead of
%%% mistaking it for an ordinary end-of-stream. A `normal' stop closes
%%% both sides cleanly instead.
%%%
%%% == Direct-dial ==
%%%
%%% `advertise/5,6' registers the handler with the pool's advertise-
%%% gossip mechanism only — nothing published lets a caller on another
%%% station find this procedure without a route having propagated
%%% between the two stations first. `advertise_direct/6,7' does that
%%% AND publishes a signed `procedure_advertisement' DHT record naming
%%% this pool's currently-connected station as the server — the exact
%%% same record type and publish function `macula_response:advertise_direct/6,7'
%%% uses for plain RPC (a `procedure_advertisement' does not distinguish
%%% RPC from streaming), so a caller using
%%% `macula_stream_sink:start_link_direct/5,6' can resolve and dial
%%% here directly, in one hop, regardless of whether the two stations
%%% have a routing edge between them.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(log_tailer_provider).
%%% -behaviour(macula_streamer).
%%% -export([init/1, handle_open/2]).
%%%
%%% init(Registry) -> {ok, Registry}.
%%%
%%% handle_open(#{topic := Topic}, Registry) ->
%%%     Registry ! {tailer_ready, Topic, self()},
%%%     {ok, Registry}.
%%% '''
%%%
%%% ```
%%% {ok, _Sup} = macula_streamer:advertise(Pool, Realm,
%%%     <<"logs.tail_v1">>, log_tailer_provider, self()).
%%%
%%% %% elsewhere, once the provider has announced its pid:
%%% ok = macula_streamer:send(TailerPid, <<"a log line\n">>).
%%% '''
%%%
%%% A `client_stream'-mode provider exports `handle_chunk/2' instead,
%%% and never calls `send/2,3' itself — the consumer is the one
%%% pushing:
%%%
%%% ```
%%% -module(batch_upload_provider).
%%% -behaviour(macula_streamer).
%%% -export([init/1, handle_open/2, handle_chunk/2]).
%%%
%%% init(Parent) -> {ok, {Parent, []}}.
%%%
%%% handle_open(_StreamArgs, State) -> {ok, State}.
%%%
%%% handle_chunk(Chunk, {Parent, Acc}) ->
%%%     {noreply, {Parent, [Chunk | Acc]}}.
%%% '''
%%%
%%% ```
%%% {ok, _Sup} = macula_streamer:advertise(Pool, Realm,
%%%     <<"bulk.ingest">>, batch_upload_provider, self(),
%%%     #{mode => client_stream}).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_streamer).

-behaviour(gen_server).

-export([advertise/5, advertise/6, advertise_direct/6, advertise_direct/7,
        unadvertise/3]).
-export([send/2, send/3, close/1]).
-export([start_link/7]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_open(StreamArgs :: term(), State :: term()) ->
    {ok, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback handle_chunk(Chunk :: term(), State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback handle_eof(State :: term()) ->
    {noreply, NewState :: term()}
  | {reply, {ok, term()} | {error, term()}, NewState :: term()}
  | {stop, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> any().

-optional_callbacks([terminate/2, handle_chunk/2, handle_eof/1]).

-define(STREAMING_STARTED, <<"streaming.started_v1">>).
-define(STREAMING_COMPLETED, <<"streaming.completed_v1">>).
-define(RECV_TIMEOUT, 30_000).
-define(CANCEL_CODE, <<"cancelled">>).

-record(tstate, {
    module    :: module(),
    pool      :: macula:pool(),
    realm     :: macula:realm(),
    announce  :: boolean(),
    stream_id :: binary(),
    stream    :: pid(),
    reader    :: pid() | undefined,
    user      :: term()
}).

%% @doc Advertise `Procedure' on `Pool'/`Realm'. Starts a private
%% factory supervisor for per-stream provider children and registers
%% a dispatch handler with `macula:advertise_stream/5'. Returns the
%% supervisor pid so the caller can supervise it (or ignore it).
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args) ->
    advertise(Pool, Realm, Procedure, Module, Args, #{}).

%% @doc As `advertise/5'. `Opts' may include `announce' (default
%% `true') and `mode' (default `server_stream').
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term(), map()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args, Opts) ->
    {ok, Sup} = macula_streamer_sup:start_link(),
    Announce = maps:get(announce, Opts, true),
    Mode = maps:get(mode, Opts, server_stream),
    Handler = fun(StreamPid, StreamArgs) ->
        dispatch(Sup, Module, Pool, Realm, Announce, Args, StreamPid, StreamArgs)
    end,
    case macula:advertise_stream(Pool, Realm, Procedure, Mode, Handler) of
        ok -> {ok, Sup};
        {error, Reason} -> {error, Reason}
    end.

%% @doc As `advertise/5', and additionally publishes a signed
%% `procedure_advertisement' DHT record naming this pool's connected
%% station as the server, so `macula_stream_sink:start_link_direct/5,6'
%% can resolve and dial here directly. `Identity' signs it — reuse the
%% same one across re-advertises so each one doesn't mint a fresh
%% advertiser identity.
%%
%% The DHT publish is best-effort: if it fails, the handler is still
%% advertised and reachable via the ordinary pooled path — direct-dial
%% callers just won't be able to resolve it until a later publish
%% succeeds.
-spec advertise_direct(macula:pool(), macula:realm(), macula:procedure(),
                       module(), term(), macula_identity:key_pair()) ->
    {ok, pid()} | {error, term()}.
advertise_direct(Pool, Realm, Procedure, Module, Args, Identity) ->
    advertise_direct(Pool, Realm, Procedure, Module, Args, Identity, #{}).

%% @doc As `advertise_direct/6', with `Opts' forwarded BOTH to
%% `advertise/6' (so `mode'/`announce' apply here too, e.g.
%% `mode => client_stream') and to
%% `macula_direct_dial:publish_advertisement/5' (e.g. `cert_chain =>
%% ChainPem', Slice 7c Direction B, managed realms only) — each side
%% reads only the keys it recognizes, so one `Opts' map serves both.
-spec advertise_direct(macula:pool(), macula:realm(), macula:procedure(),
                       module(), term(), macula_identity:key_pair(), map()) ->
    {ok, pid()} | {error, term()}.
advertise_direct(Pool, Realm, Procedure, Module, Args, Identity, Opts) ->
    case advertise(Pool, Realm, Procedure, Module, Args, Opts) of
        {ok, Sup} ->
            _ = macula_direct_dial:publish_advertisement(Pool, Realm,
                                                          Procedure, Identity,
                                                          Opts),
            {ok, Sup};
        {error, _} = Error ->
            Error
    end.

%% @doc Stop advertising. Does not stop the factory supervisor
%% returned by `advertise/5,6' — callers that want to tear it down
%% should `exit(Sup, shutdown)' themselves.
-spec unadvertise(macula:pool(), macula:realm(), macula:procedure()) -> ok.
unadvertise(Pool, Realm, Procedure) ->
    macula:unadvertise_stream(Pool, Realm, Procedure).

dispatch(Sup, Module, Pool, Realm, Announce, Args, StreamPid, StreamArgs) ->
    case supervisor:start_child(Sup, [Module, Pool, Realm, Announce, Args,
                                      StreamPid, StreamArgs]) of
        {ok, _Pid} -> ok;
        {error, _Reason} -> ok
    end.

%% @doc Send a chunk out on the stream this streamer owns.
-spec send(pid(), binary()) -> ok | {error, term()}.
send(Pid, Chunk) -> gen_server:call(Pid, {send, Chunk}).

%% @doc As `send/2', with an explicit encoding.
-spec send(pid(), binary() | term(), macula_stream:encoding()) ->
    ok | {error, term()}.
send(Pid, Chunk, Encoding) -> gen_server:call(Pid, {send, Chunk, Encoding}).

%% @doc Close the send side of the stream.
-spec close(pid()) -> ok.
close(Pid) -> gen_server:call(Pid, close).

%% @private
-spec start_link(module(), macula:pool(), macula:realm(), boolean(),
                 term(), pid(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Announce, InitArgs, StreamPid, StreamArgs) ->
    gen_server:start_link(?MODULE,
        {Module, Pool, Realm, Announce, InitArgs, StreamPid, StreamArgs}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Module, Pool, Realm, Announce, InitArgs, StreamPid, StreamArgs}) ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            open(Module, Pool, Realm, Announce, StreamPid, StreamArgs, UserState);
        {stop, Reason} ->
            {stop, Reason}
    end.

open(Module, Pool, Realm, Announce, StreamPid, StreamArgs, UserState) ->
    case Module:handle_open(StreamArgs, UserState) of
        {ok, NewUserState} ->
            link(StreamPid),
            Reader = maybe_spawn_reader(Module, StreamPid),
            StreamId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?STREAMING_STARTED,
                    #{stream_id => StreamId}),
            {ok, #tstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, stream_id = StreamId,
                        stream = StreamPid, reader = Reader,
                        user = NewUserState}};
        {stop, Reason, _NewUserState} ->
            {stop, Reason}
    end.

%% @private For `client_stream'-mode providers that export
%% `handle_chunk/2': spawn the same linked-reader `recv/2' loop
%% `macula_stream_sink' drives on the consumer side, applied here to
%% the provider's own stream. A `server_stream'-mode module has no
%% reason to export `handle_chunk/2', so this is a no-op for it.
maybe_spawn_reader(Module, Stream) ->
    case erlang:function_exported(Module, handle_chunk, 2) of
        true -> spawn_reader(Stream);
        false -> undefined
    end.

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
handle_call({send, Chunk}, _From, #tstate{stream = Stream} = State) ->
    {reply, macula_stream:send(Stream, Chunk), State};
handle_call({send, Chunk, Encoding}, _From, #tstate{stream = Stream} = State) ->
    {reply, macula_stream:send(Stream, Chunk, Encoding), State};
handle_call(close, _From, #tstate{stream = Stream} = State) ->
    {reply, macula_stream:close_send(Stream), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({stream_item, Data}, #tstate{module = Module, user = User} = State) ->
    deliver(Module:handle_chunk(Data, User), State);
handle_info(stream_eof, State) ->
    handle_eof(State);
handle_info({stream_error, Reason}, State) ->
    {stop, Reason, State};
handle_info({'EXIT', Reader, Reason}, #tstate{reader = Reader} = State)
        when Reason =/= normal ->
    {stop, {reader_crashed, Reason}, State};
handle_info({'EXIT', Stream, Reason}, #tstate{stream = Stream} = State) ->
    {stop, Reason, State};
handle_info(_Msg, State) ->
    {noreply, State}.

deliver({noreply, NewUser}, State) ->
    {noreply, State#tstate{user = NewUser}};
deliver({stop, Reason, NewUser}, State) ->
    {stop, Reason, State#tstate{user = NewUser}}.

%% @private Default (no `handle_eof/1' exported): unchanged prior
%% behavior, eof just stops the stream. Otherwise gives the callback
%% one last chance to set a terminal reply before stopping.
handle_eof(#tstate{module = Module, user = User} = State) ->
    case erlang:function_exported(Module, handle_eof, 1) of
        true -> deliver_eof(Module:handle_eof(User), State);
        false -> {stop, normal, State}
    end.

deliver_eof({noreply, NewUser}, State) ->
    {stop, normal, State#tstate{user = NewUser}};
deliver_eof({reply, {ok, Value}, NewUser}, #tstate{stream = Stream} = State) ->
    _ = macula_stream:set_reply(Stream, Value),
    {stop, normal, State#tstate{user = NewUser}};
deliver_eof({reply, {error, Reason}, NewUser}, #tstate{stream = Stream} = State) ->
    _ = macula_stream:set_error(Stream, Reason),
    {stop, normal, State#tstate{user = NewUser}};
deliver_eof({stop, Reason, NewUser}, State) ->
    {stop, Reason, State#tstate{user = NewUser}}.

%% @private
terminate(Reason, #tstate{module = Module, pool = Pool, realm = Realm,
                          announce = Announce, stream_id = StreamId,
                          stream = Stream, reader = Reader, user = User}) ->
    stop_reader(Reader),
    finish_stream(Reason, Stream),
    publish(Announce, Pool, Realm, ?STREAMING_COMPLETED,
            outcome_fields(#{stream_id => StreamId}, Reason)),
    maybe_terminate(Module, Reason, User).

stop_reader(undefined) -> ok;
stop_reader(Reader) ->
    unlink(Reader),
    exit(Reader, kill).

%% @private A `normal' reason closes both sides cleanly. Anything else
%% (a crash, the underlying stream dying, a non-normal stop from
%% `handle_open/2'/`handle_chunk/2') sends the peer an explicit
%% `STREAM_ERROR' abort instead of leaving it to infer cancellation
%% from the connection simply going away. `Stream' may already be
%% dead by the time this runs (e.g. its own exit is what triggered
%% this termination) — harmless, caught below.
finish_stream(normal, Stream) ->
    try macula_stream:close(Stream) catch _:_ -> ok end;
finish_stream(Reason, Stream) ->
    Message = iolist_to_binary(io_lib:format("~p", [Reason])),
    try macula_stream:abort(Stream, ?CANCEL_CODE, Message) catch _:_ -> ok end.

outcome_fields(Base, normal) -> Base#{outcome => completed};
outcome_fields(Base, Reason) -> Base#{outcome => failed, reason => Reason}.

maybe_terminate(Module, Reason, User) ->
    case erlang:function_exported(Module, terminate, 2) of
        true -> Module:terminate(Reason, User);
        false -> ok
    end.

publish(false, _, _, _, _) -> ok;
publish(true, Pool, Realm, Topic, Payload) ->
    _ = macula:publish(Pool, Realm, Topic, Payload), ok.
