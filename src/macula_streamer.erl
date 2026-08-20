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
%%% `Module:handle_open/3', and publishing `streaming.started_v1' /
%%% `streaming.completed_v1' mesh facts around the stream's lifetime.
%%%
%%% Sending is push-based and driven from outside the callback: once
%%% `Module:handle_open/3' has done whatever registration it needs
%%% (e.g. stashing `self()' in a registry keyed by some connection id),
%%% any process holding this streamer's pid can call `send/2,3' /
%%% `close/1' on it. This module does not prescribe the discovery
%%% mechanism.
%%%
%%% This is the general-purpose RPC streaming feature (`call_stream/5',
%%% `advertise_stream/5', e.g. a `logs.tail_v1'-style procedure) —
%%% unrelated to content sharing's own chunked-transfer protocol; see
%%% `macula_feeder' / `macula_downloader' for that.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(log_tailer_provider).
%%% -behaviour(macula_streamer).
%%% -export([init/1, handle_open/3]).
%%%
%%% init(Registry) -> {ok, Registry}.
%%%
%%% handle_open(#{topic := Topic}, Registry, State) ->
%%%     Registry ! {tailer_ready, Topic, self()},
%%%     {ok, State}.
%%% '''
%%%
%%% ```
%%% {ok, _Sup} = macula_streamer:advertise(Pool, Realm,
%%%     <<"logs.tail_v1">>, log_tailer_provider, self()).
%%%
%%% %% elsewhere, once the provider has announced its pid:
%%% ok = macula_streamer:send(TailerPid, <<"a log line\n">>).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_streamer).

-behaviour(gen_server).

-export([advertise/5, advertise/6, unadvertise/3]).
-export([send/2, send/3, close/1]).
-export([start_link/7]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_open(StreamArgs :: term(), State :: term()) ->
    {ok, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> any().

-optional_callbacks([terminate/2]).

-define(STREAMING_STARTED, <<"streaming.started_v1">>).
-define(STREAMING_COMPLETED, <<"streaming.completed_v1">>).

-record(tstate, {
    module    :: module(),
    pool      :: macula:pool(),
    realm     :: macula:realm(),
    announce  :: boolean(),
    stream_id :: binary(),
    stream    :: pid(),
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
            StreamId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?STREAMING_STARTED,
                    #{stream_id => StreamId}),
            {ok, #tstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, stream_id = StreamId,
                        stream = StreamPid, user = NewUserState}};
        {stop, Reason, _NewUserState} ->
            {stop, Reason}
    end.

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
handle_info({'EXIT', Stream, Reason}, #tstate{stream = Stream} = State) ->
    {stop, Reason, State};
handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
terminate(Reason, #tstate{module = Module, pool = Pool, realm = Realm,
                          announce = Announce, stream_id = StreamId,
                          user = User}) ->
    publish(Announce, Pool, Realm, ?STREAMING_COMPLETED,
            outcome_fields(#{stream_id => StreamId}, Reason)),
    maybe_terminate(Module, Reason, User).

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
