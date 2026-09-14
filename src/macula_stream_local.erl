%%%-------------------------------------------------------------------
%%% @doc Local registry + dispatcher for streaming RPC.
%%%
%%% LOCAL streaming only — the client-side and server-side
%%% `macula_stream' processes both live in the same BEAM and are
%%% paired with `macula_stream:pair/2'. This module is the registry
%%% that lets `call_stream' find a locally-advertised handler for a
%%% given procedure name.
%%%
%%% Cross-node streaming travels through `macula_station_link' (V2
%%% pool); the public SDK surface in `macula.erl' is identical
%%% between the LOCAL and pool paths — only the entry point arity
%%% differs (`call_stream/3' vs `call_stream/5').
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_local).

-behaviour(gen_server).

-export([
    start_link/0,
    advertise/2,
    advertise/3,
    unadvertise/1,
    call_stream/3,
    open_stream/3,
    list_advertised/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).

-type handler() :: fun((Stream :: pid(), Args :: term()) -> any()).
-type mode() :: macula_stream:mode().

-record(state, {
    %% procedure (binary) -> {Mode, Handler}
    handlers = #{} :: #{binary() => {mode(), handler()}}
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Advertise a streaming procedure (default mode: server_stream).
-spec advertise(binary(), handler()) -> ok | {error, term()}.
advertise(Procedure, Handler) ->
    advertise(Procedure, server_stream, Handler).

-spec advertise(binary(), mode(), handler()) -> ok | {error, term()}.
advertise(Procedure, Mode, Handler)
  when is_binary(Procedure), is_function(Handler, 2) ->
    case Mode of
        server_stream -> ok;
        client_stream -> ok;
        bidi -> ok
    end,
    gen_server:call(?SERVER, {advertise, Procedure, Mode, Handler}).

-spec unadvertise(binary()) -> ok.
unadvertise(Procedure) when is_binary(Procedure) ->
    gen_server:call(?SERVER, {unadvertise, Procedure}).

%% @doc Open a server-stream call. Returns the client-side stream pid.
%% The caller drains chunks with macula_stream:recv/2.
-spec call_stream(binary(), term(), map()) ->
        {ok, pid()} | {error, term()}.
call_stream(Procedure, Args, Opts) ->
    open_kind(Procedure, Args, Opts, server_stream).

%% @doc Open a client-stream or bidi call. Returns the client-side
%% stream pid; caller writes with macula_stream:send/2,3 and reads
%% the terminal value with macula_stream:await_reply/1,2.
-spec open_stream(binary(), term(), map()) ->
        {ok, pid()} | {error, term()}.
open_stream(Procedure, Args, Opts) ->
    Mode = maps:get(mode, Opts, bidi),
    open_kind(Procedure, Args, Opts, Mode).

-spec list_advertised() -> [{binary(), mode()}].
list_advertised() ->
    gen_server:call(?SERVER, list_advertised).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({advertise, Procedure, Mode, Handler}, _From, State) ->
    NewHandlers = maps:put(Procedure, {Mode, Handler}, State#state.handlers),
    {reply, ok, State#state{handlers = NewHandlers}};

handle_call({unadvertise, Procedure}, _From, State) ->
    {reply, ok, State#state{handlers = maps:remove(Procedure, State#state.handlers)}};

handle_call({lookup, Procedure}, _From, State) ->
    case maps:find(Procedure, State#state.handlers) of
        {ok, Entry} -> {reply, {ok, Entry}, State};
        error -> {reply, {error, not_advertised}, State}
    end;

handle_call(list_advertised, _From, State) ->
    L = [{P, M} || {P, {M, _H}} <- maps:to_list(State#state.handlers)],
    {reply, L, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

%%%===================================================================
%%% Internal
%%%===================================================================

open_kind(Procedure, Args, Opts, RequestedMode) ->
    case gen_server:call(?SERVER, {lookup, Procedure}) of
        {ok, {AdvertisedMode, Handler}} ->
            ensure_compatible(RequestedMode, AdvertisedMode),
            spawn_pair(Procedure, AdvertisedMode, Handler, Args, Opts);
        {error, not_advertised} = Err ->
            Err
    end.

ensure_compatible(_Requested, _Advertised) ->
    %% Mode mismatch handling left lenient in Phase 1: we trust the
    %% advertise side. Phase 2 protocol will negotiate / reject up
    %% front in STREAM_OPEN.
    ok.

spawn_pair(Procedure, Mode, Handler, Args, Opts) ->
    StreamId = stream_id(),
    Caller = maps:get(owner, Opts, self()),
    {ok, ClientPid} = macula_stream:start_link(#{
        id => StreamId,
        role => client,
        mode => Mode,
        owner => Caller
    }),
    %% The handler runs in a dedicated process that owns the server-side
    %% stream: a crashing handler doesn't take down the caller, the handler
    %% can block on recv/send without affecting the client, and the stream
    %% ends when the handler does, unless the handler hands it over first
    %% (`macula_stream:controlling_process/2').
    HandlerPid = spawn_handler(Handler, Args, Procedure),
    {ok, ServerPid} = macula_stream:start_link(#{
        id => StreamId,
        role => server,
        mode => Mode,
        owner => HandlerPid
    }),
    ok = macula_stream:pair(ClientPid, ServerPid),
    HandlerPid ! {serve, ServerPid},
    {ok, ClientPid}.

spawn_handler(Handler, Args, Procedure) ->
    spawn(fun() -> serve_when_paired(Handler, Args, Procedure) end).

serve_when_paired(Handler, Args, Procedure) ->
    receive
        {serve, Stream} -> run_handler(Handler, Stream, Args, Procedure)
    end.

%% A handler crash aborts the stream with the crash class as the code
%% and the reason's name as the message, and none of the crash's
%% terms; the crash goes to the node's log.
run_handler(Handler, Stream, Args, Procedure) ->
    try Handler(Stream, Args)
    catch
        Class:Reason:Stack ->
            logger:warning(
              "[macula_stream_local] handler ~ts crashed: ~ts",
              [Procedure, macula_reason_name:logged("~p:~p~n  stack=~p",
                                                    [Class, Reason, Stack])]),
            _ = macula_stream:abort(Stream, atom_to_binary(Class, utf8),
                                    macula_reason_name:text(Reason)),
            ok
    end.

%% @private 16-byte unique stream id.
stream_id() ->
    crypto:strong_rand_bytes(16).
