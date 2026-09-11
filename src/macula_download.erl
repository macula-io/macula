%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised content downloads (the get/fetch side).
%%%
%%% `start_link/4,5' returns immediately with a pid, delivers the
%%% outcome to `Module:handle_downloaded/2', and publishes
%%% `sharing.get_started_v1' / `sharing.get_completed_v1' mesh facts
%%% around the transfer — including `outcome => cancelled' if the
%%% download is cancelled before the get resolves.
%%%
%%% This is content sharing, not general-purpose RPC streaming — see
%%% `macula_streamer' / `macula_stream_sink' for that (`streaming.*'
%%% facts belong to that pair).
%%%
%%% == Real cancel, real underneath ==
%%%
%%% Internally this drives `macula_content_transfer' (PLAN_PUSH_UPLOAD.md
%%% Phase 4) rather than a blocking `macula:get_content/2' call run in
%%% a linked worker — see `macula_feeder''s module doc for the full
%%% reasoning (the same gap, the same fix, mirrored here): a blocking
%%% call gives `cancel/1' no addressable handle to the actual transfer,
%%% so it could only ever kill the local worker waiting on it, leaving
%%% the underlying `macula_content_transfer' orphaned — running to
%%% completion or sitting resolved-but-never-reaped forever, since
%%% nothing links a `gen_server:call' caller's death to the callee.
%%% This module now holds the `macula_content_transfer' pid directly (a
%%% `content_transfer' state field, alongside the lightweight resolve +
%%% await proxy `worker', which asks this process to start the transfer)
%%% so `cancel/1' reaches it
%%% for a real, peer-visible QUIC RESET_STREAM abort. The share_id this
%%% module already minted for its own `sharing.*' mesh facts is
%%% threaded through as `macula_content_transfer''s own `share_id' too.
%%%
%%% == Direct-dial ==
%%%
%%% `start_link/4,5' fetches through the pool's own connected link
%%% (whichever `pick_connected_link/1' picks), reaching a copy via that
%%% station's 1-hop peer relay. `start_link_direct/4,5' is the
%%% direct-dial counterpart: it resolves `Mcid''s provider from its
%%% signed `content_announcement' (published automatically by the
%%% provider's station on receipt — nothing to advertise explicitly, no
%%% direct-dial counterpart needed on the `macula_feeder' side, a fast,
%%% non-addressable DHT lookup that stays a plain blocking call inside
%%% the resolve+await proxy — nothing has ever needed to cancel
%%% mid-resolve) and dials that station directly, in one hop, via
%%% `macula_content_transfer:start_get_station/5', instead of depending
%%% on the caller's own station being able to reach it via relay. Every
%%% provider that announced `Mcid' is a candidate: one whose fetch fails,
%%% or whose bytes don't verify, is passed over for the next, within
%%% `?DIRECT_DIAL_TIMEOUT_MS' (`macula_direct_dial:fetch_content/4'). Only
%%% chunked content is discoverable this way — see
%%% `macula:find_content_providers/2'. See `macula_direct_dial''s module
%%% doc, "Content" section, for the trust model (deliberately lighter
%%% than RPC's — content is self-verifying by hash).
%%%
%%% == Transfer I/O ==
%%%
%%% A download starts, awaits and cancels its transfer with `start_get/3',
%%% `start_get_station/5', `await/1' and `cancel/1', the
%%% `macula_content_transfer' ones by default; chooses its provider for
%%% `start_link_direct' with `fetch_content/4', `macula_direct_dial''s by
%%% default; and announces its facts with `fact_publish',
%%% `macula:publish/4' by default. `start_link/6' and
%%% `start_link_direct/6' take them in their start options, the four
%%% transfer functions as `transfer_io', checked by
%%% `macula_content_transfer:transfer_io/2', and pass a `link_io' option on
%%% to the transfer they start. A function of another shape is refused
%%% with `function_clause', in the caller.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(doc_download).
%%% -behaviour(macula_download).
%%% -export([init/1, handle_downloaded/2]).
%%%
%%% init(Parent) -> {ok, Parent}.
%%%
%%% handle_downloaded(Result, Parent) ->
%%%     Parent ! {downloaded, Result},
%%%     {stop, normal, Parent}.
%%% '''
%%%
%%% ```
%%% {ok, Pid} = macula_download:start_link(doc_download, Pool, Realm,
%%%     Mcid, self()).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_download).

-behaviour(gen_server).

-export([start_link/4, start_link/5, start_link/6]).
-export([start_link_direct/4, start_link_direct/5, start_link_direct/6]).
-export([cancel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_downloaded(Result :: {ok, binary()} | {error, term()}, State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-define(GET_STARTED, <<"sharing.get_started_v1">>).
-define(GET_COMPLETED, <<"sharing.get_completed_v1">>).
%% Bounds how long `start_link_direct/4,5' looks for a provider whose fetch
%% succeeds: the DHT lookups, each provider's connect wait, and moving on
%% to the next provider after one fails. A transfer that has started runs
%% to its own end, bounded by its internal timeouts and by `cancel/1'.
%% Matches `macula_client:connect/2''s own `connect_timeout_ms' default.
-define(DIRECT_DIAL_TIMEOUT_MS, 30_000).

-export_type([start_opts/0]).

-type start_opts() :: #{transfer_io => macula_content_transfer:transfer_io(),
                        fetch_content => fun((macula:pool(), macula:mcid(), pos_integer(),
                                              fun()) -> {ok, binary()} | {error, term()}),
                        fact_publish => macula_lifetime_announcer:publish(),
                        link_io => macula_content_transfer:link_io()}.

-record(dstate, {
    module           :: module(),
    pool             :: macula:pool(),
    realm            :: macula:realm(),
    announce         :: boolean(),
    transfer_io      :: macula_content_transfer:transfer_io(),
    fact_publish     :: macula_lifetime_announcer:publish(),
    share_id         :: binary(),
    worker           :: pid(),
    content_transfer :: pid() | undefined,
    completed        :: boolean(),
    user             :: term()
}).

%% @doc Start a download. Fetches `Mcid' via `Pool'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:mcid()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Mcid) ->
    start_link(Module, Pool, Realm, Mcid, undefined).

%% @doc As `start_link/4', with `Args' passed to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:mcid(), term()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Mcid, Args) ->
    start_link(Module, Pool, Realm, Mcid, Args, #{}).

%% @doc As `start_link/5', with start options (see "Transfer I/O" above).
-spec start_link(module(), macula:pool(), macula:realm(), macula:mcid(), term(),
                 start_opts()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Mcid, Args, Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE,
        {pooled, Module, Pool, Realm, Mcid, true, Args, functions(Opts)}, []).

%% @doc As `start_link/4', but resolves and dials the MCID's provider
%% directly instead of fetching through the pool's existing links. See
%% the "Direct-dial" section above.
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:mcid()) -> {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Mcid) ->
    start_link_direct(Module, Pool, Realm, Mcid, undefined).

%% @doc As `start_link_direct/4', with `Args' passed to `Module:init/1'.
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:mcid(), term()) -> {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Mcid, Args) ->
    start_link_direct(Module, Pool, Realm, Mcid, Args, #{}).

%% @doc As `start_link_direct/5', with start options (see "Transfer I/O"
%% above).
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:mcid(), term(), start_opts()) -> {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Mcid, Args, Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE,
        {direct, Module, Pool, Realm, Mcid, true, Args, functions(Opts)}, []).

%% @doc Cancel an in-flight download. Publishes `sharing.get_completed_v1'
%% with `outcome => cancelled' if the get had not resolved yet.
-spec cancel(pid()) -> ok.
cancel(Pid) -> gen_server:stop(Pid).

%% The functions a download runs on, from its start options or else the
%% defaults; one of another shape is refused with function_clause, in the
%% caller. `link_io' goes on to the transfer the download starts.
functions(Opts) ->
    TransferIo = macula_content_transfer:transfer_io(default_transfer_io(),
                                                     maps:get(transfer_io, Opts, undefined)),
    #{transfer_io => TransferIo,
      fetch_content =>
          arity_4(maps:get(fetch_content, Opts, fun macula_direct_dial:fetch_content/4)),
      fact_publish => arity_4(maps:get(fact_publish, Opts, fun macula:publish/4)),
      transfer_opts => maps:with([link_io], Opts)}.

default_transfer_io() ->
    #{start_get => fun macula_content_transfer:start_get/3,
      start_get_station => fun macula_content_transfer:start_get_station/5,
      await => fun macula_content_transfer:await/1,
      cancel => fun macula_content_transfer:cancel/1}.

arity_4(Fun) when is_function(Fun, 4) -> Fun.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private `Mcid' is rejected here, before `Module:init/1' runs or
%% anything is spawned/announced, when it doesn't carry one of the two
%% codec bytes `macula:put_content/2' ever mints — see `macula:get_content/2'
%% for the full reasoning. This is a share/download entry point, so
%% `Mcid' plausibly comes from outside this SDK (a share link, a
%% caller's own storage) rather than always being freshly minted.
init({DialMode, Module, Pool, Realm, <<1, Codec, _/binary>> = Mcid, Announce, InitArgs,
      #{transfer_io := TransferIo, fact_publish := FactPublish} = Functions})
        when Codec =:= 16#55 orelse Codec =:= 16#56 ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            ShareId = crypto:strong_rand_bytes(16),
            publish(Announce, FactPublish, Pool, Realm, ?GET_STARTED,
                    #{share_id => ShareId, mcid => Mcid,
                      chunked => is_chunked_mcid(Mcid)}),
            Worker = spawn_worker(DialMode, Functions, Pool, Mcid, ShareId),
            {ok, #dstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, transfer_io = TransferIo,
                        fact_publish = FactPublish, share_id = ShareId,
                        worker = Worker, content_transfer = undefined,
                        completed = false, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end;
init({_DialMode, _Module, _Pool, _Realm, _Mcid, _Announce, _InitArgs, _Functions}) ->
    {stop, invalid_mcid}.

%% The lightweight proxy: have this download start the addressable
%% transfer (see `run_transfer/2'), block for the outcome, reap the
%% transfer (a no-op if it's already being cancelled from outside, see
%% `reap_content_transfer/1'), report the outcome.
spawn_worker(pooled, #{transfer_io := TransferIo, transfer_opts := TransferOpts}, Pool, Mcid,
             ShareId) ->
    Parent = self(),
    Start = pooled_get(TransferIo, Pool, Mcid, TransferOpts#{share_id => ShareId}),
    spawn_link(fun() -> run_transfer(Parent, TransferIo, Start) end);
%% Choosing among `Mcid''s providers runs in the worker through the
%% download's fetch function, `macula_direct_dial:fetch_content/4' by
%% default, and each provider's transfer is started by this download
%% (`transfer/3'), so a cancel reaches whichever transfer is running.
spawn_worker(direct, Functions, Pool, Mcid, ShareId) ->
    Parent = self(),
    spawn_link(fun() -> direct_worker_run(Functions, Pool, Mcid, ShareId, Parent) end).

direct_worker_run(#{transfer_io := TransferIo, transfer_opts := TransferOpts,
                    fetch_content := FetchContent}, Pool, Mcid, ShareId, Parent) ->
    Fetch = fun(Endpoint, Pinned, ConnectMs, _RemainingMs) ->
                Opts = maps:merge(TransferOpts, Pinned#{share_id => ShareId}),
                transfer(Parent, TransferIo,
                         station_get(TransferIo, Pool, Endpoint, Mcid, ConnectMs, Opts))
            end,
    Parent ! {download_result, FetchContent(Pool, Mcid, ?DIRECT_DIAL_TIMEOUT_MS, Fetch)}.

pooled_get(#{start_get := StartGet}, Pool, Mcid, Opts) ->
    fun() -> StartGet(Pool, Mcid, Opts) end.

station_get(#{start_get_station := StartGetStation}, Pool, Endpoint, Mcid, ConnectMs, Opts) ->
    fun() -> StartGetStation(Pool, Endpoint, Mcid, ConnectMs, Opts) end.

%% This download starts the transfer itself, in a call from the worker,
%% so the transfer's pid is in the state before any `cancel/1' is
%% handled: a cancel handled first finds nothing started, and a cancel
%% handled after finds the pid.
transfer(Parent, #{await := Await} = TransferIo, Start) ->
    {ok, CTPid} = gen_server:call(Parent, {start_transfer, Start}, infinity),
    Result = Await(CTPid),
    reap_content_transfer(TransferIo, CTPid),
    Result.

run_transfer(Parent, TransferIo, Start) ->
    Parent ! {download_result, transfer(Parent, TransferIo, Start)}.

%% @private
handle_call({start_transfer, Start}, _From, State) ->
    Started = Start(),
    {reply, Started, record_transfer(Started, State)};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

record_transfer({ok, CTPid}, State) -> State#dstate{content_transfer = CTPid};
record_transfer(_NotStarted, State) -> State.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({download_result, Result}, State) ->
    NewState = announce_completed(State, Result),
    #dstate{module = Module, user = User} = NewState,
    deliver(Module:handle_downloaded(Result, User), NewState#dstate{content_transfer = undefined});
handle_info({'EXIT', Worker, Reason}, #dstate{worker = Worker} = State)
        when Reason =/= normal ->
    {stop, {worker_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

deliver({noreply, NewUser}, State) -> {noreply, State#dstate{user = NewUser}};
deliver({stop, Reason, NewUser}, State) -> {stop, Reason, State#dstate{user = NewUser}}.

%% @private
terminate(_Reason, #dstate{worker = Worker, completed = true}) ->
    unlink(Worker),
    exit(Worker, kill),
    ok;
terminate(_Reason, #dstate{transfer_io = TransferIo, content_transfer = CTPid} = State) ->
    unlink(State#dstate.worker),
    exit(State#dstate.worker, kill),
    reap_content_transfer(TransferIo, CTPid),
    _ = announce_completed(State, {error, cancelled}),
    ok.

%% Killing the proxy `worker' does NOT cascade into stopping the
%% `macula_content_transfer' it waits on — see `macula_feeder''s
%% identical helper for the full reasoning. `undefined' covers a cancel
%% handled before the worker asked for the transfer to start (still
%% resolving, for direct-dial): nothing was started. `catch' covers the
%% benign race between the proxy's own natural reap and an external
%% `cancel/1' landing at the same time.
reap_content_transfer(_TransferIo, undefined) -> ok;
reap_content_transfer(#{cancel := Cancel}, CTPid) ->
    try Cancel(CTPid) catch _:_ -> ok end,
    ok.

announce_completed(#dstate{completed = true} = State, _Result) ->
    State;
announce_completed(#dstate{pool = Pool, realm = Realm, announce = Announce,
                           fact_publish = FactPublish, share_id = ShareId} = State,
                   Result) ->
    publish(Announce, FactPublish, Pool, Realm, ?GET_COMPLETED,
            outcome_fields(#{share_id => ShareId}, Result)),
    State#dstate{completed = true}.

outcome_fields(Base, {ok, Bytes}) ->
    Base#{outcome => completed, size => byte_size(Bytes)};
outcome_fields(Base, {error, cancelled}) ->
    Base#{outcome => cancelled};
outcome_fields(Base, {error, Reason}) ->
    Base#{outcome => failed, reason => Reason}.

is_chunked_mcid(<<1, 16#56, _/binary>>) -> true;
is_chunked_mcid(_) -> false.

publish(false, _FactPublish, _, _, _, _) -> ok;
publish(true, FactPublish, Pool, Realm, Topic, Payload) ->
    _ = FactPublish(Pool, Realm, Topic, Payload), ok.
