%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised content feeders (the put/share side).
%%%
%%% `start_link/4,5' returns immediately with a pid, delivers the
%%% outcome to `Module:handle_fed/2', and publishes
%%% `sharing.put_started_v1' / `sharing.put_completed_v1' mesh facts
%%% around the transfer — including `outcome => cancelled' if the
%%% feeder is stopped before the put resolves.
%%%
%%% This is content sharing, not general-purpose RPC streaming — see
%%% `macula_streamer' / `macula_stream_sink' for that (`streaming.*'
%%% facts belong to that pair).
%%%
%%% == Real cancel, real underneath ==
%%%
%%% Internally this drives `macula_content_transfer' (PLAN_PUSH_UPLOAD.md
%%% Phase 4) rather than a blocking `macula:put_content/2' call run in
%%% a linked worker — that blocking shape had no addressable handle to
%%% the actual transfer, so `cancel/1' (`gen_server:stop/1') could only
%%% ever kill the local worker process waiting on it, never touch the
%%% underlying stream. A `macula_content_transfer' cancelled that way
%%% doesn't even notice: nothing links a `gen_server:call' caller's
%%% death to the callee, so it would run to completion, or sit
%%% resolved-but-never-reaped, forever — orphaned, leaking its
%%% `content_stream_bufs' entry on the link and its
%%% `macula_content_transfer_registry' entry, for no purpose. This
%%% module now holds the `macula_content_transfer' pid directly (a
%%% `content_transfer' state field, alongside the lightweight resolve
%%% + await proxy `worker' that reports it back) so `cancel/1' can call
%%% `macula_content_transfer:cancel/1' on it for real — the same
%%% peer-visible QUIC RESET_STREAM abort described there, not a local
%%% kill with nothing downstream the wiser. The share_id this module
%%% already minted for its own `sharing.*' mesh facts is threaded
%%% through as `macula_content_transfer''s own `share_id' too, so both
%%% layers resolve to the same id.
%%%
%%% == Direct-dial ==
%%%
%%% `start_link/4,5' puts through the pool's own connected link
%%% (whichever `pick_connected_link/1' picks). `start_link_direct/4,5'
%%% is the direct-dial counterpart: unlike `macula_download''s (which
%%% resolves an MCID to find out WHO has it), a PUT already knows its
%%% own target — the caller names `Station' directly, and it is
%%% resolved to a dialable endpoint via that station's own signed
%%% `station_endpoint' record (`macula_direct_dial:resolve_station_endpoint/2',
%%% a fast, non-addressable DHT lookup that stays a plain blocking call
%%% inside the resolve+await proxy — nothing has ever needed to cancel
%%% mid-resolve) and dialed in one hop via `macula_content_transfer:
%%% start_put_station/5', deliberately seeding that specific station
%%% instead of whichever the pool picks. See `macula_direct_dial''s
%%% module doc, "Content" section, for the trust model.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(doc_feeder).
%%% -behaviour(macula_feeder).
%%% -export([init/1, handle_fed/2]).
%%%
%%% init(Parent) -> {ok, Parent}.
%%%
%%% handle_fed(Result, Parent) ->
%%%     Parent ! {fed, Result},
%%%     {stop, normal, Parent}.
%%% '''
%%%
%%% ```
%%% {ok, Pid} = macula_feeder:start_link(doc_feeder, Pool, Realm,
%%%     Bytes, self()).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_feeder).

-behaviour(gen_server).

-export([start_link/4, start_link/5]).
-export([start_link_direct/5, start_link_direct/6]).
-export([cancel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_fed(Result :: {ok, macula:mcid()} | {error, term()}, State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-define(PUT_STARTED, <<"sharing.put_started_v1">>).
-define(PUT_COMPLETED, <<"sharing.put_completed_v1">>).
%% Bounds only the QUIC handshake wait when `start_link_direct/5,6'
%% must dial a fresh link — matches `macula_client:connect/2''s own
%% `connect_timeout_ms' default. The block/manifest transfer that
%% follows has its own separate, internal timeouts regardless.
-define(DIRECT_DIAL_CONNECT_TIMEOUT_MS, 30_000).

-record(fstate, {
    module           :: module(),
    pool             :: macula:pool(),
    realm            :: macula:realm(),
    announce         :: boolean(),
    share_id         :: binary(),
    worker           :: pid(),
    content_transfer :: pid() | undefined,
    completed        :: boolean(),
    user             :: term()
}).

%% @doc Start a feeder. Puts `Bytes' into content storage via `Pool'.
-spec start_link(module(), macula:pool(), macula:realm(), binary()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Bytes) ->
    start_link(Module, Pool, Realm, Bytes, undefined).

%% @doc As `start_link/4', with `Args' passed to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), binary(), term()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Bytes, Args) ->
    gen_server:start_link(?MODULE,
        {pooled, Module, Pool, Realm, Bytes, true, Args}, []).

%% @doc As `start_link/4', but resolves `Station''s own
%% `station_endpoint' and dials it directly instead of putting through
%% the pool's existing links. See the "Direct-dial" section above.
-spec start_link_direct(module(), macula:pool(), macula_identity:pubkey(),
                        macula:realm(), binary()) ->
    {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Station, Realm, Bytes) ->
    start_link_direct(Module, Pool, Station, Realm, Bytes, undefined).

%% @doc As `start_link_direct/5', with `Args' passed to `Module:init/1'.
-spec start_link_direct(module(), macula:pool(), macula_identity:pubkey(),
                        macula:realm(), binary(), term()) ->
    {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Station, Realm, Bytes, Args) ->
    gen_server:start_link(?MODULE,
        {direct, Module, Pool, Station, Realm, Bytes, true, Args}, []).

%% @doc Cancel an in-flight feed. Publishes `sharing.put_completed_v1'
%% with `outcome => cancelled' if the put had not resolved yet.
-spec cancel(pid()) -> ok.
cancel(Pid) -> gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({pooled, Module, Pool, Realm, Bytes, Announce, InitArgs}) ->
    start_feeder(Module, InitArgs, Pool, Realm, Bytes, Announce,
                fun(ShareId) -> spawn_worker(pooled, Pool, Bytes, ShareId) end);
init({direct, Module, Pool, Station, Realm, Bytes, Announce, InitArgs}) ->
    start_feeder(Module, InitArgs, Pool, Realm, Bytes, Announce,
                fun(ShareId) -> spawn_worker(direct, Pool, Station, Bytes, ShareId) end).

start_feeder(Module, InitArgs, Pool, Realm, Bytes, Announce, SpawnFun) ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            ShareId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?PUT_STARTED,
                    #{share_id => ShareId, size => byte_size(Bytes)}),
            Worker = SpawnFun(ShareId),
            {ok, #fstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, share_id = ShareId,
                        worker = Worker, content_transfer = undefined,
                        completed = false, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

%% The lightweight proxy: start the addressable transfer, report its
%% pid back immediately (so `terminate/2' can reach it even if this
%% proxy itself gets killed mid-flight), block for the outcome, reap
%% the transfer (a no-op if it's already being cancelled from outside
%% — see `reap_content_transfer/1'), report the outcome.
spawn_worker(pooled, Pool, Bytes, ShareId) ->
    Parent = self(),
    spawn_link(fun() ->
        {ok, CTPid} = macula_content_transfer:start_put(Pool, Bytes, #{share_id => ShareId}),
        Parent ! {content_transfer, CTPid},
        Result = macula_content_transfer:await(CTPid),
        try macula_content_transfer:cancel(CTPid) catch _:_ -> ok end,
        Parent ! {feed_result, Result}
    end).

%% Resolving `Station''s endpoint stays a plain blocking DHT lookup
%% here (matches what `macula_direct_dial:put_content/4' already did)
%% — only the transfer itself becomes addressable.
spawn_worker(direct, Pool, Station, Bytes, ShareId) ->
    Parent = self(),
    spawn_link(fun() -> direct_worker_run(Pool, Station, Bytes, ShareId, Parent) end).

direct_worker_run(Pool, Station, Bytes, ShareId, Parent) ->
    case macula_direct_dial:resolve_station_endpoint(Pool, Station) of
        {ok, DialUrl} ->
            Opts = #{share_id => ShareId, expected_node_id => Station,
                    pin_tls_cert => false, verify => none},
            {ok, CTPid} = macula_content_transfer:start_put_station(
                Pool, DialUrl, Bytes, ?DIRECT_DIAL_CONNECT_TIMEOUT_MS, Opts),
            Parent ! {content_transfer, CTPid},
            Result = macula_content_transfer:await(CTPid),
            try macula_content_transfer:cancel(CTPid) catch _:_ -> ok end,
            Parent ! {feed_result, Result};
        {error, Reason} ->
            Parent ! {feed_result, {error, {unresolved, Reason}}}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({content_transfer, CTPid}, State) ->
    {noreply, State#fstate{content_transfer = CTPid}};
handle_info({feed_result, Result}, State) ->
    NewState = announce_completed(State, Result),
    #fstate{module = Module, user = User} = NewState,
    deliver(Module:handle_fed(Result, User), NewState#fstate{content_transfer = undefined});
handle_info({'EXIT', Worker, Reason}, #fstate{worker = Worker} = State)
        when Reason =/= normal ->
    {stop, {worker_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

deliver({noreply, NewUser}, State) -> {noreply, State#fstate{user = NewUser}};
deliver({stop, Reason, NewUser}, State) -> {stop, Reason, State#fstate{user = NewUser}}.

%% @private
terminate(_Reason, #fstate{worker = Worker, completed = true}) ->
    unlink(Worker),
    exit(Worker, kill),
    ok;
terminate(_Reason, #fstate{content_transfer = CTPid} = State) ->
    unlink(State#fstate.worker),
    exit(State#fstate.worker, kill),
    reap_content_transfer(CTPid),
    _ = announce_completed(State, {error, cancelled}),
    ok.

%% Killing the proxy `worker' does NOT cascade into stopping the
%% `macula_content_transfer' it started — that gen_server traps exits
%% and doesn't recognize the proxy as one of ITS OWN tracked workers,
%% so an incoming `{'EXIT', Proxy, killed}' just falls through its
%% catch-all `handle_info' clause, unnoticed. This is the actual fix:
%% reach in and cancel it explicitly. `undefined' covers the window
%% before `{content_transfer, CTPid}' has arrived yet (still resolving,
%% for direct-dial) — nothing addressable exists to cancel there,
%% same as before this phase. `catch' covers the benign race where the
%% proxy's own natural reap (in `spawn_worker/4') and an external
%% `cancel/1' land at the same time — the second `cancel/1' call
%% reaches an already-dead pid.
reap_content_transfer(undefined) -> ok;
reap_content_transfer(CTPid) ->
    try macula_content_transfer:cancel(CTPid) catch _:_ -> ok end,
    ok.

announce_completed(#fstate{completed = true} = State, _Result) ->
    State;
announce_completed(#fstate{pool = Pool, realm = Realm, announce = Announce,
                           share_id = ShareId} = State, Result) ->
    publish(Announce, Pool, Realm, ?PUT_COMPLETED,
            outcome_fields(#{share_id => ShareId}, Result)),
    State#fstate{completed = true}.

outcome_fields(Base, {ok, Mcid}) ->
    Base#{outcome => completed, mcid => Mcid, chunked => is_chunked_mcid(Mcid)};
outcome_fields(Base, {error, cancelled}) ->
    Base#{outcome => cancelled};
outcome_fields(Base, {error, Reason}) ->
    Base#{outcome => failed, reason => Reason}.

is_chunked_mcid(<<1, 16#56, _/binary>>) -> true;
is_chunked_mcid(_) -> false.

publish(false, _, _, _, _) -> ok;
publish(true, Pool, Realm, Topic, Payload) ->
    _ = macula:publish(Pool, Realm, Topic, Payload), ok.
