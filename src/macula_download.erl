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
%%% await proxy `worker' that reports it back) so `cancel/1' reaches it
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
%%% on the caller's own station being able to reach it via relay. Only
%%% chunked content is discoverable this way — see
%%% `macula:find_content_providers/2'. See `macula_direct_dial''s module
%%% doc, "Content" section, for the trust model (deliberately lighter
%%% than RPC's — content is self-verifying by hash).
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

-export([start_link/4, start_link/5]).
-export([start_link_direct/4, start_link_direct/5]).
-export([cancel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_downloaded(Result :: {ok, binary()} | {error, term()}, State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-define(GET_STARTED, <<"sharing.get_started_v1">>).
-define(GET_COMPLETED, <<"sharing.get_completed_v1">>).
%% Bounds only the QUIC handshake wait when `start_link_direct/4,5'
%% must dial a fresh link — matches `macula_client:connect/2''s own
%% `connect_timeout_ms' default. The block/manifest transfer that
%% follows has its own separate, internal timeouts regardless.
-define(DIRECT_DIAL_CONNECT_TIMEOUT_MS, 30_000).

-record(dstate, {
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

%% @doc Start a download. Fetches `Mcid' via `Pool'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:mcid()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Mcid) ->
    start_link(Module, Pool, Realm, Mcid, undefined).

%% @doc As `start_link/4', with `Args' passed to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:mcid(), term()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Mcid, Args) ->
    gen_server:start_link(?MODULE,
        {pooled, Module, Pool, Realm, Mcid, true, Args}, []).

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
    gen_server:start_link(?MODULE,
        {direct, Module, Pool, Realm, Mcid, true, Args}, []).

%% @doc Cancel an in-flight download. Publishes `sharing.get_completed_v1'
%% with `outcome => cancelled' if the get had not resolved yet.
-spec cancel(pid()) -> ok.
cancel(Pid) -> gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private `Mcid' is rejected here, before `Module:init/1' runs or
%% anything is spawned/announced, when it doesn't carry one of the two
%% codec bytes `macula:put_content/2' ever mints — see `macula:get_content/2'
%% for the full reasoning. This is a share/download entry point, so
%% `Mcid' plausibly comes from outside this SDK (a share link, a
%% caller's own storage) rather than always being freshly minted.
init({DialMode, Module, Pool, Realm, <<1, Codec, _/binary>> = Mcid, Announce, InitArgs})
        when Codec =:= 16#55 orelse Codec =:= 16#56 ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            ShareId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?GET_STARTED,
                    #{share_id => ShareId, mcid => Mcid,
                      chunked => is_chunked_mcid(Mcid)}),
            Worker = spawn_worker(DialMode, Pool, Mcid, ShareId),
            {ok, #dstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, share_id = ShareId,
                        worker = Worker, content_transfer = undefined,
                        completed = false, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end;
init({_DialMode, _Module, _Pool, _Realm, _Mcid, _Announce, _InitArgs}) ->
    {stop, invalid_mcid}.

%% The lightweight proxy: start the addressable transfer, report its
%% pid back immediately (so `terminate/2' can reach it even if this
%% proxy itself gets killed mid-flight), block for the outcome, reap
%% the transfer (a no-op if it's already being cancelled from outside
%% — see `reap_content_transfer/1'), report the outcome.
spawn_worker(pooled, Pool, Mcid, ShareId) ->
    Parent = self(),
    spawn_link(fun() ->
        {ok, CTPid} = macula_content_transfer:start_get(Pool, Mcid, #{share_id => ShareId}),
        Parent ! {content_transfer, CTPid},
        Result = macula_content_transfer:await(CTPid),
        try macula_content_transfer:cancel(CTPid) catch _:_ -> ok end,
        Parent ! {download_result, Result}
    end);
%% Resolving `Mcid''s provider stays a plain blocking DHT lookup here
%% (matches what `macula_direct_dial:get_content/3' already did) —
%% only the transfer itself becomes addressable.
spawn_worker(direct, Pool, Mcid, ShareId) ->
    Parent = self(),
    spawn_link(fun() -> direct_worker_run(Pool, Mcid, ShareId, Parent) end).

direct_worker_run(Pool, Mcid, ShareId, Parent) ->
    case macula_direct_dial:resolve_content_provider(Pool, Mcid) of
        {ok, #{announcer_node := Node, endpoint := Endpoint}} ->
            Opts = #{share_id => ShareId, expected_node_id => Node,
                    pin_tls_cert => false, verify => none},
            {ok, CTPid} = macula_content_transfer:start_get_station(
                Pool, Endpoint, Mcid, ?DIRECT_DIAL_CONNECT_TIMEOUT_MS, Opts),
            Parent ! {content_transfer, CTPid},
            Result = macula_content_transfer:await(CTPid),
            try macula_content_transfer:cancel(CTPid) catch _:_ -> ok end,
            Parent ! {download_result, Result};
        {error, Reason} ->
            Parent ! {download_result, {error, {unresolved, Reason}}}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({content_transfer, CTPid}, State) ->
    {noreply, State#dstate{content_transfer = CTPid}};
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
terminate(_Reason, #dstate{content_transfer = CTPid} = State) ->
    unlink(State#dstate.worker),
    exit(State#dstate.worker, kill),
    reap_content_transfer(CTPid),
    _ = announce_completed(State, {error, cancelled}),
    ok.

%% Killing the proxy `worker' does NOT cascade into stopping the
%% `macula_content_transfer' it started — see `macula_feeder''s
%% identical helper for the full reasoning. `undefined' covers the
%% window before `{content_transfer, CTPid}' has arrived yet (still
%% resolving, for direct-dial). `catch' covers the benign race between
%% the proxy's own natural reap and an external `cancel/1' landing at
%% the same time.
reap_content_transfer(undefined) -> ok;
reap_content_transfer(CTPid) ->
    try macula_content_transfer:cancel(CTPid) catch _:_ -> ok end,
    ok.

announce_completed(#dstate{completed = true} = State, _Result) ->
    State;
announce_completed(#dstate{pool = Pool, realm = Realm, announce = Announce,
                           share_id = ShareId} = State, Result) ->
    publish(Announce, Pool, Realm, ?GET_COMPLETED,
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

publish(false, _, _, _, _) -> ok;
publish(true, Pool, Realm, Topic, Payload) ->
    _ = macula:publish(Pool, Realm, Topic, Payload), ok.
