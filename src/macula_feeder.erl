%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised content feeders (the put/share side).
%%%
%%% `put_content/2' is a plain blocking call — no addressable pid to
%%% cancel it from outside. This is the provider-side counterpart to
%%% `macula_download': `start_link/4,5' returns immediately with a
%%% pid, runs `macula:put_content/2' in a linked worker, delivers the
%%% outcome to `Module:handle_fed/2', and publishes
%%% `sharing.put_started_v1' / `sharing.put_completed_v1' mesh facts
%%% around the transfer — including `outcome => cancelled' if the
%%% feeder is stopped before the put resolves.
%%%
%%% This is content sharing, not general-purpose RPC streaming — see
%%% `macula_streamer' / `macula_stream_sink' for that (`streaming.*'
%%% facts belong to that pair).
%%%
%%% == Direct-dial ==
%%%
%%% `start_link/4,5' puts through the pool's own connected link
%%% (whichever `pick_connected_link/1' picks). `start_link_direct/4,5'
%%% is the direct-dial counterpart: unlike `macula_download''s (which
%%% resolves an MCID to find out WHO has it), a PUT already knows its
%%% own target — the caller names `Station' directly, and it is
%%% resolved to a dialable endpoint via that station's own signed
%%% `station_endpoint' record and dialed in one hop, deliberately
%%% seeding that specific station instead of whichever the pool picks.
%%% See `macula_direct_dial''s module doc, "Content" section, for the
%%% trust model.
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
    module    :: module(),
    pool      :: macula:pool(),
    realm     :: macula:realm(),
    announce  :: boolean(),
    share_id  :: binary(),
    worker    :: pid(),
    completed :: boolean(),
    user      :: term()
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
                fun() -> spawn_worker(pooled, Pool, Bytes) end);
init({direct, Module, Pool, Station, Realm, Bytes, Announce, InitArgs}) ->
    start_feeder(Module, InitArgs, Pool, Realm, Bytes, Announce,
                fun() -> spawn_worker(direct, Pool, Station, Bytes) end).

start_feeder(Module, InitArgs, Pool, Realm, Bytes, Announce, SpawnFun) ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            ShareId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?PUT_STARTED,
                    #{share_id => ShareId, size => byte_size(Bytes)}),
            Worker = SpawnFun(),
            {ok, #fstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, share_id = ShareId,
                        worker = Worker, completed = false, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

spawn_worker(pooled, Pool, Bytes) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = macula:put_content(Pool, Bytes),
        Parent ! {feed_result, Result}
    end).

spawn_worker(direct, Pool, Station, Bytes) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = macula_direct_dial:put_content(Pool, Station, Bytes,
                                                 ?DIRECT_DIAL_CONNECT_TIMEOUT_MS),
        Parent ! {feed_result, Result}
    end).

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({feed_result, Result}, State) ->
    NewState = announce_completed(State, Result),
    #fstate{module = Module, user = User} = NewState,
    deliver(Module:handle_fed(Result, User), NewState);
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
terminate(_Reason, State) ->
    unlink(State#fstate.worker),
    exit(State#fstate.worker, kill),
    _ = announce_completed(State, {error, cancelled}),
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
