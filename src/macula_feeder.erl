%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised content feeders (the share side).
%%%
%%% `start_link/4,5,6' returns immediately with a pid, shares the bytes from
%%% this node (`macula:share_content/4', D27: the node keeps and serves what
%%% it shares), delivers the root content id to `Module:handle_fed/2', and
%%% publishes `sharing.put_started_v1' / `sharing.put_completed_v1' mesh facts
%%% around it, including `outcome => cancelled' when `cancel/1' lands before
%%% the share resolves. A cancelled share is withdrawn: the root content id
%%% is known from the bytes before anything is sent, and it is unshared, so a
%%% cancel never leaves content shared behind the caller's back.
%%%
%%% This is content sharing, not general-purpose RPC streaming; see
%%% `macula_streamer' / `macula_stream_sink' for that.
%%%
%%% == Start options ==
%%%
%%% `share' and `unshare', as `macula:share_content/4' and
%%% `macula:unshare_content/3' (the defaults); `share_opts', passed to
%%% `share' (`name', `org'); `fact_publish', `macula:publish/4' by default. A
%%% function of another shape is refused with `function_clause', in the caller.
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

-export([start_link/4, start_link/5, start_link/6]).
-export([cancel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_fed(Result :: {ok, macula:mcid()} | {error, term()}, State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-define(PUT_STARTED, <<"sharing.put_started_v1">>).
-define(PUT_COMPLETED, <<"sharing.put_completed_v1">>).

-export_type([start_opts/0]).

-type start_opts() :: #{share => fun((macula:pool(), macula:realm(), binary(), map()) ->
                                          {ok, macula:mcid()} | {error, term()}),
                        unshare => fun((macula:pool(), macula:realm(), macula:mcid()) -> ok),
                        share_opts => map(),
                        fact_publish => macula_lifetime_announcer:publish()}.

-record(fstate, {
    module       :: module(),
    pool         :: macula:pool(),
    realm        :: macula:realm(),
    mcid         :: macula:mcid(),
    unshare      :: fun((macula:pool(), macula:realm(), macula:mcid()) -> ok),
    fact_publish :: macula_lifetime_announcer:publish(),
    share_id     :: binary(),
    worker       :: pid(),
    completed    :: boolean(),
    user         :: term()
}).

%% @doc Start a feeder sharing `Bytes' in `Realm' through `Pool'.
-spec start_link(module(), macula:pool(), macula:realm(), binary()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Bytes) ->
    start_link(Module, Pool, Realm, Bytes, undefined).

%% @doc As `start_link/4', with `Args' passed to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), binary(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Bytes, Args) ->
    start_link(Module, Pool, Realm, Bytes, Args, #{}).

%% @doc As `start_link/5', with start options (see "Start options" above).
-spec start_link(module(), macula:pool(), macula:realm(), binary(), term(), start_opts()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Bytes, Args, Opts) when is_binary(Bytes), is_map(Opts) ->
    Functions = #{share => arity_4(maps:get(share, Opts, fun macula:share_content/4)),
                  unshare => arity_3(maps:get(unshare, Opts, fun macula:unshare_content/3)),
                  share_opts => maps:get(share_opts, Opts, #{}),
                  fact_publish => arity_4(maps:get(fact_publish, Opts, fun macula:publish/4))},
    gen_server:start_link(?MODULE, {Module, Pool, Realm, Bytes, Args, Functions}, []).

%% @doc Cancel an in-flight feed. Publishes `sharing.put_completed_v1' with
%% `outcome => cancelled' and withdraws the share if it had not resolved yet.
-spec cancel(pid()) -> ok.
cancel(Pid) -> gen_server:stop(Pid).

arity_3(Fun) when is_function(Fun, 3) -> Fun.

arity_4(Fun) when is_function(Fun, 4) -> Fun.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Module, Pool, Realm, Bytes, InitArgs, Functions}) ->
    process_flag(trap_exit, true),
    started(Module:init(InitArgs), Module, Pool, Realm, Bytes, Functions).

started({ok, User}, Module, Pool, Realm, Bytes,
        #{share := Share, unshare := Unshare, share_opts := ShareOpts, fact_publish := Publish}) ->
    ShareId = crypto:strong_rand_bytes(16),
    _ = Publish(Pool, Realm, ?PUT_STARTED, #{share_id => ShareId, size => byte_size(Bytes)}),
    %% The root the share will name, known before anything is sent, so a cancel can withdraw it.
    {MCID, _} = macula_content_store:added(Bytes, ShareOpts, macula_content_store:new()),
    Parent = self(),
    Worker = spawn_link(fun() -> Parent ! {feed_result, Share(Pool, Realm, Bytes, ShareOpts)} end),
    {ok, #fstate{module = Module, pool = Pool, realm = Realm, mcid = MCID, unshare = Unshare,
                 fact_publish = Publish, share_id = ShareId, worker = Worker, completed = false, user = User}};
started({stop, Reason}, _Module, _Pool, _Realm, _Bytes, _Functions) ->
    {stop, Reason}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({feed_result, Result}, #fstate{module = Module, user = User} = State) ->
    deliver(Module:handle_fed(Result, User), completed(Result, State));
handle_info({'EXIT', Worker, Reason}, #fstate{worker = Worker} = State) when Reason =/= normal ->
    {stop, {worker_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

deliver({noreply, User}, State) -> {noreply, State#fstate{user = User}};
deliver({stop, Reason, User}, State) -> {stop, Reason, State#fstate{user = User}}.

%% @private
terminate(_Reason, #fstate{completed = true, worker = Worker}) ->
    unlink(Worker),
    exit(Worker, kill),
    ok;
terminate(_Reason, #fstate{worker = Worker, pool = Pool, realm = Realm, mcid = MCID, unshare = Unshare} = State) ->
    unlink(Worker),
    exit(Worker, kill),
    _ = Unshare(Pool, Realm, MCID),
    _ = completed({error, cancelled}, State),
    ok.

completed(_Result, #fstate{completed = true} = State) ->
    State;
completed(Result, #fstate{pool = Pool, realm = Realm, fact_publish = Publish, share_id = ShareId} = State) ->
    _ = Publish(Pool, Realm, ?PUT_COMPLETED, outcome(#{share_id => ShareId}, Result)),
    State#fstate{completed = true}.

outcome(Base, {ok, MCID}) -> Base#{outcome => completed, mcid => MCID, chunked => is_chunked(MCID)};
outcome(Base, {error, cancelled}) -> Base#{outcome => cancelled};
outcome(Base, {error, Reason}) -> Base#{outcome => failed, reason => Reason}.

is_chunked(<<2, 16#56, _/binary>>) -> true;
is_chunked(_) -> false.
