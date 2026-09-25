%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised content downloads (the get/fetch side).
%%%
%%% `start_link/4,5,6' returns immediately with a pid, fetches the content
%%% from a node that shares it (`macula:get_content/4', D27), delivers the
%%% outcome to `Module:handle_downloaded/2', and publishes
%%% `sharing.get_started_v1' / `sharing.get_completed_v1' mesh facts around
%%% the fetch, including `outcome => cancelled' when `cancel/1' lands before
%%% the fetch resolves.
%%%
%%% This is content sharing, not general-purpose RPC streaming; see
%%% `macula_streamer' / `macula_stream_sink' for that.
%%%
%%% == Start options ==
%%%
%%% `fetch', the function the content is fetched with, as
%%% `macula:get_content/4' (the default); `fetch_opts', passed to it
%%% (`max_bytes', `max_chunks', ...); `fact_publish', the function the facts
%%% are published with, `macula:publish/4' by default. A test gives its own
%%% functions this way instead of replacing a module. A function of another
%%% shape is refused with `function_clause', in the caller.
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
%%%     MCID, self()).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_download).

-behaviour(gen_server).

-export([start_link/4, start_link/5, start_link/6]).
-export([cancel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_downloaded(Result :: {ok, binary()} | {error, term()}, State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-define(GET_STARTED, <<"sharing.get_started_v1">>).
-define(GET_COMPLETED, <<"sharing.get_completed_v1">>).

-export_type([start_opts/0]).

-type start_opts() :: #{fetch => fun((macula:pool(), macula:realm(), macula:mcid(), map()) ->
                                          {ok, binary()} | {error, term()}),
                        fetch_opts => map(),
                        fact_publish => macula_lifetime_announcer:publish()}.

-record(dstate, {
    module       :: module(),
    pool         :: macula:pool(),
    realm        :: macula:realm(),
    fact_publish :: macula_lifetime_announcer:publish(),
    share_id     :: binary(),
    worker       :: pid(),
    completed    :: boolean(),
    user         :: term()
}).

%% @doc Start a download of `MCID' in `Realm' through `Pool'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:mcid()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, MCID) ->
    start_link(Module, Pool, Realm, MCID, undefined).

%% @doc As `start_link/4', with `Args' passed to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:mcid(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, MCID, Args) ->
    start_link(Module, Pool, Realm, MCID, Args, #{}).

%% @doc As `start_link/5', with start options (see "Start options" above).
-spec start_link(module(), macula:pool(), macula:realm(), macula:mcid(), term(), start_opts()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, MCID, Args, Opts) when is_map(Opts) ->
    Functions = #{fetch => arity_4(maps:get(fetch, Opts, fun macula:get_content/4)),
                  fetch_opts => maps:get(fetch_opts, Opts, #{}),
                  fact_publish => arity_4(maps:get(fact_publish, Opts, fun macula:publish/4))},
    gen_server:start_link(?MODULE, {Module, Pool, Realm, MCID, Args, Functions}, []).

%% @doc Cancel an in-flight download. Publishes `sharing.get_completed_v1'
%% with `outcome => cancelled' if the fetch had not resolved yet.
-spec cancel(pid()) -> ok.
cancel(Pid) -> gen_server:stop(Pid).

arity_4(Fun) when is_function(Fun, 4) -> Fun.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Module, Pool, Realm, MCID, InitArgs, #{fetch := Fetch, fetch_opts := FetchOpts, fact_publish := Publish}}) ->
    process_flag(trap_exit, true),
    started(Module:init(InitArgs), Module, Pool, Realm, MCID, Fetch, FetchOpts, Publish).

started({ok, User}, Module, Pool, Realm, MCID, Fetch, FetchOpts, Publish) ->
    ShareId = crypto:strong_rand_bytes(16),
    _ = Publish(Pool, Realm, ?GET_STARTED, #{share_id => ShareId, mcid => MCID, chunked => is_chunked(MCID)}),
    Parent = self(),
    Worker = spawn_link(fun() -> Parent ! {download_result, Fetch(Pool, Realm, MCID, FetchOpts)} end),
    {ok, #dstate{module = Module, pool = Pool, realm = Realm, fact_publish = Publish, share_id = ShareId,
                 worker = Worker, completed = false, user = User}};
started({stop, Reason}, _Module, _Pool, _Realm, _MCID, _Fetch, _FetchOpts, _Publish) ->
    {stop, Reason}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({download_result, Result}, #dstate{module = Module, user = User} = State) ->
    deliver(Module:handle_downloaded(Result, User), completed(Result, State));
handle_info({'EXIT', Worker, Reason}, #dstate{worker = Worker} = State) when Reason =/= normal ->
    {stop, {worker_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

deliver({noreply, User}, State) -> {noreply, State#dstate{user = User}};
deliver({stop, Reason, User}, State) -> {stop, Reason, State#dstate{user = User}}.

%% @private
%% A fetch that has not resolved is cancelled: its worker is stopped, and with it the streams it opened.
terminate(_Reason, #dstate{worker = Worker} = State) ->
    unlink(Worker),
    exit(Worker, kill),
    _ = completed({error, cancelled}, State),
    ok.

completed(_Result, #dstate{completed = true} = State) ->
    State;
completed(Result, #dstate{pool = Pool, realm = Realm, fact_publish = Publish, share_id = ShareId} = State) ->
    _ = Publish(Pool, Realm, ?GET_COMPLETED, outcome(#{share_id => ShareId}, Result)),
    State#dstate{completed = true}.

outcome(Base, {ok, Bytes}) -> Base#{outcome => completed, size => byte_size(Bytes)};
outcome(Base, {error, cancelled}) -> Base#{outcome => cancelled};
outcome(Base, {error, Reason}) -> Base#{outcome => failed, reason => Reason}.

is_chunked(<<2, 16#56, _/binary>>) -> true;
is_chunked(_) -> false.
