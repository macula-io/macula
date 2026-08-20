%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised content downloads (the get/fetch side).
%%%
%%% `get_content/2' is a plain blocking call — no addressable pid to
%%% cancel it from outside. This is the consumer-side counterpart to
%%% `macula_feeder': `start_link/4,5' returns immediately with a pid,
%%% runs `macula:get_content/2' in a linked worker, delivers the
%%% outcome to `Module:handle_downloaded/2', and publishes
%%% `sharing.get_started_v1' / `sharing.get_completed_v1' mesh facts
%%% around the transfer — including `outcome => cancelled' if the
%%% download is cancelled before the get resolves.
%%%
%%% This is content sharing, not general-purpose RPC streaming — see
%%% `macula_streamer' / `macula_stream_sink' for that (`streaming.*'
%%% facts belong to that pair).
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
-export([cancel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_downloaded(Result :: {ok, binary()} | {error, term()}, State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-define(GET_STARTED, <<"sharing.get_started_v1">>).
-define(GET_COMPLETED, <<"sharing.get_completed_v1">>).

-record(dstate, {
    module    :: module(),
    pool      :: macula:pool(),
    realm     :: macula:realm(),
    announce  :: boolean(),
    share_id  :: binary(),
    worker    :: pid(),
    completed :: boolean(),
    user      :: term()
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
    gen_server:start_link(?MODULE, {Module, Pool, Realm, Mcid, true, Args}, []).

%% @doc Cancel an in-flight download. Publishes `sharing.get_completed_v1'
%% with `outcome => cancelled' if the get had not resolved yet.
-spec cancel(pid()) -> ok.
cancel(Pid) -> gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Module, Pool, Realm, Mcid, Announce, InitArgs}) ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            ShareId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?GET_STARTED,
                    #{share_id => ShareId, mcid => Mcid,
                      chunked => is_chunked_mcid(Mcid)}),
            Worker = spawn_worker(Pool, Mcid),
            {ok, #dstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, share_id = ShareId,
                        worker = Worker, completed = false, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

spawn_worker(Pool, Mcid) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = macula:get_content(Pool, Mcid),
        Parent ! {download_result, Result}
    end).

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({download_result, Result}, State) ->
    NewState = announce_completed(State, Result),
    #dstate{module = Module, user = User} = NewState,
    deliver(Module:handle_downloaded(Result, User), NewState);
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
terminate(_Reason, State) ->
    unlink(State#dstate.worker),
    exit(State#dstate.worker, kill),
    _ = announce_completed(State, {error, cancelled}),
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
