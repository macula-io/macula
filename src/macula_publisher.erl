%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised content publishers.
%%%
%%% `macula:publish/4' is a plain blocking call — no addressable pid to
%%% cancel it from outside. This is the missing supervised counterpart
%%% to `macula_subscriber': every other primitive pair already has one
%%% on each side (`macula_request'/`macula_response' for RPC,
%%% `macula_streamer'/`macula_stream_sink' for streaming RPC,
%%% `macula_feeder'/`macula_download' for content sharing) — pubsub had
%%% only the consumer half. `start_link/5,6' returns immediately with a
%%% pid, runs `macula:publish/4' in a linked worker, delivers the
%%% outcome to `Module:handle_published/2', and publishes
%%% `pubsub.publish_started_v1' / `pubsub.publish_completed_v1' mesh
%%% facts around the publish — including `outcome => cancelled' if the
%%% publisher is stopped before the publish resolves.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(status_publisher).
%%% -behaviour(macula_publisher).
%%% -export([init/1, handle_published/2]).
%%%
%%% init(Parent) -> {ok, Parent}.
%%%
%%% handle_published(Result, Parent) ->
%%%     Parent ! {published, Result},
%%%     {stop, normal, Parent}.
%%% '''
%%%
%%% ```
%%% {ok, Pid} = macula_publisher:start_link(status_publisher, Pool, Realm,
%%%     Topic, Payload, self()).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_publisher).

-behaviour(gen_server).

-export([start_link/5, start_link/6]).
-export([cancel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_published(Result :: ok | {error, term()}, State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-define(PUBLISH_STARTED, <<"pubsub.publish_started_v1">>).
-define(PUBLISH_COMPLETED, <<"pubsub.publish_completed_v1">>).

-record(pstate, {
    module     :: module(),
    pool       :: macula:pool(),
    realm      :: macula:realm(),
    announce   :: boolean(),
    publish_id :: binary(),
    worker     :: pid(),
    completed  :: boolean(),
    user       :: term()
}).

%% @doc Start a publisher. Publishes `Payload' on `Topic' via `Pool'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:topic(),
                 term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Topic, Payload) ->
    start_link(Module, Pool, Realm, Topic, Payload, undefined).

%% @doc As `start_link/5', with `Args' passed to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:topic(),
                 term(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Topic, Payload, Args) ->
    gen_server:start_link(?MODULE,
                          {Module, Pool, Realm, Topic, Payload, true, Args}, []).

%% @doc Cancel an in-flight publish. Publishes
%% `pubsub.publish_completed_v1' with `outcome => cancelled' if the
%% publish had not resolved yet.
-spec cancel(pid()) -> ok.
cancel(Pid) -> gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Module, Pool, Realm, Topic, Payload, Announce, InitArgs}) ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            PublishId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?PUBLISH_STARTED,
                    #{publish_id => PublishId, topic => Topic}),
            Worker = spawn_worker(Pool, Realm, Topic, Payload),
            {ok, #pstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, publish_id = PublishId,
                        worker = Worker, completed = false, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

spawn_worker(Pool, Realm, Topic, Payload) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = macula:publish(Pool, Realm, Topic, Payload),
        Parent ! {publish_result, Result}
    end).

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({publish_result, Result}, State) ->
    NewState = announce_completed(State, Result),
    #pstate{module = Module, user = User} = NewState,
    deliver(Module:handle_published(Result, User), NewState);
handle_info({'EXIT', Worker, Reason}, #pstate{worker = Worker} = State)
        when Reason =/= normal ->
    {stop, {worker_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

deliver({noreply, NewUser}, State) -> {noreply, State#pstate{user = NewUser}};
deliver({stop, Reason, NewUser}, State) -> {stop, Reason, State#pstate{user = NewUser}}.

%% @private
terminate(_Reason, #pstate{worker = Worker, completed = true}) ->
    unlink(Worker),
    exit(Worker, kill),
    ok;
terminate(_Reason, State) ->
    unlink(State#pstate.worker),
    exit(State#pstate.worker, kill),
    _ = announce_completed(State, {error, cancelled}),
    ok.

announce_completed(#pstate{completed = true} = State, _Result) ->
    State;
announce_completed(#pstate{pool = Pool, realm = Realm, announce = Announce,
                           publish_id = PublishId} = State, Result) ->
    publish(Announce, Pool, Realm, ?PUBLISH_COMPLETED,
            outcome_fields(#{publish_id => PublishId}, Result)),
    State#pstate{completed = true}.

outcome_fields(Base, ok) ->
    Base#{outcome => completed};
outcome_fields(Base, {error, cancelled}) ->
    Base#{outcome => cancelled};
outcome_fields(Base, {error, Reason}) ->
    Base#{outcome => failed, reason => Reason}.

publish(false, _, _, _, _) -> ok;
publish(true, Pool, Realm, Topic, Payload) ->
    _ = macula:publish(Pool, Realm, Topic, Payload), ok.
