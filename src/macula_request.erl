%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised RPC requests.
%%%
%%% `call/5' is a plain blocking call in the caller's own process —
%%% there is no addressable pid to cancel it from outside. This is the
%%% consumer-side counterpart to `macula_response': `start_link/6,7'
%%% returns immediately with a pid, runs `macula:call/5' in a linked
%%% worker, delivers the outcome to `Module:handle_reply/2', and
%%% publishes `rpc.sent_v1' / `rpc.completed_v1' mesh facts around the
%%% request — including `outcome => cancelled' if the request is
%%% cancelled before a reply arrives.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(add_caller).
%%% -behaviour(macula_request).
%%% -export([init/1, handle_reply/2]).
%%%
%%% init(Parent) -> {ok, Parent}.
%%%
%%% handle_reply(Result, Parent) ->
%%%     Parent ! {add_result, Result},
%%%     {stop, normal, Parent}.
%%% '''
%%%
%%% ```
%%% {ok, Pid} = macula_request:start_link(add_caller, Pool, Realm,
%%%     <<"math.add_v1">>, #{a => 2, b => 3}, 30_000, self()).
%%% '''
%%%
%%% == Direct-dial ==
%%%
%%% `start_link/6,7' routes through the pool's existing links — first
%%% success across whichever are healthy, the same gossip-propagated
%%% routing `call/5' always used. `start_link_direct/6,7' is the
%%% direct-dial counterpart: it resolves the procedure's
%%% `procedure_advertisement' from the DHT (published by
%%% `macula_response:advertise_direct/6' on the provider side),
%%% resolves that record's `serving_station' to a dialable endpoint via
%%% the station's own `station_endpoint' record (every macula-station
%%% publishes its own automatically), and calls there in one hop via
%%% `macula:call_station/6' — instead of depending on advertise-gossip
%%% having propagated a route between arbitrary stations. Requires the
%%% provider to have advertised via `advertise_direct/6', not plain
%%% `advertise/5' — a plain advertise publishes no discoverable record.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_request).

-behaviour(gen_server).

-export([start_link/6, start_link/7]).
-export([start_link_direct/6, start_link_direct/7]).
-export([cancel/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_reply(Result :: {ok, term()} | {error, term()}, State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-optional_callbacks([]).

-define(REQUEST_SENT, <<"rpc.sent_v1">>).
-define(REQUEST_COMPLETED, <<"rpc.completed_v1">>).

-record(qstate, {
    module     :: module(),
    pool       :: macula:pool(),
    realm      :: macula:realm(),
    announce   :: boolean(),
    request_id :: binary(),
    worker     :: pid(),
    completed  :: boolean(),
    user       :: term()
}).

%% @doc Start a request. Calls `Procedure' on `(Pool, Realm)' with
%% `Payload', timing out after `TimeoutMs'; `Args' is passed to
%% `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:procedure(),
                 term(), pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Procedure, Payload, TimeoutMs) ->
    start_link(Module, Pool, Realm, Procedure, Payload, TimeoutMs, undefined).

%% @doc As `start_link/6', with `Args' passed to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:procedure(),
                 term(), pos_integer(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Procedure, Payload, TimeoutMs, Args) ->
    gen_server:start_link(?MODULE,
        {pooled, Module, Pool, Realm, Procedure, Payload, TimeoutMs, true, Args}, []).

%% @doc As `start_link/6', but resolves and dials the serving station
%% directly instead of routing through the pool's existing links. See
%% the "Direct-dial" section above.
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:procedure(), term(), pos_integer()) ->
    {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Procedure, Payload, TimeoutMs) ->
    start_link_direct(Module, Pool, Realm, Procedure, Payload, TimeoutMs, undefined).

%% @doc As `start_link_direct/6', with `Args' passed to `Module:init/1'.
-spec start_link_direct(module(), macula:pool(), macula:realm(),
                        macula:procedure(), term(), pos_integer(), term()) ->
    {ok, pid()} | {error, term()}.
start_link_direct(Module, Pool, Realm, Procedure, Payload, TimeoutMs, Args) ->
    gen_server:start_link(?MODULE,
        {direct, Module, Pool, Realm, Procedure, Payload, TimeoutMs, true, Args}, []).

%% @doc Cancel an in-flight request. Publishes `rpc.completed_v1' with
%% `outcome => cancelled' if no reply had arrived yet.
-spec cancel(pid()) -> ok.
cancel(Pid) -> gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({DialMode, Module, Pool, Realm, Procedure, Payload, TimeoutMs, Announce,
      InitArgs}) ->
    process_flag(trap_exit, true),
    case Module:init(InitArgs) of
        {ok, UserState} ->
            RequestId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?REQUEST_SENT,
                    #{request_id => RequestId}),
            Worker = spawn_worker(DialMode, Pool, Realm, Procedure, Payload,
                                  TimeoutMs),
            {ok, #qstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, request_id = RequestId,
                        worker = Worker, completed = false, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

spawn_worker(pooled, Pool, Realm, Procedure, Payload, TimeoutMs) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = macula:call(Pool, Realm, Procedure, Payload, TimeoutMs),
        Parent ! {request_result, Result}
    end);
spawn_worker(direct, Pool, Realm, Procedure, Payload, TimeoutMs) ->
    Parent = self(),
    spawn_link(fun() ->
        Result = macula_direct_dial:call(Pool, Realm, Procedure, Payload,
                                         TimeoutMs),
        Parent ! {request_result, Result}
    end).

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info({request_result, Result}, State) ->
    NewState = announce_completed(State, Result),
    #qstate{module = Module, user = User} = NewState,
    deliver(Module:handle_reply(Result, User), NewState);
handle_info({'EXIT', Worker, Reason}, #qstate{worker = Worker} = State)
        when Reason =/= normal ->
    {stop, {worker_crashed, Reason}, State};
handle_info(_Msg, State) ->
    {noreply, State}.

deliver({noreply, NewUser}, State) -> {noreply, State#qstate{user = NewUser}};
deliver({stop, Reason, NewUser}, State) -> {stop, Reason, State#qstate{user = NewUser}}.

%% @private
terminate(_Reason, #qstate{worker = Worker, completed = true}) ->
    unlink(Worker),
    exit(Worker, kill),
    ok;
terminate(_Reason, State) ->
    unlink(State#qstate.worker),
    exit(State#qstate.worker, kill),
    _ = announce_completed(State, {error, cancelled}),
    ok.

announce_completed(#qstate{completed = true} = State, _Result) ->
    State;
announce_completed(#qstate{pool = Pool, realm = Realm, announce = Announce,
                           request_id = RequestId} = State, Result) ->
    publish(Announce, Pool, Realm, ?REQUEST_COMPLETED,
            outcome_fields(#{request_id => RequestId}, Result)),
    State#qstate{completed = true}.

outcome_fields(Base, {ok, _}) -> Base#{outcome => completed};
outcome_fields(Base, {error, cancelled}) -> Base#{outcome => cancelled};
outcome_fields(Base, {error, Reason}) -> Base#{outcome => failed, reason => Reason}.

publish(false, _, _, _, _) -> ok;
publish(true, Pool, Realm, Topic, Payload) ->
    _ = macula:publish(Pool, Realm, Topic, Payload), ok.
