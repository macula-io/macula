%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised RPC responders.
%%%
%%% `advertise/5' on the raw SDK takes a bare handler fun invoked in a
%%% transient process spawned per inbound CALL (see the internal
%%% macula_station_link advertise/5 — "Handlers run in a transient
%%% process spawned per CALL"). This module gives that transient
%%% process a proper shape: each inbound call starts one supervised
%%% `macula_responder' child (under a `simple_one_for_one' factory
%%% this module owns), threading state through `Module:init/1' and
%%% `Module:handle_request/2', and publishing `rpc.received_v1' /
%%% `rpc.replied_v1' mesh facts around the request. This is the
%%% provider-side counterpart to `macula_requester'.
%%%
%%% A crashing `Module:handle_request/2' kills the responder child;
%%% that composes with the SDK's own crash mapping unchanged, since
%%% `gen_server:call/3' against a dead callee raises the same way a
%%% crashing bare handler fun already does.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(math_service).
%%% -behaviour(macula_responder).
%%% -export([init/1, handle_request/2]).
%%%
%%% init(_Args) -> {ok, []}.
%%%
%%% handle_request(#{a := A, b := B}, State) ->
%%%     {reply, #{result => A + B}, State}.
%%% '''
%%%
%%% ```
%%% {ok, _Sup} = macula_responder:advertise(Pool, Realm,
%%%     <<"math.add_v1">>, math_service, []).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_responder).

-behaviour(gen_server).

-export([advertise/5, advertise/6, unadvertise/3]).
-export([start_link/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_request(Payload :: term(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {error, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> any().

-optional_callbacks([terminate/2]).

-define(CALL_TIMEOUT, 30_000).
-define(REQUEST_RECEIVED, <<"rpc.received_v1">>).
-define(REQUEST_REPLIED, <<"rpc.replied_v1">>).

-record(rstate, {
    module     :: module(),
    pool       :: macula:pool(),
    realm      :: macula:realm(),
    announce   :: boolean(),
    request_id :: binary(),
    payload    :: term(),
    user       :: term()
}).

%% @doc Advertise `Procedure' on `Pool'/`Realm'. Starts a private
%% factory supervisor for per-request responder children and registers
%% a dispatch handler with `macula:advertise/5'. Returns the
%% supervisor pid so the caller can supervise it (or ignore it).
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args) ->
    advertise(Pool, Realm, Procedure, Module, Args, #{}).

%% @doc As `advertise/5'. `Opts' may include `announce' (default
%% `true') and `auth' (forwarded to `macula:advertise/5').
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term(), map()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args, Opts) ->
    {ok, Sup} = macula_responder_sup:start_link(),
    Announce = maps:get(announce, Opts, true),
    Handler = fun(Payload) ->
        dispatch(Sup, Module, Pool, Realm, Announce, Args, Payload)
    end,
    case macula:advertise(Pool, Realm, Procedure, Handler, Opts) of
        ok -> {ok, Sup};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Stop advertising. Does not stop the factory supervisor
%% returned by `advertise/5,6' — callers that want to tear it down
%% should `exit(Sup, shutdown)' themselves.
-spec unadvertise(macula:pool(), macula:realm(), macula:procedure()) -> ok.
unadvertise(Pool, Realm, Procedure) ->
    macula:unadvertise(Pool, Realm, Procedure).

dispatch(Sup, Module, Pool, Realm, Announce, Args, Payload) ->
    case supervisor:start_child(Sup, [Module, Pool, Realm, Announce, Args, Payload]) of
        {ok, Pid} -> gen_server:call(Pid, run, ?CALL_TIMEOUT);
        {error, Reason} -> {error, Reason}
    end.

%% @private
-spec start_link(module(), macula:pool(), macula:realm(), boolean(),
                 term(), term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Announce, InitArgs, Payload) ->
    gen_server:start_link(?MODULE,
        {Module, Pool, Realm, Announce, InitArgs, Payload}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Module, Pool, Realm, Announce, InitArgs, Payload}) ->
    case Module:init(InitArgs) of
        {ok, UserState} ->
            RequestId = crypto:strong_rand_bytes(16),
            publish(Announce, Pool, Realm, ?REQUEST_RECEIVED,
                    #{request_id => RequestId}),
            {ok, #rstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, request_id = RequestId,
                        payload = Payload, user = UserState}};
        {stop, Reason} ->
            {stop, Reason}
    end.

%% @private
handle_call(run, _From, #rstate{module = Module, payload = Payload,
                                user = User} = State) ->
    {Reply, NewUser} = outcome(Module:handle_request(Payload, User)),
    publish_replied(State, Reply),
    {stop, normal, Reply, State#rstate{user = NewUser}};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

outcome({reply, Reply, NewUser}) -> {{ok, Reply}, NewUser};
outcome({error, Reason, NewUser}) -> {{error, Reason}, NewUser}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info(_Msg, State) -> {noreply, State}.

%% @private
terminate(Reason, #rstate{module = Module, user = User}) ->
    maybe_terminate(Module, Reason, User).

maybe_terminate(Module, Reason, User) ->
    case erlang:function_exported(Module, terminate, 2) of
        true -> Module:terminate(Reason, User);
        false -> ok
    end.

publish_replied(#rstate{pool = Pool, realm = Realm, announce = Announce,
                        request_id = RequestId}, Reply) ->
    publish(Announce, Pool, Realm, ?REQUEST_REPLIED,
            outcome_fields(#{request_id => RequestId}, Reply)).

outcome_fields(Base, {ok, _}) -> Base#{outcome => replied};
outcome_fields(Base, {error, Reason}) -> Base#{outcome => failed, reason => Reason}.

publish(false, _, _, _, _) -> ok;
publish(true, Pool, Realm, Topic, Payload) ->
    _ = macula:publish(Pool, Realm, Topic, Payload), ok.
