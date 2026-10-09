%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised RPC responses.
%%%
%%% `advertise/5' on the raw SDK takes a bare handler fun invoked in a
%%% transient process spawned per inbound CALL (see the internal
%%% macula_station_link advertise/5 — "Handlers run in a transient
%%% process spawned per CALL"). This module gives that transient
%%% process a proper shape: each inbound call starts one supervised
%%% `macula_response' child (under a `simple_one_for_one' factory
%%% this module owns), threading state through `Module:init/1' and
%%% `Module:handle_request/2', and publishing `rpc.received_v1' /
%%% `rpc.replied_v1' mesh facts around the request. This is the
%%% provider-side counterpart to `macula_request'.
%%%
%%% A crashing `Module:handle_request/2' kills the response child;
%%% that composes with the SDK's own crash mapping unchanged, since
%%% `gen_server:call/3' against a dead callee raises the same way a
%%% crashing bare handler fun already does.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(math_service).
%%% -behaviour(macula_response).
%%% -export([init/1, handle_request/2]).
%%%
%%% init(_Args) -> {ok, []}.
%%%
%%% handle_request(#{a := A, b := B}, State) ->
%%%     {reply, #{result => A + B}, State}.
%%% '''
%%%
%%% ```
%%% {ok, _Sup} = macula_response:advertise(Pool, Realm,
%%%     <<"math.add_v1">>, math_service, []).
%%% '''
%%%
%%% == Advertise and publish functions ==
%%%
%%% The options of `advertise/6' and `advertise_direct/7' take
%%% `advertise', the function the handler is advertised with,
%%% `macula:advertise/5' by default; and `fact_publish', the function each
%%% response announces its facts with, `macula:publish/4' by default. The
%%% other options go on to those functions without these two. A test gives its own functions this
%%% way instead of replacing a module.
%%%
%%% == Direct-dial ==
%%%
%%% `advertise/5,6' registers the handler with the pool, and since
%%% 14.1.0 that alone makes it resolvable: each link sends its station an
%%% ADVERTISE and puts the same signed `procedure_advertisement' in the
%%% DHT, signed once by the pool and naming that link's station
%%% (macula#33). A caller using `macula_request:start_link_direct/6,7' resolves it and dials in one hop,
%%% whether or not the two stations have a routing edge between them.
%%% `advertise_direct/6,7' is the same registration, kept for the 14.x API.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_response).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([advertise/5, advertise/6, advertise_direct/6, advertise_direct/7,
        unadvertise/3]).
-export([start_link/7, start_link/8]).
-export_type([advertise/0, advertise_opts/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% The wire-authenticated caller of the request this child is answering
%% (macula#60): `handle_request/2' reads it for a payload of any shape,
%% map or not. The link merges it into map payloads too (`with_caller/2'),
%% so a handler may read either. `undefined' outside a served request.
-export([caller/0]).

%% The same provenance key `macula_station_link' sets in the process that
%% runs a handler; this module sets it in its own child's process, where
%% `handle_request/2' runs. Set by the link before dispatch for every
%% payload shape (macula#60).
-define(CALLER_CONTEXT_KEY, '$macula_handler_caller').

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_request(Payload :: term(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {error, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> any().

-optional_callbacks([terminate/2]).

%% How long a response waits for its handler: `handler_timeout_ms', 30 s by
%% default, and never past the 600 s a caller may wait for any call.
-define(DEFAULT_HANDLER_TIMEOUT_MS, 30_000).
-define(MAX_HANDLER_TIMEOUT_MS, 600_000).
-define(REQUEST_RECEIVED, <<"rpc.received_v1">>).
-define(REQUEST_REPLIED, <<"rpc.replied_v1">>).

-type advertise() :: fun((macula:pool(), macula:realm(), macula:procedure(),
                          macula_client:handler(), map()) -> ok | {error, term()}).
-type advertise_opts() :: #{advertise => advertise(),
                            fact_publish => macula_lifetime_announcer:publish(),
                            handler_timeout_ms => 1..600_000,
                            atom() => term()}.

-record(rstate, {
    module       :: module(),
    pool         :: macula:pool(),
    realm        :: macula:realm(),
    announce     :: boolean(),
    fact_publish :: macula_lifetime_announcer:publish(),
    request_id   :: binary(),
    payload      :: term(),
    user         :: term()
}).

%% @doc Advertise `Procedure' on `Pool'/`Realm'. Starts a private
%% factory supervisor for per-request response children and registers
%% a dispatch handler with `macula:advertise/5'. Returns the
%% supervisor pid so the caller can supervise it (or ignore it).
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args) ->
    advertise(Pool, Realm, Procedure, Module, Args, #{}).

%% @doc As `advertise/5'. `Opts' may include `announce' (default
%% `true'), `auth' (forwarded to `macula:advertise/5'),
%% `handler_timeout_ms' — how long to wait for the handler before the caller
%% is answered `temporary_relay_failure', an integer from 1 to 600000,
%% default 30000; anything else is refused as
%% `{error, {invalid_handler_timeout_ms, Value}}' — and
%% `reuse_sup' — an existing supervisor pid (as returned by a prior
%% `advertise/5,6' call) to register the handler again with, without
%% starting a new factory supervisor. Use this for a periodic
%% re-advertise (see `advertise_direct/6,7''s own doc) — calling
%% plain `advertise/5,6' on a timer would leak one orphaned
%% supervisor per tick, since each call otherwise starts a fresh one.
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term(), advertise_opts()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args, Opts) when is_map(Opts) ->
    advertise_within(handler_timeout(maps:get(handler_timeout_ms, Opts, ?DEFAULT_HANDLER_TIMEOUT_MS)),
                     Pool, Realm, Procedure, Module, Args, Opts).

advertise_within({ok, Timeout}, Pool, Realm, Procedure, Module, Args, Opts) ->
    Advertise = arity_5(maps:get(advertise, Opts, fun macula:advertise/5)),
    FactPublish = arity_4(maps:get(fact_publish, Opts, fun macula:publish/4)),
    Sup = existing_or_new_sup(maps:get(reuse_sup, Opts, undefined)),
    Announce = maps:get(announce, Opts, true),
    Handler = fun(Payload) ->
        dispatch(Sup, Module, Pool, Realm, Announce, FactPublish, Args, Payload, Timeout)
    end,
    case Advertise(Pool, Realm, Procedure, Handler, without_functions(Opts)) of
        ok -> {ok, Sup};
        {error, Reason} -> {error, Reason}
    end;
advertise_within({error, _} = Refused, _Pool, _Realm, _Procedure, _Module, _Args, _Opts) ->
    Refused.

handler_timeout(Ms) when is_integer(Ms), Ms >= 1, Ms =< ?MAX_HANDLER_TIMEOUT_MS -> {ok, Ms};
handler_timeout(Other) -> {error, {invalid_handler_timeout_ms, Other}}.

%% A `reuse_sup' pid from a caller's prior `advertise/6' call can have
%% died since (e.g. the caller itself crashed and, being linked to the
%% factory sup it started, took it down too — see `mcl_om_capabilities'
%% for a real periodic-republish caller that does exactly this on a
%% timed-out advertise). Reusing a dead pid unconditionally used to hand
%% `dispatch/7' a `Sup' that would `noproc' on its very first
%% `supervisor:start_child' — silently breaking every inbound call for
%% that procedure until the next re-advertise happened to land. Checking
%% liveness here is pattern matching on a plain predicate, not a
%% try/catch: a dead reuse target is exactly as valid an input as an
%% absent one, and both fall through to `new_sup/0'.
existing_or_new_sup(Pid) when is_pid(Pid) ->
    existing_or_new_sup(Pid, erlang:is_process_alive(Pid));
existing_or_new_sup(undefined) ->
    new_sup().

existing_or_new_sup(Pid, true)  -> Pid;
existing_or_new_sup(_Pid, false) -> new_sup().

new_sup() ->
    {ok, Sup} = macula_response_sup:start_link(),
    Sup.

%% @doc As `advertise/5'. Since 14.1.0 `advertise/5' itself makes the
%% provider resolvable: each link puts the advertisement it sends its station
%% in the DHT, signed once by the pool (macula#33), so this publishes nothing
%% of its own. `NodeIdentity' is kept for the 14.x signature and is not used.
-spec advertise_direct(macula:pool(), macula:realm(), macula:procedure(),
                       module(), term(), macula_node_keys:node_key()) ->
    {ok, pid()} | {error, term()}.
advertise_direct(Pool, Realm, Procedure, Module, Args, NodeIdentity) ->
    advertise_direct(Pool, Realm, Procedure, Module, Args, NodeIdentity, #{}).

%% @doc As `advertise_direct/6', with `Opts' forwarded to `advertise/6'.
%% `cert_chain', a 10.x option `authorization' replaces, is refused
%% with `{error, {removed_option, cert_chain}}' before the handler is
%% registered.
-spec advertise_direct(macula:pool(), macula:realm(), macula:procedure(),
                       module(), term(), macula_node_keys:node_key(), advertise_opts()) ->
    {ok, pid()} | {error, term()}.
advertise_direct(Pool, Realm, Procedure, Module, Args, NodeIdentity, Opts) when is_map(Opts) ->
    advertise_direct_unless_removed(macula_direct_dial:removed_option(advertise, Opts), Pool,
                                    Realm, Procedure, Module, Args, NodeIdentity, Opts).

advertise_direct_unless_removed(none, Pool, Realm, Procedure, Module, Args, _NodeIdentity, Opts) ->
    advertise(Pool, Realm, Procedure, Module, Args, Opts);
advertise_direct_unless_removed(Removed, _Pool, _Realm, _Procedure, _Module, _Args, _NodeIdentity,
                                _Opts) ->
    {error, Removed}.

%% @doc Stop advertising. Does not stop the factory supervisor
%% returned by `advertise/5,6' — callers that want to tear it down
%% should `exit(Sup, shutdown)' themselves.
-spec unadvertise(macula:pool(), macula:realm(), macula:procedure()) -> ok.
unadvertise(Pool, Realm, Procedure) ->
    macula:unadvertise(Pool, Realm, Procedure).

dispatch(Sup, Module, Pool, Realm, Announce, FactPublish, Args, Payload, Timeout) ->
    %% The verified caller of the request this handler fun serves lives in
    %% this process's context (macula_station_link:caller/0); carry it into
    %% the response child, which runs `handle_request/2' in its own process
    %% where the handler fun's context is not visible (macula#60).
    Child = [Module, Pool, Realm, Announce, FactPublish, Args, Payload,
             macula_station_link:caller()],
    case supervisor:start_child(Sup, Child) of
        {ok, Pid} -> run(Pid, Timeout);
        {error, Reason} -> {error, Reason}
    end.

%% The handler lives no longer than the wait on it (macula#64 F5). A watcher stops it when the waiting process ends
%% first (a station link stops a CALL's worker when admission releases the request, F6), and a wait that times out
%% stops it before the timeout reaches the caller. A watcher rather than a link, so a handler that crashes still
%% reaches its caller as the call's exit, which the caller can catch, and never as an exit signal.
run(Pid, Timeout) ->
    Caller = self(),
    _ = spawn(fun() -> watch(Caller, Pid) end),
    try gen_server:call(Pid, run, Timeout)
    catch exit:{timeout, _} = Reason:Stack ->
        exit(Pid, kill),
        erlang:raise(exit, Reason, Stack)
    end.

%% Stops the handler if its caller ends first, and ends with the handler otherwise.
watch(Caller, Handler) ->
    CallerMon = erlang:monitor(process, Caller),
    HandlerMon = erlang:monitor(process, Handler),
    receive
        {'DOWN', CallerMon, process, Caller, _} -> exit(Handler, kill);
        {'DOWN', HandlerMon, process, Handler, _} -> true
    end.

%% @private
-spec start_link(module(), macula:pool(), macula:realm(), boolean(),
                 macula_lifetime_announcer:publish(), term(), term()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Announce, FactPublish, InitArgs, Payload) ->
    start_link(Module, Pool, Realm, Announce, FactPublish, InitArgs, Payload, undefined).

%% @private
-spec start_link(module(), macula:pool(), macula:realm(), boolean(),
                 macula_lifetime_announcer:publish(), term(), term(), term()) ->
    {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Announce, FactPublish, InitArgs, Payload, Caller) ->
    gen_server:start_link(?MODULE,
        {Module, Pool, Realm, Announce, FactPublish, InitArgs, Payload, Caller}, []).

%% @doc The verified caller of the request this process is answering, or
%% `undefined' when it answers none. Set by the link for every payload
%% shape (macula#60), so `handle_request/2' can attribute a non-map
%% payload like a map one; a map payload also keeps the merged `caller'
%% key (`with_caller/2').
caller() ->
    erlang:get(?CALLER_CONTEXT_KEY).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Module, Pool, Realm, Announce, FactPublish, InitArgs, Payload, Caller}) ->
    case Module:init(InitArgs) of
        {ok, UserState} ->
            RequestId = crypto:strong_rand_bytes(16),
            publish(FactPublish, Announce, Pool, Realm, ?REQUEST_RECEIVED,
                    #{request_id => RequestId}),
            %% run in the child's process, where `caller/0' reads it during
            %% `handle_request/2' (macula#60).
            erlang:put(?CALLER_CONTEXT_KEY, Caller),
            {ok, #rstate{module = Module, pool = Pool, realm = Realm,
                        announce = Announce, fact_publish = FactPublish,
                        request_id = RequestId,
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
                        fact_publish = FactPublish, request_id = RequestId}, Reply) ->
    publish(FactPublish, Announce, Pool, Realm, ?REQUEST_REPLIED,
            outcome_fields(#{request_id => RequestId}, Reply)).

outcome_fields(Base, {ok, _}) -> Base#{outcome => replied};
outcome_fields(Base, {error, Reason}) -> Base#{outcome => failed, reason => Reason}.

publish(_FactPublish, false, _, _, _, _) -> ok;
publish(FactPublish, true, Pool, Realm, Topic, Payload) ->
    _ = FactPublish(Pool, Realm, Topic, Payload), ok.

%% A function option of the wrong arity is refused with function_clause,
%% in the caller.
arity_4(Fun) when is_function(Fun, 4) -> Fun.

arity_5(Fun) when is_function(Fun, 5) -> Fun.

%% The options that are this node's own business: the functions, and the
%% handler timeout, which bounds this node's wait on its handler.
without_functions(Opts) ->
    maps:without([advertise, fact_publish, handler_timeout_ms], Opts).
