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
%%% == Direct-dial ==
%%%
%%% `advertise/5,6' registers the handler with the pool's advertise-
%%% gossip mechanism only — nothing published lets a caller on another
%%% station find this procedure without a route having propagated
%%% between the two stations first. `advertise_direct/6' does that
%%% AND publishes a signed `procedure_advertisement' DHT record naming
%%% this pool's currently-connected station as the server, so a caller
%%% using `macula_request:start_link_direct/6,7' can resolve and dial
%%% here directly, in one hop, regardless of whether the two stations
%%% have a routing edge between them.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_response).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([advertise/5, advertise/6, advertise_direct/6, advertise_direct/7,
        unadvertise/3]).
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
%% factory supervisor for per-request response children and registers
%% a dispatch handler with `macula:advertise/5'. Returns the
%% supervisor pid so the caller can supervise it (or ignore it).
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args) ->
    advertise(Pool, Realm, Procedure, Module, Args, #{}).

%% @doc As `advertise/5'. `Opts' may include `announce' (default
%% `true'), `auth' (forwarded to `macula:advertise/5'), and
%% `reuse_sup' — an existing supervisor pid (as returned by a prior
%% `advertise/5,6' call) to re-send the wire `ADVERTISE' frame on
%% without starting a new factory supervisor. Use this for periodic
%% re-advertise (a station's registration for a procedure is tied to
%% the connection that sent it, and does not survive that connection
%% being replaced — see `advertise_direct/6,7''s own doc) — calling
%% plain `advertise/5,6' on a timer would leak one orphaned
%% supervisor per tick, since each call otherwise starts a fresh one.
-spec advertise(macula:pool(), macula:realm(), macula:procedure(),
                module(), term(), map()) -> {ok, pid()} | {error, term()}.
advertise(Pool, Realm, Procedure, Module, Args, Opts) ->
    Sup = existing_or_new_sup(maps:get(reuse_sup, Opts, undefined)),
    Announce = maps:get(announce, Opts, true),
    Handler = fun(Payload) ->
        dispatch(Sup, Module, Pool, Realm, Announce, Args, Payload)
    end,
    case macula:advertise(Pool, Realm, Procedure, Handler, Opts) of
        ok -> {ok, Sup};
        {error, Reason} -> {error, Reason}
    end.

existing_or_new_sup(Pid) when is_pid(Pid) ->
    Pid;
existing_or_new_sup(undefined) ->
    {ok, Sup} = macula_response_sup:start_link(),
    Sup.

%% @doc As `advertise/5', and additionally publishes a signed
%% `procedure_advertisement' DHT record naming this pool's connected
%% station as the server, so `macula_request:start_link_direct/6,7'
%% can resolve and dial here directly. `Identity' signs the
%% advertisement — reuse the same one across re-advertises so each one
%% doesn't mint a fresh advertiser identity.
%%
%% The DHT publish is best-effort: if it fails (e.g. no healthy link
%% at that instant), the handler is still advertised and reachable via
%% the ordinary pooled path — direct-dial callers just won't be able
%% to resolve it until a later publish succeeds. "Best-effort" still
%% means the failure is logged, not silently discarded — a caller
%% that only ever calls this once (never retries) has no other way to
%% learn its handler is pooled-only, and "a later publish succeeds"
%% cannot happen if nothing ever tries again.
-spec advertise_direct(macula:pool(), macula:realm(), macula:procedure(),
                       module(), term(), macula_identity:key_pair()) ->
    {ok, pid()} | {error, term()}.
advertise_direct(Pool, Realm, Procedure, Module, Args, Identity) ->
    advertise_direct(Pool, Realm, Procedure, Module, Args, Identity, #{}).

%% @doc As `advertise_direct/6', with `Opts' forwarded BOTH to
%% `advertise/6' (so `announce'/`auth'/`reuse_sup' apply here too) and
%% to `macula_direct_dial:publish_advertisement/5' — e.g. `cert_chain =>
%% ChainPem' (leaf ++ org CA, PEM), so a verifying consumer's
%% `verify_cert_chain' opt can check this advertiser's org/realm
%% authorization (Slice 7c Direction B, managed realms only. See
%% `macula_direct_dial''s module doc, "Trust model") — each side reads
%% only the keys it recognizes, so one `Opts' map serves both.
%% `reuse_sup' matters here specifically: a station's wire-level
%% registration for a procedure is tied to whichever connection sent
%% the `ADVERTISE' frame, and does not survive that connection being
%% replaced (reconnect, station-side eviction, etc.) — a periodic
%% re-advertise with `reuse_sup => Sup' (the pid this function
%% returned the first time) re-sends both the wire frame and the DHT
%% record without leaking a new supervisor per tick.
-spec advertise_direct(macula:pool(), macula:realm(), macula:procedure(),
                       module(), term(), macula_identity:key_pair(), map()) ->
    {ok, pid()} | {error, term()}.
advertise_direct(Pool, Realm, Procedure, Module, Args, Identity, Opts) ->
    case advertise(Pool, Realm, Procedure, Module, Args, Opts) of
        {ok, Sup} ->
            log_publish_result(
              macula_direct_dial:publish_advertisement(
                Pool, Realm, Procedure, Identity, Opts),
              Procedure),
            {ok, Sup};
        {error, _} = Error ->
            Error
    end.

log_publish_result(ok, _Procedure) ->
    ok;
log_publish_result({error, Reason}, Procedure) ->
    ?LOG_WARNING("[macula_response] direct-dial advertisement publish "
                 "failed for ~s: ~p -- handler stays reachable via the "
                 "pooled path only until a later publish succeeds",
                 [Procedure, Reason]).

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
