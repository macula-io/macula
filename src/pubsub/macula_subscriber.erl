%%%-------------------------------------------------------------------
%%% @doc Behaviour for supervised, stateful PubSub consumers.
%%%
%%% `macula:subscribe_callback/4' is the right tool for a stateless
%%% one-shot reaction (log a line, forward to another process): it
%%% spawns its own receiver process so a slow callback does not
%%% back-pressure the pool, but that process is not part of your
%%% supervision tree, has no `init/1' for setup, and gives you nowhere
%%% to thread state across events.
%%%
%%% `macula_subscriber' is the supervised alternative. Implement three
%%% callbacks; `start_link/5,6' returns an ordinary `gen_server' pid you
%%% drop straight into your own supervision tree, and every event
%%% arrives as a `Module:handle_event/4' call against state your module
%%% owns and threads itself.
%%%
%%% == Example ==
%%%
%%% ```
%%% -module(temperature_logger).
%%% -behaviour(macula_subscriber).
%%% -export([init/1, handle_event/4, terminate/2]).
%%%
%%% init(_Args) -> {ok, #{count => 0}}.
%%%
%%% handle_event(_Topic, #{value := V}, _Meta, State) ->
%%%     Count = maps:get(count, State) + 1,
%%%     io:format("reading ~p: ~p~n", [Count, V]),
%%%     {noreply, State#{count := Count}}.
%%%
%%% terminate(_Reason, _State) -> ok.
%%% '''
%%%
%%% ```
%%% {ok, Pid} = macula_subscriber:start_link(temperature_logger, Pool,
%%%     Realm, <<"sensors.temperature_v1">>, []).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_subscriber).

-behaviour(gen_server).

-export([start_link/5, start_link/6]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {stop, Reason :: term()}.

-callback handle_event(Topic :: binary(), Payload :: term(), Meta :: map(),
                        State :: term()) ->
    {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> any().

-optional_callbacks([terminate/2]).

-record(sstate, {
    module  :: module(),
    sub_ref :: term(),
    user    :: term()
}).

%% @doc Start a subscriber. Subscribes `Module' to `(Realm, Topic)' on
%% `Pool'; `Args' is passed to `Module:init/1'.
-spec start_link(module(), macula:pool(), macula:realm(), macula:topic(),
                  term()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Topic, Args) ->
    start_link(Module, Pool, Realm, Topic, Args, #{}).

%% @doc As `start_link/5', passing `Opts' through to `macula:subscribe/5'
%% (e.g. `delivery').
-spec start_link(module(), macula:pool(), macula:realm(), macula:topic(),
                  term(), map()) -> {ok, pid()} | {error, term()}.
start_link(Module, Pool, Realm, Topic, Args, Opts) ->
    gen_server:start_link(?MODULE, {Module, Pool, Realm, Topic, Args, Opts}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Module, Pool, Realm, Topic, Args, Opts}) ->
    case Module:init(Args) of
        {ok, UserState} ->
            subscribe(Module, Pool, Realm, Topic, Opts, UserState);
        {stop, Reason} ->
            {stop, Reason}
    end.

subscribe(Module, Pool, Realm, Topic, Opts, UserState) ->
    case macula:subscribe(Pool, Realm, Topic, self(), Opts) of
        {ok, SubRef} ->
            {ok, #sstate{module = Module, sub_ref = SubRef, user = UserState}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({macula_event, SubRef, Topic, Payload, Meta},
            #sstate{module = Module, sub_ref = SubRef, user = User} = State) ->
    dispatch(Module:handle_event(Topic, Payload, Meta, User), State);
handle_info({macula_event_gone, SubRef, Reason}, #sstate{sub_ref = SubRef} = State) ->
    {stop, Reason, State};
handle_info(_Msg, State) ->
    {noreply, State}.

dispatch({noreply, NewUser}, State) ->
    {noreply, State#sstate{user = NewUser}};
dispatch({stop, Reason, NewUser}, State) ->
    {stop, Reason, State#sstate{user = NewUser}}.

%% @private
terminate(Reason, #sstate{module = Module, user = User}) ->
    maybe_terminate(Module, Reason, User).

maybe_terminate(Module, Reason, User) ->
    case erlang:function_exported(Module, terminate, 2) of
        true -> Module:terminate(Reason, User);
        false -> ok
    end.
