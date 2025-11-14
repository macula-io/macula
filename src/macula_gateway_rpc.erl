%%%-------------------------------------------------------------------
%%% @doc
%%% RPC Handler GenServer - manages RPC handler registration and call routing.
%%%
%%% Responsibilities:
%%% - Register/unregister RPC handlers for procedures
%%% - Route RPC calls to registered handlers
%%% - Handle call/response matching
%%% - Monitor handler processes for automatic cleanup
%%%
%%% Extracted from macula_gateway.erl (Phase 4)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_rpc).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    stop/1,
    register_handler/3,
    unregister_handler/2,
    call/4,
    get_handler/2,
    list_handlers/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    opts :: map(),
    registrations :: #{binary() => pid()},      % procedure => handler_pid
    monitors :: #{reference() => binary()}      % monitor_ref => procedure
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the RPC handler with options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the RPC handler.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Register a handler for an RPC procedure.
-spec register_handler(pid(), binary(), pid()) -> ok.
register_handler(Pid, Procedure, Handler) ->
    gen_server:call(Pid, {register_handler, Procedure, Handler}).

%% @doc Unregister a handler for an RPC procedure.
-spec unregister_handler(pid(), binary()) -> ok.
unregister_handler(Pid, Procedure) ->
    gen_server:call(Pid, {unregister_handler, Procedure}).

%% @doc Make an RPC call to a procedure.
-spec call(pid(), binary(), map(), map()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Opts) ->
    gen_server:call(Pid, {call, Procedure, Args, Opts}, get_timeout(Opts)).

%% @doc Get the handler for a procedure.
-spec get_handler(pid(), binary()) -> {ok, pid()} | not_found.
get_handler(Pid, Procedure) ->
    gen_server:call(Pid, {get_handler, Procedure}).

%% @doc List all registered handlers.
-spec list_handlers(pid()) -> {ok, [{binary(), pid()}]}.
list_handlers(Pid) ->
    gen_server:call(Pid, list_handlers).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    State = #state{
        opts = Opts,
        registrations = #{},
        monitors = #{}
    },
    {ok, State}.

handle_call({register_handler, Procedure, Handler}, _From, State)
    when is_binary(Procedure), is_pid(Handler) ->
    %% Check if procedure already has a handler
    NewMonitors = case maps:get(Procedure, State#state.registrations, undefined) of
        undefined ->
            State#state.monitors;
        ExistingHandler ->
            %% Demonitor old handler
            OldMonRef = find_monitor_ref(ExistingHandler, Procedure, State#state.monitors),
            case OldMonRef of
                undefined -> ok;
                Ref -> erlang:demonitor(Ref, [flush])
            end,
            case OldMonRef of
                undefined -> State#state.monitors;
                _ -> maps:remove(OldMonRef, State#state.monitors)
            end
    end,

    %% Register new handler
    MonitorRef = erlang:monitor(process, Handler),
    Registrations = maps:put(Procedure, Handler, State#state.registrations),
    Monitors = maps:put(MonitorRef, Procedure, NewMonitors),

    NewState = State#state{
        registrations = Registrations,
        monitors = Monitors
    },
    {reply, ok, NewState};

handle_call({unregister_handler, Procedure}, _From, State) when is_binary(Procedure) ->
    %% Remove registration
    NewRegistrations = maps:remove(Procedure, State#state.registrations),

    %% Find and remove monitor
    Handler = maps:get(Procedure, State#state.registrations, undefined),
    NewMonitors = case Handler of
        undefined ->
            State#state.monitors;
        _ ->
            MonRef = find_monitor_ref(Handler, Procedure, State#state.monitors),
            case MonRef of
                undefined -> State#state.monitors;
                Ref ->
                    erlang:demonitor(Ref, [flush]),
                    maps:remove(Ref, State#state.monitors)
            end
    end,

    NewState = State#state{
        registrations = NewRegistrations,
        monitors = NewMonitors
    },
    {reply, ok, NewState};

handle_call({call, Procedure, Args, _Opts}, From, State) when is_binary(Procedure) ->
    case maps:get(Procedure, State#state.registrations, undefined) of
        undefined ->
            {reply, {error, no_handler}, State};
        Handler ->
            %% Send call to handler
            Handler ! {rpc_call, Procedure, Args, From},
            %% Don't reply yet - handler will reply to From directly
            {noreply, State}
    end;

handle_call({get_handler, Procedure}, _From, State) when is_binary(Procedure) ->
    Result = case maps:get(Procedure, State#state.registrations, undefined) of
        undefined -> not_found;
        Handler -> {ok, Handler}
    end,
    {reply, Result, State};

handle_call(list_handlers, _From, State) ->
    Handlers = maps:to_list(State#state.registrations),
    {reply, {ok, Handlers}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle handler process death - automatic cleanup.
handle_info({'DOWN', MonitorRef, process, _HandlerPid, _Reason}, State) ->
    case maps:get(MonitorRef, State#state.monitors, undefined) of
        undefined ->
            %% Unknown monitor (shouldn't happen)
            {noreply, State};
        Procedure ->
            %% Remove registration and monitor
            NewRegistrations = maps:remove(Procedure, State#state.registrations),
            NewMonitors = maps:remove(MonitorRef, State#state.monitors),

            NewState = State#state{
                registrations = NewRegistrations,
                monitors = NewMonitors
            },
            {noreply, NewState}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Get timeout from options (default 5000ms).
-spec get_timeout(map()) -> non_neg_integer().
get_timeout(Opts) ->
    maps:get(timeout, Opts, 5000).

%% @doc Find monitor reference for a handler and procedure.
-spec find_monitor_ref(pid(), binary(), #{reference() => binary()}) -> reference() | undefined.
find_monitor_ref(_Handler, Procedure, Monitors) ->
    %% This is a bit inefficient but works for now
    %% We need to find the monitor ref that corresponds to this handler
    %% Since we don't store handler pids in monitors map, we need to search
    MonitorList = maps:to_list(Monitors),
    extract_monitor_ref(lists:filter(fun({_Ref, Proc}) -> Proc =:= Procedure end, MonitorList)).

%% @doc Extract monitor ref from filter result.
extract_monitor_ref([{Ref, _Proc}]) -> Ref;
extract_monitor_ref(_) -> undefined.
