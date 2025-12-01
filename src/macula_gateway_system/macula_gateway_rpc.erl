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

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    stop/1,
    register_handler/3,
    unregister_handler/2,
    call/4,
    invoke_handler/3,
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
%% Handler can be either a PID or a function.
-spec register_handler(pid(), binary(), pid() | fun()) -> ok.
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

%% @doc Invoke a handler directly for local calls.
%% If handler is a function, invokes it directly.
%% If handler is a PID, sends rpc_call message and waits for response.
-spec invoke_handler(pid(), binary(), map()) -> {ok, term()} | {error, term()}.
invoke_handler(Pid, Procedure, Args) ->
    gen_server:call(Pid, {invoke_handler, Procedure, Args}, 5000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    ?LOG_INFO("Initializing RPC handler"),
    State = #state{
        opts = Opts,
        registrations = #{},
        monitors = #{}
    },
    ?LOG_INFO("RPC handler initialized"),
    {ok, State}.

handle_call({register_handler, Procedure, Handler}, _From, State)
    when is_binary(Procedure), (is_pid(Handler) orelse is_function(Handler)) ->
    ExistingHandler = maps:get(Procedure, State#state.registrations, undefined),
    NewMonitors = cleanup_existing_handler(ExistingHandler, Procedure, State#state.monitors),
    {Registrations, Monitors} = register_new_handler(Handler, Procedure, State#state.registrations, NewMonitors),
    NewState = State#state{registrations = Registrations, monitors = Monitors},
    {reply, ok, NewState};

handle_call({unregister_handler, Procedure}, _From, State) when is_binary(Procedure) ->
    Handler = maps:get(Procedure, State#state.registrations, undefined),
    NewMonitors = cleanup_existing_handler(Handler, Procedure, State#state.monitors),
    NewRegistrations = maps:remove(Procedure, State#state.registrations),
    NewState = State#state{registrations = NewRegistrations, monitors = NewMonitors},
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

handle_call({invoke_handler, Procedure, Args}, _From, State) when is_binary(Procedure) ->
    case maps:get(Procedure, State#state.registrations, undefined) of
        undefined ->
            {reply, {error, no_handler}, State};
        Handler when is_function(Handler) ->
            %% Direct function call
            try
                Result = Handler(Args),
                {reply, {ok, Result}, State}
            catch
                _:Reason ->
                    {reply, {error, Reason}, State}
            end;
        Handler when is_pid(Handler) ->
            %% Synchronous call to handler process
            try
                Result = gen_server:call(Handler, {invoke, Procedure, Args}, 5000),
                {reply, Result, State}
            catch
                exit:{timeout, _} ->
                    {reply, {error, timeout}, State};
                _:Reason ->
                    {reply, {error, Reason}, State}
            end
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
    NewState = handle_monitor_down(maps:get(MonitorRef, State#state.monitors, undefined), MonitorRef, State),
    {noreply, NewState};

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
%% First argument is ignored (kept for API compatibility).
-spec find_monitor_ref(term(), binary(), #{reference() => binary()}) -> reference() | undefined.
find_monitor_ref(_Handler, Procedure, Monitors) ->
    %% This is a bit inefficient but works for now
    %% We need to find the monitor ref that corresponds to this handler
    %% Since we don't store handler pids in monitors map, we need to search
    MonitorList = maps:to_list(Monitors),
    extract_monitor_ref(lists:filter(fun({_Ref, Proc}) -> Proc =:= Procedure end, MonitorList)).

%% @doc Extract monitor ref from filter result.
extract_monitor_ref([{Ref, _Proc}]) -> Ref;
extract_monitor_ref(_) -> undefined.

%%%===================================================================
%%% Handler registration helpers
%%%===================================================================

%% @private No existing handler to clean up
cleanup_existing_handler(undefined, _Procedure, Monitors) ->
    Monitors;
%% @private Clean up existing handler's monitor
cleanup_existing_handler(_ExistingHandler, Procedure, Monitors) ->
    OldMonRef = find_monitor_ref(undefined, Procedure, Monitors),
    demonitor_and_remove(OldMonRef, Monitors).

demonitor_and_remove(undefined, Monitors) ->
    Monitors;
demonitor_and_remove(Ref, Monitors) ->
    erlang:demonitor(Ref, [flush]),
    maps:remove(Ref, Monitors).

%% @private Register PID handler with monitoring
register_new_handler(Handler, Procedure, Registrations, Monitors) when is_pid(Handler) ->
    MonitorRef = erlang:monitor(process, Handler),
    {maps:put(Procedure, Handler, Registrations),
     maps:put(MonitorRef, Procedure, Monitors)};
%% @private Register function handler (no monitoring)
register_new_handler(Handler, Procedure, Registrations, Monitors) ->
    {maps:put(Procedure, Handler, Registrations), Monitors}.

%%%===================================================================
%%% Monitor down helpers
%%%===================================================================

%% @private Unknown monitor - ignore
handle_monitor_down(undefined, _MonitorRef, State) ->
    State;
%% @private Known monitor - clean up registration
handle_monitor_down(Procedure, MonitorRef, State) ->
    State#state{
        registrations = maps:remove(Procedure, State#state.registrations),
        monitors = maps:remove(MonitorRef, State#state.monitors)
    }.
