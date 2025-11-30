%%%-------------------------------------------------------------------
%%% @doc
%%% QUIC Hole Punch Executor with Cancellation and Adaptive Timing.
%%%
%%% Implements the simultaneous open (SYN-SYN) pattern for QUIC
%%% to establish direct connections through NAT devices.
%%%
%%% Features:
%%% - Proper cancellation of in-progress punch attempts
%%% - Adaptive timing based on NAT type and previous attempts
%%% - Tracks active punches via gen_server state
%%% - Supports both sync and async execution
%%%
%%% QUIC Hole Punching Approach:
%%% Unlike TCP's explicit SYN packets, QUIC uses encrypted handshakes.
%%% The hole punching strategy is:
%%%
%%% 1. Both peers start QUIC connect() at the same coordinated time
%%% 2. Initial packets "punch" holes in both NATs
%%% 3. One peer's connection will succeed (race condition)
%%% 4. The other peer retries connecting through the opened hole
%%%
%%% NAT Behavior Considerations:
%%% - EI mapping: External address is consistent - easy hole punch
%%% - HD mapping: Must target specific host - coordinate addresses
%%% - PP allocation: Same port - single target port
%%% - PC allocation: Sequential ports - try predicted range
%%% - RD allocation: Random ports - harder to predict, try range
%%%
%%% Adaptive Timing:
%%% - Symmetric NAT: Longer timeouts, more port attempts
%%% - Restricted NAT: Standard timeouts
%%% - Full Cone: Fast timeouts, single port
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_hole_punch).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    execute/3,
    execute_async/3,
    cancel/1,
    get_active_punches/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

%% Default timing parameters
-define(DEFAULT_PUNCH_TIMEOUT_MS, 2000).
-define(DEFAULT_CONNECT_TIMEOUT_MS, 1500).
-define(DEFAULT_MAX_PARALLEL, 3).

%% Adaptive timing parameters by NAT type
-define(SYMMETRIC_PUNCH_TIMEOUT_MS, 4000).
-define(SYMMETRIC_CONNECT_TIMEOUT_MS, 3000).
-define(SYMMETRIC_MAX_PARALLEL, 5).

-define(FULL_CONE_PUNCH_TIMEOUT_MS, 1000).
-define(FULL_CONE_CONNECT_TIMEOUT_MS, 800).
-define(FULL_CONE_MAX_PARALLEL, 1).

%%%===================================================================
%%% Types
%%%===================================================================

-type nat_type() :: full_cone | restricted | port_restricted | symmetric | unknown.

-type punch_opts() :: #{
    target_host := binary() | string(),
    target_ports := [inet:port_number()],
    local_port => inet:port_number(),
    session_id := binary(),
    punch_time => integer(),
    role => initiator | target,
    local_nat_type => nat_type(),
    remote_nat_type => nat_type()
}.

-type punch_result() ::
    {ok, quicer:connection_handle()} |
    {error, timeout | unreachable | all_ports_failed | cancelled}.

-record(state, {
    %% Map from Ref -> {WorkerPid, [AttemptPids], ReplyTo}
    active_punches = #{} :: #{reference() => {pid(), [pid()], pid()}}
}).

-export_type([punch_opts/0, punch_result/0, nat_type/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the hole punch executor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Execute a hole punch attempt synchronously.
%% Blocks until connection established or timeout.
-spec execute(binary(), punch_opts(), timeout()) -> punch_result().
execute(TargetNodeId, Opts, Timeout) ->
    gen_server:call(?SERVER, {execute, TargetNodeId, Opts, Timeout}, Timeout + 1000).

%% @doc Execute hole punch asynchronously.
%% Returns immediately, caller receives result via message.
-spec execute_async(binary(), punch_opts(), pid()) -> reference().
execute_async(TargetNodeId, Opts, ReplyTo) ->
    gen_server:call(?SERVER, {execute_async, TargetNodeId, Opts, ReplyTo}).

%% @doc Cancel an ongoing hole punch attempt.
-spec cancel(reference()) -> ok | {error, not_found}.
cancel(Ref) ->
    gen_server:call(?SERVER, {cancel, Ref}).

%% @doc Get list of active punch attempts (for debugging/monitoring).
-spec get_active_punches() -> [{reference(), map()}].
get_active_punches() ->
    gen_server:call(?SERVER, get_active_punches).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Hole punch executor started"),
    {ok, #state{}}.

handle_call({execute, TargetNodeId, Opts, Timeout}, From, State) ->
    Ref = make_ref(),
    {_ReplyPid, _} = From,

    %% Spawn worker to do the actual punch
    Self = self(),
    WorkerPid = spawn_link(fun() ->
        Result = do_execute(TargetNodeId, Opts, Timeout),
        Self ! {punch_complete, Ref, Result}
    end),

    NewState = State#state{
        active_punches = maps:put(Ref, {WorkerPid, [], From}, State#state.active_punches)
    },

    %% Don't reply now - reply when punch completes
    {noreply, NewState};

handle_call({execute_async, TargetNodeId, Opts, ReplyTo}, _From, State) ->
    Ref = make_ref(),

    %% Spawn worker to do the actual punch
    Self = self(),
    WorkerPid = spawn_link(fun() ->
        Timeout = get_adaptive_timeout(Opts),
        Result = do_execute(TargetNodeId, Opts, Timeout),
        Self ! {punch_complete, Ref, Result}
    end),

    NewState = State#state{
        active_punches = maps:put(Ref, {WorkerPid, [], ReplyTo}, State#state.active_punches)
    },

    {reply, Ref, NewState};

handle_call({cancel, Ref}, _From, State) ->
    case maps:get(Ref, State#state.active_punches, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        {WorkerPid, AttemptPids, ReplyTo} ->
            ?LOG_DEBUG("Cancelling hole punch ~p with ~p attempts",
                       [Ref, length(AttemptPids)]),

            %% Kill all attempt processes
            lists:foreach(fun(Pid) ->
                exit(Pid, cancelled)
            end, [WorkerPid | AttemptPids]),

            %% Notify waiting caller if sync
            notify_cancelled(ReplyTo, Ref),

            NewState = State#state{
                active_punches = maps:remove(Ref, State#state.active_punches)
            },
            {reply, ok, NewState}
    end;

handle_call(get_active_punches, _From, State) ->
    Punches = maps:fold(fun(Ref, {WorkerPid, AttemptPids, _}, Acc) ->
        [{Ref, #{
            worker => WorkerPid,
            attempts => length(AttemptPids),
            alive => is_process_alive(WorkerPid)
        }} | Acc]
    end, [], State#state.active_punches),
    {reply, Punches, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register_attempts, Ref, Pids}, State) ->
    case maps:get(Ref, State#state.active_punches, undefined) of
        undefined ->
            {noreply, State};
        {WorkerPid, _OldPids, ReplyTo} ->
            NewState = State#state{
                active_punches = maps:put(Ref, {WorkerPid, Pids, ReplyTo},
                                          State#state.active_punches)
            },
            {noreply, NewState}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({punch_complete, Ref, Result}, State) ->
    case maps:get(Ref, State#state.active_punches, undefined) of
        undefined ->
            %% Already cancelled
            {noreply, State};
        {_WorkerPid, _AttemptPids, ReplyTo} ->
            %% Notify the caller
            notify_result(ReplyTo, Ref, Result),
            NewState = State#state{
                active_punches = maps:remove(Ref, State#state.active_punches)
            },
            {noreply, NewState}
    end;

handle_info({'EXIT', Pid, Reason}, State) ->
    ?LOG_DEBUG("Hole punch process ~p exited: ~p", [Pid, Reason]),
    %% Clean up any punches associated with this pid
    NewPunches = maps:filter(fun(_Ref, {WorkerPid, _, _}) ->
        WorkerPid =/= Pid
    end, State#state.active_punches),
    {noreply, State#state{active_punches = NewPunches}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel all active punches
    maps:foreach(fun(_Ref, {WorkerPid, AttemptPids, _}) ->
        lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, [WorkerPid | AttemptPids])
    end, State#state.active_punches),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Notify caller of result (handles both sync and async)
notify_result({Pid, Tag}, _Ref, Result) when is_pid(Pid) ->
    %% Sync call - reply via gen_server
    gen_server:reply({Pid, Tag}, Result);
notify_result(Pid, Ref, Result) when is_pid(Pid) ->
    %% Async call - send message
    Pid ! {hole_punch_result, Ref, Result}.

%% @private
notify_cancelled({Pid, Tag}, _Ref) when is_pid(Pid) ->
    gen_server:reply({Pid, Tag}, {error, cancelled});
notify_cancelled(Pid, Ref) when is_pid(Pid) ->
    Pid ! {hole_punch_result, Ref, {error, cancelled}}.

%% @private
%% @doc Actual execution logic (runs in spawned process)
-spec do_execute(binary(), punch_opts(), timeout()) -> punch_result().
do_execute(TargetNodeId, Opts, Timeout) ->
    #{target_host := Host, target_ports := Ports} = Opts,

    ?LOG_DEBUG("Executing hole punch to ~s (~s) on ports ~p",
               [TargetNodeId, Host, Ports]),

    %% Wait until coordinated punch time if specified
    maybe_wait_for_punch_time(Opts),

    %% Get adaptive parameters
    {MaxParallel, ConnectTimeout} = get_adaptive_params(Opts),

    %% Try connecting to target ports in parallel
    attempt_connection(Host, Ports, Opts, Timeout, MaxParallel, ConnectTimeout).

%% @private
%% @doc Get adaptive timeout based on NAT types.
-spec get_adaptive_timeout(punch_opts()) -> timeout().
get_adaptive_timeout(Opts) ->
    LocalNat = maps:get(local_nat_type, Opts, unknown),
    RemoteNat = maps:get(remote_nat_type, Opts, unknown),

    case worst_nat_type(LocalNat, RemoteNat) of
        symmetric -> ?SYMMETRIC_PUNCH_TIMEOUT_MS;
        full_cone -> ?FULL_CONE_PUNCH_TIMEOUT_MS;
        _ -> ?DEFAULT_PUNCH_TIMEOUT_MS
    end.

%% @private
%% @doc Get adaptive parameters based on NAT types.
-spec get_adaptive_params(punch_opts()) -> {pos_integer(), timeout()}.
get_adaptive_params(Opts) ->
    LocalNat = maps:get(local_nat_type, Opts, unknown),
    RemoteNat = maps:get(remote_nat_type, Opts, unknown),

    case worst_nat_type(LocalNat, RemoteNat) of
        symmetric ->
            {?SYMMETRIC_MAX_PARALLEL, ?SYMMETRIC_CONNECT_TIMEOUT_MS};
        full_cone ->
            {?FULL_CONE_MAX_PARALLEL, ?FULL_CONE_CONNECT_TIMEOUT_MS};
        _ ->
            {?DEFAULT_MAX_PARALLEL, ?DEFAULT_CONNECT_TIMEOUT_MS}
    end.

%% @private
%% @doc Determine the worst (most restrictive) NAT type.
-spec worst_nat_type(nat_type(), nat_type()) -> nat_type().
worst_nat_type(symmetric, _) -> symmetric;
worst_nat_type(_, symmetric) -> symmetric;
worst_nat_type(port_restricted, _) -> port_restricted;
worst_nat_type(_, port_restricted) -> port_restricted;
worst_nat_type(restricted, _) -> restricted;
worst_nat_type(_, restricted) -> restricted;
worst_nat_type(full_cone, full_cone) -> full_cone;
worst_nat_type(_, _) -> unknown.

%% @private
%% @doc Wait until the coordinated punch time arrives.
-spec maybe_wait_for_punch_time(punch_opts()) -> ok.
maybe_wait_for_punch_time(#{punch_time := PunchTime}) when is_integer(PunchTime) ->
    Now = erlang:system_time(millisecond),
    WaitMs = max(0, PunchTime - Now),
    case WaitMs > 0 of
        true ->
            ?LOG_DEBUG("Waiting ~p ms for coordinated punch time", [WaitMs]),
            timer:sleep(WaitMs);
        false ->
            ok
    end;
maybe_wait_for_punch_time(_) ->
    ok.

%% @private
%% @doc Attempt connection to target on multiple ports.
-spec attempt_connection(binary() | string(), [inet:port_number()],
                         punch_opts(), timeout(), pos_integer(), timeout()) ->
    punch_result().
attempt_connection(_Host, [], _Opts, _Timeout, _MaxPar, _ConnTimeout) ->
    {error, all_ports_failed};
attempt_connection(Host, Ports, Opts, Timeout, MaxParallel, ConnectTimeout) ->
    %% Limit parallel attempts
    AttemptsCount = min(length(Ports), MaxParallel),
    PortsToTry = lists:sublist(Ports, AttemptsCount),

    ?LOG_DEBUG("Trying ~p ports in parallel: ~p (timeout: ~p, conn_timeout: ~p)",
               [AttemptsCount, PortsToTry, Timeout, ConnectTimeout]),

    %% Spawn parallel connection attempts
    Self = self(),
    Ref = make_ref(),

    Pids = [spawn_link(fun() ->
        Result = try_single_connection(Host, Port, Opts, ConnectTimeout),
        Self ! {punch_attempt, Ref, Port, Result}
    end) || Port <- PortsToTry],

    %% Wait for first success or all failures
    collect_results(Ref, length(Pids), Timeout, Pids).

%% @private
%% @doc Try a single QUIC connection to host:port.
-spec try_single_connection(binary() | string(), inet:port_number(),
                            punch_opts(), timeout()) ->
    {ok, quicer:connection_handle()} | {error, term()}.
try_single_connection(Host, Port, Opts, ConnectTimeout) ->
    ?LOG_DEBUG("Attempting QUIC connect to ~s:~p (timeout: ~p)",
               [Host, Port, ConnectTimeout]),

    %% Build QUIC connection options
    ConnOpts = build_quic_opts(Opts, ConnectTimeout),

    %% Convert host to appropriate format
    HostStr = case is_binary(Host) of
        true -> binary_to_list(Host);
        false -> Host
    end,

    %% Attempt QUIC connection
    case quicer:connect(HostStr, Port, ConnOpts, ConnectTimeout) of
        {ok, Conn} ->
            ?LOG_INFO("Hole punch SUCCESS: connected to ~s:~p", [Host, Port]),
            {ok, Conn};
        {error, Reason} = Error ->
            ?LOG_DEBUG("Hole punch to ~s:~p failed: ~p", [Host, Port, Reason]),
            Error
    end.

%% @private
%% @doc Build QUIC connection options for hole punching.
-spec build_quic_opts(punch_opts(), timeout()) -> list().
build_quic_opts(Opts, ConnectTimeout) ->
    BaseOpts = [
        {alpn, ["macula"]},
        {verify, none},
        %% Adaptive timeouts based on NAT type
        {idle_timeout_ms, ConnectTimeout * 3},
        {handshake_idle_timeout_ms, ConnectTimeout},
        %% Keep-alive to maintain NAT binding
        {keep_alive_interval_ms, 1000}
    ],

    %% Add local port binding if specified (for predictable source port)
    case maps:get(local_port, Opts, undefined) of
        undefined -> BaseOpts;
        LocalPort -> [{local_port, LocalPort} | BaseOpts]
    end.

%% @private
%% @doc Collect results from parallel connection attempts.
-spec collect_results(reference(), non_neg_integer(), timeout(), [pid()]) ->
    punch_result().
collect_results(_Ref, 0, _Timeout, _Pids) ->
    {error, all_ports_failed};
collect_results(Ref, Remaining, Timeout, Pids) ->
    receive
        {punch_attempt, Ref, Port, {ok, Conn}} ->
            ?LOG_DEBUG("Connection succeeded on port ~p, killing other attempts", [Port]),
            %% Kill remaining attempts
            lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, Pids),
            {ok, Conn};
        {punch_attempt, Ref, Port, {error, Reason}} ->
            ?LOG_DEBUG("Connection to port ~p failed: ~p", [Port, Reason]),
            collect_results(Ref, Remaining - 1, Timeout, Pids)
    after Timeout ->
        ?LOG_DEBUG("Hole punch timeout waiting for ~p attempts", [Remaining]),
        %% Kill all attempts on timeout
        lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, Pids),
        {error, timeout}
    end.
