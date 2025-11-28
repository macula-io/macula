%%%-------------------------------------------------------------------
%%% @doc
%%% QUIC Hole Punch Executor.
%%%
%%% Implements the simultaneous open (SYN-SYN) pattern for QUIC
%%% to establish direct connections through NAT devices.
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
%%% Timeout Handling:
%%% - Each punch attempt has 2 second timeout
%%% - Up to 3 attempts before declaring failure
%%% - Falls back to relay on failure
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_hole_punch).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    execute/3,
    execute_async/3,
    cancel/1
]).

-define(PUNCH_ATTEMPT_TIMEOUT_MS, 2000).  % 2 seconds per attempt
-define(CONNECT_TIMEOUT_MS, 1500).        % 1.5 second QUIC connect timeout
-define(MAX_PARALLEL_ATTEMPTS, 3).        % Try 3 ports in parallel

%%%===================================================================
%%% Types
%%%===================================================================

-type punch_opts() :: #{
    target_host := binary() | string(),
    target_ports := [inet:port_number()],
    local_port => inet:port_number(),
    session_id := binary(),
    punch_time := integer(),
    role := initiator | target
}.

-type punch_result() ::
    {ok, quicer:connection_handle()} |
    {error, timeout | unreachable | all_ports_failed}.

-export_type([punch_opts/0, punch_result/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Execute a hole punch attempt synchronously.
%% Blocks until connection established or timeout.
-spec execute(binary(), punch_opts(), timeout()) -> punch_result().
execute(TargetNodeId, Opts, Timeout) ->
    #{target_host := Host, target_ports := Ports} = Opts,

    ?LOG_DEBUG("Executing hole punch to ~s (~s) on ports ~p",
               [TargetNodeId, Host, Ports]),

    %% Wait until coordinated punch time if specified
    maybe_wait_for_punch_time(Opts),

    %% Try connecting to target ports in parallel
    attempt_connection(Host, Ports, Opts, Timeout).

%% @doc Execute hole punch asynchronously.
%% Returns immediately, caller receives result via message.
-spec execute_async(binary(), punch_opts(), pid()) -> reference().
execute_async(TargetNodeId, Opts, ReplyTo) ->
    Ref = make_ref(),
    spawn_link(fun() ->
        Result = execute(TargetNodeId, Opts, ?PUNCH_ATTEMPT_TIMEOUT_MS),
        ReplyTo ! {hole_punch_result, Ref, Result}
    end),
    Ref.

%% @doc Cancel an ongoing hole punch attempt.
-spec cancel(reference()) -> ok.
cancel(Ref) ->
    %% In a full implementation, this would track and kill the spawn
    ?LOG_DEBUG("Cancelling hole punch ~p (not fully implemented)", [Ref]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
%% Tries ports in parallel for faster hole punching.
-spec attempt_connection(binary() | string(), [inet:port_number()],
                          punch_opts(), timeout()) -> punch_result().
attempt_connection(_Host, [], _Opts, _Timeout) ->
    {error, all_ports_failed};
attempt_connection(Host, Ports, Opts, Timeout) ->
    %% Limit parallel attempts
    AttemptsCount = min(length(Ports), ?MAX_PARALLEL_ATTEMPTS),
    PortsToTry = lists:sublist(Ports, AttemptsCount),

    ?LOG_DEBUG("Trying ~p ports in parallel: ~p", [AttemptsCount, PortsToTry]),

    %% Spawn parallel connection attempts
    Self = self(),
    Ref = make_ref(),

    Pids = [spawn_link(fun() ->
        Result = try_single_connection(Host, Port, Opts),
        Self ! {punch_attempt, Ref, Port, Result}
    end) || Port <- PortsToTry],

    %% Wait for first success or all failures
    collect_results(Ref, length(Pids), Timeout, Pids).

%% @private
%% @doc Try a single QUIC connection to host:port.
-spec try_single_connection(binary() | string(), inet:port_number(), punch_opts()) ->
    {ok, quicer:connection_handle()} | {error, term()}.
try_single_connection(Host, Port, Opts) ->
    ?LOG_DEBUG("Attempting QUIC connect to ~s:~p", [Host, Port]),

    %% Build QUIC connection options
    ConnOpts = build_quic_opts(Opts),

    %% Convert host to appropriate format
    HostStr = case is_binary(Host) of
        true -> binary_to_list(Host);
        false -> Host
    end,

    %% Attempt QUIC connection
    case quicer:connect(HostStr, Port, ConnOpts, ?CONNECT_TIMEOUT_MS) of
        {ok, Conn} ->
            ?LOG_INFO("Hole punch SUCCESS: connected to ~s:~p", [Host, Port]),
            {ok, Conn};
        {error, Reason} = Error ->
            ?LOG_DEBUG("Hole punch to ~s:~p failed: ~p", [Host, Port, Reason]),
            Error
    end.

%% @private
%% @doc Build QUIC connection options for hole punching.
-spec build_quic_opts(punch_opts()) -> list().
build_quic_opts(Opts) ->
    BaseOpts = [
        {alpn, ["macula"]},
        {verify, none},
        %% Short timeouts for hole punching attempts
        {idle_timeout_ms, 5000},
        {handshake_idle_timeout_ms, 2000},
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
            [exit(Pid, shutdown) || Pid <- Pids],
            {ok, Conn};
        {punch_attempt, Ref, Port, {error, Reason}} ->
            ?LOG_DEBUG("Connection to port ~p failed: ~p", [Port, Reason]),
            collect_results(Ref, Remaining - 1, Timeout, Pids)
    after Timeout ->
        ?LOG_DEBUG("Hole punch timeout waiting for ~p attempts", [Remaining]),
        %% Kill all attempts on timeout
        [exit(Pid, shutdown) || Pid <- Pids],
        {error, timeout}
    end.
