%%%-------------------------------------------------------------------
%%% @doc
%%% Port Prediction for NAT Traversal.
%%%
%%% Provides intelligent port prediction based on NAT allocation policies
%%% and historical port allocation data. Improves hole punch success rates
%%% by predicting the external ports a peer will use.
%%%
%%% Prediction Strategies by Allocation Policy:
%%%
%%% - PP (Port Preservation): External port = internal port
%%%   Prediction: Use last known port (high confidence)
%%%
%%% - PC (Port Contiguity): Sequential port allocation
%%%   Prediction: Calculate delta from history, predict next ports
%%%   Delta tracking with exponential moving average
%%%
%%% - RD (Random): Random port allocation
%%%   Prediction: Use historical range + common NAT port ranges
%%%   Statistical approach with lower confidence
%%%
%%% Port History Storage:
%%% - Tracks last N port allocations per peer (default: 10)
%%% - Calculates port deltas for PC allocation
%%% - Maintains statistics for RD allocation (mean, stddev, range)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_port_predictor).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    predict/2,
    predict/3,
    record_port/3,
    get_history/1,
    get_stats/1,
    clear/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, macula_port_history).
-define(DEFAULT_HISTORY_SIZE, 10).
-define(DEFAULT_PREDICTION_COUNT, 5).
-define(MAX_PORT, 65535).
-define(MIN_PORT, 1024).
-define(COMMON_NAT_PORT_RANGE_START, 32768).
-define(COMMON_NAT_PORT_RANGE_END, 61000).
-define(CLEANUP_INTERVAL_MS, 300000).  % Cleanup every 5 minutes
-define(HISTORY_TTL_SECONDS, 1800).    % 30 minutes TTL for history

%%%===================================================================
%%% Types
%%%===================================================================

-type allocation_policy() :: pp | pc | rd | unknown.

-type port_prediction() :: #{
    ports := [inet:port_number()],
    confidence := float(),              % 0.0 - 1.0
    strategy := pp | pc | rd | fallback,
    delta => integer(),                 % For PC strategy
    stats => port_stats()               % For RD strategy
}.

-type port_history() :: #{
    node_id := binary(),
    ports := [inet:port_number()],
    deltas := [integer()],
    updated_at := integer()
}.

-type port_stats() :: #{
    mean := float(),
    stddev := float(),
    min := inet:port_number(),
    max := inet:port_number(),
    count := non_neg_integer()
}.

-export_type([port_prediction/0, port_history/0, port_stats/0]).

-record(state, {
    table :: ets:tid(),
    history_size :: pos_integer(),
    prediction_count :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the port predictor server.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Predict external ports for a peer.
%% Uses NAT profile if available, otherwise uses historical data.
-spec predict(binary(), allocation_policy()) -> port_prediction().
predict(NodeId, AllocationPolicy) ->
    predict(NodeId, AllocationPolicy, #{}).

%% @doc Predict external ports with options.
%% Options:
%%   base_port - Known/expected base port for prediction
%%   count - Number of ports to predict (default: 5)
-spec predict(binary(), allocation_policy(), map()) -> port_prediction().
predict(NodeId, AllocationPolicy, Opts) ->
    gen_server:call(?SERVER, {predict, NodeId, AllocationPolicy, Opts}).

%% @doc Record an observed external port for a peer.
%% Used to build history for better predictions.
-spec record_port(binary(), inet:port_number(), allocation_policy()) -> ok.
record_port(NodeId, Port, AllocationPolicy) ->
    gen_server:cast(?SERVER, {record_port, NodeId, Port, AllocationPolicy}).

%% @doc Get port history for a peer.
-spec get_history(binary()) -> {ok, port_history()} | not_found.
get_history(NodeId) ->
    gen_server:call(?SERVER, {get_history, NodeId}).

%% @doc Get port statistics for a peer.
-spec get_stats(binary()) -> {ok, port_stats()} | not_found.
get_stats(NodeId) ->
    gen_server:call(?SERVER, {get_stats, NodeId}).

%% @doc Clear port history for a peer.
-spec clear(binary()) -> ok.
clear(NodeId) ->
    gen_server:cast(?SERVER, {clear, NodeId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    HistorySize = maps:get(port_history_size, Opts, ?DEFAULT_HISTORY_SIZE),
    PredictionCount = maps:get(port_prediction_count, Opts, ?DEFAULT_PREDICTION_COUNT),

    %% Create ETS table for port history
    Table = ets:new(?TABLE, [
        set,
        protected,
        {keypos, 1},
        {read_concurrency, true}
    ]),

    %% Schedule periodic cleanup
    schedule_cleanup(),

    ?LOG_INFO("Port predictor started (history_size=~p, prediction_count=~p)",
              [HistorySize, PredictionCount]),

    {ok, #state{
        table = Table,
        history_size = HistorySize,
        prediction_count = PredictionCount
    }}.

handle_call({predict, NodeId, AllocationPolicy, Opts}, _From, State) ->
    #state{table = Table, prediction_count = DefaultCount} = State,
    Count = maps:get(count, Opts, DefaultCount),
    BasePort = maps:get(base_port, Opts, undefined),

    Prediction = do_predict(Table, NodeId, AllocationPolicy, BasePort, Count),
    {reply, Prediction, State};

handle_call({get_history, NodeId}, _From, State) ->
    Result = case ets:lookup(State#state.table, NodeId) of
        [{NodeId, History}] -> {ok, History};
        [] -> not_found
    end,
    {reply, Result, State};

handle_call({get_stats, NodeId}, _From, State) ->
    Result = case ets:lookup(State#state.table, NodeId) of
        [{NodeId, #{ports := Ports}}] when length(Ports) > 0 ->
            {ok, calculate_stats(Ports)};
        _ ->
            not_found
    end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_port, NodeId, Port, AllocationPolicy}, State) ->
    #state{table = Table, history_size = MaxHistory} = State,
    Now = erlang:system_time(second),

    History = case ets:lookup(Table, NodeId) of
        [{NodeId, Existing}] ->
            update_history(Existing, Port, AllocationPolicy, MaxHistory, Now);
        [] ->
            new_history(NodeId, Port, AllocationPolicy, Now)
    end,

    ets:insert(Table, {NodeId, History}),
    ?LOG_DEBUG("Recorded port ~p for ~s (policy: ~p)", [Port, NodeId, AllocationPolicy]),
    {noreply, State};

handle_cast({clear, NodeId}, State) ->
    ets:delete(State#state.table, NodeId),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    cleanup_expired_history(State#state.table),
    schedule_cleanup(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{table = Table}) ->
    ets:delete(Table),
    ok.

%%%===================================================================
%%% Internal functions - Prediction
%%%===================================================================

%% @private
%% @doc Perform port prediction based on allocation policy and history.
-spec do_predict(ets:tid(), binary(), allocation_policy(),
                 inet:port_number() | undefined, pos_integer()) -> port_prediction().
do_predict(Table, NodeId, pp, BasePort, _Count) ->
    %% Port Preservation - use base port or last known port
    predict_pp(Table, NodeId, BasePort);
do_predict(Table, NodeId, pc, BasePort, Count) ->
    %% Port Contiguity - calculate delta and predict sequence
    predict_pc(Table, NodeId, BasePort, Count);
do_predict(Table, NodeId, rd, BasePort, Count) ->
    %% Random - use statistical approach
    predict_rd(Table, NodeId, BasePort, Count);
do_predict(Table, NodeId, unknown, BasePort, Count) ->
    %% Unknown - try PC first, fall back to range
    predict_unknown(Table, NodeId, BasePort, Count).

%% @private
%% @doc Predict ports for PP (Port Preservation) NAT.
-spec predict_pp(ets:tid(), binary(), inet:port_number() | undefined) -> port_prediction().
predict_pp(_Table, _NodeId, BasePort) when is_integer(BasePort), BasePort > 0 ->
    #{
        ports => [BasePort],
        confidence => 0.95,
        strategy => pp
    };
predict_pp(Table, NodeId, undefined) ->
    case ets:lookup(Table, NodeId) of
        [{NodeId, #{ports := [LastPort | _]}}] ->
            #{
                ports => [LastPort],
                confidence => 0.85,
                strategy => pp
            };
        _ ->
            %% No history - can't predict PP
            fallback_prediction()
    end.

%% @private
%% @doc Predict ports for PC (Port Contiguity) NAT.
-spec predict_pc(ets:tid(), binary(), inet:port_number() | undefined, pos_integer()) ->
    port_prediction().
predict_pc(Table, NodeId, BasePort, Count) ->
    case get_average_delta(Table, NodeId) of
        {ok, Delta} when Delta =/= 0 ->
            Base = determine_base_port_pc(Table, NodeId, BasePort),
            Ports = generate_sequential_ports(Base, Delta, Count),
            #{
                ports => Ports,
                confidence => calculate_pc_confidence(Table, NodeId),
                strategy => pc,
                delta => Delta
            };
        _ ->
            %% No reliable delta - use fallback with base port
            case BasePort of
                undefined -> fallback_prediction();
                _ -> predict_around_port(BasePort, Count)
            end
    end.

%% @private
%% @doc Predict ports for RD (Random) NAT.
-spec predict_rd(ets:tid(), binary(), inet:port_number() | undefined, pos_integer()) ->
    port_prediction().
predict_rd(Table, NodeId, BasePort, Count) ->
    case ets:lookup(Table, NodeId) of
        [{NodeId, #{ports := Ports}}] when length(Ports) >= 3 ->
            Stats = calculate_stats(Ports),
            PredictedPorts = predict_from_stats(Stats, BasePort, Count),
            #{
                ports => PredictedPorts,
                confidence => calculate_rd_confidence(Stats),
                strategy => rd,
                stats => Stats
            };
        _ ->
            %% Not enough history - use common NAT range
            predict_common_nat_range(BasePort, Count)
    end.

%% @private
%% @doc Predict ports when allocation policy is unknown.
-spec predict_unknown(ets:tid(), binary(), inet:port_number() | undefined, pos_integer()) ->
    port_prediction().
predict_unknown(Table, NodeId, BasePort, Count) ->
    %% Try to infer policy from history
    case infer_allocation_policy(Table, NodeId) of
        {ok, pp} -> predict_pp(Table, NodeId, BasePort);
        {ok, pc} -> predict_pc(Table, NodeId, BasePort, Count);
        {ok, rd} -> predict_rd(Table, NodeId, BasePort, Count);
        unknown ->
            case BasePort of
                undefined -> fallback_prediction();
                _ -> predict_around_port(BasePort, Count)
            end
    end.

%% @private
%% @doc Generate fallback prediction when no data available.
-spec fallback_prediction() -> port_prediction().
fallback_prediction() ->
    %% Use common NAT ephemeral port range
    Ports = [
        ?COMMON_NAT_PORT_RANGE_START,
        ?COMMON_NAT_PORT_RANGE_START + 1000,
        ?COMMON_NAT_PORT_RANGE_START + 2000,
        ?COMMON_NAT_PORT_RANGE_START + 3000,
        ?COMMON_NAT_PORT_RANGE_START + 4000
    ],
    #{
        ports => Ports,
        confidence => 0.1,
        strategy => fallback
    }.

%% @private
%% @doc Predict ports around a known base port.
-spec predict_around_port(inet:port_number(), pos_integer()) -> port_prediction().
predict_around_port(BasePort, Count) ->
    HalfCount = Count div 2,
    Offsets = lists:seq(-HalfCount, HalfCount),
    Ports = [clamp_port(BasePort + Offset) || Offset <- Offsets],
    UniquePorts = lists:usort(Ports),
    #{
        ports => lists:sublist(UniquePorts, Count),
        confidence => 0.5,
        strategy => fallback
    }.

%% @private
%% @doc Predict ports in common NAT range.
-spec predict_common_nat_range(inet:port_number() | undefined, pos_integer()) ->
    port_prediction().
predict_common_nat_range(BasePort, Count) when is_integer(BasePort) ->
    %% Center predictions around base port within NAT range
    RangeSize = ?COMMON_NAT_PORT_RANGE_END - ?COMMON_NAT_PORT_RANGE_START,
    Step = RangeSize div (Count + 1),
    BasePorts = [clamp_port(BasePort + (I - Count div 2) * Step) || I <- lists:seq(0, Count - 1)],
    #{
        ports => BasePorts,
        confidence => 0.2,
        strategy => rd
    };
predict_common_nat_range(undefined, Count) ->
    %% Spread across common NAT range
    RangeSize = ?COMMON_NAT_PORT_RANGE_END - ?COMMON_NAT_PORT_RANGE_START,
    Step = RangeSize div (Count + 1),
    Ports = [?COMMON_NAT_PORT_RANGE_START + (I * Step) || I <- lists:seq(1, Count)],
    #{
        ports => Ports,
        confidence => 0.15,
        strategy => rd
    }.

%%%===================================================================
%%% Internal functions - History Management
%%%===================================================================

%% @private
%% @doc Create new port history entry.
-spec new_history(binary(), inet:port_number(), allocation_policy(), integer()) ->
    port_history().
new_history(NodeId, Port, _AllocationPolicy, Now) ->
    #{
        node_id => NodeId,
        ports => [Port],
        deltas => [],
        updated_at => Now
    }.

%% @private
%% @doc Update existing port history.
-spec update_history(port_history(), inet:port_number(), allocation_policy(),
                     pos_integer(), integer()) -> port_history().
update_history(#{ports := Ports, deltas := Deltas} = History, Port, _Policy, MaxHistory, Now) ->
    NewPorts = lists:sublist([Port | Ports], MaxHistory),

    %% Calculate delta from previous port
    NewDeltas = case Ports of
        [PrevPort | _] ->
            Delta = Port - PrevPort,
            lists:sublist([Delta | Deltas], MaxHistory - 1);
        [] ->
            Deltas
    end,

    History#{
        ports := NewPorts,
        deltas := NewDeltas,
        updated_at := Now
    }.

%% @private
%% @doc Get average delta from port history.
-spec get_average_delta(ets:tid(), binary()) -> {ok, integer()} | not_found.
get_average_delta(Table, NodeId) ->
    case ets:lookup(Table, NodeId) of
        [{NodeId, #{deltas := Deltas}}] when length(Deltas) >= 2 ->
            %% Use exponential weighted moving average (recent deltas weighted more)
            AvgDelta = calculate_weighted_average(Deltas),
            {ok, round(AvgDelta)};
        _ ->
            not_found
    end.

%% @private
%% @doc Calculate exponentially weighted moving average.
-spec calculate_weighted_average([integer()]) -> float().
calculate_weighted_average([]) -> 0.0;
calculate_weighted_average(Deltas) ->
    Alpha = 0.3,  % Weight for most recent value
    calculate_ewma(Deltas, Alpha, 0.0, 0.0).

calculate_ewma([], _Alpha, Sum, TotalWeight) when TotalWeight > 0 ->
    Sum / TotalWeight;
calculate_ewma([], _Alpha, _Sum, _TotalWeight) ->
    0.0;
calculate_ewma([Delta | Rest], Alpha, Sum, TotalWeight) ->
    Weight = math:pow(1 - Alpha, length(Rest)),
    calculate_ewma(Rest, Alpha, Sum + (Delta * Weight), TotalWeight + Weight).

%% @private
%% @doc Determine base port for PC prediction.
-spec determine_base_port_pc(ets:tid(), binary(), inet:port_number() | undefined) ->
    inet:port_number().
determine_base_port_pc(_Table, _NodeId, BasePort) when is_integer(BasePort), BasePort > 0 ->
    BasePort;
determine_base_port_pc(Table, NodeId, undefined) ->
    case ets:lookup(Table, NodeId) of
        [{NodeId, #{ports := [LastPort | _]}}] -> LastPort;
        _ -> ?COMMON_NAT_PORT_RANGE_START
    end.

%% @private
%% @doc Generate sequential ports based on delta.
-spec generate_sequential_ports(inet:port_number(), integer(), pos_integer()) ->
    [inet:port_number()].
generate_sequential_ports(Base, Delta, Count) ->
    HalfCount = Count div 2,
    %% Generate ports both forward and backward from base
    Offsets = lists:seq(-HalfCount, HalfCount + (Count rem 2)),
    Ports = [clamp_port(Base + (Offset * Delta)) || Offset <- Offsets],
    lists:usort(Ports).

%% @private
%% @doc Calculate confidence for PC prediction.
-spec calculate_pc_confidence(ets:tid(), binary()) -> float().
calculate_pc_confidence(Table, NodeId) ->
    case ets:lookup(Table, NodeId) of
        [{NodeId, #{deltas := Deltas}}] when length(Deltas) >= 3 ->
            %% Confidence based on delta consistency (low variance = high confidence)
            Variance = calculate_variance(Deltas),
            %% Map variance to confidence: low variance = high confidence
            max(0.3, min(0.9, 1.0 - (Variance / 1000)));
        [{NodeId, #{deltas := Deltas}}] when length(Deltas) >= 1 ->
            %% Some history - moderate confidence
            0.5;
        _ ->
            0.3
    end.

%%%===================================================================
%%% Internal functions - Statistics
%%%===================================================================

%% @private
%% @doc Calculate statistics from port list.
-spec calculate_stats([inet:port_number()]) -> port_stats().
calculate_stats([]) ->
    #{mean => 0.0, stddev => 0.0, min => 0, max => 0, count => 0};
calculate_stats(Ports) ->
    Count = length(Ports),
    Sum = lists:sum(Ports),
    Mean = Sum / Count,
    Variance = calculate_variance_from_mean(Ports, Mean),
    StdDev = math:sqrt(Variance),
    #{
        mean => Mean,
        stddev => StdDev,
        min => lists:min(Ports),
        max => lists:max(Ports),
        count => Count
    }.

%% @private
%% @doc Calculate variance of a list.
-spec calculate_variance([number()]) -> float().
calculate_variance([]) -> 0.0;
calculate_variance([_]) -> 0.0;
calculate_variance(List) ->
    Mean = lists:sum(List) / length(List),
    calculate_variance_from_mean(List, Mean).

-spec calculate_variance_from_mean([number()], float()) -> float().
calculate_variance_from_mean(List, Mean) ->
    SumSquaredDiffs = lists:sum([math:pow(X - Mean, 2) || X <- List]),
    SumSquaredDiffs / max(1, length(List) - 1).

%% @private
%% @doc Predict ports from statistics.
-spec predict_from_stats(port_stats(), inet:port_number() | undefined, pos_integer()) ->
    [inet:port_number()].
predict_from_stats(#{mean := Mean, stddev := StdDev, min := Min, max := Max}, BasePort, Count) ->
    %% Generate ports within 2 standard deviations of mean
    Center = case BasePort of
        undefined -> round(Mean);
        _ -> BasePort
    end,

    %% Spread predictions across the observed range
    Range = max(1, Max - Min),
    Step = max(1, Range div (Count + 1)),

    %% Generate ports centered around the mean/base
    HalfCount = Count div 2,
    Ports = [clamp_port(Center + ((I - HalfCount) * Step)) || I <- lists:seq(0, Count - 1)],

    %% Also include ports at +/- 1 and 2 stddev if we have room
    StdDevPorts = case StdDev > 10 of
        true ->
            [clamp_port(round(Mean + StdDev)),
             clamp_port(round(Mean - StdDev))];
        false ->
            []
    end,

    lists:usort(Ports ++ StdDevPorts).

%% @private
%% @doc Calculate confidence for RD prediction.
-spec calculate_rd_confidence(port_stats()) -> float().
calculate_rd_confidence(#{stddev := StdDev, count := Count}) when Count >= 5 ->
    %% More samples and tighter distribution = higher confidence
    CountFactor = min(1.0, Count / 10),
    StdDevFactor = max(0.1, 1.0 - (StdDev / 10000)),
    CountFactor * StdDevFactor * 0.5;  % Cap at 0.5 for RD
calculate_rd_confidence(#{count := Count}) when Count >= 3 ->
    0.25;
calculate_rd_confidence(_) ->
    0.1.

%% @private
%% @doc Infer allocation policy from port history.
-spec infer_allocation_policy(ets:tid(), binary()) -> {ok, allocation_policy()} | unknown.
infer_allocation_policy(Table, NodeId) ->
    case ets:lookup(Table, NodeId) of
        [{NodeId, #{ports := Ports, deltas := Deltas}}] when length(Ports) >= 3 ->
            analyze_port_pattern(Ports, Deltas);
        _ ->
            unknown
    end.

%% @private
%% @doc Analyze port pattern to determine allocation policy.
-spec analyze_port_pattern([inet:port_number()], [integer()]) ->
    {ok, allocation_policy()} | unknown.
analyze_port_pattern(Ports, Deltas) ->
    %% Check for port preservation (all same or very close)
    UniqueCount = length(lists:usort(Ports)),
    case UniqueCount of
        1 -> {ok, pp};
        _ ->
            case length(Deltas) >= 2 of
                true ->
                    %% Check for port contiguity (consistent delta)
                    Variance = calculate_variance(Deltas),
                    case Variance < 10 of  % Low variance = consistent delta
                        true -> {ok, pc};
                        false -> {ok, rd}
                    end;
                false ->
                    unknown
            end
    end.

%%%===================================================================
%%% Internal functions - Utilities
%%%===================================================================

%% @private
%% @doc Clamp port to valid range.
-spec clamp_port(integer()) -> inet:port_number().
clamp_port(Port) when Port < ?MIN_PORT -> ?MIN_PORT;
clamp_port(Port) when Port > ?MAX_PORT -> ?MAX_PORT;
clamp_port(Port) -> Port.

%% @private
%% @doc Schedule periodic cleanup.
-spec schedule_cleanup() -> reference().
schedule_cleanup() ->
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup).

%% @private
%% @doc Clean up expired history entries.
-spec cleanup_expired_history(ets:tid()) -> ok.
cleanup_expired_history(Table) ->
    Now = erlang:system_time(second),
    MaxAge = ?HISTORY_TTL_SECONDS,

    ExpiredKeys = ets:foldl(
        fun({NodeId, #{updated_at := UpdatedAt}}, Acc) ->
            case Now - UpdatedAt > MaxAge of
                true -> [NodeId | Acc];
                false -> Acc
            end
        end,
        [],
        Table
    ),

    lists:foreach(fun(Key) -> ets:delete(Table, Key) end, ExpiredKeys),

    case ExpiredKeys of
        [] -> ok;
        _ -> ?LOG_DEBUG("Port predictor cleanup: removed ~p expired entries",
                        [length(ExpiredKeys)])
    end.
