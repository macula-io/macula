%%%-------------------------------------------------------------------
%%% @doc Macula App Monitor
%%%
%%% Runtime defense monitor for deployed applications:
%%% - Memory usage monitoring
%%% - Message queue monitoring
%%% - Crash rate detection
%%% - Throttle/Kill/Quarantine actions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_app_monitor).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([start_monitoring/2, stop_monitoring/1]).
-export([set_memory_limit/2, set_message_queue_limit/2, set_crash_threshold/3]).
-export([get_stats/1, get_all_stats/0]).
-export([throttle_app/1, kill_app/1, quarantine_app/1, restore_app/1]).
-export([is_quarantined/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL, 5000).  % 5 seconds
-define(DEFAULT_MEMORY_LIMIT_MB, 256).
-define(DEFAULT_MSG_QUEUE_LIMIT, 10000).
-define(DEFAULT_CRASH_THRESHOLD, 3).
-define(DEFAULT_CRASH_WINDOW_SEC, 60).

%% app_state must be defined before state since state references it
-record(app_state, {
    pid :: pid(),
    monitor_ref :: reference(),
    memory_limit_mb :: pos_integer(),
    msg_queue_limit :: pos_integer(),
    crash_threshold :: pos_integer(),
    crash_window_sec :: pos_integer(),
    crash_times :: [integer()],  % List of crash timestamps
    status :: running | throttled | quarantined,
    metrics :: #{
        memory_mb := non_neg_integer(),
        message_queue := non_neg_integer(),
        crash_count := non_neg_integer()
    }
}).

-record(state, {
    monitored_apps :: #{binary() => #app_state{}},
    check_timer :: reference() | undefined
}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the app monitor
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc Start monitoring an application
-spec start_monitoring(PackageName :: binary(), Pid :: pid()) -> ok.
start_monitoring(PackageName, Pid) ->
    gen_server:call(?SERVER, {start_monitoring, PackageName, Pid}).

%% @doc Stop monitoring an application
-spec stop_monitoring(PackageName :: binary()) -> ok.
stop_monitoring(PackageName) ->
    gen_server:call(?SERVER, {stop_monitoring, PackageName}).

%% @doc Set memory limit for an application
-spec set_memory_limit(PackageName :: binary(), LimitMB :: pos_integer()) -> ok | {error, not_found}.
set_memory_limit(PackageName, LimitMB) ->
    gen_server:call(?SERVER, {set_memory_limit, PackageName, LimitMB}).

%% @doc Set message queue limit for an application
-spec set_message_queue_limit(PackageName :: binary(), Limit :: pos_integer()) -> ok | {error, not_found}.
set_message_queue_limit(PackageName, Limit) ->
    gen_server:call(?SERVER, {set_msg_queue_limit, PackageName, Limit}).

%% @doc Set crash threshold for an application
-spec set_crash_threshold(PackageName :: binary(), MaxCrashes :: pos_integer(),
                           WindowSec :: pos_integer()) -> ok | {error, not_found}.
set_crash_threshold(PackageName, MaxCrashes, WindowSec) ->
    gen_server:call(?SERVER, {set_crash_threshold, PackageName, MaxCrashes, WindowSec}).

%% @doc Get monitoring stats for an application
-spec get_stats(PackageName :: binary()) ->
    {ok, #{memory_mb := non_neg_integer(),
           message_queue := non_neg_integer(),
           crash_count := non_neg_integer(),
           status := atom()}} |
    {error, not_found}.
get_stats(PackageName) ->
    gen_server:call(?SERVER, {get_stats, PackageName}).

%% @doc Get stats for all monitored applications
-spec get_all_stats() -> #{binary() => map()}.
get_all_stats() ->
    gen_server:call(?SERVER, get_all_stats).

%% @doc Throttle an application (reduce priority)
-spec throttle_app(PackageName :: binary()) -> ok | {error, term()}.
throttle_app(PackageName) ->
    gen_server:call(?SERVER, {throttle, PackageName}).

%% @doc Kill an application
-spec kill_app(PackageName :: binary()) -> ok | {error, term()}.
kill_app(PackageName) ->
    gen_server:call(?SERVER, {kill, PackageName}).

%% @doc Quarantine an application (stop and prevent restart)
-spec quarantine_app(PackageName :: binary()) -> ok | {error, term()}.
quarantine_app(PackageName) ->
    gen_server:call(?SERVER, {quarantine, PackageName}).

%% @doc Restore a quarantined application
-spec restore_app(PackageName :: binary()) -> ok | {error, term()}.
restore_app(PackageName) ->
    gen_server:call(?SERVER, {restore, PackageName}).

%% @doc Check if an application is quarantined
-spec is_quarantined(PackageName :: binary()) -> boolean().
is_quarantined(PackageName) ->
    gen_server:call(?SERVER, {is_quarantined, PackageName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(_Config) ->
    Timer = erlang:send_after(?CHECK_INTERVAL, self(), check_apps),
    {ok, #state{
        monitored_apps = #{},
        check_timer = Timer
    }}.

%% @private
handle_call({start_monitoring, PackageName, Pid}, _From, State) ->
    MonitorRef = erlang:monitor(process, Pid),
    AppState = #app_state{
        pid = Pid,
        monitor_ref = MonitorRef,
        memory_limit_mb = ?DEFAULT_MEMORY_LIMIT_MB,
        msg_queue_limit = ?DEFAULT_MSG_QUEUE_LIMIT,
        crash_threshold = ?DEFAULT_CRASH_THRESHOLD,
        crash_window_sec = ?DEFAULT_CRASH_WINDOW_SEC,
        crash_times = [],
        status = running,
        metrics = #{memory_mb => 0, message_queue => 0, crash_count => 0}
    },
    Apps = maps:put(PackageName, AppState, State#state.monitored_apps),
    ?LOG_INFO("[AppMonitor] Started monitoring ~s (pid=~p)", [PackageName, Pid]),
    {reply, ok, State#state{monitored_apps = Apps}};

handle_call({stop_monitoring, PackageName}, _From, State) ->
    case maps:get(PackageName, State#state.monitored_apps, undefined) of
        undefined ->
            {reply, ok, State};
        #app_state{monitor_ref = MonitorRef} ->
            erlang:demonitor(MonitorRef, [flush]),
            Apps = maps:remove(PackageName, State#state.monitored_apps),
            ?LOG_INFO("[AppMonitor] Stopped monitoring ~s", [PackageName]),
            {reply, ok, State#state{monitored_apps = Apps}}
    end;

handle_call({set_memory_limit, PackageName, LimitMB}, _From, State) ->
    Result = update_app_config(PackageName, State, fun(AppState) ->
        AppState#app_state{memory_limit_mb = LimitMB}
    end),
    {reply, element(1, Result), element(2, Result)};

handle_call({set_msg_queue_limit, PackageName, Limit}, _From, State) ->
    Result = update_app_config(PackageName, State, fun(AppState) ->
        AppState#app_state{msg_queue_limit = Limit}
    end),
    {reply, element(1, Result), element(2, Result)};

handle_call({set_crash_threshold, PackageName, MaxCrashes, WindowSec}, _From, State) ->
    Result = update_app_config(PackageName, State, fun(AppState) ->
        AppState#app_state{crash_threshold = MaxCrashes, crash_window_sec = WindowSec}
    end),
    {reply, element(1, Result), element(2, Result)};

handle_call({get_stats, PackageName}, _From, State) ->
    case maps:get(PackageName, State#state.monitored_apps, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #app_state{metrics = Metrics, status = Status} ->
            {reply, {ok, Metrics#{status => Status}}, State}
    end;

handle_call(get_all_stats, _From, State) ->
    Stats = maps:map(fun(_K, #app_state{metrics = Metrics, status = Status}) ->
        Metrics#{status => Status}
    end, State#state.monitored_apps),
    {reply, Stats, State};

handle_call({throttle, PackageName}, _From, State) ->
    Result = update_app_status(PackageName, throttled, State),
    {reply, element(1, Result), element(2, Result)};

handle_call({kill, PackageName}, _From, State) ->
    Result = do_kill_app(PackageName, State),
    {reply, Result, State};

handle_call({quarantine, PackageName}, _From, State) ->
    %% Kill and mark as quarantined
    _ = do_kill_app(PackageName, State),
    Result = update_app_status(PackageName, quarantined, State),
    {reply, element(1, Result), element(2, Result)};

handle_call({restore, PackageName}, _From, State) ->
    Result = update_app_status(PackageName, running, State),
    {reply, element(1, Result), element(2, Result)};

handle_call({is_quarantined, PackageName}, _From, State) ->
    case maps:get(PackageName, State#state.monitored_apps, undefined) of
        undefined -> {reply, false, State};
        #app_state{status = quarantined} -> {reply, true, State};
        _ -> {reply, false, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(check_apps, State) ->
    NewState = check_all_apps(State),
    Timer = erlang:send_after(?CHECK_INTERVAL, self(), check_apps),
    {noreply, NewState#state{check_timer = Timer}};

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    NewState = handle_app_down(MonitorRef, Pid, Reason, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    case State#state.check_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Update app configuration
update_app_config(PackageName, State, UpdateFun) ->
    case maps:get(PackageName, State#state.monitored_apps, undefined) of
        undefined ->
            {{error, not_found}, State};
        AppState ->
            NewAppState = UpdateFun(AppState),
            Apps = maps:put(PackageName, NewAppState, State#state.monitored_apps),
            {ok, State#state{monitored_apps = Apps}}
    end.

%% @private Update app status
update_app_status(PackageName, NewStatus, State) ->
    update_app_config(PackageName, State, fun(AppState) ->
        AppState#app_state{status = NewStatus}
    end).

%% @private Kill an application
do_kill_app(PackageName, State) ->
    case maps:get(PackageName, State#state.monitored_apps, undefined) of
        undefined ->
            {error, not_found};
        #app_state{pid = Pid} ->
            ?LOG_WARNING("[AppMonitor] Killing application ~s (pid=~p)", [PackageName, Pid]),
            exit(Pid, kill),
            ok
    end.

%% @private Check all monitored applications
check_all_apps(State) ->
    Apps = maps:map(fun(PackageName, AppState) ->
        check_single_app(PackageName, AppState)
    end, State#state.monitored_apps),
    State#state{monitored_apps = Apps}.

%% @private Check a single application
check_single_app(_PackageName, #app_state{status = quarantined} = AppState) ->
    %% Skip quarantined apps
    AppState;
check_single_app(PackageName, #app_state{pid = Pid} = AppState) ->
    case is_process_alive(Pid) of
        false ->
            AppState;
        true ->
            %% Get process info
            case erlang:process_info(Pid, [memory, message_queue_len]) of
                undefined ->
                    AppState;
                Info ->
                    MemoryBytes = proplists:get_value(memory, Info, 0),
                    MemoryMB = MemoryBytes div (1024 * 1024),
                    MsgQueue = proplists:get_value(message_queue_len, Info, 0),

                    NewMetrics = #{
                        memory_mb => MemoryMB,
                        message_queue => MsgQueue,
                        crash_count => length(AppState#app_state.crash_times)
                    },

                    NewAppState = AppState#app_state{metrics = NewMetrics},

                    %% Check thresholds
                    check_thresholds(PackageName, NewAppState)
            end
    end.

%% @private Check if thresholds are exceeded
check_thresholds(PackageName, #app_state{metrics = Metrics} = AppState) ->
    MemoryMB = maps:get(memory_mb, Metrics),
    MsgQueue = maps:get(message_queue, Metrics),

    %% Check memory limit
    case MemoryMB > AppState#app_state.memory_limit_mb of
        true ->
            ?LOG_WARNING("[AppMonitor] ~s exceeds memory limit: ~p MB > ~p MB",
                        [PackageName, MemoryMB, AppState#app_state.memory_limit_mb]),
            take_action(PackageName, memory_exceeded, AppState);
        false ->
            %% Check message queue
            case MsgQueue > AppState#app_state.msg_queue_limit of
                true ->
                    ?LOG_WARNING("[AppMonitor] ~s exceeds message queue limit: ~p > ~p",
                                [PackageName, MsgQueue, AppState#app_state.msg_queue_limit]),
                    take_action(PackageName, queue_exceeded, AppState);
                false ->
                    AppState
            end
    end.

%% @private Take action on threshold violation
take_action(PackageName, Reason, #app_state{status = running} = AppState) ->
    ?LOG_WARNING("[AppMonitor] Throttling ~s due to ~p", [PackageName, Reason]),
    AppState#app_state{status = throttled};
take_action(PackageName, Reason, #app_state{status = throttled, pid = Pid} = AppState) ->
    ?LOG_ERROR("[AppMonitor] Killing ~s due to persistent ~p", [PackageName, Reason]),
    exit(Pid, kill),
    AppState#app_state{status = quarantined};
take_action(_PackageName, _Reason, AppState) ->
    AppState.

%% @private Handle application crash
handle_app_down(MonitorRef, _Pid, Reason, State) ->
    %% Find the app by monitor ref
    case find_app_by_monitor_ref(MonitorRef, State#state.monitored_apps) of
        {ok, PackageName, AppState} ->
            Now = erlang:system_time(second),
            WindowStart = Now - AppState#app_state.crash_window_sec,

            %% Filter crash times within window
            RecentCrashes = [T || T <- AppState#app_state.crash_times, T > WindowStart],
            NewCrashTimes = [Now | RecentCrashes],

            ?LOG_WARNING("[AppMonitor] ~s crashed (~p crashes in window): ~p",
                        [PackageName, length(NewCrashTimes), Reason]),

            %% Check if threshold exceeded
            NewStatus = case length(NewCrashTimes) >= AppState#app_state.crash_threshold of
                true ->
                    ?LOG_ERROR("[AppMonitor] ~s exceeded crash threshold - quarantining",
                              [PackageName]),
                    quarantined;
                false ->
                    AppState#app_state.status
            end,

            %% Update app state
            NewAppState = AppState#app_state{
                crash_times = NewCrashTimes,
                status = NewStatus,
                metrics = maps:put(crash_count, length(NewCrashTimes),
                                   AppState#app_state.metrics)
            },
            Apps = maps:put(PackageName, NewAppState, State#state.monitored_apps),
            State#state{monitored_apps = Apps};
        not_found ->
            State
    end.

%% @private Find app by monitor ref
find_app_by_monitor_ref(MonitorRef, Apps) ->
    maps:fold(fun(PackageName, #app_state{monitor_ref = Ref} = AppState, Acc) ->
        case Ref =:= MonitorRef of
            true -> {ok, PackageName, AppState};
            false -> Acc
        end
    end, not_found, Apps).
