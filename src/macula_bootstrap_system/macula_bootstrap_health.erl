%%%-------------------------------------------------------------------
%%% @doc
%%% Bootstrap Health Monitor - Tracks system health metrics.
%%%
%%% Monitors:
%%% - Service registry size
%%% - DHT query rate
%%% - System uptime
%%% - Memory usage
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bootstrap_health).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_health/0, is_healthy/0]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    check_interval_ms :: pos_integer(),
    last_check :: integer(),
    health_status :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Get current health status
-spec get_health() -> {ok, map()}.
get_health() ->
    gen_server:call(?MODULE, get_health).

%% @doc Check if system is healthy
-spec is_healthy() -> boolean().
is_healthy() ->
    case get_health() of
        {ok, #{status := healthy}} -> true;
        _ -> false
    end.

%%%===================================================================
%%% GenServer Callbacks
%%%===================================================================

init(Config) ->
    CheckIntervalMs = maps:get(health_check_interval, Config, 30000),  % 30 seconds default

    ?LOG_INFO("[BootstrapHealth] Starting health monitor (interval: ~pms)", [CheckIntervalMs]),

    %% Schedule first health check
    erlang:send_after(CheckIntervalMs, self(), health_check),

    State = #state{
        check_interval_ms = CheckIntervalMs,
        last_check = erlang:system_time(second),
        health_status = #{
            status => unknown,
            message => <<"Health check pending">>
        }
    },

    {ok, State}.

handle_call(get_health, _From, State) ->
    {reply, {ok, State#state.health_status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Perform health check
handle_info(health_check, State) ->
    NewHealth = perform_health_check(),
    NewState = State#state{
        last_check = erlang:system_time(second),
        health_status = NewHealth
    },

    %% Log if unhealthy
    case maps:get(status, NewHealth) of
        healthy -> ok;
        Status ->
            ?LOG_WARNING("[BootstrapHealth] System unhealthy: ~p - ~s",
                        [Status, maps:get(message, NewHealth)])
    end,

    %% Schedule next check
    erlang:send_after(State#state.check_interval_ms, self(), health_check),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

perform_health_check() ->
    Checks = [
        check_server_running(),
        check_registry_running(),
        check_memory_usage()
    ],

    %% If any check fails, system is unhealthy
    case lists:filter(fun({Status, _}) -> Status =/= ok end, Checks) of
        [] ->
            #{
                status => healthy,
                message => <<"All checks passed">>,
                checks => Checks,
                timestamp => erlang:system_time(second)
            };
        Failures ->
            #{
                status => unhealthy,
                message => format_failures(Failures),
                checks => Checks,
                timestamp => erlang:system_time(second)
            }
    end.

check_server_running() ->
    case whereis(macula_bootstrap_server) of
        undefined -> {error, <<"Bootstrap server not running">>};
        Pid when is_pid(Pid) -> {ok, <<"Bootstrap server running">>}
    end.

check_registry_running() ->
    case whereis(macula_bootstrap_registry) of
        undefined -> {error, <<"Service registry not running">>};
        Pid when is_pid(Pid) ->
            %% Registry is running (delegates to DHT)
            {ok, <<"Bootstrap registry running (delegates to DHT)">>}
    end.

check_memory_usage() ->
    MemoryMB = erlang:memory(total) div (1024 * 1024),
    case MemoryMB > 1000 of  % Warn if > 1GB
        true -> {warning, iolist_to_binary(io_lib:format("High memory usage: ~pMB", [MemoryMB]))};
        false -> {ok, iolist_to_binary(io_lib:format("Memory usage: ~pMB", [MemoryMB]))}
    end.

format_failures(Failures) ->
    Messages = [Msg || {_Status, Msg} <- Failures],
    iolist_to_binary(lists:join("; ", Messages)).
