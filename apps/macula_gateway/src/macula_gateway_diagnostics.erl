%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Gateway Diagnostics Service
%%%
%%% Provides simple diagnostic procedures that clients can call to verify
%%% connectivity and test the gateway's RPC functionality.
%%%
%%% Available procedures:
%%%   - com.macula.diagnostics.hello - Returns a friendly greeting with gateway info
%%%   - com.macula.diagnostics.echo   - Echoes back the arguments sent by client
%%%   - com.macula.diagnostics.info   - Returns detailed gateway information
%%%
%%% Usage:
%%%   The diagnostics service automatically registers when the gateway starts.
%%%   Clients can call these procedures using the Macula SDK:
%%%
%%%   Elixir:
%%%     {:ok, result} = MaculaSdk.Client.call(client, "com.macula.diagnostics.hello", %{})
%%%
%%%   Result:
%%%     %{
%%%       "message" => "Hello from Macula Gateway!",
%%%       "gateway" => "macula@127.0.0.1",
%%%       "realm" => "be.cortexiq.energy",
%%%       "uptime_seconds" => 42,
%%%       "timestamp" => 1699612800
%%%     }
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_diagnostics).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    register_procedures/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    gateway_pid :: pid(),
    realm :: binary(),
    started_at :: integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the diagnostics service
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Register diagnostic procedures with the gateway
-spec register_procedures(pid()) -> ok.
register_procedures(GatewayPid) ->
    gen_server:cast(?MODULE, {register_procedures, GatewayPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Realm = proplists:get_value(realm, Opts, <<"macula.default">>),

    io:format("Starting diagnostics service for realm: ~s~n", [Realm]),

    State = #state{
        gateway_pid = undefined,
        realm = Realm,
        started_at = erlang:system_time(second)
    },

    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register_procedures, GatewayPid}, State) ->
    %% Register diagnostic procedures
    Procedures = [
        <<"com.macula.diagnostics.hello">>,
        <<"com.macula.diagnostics.echo">>,
        <<"com.macula.diagnostics.info">>
    ],

    lists:foreach(fun(Procedure) ->
        GatewayPid ! {register, self(), Procedure}
    end, Procedures),

    io:format("Registered ~p diagnostic procedures~n", [length(Procedures)]),

    {noreply, State#state{gateway_pid = GatewayPid}};

handle_cast(_Request, State) ->
    {noreply, State}.

%% Handle RPC invocations from gateway
handle_info({invoke, CallerPid, CallId, Procedure, Args}, State) ->
    Result = handle_procedure(Procedure, Args, State),

    %% Send result back to caller
    CallerPid ! {call_result, CallId, Result},

    {noreply, State};

%% Handle registration confirmations
handle_info({registered, Procedure}, State) ->
    io:format("Diagnostic procedure registered: ~s~n", [Procedure]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
handle_procedure(<<"com.macula.diagnostics.hello">>, _Args, State) ->
    Uptime = erlang:system_time(second) - State#state.started_at,

    #{
        <<"message">> => <<"Hello from Macula Gateway!">>,
        <<"gateway">> => list_to_binary(atom_to_list(node())),
        <<"realm">> => State#state.realm,
        <<"uptime_seconds">> => Uptime,
        <<"timestamp">> => erlang:system_time(second),
        <<"version">> => <<"0.1.0">>,
        <<"protocol">> => <<"HTTP/3 (QUIC)">>
    };

handle_procedure(<<"com.macula.diagnostics.echo">>, Args, _State) ->
    #{
        <<"echo">> => Args,
        <<"timestamp">> => erlang:system_time(second)
    };

handle_procedure(<<"com.macula.diagnostics.info">>, _Args, State) ->
    Uptime = erlang:system_time(second) - State#state.started_at,

    %% Get memory info
    MemoryInfo = erlang:memory(),
    TotalMemory = proplists:get_value(total, MemoryInfo, 0),
    ProcessMemory = proplists:get_value(processes, MemoryInfo, 0),

    %% Get process count
    ProcessCount = erlang:system_info(process_count),

    #{
        <<"gateway">> => #{
            <<"node">> => list_to_binary(atom_to_list(node())),
            <<"realm">> => State#state.realm,
            <<"uptime_seconds">> => Uptime,
            <<"version">> => <<"0.1.0">>
        },
        <<"system">> => #{
            <<"otp_version">> => list_to_binary(erlang:system_info(otp_release)),
            <<"erts_version">> => list_to_binary(erlang:system_info(version)),
            <<"process_count">> => ProcessCount,
            <<"memory_bytes">> => TotalMemory,
            <<"process_memory_bytes">> => ProcessMemory
        },
        <<"timestamp">> => erlang:system_time(second)
    };

handle_procedure(_Unknown, _Args, _State) ->
    #{
        <<"error">> => <<"unknown_procedure">>,
        <<"timestamp">> => erlang:system_time(second)
    }.
