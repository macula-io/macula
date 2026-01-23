%%%-------------------------------------------------------------------
%%% @doc
%%% RPC call execution with timeout handling.
%%% Executes local handlers and remote calls via QUIC.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_executor).

%% API
-export([
    execute_local/3,
    execute_remote/5,
    generate_call_id/0
]).

%% Types
-type handler_fn() :: macula_rpc_registry:handler_fn().
-type provider_info() :: macula_rpc_dht:provider_info().
-type address() :: macula_rpc_dht:address().

-type send_fun() :: fun((binary(), map(), address(), pos_integer()) ->
    {ok, term()} | {error, term()}).

-export_type([send_fun/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Execute local handler with timeout.
-spec execute_local(handler_fn(), map(), pos_integer()) -> {ok, term()} | {error, term()}.
execute_local(Handler, Args, Timeout) ->
    Parent = self(),
    Ref = make_ref(),

    %% Spawn worker process
    WorkerPid = spawn(fun() ->
        execute_handler_and_send_result(Handler, Args, Parent, Ref)
    end),

    %% Wait for result or timeout
    receive
        {Ref, Result} ->
            Result
    after Timeout ->
        %% Timeout occurred, kill worker
        exit(WorkerPid, kill),
        {error, timeout}
    end.

%% @private Execute handler and send result to parent
execute_handler_and_send_result(Handler, Args, Parent, Ref) ->
    Result = execute_handler_safe(Handler, Args),
    Parent ! {Ref, Result}.

%% @private Execute handler using catch expression
execute_handler_safe(Handler, Args) ->
    handle_execution_result(catch Handler(Args)).

%% @private Handle execution result
handle_execution_result({'EXIT', {Reason, Stacktrace}}) ->
    {error, {exception, error, Reason, Stacktrace}};
handle_execution_result({'EXIT', Reason}) ->
    {error, {exception, error, Reason, []}};
handle_execution_result(Result) ->
    Result.

%% @doc Execute remote call via QUIC with timeout.
-spec execute_remote(binary(), map(), provider_info(), send_fun(), pos_integer()) ->
    {ok, term()} | {error, term()}.
execute_remote(Uri, Args, Provider, SendFun, Timeout) ->
    Address = maps:get(address, Provider),

    %% Send call to remote provider
    SendFun(Uri, Args, Address, Timeout).

%% @doc Generate unique call ID (16-byte UUID).
-spec generate_call_id() -> binary().
generate_call_id() ->
    %% Generate 128-bit random UUID
    <<A:64, B:64>> = crypto:strong_rand_bytes(16),
    <<A:64, B:64>>.
