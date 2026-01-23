%%%-------------------------------------------------------------------
%%% @doc
%%% Async RPC Module (NATS-style Request/Reply)
%%%
%%% Handles asynchronous RPC operations with callback-based responses:
%%% - Callback management (fun callbacks and pid callbacks)
%%% - Request ID generation and tracking
%%% - Request message building for P2P delivery
%%% - Reply processing and callback invocation
%%% - Timeout handling for async requests
%%%
%%% This module provides stateless helper functions used by macula_rpc_handler.
%%% The actual state (pending_requests map) remains in the handler.
%%%
%%% Extracted from macula_rpc_handler.erl (Dec 2025) to improve testability
%%% and separation of concerns.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_async).

-include_lib("kernel/include/logger.hrl").

%% API - Callback management
-export([
    get_callback/2,
    invoke_callback/3
]).

%% API - Request building
-export([
    build_request_message/5,
    get_local_endpoint/0
]).

%% API - Reply processing
-export([
    extract_result/1,
    calculate_rtt/1
]).

%% Types
-export_type([callback/0]).

-type callback() :: {fun_cb, fun((term()) -> any())} | {pid_cb, pid()}.

%%%===================================================================
%%% Callback Management
%%%===================================================================

%% @doc Extract callback from options, defaulting to pid callback.
%% If opts contains a callback function, use it. Otherwise send to caller pid.
-spec get_callback(map(), pid()) -> callback().
get_callback(Opts, CallerPid) ->
    case maps:get(callback, Opts, undefined) of
        undefined -> {pid_cb, CallerPid};
        Fun when is_function(Fun) -> {fun_cb, Fun}
    end.

%% @doc Invoke async callback with result.
%% For function callbacks, spawns a process to avoid blocking.
%% For pid callbacks, sends a message.
-spec invoke_callback(callback(), binary(), term()) -> ok.
invoke_callback({fun_cb, Fun}, _RequestId, Result) ->
    %% Spawn to avoid blocking the gen_server
    spawn(fun() -> invoke_fun_callback_safe(Fun, Result) end),
    ok;
invoke_callback({pid_cb, Pid}, RequestId, Result) ->
    Pid ! {rpc_reply, RequestId, Result},
    ok.

%% @private Invoke function callback safely
invoke_fun_callback_safe(Fun, Result) ->
    handle_fun_callback_result(catch Fun(Result)).

%% @private Handle function callback result
handle_fun_callback_result({'EXIT', {Error, Stacktrace}}) ->
    ?LOG_ERROR("Async callback crashed: error:~p~n~p", [Error, Stacktrace]);
handle_fun_callback_result({'EXIT', Error}) ->
    ?LOG_ERROR("Async callback crashed: ~p", [Error]);
handle_fun_callback_result(_) ->
    ok.

%%%===================================================================
%%% Request Building
%%%===================================================================

%% @doc Build an RPC_REQUEST message for NATS-style async RPC.
%% Includes from_endpoint so receiver can route reply back directly.
-spec build_request_message(binary(), binary(), binary(), binary(), binary()) -> map().
build_request_message(RequestId, Procedure, EncodedArgs, FromNodeId, Realm) ->
    LocalEndpoint = get_local_endpoint(),
    #{
        type => <<"rpc_request">>,
        request_id => RequestId,
        procedure => Procedure,
        args => EncodedArgs,
        from_node => FromNodeId,
        from_endpoint => LocalEndpoint,
        realm => Realm,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc Get local endpoint from environment variables.
%% Used to include sender's endpoint in RPC requests so receivers can route replies back.
%% Format: "hostname:port" (e.g., "fc01:4433" in Docker)
-spec get_local_endpoint() -> binary().
get_local_endpoint() ->
    Hostname = get_hostname_for_endpoint(),
    Port = get_port_for_endpoint(),
    iolist_to_binary([Hostname, <<":">>, Port]).

%%%===================================================================
%%% Reply Processing
%%%===================================================================

%% @doc Extract result from RPC reply message.
%% Returns {ok, DecodedValue} or {error, ErrorReason}.
-spec extract_result(map()) -> {ok, term()} | {error, term()}.
extract_result(Msg) ->
    case maps:get(<<"result">>, Msg, maps:get(result, Msg, undefined)) of
        undefined ->
            Error = maps:get(<<"error">>, Msg, maps:get(error, Msg, <<"Unknown error">>)),
            {error, Error};
        Value ->
            %% Try to decode JSON result
            DecodedValue = safe_decode_json(Value),
            {ok, DecodedValue}
    end.

%% @private Safely decode JSON, returning original on failure
safe_decode_json(Value) ->
    handle_decode_result(catch macula_utils:decode_json(Value), Value).

%% @private Handle decode result
handle_decode_result({'EXIT', _}, Original) ->
    Original;
handle_decode_result(Decoded, _Original) ->
    Decoded.

%% @doc Calculate RTT from sent_at timestamp.
-spec calculate_rtt(integer()) -> non_neg_integer().
calculate_rtt(SentAt) ->
    erlang:system_time(millisecond) - SentAt.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Get hostname from environment variables.
-spec get_hostname_for_endpoint() -> binary().
get_hostname_for_endpoint() ->
    case os:getenv("MACULA_HOSTNAME") of
        false ->
            case os:getenv("HOSTNAME") of
                false -> <<"localhost">>;
                Hostname -> list_to_binary(Hostname)
            end;
        Hostname -> list_to_binary(Hostname)
    end.

%% @private Get QUIC port from environment or default.
-spec get_port_for_endpoint() -> binary().
get_port_for_endpoint() ->
    case os:getenv("QUIC_PORT") of
        false -> <<"4433">>;  %% Default QUIC port for Docker setup
        Port -> list_to_binary(Port)
    end.
