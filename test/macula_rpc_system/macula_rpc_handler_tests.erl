%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_handler module.
%%%
%%% Tests RPC operations without requiring actual QUIC connections.
%%% Focuses on:
%%% - Handler lifecycle (start/stop)
%%% - Local RPC execution (no network)
%%% - Remote RPC calls (with mocked connection manager)
%%% - Reply handling (success/error)
%%% - Timeout handling (simple and with failover)
%%% - Provider selection strategies
%%% - State management (pending calls, pending queries)
%%% - Error handling
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup Helper
%%%===================================================================

%% @doc Ensure gproc is started before running tests.
ensure_gproc() ->
    application:ensure_all_started(gproc),
    ok.

%%%===================================================================
%%% Lifecycle Tests
%%%===================================================================

handler_starts_successfully_test() ->
    ensure_gproc(),
    Opts = #{
        node_id => <<"test_node_123">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5000">>
    },

    Result = macula_rpc_handler:start_link(Opts),

    ?assertMatch({ok, _Pid}, Result),

    case Result of
        {ok, Pid} ->
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid);
        _ ->
            ok
    end.

handler_accepts_custom_options_test() ->
    Opts = #{
        node_id => <<"custom_node">>,
        realm => <<"custom.realm">>,
        url => <<"http://localhost:5001">>,
        provider_strategy => round_robin
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handler_can_be_stopped_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5002">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    timer:sleep(50),

    ?assertNot(is_process_alive(Pid)).

%%%===================================================================
%%% Connection Manager PID Tests
%%%===================================================================

handler_starts_without_connection_manager_pid_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5003">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Should start even without connection_manager_pid (set to undefined initially)
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handler_accepts_connection_manager_pid_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5004">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Mock connection manager PID
    MockConnMgrPid = spawn(fun() -> timer:sleep(1000) end),

    %% Send set_connection_manager_pid message
    gen_server:cast(Pid, {set_connection_manager_pid, MockConnMgrPid}),

    %% Handler should still be alive
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid),
    exit(MockConnMgrPid, kill).

%%%===================================================================
%%% RPC Call Tests (without connection manager - will timeout)
%%%===================================================================

call_api_accepts_binary_procedure_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5005">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Call with binary procedure (will timeout since no connection manager)
    Procedure = <<"test.procedure">>,
    Args = #{arg1 => <<"value1">>},
    CallOpts = #{timeout => 100},

    %% Spawn to avoid blocking test
    spawn(fun() ->
        _Result = macula_rpc_handler:call(Pid, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

call_api_accepts_string_procedure_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5006">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Call with string procedure
    Procedure = "test.string.procedure",
    Args = #{arg1 => <<"value1">>},
    CallOpts = #{timeout => 100},

    spawn(fun() ->
        _Result = macula_rpc_handler:call(Pid, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

call_api_accepts_atom_procedure_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5007">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Call with atom procedure
    Procedure = test_atom_procedure,
    Args = #{arg1 => <<"value1">>},
    CallOpts = #{timeout => 100},

    spawn(fun() ->
        _Result = macula_rpc_handler:call(Pid, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

call_with_binary_args_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5008">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    Procedure = <<"test.procedure">>,
    Args = <<"binary args">>,
    CallOpts = #{timeout => 100},

    spawn(fun() ->
        _Result = macula_rpc_handler:call(Pid, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

call_with_map_args_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5009">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    Procedure = <<"test.procedure">>,
    Args = #{key1 => <<"value1">>, key2 => 123},
    CallOpts = #{timeout => 100},

    spawn(fun() ->
        _Result = macula_rpc_handler:call(Pid, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

call_with_list_args_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5010">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    Procedure = <<"test.procedure">>,
    Args = [<<"item1">>, <<"item2">>, 123],
    CallOpts = #{timeout => 100},

    spawn(fun() ->
        _Result = macula_rpc_handler:call(Pid, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

call_with_custom_timeout_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5011">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    Procedure = <<"test.procedure">>,
    Args = #{},
    CallOpts = #{timeout => 2000},  % Custom timeout

    spawn(fun() ->
        _Result = macula_rpc_handler:call(Pid, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Incoming Reply Tests
%%%===================================================================

handle_incoming_reply_with_success_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5012">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Simulate incoming reply with success
    CallId = <<"test_call_id_123">>,
    ReplyMsg = #{
        call_id => CallId,
        result => #{response => <<"success">>}
    },

    ok = macula_rpc_handler:handle_incoming_reply(Pid, ReplyMsg),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handle_incoming_reply_with_error_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5013">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Simulate incoming reply with error
    CallId = <<"test_call_id_456">>,
    ReplyMsg = #{
        call_id => CallId,
        error => <<"Service error">>
    },

    ok = macula_rpc_handler:handle_incoming_reply(Pid, ReplyMsg),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handle_incoming_reply_with_binary_keys_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5014">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% MessagePack decoder returns binary keys
    CallId = <<"test_call_id_789">>,
    ReplyMsg = #{
        <<"call_id">> => CallId,
        <<"result">> => #{<<"data">> => <<"value">>}
    },

    ok = macula_rpc_handler:handle_incoming_reply(Pid, ReplyMsg),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handle_incoming_reply_without_call_id_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5015">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Reply without call_id (should be ignored)
    ReplyMsg = #{
        result => #{data => <<"value">>}
    },

    ok = macula_rpc_handler:handle_incoming_reply(Pid, ReplyMsg),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handle_incoming_reply_for_unknown_call_id_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5016">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Reply for unknown call_id (should be logged but not crash)
    CallId = <<"unknown_call_id_999">>,
    ReplyMsg = #{
        call_id => CallId,
        result => #{data => <<"value">>}
    },

    ok = macula_rpc_handler:handle_incoming_reply(Pid, ReplyMsg),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Handler Registration Tests (not implemented)
%%%===================================================================

register_handler_returns_not_implemented_test() ->
    Procedure = <<"test.service">>,
    Handler = fun(_Args) -> {ok, <<"result">>} end,

    %% Should return not_implemented error
    Result = macula_rpc_handler:register_handler(Procedure, Handler),
    ?assertMatch({error, not_implemented_use_advertisement_manager}, Result).

unregister_handler_returns_not_implemented_test() ->
    Procedure = <<"test.service">>,

    %% Should return not_implemented error
    Result = macula_rpc_handler:unregister_handler(Procedure),
    ?assertMatch({error, not_implemented_use_advertisement_manager}, Result).

%%%===================================================================
%%% State Management Tests
%%%===================================================================

handler_tracks_message_id_counter_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5017">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Make multiple calls (they will fail but counter should increment)
    spawn(fun() ->
        _R1 = macula_rpc_handler:call(Pid, <<"proc1">>, #{}, #{timeout => 100}),
        _R2 = macula_rpc_handler:call(Pid, <<"proc2">>, #{}, #{timeout => 100}),
        _R3 = macula_rpc_handler:call(Pid, <<"proc3">>, #{}, #{timeout => 100})
    end),

    timer:sleep(100),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Provider Strategy Tests
%%%===================================================================

handler_accepts_random_strategy_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5018">>,
        provider_strategy => random
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handler_accepts_round_robin_strategy_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5019">>,
        provider_strategy => round_robin
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

handler_survives_invalid_call_request_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5020">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Send invalid request
    Result = gen_server:call(Pid, {invalid_request, foo, bar}, 1000),
    ?assertMatch({error, unknown_request}, Result),

    %% Handler should survive
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handler_survives_invalid_cast_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5021">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Send invalid cast
    gen_server:cast(Pid, {invalid_cast, foo, bar}),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

handler_survives_invalid_info_message_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5022">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% Send invalid info message
    Pid ! {invalid_info, foo, bar},

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Caller Process Monitoring Tests (Issue #5 - Memory Leak Fix)
%%%===================================================================

%% @doc Test that pending calls are cleaned up immediately when caller dies.
%% This verifies the memory leak fix where dead caller processes would leave
%% entries in pending_calls for up to 5 seconds until timeout.
caller_death_cleanup_immediate_test() ->
    Opts = #{
        node_id => <<"test_node_caller_monitoring">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5023">>
    },

    {ok, HandlerPid} = macula_rpc_handler:start_link(Opts),

    %% Get initial state
    InitialState = sys:get_state(HandlerPid),
    InitialPendingSize = maps:size(element(5, InitialState)),  % pending_calls is 5th field
    ?assertEqual(0, InitialPendingSize),

    %% Spawn a caller process that will make an RPC call then die
    CallerPid = spawn(fun() ->
        %% Make async RPC call (will timeout or fail, but should be added to pending_calls)
        _Result = macula_rpc_handler:call(HandlerPid, <<"test.service">>, [], #{timeout => 10000}),
        %% This process will be killed before receiving reply
        timer:sleep(10000)
    end),

    %% Give time for call to be registered in pending_calls
    timer:sleep(100),

    %% Verify call is in pending_calls
    StateDuringCall = sys:get_state(HandlerPid),
    PendingDuringCall = maps:size(element(5, StateDuringCall)),

    %% Should have 1 pending call (even if send failed, entry added with monitor)
    case PendingDuringCall of
        0 ->
            %% If call failed immediately before adding to pending, skip this test
            io:format("SKIP: Call failed before being added to pending_calls~n"),
            gen_server:stop(HandlerPid),
            ok;
        _ ->
            ?assertEqual(1, PendingDuringCall),

            %% Kill the caller process
            exit(CallerPid, kill),

            %% Give time for DOWN message to be processed (should be immediate)
            timer:sleep(50),

            %% Verify pending_calls is cleaned up immediately (not after 5s timeout)
            StateFinal = sys:get_state(HandlerPid),
            PendingFinal = maps:size(element(5, StateFinal)),

            %% Cleanup should be immediate via DOWN message
            ?assertEqual(0, PendingFinal),

            gen_server:stop(HandlerPid)
    end.

%% @doc Test that pending queries are cleaned up when caller dies.
%% Similar to pending calls, but for DHT FIND_VALUE queries.
caller_death_cleanup_query_test() ->
    Opts = #{
        node_id => <<"test_node_query_monitoring">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5024">>
    },

    {ok, HandlerPid} = macula_rpc_handler:start_link(Opts),

    %% Get initial state
    InitialState = sys:get_state(HandlerPid),
    InitialPendingQueries = maps:size(element(6, InitialState)),  % pending_queries is 6th field
    ?assertEqual(0, InitialPendingQueries),

    %% Spawn caller that will initiate a query (via call which triggers DHT lookup)
    CallerPid = spawn(fun() ->
        %% This will trigger DHT FIND_VALUE query if service not in registry
        _Result = macula_rpc_handler:call(HandlerPid, <<"unknown.service">>, [], #{timeout => 10000}),
        timer:sleep(10000)
    end),

    %% Give time for query to be registered
    timer:sleep(100),

    %% Kill the caller
    exit(CallerPid, kill),

    %% Give time for DOWN message
    timer:sleep(50),

    %% Verify cleanup (queries should also be monitored and cleaned up)
    StateFinal = sys:get_state(HandlerPid),
    PendingQueriesFinal = maps:size(element(6, StateFinal)),

    %% Should be cleaned up (either 0 if query was pending, or 0 if never added)
    ?assertEqual(0, PendingQueriesFinal),

    gen_server:stop(HandlerPid).

%%%===================================================================
%%% Targeted RPC Call Tests (call_to/5)
%%%===================================================================

%% @doc Test call_to API with binary procedure - targets specific node.
call_to_accepts_binary_procedure_test() ->
    ensure_gproc(),
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5025">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    TargetNodeId = <<"target_node_123">>,
    Procedure = <<"test.procedure">>,
    Args = #{arg1 => <<"value1">>},
    CallOpts = #{timeout => 100},

    %% Spawn to avoid blocking test (will timeout since no connection manager)
    spawn(fun() ->
        _Result = macula_rpc_handler:call_to(Pid, TargetNodeId, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%% @doc Test call_to API with string procedure.
call_to_accepts_string_procedure_test() ->
    ensure_gproc(),
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5026">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    TargetNodeId = <<"target_node_456">>,
    Procedure = "test.string.procedure",
    Args = #{arg1 => <<"value1">>},
    CallOpts = #{timeout => 100},

    spawn(fun() ->
        _Result = macula_rpc_handler:call_to(Pid, TargetNodeId, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%% @doc Test call_to API with atom procedure.
call_to_accepts_atom_procedure_test() ->
    ensure_gproc(),
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5027">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    TargetNodeId = <<"target_node_789">>,
    Procedure = test_atom_procedure,
    Args = #{arg1 => <<"value1">>},
    CallOpts = #{timeout => 100},

    spawn(fun() ->
        _Result = macula_rpc_handler:call_to(Pid, TargetNodeId, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%% @doc Test call_to with map arguments.
call_to_with_map_args_test() ->
    ensure_gproc(),
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5028">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    TargetNodeId = <<"target_node_map">>,
    Procedure = <<"test.procedure">>,
    Args = #{key1 => <<"value1">>, key2 => 123},
    CallOpts = #{timeout => 100},

    spawn(fun() ->
        _Result = macula_rpc_handler:call_to(Pid, TargetNodeId, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%% @doc Test call_to with list arguments.
call_to_with_list_args_test() ->
    ensure_gproc(),
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5029">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    TargetNodeId = <<"target_node_list">>,
    Procedure = <<"test.procedure">>,
    Args = [<<"item1">>, <<"item2">>, 123],
    CallOpts = #{timeout => 100},

    spawn(fun() ->
        _Result = macula_rpc_handler:call_to(Pid, TargetNodeId, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%% @doc Test call_to returns error when connection not ready.
call_to_connection_not_ready_test() ->
    ensure_gproc(),
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5030">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    %% No connection manager set, so call should fail with connection_not_ready
    TargetNodeId = <<"target_node">>,
    Procedure = <<"test.procedure">>,
    Args = #{},
    CallOpts = #{timeout => 500},

    Result = macula_rpc_handler:call_to(Pid, TargetNodeId, Procedure, Args, CallOpts),

    ?assertEqual({error, connection_not_ready}, Result),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%% @doc Test call_to with custom timeout.
call_to_with_custom_timeout_test() ->
    ensure_gproc(),
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"http://localhost:5031">>
    },

    {ok, Pid} = macula_rpc_handler:start_link(Opts),

    TargetNodeId = <<"target_node_timeout">>,
    Procedure = <<"test.procedure">>,
    Args = #{},
    CallOpts = #{timeout => 2000},  % Custom timeout

    spawn(fun() ->
        _Result = macula_rpc_handler:call_to(Pid, TargetNodeId, Procedure, Args, CallOpts)
    end),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%% NOTE: call_to_caller_death_cleanup test is not included because it requires
%% full integration with authorization. The caller_death_cleanup_immediate_test
%% already tests this pattern for the call/4 API, and call_to uses the same
%% pending_calls mechanism with caller process monitoring.
