%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_gateway_rpc module.
%%% Tests RPC handler registration and call routing - Phase 4 of gateway refactoring.
%%%
%%% TDD Approach:
%%% 1. Write failing tests first
%%% 2. Implement minimal functionality
%%% 3. Make tests pass incrementally
%%% 4. Refactor for idiomatic Erlang
%%%
%%% Responsibilities:
%%% - Register/unregister RPC handlers
%%% - Route RPC calls to handlers
%%% - Handle call/response matching
%%% - Track handler lifecycle
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_rpc_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    {ok, Pid} = macula_gateway_rpc:start_link(#{}),
    Pid.

cleanup(Pid) ->
    case erlang:is_process_alive(Pid) of
        true -> macula_gateway_rpc:stop(Pid);
        false -> ok
    end.

%%%===================================================================
%%% Basic API Tests
%%%===================================================================

module_exports_test() ->
    Exports = macula_gateway_rpc:module_info(exports),

    ?assert(lists:member({start_link, 1}, Exports)),
    ?assert(lists:member({stop, 1}, Exports)),
    ?assert(lists:member({register_handler, 3}, Exports)),
    ?assert(lists:member({unregister_handler, 2}, Exports)),
    ?assert(lists:member({call, 4}, Exports)),
    ?assert(lists:member({get_handler, 2}, Exports)),
    ?assert(lists:member({list_handlers, 1}, Exports)).

gen_server_callbacks_test() ->
    Exports = macula_gateway_rpc:module_info(exports),

    ?assert(lists:member({init, 1}, Exports)),
    ?assert(lists:member({handle_call, 3}, Exports)),
    ?assert(lists:member({handle_cast, 2}, Exports)),
    ?assert(lists:member({handle_info, 2}, Exports)),
    ?assert(lists:member({terminate, 2}, Exports)).

%%%===================================================================
%%% Startup/Shutdown Tests
%%%===================================================================

start_link_test() ->
    {ok, Pid} = macula_gateway_rpc:start_link(#{}),
    ?assert(erlang:is_process_alive(Pid)),
    macula_gateway_rpc:stop(Pid).

stop_test() ->
    {ok, Pid} = macula_gateway_rpc:start_link(#{}),
    ok = macula_gateway_rpc:stop(Pid),
    timer:sleep(50),
    ?assertNot(erlang:is_process_alive(Pid)).

%%%===================================================================
%%% Handler Registration Tests
%%%===================================================================

register_handler_stores_mapping_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Handler = spawn(fun() -> timer:sleep(1000) end),
        Procedure = <<"test.procedure">>,

        ok = macula_gateway_rpc:register_handler(Pid, Procedure, Handler),

        {ok, StoredHandler} = macula_gateway_rpc:get_handler(Pid, Procedure),
        [?_assertEqual(Handler, StoredHandler)]
     end}.

register_handler_monitors_process_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Handler = spawn(fun() -> timer:sleep(100) end),
        Procedure = <<"test.procedure">>,

        ok = macula_gateway_rpc:register_handler(Pid, Procedure, Handler),

        %% Handler should be registered
        {ok, _} = macula_gateway_rpc:get_handler(Pid, Procedure),

        %% Kill handler
        exit(Handler, kill),
        timer:sleep(150),

        %% Handler should be auto-removed
        [?_assertEqual(not_found, macula_gateway_rpc:get_handler(Pid, Procedure))]
     end}.

register_handler_overwrites_existing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Handler1 = spawn(fun() -> timer:sleep(1000) end),
        Handler2 = spawn(fun() -> timer:sleep(1000) end),
        Procedure = <<"test.procedure">>,

        ok = macula_gateway_rpc:register_handler(Pid, Procedure, Handler1),
        ok = macula_gateway_rpc:register_handler(Pid, Procedure, Handler2),

        {ok, StoredHandler} = macula_gateway_rpc:get_handler(Pid, Procedure),
        [?_assertEqual(Handler2, StoredHandler)]
     end}.

%%%===================================================================
%%% Handler Unregistration Tests
%%%===================================================================

unregister_handler_removes_mapping_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Handler = spawn(fun() -> timer:sleep(1000) end),
        Procedure = <<"test.procedure">>,

        macula_gateway_rpc:register_handler(Pid, Procedure, Handler),
        ok = macula_gateway_rpc:unregister_handler(Pid, Procedure),

        [?_assertEqual(not_found, macula_gateway_rpc:get_handler(Pid, Procedure))]
     end}.

unregister_handler_idempotent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Procedure = <<"test.procedure">>,

        ok = macula_gateway_rpc:unregister_handler(Pid, Procedure),
        ok = macula_gateway_rpc:unregister_handler(Pid, Procedure),

        [?_assertEqual(ok, ok)] % Should not crash
     end}.

%%%===================================================================
%%% RPC Call Tests
%%%===================================================================

call_routes_to_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        %% Verify handler receives the call message
        Parent = self(),
        Handler = spawn(fun() ->
            receive
                {rpc_call, Procedure, Args, _From} ->
                    Parent ! {handler_received, Procedure, Args, ok}
            after 1000 ->
                Parent ! handler_timeout
            end
        end),

        Procedure = <<"test.echo">>,
        Args = #{input => <<"hello">>},

        macula_gateway_rpc:register_handler(Pid, Procedure, Handler),

        %% Make call (will timeout since handler doesn't reply, but that's ok for this test)
        spawn(fun() ->
            catch macula_gateway_rpc:call(Pid, Procedure, Args, #{timeout => 100})
        end),

        %% Wait for handler to receive message
        receive
            {handler_received, RecvProc, RecvArgs, ok} ->
                [
                    ?_assertEqual(Procedure, RecvProc),
                    ?_assertEqual(Args, RecvArgs)
                ]
        after 2000 ->
            [?_assert(false)] % Timeout
        end
     end}.

call_with_no_handler_returns_error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Procedure = <<"nonexistent.procedure">>,
        Args = #{input => <<"test">>},

        Result = macula_gateway_rpc:call(Pid, Procedure, Args, #{}),
        [?_assertEqual({error, no_handler}, Result)]
     end}.

call_timeout_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        %% Create handler that never responds
        Handler = spawn(fun() ->
            receive
                {rpc_call, _Procedure, _Args, _From} ->
                    timer:sleep(10000) % Never respond
            end
        end),

        Procedure = <<"test.slow">>,
        Args = #{input => <<"test">>},

        macula_gateway_rpc:register_handler(Pid, Procedure, Handler),

        %% Call with short timeout - will timeout at gen_server level
        Result = (catch macula_gateway_rpc:call(Pid, Procedure, Args, #{timeout => 100})),
        [?_assertMatch({'EXIT', {timeout, _}}, Result)]
     end}.

%%%===================================================================
%%% Query Tests
%%%===================================================================

get_handler_not_found_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        [?_assertEqual(not_found, macula_gateway_rpc:get_handler(Pid, <<"unknown">>))]
     end}.

list_handlers_empty_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        {ok, Handlers} = macula_gateway_rpc:list_handlers(Pid),
        [?_assertEqual([], Handlers)]
     end}.

list_handlers_multiple_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Handler1 = spawn(fun() -> timer:sleep(1000) end),
        Handler2 = spawn(fun() -> timer:sleep(1000) end),
        Proc1 = <<"proc1">>,
        Proc2 = <<"proc2">>,

        macula_gateway_rpc:register_handler(Pid, Proc1, Handler1),
        macula_gateway_rpc:register_handler(Pid, Proc2, Handler2),

        {ok, Handlers} = macula_gateway_rpc:list_handlers(Pid),
        [
            ?_assertEqual(2, length(Handlers)),
            ?_assert(lists:keymember(Proc1, 1, Handlers)),
            ?_assert(lists:keymember(Proc2, 1, Handlers))
        ]
     end}.

%%%===================================================================
%%% Edge Cases
%%%===================================================================

concurrent_registrations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        %% Register many handlers concurrently
        Handlers = [{<<"proc", (integer_to_binary(I))/binary>>,
                     spawn(fun() -> timer:sleep(1000) end)}
                    || I <- lists:seq(1, 20)],

        [macula_gateway_rpc:register_handler(Pid, Proc, H) || {Proc, H} <- Handlers],

        {ok, Registered} = macula_gateway_rpc:list_handlers(Pid),
        [?_assertEqual(20, length(Registered))]
     end}.

handler_crash_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        Handler = spawn(fun() -> timer:sleep(50) end),
        Procedure = <<"test.crasher">>,

        macula_gateway_rpc:register_handler(Pid, Procedure, Handler),

        %% Kill handler brutally
        exit(Handler, kill),
        timer:sleep(100),

        %% Should be cleaned up
        {ok, Handlers} = macula_gateway_rpc:list_handlers(Pid),
        [?_assertEqual([], Handlers)]
     end}.

multiple_calls_to_same_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
        %% Just verify that handler stays registered after multiple calls
        %% Handler should stay alive and handle all messages
        Handler = spawn(fun Loop() ->
            receive
                {rpc_call, _, _, _} -> Loop();  % Keep looping to handle multiple calls
                stop -> ok
            after 10000 -> ok
            end
        end),

        Procedure = <<"test.multi">>,
        macula_gateway_rpc:register_handler(Pid, Procedure, Handler),

        %% Make multiple calls (they'll timeout but that's ok)
        [spawn(fun() ->
            catch macula_gateway_rpc:call(Pid, Procedure, #{id => I}, #{timeout => 50})
        end) || I <- lists:seq(1, 5)],

        timer:sleep(200),

        %% Verify handler still registered
        Result = case macula_gateway_rpc:get_handler(Pid, Procedure) of
            {ok, StillRegistered} ->
                Handler ! stop,  % Clean up handler
                [?_assertEqual(Handler, StillRegistered)];
            not_found ->
                Handler ! stop,  % Clean up handler
                [?_assert(false)]  % Fail if handler was unregistered
        end,
        Result
     end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================
%% (None currently needed)
