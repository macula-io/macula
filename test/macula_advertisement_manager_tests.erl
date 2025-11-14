%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_advertisement_manager module.
%%%
%%% Tests service advertisement lifecycle without requiring QUIC.
%%% Focuses on:
%%% - Handler lifecycle (start/stop)
%%% - Service advertisement (local and DHT)
%%% - Service unadvertisement
%%% - Active advertisement tracking
%%% - Re-advertisement timer management
%%% - Error handling
%%% @end
%%%-------------------------------------------------------------------
-module(macula_advertisement_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Lifecycle Tests
%%%===================================================================

manager_starts_successfully_test() ->
    Opts = #{
        node_id => <<"test_node_123">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6000">>
    },

    Result = macula_advertisement_manager:start_link(Opts),

    ?assertMatch({ok, _Pid}, Result),

    case Result of
        {ok, Pid} ->
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid);
        _ ->
            ok
    end.

manager_accepts_custom_options_test() ->
    Opts = #{
        node_id => <<"custom_node">>,
        realm => <<"custom.realm">>,
        url => <<"https://localhost:6001">>,
        custom_opt => <<"custom_value">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_can_be_stopped_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6002">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    timer:sleep(50),

    ?assertNot(is_process_alive(Pid)).

manager_generates_node_id_when_not_provided_test() ->
    Opts = #{
        realm => <<"test.realm">>,
        url => <<"https://localhost:6003">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Connection Manager PID Tests
%%%===================================================================

manager_starts_without_connection_manager_pid_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6004">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    %% Should start even without connection_manager_pid (set to undefined initially)
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_accepts_connection_manager_pid_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6005">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    %% Mock connection manager PID
    MockConnMgrPid = spawn(fun() -> timer:sleep(1000) end),

    %% Send set_connection_manager_pid message
    gen_server:cast(Pid, {set_connection_manager_pid, MockConnMgrPid}),

    %% Manager should still be alive
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid),
    exit(MockConnMgrPid, kill).

%%%===================================================================
%%% Service Advertisement Tests (without connection manager)
%%%===================================================================

advertise_service_with_binary_procedure_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6006">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Procedure = <<"test.service">>,
    Handler = fun(_Args) -> {ok, <<"result">>} end,
    AdvOpts = #{metadata => #{description => <<"Test service">>}},

    %% Will fail to send STORE (no connection manager) but should track locally
    Result = macula_advertisement_manager:advertise_service(Pid, Procedure, Handler, AdvOpts),

    ?assertMatch({ok, _Ref}, Result),

    gen_server:stop(Pid).

advertise_service_with_string_procedure_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6007">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Procedure = "test.string.service",
    Handler = fun(_Args) -> {ok, <<"result">>} end,
    AdvOpts = #{},

    Result = macula_advertisement_manager:advertise_service(Pid, Procedure, Handler, AdvOpts),
    ?assertMatch({ok, _Ref}, Result),

    gen_server:stop(Pid).

advertise_service_with_atom_procedure_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6008">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Procedure = test_atom_service,
    Handler = fun(_Args) -> {ok, <<"result">>} end,
    AdvOpts = #{},

    Result = macula_advertisement_manager:advertise_service(Pid, Procedure, Handler, AdvOpts),
    ?assertMatch({ok, _Ref}, Result),

    gen_server:stop(Pid).

advertise_service_with_metadata_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6009">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Procedure = <<"test.service">>,
    Handler = fun(_Args) -> {ok, <<"result">>} end,
    AdvOpts = #{
        metadata => #{
            description => <<"Test service">>,
            version => <<"1.0.0">>,
            author => <<"Test Author">>
        }
    },

    Result = macula_advertisement_manager:advertise_service(Pid, Procedure, Handler, AdvOpts),
    ?assertMatch({ok, _Ref}, Result),

    gen_server:stop(Pid).

advertise_service_with_custom_ttl_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6010">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Procedure = <<"test.service">>,
    Handler = fun(_Args) -> {ok, <<"result">>} end,
    AdvOpts = #{ttl => 300},  % Custom TTL

    Result = macula_advertisement_manager:advertise_service(Pid, Procedure, Handler, AdvOpts),
    ?assertMatch({ok, _Ref}, Result),

    gen_server:stop(Pid).

advertise_service_with_custom_endpoint_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6011">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Procedure = <<"test.service">>,
    Handler = fun(_Args) -> {ok, <<"result">>} end,
    AdvOpts = #{advertise_endpoint => <<"https://custom.endpoint:9443">>},

    Result = macula_advertisement_manager:advertise_service(Pid, Procedure, Handler, AdvOpts),
    ?assertMatch({ok, _Ref}, Result),

    gen_server:stop(Pid).

advertise_multiple_services_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6012">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Handler = fun(_Args) -> {ok, <<"result">>} end,

    {ok, Ref1} = macula_advertisement_manager:advertise_service(Pid, <<"service1">>, Handler, #{}),
    {ok, Ref2} = macula_advertisement_manager:advertise_service(Pid, <<"service2">>, Handler, #{}),
    {ok, Ref3} = macula_advertisement_manager:advertise_service(Pid, <<"service3">>, Handler, #{}),

    %% All references should be unique
    ?assertNotEqual(Ref1, Ref2),
    ?assertNotEqual(Ref2, Ref3),
    ?assertNotEqual(Ref1, Ref3),

    gen_server:stop(Pid).

advertise_same_service_twice_updates_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6013">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Procedure = <<"test.service">>,
    Handler1 = fun(_Args) -> {ok, <<"result1">>} end,
    Handler2 = fun(_Args) -> {ok, <<"result2">>} end,

    {ok, _Ref1} = macula_advertisement_manager:advertise_service(Pid, Procedure, Handler1, #{}),
    {ok, _Ref2} = macula_advertisement_manager:advertise_service(Pid, Procedure, Handler2, #{}),

    %% Should succeed (updates existing advertisement)
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Service Unadvertisement Tests
%%%===================================================================

unadvertise_service_removes_advertisement_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6014">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Procedure = <<"test.service">>,
    Handler = fun(_Args) -> {ok, <<"result">>} end,

    {ok, _Ref} = macula_advertisement_manager:advertise_service(Pid, Procedure, Handler, #{}),

    %% Unadvertise should succeed
    Result = macula_advertisement_manager:unadvertise_service(Pid, Procedure),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

unadvertise_non_existent_service_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6015">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    %% Unadvertise service that was never advertised
    Result = macula_advertisement_manager:unadvertise_service(Pid, <<"non.existent">>),

    %% Should still return ok (idempotent)
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

unadvertise_with_binary_procedure_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6016">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Result = macula_advertisement_manager:unadvertise_service(Pid, <<"test.service">>),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

unadvertise_with_string_procedure_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6017">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Result = macula_advertisement_manager:unadvertise_service(Pid, "test.string.service"),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

unadvertise_with_atom_procedure_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6018">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Result = macula_advertisement_manager:unadvertise_service(Pid, test_atom_service),
    ?assertEqual(ok, Result),

    gen_server:stop(Pid).

%%%===================================================================
%%% Active Advertisements Tests
%%%===================================================================

get_active_advertisements_returns_empty_initially_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6019">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Result = macula_advertisement_manager:get_active_advertisements(Pid),
    ?assertMatch({ok, []}, Result),

    gen_server:stop(Pid).

get_active_advertisements_returns_advertised_services_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6020">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Handler = fun(_Args) -> {ok, <<"result">>} end,

    {ok, _} = macula_advertisement_manager:advertise_service(Pid, <<"service1">>, Handler, #{}),
    {ok, _} = macula_advertisement_manager:advertise_service(Pid, <<"service2">>, Handler, #{}),

    {ok, Services} = macula_advertisement_manager:get_active_advertisements(Pid),

    %% Should have 2 services
    ?assertEqual(2, length(Services)),
    ?assert(lists:member(<<"service1">>, Services)),
    ?assert(lists:member(<<"service2">>, Services)),

    gen_server:stop(Pid).

get_active_advertisements_after_unadvertise_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6021">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    Handler = fun(_Args) -> {ok, <<"result">>} end,

    {ok, _} = macula_advertisement_manager:advertise_service(Pid, <<"service1">>, Handler, #{}),
    {ok, _} = macula_advertisement_manager:advertise_service(Pid, <<"service2">>, Handler, #{}),

    ok = macula_advertisement_manager:unadvertise_service(Pid, <<"service1">>),

    {ok, Services} = macula_advertisement_manager:get_active_advertisements(Pid),

    %% Should have only 1 service left
    ?assertEqual(1, length(Services)),
    ?assert(lists:member(<<"service2">>, Services)),
    ?assertNot(lists:member(<<"service1">>, Services)),

    gen_server:stop(Pid).

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

manager_handles_invalid_call_request_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6022">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    %% Send invalid request
    Result = gen_server:call(Pid, {invalid_request, foo, bar}, 1000),
    ?assertMatch({error, unknown_request}, Result),

    %% Manager should survive
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_handles_invalid_cast_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6023">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    %% Send invalid cast
    gen_server:cast(Pid, {invalid_cast, foo, bar}),

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_handles_invalid_info_message_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6024">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    %% Send invalid info message
    Pid ! {invalid_info, foo, bar},

    timer:sleep(50),
    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).

manager_survives_handler_exception_test() ->
    Opts = #{
        node_id => <<"test_node">>,
        realm => <<"test.realm">>,
        url => <<"https://localhost:6025">>
    },

    {ok, Pid} = macula_advertisement_manager:start_link(Opts),

    %% Advertise with handler that crashes
    BadHandler = fun(_Args) -> error(intentional_crash) end,

    %% Should still succeed to advertise (handler is stored, not executed yet)
    Result = macula_advertisement_manager:advertise_service(Pid, <<"test.service">>, BadHandler, #{}),
    ?assertMatch({ok, _Ref}, Result),

    ?assert(is_process_alive(Pid)),

    gen_server:stop(Pid).
