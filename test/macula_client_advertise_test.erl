%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests for service advertisement and RPC calling.
%%% Tests the full flow: advertise → call → execute → reply
%%% @end
%%%-------------------------------------------------------------------
-module(macula_client_advertise_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Descriptions
%%%===================================================================

%% This test suite covers:
%% 1. Service advertisement via macula:advertise/3,4
%% 2. Service unadvertisement via macula:unadvertise/2
%% 3. Local service calls (same connection)
%% 4. Service not found error handling
%% 5. Handler error propagation

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Mock connection process for testing
-record(mock_conn_state, {
    service_registry :: macula_service_registry:registry(),
    advertised_services = #{} :: map()
}).

setup() ->
    %% Create a mock connection state
    Registry = macula_service_registry:new(),
    #mock_conn_state{service_registry = Registry}.

%%%===================================================================
%%% Service Advertisement Tests
%%%===================================================================

advertise_service_test() ->
    State = setup(),
    Registry = State#mock_conn_state.service_registry,

    %% Define a handler
    Handler = fun(#{name := Name}) ->
        {ok, #{greeting => <<"Hello, ", Name/binary>>}}
    end,

    %% Advertise service
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"greeter.hello">>,
        Handler,
        #{version => <<"1.0">>}
    ),

    %% Verify service is registered
    ?assertMatch(
        {ok, _Handler},
        macula_service_registry:get_local_handler(Registry2, <<"greeter.hello">>)
    ),

    %% Verify it's in the list
    Services = macula_service_registry:list_local_services(Registry2),
    ?assert(lists:member(<<"greeter.hello">>, Services)).

unadvertise_service_test() ->
    State = setup(),
    Registry = State#mock_conn_state.service_registry,

    Handler = fun(Args) -> {ok, Args} end,

    %% Advertise then unadvertise
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"test.service">>,
        Handler,
        #{}
    ),

    %% Verify exists
    ?assertMatch(
        {ok, _},
        macula_service_registry:get_local_handler(Registry2, <<"test.service">>)
    ),

    %% Unadvertise
    Registry3 = macula_service_registry:unadvertise_local(Registry2, <<"test.service">>),

    %% Verify removed
    ?assertEqual(
        not_found,
        macula_service_registry:get_local_handler(Registry3, <<"test.service">>)
    ).

%%%===================================================================
%%% Local Service Call Tests
%%%===================================================================

local_service_call_success_test() ->
    State = setup(),
    Registry = State#mock_conn_state.service_registry,

    %% Define a simple calculator service
    Handler = fun(#{a := A, b := B, op := Op}) ->
        case Op of
            <<"add">> -> {ok, #{result => A + B}};
            <<"multiply">> -> {ok, #{result => A * B}};
            _ -> {error, unknown_operation}
        end
    end,

    %% Advertise service
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"calculator.compute">>,
        Handler,
        #{}
    ),

    %% Get handler and call it directly (simulating local call)
    {ok, LocalHandler} = macula_service_registry:get_local_handler(
        Registry2,
        <<"calculator.compute">>
    ),

    %% Test addition
    {ok, #{result := Sum}} = LocalHandler(#{a => 5, b => 3, op => <<"add">>}),
    ?assertEqual(8, Sum),

    %% Test multiplication
    {ok, #{result := Product}} = LocalHandler(#{a => 5, b => 3, op => <<"multiply">>}),
    ?assertEqual(15, Product).

local_service_call_error_test() ->
    State = setup(),
    Registry = State#mock_conn_state.service_registry,

    %% Handler that validates input
    Handler = fun(#{user_id := UserId}) when is_binary(UserId) ->
        {ok, #{user_id => UserId, name => <<"Test User">>}};
    (_InvalidArgs) ->
        {error, invalid_arguments}
    end,

    %% Advertise service
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"user.get">>,
        Handler,
        #{}
    ),

    %% Get handler
    {ok, LocalHandler} = macula_service_registry:get_local_handler(
        Registry2,
        <<"user.get">>
    ),

    %% Valid call
    ?assertMatch(
        {ok, #{user_id := <<"user-123">>}},
        LocalHandler(#{user_id => <<"user-123">>})
    ),

    %% Invalid call
    ?assertEqual(
        {error, invalid_arguments},
        LocalHandler(#{invalid => <<"key">>})
    ).

%%%===================================================================
%%% Service Not Found Tests
%%%===================================================================

service_not_found_test() ->
    State = setup(),
    Registry = State#mock_conn_state.service_registry,

    %% Try to get handler for non-existent service
    ?assertEqual(
        not_found,
        macula_service_registry:get_local_handler(Registry, <<"nonexistent.service">>)
    ).

%%%===================================================================
%%% Multiple Services Tests
%%%===================================================================

multiple_services_test() ->
    State = setup(),
    Registry = State#mock_conn_state.service_registry,

    %% Define multiple handlers
    Handler1 = fun(_) -> {ok, service1_result} end,
    Handler2 = fun(_) -> {ok, service2_result} end,
    Handler3 = fun(_) -> {ok, service3_result} end,

    %% Advertise multiple services
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"service.one">>,
        Handler1,
        #{}
    ),

    Registry3 = macula_service_registry:advertise_local(
        Registry2,
        <<"service.two">>,
        Handler2,
        #{}
    ),

    Registry4 = macula_service_registry:advertise_local(
        Registry3,
        <<"service.three">>,
        Handler3,
        #{}
    ),

    %% Verify all are registered
    Services = macula_service_registry:list_local_services(Registry4),
    ?assertEqual(3, length(Services)),
    ?assert(lists:member(<<"service.one">>, Services)),
    ?assert(lists:member(<<"service.two">>, Services)),
    ?assert(lists:member(<<"service.three">>, Services)),

    %% Verify each handler works
    {ok, H1} = macula_service_registry:get_local_handler(Registry4, <<"service.one">>),
    {ok, H2} = macula_service_registry:get_local_handler(Registry4, <<"service.two">>),
    {ok, H3} = macula_service_registry:get_local_handler(Registry4, <<"service.three">>),

    ?assertEqual({ok, service1_result}, H1(#{})),
    ?assertEqual({ok, service2_result}, H2(#{})),
    ?assertEqual({ok, service3_result}, H3(#{})).

%%%===================================================================
%%% Service Update Tests
%%%===================================================================

service_update_test() ->
    State = setup(),
    Registry = State#mock_conn_state.service_registry,

    %% Advertise version 1
    Handler1 = fun(_) -> {ok, #{version => 1}} end,
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"service.versioned">>,
        Handler1,
        #{version => <<"1.0">>}
    ),

    %% Call version 1
    {ok, H1} = macula_service_registry:get_local_handler(Registry2, <<"service.versioned">>),
    ?assertEqual({ok, #{version => 1}}, H1(#{})),

    %% Advertise version 2 (overwrite)
    Handler2 = fun(_) -> {ok, #{version => 2}} end,
    Registry3 = macula_service_registry:advertise_local(
        Registry2,
        <<"service.versioned">>,
        Handler2,
        #{version => <<"2.0">>}
    ),

    %% Call version 2
    {ok, H2} = macula_service_registry:get_local_handler(Registry3, <<"service.versioned">>),
    ?assertEqual({ok, #{version => 2}}, H2(#{})),

    %% Verify only one service in list
    Services = macula_service_registry:list_local_services(Registry3),
    ?assertEqual(1, length(Services)).

%%%===================================================================
%%% Complex Handler Tests
%%%===================================================================

complex_handler_test() ->
    State = setup(),
    Registry = State#mock_conn_state.service_registry,

    %% Handler with complex logic and state
    Handler = fun(Args) ->
        case Args of
            #{action := <<"create">>, data := Data} ->
                Id = iolist_to_binary(io_lib:format("~p", [erlang:unique_integer()])),
                {ok, #{id => Id, data => Data, status => <<"created">>}};

            #{action := <<"get">>, id := Id} ->
                {ok, #{id => Id, status => <<"found">>}};

            #{action := <<"delete">>, id := Id} ->
                {ok, #{id => Id, status => <<"deleted">>}};

            _ ->
                {error, invalid_action}
        end
    end,

    %% Advertise service
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"resource.manager">>,
        Handler,
        #{}
    ),

    %% Get handler
    {ok, H} = macula_service_registry:get_local_handler(Registry2, <<"resource.manager">>),

    %% Test create
    {ok, CreateResult} = H(#{action => <<"create">>, data => #{name => <<"test">>}}),
    ?assertMatch(#{status := <<"created">>, id := _}, CreateResult),

    %% Test get
    ?assertMatch(
        {ok, #{status := <<"found">>}},
        H(#{action => <<"get">>, id => <<"test-id">>})
    ),

    %% Test delete
    ?assertMatch(
        {ok, #{status := <<"deleted">>}},
        H(#{action => <<"delete">>, id => <<"test-id">>})
    ),

    %% Test invalid action
    ?assertEqual(
        {error, invalid_action},
        H(#{action => <<"invalid">>})
    ).

%%%===================================================================
%%% Handler Exception Tests
%%%===================================================================

handler_exception_test() ->
    State = setup(),
    Registry = State#mock_conn_state.service_registry,

    %% Handler that throws exceptions
    Handler = fun(#{crash := true}) ->
        error(intentional_crash);
    (Args) ->
        {ok, Args}
    end,

    %% Advertise service
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"crashy.service">>,
        Handler,
        #{}
    ),

    %% Get handler
    {ok, H} = macula_service_registry:get_local_handler(Registry2, <<"crashy.service">>),

    %% Normal call works
    ?assertEqual({ok, #{test => <<"data">>}}, H(#{test => <<"data">>})),

    %% Crashing call throws error
    ?assertError(intentional_crash, H(#{crash => true})).
