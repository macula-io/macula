#!/usr/bin/env escript
%%! -pa ../_build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%%% @doc
%%% Service Advertisement Demo
%%%
%%% Demonstrates how to:
%%% 1. Connect to Macula mesh
%%% 2. Advertise a service with a handler
%%% 3. Call the service (local-first optimization)
%%% 4. Handle service errors
%%% 5. Unadvertise a service
%%%
%%% Usage:
%%%   ./examples/service_advertisement_demo.erl
%%%
%%% @end
%%%-------------------------------------------------------------------

main(_Args) ->
    io:format("~n=== Macula Service Advertisement Demo ===~n~n"),

    %% Ensure application is started
    application:ensure_all_started(macula),

    io:format("Step 1: Connecting to Macula mesh...~n"),
    {ok, Client} = macula_client:connect(
        <<"https://localhost:9443">>,
        #{
            realm => <<"com.example.demo">>,
            node_id => <<"demo-node-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>
        }
    ),
    io:format("  ✓ Connected! Client PID: ~p~n~n", [Client]),

    io:format("Step 2: Advertising a 'calculator' service...~n"),
    CalculatorHandler = fun(Args) ->
        case Args of
            #{operation := <<"add">>, a := A, b := B} ->
                Result = A + B,
                io:format("  [Handler] Computing: ~p + ~p = ~p~n", [A, B, Result]),
                {ok, #{result => Result, operation => <<"add">>}};

            #{operation := <<"subtract">>, a := A, b := B} ->
                Result = A - B,
                io:format("  [Handler] Computing: ~p - ~p = ~p~n", [A, B, Result]),
                {ok, #{result => Result, operation => <<"subtract">>}};

            #{operation := <<"multiply">>, a := A, b := B} ->
                Result = A * B,
                io:format("  [Handler] Computing: ~p * ~p = ~p~n", [A, B, Result]),
                {ok, #{result => Result, operation => <<"multiply">>}};

            #{operation := <<"divide">>, a := A, b := B} when B =/= 0 ->
                Result = A / B,
                io:format("  [Handler] Computing: ~p / ~p = ~p~n", [A, B, Result]),
                {ok, #{result => Result, operation => <<"divide">>}};

            #{operation := <<"divide">>, b := 0} ->
                io:format("  [Handler] Error: Division by zero~n"),
                {error, division_by_zero};

            _ ->
                io:format("  [Handler] Error: Invalid operation~n"),
                {error, invalid_operation}
        end
    end,

    {ok, ServiceRef} = macula_client:advertise(
        Client,
        <<"com.example.calculator">>,
        CalculatorHandler,
        #{
            metadata => #{
                version => <<"1.0.0">>,
                description => <<"Simple calculator service">>
            },
            ttl => 300
        }
    ),
    io:format("  ✓ Service advertised! Reference: ~p~n~n", [ServiceRef]),

    io:format("Step 3: Calling the calculator service (local call - no network overhead)...~n"),

    %% Addition
    io:format("  → Calling: add(10, 5)~n"),
    {ok, AddResult} = macula_client:call(
        Client,
        <<"com.example.calculator">>,
        #{operation => <<"add">>, a => 10, b => 5}
    ),
    io:format("  ✓ Result: ~p~n", [AddResult]),

    timer:sleep(100),

    %% Subtraction
    io:format("  → Calling: subtract(20, 8)~n"),
    {ok, SubResult} = macula_client:call(
        Client,
        <<"com.example.calculator">>,
        #{operation => <<"subtract">>, a => 20, b => 8}
    ),
    io:format("  ✓ Result: ~p~n", [SubResult]),

    timer:sleep(100),

    %% Multiplication
    io:format("  → Calling: multiply(6, 7)~n"),
    {ok, MulResult} = macula_client:call(
        Client,
        <<"com.example.calculator">>,
        #{operation => <<"multiply">>, a => 6, b => 7}
    ),
    io:format("  ✓ Result: ~p~n", [MulResult]),

    timer:sleep(100),

    %% Division
    io:format("  → Calling: divide(100, 4)~n"),
    {ok, DivResult} = macula_client:call(
        Client,
        <<"com.example.calculator">>,
        #{operation => <<"divide">>, a => 100, b => 4}
    ),
    io:format("  ✓ Result: ~p~n~n", [DivResult]),

    timer:sleep(100),

    io:format("Step 4: Testing error handling...~n"),

    %% Division by zero
    io:format("  → Calling: divide(10, 0) [should fail]~n"),
    {error, division_by_zero} = macula_client:call(
        Client,
        <<"com.example.calculator">>,
        #{operation => <<"divide">>, a => 10, b => 0}
    ),
    io:format("  ✓ Correctly handled error: division_by_zero~n~n"),

    timer:sleep(100),

    io:format("Step 5: Advertising a second service (user management)...~n"),
    UserHandler = fun(Args) ->
        case Args of
            #{action := <<"get">>, user_id := UserId} ->
                io:format("  [Handler] Fetching user: ~s~n", [UserId]),
                {ok, #{
                    user_id => UserId,
                    name => <<"Alice Smith">>,
                    email => <<"alice@example.com">>,
                    role => <<"admin">>
                }};

            #{action := <<"create">>, name := Name, email := Email} ->
                UserId = iolist_to_binary(io_lib:format("user-~p", [erlang:unique_integer([positive])])),
                io:format("  [Handler] Creating user: ~s (~s)~n", [Name, Email]),
                {ok, #{
                    user_id => UserId,
                    name => Name,
                    email => Email,
                    role => <<"user">>,
                    created_at => erlang:system_time(second)
                }};

            #{action := <<"delete">>, user_id := UserId} ->
                io:format("  [Handler] Deleting user: ~s~n", [UserId]),
                {ok, #{user_id => UserId, status => <<"deleted">>}};

            _ ->
                {error, invalid_action}
        end
    end,

    {ok, UserServiceRef} = macula_client:advertise(
        Client,
        <<"com.example.user_service">>,
        UserHandler,
        #{metadata => #{version => <<"2.0.0">>}}
    ),
    io:format("  ✓ User service advertised! Reference: ~p~n~n", [UserServiceRef]),

    io:format("Step 6: Calling the user service...~n"),

    %% Get user
    io:format("  → Calling: get_user('user-123')~n"),
    {ok, GetUserResult} = macula_client:call(
        Client,
        <<"com.example.user_service">>,
        #{action => <<"get">>, user_id => <<"user-123">>}
    ),
    io:format("  ✓ Result: ~p~n", [GetUserResult]),

    timer:sleep(100),

    %% Create user
    io:format("  → Calling: create_user('Bob Jones', 'bob@example.com')~n"),
    {ok, CreateUserResult} = macula_client:call(
        Client,
        <<"com.example.user_service">>,
        #{action => <<"create">>, name => <<"Bob Jones">>, email => <<"bob@example.com">>}
    ),
    io:format("  ✓ Result: ~p~n~n", [CreateUserResult]),

    timer:sleep(100),

    io:format("Step 7: Unadvertising services...~n"),
    ok = macula_client:unadvertise(Client, <<"com.example.calculator">>),
    io:format("  ✓ Calculator service unadvertised~n"),

    ok = macula_client:unadvertise(Client, <<"com.example.user_service">>),
    io:format("  ✓ User service unadvertised~n~n"),

    io:format("Step 8: Verifying services are no longer available...~n"),
    io:format("  → Trying to call calculator (should fail)~n"),
    %% Note: This will currently fall back to direct call since DHT is not implemented
    %% In the future, this should return {error, service_not_found}
    io:format("  ⚠ Note: Without DHT, this falls back to direct call~n~n"),

    io:format("Step 9: Disconnecting...~n"),
    ok = macula_client:disconnect(Client),
    io:format("  ✓ Disconnected~n~n"),

    io:format("=== Demo Complete ===~n~n"),

    io:format("Summary:~n"),
    io:format("  ✓ Connected to Macula mesh~n"),
    io:format("  ✓ Advertised 2 services with custom handlers~n"),
    io:format("  ✓ Called services locally (zero network overhead)~n"),
    io:format("  ✓ Handled errors gracefully~n"),
    io:format("  ✓ Unadvertised services~n"),
    io:format("  ✓ Disconnected cleanly~n~n"),

    io:format("Key Features Demonstrated:~n"),
    io:format("  • Local-first optimization (no network for local services)~n"),
    io:format("  • Multiple services on same connection~n"),
    io:format("  • Rich handler logic with pattern matching~n"),
    io:format("  • Error handling and propagation~n"),
    io:format("  • Service lifecycle management~n~n"),

    io:format("Next Steps:~n"),
    io:format("  • Implement DHT integration for distributed discovery~n"),
    io:format("  • Add multi-endpoint RPC (call services on other nodes)~n"),
    io:format("  • Add provider selection strategies~n"),
    io:format("  • Add periodic re-advertisement~n~n"),

    halt(0).
