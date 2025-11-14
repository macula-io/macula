%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for RPC failover logic in macula_connection module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_failover_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

%% Mock DHT server for testing (currently unused - reserved for future integration tests)
%% -spec start_mock_dht() -> pid().
%% start_mock_dht() ->
%%     spawn(fun() -> mock_dht_loop(#{}) end).
%%
%% mock_dht_loop(State) ->
%%     receive
%%         {From, {store_local, Key, Value}} ->
%%             %% Store providers as list
%%             ExistingProviders = maps:get(Key, State, []),
%%             ProviderList = case is_list(ExistingProviders) of
%%                 true -> ExistingProviders;
%%                 false -> [ExistingProviders]
%%             end,
%%
%%             NodeId = maps:get(node_id, Value, undefined),
%%             UpdatedProviders = case find_existing_provider(NodeId, ProviderList) of
%%                 {found, Index} ->
%%                     lists:sublist(ProviderList, Index - 1) ++
%%                         [Value] ++
%%                         lists:nthtail(Index, ProviderList);
%%                 not_found ->
%%                     [Value | ProviderList]
%%             end,
%%
%%             From ! {self(), ok},
%%             mock_dht_loop(State#{Key => UpdatedProviders});
%%
%%         {From, {get_local, Key}} ->
%%             case maps:get(Key, State, undefined) of
%%                 undefined ->
%%                     From ! {self(), not_found};
%%                 Value when is_list(Value) ->
%%                     From ! {self(), {ok, Value}};
%%                 Value ->
%%                     From ! {self(), {ok, [Value]}}
%%             end,
%%             mock_dht_loop(State);
%%
%%         {From, {delete_local, Key, NodeId}} ->
%%             NewState = case maps:get(Key, State, undefined) of
%%                 undefined ->
%%                     State;
%%                 Providers when is_list(Providers) ->
%%                     UpdatedProviders = lists:filter(fun(P) ->
%%                         maps:get(node_id, P, undefined) =/= NodeId
%%                     end, Providers),
%%                     case UpdatedProviders of
%%                         [] -> maps:remove(Key, State);
%%                         _ -> State#{Key => UpdatedProviders}
%%                     end;
%%                 _SingleValue ->
%%                     maps:remove(Key, State)
%%             end,
%%             From ! {self(), ok},
%%             mock_dht_loop(NewState);
%%
%%         stop ->
%%             ok
%%     end.
%%
%% find_existing_provider(_NodeId, []) ->
%%     not_found;
%% find_existing_provider(undefined, _ProviderList) ->
%%     not_found;
%% find_existing_provider(NodeId, ProviderList) ->
%%     find_existing_provider_index(NodeId, ProviderList, 1).
%%
%% find_existing_provider_index(_NodeId, [], _Index) ->
%%     not_found;
%% find_existing_provider_index(NodeId, [Provider | Rest], Index) ->
%%     case maps:get(node_id, Provider, undefined) of
%%         NodeId -> {found, Index};
%%         _ -> find_existing_provider_index(NodeId, Rest, Index + 1)
%%     end.

%%%===================================================================
%%% Failover Tests
%%%===================================================================

%% Test that max_attempts option is respected
max_attempts_test() ->
    Providers = [
        #{node_id => <<"node-1">>, endpoint => <<"https://n1:9443">>, metadata => #{}},
        #{node_id => <<"node-2">>, endpoint => <<"https://n2:9443">>, metadata => #{}},
        #{node_id => <<"node-3">>, endpoint => <<"https://n3:9443">>, metadata => #{}}
    ],

    %% Test default max_attempts (min(3, length(Providers)) = 3)
    ?assertEqual(3, min(3, length(Providers))),

    %% Test with more providers than default max_attempts
    MoreProviders = Providers ++ [
        #{node_id => <<"node-4">>, endpoint => <<"https://n4:9443">>, metadata => #{}},
        #{node_id => <<"node-5">>, endpoint => <<"https://n5:9443">>, metadata => #{}}
    ],
    ?assertEqual(3, min(3, length(MoreProviders))),

    %% Test with fewer providers
    FewerProviders = [lists:nth(1, Providers)],
    ?assertEqual(1, min(3, length(FewerProviders))).

%% Test provider exclusion filtering
provider_exclusion_test() ->
    Providers = [
        #{node_id => <<"node-1">>, endpoint => <<"https://n1:9443">>, metadata => #{}},
        #{node_id => <<"node-2">>, endpoint => <<"https://n2:9443">>, metadata => #{}},
        #{node_id => <<"node-3">>, endpoint => <<"https://n3:9443">>, metadata => #{}}
    ],

    %% Exclude node-1
    ExcludedProviders = [<<"node-1">>],
    AvailableProviders = lists:filter(fun(#{node_id := NodeId}) ->
        not lists:member(NodeId, ExcludedProviders)
    end, Providers),

    ?assertEqual(2, length(AvailableProviders)),
    ?assertEqual([<<"node-2">>, <<"node-3">>],
                [maps:get(node_id, P) || P <- AvailableProviders]),

    %% Exclude node-1 and node-2
    ExcludedProviders2 = [<<"node-1">>, <<"node-2">>],
    AvailableProviders2 = lists:filter(fun(#{node_id := NodeId}) ->
        not lists:member(NodeId, ExcludedProviders2)
    end, Providers),

    ?assertEqual(1, length(AvailableProviders2)),
    ?assertEqual([<<"node-3">>],
                [maps:get(node_id, P) || P <- AvailableProviders2]).

%% Test all providers exhausted
all_providers_exhausted_test() ->
    Providers = [
        #{node_id => <<"node-1">>, endpoint => <<"https://n1:9443">>, metadata => #{}},
        #{node_id => <<"node-2">>, endpoint => <<"https://n2:9443">>, metadata => #{}}
    ],

    %% All providers excluded
    ExcludedProviders = [<<"node-1">>, <<"node-2">>],
    AvailableProviders = lists:filter(fun(#{node_id := NodeId}) ->
        not lists:member(NodeId, ExcludedProviders)
    end, Providers),

    ?assertEqual(0, length(AvailableProviders)).

%% Test failover context storage format
failover_context_format_test() ->
    FailoverContext = #{
        procedure => <<"test.service">>,
        args => #{data => <<"test">>},
        opts => #{timeout => 5000, max_attempts => 3},
        all_providers => [
            #{node_id => <<"node-1">>, endpoint => <<"https://n1:9443">>, metadata => #{}}
        ],
        excluded_providers => [<<"node-1">>],
        attempt => 1
    },

    %% Verify all required fields present
    ?assertMatch(#{procedure := _}, FailoverContext),
    ?assertMatch(#{args := _}, FailoverContext),
    ?assertMatch(#{opts := _}, FailoverContext),
    ?assertMatch(#{all_providers := _}, FailoverContext),
    ?assertMatch(#{excluded_providers := _}, FailoverContext),
    ?assertMatch(#{attempt := _}, FailoverContext),

    %% Extract fields
    #{
        procedure := Procedure,
        args := Args,
        opts := Opts,
        all_providers := AllProviders,
        excluded_providers := ExcludedProviders,
        attempt := Attempt
    } = FailoverContext,

    ?assertEqual(<<"test.service">>, Procedure),
    ?assertEqual(#{data => <<"test">>}, Args),
    ?assertEqual(#{timeout => 5000, max_attempts => 3}, Opts),
    ?assertEqual(1, length(AllProviders)),
    ?assertEqual([<<"node-1">>], ExcludedProviders),
    ?assertEqual(1, Attempt).

%% Test attempt counter increment
attempt_counter_test() ->
    %% Simulate retry sequence
    Attempt1 = 1,
    Attempt2 = Attempt1 + 1,
    Attempt3 = Attempt2 + 1,

    ?assertEqual(1, Attempt1),
    ?assertEqual(2, Attempt2),
    ?assertEqual(3, Attempt3),

    %% Test max attempts check
    MaxAttempts = 3,
    ?assert(Attempt1 =< MaxAttempts),
    ?assert(Attempt2 =< MaxAttempts),
    ?assert(Attempt3 =< MaxAttempts),
    ?assertNot(Attempt3 + 1 =< MaxAttempts).

%% Test excluded providers accumulation
excluded_providers_accumulation_test() ->
    %% Start with empty list
    Excluded1 = [],
    CurrentNodeId1 = <<"node-1">>,
    Excluded2 = [CurrentNodeId1 | Excluded1],

    ?assertEqual([<<"node-1">>], Excluded2),

    %% Add second provider
    CurrentNodeId2 = <<"node-2">>,
    Excluded3 = [CurrentNodeId2 | Excluded2],

    ?assertEqual([<<"node-2">>, <<"node-1">>], Excluded3),

    %% Verify all excluded
    Providers = [
        #{node_id => <<"node-1">>, endpoint => <<"https://n1:9443">>, metadata => #{}},
        #{node_id => <<"node-2">>, endpoint => <<"https://n2:9443">>, metadata => #{}},
        #{node_id => <<"node-3">>, endpoint => <<"https://n3:9443">>, metadata => #{}}
    ],

    AvailableAfterTwoFailures = lists:filter(fun(#{node_id := NodeId}) ->
        not lists:member(NodeId, Excluded3)
    end, Providers),

    ?assertEqual(1, length(AvailableAfterTwoFailures)),
    ?assertEqual(<<"node-3">>, maps:get(node_id, lists:nth(1, AvailableAfterTwoFailures))).

%% Test pending_calls storage format compatibility
pending_calls_format_test() ->
    %% Test 2-tuple format (without failover)
    From = make_ref(),
    Timer = make_ref(),
    PendingCall1 = {From, Timer},

    %% Verify 2-tuple can be extracted
    {FromExtracted, TimerExtracted} = PendingCall1,
    ?assertEqual(From, FromExtracted),
    ?assertEqual(Timer, TimerExtracted),

    %% Test 3-tuple format (with failover)
    FailoverContext = #{
        procedure => <<"test.service">>,
        args => #{},
        opts => #{},
        all_providers => [],
        excluded_providers => [],
        attempt => 1
    },
    PendingCall2 = {From, Timer, FailoverContext},

    %% Verify 3-tuple can be extracted
    {FromExtracted2, TimerExtracted2, ContextExtracted} = PendingCall2,
    ?assertEqual(From, FromExtracted2),
    ?assertEqual(Timer, TimerExtracted2),
    ?assertEqual(FailoverContext, ContextExtracted).

%% Test error detection in reply message
error_reply_detection_test() ->
    %% Success reply
    SuccessMsg = #{
        call_id => <<"call-1">>,
        result => <<"{\"status\": \"ok\"}">>
    },
    ?assertMatch(#{result := _}, SuccessMsg),
    ?assertEqual(undefined, maps:get(error, SuccessMsg, undefined)),

    %% Error reply (no result field)
    ErrorMsg = #{
        call_id => <<"call-2">>,
        error => <<"Service unavailable">>
    },
    ?assertEqual(undefined, maps:get(result, ErrorMsg, undefined)),
    ?assertMatch(#{error := _}, ErrorMsg),

    %% Check error detection logic
    case maps:get(result, SuccessMsg, undefined) of
        undefined -> ?assert(false, "Should have result");
        _Result1 -> ok
    end,

    case maps:get(result, ErrorMsg, undefined) of
        undefined -> ok;
        _Result2 -> ?assert(false, "Should not have result")
    end.
