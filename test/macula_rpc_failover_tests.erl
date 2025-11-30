%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_rpc_failover module.
%%% Tests failover strategy logic including:
%%% - Context creation
%%% - Provider exclusion
%%% - Retry decisions
%%% - Attempt tracking
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_failover_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

sample_providers() ->
    [
        #{node_id => <<"node1">>, endpoint => <<"host1:4433">>},
        #{node_id => <<"node2">>, endpoint => <<"host2:4433">>},
        #{node_id => <<"node3">>, endpoint => <<"host3:4433">>}
    ].

%%%===================================================================
%%% Context Creation Tests
%%%===================================================================

new_context_test_() ->
    [
        {"creates context with default max_attempts",
         fun() ->
             Providers = sample_providers(),
             Ctx = macula_rpc_failover:new_context(<<"my.proc">>, #{arg => 1}, #{}, Providers),
             ?assertEqual(<<"my.proc">>, maps:get(procedure, Ctx)),
             ?assertEqual(#{arg => 1}, maps:get(args, Ctx)),
             ?assertEqual(1, maps:get(attempt, Ctx)),
             ?assertEqual([], maps:get(excluded, Ctx)),
             ?assertEqual(3, maps:get(max_attempts, Ctx))
         end},

        {"respects max_attempts from opts when providers available",
         fun() ->
             %% Use 5 providers so max_attempts=5 isn't capped
             Providers = [
                 #{node_id => <<N>>} || N <- lists:seq($1, $5)
             ],
             Ctx = macula_rpc_failover:new_context(<<"proc">>, [], #{max_attempts => 5}, Providers),
             ?assertEqual(5, maps:get(max_attempts, Ctx))
         end},

        {"max_attempts capped at provider count",
         fun() ->
             Providers = [#{node_id => <<"single">>}],
             Ctx = macula_rpc_failover:new_context(<<"proc">>, [], #{max_attempts => 10}, Providers),
             ?assertEqual(1, maps:get(max_attempts, Ctx))
         end}
    ].

%%%===================================================================
%%% Can Retry Tests
%%%===================================================================

can_retry_test_() ->
    [
        {"can retry with available providers",
         fun() ->
             Ctx = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             ?assert(macula_rpc_failover:can_retry(Ctx))
         end},

        {"cannot retry when max attempts exceeded",
         fun() ->
             Ctx0 = macula_rpc_failover:new_context(<<"proc">>, [], #{max_attempts => 2}, sample_providers()),
             Ctx1 = macula_rpc_failover:increment_attempt(Ctx0),
             Ctx2 = macula_rpc_failover:increment_attempt(Ctx1),
             Ctx3 = macula_rpc_failover:increment_attempt(Ctx2),
             ?assertNot(macula_rpc_failover:can_retry(Ctx3))
         end},

        {"cannot retry when all providers excluded",
         fun() ->
             Ctx0 = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             Ctx1 = macula_rpc_failover:mark_provider_failed(<<"node1">>, Ctx0),
             Ctx2 = macula_rpc_failover:mark_provider_failed(<<"node2">>, Ctx1),
             Ctx3 = macula_rpc_failover:mark_provider_failed(<<"node3">>, Ctx2),
             ?assertNot(macula_rpc_failover:can_retry(Ctx3))
         end}
    ].

%%%===================================================================
%%% Provider Exclusion Tests
%%%===================================================================

mark_provider_failed_test_() ->
    [
        {"adds provider to exclusion list",
         fun() ->
             Ctx0 = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             Ctx1 = macula_rpc_failover:mark_provider_failed(<<"node1">>, Ctx0),
             Excluded = maps:get(excluded, Ctx1),
             ?assertEqual([<<"node1">>], Excluded)
         end},

        {"undefined provider ignored",
         fun() ->
             Ctx0 = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             Ctx1 = macula_rpc_failover:mark_provider_failed(undefined, Ctx0),
             ?assertEqual([], maps:get(excluded, Ctx1))
         end},

        {"multiple providers can be excluded",
         fun() ->
             Ctx0 = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             Ctx1 = macula_rpc_failover:mark_provider_failed(<<"node1">>, Ctx0),
             Ctx2 = macula_rpc_failover:mark_provider_failed(<<"node2">>, Ctx1),
             Available = macula_rpc_failover:get_available_providers(Ctx2),
             ?assertEqual(1, length(Available)),
             ?assertEqual(<<"node3">>, maps:get(node_id, hd(Available)))
         end}
    ].

%%%===================================================================
%%% Available Providers Tests
%%%===================================================================

get_available_providers_test_() ->
    [
        {"returns all providers when none excluded",
         fun() ->
             Ctx = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             Available = macula_rpc_failover:get_available_providers(Ctx),
             ?assertEqual(3, length(Available))
         end},

        {"filters excluded providers",
         fun() ->
             Ctx0 = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             Ctx1 = macula_rpc_failover:mark_provider_failed(<<"node2">>, Ctx0),
             Available = macula_rpc_failover:get_available_providers(Ctx1),
             ?assertEqual(2, length(Available)),
             NodeIds = [maps:get(node_id, P) || P <- Available],
             ?assertNot(lists:member(<<"node2">>, NodeIds))
         end}
    ].

%%%===================================================================
%%% Attempt Tracking Tests
%%%===================================================================

attempt_tracking_test_() ->
    [
        {"starts at attempt 1",
         fun() ->
             Ctx = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             ?assertEqual(1, macula_rpc_failover:get_attempt_count(Ctx))
         end},

        {"increment_attempt increases count",
         fun() ->
             Ctx0 = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             Ctx1 = macula_rpc_failover:increment_attempt(Ctx0),
             ?assertEqual(2, macula_rpc_failover:get_attempt_count(Ctx1)),
             Ctx2 = macula_rpc_failover:increment_attempt(Ctx1),
             ?assertEqual(3, macula_rpc_failover:get_attempt_count(Ctx2))
         end}
    ].

%%%===================================================================
%%% Should Failover Tests
%%%===================================================================

should_failover_test_() ->
    [
        {"timeout triggers failover",
         fun() ->
             Ctx = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             ?assert(macula_rpc_failover:should_failover(timeout, Ctx))
         end},

        {"connection_error triggers failover",
         fun() ->
             Ctx = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             ?assert(macula_rpc_failover:should_failover(connection_error, Ctx))
         end},

        {"provider_error triggers failover",
         fun() ->
             Ctx = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             ?assert(macula_rpc_failover:should_failover(provider_error, Ctx))
         end},

        {"gateway_timeout never triggers failover",
         fun() ->
             Ctx = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             ?assertNot(macula_rpc_failover:should_failover(gateway_timeout, Ctx))
         end},

        {"timeout with exhausted providers returns false",
         fun() ->
             Ctx0 = macula_rpc_failover:new_context(<<"proc">>, [], #{}, sample_providers()),
             Ctx1 = macula_rpc_failover:mark_provider_failed(<<"node1">>, Ctx0),
             Ctx2 = macula_rpc_failover:mark_provider_failed(<<"node2">>, Ctx1),
             Ctx3 = macula_rpc_failover:mark_provider_failed(<<"node3">>, Ctx2),
             ?assertNot(macula_rpc_failover:should_failover(timeout, Ctx3))
         end}
    ].
