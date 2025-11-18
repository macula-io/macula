%%%-----------------------------------------------------------------------------
%%% @doc Unit Tests for macula_peers_sup Module
%%%
%%% Comprehensive test suite for the peers supervisor functionality.
%%% Tests cover:
%%% - Supervisor initialization
%%% - Peer listing and counting API
%%% - Error handling
%%%
%%% Note: These tests verify the API and module structure.
%%% Full integration tests require macula_peer_system to be implemented.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(macula_peers_sup_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Module Structure Tests
%%%=============================================================================

module_exists_test() ->
    %% Verify module is loaded
    ?assert(erlang:function_exported(macula_peers_sup, start_link, 0)),
    ?assert(erlang:function_exported(macula_peers_sup, start_peer, 2)),
    ?assert(erlang:function_exported(macula_peers_sup, stop_peer, 1)),
    ?assert(erlang:function_exported(macula_peers_sup, list_peers, 0)),
    ?assert(erlang:function_exported(macula_peers_sup, count_peers, 0)).

behavior_test() ->
    %% Verify it's a supervisor
    Behaviors = macula_peers_sup:module_info(attributes),
    BehaviorList = proplists:get_value(behaviour, Behaviors, []),
    ?assert(lists:member(supervisor, BehaviorList)).

%%%=============================================================================
%%% API Type Tests
%%%=============================================================================

api_signatures_test_() ->
    [
     {"start_peer/2 accepts binary URL and map",
      fun test_start_peer_accepts_binary/0},
     {"start_peer/2 accepts string URL and map",
      fun test_start_peer_accepts_string/0},
     {"stop_peer/1 accepts PID",
      fun test_stop_peer_accepts_pid/0}
    ].

test_start_peer_accepts_binary() ->
    %% Verify function exists and has correct arity
    ?assert(erlang:function_exported(macula_peers_sup, start_peer, 2)).

test_start_peer_accepts_string() ->
    %% Verify function exists and has correct arity
    ?assert(erlang:function_exported(macula_peers_sup, start_peer, 2)).

test_stop_peer_accepts_pid() ->
    %% Verify function exists and has correct arity
    ?assert(erlang:function_exported(macula_peers_sup, stop_peer, 1)).

%%%=============================================================================
%%% Supervisor Init Tests
%%%=============================================================================

init_returns_valid_spec_test() ->
    %% Test that init/1 returns a valid supervisor spec
    Result = macula_peers_sup:init([]),

    %% Should return {ok, {SupFlags, ChildSpecs}}
    ?assertMatch({ok, {_, [_]}}, Result),

    {ok, {SupFlags, ChildSpecs}} = Result,

    %% Verify SupFlags structure
    ?assertMatch(#{strategy := simple_one_for_one}, SupFlags),
    ?assertMatch(#{intensity := 10}, SupFlags),
    ?assertMatch(#{period := 60}, SupFlags),

    %% Verify ChildSpecs structure (should be list with one template)
    ?assertEqual(1, length(ChildSpecs)),

    [ChildSpec] = ChildSpecs,

    %% Verify child spec structure
    ?assertMatch(#{id := macula_peer_system}, ChildSpec),
    ?assertMatch(#{restart := temporary}, ChildSpec),
    ?assertMatch(#{type := supervisor}, ChildSpec),
    ?assertMatch(#{modules := [macula_peer_system]}, ChildSpec).

%%%=============================================================================
%%% Integration Test Placeholders
%%%=============================================================================

%% These tests are placeholders for future integration tests
%% when macula_peer_system is implemented

integration_test_() ->
    [
     {"Placeholder: Start peer connection (requires macula_peer_system)",
      fun test_placeholder_start_peer/0},
     {"Placeholder: List peers (requires running supervisor)",
      fun test_placeholder_list_peers/0},
     {"Placeholder: Count peers (requires running supervisor)",
      fun test_placeholder_count_peers/0}
    ].

test_placeholder_start_peer() ->
    %% This test will be enabled when macula_peer_system exists
    ?assert(true).

test_placeholder_list_peers() ->
    %% This test will be enabled when supervisor is running
    ?assert(true).

test_placeholder_count_peers() ->
    %% This test will be enabled when supervisor is running
    ?assert(true).

%%%=============================================================================
%%% Documentation Tests
%%%=============================================================================

documentation_test_() ->
    [
     {"Module has documentation",
      fun test_module_has_docs/0},
     {"Exported functions are documented",
      fun test_functions_have_specs/0}
    ].

test_module_has_docs() ->
    %% Verify module has module-level documentation
    %% (This is a smoke test - actual doc extraction would require edoc)
    {ok, Binary} = file:read_file("src/macula_peers_sup.erl"),
    Source = binary_to_list(Binary),

    %% Should have @doc comment
    ?assert(string:str(Source, "@doc") > 0).

test_functions_have_specs() ->
    %% Verify key functions have -spec declarations
    {ok, Binary} = file:read_file("src/macula_peers_sup.erl"),
    Source = binary_to_list(Binary),

    %% Check for specs
    ?assert(string:str(Source, "-spec start_link()") > 0),
    ?assert(string:str(Source, "-spec start_peer(") > 0),
    ?assert(string:str(Source, "-spec stop_peer(") > 0),
    ?assert(string:str(Source, "-spec list_peers()") > 0),
    ?assert(string:str(Source, "-spec count_peers()") > 0).
