%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_registry module.
%%%
%%% Tests local RPC procedure registration registry.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% new/0,1 Tests
%%%===================================================================

new_creates_empty_registry_test() ->
    %% WHEN: Creating a new registry
    Registry = macula_rpc_registry:new(),

    %% THEN: Should be empty with default strategy
    ?assertEqual(0, macula_rpc_registry:size(Registry)),
    ?assertEqual([], macula_rpc_registry:list_uris(Registry)).

new_with_strategy_test() ->
    %% WHEN: Creating with custom strategy
    Registry = macula_rpc_registry:new(random),

    %% THEN: Should be empty
    ?assertEqual(0, macula_rpc_registry:size(Registry)).

%%%===================================================================
%%% register/4 Tests
%%%===================================================================

register_adds_handler_test() ->
    %% GIVEN: An empty registry
    Registry = macula_rpc_registry:new(),
    Handler = fun(_Args) -> {ok, done} end,

    %% WHEN: Registering a handler
    Updated = macula_rpc_registry:register(Registry, <<"com.example.service">>, Handler, #{}),

    %% THEN: Should have one registration
    ?assertEqual(1, macula_rpc_registry:size(Updated)),
    ?assertEqual([<<"com.example.service">>], macula_rpc_registry:list_uris(Updated)).

register_multiple_handlers_same_uri_test() ->
    %% GIVEN: A registry with one handler
    Registry0 = macula_rpc_registry:new(),
    Handler1 = fun(_Args) -> {ok, handler1} end,
    Handler2 = fun(_Args) -> {ok, handler2} end,

    Registry1 = macula_rpc_registry:register(Registry0, <<"com.example.service">>, Handler1, #{}),

    %% WHEN: Registering another handler for same URI
    Registry2 = macula_rpc_registry:register(Registry1, <<"com.example.service">>, Handler2, #{}),

    %% THEN: Should have both handlers
    ?assertEqual(2, macula_rpc_registry:size(Registry2)),
    %% But only one unique URI
    ?assertEqual([<<"com.example.service">>], macula_rpc_registry:list_uris(Registry2)).

register_with_metadata_test() ->
    %% GIVEN: An empty registry
    Registry = macula_rpc_registry:new(),
    Handler = fun(_Args) -> {ok, done} end,
    Metadata = #{version => <<"1.0">>, cache_ttl => 60},

    %% WHEN: Registering with metadata
    Updated = macula_rpc_registry:register(Registry, <<"com.example.service">>, Handler, Metadata),

    %% THEN: Registration should have metadata
    [Reg] = macula_rpc_registry:find(Updated, <<"com.example.service">>),
    ?assertEqual(Metadata, maps:get(metadata, Reg)).

%%%===================================================================
%%% unregister/3 Tests
%%%===================================================================

unregister_removes_handler_test() ->
    %% GIVEN: A registry with one handler
    Registry0 = macula_rpc_registry:new(),
    Handler = fun(_Args) -> {ok, done} end,
    Registry1 = macula_rpc_registry:register(Registry0, <<"com.example.service">>, Handler, #{}),
    ?assertEqual(1, macula_rpc_registry:size(Registry1)),

    %% WHEN: Unregistering
    Registry2 = macula_rpc_registry:unregister(Registry1, <<"com.example.service">>, Handler),

    %% THEN: Should be empty
    ?assertEqual(0, macula_rpc_registry:size(Registry2)).

unregister_removes_only_matching_handler_test() ->
    %% GIVEN: A registry with two handlers for same URI
    Registry0 = macula_rpc_registry:new(),
    Handler1 = fun(_Args) -> {ok, handler1} end,
    Handler2 = fun(_Args) -> {ok, handler2} end,

    Registry1 = macula_rpc_registry:register(Registry0, <<"com.example.service">>, Handler1, #{}),
    Registry2 = macula_rpc_registry:register(Registry1, <<"com.example.service">>, Handler2, #{}),
    ?assertEqual(2, macula_rpc_registry:size(Registry2)),

    %% WHEN: Unregistering only Handler1
    Registry3 = macula_rpc_registry:unregister(Registry2, <<"com.example.service">>, Handler1),

    %% THEN: Should have one handler left
    ?assertEqual(1, macula_rpc_registry:size(Registry3)),
    [Remaining] = macula_rpc_registry:find(Registry3, <<"com.example.service">>),
    ?assertEqual(Handler2, maps:get(handler, Remaining)).

unregister_nonexistent_is_noop_test() ->
    %% GIVEN: An empty registry
    Registry = macula_rpc_registry:new(),
    Handler = fun(_Args) -> {ok, done} end,

    %% WHEN: Unregistering non-existent handler
    Updated = macula_rpc_registry:unregister(Registry, <<"missing">>, Handler),

    %% THEN: Should remain empty
    ?assertEqual(0, macula_rpc_registry:size(Updated)).

%%%===================================================================
%%% find/2 Tests
%%%===================================================================

find_returns_matching_registrations_test() ->
    %% GIVEN: A registry with handlers
    Registry0 = macula_rpc_registry:new(),
    Handler1 = fun(_Args) -> {ok, h1} end,
    Handler2 = fun(_Args) -> {ok, h2} end,

    Registry1 = macula_rpc_registry:register(Registry0, <<"com.example.service">>, Handler1, #{}),
    Registry2 = macula_rpc_registry:register(Registry1, <<"com.example.service">>, Handler2, #{}),

    %% WHEN: Finding
    Result = macula_rpc_registry:find(Registry2, <<"com.example.service">>),

    %% THEN: Should return both handlers
    ?assertEqual(2, length(Result)).

find_returns_empty_for_no_match_test() ->
    %% GIVEN: A registry with handlers
    Registry0 = macula_rpc_registry:new(),
    Handler = fun(_Args) -> {ok, done} end,
    Registry1 = macula_rpc_registry:register(Registry0, <<"com.example.service">>, Handler, #{}),

    %% WHEN: Finding non-matching URI
    Result = macula_rpc_registry:find(Registry1, <<"com.other.service">>),

    %% THEN: Should be empty list
    ?assertEqual([], Result).

%%%===================================================================
%%% find_handlers/2 Tests
%%%===================================================================

find_handlers_returns_ok_with_matches_test() ->
    %% GIVEN: A registry with a handler
    Registry0 = macula_rpc_registry:new(),
    Handler = fun(_Args) -> {ok, done} end,
    Registry1 = macula_rpc_registry:register(Registry0, <<"com.example.service">>, Handler, #{}),

    %% WHEN: Finding handlers
    Result = macula_rpc_registry:find_handlers(Registry1, <<"com.example.service">>),

    %% THEN: Should return ok with handlers
    ?assertMatch({ok, [_]}, Result).

find_handlers_returns_not_found_for_no_match_test() ->
    %% GIVEN: An empty registry
    Registry = macula_rpc_registry:new(),

    %% WHEN: Finding handlers for non-existent URI
    Result = macula_rpc_registry:find_handlers(Registry, <<"missing">>),

    %% THEN: Should return not_found
    ?assertEqual(not_found, Result).

%%%===================================================================
%%% list_uris/1 Tests
%%%===================================================================

list_uris_returns_unique_uris_test() ->
    %% GIVEN: A registry with multiple handlers for some URIs
    Registry0 = macula_rpc_registry:new(),
    Handler1 = fun(_) -> {ok, 1} end,
    Handler2 = fun(_) -> {ok, 2} end,
    Handler3 = fun(_) -> {ok, 3} end,

    Registry1 = macula_rpc_registry:register(Registry0, <<"service1">>, Handler1, #{}),
    Registry2 = macula_rpc_registry:register(Registry1, <<"service1">>, Handler2, #{}),  % same URI
    Registry3 = macula_rpc_registry:register(Registry2, <<"service2">>, Handler3, #{}),

    %% WHEN: Listing URIs
    Uris = macula_rpc_registry:list_uris(Registry3),

    %% THEN: Should have unique URIs
    ?assertEqual(2, length(Uris)),
    ?assert(lists:member(<<"service1">>, Uris)),
    ?assert(lists:member(<<"service2">>, Uris)).

%%%===================================================================
%%% list_registrations/1 Tests
%%%===================================================================

list_registrations_returns_all_test() ->
    %% GIVEN: A registry with multiple registrations
    Registry0 = macula_rpc_registry:new(),
    Handler1 = fun(_) -> {ok, 1} end,
    Handler2 = fun(_) -> {ok, 2} end,
    Handler3 = fun(_) -> {ok, 3} end,

    Registry1 = macula_rpc_registry:register(Registry0, <<"s1">>, Handler1, #{}),
    Registry2 = macula_rpc_registry:register(Registry1, <<"s1">>, Handler2, #{}),
    Registry3 = macula_rpc_registry:register(Registry2, <<"s2">>, Handler3, #{}),

    %% WHEN: Listing all registrations
    All = macula_rpc_registry:list_registrations(Registry3),

    %% THEN: Should have all 3 registrations
    ?assertEqual(3, length(All)).

%%%===================================================================
%%% size/1 Tests
%%%===================================================================

size_reflects_total_registrations_test() ->
    %% GIVEN: Build up registry
    Registry0 = macula_rpc_registry:new(),
    ?assertEqual(0, macula_rpc_registry:size(Registry0)),

    Handler = fun(_) -> {ok, done} end,
    Registry1 = macula_rpc_registry:register(Registry0, <<"s1">>, Handler, #{}),
    ?assertEqual(1, macula_rpc_registry:size(Registry1)),

    Registry2 = macula_rpc_registry:register(Registry1, <<"s1">>, Handler, #{}),
    ?assertEqual(2, macula_rpc_registry:size(Registry2)),

    Registry3 = macula_rpc_registry:register(Registry2, <<"s2">>, Handler, #{}),
    ?assertEqual(3, macula_rpc_registry:size(Registry3)).
