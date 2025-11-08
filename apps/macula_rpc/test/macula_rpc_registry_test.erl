%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_registry module.
%%% Tests written FIRST (TDD red phase).
%%% Local RPC procedure registration and lookup.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_registry_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Create a test handler function
test_handler() ->
    fun(Args) ->
        {ok, maps:merge(#{result => <<"success">>}, Args)}
    end.

test_handler_error() ->
    fun(_Args) ->
        {error, not_implemented}
    end.

%%%===================================================================
%%% Registry Creation Tests
%%%===================================================================

%% Test: new creates empty registry with default strategy
new_registry_test() ->
    Registry = macula_rpc_registry:new(),
    ?assertEqual(0, macula_rpc_registry:size(Registry)).

%% Test: new creates registry with custom strategy
new_registry_with_strategy_test() ->
    Registry = macula_rpc_registry:new(round_robin),
    ?assertEqual(0, macula_rpc_registry:size(Registry)).

%%%===================================================================
%%% Register Tests
%%%===================================================================

%% Test: register adds handler
register_adds_test() ->
    Registry = macula_rpc_registry:new(),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = test_handler(),

    Registry2 = macula_rpc_registry:register(Registry, Uri, Handler, #{}),
    ?assertEqual(1, macula_rpc_registry:size(Registry2)).

%% Test: register multiple URIs
register_multiple_test() ->
    Registry = macula_rpc_registry:new(),
    Uri1 = <<"be.cortexiq.home.get_measurement">>,
    Uri2 = <<"be.cortexiq.provider.calculate_price">>,
    Handler1 = test_handler(),
    Handler2 = test_handler(),

    Registry2 = macula_rpc_registry:register(Registry, Uri1, Handler1, #{}),
    Registry3 = macula_rpc_registry:register(Registry2, Uri2, Handler2, #{}),

    ?assertEqual(2, macula_rpc_registry:size(Registry3)).

%% Test: register same URI with different handlers (multiple providers)
register_same_uri_test() ->
    Registry = macula_rpc_registry:new(),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler1 = test_handler(),
    Handler2 = test_handler_error(),

    Registry2 = macula_rpc_registry:register(Registry, Uri, Handler1, #{}),
    Registry3 = macula_rpc_registry:register(Registry2, Uri, Handler2, #{}),

    %% Should have 2 registrations (multiple providers for same URI)
    ?assertEqual(2, macula_rpc_registry:size(Registry3)).

%% Test: register with metadata
register_with_metadata_test() ->
    Registry = macula_rpc_registry:new(),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = test_handler(),
    Metadata = #{rate_limit => 100, auth_required => true},

    Registry2 = macula_rpc_registry:register(Registry, Uri, Handler, Metadata),
    ?assertEqual(1, macula_rpc_registry:size(Registry2)).

%%%===================================================================
%%% Unregister Tests
%%%===================================================================

%% Test: unregister removes handler
unregister_test() ->
    Registry = macula_rpc_registry:new(),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = test_handler(),

    Registry2 = macula_rpc_registry:register(Registry, Uri, Handler, #{}),
    ?assertEqual(1, macula_rpc_registry:size(Registry2)),

    Registry3 = macula_rpc_registry:unregister(Registry2, Uri, Handler),
    ?assertEqual(0, macula_rpc_registry:size(Registry3)).

%% Test: unregister specific handler keeps others
unregister_keeps_others_test() ->
    Registry = macula_rpc_registry:new(),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler1 = test_handler(),
    Handler2 = test_handler_error(),

    Registry2 = macula_rpc_registry:register(Registry, Uri, Handler1, #{}),
    Registry3 = macula_rpc_registry:register(Registry2, Uri, Handler2, #{}),
    ?assertEqual(2, macula_rpc_registry:size(Registry3)),

    %% Unregister handler1 only
    Registry4 = macula_rpc_registry:unregister(Registry3, Uri, Handler1),
    ?assertEqual(1, macula_rpc_registry:size(Registry4)).

%% Test: unregister nonexistent does nothing
unregister_nonexistent_test() ->
    Registry = macula_rpc_registry:new(),
    Handler = test_handler(),

    Registry2 = macula_rpc_registry:unregister(Registry, <<"nonexistent">>, Handler),
    ?assertEqual(Registry, Registry2).

%%%===================================================================
%%% Find Tests
%%%===================================================================

%% Test: find_handlers returns registered handlers
find_handlers_test() ->
    Registry = macula_rpc_registry:new(),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler = test_handler(),

    Registry2 = macula_rpc_registry:register(Registry, Uri, Handler, #{}),

    {ok, Handlers} = macula_rpc_registry:find_handlers(Registry2, Uri),
    ?assertEqual(1, length(Handlers)).

%% Test: find_handlers returns multiple handlers
find_handlers_multiple_test() ->
    Registry = macula_rpc_registry:new(),
    Uri = <<"be.cortexiq.home.get_measurement">>,
    Handler1 = test_handler(),
    Handler2 = test_handler_error(),

    Registry2 = macula_rpc_registry:register(Registry, Uri, Handler1, #{}),
    Registry3 = macula_rpc_registry:register(Registry2, Uri, Handler2, #{}),

    {ok, Handlers} = macula_rpc_registry:find_handlers(Registry3, Uri),
    ?assertEqual(2, length(Handlers)).

%% Test: find_handlers returns not_found for missing URI
find_handlers_not_found_test() ->
    Registry = macula_rpc_registry:new(),
    ?assertEqual(not_found, macula_rpc_registry:find_handlers(Registry, <<"nonexistent">>)).

%%%===================================================================
%%% List Tests
%%%===================================================================

%% Test: list_uris returns all unique URIs
list_uris_test() ->
    Registry = macula_rpc_registry:new(),
    Uri1 = <<"be.cortexiq.home.get_measurement">>,
    Uri2 = <<"be.cortexiq.provider.calculate_price">>,

    Registry2 = macula_rpc_registry:register(Registry, Uri1, test_handler(), #{}),
    Registry3 = macula_rpc_registry:register(Registry2, Uri2, test_handler(), #{}),

    Uris = macula_rpc_registry:list_uris(Registry3),
    ?assertEqual(2, length(Uris)),
    ?assert(lists:member(Uri1, Uris)),
    ?assert(lists:member(Uri2, Uris)).

%% Test: list_uris returns unique URIs even with multiple handlers
list_uris_unique_test() ->
    Registry = macula_rpc_registry:new(),
    Uri = <<"be.cortexiq.home.get_measurement">>,

    Registry2 = macula_rpc_registry:register(Registry, Uri, test_handler(), #{}),
    Registry3 = macula_rpc_registry:register(Registry2, Uri, test_handler_error(), #{}),

    Uris = macula_rpc_registry:list_uris(Registry3),
    ?assertEqual(1, length(Uris)),
    ?assertEqual([Uri], Uris).

%% Test: list_registrations returns all registrations
list_registrations_test() ->
    Registry = macula_rpc_registry:new(),
    Uri = <<"be.cortexiq.home.get_measurement">>,

    Registry2 = macula_rpc_registry:register(Registry, Uri, test_handler(), #{}),
    Registry3 = macula_rpc_registry:register(Registry2, Uri, test_handler_error(), #{}),

    Registrations = macula_rpc_registry:list_registrations(Registry3),
    ?assertEqual(2, length(Registrations)).

%%%===================================================================
%%% Size Tests
%%%===================================================================

%% Test: size returns correct count
size_test() ->
    Registry = macula_rpc_registry:new(),
    ?assertEqual(0, macula_rpc_registry:size(Registry)),

    Registry2 = macula_rpc_registry:register(Registry, <<"uri1">>, test_handler(), #{}),
    ?assertEqual(1, macula_rpc_registry:size(Registry2)),

    Registry3 = macula_rpc_registry:register(Registry2, <<"uri2">>, test_handler(), #{}),
    ?assertEqual(2, macula_rpc_registry:size(Registry3)).
