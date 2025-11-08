%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_delivery module.
%%% Tests written FIRST (TDD red phase).
%%% Message routing and delivery to local and remote subscribers.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_delivery_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Create a test message
test_message(Topic) ->
    #{
        topic => Topic,
        payload => <<"test data">>,
        timestamp => erlang:system_time(millisecond)
    }.

%% Create a remote subscriber
test_remote_subscriber(NodeId, Address) ->
    #{
        node_id => NodeId,
        address => Address
    }.

%%%===================================================================
%%% Deliver Local Tests
%%%===================================================================

%% Test: deliver_local sends message to matching local subscribers
deliver_local_test() ->
    Registry = macula_pubsub_registry:new(),
    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),

    Message = test_message(<<"be.cortexiq.home.measured">>),

    Results = macula_pubsub_delivery:deliver_local(Message, Registry2),

    %% Should deliver to 1 subscriber
    ?assertEqual(1, length(Results)),
    [Result] = Results,
    ?assertMatch({ok, _}, Result),

    %% Should receive message
    receive
        Msg -> ?assertEqual(Message, Msg)
    after 100 -> ?assert(false)  % Should have received message
    end.

%% Test: deliver_local sends to multiple matching subscribers
deliver_local_multiple_test() ->
    Registry = macula_pubsub_registry:new(),
    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub_002">>, <<"be.*.*.measured">>, self()),

    Message = test_message(<<"be.cortexiq.home.measured">>),

    Results = macula_pubsub_delivery:deliver_local(Message, Registry3),

    %% Should deliver to 2 subscribers
    ?assertEqual(2, length(Results)),

    %% Should receive 2 messages
    receive Msg1 -> ?assertEqual(Message, Msg1) after 100 -> ?assert(false) end,
    receive Msg2 -> ?assertEqual(Message, Msg2) after 100 -> ?assert(false) end.

%% Test: deliver_local returns empty list when no matches
deliver_local_no_match_test() ->
    Registry = macula_pubsub_registry:new(),
    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"org.example.*">>, self()),

    Message = test_message(<<"be.cortexiq.home.measured">>),

    Results = macula_pubsub_delivery:deliver_local(Message, Registry2),

    %% Should deliver to 0 subscribers
    ?assertEqual([], Results).

%% Test: deliver_local succeeds even with dead PID (Erlang behavior)
deliver_local_dead_pid_test() ->
    Registry = macula_pubsub_registry:new(),

    %% Subscribe with dead PID (message sending still succeeds in Erlang)
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),  % Ensure process is dead

    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, DeadPid),

    Message = test_message(<<"be.cortexiq.home.measured">>),

    Results = macula_pubsub_delivery:deliver_local(Message, Registry2),

    %% Erlang allows sending to dead PIDs (returns ok)
    ?assertEqual(1, length(Results)),
    [Result] = Results,
    ?assertMatch({ok, _}, Result).

%%%===================================================================
%%% Deliver Remote Tests
%%%===================================================================

%% Test: deliver_remote sends to remote subscribers via QUIC
deliver_remote_test() ->
    RemoteSubs = [
        test_remote_subscriber(<<1:256>>, {{127,0,0,1}, 8080}),
        test_remote_subscriber(<<2:256>>, {{127,0,0,1}, 8081})
    ],

    Message = test_message(<<"be.cortexiq.home.measured">>),

    %% Mock send function
    SendCount = erlang:make_ref(),
    put(SendCount, 0),

    SendFun = fun(Msg, Address) ->
        ?assertEqual(Message, Msg),
        ?assert(Address =:= {{127,0,0,1}, 8080} orelse Address =:= {{127,0,0,1}, 8081}),
        Count = get(SendCount),
        put(SendCount, Count + 1),
        ok
    end,

    Results = macula_pubsub_delivery:deliver_remote(Message, RemoteSubs, SendFun),

    %% Should send to 2 remote subscribers
    ?assertEqual(2, length(Results)),
    ?assertEqual(2, get(SendCount)),

    %% All should succeed
    lists:foreach(fun(R) -> ?assertEqual(ok, R) end, Results).

%% Test: deliver_remote handles send failures
deliver_remote_error_test() ->
    RemoteSubs = [
        test_remote_subscriber(<<1:256>>, {{127,0,0,1}, 8080})
    ],

    Message = test_message(<<"be.cortexiq.home.measured">>),

    %% Mock send function that fails
    SendFun = fun(_Msg, _Address) -> {error, network_error} end,

    Results = macula_pubsub_delivery:deliver_remote(Message, RemoteSubs, SendFun),

    %% Should return error
    ?assertEqual(1, length(Results)),
    [Result] = Results,
    ?assertEqual({error, network_error}, Result).

%% Test: deliver_remote returns empty list for no subscribers
deliver_remote_empty_test() ->
    Message = test_message(<<"be.cortexiq.home.measured">>),
    SendFun = fun(_Msg, _Address) -> ok end,

    Results = macula_pubsub_delivery:deliver_remote(Message, [], SendFun),

    ?assertEqual([], Results).

%%%===================================================================
%%% Publish Tests (Local + Remote)
%%%===================================================================

%% Test: publish delivers to both local and remote subscribers
publish_test() ->
    %% Local registry
    Registry = macula_pubsub_registry:new(),
    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),

    %% Remote subscribers cache
    Cache = macula_pubsub_cache:new(100),
    Pattern = <<"be.cortexiq.#">>,
    RemoteSubs = [test_remote_subscriber(<<1:256>>, {{127,0,0,1}, 8080})],
    Cache2 = macula_pubsub_cache:put(Cache, Pattern, RemoteSubs),

    Message = test_message(<<"be.cortexiq.home.measured">>),

    %% Mock discovery (use cache)
    DiscoveryFun = fun(Topic) ->
        case macula_pubsub_cache:get(Cache2, Topic) of
            {ok, Subs, _} -> {ok, Subs};
            not_found -> {ok, []}
        end
    end,

    %% Mock send
    SendFun = fun(_Msg, _Address) -> ok end,

    {LocalResults, RemoteResults} = macula_pubsub_delivery:publish(
        Message, Registry2, DiscoveryFun, SendFun
    ),

    %% Should deliver to 1 local and 1 remote
    ?assertEqual(1, length(LocalResults)),
    ?assertEqual(1, length(RemoteResults)),

    %% Should receive local message
    receive
        Msg -> ?assertEqual(Message, Msg)
    after 100 -> ?assert(false)
    end.

%% Test: publish handles discovery errors gracefully
publish_discovery_error_test() ->
    Registry = macula_pubsub_registry:new(),
    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),

    Message = test_message(<<"be.cortexiq.home.measured">>),

    %% Mock discovery that fails
    DiscoveryFun = fun(_Topic) -> {error, dht_timeout} end,

    %% Mock send
    SendFun = fun(_Msg, _Address) -> ok end,

    {LocalResults, RemoteResults} = macula_pubsub_delivery:publish(
        Message, Registry2, DiscoveryFun, SendFun
    ),

    %% Should still deliver locally
    ?assertEqual(1, length(LocalResults)),

    %% Remote should be empty (discovery failed)
    ?assertEqual([], RemoteResults),

    %% Should receive local message
    receive
        Msg -> ?assertEqual(Message, Msg)
    after 100 -> ?assert(false)
    end.

%%%===================================================================
%%% Fan-out Pattern Tests
%%%===================================================================

%% Test: get_matching_patterns returns all patterns that match topic
get_matching_patterns_test() ->
    Registry = macula_pubsub_registry:new(),
    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub_002">>, <<"be.*.*.measured">>, self()),
    Registry4 = macula_pubsub_registry:subscribe(Registry3, <<"sub_003">>, <<"org.example.*">>, self()),

    Topic = <<"be.cortexiq.home.measured">>,

    Patterns = macula_pubsub_delivery:get_matching_patterns(Topic, Registry4),

    %% Should match first 2 patterns, not third
    ?assertEqual(2, length(Patterns)),
    ?assert(lists:member(<<"be.cortexiq.#">>, Patterns)),
    ?assert(lists:member(<<"be.*.*.measured">>, Patterns)),
    ?assertNot(lists:member(<<"org.example.*">>, Patterns)).

%% Test: get_matching_patterns returns unique patterns
get_matching_patterns_unique_test() ->
    Registry = macula_pubsub_registry:new(),

    %% Same pattern, different subscribers
    Registry2 = macula_pubsub_registry:subscribe(Registry, <<"sub_001">>, <<"be.cortexiq.#">>, self()),
    Registry3 = macula_pubsub_registry:subscribe(Registry2, <<"sub_002">>, <<"be.cortexiq.#">>, self()),

    Topic = <<"be.cortexiq.home.measured">>,

    Patterns = macula_pubsub_delivery:get_matching_patterns(Topic, Registry3),

    %% Should return pattern only once
    ?assertEqual(1, length(Patterns)),
    ?assertEqual([<<"be.cortexiq.#">>], Patterns).
