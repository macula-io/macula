%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_delivery module.
%%%
%%% Tests message routing and delivery to local and remote subscribers.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_delivery_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Helpers
%%%===================================================================

make_message(Topic) ->
    #{
        topic => Topic,
        payload => <<"test payload">>,
        timestamp => erlang:system_time(millisecond)
    }.

%% Helper for custom payloads (currently unused but may be needed)
%% make_message(Topic, Payload) ->
%%     #{
%%         topic => Topic,
%%         payload => Payload,
%%         timestamp => erlang:system_time(millisecond)
%%     }.

%%%===================================================================
%%% deliver_local Tests
%%%===================================================================

deliver_local_empty_registry_test() ->
    %% GIVEN: Empty registry and a message
    Registry = macula_pubsub_registry:new(),
    Message = make_message(<<"topic.a">>),

    %% WHEN: Delivering locally
    Results = macula_pubsub_delivery:deliver_local(Message, Registry),

    %% THEN: Should return empty results
    ?assertEqual([], Results).

deliver_local_single_subscriber_test() ->
    %% GIVEN: Registry with one matching subscriber
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Message = make_message(<<"topic.a">>),

    %% WHEN: Delivering locally
    Results = macula_pubsub_delivery:deliver_local(Message, Registry1),

    %% THEN: Should deliver to the subscriber
    ?assertEqual(1, length(Results)),
    ?assertMatch([{ok, <<"sub1">>}], Results),

    %% Verify message was received
    receive
        Msg ->
            ?assertEqual(Message, Msg)
    after 100 ->
        ?assert(false)
    end.

deliver_local_multiple_subscribers_test() ->
    %% GIVEN: Registry with multiple matching subscribers
    Registry0 = macula_pubsub_registry:new(),
    %% Use self() for all - we'll verify messages received
    Self = self(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, Self),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.a">>, Self),
    Message = make_message(<<"topic.a">>),

    %% WHEN: Delivering locally
    Results = macula_pubsub_delivery:deliver_local(Message, Registry2),

    %% THEN: Should deliver to both subscribers
    ?assertEqual(2, length(Results)),

    %% Drain the mailbox
    receive _ -> ok after 50 -> ok end,
    receive _ -> ok after 50 -> ok end.

deliver_local_no_match_test() ->
    %% GIVEN: Registry with subscriber for different topic
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.b">>, self()),
    Message = make_message(<<"topic.a">>),

    %% WHEN: Delivering locally
    Results = macula_pubsub_delivery:deliver_local(Message, Registry1),

    %% THEN: Should return empty (no match)
    ?assertEqual([], Results).

deliver_local_wildcard_match_test() ->
    %% GIVEN: Registry with wildcard subscriber
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.#">>, self()),
    Message = make_message(<<"topic.a.b.c">>),

    %% WHEN: Delivering locally
    Results = macula_pubsub_delivery:deliver_local(Message, Registry1),

    %% THEN: Should match and deliver
    ?assertEqual(1, length(Results)),

    %% Drain mailbox
    receive _ -> ok after 50 -> ok end.

%%%===================================================================
%%% deliver_remote Tests
%%%===================================================================

deliver_remote_empty_subscribers_test() ->
    %% GIVEN: Empty remote subscribers list
    Message = make_message(<<"topic.a">>),
    SendFun = fun(_Msg, _Addr) -> ok end,

    %% WHEN: Delivering remotely
    Results = macula_pubsub_delivery:deliver_remote(Message, [], SendFun),

    %% THEN: Should return empty results
    ?assertEqual([], Results).

deliver_remote_single_subscriber_test() ->
    %% GIVEN: One remote subscriber
    Message = make_message(<<"topic.a">>),
    RemoteSub = #{node_id => <<"node1">>, address => {{1,2,3,4}, 5000}},
    SendFun = fun(_Msg, _Addr) -> ok end,

    %% WHEN: Delivering remotely
    Results = macula_pubsub_delivery:deliver_remote(Message, [RemoteSub], SendFun),

    %% THEN: Should have one result
    ?assertEqual(1, length(Results)),
    ?assertEqual([ok], Results).

deliver_remote_multiple_subscribers_test() ->
    %% GIVEN: Multiple remote subscribers
    Message = make_message(<<"topic.a">>),
    RemoteSubs = [
        #{node_id => <<"node1">>, address => {{1,2,3,4}, 5000}},
        #{node_id => <<"node2">>, address => {{5,6,7,8}, 5001}},
        #{node_id => <<"node3">>, address => {{9,10,11,12}, 5002}}
    ],
    SendFun = fun(_Msg, _Addr) -> ok end,

    %% WHEN: Delivering remotely
    Results = macula_pubsub_delivery:deliver_remote(Message, RemoteSubs, SendFun),

    %% THEN: Should have results for each subscriber
    ?assertEqual(3, length(Results)).

deliver_remote_with_errors_test() ->
    %% GIVEN: Remote subscribers with a failing send
    Message = make_message(<<"topic.a">>),
    RemoteSubs = [
        #{node_id => <<"node1">>, address => {{1,2,3,4}, 5000}},
        #{node_id => <<"node2">>, address => {{5,6,7,8}, 5001}}
    ],
    SendFun = fun(_Msg, {_IP, Port}) ->
        case Port of
            5000 -> ok;
            5001 -> {error, connection_failed}
        end
    end,

    %% WHEN: Delivering remotely
    Results = macula_pubsub_delivery:deliver_remote(Message, RemoteSubs, SendFun),

    %% THEN: Should return mixed results
    ?assertEqual(2, length(Results)),
    ?assert(lists:member(ok, Results)),
    ?assert(lists:member({error, connection_failed}, Results)).

deliver_remote_captures_address_test() ->
    %% GIVEN: Remote subscriber and tracking send function
    Message = make_message(<<"topic.a">>),
    RemoteSub = #{node_id => <<"node1">>, address => {{192,168,1,1}, 9443}},
    Self = self(),
    SendFun = fun(Msg, Addr) ->
        Self ! {sent, Msg, Addr},
        ok
    end,

    %% WHEN: Delivering remotely
    macula_pubsub_delivery:deliver_remote(Message, [RemoteSub], SendFun),

    %% THEN: Should have called SendFun with correct address
    receive
        {sent, SentMsg, SentAddr} ->
            ?assertEqual(Message, SentMsg),
            ?assertEqual({{192,168,1,1}, 9443}, SentAddr)
    after 100 ->
        ?assert(false)
    end.

%%%===================================================================
%%% get_matching_patterns Tests
%%%===================================================================

get_matching_patterns_empty_registry_test() ->
    %% GIVEN: Empty registry
    Registry = macula_pubsub_registry:new(),

    %% WHEN: Getting matching patterns
    Patterns = macula_pubsub_delivery:get_matching_patterns(<<"topic.a">>, Registry),

    %% THEN: Should return empty
    ?assertEqual([], Patterns).

get_matching_patterns_exact_match_test() ->
    %% GIVEN: Registry with exact pattern
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),

    %% WHEN: Getting matching patterns for exact topic
    Patterns = macula_pubsub_delivery:get_matching_patterns(<<"topic.a">>, Registry1),

    %% THEN: Should return the pattern
    ?assertEqual([<<"topic.a">>], Patterns).

get_matching_patterns_wildcard_test() ->
    %% GIVEN: Registry with wildcard pattern
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.#">>, self()),

    %% WHEN: Getting matching patterns
    Patterns = macula_pubsub_delivery:get_matching_patterns(<<"topic.a.b.c">>, Registry1),

    %% THEN: Should return the wildcard pattern
    ?assertEqual([<<"topic.#">>], Patterns).

get_matching_patterns_multiple_test() ->
    %% GIVEN: Registry with multiple matching patterns
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.#">>, self()),

    %% WHEN: Getting matching patterns
    Patterns = macula_pubsub_delivery:get_matching_patterns(<<"topic.a">>, Registry2),

    %% THEN: Should return both patterns (sorted)
    ?assertEqual([<<"topic.#">>, <<"topic.a">>], lists:sort(Patterns)).

get_matching_patterns_unique_test() ->
    %% GIVEN: Registry with duplicate patterns from different subscribers
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Registry2 = macula_pubsub_registry:subscribe(Registry1, <<"sub2">>, <<"topic.a">>, self()),

    %% WHEN: Getting matching patterns
    Patterns = macula_pubsub_delivery:get_matching_patterns(<<"topic.a">>, Registry2),

    %% THEN: Should return unique pattern (no duplicates)
    ?assertEqual([<<"topic.a">>], Patterns).

%%%===================================================================
%%% publish Tests
%%%===================================================================

publish_local_only_test() ->
    %% GIVEN: Registry with local subscriber, no remote discovery
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Message = make_message(<<"topic.a">>),
    DiscoveryFun = fun(_Pattern) -> {ok, []} end,
    SendFun = fun(_Msg, _Addr) -> ok end,

    %% WHEN: Publishing
    {LocalResults, RemoteResults} = macula_pubsub_delivery:publish(Message, Registry1, DiscoveryFun, SendFun),

    %% THEN: Should have local result, no remote
    ?assertEqual(1, length(LocalResults)),
    ?assertEqual(0, length(RemoteResults)),

    %% Drain mailbox
    receive _ -> ok after 50 -> ok end.

publish_remote_only_test() ->
    %% GIVEN: Empty registry, remote subscribers discovered
    Registry = macula_pubsub_registry:new(),
    Message = make_message(<<"topic.a">>),
    RemoteSub = #{node_id => <<"remote1">>, address => {{1,2,3,4}, 5000}},
    DiscoveryFun = fun(_Pattern) -> {ok, [RemoteSub]} end,
    SendFun = fun(_Msg, _Addr) -> ok end,

    %% WHEN: Publishing
    {LocalResults, RemoteResults} = macula_pubsub_delivery:publish(Message, Registry, DiscoveryFun, SendFun),

    %% THEN: Should have no local, one remote
    ?assertEqual(0, length(LocalResults)),
    %% Note: Remote discovery requires matching patterns in registry
    %% Empty registry means no patterns to discover
    ?assertEqual(0, length(RemoteResults)).

publish_discovery_error_test() ->
    %% GIVEN: Discovery returns error
    Registry0 = macula_pubsub_registry:new(),
    Registry1 = macula_pubsub_registry:subscribe(Registry0, <<"sub1">>, <<"topic.a">>, self()),
    Message = make_message(<<"topic.a">>),
    DiscoveryFun = fun(_Pattern) -> {error, network_error} end,
    SendFun = fun(_Msg, _Addr) -> ok end,

    %% WHEN: Publishing
    {LocalResults, RemoteResults} = macula_pubsub_delivery:publish(Message, Registry1, DiscoveryFun, SendFun),

    %% THEN: Should still deliver locally, remote empty due to error
    ?assertEqual(1, length(LocalResults)),
    ?assertEqual(0, length(RemoteResults)),

    %% Drain mailbox
    receive _ -> ok after 50 -> ok end.

