%%%-------------------------------------------------------------------
%%% @doc EUnit tests for macula_gossip module.
%%%
%%% Tests the gossip protocol for CRDT state replication, including:
%%% - State management (put, get, delete)
%%% - Peer management (add, remove, list)
%%% - Push/pull operations
%%% - Anti-entropy synchronization
%%% - Vector clock operations
%%% - CRDT merging
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gossip_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

gossip_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      %% State Management Tests
      {"put and get value", fun test_put_get_value/0},
      {"get non-existent key", fun test_get_nonexistent/0},
      {"delete value", fun test_delete_value/0},
      {"get all values", fun test_get_all/0},
      {"put overwrites existing", fun test_put_overwrites/0},

      %% Peer Management Tests
      {"add peer", fun test_add_peer/0},
      {"add duplicate peer", fun test_add_duplicate_peer/0},
      {"remove peer", fun test_remove_peer/0},
      {"get peers", fun test_get_peers/0},

      %% Statistics Tests
      {"get stats", fun test_get_stats/0},

      %% Push/Pull Tests
      {"push state to peer", fun test_push_state/0},
      {"handle incoming push", fun test_handle_push/0},
      {"handle incoming pull", fun test_handle_pull/0},

      %% Anti-entropy Tests
      {"anti-entropy with empty peers", fun test_anti_entropy_empty_peers/0},
      {"handle incoming sync", fun test_handle_sync/0},
      {"handle sync reply", fun test_handle_sync_reply/0},

      %% CRDT Merge Tests
      {"merge lww_register", fun test_merge_lww/0},
      {"merge or_set", fun test_merge_or_set/0},
      {"merge gcounter", fun test_merge_gcounter/0},
      {"merge pncounter", fun test_merge_pncounter/0},
      {"merge concurrent updates", fun test_merge_concurrent/0}
     ]}.

setup() ->
    application:ensure_all_started(gproc),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% State Management Tests
%%%===================================================================

test_put_get_value() ->
    %% GIVEN: A gossip server
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),

    %% WHEN: Storing a value
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, macula_crdt:new_lww_register(<<"value1">>)),

    %% THEN: Can retrieve the value
    {ok, {lww_register, Register}} = macula_gossip:get(Pid, <<"key1">>),
    ?assertEqual(<<"value1">>, macula_crdt:lww_get(Register)),

    macula_gossip:stop(Pid).

test_get_nonexistent() ->
    %% GIVEN: An empty gossip server
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),

    %% WHEN: Getting a non-existent key
    Result = macula_gossip:get(Pid, <<"nonexistent">>),

    %% THEN: Returns not_found
    ?assertEqual({error, not_found}, Result),

    macula_gossip:stop(Pid).

test_delete_value() ->
    %% GIVEN: A gossip server with a stored value
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, macula_crdt:new_lww_register(<<"value1">>)),

    %% WHEN: Deleting the value
    ok = macula_gossip:delete(Pid, <<"key1">>),

    %% THEN: Value is no longer retrievable
    ?assertEqual({error, not_found}, macula_gossip:get(Pid, <<"key1">>)),

    macula_gossip:stop(Pid).

test_get_all() ->
    %% GIVEN: A gossip server with multiple values
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, macula_crdt:new_lww_register(<<"v1">>)),
    ok = macula_gossip:put(Pid, <<"key2">>, gcounter, macula_crdt:new_gcounter()),

    %% WHEN: Getting all values
    All = macula_gossip:get_all(Pid),

    %% THEN: Both values are returned
    ?assertEqual(2, maps:size(All)),
    ?assert(maps:is_key(<<"key1">>, All)),
    ?assert(maps:is_key(<<"key2">>, All)),

    macula_gossip:stop(Pid).

test_put_overwrites() ->
    %% GIVEN: A gossip server with an existing value
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, macula_crdt:new_lww_register(<<"old">>)),

    %% WHEN: Storing a new value with the same key
    timer:sleep(1),  % Ensure different timestamp
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, macula_crdt:new_lww_register(<<"new">>)),

    %% THEN: New value is returned
    {ok, {lww_register, Register}} = macula_gossip:get(Pid, <<"key1">>),
    ?assertEqual(<<"new">>, macula_crdt:lww_get(Register)),

    macula_gossip:stop(Pid).

%%%===================================================================
%%% Peer Management Tests
%%%===================================================================

test_add_peer() ->
    %% GIVEN: A gossip server
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),

    %% WHEN: Adding a peer
    ok = macula_gossip:add_peer(Pid, <<"peer1">>),

    %% THEN: Peer is in the list
    Peers = macula_gossip:get_peers(Pid),
    ?assert(lists:member(<<"peer1">>, Peers)),

    macula_gossip:stop(Pid).

test_add_duplicate_peer() ->
    %% GIVEN: A gossip server with an existing peer
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),
    ok = macula_gossip:add_peer(Pid, <<"peer1">>),

    %% WHEN: Adding the same peer again
    ok = macula_gossip:add_peer(Pid, <<"peer1">>),

    %% THEN: Only one copy of the peer exists
    Peers = macula_gossip:get_peers(Pid),
    ?assertEqual(1, length([P || P <- Peers, P =:= <<"peer1">>])),

    macula_gossip:stop(Pid).

test_remove_peer() ->
    %% GIVEN: A gossip server with a peer
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>, peers => [<<"peer1">>, <<"peer2">>]}),

    %% WHEN: Removing the peer
    ok = macula_gossip:remove_peer(Pid, <<"peer1">>),

    %% THEN: Peer is no longer in the list
    Peers = macula_gossip:get_peers(Pid),
    ?assertNot(lists:member(<<"peer1">>, Peers)),
    ?assert(lists:member(<<"peer2">>, Peers)),

    macula_gossip:stop(Pid).

test_get_peers() ->
    %% GIVEN: A gossip server with initial peers
    InitialPeers = [<<"peer1">>, <<"peer2">>, <<"peer3">>],
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>, peers => InitialPeers}),

    %% WHEN: Getting peers
    Peers = macula_gossip:get_peers(Pid),

    %% THEN: All initial peers are returned
    ?assertEqual(3, length(Peers)),

    macula_gossip:stop(Pid).

%%%===================================================================
%%% Statistics Tests
%%%===================================================================

test_get_stats() ->
    %% GIVEN: A gossip server with some activity
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>, peers => [<<"peer1">>]}),
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, macula_crdt:new_lww_register(<<"v1">>)),

    %% WHEN: Getting stats
    Stats = macula_gossip:get_stats(Pid),

    %% THEN: Stats contain expected fields
    ?assertEqual(<<"node1">>, maps:get(node_id, Stats)),
    ?assertEqual(1, maps:get(state_count, Stats)),
    ?assertEqual(1, maps:get(peer_count, Stats)),
    ?assert(maps:is_key(push_count, Stats)),
    ?assert(maps:is_key(merge_count, Stats)),

    macula_gossip:stop(Pid).

%%%===================================================================
%%% Push/Pull Tests
%%%===================================================================

test_push_state() ->
    %% GIVEN: A gossip server with a send function that captures messages
    Self = self(),
    SendFn = fun(PeerNodeId, Msg) ->
        Self ! {sent, PeerNodeId, Msg},
        ok
    end,
    {ok, Pid} = macula_gossip:start_link(#{
        node_id => <<"node1">>,
        send_fn => SendFn,
        push_interval => 60000  % Don't auto-push during test
    }),
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, macula_crdt:new_lww_register(<<"value1">>)),

    %% WHEN: Pushing state to a peer
    ok = macula_gossip:push_state(Pid, <<"peer1">>),

    %% THEN: Message was sent to the peer
    receive
        {sent, <<"peer1">>, {gossip_push, Msg}} ->
            ?assertEqual(<<"node1">>, maps:get(<<"node_id">>, Msg)),
            ?assertEqual(<<"key1">>, maps:get(<<"state_key">>, Msg)),
            ?assertEqual(lww_register, maps:get(<<"state_type">>, Msg))
    after 1000 ->
        ?assert(false, "Expected push message not received")
    end,

    macula_gossip:stop(Pid).

test_handle_push() ->
    %% GIVEN: A gossip server
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),

    %% WHEN: Receiving a push from another node
    RemoteRegister = macula_crdt:new_lww_register(<<"remote_value">>),
    PushMsg = #{
        <<"node_id">> => <<"node2">>,
        <<"state_key">> => <<"shared_key">>,
        <<"state_type">> => lww_register,
        <<"state">> => RemoteRegister,
        <<"vector_clock">> => #{<<"node2">> => 1}
    },
    macula_gossip:handle_gossip_push(Pid, PushMsg),

    %% Allow cast to be processed
    timer:sleep(50),

    %% THEN: State is stored locally
    {ok, {lww_register, Stored}} = macula_gossip:get(Pid, <<"shared_key">>),
    ?assertEqual(<<"remote_value">>, macula_crdt:lww_get(Stored)),

    macula_gossip:stop(Pid).

test_handle_pull() ->
    %% GIVEN: A gossip server with some state
    Self = self(),
    SendFn = fun(PeerNodeId, Msg) ->
        Self ! {sent, PeerNodeId, Msg},
        ok
    end,
    {ok, Pid} = macula_gossip:start_link(#{
        node_id => <<"node1">>,
        send_fn => SendFn
    }),
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, macula_crdt:new_lww_register(<<"v1">>)),

    %% WHEN: Receiving a pull request
    PullMsg = #{
        <<"node_id">> => <<"node2">>,
        <<"request_id">> => <<"req123">>,
        <<"state_keys">> => []  % Request all
    },
    macula_gossip:handle_gossip_pull(Pid, PullMsg),

    %% THEN: Pull reply is sent
    receive
        {sent, <<"node2">>, {gossip_pull_reply, Reply}} ->
            ?assertEqual(<<"node1">>, maps:get(<<"node_id">>, Reply)),
            ?assertEqual(<<"req123">>, maps:get(<<"request_id">>, Reply)),
            States = maps:get(<<"states">>, Reply),
            ?assertEqual(1, length(States))
    after 1000 ->
        ?assert(false, "Expected pull reply not received")
    end,

    macula_gossip:stop(Pid).

%%%===================================================================
%%% Anti-entropy Tests
%%%===================================================================

test_anti_entropy_empty_peers() ->
    %% GIVEN: A gossip server with no peers
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),

    %% WHEN: Triggering anti-entropy
    macula_gossip:anti_entropy(Pid),

    %% THEN: No crash (nothing to do)
    Stats = macula_gossip:get_stats(Pid),
    ?assert(maps:is_key(node_id, Stats)),

    macula_gossip:stop(Pid).

test_handle_sync() ->
    %% GIVEN: A gossip server with some state
    Self = self(),
    SendFn = fun(PeerNodeId, Msg) ->
        Self ! {sent, PeerNodeId, Msg},
        ok
    end,
    {ok, Pid} = macula_gossip:start_link(#{
        node_id => <<"node1">>,
        send_fn => SendFn
    }),
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, macula_crdt:new_lww_register(<<"v1">>)),

    %% WHEN: Receiving a sync request with outdated digest
    SyncMsg = #{
        <<"node_id">> => <<"node2">>,
        <<"digest">> => #{}  % Node2 has nothing
    },
    macula_gossip:handle_gossip_sync(Pid, SyncMsg),

    %% THEN: Sync reply includes our state
    receive
        {sent, <<"node2">>, {gossip_sync_reply, Reply}} ->
            ?assertEqual(<<"node1">>, maps:get(<<"node_id">>, Reply)),
            States = maps:get(<<"states">>, Reply),
            ?assertEqual(1, length(States))
    after 1000 ->
        ?assert(false, "Expected sync reply not received")
    end,

    macula_gossip:stop(Pid).

test_handle_sync_reply() ->
    %% GIVEN: A gossip server
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),

    %% WHEN: Receiving a sync reply with new state
    RemoteRegister = macula_crdt:new_lww_register(<<"sync_value">>),
    SyncReplyMsg = #{
        <<"node_id">> => <<"node2">>,
        <<"states">> => [
            #{key => <<"synced_key">>,
              type => lww_register,
              state => RemoteRegister,
              vector_clock => #{<<"node2">> => 1}}
        ],
        <<"missing">> => []
    },
    macula_gossip:handle_gossip_sync_reply(Pid, SyncReplyMsg),

    %% Allow cast to be processed
    timer:sleep(50),

    %% THEN: State is stored locally
    {ok, {lww_register, Stored}} = macula_gossip:get(Pid, <<"synced_key">>),
    ?assertEqual(<<"sync_value">>, macula_crdt:lww_get(Stored)),

    macula_gossip:stop(Pid).

%%%===================================================================
%%% CRDT Merge Tests
%%%===================================================================

test_merge_lww() ->
    %% GIVEN: A gossip server with an older LWW value
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),
    OldRegister = macula_crdt:lww_set(macula_crdt:new_lww_register(), <<"old">>, 100),
    ok = macula_gossip:put(Pid, <<"key1">>, lww_register, OldRegister),

    %% WHEN: Receiving a push with newer timestamp
    NewRegister = macula_crdt:lww_set(macula_crdt:new_lww_register(), <<"new">>, 200),
    PushMsg = #{
        <<"node_id">> => <<"node2">>,
        <<"state_key">> => <<"key1">>,
        <<"state_type">> => lww_register,
        <<"state">> => NewRegister,
        <<"vector_clock">> => #{<<"node2">> => 2}
    },
    macula_gossip:handle_gossip_push(Pid, PushMsg),
    timer:sleep(50),

    %% THEN: Newer value wins
    {ok, {lww_register, Merged}} = macula_gossip:get(Pid, <<"key1">>),
    ?assertEqual(<<"new">>, macula_crdt:lww_get(Merged)),

    macula_gossip:stop(Pid).

test_merge_or_set() ->
    %% GIVEN: A gossip server with an OR-Set
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),
    LocalSet = macula_crdt:or_add(macula_crdt:new_or_set(), <<"a">>),
    ok = macula_gossip:put(Pid, <<"set1">>, or_set, LocalSet),

    %% WHEN: Receiving a push with different elements
    RemoteSet = macula_crdt:or_add(macula_crdt:new_or_set(), <<"b">>),
    PushMsg = #{
        <<"node_id">> => <<"node2">>,
        <<"state_key">> => <<"set1">>,
        <<"state_type">> => or_set,
        <<"state">> => RemoteSet,
        <<"vector_clock">> => #{<<"node2">> => 1}
    },
    macula_gossip:handle_gossip_push(Pid, PushMsg),
    timer:sleep(50),

    %% THEN: Both elements are in the merged set
    {ok, {or_set, Merged}} = macula_gossip:get(Pid, <<"set1">>),
    Elements = macula_crdt:or_elements(Merged),
    ?assert(lists:member(<<"a">>, Elements)),
    ?assert(lists:member(<<"b">>, Elements)),

    macula_gossip:stop(Pid).

test_merge_gcounter() ->
    %% GIVEN: A gossip server with a G-Counter
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),
    LocalCounter = macula_crdt:gcounter_increment(macula_crdt:new_gcounter(), 5),
    ok = macula_gossip:put(Pid, <<"counter1">>, gcounter, LocalCounter),

    %% WHEN: Receiving a push with a counter from a different node
    %% Simulate counter from 'remote@host' node
    RemoteCounter = #{remote@host => 3},  % Direct map construction to simulate different node
    PushMsg = #{
        <<"node_id">> => <<"node2">>,
        <<"state_key">> => <<"counter1">>,
        <<"state_type">> => gcounter,
        <<"state">> => RemoteCounter,
        <<"vector_clock">> => #{<<"node2">> => 1}
    },
    macula_gossip:handle_gossip_push(Pid, PushMsg),
    timer:sleep(50),

    %% THEN: Counters are merged (max of each node's count)
    {ok, {gcounter, Merged}} = macula_gossip:get(Pid, <<"counter1">>),
    %% node() has 5, remote@host has 3, total = 5 + 3 = 8
    ?assertEqual(8, macula_crdt:gcounter_value(Merged)),

    macula_gossip:stop(Pid).

test_merge_pncounter() ->
    %% GIVEN: A gossip server with a PN-Counter
    {ok, Pid} = macula_gossip:start_link(#{node_id => <<"node1">>}),
    LocalCounter = macula_crdt:pncounter_increment(macula_crdt:new_pncounter(), 10),
    ok = macula_gossip:put(Pid, <<"pn1">>, pncounter, LocalCounter),

    %% WHEN: Receiving a push with decrements
    RemoteCounter = macula_crdt:pncounter_decrement(macula_crdt:new_pncounter(), 3),
    PushMsg = #{
        <<"node_id">> => <<"node2">>,
        <<"state_key">> => <<"pn1">>,
        <<"state_type">> => pncounter,
        <<"state">> => RemoteCounter,
        <<"vector_clock">> => #{<<"node2">> => 1}
    },
    macula_gossip:handle_gossip_push(Pid, PushMsg),
    timer:sleep(50),

    %% THEN: Counters are merged
    {ok, {pncounter, Merged}} = macula_gossip:get(Pid, <<"pn1">>),
    %% node1 has +10, node2 has -3, total = 10 - 3 = 7
    ?assertEqual(7, macula_crdt:pncounter_value(Merged)),

    macula_gossip:stop(Pid).

test_merge_concurrent() ->
    %% GIVEN: Two nodes with concurrent updates to same key
    {ok, Pid1} = macula_gossip:start_link(#{node_id => <<"node1">>}),
    {ok, Pid2} = macula_gossip:start_link(#{node_id => <<"node2">>}),

    %% Both add different elements concurrently
    Set1 = macula_crdt:or_add(macula_crdt:new_or_set(), <<"from_node1">>),
    Set2 = macula_crdt:or_add(macula_crdt:new_or_set(), <<"from_node2">>),

    ok = macula_gossip:put(Pid1, <<"shared">>, or_set, Set1),
    ok = macula_gossip:put(Pid2, <<"shared">>, or_set, Set2),

    %% WHEN: Node1 receives push from Node2
    PushMsg = #{
        <<"node_id">> => <<"node2">>,
        <<"state_key">> => <<"shared">>,
        <<"state_type">> => or_set,
        <<"state">> => Set2,
        <<"vector_clock">> => #{<<"node2">> => 1}
    },
    macula_gossip:handle_gossip_push(Pid1, PushMsg),
    timer:sleep(50),

    %% THEN: Merged set has both elements
    {ok, {or_set, Merged}} = macula_gossip:get(Pid1, <<"shared">>),
    Elements = macula_crdt:or_elements(Merged),
    ?assert(lists:member(<<"from_node1">>, Elements)),
    ?assert(lists:member(<<"from_node2">>, Elements)),

    %% Stats show conflict was detected
    Stats = macula_gossip:get_stats(Pid1),
    ?assert(maps:get(conflict_count, Stats) >= 1),

    macula_gossip:stop(Pid1),
    macula_gossip:stop(Pid2).

%%%===================================================================
%%% Configuration Tests
%%%===================================================================

configuration_test_() ->
    [
     {"custom push interval", fun test_custom_push_interval/0},
     {"custom fanout", fun test_custom_fanout/0},
     {"initial peers", fun test_initial_peers/0}
    ].

test_custom_push_interval() ->
    %% GIVEN: Custom push interval
    {ok, Pid} = macula_gossip:start_link(#{
        node_id => <<"node1">>,
        push_interval => 500
    }),

    %% THEN: Server starts successfully
    ?assert(is_process_alive(Pid)),

    macula_gossip:stop(Pid).

test_custom_fanout() ->
    %% GIVEN: Custom fanout
    {ok, Pid} = macula_gossip:start_link(#{
        node_id => <<"node1">>,
        fanout => 5
    }),

    %% THEN: Server starts successfully
    ?assert(is_process_alive(Pid)),

    macula_gossip:stop(Pid).

test_initial_peers() ->
    %% GIVEN: Initial peers in config
    InitialPeers = [<<"peer1">>, <<"peer2">>],
    {ok, Pid} = macula_gossip:start_link(#{
        node_id => <<"node1">>,
        peers => InitialPeers
    }),

    %% THEN: Peers are available
    Peers = macula_gossip:get_peers(Pid),
    ?assertEqual(2, length(Peers)),

    macula_gossip:stop(Pid).

%%%===================================================================
%%% Protocol Type Tests
%%%===================================================================

protocol_types_test_() ->
    [
     {"gossip_push type ID", fun test_gossip_push_type/0},
     {"gossip_pull type ID", fun test_gossip_pull_type/0},
     {"gossip_pull_reply type ID", fun test_gossip_pull_reply_type/0},
     {"gossip_sync type ID", fun test_gossip_sync_type/0},
     {"gossip_sync_reply type ID", fun test_gossip_sync_reply_type/0}
    ].

test_gossip_push_type() ->
    ?assertEqual(16#70, macula_protocol_types:message_type_id(gossip_push)),
    ?assertEqual({ok, gossip_push}, macula_protocol_types:message_type_name(16#70)).

test_gossip_pull_type() ->
    ?assertEqual(16#71, macula_protocol_types:message_type_id(gossip_pull)),
    ?assertEqual({ok, gossip_pull}, macula_protocol_types:message_type_name(16#71)).

test_gossip_pull_reply_type() ->
    ?assertEqual(16#72, macula_protocol_types:message_type_id(gossip_pull_reply)),
    ?assertEqual({ok, gossip_pull_reply}, macula_protocol_types:message_type_name(16#72)).

test_gossip_sync_type() ->
    ?assertEqual(16#73, macula_protocol_types:message_type_id(gossip_sync)),
    ?assertEqual({ok, gossip_sync}, macula_protocol_types:message_type_name(16#73)).

test_gossip_sync_reply_type() ->
    ?assertEqual(16#74, macula_protocol_types:message_type_id(gossip_sync_reply)),
    ?assertEqual({ok, gossip_sync_reply}, macula_protocol_types:message_type_name(16#74)).
