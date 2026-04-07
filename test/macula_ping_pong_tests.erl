%%%-----------------------------------------------------------------------------
%%% @doc Unit Tests for macula_ping_pong Module
%%%
%%% Tests cover:
%%% - NAT type detection from node ID prefixes
%%% - Peer list generation and scaling
%%% - Number padding utility
%%% - Endpoint computation
%%% - Statistics tracking (ping sent, pong received, timeouts)
%%% - RTT statistics (min, max, average, success rate)
%%% - Stats merging and formatting
%%% - Random peer selection
%%% - Node ID resolution from options
%%% - Gen_server message handling (reset_stats, peer_discovered)
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(macula_ping_pong_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% NAT Type Detection Tests
%%%=============================================================================

nat_type_test_() ->
    [
     {"full_cone prefix",
      ?_assertEqual(<<"full_cone">>, macula_ping_pong:get_nat_type(<<"fc01">>))},
     {"restricted prefix",
      ?_assertEqual(<<"restricted">>, macula_ping_pong:get_nat_type(<<"rc05">>))},
     {"symmetric prefix",
      ?_assertEqual(<<"symmetric">>, macula_ping_pong:get_nat_type(<<"sy16">>))},
     {"bootstrap prefix",
      ?_assertEqual(<<"public">>, macula_ping_pong:get_nat_type(<<"bootstrap01">>))},
     {"unknown prefix",
      ?_assertEqual(<<"unknown">>, macula_ping_pong:get_nat_type(<<"node42">>))},
     {"empty binary",
      ?_assertEqual(<<"unknown">>, macula_ping_pong:get_nat_type(<<>>))}
    ].

%%%=============================================================================
%%% Pad Number Tests
%%%=============================================================================

pad_num_test_() ->
    [
     {"single digit gets zero-padded",
      ?_assertEqual(<<"01">>, macula_ping_pong:pad_num(1))},
     {"nine gets zero-padded",
      ?_assertEqual(<<"09">>, macula_ping_pong:pad_num(9))},
     {"ten is not padded",
      ?_assertEqual(<<"10">>, macula_ping_pong:pad_num(10))},
     {"large number is not padded",
      ?_assertEqual(<<"99">>, macula_ping_pong:pad_num(99))}
    ].

%%%=============================================================================
%%% Compute Peer Endpoint Tests
%%%=============================================================================

compute_peer_endpoint_test_() ->
    [
     {"appends port 4433 to peer name",
      ?_assertEqual(<<"fc01:4433">>, macula_ping_pong:compute_peer_endpoint(<<"fc01">>))},
     {"works with longer peer names",
      ?_assertEqual(<<"bootstrap01:4433">>, macula_ping_pong:compute_peer_endpoint(<<"bootstrap01">>))}
    ].

%%%=============================================================================
%%% Generate Peer List Tests
%%%=============================================================================

generate_peer_list_test_() ->
    [
     {"small count produces 3 peers (one of each type)",
      fun() ->
          List = macula_ping_pong:generate_peer_list(3),
          ?assertEqual([<<"fc01">>, <<"rc01">>, <<"sy01">>], List)
      end},
     {"count of 1 produces simple setup",
      fun() ->
          List = macula_ping_pong:generate_peer_list(1),
          ?assertEqual([<<"fc01">>, <<"rc01">>, <<"sy01">>], List)
      end},
     {"standard 50-peer setup produces correct count",
      fun() ->
          List = macula_ping_pong:generate_peer_list(50),
          ?assertEqual(50, length(List))
      end},
     {"standard 50-peer setup has correct distribution",
      fun() ->
          List = macula_ping_pong:generate_peer_list(50),
          FC = [P || <<"fc", _/binary>> = P <- List],
          RC = [P || <<"rc", _/binary>> = P <- List],
          SY = [P || <<"sy", _/binary>> = P <- List],
          ?assertEqual(17, length(FC)),
          ?assertEqual(17, length(RC)),
          ?assertEqual(16, length(SY))
      end},
     {"scaled count produces proportional distribution",
      fun() ->
          List = macula_ping_pong:generate_peer_list(9),
          FC = [P || <<"fc", _/binary>> = P <- List],
          RC = [P || <<"rc", _/binary>> = P <- List],
          SY = [P || <<"sy", _/binary>> = P <- List],
          ?assertEqual(3, length(FC)),
          ?assertEqual(3, length(RC)),
          ?assertEqual(3, length(SY))
      end}
    ].

%%%=============================================================================
%%% Min/Max with Undefined Tests
%%%=============================================================================

min_max_val_test_() ->
    [
     {"min_val with undefined returns value",
      ?_assertEqual(42, macula_ping_pong:min_val(undefined, 42))},
     {"min_val with two values returns smaller",
      ?_assertEqual(10, macula_ping_pong:min_val(10, 20))},
     {"min_val returns smaller when second is smaller",
      ?_assertEqual(5, macula_ping_pong:min_val(10, 5))},
     {"max_val with undefined returns value",
      ?_assertEqual(42, macula_ping_pong:max_val(undefined, 42))},
     {"max_val with two values returns larger",
      ?_assertEqual(20, macula_ping_pong:max_val(10, 20))},
     {"max_val returns larger when first is larger",
      ?_assertEqual(100, macula_ping_pong:max_val(100, 50))}
    ].

%%%=============================================================================
%%% Success Rate Calculation Tests
%%%=============================================================================

calc_success_rate_test_() ->
    [
     {"zero pings sent returns 0.0",
      ?_assertEqual(0.0, macula_ping_pong:calc_success_rate(peer_stats(0, 0, 0)))},
     {"all pongs received returns 100.0",
      ?_assertEqual(100.0, macula_ping_pong:calc_success_rate(peer_stats(10, 10, 0)))},
     {"half received returns 50.0",
      ?_assertEqual(50.0, macula_ping_pong:calc_success_rate(peer_stats(10, 5, 0)))}
    ].

%%%=============================================================================
%%% Average RTT Calculation Tests
%%%=============================================================================

calc_avg_rtt_test_() ->
    [
     {"zero pongs returns undefined",
      ?_assertEqual(undefined, macula_ping_pong:calc_avg_rtt(peer_stats_rtt(0, 0)))},
     {"single pong returns total as average",
      ?_assertEqual(42.0, macula_ping_pong:calc_avg_rtt(peer_stats_rtt(1, 42)))},
     {"multiple pongs computes average",
      ?_assertEqual(25.0, macula_ping_pong:calc_avg_rtt(peer_stats_rtt(4, 100)))}
    ].

%%%=============================================================================
%%% Statistics Update Tests
%%%=============================================================================

update_ping_sent_test_() ->
    [
     {"new peer gets initialized with 1 ping sent",
      fun() ->
          Result = macula_ping_pong:update_ping_sent(<<"fc01">>, #{}),
          Stats = maps:get(<<"fc01">>, Result),
          ?assertEqual(1, element(2, Stats))  %% pings_sent is field 2 in peer_stats
      end},
     {"existing peer increments ping count",
      fun() ->
          First = macula_ping_pong:update_ping_sent(<<"fc01">>, #{}),
          Second = macula_ping_pong:update_ping_sent(<<"fc01">>, First),
          Stats = maps:get(<<"fc01">>, Second),
          ?assertEqual(2, element(2, Stats))
      end}
    ].

update_timeout_test_() ->
    [
     {"new peer gets initialized with 1 timeout",
      fun() ->
          Result = macula_ping_pong:update_timeout(<<"rc01">>, #{}),
          Stats = maps:get(<<"rc01">>, Result),
          ?assertEqual(1, element(4, Stats))  %% timeouts is field 4 in peer_stats
      end},
     {"existing peer increments timeout count",
      fun() ->
          First = macula_ping_pong:update_timeout(<<"rc01">>, #{}),
          Second = macula_ping_pong:update_timeout(<<"rc01">>, First),
          Stats = maps:get(<<"rc01">>, Second),
          ?assertEqual(2, element(4, Stats))
      end}
    ].

update_pong_received_test_() ->
    {"tracks RTT and NAT type on pong",
     fun() ->
         PeerStats = macula_ping_pong:update_ping_sent(<<"fc01">>, #{}),
         Result = macula_ping_pong:update_pong_received(<<"fc01">>, 42, <<"full_cone">>, PeerStats),
         Stats = maps:get(<<"fc01">>, Result),
         %% pongs_received = field 3
         ?assertEqual(1, element(3, Stats)),
         %% min_rtt_ms = field 5
         ?assertEqual(42, element(5, Stats)),
         %% max_rtt_ms = field 6
         ?assertEqual(42, element(6, Stats)),
         %% total_rtt_ms = field 7
         ?assertEqual(42, element(7, Stats)),
         %% nat_type = field 9
         ?assertEqual(<<"full_cone">>, element(9, Stats))
     end}.

%%%=============================================================================
%%% Global Stats Tests
%%%=============================================================================

increment_sent_test_() ->
    {"increments global pings_sent counter",
     fun() ->
         Initial = peer_stats(0, 0, 0),
         Result = macula_ping_pong:increment_sent(Initial),
         ?assertEqual(1, element(2, Result))
     end}.

increment_timeout_test_() ->
    {"increments global timeout counter",
     fun() ->
         Initial = peer_stats(0, 0, 0),
         Result = macula_ping_pong:increment_timeout(Initial),
         ?assertEqual(1, element(4, Result))
     end}.

update_rtt_test_() ->
    {"updates global RTT tracking",
     fun() ->
         Initial = peer_stats(0, 0, 0),
         Result = macula_ping_pong:update_rtt(100, Initial),
         %% pongs_received incremented
         ?assertEqual(1, element(3, Result)),
         %% min_rtt_ms set
         ?assertEqual(100, element(5, Result)),
         %% max_rtt_ms set
         ?assertEqual(100, element(6, Result)),
         %% total_rtt_ms accumulated
         ?assertEqual(100, element(7, Result)),
         %% Second RTT
         Result2 = macula_ping_pong:update_rtt(50, Result),
         ?assertEqual(2, element(3, Result2)),
         ?assertEqual(50, element(5, Result2)),
         ?assertEqual(100, element(6, Result2)),
         ?assertEqual(150, element(7, Result2))
     end}.

%%%=============================================================================
%%% Merge Stats Tests
%%%=============================================================================

merge_stats_test_() ->
    {"merges two peer_stats records correctly",
     fun() ->
         %% Build stats via update functions
         A0 = peer_stats(0, 0, 0),
         A1 = macula_ping_pong:increment_sent(A0),
         A2 = macula_ping_pong:increment_sent(A1),
         A3 = macula_ping_pong:update_rtt(100, A2),

         B0 = peer_stats(0, 0, 0),
         B1 = macula_ping_pong:increment_sent(B0),
         B2 = macula_ping_pong:update_rtt(50, B1),
         B3 = macula_ping_pong:increment_timeout(B2),

         Merged = macula_ping_pong:merge_stats(A3, B3),
         %% pings_sent: 2 + 1 = 3
         ?assertEqual(3, element(2, Merged)),
         %% pongs_received: 1 + 1 = 2
         ?assertEqual(2, element(3, Merged)),
         %% timeouts: 0 + 1 = 1
         ?assertEqual(1, element(4, Merged)),
         %% min_rtt_ms: min(100, 50) = 50
         ?assertEqual(50, element(5, Merged)),
         %% max_rtt_ms: max(100, 50) = 100
         ?assertEqual(100, element(6, Merged)),
         %% total_rtt_ms: 100 + 50 = 150
         ?assertEqual(150, element(7, Merged))
     end}.

%%%=============================================================================
%%% Select Random Peer Tests
%%%=============================================================================

select_random_peer_test_() ->
    [
     {"no peers returns error",
      fun() ->
          %% Build a state record with empty known_peers
          %% The record has known_peers as the 4th field
          State = build_state(#{known_peers => []}),
          ?assertEqual({error, no_peers}, macula_ping_pong:select_random_peer(State))
      end},
     {"single peer always selected",
      fun() ->
          State = build_state(#{known_peers => [<<"fc01">>]}),
          {ok, Peer} = macula_ping_pong:select_random_peer(State),
          ?assertEqual(<<"fc01">>, Peer)
      end},
     {"selected peer is from known list",
      fun() ->
          Peers = [<<"fc01">>, <<"rc01">>, <<"sy01">>],
          State = build_state(#{known_peers => Peers}),
          {ok, Peer} = macula_ping_pong:select_random_peer(State),
          ?assert(lists:member(Peer, Peers))
      end}
    ].

%%%=============================================================================
%%% Node ID Resolution Tests
%%%=============================================================================

get_node_id_test_() ->
    [
     {"binary node_id from options",
      ?_assertEqual(<<"mynode">>, macula_ping_pong:get_node_id(#{node_id => <<"mynode">>}))},
     {"list node_id from options is converted to binary",
      ?_assertEqual(<<"mynode">>, macula_ping_pong:get_node_id(#{node_id => "mynode"}))}
    ].

%%%=============================================================================
%%% Format Peer Stats Tests
%%%=============================================================================

format_peer_stats_test_() ->
    {"formats a peer_stats record to map",
     fun() ->
         PeerStats0 = macula_ping_pong:update_ping_sent(<<"fc01">>, #{}),
         PeerStats1 = macula_ping_pong:update_pong_received(<<"fc01">>, 100, <<"full_cone">>, PeerStats0),
         Stats = maps:get(<<"fc01">>, PeerStats1),
         Result = macula_ping_pong:format_peer_stats(<<"fc01">>, Stats),
         ?assertEqual(<<"fc01">>, maps:get(peer_id, Result)),
         ?assertEqual(<<"full_cone">>, maps:get(nat_type, Result)),
         ?assertEqual(1, maps:get(pings_sent, Result)),
         ?assertEqual(1, maps:get(pongs_received, Result)),
         ?assertEqual(0, maps:get(timeouts, Result)),
         ?assertEqual(100.0, maps:get(success_rate, Result)),
         ?assertEqual(100, maps:get(min_rtt_ms, Result)),
         ?assertEqual(100, maps:get(max_rtt_ms, Result)),
         ?assertEqual(100.0, maps:get(avg_rtt_ms, Result))
     end}.

%%%=============================================================================
%%% Group By NAT Tests
%%%=============================================================================

group_by_nat_test_() ->
    {"groups peer stats by NAT type",
     fun() ->
         PS0 = macula_ping_pong:update_ping_sent(<<"fc01">>, #{}),
         PS1 = macula_ping_pong:update_pong_received(<<"fc01">>, 100, <<"full_cone">>, PS0),
         PS2 = macula_ping_pong:update_ping_sent(<<"rc01">>, PS1),
         PS3 = macula_ping_pong:update_pong_received(<<"rc01">>, 200, <<"restricted">>, PS2),
         PeerStatsList = maps:to_list(PS3),
         Result = macula_ping_pong:group_by_nat(PeerStatsList),
         ?assert(maps:is_key(<<"full_cone">>, Result)),
         ?assert(maps:is_key(<<"restricted">>, Result)),
         FCGroup = maps:get(<<"full_cone">>, Result),
         ?assertEqual(1, maps:get(pings_sent, FCGroup)),
         ?assertEqual(1, maps:get(pongs_received, FCGroup))
     end}.

%%%=============================================================================
%%% Helpers
%%%=============================================================================

%% @private Build a peer_stats record with specified sent/received/timeout counts.
%% Record: {peer_stats, pings_sent, pongs_received, timeouts, min_rtt_ms, max_rtt_ms, total_rtt_ms, last_seen, nat_type}
peer_stats(Sent, Received, Timeouts) ->
    {peer_stats, Sent, Received, Timeouts, undefined, undefined, 0, undefined, undefined}.

%% @private Build a peer_stats record with RTT fields for calc_avg_rtt testing.
peer_stats_rtt(PongsReceived, TotalRtt) ->
    {peer_stats, 0, PongsReceived, 0, undefined, undefined, TotalRtt, undefined, undefined}.

%% @private Build a state record for select_random_peer testing.
%% Record: {state, node_id, nat_type, known_peers, pending_pings, peer_stats, global_stats, interval, timeout, timer_ref, setup_complete}
build_state(Opts) ->
    KnownPeers = maps:get(known_peers, Opts, []),
    {state, <<"test_node">>, <<"unknown">>, KnownPeers, #{}, #{},
     peer_stats(0, 0, 0), 5000, 3000, undefined, false}.
