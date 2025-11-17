%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_rpc_routing (DHT-routed RPC).
%%% Tests wrapping, routing, and delivery of RPC messages through DHT.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_routing_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

rpc_routing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Fixture) ->
         [
          {"wrap_call creates valid rpc_route envelope", fun() -> test_wrap_call(Fixture) end},
          {"wrap_reply creates valid rpc_route envelope", fun() -> test_wrap_reply(Fixture) end},
          {"should_deliver_locally when destination matches", fun() -> test_should_deliver_locally(Fixture) end},
          {"route_or_deliver delivers locally", fun() -> test_route_deliver_local(Fixture) end},
          {"route_or_deliver forwards to next hop", fun() -> test_route_forward(Fixture) end},
          {"route_or_deliver rejects max hops exceeded", fun() -> test_max_hops(Fixture) end}
         ]
     end}.

%%%===================================================================
%%% Setup/Cleanup
%%%===================================================================

setup() ->
    %% Setup routing server for routing tests
    LocalNodeId = crypto:hash(sha256, <<"test_node">>),
    RoutingConfig = #{k => 20, alpha => 3},
    {ok, RoutingPid} = macula_routing_server:start_link(LocalNodeId, RoutingConfig),

    %% Add a peer node to routing table for forwarding tests
    PeerNodeId = crypto:hash(sha256, <<"peer_node">>),
    PeerNode = #{node_id => PeerNodeId, address => <<"127.0.0.1">>, port => 9001},
    ok = macula_routing_server:add_node(RoutingPid, PeerNode),

    #{
        routing_pid => RoutingPid,
        local_node_id => LocalNodeId,
        peer_node_id => PeerNodeId
    }.

cleanup(#{routing_pid := RoutingPid}) ->
    catch gen_server:stop(RoutingPid),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

test_wrap_call(_Fixture) ->
    SourceNodeId = crypto:hash(sha256, <<"source">>),
    DestNodeId = crypto:hash(sha256, <<"dest">>),

    CallMsg = #{
        call_id => crypto:strong_rand_bytes(16),
        procedure => <<"test.add">>,
        args => <<"{\"a\": 1, \"b\": 2}">>,
        realm => <<"test.realm">>
    },

    RpcRouteMsg = macula_rpc_routing:wrap_call(SourceNodeId, DestNodeId, CallMsg, 10),

    %% Verify envelope structure
    ?assertMatch(#{
        <<"destination_node_id">> := DestNodeId,
        <<"source_node_id">> := SourceNodeId,
        <<"hop_count">> := 0,
        <<"max_hops">> := 10,
        <<"payload_type">> := <<"call">>,
        <<"payload">> := CallMsg
    }, RpcRouteMsg).

test_wrap_reply(_Fixture) ->
    SourceNodeId = crypto:hash(sha256, <<"source">>),
    DestNodeId = crypto:hash(sha256, <<"dest">>),

    ReplyMsg = #{
        call_id => crypto:strong_rand_bytes(16),
        result => <<"{\"sum\": 3}">>
    },

    RpcRouteMsg = macula_rpc_routing:wrap_reply(SourceNodeId, DestNodeId, ReplyMsg, 10),

    %% Verify envelope structure
    ?assertMatch(#{
        <<"destination_node_id">> := DestNodeId,
        <<"source_node_id">> := SourceNodeId,
        <<"hop_count">> := 0,
        <<"max_hops">> := 10,
        <<"payload_type">> := <<"reply">>,
        <<"payload">> := ReplyMsg
    }, RpcRouteMsg).

test_should_deliver_locally(#{local_node_id := LocalNodeId}) ->
    %% Create rpc_route message destined for local node
    RpcRouteMsg = #{
        <<"destination_node_id">> => LocalNodeId,
        <<"source_node_id">> => crypto:hash(sha256, <<"source">>)
    },

    %% Should deliver locally
    ?assert(macula_rpc_routing:should_deliver_locally(LocalNodeId, RpcRouteMsg)),

    %% Create message destined for different node
    OtherNodeId = crypto:hash(sha256, <<"other">>),
    RpcRouteMsg2 = RpcRouteMsg#{<<"destination_node_id">> => OtherNodeId},

    %% Should NOT deliver locally
    ?assertNot(macula_rpc_routing:should_deliver_locally(LocalNodeId, RpcRouteMsg2)).

test_route_deliver_local(#{local_node_id := LocalNodeId, routing_pid := RoutingPid}) ->
    %% Create rpc_route message for local delivery
    CallMsg = #{
        call_id => crypto:strong_rand_bytes(16),
        procedure => <<"test.method">>,
        args => <<"{}">>
    },

    RpcRouteMsg = #{
        <<"destination_node_id">> => LocalNodeId,
        <<"source_node_id">> => crypto:hash(sha256, <<"source">>),
        <<"hop_count">> => 2,
        <<"max_hops">> => 10,
        <<"payload_type">> => <<"call">>,
        <<"payload">> => CallMsg
    },

    %% Route should deliver locally
    Result = macula_rpc_routing:route_or_deliver(LocalNodeId, RpcRouteMsg, RoutingPid),

    ?assertMatch({deliver, <<"call">>, CallMsg}, Result).

test_route_forward(#{local_node_id := LocalNodeId, peer_node_id := PeerNodeId, routing_pid := RoutingPid}) ->
    %% Create rpc_route message for remote delivery
    CallMsg = #{
        call_id => crypto:strong_rand_bytes(16),
        procedure => <<"test.method">>,
        args => <<"{}">>
    },

    RpcRouteMsg = #{
        <<"destination_node_id">> => PeerNodeId,
        <<"source_node_id">> => LocalNodeId,
        <<"hop_count">> => 0,
        <<"max_hops">> => 10,
        <<"payload_type">> => <<"call">>,
        <<"payload">> => CallMsg
    },

    %% Route should forward to next hop
    Result = macula_rpc_routing:route_or_deliver(LocalNodeId, RpcRouteMsg, RoutingPid),

    case Result of
        {forward, NextHop, UpdatedMsg} ->
            %% Verify next hop is a valid node
            ?assertMatch(#{node_id := _}, NextHop),
            %% Verify hop count incremented
            ?assertMatch(#{<<"hop_count">> := 1}, UpdatedMsg);
        Other ->
            ?debugFmt("Unexpected result: ~p", [Other]),
            ?assert(false)
    end.

test_max_hops(#{local_node_id := LocalNodeId, peer_node_id := PeerNodeId, routing_pid := RoutingPid}) ->
    %% Create rpc_route message with max hops exceeded
    CallMsg = #{
        call_id => crypto:strong_rand_bytes(16),
        procedure => <<"test.method">>,
        args => <<"{}">>
    },

    RpcRouteMsg = #{
        <<"destination_node_id">> => PeerNodeId,
        <<"source_node_id">> => LocalNodeId,
        <<"hop_count">> => 10,
        <<"max_hops">> => 10,
        <<"payload_type">> => <<"call">>,
        <<"payload">> => CallMsg
    },

    %% Route should reject with max_hops_exceeded
    Result = macula_rpc_routing:route_or_deliver(LocalNodeId, RpcRouteMsg, RoutingPid),

    ?assertEqual({error, max_hops_exceeded}, Result).
