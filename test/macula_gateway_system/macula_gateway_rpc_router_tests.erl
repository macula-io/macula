%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_gateway_rpc_router module.
%%% Tests RPC routing message handling (routed CALL/REPLY processing).
%%%
%%% TDD Approach:
%%% 1. Write failing tests first
%%% 2. Implement minimal functionality
%%% 3. Make tests pass incrementally
%%% 4. Refactor for idiomatic Erlang
%%%
%%% Responsibilities:
%%% - Handle routed CALL messages delivered locally
%%% - Handle routed REPLY messages delivered locally
%%% - Send REPLY back via routing path
%%% - Forward rpc_route messages to next hop
%%% - Coordinate between RPC handler, mesh, and routing modules
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_rpc_router_tests).

-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks for mock servers
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% No setup needed - functional module (stateless)

%%%===================================================================
%%% Basic API Tests
%%%===================================================================

module_exports_test() ->
    Exports = macula_gateway_rpc_router:module_info(exports),

    ?assert(lists:member({handle_routed_call, 5}, Exports)),
    ?assert(lists:member({handle_routed_reply, 4}, Exports)),
    ?assert(lists:member({send_reply_via_routing, 4}, Exports)),
    ?assert(lists:member({forward_rpc_route, 3}, Exports)).

%%%===================================================================
%%% handle_routed_call Tests
%%%===================================================================

handle_routed_call_with_handler_found_test_() ->
    {setup,
     fun() ->
        %% Start mock RPC handler GenServer
        {ok, RpcPid} = start_mock_rpc_handler(),

        %% Start mock mesh manager
        {ok, MeshPid} = start_mock_mesh_manager(),

        #{rpc => RpcPid, mesh => MeshPid}
     end,
     fun(#{rpc := RpcPid, mesh := MeshPid}) ->
        catch gen_server:stop(RpcPid),
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(#{rpc := RpcPid, mesh := MeshPid}) ->
        %% Node ID
        NodeId = crypto:strong_rand_bytes(32),

        %% Create routed CALL message
        %% Include caller_did that owns the "test" namespace for authorization (v0.17.0+)
        CallMsg = #{
            <<"procedure">> => <<"test.echo">>,
            <<"args">> => <<"[{\"message\":\"hello\"}]">>,
            <<"call_id">> => <<"call_123">>,
            <<"caller_did">> => <<"did:macula:test">>
        },

        %% Create rpc_route envelope
        SourceNodeId = crypto:strong_rand_bytes(32),
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"call">>,
            <<"source_node_id">> => SourceNodeId,
            <<"destination_node_id">> => NodeId,
            <<"path">> => [SourceNodeId, NodeId],
            <<"message">> => CallMsg
        },

        %% Call handler
        Result = macula_gateway_rpc_router:handle_routed_call(
            CallMsg, RpcRouteMsg, NodeId, RpcPid, MeshPid
        ),

        %% Should return ok
        [?_assertMatch(ok, Result)]
     end}.

handle_routed_call_with_handler_not_found_test_() ->
    {setup,
     fun() ->
        %% Start mock RPC handler (returns not_found)
        {ok, RpcPid} = start_mock_rpc_handler_no_handler(),

        %% Start mock mesh manager
        {ok, MeshPid} = start_mock_mesh_manager(),

        #{rpc => RpcPid, mesh => MeshPid}
     end,
     fun(#{rpc := RpcPid, mesh := MeshPid}) ->
        catch gen_server:stop(RpcPid),
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(#{rpc := RpcPid, mesh := MeshPid}) ->
        %% Node ID
        NodeId = crypto:strong_rand_bytes(32),

        %% Create routed CALL message
        %% Include caller_did that owns the "test" namespace for authorization (v0.17.0+)
        CallMsg = #{
            <<"procedure">> => <<"test.unknown">>,
            <<"args">> => <<"[]">>,
            <<"call_id">> => <<"call_456">>,
            <<"caller_did">> => <<"did:macula:test">>
        },

        %% Create rpc_route envelope
        SourceNodeId = crypto:strong_rand_bytes(32),
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"call">>,
            <<"source_node_id">> => SourceNodeId,
            <<"destination_node_id">> => NodeId,
            <<"path">> => [SourceNodeId, NodeId],
            <<"message">> => CallMsg
        },

        %% Call handler
        Result = macula_gateway_rpc_router:handle_routed_call(
            CallMsg, RpcRouteMsg, NodeId, RpcPid, MeshPid
        ),

        %% Should return ok (error reply sent via routing)
        [?_assertMatch(ok, Result)]
     end}.

handle_routed_call_with_handler_error_test_() ->
    {setup,
     fun() ->
        %% Start mock RPC handler (returns handler that crashes)
        {ok, RpcPid} = start_mock_rpc_handler_crashes(),

        %% Start mock mesh manager
        {ok, MeshPid} = start_mock_mesh_manager(),

        #{rpc => RpcPid, mesh => MeshPid}
     end,
     fun(#{rpc := RpcPid, mesh := MeshPid}) ->
        catch gen_server:stop(RpcPid),
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(#{rpc := RpcPid, mesh := MeshPid}) ->
        %% Node ID
        NodeId = crypto:strong_rand_bytes(32),

        %% Create routed CALL message
        %% Include caller_did that owns the "test" namespace for authorization (v0.17.0+)
        CallMsg = #{
            <<"procedure">> => <<"test.crash">>,
            <<"args">> => <<"[]">>,
            <<"call_id">> => <<"call_789">>,
            <<"caller_did">> => <<"did:macula:test">>
        },

        %% Create rpc_route envelope
        SourceNodeId = crypto:strong_rand_bytes(32),
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"call">>,
            <<"source_node_id">> => SourceNodeId,
            <<"destination_node_id">> => NodeId,
            <<"path">> => [SourceNodeId, NodeId],
            <<"message">> => CallMsg
        },

        %% Call handler
        Result = macula_gateway_rpc_router:handle_routed_call(
            CallMsg, RpcRouteMsg, NodeId, RpcPid, MeshPid
        ),

        %% Should return ok (error reply sent)
        [?_assertMatch(ok, Result)]
     end}.

handle_routed_call_with_invalid_args_test_() ->
    {setup,
     fun() ->
        %% Start mock RPC handler
        {ok, RpcPid} = start_mock_rpc_handler(),

        %% Start mock mesh manager
        {ok, MeshPid} = start_mock_mesh_manager(),

        #{rpc => RpcPid, mesh => MeshPid}
     end,
     fun(#{rpc := RpcPid, mesh := MeshPid}) ->
        catch gen_server:stop(RpcPid),
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(#{rpc := RpcPid, mesh := MeshPid}) ->
        %% Node ID
        NodeId = crypto:strong_rand_bytes(32),

        %% Create routed CALL with invalid JSON args
        %% Include caller_did that owns the "test" namespace for authorization (v0.17.0+)
        CallMsg = #{
            <<"procedure">> => <<"test.echo">>,
            <<"args">> => <<"invalid json">>,
            <<"call_id">> => <<"call_999">>,
            <<"caller_did">> => <<"did:macula:test">>
        },

        %% Create rpc_route envelope
        SourceNodeId = crypto:strong_rand_bytes(32),
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"call">>,
            <<"source_node_id">> => SourceNodeId,
            <<"destination_node_id">> => NodeId,
            <<"path">> => [SourceNodeId, NodeId],
            <<"message">> => CallMsg
        },

        %% Call handler
        Result = macula_gateway_rpc_router:handle_routed_call(
            CallMsg, RpcRouteMsg, NodeId, RpcPid, MeshPid
        ),

        %% Should return ok (error reply sent)
        [?_assertMatch(ok, Result)]
     end}.

%%%===================================================================
%%% handle_routed_reply Tests
%%%===================================================================

handle_routed_reply_for_local_node_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Node ID
        NodeId = crypto:strong_rand_bytes(32),

        %% Create mock connection process that expects reply
        Parent = self(),
        ConnPid = spawn(fun() ->
            receive
                {rpc_reply, ReplyMsg} ->
                    Parent ! {got_reply, ReplyMsg}
            after 1000 ->
                Parent ! {got_reply, timeout}
            end
        end),

        %% Register connection via gproc (simulating client connection)
        CallId = <<"call_local_123">>,
        try gproc:reg({n, l, {rpc_call, CallId}}, ConnPid) catch _:_ -> ok end,

        %% Create REPLY message
        ReplyMsg = #{
            <<"call_id">> => CallId,
            <<"result">> => <<"{\"status\":\"ok\"}">>
        },

        %% Create rpc_route envelope
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"reply">>,
            <<"destination_node_id">> => NodeId,
            <<"message">> => ReplyMsg
        },

        %% Empty client_streams (not needed for local node)
        ClientStreams = #{},

        %% Call handler
        Result = macula_gateway_rpc_router:handle_routed_reply(
            ReplyMsg, RpcRouteMsg, NodeId, ClientStreams
        ),

        %% Clean up
        catch gproc:unreg({n, l, {rpc_call, CallId}}),

        %% Should return ok
        [?_assertMatch(ok, Result)]
     end}.

handle_routed_reply_for_remote_client_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Node ID
        NodeId = crypto:strong_rand_bytes(32),

        %% Create mock stream process
        Parent = self(),
        StreamPid = spawn(fun() ->
            receive
                Message -> Parent ! {stream_received, Message}
            after 1000 -> Parent ! {stream_received, timeout}
            end
        end),

        %% Create REPLY message
        ConnectionId = <<"conn_123">>,
        ReplyMsg = #{
            <<"call_id">> => <<"call_remote_456">>,
            <<"result">> => <<"{\"data\":\"test\"}">>
        },

        %% Create rpc_route envelope (destination is not local node)
        RemoteNodeId = crypto:strong_rand_bytes(32),
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"reply">>,
            <<"destination_node_id">> => RemoteNodeId,
            <<"connection_id">> => ConnectionId,
            <<"message">> => ReplyMsg
        },

        %% Client streams with our connection
        ClientStreams = #{ConnectionId => StreamPid},

        %% Call handler
        Result = macula_gateway_rpc_router:handle_routed_reply(
            ReplyMsg, RpcRouteMsg, NodeId, ClientStreams
        ),

        %% Should return ok
        [?_assertMatch(ok, Result)]
     end}.

handle_routed_reply_client_not_found_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
        %% Node ID
        NodeId = crypto:strong_rand_bytes(32),

        %% Create REPLY message
        ReplyMsg = #{
            <<"call_id">> => <<"call_missing_789">>,
            <<"result">> => <<"{}">>
        },

        %% Create rpc_route envelope
        RemoteNodeId = crypto:strong_rand_bytes(32),
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"reply">>,
            <<"destination_node_id">> => RemoteNodeId,
            <<"connection_id">> => <<"conn_missing">>,
            <<"message">> => ReplyMsg
        },

        %% Empty client_streams
        ClientStreams = #{},

        %% Call handler
        Result = macula_gateway_rpc_router:handle_routed_reply(
            ReplyMsg, RpcRouteMsg, NodeId, ClientStreams
        ),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

%%%===================================================================
%%% send_reply_via_routing Tests
%%%===================================================================

send_reply_via_routing_local_destination_test_() ->
    {setup,
     fun() ->
        %% Start mock mesh manager
        {ok, MeshPid} = start_mock_mesh_manager(),
        MeshPid
     end,
     fun(MeshPid) ->
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(MeshPid) ->
        %% Node IDs
        NodeId = crypto:strong_rand_bytes(32),
        DestNodeId = NodeId,  % Same as local node

        %% Create REPLY message
        ReplyMsg = #{
            <<"call_id">> => <<"call_local_send">>,
            <<"result">> => <<"{\"status\":\"success\"}">>
        },

        %% Call send_reply_via_routing
        Result = macula_gateway_rpc_router:send_reply_via_routing(
            ReplyMsg, DestNodeId, NodeId, MeshPid
        ),

        %% Should return ok (no routing needed for local)
        [?_assertMatch(ok, Result)]
     end}.

send_reply_via_routing_remote_destination_test_() ->
    {setup,
     fun() ->
        %% Start mock mesh manager
        {ok, MeshPid} = start_mock_mesh_manager(),
        MeshPid
     end,
     fun(MeshPid) ->
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(MeshPid) ->
        %% Node IDs
        NodeId = crypto:strong_rand_bytes(32),
        DestNodeId = crypto:strong_rand_bytes(32),  % Different node

        %% Create REPLY message
        ReplyMsg = #{
            <<"call_id">> => <<"call_remote_send">>,
            <<"result">> => <<"{\"value\":42}">>
        },

        %% Call send_reply_via_routing
        Result = macula_gateway_rpc_router:send_reply_via_routing(
            ReplyMsg, DestNodeId, NodeId, MeshPid
        ),

        %% Should return ok (routes via mesh)
        [?_assertMatch(ok, Result)]
     end}.

send_reply_via_routing_with_error_test_() ->
    {setup,
     fun() ->
        %% Start mock mesh manager that fails
        {ok, MeshPid} = start_mock_mesh_manager_fails(),
        MeshPid
     end,
     fun(MeshPid) ->
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(MeshPid) ->
        %% Node IDs
        NodeId = crypto:strong_rand_bytes(32),
        DestNodeId = crypto:strong_rand_bytes(32),

        %% Create REPLY message
        ReplyMsg = #{
            <<"call_id">> => <<"call_error_send">>,
            <<"result">> => <<"{}">>
        },

        %% Call send_reply_via_routing
        Result = macula_gateway_rpc_router:send_reply_via_routing(
            ReplyMsg, DestNodeId, NodeId, MeshPid
        ),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

%%%===================================================================
%%% forward_rpc_route Tests
%%%===================================================================

forward_rpc_route_success_test_() ->
    {setup,
     fun() ->
        %% Start mock mesh manager
        {ok, MeshPid} = start_mock_mesh_manager(),
        MeshPid
     end,
     fun(MeshPid) ->
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(MeshPid) ->
        %% Next hop node info
        NextHopNodeId = crypto:strong_rand_bytes(32),
        NextHopNodeInfo = #{
            <<"node_id">> => NextHopNodeId,
            <<"address">> => <<"192.168.1.100">>,
            <<"port">> => 4242
        },

        %% Create rpc_route message
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"call">>,
            <<"source_node_id">> => crypto:strong_rand_bytes(32),
            <<"destination_node_id">> => crypto:strong_rand_bytes(32),
            <<"path">> => [],
            <<"message">> => #{}
        },

        %% Call forward_rpc_route
        Result = macula_gateway_rpc_router:forward_rpc_route(
            NextHopNodeInfo, RpcRouteMsg, MeshPid
        ),

        %% Should return ok
        [?_assertMatch(ok, Result)]
     end}.

forward_rpc_route_connection_error_test_() ->
    {setup,
     fun() ->
        %% Start mock mesh manager that fails connections
        {ok, MeshPid} = start_mock_mesh_manager_fails(),
        MeshPid
     end,
     fun(MeshPid) ->
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(MeshPid) ->
        %% Next hop node info
        NextHopNodeId = crypto:strong_rand_bytes(32),
        NextHopNodeInfo = #{
            <<"node_id">> => NextHopNodeId,
            <<"address">> => <<"192.168.1.100">>,
            <<"port">> => 4242
        },

        %% Create rpc_route message
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"call">>,
            <<"source_node_id">> => crypto:strong_rand_bytes(32),
            <<"destination_node_id">> => crypto:strong_rand_bytes(32),
            <<"path">> => [],
            <<"message">> => #{}
        },

        %% Call forward_rpc_route
        Result = macula_gateway_rpc_router:forward_rpc_route(
            NextHopNodeInfo, RpcRouteMsg, MeshPid
        ),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

forward_rpc_route_with_malformed_node_info_test_() ->
    {setup,
     fun() ->
        %% Start mock mesh manager
        {ok, MeshPid} = start_mock_mesh_manager(),
        MeshPid
     end,
     fun(MeshPid) ->
        catch gen_server:stop(MeshPid),
        ok
     end,
     fun(MeshPid) ->
        %% Malformed node info (missing fields)
        MalformedNodeInfo = #{
            <<"invalid">> => <<"data">>
        },

        %% Create rpc_route message
        RpcRouteMsg = #{
            <<"type">> => <<"rpc_route">>,
            <<"message_type">> => <<"call">>,
            <<"message">> => #{}
        },

        %% Call forward_rpc_route
        Result = macula_gateway_rpc_router:forward_rpc_route(
            MalformedNodeInfo, RpcRouteMsg, MeshPid
        ),

        %% Should return error
        [?_assertMatch({error, _}, Result)]
     end}.

%%%===================================================================
%%% Helper Functions - Mock Servers
%%%===================================================================

%% Mock RPC handler that finds handlers
start_mock_rpc_handler() ->
    gen_server:start_link(?MODULE, {rpc_handler, found}, []).

%% Mock RPC handler that returns not_found
start_mock_rpc_handler_no_handler() ->
    gen_server:start_link(?MODULE, {rpc_handler, not_found}, []).

%% Mock RPC handler that returns crashing handler
start_mock_rpc_handler_crashes() ->
    gen_server:start_link(?MODULE, {rpc_handler, crashes}, []).

%% Mock mesh manager
start_mock_mesh_manager() ->
    gen_server:start_link(?MODULE, {mesh_manager, success}, []).

%% Mock mesh manager that fails
start_mock_mesh_manager_fails() ->
    gen_server:start_link(?MODULE, {mesh_manager, fails}, []).

%%%===================================================================
%%% gen_server callbacks for mocks
%%%===================================================================

init({rpc_handler, Mode}) ->
    {ok, {rpc_handler, Mode}};
init({mesh_manager, Mode}) ->
    {ok, {mesh_manager, Mode}}.

handle_call({get_handler, _Procedure}, _From, {rpc_handler, found} = State) ->
    %% Return mock handler function
    Handler = fun(Args) ->
        #{result => <<"echo: ", Args/binary>>}
    end,
    {reply, {ok, Handler}, State};

handle_call({get_handler, _Procedure}, _From, {rpc_handler, not_found} = State) ->
    {reply, not_found, State};

handle_call({get_handler, _Procedure}, _From, {rpc_handler, crashes} = State) ->
    %% Return handler that crashes
    Handler = fun(_Args) ->
        error(intentional_crash)
    end,
    {reply, {ok, Handler}, State};

handle_call({get_or_create_connection, _NodeId, _Address}, _From, {mesh_manager, success} = State) ->
    %% Return mock stream
    Stream = spawn(fun() -> receive _ -> ok end end),
    {reply, {ok, Stream}, State};

handle_call({get_or_create_connection, _NodeId, _Address}, _From, {mesh_manager, fails} = State) ->
    {reply, {error, connection_failed}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
