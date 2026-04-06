%%%-------------------------------------------------------------------
%%% @doc Relay handler — one process per connected node.
%%%
%%% Manages a single QUIC stream to a node. Routes messages using:
%%% - pg groups for pub/sub (subscribe = join, publish = broadcast)
%%% - gproc for RPC (register = reg, call = lookup + forward)
%%%
%%% Cross-relay routing uses dual pg groups per topic:
%%% - `{relay_topic, Topic}' — ALL handlers (client + peer) join.
%%%   Used for local PUBLISH broadcasting. Peer handlers forward
%%%   messages to their remote relay.
%%% - `{relay_local, Topic}' — only CLIENT handlers join.
%%%   Used for cross-relay delivery (loop-safe). When a message
%%%   arrives from a peer relay, it is delivered ONLY to local
%%%   client handlers via this group.
%%%
%%% Peer vs client is determined by the CONNECT handshake: peers
%%% send `type => <<"relay">>'. This prevents pub/sub loops where
%%% a message would bounce between relays indefinitely.
%%%
%%% When this process dies (node disconnects), pg and gproc automatically
%%% remove all subscriptions and procedure registrations.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_handler).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    conn :: reference(),
    stream :: reference() | undefined,
    node_id :: binary(),
    node_name :: binary(),                  %% stable identity (e.g. "hecate@beam00.lab")
    is_peer :: boolean(),                   %% true if this is a relay-to-relay peering connection
    identified :: boolean(),                %% true after CONNECT handshake processed
    pending_msgs :: [term()],               %% messages received before CONNECT (replayed after)
    recv_buffer :: binary(),
    pending_calls :: #{binary() => pid()}  %% call_id => caller handler pid
}).

%%====================================================================
%% API
%%====================================================================

start_link(Conn, Stream) ->
    gen_server:start_link(?MODULE, {Conn, Stream}, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Conn, Stream}) ->
    %% Relay transfers ownership to us after start_link returns.
    %% We wait for the signal before setting active mode.
    ?LOG_INFO("[relay_handler] Waiting for stream ownership"),
    {ok, #state{conn = Conn, stream = Stream, node_id = <<>>,
                node_name = <<>>, is_peer = false, identified = false,
                pending_msgs = [], recv_buffer = <<>>, pending_calls = #{}}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%====================================================================
%% QUIC data received
%%====================================================================

handle_info(ownership_transferred, #state{stream = Stream} = State) ->
    quicer:setopt(Stream, active, true),
    ?LOG_INFO("[relay_handler] Ownership received, stream active"),
    {noreply, State};

handle_info({quic, Data, Stream, _Flags}, #state{stream = Stream} = State)
  when is_binary(Data) ->
    Buffer = <<(State#state.recv_buffer)/binary, Data/binary>>,
    {NewBuffer, State2} = process_buffer(Buffer, State),
    {noreply, State2#state{recv_buffer = NewBuffer}};

%% New stream from this connection (e.g., for additional data)
handle_info({quic, Data, NewStream, _Flags}, State) when is_binary(Data) ->
    quicer:setopt(NewStream, active, true),
    Buffer = <<(State#state.recv_buffer)/binary, Data/binary>>,
    {NewBuffer, State2} = process_buffer(Buffer, State),
    {noreply, State2#state{recv_buffer = NewBuffer, stream = NewStream}};

%%====================================================================
%% Inter-handler messages (from other relay_handler processes)
%%====================================================================

%% Pub/sub: another handler published to a topic we're subscribed to.
%% After processing, drain any queued RPC messages first (priority inversion fix).
handle_info({relay_publish, Topic, Payload}, State) ->
    send_to_node(publish, #{
        <<"topic">> => Topic,
        <<"payload">> => Payload,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    }, State),
    drain_rpc_messages(State);

%% RPC: another handler wants us to forward a call to our node
handle_info({relay_call, CallerPid, CallId, Procedure, Args}, State) ->
    State2 = handle_rpc_call(CallerPid, CallId, Procedure, Args, State),
    drain_rpc_messages(State2);

%% RPC reply from caller side: send reply back to the calling node
handle_info({relay_reply, CallId, Result}, State) ->
    State2 = handle_rpc_reply(CallId, Result, State),
    drain_rpc_messages(State2);

%%====================================================================
%% QUIC lifecycle
%%====================================================================

handle_info({quic, peer_send_shutdown, _Stream, _}, State) ->
    ?LOG_INFO("[relay_handler] Peer send shutdown, closing"),
    {stop, normal, State};

handle_info({quic, shutdown, _Ref, _Reason}, State) ->
    {stop, normal, State};

handle_info({quic, closed, _Ref, _Reason}, State) ->
    {stop, normal, State};

handle_info({quic, streams_available, _Conn, _Info}, State) ->
    {noreply, State};

handle_info({quic, peer_needs_streams, _Conn, _Info}, State) ->
    {noreply, State};

%% Monitor DOWN from spawn_monitor (RPC forwarding processes)
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{is_peer = false, node_name = NodeName}) when NodeName =/= undefined ->
    SiteId = get(relay_site_id),
    %% Publish node.down before pg auto-cleanup removes us from groups
    publish_system_event(<<"_mesh.node.down">>, #{
        name => NodeName,
        target_relay => get(relay_target),
        site_id => SiteId,
        reason => iolist_to_binary(io_lib:format("~p", [Reason]))
    }),
    %% If last node in site, publish site.down
    maybe_publish_site_down(SiteId),
    ?LOG_INFO("[relay_handler] Node ~s terminated: ~p", [NodeName, Reason]),
    ok;
terminate(Reason, _State) ->
    ?LOG_INFO("[relay_handler] Terminated: ~p", [Reason]),
    ok.

%%====================================================================
%% Message processing
%%====================================================================

%% Process complete messages from the receive buffer.
%% Returns {RemainingBuffer, UpdatedState}.
process_buffer(Buffer, State) when byte_size(Buffer) < 8 ->
    {Buffer, State};
process_buffer(<<_Version:8, _TypeId:8, _Flags:8, _Reserved:8,
                 PayloadLen:32/big-unsigned, Rest/binary>> = Buffer, State) ->
    case byte_size(Rest) >= PayloadLen of
        true ->
            MsgBytes = binary:part(Buffer, 0, 8 + PayloadLen),
            Remaining = binary:part(Buffer, 8 + PayloadLen, byte_size(Buffer) - 8 - PayloadLen),
            State2 = handle_message(macula_protocol_decoder:decode(MsgBytes), State),
            process_buffer(Remaining, State2);
        false ->
            {Buffer, State}
    end.

%%====================================================================
%% Protocol message handlers
%%====================================================================

%% CONNECT — node identifies itself
handle_message({ok, {connect, Msg}}, State) ->
    NodeId = maps:get(<<"node_id">>, Msg, <<>>),
    Realm = maps:get(<<"realm_id">>, Msg, <<>>),
    IsPeer = maps:get(<<"type">>, Msg, <<"node">>) =:= <<"relay">>,
    NodeName = maps:get(<<"node_name">>, Msg, binary:encode_hex(NodeId)),
    Identity = maps:get(<<"identity">>, Msg, #{}),
    TargetRelay = maps:get(<<"target_relay">>, Msg, <<>>),
    Site = maps:get(<<"site">>, Msg, undefined),
    SiteId = case Site of
        #{<<"site_id">> := Id} -> Id;
        _ -> undefined
    end,
    Label = case IsPeer of true -> <<"Peer relay">>; false -> <<"Node">> end,
    ?LOG_INFO("[relay_handler] ~s connected: realm=~s name=~s target=~s site=~s",
              [Label, Realm, NodeName, TargetRelay, SiteId]),
    register_client_node(IsPeer, NodeName, Identity, TargetRelay, Site, SiteId),
    %% Store peer URL for Bloom filter routing (peer relays only)
    case IsPeer of
        true ->
            Endpoint = maps:get(<<"endpoint">>, Msg, <<>>),
            put(relay_peer_url, Endpoint);
        false -> ok
    end,
    %% Publish system events (skip for peer relays)
    PreviousRelay = maps:get(<<"previous_relay">>, Msg, undefined),
    case IsPeer of
        false ->
            publish_system_event(<<"_mesh.node.up">>, #{
                name => NodeName, target_relay => TargetRelay,
                identity => Identity, realm => Realm,
                site => Site
            }),
            maybe_publish_site_up(SiteId, Site, TargetRelay, Realm),
            %% Reroute detection: node was on a different relay before failover
            case PreviousRelay of
                Prev when is_binary(Prev), Prev =/= <<>>, Prev =/= TargetRelay ->
                    publish_system_event(<<"_mesh.node.reroute">>, #{
                        name => NodeName,
                        from_relay => Prev,
                        to_relay => TargetRelay,
                        site => Site,
                        realm => Realm
                    });
                _ -> ok
            end;
        true -> ok
    end,
    send_to_node(pong, #{
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"server_time">> => erlang:system_time(millisecond)
    }, State),
    State2 = State#state{node_id = NodeId, node_name = NodeName,
                         is_peer = IsPeer, identified = true},
    replay_pending(State2);

%% Buffer messages received before CONNECT handshake.
%% Replayed after CONNECT sets is_peer correctly, preventing
%% the race where SUBSCRIBE joins wrong pg groups.
handle_message({ok, {Type, _}} = Msg, #state{identified = false, pending_msgs = Pending} = State)
  when Type =/= connect ->
    State#state{pending_msgs = [Msg | Pending]};

%% SUBSCRIBE — join pg groups for topics
%% Client handlers join both {relay_topic, T} and {relay_local, T}.
%% Peer handlers join only {relay_topic, T} (prevents cross-relay loops).
handle_message({ok, {subscribe, Msg}}, #state{is_peer = IsPeer} = State) ->
    Topics = maps:get(<<"topics">>, Msg, []),
    lists:foreach(fun(Topic) -> join_topic(Topic, IsPeer) end, Topics),
    State;

%% UNSUBSCRIBE — leave pg groups
handle_message({ok, {unsubscribe, Msg}}, State) ->
    Topics = maps:get(<<"topics">>, Msg, []),
    lists:foreach(fun(Topic) ->
        pg:leave(pg, {relay_topic, Topic}, self()),
        pg:leave(pg, {relay_local, Topic}, self())
    end, Topics),
    State;

%% PUBLISH on _relay.graph — merge into relay graph (QUIC-based graph propagation)
handle_message({ok, {publish, #{<<"topic">> := <<"_relay.graph">>, <<"payload">> := Payload}}}, State) ->
    case safe_decode(Payload) of
        Entries when is_list(Entries) ->
            ?LOG_DEBUG("[relay_handler] Graph update: ~b entries", [length(Entries)]),
            catch macula_relay_peering:merge_remote_graph(Entries);
        _ ->
            ?LOG_WARNING("[relay_handler] Invalid graph update payload")
    end,
    State;

%% Internal protocol topics — ONLY accepted from peer relay connections.
%% Nodes publishing to _swim.*, _dht.*, _relay.bloom are silently dropped.

handle_message({ok, {publish, #{<<"topic">> := <<"_swim.", _/binary>> = Topic,
                                <<"payload">> := Payload}}},
               #state{is_peer = true} = State) ->
    SwimType = case Topic of
        <<"_swim.ping">> -> ping;
        <<"_swim.ack">> -> ack;
        <<"_swim.ping_req">> -> ping_req;
        _ -> unknown
    end,
    case SwimType of
        unknown -> ok;
        _ -> handle_swim_msg(SwimType, Payload)
    end,
    State;

handle_message({ok, {publish, #{<<"topic">> := <<"_dht.", _/binary>> = Topic,
                                <<"payload">> := Payload}}},
               #state{is_peer = true} = State) ->
    catch macula_relay_dht:handle_dht_message(Topic, safe_decode(Payload)),
    State;

handle_message({ok, {publish, #{<<"topic">> := <<"_relay.bloom">>,
                                <<"payload">> := BloomBin}}},
               #state{is_peer = true} = State) when byte_size(BloomBin) =:= 1024 ->
    case get(relay_peer_url) of
        undefined -> ok;
        PeerUrl -> catch macula_relay_peering:receive_peer_bloom(PeerUrl, BloomBin)
    end,
    State;

%% Drop internal protocol messages from non-peer connections (nodes)
handle_message({ok, {publish, #{<<"topic">> := <<"_swim.", _/binary>>}}}, State) -> State;
handle_message({ok, {publish, #{<<"topic">> := <<"_dht.", _/binary>>}}}, State) -> State;
handle_message({ok, {publish, #{<<"topic">> := <<"_relay.bloom">>}}}, State) -> State;

%% PUBLISH — broadcast to all subscribers via pg
handle_message({ok, {publish, Msg}}, State) ->
    Topic = maps:get(<<"topic">>, Msg, <<>>),
    Payload = maps:get(<<"payload">>, Msg, <<>>),
    Members = pg_members({relay_topic, Topic}),
    ?LOG_INFO("[relay_handler] PUBLISH ~s → ~p subscriber(s)", [Topic, length(Members)]),
    lists:foreach(fun(Pid) ->
        case Pid =/= self() of
            true -> Pid ! {relay_publish, Topic, Payload};
            false -> ok
        end
    end, Members),
    State;

%% REGISTER_PROCEDURE — register in gproc for RPC routing
handle_message({ok, {register_procedure, Msg}}, State) ->
    Procedure = maps:get(<<"procedure">>, Msg, <<>>),
    %% Use property (not name) so multiple nodes can register same procedure
    try
        gproc:reg({p, l, {relay_rpc, Procedure}}),
        ?LOG_INFO("[relay_handler] Registered RPC procedure: ~s", [Procedure])
    catch
        _:_ -> ?LOG_DEBUG("[relay_handler] Procedure ~s already registered", [Procedure])
    end,
    %% Store in relay DHT so other relays can find this procedure
    SelfUrl = case get(relay_target) of
        undefined -> <<>>;
        Target -> <<"https://", Target/binary, ":4433">>
    end,
    catch macula_relay_dht:store_procedure(Procedure, SelfUrl),
    State;

%% CALL — find provider via gproc, forward to its handler.
%% If not found locally, try peer relays via the peering module.
handle_message({ok, {call, Msg}}, State) ->
    Procedure = maps:get(<<"procedure">>, Msg, <<>>),
    CallId = maps:get(<<"call_id">>, Msg, <<>>),
    Args = maps:get(<<"args">>, Msg, #{}),
    AlivePids = [P || P <- gproc:lookup_pids({p, l, {relay_rpc, Procedure}}),
                       is_process_alive(P)],
    case AlivePids of
        [] ->
            ?LOG_INFO("[relay_handler] RPC ~s not local, trying DHT then peers", [Procedure]),
            forward_rpc_via_dht(Procedure, CallId, Args, State);
        [ProviderPid | _] ->
            ?LOG_INFO("[relay_handler] Forwarding RPC ~s to provider ~p", [Procedure, ProviderPid]),
            ProviderPid ! {relay_call, self(), CallId, Procedure, Args}
    end,
    State;

%% REPLY — route back to the caller handler
handle_message({ok, {reply, Msg}}, State) ->
    CallId = maps:get(<<"call_id">>, Msg, <<>>),
    Result = maps:get(<<"result">>, Msg, maps:get(<<"error">>, Msg, #{})),
    case maps:get(CallId, State#state.pending_calls, undefined) of
        undefined ->
            ?LOG_DEBUG("[relay_handler] Reply for unknown call_id ~s", [CallId]);
        CallerPid ->
            CallerPid ! {relay_reply, CallId, Result}
    end,
    State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)};

%% PING — respond with PONG
handle_message({ok, {ping, Msg}}, State) ->
    Timestamp = maps:get(<<"timestamp">>, Msg, erlang:system_time(millisecond)),
    send_to_node(pong, #{
        <<"timestamp">> => Timestamp,
        <<"server_time">> => erlang:system_time(millisecond)
    }, State),
    State;

%% Catch-all for messages we don't handle
handle_message({ok, {Type, _Msg}}, State) ->
    ?LOG_DEBUG("[relay_handler] Ignoring message type: ~p", [Type]),
    State;

handle_message({error, Reason}, State) ->
    ?LOG_WARNING("[relay_handler] Decode error: ~p", [Reason]),
    State.

%%====================================================================
%% Send helper
%%====================================================================

%% Replay messages that arrived before CONNECT.
replay_pending(#state{pending_msgs = []} = State) -> State;
replay_pending(#state{pending_msgs = Msgs} = State) ->
    ?LOG_INFO("[relay_handler] Replaying ~b buffered message(s)", [length(Msgs)]),
    lists:foldl(fun(Msg, S) -> handle_message(Msg, S) end,
                State#state{pending_msgs = []},
                lists:reverse(Msgs)).

%% Register this handler as a connected client node (not peers).
%% Joins both the global group AND the target-relay-specific group
%% for multi-tenant relay support.
register_client_node(true, _NodeName, _Identity, _TargetRelay, _Site, _SiteId) -> ok;
register_client_node(false, NodeName, Identity, TargetRelay, Site, SiteId) ->
    put(relay_node_name, NodeName),
    put(relay_node_identity, Identity),
    put(relay_target, TargetRelay),
    put(relay_site, Site),
    put(relay_site_id, SiteId),
    pg:join(pg, relay_connected_nodes, self()),
    %% Target-specific group for multi-tenant node counting
    case TargetRelay of
        <<>> -> ok;
        _ -> pg:join(pg, {relay_connected_nodes, TargetRelay}, self())
    end,
    %% Site-specific group — tuple key avoids collision with target_relay groups
    case SiteId of
        undefined -> ok;
        _ -> pg:join(pg, {relay_connected_nodes, {site, SiteId}}, self())
    end.

%% Join topic pg groups. Clients join both groups; peers only relay_topic.
join_topic(Topic, false) ->
    pg:join(pg, {relay_topic, Topic}, self()),
    pg:join(pg, {relay_local, Topic}, self()),
    notify_peering(topic_subscribed, Topic),
    ?LOG_INFO("[relay_handler] Subscribed to ~s (peer=false)", [Topic]);
join_topic(Topic, true) ->
    pg:join(pg, {relay_topic, Topic}, self()),
    ?LOG_INFO("[relay_handler] Subscribed to ~s (peer=true)", [Topic]).

%% Forward RPC call to peer relays when procedure not found locally.
%% Spawns async — tries each peer, sends first success back to caller.
forward_rpc_via_dht(Procedure, CallId, Args, _State) ->
    HandlerPid = self(),
    spawn_monitor(fun() ->
        case catch macula_relay_dht:find_procedure(Procedure) of
            {ok, RelayUrl} ->
                ?LOG_INFO("[relay_handler] DHT found ~s on ~s", [Procedure, RelayUrl]),
                forward_rpc_to_relay(RelayUrl, Procedure, CallId, Args, HandlerPid);
            _ ->
                forward_rpc_to_peers_sync(Procedure, CallId, Args, HandlerPid)
        end
    end).

forward_rpc_to_relay(RelayUrl, Procedure, CallId, Args, HandlerPid) ->
    Clients = try gen_server:call(macula_relay_peering, peer_clients, 2000)
              catch _:_ -> #{} end,
    case maps:get(RelayUrl, Clients, undefined) of
        undefined ->
            %% DHT relay URL doesn't match any peer client (identity vs box URL mismatch).
            %% Fall through to sequential peer forwarding as fallback.
            ?LOG_INFO("[relay_handler] DHT relay ~s not in peer_clients, trying peers", [RelayUrl]),
            forward_rpc_to_peers_sync(Procedure, CallId, Args, HandlerPid);
        ClientPid ->
            case catch macula_relay_client:call(ClientPid, Procedure, Args, 4000) of
                {ok, Response} ->
                    HandlerPid ! {relay_reply, CallId, Response};
                _ ->
                    HandlerPid ! {relay_reply, CallId,
                        #{<<"error">> => #{<<"code">> => <<"rpc_failed">>,
                                           <<"message">> => <<"DHT relay did not respond">>}}}
            end
    end.

forward_rpc_to_peers_sync(Procedure, CallId, Args, HandlerPid) ->
    case erlang:whereis(macula_relay_peering) of
        undefined ->
            HandlerPid ! {relay_reply, CallId,
                #{<<"error">> => #{<<"code">> => <<"procedure_not_found">>,
                                   <<"message">> => <<"No peering available">>}}};
        PeeringPid ->
            Clients = gen_server:call(PeeringPid, peer_clients, 2000),
            Result = try_rpc_on_peers(Procedure, Args, maps:to_list(Clients)),
            case Result of
                {ok, Response} ->
                    HandlerPid ! {relay_reply, CallId, Response};
                {error, _} ->
                    HandlerPid ! {relay_reply, CallId,
                        #{<<"error">> => #{<<"code">> => <<"procedure_not_found">>,
                                           <<"message">> => <<"No provider on any relay">>}}}
            end
    end.


try_rpc_on_peers(_Procedure, _Args, []) ->
    {error, not_found};
try_rpc_on_peers(Procedure, Args, [{_Url, ClientPid} | Rest]) ->
    case catch macula_relay_client:call(ClientPid, Procedure, Args, 4000) of
        {ok, Response} -> {ok, Response};
        _ -> try_rpc_on_peers(Procedure, Args, Rest)
    end.

%% Notify relay peering module (if running) about subscription changes.
notify_peering(Event, Topic) ->
    case erlang:whereis(macula_relay_peering) of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, {Event, Topic})
    end.

send_to_node(Type, Msg, #state{stream = Stream}) ->
    Binary = macula_protocol_encoder:encode(Type, Msg),
    case macula_quic:async_send(Stream, Binary) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING("[relay_handler] Send ~p failed: ~p", [Type, Reason])
    end.

%% Publish _mesh.site.up if this is the first node of its site on this relay.
maybe_publish_site_up(undefined, _, _, _) -> ok;
maybe_publish_site_up(SiteId, Site, TargetRelay, Realm) ->
    Members = pg_members({relay_connected_nodes, {site, SiteId}}),
    case length(Members) of
        1 ->
            %% First node in this site — site is now online
            SitePayload = case Site of
                M when is_map(M) -> M;
                _ -> #{site_id => SiteId}
            end,
            publish_system_event(<<"_mesh.site.up">>,
                SitePayload#{target_relay => TargetRelay, realm => Realm});
        _ -> ok
    end.

%% Publish _mesh.site.down if this is the last node of its site on this relay.
maybe_publish_site_down(undefined) -> ok;
maybe_publish_site_down(SiteId) ->
    Members = pg_members({relay_connected_nodes, {site, SiteId}}),
    %% At terminate time, we're still in the group (pg removes after process exits)
    case length(Members) of
        1 ->
            Site = get(relay_site),
            SitePayload = case Site of
                M when is_map(M) -> M;
                _ -> #{site_id => SiteId}
            end,
            publish_system_event(<<"_mesh.site.down">>,
                SitePayload#{target_relay => get(relay_target)});
        _ -> ok
    end.

%% Publish a system event to all subscribers of a topic.
%% Used for _mesh.node.up/down, _mesh.site.up/down etc.
publish_system_event(Topic, Payload) ->
    BinPayload = iolist_to_binary(json:encode(
        Payload#{<<"timestamp">> => erlang:system_time(millisecond)})),
    Members = pg_members({relay_topic, Topic}),
    lists:foreach(fun(Pid) ->
        Pid ! {relay_publish, Topic, BinPayload}
    end, Members).

handle_swim_msg(ping, Payload) ->
    From = maps:get(<<"from">>, safe_decode(Payload), <<>>),
    catch macula_relay_swim:receive_ping(From, Payload),
    maybe_store_bloom_from_swim(Payload);
handle_swim_msg(ack, Payload) ->
    From = maps:get(<<"from">>, safe_decode(Payload), <<>>),
    catch macula_relay_swim:receive_ack(From, Payload),
    maybe_store_bloom_from_swim(Payload);
handle_swim_msg(ping_req, Payload) ->
    Decoded = safe_decode(Payload),
    From = maps:get(<<"from">>, Decoded, <<>>),
    Target = maps:get(<<"target">>, Decoded, <<>>),
    catch macula_relay_swim:receive_ping_req(From, Target, Payload).

maybe_store_bloom_from_swim(Payload) ->
    Decoded = safe_decode(Payload),
    From = maps:get(<<"from">>, Decoded, <<>>),
    case maps:get(<<"bloom">>, Decoded, undefined) of
        undefined -> ok;
        BloomB64 ->
            BloomBin = base64:decode(BloomB64),
            catch macula_relay_peering:receive_peer_bloom(From, BloomBin)
    end.

pg_members(Group) ->
    try pg:get_members(pg, Group) catch _:_ -> [] end.

safe_decode(Bin) when is_binary(Bin) ->
    try json:decode(Bin) catch _:_ -> #{} end;
safe_decode(Map) when is_map(Map) ->
    Map;
safe_decode(_) ->
    #{}.


%%====================================================================
%% RPC priority drain — process all queued CALL/REPLY before returning
%% to the gen_server loop. Prevents priority inversion where high-volume
%% PUBLISH messages starve RPC calls.
%%====================================================================

handle_rpc_call(CallerPid, CallId, Procedure, Args, State) ->
    PendingCalls = maps:put(CallId, CallerPid, State#state.pending_calls),
    send_to_node(call, #{
        <<"call_id">> => CallId,
        <<"procedure">> => Procedure,
        <<"args">> => Args
    }, State),
    State#state{pending_calls = PendingCalls}.

handle_rpc_reply(CallId, Result, State) ->
    send_to_node(reply, #{
        <<"call_id">> => CallId,
        <<"result">> => Result
    }, State),
    State.

drain_rpc_messages(State) ->
    receive
        {relay_call, CallerPid, CallId, Procedure, Args} ->
            State2 = handle_rpc_call(CallerPid, CallId, Procedure, Args, State),
            drain_rpc_messages(State2);
        {relay_reply, CallId, Result} ->
            State2 = handle_rpc_reply(CallId, Result, State),
            drain_rpc_messages(State2)
    after 0 ->
        {noreply, State}
    end.
