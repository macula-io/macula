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
    is_peer :: boolean(),                   %% true if this is a relay-to-relay peering connection
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
                is_peer = false, recv_buffer = <<>>, pending_calls = #{}}}.

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

%% Pub/sub: another handler published to a topic we're subscribed to
handle_info({relay_publish, Topic, Payload}, State) ->
    send_to_node(publish, #{
        <<"topic">> => Topic,
        <<"payload">> => Payload,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    }, State),
    {noreply, State};

%% RPC: another handler wants us to forward a call to our node
handle_info({relay_call, CallerPid, CallId, Procedure, Args}, State) ->
    %% Track: when our node replies, forward to CallerPid
    PendingCalls = maps:put(CallId, CallerPid, State#state.pending_calls),
    send_to_node(call, #{
        <<"call_id">> => CallId,
        <<"procedure">> => Procedure,
        <<"args">> => Args
    }, State),
    {noreply, State#state{pending_calls = PendingCalls}};

%% RPC reply from caller side: send reply back to the calling node
handle_info({relay_reply, CallId, Result}, State) ->
    send_to_node(reply, #{
        <<"call_id">> => CallId,
        <<"result">> => Result
    }, State),
    {noreply, State};

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

handle_info(Info, State) ->
    ?LOG_DEBUG("[relay_handler] Unhandled: ~p", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG_INFO("[relay_handler] Terminated: ~p (pg/gproc auto-cleanup)", [Reason]),
    %% pg and gproc automatically remove this process from all groups/registrations
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
    ?LOG_INFO("[relay_handler] ~s connected: realm=~s node=~s",
             [case IsPeer of true -> <<"Peer relay">>; false -> <<"Node">> end,
              Realm, binary:encode_hex(NodeId)]),
    send_to_node(pong, #{
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"server_time">> => erlang:system_time(millisecond)
    }, State),
    State#state{node_id = NodeId, is_peer = IsPeer};

%% SUBSCRIBE — join pg groups for topics
%% Client handlers join both {relay_topic, T} and {relay_local, T}.
%% Peer handlers join only {relay_topic, T} (prevents cross-relay loops).
handle_message({ok, {subscribe, Msg}}, #state{is_peer = IsPeer} = State) ->
    Topics = maps:get(<<"topics">>, Msg, []),
    lists:foreach(fun(Topic) ->
        pg:join(pg, {relay_topic, Topic}, self()),
        case IsPeer of
            false ->
                pg:join(pg, {relay_local, Topic}, self()),
                notify_peering(topic_subscribed, Topic);
            true  -> ok
        end,
        ?LOG_INFO("[relay_handler] Subscribed to ~s (peer=~p)", [Topic, IsPeer])
    end, Topics),
    State;

%% UNSUBSCRIBE — leave pg groups
handle_message({ok, {unsubscribe, Msg}}, State) ->
    Topics = maps:get(<<"topics">>, Msg, []),
    lists:foreach(fun(Topic) ->
        pg:leave(pg, {relay_topic, Topic}, self()),
        pg:leave(pg, {relay_local, Topic}, self())
    end, Topics),
    State;

%% PUBLISH — broadcast to all subscribers via pg
handle_message({ok, {publish, Msg}}, State) ->
    Topic = maps:get(<<"topic">>, Msg, <<>>),
    Payload = maps:get(<<"payload">>, Msg, <<>>),
    Members = try pg:get_members(pg, {relay_topic, Topic}) catch _:_ -> [] end,
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
    State;

%% CALL — find provider via gproc, forward to its handler
handle_message({ok, {call, Msg}}, State) ->
    Procedure = maps:get(<<"procedure">>, Msg, <<>>),
    CallId = maps:get(<<"call_id">>, Msg, <<>>),
    Args = maps:get(<<"args">>, Msg, #{}),
    case gproc:lookup_pids({p, l, {relay_rpc, Procedure}}) of
        [] ->
            ?LOG_WARNING("[relay_handler] No provider for RPC ~s", [Procedure]),
            send_to_node(reply, #{
                <<"call_id">> => CallId,
                <<"error">> => #{
                    <<"code">> => <<"procedure_not_found">>,
                    <<"message">> => <<"No node has registered this procedure">>
                }
            }, State);
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

%% Notify relay peering module (if running) about subscription changes.
%% Best-effort: if peering isn't running, silently ignore.
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
