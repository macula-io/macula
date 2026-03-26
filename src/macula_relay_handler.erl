%%%-------------------------------------------------------------------
%%% @doc Relay handler — one process per connected node.
%%%
%%% Manages a single QUIC stream to a node. Routes messages using:
%%% - pg groups for pub/sub (subscribe = join, publish = broadcast)
%%% - gproc for RPC (register = reg, call = lookup + forward)
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
    %% Stream already accepted by relay via async_accept_stream
    quicer:setopt(Stream, active, true),
    ?LOG_INFO("[relay_handler] Started with stream"),
    {ok, #state{conn = Conn, stream = Stream, node_id = <<>>,
                recv_buffer = <<>>, pending_calls = #{}}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%====================================================================
%% QUIC data received
%%====================================================================

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
    ?LOG_INFO("[relay_handler] Node connected: realm=~s node=~s",
             [Realm, binary:encode_hex(NodeId)]),
    %% Send PONG as handshake acknowledgment
    send_to_node(pong, #{
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"server_time">> => erlang:system_time(millisecond)
    }, State),
    State#state{node_id = NodeId};

%% SUBSCRIBE — join pg groups for topics
handle_message({ok, {subscribe, Msg}}, State) ->
    Topics = maps:get(<<"topics">>, Msg, []),
    lists:foreach(fun(Topic) ->
        pg:join(pg, {relay_topic, Topic}, self()),
        ?LOG_INFO("[relay_handler] Subscribed to ~s", [Topic])
    end, Topics),
    State;

%% UNSUBSCRIBE — leave pg groups
handle_message({ok, {unsubscribe, Msg}}, State) ->
    Topics = maps:get(<<"topics">>, Msg, []),
    lists:foreach(fun(Topic) ->
        pg:leave(pg, {relay_topic, Topic}, self())
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

send_to_node(Type, Msg, #state{stream = Stream}) ->
    Binary = macula_protocol_encoder:encode(Type, Msg),
    case macula_quic:async_send(Stream, Binary) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING("[relay_handler] Send ~p failed: ~p", [Type, Reason])
    end.
