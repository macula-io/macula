%%%-------------------------------------------------------------------
%%% @doc
%%% Macula SDK connection manager.
%%%
%%% Manages individual HTTP/3 (QUIC) connections to Macula mesh nodes.
%%% Handles connection lifecycle, authentication, message sending/receiving,
%%% and subscription management.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection).

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    stop/1,
    publish/3,
    publish/4,
    subscribe/3,
    unsubscribe/2,
    call/3,
    call/4
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    url :: binary(),
    opts :: map(),
    realm :: binary(),
    node_id :: binary(),

    %% QUIC connection and stream
    connection :: undefined | pid(),
    stream :: undefined | pid(),

    %% Connection state
    status :: connecting | connected | disconnected | error,

    %% Subscriptions: #{SubscriptionRef => {Topic, Callback}}
    subscriptions :: #{reference() => {binary(), fun((map()) -> ok)}},

    %% Pending RPC calls: #{CallId => {From, Timer}}
    pending_calls :: #{binary() => {term(), reference()}},

    %% Message ID counter
    msg_id_counter :: non_neg_integer(),

    %% Receive buffer for partial messages
    recv_buffer :: binary()
}).

-define(DEFAULT_TIMEOUT, 5000).
-define(CALL_TIMEOUT, 30000).
-define(CONNECT_RETRY_DELAY, 1000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start a client connection to a Macula mesh.
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Url, Opts) ->
    gen_server:start_link(?MODULE, {Url, Opts}, []).

%% @doc Stop the client connection.
-spec stop(pid()) -> ok.
stop(Client) ->
    gen_server:stop(Client).

%% @doc Publish an event through this client (no options).
-spec publish(pid(), binary(), map() | binary()) -> ok | {error, term()}.
publish(Client, Topic, Data) ->
    publish(Client, Topic, Data, #{}).

%% @doc Publish an event through this client with options.
-spec publish(pid(), binary(), map() | binary(), map()) -> ok | {error, term()}.
publish(Client, Topic, Data, Opts) ->
    gen_server:call(Client, {publish, Topic, Data, Opts}, ?DEFAULT_TIMEOUT).

%% @doc Subscribe to a topic through this client.
-spec subscribe(pid(), binary(), fun((map()) -> ok)) ->
    {ok, reference()} | {error, term()}.
subscribe(Client, Topic, Callback) ->
    gen_server:call(Client, {subscribe, Topic, Callback}, ?DEFAULT_TIMEOUT).

%% @doc Unsubscribe from a topic.
-spec unsubscribe(pid(), reference()) -> ok | {error, term()}.
unsubscribe(Client, SubRef) ->
    gen_server:call(Client, {unsubscribe, SubRef}, ?DEFAULT_TIMEOUT).

%% @doc Make an RPC call through this client (default timeout).
-spec call(pid(), binary(), map() | list()) -> {ok, term()} | {error, term()}.
call(Client, Procedure, Args) ->
    call(Client, Procedure, Args, #{}).

%% @doc Make an RPC call through this client with options.
-spec call(pid(), binary(), map() | list(), map()) ->
    {ok, term()} | {error, term()}.
call(Client, Procedure, Args, Opts) ->
    Timeout = maps:get(timeout, Opts, ?CALL_TIMEOUT),
    gen_server:call(Client, {call, Procedure, Args, Opts}, Timeout + 1000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Url, Opts}) ->
    %% Parse URL to extract host and port
    {Host, Port} = parse_url(Url),

    %% Get realm (required)
    Realm = case maps:get(realm, Opts, undefined) of
        undefined -> error({missing_required_option, realm});
        R when is_binary(R) -> R;
        R when is_list(R) -> list_to_binary(R);
        R when is_atom(R) -> atom_to_binary(R)
    end,

    %% Generate or get node ID
    NodeId = maps:get(node_id, Opts, generate_node_id()),

    State = #state{
        url = Url,
        opts = Opts#{host => Host, port => Port},
        realm = Realm,
        node_id = NodeId,
        connection = undefined,
        stream = undefined,
        status = connecting,
        subscriptions = #{},
        pending_calls = #{},
        msg_id_counter = 0,
        recv_buffer = <<>>
    },

    %% Initiate connection asynchronously
    self() ! connect,

    {ok, State}.

%% @private
handle_call({publish, Topic, Data, Opts}, _From, #state{status = connected} = State) ->
    %% Build publish message
    Qos = maps:get(qos, Opts, 0),
    Retain = maps:get(retain, Opts, false),

    {MsgId, State2} = next_message_id(State),

    %% Encode data to binary if it's a map
    Payload = case Data of
        D when is_binary(D) -> D;
        D when is_map(D) -> encode_json(D);
        D when is_list(D) -> list_to_binary(D)
    end,

    PublishMsg = #{
        topic => ensure_binary(Topic),
        payload => Payload,
        qos => Qos,
        retain => Retain,
        message_id => MsgId
    },

    %% Encode and send
    case send_message(publish, PublishMsg, State2) of
        ok ->
            {reply, ok, State2};
        {error, Reason} ->
            {reply, {error, Reason}, State2}
    end;

handle_call({publish, _Topic, _Data, _Opts}, _From, State) ->
    {reply, {error, not_connected}, State};

handle_call({subscribe, Topic, Callback}, _From, #state{status = connected} = State) ->
    %% Generate subscription reference
    SubRef = make_ref(),

    %% Send subscribe message
    SubscribeMsg = #{
        topics => [ensure_binary(Topic)],
        qos => 0
    },

    case send_message(subscribe, SubscribeMsg, State) of
        ok ->
            %% Store subscription
            Subscriptions = maps:put(SubRef, {ensure_binary(Topic), Callback},
                                    State#state.subscriptions),
            State2 = State#state{subscriptions = Subscriptions},
            {reply, {ok, SubRef}, State2};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({subscribe, _Topic, _Callback}, _From, State) ->
    {reply, {error, not_connected}, State};

handle_call({unsubscribe, SubRef}, _From, #state{status = connected} = State) ->
    case maps:get(SubRef, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_subscribed}, State};
        {Topic, _Callback} ->
            %% Send unsubscribe message
            UnsubscribeMsg = #{
                topics => [Topic]
            },

            case send_message(unsubscribe, UnsubscribeMsg, State) of
                ok ->
                    %% Remove subscription
                    Subscriptions = maps:remove(SubRef, State#state.subscriptions),
                    State2 = State#state{subscriptions = Subscriptions},
                    {reply, ok, State2};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({unsubscribe, _SubRef}, _From, State) ->
    {reply, {error, not_connected}, State};

handle_call({call, Procedure, Args, Opts}, From, #state{status = connected} = State) ->
    %% Generate call ID
    {CallId, State2} = next_message_id(State),

    %% Encode args
    EncodedArgs = case Args of
        A when is_binary(A) -> A;
        A when is_map(A) -> encode_json(A);
        A when is_list(A) -> encode_json(A)
    end,

    %% Build call message (RPC call)
    CallMsg = #{
        procedure => ensure_binary(Procedure),
        args => EncodedArgs,
        call_id => CallId
    },

    case send_message(call, CallMsg, State2) of
        ok ->
            %% Set up timeout timer
            Timeout = maps:get(timeout, Opts, ?CALL_TIMEOUT),
            Timer = erlang:send_after(Timeout, self(), {call_timeout, CallId}),

            %% Store pending call
            PendingCalls = maps:put(CallId, {From, Timer}, State2#state.pending_calls),
            State3 = State2#state{pending_calls = PendingCalls},

            {noreply, State3};
        {error, Reason} ->
            {reply, {error, Reason}, State2}
    end;

handle_call({call, _Procedure, _Args, _Opts}, _From, State) ->
    {reply, {error, not_connected}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(connect, State) ->
    case do_connect(State) of
        {ok, State2} ->
            {noreply, State2};
        {error, Reason} ->
            ?LOG_ERROR("Connection failed: ~p, retrying in ~p ms",
                      [Reason, ?CONNECT_RETRY_DELAY]),
            erlang:send_after(?CONNECT_RETRY_DELAY, self(), connect),
            {noreply, State#state{status = error}}
    end;

handle_info({quic, Data, Stream, _Props}, #state{stream = Stream} = State) ->
    %% Received data from QUIC stream
    handle_received_data(Data, State);

handle_info({call_timeout, CallId}, State) ->
    %% RPC call timed out
    case maps:get(CallId, State#state.pending_calls, undefined) of
        undefined ->
            {noreply, State};
        {From, _Timer} ->
            gen_server:reply(From, {error, timeout}),
            PendingCalls = maps:remove(CallId, State#state.pending_calls),
            {noreply, State#state{pending_calls = PendingCalls}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{stream = Stream, connection = Conn}) ->
    %% Clean up QUIC resources
    catch macula_quic:close(Stream),
    catch macula_quic:close(Conn),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Establish QUIC connection and perform handshake.
-spec do_connect(#state{}) -> {ok, #state{}} | {error, term()}.
do_connect(State) ->
    #{host := Host, port := Port} = State#state.opts,

    %% Connect via QUIC
    QuicOpts = [
        {alpn, ["macula"]},
        {verify, none}  %% TODO: Add proper TLS verification
    ],

    ConnectResult = try
        macula_quic:connect(Host, Port, QuicOpts, ?DEFAULT_TIMEOUT)
    catch
        _:Error ->
            {error, Error}
    end,

    case ConnectResult of
        {ok, Conn} ->
            %% Open bidirectional stream
            case macula_quic:open_stream(Conn) of
                {ok, Stream} ->
                    %% Send CONNECT message
                    ConnectMsg = #{
                        version => <<"1.0">>,
                        node_id => State#state.node_id,
                        realm_id => State#state.realm,
                        capabilities => [pubsub, rpc]
                    },

                    case send_message_raw(connect, ConnectMsg, Stream) of
                        ok ->
                            ?LOG_INFO("Connected to Macula mesh: ~s:~p", [Host, Port]),
                            {ok, State#state{
                                connection = Conn,
                                stream = Stream,
                                status = connected
                            }};
                        {error, Reason} ->
                            macula_quic:close(Stream),
                            macula_quic:close(Conn),
                            {error, {handshake_failed, Reason}}
                    end;
                {error, Reason} ->
                    macula_quic:close(Conn),
                    {error, {stream_open_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {connection_failed, Reason}};
        {error, Type, Details} ->
            {error, {connection_failed, {Type, Details}}};
        Other ->
            {error, {connection_failed, Other}}
    end.

%% @doc Send a protocol message through the established stream.
-spec send_message(atom(), map(), #state{}) -> ok | {error, term()}.
send_message(Type, Msg, #state{stream = Stream}) ->
    send_message_raw(Type, Msg, Stream).

%% @doc Send a protocol message through a stream (raw).
-spec send_message_raw(atom(), map(), pid()) -> ok | {error, term()}.
send_message_raw(Type, Msg, Stream) ->
    try
        Binary = macula_protocol_encoder:encode(Type, Msg),
        macula_quic:send(Stream, Binary)
    catch
        error:Reason ->
            {error, {encode_error, Reason}}
    end.

%% @doc Handle received data from the stream.
-spec handle_received_data(binary(), #state{}) -> {noreply, #state{}}.
handle_received_data(Data, State) ->
    %% Append to receive buffer
    Buffer = <<(State#state.recv_buffer)/binary, Data/binary>>,

    %% Try to decode messages
    {Messages, RemainingBuffer} = decode_messages(Buffer, []),

    %% Process each message
    State2 = lists:foldl(fun process_message/2, State, Messages),

    {noreply, State2#state{recv_buffer = RemainingBuffer}}.

%% @doc Decode all complete messages from buffer.
-spec decode_messages(binary(), list()) -> {list(), binary()}.
decode_messages(Buffer, Acc) when byte_size(Buffer) < 8 ->
    %% Not enough for header
    {lists:reverse(Acc), Buffer};
decode_messages(<<_Version:8, _TypeId:8, _Flags:8, _Reserved:8,
                  PayloadLen:32/big-unsigned, Rest/binary>> = Buffer, Acc) ->
    case byte_size(Rest) of
        ActualLen when ActualLen >= PayloadLen ->
            %% We have a complete message
            case macula_protocol_decoder:decode(Buffer) of
                {ok, {Type, Msg}} ->
                    %% Skip this message and continue
                    <<_:8/binary, _Payload:PayloadLen/binary, Remaining/binary>> = Buffer,
                    decode_messages(Remaining, [{Type, Msg} | Acc]);
                {error, _Reason} ->
                    %% Decode error, skip this message
                    <<_:8/binary, _Payload:PayloadLen/binary, Remaining/binary>> = Buffer,
                    decode_messages(Remaining, Acc)
            end;
        _ ->
            %% Incomplete message, wait for more data
            {lists:reverse(Acc), Buffer}
    end.

%% @doc Process a received message.
-spec process_message({atom(), map()}, #state{}) -> #state{}.
process_message({publish, Msg}, State) ->
    %% Handle incoming publish (for subscriptions)
    #{topic := Topic, payload := Payload} = Msg,

    %% Find matching subscriptions and invoke callbacks
    maps:fold(fun(_SubRef, {SubTopic, Callback}, St) ->
        case SubTopic of
            Topic ->
                %% Decode payload
                Event = decode_json(Payload),
                %% Invoke callback (spawn to avoid blocking)
                spawn(fun() -> Callback(Event) end);
            _ ->
                ok
        end,
        St
    end, State, State#state.subscriptions);

process_message({reply, Msg}, State) ->
    %% Handle RPC reply
    #{call_id := CallId, result := Result} = Msg,

    case maps:get(CallId, State#state.pending_calls, undefined) of
        undefined ->
            State;
        {From, Timer} ->
            erlang:cancel_timer(Timer),
            DecodedResult = decode_json(Result),
            gen_server:reply(From, {ok, DecodedResult}),
            State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)}
    end;

process_message({pong, _Msg}, State) ->
    %% TODO: Handle ping/pong for keepalive
    State;

process_message(_OtherMsg, State) ->
    %% Ignore unknown messages
    State.

%% @doc Parse URL to extract host and port.
-spec parse_url(binary()) -> {string(), inet:port_number()}.
parse_url(Url) when is_binary(Url) ->
    parse_url(binary_to_list(Url));
parse_url("https://" ++ Rest) ->
    parse_host_port(Rest, 443);
parse_url("http://" ++ Rest) ->
    parse_host_port(Rest, 80);
parse_url(Rest) ->
    parse_host_port(Rest, 443).

parse_host_port(HostPort, DefaultPort) ->
    case string:split(HostPort, ":") of
        [Host] ->
            {Host, DefaultPort};
        [Host, PortStr] ->
            Port = list_to_integer(string:trim(PortStr, trailing, "/")),
            {Host, Port}
    end.

%% @doc Generate a random node ID.
-spec generate_node_id() -> binary().
generate_node_id() ->
    %% 32-byte random ID
    crypto:strong_rand_bytes(32).

%% @doc Get next message ID.
-spec next_message_id(#state{}) -> {binary(), #state{}}.
next_message_id(State) ->
    Counter = State#state.msg_id_counter + 1,
    %% 16-byte message ID from counter
    MsgId = <<Counter:128>>,
    {MsgId, State#state{msg_id_counter = Counter}}.

%% @doc Ensure value is binary.
-spec ensure_binary(binary() | list() | atom()) -> binary().
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A).

%% @doc Encode map/list to JSON binary.
-spec encode_json(map() | list()) -> binary().
encode_json(Data) ->
    json:encode(Data).

%% @doc Decode JSON binary to map/list.
-spec decode_json(binary()) -> map() | list().
decode_json(Binary) ->
    json:decode(Binary).
