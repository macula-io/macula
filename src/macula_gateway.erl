%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Gateway - HTTP/3 Message Router
%%%
%%% Main API module for the Macula Gateway.
%%% The gateway can be embedded in applications or run standalone.
%%%
%%% Architecture:
%%% - QUIC Listener: Accepts HTTP/3 connections from SDK clients
%%% - Router: Routes pub/sub messages between clients
%%% - RPC: Handles remote procedure calls
%%% - Realm Manager: Manages multiple realms
%%%
%%% Usage (Embedded):
%%% ```
%%% {ok, Pid} = macula_gateway:start_link([
%%%     {port, 9443},
%%%     {realm, <<"com.example.realm">>}
%%% ]).
%%% '''
%%%
%%% Usage (Standalone):
%%% ```
%%% application:start(macula_gateway).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    get_stats/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    port :: inet:port_number(),
    realm :: binary(),
    listener :: pid() | undefined,
    clients :: #{pid() => client_info()},
    subscriptions :: #{binary() => [pid()]},  % topic => [client_pids]
    registrations :: #{binary() => pid()}     % procedure => client_pid
}).

-type client_info() :: #{
    realm := binary(),
    node_id := binary(),
    capabilities := [atom()]
}.

-define(DEFAULT_PORT, 9443).
-define(DEFAULT_REALM, <<"macula.default">>).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the gateway with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the gateway with custom options.
%% Options:
%%   {port, Port} - Listen port (default: 9443)
%%   {realm, Realm} - Default realm (default: "macula.default")
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the gateway.
-spec stop(pid()) -> ok.
stop(Gateway) ->
    gen_server:stop(Gateway).

%% @doc Get gateway statistics.
-spec get_stats(pid()) -> map().
get_stats(Gateway) ->
    gen_server:call(Gateway, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
    Realm = proplists:get_value(realm, Opts, ?DEFAULT_REALM),

    %% Get TLS certificates
    %% Priority: 1. Environment variables (production)
    %%           2. Pre-generated certs (development)
    {CertFile, KeyFile} = get_tls_certificates(),

    io:format("Using TLS certificates:~n"),
    io:format("  Cert: ~s~n", [CertFile]),
    io:format("  Key:  ~s~n", [KeyFile]),

    %% Validate certificate files exist
    case macula_quic_cert:validate_files(CertFile, KeyFile) of
        ok ->
            start_quic_listener(Port, Realm, CertFile, KeyFile);
        {error, Reason} ->
            io:format("Certificate validation failed: ~p~n", [Reason]),
            {stop, {cert_validation_failed, Reason}}
    end.

%% @private
%% @doc Get TLS certificate paths from environment or use pre-generated ones.
get_tls_certificates() ->
    case {os:getenv("TLS_CERT_FILE"), os:getenv("TLS_KEY_FILE")} of
        {false, false} ->
            %% No env vars, use pre-generated certs
            io:format("Using pre-generated TLS certificates~n"),
            {"/opt/macula/certs/cert.pem", "/opt/macula/certs/key.pem"};
        {CertEnv, KeyEnv} when CertEnv =/= false andalso KeyEnv =/= false ->
            %% Use mounted certificates (production)
            io:format("Using mounted TLS certificates from environment~n"),
            {CertEnv, KeyEnv};
        _ ->
            %% Partial configuration, log warning and use defaults
            io:format("WARNING: Partial TLS environment config, using pre-generated certs~n"),
            {"/opt/macula/certs/cert.pem", "/opt/macula/certs/key.pem"}
    end.

%% @private
%% @doc Start the QUIC listener with given certificates.
start_quic_listener(Port, Realm, CertFile, KeyFile) ->
    %% Start QUIC listener
    ListenOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {peer_unidi_stream_count, 3},
        {peer_bidi_stream_count, 100}  % Allow clients to create bidirectional streams
    ],

    case macula_quic:listen(Port, ListenOpts) of
        {ok, Listener} ->
            io:format("Macula Gateway listening on port ~p (realm: ~s)~n", [Port, Realm]),

            %% Mark health server as ready
            try
                macula_gateway_health:set_ready(true)
            catch
                _:_ -> ok  % Health server might not be running in embedded mode
            end,

            %% Register diagnostics procedures
            try
                macula_gateway_diagnostics:register_procedures(self())
            catch
                _:_ -> ok  % Diagnostics service might not be running in embedded mode
            end,

            %% Start accepting connections
            self() ! accept,

            State = #state{
                port = Port,
                realm = Realm,
                listener = Listener,
                clients = #{},
                subscriptions = #{},
                registrations = #{}
            },

            {ok, State};

        {error, Reason} ->
            io:format("QUIC listen failed: ~p~n", [Reason]),
            {stop, {listen_failed, Reason}};

        {error, Type, Details} ->
            io:format("QUIC listen failed: ~p ~p~n", [Type, Details]),
            {stop, {listen_failed, {Type, Details}}};

        Other ->
            io:format("QUIC listen unexpected result: ~p~n", [Other]),
            {stop, {listen_failed, Other}}
    end.

handle_call(get_stats, _From, State) ->
    Stats = #{
        port => State#state.port,
        realm => State#state.realm,
        clients => maps:size(State#state.clients),
        subscriptions => maps:size(State#state.subscriptions),
        registrations => maps:size(State#state.registrations)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(accept, #state{listener = Listener} = State) ->
    AcceptResult = macula_quic:accept(Listener, 5000),
    handle_accept_result(AcceptResult, State);

%% Handle new stream from peer - PRIMARY handler for direct stream acceptance
handle_info({quic, new_stream, Stream, Props}, State) ->
    io:format("[Gateway] ========================================~n"),
    io:format("[Gateway] NEW STREAM RECEIVED!~n"),
    io:format("[Gateway] Stream: ~p~n", [Stream]),
    io:format("[Gateway] Props: ~p~n", [Props]),
    io:format("[Gateway] ========================================~n"),

    %% Extract Conn from Props if available
    Conn = maps:get(conn, Props, undefined),

    %% Enable active mode for stream
    case quicer:setopt(Stream, active, true) of
        ok ->
            io:format("[Gateway] Enabled active mode for stream~n");
        {error, SetOptErr} ->
            io:format("[Gateway] WARNING: Failed to set active mode: ~p~n", [SetOptErr])
    end,

    %% Re-register for next stream on this connection if we have Conn ref
    case Conn of
        undefined ->
            io:format("[Gateway] WARNING: No conn reference in Props, cannot re-register~n");
        _ ->
            StreamOpts = #{active => true},
            case quicer:async_accept_stream(Conn, StreamOpts) of
                {ok, Conn} ->
                    io:format("[Gateway] Re-registered for next stream on connection~n");
                {error, RegErr} ->
                    io:format("[Gateway] WARNING: Failed to re-register for streams: ~p~n", [RegErr])
            end
    end,

    {noreply, State};

%% Handle data from QUIC stream (active mode)
handle_info({quic, Data, Stream, _Flags}, State) when is_binary(Data) ->
    io:format("[Gateway] ===== Received ~p bytes from stream ~p =====~n", [byte_size(Data), Stream]),
    io:format("[Gateway] Raw data (first 100 bytes): ~p~n", [binary:part(Data, 0, min(100, byte_size(Data)))]),
    DecodeResult = macula_protocol_decoder:decode(Data),
    handle_decoded_message(DecodeResult, Stream, State);

%% Client registered
handle_info({client_connected, ClientPid, ClientInfo}, State) ->
    io:format("Client connected: ~p~n", [ClientInfo]),

    %% Monitor client
    erlang:monitor(process, ClientPid),

    Clients = maps:put(ClientPid, ClientInfo, State#state.clients),
    {noreply, State#state{clients = Clients}};

%% Client disconnected
handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
    io:format("Client disconnected: ~p~n", [ClientPid]),

    %% Remove client from all subscriptions
    Subscriptions = maps:map(fun(_Topic, Subscribers) ->
        lists:delete(ClientPid, Subscribers)
    end, State#state.subscriptions),

    %% Remove client registrations
    Registrations = maps:filter(fun(_Proc, Pid) ->
        Pid =/= ClientPid
    end, State#state.registrations),

    %% Remove client
    Clients = maps:remove(ClientPid, State#state.clients),

    {noreply, State#state{
        clients = Clients,
        subscriptions = Subscriptions,
        registrations = Registrations
    }};

%% Publish message (from client)
handle_info({publish, FromPid, Topic, Payload}, State) ->
    %% Get all subscribers to this topic
    Subscribers = maps:get(Topic, State#state.subscriptions, []),

    %% Send to all subscribers (except sender)
    [SubPid ! {event, Topic, Payload} || SubPid <- Subscribers, SubPid =/= FromPid],

    {noreply, State};

%% Subscribe request
handle_info({subscribe, ClientPid, Topic}, State) ->
    %% Add client to topic subscribers
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    NewSubscribers = [ClientPid | Subscribers],
    Subscriptions = maps:put(Topic, NewSubscribers, State#state.subscriptions),

    %% Acknowledge subscription
    ClientPid ! {subscribed, Topic},

    {noreply, State#state{subscriptions = Subscriptions}};

%% Unsubscribe request
handle_info({unsubscribe, ClientPid, Topic}, State) ->
    %% Remove client from topic subscribers
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    NewSubscribers = lists:delete(ClientPid, Subscribers),
    Subscriptions = maps:put(Topic, NewSubscribers, State#state.subscriptions),

    %% Acknowledge unsubscription
    ClientPid ! {unsubscribed, Topic},

    {noreply, State#state{subscriptions = Subscriptions}};

%% RPC Call request
handle_info({call, FromPid, CallId, Procedure, Args}, State) ->
    %% Find registered handler for procedure
    case maps:get(Procedure, State#state.registrations, undefined) of
        undefined ->
            %% No handler registered
            FromPid ! {call_error, CallId, <<"wamp.error.no_such_procedure">>};
        HandlerPid ->
            %% Forward to handler
            HandlerPid ! {invoke, FromPid, CallId, Procedure, Args}
    end,

    {noreply, State};

%% QUIC control event: peer_needs_streams
handle_info({quic, peer_needs_streams, _Conn, _StreamType}, State) ->
    %% Peer is signaling it wants to open more streams (bidi_streams or uni_streams)
    %% This is normal - just acknowledge
    {noreply, State};

%% QUIC event: new_conn (connection established)
handle_info({quic, new_conn, Conn}, State) ->
    io:format("[Gateway] QUIC event: new_conn ~p~n", [Conn]),
    {noreply, State};

%% QUIC event: shutdown (connection shutting down)
handle_info({quic, shutdown, Conn, Reason}, State) ->
    io:format("[Gateway] QUIC shutdown: Conn=~p, Reason=~p~n", [Conn, Reason]),
    {noreply, State};

%% QUIC event: transport_shutdown (transport layer shutting down)
handle_info({quic, transport_shutdown, Conn, Reason}, State) ->
    io:format("[Gateway] QUIC transport_shutdown: Conn=~p, Reason=~p~n", [Conn, Reason]),
    {noreply, State};

%% DHT query (find_node, find_value, store)
handle_info({dht_query, FromPid, _QueryType, QueryData}, State) ->
    %% Decode the query message and handle via routing server
    case macula_protocol_decoder:decode(QueryData) of
        {ok, {MessageType, Message}} ->
            %% Forward to DHT routing server
            Reply = macula_routing_server:handle_message(macula_routing_server, Message),

            %% Encode reply based on message type
            ReplyData = case MessageType of
                find_node ->
                    macula_protocol_encoder:encode(find_node_reply, Reply);
                find_value ->
                    macula_protocol_encoder:encode(find_value_reply, Reply);
                store ->
                    macula_protocol_encoder:encode(store, Reply);
                _ ->
                    macula_protocol_encoder:encode(reply, #{error => <<"Unknown DHT message type">>})
            end,

            %% Send reply back to client
            FromPid ! {dht_reply, ReplyData};

        {error, Reason} ->
            io:format("DHT query decode error: ~p~n", [Reason]),
            FromPid ! {dht_reply, macula_protocol_encoder:encode(reply, #{error => <<"Invalid query">>})}
    end,

    {noreply, State};

handle_info(Info, State) ->
    io:format("[Gateway] WARNING: Unhandled handle_info message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{listener = Listener}) ->
    %% Close listener
    case Listener of
        undefined -> ok;
        _ -> macula_quic:close(Listener)
    end,
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Handle CONNECT message from client.
handle_connect(Stream, ConnectMsg, State) ->
    RealmId = maps:get(realm_id, ConnectMsg),
    case RealmId =:= State#state.realm of
        true ->
            ClientInfo = #{
                realm => RealmId,
                node_id => maps:get(node_id, ConnectMsg),
                capabilities => maps:get(capabilities, ConnectMsg, [])
            },
            io:format("[Gateway] Client connected: ~p~n", [ClientInfo]),
            {noreply, State};
        false ->
            io:format("[Gateway] Realm mismatch: ~p != ~p~n", [RealmId, State#state.realm]),
            macula_quic:close(Stream),
            {noreply, State}
    end.

%% @doc Handle DHT STORE message.
handle_dht_store(_Stream, StoreMsg, State) ->
    io:format("[Gateway] Processing STORE message: ~p~n", [StoreMsg]),
    %% Forward to routing server
    try
        Reply = macula_routing_server:handle_message(macula_routing_server, StoreMsg),
        io:format("[Gateway] STORE processed, reply: ~p~n", [Reply]),
        %% STORE doesn't require a response to be sent back
        {noreply, State}
    catch
        _:Error ->
            io:format("[Gateway] STORE processing error: ~p~n", [Error]),
            {noreply, State}
    end.

%% @doc Handle DHT FIND_VALUE message.
handle_dht_find_value(Stream, FindValueMsg, State) ->
    io:format("[Gateway] Processing FIND_VALUE message: ~p~n", [FindValueMsg]),
    %% Forward to routing server
    try
        Reply = macula_routing_server:handle_message(macula_routing_server, FindValueMsg),
        io:format("[Gateway] FIND_VALUE processed, reply: ~p~n", [Reply]),
        %% Send reply back over stream
        ReplyBinary = macula_protocol_encoder:encode(find_value_reply, Reply),
        macula_quic:send(Stream, ReplyBinary),
        {noreply, State}
    catch
        _:Error ->
            io:format("[Gateway] FIND_VALUE processing error: ~p~n", [Error]),
            {noreply, State}
    end.

%% @doc Handle DHT FIND_NODE message.
handle_dht_find_node(Stream, FindNodeMsg, State) ->
    io:format("[Gateway] Processing FIND_NODE message: ~p~n", [FindNodeMsg]),
    %% Forward to routing server
    try
        Reply = macula_routing_server:handle_message(macula_routing_server, FindNodeMsg),
        io:format("[Gateway] FIND_NODE processed, reply: ~p~n", [Reply]),
        %% Send reply back over stream
        ReplyBinary = macula_protocol_encoder:encode(find_node_reply, Reply),
        macula_quic:send(Stream, ReplyBinary),
        {noreply, State}
    catch
        _:Error ->
            io:format("[Gateway] FIND_NODE processing error: ~p~n", [Error]),
            {noreply, State}
    end.

%% @doc Handle successful connection acceptance.
%% Register for incoming streams with active mode enabled.
handle_accept_result({ok, Conn}, State) ->
    io:format("[Gateway] Accepted connection: ~p~n", [Conn]),

    %% Register for incoming streams (connection owner receives events)
    StreamOpts = #{active => true},
    case quicer:async_accept_stream(Conn, StreamOpts) of
        {ok, Conn} ->
            io:format("[Gateway] Registered for incoming streams (active mode)~n");
        {error, Reason} ->
            io:format("[Gateway] WARNING: Failed to register for streams: ~p~n", [Reason])
    end,

    %% Continue accepting more connections
    self() ! accept,
    {noreply, State};

%% @doc Handle accept timeout - continue accepting.
handle_accept_result({error, timeout}, State) ->
    self() ! accept,
    {noreply, State};

%% @doc Handle accept error - stop gateway.
handle_accept_result({error, Reason}, State) ->
    io:format("Accept error: ~p~n", [Reason]),
    {stop, {accept_error, Reason}, State}.

%% @doc Handle decoded CONNECT message.
handle_decoded_message({ok, {connect, ConnectMsg}}, Stream, State) ->
    io:format("[Gateway] Decoded CONNECT message~n"),
    handle_connect(Stream, ConnectMsg, State);

%% @doc Handle decoded STORE message.
handle_decoded_message({ok, {store, StoreMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED STORE MESSAGE ***~n"),
    io:format("[Gateway] STORE message: ~p~n", [StoreMsg]),
    handle_dht_store(Stream, StoreMsg, State);

%% @doc Handle decoded FIND_VALUE message.
handle_decoded_message({ok, {find_value, FindValueMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED FIND_VALUE MESSAGE ***~n"),
    io:format("[Gateway] FIND_VALUE message: ~p~n", [FindValueMsg]),
    handle_dht_find_value(Stream, FindValueMsg, State);

%% @doc Handle decoded FIND_NODE message.
handle_decoded_message({ok, {find_node, FindNodeMsg}}, Stream, State) ->
    io:format("[Gateway] *** RECEIVED FIND_NODE MESSAGE ***~n"),
    io:format("[Gateway] FIND_NODE message: ~p~n", [FindNodeMsg]),
    handle_dht_find_node(Stream, FindNodeMsg, State);

%% @doc Handle other decoded message types.
handle_decoded_message({ok, {Type, Other}}, _Stream, State) ->
    io:format("[Gateway] Received message type ~p: ~p~n", [Type, Other]),
    {noreply, State};

%% @doc Handle decode error.
handle_decoded_message({error, DecodeErr}, _Stream, State) ->
    io:format("[Gateway] !!! DECODE ERROR: ~p !!!~n", [DecodeErr]),
    {noreply, State}.

