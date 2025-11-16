%%%-------------------------------------------------------------------
%%% @doc
%%% QUIC Transport Layer Gen_Server
%%%
%%% Handles all QUIC transport operations for the gateway:
%%% - Owns QUIC listener
%%% - Receives {quic, ...} events
%%% - Decodes protocol messages
%%% - Routes messages to gateway for business logic
%%%
%%% This separation follows proper OTP design:
%%% - One process, one responsibility (transport vs routing)
%%% - Clean fault isolation (QUIC crashes don't crash gateway)
%%% - Proper supervision (supervisor can restart independently)
%%% - Testability (can test transport in isolation)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_quic_server).
-behaviour(gen_server).

%% Suppress warnings for functions only used in tests
-compile({nowarn_unused_function, [parse_endpoint/1, resolve_host/2]}).

%% API
-export([start_link/1, set_gateway/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Helper functions (exported for testing)
-ifdef(TEST).
-export([parse_endpoint/1, resolve_host/2, complete_handshake/1,
         accept_streams/1, register_next_connection/1]).
-endif.

-record(state, {
    listener :: pid() | undefined,
    gateway :: pid() | undefined,
    node_id :: binary(),
    port :: inet:port_number(),
    realm :: binary(),
    buffer = <<>> :: binary()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the QUIC server gen_server.
-spec start_link(Opts :: proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Set the gateway PID for message routing.
%% Called by supervisor after both quic_server and gateway have started.
-spec set_gateway(pid(), pid()) -> ok.
set_gateway(QuicServerPid, GatewayPid) when is_pid(QuicServerPid), is_pid(GatewayPid) ->
    gen_server:call(QuicServerPid, {set_gateway, GatewayPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Initialize the QUIC server and start QUIC listener.
init(Opts) ->
    Port = proplists:get_value(port, Opts, 9443),
    Realm = proplists:get_value(realm, Opts, <<"macula.default">>),
    GatewayPid = proplists:get_value(gateway, Opts),  % Passed by gateway
    CertFile = proplists:get_value(cert_file, Opts),
    KeyFile = proplists:get_value(key_file, Opts),
    NodeId = get_node_id(Realm, Port),

    io:format("[QuicServer] Initializing QUIC server for realm ~s on port ~p~n", [Realm, Port]),

    %% Start QUIC listener
    ListenOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {peer_unidi_stream_count, 3},
        {peer_bidi_stream_count, 100}
    ],

    case macula_quic:listen(Port, ListenOpts) of
        {ok, Listener} ->
            io:format("[QuicServer] QUIC listener started on port ~p~n", [Port]),

            %% Start async accept to receive connections
            case quicer:async_accept(Listener, #{}) of
                {ok, Listener} ->
                    io:format("[QuicServer] Async accept registered~n"),
                    ok;
                {error, AcceptErr} ->
                    io:format("[QuicServer] WARNING: async_accept failed: ~p~n", [AcceptErr]),
                    ok
            end,

            State = #state{
                listener = Listener,
                port = Port,
                realm = Realm,
                node_id = NodeId,
                gateway = GatewayPid
            },

            {ok, State};

        {error, ErrorType, ErrorDetail} ->
            io:format("[QuicServer] QUIC listen failed: ~p ~p~n", [ErrorType, ErrorDetail]),
            io:format("[QuicServer] Certificate file: ~p~n", [CertFile]),
            io:format("[QuicServer] Key file: ~p~n", [KeyFile]),
            {stop, {listen_failed, {ErrorType, ErrorDetail}}};

        {error, Reason} ->
            io:format("[QuicServer] QUIC listen failed: ~p~n", [Reason]),
            {stop, {listen_failed, Reason}}
    end.

%% @doc Handle synchronous calls.
%% Set gateway PID for message routing
handle_call({set_gateway, GatewayPid}, _From, State) when is_pid(GatewayPid) ->
    io:format("[QuicServer] Gateway PID set: ~p~n", [GatewayPid]),
    {reply, ok, State#state{gateway = GatewayPid}};

%% Unknown calls
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle QUIC event: new_stream (stream created by peer).
handle_info({quic, new_stream, Stream, StreamProps}, State) ->
    io:format("[QuicServer] ========================================~n"),
    io:format("[QuicServer] NEW STREAM RECEIVED!~n"),
    io:format("[QuicServer] Stream: ~p~n", [Stream]),
    io:format("[QuicServer] StreamProps: ~p~n", [StreamProps]),
    io:format("[QuicServer] ========================================~n"),

    %% Set stream to active mode to receive data automatically
    case quicer:setopt(Stream, active, true) of
        ok ->
            io:format("[QuicServer] Stream set to active mode~n"),
            {noreply, State};
        {error, Reason} ->
            io:format("[QuicServer] Failed to set stream active: ~p~n", [Reason]),
            {noreply, State}
    end;

%% @doc Handle QUIC data reception - decode and route to gateway.
%% Pattern matches on binary data, uses buffer for partial messages.
handle_info({quic, Data, Stream, _Flags}, State) when is_binary(Data) ->
    io:format("[QuicServer] ===== Received ~p bytes from stream ~p =====~n",
              [byte_size(Data), Stream]),
    io:format("[QuicServer] Raw data (first 100 bytes): ~p~n",
              [binary:part(Data, 0, min(100, byte_size(Data)))]),

    %% Decode message and route to gateway
    DecodeResult = macula_protocol_decoder:decode(Data),
    route_to_gateway(DecodeResult, Stream, State);

%% @doc Handle QUIC event: new_conn (connection established).
handle_info({quic, new_conn, Conn, ConnInfo}, State) ->
    io:format("[QuicServer] ========================================~n"),
    io:format("[QuicServer] NEW CONNECTION RECEIVED!~n"),
    io:format("[QuicServer] Connection: ~p~n", [Conn]),
    io:format("[QuicServer] Connection Info: ~p~n", [ConnInfo]),
    io:format("[QuicServer] ========================================~n"),

    %% Use helper functions to complete handshake
    complete_handshake(Conn),
    register_next_connection(State#state.listener),

    {noreply, State};

%% @doc Handle QUIC event: peer_needs_streams (peer wants to open more streams).
handle_info({quic, peer_needs_streams, _Conn, _StreamType}, State) ->
    %% Normal QUIC control event - just acknowledge
    {noreply, State};

%% @doc Handle QUIC event: shutdown (connection shutting down).
handle_info({quic, shutdown, Conn, Reason}, State) ->
    io:format("[QuicServer] QUIC shutdown: Conn=~p, Reason=~p~n", [Conn, Reason]),
    {noreply, State};

%% @doc Handle QUIC event: transport_shutdown (transport layer shutting down).
handle_info({quic, transport_shutdown, Conn, Reason}, State) ->
    io:format("[QuicServer] QUIC transport_shutdown: Conn=~p, Reason=~p~n", [Conn, Reason]),
    {noreply, State};

%% @doc Handle unknown messages.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination.
terminate(_Reason, _State) ->
    io:format("[QuicServer] Shutting down QUIC server~n"),
    ok.

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% @doc Generate node ID from realm and port.
%% Uses MD5 hash of "realm:port" similar to gateway implementation.
get_node_id(Realm, Port) when is_binary(Realm), is_integer(Port) ->
    PortBin = integer_to_binary(Port),
    Input = <<Realm/binary, ":", PortBin/binary>>,
    crypto:hash(md5, Input).

%%%===================================================================
%%% QUIC Connection Helper Functions
%%%===================================================================

%% @doc Complete TLS handshake on QUIC connection.
-spec complete_handshake(quicer:connection_handle()) -> ok.
complete_handshake(Conn) ->
    case quicer:handshake(Conn) of
        ok ->
            accept_streams(Conn);
        {ok, _} ->
            accept_streams(Conn);
        {error, Reason} ->
            io:format("[QuicServer] Handshake failed: ~p~n", [Reason]),
            ok
    end.

%% @doc Start accepting streams on an established connection.
-spec accept_streams(quicer:connection_handle()) -> ok.
accept_streams(Conn) ->
    io:format("[QuicServer] Connection handshake completed successfully~n"),
    case quicer:async_accept_stream(Conn, #{}) of
        {ok, Conn} ->
            io:format("[QuicServer] Ready to accept streams on connection~n"),
            ok;
        {error, StreamAcceptErr} ->
            io:format("[QuicServer] WARNING: async_accept_stream failed: ~p~n", [StreamAcceptErr]),
            ok
    end.

%% @doc Register listener for next incoming connection.
-spec register_next_connection(quicer:listener_handle()) -> ok.
register_next_connection(Listener) ->
    case quicer:async_accept(Listener, #{}) of
        {ok, _} ->
            io:format("[QuicServer] Ready for next connection~n"),
            ok;
        {error, AcceptErr} ->
            io:format("[QuicServer] WARNING: async_accept failed: ~p~n", [AcceptErr]),
            ok
    end.

%%%===================================================================
%%% Message Routing Functions
%%%===================================================================

%% @doc Route decoded message to gateway for business logic handling.
%% Pattern matches on decode result - uses declarative style.
-spec route_to_gateway(DecodeResult, Stream, State) -> {noreply, State}
    when DecodeResult :: {ok, {atom(), map()}} | {error, term()},
         Stream :: quicer:stream_handle(),
         State :: #state{}.

%% Pattern 1: Successfully decoded message - route to gateway
route_to_gateway({ok, {MessageType, Message}}, Stream, State) when State#state.gateway =/= undefined ->
    io:format("[QuicServer] Decoded message type: ~p~n", [MessageType]),
    Gateway = State#state.gateway,

    %% Route to gateway via gen_server:call
    case gen_server:call(Gateway, {route_message, MessageType, Message, Stream}) of
        ok ->
            io:format("[QuicServer] Message routed successfully~n"),
            {noreply, State};
        {error, Reason} ->
            io:format("[QuicServer] Routing failed: ~p~n", [Reason]),
            {noreply, State}
    end;

%% Pattern 2: No gateway configured yet - log warning
route_to_gateway({ok, {MessageType, _Message}}, _Stream, State) ->
    io:format("[QuicServer] WARNING: No gateway configured, dropping message type: ~p~n",
              [MessageType]),
    {noreply, State};

%% Pattern 3: Decode error - log and continue
route_to_gateway({error, Reason}, _Stream, State) ->
    io:format("[QuicServer] Decode error: ~p~n", [Reason]),
    {noreply, State}.

%%%===================================================================
%%% Endpoint Parsing Functions
%%%===================================================================

%% @doc Parse endpoint URL to address tuple.
%% Converts "https://host:port" to {{IP_tuple}, Port}
%% Returns placeholder on parsing errors instead of crashing.
-spec parse_endpoint(undefined | binary()) -> {{byte(), byte(), byte(), byte()}, inet:port_number()}.
parse_endpoint(undefined) ->
    {{0,0,0,0}, 0};
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    %% Parse URL using uri_string
    case uri_string:parse(Endpoint) of
        #{host := Host, port := Port} when is_integer(Port) ->
            resolve_host(Host, Port);
        #{host := Host} ->
            resolve_host(Host, 9443);  %% Default port
        _ ->
            io:format("[QuicServer] Invalid endpoint URL format: ~s, using placeholder~n", [Endpoint]),
            {{0,0,0,0}, 0}
    end.

%% @doc Resolve hostname to IP address.
%% Returns localhost fallback on DNS resolution failure.
-spec resolve_host(binary(), inet:port_number()) -> {{byte(), byte(), byte(), byte()}, inet:port_number()}.
resolve_host(Host, Port) when is_binary(Host), is_integer(Port) ->
    HostStr = binary_to_list(Host),
    case inet:getaddr(HostStr, inet) of
        {ok, IPTuple} ->
            {IPTuple, Port};
        {error, Reason} ->
            io:format("[QuicServer] Failed to resolve host ~s: ~p, using localhost fallback~n",
                     [Host, Reason]),
            {{127,0,0,1}, Port}
    end.
