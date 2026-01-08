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

-include_lib("kernel/include/logger.hrl").

%% Suppress warnings for functions only used in tests
-compile({nowarn_unused_function, [parse_endpoint/1, resolve_host/2]}).

%% API
-export([start_link/1, set_gateway/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Helper functions (exported for testing)
-ifdef(TEST).
-export([parse_endpoint/1, resolve_host/2, complete_handshake/1,
         accept_streams/1, register_next_connection/1, get_node_id/2]).
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

    ?LOG_INFO("Initializing QUIC server for realm ~s on port ~p", [Realm, Port]),

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
            ?LOG_INFO("QUIC listener started on port ~p", [Port]),

            %% Start async accept to receive connections
            case quicer:async_accept(Listener, #{}) of
                {ok, Listener} ->
                    ?LOG_INFO("Async accept registered", []),
                    ok;
                {error, AcceptErr} ->
                    ?LOG_WARNING("async_accept failed: ~p", [AcceptErr]),
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
            ?LOG_ERROR("QUIC listen failed: ~p ~p", [ErrorType, ErrorDetail]),
            ?LOG_ERROR("Certificate file: ~p", [CertFile]),
            ?LOG_ERROR("Key file: ~p", [KeyFile]),
            {stop, {listen_failed, {ErrorType, ErrorDetail}}};

        {error, Reason} ->
            ?LOG_ERROR("QUIC listen failed: ~p", [Reason]),
            {stop, {listen_failed, Reason}}
    end.

%% @doc Handle synchronous calls.
%% Set gateway PID for message routing
handle_call({set_gateway, GatewayPid}, _From, State) when is_pid(GatewayPid) ->
    ?LOG_INFO("Gateway PID set: ~p", [GatewayPid]),
    {reply, ok, State#state{gateway = GatewayPid}};

%% Unknown calls
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle QUIC event: new_stream (stream created by peer).
%% Associates the stream with its parent connection to enable peer address lookup.
handle_info({quic, new_stream, Stream, StreamProps}, State) ->
    ?LOG_DEBUG("[Gateway QUIC] New stream received: ~p, props=~p", [Stream, StreamProps]),

    %% Get the pending connection this stream belongs to
    %% This was set by accept_streams when we called async_accept_stream
    Conn = get(pending_stream_conn),
    ?LOG_DEBUG("[Gateway QUIC] Stream's connection: ~p", [Conn]),

    %% Look up peer address from the connection (stored in new_conn handler)
    PeerAddr = case Conn of
        undefined -> undefined;
        _ -> get({conn_peer_addr, Conn})
    end,
    ?LOG_DEBUG("[Gateway QUIC] Peer addr from conn: ~p", [PeerAddr]),

    %% Store stream->peer_addr mapping for quick lookup during data handling
    case PeerAddr of
        undefined -> ok;
        Addr -> put({stream_peer_addr, Stream}, Addr)
    end,

    %% Accept more streams on this connection
    case Conn of
        undefined -> ok;
        _ -> quicer:async_accept_stream(Conn, #{})
    end,

    %% Set stream to active mode to receive data automatically
    case quicer:setopt(Stream, active, true) of
        ok ->
            ?LOG_DEBUG("[Gateway QUIC] Stream set to active mode"),
            {noreply, State};
        {error, Reason} ->
            ?LOG_ERROR("[Gateway QUIC] Failed to set stream active: ~p", [Reason]),
            {noreply, State}
    end;

%% @doc Handle QUIC data reception - decode and route to gateway.
%% Pattern matches on binary data, uses buffer for partial messages.
%% Uses stored stream->peer_addr mapping (populated in new_stream handler).
handle_info({quic, Data, Stream, Flags}, State) when is_binary(Data) ->
    ?LOG_INFO("[Gateway QUIC] Received ~p bytes, Flags=~p", [byte_size(Data), Flags]),

    %% Look up peer address from stored mapping (v0.12.0 fix)
    %% This was populated in new_stream handler when we associated stream with connection
    PeerAddr = case get({stream_peer_addr, Stream}) of
        undefined ->
            %% Fallback: try quicer:peername (may fail for streams)
            quicer:peername(Stream);
        StoredAddr ->
            {ok, StoredAddr}
    end,
    ?LOG_DEBUG("[Gateway QUIC] Peer address for stream: ~p", [PeerAddr]),

    %% Decode message and route to gateway with peer address
    DecodeResult = macula_protocol_decoder:decode(Data),
    ?LOG_DEBUG("[Gateway QUIC] Decoded result: ~p", [element(1, DecodeResult)]),
    route_to_gateway(DecodeResult, Stream, PeerAddr, State);

%% @doc Handle QUIC event: new_conn (connection established).
handle_info({quic, new_conn, Conn, ConnInfo}, State) ->
    %% Try to get peer address from CONNECTION handle (not stream)
    PeerAddrResult = quicer:peername(Conn),
    ?LOG_DEBUG("[Gateway QUIC] New connection: ~p, info=~p, peer=~p",
              [Conn, ConnInfo, PeerAddrResult]),

    %% Store the connection->peer_addr mapping in process dictionary
    %% (We'll use this to look up peer addr when we get streams)
    case PeerAddrResult of
        {ok, PeerAddr} ->
            %% Store connection ref as key, peer addr as value
            put({conn_peer_addr, Conn}, PeerAddr);
        _ ->
            ok
    end,

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
    ?LOG_INFO("QUIC shutdown: Conn=~p, Reason=~p", [Conn, Reason]),
    {noreply, State};

%% @doc Handle QUIC event: transport_shutdown (transport layer shutting down).
handle_info({quic, transport_shutdown, Conn, Reason}, State) ->
    ?LOG_INFO("QUIC transport_shutdown: Conn=~p, Reason=~p", [Conn, Reason]),
    {noreply, State};

%% @doc Handle unknown messages.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination.
terminate(_Reason, _State) ->
    ?LOG_INFO("Shutting down QUIC server", []),
    ok.

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% @doc Generate node ID from HOSTNAME env var (set by Docker) or generate from {Realm, Port}.
%% Returns a 32-byte binary (raw binary for Kademlia, never hex-encoded).
%% MUST match macula_gateway_system:get_node_id/2 and macula_gateway:get_node_id/2!
%%
%% Priority:
%% 1. NODE_NAME env var (explicit, highest priority)
%% 2. HOSTNAME env var (Docker sets this to container hostname - unique per container)
%% 3. Fallback to {Realm, Port} only (NO MAC - MAC is shared across Docker containers)
get_node_id(Realm, Port) when is_list(Realm), is_integer(Port) ->
    %% Handle charlist realm by converting to binary
    get_node_id(list_to_binary(Realm), Port);
get_node_id(Realm, Port) when is_binary(Realm), is_integer(Port) ->
    case os:getenv("NODE_NAME") of
        false ->
            %% No NODE_NAME, try HOSTNAME (Docker sets this to container hostname)
            case os:getenv("HOSTNAME") of
                false ->
                    %% No HOSTNAME either, use {Realm, Port} as last resort
                    crypto:hash(sha256, term_to_binary({Realm, Port}));
                Hostname when is_list(Hostname) ->
                    %% Use HOSTNAME from Docker - unique per container
                    crypto:hash(sha256, term_to_binary({Realm, list_to_binary(Hostname), Port}))
            end;
        NodeName when is_list(NodeName) ->
            %% Use NODE_NAME from environment - hash it to get 32-byte binary
            crypto:hash(sha256, list_to_binary(NodeName))
    end.

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
            ?LOG_ERROR("Handshake failed: ~p", [Reason]),
            ok
    end.

%% @doc Start accepting streams on an established connection.
%% Also stores the connection in process dictionary so we can look up peer address later.
-spec accept_streams(quicer:connection_handle()) -> ok.
accept_streams(Conn) ->
    ?LOG_INFO("Connection handshake completed successfully", []),
    case quicer:async_accept_stream(Conn, #{}) of
        {ok, Conn} ->
            %% Track which connection we're accepting streams for
            %% We'll need this to look up peer address when streams arrive
            put(pending_stream_conn, Conn),
            ?LOG_INFO("Ready to accept streams on connection ~p", [Conn]),
            ok;
        {error, StreamAcceptErr} ->
            ?LOG_WARNING("async_accept_stream failed: ~p", [StreamAcceptErr]),
            ok
    end.

%% @doc Register listener for next incoming connection.
-spec register_next_connection(quicer:listener_handle()) -> ok.
register_next_connection(Listener) ->
    case quicer:async_accept(Listener, #{}) of
        {ok, _} ->
            ?LOG_INFO("Ready for next connection", []),
            ok;
        {error, AcceptErr} ->
            ?LOG_WARNING("async_accept failed: ~p", [AcceptErr]),
            ok
    end.

%%%===================================================================
%%% Message Routing Functions
%%%===================================================================

%% @doc Route decoded message to gateway for business logic handling.
%% Pattern matches on decode result - uses declarative style.
%% PeerAddr is captured at receive time for NAT detection (v0.12.0+).
-spec route_to_gateway(DecodeResult, Stream, PeerAddr, State) -> {noreply, State}
    when DecodeResult :: {ok, {atom(), map()}} | {error, term()},
         Stream :: quicer:stream_handle(),
         PeerAddr :: {ok, {inet:ip_address(), inet:port_number()}} | {error, term()},
         State :: #state{}.

%% Pattern 1: Successfully decoded message - route to gateway with peer address
route_to_gateway({ok, {MessageType, Message}}, Stream, PeerAddr, State) when State#state.gateway =/= undefined ->
    ?LOG_INFO("[Gateway QUIC] Routing message type=~p to gateway, msg_keys=~p",
              [MessageType, maps:keys(Message)]),
    Gateway = State#state.gateway,

    %% Route to gateway via gen_server:cast (async) to prevent blocking
    %% This avoids timeout issues when gateway is busy processing other messages
    %% Include PeerAddr for NAT detection (v0.12.0+)
    gen_server:cast(Gateway, {route_message, MessageType, Message, Stream, PeerAddr}),
    ?LOG_DEBUG("Message routed (async)", []),
    {noreply, State};

%% Pattern 2: No gateway configured yet - log warning
route_to_gateway({ok, {MessageType, _Message}}, _Stream, _PeerAddr, State) ->
    ?LOG_WARNING("No gateway configured, dropping message type: ~p",
              [MessageType]),
    {noreply, State};

%% Pattern 3: Decode error - log and continue
route_to_gateway({error, Reason}, _Stream, _PeerAddr, State) ->
    ?LOG_ERROR("Decode error: ~p", [Reason]),
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
            ?LOG_WARNING("Invalid endpoint URL format: ~s, using placeholder", [Endpoint]),
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
            ?LOG_WARNING("Failed to resolve host ~s: ~p, using localhost fallback",
                     [Host, Reason]),
            {{127,0,0,1}, Port}
    end.
