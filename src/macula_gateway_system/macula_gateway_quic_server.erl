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
-export([start_link/1, set_gateway/2, shield_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Helper functions (exported for testing)
-ifdef(TEST).
-export([parse_endpoint/1, resolve_host/2, complete_handshake/1,
         accept_streams/1, register_next_connection/1, get_node_id/2,
         check_rate_limit/3, cleanup_stale_rate_entries/1,
         record_recent_connection/3, collect_shield_metrics/0]).
-endif.

%% Rate limiting defaults — must be high enough for mesh peers creating
%% concurrent connections (DHT queries, pubsub, RPC). 5/IP blocks LAN clusters.
-define(DEFAULT_MAX_CONN_PER_IP, 50).
-define(DEFAULT_IP_WINDOW_MS, 10000).
-define(DEFAULT_MAX_CONN_GLOBAL_PER_SEC, 200).
-define(RATE_CLEANUP_INTERVAL_MS, 30000).
-define(RATE_LOG_SUPPRESS_MS, 10000).
-define(MAX_RECENT_CONNECTIONS, 100).

-record(state, {
    listener :: pid() | undefined,
    gateway :: pid() | undefined,
    node_id :: binary(),
    port :: inet:port_number(),
    realm :: binary(),
    buffer = <<>> :: binary(),
    ip_rate_table :: ets:tid() | undefined,
    global_rate_table :: ets:tid() | undefined,
    max_conn_per_ip :: pos_integer(),
    ip_window_ms :: pos_integer(),
    max_conn_global_per_sec :: pos_integer()
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

%% @doc Returns shield metrics: recent connections with IPs, timestamps, and status.
%% Reads directly from named ETS tables. No gen_server call needed.
%% Returns empty metrics if the QUIC server has not started yet.
-spec shield_metrics() -> map().
shield_metrics() ->
    case ets:whereis(macula_gateway_ip_rate_limit) of
        undefined -> empty_shield_metrics();
        _ -> collect_shield_metrics()
    end.

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

    %% Rate limiting configuration
    MaxPerIp = application:get_env(macula, quic_max_conn_per_ip, ?DEFAULT_MAX_CONN_PER_IP),
    IpWindowMs = application:get_env(macula, quic_ip_window_ms, ?DEFAULT_IP_WINDOW_MS),
    MaxGlobalPerSec = application:get_env(macula, quic_max_conn_global_per_sec, ?DEFAULT_MAX_CONN_GLOBAL_PER_SEC),

    %% Create rate limiting ETS tables (named_table + public for shield_metrics access)
    IpRateTable = ets:new(macula_gateway_ip_rate_limit, [named_table, set, public, {write_concurrency, true}]),
    GlobalRateTable = ets:new(macula_gateway_global_rate_limit, [named_table, set, public, {write_concurrency, true}]),
    %% Recent connections table for shield metrics (ordered_set for timestamp ordering)
    ets:new(macula_gateway_recent_connections, [named_table, ordered_set, public, {write_concurrency, true}]),

    case macula_quic:listen(Port, ListenOpts) of
        {ok, Listener} ->
            ?LOG_INFO("QUIC listener started on port ~p (rate limit: ~p/ip/~pms, ~p/s global)",
                       [Port, MaxPerIp, IpWindowMs, MaxGlobalPerSec]),

            %% Start async accept to receive connections
            case quicer:async_accept(Listener, #{}) of
                {ok, Listener} ->
                    ?LOG_INFO("Async accept registered", []),
                    ok;
                {error, AcceptErr} ->
                    ?LOG_WARNING("async_accept failed: ~p", [AcceptErr]),
                    ok
            end,

            %% Schedule periodic rate limit cleanup
            erlang:send_after(?RATE_CLEANUP_INTERVAL_MS, self(), cleanup_rate_limits),

            State = #state{
                listener = Listener,
                port = Port,
                realm = Realm,
                node_id = NodeId,
                gateway = GatewayPid,
                ip_rate_table = IpRateTable,
                global_rate_table = GlobalRateTable,
                max_conn_per_ip = MaxPerIp,
                ip_window_ms = IpWindowMs,
                max_conn_global_per_sec = MaxGlobalPerSec
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
%% Rate limits by source IP and global connection rate before completing handshake.
handle_info({quic, new_conn, Conn, ConnInfo}, State) ->
    PeerAddrResult = quicer:peername(Conn),
    ?LOG_DEBUG("[Gateway QUIC] New connection: ~p, info=~p, peer=~p",
              [Conn, ConnInfo, PeerAddrResult]),

    %% Extract IP for rate limiting (strip port)
    IP = case PeerAddrResult of
        {ok, {Addr, _Port}} -> Addr;
        _ -> undefined
    end,

    Now = erlang:system_time(millisecond),
    case check_rate_limit(IP, Now, State) of
        allowed ->
            record_recent_connection(IP, Now, accepted),
            %% Store conn->peer_addr mapping for stream lookup
            case PeerAddrResult of
                {ok, PeerAddr} -> put({conn_peer_addr, Conn}, PeerAddr);
                _ -> ok
            end,
            complete_handshake(Conn);
        rejected ->
            record_recent_connection(IP, Now, rejected),
            maybe_log_rate_limited(IP),
            catch quicer:close_connection(Conn)
    end,

    %% MUST always re-register for next connection
    register_next_connection(State#state.listener),
    {noreply, State};

%% @doc Handle QUIC event: peer_needs_streams (peer wants to open more streams).
handle_info({quic, peer_needs_streams, _Conn, _StreamType}, State) ->
    %% Normal QUIC control event - just acknowledge
    {noreply, State};

%% @doc Handle QUIC event: shutdown (connection shutting down).
handle_info({quic, shutdown, Conn, Reason}, State) ->
    ?LOG_INFO("QUIC shutdown: Conn=~p, Reason=~p", [Conn, Reason]),
    notify_gateway_conn_closed(Conn, State),
    {noreply, State};

%% @doc Handle QUIC event: transport_shutdown (transport layer shutting down).
handle_info({quic, transport_shutdown, Conn, Reason}, State) ->
    ?LOG_INFO("QUIC transport_shutdown: Conn=~p, Reason=~p", [Conn, Reason]),
    notify_gateway_conn_closed(Conn, State),
    {noreply, State};

%% @doc Periodic cleanup of stale rate limit entries.
handle_info(cleanup_rate_limits, State) ->
    cleanup_stale_rate_entries(State),
    erlang:send_after(?RATE_CLEANUP_INTERVAL_MS, self(), cleanup_rate_limits),
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
                    Host = macula_connection:hostname_from_node(),
                    crypto:hash(sha256, term_to_binary({Realm, Host, Port}));
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

    %% Track Conn→NodeId for CONNECT messages (used for eviction on disconnect)
    maybe_track_conn_node_id(MessageType, Message, Stream),

    %% Route to gateway via gen_server:cast (async) to prevent blocking
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
%%% Rate Limiting Functions
%%%===================================================================

%% @doc Check if a connection from IP should be allowed.
%% Checks global rate first, then per-IP rate.
%% Returns allowed or rejected.
-spec check_rate_limit(IP, Now, State) -> allowed | rejected
    when IP :: inet:ip_address() | undefined,
         Now :: integer(),
         State :: #state{}.
check_rate_limit(_IP, _Now, #state{ip_rate_table = undefined}) ->
    allowed;
check_rate_limit(IP, Now, State) ->
    case check_global_rate(Now, State) of
        rejected -> rejected;
        allowed -> check_ip_rate(IP, Now, State)
    end.

%% @doc Check global connections-per-second limit.
check_global_rate(Now, #state{global_rate_table = Tab, max_conn_global_per_sec = Max}) ->
    Second = Now div 1000,
    case ets:lookup(Tab, global) of
        [{global, Count, Second}] when Count >= Max ->
            rejected;
        [{global, _Count, Second}] ->
            ets:update_counter(Tab, global, {2, 1}),
            allowed;
        _ ->
            %% New second or no entry — reset
            ets:insert(Tab, {global, 1, Second}),
            allowed
    end.

%% @doc Check per-IP connection rate within sliding window.
%% Undefined IP (peername failed) — skip IP check, global rate still applies.
check_ip_rate(undefined, _Now, _State) ->
    allowed;
check_ip_rate(IP, Now, #state{ip_rate_table = Tab, max_conn_per_ip = Max, ip_window_ms = WindowMs}) ->
    case ets:lookup(Tab, IP) of
        [{IP, Count, WindowStart}] when Now - WindowStart < WindowMs, Count >= Max ->
            rejected;
        [{IP, _Count, WindowStart}] when Now - WindowStart < WindowMs ->
            ets:update_counter(Tab, IP, {2, 1}),
            allowed;
        _ ->
            %% Window expired or no entry — reset
            ets:insert(Tab, {IP, 1, Now}),
            allowed
    end.

%% @doc Log rate-limited IP (suppressed to once per window per IP).
maybe_log_rate_limited(undefined) ->
    ok;
maybe_log_rate_limited(IP) ->
    Now = erlang:system_time(millisecond),
    Key = {rate_limit_logged, IP},
    case get(Key) of
        undefined ->
            ?LOG_WARNING("[Gateway QUIC] Rate limiting connections from ~p", [IP]),
            put(Key, Now);
        LastLogged when Now - LastLogged >= ?RATE_LOG_SUPPRESS_MS ->
            ?LOG_WARNING("[Gateway QUIC] Rate limiting connections from ~p", [IP]),
            put(Key, Now);
        _ ->
            ok
    end.

%% @doc Remove stale entries from rate limit tables.
cleanup_stale_rate_entries(#state{ip_rate_table = undefined}) ->
    ok;
cleanup_stale_rate_entries(#state{ip_rate_table = IpTab, global_rate_table = GlobalTab,
                                  ip_window_ms = WindowMs}) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - (2 * WindowMs),
    %% Delete IP entries whose window started before cutoff
    ets:select_delete(IpTab, [{{'_', '_', '$1'}, [{'<', '$1', Cutoff}], [true]}]),
    %% Delete global entry if older than 5 seconds
    GlobalCutoff = (Now div 1000) - 5,
    ets:select_delete(GlobalTab, [{{global, '_', '$1'}, [{'<', '$1', GlobalCutoff}], [true]}]),
    ok.

%%%===================================================================
%%% Shield Metrics Functions
%%%===================================================================

%% @doc Record a connection attempt in the recent connections ETS table.
%% Uses {Timestamp, UniqueRef} as key for ordered_set ordering.
%% Trims entries beyond MAX_RECENT_CONNECTIONS.
record_recent_connection(undefined, _Now, _Status) ->
    ok;
record_recent_connection(IP, Now, Status) ->
    Key = {Now, make_ref()},
    ets:insert(macula_gateway_recent_connections, {Key, IP, Status}),
    trim_recent_connections().

%% @doc Keep only the most recent MAX_RECENT_CONNECTIONS entries.
trim_recent_connections() ->
    Size = ets:info(macula_gateway_recent_connections, size),
    trim_oldest(Size - ?MAX_RECENT_CONNECTIONS).

trim_oldest(N) when N =< 0 ->
    ok;
trim_oldest(N) ->
    case ets:first(macula_gateway_recent_connections) of
        '$end_of_table' -> ok;
        Key ->
            ets:delete(macula_gateway_recent_connections, Key),
            trim_oldest(N - 1)
    end.

%% @doc Collect shield metrics from ETS tables.
%% Computes stats over a 10-second window and returns the last 100 connections.
collect_shield_metrics() ->
    Now = erlang:system_time(millisecond),
    WindowMs = 10000,
    Cutoff = Now - WindowMs,
    All = ets:tab2list(macula_gateway_recent_connections),
    Parsed = [{IP, Ts, St} || {{Ts, _Ref}, IP, St} <- All],
    InWindow = [{IP, Ts, St} || {IP, Ts, St} <- Parsed, Ts >= Cutoff],
    Accepted = length([ok || {_, _, accepted} <- InWindow]),
    Rejected = length([ok || {_, _, rejected} <- InWindow]),
    Total = Accepted + Rejected,
    Cps = Total * 1000.0 / WindowMs,
    Entries = [#{
        ip => format_ip(IP),
        status => St,
        timestamp => Ts
    } || {IP, Ts, St} <- Parsed],
    #{
        window_seconds => WindowMs div 1000,
        total_accepted => Accepted,
        total_rejected => Rejected,
        connections_per_second => Cps,
        recent => lists:reverse(Entries)
    }.

%% @doc Return empty shield metrics when QUIC server is not running.
empty_shield_metrics() ->
    #{
        window_seconds => 10,
        total_accepted => 0,
        total_rejected => 0,
        connections_per_second => 0.0,
        recent => []
    }.

%% @doc Format an IP address tuple as a binary string.
format_ip({A, B, C, D}) ->
    list_to_binary(io_lib:format("~B.~B.~B.~B", [A, B, C, D]));
format_ip({A, B, C, D, E, F, G, H}) ->
    list_to_binary(io_lib:format("~.16B:~.16B:~.16B:~.16B:~.16B:~.16B:~.16B:~.16B",
                                  [A, B, C, D, E, F, G, H]));
format_ip(IP) ->
    list_to_binary(io_lib:format("~p", [IP])).

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

%%% ===================================================================
%%% Connection lifecycle tracking (for peer eviction on disconnect)
%%% ===================================================================

%% @private Track Conn→NodeId when a CONNECT message is routed.
%% Stores in process dict so we can look up node_id on connection shutdown.
maybe_track_conn_node_id(connect, Message, _Stream) ->
    NodeId = maps:get(<<"node_id">>, Message, undefined),
    Conn = get(pending_stream_conn),
    store_conn_node_id(Conn, NodeId);
maybe_track_conn_node_id(_, _, _) ->
    ok.

store_conn_node_id(undefined, _) -> ok;
store_conn_node_id(_, undefined) -> ok;
store_conn_node_id(Conn, NodeId) ->
    put({conn_node_id, Conn}, NodeId),
    ok.

%% @private Notify gateway that a connection closed, so it can evict the peer.
notify_gateway_conn_closed(_Conn, #state{gateway = undefined}) ->
    ok;
notify_gateway_conn_closed(Conn, #state{gateway = Gateway}) ->
    NodeId = get({conn_node_id, Conn}),
    notify_gateway_with_node_id(Gateway, NodeId, Conn).

notify_gateway_with_node_id(_, undefined, _) ->
    ok;
notify_gateway_with_node_id(Gateway, NodeId, Conn) ->
    ?LOG_INFO("[Gateway QUIC] Connection closed for node ~s, notifying gateway",
              [binary:encode_hex(NodeId)]),
    gen_server:cast(Gateway, {connection_closed, NodeId}),
    %% Clean up process dict
    erase({conn_node_id, Conn}),
    erase({conn_peer_addr, Conn}),
    ok.
