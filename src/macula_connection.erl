%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Connection - QUIC Transport Layer (v0.7.0+).
%%%
%%% This module manages the low-level QUIC connection lifecycle and
%%% message transport for mesh participants.
%%%
%%% Responsibilities:
%%% - Establish and maintain QUIC connection
%%% - Send messages via QUIC stream
%%% - Receive and route incoming messages to handlers
%%% - Handle connection errors and reconnection
%%% - Message encoding/decoding and buffering
%%%
%%% Renamed from macula_connection in v0.7.0 for clarity:
%%% - macula_connection = QUIC transport (this module - low-level)
%%% - macula_peer = mesh participant (high-level API)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%% API
-export([start_link/2, send_message/3, send_message_async/3, get_status/1, default_config/0]).
-export([hostname_from_node/0]).

%% Bridge system API (v0.13.0+)
%% Used by macula_bridge_node for parent mesh connections.
%% Uses bridge_rpc and bridge_data message types for communication.
-export([close/1, call/4, send/2, connect/3]).

%% API for testing
-export([decode_messages/2]).

%% Internal exports
-export([start_keepalive_timer/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Test exports for bootstrap lookup helpers
-ifdef(TEST).
-export([extract_peer_info/2, make_peer_info/3, get_map_field/3, get_map_field/4]).
-endif.

-define(SERVER, ?MODULE).
-define(INITIAL_RETRY_DELAY_MS, 2000).
-define(MAX_RETRY_DELAY_MS, 120000).

-record(state, {
    url :: binary(),
    opts :: map(),
    node_id :: binary(),
    realm :: binary(),
    peer_id :: integer(),  %% Unique peer system identifier for gproc lookups
    connection :: pid() | undefined,
    stream :: pid() | undefined,
    status = connecting :: connecting | connected | disconnected | error,
    recv_buffer = <<>> :: binary(),
    keepalive_timer :: reference() | undefined,
    connect_in_flight = false :: boolean(),
    retry_delay_ms = ?INITIAL_RETRY_DELAY_MS :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Url, Opts) ->
    gen_server:start_link(?MODULE, {Url, Opts}, []).

-spec default_config() -> map().
default_config() ->
    #{
        keepalive_enabled => true,
        keepalive_interval => 30000  %% 30 seconds
    }.

-spec send_message(pid(), atom(), map()) -> ok | {error, term()}.
send_message(Pid, Type, Msg) ->
    gen_server:call(Pid, {send_message, Type, Msg}, 5000).

%% @doc Send message asynchronously (fire-and-forget).
%% Use for operations where blocking is unacceptable and failures can be tolerated.
%% The message will be sent if connected, silently dropped if not.
-spec send_message_async(pid(), atom(), map()) -> ok.
send_message_async(Pid, Type, Msg) ->
    gen_server:cast(Pid, {send_message_async, Type, Msg}).

-spec get_status(pid()) -> connecting | connected | disconnected | error.
get_status(Pid) ->
    gen_server:call(Pid, get_status, 5000).

%%%===================================================================
%%% Bridge System API (v0.13.0+)
%%% These functions provide a simplified API for bridge-to-bridge
%%% connections, using the underlying QUIC transport layer.
%%%===================================================================

%% @doc Close a connection gracefully.
%% Stops the gen_server which triggers proper QUIC cleanup in terminate/2.
%% Uses catch expression to handle race conditions where process may already be dead.
-spec close(pid()) -> ok.
close(Pid) when is_pid(Pid) ->
    _ = (catch gen_server:stop(Pid, normal, 5000)),
    ok;
close(_) ->
    ok.

%% @doc Make an RPC-style call over a connection.
%% Sends a call message and waits for a reply. Uses the underlying
%% send_message API with type 'bridge_rpc'.
-spec call(pid(), binary(), map(), map()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Opts) when is_pid(Pid) ->
    Timeout = maps:get(timeout, Opts, 5000),
    CallId = crypto:strong_rand_bytes(16),
    Msg = #{
        procedure => Procedure,
        args => Args,
        call_id => CallId,
        timeout => Timeout
    },
    %% Use synchronous send which validates connection state
    case send_message(Pid, bridge_rpc, Msg) of
        ok -> {ok, sent};  % Bridge RPC is fire-and-forget at transport level
        {error, Reason} -> {error, Reason}
    end;
call(_, _, _, _) ->
    {error, invalid_connection}.

%% @doc Send a message over a connection asynchronously.
%% Fire-and-forget delivery - returns immediately.
-spec send(pid(), term()) -> ok | {error, term()}.
send(Pid, Message) when is_pid(Pid), is_map(Message) ->
    send_message_async(Pid, bridge_data, Message),
    ok;
send(Pid, Message) when is_pid(Pid) ->
    %% Wrap non-map messages
    send_message_async(Pid, bridge_data, #{payload => Message}),
    ok;
send(_, _) ->
    {error, invalid_connection}.

%% @doc Connect to a remote endpoint.
%% Creates a new QUIC connection to the specified host:port.
-spec connect(binary(), pos_integer(), map()) -> {ok, pid()} | {error, term()}.
connect(Host, Port, Opts) when is_binary(Host), is_integer(Port), Port > 0 ->
    Url = iolist_to_binary([<<"quic://">>, Host, <<":">>, integer_to_binary(Port)]),
    start_link(Url, Opts);
connect(Host, Port, Opts) when is_list(Host) ->
    connect(list_to_binary(Host), Port, Opts);
connect(_, _, _) ->
    {error, invalid_arguments}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Url, Opts}) ->
    ?LOG_INFO("[Connection] Starting for ~s", [Url]),

    %% Parse URL to extract host and port
    {Host, Port} = parse_url(Url),

    %% Get realm (required)
    Realm = get_realm_from_opts(Opts),

    %% Generate or get node ID
    NodeId = maps:get(node_id, Opts, generate_node_id()),

    %% Get peer_id from opts (set by macula_peer_system)
    PeerId = maps:get(peer_id, Opts, erlang:unique_integer([monotonic, positive])),

    State = #state{
        url = Url,
        opts = Opts#{host => Host, port => Port},
        node_id = NodeId,
        realm = Realm,
        peer_id = PeerId,
        connection = undefined,
        stream = undefined,
        status = connecting,
        recv_buffer = <<>>,
        keepalive_timer = undefined
    },

    %% Initiate connection asynchronously
    self() ! connect,

    {ok, State}.

handle_call({send_message, Type, Msg}, _From, #state{status = connected, stream = Stream} = State) ->
    case send_message_raw(Type, Msg, Stream) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({send_message, _Type, _Msg}, _From, State) ->
    {reply, {error, not_connected}, State};

handle_call(get_status, _From, State) ->
    {reply, State#state.status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle async send message - fire and forget.
%% Sends if connected, logs warning and drops if not.
handle_cast({send_message_async, Type, Msg}, #state{status = connected, stream = Stream} = State) ->
    case send_message_raw(Type, Msg, Stream) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            ?LOG_WARNING("Async send failed for type ~p: ~p", [Type, Reason]),
            {noreply, State}
    end;

handle_cast({send_message_async, Type, _Msg}, State) ->
    ?LOG_DEBUG("Async send dropped (not connected): type=~p, status=~p", [Type, State#state.status]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Already connecting — ignore duplicate connect messages
handle_info(connect, #state{connect_in_flight = true} = State) ->
    {noreply, State};
%% Already connected — ignore
handle_info(connect, #state{status = connected} = State) ->
    {noreply, State};
%% Not connected, not in flight — attempt connection
handle_info(connect, State) ->
    ?LOG_DEBUG("[Connection] Attempting async connect to ~s", [State#state.url]),
    spawn_connect(State),
    {noreply, State#state{connect_in_flight = true}};

handle_info({connect_result, {ok, Conn, Stream}}, State) ->
    %% Ownership was already transferred by the spawned process before it exited.
    %% Now set active mode so we receive {quic, ...} messages.
    quicer:setopt(Stream, active, true),
    #{host := Host, port := Port} = State#state.opts,
    case complete_connection_setup(Conn, Stream, Host, Port, State) of
        {ok, State2} ->
            ?LOG_INFO("[Connection] Successfully connected to ~s", [State#state.url]),
            {noreply, State2#state{connect_in_flight = false,
                                    retry_delay_ms = ?INITIAL_RETRY_DELAY_MS}};
        {error, Reason} ->
            NextDelay = State#state.retry_delay_ms,
            ?LOG_ERROR("[Connection] Connection setup failed for ~s: ~p, retrying in ~.1fs",
                      [State#state.url, Reason, NextDelay / 1000]),
            erlang:send_after(NextDelay, self(), connect),
            {noreply, State#state{status = error, connect_in_flight = false,
                                   retry_delay_ms = min(NextDelay * 2, ?MAX_RETRY_DELAY_MS)}}
    end;

handle_info({connect_result, {error, Reason}}, State) ->
    NextDelay = State#state.retry_delay_ms,
    ?LOG_ERROR("[Connection] Connection setup failed for ~s: ~p, retrying in ~.1fs",
              [State#state.url, Reason, NextDelay / 1000]),
    erlang:send_after(NextDelay, self(), connect),
    {noreply, State#state{status = error, connect_in_flight = false,
                           retry_delay_ms = min(NextDelay * 2, ?MAX_RETRY_DELAY_MS)}};

handle_info({quic, Data, Stream, _Props}, State) when is_binary(Data) ->
    %% Received data from QUIC stream
    MainStream = State#state.stream,
    ?LOG_DEBUG("[Connection] QUIC data received: ~p bytes, from_stream=~p, main_stream=~p, match=~p",
                 [byte_size(Data), Stream, MainStream, Stream =:= MainStream]),
    handle_stream_data(Stream =:= MainStream, Data, Stream, State);

%% Handle QUIC control messages on the MAIN stream — trigger reconnect
handle_info({quic, ControlMsg, Stream, _Props}, #state{stream = Stream} = State) when is_atom(ControlMsg) ->
    handle_quic_control_message(ControlMsg, Stream, State);
%% Handle QUIC control messages on OTHER streams (temp DHT streams) — ignore
handle_info({quic, ControlMsg, OtherStream, _Props}, State) when is_atom(ControlMsg) ->
    ?LOG_DEBUG("[Connection] Ignoring control message ~p on non-main stream ~p", [ControlMsg, OtherStream]),
    {noreply, State};

%% Handle keep-alive tick - send PING message
handle_info(keepalive_tick, #state{status = connected, stream = Stream} = State) ->
    %% Send PING message
    PingMsg = #{timestamp => erlang:system_time(millisecond)},
    case send_message_raw(ping, PingMsg, Stream) of
        ok ->
            ?LOG_DEBUG("Keep-alive PING sent"),
            %% Restart timer for next keep-alive
            StateWithTimer = start_keepalive_timer(State),
            {noreply, StateWithTimer};
        {error, Reason} ->
            ?LOG_WARNING("Failed to send keep-alive PING: ~p", [Reason]),
            %% Connection might be dead - let it retry
            {noreply, State}
    end;

%% Ignore keep-alive tick if not connected
handle_info(keepalive_tick, State) ->
    {noreply, State};

%% Mesh peer lifecycle events (from gateway pub/sub via _mesh.peer.* topics)
handle_info({mesh_peer_connected, PeerInfo}, State) ->
    PeerNodeId = maps:get(<<"node_id">>, PeerInfo, maps:get(node_id, PeerInfo, <<>>)),
    ?LOG_INFO("[Connection] Peer connected: ~s", [PeerNodeId]),
    add_discovered_peers([PeerInfo], State#state.node_id),
    notify_mesh_lifecycle_observers(mesh_peer_connected, PeerInfo),
    {noreply, State};

handle_info({mesh_peer_disconnected, PeerInfo}, State) ->
    NodeId = maps:get(<<"node_id">>, PeerInfo, maps:get(node_id, PeerInfo, undefined)),
    RawNodeId = normalize_peer_node_id(NodeId),
    ?LOG_INFO("[Connection] Peer disconnected: ~s",
              [binary:encode_hex(RawNodeId)]),
    remove_peer_from_routing_table(RawNodeId),
    notify_mesh_lifecycle_observers(mesh_peer_disconnected, PeerInfo),
    {noreply, State};

handle_info(_Info, State) ->
    %% Ignore unexpected messages
    ?LOG_DEBUG("Unhandled handle_info message: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{stream = Stream, connection = Conn}) ->
    ?LOG_INFO("Connection manager terminating"),
    %% Do NOT invalidate pool — pool detects dead connections via is_connection_alive.
    %% Clean up QUIC resources
    catch macula_quic:close(Stream),
    catch macula_quic:close(Conn),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Dispatch stream data based on validity (pattern matching on boolean).
handle_stream_data(true, Data, _Stream, State) ->
    ?LOG_DEBUG("[Connection] handle_stream_data(true) ENTRY: ~p bytes", [byte_size(Data)]),
    handle_received_data(Data, State);
handle_stream_data(false, _Data, Stream, State) ->
    ?LOG_WARNING("Received data from unknown stream: ~p", [Stream]),
    {noreply, State}.

%% @doc Handle QUIC control messages - trigger reconnect on connection/stream closure.
-spec handle_quic_control_message(atom(), pid(), #state{}) -> {noreply, #state{}}.

%% Stream closed - trigger reconnection
handle_quic_control_message(peer_send_shutdown, _Stream, State) ->
    ?LOG_WARNING("QUIC stream closed by peer (peer_send_shutdown), reconnecting..."),
    trigger_reconnect(State);

handle_quic_control_message(peer_send_aborted, _Stream, State) ->
    ?LOG_WARNING("QUIC stream aborted by peer, reconnecting..."),
    trigger_reconnect(State);

handle_quic_control_message(send_shutdown_complete, _Stream, State) ->
    ?LOG_WARNING("QUIC send shutdown complete, reconnecting..."),
    trigger_reconnect(State);

handle_quic_control_message(shutdown, _Stream, State) ->
    ?LOG_WARNING("QUIC shutdown, reconnecting..."),
    trigger_reconnect(State);

handle_quic_control_message(closed, _Stream, State) ->
    ?LOG_WARNING("QUIC connection closed, reconnecting..."),
    trigger_reconnect(State);

%% Other control messages - log and ignore
handle_quic_control_message(ControlMsg, _Stream, State) ->
    ?LOG_DEBUG("Ignoring QUIC control message: ~p", [ControlMsg]),
    {noreply, State}.

%% @doc Trigger reconnection by cleaning up and scheduling reconnect.
-spec trigger_reconnect(#state{}) -> {noreply, #state{}}.
trigger_reconnect(#state{stream = Stream, connection = Conn, keepalive_timer = Timer} = State) ->
    %% Cancel keep-alive timer
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,

    %% Do NOT invalidate pool here — another macula_connection instance may
    %% have seeded the pool with ITS connection. The pool's is_connection_alive
    %% check will detect dead connections on the next get_connection call.

    %% Clean up old connection resources
    catch macula_quic:close(Stream),
    catch macula_quic:close(Conn),

    %% Schedule reconnect with backoff (reset to initial since this was a working connection)
    erlang:send_after(?INITIAL_RETRY_DELAY_MS, self(), connect),

    %% Update state to disconnected
    NewState = State#state{
        connection = undefined,
        stream = undefined,
        status = disconnected,
        recv_buffer = <<>>,
        keepalive_timer = undefined,
        connect_in_flight = false,
        retry_delay_ms = ?INITIAL_RETRY_DELAY_MS
    },
    {noreply, NewState}.

%% @doc Spawn async QUIC connection attempt so the gen_server remains responsive
%% to calls (e.g., get_status) while the QUIC handshake is in progress.
-spec spawn_connect(#state{}) -> pid().
spawn_connect(State) ->
    Self = self(),
    #{host := Host, port := Port} = State#state.opts,
    QuicOpts = build_quic_opts(Host),
    spawn(fun() ->
        Result = try
            case attempt_quic_connection(Host, Port, QuicOpts) of
                {ok, Conn, Stream} ->
                    %% Transfer ownership to gen_server BEFORE this process exits.
                    %% If we don't, quicer closes the stream/connection when this
                    %% spawned process terminates (it's the controlling process).
                    ok = quicer:controlling_process(Conn, Self),
                    ok = quicer:controlling_process(Stream, Self),
                    {ok, Conn, Stream};
                Other ->
                    Other
            end
        catch
            _:Reason -> {error, {crashed, Reason}}
        end,
        Self ! {connect_result, Result}
    end).

%% @doc Build QUIC connection options with TLS configuration.
%% Uses macula_tls module for centralized TLS settings (v0.11.0+).
%% Hostname is used for TLS hostname verification in production mode.
-spec build_quic_opts(Host :: string() | binary()) -> list().
build_quic_opts(Host) ->
    %% Get TLS options with hostname verification from centralized module
    TlsOpts = macula_tls:quic_client_opts_with_hostname(Host),

    %% Merge with QUIC-specific options
    BaseOpts = [
        {alpn, ["macula"]},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000},
        {handshake_idle_timeout_ms, 30000}
    ],
    merge_opts(BaseOpts, TlsOpts).

%% @doc Merge two option lists, with second list taking precedence.
-spec merge_opts(list(), list()) -> list().
merge_opts(BaseOpts, OverrideOpts) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            lists:keystore(Key, 1, Acc, {Key, Value})
        end,
        BaseOpts,
        OverrideOpts
    ).

%% @doc Attempt QUIC connection and stream setup
-spec attempt_quic_connection(string(), integer(), list()) ->
    {ok, pid(), pid()} | {error, term()}.
attempt_quic_connection(Host, Port, QuicOpts) ->
    case safe_quic_connect(Host, Port, QuicOpts) of
        {ok, Conn} ->
            setup_bidirectional_stream(Conn);
        {error, _Reason} = Error ->
            Error;
        {error, _Type, _Details} = Error ->
            {error, {connection_failed, Error}};
        Other ->
            {error, {connection_failed, Other}}
    end.

%% @doc Safe QUIC connect with error handling.
%% NIF boundary - quicer can throw exceptions, convert to tagged tuples.
-spec safe_quic_connect(string(), integer(), list()) ->
    {ok, pid()} | {error, term()}.
safe_quic_connect(Host, Port, QuicOpts) ->
    handle_quic_connect_result(catch macula_quic:connect(Host, Port, QuicOpts, ?CONNECTION_TIMEOUT_MS)).

%% @private Handle QUIC connect result, converting exceptions to error tuples.
handle_quic_connect_result({'EXIT', Error}) ->
    {error, {connection_failed, Error}};
handle_quic_connect_result({ok, _Conn} = Result) ->
    Result;
handle_quic_connect_result({error, _Reason} = Result) ->
    Result;
handle_quic_connect_result({error, Type, Details}) ->
    {error, {quic_transport_error, Type, Details}};
handle_quic_connect_result(Other) ->
    {error, {unexpected_connect_result, Other}}.

%% @doc Open and configure bidirectional stream
-spec setup_bidirectional_stream(reference()) ->
    {ok, reference(), reference()} | {error, term()}.
setup_bidirectional_stream(Conn) ->
    case macula_quic:open_stream(Conn) of
        {ok, Stream} ->
            configure_stream_active_mode(Conn, Stream);
        {error, Reason} ->
            macula_quic:close(Conn),
            {error, {stream_open_failed, Reason}}
    end.

%% @doc Verify stream is usable after opening.
%% NOTE: active mode is NOT set here because this runs in a spawned process.
%% The gen_server takes ownership and sets active mode in handle_info({connect_result, ...}).
%% Setting active here would make the spawned process the stream owner, and when it
%% terminates, quicer closes the stream — causing {error, closed} on subsequent sends.
-spec configure_stream_active_mode(reference(), reference()) ->
    {ok, reference(), reference()} | {error, term()}.
configure_stream_active_mode(Conn, Stream) ->
    {ok, Conn, Stream}.

%% @doc Complete connection setup with handshake and DHT registration
-spec complete_connection_setup(reference(), reference(), string(), integer(), #state{}) ->
    {ok, #state{}} | {error, term()}.
complete_connection_setup(Conn, Stream, Host, Port, State) ->
    ConnectMsg = build_connect_message(State),

    case send_message_raw(connect, ConnectMsg, Stream) of
        ok ->
            ?LOG_INFO("Connected to Macula mesh: ~s:~p", [Host, Port]),

            %% Add server to DHT routing table immediately so STORE/FIND_VALUE
            %% operations have a target. In client mode, this is the ONLY peer
            %% in the routing table — without it, DHT operations go nowhere.
            add_server_to_routing_table(Host, Port),

            %% Seed connection pool so DHT reuses this QUIC connection
            seed_connection_pool(Host, Port, Conn),

            %% Start keep-alive timer if enabled
            ConnectedState = State#state{
                connection = Conn,
                stream = Stream,
                status = connected
            },
            StateWithKeepalive = start_keepalive_timer(ConnectedState),

            %% Replay subscriptions so the gateway knows about them on the new stream.
            %% After reconnect, the gateway lost all stream subscriptions from the old
            %% connection. The pubsub_handler still has the application's subscriptions.
            replay_peer_subscriptions(StateWithKeepalive),

            {ok, StateWithKeepalive};
        {error, Reason} ->
            macula_quic:close(Stream),
            macula_quic:close(Conn),
            {error, {handshake_failed, Reason}}
    end.

%% @doc Build CONNECT protocol message
-spec build_connect_message(#state{}) -> map().
build_connect_message(State) ->
    LocalEndpoint = get_advertise_endpoint(),
    #{
        version => <<"1.0">>,
        node_id => State#state.node_id,
        realm_id => State#state.realm,
        capabilities => [pubsub, rpc],
        endpoint => LocalEndpoint
    }.

%% @doc Get endpoint to advertise for peer-to-peer connections
-spec get_advertise_endpoint() -> binary().
get_advertise_endpoint() ->
    case application:get_env(macula, advertise_endpoint) of
        {ok, Endpoint} when is_binary(Endpoint) ->
            Endpoint;
        _ ->
            construct_default_endpoint()
    end.

%% @doc Notify the peer's pubsub_handler to re-send SUBSCRIBE messages after reconnect.
-spec replay_peer_subscriptions(#state{}) -> ok.
replay_peer_subscriptions(#state{realm = Realm, peer_id = PeerId}) ->
    case gproc:lookup_local_name({pubsub_handler, Realm, PeerId}) of
        undefined ->
            ?LOG_DEBUG("[Connection] No pubsub_handler for replay (realm=~s, peer_id=~p)",
                      [Realm, PeerId]);
        PubSubPid ->
            gen_server:cast(PubSubPid, replay_subscriptions)
    end,
    ok.

%% @doc Add the connected server to the local DHT routing table.
%% In client mode, this is the ONLY peer — without it, STORE and FIND_VALUE
%% have no target and DHT operations silently fail (empty results).
-spec add_server_to_routing_table(string(), integer()) -> ok.
add_server_to_routing_table(Host, Port) ->
    Endpoint = iolist_to_binary([<<"https://">>, list_to_binary(Host), <<":">>, integer_to_binary(Port)]),
    ServerNodeId = crypto:hash(sha256, Endpoint),
    ServerNodeInfo = #{
        node_id => ServerNodeId,
        address => Endpoint,
        endpoint => Endpoint
    },
    case whereis(macula_routing_server) of
        undefined -> ok;
        _Pid ->
            case macula_routing_server:add_node(macula_routing_server, ServerNodeInfo) of
                ok ->
                    ?LOG_INFO("[Connection] Added server to routing table: ~s", [Endpoint]);
                {error, Reason} ->
                    ?LOG_WARNING("[Connection] Failed to add server to routing table: ~p", [Reason])
            end
    end,
    ok.

%% @doc Construct default endpoint from environment and application config.
%% Port resolution order: MACULA_QUIC_PORT env var, application:get_env(macula, quic_port), 9443.
-spec construct_default_endpoint() -> binary().
construct_default_endpoint() ->
    NodeHost = get_hostname_from_env(),
    Port = get_quic_port_binary(),
    <<"https://", NodeHost/binary, ":", Port/binary>>.

%% @doc Resolve QUIC port: env var first, then app config, then default 9443.
-spec get_quic_port_binary() -> binary().
get_quic_port_binary() ->
    case os:getenv("MACULA_QUIC_PORT") of
        false ->
            case application:get_env(macula, quic_port) of
                {ok, P} when is_integer(P) -> integer_to_binary(P);
                {ok, P} when is_list(P) -> list_to_binary(P);
                _ -> <<"9443">>
            end;
        EnvPort -> list_to_binary(EnvPort)
    end.

%% @doc Get hostname from environment variables.
%% Checks MACULA_HOSTNAME first (Docker), then NODE_HOST, then HOSTNAME, fallback to localhost.
-spec get_hostname_from_env() -> binary().
get_hostname_from_env() ->
    get_hostname_from_env([
        "MACULA_HOSTNAME",  %% Docker compose sets this
        "NODE_HOST",        %% Legacy/alternative
        "HOSTNAME"          %% Standard shell variable
    ]).

%% @doc Try environment variables in order, then node name, then localhost.
-spec get_hostname_from_env([string()]) -> binary().
get_hostname_from_env([]) ->
    hostname_from_node();
get_hostname_from_env([EnvVar | Rest]) ->
    case os:getenv(EnvVar) of
        false -> get_hostname_from_env(Rest);
        Value -> list_to_binary(Value)
    end.

%% @private Extract hostname from Erlang node name (e.g. hecate@beam00.lab -> beam00.lab).
hostname_from_node() ->
    case atom_to_list(node()) of
        "nonode@nohost" -> <<"localhost">>;
        NodeStr ->
            case string:split(NodeStr, "@") of
                [_, Host] -> list_to_binary(Host);
                _ -> <<"localhost">>
            end
    end.

%% @doc Register connected server in DHT routing table using info from PONG response.
%% Called when PONG message contains server's real node_id and endpoint.
%% If PONG doesn't contain node_id (keep-alive pong), does nothing.
-spec maybe_register_server_in_dht(map(), #state{}) -> ok.
maybe_register_server_in_dht(PongMsg, State) ->
    %% Check for node_id in PONG (binary key from msgpack decoding)
    ServerNodeId = maps:get(<<"node_id">>, PongMsg, undefined),
    ServerEndpoint = maps:get(<<"endpoint">>, PongMsg, undefined),
    do_register_server_in_dht(ServerNodeId, ServerEndpoint, State).

%% @private Only register if we have the server's real node_id
do_register_server_in_dht(undefined, _Endpoint, _State) ->
    %% Keep-alive PONG - no server info to register
    ok;
do_register_server_in_dht(ServerNodeId, ServerEndpoint, State) when is_binary(ServerNodeId) ->
    %% First PONG after CONNECT - contains real server info
    ServerAddress = parse_server_endpoint(State#state.url),
    ServerNodeInfo = #{
        node_id => ServerNodeId,
        address => ServerAddress,
        endpoint => ServerEndpoint
    },
    ?LOG_DEBUG("[Connection] Registering server in DHT: node_id=~s, endpoint=~s",
              [binary:encode_hex(ServerNodeId), ServerEndpoint]),

    case macula_routing_server:add_node(macula_routing_server, ServerNodeInfo) of
        ok ->
            ?LOG_DEBUG("[Connection] Added server to DHT routing table: ~s",
                     [binary:encode_hex(ServerNodeId)]);
        {error, Reason} ->
            ?LOG_WARNING("DHT registration failed (expected if DHT not running): ~p", [Reason])
    end,
    ok.

%% @doc Add discovered peers from a FIND_NODE reply to our routing table.
-spec add_discovered_peers([map()], binary()) -> ok.
add_discovered_peers(Nodes, MyNodeId) ->
    add_discovered_peers(whereis(macula_routing_server), Nodes, MyNodeId).

add_discovered_peers(undefined, _Nodes, _MyNodeId) ->
    ?LOG_WARNING("[Connection] No routing server — cannot add discovered peers"),
    ok;
add_discovered_peers(RoutingServer, Nodes, MyNodeId) ->
    ValidPeers = lists:filtermap(fun(NodeInfo) -> extract_peer_info(NodeInfo, MyNodeId) end, Nodes),
    lists:foreach(fun(Info) ->
        macula_routing_server:add_node(RoutingServer, Info)
    end, ValidPeers),
    ok.

%% @doc Extract peer info from a FIND_NODE reply node entry, skipping ourselves.
-spec extract_peer_info(map(), binary()) -> {true, map()} | false.
extract_peer_info(NodeInfo, MyNodeId) when is_map(NodeInfo) ->
    NodeId = get_map_field(NodeInfo, node_id, <<"node_id">>),
    Endpoint = get_map_field(NodeInfo, endpoint, <<"endpoint">>,
                  get_map_field(NodeInfo, address, <<"address">>)),
    make_peer_info(NodeId, Endpoint, MyNodeId);
extract_peer_info(_, _) ->
    false.

make_peer_info(undefined, _, _) -> false;
make_peer_info(MyId, _, MyId) -> false;
make_peer_info(NodeId, Endpoint, MyId) ->
    %% Normalize: hex-encoded (64 bytes) → raw binary (32 bytes)
    RawNodeId = normalize_peer_node_id(NodeId),
    skip_if_self(RawNodeId, MyId, Endpoint).

skip_if_self(MyId, MyId, _) -> false;
skip_if_self(RawNodeId, _, Endpoint) ->
    {true, #{node_id => RawNodeId, address => Endpoint, endpoint => Endpoint}}.

normalize_peer_node_id(Bin) when byte_size(Bin) =:= 32 -> Bin;
normalize_peer_node_id(Hex) when byte_size(Hex) =:= 64 -> binary:decode_hex(Hex);
normalize_peer_node_id(Other) -> Other.

%% @doc Get a field from a map trying atom key first, then binary key.
-spec get_map_field(map(), atom(), binary()) -> term().
get_map_field(Map, AtomKey, BinKey) ->
    maps:get(AtomKey, Map, maps:get(BinKey, Map, undefined)).

-spec get_map_field(map(), atom(), binary(), term()) -> term().
get_map_field(Map, AtomKey, BinKey, Default) ->
    case maps:get(AtomKey, Map, maps:get(BinKey, Map, undefined)) of
        undefined -> Default;
        Value -> Value
    end.

%% @doc Handle internal mesh lifecycle pub/sub events.
%% Intercepts _mesh.peer.connected and _mesh.peer.disconnected before
%% passing other publish messages to the application-level pubsub handler.
handle_mesh_lifecycle_publish(<<"_mesh.peer.connected">>, Msg) ->
    Payload = maps:get(<<"payload">>, Msg, Msg),
    self() ! {mesh_peer_connected, Payload},
    handled;
handle_mesh_lifecycle_publish(<<"_mesh.peer.disconnected">>, Msg) ->
    Payload = maps:get(<<"payload">>, Msg, Msg),
    self() ! {mesh_peer_disconnected, Payload},
    handled;
handle_mesh_lifecycle_publish(_, _) ->
    passthrough.

%% @doc Populate routing table from peers list in PONG (initial state on connect).
maybe_populate_peers_from_pong(PongMsg, #state{node_id = MyNodeId}) ->
    Peers = maps:get(<<"peers">>, PongMsg, maps:get(peers, PongMsg, undefined)),
    populate_peers(Peers, MyNodeId).

populate_peers(undefined, _) -> ok;
populate_peers(Peers, MyNodeId) when is_list(Peers), length(Peers) > 0 ->
    ?LOG_DEBUG("[Connection] PONG contains ~p peer(s), populating routing table", [length(Peers)]),
    add_discovered_peers(Peers, MyNodeId),
    notify_mesh_lifecycle_observers(mesh_peers_initial, #{peers => Peers});
populate_peers(_, _) -> ok.

%% @doc Notify external observers of mesh peer lifecycle events via pg group.
%% Any process can join the macula_mesh_lifecycle pg group to receive these.
notify_mesh_lifecycle_observers(EventType, PeerInfo) ->
    Members = pg:get_members(pg, macula_mesh_lifecycle),
    [Pid ! {macula_mesh_event, EventType, PeerInfo} || Pid <- Members],
    ok.

%% @doc Remove a peer from the local routing table on disconnect notification.
remove_peer_from_routing_table(undefined) -> ok;
remove_peer_from_routing_table(NodeId) when is_binary(NodeId) ->
    case whereis(macula_routing_server) of
        undefined -> ok;
        RoutingServer -> macula_routing_server:remove_node(RoutingServer, NodeId)
    end.

%% @doc Legacy function - replaced by maybe_register_server_in_dht/2.
%% Called during connect but now just logs - real registration happens in PONG handler.
%% @doc Send a protocol message through a stream (raw).
%% Crashes if message is invalid - this indicates a bug in the caller.
-spec send_message_raw(atom(), map(), reference()) -> ok | {error, term()}.
send_message_raw(Type, Msg, Stream) ->
    ?LOG_DEBUG("Type=~p, Msg=~p", [Type, Msg]),
    Binary = macula_protocol_encoder:encode(Type, Msg),
    macula_quic:async_send(Stream, Binary).

%% @doc Handle received data from the stream.
-spec handle_received_data(binary(), #state{}) -> {noreply, #state{}}.
handle_received_data(Data, State) ->
    %% Append to receive buffer
    Buffer = <<(State#state.recv_buffer)/binary, Data/binary>>,
    ?LOG_DEBUG("[Connection] handle_received_data ENTRY: data=~p bytes, buffer=~p bytes", [byte_size(Data), byte_size(Buffer)]),

    %% Try to decode messages
    {Messages, RemainingBuffer} = decode_messages(Buffer, []),
    ?LOG_DEBUG("[Connection] decode_messages returned: ~p messages, remaining=~p bytes", [length(Messages), byte_size(RemainingBuffer)]),

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
            ?LOG_DEBUG("[Connection] DECODING message: buffer=~p bytes, payload_len=~p", [byte_size(Buffer), PayloadLen]),
            case macula_protocol_decoder:decode(Buffer) of
                {ok, {Type, Msg}} ->
                    %% Skip this message and continue
                    ?LOG_DEBUG("[Connection] DECODED OK: type=~p", [Type]),
                    <<_:8/binary, _Payload:PayloadLen/binary, Remaining/binary>> = Buffer,
                    decode_messages(Remaining, [{Type, Msg} | Acc]);
                {error, Reason} ->
                    %% Decode error, skip this message - LOG THE ERROR
                    <<_:8, TypeIdByte:8, _:6/binary, _Payload:PayloadLen/binary, Remaining/binary>> = Buffer,
                    ?LOG_WARNING("[Connection] DECODE ERROR: type_id=~p (0x~.16B), reason=~p, payload_len=~p",
                                 [TypeIdByte, TypeIdByte, Reason, PayloadLen]),
                    decode_messages(Remaining, Acc)
            end;
        _ ->
            %% Incomplete message, wait for more data
            {lists:reverse(Acc), Buffer}
    end.

%% @doc Process a received message - route to appropriate handler.
-spec process_message({atom(), map()}, #state{}) -> #state{}.

%% Route PUBLISH messages to pub/sub handler
process_message({publish, Msg}, State) ->
    Topic = maps:get(<<"topic">>, Msg, maps:get(topic, Msg, <<>>)),
    ?LOG_INFO("[peer] Incoming PUBLISH topic=~s", [Topic]),
    %% Intercept internal mesh lifecycle events
    case handle_mesh_lifecycle_publish(Topic, Msg) of
        handled -> State;
        passthrough ->
            %% Route to pubsub handler for application-level subscribers
            case gproc:lookup_local_name({pubsub_handler, State#state.realm, State#state.peer_id}) of
                undefined ->
                    ?LOG_WARNING("PubSub handler not found for realm ~s, peer_id ~p",
                                [State#state.realm, State#state.peer_id]),
                    State;
                PubSubPid ->
                    macula_pubsub_handler:handle_incoming_publish(PubSubPid, Msg),
                    State
            end
    end;

%% Route REPLY messages to RPC handler
process_message({reply, Msg}, State) ->
    ?LOG_INFO("Connection manager routing message type: ~p", [reply]),
    %% Look up the RPC handler by node_id (not peer_id) so replies arriving on ANY
    %% connection belonging to this node route to the correct handler
    case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.node_id}) of
        undefined ->
            ?LOG_WARNING("RPC handler not found for realm ~s, node_id ~s when routing REPLY",
                        [State#state.realm, binary:encode_hex(State#state.node_id)]),
            State;
        RpcPid ->
            macula_rpc_handler:handle_incoming_reply(RpcPid, Msg),
            ?LOG_DEBUG("Routed REPLY to rpc_handler"),
            State
    end;

%% Handle CONNECTED acknowledgment
process_message({connected, Msg}, State) ->
    ?LOG_INFO("Received CONNECTED acknowledgment from server: ~p", [Msg]),
    State;

%% Handle PING message - respond with PONG
process_message({ping, Msg}, #state{stream = Stream} = State) ->
    ?LOG_DEBUG("Received PING, responding with PONG"),
    PongMsg = #{timestamp => maps:get(timestamp, Msg, 0)},
    _ = send_message_raw(pong, PongMsg, Stream),
    State;

%% Handle PONG message - keep-alive acknowledgment and server DHT registration
%% The first PONG after CONNECT contains server's node_id and endpoint for DHT routing
process_message({pong, PongMsg}, State) ->
    ?LOG_DEBUG("Received PONG - connection alive"),
    %% Check if PONG contains server's node_id (set by gateway on first response)
    maybe_register_server_in_dht(PongMsg, State),
    %% Check if PONG contains peers list (initial state on connect)
    maybe_populate_peers_from_pong(PongMsg, State),
    State;

%% Handle FIND_VALUE_REPLY message - route to RPC handler for DHT query results
process_message({find_value_reply, Msg}, State) ->
    ?LOG_DEBUG("Connection manager routing message type: ~p", [find_value_reply]),
    %% Look up the RPC handler by node_id (not peer_id) so replies arriving on ANY
    %% connection belonging to this node route to the correct handler
    case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.node_id}) of
        undefined ->
            ?LOG_WARNING("RPC handler not found for realm ~s, node_id ~s",
                        [State#state.realm, binary:encode_hex(State#state.node_id)]),
            State;
        RpcPid ->
            macula_rpc_handler:handle_find_value_reply(RpcPid, Msg),
            ?LOG_DEBUG("Routed FIND_VALUE_REPLY to rpc_handler"),
            State
    end;

%% Handle RPC_REQUEST message - execute local handler and send reply back via stream.
%% This is used for relay scenarios: bootstrap forwards RPC_REQUEST through client stream,
%% and we handle it locally and reply through the same stream.
%%
%% IMPORTANT: We search ALL RPC handlers in this realm for the procedure, not just
%% the one associated with this connection's peer_id. This is because:
%% - The procedure handler (e.g., ping.handler) is registered by macula_ping_pong
%% - macula_ping_pong uses the RPC handler from macula_peers_sup (outbound connection)
%% - But the RPC_REQUEST arrives via the gateway's inbound client stream (different peer_id)
%% - So we need to search all RPC handlers to find the procedure
process_message({rpc_request, Msg}, #state{stream = Stream} = State) ->
    ?LOG_WARNING("[Connection] PROCESSING RPC_REQUEST locally (relay scenario)"),
    RequestId = maps:get(<<"request_id">>, Msg, undefined),
    Procedure = maps:get(<<"procedure">>, Msg, undefined),
    Args = maps:get(<<"args">>, Msg, <<>>),
    FromNode = maps:get(<<"from_node">>, Msg, undefined),

    ?LOG_WARNING("[Connection] RPC_REQUEST details: procedure=~s, request_id=~p, from_node=~p",
                [Procedure, RequestId, FromNode]),

    %% Search ALL RPC handlers in this realm for the procedure
    %% This handles relay scenarios where handler is in a different peer system
    Result = find_and_execute_handler(State#state.realm, Procedure, Args),

    ?LOG_WARNING("[Connection] RPC_REQUEST processing result: ~p", [Result]),

    %% Build and send reply back through the same stream
    %% Include to_node (original requester) so bootstrap can forward the reply
    ReplyMsg = case Result of
        {ok, ResultValue} ->
            EncodedResult = try macula_utils:encode_json(ResultValue) catch _:_ -> ResultValue end,
            #{
                request_id => RequestId,
                result => EncodedResult,
                from_node => State#state.node_id,
                to_node => FromNode,  %% Original requester - bootstrap uses this to route reply
                timestamp => erlang:system_time(millisecond)
            };
        {error, ErrorReason} ->
            #{
                request_id => RequestId,
                error => ErrorReason,
                from_node => State#state.node_id,
                to_node => FromNode,  %% Original requester - bootstrap uses this to route reply
                timestamp => erlang:system_time(millisecond)
            }
    end,

    ?LOG_WARNING("[Connection] Sending RPC_REPLY back through stream: ~p", [ReplyMsg]),
    EncodedReply = macula_protocol_encoder:encode(rpc_reply, ReplyMsg),
    ?LOG_WARNING("[Connection] EncodedReply size: ~p bytes", [byte_size(EncodedReply)]),
    case macula_quic:send(Stream, EncodedReply) of
        ok ->
            ?LOG_WARNING("[Connection] Successfully sent RPC_REPLY");
        {error, SendError} ->
            ?LOG_WARNING("[Connection] Failed to send RPC_REPLY: ~p", [SendError])
    end,
    State;

%% Handle RPC_REPLY message - route to RPC handler for async callback invocation.
%% This handles replies that come back through the client connection stream.
%% IMPORTANT: Use handle_async_reply (not handle_incoming_reply) because
%% async RPC uses request_id field, while sync RPC uses call_id field.
%% CRITICAL: Look up by node_id (not peer_id) so replies arriving on ANY connection
%% belonging to this node route to the correct handler. This fixes the relay scenario
%% where request goes out via outbound connection but reply arrives via inbound stream.
process_message({rpc_reply, Msg}, State) ->
    RequestId = maps:get(<<"request_id">>, Msg, maps:get(request_id, Msg, undefined)),
    ?LOG_WARNING("[Connection] RECEIVED RPC_REPLY: request_id=~p, node_id=~s",
                [RequestId, binary:encode_hex(State#state.node_id)]),
    case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.node_id}) of
        undefined ->
            ?LOG_WARNING("[Connection] RPC_REPLY: RPC handler not found, realm=~p, node_id=~s",
                        [State#state.realm, binary:encode_hex(State#state.node_id)]),
            State;
        RpcPid ->
            ?LOG_WARNING("[Connection] RPC_REPLY: routing to RPC handler pid=~p", [RpcPid]),
            macula_rpc_handler:handle_async_reply(RpcPid, Msg),
            ?LOG_WARNING("[Connection] RPC_REPLY: delivered to RPC handler"),
            State
    end;

%% Handle unknown message types
%% Handle FIND_NODE reply — add discovered peers to our routing table
process_message({find_node_reply, Msg}, State) ->
    Nodes = maps:get(<<"nodes">>, Msg, maps:get(nodes, Msg, [])),
    ?LOG_INFO("[Connection] FIND_NODE reply with ~p node(s)", [length(Nodes)]),
    add_discovered_peers(Nodes, State#state.node_id),
    State;

process_message({Type, _Msg}, State) ->
    ?LOG_INFO("Connection manager routing message type: ~p", [Type]),
    ?LOG_WARNING("Unknown message type: ~p", [Type]),
    State.

%% @doc Parse URL to extract host and port.
-spec parse_url(binary()) -> {string(), integer()}.
parse_url(Url) when is_binary(Url) ->
    parse_url(binary_to_list(Url));
parse_url("https://" ++ Rest) ->
    parse_host_port(Rest, 443);
parse_url("http://" ++ Rest) ->
    parse_host_port(Rest, 80);
parse_url(Url) ->
    error({invalid_url, Url}).

%% @doc Parse host and port from URL remainder, using default port if not specified.
-spec parse_host_port(string(), integer()) -> {string(), integer()}.
parse_host_port(HostPort, DefaultPort) ->
    case string:split(HostPort, ":") of
        [Host, PortStr] -> {Host, list_to_integer(PortStr)};
        [Host] -> {Host, DefaultPort}
    end.

%% @doc Parse server endpoint to extract address for DHT.
-spec parse_server_endpoint(binary()) -> binary().
parse_server_endpoint(Url) ->
    %% Extract host from URL for DHT address
    case parse_url(Url) of
        {Host, Port} ->
            list_to_binary(Host ++ ":" ++ integer_to_list(Port));
        _ ->
            Url
    end.

%% @doc Generate a random node ID.
-spec generate_node_id() -> binary().
generate_node_id() ->
    macula_utils:generate_node_id().

%% @doc Get realm from options and normalize.
-spec get_realm_from_opts(map()) -> binary().
get_realm_from_opts(Opts) ->
    normalize_realm(maps:get(realm, Opts, undefined)).

%% @doc Normalize realm to binary.
-spec normalize_realm(undefined | binary() | list() | atom()) -> binary().
normalize_realm(undefined) ->
    error({missing_required_option, realm});
normalize_realm(Realm) when is_binary(Realm) ->
    Realm;
normalize_realm(Realm) when is_list(Realm) ->
    list_to_binary(Realm);
normalize_realm(Realm) when is_atom(Realm) ->
    atom_to_binary(Realm).

%% @doc Start keep-alive timer if enabled in options.
-spec start_keepalive_timer(#state{}) -> #state{}.
start_keepalive_timer(#state{opts = Opts, keepalive_timer = OldTimer} = State) ->
    %% Cancel existing timer if present
    case OldTimer of
        undefined -> ok;
        _ -> erlang:cancel_timer(OldTimer)
    end,

    %% Check if keep-alive is enabled
    Enabled = maps:get(keepalive_enabled, Opts, true),
    start_keepalive_timer(Enabled, State).

%% Keep-alive disabled
start_keepalive_timer(false, State) ->
    State#state{keepalive_timer = undefined};

%% Keep-alive enabled - start timer
start_keepalive_timer(true, #state{opts = Opts} = State) ->
    Interval = maps:get(keepalive_interval, Opts, 30000),
    TimerRef = erlang:send_after(Interval, self(), keepalive_tick),
    State#state{keepalive_timer = TimerRef}.

%% @private Find and execute a procedure handler by searching all RPC handlers in the realm.
%% This is used for relay scenarios where the handler may be registered with a different
%% peer system than the one associated with the incoming connection.
-spec find_and_execute_handler(binary(), binary(), binary() | map()) ->
    {ok, term()} | {error, binary()}.
find_and_execute_handler(Realm, Procedure, Args) ->
    ?LOG_WARNING("[Connection] Searching ALL RPC handlers in realm ~s for procedure ~s",
                [Realm, Procedure]),

    %% Get all RPC handlers registered in gproc for this realm
    %% Pattern: {rpc_handler, Realm, _} matches any peer_id
    Pattern = {n, l, {rpc_handler, Realm, '_'}},
    RpcHandlers = gproc:lookup_pids(Pattern),

    ?LOG_WARNING("[Connection] Found ~p RPC handlers in realm", [length(RpcHandlers)]),

    %% Search each RPC handler's service registry for the procedure
    find_handler_in_registries(RpcHandlers, Procedure, Args).

%% @private Search through RPC handlers to find one that has the procedure registered.
-spec find_handler_in_registries([pid()], binary(), binary() | map()) ->
    {ok, term()} | {error, binary()}.
find_handler_in_registries([], Procedure, _Args) ->
    ?LOG_WARNING("[Connection] Procedure ~s not found in any RPC handler", [Procedure]),
    {error, <<"procedure_not_found">>};
find_handler_in_registries([RpcPid | Rest], Procedure, Args) ->
    ?LOG_WARNING("[Connection] Checking RPC handler ~p for procedure ~s", [RpcPid, Procedure]),
    case macula_rpc_handler:get_service_registry(RpcPid) of
        {error, _} ->
            %% Try next handler
            find_handler_in_registries(Rest, Procedure, Args);
        Registry ->
            case macula_service_registry:get_local_handler(Registry, Procedure) of
                {ok, Handler} ->
                    ?LOG_WARNING("[Connection] FOUND handler for ~s in RPC handler ~p",
                                [Procedure, RpcPid]),
                    execute_handler(Handler, Args);
                not_found ->
                    %% Try next handler
                    find_handler_in_registries(Rest, Procedure, Args)
            end
    end.

%% @private Execute a handler function with the provided arguments.
-spec execute_handler(fun((map()) -> {ok, term()} | {error, term()}), binary() | map()) ->
    {ok, term()} | {error, binary()}.
execute_handler(Handler, Args) ->
    DecodedArgs = safe_decode_json(Args),
    safe_execute_handler(Handler, DecodedArgs).

%% @private Safely decode JSON, falling back to original args on decode failure.
-spec safe_decode_json(binary() | map()) -> term().
safe_decode_json(Args) ->
    case catch macula_utils:decode_json(Args) of
        {'EXIT', _} -> Args;
        Decoded -> Decoded
    end.

%% @private Safely execute handler, returning error tuple on exception.
-spec safe_execute_handler(fun((map()) -> {ok, term()} | {error, term()}), term()) ->
    {ok, term()} | {error, binary()}.
safe_execute_handler(Handler, Args) ->
    handle_execution_result(catch Handler(Args)).

%% @private Handle handler execution result
handle_execution_result({'EXIT', Error}) ->
    ?LOG_WARNING("[Connection] Handler THREW error: ~p", [Error]),
    {error, iolist_to_binary(io_lib:format("~p", [Error]))};
handle_execution_result(HandlerResult) ->
    ?LOG_WARNING("[Connection] Handler executed, result=~p", [HandlerResult]),
    HandlerResult.

%%%===================================================================
%%% Connection Pool Seeding
%%%===================================================================

%% @doc Seed the peer connection pool with the main QUIC connection.
%% This allows DHT operations (STORE, FIND_VALUE) to reuse the existing
%% connection instead of opening new ones that trigger rate limiting.
-spec seed_connection_pool(string(), integer(), pid()) -> ok.
seed_connection_pool(Host, Port, Conn) ->
    PoolKey = pool_key(Host, Port),
    case whereis(macula_peer_connection_pool) of
        undefined ->
            ?LOG_DEBUG("[Connection] Pool not running, skipping seed for ~s", [PoolKey]),
            ok;
        _Pid ->
            ?LOG_INFO("[Connection] Seeding connection pool: ~s", [PoolKey]),
            macula_peer_connection_pool:put(PoolKey, Conn),
            ok
    end.

%% @doc Build pool key matching the format used by DHT/peer_connector.
%% Returns a bare "host:port" binary with no scheme prefix.
-spec pool_key(string(), integer()) -> binary().
pool_key(Host, Port) ->
    iolist_to_binary([Host, ":", integer_to_list(Port)]).
