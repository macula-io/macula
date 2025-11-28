%%%-------------------------------------------------------------------
%%% @doc
%%% NAT-aware Peer Connector.
%%%
%%% Provides intelligent connection establishment using the optimal
%%% strategy based on NAT profiles. Integrates:
%%% - NAT coordinator for strategy determination
%%% - Hole punch executor for direct connections
%%% - Relay fallback when direct fails
%%%
%%% Connection Strategy Order:
%%% 1. Direct - If target can receive unsolicited (Full Cone NAT)
%%% 2. Hole Punch - If NAT profiles indicate feasibility
%%% 3. Relay - Fallback via gateway
%%%
%%% Usage:
%%% ```
%%% {ok, Conn} = macula_nat_connector:connect(TargetNodeId, Opts).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_connector).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    connect/2,
    connect/3,
    disconnect/1
]).

-define(DEFAULT_TIMEOUT_MS, 10000).     % 10 seconds total timeout
-define(HOLE_PUNCH_TIMEOUT_MS, 5000).   % 5 seconds for hole punch
-define(DIRECT_TIMEOUT_MS, 3000).       % 3 seconds for direct connect

%%%===================================================================
%%% Types
%%%===================================================================

-type connect_opts() :: #{
    timeout => timeout(),
    skip_hole_punch => boolean(),
    relay_endpoint => binary()
}.

-type connect_result() ::
    {ok, quicer:connection_handle(), direct | hole_punch | relay} |
    {error, term()}.

-export_type([connect_opts/0, connect_result/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Connect to a peer using optimal strategy.
%% Automatically determines best approach based on NAT profiles.
-spec connect(binary(), binary()) -> connect_result().
connect(LocalNodeId, TargetNodeId) ->
    connect(LocalNodeId, TargetNodeId, #{}).

%% @doc Connect with options.
-spec connect(binary(), binary(), connect_opts()) -> connect_result().
connect(LocalNodeId, TargetNodeId, Opts) ->
    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT_MS),

    ?LOG_INFO("Initiating NAT-aware connection: ~s -> ~s", [LocalNodeId, TargetNodeId]),

    %% Get connection strategy from coordinator
    case get_connection_strategy(LocalNodeId, TargetNodeId) of
        {ok, Strategy, SessionId} ->
            execute_strategy(Strategy, LocalNodeId, TargetNodeId, SessionId, Opts, Timeout);
        {error, Reason} ->
            ?LOG_WARNING("Failed to get connection strategy: ~p, trying relay", [Reason]),
            try_relay_connection(TargetNodeId, Opts)
    end.

%% @doc Disconnect from a peer.
-spec disconnect(quicer:connection_handle()) -> ok.
disconnect(Conn) ->
    catch quicer:close_connection(Conn),
    ok.

%%%===================================================================
%%% Internal functions - Strategy Selection
%%%===================================================================

%% @private
%% @doc Get connection strategy from NAT coordinator.
-spec get_connection_strategy(binary(), binary()) ->
    {ok, macula_nat_coordinator:connection_strategy(), binary()} | {error, term()}.
get_connection_strategy(LocalNodeId, TargetNodeId) ->
    case whereis(macula_nat_coordinator) of
        undefined ->
            %% Coordinator not running - default to hole_punch (optimistic)
            {ok, hole_punch, generate_session_id()};
        _Pid ->
            macula_nat_coordinator:request_connection(LocalNodeId, TargetNodeId)
    end.

%% @private
%% @doc Execute the determined connection strategy.
-spec execute_strategy(macula_nat_coordinator:connection_strategy(),
                       binary(), binary(), binary(), connect_opts(), timeout()) ->
    connect_result().
execute_strategy(direct, _LocalNodeId, TargetNodeId, _SessionId, Opts, _Timeout) ->
    ?LOG_DEBUG("Trying direct connection to ~s", [TargetNodeId]),
    try_direct_connection(TargetNodeId, Opts);

execute_strategy(hole_punch, LocalNodeId, TargetNodeId, SessionId, Opts, _Timeout) ->
    ?LOG_DEBUG("Trying hole punch to ~s (session ~s)", [TargetNodeId, SessionId]),
    case try_hole_punch(LocalNodeId, TargetNodeId, SessionId, Opts) of
        {ok, Conn} ->
            report_result(SessionId, LocalNodeId, success),
            {ok, Conn, hole_punch};
        {error, Reason} ->
            report_result(SessionId, LocalNodeId, failure),
            ?LOG_DEBUG("Hole punch failed: ~p, falling back to relay", [Reason]),
            try_relay_connection(TargetNodeId, Opts)
    end;

execute_strategy(relay, _LocalNodeId, TargetNodeId, _SessionId, Opts, _Timeout) ->
    ?LOG_DEBUG("Using relay for ~s (NAT profile requires it)", [TargetNodeId]),
    try_relay_connection(TargetNodeId, Opts).

%%%===================================================================
%%% Internal functions - Connection Attempts
%%%===================================================================

%% @private
%% @doc Try direct QUIC connection to target.
-spec try_direct_connection(binary(), connect_opts()) -> connect_result().
try_direct_connection(TargetNodeId, _Opts) ->
    %% Look up target's advertised endpoint from DHT
    case lookup_peer_endpoint(TargetNodeId) of
        {ok, Host, Port} ->
            case attempt_quic_connect(Host, Port, ?DIRECT_TIMEOUT_MS) of
                {ok, Conn} ->
                    {ok, Conn, direct};
                {error, Reason} ->
                    {error, {direct_failed, Reason}}
            end;
        {error, not_found} ->
            {error, endpoint_not_found}
    end.

%% @private
%% @doc Try hole punch connection.
-spec try_hole_punch(binary(), binary(), binary(), connect_opts()) ->
    {ok, quicer:connection_handle()} | {error, term()}.
try_hole_punch(_LocalNodeId, TargetNodeId, SessionId, Opts) ->
    %% Get target's reflexive address and predicted ports
    case get_hole_punch_target(TargetNodeId) of
        {ok, Host, Ports} ->
            PunchOpts = #{
                target_host => Host,
                target_ports => Ports,
                session_id => SessionId,
                role => initiator
            },

            SkipPunch = maps:get(skip_hole_punch, Opts, false),
            case SkipPunch of
                true ->
                    {error, hole_punch_skipped};
                false ->
                    macula_hole_punch:execute(TargetNodeId, PunchOpts, ?HOLE_PUNCH_TIMEOUT_MS)
            end;
        {error, Reason} ->
            {error, {hole_punch_target_not_found, Reason}}
    end.

%% @private
%% @doc Try relay connection via gateway.
-spec try_relay_connection(binary(), connect_opts()) -> connect_result().
try_relay_connection(TargetNodeId, Opts) ->
    %% Get relay endpoint (use provided or find from DHT)
    RelayEndpoint = case maps:get(relay_endpoint, Opts, undefined) of
        undefined -> find_relay_endpoint();
        Endpoint -> Endpoint
    end,

    case RelayEndpoint of
        {ok, Host, Port} ->
            %% Connect to relay
            case attempt_quic_connect(Host, Port, ?DIRECT_TIMEOUT_MS) of
                {ok, Conn} ->
                    %% Send relay request to establish tunnel
                    case setup_relay_tunnel(Conn, TargetNodeId) of
                        ok ->
                            {ok, Conn, relay};
                        {error, Reason} ->
                            quicer:close_connection(Conn),
                            {error, {relay_setup_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {relay_connect_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {no_relay_available, Reason}}
    end.

%%%===================================================================
%%% Internal functions - Helpers
%%%===================================================================

%% @private
%% @doc Look up peer's advertised endpoint from DHT.
-spec lookup_peer_endpoint(binary()) ->
    {ok, binary() | string(), inet:port_number()} | {error, not_found}.
lookup_peer_endpoint(NodeId) ->
    case whereis(macula_routing_server) of
        undefined ->
            {error, not_found};
        Pid ->
            %% Build DHT key for peer endpoint
            Key = crypto:hash(sha256, <<"peer.endpoint.", NodeId/binary>>),
            case macula_routing_server:find_value(Pid, Key, 20) of
                {ok, #{<<"host">> := Host, <<"port">> := Port}} ->
                    {ok, Host, Port};
                _ ->
                    {error, not_found}
            end
    end.

%% @private
%% @doc Get hole punch target info (reflexive address and predicted ports).
-spec get_hole_punch_target(binary()) ->
    {ok, binary() | string(), [inet:port_number()]} | {error, term()}.
get_hole_punch_target(NodeId) ->
    case whereis(macula_nat_cache) of
        undefined ->
            {error, nat_cache_unavailable};
        _Pid ->
            case macula_nat_cache:get_from_dht(NodeId) of
                {ok, Profile} ->
                    %% Extract reflexive address and predict ports
                    case maps:get(reflexive_address, Profile, undefined) of
                        {IP, Port} ->
                            Ports = predict_target_ports(Profile, Port),
                            Host = format_ip(IP),
                            {ok, Host, Ports};
                        undefined ->
                            {error, no_reflexive_address}
                    end;
                not_found ->
                    {error, nat_profile_not_found};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @private
%% @doc Predict target ports based on NAT profile using port predictor.
-spec predict_target_ports(macula_nat_cache:nat_profile(), inet:port_number()) ->
    [inet:port_number()].
predict_target_ports(Profile, Port) ->
    NodeId = maps:get(node_id, Profile, <<>>),
    AllocationPolicy = maps:get(allocation_policy, Profile, unknown),

    %% Use the port predictor for intelligent prediction
    case whereis(macula_port_predictor) of
        undefined ->
            %% Fallback to simple prediction if predictor not running
            simple_predict_ports(AllocationPolicy, Port, Profile);
        _Pid ->
            Prediction = macula_port_predictor:predict(NodeId, AllocationPolicy,
                                                        #{base_port => Port, count => 5}),
            maps:get(ports, Prediction, [Port])
    end.

%% @private
%% @doc Simple port prediction fallback.
-spec simple_predict_ports(atom(), inet:port_number(), map()) -> [inet:port_number()].
simple_predict_ports(pp, Port, _Profile) ->
    %% Port preservation - use same port
    [Port];
simple_predict_ports(pc, Port, #{port_delta := Delta}) when is_integer(Delta) ->
    %% Port contiguity - try sequential range
    [Port, Port + Delta, Port + (2 * Delta), Port - Delta];
simple_predict_ports(_, Port, _Profile) ->
    %% Random or unknown - try range around known port
    [Port, Port + 1, Port - 1, Port + 2, Port - 2].

%% @private
%% @doc Format IP address as string.
-spec format_ip(inet:ip_address()) -> string().
format_ip({A, B, C, D}) ->
    lists:flatten(io_lib:format("~B.~B.~B.~B", [A, B, C, D]));
format_ip(IP) when is_tuple(IP), tuple_size(IP) == 8 ->
    inet:ntoa(IP).

%% @private
%% @doc Find available relay endpoint.
-spec find_relay_endpoint() ->
    {ok, binary() | string(), inet:port_number()} | {error, term()}.
find_relay_endpoint() ->
    case whereis(macula_routing_server) of
        undefined ->
            {error, routing_unavailable};
        Pid ->
            %% Look for relay service in DHT
            Key = crypto:hash(sha256, <<"service.relay">>),
            case macula_routing_server:find_value(Pid, Key, 20) of
                {ok, #{<<"host">> := Host, <<"port">> := Port}} ->
                    {ok, Host, Port};
                _ ->
                    %% Fallback to bootstrap gateway
                    {error, no_relay_found}
            end
    end.

%% @private
%% @doc Attempt QUIC connection.
-spec attempt_quic_connect(binary() | string(), inet:port_number(), timeout()) ->
    {ok, quicer:connection_handle()} | {error, term()}.
attempt_quic_connect(Host, Port, Timeout) ->
    HostStr = case is_binary(Host) of
        true -> binary_to_list(Host);
        false -> Host
    end,

    ConnOpts = [
        {alpn, ["macula"]},
        {verify, none},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000}
    ],

    quicer:connect(HostStr, Port, ConnOpts, Timeout).

%% @private
%% @doc Setup relay tunnel to target.
-spec setup_relay_tunnel(quicer:connection_handle(), binary()) -> ok | {error, term()}.
setup_relay_tunnel(Conn, TargetNodeId) ->
    %% Open stream and send RELAY_REQUEST
    case quicer:start_stream(Conn, []) of
        {ok, Stream} ->
            RelayRequest = #{
                type => relay_request,
                target_node_id => TargetNodeId
            },
            Encoded = msgpack:pack(RelayRequest),
            case quicer:send(Stream, Encoded) of
                {ok, _} -> ok;
                Error -> Error
            end;
        Error ->
            Error
    end.

%% @private
%% @doc Report hole punch result to coordinator.
-spec report_result(binary(), binary(), success | failure) -> ok.
report_result(SessionId, ReporterId, Result) ->
    case whereis(macula_nat_coordinator) of
        undefined -> ok;
        _Pid -> macula_nat_coordinator:report_result(SessionId, ReporterId, Result)
    end.

%% @private
%% @doc Generate unique session ID.
-spec generate_session_id() -> binary().
generate_session_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).
