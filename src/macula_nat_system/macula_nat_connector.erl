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
%%% Usage: connect(TargetNodeId, Opts) to establish connection.
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
            case normalize_quic_result(attempt_quic_connect(Host, Port, ?DIRECT_TIMEOUT_MS)) of
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
    case get_hole_punch_target(TargetNodeId) of
        {ok, Host, Ports} ->
            PunchOpts = build_hole_punch_opts(Host, Ports, SessionId),
            maybe_execute_hole_punch(TargetNodeId, PunchOpts, Opts);
        {error, Reason} ->
            {error, {hole_punch_target_not_found, Reason}}
    end.

build_hole_punch_opts(Host, Ports, SessionId) ->
    #{
        target_host => Host,
        target_ports => Ports,
        session_id => SessionId,
        role => initiator
    }.

maybe_execute_hole_punch(_TargetNodeId, _PunchOpts, #{skip_hole_punch := true}) ->
    {error, hole_punch_skipped};
maybe_execute_hole_punch(TargetNodeId, PunchOpts, _Opts) ->
    macula_hole_punch:execute(TargetNodeId, PunchOpts, ?HOLE_PUNCH_TIMEOUT_MS).

%% @private
%% @doc Try relay connection via gateway.
-spec try_relay_connection(binary(), connect_opts()) -> connect_result().
try_relay_connection(TargetNodeId, Opts) ->
    RelayEndpoint = get_relay_endpoint(Opts),
    do_relay_connection(RelayEndpoint, TargetNodeId).

get_relay_endpoint(#{relay_endpoint := Endpoint}) ->
    Endpoint;
get_relay_endpoint(_Opts) ->
    find_relay_endpoint().

do_relay_connection({ok, Host, Port}, TargetNodeId) ->
    connect_and_setup_relay(normalize_quic_result(attempt_quic_connect(Host, Port, ?DIRECT_TIMEOUT_MS)), TargetNodeId);
do_relay_connection({error, Reason}, _TargetNodeId) ->
    {error, {no_relay_available, Reason}}.

connect_and_setup_relay({ok, Conn}, TargetNodeId) ->
    setup_relay_or_close(setup_relay_tunnel(Conn, TargetNodeId), Conn);
connect_and_setup_relay({error, Reason}, _TargetNodeId) ->
    {error, {relay_connect_failed, Reason}}.

setup_relay_or_close(ok, Conn) ->
    {ok, Conn, relay};
setup_relay_or_close({error, Reason}, Conn) ->
    quicer:close_connection(Conn),
    {error, {relay_setup_failed, Reason}}.

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
%% Uses relay registry which checks local ETS first, then DHT.
%% Falls back to bootstrap peers if no relay found.
-spec find_relay_endpoint() ->
    {ok, binary() | string(), inet:port_number()} | {error, term()}.
find_relay_endpoint() ->
    case find_relay_from_registry() of
        {ok, Host, Port} ->
            ?LOG_DEBUG("[NAT_CONNECTOR] Found relay from registry: ~s:~p", [Host, Port]),
            {ok, Host, Port};
        {error, _} ->
            %% Fallback: use bootstrap peer as relay
            find_relay_from_bootstrap()
    end.

%% @private
%% @doc Find relay from local registry.
-spec find_relay_from_registry() ->
    {ok, binary() | string(), inet:port_number()} | {error, term()}.
find_relay_from_registry() ->
    case whereis(macula_relay_registry) of
        undefined ->
            {error, relay_registry_unavailable};
        _Pid ->
            case macula_relay_registry:find_relay(<<>>) of
                {ok, #{endpoint := {Host, Port}}} ->
                    {ok, Host, Port};
                {error, no_relays_available} ->
                    {error, no_relay_found}
            end
    end.

%% @private
%% @doc Find relay from bootstrap peers (fallback).
%% Bootstrap nodes typically run relay services.
-spec find_relay_from_bootstrap() ->
    {ok, binary() | string(), inet:port_number()} | {error, term()}.
find_relay_from_bootstrap() ->
    case os:getenv("MACULA_BOOTSTRAP_PEERS") of
        false ->
            ?LOG_WARNING("[NAT_CONNECTOR] No bootstrap peers configured for relay fallback"),
            {error, no_bootstrap_peers};
        BootstrapPeers ->
            parse_bootstrap_for_relay(BootstrapPeers)
    end.

%% @private
%% @doc Parse bootstrap peers string and extract first as relay.
%% Format: "https://host:port" or "host:port"
-spec parse_bootstrap_for_relay(string()) ->
    {ok, binary() | string(), inet:port_number()} | {error, term()}.
parse_bootstrap_for_relay(BootstrapPeers) ->
    %% Take first bootstrap peer
    FirstPeer = hd(string:tokens(BootstrapPeers, ",")),
    Stripped = string:trim(FirstPeer),
    %% Remove https:// prefix if present
    WithoutScheme = case string:prefix(Stripped, "https://") of
        nomatch -> Stripped;
        Rest -> Rest
    end,
    %% Parse host:port
    case string:split(WithoutScheme, ":") of
        [Host, PortStr] ->
            parse_bootstrap_port(Host, PortStr);
        _ ->
            ?LOG_WARNING("[NAT_CONNECTOR] Invalid bootstrap format: ~s", [Stripped]),
            {error, invalid_bootstrap_format}
    end.

%% @private Parse bootstrap port string
parse_bootstrap_port(Host, PortStr) ->
    handle_port_parse(catch list_to_integer(string:trim(PortStr)), Host, PortStr).

%% @private Handle port parsing result
handle_port_parse({'EXIT', _}, _Host, PortStr) ->
    ?LOG_WARNING("[NAT_CONNECTOR] Invalid bootstrap port: ~s", [PortStr]),
    {error, invalid_bootstrap_port};
handle_port_parse(Port, Host, _PortStr) when is_integer(Port) ->
    ?LOG_WARNING("[NAT_CONNECTOR] Using bootstrap as relay: ~s:~p", [Host, Port]),
    {ok, list_to_binary(Host), Port};
handle_port_parse(_, _Host, PortStr) ->
    ?LOG_WARNING("[NAT_CONNECTOR] Invalid bootstrap port: ~s", [PortStr]),
    {error, invalid_bootstrap_port}.

%% @private
%% @doc Attempt QUIC connection.
-spec attempt_quic_connect(binary() | string(), inet:port_number(), timeout()) ->
    {ok, quicer:connection_handle()} | {error, term()}.
attempt_quic_connect(Host, Port, Timeout) when is_binary(Host) ->
    attempt_quic_connect(binary_to_list(Host), Port, Timeout);
attempt_quic_connect(Host, Port, Timeout) when is_list(Host) ->
    ConnOpts = quic_connection_opts(),
    quicer:connect(Host, Port, ConnOpts, Timeout).

quic_connection_opts() ->
    [
        {alpn, ["macula"]},
        {verify, none},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000}
    ].

%% @private
%% @doc Normalize QUIC connection results to standard {ok, _} | {error, _} format.
%% quicer:connect/4 can return various error formats including 3-tuples like
%% {error, transport_down, Details} which need to be normalized to 2-tuples.
-spec normalize_quic_result(term()) -> {ok, term()} | {error, term()}.
normalize_quic_result({ok, Conn}) ->
    {ok, Conn};
normalize_quic_result({error, Type, Details}) when is_atom(Type), is_map(Details) ->
    %% e.g., {error, transport_down, #{error => 2, status => connection_refused}}
    {error, {Type, Details}};
normalize_quic_result({error, Reason}) ->
    {error, Reason};
normalize_quic_result(Other) ->
    {error, {unexpected_quic_result, Other}}.

%% @private
%% @doc Setup relay tunnel to target.
-spec setup_relay_tunnel(quicer:connection_handle(), binary()) -> ok | {error, term()}.
setup_relay_tunnel(Conn, TargetNodeId) ->
    send_relay_request_on_stream(quicer:start_stream(Conn, []), TargetNodeId).

send_relay_request_on_stream({ok, Stream}, TargetNodeId) ->
    RelayRequest = #{type => relay_request, target_node_id => TargetNodeId},
    send_relay_request(quicer:send(Stream, msgpack:pack(RelayRequest)));
send_relay_request_on_stream(Error, _TargetNodeId) ->
    Error.

send_relay_request({ok, _}) -> ok;
send_relay_request(Error) -> Error.

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
