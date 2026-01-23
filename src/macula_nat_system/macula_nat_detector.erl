%%%-------------------------------------------------------------------
%%% @doc
%%% NAT Type Detector using NATCracker Methodology.
%%%
%%% Detects the local peer's NAT characteristics by probing external
%%% observers (gateways/peers with public IPs) and analyzing reflexive
%%% addresses. Classification follows NATCracker's 27 NAT type model.
%%%
%%% NAT Policy Detection:
%%% 1. Mapping Policy (m): How NAT maps internal to external addresses
%%%    - EI (Endpoint-Independent): Same external addr for all destinations
%%%    - HD (Host-Dependent): Different external addr per destination host
%%%    - PD (Port-Dependent): Different external addr per destination host:port
%%%
%%% 2. Filtering Policy (f): What incoming packets NAT accepts
%%%    - EI: Accepts from any source
%%%    - HD: Accepts from hosts we've contacted
%%%    - PD: Accepts from host:port we've contacted
%%%
%%% 3. Allocation Policy (a): How NAT chooses external ports
%%%    - PP (Port-Preservation): external_port = local_port
%%%    - PC (Port-Contiguity): external_port = last_port + delta
%%%    - RD (Random): No predictable pattern
%%%
%%% Detection Algorithm (Fast - 200-400ms):
%%% 1. Send NAT_PROBE to primary observer (100ms RTT)
%%% 2. Send NAT_PROBE to secondary observer (parallel, 100ms RTT)
%%% 3. Compare reflexive addresses to classify NAT type
%%%
%%% Most Common NAT Types (per NATCracker):
%%% - (EI, PD, PP): 37% of consumer NATs
%%% - (EI, EI, PP): 15% (Full Cone)
%%% - (PD, PD, RD): 12% (Symmetric)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_detector).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    detect/0,
    detect/1,
    get_local_profile/0,
    add_observation/2,
    handle_probe_reply/1,
    refresh/0,
    %% Local address detection (for testing and external use)
    get_local_address/0,
    get_local_quic_port/0,
    is_relay_capable/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_DETECTION_TIMEOUT_MS, 2000).
-define(REFRESH_INTERVAL_MS, 300000).  % 5 minutes

%%%===================================================================
%%% Types
%%%===================================================================

-type observation() :: #{
    observer_id := binary(),
    reflexive_address := {inet:ip_address(), inet:port_number()},
    local_address := {inet:ip_address(), inet:port_number()},
    observed_at := integer()
}.

-record(state, {
    local_profile :: macula_nat_cache:nat_profile() | undefined,
    observations :: [observation()],
    detection_timeout_ms :: pos_integer(),
    last_detection :: integer() | undefined,
    pending_probes :: #{binary() => pending_probe()}  % request_id => probe info
}).

-type pending_probe() :: #{
    observer_endpoint := binary(),
    sent_at := integer(),
    local_port := inet:port_number()
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the NAT detector server.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Detect local NAT type (async, returns immediately).
%% Results are cached and available via get_local_profile/0.
-spec detect() -> ok.
detect() ->
    gen_server:cast(?SERVER, detect).

%% @doc Detect NAT type using specific observer endpoint.
-spec detect(binary()) -> ok.
detect(ObserverEndpoint) when is_binary(ObserverEndpoint) ->
    gen_server:cast(?SERVER, {detect, ObserverEndpoint}).

%% @doc Get the cached local NAT profile.
-spec get_local_profile() -> {ok, macula_nat_cache:nat_profile()} | not_detected.
get_local_profile() ->
    gen_server:call(?SERVER, get_local_profile).

%% @doc Add an observation from an external observer.
%% Called when we receive a NAT_PROBE_REPLY with our reflexive address.
-spec add_observation(binary(), {inet:ip_address(), inet:port_number()}) -> ok.
add_observation(ObserverId, ReflexiveAddress) ->
    gen_server:cast(?SERVER, {add_observation, ObserverId, ReflexiveAddress}).

%% @doc Handle NAT_PROBE_REPLY message from an observer.
%% Called when we receive a reply containing our reflexive address.
-spec handle_probe_reply(map()) -> ok.
handle_probe_reply(ReplyMsg) when is_map(ReplyMsg) ->
    gen_server:cast(?SERVER, {probe_reply, ReplyMsg}).

%% @doc Trigger NAT type refresh (re-detection).
-spec refresh() -> ok.
refresh() ->
    gen_server:cast(?SERVER, refresh).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Timeout = maps:get(detection_timeout_ms, Opts, ?DEFAULT_DETECTION_TIMEOUT_MS),

    ?LOG_INFO("NAT detector started (timeout=~p ms)", [Timeout]),

    %% Schedule periodic refresh
    schedule_refresh(),

    {ok, #state{
        local_profile = undefined,
        observations = [],
        detection_timeout_ms = Timeout,
        last_detection = undefined,
        pending_probes = #{}
    }}.

handle_call(get_local_profile, _From, #state{local_profile = undefined} = State) ->
    {reply, not_detected, State};
handle_call(get_local_profile, _From, #state{local_profile = Profile} = State) ->
    {reply, {ok, Profile}, State};

handle_call(get_relay_capability, _From, State) ->
    %% Check if we can act as a relay
    Capable = check_relay_capability(State),
    {reply, {ok, Capable}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
%% @doc Check if this node can act as a relay.
%% True if: has public IP OR NAT profile says can_receive_unsolicited.
check_relay_capability(#state{local_profile = undefined} = State) ->
    %% No profile yet - check if local IP is public
    has_public_ip(State);
check_relay_capability(#state{local_profile = Profile} = State) ->
    %% Check profile first, then fall back to public IP check
    case maps:get(can_receive_unsolicited, Profile, false) of
        true -> true;
        false -> has_public_ip(State)
    end.

handle_cast(detect, State) ->
    %% Trigger detection using any available observers
    NewState = trigger_detection(State),
    {noreply, NewState};

handle_cast({detect, ObserverEndpoint}, State) ->
    %% Trigger detection using specific observer
    NewState = trigger_detection_with_observer(ObserverEndpoint, State),
    {noreply, NewState};

handle_cast({add_observation, ObserverId, ReflexiveAddress}, State) ->
    %% Add observation and potentially update NAT classification
    NewState = process_observation(ObserverId, ReflexiveAddress, State),
    {noreply, NewState};

handle_cast({probe_reply, ReplyMsg}, State) ->
    %% Handle NAT_PROBE_REPLY with request_id correlation
    NewState = process_probe_reply(ReplyMsg, State),
    {noreply, NewState};

handle_cast(refresh, State) ->
    ?LOG_DEBUG("NAT type refresh triggered"),
    NewState = State#state{observations = []},
    trigger_detection(NewState),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    ?LOG_DEBUG("Periodic NAT refresh"),
    schedule_refresh(),
    NewState = maybe_refresh_detection(State),
    {noreply, NewState};

handle_info({probe_timeout, RequestId}, #state{pending_probes = Pending} = State) ->
    %% Clean up timed-out probe
    NewPending = maps:remove(RequestId, Pending),
    case maps:is_key(RequestId, Pending) of
        true ->
            ?LOG_DEBUG("NAT probe timed out (request_id=~p)", [base64:encode(RequestId)]);
        false ->
            ok  % Already processed
    end,
    {noreply, State#state{pending_probes = NewPending}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Trigger NAT detection by sending probes to known observers.
-spec trigger_detection(#state{}) -> #state{}.
trigger_detection(State) ->
    %% Get observers from application configuration or DHT
    Observers = get_known_observers(),
    send_probes_to_observers(Observers, State).

%% @private
%% @doc Trigger detection using a specific observer.
-spec trigger_detection_with_observer(binary(), #state{}) -> #state{}.
trigger_detection_with_observer(ObserverEndpoint, State) ->
    send_probes_to_observers([ObserverEndpoint], State).

%% @private
%% @doc Get known observer endpoints for NAT probing.
%% Observers are nodes with public IPs (gateways/bootstrap nodes).
-spec get_known_observers() -> [binary()].
get_known_observers() ->
    %% Try multiple sources for observers
    case application:get_env(macula, nat_observers) of
        {ok, Observers} when is_list(Observers) ->
            Observers;
        _ ->
            %% Fall back to gateway endpoint from connection
            get_connected_gateway_endpoints()
    end.

%% @private
%% @doc Get gateway endpoints from active connections.
-spec get_connected_gateway_endpoints() -> [binary()].
get_connected_gateway_endpoints() ->
    %% Try to get from macula_peer_connection_pool
    case whereis(macula_peer_connection_pool) of
        undefined -> [];
        _Pid ->
            %% Get known peer endpoints from pool (they're likely gateways)
            case catch macula_peer_connection_pool:get_connected_peers() of
                Peers when is_list(Peers) -> Peers;
                _ -> []
            end
    end.

%% @private
%% @doc Send NAT_PROBE messages to multiple observers.
-spec send_probes_to_observers([binary()], #state{}) -> #state{}.
send_probes_to_observers([], State) ->
    ?LOG_DEBUG("No observers available for NAT detection"),
    State;
send_probes_to_observers(Observers, State) ->
    ?LOG_DEBUG("Sending NAT probes to ~p observer(s)", [length(Observers)]),
    lists:foldl(fun send_nat_probe/2, State, Observers).

%% @private
%% @doc Send NAT_PROBE to a single observer and wait for reply (v0.12.0+).
%% Uses synchronous request-response pattern to ensure reply is received.
-spec send_nat_probe(binary(), #state{}) -> #state{}.
send_nat_probe(ObserverEndpoint, #state{detection_timeout_ms = Timeout} = State) ->
    RequestId = crypto:strong_rand_bytes(16),
    LocalPort = get_local_quic_port(),
    NodeId = get_local_node_id(),

    ProbeMsg = #{
        node_id => NodeId,
        local_port => LocalPort,
        request_id => RequestId
    },

    ?LOG_DEBUG("Sending NAT probe to ~s (request_id=~p)",
               [ObserverEndpoint, base64:encode(RequestId)]),

    %% Send NAT_PROBE and wait for NAT_PROBE_REPLY (v0.12.0+)
    %% This blocks until reply received or timeout
    case macula_peer_connector:send_message_and_wait(ObserverEndpoint, nat_probe, ProbeMsg, Timeout) of
        {ok, {nat_probe_reply, ReplyMsg}} ->
            ?LOG_DEBUG("Received NAT_PROBE_REPLY from ~s", [ObserverEndpoint]),
            %% Process the reply directly (bypass pending_probes lookup since we're synchronous)
            process_probe_reply_direct(ReplyMsg, ObserverEndpoint, LocalPort, State);
        {ok, {OtherType, _Msg}} ->
            ?LOG_WARNING("Received unexpected reply type ~p from ~s", [OtherType, ObserverEndpoint]),
            State;
        {error, timeout} ->
            ?LOG_WARNING("NAT probe to ~s timed out after ~pms", [ObserverEndpoint, Timeout]),
            State;
        {error, Reason} ->
            ?LOG_WARNING("Failed to send NAT probe to ~s: ~p", [ObserverEndpoint, Reason]),
            State
    end.

%% @private
%% @doc Process NAT_PROBE_REPLY directly for synchronous request-response flow.
%% Bypasses pending_probes correlation since reply was received synchronously.
-spec process_probe_reply_direct(map(), binary(), inet:port_number(), #state{}) -> #state{}.
process_probe_reply_direct(ReplyMsg, ObserverEndpoint, LocalPort, State) ->
    %% Extract reflexive address from reply (supports both atom and binary keys)
    ReflexiveIP = get_msg_field(ReplyMsg, reflexive_ip, <<"reflexive_ip">>),
    ReflexivePort = get_msg_field(ReplyMsg, reflexive_port, <<"reflexive_port">>),
    ObserverId = get_msg_field(ReplyMsg, node_id, <<"node_id">>),

    %% Check for missing fields
    case {ReflexiveIP, ReflexivePort} of
        {undefined, _} ->
            ?LOG_DEBUG("NAT probe reply missing reflexive_ip"),
            State;
        {_, undefined} ->
            ?LOG_DEBUG("NAT probe reply missing reflexive_port"),
            State;
        _ ->
            %% Parse IP address - returns {IP, Port} tuple directly
            ReflexiveAddr = parse_reflexive_address(ReflexiveIP, ReflexivePort),
            %% Check if parsing succeeded (invalid IP returns {0,0,0,0})
            case ReflexiveAddr of
                {{0, 0, 0, 0}, _} ->
                    ?LOG_DEBUG("Failed to parse reflexive IP address: ~p", [ReflexiveIP]),
                    State;
                _ ->
                    record_observation_direct(ReflexiveAddr, ObserverEndpoint, ObserverId, LocalPort, State)
            end
    end.

%% @private
%% @doc Record NAT observation for synchronous flow.
-spec record_observation_direct(
    {inet:ip_address(), inet:port_number()} | error,
    binary(), binary() | undefined, inet:port_number(), #state{}) -> #state{}.
record_observation_direct(error, _, _, _, State) ->
    ?LOG_DEBUG("Failed to parse reflexive address"),
    State;
record_observation_direct(ReflexiveAddr, ObserverEndpoint, ObserverId, LocalPort, State) ->
    Observation = #{
        observer_id => ensure_observer_id(ObserverId, ObserverEndpoint),
        reflexive_address => ReflexiveAddr,
        local_address => {{0, 0, 0, 0}, LocalPort},
        observed_at => erlang:system_time(second)
    },

    ?LOG_DEBUG("Recording NAT observation: reflexive=~p local_port=~p", [ReflexiveAddr, LocalPort]),

    %% Add to observations list
    NewObservations = [Observation | State#state.observations],
    NewState = State#state{observations = NewObservations},

    %% Update NAT profile classification
    maybe_classify_nat(NewState).

%% @private
%% @doc Get the local QUIC port we're using for connections.
%% Checks environment variable first, then application config.
-spec get_local_quic_port() -> inet:port_number().
get_local_quic_port() ->
    case os:getenv("MACULA_QUIC_PORT") of
        false ->
            case os:getenv("GATEWAY_PORT") of
                false ->
                    application:get_env(macula, quic_port,
                        application:get_env(macula, gateway_port, 9443));
                PortStr ->
                    list_to_integer(PortStr)
            end;
        PortStr ->
            list_to_integer(PortStr)
    end.

%% @private
%% @doc Process a NAT_PROBE_REPLY and update observations.
-spec process_probe_reply(map(), #state{}) -> #state{}.
process_probe_reply(ReplyMsg, #state{pending_probes = Pending} = State) ->
    %% Extract request_id (supports both atom and binary keys from msgpack)
    RequestId = get_msg_field(ReplyMsg, request_id, <<"request_id">>),
    process_probe_reply_with_id(RequestId, ReplyMsg, Pending, State).

%% @private Request ID not found in reply - ignore
process_probe_reply_with_id(undefined, _ReplyMsg, _Pending, State) ->
    ?LOG_DEBUG("NAT probe reply missing request_id"),
    State;

%% @private Request ID found - correlate with pending probe
process_probe_reply_with_id(RequestId, ReplyMsg, Pending, State) ->
    case maps:find(RequestId, Pending) of
        {ok, PendingProbe} ->
            %% Extract reflexive address from reply
            ReflexiveIP = get_msg_field(ReplyMsg, reflexive_ip, <<"reflexive_ip">>),
            ReflexivePort = get_msg_field(ReplyMsg, reflexive_port, <<"reflexive_port">>),
            ObserverId = get_msg_field(ReplyMsg, node_id, <<"node_id">>),

            process_valid_probe_reply(ReflexiveIP, ReflexivePort, ObserverId, PendingProbe, RequestId, Pending, State);

        error ->
            ?LOG_DEBUG("Received NAT probe reply for unknown request_id=~p",
                       [base64:encode(RequestId)]),
            State
    end.

%% @private Missing reflexive address fields
process_valid_probe_reply(undefined, _, _, _, _, _, State) ->
    ?LOG_DEBUG("NAT probe reply missing reflexive_ip"),
    State;
process_valid_probe_reply(_, undefined, _, _, _, _, State) ->
    ?LOG_DEBUG("NAT probe reply missing reflexive_port"),
    State;

%% @private Valid reply - process observation
process_valid_probe_reply(ReflexiveIP, ReflexivePort, ObserverId, PendingProbe, RequestId, Pending, State) ->
    #{observer_endpoint := ObserverEndpoint, local_port := LocalPort} = PendingProbe,

    %% Parse IP address
    ReflexiveAddr = parse_reflexive_address(ReflexiveIP, ReflexivePort),

    ?LOG_DEBUG("NAT probe reply from ~s: reflexive=~p, local_port=~p",
               [ObserverEndpoint, ReflexiveAddr, LocalPort]),

    %% Add observation with local address info
    Observation = #{
        observer_id => ensure_observer_id(ObserverId, ObserverEndpoint),
        reflexive_address => ReflexiveAddr,
        local_address => {{0, 0, 0, 0}, LocalPort},
        observed_at => erlang:system_time(second)
    },

    %% Update state: remove pending probe, add observation
    NewObservations = [Observation | State#state.observations],
    NewPending = maps:remove(RequestId, Pending),

    %% Try to classify NAT type with new observation
    maybe_classify_nat(State#state{
        observations = NewObservations,
        pending_probes = NewPending
    }).

%% @private Parse reflexive IP from binary or tuple format.
-spec parse_reflexive_address(binary() | tuple(), inet:port_number()) ->
    {inet:ip_address(), inet:port_number()}.
parse_reflexive_address(IP, Port) when is_tuple(IP) ->
    {IP, Port};
parse_reflexive_address(IP, Port) when is_binary(IP) ->
    case inet:parse_address(binary_to_list(IP)) of
        {ok, ParsedIP} -> {ParsedIP, Port};
        {error, _} -> {{0, 0, 0, 0}, Port}
    end.

%% @private Helper to get field with atom or binary key.
-spec get_msg_field(map(), atom(), binary()) -> term() | undefined.
get_msg_field(Map, AtomKey, BinaryKey) ->
    case maps:find(AtomKey, Map) of
        {ok, Value} -> Value;
        error ->
            case maps:find(BinaryKey, Map) of
                {ok, Value} -> Value;
                error -> undefined
            end
    end.

%% @private Ensure observer ID is valid binary.
-spec ensure_observer_id(term(), binary()) -> binary().
ensure_observer_id(undefined, Endpoint) -> Endpoint;
ensure_observer_id(Id, _Endpoint) when is_binary(Id) -> Id;
ensure_observer_id(_, Endpoint) -> Endpoint.

%% @private
%% @doc Process an observation and update NAT classification if enough data.
-spec process_observation(binary(), {inet:ip_address(), inet:port_number()}, #state{}) -> #state{}.
process_observation(ObserverId, ReflexiveAddress, State) ->
    #state{observations = Observations} = State,

    %% Get local address (for port delta calculation)
    LocalAddress = get_local_address(),

    Observation = #{
        observer_id => ObserverId,
        reflexive_address => ReflexiveAddress,
        local_address => LocalAddress,
        observed_at => erlang:system_time(second)
    },

    NewObservations = [Observation | Observations],

    ?LOG_DEBUG("Added NAT observation from ~s: ~p", [ObserverId, ReflexiveAddress]),

    %% Try to classify NAT type with available observations
    NewState = State#state{observations = NewObservations},
    maybe_classify_nat(NewState).

%% @private
%% @doc Attempt to classify NAT type if we have enough observations.
-spec maybe_classify_nat(#state{}) -> #state{}.
maybe_classify_nat(#state{observations = Observations} = State) when length(Observations) < 1 ->
    %% Need at least 1 observation for basic classification
    State;
maybe_classify_nat(#state{observations = Observations} = State) ->
    %% Classify based on available observations
    Profile = classify_nat_type(Observations),

    %% Cache the profile
    NodeId = get_local_node_id(),
    macula_nat_cache:put(NodeId, Profile),

    ?LOG_INFO("NAT type detected: mapping=~p, filtering=~p, allocation=~p",
              [maps:get(mapping_policy, Profile),
               maps:get(filtering_policy, Profile),
               maps:get(allocation_policy, Profile)]),

    State#state{
        local_profile = Profile,
        last_detection = erlang:system_time(second)
    }.

%% @private
%% @doc Classify NAT type based on observations.
%% With 1 observation: Basic classification (conservative)
%% With 2+ observations: Full classification (accurate)
-spec classify_nat_type([observation()]) -> macula_nat_cache:nat_profile().
classify_nat_type([]) ->
    %% No observations - assume worst case (symmetric NAT)
    create_profile(pd, pd, rd);
classify_nat_type([Obs]) ->
    %% Single observation - can determine port preservation
    #{reflexive_address := {_IP, ExtPort}, local_address := {_, LocalPort}} = Obs,
    AllocationPolicy = port_preservation_policy(ExtPort, LocalPort),
    %% With single observation, assume conservative (port-dependent filtering)
    %% as we can't determine mapping/filtering policy without multiple observers
    create_profile(ei, pd, AllocationPolicy);

classify_nat_type(Observations) when length(Observations) >= 2 ->
    %% Multiple observations - can classify mapping and allocation policies
    [Obs1 | Rest] = Observations,
    Obs2 = hd(Rest),

    #{reflexive_address := {IP1, Port1}, local_address := {_, LocalPort1}} = Obs1,
    #{reflexive_address := {IP2, Port2}, local_address := {_, LocalPort2}} = Obs2,

    %% Determine mapping policy based on reflexive addresses
    MappingPolicy = classify_mapping_policy(IP1, Port1, IP2, Port2),

    %% Determine allocation policy based on port patterns
    AllocationPolicy = classify_allocation_policy(LocalPort1, Port1, LocalPort2, Port2),

    %% Filtering policy is harder to determine without active probing
    %% Default to port-dependent (most common conservative case)
    FilteringPolicy = infer_filtering_policy(MappingPolicy),

    create_profile(MappingPolicy, FilteringPolicy, AllocationPolicy).

%% @private
%% @doc Classify mapping policy based on observed reflexive addresses.
-spec classify_mapping_policy(inet:ip_address(), inet:port_number(),
                               inet:ip_address(), inet:port_number()) ->
    macula_nat_cache:mapping_policy().
classify_mapping_policy(IP, Port, IP, Port) ->
    ei;    % Same IP and port - Endpoint Independent
classify_mapping_policy(IP, _Port1, IP, _Port2) ->
    hd;    % Same IP, different port - Host Dependent
classify_mapping_policy(_IP1, _Port1, _IP2, _Port2) ->
    pd.    % Different IP - Port Dependent

%% @private
%% @doc Classify allocation policy based on port patterns.
-spec classify_allocation_policy(inet:port_number(), inet:port_number(),
                                  inet:port_number(), inet:port_number()) ->
    macula_nat_cache:allocation_policy().
classify_allocation_policy(Port, Port, Port, Port) ->
    pp;    % Both preserved - Port Preservation
classify_allocation_policy(_LocalPort1, ExtPort1, _LocalPort2, ExtPort2) ->
    classify_port_contiguity(abs(ExtPort2 - ExtPort1)).

%% @private
%% @doc Check for port contiguity based on delta between external ports.
-spec classify_port_contiguity(non_neg_integer()) -> macula_nat_cache:allocation_policy().
classify_port_contiguity(Delta) when Delta < 10 ->
    pc;    % Sequential ports - Port Contiguity
classify_port_contiguity(_Delta) ->
    rd.    % No pattern - Random

%% @private
%% @doc Infer filtering policy from mapping policy.
%% Conservative: assume filtering is at least as restrictive as mapping.
-spec infer_filtering_policy(macula_nat_cache:mapping_policy()) ->
    macula_nat_cache:filtering_policy().
infer_filtering_policy(ei) -> pd;   % EI mapping often has PD filtering
infer_filtering_policy(hd) -> pd;   % HD mapping typically has PD filtering
infer_filtering_policy(pd) -> pd.   % PD mapping has PD filtering

%% @private
%% @doc Determine port preservation policy from external and local ports.
-spec port_preservation_policy(inet:port_number(), inet:port_number()) ->
    macula_nat_cache:allocation_policy().
port_preservation_policy(Port, Port) ->
    pp;    % Port Preservation
port_preservation_policy(_ExtPort, _LocalPort) ->
    rd.    % Can't determine without more data, assume random

%% @private
%% @doc Create a NAT profile with given policies.
%% Includes optional geo-location fields from environment variables:
%%   - MACULA_LATITUDE: Float, -90.0 to 90.0
%%   - MACULA_LONGITUDE: Float, -180.0 to 180.0
%%   - MACULA_LOCATION: String, human-readable label e.g. "Amsterdam, NL"
-spec create_profile(macula_nat_cache:mapping_policy(),
                     macula_nat_cache:filtering_policy(),
                     macula_nat_cache:allocation_policy()) ->
    macula_nat_cache:nat_profile().
create_profile(MappingPolicy, FilteringPolicy, AllocationPolicy) ->
    BaseProfile = #{
        node_id => get_local_node_id(),
        mapping_policy => MappingPolicy,
        filtering_policy => FilteringPolicy,
        allocation_policy => AllocationPolicy,
        can_receive_unsolicited => (MappingPolicy =:= ei andalso FilteringPolicy =:= ei),
        requires_relay => (MappingPolicy =:= pd andalso AllocationPolicy =:= rd),
        relay_capable => is_relay_capable(),
        detected_at => erlang:system_time(second),
        ttl_seconds => 300
    },
    %% Add geo-location fields from environment variables if present
    add_geo_config(BaseProfile).

%% @private
%% @doc Read geo-location configuration from environment variables.
%% Returns profile with optional latitude, longitude, and location_label fields.
-spec add_geo_config(macula_nat_cache:nat_profile()) -> macula_nat_cache:nat_profile().
add_geo_config(Profile) ->
    GeoConfig = get_geo_config(),
    maps:merge(Profile, GeoConfig).

%% @private
%% @doc Get geo configuration from environment variables.
%% Filters out undefined values.
-spec get_geo_config() -> map().
get_geo_config() ->
    maps:filter(fun(_K, V) -> V =/= undefined end, #{
        latitude => parse_float_env("MACULA_LATITUDE"),
        longitude => parse_float_env("MACULA_LONGITUDE"),
        location_label => get_binary_env("MACULA_LOCATION")
    }).

%% @private
%% @doc Parse a float from environment variable.
%% Returns undefined if not set or invalid.
-spec parse_float_env(string()) -> float() | undefined.
parse_float_env(EnvVar) ->
    case os:getenv(EnvVar) of
        false -> undefined;
        "" -> undefined;
        Value ->
            parse_float_safe(Value)
    end.

parse_float_safe(Value) ->
    handle_float_parse(catch string:to_float(Value), Value).

%% @private Handle float parsing result
handle_float_parse({'EXIT', _}, _Value) ->
    undefined;
handle_float_parse({Float, _Rest}, _Value) when is_float(Float) ->
    Float;
handle_float_parse({error, no_float}, Value) ->
    try_parse_as_integer(Value);
handle_float_parse(_, _Value) ->
    undefined.

%% @private Try parsing as integer and convert to float
try_parse_as_integer(Value) ->
    handle_int_parse(catch string:to_integer(Value)).

%% @private Handle integer parsing result
handle_int_parse({'EXIT', _}) ->
    undefined;
handle_int_parse({Int, _}) when is_integer(Int) ->
    float(Int);
handle_int_parse(_) ->
    undefined.

%% @private
%% @doc Get a binary value from environment variable.
%% Returns undefined if not set.
-spec get_binary_env(string()) -> binary() | undefined.
get_binary_env(EnvVar) ->
    case os:getenv(EnvVar) of
        false -> undefined;
        "" -> undefined;
        Value -> list_to_binary(Value)
    end.

%% @private
%% @doc Get the local node ID.
-spec get_local_node_id() -> binary().
get_local_node_id() ->
    %% Try to get from macula_names, fallback to generated
    case catch macula_names:local_node_id() of
        NodeId when is_binary(NodeId) -> NodeId;
        _ -> crypto:strong_rand_bytes(32)
    end.

%% @private
%% @doc Get local address by finding the best non-loopback interface.
%% Returns {IP, Port} where Port is our configured QUIC port.
-spec get_local_address() -> {inet:ip_address(), inet:port_number()}.
get_local_address() ->
    Port = get_local_quic_port(),
    IP = find_best_local_ip(),
    {IP, Port}.

%% @private
%% @doc Find the best local IP address (non-loopback, non-link-local).
%% Prefers private IPs (10.x, 172.16-31.x, 192.168.x) over loopback.
-spec find_best_local_ip() -> inet:ip_address().
find_best_local_ip() ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            IPs = extract_ipv4_addresses(Interfaces),
            select_best_ip(IPs);
        {error, _} ->
            {0, 0, 0, 0}
    end.

%% @private Extract all IPv4 addresses from interfaces
extract_ipv4_addresses(Interfaces) ->
    lists:foldl(fun({_Name, Props}, Acc) ->
        case proplists:get_value(addr, Props) of
            {A, B, C, _D} = IP when is_integer(A) ->
                [{IP, is_private_ip(A, B, C), is_loopback(A)} | Acc];
            _ ->
                Acc
        end
    end, [], Interfaces).

%% @private Select best IP: prefer non-loopback, then private
select_best_ip([]) ->
    {0, 0, 0, 0};
select_best_ip(IPs) ->
    %% Sort: non-loopback first, then private IPs preferred
    Sorted = lists:sort(fun({_, Private1, Loopback1}, {_, Private2, Loopback2}) ->
        case {Loopback1, Loopback2} of
            {true, false} -> false;  % Non-loopback wins
            {false, true} -> true;
            _ ->
                %% Both same loopback status, prefer private
                Private1 >= Private2
        end
    end, IPs),
    {IP, _, _} = hd(Sorted),
    IP.

%% @private Check if IP is private (RFC 1918)
is_private_ip(10, _, _) -> true;
is_private_ip(172, B, _) when B >= 16, B =< 31 -> true;
is_private_ip(192, 168, _) -> true;
is_private_ip(_, _, _) -> false.

%% @private Check if IP is loopback
is_loopback(127) -> true;
is_loopback(_) -> false.

%% @private
%% @doc Check if local peer can act as relay (has public IP or port-forwarded).
%% A peer is relay-capable if:
%% 1. Local IP equals reflexive IP (we have public IP), OR
%% 2. NAT profile shows can_receive_unsolicited = true
-spec is_relay_capable() -> boolean().
is_relay_capable() ->
    case whereis(?SERVER) of
        undefined ->
            false;
        Pid ->
            case catch gen_server:call(Pid, get_relay_capability, 5000) of
                {ok, Capable} -> Capable;
                _ -> false
            end
    end.

%% @private
%% @doc Check if we have a public IP (local IP equals reflexive IP).
-spec has_public_ip(#state{}) -> boolean().
has_public_ip(#state{observations = []}) ->
    %% No observations yet - check if local IP is public
    {LocalIP, _Port} = get_local_address(),
    is_public_ip(LocalIP);
has_public_ip(#state{observations = [Obs | _]}) ->
    %% Compare local IP with reflexive IP from observation
    {LocalIP, _LocalPort} = get_local_address(),
    case maps:get(reflexive_address, Obs, undefined) of
        {ReflexiveIP, _ReflexivePort} ->
            LocalIP =:= ReflexiveIP;
        _ ->
            false
    end.

%% @private
%% @doc Check if IP is public (not private, not loopback, not link-local).
-spec is_public_ip(inet:ip_address()) -> boolean().
is_public_ip({0, 0, 0, 0}) -> false;
is_public_ip({127, _, _, _}) -> false;  % Loopback
is_public_ip({10, _, _, _}) -> false;   % Private Class A
is_public_ip({172, B, _, _}) when B >= 16, B =< 31 -> false;  % Private Class B
is_public_ip({192, 168, _, _}) -> false;  % Private Class C
is_public_ip({169, 254, _, _}) -> false;  % Link-local
is_public_ip({_, _, _, _}) -> true;       % Everything else is public
is_public_ip(_) -> false.                 % IPv6 or invalid

%% @private
%% @doc Schedule periodic refresh.
-spec schedule_refresh() -> reference().
schedule_refresh() ->
    erlang:send_after(?REFRESH_INTERVAL_MS, self(), refresh).

%% @private
%% @doc Conditionally refresh NAT detection if profile exists.
-spec maybe_refresh_detection(#state{}) -> #state{}.
maybe_refresh_detection(#state{local_profile = undefined} = State) ->
    State;
maybe_refresh_detection(State) ->
    NewState = State#state{observations = []},
    trigger_detection(NewState).
