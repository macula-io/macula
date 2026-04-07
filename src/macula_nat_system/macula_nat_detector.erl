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
%%%
%%% Pure classification logic is in macula_nat_classifier.
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

%% Re-export classifier functions for backward compatibility with tests
-ifdef(TEST).
-export([
    classify_mapping_policy/4,
    classify_allocation_policy/4,
    classify_port_contiguity/1,
    infer_filtering_policy/1,
    port_preservation_policy/2,
    parse_reflexive_address/2,
    get_msg_field/3,
    ensure_observer_id/2,
    is_public_ip/1,
    is_private_ip/3,
    is_loopback/1,
    select_best_ip/1,
    extract_ipv4_addresses/1,
    parse_float_env/1,
    get_binary_env/1,
    create_profile/3
]).
-endif.

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
%%% Test-only delegations to macula_nat_classifier
%%%===================================================================

-ifdef(TEST).
classify_mapping_policy(IP1, P1, IP2, P2) ->
    macula_nat_classifier:classify_mapping_policy(IP1, P1, IP2, P2).
classify_allocation_policy(LP1, EP1, LP2, EP2) ->
    macula_nat_classifier:classify_allocation_policy(LP1, EP1, LP2, EP2).
classify_port_contiguity(Delta) ->
    macula_nat_classifier:classify_port_contiguity(Delta).
infer_filtering_policy(M) ->
    macula_nat_classifier:infer_filtering_policy(M).
port_preservation_policy(Ext, Local) ->
    macula_nat_classifier:port_preservation_policy(Ext, Local).
parse_reflexive_address(IP, Port) ->
    macula_nat_classifier:parse_reflexive_address(IP, Port).
get_msg_field(Map, AtomKey, BinaryKey) ->
    macula_nat_classifier:get_msg_field(Map, AtomKey, BinaryKey).
ensure_observer_id(Id, Endpoint) ->
    macula_nat_classifier:ensure_observer_id(Id, Endpoint).
is_public_ip(IP) ->
    macula_nat_classifier:is_public_ip(IP).
is_private_ip(A, B, C) ->
    macula_nat_classifier:is_private_ip(A, B, C).
is_loopback(A) ->
    macula_nat_classifier:is_loopback(A).
select_best_ip(IPs) ->
    macula_nat_classifier:select_best_ip(IPs).
extract_ipv4_addresses(Interfaces) ->
    macula_nat_classifier:extract_ipv4_addresses(Interfaces).
parse_float_env(EnvVar) ->
    macula_nat_classifier:parse_float_env(EnvVar).
get_binary_env(EnvVar) ->
    macula_nat_classifier:get_binary_env(EnvVar).
create_profile(M, F, A) ->
    macula_nat_classifier:create_profile(M, F, A).
-endif.

%%%===================================================================
%%% Internal functions - Probe sending & state management
%%%===================================================================

%% @private
%% @doc Check if this node can act as a relay.
check_relay_capability(#state{local_profile = undefined} = State) ->
    has_public_ip(State);
check_relay_capability(#state{local_profile = Profile} = State) ->
    case maps:get(can_receive_unsolicited, Profile, false) of
        true -> true;
        false -> has_public_ip(State)
    end.

%% @private
%% @doc Trigger NAT detection by sending probes to known observers.
-spec trigger_detection(#state{}) -> #state{}.
trigger_detection(State) ->
    Observers = get_known_observers(),
    send_probes_to_observers(Observers, State).

%% @private
-spec trigger_detection_with_observer(binary(), #state{}) -> #state{}.
trigger_detection_with_observer(ObserverEndpoint, State) ->
    send_probes_to_observers([ObserverEndpoint], State).

%% @private
-spec get_known_observers() -> [binary()].
get_known_observers() ->
    case application:get_env(macula, nat_observers) of
        {ok, Observers} when is_list(Observers) ->
            Observers;
        _ ->
            get_connected_gateway_endpoints()
    end.

%% @private
-spec get_connected_gateway_endpoints() -> [binary()].
get_connected_gateway_endpoints() ->
    case whereis(macula_peer_connection_pool) of
        undefined -> [];
        _Pid ->
            case catch macula_peer_connection_pool:get_connected_peers() of
                Peers when is_list(Peers) -> Peers;
                _ -> []
            end
    end.

%% @private
-spec send_probes_to_observers([binary()], #state{}) -> #state{}.
send_probes_to_observers([], State) ->
    ?LOG_DEBUG("No observers available for NAT detection"),
    State;
send_probes_to_observers(Observers, State) ->
    ?LOG_DEBUG("Sending NAT probes to ~p observer(s)", [length(Observers)]),
    lists:foldl(fun send_nat_probe/2, State, Observers).

%% @private
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

    case macula_peer_connector:send_message_and_wait(ObserverEndpoint, nat_probe, ProbeMsg, Timeout) of
        {ok, {nat_probe_reply, ReplyMsg}} ->
            ?LOG_DEBUG("Received NAT_PROBE_REPLY from ~s", [ObserverEndpoint]),
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
-spec process_probe_reply_direct(map(), binary(), inet:port_number(), #state{}) -> #state{}.
process_probe_reply_direct(ReplyMsg, ObserverEndpoint, LocalPort, State) ->
    ReflexiveIP = macula_nat_classifier:get_msg_field(ReplyMsg, reflexive_ip, <<"reflexive_ip">>),
    ReflexivePort = macula_nat_classifier:get_msg_field(ReplyMsg, reflexive_port, <<"reflexive_port">>),
    ObserverId = macula_nat_classifier:get_msg_field(ReplyMsg, node_id, <<"node_id">>),

    case {ReflexiveIP, ReflexivePort} of
        {undefined, _} ->
            ?LOG_DEBUG("NAT probe reply missing reflexive_ip"),
            State;
        {_, undefined} ->
            ?LOG_DEBUG("NAT probe reply missing reflexive_port"),
            State;
        _ ->
            ReflexiveAddr = macula_nat_classifier:parse_reflexive_address(ReflexiveIP, ReflexivePort),
            case ReflexiveAddr of
                {{0, 0, 0, 0}, _} ->
                    ?LOG_DEBUG("Failed to parse reflexive IP address: ~p", [ReflexiveIP]),
                    State;
                _ ->
                    record_observation_direct(ReflexiveAddr, ObserverEndpoint, ObserverId, LocalPort, State)
            end
    end.

%% @private
-spec record_observation_direct(
    {inet:ip_address(), inet:port_number()} | error,
    binary(), binary() | undefined, inet:port_number(), #state{}) -> #state{}.
record_observation_direct(error, _, _, _, State) ->
    ?LOG_DEBUG("Failed to parse reflexive address"),
    State;
record_observation_direct(ReflexiveAddr, ObserverEndpoint, ObserverId, LocalPort, State) ->
    Observation = #{
        observer_id => macula_nat_classifier:ensure_observer_id(ObserverId, ObserverEndpoint),
        reflexive_address => ReflexiveAddr,
        local_address => {{0, 0, 0, 0}, LocalPort},
        observed_at => erlang:system_time(second)
    },

    ?LOG_DEBUG("Recording NAT observation: reflexive=~p local_port=~p", [ReflexiveAddr, LocalPort]),

    NewObservations = [Observation | State#state.observations],
    NewState = State#state{observations = NewObservations},
    maybe_classify_nat(NewState).

%% @private
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
-spec process_probe_reply(map(), #state{}) -> #state{}.
process_probe_reply(ReplyMsg, #state{pending_probes = Pending} = State) ->
    RequestId = macula_nat_classifier:get_msg_field(ReplyMsg, request_id, <<"request_id">>),
    process_probe_reply_with_id(RequestId, ReplyMsg, Pending, State).

%% @private
process_probe_reply_with_id(undefined, _ReplyMsg, _Pending, State) ->
    ?LOG_DEBUG("NAT probe reply missing request_id"),
    State;
process_probe_reply_with_id(RequestId, ReplyMsg, Pending, State) ->
    case maps:find(RequestId, Pending) of
        {ok, PendingProbe} ->
            ReflexiveIP = macula_nat_classifier:get_msg_field(ReplyMsg, reflexive_ip, <<"reflexive_ip">>),
            ReflexivePort = macula_nat_classifier:get_msg_field(ReplyMsg, reflexive_port, <<"reflexive_port">>),
            ObserverId = macula_nat_classifier:get_msg_field(ReplyMsg, node_id, <<"node_id">>),
            process_valid_probe_reply(ReflexiveIP, ReflexivePort, ObserverId, PendingProbe, RequestId, Pending, State);
        error ->
            ?LOG_DEBUG("Received NAT probe reply for unknown request_id=~p",
                       [base64:encode(RequestId)]),
            State
    end.

%% @private
process_valid_probe_reply(undefined, _, _, _, _, _, State) ->
    ?LOG_DEBUG("NAT probe reply missing reflexive_ip"),
    State;
process_valid_probe_reply(_, undefined, _, _, _, _, State) ->
    ?LOG_DEBUG("NAT probe reply missing reflexive_port"),
    State;
process_valid_probe_reply(ReflexiveIP, ReflexivePort, ObserverId, PendingProbe, RequestId, Pending, State) ->
    #{observer_endpoint := ObserverEndpoint, local_port := LocalPort} = PendingProbe,

    ReflexiveAddr = macula_nat_classifier:parse_reflexive_address(ReflexiveIP, ReflexivePort),

    ?LOG_DEBUG("NAT probe reply from ~s: reflexive=~p, local_port=~p",
               [ObserverEndpoint, ReflexiveAddr, LocalPort]),

    Observation = #{
        observer_id => macula_nat_classifier:ensure_observer_id(ObserverId, ObserverEndpoint),
        reflexive_address => ReflexiveAddr,
        local_address => {{0, 0, 0, 0}, LocalPort},
        observed_at => erlang:system_time(second)
    },

    NewObservations = [Observation | State#state.observations],
    NewPending = maps:remove(RequestId, Pending),

    maybe_classify_nat(State#state{
        observations = NewObservations,
        pending_probes = NewPending
    }).

%% @private
-spec process_observation(binary(), {inet:ip_address(), inet:port_number()}, #state{}) -> #state{}.
process_observation(ObserverId, ReflexiveAddress, State) ->
    #state{observations = Observations} = State,
    LocalAddress = get_local_address(),

    Observation = #{
        observer_id => ObserverId,
        reflexive_address => ReflexiveAddress,
        local_address => LocalAddress,
        observed_at => erlang:system_time(second)
    },

    NewObservations = [Observation | Observations],

    ?LOG_DEBUG("Added NAT observation from ~s: ~p", [ObserverId, ReflexiveAddress]),

    NewState = State#state{observations = NewObservations},
    maybe_classify_nat(NewState).

%% @private
-spec maybe_classify_nat(#state{}) -> #state{}.
maybe_classify_nat(#state{observations = Observations} = State) when length(Observations) < 1 ->
    State;
maybe_classify_nat(#state{observations = Observations} = State) ->
    Profile = macula_nat_classifier:classify_nat_type(Observations),

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
-spec get_local_node_id() -> binary().
get_local_node_id() ->
    case catch macula_names:local_node_id() of
        NodeId when is_binary(NodeId) -> NodeId;
        _ -> crypto:strong_rand_bytes(32)
    end.

%% @private
-spec get_local_address() -> {inet:ip_address(), inet:port_number()}.
get_local_address() ->
    Port = get_local_quic_port(),
    IP = find_best_local_ip(),
    {IP, Port}.

%% @private
-spec find_best_local_ip() -> inet:ip_address().
find_best_local_ip() ->
    case inet:getifaddrs() of
        {ok, Interfaces} ->
            IPs = macula_nat_classifier:extract_ipv4_addresses(Interfaces),
            macula_nat_classifier:select_best_ip(IPs);
        {error, _} ->
            {0, 0, 0, 0}
    end.

%% @private
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
-spec has_public_ip(#state{}) -> boolean().
has_public_ip(#state{observations = []}) ->
    {LocalIP, _Port} = get_local_address(),
    macula_nat_classifier:is_public_ip(LocalIP);
has_public_ip(#state{observations = [Obs | _]}) ->
    {LocalIP, _LocalPort} = get_local_address(),
    case maps:get(reflexive_address, Obs, undefined) of
        {ReflexiveIP, _ReflexivePort} ->
            LocalIP =:= ReflexiveIP;
        _ ->
            false
    end.

%% @private
-spec schedule_refresh() -> reference().
schedule_refresh() ->
    erlang:send_after(?REFRESH_INTERVAL_MS, self(), refresh).

%% @private
-spec maybe_refresh_detection(#state{}) -> #state{}.
maybe_refresh_detection(#state{local_profile = undefined} = State) ->
    State;
maybe_refresh_detection(State) ->
    NewState = State#state{observations = []},
    trigger_detection(NewState).
