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
    refresh/0
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
    last_detection :: integer() | undefined
}).

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
        last_detection = undefined
    }}.

handle_call(get_local_profile, _From, #state{local_profile = undefined} = State) ->
    {reply, not_detected, State};
handle_call(get_local_profile, _From, #state{local_profile = Profile} = State) ->
    {reply, {ok, Profile}, State};

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
    %% Only refresh if we have a profile (initial detection may not have completed)
    case State#state.local_profile of
        undefined -> {noreply, State};
        _ ->
            NewState = State#state{observations = []},
            trigger_detection(NewState),
            {noreply, NewState}
    end;

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
    %% In a real implementation, this would:
    %% 1. Look up bootstrap gateways from DHT
    %% 2. Send NAT_PROBE messages to multiple observers
    %% 3. Wait for NAT_PROBE_REPLY with reflexive addresses
    %% 4. Call add_observation for each reply
    %%
    %% For now, we log and wait for observations from external sources
    ?LOG_DEBUG("NAT detection triggered, waiting for observations"),
    State.

%% @private
%% @doc Trigger detection using a specific observer.
-spec trigger_detection_with_observer(binary(), #state{}) -> #state{}.
trigger_detection_with_observer(ObserverEndpoint, State) ->
    ?LOG_DEBUG("NAT detection triggered with observer ~s", [ObserverEndpoint]),
    %% TODO: Send NAT_PROBE to specific observer
    State.

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

    AllocationPolicy = case ExtPort =:= LocalPort of
        true -> pp;   % Port Preservation
        false -> rd   % Can't determine without more data, assume random
    end,

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
classify_mapping_policy(IP1, Port1, IP2, Port2) ->
    case {IP1 =:= IP2, Port1 =:= Port2} of
        {true, true} -> ei;    % Same IP and port - Endpoint Independent
        {true, false} -> hd;   % Same IP, different port - Host Dependent
        {false, _} -> pd       % Different IP - Port Dependent
    end.

%% @private
%% @doc Classify allocation policy based on port patterns.
-spec classify_allocation_policy(inet:port_number(), inet:port_number(),
                                  inet:port_number(), inet:port_number()) ->
    macula_nat_cache:allocation_policy().
classify_allocation_policy(LocalPort1, ExtPort1, LocalPort2, ExtPort2) ->
    %% Check for port preservation
    case {LocalPort1 =:= ExtPort1, LocalPort2 =:= ExtPort2} of
        {true, true} -> pp;    % Both preserved - Port Preservation
        _ ->
            %% Check for port contiguity
            Delta = abs(ExtPort2 - ExtPort1),
            case Delta < 10 of
                true -> pc;    % Sequential ports - Port Contiguity
                false -> rd    % No pattern - Random
            end
    end.

%% @private
%% @doc Infer filtering policy from mapping policy.
%% Conservative: assume filtering is at least as restrictive as mapping.
-spec infer_filtering_policy(macula_nat_cache:mapping_policy()) ->
    macula_nat_cache:filtering_policy().
infer_filtering_policy(ei) -> pd;   % EI mapping often has PD filtering
infer_filtering_policy(hd) -> pd;   % HD mapping typically has PD filtering
infer_filtering_policy(pd) -> pd.   % PD mapping has PD filtering

%% @private
%% @doc Create a NAT profile with given policies.
-spec create_profile(macula_nat_cache:mapping_policy(),
                     macula_nat_cache:filtering_policy(),
                     macula_nat_cache:allocation_policy()) ->
    macula_nat_cache:nat_profile().
create_profile(MappingPolicy, FilteringPolicy, AllocationPolicy) ->
    #{
        node_id => get_local_node_id(),
        mapping_policy => MappingPolicy,
        filtering_policy => FilteringPolicy,
        allocation_policy => AllocationPolicy,
        can_receive_unsolicited => (MappingPolicy =:= ei andalso FilteringPolicy =:= ei),
        requires_relay => (MappingPolicy =:= pd andalso AllocationPolicy =:= rd),
        relay_capable => is_relay_capable(),
        detected_at => erlang:system_time(second),
        ttl_seconds => 300
    }.

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
%% @doc Get local address (best guess).
-spec get_local_address() -> {inet:ip_address(), inet:port_number()}.
get_local_address() ->
    %% Try to determine local address from bound sockets
    %% For now, return placeholder
    {{0, 0, 0, 0}, 0}.

%% @private
%% @doc Check if local peer can act as relay (has public IP).
-spec is_relay_capable() -> boolean().
is_relay_capable() ->
    %% TODO: Detect if we have a public IP
    false.

%% @private
%% @doc Schedule periodic refresh.
-spec schedule_refresh() -> reference().
schedule_refresh() ->
    erlang:send_after(?REFRESH_INTERVAL_MS, self(), refresh).
