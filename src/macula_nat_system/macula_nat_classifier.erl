%%%-------------------------------------------------------------------
%%% @doc
%%% Pure functional helpers for NAT type classification.
%%%
%%% Extracted from macula_nat_detector to separate pure logic from
%%% gen_server state management. All functions here are stateless
%%% and side-effect free (except environment variable reads for
%%% geo-config and node ID).
%%%
%%% Responsibilities:
%%% - NAT policy classification (mapping, filtering, allocation)
%%% - NAT profile creation with geo-location
%%% - IP address utilities (public/private/loopback detection)
%%% - Reflexive address parsing
%%% - Message field extraction (atom/binary key support)
%%% - Environment variable parsing helpers
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_classifier).

%% NAT classification
-export([
    classify_nat_type/1,
    classify_mapping_policy/4,
    classify_allocation_policy/4,
    classify_port_contiguity/1,
    infer_filtering_policy/1,
    port_preservation_policy/2,
    create_profile/3
]).

%% IP address utilities
-export([
    is_public_ip/1,
    is_private_ip/3,
    is_loopback/1,
    select_best_ip/1,
    extract_ipv4_addresses/1
]).

%% Message parsing helpers
-export([
    parse_reflexive_address/2,
    get_msg_field/3,
    ensure_observer_id/2
]).

%% Environment variable helpers
-export([
    parse_float_env/1,
    get_binary_env/1
]).

%%%===================================================================
%%% NAT Classification
%%%===================================================================

%% @doc Classify NAT type based on observations.
%% With 0 observations: assume worst case (symmetric NAT).
%% With 1 observation: basic classification (conservative).
%% With 2+ observations: full classification (accurate).
-spec classify_nat_type([map()]) -> macula_nat_cache:nat_profile().
classify_nat_type([]) ->
    create_profile(pd, pd, rd);
classify_nat_type([Obs]) ->
    #{reflexive_address := {_IP, ExtPort}, local_address := {_, LocalPort}} = Obs,
    AllocationPolicy = port_preservation_policy(ExtPort, LocalPort),
    create_profile(ei, pd, AllocationPolicy);
classify_nat_type(Observations) when length(Observations) >= 2 ->
    [Obs1 | Rest] = Observations,
    Obs2 = hd(Rest),

    #{reflexive_address := {IP1, Port1}, local_address := {_, LocalPort1}} = Obs1,
    #{reflexive_address := {IP2, Port2}, local_address := {_, LocalPort2}} = Obs2,

    MappingPolicy = classify_mapping_policy(IP1, Port1, IP2, Port2),
    AllocationPolicy = classify_allocation_policy(LocalPort1, Port1, LocalPort2, Port2),
    FilteringPolicy = infer_filtering_policy(MappingPolicy),

    create_profile(MappingPolicy, FilteringPolicy, AllocationPolicy).

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

%% @doc Classify allocation policy based on port patterns.
-spec classify_allocation_policy(inet:port_number(), inet:port_number(),
                                  inet:port_number(), inet:port_number()) ->
    macula_nat_cache:allocation_policy().
classify_allocation_policy(Port, Port, Port, Port) ->
    pp;    % Both preserved - Port Preservation
classify_allocation_policy(_LocalPort1, ExtPort1, _LocalPort2, ExtPort2) ->
    classify_port_contiguity(abs(ExtPort2 - ExtPort1)).

%% @doc Check for port contiguity based on delta between external ports.
-spec classify_port_contiguity(non_neg_integer()) -> macula_nat_cache:allocation_policy().
classify_port_contiguity(Delta) when Delta < 10 ->
    pc;    % Sequential ports - Port Contiguity
classify_port_contiguity(_Delta) ->
    rd.    % No pattern - Random

%% @doc Infer filtering policy from mapping policy.
%% Conservative: assume filtering is at least as restrictive as mapping.
-spec infer_filtering_policy(macula_nat_cache:mapping_policy()) ->
    macula_nat_cache:filtering_policy().
infer_filtering_policy(ei) -> pd;   % EI mapping often has PD filtering
infer_filtering_policy(hd) -> pd;   % HD mapping typically has PD filtering
infer_filtering_policy(pd) -> pd.   % PD mapping has PD filtering

%% @doc Determine port preservation policy from external and local ports.
-spec port_preservation_policy(inet:port_number(), inet:port_number()) ->
    macula_nat_cache:allocation_policy().
port_preservation_policy(Port, Port) ->
    pp;    % Port Preservation
port_preservation_policy(_ExtPort, _LocalPort) ->
    rd.    % Can't determine without more data, assume random

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
        relay_capable => macula_nat_detector:is_relay_capable(),
        detected_at => erlang:system_time(second),
        ttl_seconds => 300
    },
    add_geo_config(BaseProfile).

%%%===================================================================
%%% IP Address Utilities
%%%===================================================================

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

%% @doc Check if IP is private (RFC 1918).
-spec is_private_ip(integer(), integer(), integer()) -> boolean().
is_private_ip(10, _, _) -> true;
is_private_ip(172, B, _) when B >= 16, B =< 31 -> true;
is_private_ip(192, 168, _) -> true;
is_private_ip(_, _, _) -> false.

%% @doc Check if IP is loopback.
-spec is_loopback(integer()) -> boolean().
is_loopback(127) -> true;
is_loopback(_) -> false.

%% @doc Select best IP: prefer non-loopback, then private.
-spec select_best_ip([{inet:ip_address(), boolean(), boolean()}]) -> inet:ip_address().
select_best_ip([]) ->
    {0, 0, 0, 0};
select_best_ip(IPs) ->
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

%% @doc Extract all IPv4 addresses from interfaces.
-spec extract_ipv4_addresses([{string(), proplists:proplist()}]) ->
    [{inet:ip_address(), boolean(), boolean()}].
extract_ipv4_addresses(Interfaces) ->
    lists:foldl(fun({_Name, Props}, Acc) ->
        case proplists:get_value(addr, Props) of
            {A, B, C, _D} = IP when is_integer(A) ->
                [{IP, is_private_ip(A, B, C), is_loopback(A)} | Acc];
            _ ->
                Acc
        end
    end, [], Interfaces).

%%%===================================================================
%%% Message Parsing Helpers
%%%===================================================================

%% @doc Parse reflexive IP from binary or tuple format.
-spec parse_reflexive_address(binary() | tuple(), inet:port_number()) ->
    {inet:ip_address(), inet:port_number()}.
parse_reflexive_address(IP, Port) when is_tuple(IP) ->
    {IP, Port};
parse_reflexive_address(IP, Port) when is_binary(IP) ->
    case inet:parse_address(binary_to_list(IP)) of
        {ok, ParsedIP} -> {ParsedIP, Port};
        {error, _} -> {{0, 0, 0, 0}, Port}
    end.

%% @doc Helper to get field with atom or binary key.
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

%% @doc Ensure observer ID is valid binary.
-spec ensure_observer_id(term(), binary()) -> binary().
ensure_observer_id(undefined, Endpoint) -> Endpoint;
ensure_observer_id(Id, _Endpoint) when is_binary(Id) -> Id;
ensure_observer_id(_, Endpoint) -> Endpoint.

%%%===================================================================
%%% Environment Variable Helpers
%%%===================================================================

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

%% @doc Get a binary value from environment variable.
%% Returns undefined if not set.
-spec get_binary_env(string()) -> binary() | undefined.
get_binary_env(EnvVar) ->
    case os:getenv(EnvVar) of
        false -> undefined;
        "" -> undefined;
        Value -> list_to_binary(Value)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Get the local node ID.
-spec get_local_node_id() -> binary().
get_local_node_id() ->
    case catch macula_names:local_node_id() of
        NodeId when is_binary(NodeId) -> NodeId;
        _ -> crypto:strong_rand_bytes(32)
    end.

%% @private Read geo-location configuration from environment variables.
-spec add_geo_config(macula_nat_cache:nat_profile()) -> macula_nat_cache:nat_profile().
add_geo_config(Profile) ->
    GeoConfig = get_geo_config(),
    maps:merge(Profile, GeoConfig).

%% @private Get geo configuration from environment variables.
-spec get_geo_config() -> map().
get_geo_config() ->
    maps:filter(fun(_K, V) -> V =/= undefined end, #{
        latitude => parse_float_env("MACULA_LATITUDE"),
        longitude => parse_float_env("MACULA_LONGITUDE"),
        location_label => get_binary_env("MACULA_LOCATION")
    }).

%% @private
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
