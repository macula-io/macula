%%%-------------------------------------------------------------------
%%% @doc
%%% PropEr property-based tests for macula_core_types.
%%% Tests invariants and edge cases through generated test data.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_core_types_prop).
-include_lib("proper/include/proper.hrl").

%%%===================================================================
%%% Properties
%%%===================================================================

%% Property: node_id always returns 32-byte binary
prop_node_id_always_32_bytes() ->
    ?FORALL(
        _N,
        integer(1, 1000),
        begin
            NodeId = macula_core_types:node_id(),
            is_binary(NodeId) andalso byte_size(NodeId) =:= 32
        end
    ).

%% Property: node_id generates unique IDs
prop_node_id_uniqueness() ->
    ?FORALL(
        Count,
        integer(10, 100),
        begin
            Ids = [macula_core_types:node_id() || _ <- lists:seq(1, Count)],
            length(Ids) =:= length(lists:usort(Ids))
        end
    ).

%% Property: realm_id is deterministic
prop_realm_id_deterministic() ->
    ?FORALL(
        RealmName,
        binary(),
        begin
            Id1 = macula_core_types:realm_id(RealmName),
            Id2 = macula_core_types:realm_id(RealmName),
            Id1 =:= Id2
        end
    ).

%% Property: realm_id always returns 32-byte binary
prop_realm_id_always_32_bytes() ->
    ?FORALL(
        RealmName,
        non_empty(binary()),
        begin
            RealmId = macula_core_types:realm_id(RealmName),
            is_binary(RealmId) andalso byte_size(RealmId) =:= 32
        end
    ).

%% Property: different realm names produce different IDs
prop_realm_id_collision_resistance() ->
    ?FORALL(
        {Name1, Name2},
        {non_empty(binary()), non_empty(binary())},
        (Name1 =:= Name2) orelse
        (macula_core_types:realm_id(Name1) =/= macula_core_types:realm_id(Name2))
    ).

%% Property: address encoding round-trip (IPv4)
prop_address_roundtrip_ipv4() ->
    ?FORALL(
        {{A, B, C, D}, Port},
        {ipv4_address(), port_number()},
        begin
            Original = {{A, B, C, D}, Port},
            Encoded = macula_core_types:encode_address(Original),
            {ok, Decoded} = macula_core_types:decode_address(Encoded),
            Decoded =:= Original
        end
    ).

%% Property: address encoding round-trip (IPv6)
prop_address_roundtrip_ipv6() ->
    ?FORALL(
        {{A, B, C, D, E, F, G, H}, Port},
        {ipv6_address(), port_number()},
        begin
            Original = {{A, B, C, D, E, F, G, H}, Port},
            Encoded = macula_core_types:encode_address(Original),
            {ok, Decoded} = macula_core_types:decode_address(Encoded),
            Decoded =:= Original
        end
    ).

%% Property: encoded addresses are valid binaries
prop_encoded_address_is_binary() ->
    ?FORALL(
        Address,
        oneof([
            {ipv4_address(), port_number()},
            {ipv6_address(), port_number()}
        ]),
        begin
            Encoded = macula_core_types:encode_address(Address),
            is_binary(Encoded)
        end
    ).

%%%===================================================================
%%% Generators
%%%===================================================================

%% Generator: IPv4 address as 4-tuple of bytes
ipv4_address() ->
    {byte_val(), byte_val(), byte_val(), byte_val()}.

%% Generator: IPv6 address as 8-tuple of 16-bit integers
ipv6_address() ->
    {uint16(), uint16(), uint16(), uint16(),
     uint16(), uint16(), uint16(), uint16()}.

%% Generator: Port number (1-65535)
port_number() ->
    integer(1, 65535).

%% Generator: 16-bit unsigned integer
uint16() ->
    integer(0, 65535).

%% Generator: 8-bit unsigned integer (byte value)
byte_val() ->
    integer(0, 255).
