%%%-------------------------------------------------------------------
%%% @doc Tests for the macula-net Phase 2 record types
%%% (`station_endpoint' and `address_pubkey_map').
%%%
%%% Covers construction, signing, encode/decode round-trip,
%%% storage_key namespacing, and the address-binding check that
%%% Phase 2 resolvers rely on to reject spoofed records.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_record_macula_net_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).

%% =============================================================================
%% Setup
%% =============================================================================

keypair() ->
    macula_identity:generate().

%% =============================================================================
%% station_endpoint
%% =============================================================================

station_endpoint_constructs_with_minimum_fields_test() ->
    Kp = keypair(),
    Pk = macula_identity:public(Kp),
    R = macula_record:station_endpoint(Pk, 4400),
    ?assertEqual(16#12, macula_record:type(R)),
    ?assertEqual(Pk, macula_record:key(R)),
    P = macula_record:payload(R),
    ?assertEqual(4400, maps:get({text, <<"quic_port">>}, P)).

station_endpoint_carries_optional_hosts_and_alpn_test() ->
    Kp = keypair(),
    Pk = macula_identity:public(Kp),
    R = macula_record:station_endpoint(Pk, 4400, #{
        host_advertised => [<<"192.168.1.12">>, <<"2a01:4f8:c0c:abcd::1">>],
        alpn            => <<"macula-net">>
    }),
    P = macula_record:payload(R),
    ?assertEqual([<<"192.168.1.12">>, <<"2a01:4f8:c0c:abcd::1">>],
                 maps:get({text, <<"host_advertised">>}, P)),
    ?assertEqual({text, <<"macula-net">>},
                 maps:get({text, <<"alpn">>}, P)).

station_endpoint_storage_key_is_namespaced_test() ->
    Kp = keypair(),
    Pk = macula_identity:public(Kp),
    %% station_endpoint storage key = sha256("station_endpoint" || pubkey).
    %% This must NOT equal the pubkey itself (which is what node_record
    %% uses) — otherwise the two record types collide in the DHT.
    R = macula_record:station_endpoint(Pk, 4400),
    Sk = macula_record:storage_key(R),
    ?assertEqual(32, byte_size(Sk)),
    ?assertNotEqual(Pk, Sk),
    Expected = crypto:hash(sha256, <<"station_endpoint", Pk/binary>>),
    ?assertEqual(Expected, Sk).

station_endpoint_signs_and_verifies_test() ->
    Kp = keypair(),
    Pk = macula_identity:public(Kp),
    Signed = macula_record:sign(
               macula_record:station_endpoint(Pk, 4400, #{
                   host_advertised => [<<"10.0.0.1">>]
               }), Kp),
    ?assertMatch({ok, _}, macula_record:verify(Signed)).

station_endpoint_round_trips_through_cbor_test() ->
    Kp = keypair(),
    Pk = macula_identity:public(Kp),
    Signed = macula_record:sign(
               macula_record:station_endpoint(Pk, 4400), Kp),
    Encoded = macula_record:encode(Signed),
    {ok, Decoded} = macula_record:decode(Encoded),
    ?assertEqual(Signed, Decoded),
    ?assertMatch({ok, _}, macula_record:verify(Decoded)).

%% =============================================================================
%% address_pubkey_map
%% =============================================================================

%% Build an address that's actually derived from the test pubkey, so
%% the binding check passes by construction.
addr_for(Pk) ->
    macula_address:derive(?REALM, Pk).

address_pubkey_map_constructs_test() ->
    Kp = keypair(),
    Pk = macula_identity:public(Kp),
    Addr = addr_for(Pk),
    R = macula_record:address_pubkey_map(Pk, Addr),
    ?assertEqual(16#13, macula_record:type(R)),
    ?assertEqual(Pk, macula_record:key(R)),
    P = macula_record:payload(R),
    ?assertEqual(Addr, maps:get({text, <<"addr">>}, P)).

address_pubkey_map_storage_key_keyed_by_address_test() ->
    %% A resolver looks up by IPv6, so the storage key MUST be
    %% derivable from the address alone (not the pubkey).
    Kp = keypair(),
    Pk = macula_identity:public(Kp),
    Addr = addr_for(Pk),
    R = macula_record:address_pubkey_map(Pk, Addr),
    Sk = macula_record:storage_key(R),
    Expected = crypto:hash(sha256, <<"address_pubkey_map", Addr/binary>>),
    ?assertEqual(Expected, Sk).

address_pubkey_map_signs_and_verifies_test() ->
    Kp = keypair(),
    Pk = macula_identity:public(Kp),
    Signed = macula_record:sign(
               macula_record:address_pubkey_map(Pk, addr_for(Pk)), Kp),
    ?assertMatch({ok, _}, macula_record:verify(Signed)).

%% The binding check that Phase 2 resolvers rely on: given the record
%% returned by find_record, the resolver MUST recompute the address
%% from `record.key' and the realm, and reject the answer if the
%% recomputed address doesn't match what was asked.
address_binding_holds_for_legitimate_record_test() ->
    Kp = keypair(),
    Pk = macula_identity:public(Kp),
    Addr = addr_for(Pk),
    R = macula_record:address_pubkey_map(Pk, Addr),
    %% Reverse: record claims `Addr' and was signed by `Pk'. Resolver
    %% recomputes derive_address(realm, Pk) and compares to Addr.
    ?assertEqual(Addr, macula_address:derive(?REALM, macula_record:key(R))).

address_binding_rejects_record_signed_by_wrong_key_test() ->
    %% Build a record where the payload claims one address but the
    %% signer is a different identity. A correctly-implemented
    %% resolver MUST reject this even though the signature itself is
    %% valid.
    AttackerKp = keypair(),
    AttackerPk = macula_identity:public(AttackerKp),
    VictimKp   = keypair(),
    VictimPk   = macula_identity:public(VictimKp),
    VictimAddr = addr_for(VictimPk),
    SpoofedRaw = macula_record:address_pubkey_map(AttackerPk, VictimAddr),
    Spoofed    = macula_record:sign(SpoofedRaw, AttackerKp),
    %% Signature is valid (attacker signed their own record).
    ?assertMatch({ok, _}, macula_record:verify(Spoofed)),
    %% But binding fails: the address derived from the attacker's
    %% pubkey is not the victim's address.
    ClaimedAddr = maps:get({text, <<"addr">>},
                            macula_record:payload(Spoofed)),
    DerivedFromKey = macula_address:derive(?REALM,
                                           macula_record:key(Spoofed)),
    ?assertNotEqual(ClaimedAddr, DerivedFromKey).
