%%%-------------------------------------------------------------------
%%% @doc Tests for Phase 3 record additions: hosted_address_map +
%%% host_delegation. PLAN_MACULA_NET_PHASE3.md §3, §4.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_record_phase3_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).

%% =============================================================================
%% Fixtures
%% =============================================================================

new_actor() ->
    Kp = macula_identity:generate(),
    Pk = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

now_ms() -> erlang:system_time(millisecond).

valid_delegation(#{pk := DaemonPk, kp := DaemonKp},
                 #{pk := HostPk}) ->
    NB = now_ms(),
    NA = NB + 5 * 60 * 1000,
    macula_record:sign_host_delegation(
      macula_record:host_delegation(DaemonPk, HostPk, ?REALM, NB, NA),
      DaemonKp).

%% =============================================================================
%% host_delegation construct + sign + verify
%% =============================================================================

host_delegation_constructs_with_required_fields_test() ->
    #{pk := DaemonPk}     = new_actor(),
    #{pk := HostPk}       = new_actor(),
    NB = now_ms(),
    NA = NB + 60_000,
    D = macula_record:host_delegation(DaemonPk, HostPk, ?REALM, NB, NA),
    ?assertEqual(DaemonPk, maps:get(daemon_pubkey, D)),
    ?assertEqual(HostPk,   maps:get(host_pubkey, D)),
    ?assertEqual(?REALM,   maps:get(realm_pubkey, D)),
    ?assertEqual(NB,       maps:get(not_before_ms, D)),
    ?assertEqual(NA,       maps:get(not_after_ms, D)),
    ?assertNot(maps:is_key(daemon_sig, D)).

host_delegation_signed_carries_daemon_sig_test() ->
    Daemon = new_actor(),
    Host   = new_actor(),
    Signed = valid_delegation(Daemon, Host),
    ?assert(maps:is_key(daemon_sig, Signed)),
    ?assertEqual(64, byte_size(maps:get(daemon_sig, Signed))).

host_delegation_signed_verifies_test() ->
    Signed = valid_delegation(new_actor(), new_actor()),
    ?assertMatch({ok, _}, macula_record:verify_host_delegation(Signed)).

host_delegation_unsigned_returns_missing_signature_test() ->
    #{pk := DPk} = new_actor(),
    #{pk := HPk} = new_actor(),
    Unsigned = macula_record:host_delegation(DPk, HPk, ?REALM,
                                              now_ms(),
                                              now_ms() + 60_000),
    ?assertEqual({error, missing_signature},
                 macula_record:verify_host_delegation(Unsigned)).

host_delegation_with_tampered_payload_fails_verify_test() ->
    %% Signed by daemon, then mutate the host_pubkey field — daemon's
    %% signature was over the original; the new value won't verify.
    Daemon = new_actor(),
    Host   = new_actor(),
    Other  = new_actor(),
    Signed = valid_delegation(Daemon, Host),
    Tampered = Signed#{host_pubkey => maps:get(pk, Other)},
    ?assertEqual({error, bad_signature},
                 macula_record:verify_host_delegation(Tampered)).

host_delegation_signed_by_attacker_fails_verify_test() ->
    %% Attacker forges a delegation that names the victim daemon as
    %% the authoriser but signs with their own key. verify must
    %% reject.
    Victim   = new_actor(),
    Host     = new_actor(),
    Attacker = new_actor(),
    NB = now_ms(),
    NA = NB + 60_000,
    Forged = macula_record:sign_host_delegation(
               macula_record:host_delegation(
                 maps:get(pk, Victim), maps:get(pk, Host), ?REALM, NB, NA),
               maps:get(kp, Attacker)),
    ?assertEqual({error, bad_signature},
                 macula_record:verify_host_delegation(Forged)).

%% =============================================================================
%% hosted_address_map record
%% =============================================================================

hosted_address_map_constructs_test() ->
    Daemon = new_actor(),
    Host   = new_actor(),
    Delegation = valid_delegation(Daemon, Host),
    R = macula_record:hosted_address_map(
          maps:get(pk, Host), maps:get(addr, Daemon), Delegation),
    ?assertEqual(16#14,            macula_record:type(R)),
    ?assertEqual(maps:get(pk, Host), macula_record:key(R)),
    P = macula_record:payload(R),
    ?assertEqual(maps:get(addr, Daemon),
                 maps:get({text, <<"addr">>}, P)),
    ?assertEqual(maps:get(pk, Daemon),
                 maps:get({text, <<"daemon">>}, P)).

hosted_address_map_storage_key_keyed_by_addr_test() ->
    Daemon = new_actor(),
    Host   = new_actor(),
    Delegation = valid_delegation(Daemon, Host),
    R = macula_record:hosted_address_map(
          maps:get(pk, Host), maps:get(addr, Daemon), Delegation),
    Sk = macula_record:storage_key(R),
    Expected = crypto:hash(sha256,
                 <<"hosted_address_map", (maps:get(addr, Daemon))/binary>>),
    ?assertEqual(Expected, Sk).

hosted_address_map_signs_and_verifies_test() ->
    Daemon = new_actor(),
    Host   = new_actor(),
    Delegation = valid_delegation(Daemon, Host),
    Signed = macula_record:sign(
               macula_record:hosted_address_map(
                 maps:get(pk, Host), maps:get(addr, Daemon), Delegation),
               maps:get(kp, Host)),
    ?assertMatch({ok, _}, macula_record:verify(Signed)).

hosted_address_map_round_trips_through_cbor_test() ->
    Daemon = new_actor(),
    Host   = new_actor(),
    Delegation = valid_delegation(Daemon, Host),
    Signed = macula_record:sign(
               macula_record:hosted_address_map(
                 maps:get(pk, Host), maps:get(addr, Daemon), Delegation),
               maps:get(kp, Host)),
    Encoded = macula_record:encode(Signed),
    {ok, Decoded} = macula_record:decode(Encoded),
    ?assertEqual(Signed, Decoded),
    ?assertMatch({ok, _}, macula_record:verify(Decoded)).

hosted_storage_key_differs_from_station_owned_storage_key_test() ->
    %% Station-owned (Phase 2) and hosted (Phase 3) records keyed
    %% under different storage namespaces so a resolver can find both
    %% by IPv6 without ambiguity.
    Daemon = new_actor(),
    Host   = new_actor(),
    Addr = maps:get(addr, Daemon),

    %% Phase 2 record (signed by daemon themselves — wouldn't normally
    %% coexist with a Phase 3 record but the storage key must differ).
    StationOwned = macula_record:address_pubkey_map(maps:get(pk, Daemon), Addr),
    Hosted       = macula_record:hosted_address_map(
                     maps:get(pk, Host), Addr,
                     valid_delegation(Daemon, Host)),
    ?assertNotEqual(macula_record:storage_key(StationOwned),
                    macula_record:storage_key(Hosted)).
