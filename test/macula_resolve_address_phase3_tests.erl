%%%-------------------------------------------------------------------
%%% @doc Tests for the Phase 3 resolver path
%%% (PLAN_MACULA_NET_PHASE3.md §5, milestone 3.2).
%%%
%%% Layered on top of the Phase 2 mock DHT pattern. Each test
%%% advertises a daemon-via-host setup, then exercises the resolve
%%% path: legitimate hosting, expired delegations, attacker-signed
%%% delegations, mismatched fields, missing endpoint records.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_resolve_address_phase3_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).

new_dht() ->
    ets:new(mock_dht, [set, public]).

put_dht(Tab, Record) ->
    Key = macula_record:storage_key(Record),
    ets:insert(Tab, {Key, Record}),
    Key.

find_fn(Tab) ->
    fun(Key) ->
        case ets:lookup(Tab, Key) of
            [{_, R}] -> {ok, R};
            []       -> {error, not_found}
        end
    end.

new_actor() ->
    Kp = macula_identity:generate(),
    Pk = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

now_ms() -> erlang:system_time(millisecond).

valid_delegation(Daemon, Host) ->
    valid_delegation(Daemon, Host, now_ms() + 60_000).

valid_delegation(#{kp := DKp, pk := DPk}, #{pk := HPk}, NotAfter) ->
    macula_record:sign_host_delegation(
      macula_record:host_delegation(DPk, HPk, ?REALM, now_ms(), NotAfter),
      DKp).

%% Advertise: host's station_endpoint + a hosted_address_map for the
%% daemon's address signed by the host (delegation embedded).
advertise_hosted(Tab, Daemon, Host, Port, Hosts, Delegation) ->
    Endpoint = macula_record:sign(
                 macula_record:station_endpoint(maps:get(pk, Host), Port,
                                                #{host_advertised => Hosts}),
                 maps:get(kp, Host)),
    Hosted   = macula_record:sign(
                 macula_record:hosted_address_map(
                   maps:get(pk, Host), maps:get(addr, Daemon), Delegation),
                 maps:get(kp, Host)),
    put_dht(Tab, Endpoint),
    put_dht(Tab, Hosted),
    ok.

%% =============================================================================
%% Tests
%% =============================================================================

resolves_hosted_daemon_address_test() ->
    Tab = new_dht(),
    Daemon = new_actor(),
    Host   = new_actor(),
    advertise_hosted(Tab, Daemon, Host, 4400, [<<"192.168.1.12">>],
                      valid_delegation(Daemon, Host)),
    {ok, R} = macula_resolve_address:resolve(
                maps:get(addr, Daemon), ?REALM, find_fn(Tab)),
    %% Endpoint points at the HOST station, not the daemon.
    ?assertEqual(maps:get(pk, Host), maps:get(station_pubkey, R)),
    ?assertEqual(4400, maps:get(quic_port, R)),
    ?assertEqual([<<"192.168.1.12">>], maps:get(host_advertised, R)),
    %% hosted_daemon tag exposes the daemon pubkey for diagnostics.
    ?assertEqual(maps:get(pk, Daemon), maps:get(hosted_daemon, R)).

phase2_path_still_works_after_phase3_added_test() ->
    %% Sanity: a station-owned address should still resolve via the
    %% Phase 2 path even though the resolver now also checks for
    %% hosted records.
    Tab = new_dht(),
    Station = new_actor(),
    Endpoint = macula_record:sign(
                 macula_record:station_endpoint(maps:get(pk, Station), 4400,
                                                 #{host_advertised => [<<"127.0.0.1">>]}),
                 maps:get(kp, Station)),
    Redirect = macula_record:sign(
                 macula_record:address_pubkey_map(maps:get(pk, Station),
                                                    maps:get(addr, Station)),
                 maps:get(kp, Station)),
    put_dht(Tab, Endpoint),
    put_dht(Tab, Redirect),
    {ok, R} = macula_resolve_address:resolve(
                maps:get(addr, Station), ?REALM, find_fn(Tab)),
    ?assertEqual(maps:get(pk, Station), maps:get(station_pubkey, R)),
    %% Not hosted.
    ?assertNot(maps:is_key(hosted_daemon, R)).

rejects_expired_delegation_test() ->
    Tab = new_dht(),
    Daemon = new_actor(),
    Host   = new_actor(),
    %% Both bounds in the past, but `not_after > not_before' so the
    %% constructor accepts. The resolver must still detect that
    %% `not_after < now'.
    Past1 = now_ms() - 10_000,
    Past2 = now_ms() - 5_000,
    Expired = macula_record:sign_host_delegation(
                macula_record:host_delegation(
                  maps:get(pk, Daemon), maps:get(pk, Host), ?REALM,
                  Past1, Past2),
                maps:get(kp, Daemon)),
    advertise_hosted(Tab, Daemon, Host, 4400, [<<"127.0.0.1">>], Expired),
    ?assertEqual({error, delegation_expired},
                 macula_resolve_address:resolve(
                   maps:get(addr, Daemon), ?REALM, find_fn(Tab))).

rejects_delegation_signed_by_attacker_test() ->
    %% Attacker forges a delegation that names the victim daemon as
    %% authoriser but signs with their own key. Resolver must reject.
    Tab = new_dht(),
    Daemon   = new_actor(),
    Host     = new_actor(),
    Attacker = new_actor(),
    Forged = macula_record:sign_host_delegation(
               macula_record:host_delegation(maps:get(pk, Daemon),
                                              maps:get(pk, Host),
                                              ?REALM,
                                              now_ms(), now_ms() + 60_000),
               maps:get(kp, Attacker)),
    advertise_hosted(Tab, Daemon, Host, 4400, [<<"127.0.0.1">>], Forged),
    ?assertEqual({error, bad_delegation},
                 macula_resolve_address:resolve(
                   maps:get(addr, Daemon), ?REALM, find_fn(Tab))).

rejects_delegation_for_a_different_realm_test() ->
    Tab = new_dht(),
    Daemon = new_actor(),
    Host   = new_actor(),
    %% Build delegation with a different realm field but record under
    %% the test realm. Resolver checks payload/delegation realm match.
    OtherRealm = <<16#22:256>>,
    Mismatched = macula_record:sign_host_delegation(
                   macula_record:host_delegation(maps:get(pk, Daemon),
                                                  maps:get(pk, Host),
                                                  OtherRealm,
                                                  now_ms(),
                                                  now_ms() + 60_000),
                   maps:get(kp, Daemon)),
    advertise_hosted(Tab, Daemon, Host, 4400, [<<"127.0.0.1">>], Mismatched),
    ?assertEqual({error, bad_delegation},
                 macula_resolve_address:resolve(
                   maps:get(addr, Daemon), ?REALM, find_fn(Tab))).

rejects_delegation_for_a_different_host_test() ->
    %% Delegation was signed for HostX but the redirect record is
    %% signed by HostY. The host_pubkey field in the delegation
    %% won't match the redirect's signer.
    Tab = new_dht(),
    Daemon = new_actor(),
    HostX  = new_actor(),
    HostY  = new_actor(),
    %% Daemon authorises HostX, but we craft a record signed by HostY
    %% that embeds the HostX-targeting delegation. (Real attacker
    %% scenario: HostY tries to claim daemons authorised for HostX.)
    DelForX = valid_delegation(Daemon, HostX),
    BadRecord = macula_record:sign(
                  macula_record:hosted_address_map(
                    maps:get(pk, HostY), maps:get(addr, Daemon), DelForX),
                  maps:get(kp, HostY)),
    %% Endpoint for HostY (so the lookup chain doesn't fall over there).
    Endpoint = macula_record:sign(
                 macula_record:station_endpoint(maps:get(pk, HostY), 4400,
                                                 #{host_advertised => [<<"127.0.0.1">>]}),
                 maps:get(kp, HostY)),
    put_dht(Tab, Endpoint),
    put_dht(Tab, BadRecord),
    ?assertEqual({error, bad_delegation},
                 macula_resolve_address:resolve(
                   maps:get(addr, Daemon), ?REALM, find_fn(Tab))).

rejects_when_daemon_pubkey_doesnt_derive_to_address_test() ->
    %% The redirect's payload addr must be derivable from
    %% payload.daemon_pubkey under the realm. Plant a record where
    %% they disagree.
    Tab = new_dht(),
    Daemon = new_actor(),
    Host   = new_actor(),
    %% Build a valid delegation for the daemon, but plant the record
    %% under a DIFFERENT address (not derived from this daemon).
    Delegation = valid_delegation(Daemon, Host),
    WrongAddr = <<16#fd, 0:120>>,
    BadRecord = macula_record:sign(
                  macula_record:hosted_address_map(
                    maps:get(pk, Host), WrongAddr, Delegation),
                  maps:get(kp, Host)),
    Endpoint = macula_record:sign(
                 macula_record:station_endpoint(maps:get(pk, Host), 4400,
                                                 #{host_advertised => [<<"127.0.0.1">>]}),
                 maps:get(kp, Host)),
    put_dht(Tab, Endpoint),
    put_dht(Tab, BadRecord),
    ?assertEqual({error, bad_address_binding},
                 macula_resolve_address:resolve(
                   WrongAddr, ?REALM, find_fn(Tab))).

returns_not_found_when_neither_redirect_present_test() ->
    Tab = new_dht(),
    Addr = <<16#fd, 16#cc:120>>,
    ?assertEqual({error, not_found},
                 macula_resolve_address:resolve(Addr, ?REALM, find_fn(Tab))).

returns_not_found_when_hosted_redirect_present_but_endpoint_missing_test() ->
    Tab = new_dht(),
    Daemon = new_actor(),
    Host   = new_actor(),
    Hosted = macula_record:sign(
               macula_record:hosted_address_map(
                 maps:get(pk, Host), maps:get(addr, Daemon),
                 valid_delegation(Daemon, Host)),
               maps:get(kp, Host)),
    put_dht(Tab, Hosted),
    %% No station_endpoint record for the host.
    ?assertEqual({error, not_found},
                 macula_resolve_address:resolve(
                   maps:get(addr, Daemon), ?REALM, find_fn(Tab))).
