%%%-------------------------------------------------------------------
%%% @doc Tests for macula_resolve_address (Phase 2 §4.2).
%%%
%%% Built around an in-memory mock DHT (just an ETS table). Each
%%% test seeds the table, builds a `find_fn' that consults it, and
%%% calls resolve/3 against various legitimate / spoofed shapes.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_resolve_address_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).

%% =============================================================================
%% Mock DHT helpers
%% =============================================================================

new_dht() ->
    ets:new(mock_dht, [set, public]).

put_dht(Tab, Record) ->
    Key = macula_record:storage_key(Record),
    true = ets:insert(Tab, {Key, Record}),
    Key.

find_fn(Tab) ->
    fun(Key) ->
        case ets:lookup(Tab, Key) of
            [{_, R}] -> {ok, R};
            []       -> {error, not_found}
        end
    end.

new_station() ->
    Kp = macula_identity:generate(),
    Pk = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

advertise(Tab, #{kp := Kp, pk := Pk, addr := Addr}, QuicPort) ->
    Endpoint = macula_record:sign(
                 macula_record:station_endpoint(Pk, QuicPort), Kp),
    Redirect = macula_record:sign(
                 macula_record:address_pubkey_map(Pk, Addr), Kp),
    _ = put_dht(Tab, Endpoint),
    _ = put_dht(Tab, Redirect),
    ok.

%% =============================================================================
%% Tests
%% =============================================================================

resolves_legitimate_advertised_address_test() ->
    Tab = new_dht(),
    St = new_station(),
    advertise(Tab, St, 4400),
    {ok, Endpoint} = macula_resolve_address:resolve(
        maps:get(addr, St), ?REALM, find_fn(Tab)),
    ?assertEqual(maps:get(pk, St), maps:get(station_pubkey, Endpoint)),
    ?assertEqual(4400, maps:get(quic_port, Endpoint)).

resolves_carries_host_advertised_and_alpn_test() ->
    Tab = new_dht(),
    #{kp := Kp, pk := Pk, addr := Addr} = new_station(),
    Endpoint = macula_record:sign(
        macula_record:station_endpoint(Pk, 4400, #{
            host_advertised => [<<"192.168.1.12">>],
            alpn            => <<"macula-net">>
        }), Kp),
    Redirect = macula_record:sign(
        macula_record:address_pubkey_map(Pk, Addr), Kp),
    put_dht(Tab, Endpoint),
    put_dht(Tab, Redirect),
    {ok, R} = macula_resolve_address:resolve(Addr, ?REALM, find_fn(Tab)),
    ?assertEqual([<<"192.168.1.12">>], maps:get(host_advertised, R)),
    ?assertEqual(<<"macula-net">>,     maps:get(alpn, R)).

returns_not_found_when_no_redirect_test() ->
    Tab = new_dht(),
    Addr = <<16#fd, 0:120>>,
    ?assertEqual({error, not_found},
                 macula_resolve_address:resolve(Addr, ?REALM, find_fn(Tab))).

returns_not_found_when_redirect_present_but_endpoint_missing_test() ->
    Tab = new_dht(),
    #{kp := Kp, pk := Pk, addr := Addr} = new_station(),
    %% Only the redirect — no endpoint.
    Redirect = macula_record:sign(
                 macula_record:address_pubkey_map(Pk, Addr), Kp),
    put_dht(Tab, Redirect),
    ?assertEqual({error, not_found},
                 macula_resolve_address:resolve(Addr, ?REALM, find_fn(Tab))).

rejects_redirect_signed_by_unrelated_key_test() ->
    %% An attacker signs a redirect record for a victim's address.
    %% Signature is valid (attacker signed their own record), but
    %% derive_address(realm, attacker_pk) =/= victim_addr — binding
    %% check rejects.
    Tab = new_dht(),
    #{addr := VictimAddr} = new_station(),
    AttackerKp = macula_identity:generate(),
    AttackerPk = macula_identity:public(AttackerKp),
    Spoof = macula_record:sign(
              macula_record:address_pubkey_map(AttackerPk, VictimAddr),
              AttackerKp),
    put_dht(Tab, Spoof),
    ?assertEqual({error, bad_address_binding},
                 macula_resolve_address:resolve(VictimAddr, ?REALM, find_fn(Tab))).

rejects_redirect_with_tampered_signature_test() ->
    %% Build a valid redirect, then mangle its signature.
    Tab = new_dht(),
    #{kp := Kp, pk := Pk, addr := Addr} = new_station(),
    Redirect = macula_record:sign(
                 macula_record:address_pubkey_map(Pk, Addr), Kp),
    Tampered = Redirect#{signature => binary:copy(<<0>>, 64)},
    Key = crypto:hash(sha256, <<"address_pubkey_map", Addr/binary>>),
    ets:insert(Tab, {Key, Tampered}),
    ?assertEqual({error, bad_signature},
                 macula_resolve_address:resolve(Addr, ?REALM, find_fn(Tab))).

rejects_endpoint_signed_by_different_key_than_redirect_test() ->
    %% Pubkey on the redirect points to station A, but the endpoint
    %% record at station_endpoint(A) is mysteriously signed by station B.
    %% Could happen if the DHT was poisoned for that slot. Resolver
    %% must reject because endpoint.key =/= redirect.key.
    Tab = new_dht(),
    #{kp := KpA, pk := PkA, addr := Addr} = new_station(),
    KpB  = macula_identity:generate(),
    PkB  = macula_identity:public(KpB),
    Redirect = macula_record:sign(
                 macula_record:address_pubkey_map(PkA, Addr), KpA),
    %% Endpoint signed by B but parked under A's slot key. We have to
    %% inject it manually because A's correctly-signed endpoint would
    %% land in the same slot.
    ImpostorEndpoint = macula_record:sign(
                         macula_record:station_endpoint(PkB, 4400), KpB),
    AKey = crypto:hash(sha256, <<"station_endpoint", PkA/binary>>),
    put_dht(Tab, Redirect),
    ets:insert(Tab, {AKey, ImpostorEndpoint}),
    ?assertEqual({error, bad_address_binding},
                 macula_resolve_address:resolve(Addr, ?REALM, find_fn(Tab))).
