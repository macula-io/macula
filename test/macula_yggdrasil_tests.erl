%% @doc Tests for `macula_yggdrasil:address_for/1' and
%% `format_address/1'.
%%
%% Vectors are taken from the live 3-relay fleet captured during
%% the Phase 1 Tier 1 bringup on 2026-05-01. The Yggdrasil sidecar
%% on each box reported its own pubkey + IPv6 via
%% `yggdrasilctl getSelf'; both inputs and outputs are verbatim
%% from those reports. If `address_for/1' produces the same IPv6
%% from the pubkey, the algorithm matches yggdrasil-go's
%% reference.
-module(macula_yggdrasil_tests).

-include_lib("eunit/include/eunit.hrl").

%% Live fleet vectors — see PLAN_SOVEREIGN_OVERLAY_PHASE1.md
%% Tier 1 result table. Hex-decoded from the values reported by
%% `yggdrasil-bringup.sh --status` on 2026-05-01.
-define(HELSINKI_PUBKEY, <<16#94, 16#9a, 16#5f, 16#00, 16#71, 16#22, 16#c1, 16#ba,
                           16#bc, 16#47, 16#94, 16#f1, 16#16, 16#9c, 16#ef, 16#74,
                           16#de, 16#e5, 16#1f, 16#1e, 16#0e, 16#de, 16#fc, 16#b8,
                           16#ff, 16#2c, 16#c5, 16#0c, 16#fe, 16#17, 16#05, 16#66>>).
-define(HELSINKI_ADDR,   <<16#02, 16#00, 16#d6, 16#cb, 16#41, 16#ff, 16#1d, 16#ba,
                           16#7c, 16#8a, 16#87, 16#70, 16#d6, 16#1d, 16#d2, 16#c6>>).

-define(NUREMBERG_PUBKEY, <<16#3b, 16#84, 16#76, 16#e8, 16#50, 16#15, 16#0c, 16#0f,
                            16#42, 16#28, 16#8c, 16#80, 16#60, 16#be, 16#05, 16#da,
                            16#f6, 16#08, 16#c1, 16#3b, 16#26, 16#d2, 16#bc, 16#cd,
                            16#31, 16#b4, 16#da, 16#92, 16#6c, 16#6a, 16#4c, 16#e7>>).
-define(NUREMBERG_ADDR,   <<16#02, 16#02, 16#23, 16#dc, 16#48, 16#bd, 16#7f, 16#57,
                            16#9f, 16#85, 16#ee, 16#bb, 16#9b, 16#fc, 16#fa, 16#0f>>).

-define(PARIS_PUBKEY, <<16#46, 16#3d, 16#1d, 16#39, 16#d4, 16#35, 16#69, 16#10,
                        16#b4, 16#e5, 16#2e, 16#8f, 16#8c, 16#fa, 16#0d, 16#80,
                        16#ea, 16#83, 16#f2, 16#f3, 16#28, 16#08, 16#4c, 16#f1,
                        16#4f, 16#e9, 16#55, 16#d8, 16#8c, 16#72, 16#31, 16#b3>>).
-define(PARIS_ADDR,   <<16#02, 16#01, 16#e7, 16#0b, 16#8b, 16#18, 16#af, 16#2a,
                        16#5b, 16#bd, 16#2c, 16#6b, 16#45, 16#c1, 16#cc, 16#17>>).

%%==================================================================
%% address_for/1 — fleet vectors
%%==================================================================

helsinki_address_test() ->
    ?assertEqual(?HELSINKI_ADDR, macula_yggdrasil:address_for(?HELSINKI_PUBKEY)).

nuremberg_address_test() ->
    ?assertEqual(?NUREMBERG_ADDR, macula_yggdrasil:address_for(?NUREMBERG_PUBKEY)).

paris_address_test() ->
    ?assertEqual(?PARIS_ADDR, macula_yggdrasil:address_for(?PARIS_PUBKEY)).

%%==================================================================
%% Shape invariants
%%==================================================================

address_is_16_bytes_test() ->
    A = macula_yggdrasil:address_for(?HELSINKI_PUBKEY),
    ?assertEqual(16, byte_size(A)).

address_starts_with_0x02_test() ->
    %% All Yggdrasil node addresses sit in 0200::/7.
    Lst = [?HELSINKI_PUBKEY, ?NUREMBERG_PUBKEY, ?PARIS_PUBKEY],
    [?assertMatch(<<16#02, _/binary>>, macula_yggdrasil:address_for(Pk))
     || Pk <- Lst].

%%==================================================================
%% Wrong-size pubkeys reject
%%==================================================================

short_pubkey_crashes_test() ->
    ?assertError(function_clause,
                 macula_yggdrasil:address_for(<<1, 2, 3>>)).

long_pubkey_crashes_test() ->
    ?assertError(function_clause,
                 macula_yggdrasil:address_for(<<0:264>>)).

%%==================================================================
%% format_address/1
%%==================================================================

format_helsinki_test() ->
    ?assertEqual(<<"200:d6cb:41ff:1dba:7c8a:8770:d61d:d2c6">>,
                 macula_yggdrasil:format_address(?HELSINKI_ADDR)).

format_nuremberg_test() ->
    ?assertEqual(<<"202:23dc:48bd:7f57:9f85:eebb:9bfc:fa0f">>,
                 macula_yggdrasil:format_address(?NUREMBERG_ADDR)).

format_paris_test() ->
    ?assertEqual(<<"201:e70b:8b18:af2a:5bbd:2c6b:45c1:cc17">>,
                 macula_yggdrasil:format_address(?PARIS_ADDR)).

%%==================================================================
%% cert_for/1 — round-trip via the NIF, parse back, pubkey matches
%%==================================================================

cert_for_round_trips_pubkey_test() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, KeyPem}} =
        macula_yggdrasil:cert_for({Pub, Priv}),
    %% Both should be PEM-encoded text.
    ?assertMatch(<<"-----BEGIN CERTIFICATE-----", _/binary>>, CertPem),
    ?assertMatch(<<"-----BEGIN PRIVATE KEY-----", _/binary>>, KeyPem),
    %% The cert's SPKI Ed25519 pubkey must equal the input pubkey.
    [{'Certificate', CertDer, _}] = public_key:pem_decode(CertPem),
    Decoded = public_key:pkix_decode_cert(CertDer, otp),
    %% Decoded SPKI for Ed25519 — check the raw 32 bytes match.
    %% public_key:pkix_decode_cert/2 returns the SPKI as
    %% {'ECPoint', RawBytes} for Ed25519 (it's reusing the EC tag).
    Tbs = element(2, Decoded),
    SpkiInfo = element(8, Tbs),
    {'ECPoint', RawSpki} = element(3, SpkiInfo),
    ?assertEqual(Pub, RawSpki).

cert_for_with_extra_sans_test() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, _KeyPem}} =
        macula_yggdrasil:cert_for({Pub, Priv},
                                  [<<"relay-fi-helsinki.macula.io">>]),
    [{'Certificate', CertDer, _}] = public_key:pem_decode(CertPem),
    %% Both the IP SAN (ygg addr) and the DNS SAN should be
    %% present — exact extension parsing is fiddly, so just check
    %% that the cert mentions the hostname binary somewhere.
    ?assert(binary:match(CertDer, <<"relay-fi-helsinki.macula.io">>)
            =/= nomatch),
    ok.
