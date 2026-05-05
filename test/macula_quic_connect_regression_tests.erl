%%%-------------------------------------------------------------------
%%% @doc Regression tests for macula_quic:connect/4 NIF arg decoding.
%%%
%%% 3.15.1 fixed a `badarg' from `nif_connect/8' caused by the Rust
%%% signature taking `verify_pubkey: Vec<u8>' (rustler's Vec<T> only
%%% accepts list terms, never binaries) while every Erlang caller
%%% passes a binary. Switched to `Binary<'a>'.
%%%
%%% These tests pin that contract — they call connect/4 the way
%%% macula-station's outbound seed-dial path does (binary verify_pubkey,
%%% binary host, binary ALPN), and assert no badarg at the NIF
%%% boundary. Keep the dependency surface minimal: no transport,
%%% no peering, no telemetry — just listen + connect.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_connect_regression_tests).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test fixtures
%% =============================================================================

connect_regression_test_() ->
    {timeout, 30,
     {setup,
      fun setup_listener/0,
      fun cleanup/1,
      fun(Ctx) ->
          [{"empty verify_pubkey (default)",
            fun() -> connect_with_empty_pubkey(Ctx) end},
           {"explicit empty verify_pubkey binary",
            fun() -> connect_with_explicit_empty_pubkey(Ctx) end},
           {"32-byte verify_pubkey binary",
            fun() -> connect_with_pinned_pubkey(Ctx) end},
           {"station-style opts (mirrors macula_station_outbound_link)",
            fun() -> connect_station_style(Ctx) end}]
      end}}.

%% =============================================================================
%% Setup: ephemeral self-signed listener on loopback
%% =============================================================================

setup_listener() ->
    {Pub, Priv} = ephemeral_keypair(),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            Pub, Priv, [<<"localhost">>, <<"127.0.0.1">>]),
    Tmp  = lists:flatten(io_lib:format("/tmp/macula-quic-conn-regress-~p",
                                       [erlang:unique_integer([positive])])),
    Cert = Tmp ++ ".crt",
    Key  = Tmp ++ ".key",
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key,  KeyPem),
    Port = pick_free_port(),
    {ok, Listener} = macula_quic:listen(
        <<"127.0.0.1">>, Port,
        [{cert, Cert}, {key, Key},
         {alpn, [<<"macula">>]},
         {idle_timeout_ms, 30000},
         {keep_alive_interval_ms, 5000}]),
    ok = macula_quic:async_accept(Listener),
    #{listener => Listener,
      port     => Port,
      cert     => Cert,
      key      => Key,
      pubkey   => Pub}.

cleanup(#{listener := L, cert := Cert, key := Key}) ->
    catch macula_quic:close_listener(L),
    file:delete(Cert),
    file:delete(Key),
    %% Drain any leftover {quic, new_conn, ...} the accepts produced.
    drain_quic_messages(),
    ok.

%% =============================================================================
%% The actual regression assertions
%% =============================================================================

%% Default path: verify_pubkey not in opts at all. Wrapper substitutes
%% `<<>>'. Pre-fix this blew up with badarg inside nif_connect.
connect_with_empty_pubkey(#{port := Port}) ->
    Result = macula_quic:connect(
        <<"127.0.0.1">>, Port,
        [{verify, none}, {alpn, [<<"macula">>]}],
        5000),
    assert_connected(Result).

%% Explicit empty binary — same path, different opt shape.
connect_with_explicit_empty_pubkey(#{port := Port}) ->
    Result = macula_quic:connect(
        <<"127.0.0.1">>, Port,
        [{verify, none},
         {alpn, [<<"macula">>]},
         {verify_pubkey, <<>>}],
        5000),
    assert_connected(Result).

%% Pinned 32-byte binary — exercises the Some(...) branch of the
%% verify_pubkey decoder. Cert won't actually match (we use random
%% bytes, not the listener's real pubkey), so we expect connect to
%% complete the handshake-attempt path and either succeed (since
%% verify=none above means the SPKI pin is the only check) or fail
%% with a connect/handshake error — but NEVER `badarg' from the NIF
%% boundary itself. That's the regression we're guarding.
connect_with_pinned_pubkey(#{port := Port, pubkey := Pub}) ->
    %% Use the *real* listener pubkey so the SPKI pin matches.
    Result = macula_quic:connect(
        <<"127.0.0.1">>, Port,
        [{verify, none},
         {alpn, [<<"macula">>]},
         {verify_pubkey, Pub}],
        5000),
    assert_connected(Result).

%% Mirrors macula_station_outbound_link's exact connect call shape.
%% If this passes, fresh-fleet seed-dial bootstrap will work end-to-end.
connect_station_style(#{port := Port}) ->
    Result = macula_quic:connect(
        <<"127.0.0.1">>, Port,
        [{alpn, [<<"macula">>]},
         {verify, none},
         {idle_timeout_ms, 60000},
         {keep_alive_interval_ms, 20000}],
        5000),
    assert_connected(Result).

%% =============================================================================
%% Helpers
%% =============================================================================

assert_connected({ok, ConnRef}) when is_reference(ConnRef) ->
    catch macula_quic:close_connection(ConnRef),
    ok;
assert_connected(Other) ->
    erlang:error({connect_failed_at_nif_boundary, Other}).

ephemeral_keypair() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {iolist_to_binary(Pub), iolist_to_binary(Priv)}.

pick_free_port() ->
    {ok, S} = gen_udp:open(0, [binary, {ip, {127,0,0,1}}]),
    {ok, P} = inet:port(S),
    gen_udp:close(S),
    P.

drain_quic_messages() ->
    receive
        {quic, _, _, _} -> drain_quic_messages()
    after 0 -> ok
    end.
