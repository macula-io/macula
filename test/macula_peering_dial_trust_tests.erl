%%% @doc TLS-vs-application-layer trust selection for `macula_peering_conn'.
%%%
%%% Regression cover for the direct-dial-against-production gap: pinning
%%% `expected_node_id' used to ALWAYS also pin the QUIC/TLS certificate's
%%% own SPKI (`verify_pubkey'), which only works when the peer's TLS cert
%%% genuinely IS its macula identity (self-signed). A production station
%%% terminates TLS via an unrelated PKI (e.g. Let's Encrypt), so that pin
%%% can never succeed there. `pin_tls_cert => false' opts a caller out of
%%% the TLS-layer pin for a specific dial while leaving the real
%%% cryptographic check — the signed CONNECT/HELLO handshake's
%%% `bind_peer_identity/2', armed by the same `expected_node_id' — fully
%%% intact regardless of this flag.
-module(macula_peering_dial_trust_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NODE_ID, crypto:strong_rand_bytes(32)).

%%====================================================================
%% Default behaviour (`pin_tls_cert' absent) is unchanged
%%====================================================================

expected_node_id_alone_pins_tls_cert_test() ->
    NodeId = ?NODE_ID,
    ?assertEqual([{verify_pubkey, NodeId}],
                 macula_peering_conn:dial_trust_opts(
                   #{expected_node_id => NodeId})).

expected_node_id_ignores_verify_when_pinning_test() ->
    %% Documented behaviour: the pubkey pin wins over an explicit
    %% `verify' when `pin_tls_cert' is not set to `false'.
    NodeId = ?NODE_ID,
    ?assertEqual([{verify_pubkey, NodeId}],
                 macula_peering_conn:dial_trust_opts(
                   #{expected_node_id => NodeId, verify => webpki})).

bare_verify_mode_flows_through_test() ->
    ?assertEqual([{verify, none}],
                 macula_peering_conn:dial_trust_opts(#{verify => none})).

no_trust_opts_test() ->
    ?assertEqual([], macula_peering_conn:dial_trust_opts(#{host => <<"x">>})).

%%====================================================================
%% `pin_tls_cert => false' — application-layer-only trust
%%====================================================================

pin_tls_cert_false_skips_verify_pubkey_test() ->
    NodeId = ?NODE_ID,
    Opts = macula_peering_conn:dial_trust_opts(
             #{expected_node_id => NodeId, pin_tls_cert => false}),
    ?assertNot(lists:keymember(verify_pubkey, 1, Opts)).

pin_tls_cert_false_defaults_verify_to_none_test() ->
    NodeId = ?NODE_ID,
    ?assertEqual([{verify, none}],
                 macula_peering_conn:dial_trust_opts(
                   #{expected_node_id => NodeId, pin_tls_cert => false})).

pin_tls_cert_false_honors_explicit_verify_test() ->
    %% A caller that also wants webpki alongside app-layer trust (an
    %% odd but valid combination) is not silently overridden.
    NodeId = ?NODE_ID,
    ?assertEqual([{verify, webpki}],
                 macula_peering_conn:dial_trust_opts(
                   #{expected_node_id => NodeId, pin_tls_cert => false,
                     verify => webpki})).

pin_tls_cert_true_matches_default_test() ->
    %% Explicit `true' behaves exactly like the field being absent.
    NodeId = ?NODE_ID,
    ?assertEqual([{verify_pubkey, NodeId}],
                 macula_peering_conn:dial_trust_opts(
                   #{expected_node_id => NodeId, pin_tls_cert => true})).
