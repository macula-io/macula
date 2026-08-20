%%% @doc Signer-vs-claim verification for `content_announcement' providers.
%%%
%%% Regression cover for a real gap: `find_content_providers/2' only
%%% checked that a record carried SOME valid signature, never that the
%%% signer was the identity the payload's `announcer_node' field
%%% actually claims. The public `content_announcement/3,4' constructor
%%% always sets both from the same argument, so this divergence can
%%% only arise from a hand-crafted record — exactly what a malicious
%%% (or buggy, non-SDK) publisher would send. Without the check, node
%%% X could validly sign a record (so the base signature check passes)
%%% while its payload falsely claims to be a DIFFERENT, perhaps more
%%% trusted, `announcer_node' — misattributing who is really serving
%%% content at the embedded `endpoint'. Same class of fix as
%%% `macula_direct_dial:verify_and_build/2' for `station_endpoint'.
-module(macula_content_provider_trust_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MCID, <<1, 16#55, (crypto:strong_rand_bytes(32))/binary>>).

consistent_signer_and_claim_is_trusted_test() ->
    Identity = macula_identity:generate(),
    Node     = macula_identity:public(Identity),
    Signed = macula_record:sign(
               macula_record:content_announcement(Node, ?MCID,
                                                  <<"quic://[::1]:4433">>),
               Identity),
    ?assertMatch({true, #{announcer_node := Node}},
                 macula:decode_provider(Signed)).

unsigned_announcement_is_rejected_test() ->
    Identity = macula_identity:generate(),
    Node     = macula_identity:public(Identity),
    Unsigned = macula_record:content_announcement(Node, ?MCID,
                                                   <<"quic://[::1]:4433">>),
    ?assertEqual(false, macula:decode_provider(Unsigned)).

signer_claiming_a_different_announcer_node_is_rejected_test() ->
    %% Node X is the ACTUAL signer (the record's own envelope key, what
    %% `macula_record:verify/1' checks the signature against) but the
    %% payload claims a DIFFERENT node entirely — the honest constructor
    %% can never produce this shape, so building it directly is exactly
    %% the malicious-publisher scenario the check exists for.
    RealIdentity = macula_identity:generate(),
    RealKey      = macula_identity:public(RealIdentity),
    ClaimedNode  = macula_identity:public(macula_identity:generate()),
    Unsigned0 = macula_record:content_announcement(ClaimedNode, ?MCID,
                                                    <<"quic://evil:4433">>),
    Unsigned  = Unsigned0#{key => RealKey},
    Signed    = macula_record:sign(Unsigned, RealIdentity),
    %% Sanity: this record's base signature genuinely IS valid (signed
    %% by the key it now carries) — the rejection below must come from
    %% the signer-vs-claim check, not from a broken signature.
    ?assertMatch({ok, _}, macula_record:verify(Signed)),
    ?assertEqual(false, macula:decode_provider(Signed)).
