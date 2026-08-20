%%% @doc Advertisement trust filtering for `macula_direct_dial'.
%%%
%%% Regression cover for a real gap: the resolve path used to trust
%%% `serving_station' out of the FIRST DHT record found for a procedure,
%%% with no signature check at all. Any identity able to sign SOME
%%% record could point a caller at a real, legitimate station it had no
%%% authority to name — the station_endpoint check that follows only
%%% proves the caller reached the station it was told to reach, not that
%%% whoever told it so was authorized to. `advertisement_trusted/2' is
%%% now the mandatory gate: a bad or absent signature is rejected
%%% unconditionally, and `verify_cert_chain' additionally requires the
%%% advertisement's embedded X.509 chain when a caller opts in (Slice 7c
%%% Direction B; the chain-verification math itself is covered by
%%% `macula_record_cert_chain_tests').
-module(macula_direct_dial_trust_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Mandatory base signature check (no `verify_cert_chain' opt)
%%====================================================================

signed_advertisement_is_trusted_test() ->
    Identity = macula_identity:generate(),
    AdvPub   = macula_identity:public(Identity),
    Station  = macula_identity:public(macula_identity:generate()),
    Ad = macula_record:sign(
           macula_record:procedure_advertisement(AdvPub, <<"realm/proc">>,
                                                 Station),
           Identity),
    ?assert(macula_direct_dial:advertisement_trusted(Ad, #{})).

unsigned_advertisement_is_rejected_test() ->
    Identity = macula_identity:generate(),
    AdvPub   = macula_identity:public(Identity),
    Station  = macula_identity:public(macula_identity:generate()),
    Unsigned = macula_record:procedure_advertisement(AdvPub, <<"realm/proc">>,
                                                      Station),
    ?assertNot(macula_direct_dial:advertisement_trusted(Unsigned, #{})).

tampered_advertisement_is_rejected_test() ->
    Identity = macula_identity:generate(),
    AdvPub   = macula_identity:public(Identity),
    Station  = macula_identity:public(macula_identity:generate()),
    Signed = macula_record:sign(
               macula_record:procedure_advertisement(AdvPub, <<"realm/proc">>,
                                                      Station),
               Identity),
    %% Rewrite `serving_station' after signing — the classic squat: point
    %% a validly-signed-looking record at a DIFFERENT station than the
    %% one the advertiser actually signed for.
    RogueStation = macula_identity:public(macula_identity:generate()),
    Tampered = Signed#{payload => maps:merge(
                 maps:get(payload, Signed),
                 #{{text, <<"serving_station">>} => RogueStation})},
    ?assertNot(macula_direct_dial:advertisement_trusted(Tampered, #{})).

%%====================================================================
%% Opt-in cert-chain path dispatches, does not silently fall through
%%====================================================================

verify_cert_chain_opt_rejects_advertisement_with_no_chain_test() ->
    %% A validly self-signed advertisement with no embedded `cert_chain'
    %% must still be rejected once the caller opted into cert-chain
    %% verification — falling back to the base signature check here
    %% would silently downgrade an explicit `verify_cert_chain' request.
    Identity = macula_identity:generate(),
    AdvPub   = macula_identity:public(Identity),
    Station  = macula_identity:public(macula_identity:generate()),
    Ad = macula_record:sign(
           macula_record:procedure_advertisement(AdvPub, <<"realm/proc">>,
                                                 Station),
           Identity),
    BogusRealmCaPem = <<"not a real PEM">>,
    ?assertNot(macula_direct_dial:advertisement_trusted(
                 Ad, #{verify_cert_chain => {BogusRealmCaPem, <<"acme">>}})).
