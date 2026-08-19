%%% @doc Tests for `macula_record:verify_advertisement_cert_chain/3'
%%% (Slice 7c Direction B): a managed-realm consumer verifies an
%%% advertisement's embedded X.509 service-cert chain against the realm
%%% CA it trusts. A real chain (realm CA -> org CA -> Ed25519 leaf, the
%%% leaf binding the advertiser key) is minted in-process with OTP
%%% `public_key' so the test exercises the actual path validation, key
%%% binding, and org RDN extraction — not a hand-rolled stand-in.
-module(macula_record_cert_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(ORG, <<"rgfaber">>).

%%%===================================================================
%%% Tests
%%%===================================================================

valid_chain_verifies_test() ->
    #{realm_ca := RealmCaPem, adv := Adv} = fixture(?ORG, ?ORG),
    ?assertEqual(ok,
                 macula_record:verify_advertisement_cert_chain(RealmCaPem, Adv, ?ORG)).

%% The running realm signs its CAs with P-256 (secp256r1), not Ed25519.
%% The leaf still binds the service's Ed25519 key. Confirms the verifier
%% handles the real mixed-key chain, not just the all-Ed25519 test shape.
valid_p256_ca_chain_verifies_test() ->
    #{realm_ca := RealmCaPem, adv := Adv} = fixture_p256(?ORG),
    ?assertEqual(ok,
                 macula_record:verify_advertisement_cert_chain(RealmCaPem, Adv, ?ORG)).

wrong_org_dropped_test() ->
    %% Leaf is issued under org "eve"; the consumer expects "rgfaber".
    #{realm_ca := RealmCaPem, adv := Adv} = fixture(<<"eve">>, <<"eve">>),
    ?assertEqual({error, cert_org_mismatch},
                 macula_record:verify_advertisement_cert_chain(RealmCaPem, Adv, ?ORG)).

self_signed_leaf_dropped_test() ->
    %% A squatter's chain rooted in its OWN realm CA, not the trusted one.
    #{adv := Adv} = fixture(?ORG, ?ORG),
    #{realm_ca := OtherRealmCaPem} = fixture(?ORG, ?ORG),
    ?assertEqual({error, cert_chain_untrusted},
                 macula_record:verify_advertisement_cert_chain(OtherRealmCaPem, Adv, ?ORG)).

key_mismatch_dropped_test() ->
    %% Advertisement signed by the advertiser key, but the embedded leaf
    %% binds a DIFFERENT key — the cert does not certify this advertiser.
    #{realm_ca := RealmCaPem, adv := Adv} = fixture_key_mismatch(?ORG),
    ?assertEqual({error, cert_key_mismatch},
                 macula_record:verify_advertisement_cert_chain(RealmCaPem, Adv, ?ORG)).

missing_chain_dropped_test() ->
    %% A validly-signed advertisement with no embedded cert chain.
    Kp = macula_identity:generate(),
    Adv = signed_advertisement(Kp, ?ORG, #{}),
    #{realm_ca := RealmCaPem} = fixture(?ORG, ?ORG),
    ?assertEqual({error, no_cert_chain},
                 macula_record:verify_advertisement_cert_chain(RealmCaPem, Adv, ?ORG)).

tampered_advertisement_dropped_test() ->
    #{realm_ca := RealmCaPem, adv := Adv} = fixture(?ORG, ?ORG),
    Tampered = Adv#{key => macula_identity:public(macula_identity:generate())},
    ?assertEqual({error, advertisement_bad_signature},
                 macula_record:verify_advertisement_cert_chain(RealmCaPem, Tampered, ?ORG)).

%% The embedded cert chain must survive the wire codec: a consumer
%% resolves advertisements over CBOR, then verifies the chain. If the
%% cert_chain field did not round-trip, verify would fail with
%% no_cert_chain even for a legitimate provider.
cert_chain_survives_wire_roundtrip_test() ->
    #{realm_ca := RealmCaPem, adv := Adv} = fixture(?ORG, ?ORG),
    {ok, Decoded} = macula_record:decode(macula_record:encode(Adv)),
    ?assertEqual(ok,
                 macula_record:verify_advertisement_cert_chain(RealmCaPem, Decoded, ?ORG)).

%%%===================================================================
%%% Fixtures
%%%===================================================================

%% A full valid setup: realm CA -> org CA (O=CertOrg) -> leaf binding the
%% advertiser key; the advertisement carries [leaf, orgCA] and is signed
%% by the advertiser keypair. `UriOrg' is the org segment in the
%% procedure_uri (matches CertOrg in the happy path).
fixture(CertOrg, UriOrg) ->
    Kp = macula_identity:generate(),
    AdvKey = macula_identity:public(Kp),
    #{realm_ca_pem := RealmCaPem, chain_pem := ChainPem} =
        issue_chain(AdvKey, CertOrg),
    Adv = signed_advertisement(Kp, UriOrg, #{cert_chain => ChainPem}),
    #{realm_ca => RealmCaPem, adv => Adv}.

%% Same, but the leaf binds a fresh key that is NOT the advertiser's.
fixture_key_mismatch(UriOrg) ->
    Kp = macula_identity:generate(),
    OtherKey = macula_identity:public(macula_identity:generate()),
    #{realm_ca_pem := RealmCaPem, chain_pem := ChainPem} =
        issue_chain(OtherKey, UriOrg),
    Adv = signed_advertisement(Kp, UriOrg, #{cert_chain => ChainPem}),
    #{realm_ca => RealmCaPem, adv => Adv}.

%% Like fixture/2 but the CAs sign with P-256 (the realm's real key type).
fixture_p256(Org) ->
    Kp = macula_identity:generate(),
    AdvKey = macula_identity:public(Kp),
    #{realm_ca_pem := RealmCaPem, chain_pem := ChainPem} =
        issue_chain_p256(AdvKey, Org),
    Adv = signed_advertisement(Kp, Org, #{cert_chain => ChainPem}),
    #{realm_ca => RealmCaPem, adv => Adv}.

signed_advertisement(Kp, Org, Opts) ->
    AdvKey  = macula_identity:public(Kp),
    Station = macula_identity:public(macula_identity:generate()),
    Uri     = <<"realmhex/", Org/binary, "/get_forecast_v1">>,
    Rec     = macula_record:procedure_advertisement(AdvKey, Uri, Station, Opts),
    macula_record:sign(Rec, Kp).

%%%===================================================================
%%% Minimal in-process X.509 CA (OTP public_key)
%%%===================================================================

%% Issue realm CA (self-signed) -> org CA -> Ed25519 leaf binding
%% `LeafPub' with organization `Org'. Returns the trusted realm CA PEM
%% and the leaf-first [leaf, orgCA] PEM bundle the advertiser embeds.
issue_chain(LeafPub, Org) ->
    {RealmPub, RealmPriv} = ca_key(),
    {OrgPub, OrgPriv}     = ca_key(),
    RealmSubj = subject(<<"io.macula">>, <<"io.macula">>),
    OrgSubj   = subject(<<"io.macula.", Org/binary>>, Org),
    LeafSubj  = subject(<<"mri:app:io.macula/", Org/binary, "/svc">>, Org),
    RealmDer = sign_cert(RealmSubj, ec_spki(RealmPub), RealmSubj, RealmPriv, true),
    OrgDer   = sign_cert(OrgSubj, ec_spki(OrgPub), RealmSubj, RealmPriv, true),
    LeafDer  = sign_cert(LeafSubj, ed_spki(LeafPub), OrgSubj, OrgPriv, false),
    #{realm_ca_pem => pem([RealmDer]),
      chain_pem    => pem([LeafDer, OrgDer])}.

%% CA keys are Ed25519 too (OTP verifies Ed25519 issuer signatures).
ca_key() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {Pub, #'ECPrivateKey'{version = 1, privateKey = Priv,
                          parameters = {namedCurve, ?'id-Ed25519'},
                          publicKey = Pub}}.

%% P-256 CA (the realm's real CA key type, from key_gen.ex secp256r1).
issue_chain_p256(LeafPub, Org) ->
    {RealmPub, RealmPriv} = p256_ca_key(),
    {OrgPub, OrgPriv}     = p256_ca_key(),
    RealmSubj = subject(<<"io.macula">>, <<"io.macula">>),
    OrgSubj   = subject(<<"io.macula.", Org/binary>>, Org),
    LeafSubj  = subject(<<"mri:app:io.macula/", Org/binary, "/svc">>, Org),
    RealmDer = sign_cert(RealmSubj, p256_spki(RealmPub), RealmSubj, RealmPriv, true),
    OrgDer   = sign_cert(OrgSubj, p256_spki(OrgPub), RealmSubj, RealmPriv, true),
    LeafDer  = sign_cert(LeafSubj, ed_spki(LeafPub), OrgSubj, OrgPriv, false),
    #{realm_ca_pem => pem([RealmDer]), chain_pem => pem([LeafDer, OrgDer])}.

p256_ca_key() ->
    {Pub, Priv} = crypto:generate_key(ecdh, secp256r1),
    {Pub, #'ECPrivateKey'{version = 1, privateKey = Priv,
                          parameters = {namedCurve, ?'secp256r1'},
                          publicKey = Pub}}.

p256_spki(Pub) ->
    #'OTPSubjectPublicKeyInfo'{
       algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-ecPublicKey',
                                         parameters = {namedCurve, ?'secp256r1'}},
       subjectPublicKey = #'ECPoint'{point = Pub}}.

ed_spki(Pub) ->
    #'OTPSubjectPublicKeyInfo'{
       algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-Ed25519',
                                         parameters = asn1_NOVALUE},
       subjectPublicKey = #'ECPoint'{point = Pub}}.

ec_spki(Pub) -> ed_spki(Pub).

subject(CN, O) ->
    {rdnSequence,
     [[#'AttributeTypeAndValue'{type = {2, 5, 4, 3}, value = {utf8String, CN}}],
      [#'AttributeTypeAndValue'{type = {2, 5, 4, 10}, value = {utf8String, O}}]]}.

sign_cert(Subject, Spki, IssuerSubject, IssuerKey, IsCA) ->
    TBS = #'OTPTBSCertificate'{
             version = v3,
             serialNumber = rand:uniform(1 bsl 60),
             signature = sig_alg(IssuerKey),
             issuer = IssuerSubject,
             validity = #'Validity'{notBefore = {utcTime, "230101000000Z"},
                                    notAfter  = {utcTime, "330101000000Z"}},
             subject = Subject,
             subjectPublicKeyInfo = Spki,
             extensions = [basic_constraints(IsCA)]},
    public_key:pkix_sign(TBS, IssuerKey).

%% The signature algorithm matching the issuer's key type.
sig_alg(#'ECPrivateKey'{parameters = {namedCurve, ?'id-Ed25519'}}) ->
    #'SignatureAlgorithm'{algorithm = ?'id-Ed25519', parameters = asn1_NOVALUE};
sig_alg(#'ECPrivateKey'{parameters = {namedCurve, ?'secp256r1'}}) ->
    #'SignatureAlgorithm'{algorithm = ?'ecdsa-with-SHA256',
                          parameters = asn1_NOVALUE}.

basic_constraints(IsCA) ->
    #'Extension'{extnID = ?'id-ce-basicConstraints', critical = true,
                 extnValue = #'BasicConstraints'{cA = IsCA,
                                                 pathLenConstraint = asn1_NOVALUE}}.

pem(Ders) ->
    public_key:pem_encode([{'Certificate', D, not_encrypted} || D <- Ders]).
