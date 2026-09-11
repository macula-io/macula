%% EUnit tests for the certificate form of a provider authorization (D25 item 6): verify_authorization/3 checks the
%% advertisement's certificate_chain, DER leaf first, against the realm CA the caller trusts. The leaf must carry the
%% advertisement's key and name the procedure's org namespace in its O, and the advertisement must not outlive the
%% leaf. A real ML-DSA-87 chain is minted with OTP public_key, so path validation is exercised for real.
-module(macula_record_cert_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(ORG, <<"rgfaber">>).
-define(PROCEDURE, <<"rgfaber/get_forecast_v1">>).
-define(MINUTE, 60000).
-define(HOUR, 3600000).
-define(DAY, 86400000).

a_valid_chain_authorizes_the_advertisement_test() ->
    #{realm_ca := Ca, adv := Adv} = fixture(#{}),
    ?assertEqual(ok, authorize(Adv, Ca)).

a_leaf_issued_to_another_org_is_refused_test() ->
    #{realm_ca := Ca, adv := Adv} = fixture(#{leaf_org => <<"eve">>}),
    ?assertEqual({error, cert_org_mismatch}, authorize(Adv, Ca)).

a_chain_to_another_realm_ca_is_refused_test() ->
    #{adv := Adv} = fixture(#{}),
    #{realm_ca := OtherCa} = fixture(#{}),
    ?assertEqual({error, cert_chain_untrusted}, authorize(Adv, OtherCa)).

a_leaf_for_another_key_is_refused_test() ->
    #{realm_ca := Ca, adv := Adv} = fixture(#{leaf_key => other}),
    ?assertEqual({error, cert_key_mismatch}, authorize(Adv, Ca)).

an_advertisement_that_outlives_its_leaf_is_refused_test() ->
    #{realm_ca := Ca, adv := Adv} = fixture(#{leaf_valid_for => 10 * ?MINUTE}),
    ?assertEqual({error, authorization_outlived}, authorize(Adv, Ca)).

the_certificate_form_needs_the_realm_ca_test() ->
    #{adv := Adv} = fixture(#{}),
    ?assertEqual({error, no_realm_ca},
                 macula_record:verify_authorization(verified(Adv), #{profile => pq_pure}, now_ms())).

an_undecodable_chain_is_refused_test() ->
    #{realm_ca := Ca} = fixture(#{}),
    {ok, A} = macula_node_keys:generate(identity, pq_pure),
    Adv = advertisement(A, [<<"not a certificate">>]),
    ?assertEqual({error, cert_chain_undecodable}, authorize(Adv, Ca)).

%% A composite key has no X.509 form yet (WP 3.1), so no leaf can carry a pq_hybrid advertiser's key.
a_hybrid_advertiser_has_no_certificate_form_yet_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, A} = macula_node_keys:generate(identity, pq_hybrid),
        <<MlDsaHalf:2592/binary, _Rsa/binary>> = macula_node_keys:public_key(A),
        #{realm_ca := Ca, org_ca := OrgCa, org_subject := OrgSubj, org_der := OrgDer} = fixture(#{}),
        Leaf = cert(subject(<<"svc">>, ?ORG), MlDsaHalf, OrgSubj, OrgCa, false, utc_after(?DAY)),
        Adv = advertisement(A, [Leaf, OrgDer]),
        {ok, V} = macula_record:verify(macula_record:encode(Adv), pq_hybrid),
        ?assertEqual({error, cert_key_mismatch},
                     macula_record:verify_authorization(V, #{profile => pq_hybrid, realm_ca => Ca}, now_ms()))
    end}.

%%------------------------------------------------------------------
%% Fixtures
%%------------------------------------------------------------------

%% Realm CA, org CA with O = rgfaber, and a leaf for the advertiser's key; the advertisement carries [leaf, org CA].
fixture(Opts) ->
    {ok, A} = macula_node_keys:generate(identity, pq_pure),
    {RealmPub, RealmPriv} = ca_key(),
    {OrgPub, OrgPriv} = ca_key(),
    RealmSubj = subject(<<"io.macula">>, <<"io.macula">>),
    OrgSubj = subject(<<"io.macula.rgfaber">>, ?ORG),
    RealmDer = cert(RealmSubj, RealmPub#'ML-DSAPublicKey'.key, RealmSubj, RealmPriv, true, utc_after(365 * ?DAY)),
    OrgDer = cert(OrgSubj, OrgPub#'ML-DSAPublicKey'.key, RealmSubj, RealmPriv, true, utc_after(365 * ?DAY)),
    LeafDer = cert(subject(<<"svc">>, maps:get(leaf_org, Opts, ?ORG)), leaf_key(maps:get(leaf_key, Opts, own), A),
                   OrgSubj, OrgPriv, false, utc_after(maps:get(leaf_valid_for, Opts, 30 * ?DAY))),
    #{realm_ca => public_key:pem_encode([{'Certificate', RealmDer, not_encrypted}]),
      adv => advertisement(A, [LeafDer, OrgDer]),
      org_ca => OrgPriv, org_subject => OrgSubj, org_der => OrgDer}.

advertisement(A, Chain) ->
    Opts = #{authorization => #{certificate_chain => Chain}, ttl_ms => ?HOUR},
    Unsigned = macula_record:procedure_advertisement(macula_node_keys:key_id(A), fill(16#11), ?PROCEDURE, fill(16#77),
                                                     Opts),
    macula_record:sign(Unsigned, A).

leaf_key(own, A) ->
    macula_node_keys:public_key(A);
leaf_key(other, _A) ->
    {ok, Other} = macula_node_keys:generate(identity, pq_pure),
    macula_node_keys:public_key(Other).

%% public_key:generate_key(mldsa87) leaves the seed at <<>>, which public_key's signing then takes for the key; set it
%% to undefined so the expanded key signs (OTP 28.4.2 to 29.0.6).
ca_key() ->
    {Pub, Priv} = public_key:generate_key(mldsa87),
    {Pub, Priv#'ML-DSAPrivateKey'{seed = undefined}}.

cert(Subject, PublicKey, IssuerSubject, IssuerKey, IsCa, NotAfter) ->
    Tbs = #'OTPTBSCertificate'{
             version = v3,
             serialNumber = rand:uniform(1 bsl 60),
             signature = #'SignatureAlgorithm'{algorithm = ?'id-ml-dsa-87', parameters = asn1_NOVALUE},
             issuer = IssuerSubject,
             validity = #'Validity'{notBefore = {utcTime, "230101000000Z"}, notAfter = NotAfter},
             subject = Subject,
             subjectPublicKeyInfo = #'OTPSubjectPublicKeyInfo'{
                                       algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-ml-dsa-87',
                                                                         parameters = asn1_NOVALUE},
                                       subjectPublicKey = #'ML-DSAPublicKey'{algorithm = mldsa87, key = PublicKey}},
             extensions = [#'Extension'{extnID = ?'id-ce-basicConstraints', critical = true,
                                        extnValue = #'BasicConstraints'{cA = IsCa, pathLenConstraint = asn1_NOVALUE}}]},
    public_key:pkix_sign(Tbs, IssuerKey).

subject(CommonName, Org) ->
    {rdnSequence,
     [[#'AttributeTypeAndValue'{type = {2, 5, 4, 3}, value = {utf8String, CommonName}}],
      [#'AttributeTypeAndValue'{type = {2, 5, 4, 10}, value = {utf8String, Org}}]]}.

utc_after(Ms) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(now_ms() + Ms, millisecond),
    {utcTime, lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0wZ", [Y rem 100, Mo, D, H, Mi, S]))}.

authorize(Adv, Ca) ->
    macula_record:verify_authorization(verified(Adv), #{profile => pq_pure, realm_ca => Ca}, now_ms()).

verified(Record) ->
    {ok, V} = macula_record:verify(macula_record:encode(Record), pq_pure),
    V.

now_ms() ->
    erlang:system_time(millisecond).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
