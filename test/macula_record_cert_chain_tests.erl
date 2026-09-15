%% EUnit tests for the certificate form of a provider authorization (D25 item 6): verify_authorization/3 checks the
%% advertisement's certificate_chain, DER leaf first, against the realm CA the caller trusts. The leaf must carry the
%% advertisement's key and name the procedure's org namespace in its O, and the advertisement must not outlive the
%% leaf. Each certificate's validity is judged at the verifier's Now, not the wall clock, a chain holds at most 4
%% certificates below the realm CA, and no rule reads a certificate's extended key usage. A real ML-DSA-87 chain is
%% minted with OTP public_key, so path validation is exercised for real.
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
    #{realm_ca := Ca, adv := Adv} = fixture(#{leaf_valid_for => 2 * ?MINUTE}),
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

%% Each certificate's window is judged at the verifier's Now, so two stacks judge a chain at the same instant. A leaf
%% not yet valid at the wall clock but valid at Now authorizes.
a_leaf_valid_at_now_but_not_yet_at_the_wall_clock_authorizes_test() ->
    #{realm_ca := Ca, adv := Adv} = fixture(#{leaf_valid_from => ?HOUR}),
    ?assertEqual(ok, authorize(Adv, Ca, now_ms() + 2 * ?HOUR)).

%% A leaf valid at the wall clock but expired at Now is refused.
a_leaf_valid_at_the_wall_clock_but_expired_at_now_is_refused_test() ->
    #{realm_ca := Ca, adv := Adv} = fixture(#{leaf_valid_for => ?HOUR}),
    ?assertEqual({error, cert_chain_untrusted}, authorize(Adv, Ca, now_ms() + 2 * ?HOUR)).

%% A chain holds at most 4 certificates below the realm CA: a leaf, two intermediate CAs and the org CA authorize.
a_chain_of_4_certificates_below_the_realm_ca_authorizes_test() ->
    #{realm_ca := Ca, adv := Adv} = fixture(#{intermediates => 2}),
    ?assertEqual(ok, authorize(Adv, Ca)).

%% A chain of 5 is refused as undecodable before any certificate is parsed, even one that would validate.
a_chain_of_5_certificates_is_refused_before_any_is_parsed_test() ->
    #{realm_ca := Ca, adv := Adv} = fixture(#{intermediates => 3}),
    ?assertEqual({error, cert_chain_undecodable}, authorize(Adv, Ca)).

%% No rule reads a certificate's extended key usage: a leaf whose extKeyUsage names only clientAuth authorizes.
a_leaf_whose_extended_key_usage_names_only_client_auth_authorizes_test() ->
    Usage = #'Extension'{extnID = ?'id-ce-extKeyUsage', critical = false, extnValue = [?'id-kp-clientAuth']},
    #{realm_ca := Ca, adv := Adv} = fixture(#{leaf_extensions => [Usage]}),
    ?assertEqual(ok, authorize(Adv, Ca)).

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

%% Realm CA, org CA with O = rgfaber, any intermediate CAs below the org CA, and a leaf for the advertiser's key; the
%% advertisement carries [leaf, intermediates lowest first, org CA].
fixture(Opts) ->
    {ok, A} = macula_node_keys:generate(identity, pq_pure),
    {RealmPub, RealmPriv} = ca_key(),
    {OrgPub, OrgPriv} = ca_key(),
    RealmSubj = subject(<<"io.macula">>, <<"io.macula">>),
    OrgSubj = subject(<<"io.macula.rgfaber">>, ?ORG),
    RealmDer = cert(RealmSubj, RealmPub#'ML-DSAPublicKey'.key, RealmSubj, RealmPriv, true, utc_after(365 * ?DAY)),
    OrgDer = cert(OrgSubj, OrgPub#'ML-DSAPublicKey'.key, RealmSubj, RealmPriv, true, utc_after(365 * ?DAY)),
    {IssuerSubj, IssuerPriv, Intermediates} = intermediates(maps:get(intermediates, Opts, 0), OrgSubj, OrgPriv, []),
    LeafDer = cert(subject(<<"svc">>, maps:get(leaf_org, Opts, ?ORG)), leaf_key(maps:get(leaf_key, Opts, own), A),
                   IssuerSubj, IssuerPriv, false, utc_after(maps:get(leaf_valid_for, Opts, 30 * ?DAY)),
                   leaf_extra(Opts)),
    #{realm_ca => public_key:pem_encode([{'Certificate', RealmDer, not_encrypted}]),
      adv => advertisement(A, [LeafDer | Intermediates] ++ [OrgDer]),
      org_ca => OrgPriv, org_subject => OrgSubj, org_der => OrgDer}.

%% N intermediate CAs below the org CA, each signed by the one above it: the subject and key that sign the leaf, and
%% the intermediates' certificates, lowest first.
intermediates(0, Subject, Key, Ders) ->
    {Subject, Key, Ders};
intermediates(N, IssuerSubject, IssuerKey, Ders) ->
    {Pub, Priv} = ca_key(),
    Subject = subject(<<"io.macula.rgfaber.", (integer_to_binary(N))/binary>>, ?ORG),
    Der = cert(Subject, Pub#'ML-DSAPublicKey'.key, IssuerSubject, IssuerKey, true, utc_after(365 * ?DAY)),
    intermediates(N - 1, Subject, Priv, [Der | Ders]).

%% The leaf's extensions beside basicConstraints, and its notBefore when the test moves it.
leaf_extra(Opts) ->
    leaf_not_before(maps:find(leaf_valid_from, Opts), #{extensions => maps:get(leaf_extensions, Opts, [])}).

leaf_not_before({ok, Ms}, Extra) -> Extra#{not_before => utc_after(Ms)};
leaf_not_before(error, Extra) -> Extra.

advertisement(A, Chain) ->
    Opts = #{authorization => #{certificate_chain => Chain}, ttl_ms => 300_000},
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
    cert(Subject, PublicKey, IssuerSubject, IssuerKey, IsCa, NotAfter, #{}).

%% Extra may give the certificate's notBefore, and extensions to add beside basicConstraints.
cert(Subject, PublicKey, IssuerSubject, IssuerKey, IsCa, NotAfter, Extra) ->
    Tbs = #'OTPTBSCertificate'{
             version = v3,
             serialNumber = rand:uniform(1 bsl 60),
             signature = #'SignatureAlgorithm'{algorithm = ?'id-ml-dsa-87', parameters = asn1_NOVALUE},
             issuer = IssuerSubject,
             validity = #'Validity'{notBefore = maps:get(not_before, Extra, {utcTime, "230101000000Z"}),
                                    notAfter = NotAfter},
             subject = Subject,
             subjectPublicKeyInfo = #'OTPSubjectPublicKeyInfo'{
                                       algorithm = #'PublicKeyAlgorithm'{algorithm = ?'id-ml-dsa-87',
                                                                         parameters = asn1_NOVALUE},
                                       subjectPublicKey = #'ML-DSAPublicKey'{algorithm = mldsa87, key = PublicKey}},
             extensions = [#'Extension'{extnID = ?'id-ce-basicConstraints', critical = true,
                                        extnValue = #'BasicConstraints'{cA = IsCa, pathLenConstraint = asn1_NOVALUE}}
                           | maps:get(extensions, Extra, [])]},
    public_key:pkix_sign(Tbs, IssuerKey).

subject(CommonName, Org) ->
    {rdnSequence,
     [[#'AttributeTypeAndValue'{type = {2, 5, 4, 3}, value = {utf8String, CommonName}}],
      [#'AttributeTypeAndValue'{type = {2, 5, 4, 10}, value = {utf8String, Org}}]]}.

utc_after(Ms) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(now_ms() + Ms, millisecond),
    {utcTime, lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0wZ", [Y rem 100, Mo, D, H, Mi, S]))}.

authorize(Adv, Ca) ->
    authorize(Adv, Ca, now_ms()).

authorize(Adv, Ca, Now) ->
    macula_record:verify_authorization(verified(Adv), #{profile => pq_pure, realm_ca => Ca}, Now).

verified(Record) ->
    {ok, V} = macula_record:verify(macula_record:encode(Record), pq_pure),
    V.

now_ms() ->
    erlang:system_time(millisecond).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
