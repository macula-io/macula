%% EUnit tests for the removed certificate form of a provider authorization (D25 item 6). In 11.0.0 a provider is
%% authorized only by the realm-signed org directory and the org-signed procedure delegation: the realm issues no X.509
%% certificates. An advertisement whose authorization is a certificate chain is refused as
%% authorization_form_unsupported, even one whose chain leads to a realm CA the caller holds. No builder writes that
%% form, and nothing in macula validates a certificate path or reads a realm CA.
-module(macula_record_cert_chain_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

-define(ORG, <<"rgfaber">>).
-define(PROCEDURE, <<"rgfaber/get_forecast_v1">>).
-define(DAY, 86400000).

%% A chain that authorized a provider before the form was removed: realm CA, org CA with O = rgfaber, and a leaf for
%% the advertiser's key. Its advertisement is refused whether or not the caller still passes the realm CA.
a_certificate_chain_authorization_is_refused_as_an_unsupported_form_test() ->
    #{realm_ca := Ca, adv := Adv} = fixture(),
    ?assertEqual({error, authorization_form_unsupported}, authorize(Adv, #{profile => pq_pure})),
    ?assertEqual({error, authorization_form_unsupported}, authorize(Adv, #{profile => pq_pure, realm_ca => Ca})).

%% No builder writes the removed form.
the_builder_refuses_a_certificate_chain_authorization_test() ->
    ?assertError(function_clause,
                 macula_record:procedure_advertisement(fill(1), fill(16#11), ?PROCEDURE, fill(16#77),
                                                       #{authorization => #{certificate_chain => [<<1>>]}})).

%% Nothing in macula's own code validates a certificate path or reads a realm CA. The scan reads every source file
%% under src/, found from macula_record's own compile information.
no_source_validates_a_certificate_path_or_reads_a_realm_ca_test() ->
    Record = proplists:get_value(source, macula_record:module_info(compile)),
    Sources = filelib:wildcard(filename:join([filename:dirname(filename:dirname(Record)), "**", "*.erl"])),
    ?assert(length(Sources) > 50),
    ?assertEqual([], [{filename:basename(File), Needle} || File <- Sources, {ok, Text} <- [file:read_file(File)],
                                                         Needle <- [<<"pkix_path_validation">>, <<"realm_ca">>],
                                                         binary:match(Text, Needle) =/= nomatch]).

%%------------------------------------------------------------------
%% Fixtures
%%------------------------------------------------------------------

%% A realm CA, an org CA with O = rgfaber, and a leaf for the advertiser's key, in the advertisement as [leaf, org CA].
fixture() ->
    {ok, A} = macula_node_keys:generate(identity, pq_pure),
    {RealmPub, RealmPriv} = ca_key(),
    {OrgPub, OrgPriv} = ca_key(),
    RealmSubj = subject(<<"io.macula">>, <<"io.macula">>),
    OrgSubj = subject(<<"io.macula.rgfaber">>, ?ORG),
    RealmDer = cert(RealmSubj, RealmPub#'ML-DSAPublicKey'.key, RealmSubj, RealmPriv, true),
    OrgDer = cert(OrgSubj, OrgPub#'ML-DSAPublicKey'.key, RealmSubj, RealmPriv, true),
    LeafDer = cert(subject(<<"svc">>, ?ORG), macula_node_keys:public_key(A), OrgSubj, OrgPriv, false),
    #{realm_ca => public_key:pem_encode([{'Certificate', RealmDer, not_encrypted}]),
      adv => advertisement(A, [LeafDer, OrgDer])}.

%% The provider's advertisement with a certificate_chain authorization, put into its payload by hand because no builder
%% writes that form.
advertisement(A, Chain) ->
    Unsigned = macula_record:procedure_advertisement(macula_node_keys:key_id(A), fill(16#11), ?PROCEDURE, fill(16#77),
                                                     #{ttl_ms => 300_000}),
    Authorization = #{{text, <<"certificate_chain">>} => Chain},
    Payload = (macula_record:payload(Unsigned))#{{text, <<"authorization">>} => Authorization},
    macula_record:sign(Unsigned#{payload := Payload}, A).

%% public_key:generate_key(mldsa87) leaves the seed at <<>>, which public_key's signing then takes for the key; set it
%% to undefined so the expanded key signs (OTP 28.4.2 to 29.0.6).
ca_key() ->
    {Pub, Priv} = public_key:generate_key(mldsa87),
    {Pub, Priv#'ML-DSAPrivateKey'{seed = undefined}}.

cert(Subject, PublicKey, IssuerSubject, IssuerKey, IsCa) ->
    Tbs = #'OTPTBSCertificate'{
             version = v3,
             serialNumber = rand:uniform(1 bsl 60),
             signature = #'SignatureAlgorithm'{algorithm = ?'id-ml-dsa-87', parameters = asn1_NOVALUE},
             issuer = IssuerSubject,
             validity = #'Validity'{notBefore = {utcTime, "230101000000Z"}, notAfter = utc_after(365 * ?DAY)},
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
    Now = erlang:system_time(millisecond),
    {{Y, Mo, D}, {H, Mi, S}} = calendar:system_time_to_universal_time(Now + Ms, millisecond),
    {utcTime, lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w~2..0wZ", [Y rem 100, Mo, D, H, Mi, S]))}.

authorize(Adv, Trust) ->
    {ok, Verified} = macula_record:verify(macula_record:encode(Adv), pq_pure),
    macula_record:verify_authorization(Verified, Trust, erlang:system_time(millisecond)).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
