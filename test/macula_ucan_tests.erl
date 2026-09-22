%% EUnit tests for macula_ucan: UCAN tokens signed by a node key in the node's crypto profile (plan decision D7), and
%% the provider's authorization of a token a verified caller presents (D7 check 2).
-module(macula_ucan_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(CAN, <<"member/email-verified">>).
-define(CAP, #{with => <<"mri:realm:io.example">>, can => ?CAN}).

%%------------------------------------------------------------------
%% The token
%%------------------------------------------------------------------

%% A US token is a JWT whose alg is RFC 9964's ML-DSA-87, whose iss is a did:key for the issuer's key and whose aud is
%% the audience's node_id.
us_token_names_its_algorithm_issuer_and_audience_test() ->
    Issuer = identity(pq_pure),
    Audience = macula_test_identity:node_id(),
    Token = macula_ucan:create(Issuer, Audience, [?CAP], #{exp => later()}),
    {Header, Payload, _Signature} = segments(Token),
    ?assertEqual(#{<<"alg">> => <<"ML-DSA-87">>, <<"typ">> => <<"JWT">>, <<"ucv">> => <<"0.10.0">>}, Header),
    ?assertEqual(macula_ucan:did_key(macula_node_keys:public_key(Issuer), pq_pure), maps:get(<<"iss">>, Payload)),
    ?assertEqual(binary:encode_hex(Audience, lowercase), maps:get(<<"aud">>, Payload)),
    ?assertEqual([#{<<"with">> => <<"mri:realm:io.example">>, <<"can">> => ?CAN}], maps:get(<<"cap">>, Payload)).

%% A US did:key is multibase base58btc over the varint of multicodec mldsa-87-pub (0x1212) and the key.
us_did_key_is_the_mldsa_87_multicodec_over_the_key_test() ->
    Public = macula_node_keys:public_key(identity(pq_pure)),
    <<"did:key:z", Encoded/binary>> = DidKey = macula_ucan:did_key(Public, pq_pure),
    ?assertEqual(<<16#92, 16#24, Public/binary>>, macula_ucan:base58btc_decode(Encoded)),
    ?assertEqual({ok, Public}, macula_ucan:carried_key(DidKey, pq_pure)),
    ?assertEqual(error, macula_ucan:carried_key(DidKey, pq_hybrid)).

%% The multibase test vectors for base58btc, leading zero bytes included.
base58btc_matches_the_multibase_vectors_test_() ->
    [?_assertEqual(Encoded, macula_ucan:base58btc_encode(Bytes))
     || {Bytes, Encoded} <- [{<<"yes mani !">>, <<"7paNL19xttacUY">>},
                             {<<0, "yes mani !">>, <<"17paNL19xttacUY">>},
                             {<<0, 0, "yes mani !">>, <<"117paNL19xttacUY">>},
                             {<<>>, <<>>}]].

eu_token_is_the_lamps_composite_under_ml_dsa_87_ps384_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        Issuer = identity(pq_hybrid),
        Caller = macula_test_identity:node_id(),
        Token = macula_ucan:create(Issuer, Caller, [?CAP], #{exp => later()}),
        {Header, _Payload, _Signature} = segments(Token),
        ?assertEqual(<<"ML-DSA-87-PS384">>, maps:get(<<"alg">>, Header)),
        {ok, IssuerId} = macula_node_keys:node_id(Issuer),
        ?assertMatch({ok, #{<<"aud">> := _}},
                     macula_ucan:authorize(Token, {ucan_required, IssuerId}, context(Caller, pq_hybrid))),
        ?assertMatch({error, _},
                     macula_ucan:authorize(Token, {ucan_required, IssuerId}, context(Caller, pq_pure)))
    end}.

%%------------------------------------------------------------------
%% Authorization: ucan_required, a token issued by one known node
%%------------------------------------------------------------------

a_token_from_the_required_issuer_for_the_caller_is_accepted_test() ->
    {Issuer, IssuerId, Caller} = parties(),
    Token = macula_ucan:create(Issuer, Caller, [?CAP], #{exp => later()}),
    ?assertMatch({ok, #{<<"cap">> := [_]}}, authorize(Token, {ucan_required, IssuerId}, Caller)).

%% The signature is checked over the header and payload exactly as they arrived. The same JSON serialised another
%% way, with its keys in another order or with whitespace, is other bytes, and the signature does not cover them.
a_token_is_verified_over_its_bytes_as_received_test() ->
    {Issuer, IssuerId, Caller} = parties(),
    Token = macula_ucan:create(Issuer, Caller, [?CAP], #{exp => later()}),
    [Header, Payload, Signature] = binary:split(Token, <<".">>, [global]),
    Claims = json:decode(base64url_decode(Payload)),
    Reordered = iolist_to_binary(["{", lists:join(",", [[json:encode(K), ":", json:encode(V)]
                                                         || {K, V} <- lists:reverse(lists:sort(maps:to_list(Claims)))]),
                                  "}"]),
    Spaced = iolist_to_binary([" ", base64url_decode(Payload)]),
    ?assertEqual(Claims, json:decode(Reordered)),
    ?assertNotEqual(base64url_decode(Payload), Reordered),
    [?assertEqual({error, signature_invalid},
                  authorize(<<Header/binary, ".", (base64url(Other))/binary, ".", Signature/binary>>,
                            {ucan_required, IssuerId}, Caller))
     || Other <- [Reordered, Spaced]],
    ?assertMatch({ok, _}, authorize(Token, {ucan_required, IssuerId}, Caller)).

a_token_for_another_audience_is_refused_test() ->
    {Issuer, IssuerId, Caller} = parties(),
    Token = macula_ucan:create(Issuer, macula_test_identity:node_id(), [?CAP], #{exp => later()}),
    ?assertEqual({error, not_the_audience}, authorize(Token, {ucan_required, IssuerId}, Caller)).

a_token_from_another_issuer_is_refused_test() ->
    {_Issuer, IssuerId, Caller} = parties(),
    Token = macula_ucan:create(identity(pq_pure), Caller, [?CAP], #{exp => later()}),
    ?assertEqual({error, not_the_issuer}, authorize(Token, {ucan_required, IssuerId}, Caller)).

an_expired_token_is_refused_test() ->
    {Issuer, IssuerId, Caller} = parties(),
    Token = macula_ucan:create(Issuer, Caller, [?CAP], #{exp => now_s() - 1}),
    ?assertEqual({error, expired}, authorize(Token, {ucan_required, IssuerId}, Caller)).

a_token_not_yet_valid_is_refused_test() ->
    {Issuer, IssuerId, Caller} = parties(),
    Token = macula_ucan:create(Issuer, Caller, [?CAP], #{exp => later(), nbf => now_s() + 600}),
    ?assertEqual({error, not_yet_valid}, authorize(Token, {ucan_required, IssuerId}, Caller)).

%% D7: every token has an exp. create/4 will not make one without it, and a token signed without one is refused.
a_token_without_exp_is_refused_test() ->
    {Issuer, IssuerId, Caller} = parties(),
    ?assertError(function_clause, macula_ucan:create(Issuer, Caller, [?CAP], #{})),
    Token = signed(Issuer, #{<<"alg">> => <<"ML-DSA-87">>, <<"typ">> => <<"JWT">>, <<"ucv">> => <<"0.10.0">>},
                   #{<<"iss">> => macula_ucan:did_key(macula_node_keys:public_key(Issuer), pq_pure),
                     <<"aud">> => binary:encode_hex(Caller, lowercase), <<"cap">> => []}),
    ?assertEqual({error, malformed}, authorize(Token, {ucan_required, IssuerId}, Caller)).

%% A token made the way 11.x made them, EdDSA over Ed25519, is refused for its alg.
an_eddsa_token_is_refused_test() ->
    {_Issuer, IssuerId, Caller} = parties(),
    {Public, Private} = crypto:generate_key(eddsa, ed25519),
    Input = signing_input(#{<<"alg">> => <<"EdDSA">>, <<"typ">> => <<"JWT">>, <<"ucv">> => <<"0.10.0">>},
                          #{<<"iss">> => binary:encode_hex(Public, lowercase),
                            <<"aud">> => binary:encode_hex(Caller, lowercase), <<"cap">> => [], <<"exp">> => later()}),
    Token = <<Input/binary, ".", (base64url(crypto:sign(eddsa, none, Input, [Private, ed25519])))/binary>>,
    ?assertEqual({error, wrong_algorithm}, authorize(Token, {ucan_required, IssuerId}, Caller)).

a_token_with_a_changed_signature_is_refused_test() ->
    {Issuer, IssuerId, Caller} = parties(),
    Token = macula_ucan:create(Issuer, Caller, [?CAP], #{exp => later()}),
    [Header, Payload, Signature] = binary:split(Token, <<".">>, [global]),
    <<First, Rest/binary>> = base64url_decode(Signature),
    Changed = <<Header/binary, ".", Payload/binary, ".", (base64url(<<(First bxor 1), Rest/binary>>))/binary>>,
    ?assertEqual({error, signature_invalid}, authorize(Changed, {ucan_required, IssuerId}, Caller)).

malformed_tokens_are_refused_without_raising_test_() ->
    {_Issuer, IssuerId, Caller} = parties(),
    [?_assertEqual({error, malformed}, authorize(Bad, {ucan_required, IssuerId}, Caller))
     || Bad <- [<<>>, <<"a.b">>, <<"a.b.c.d">>, <<"!!.??.**">>, <<"e30.e30.">>, not_a_binary]].

%%------------------------------------------------------------------
%% Authorization: realm_member_required, a membership token from a realm key
%%------------------------------------------------------------------

a_membership_token_from_the_realm_key_with_the_required_can_is_accepted_test() ->
    {RealmKey, RealmKeyId, Caller} = realm_parties(),
    Token = macula_ucan:create(RealmKey, Caller, [?CAP], #{exp => later()}),
    ?assertMatch({ok, _}, authorize(Token, {realm_member_required, RealmKeyId, ?CAN}, Caller)).

%% A realm mints membership tokens at more than one tier from one key, so the required can is checked too.
a_membership_token_at_another_tier_is_refused_test() ->
    {RealmKey, RealmKeyId, Caller} = realm_parties(),
    Token = macula_ucan:create(RealmKey, Caller, [#{with => <<"mri:realm:io.example">>, can => <<"member/device-verified">>}], #{exp => later()}),
    ?assertEqual({error, missing_capability}, authorize(Token, {realm_member_required, RealmKeyId, ?CAN}, Caller)).

a_membership_token_from_another_realm_key_is_refused_test() ->
    {_RealmKey, RealmKeyId, Caller} = realm_parties(),
    {ok, Other} = macula_node_keys:generate(realm, pq_pure),
    Token = macula_ucan:create(Other, Caller, [?CAP], #{exp => later()}),
    ?assertEqual({error, not_the_issuer}, authorize(Token, {realm_member_required, RealmKeyId, ?CAN}, Caller)).

%% Each policy names its issuer by one derivation of the issuer's key: realm_member_required by key id (D5's
%% MACULA-KEY-ID-V1), ucan_required by node_id (MACULA-NODE-ID-V1). The two differ for one key, so a membership policy
%% given the realm key's node_id in place of its key id accepts no token from that key.
issuers_are_named_by_the_id_their_policy_takes_test() ->
    {RealmKey, RealmKeyId, Caller} = realm_parties(),
    Token = macula_ucan:create(RealmKey, Caller, [?CAP], #{exp => later()}),
    NodeIdOfRealmKey = macula_node_keys:node_id(macula_node_keys:public_key(RealmKey), pq_pure),
    ?assertNotEqual(RealmKeyId, NodeIdOfRealmKey),
    ?assertEqual({error, not_the_issuer},
                 authorize(Token, {realm_member_required, NodeIdOfRealmKey, ?CAN}, Caller)),
    ?assertMatch({ok, _}, authorize(Token, {realm_member_required, RealmKeyId, ?CAN}, Caller)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

identity(Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Key.

parties() ->
    Issuer = identity(pq_pure),
    {ok, IssuerId} = macula_node_keys:node_id(Issuer),
    {Issuer, IssuerId, macula_test_identity:node_id()}.

realm_parties() ->
    {ok, RealmKey} = macula_node_keys:generate(realm, pq_pure),
    {RealmKey, macula_node_keys:key_id(RealmKey), macula_test_identity:node_id()}.

authorize(Token, Policy, Caller) ->
    macula_ucan:authorize(Token, Policy, context(Caller, pq_pure)).

context(Caller, Profile) ->
    #{caller => Caller, profile => Profile, now => now_s()}.

now_s() -> erlang:system_time(second).

later() -> now_s() + 3600.

segments(Token) ->
    [H, P, S] = binary:split(Token, <<".">>, [global]),
    {json:decode(base64url_decode(H)), json:decode(base64url_decode(P)), base64url_decode(S)}.

%% A token built here independently of macula_ucan: header and payload as JSON, base64url, signed by a node key.
signed(Key, Header, Payload) ->
    Input = signing_input(Header, Payload),
    <<Input/binary, ".", (base64url(macula_node_keys:sign(Input, Key)))/binary>>.

signing_input(Header, Payload) ->
    <<(base64url(iolist_to_binary(json:encode(Header))))/binary, ".",
      (base64url(iolist_to_binary(json:encode(Payload))))/binary>>.

base64url(Bytes) -> base64:encode(Bytes, #{mode => urlsafe, padding => false}).

base64url_decode(Text) -> base64:decode(Text, #{mode => urlsafe, padding => false}).
