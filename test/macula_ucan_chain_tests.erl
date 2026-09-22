%% EUnit tests for delegation chains in macula_ucan (plan decision D7, WP 1.4 U2).
%%
%% A caller presents a token it was delegated, and the proof tokens that carry the delegation from the issuer the
%% policy requires down to that token. The proofs travel in the request's caller-signed `proofs' set and reach
%% authorize/3 in its context, each found by the SHA-384 content id the child's `prf' names, never by position.
%%
%% Every check here is one of D7's rules: a proof's aud is the node_id of the child's issuer key, the chain roots at
%% the policy's issuer, each token's capability is covered by the one it proves from, `can' is equal at every step,
%% every token names the same realm, and a procedure name has an org namespace.
-module(macula_ucan_chain_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<"io.example">>).
-define(ORG, <<"acme">>).
-define(PROCEDURE, <<"acme/api.users.get">>).
-define(CAN, <<"call">>).
-define(REALM_GRANT, <<"mri:realm:io.example">>).
-define(ORG_GRANT, <<"mri:org:io.example/acme">>).
-define(PROC_GRANT, <<"mri:proc:io.example/acme/api.users.get">>).

%%------------------------------------------------------------------
%% A proof's id
%%------------------------------------------------------------------

%% A proof is named by the lowercase hex of the SHA-384 of the parent token's bytes AS THEY TRAVEL: the three
%% base64url parts and their dots. A token re-encoded on the way, even to the same claims, has another id and so
%% proves nothing.
proof_id_is_the_hex_sha384_of_the_token_as_it_travels_test() ->
    #{realm_key := RealmKey, org_id := OrgId} = parties(),
    Token = mint(RealmKey, OrgId, ?ORG_GRANT, #{}),
    ?assertEqual(binary:encode_hex(crypto:hash(sha384, Token), lowercase), macula_ucan:proof_id(Token)),
    ?assertEqual(96, byte_size(macula_ucan:proof_id(Token))),
    ?assertNotEqual(macula_ucan:proof_id(Token), macula_ucan:proof_id(re_encoded(Token))).

%%------------------------------------------------------------------
%% A chain that authorizes
%%------------------------------------------------------------------

%% The realm key delegates an org to an org key, which delegates one procedure of that org to the caller. The caller
%% presents its own token and the org's as a proof.
a_chain_of_two_authorizes_the_caller_test() ->
    #{world := World} = chain(#{}),
    ?assertMatch({ok, #{<<"cap">> := [_]}}, authorize(World)).

%% A token with no prf still authorizes when its own issuer is the policy's, as it did before chains existed.
a_single_token_still_authorizes_test() ->
    #{realm_key := RealmKey, caller_id := CallerId} = Parties = parties(),
    Token = mint(RealmKey, CallerId, ?REALM_GRANT, #{}),
    ?assertMatch({ok, _}, authorize(world(Parties, Token, #{}))).

%%------------------------------------------------------------------
%% The proofs a chain names
%%------------------------------------------------------------------

%% A prf naming a proof that did not travel is refused: the chain cannot be walked.
a_prf_without_its_proof_is_refused_test() ->
    #{world := World} = chain(#{}),
    ?assertEqual({error, missing_proof}, authorize(World#{proofs := #{}})).

%% Every proof that travels must be referenced from the chain. An extra one is the caller sending what nothing reads,
%% and D7 refuses it rather than ignoring it.
an_unreferenced_proof_is_refused_test() ->
    #{world := World, realm_key := RealmKey, caller_id := CallerId} = chain(#{}),
    Extra = mint(RealmKey, CallerId, ?REALM_GRANT, #{}),
    Proofs = maps:get(proofs, World),
    ?assertEqual({error, unreferenced_proof},
                 authorize(World#{proofs := Proofs#{macula_ucan:proof_id(Extra) => Extra}})).

%% A proof authorizes the token it proves only when its audience IS that token's issuer, by node_id.
a_proof_for_another_audience_is_refused_test() ->
    #{world := World} = chain(#{parent_audience => macula_test_identity:node_id()}),
    ?assertEqual({error, not_the_delegate}, authorize(World)).

%% The chain must root at the issuer the policy names: another realm key's chain proves nothing here.
a_chain_rooted_elsewhere_is_refused_test() ->
    #{world := World, realm_key := Other} = chain(#{}),
    Policy = {realm_member_required, macula_node_keys:key_id(key(realm)), ?CAN},
    _ = Other,
    ?assertEqual({error, not_the_issuer}, authorize(World, Policy)).

%% A token names at most one parent: a chain, not a graph (D7).
a_token_naming_two_parents_is_refused_test() ->
    #{world := World, proof := Proof} = chain(#{}),
    #{realm_key := RealmKey, org_key := OrgKey, org_id := OrgId, caller_id := CallerId} = maps:get(parties, World),
    Second = mint(RealmKey, OrgId, ?ORG_GRANT, #{nnc => <<"second">>}),
    Leaf = mint(OrgKey, CallerId, ?PROC_GRANT,
                #{prf => [macula_ucan:proof_id(Proof), macula_ucan:proof_id(Second)]}),
    Proofs = #{macula_ucan:proof_id(Proof) => Proof, macula_ucan:proof_id(Second) => Second},
    ?assertEqual({error, chain_not_linear}, authorize(World#{token := Leaf, proofs := Proofs})).

%% Every link is checked in its own right: an expired proof ends the chain, whatever the leaf says.
an_expired_proof_is_refused_test() ->
    #{world := World} = chain(#{parent_exp => now_s() - 1}),
    ?assertEqual({error, expired}, authorize(World)).

%%------------------------------------------------------------------
%% Narrowing: the matrix of D7, one case per row
%%------------------------------------------------------------------

%% A child cannot grant what its parent did not: an org key handed one org cannot grant the realm.
a_child_granting_its_realm_from_an_org_proof_is_refused_test() ->
    #{world := World} = chain(#{child_grant => ?REALM_GRANT}),
    ?assertEqual({error, grants_more_than_proof}, authorize(World)).

%% Nor another org's procedure.
a_child_granting_another_org_is_refused_test() ->
    #{world := World} = chain(#{child_grant => <<"mri:proc:io.example/other/api.users.get">>,
                                procedure => <<"other/api.users.get">>}),
    ?assertEqual({error, grants_more_than_proof}, authorize(World)).

%% A realm grant covers an org and a procedure of that realm, and an org grant covers its own procedures.
the_narrowing_matrix_covers_what_it_says_test_() ->
    [{lists:flatten(io_lib:format("~s covers ~s", [Parent, Child])),
      ?_assertEqual(Covered, macula_ucan:covers(Parent, Child))}
     || {Parent, Child, Covered} <-
            [{?REALM_GRANT, ?ORG_GRANT, true},
             {?REALM_GRANT, ?PROC_GRANT, true},
             {?REALM_GRANT, ?REALM_GRANT, true},
             {?REALM_GRANT, <<"mri:proc:io.other/acme/api.users.get">>, false},
             {?ORG_GRANT, ?PROC_GRANT, true},
             {?ORG_GRANT, ?ORG_GRANT, true},
             {?ORG_GRANT, <<"mri:proc:io.example/other/api.users.get">>, false},
             {?ORG_GRANT, ?REALM_GRANT, false},
             {?PROC_GRANT, ?PROC_GRANT, true},
             {?PROC_GRANT, <<"mri:proc:io.example/acme/api.users.set">>, false},
             {?PROC_GRANT, <<"mri:proc:io.other/acme/api.users.get">>, false},
             {?PROC_GRANT, ?ORG_GRANT, false}]].

%% `can' is equal at every step: a delegate cannot widen what it may do with what it was given. The leaf keeps the
%% `can' the policy asks for, so what fails is the step above it, not the policy.
a_chain_that_changes_can_is_refused_test() ->
    #{world := World} = chain(#{parent_can => <<"publish">>}),
    ?assertEqual({error, can_changed}, authorize(World)).

%% Every token in a chain names the same realm.
a_chain_that_changes_realm_is_refused_test() ->
    #{world := World} = chain(#{child_grant => <<"mri:proc:io.other/acme/api.users.get">>}),
    ?assertEqual({error, wrong_realm}, authorize(World)).

%%------------------------------------------------------------------
%% What a grant may say at all
%%------------------------------------------------------------------

%% A procedure without an org namespace has no owner, so no policy authorizes it (D25).
a_procedure_without_an_org_namespace_is_refused_test() ->
    #{world := World} = chain(#{child_grant => <<"mri:proc:io.example/ping">>, procedure => <<"ping">>}),
    ?assertEqual({error, procedure_without_org}, authorize(World)).

%% A realm name is lowercase and compared byte for byte, because a realm id is a hash of those bytes: a name that
%% differs only by case is another realm, and one outside the form is refused rather than folded.
a_grant_whose_realm_name_is_not_canonical_is_refused_test() ->
    #{world := World} = chain(#{child_grant => <<"mri:proc:IO.Example/acme/api.users.get">>}),
    ?assertEqual({error, realm_name_not_canonical}, authorize(World)),
    ?assertEqual(false, macula_ucan:covers(<<"mri:realm:IO.Example">>, ?PROC_GRANT)).

%%------------------------------------------------------------------
%% The realm a grant names, against the realm the request carries
%%------------------------------------------------------------------

%% A realm id is SHA-256 over the normalised realm name (D7), so the grant's name is checked against the id the
%% request carries, with nothing looked up.
a_grants_realm_name_hashes_to_the_requests_realm_id_test() ->
    #{world := World} = chain(#{}),
    ?assertEqual(crypto:hash(sha256, ?REALM), maps:get(realm, World)),
    ?assertMatch({ok, _}, authorize(World)).

%% A request in another realm is refused, even with a chain that is sound in its own realm.
a_chain_for_another_realm_is_refused_test() ->
    #{world := World} = chain(#{}),
    ?assertEqual({error, wrong_realm}, authorize(World#{realm := crypto:hash(sha256, <<"io.other">>)})).

%% And a request whose realm id is the hash of a name differing only by case is another realm.
a_realm_id_of_a_name_differing_only_by_case_is_another_realm_test() ->
    #{world := World} = chain(#{}),
    ?assertEqual({error, wrong_realm}, authorize(World#{realm := crypto:hash(sha256, <<"IO.Example">>)})).

%% The all-zero realm has no name, so no grant covers a request in it (D7).
no_grant_covers_the_all_zero_realm_test() ->
    #{world := World} = chain(#{}),
    ?assertEqual({error, wrong_realm}, authorize(World#{realm := <<0:256>>})).

%% The capability has to cover the request: a grant for another procedure of the same org does not.
a_grant_for_another_procedure_is_refused_test() ->
    #{world := World} = chain(#{child_grant => <<"mri:proc:io.example/acme/api.users.set">>}),
    ?assertEqual({error, missing_capability}, authorize(World)).

%%------------------------------------------------------------------
%% The world under test
%%------------------------------------------------------------------

%% A realm key delegating an org to an org key, which delegates one procedure to the caller, and the context a
%% provider authorizes in. Opts changes one thing at a time.
chain(Opts) ->
    #{realm_key := RealmKey, org_key := OrgKey, org_id := OrgId, caller_id := CallerId} = Parties = parties(),
    ParentAudience = maps:get(parent_audience, Opts, OrgId),
    Proof = mint(RealmKey, ParentAudience, ?ORG_GRANT,
                 #{exp => maps:get(parent_exp, Opts, later()), can => maps:get(parent_can, Opts, ?CAN)}),
    Leaf = mint(OrgKey, CallerId, maps:get(child_grant, Opts, ?PROC_GRANT),
                #{can => maps:get(child_can, Opts, ?CAN), prf => [macula_ucan:proof_id(Proof)]}),
    World = (world(Parties, Leaf, #{macula_ucan:proof_id(Proof) => Proof}))#{
              procedure => maps:get(procedure, Opts, ?PROCEDURE)},
    #{world => World, proof => Proof, realm_key => RealmKey, caller_id => CallerId}.

world(#{realm_key := RealmKey, caller_id := CallerId} = Parties, Token, Proofs) ->
    #{parties => Parties, token => Token, proofs => Proofs, caller => CallerId, procedure => ?PROCEDURE,
      realm => crypto:hash(sha256, ?REALM),
      policy => {realm_member_required, macula_node_keys:key_id(RealmKey), ?CAN}}.

authorize(World) ->
    authorize(World, maps:get(policy, World)).

authorize(#{token := Token, proofs := Proofs, caller := Caller, procedure := Procedure, realm := Realm}, Policy) ->
    macula_ucan:authorize(Token, Policy,
                          #{caller => Caller, profile => pq_pure, now => now_s(),
                            realm => Realm, procedure => Procedure, proofs => Proofs}).

parties() ->
    RealmKey = key(realm),
    OrgKey = key(org),
    %% A proof's audience is the node_id derived from the key that issues the token it proves (D7), whatever that
    %% key's purpose.
    #{realm_key => RealmKey, org_key => OrgKey,
      org_id => macula_node_keys:node_id(macula_node_keys:public_key(OrgKey), pq_pure),
      caller_id => macula_test_identity:node_id()}.

key(Purpose) ->
    {ok, Key} = macula_node_keys:generate(Purpose, pq_pure),
    Key.

mint(Key, Audience, With, Opts) ->
    macula_ucan:create(Key, Audience, [#{with => With, can => maps:get(can, Opts, ?CAN)}],
                       maps:merge(#{exp => later()}, maps:without([can], Opts))).

%% The same claims with a space after every comma: one JSON value, two byte strings.
re_encoded(Token) ->
    [Header, Payload, Signature] = binary:split(Token, <<".">>, [global]),
    Json = iolist_to_binary(json:encode(json:decode(base64url_decode(Payload)))),
    Spaced = binary:replace(Json, <<",">>, <<", ">>, [global]),
    true = Spaced =/= Json,
    #{} = json:decode(Spaced),
    <<Header/binary, ".", (base64url(Spaced))/binary, ".", Signature/binary>>.

base64url(Bytes) -> base64:encode(Bytes, #{mode => urlsafe, padding => false}).
base64url_decode(Text) -> base64:decode(Text, #{mode => urlsafe, padding => false}).

now_s() -> erlang:system_time(second).
later() -> now_s() + 3600.
