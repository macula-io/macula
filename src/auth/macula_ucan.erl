%% @doc UCAN tokens in the node's crypto profile (plan decision D7), and a provider's authorization of one (D7 check 2).
%%
%% A token is a JWT, header.payload.signature, each part base64url without padding. The header names the profile's
%% algorithm: RFC 9964's ML-DSA-87 in pq_pure, and ML-DSA-87-PS384, the IETF LAMPS composite
%% id-MLDSA87-RSA4096-PSS-SHA512, in pq_hybrid (JOSE has no name for it). The signature is the issuer's node key's
%% signature over the header and payload as they are sent, made by macula_node_keys:sign/2, so ML-DSA-87 is
%% macula-mldsa and the composite's RSA-PSS half is OTP's.
%%
%% The payload's iss is a did:key for the issuer's key as carried (D13): multibase base58btc over the varint of a
%% multicodec and the key. pq_pure uses mldsa-87-pub, 0x1212; pq_hybrid has no multicodec yet and uses Macula's own
%% key type, 0x300087, from the private-use range. aud is the audience's node_id in lowercase hex, since a token is
%% presented by the node it names, inside a request that node signed (D7 check 2). Every token has an exp, in
%% seconds, at most max_lifetime/0 past now: UCAN_V1 has no revocation, so a token's exp is the only bound on its life.
%%
%% authorize/3 is the provider's check, after the request's own signature and target have verified: the header and
%% payload are verified over the bytes as received, never as re-encoded JSON, then the issuer, the audience, the
%% validity window and the capability.
%%
%% A token may be one the caller was delegated. Its `prf' names its parent by proof_id/1, the lowercase hex of the
%% SHA-384 of that parent's bytes as they travel, and the parents travel beside it in the request's caller-signed
%% `proofs' set, which reaches this module as the context's proofs map. The chain is walked to its root, which must
%% be the issuer the policy names, and every step is checked: the parent's audience is the node_id of the child's
%% issuer key, each link's own signature and validity window hold, `can' is equal at every step, and a child's
%% capability is covered by one of its parent's (covers/2, D7's narrowing matrix). A token names at most one parent,
%% and every proof that travelled must be referenced, so an unused one is refused rather than ignored.
%%
%% A capability's `with' is an MRI: `mri:realm:<realm>', `mri:org:<realm>/<org>', `mri:proc:<realm>/<procedure>' or
%% `mri:topic:<realm>/<path>', where a procedure's org is the text before its first `/'. A topic's grant is the topic
%% itself behind `mri:topic:', since a topic's first segment is its realm's name (macula_topic). A realm id is SHA-256
%% over the normalised realm name (D7), so the grant's name is checked against the realm id the request carries, with
%% nothing looked up.
%%
%% A procedure or topic grant whose last segment is `*' is a prefix: it covers every name below the segments before
%% the `*', never that name itself, and only whole segments, so `mri:proc:<realm>/acme/*' covers `acme/api.users.get'
%% and `mri:topic:<realm>/acme/orders/*' covers `<realm>/acme/orders/placed_v1', but `acme/ord/*' covers nothing of
%% `acme/orders/'. A `*' segment anywhere else, a prefix with an empty segment, and a prefix with no segment before
%% its `*' are not grants; a `*' inside a longer segment is a literal. A request names one procedure or one topic, never
%% a prefix.
-module(macula_ucan).

-export([create/4, authorize/3, proof_id/1, covers/2, did_key/2, carried_key/2, max_lifetime/0, max_did_key_encoded/0]).

-ifdef(TEST).
-export([base58btc_encode/1, base58btc_decode/1]).
-endif.

-export_type([capability/0, policy/0, issuer_id/0, refusal/0]).

-type capability() :: #{with := binary(), can := binary()}.
%% The issuer a policy requires: a node's node_id for ucan_required, a realm key's key id for realm_member_required.
-type issuer_id()  :: <<_:256>>.
-type policy()     :: {ucan_required, issuer_id()} | {realm_member_required, issuer_id(), binary()}.
-type refusal()    :: malformed | wrong_algorithm | signature_invalid | not_the_issuer | not_the_audience | expired
                    | exp_beyond_max_lifetime | not_yet_valid | missing_capability | missing_proof | unreferenced_proof
                    | not_the_delegate | chain_not_linear | grants_more_than_proof | can_changed | wrong_realm
                    | realm_name_not_canonical | procedure_without_org | topic_without_realm.
%% What a capability grants, parsed from its `with': a realm, an org of that realm, one procedure or topic of that
%% realm, or a prefix of procedures or of topics, held as the segments before its `*'.
-type grant()      :: {realm, binary()} | {org, binary(), binary()} | {proc, binary(), binary()}
                    | {proc_prefix, binary(), binary()} | {topic, binary(), binary()}
                    | {topic_prefix, binary(), binary()}.

%% The longest did:key text after its prefix (max_did_key_encoded/0).
-define(MAX_DID_KEY_ENCODED, 4400).
-define(TYP, <<"JWT">>).
-define(UCV, <<"0.10.0">>).
-define(MLDSA87_PUB, 16#1212).
-define(MLDSA87_RSA4096_PUB, 16#300087).
%% Ten years of 365.25 days, in seconds.
-define(MAX_LIFETIME, 315576000).
-define(BASE58, "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz").

%%------------------------------------------------------------------
%% Making a token
%%------------------------------------------------------------------

%% @doc The furthest past now, in seconds, a token's exp may lie: ten years of 365.25 days. An exp in milliseconds lies
%% far beyond it, so a token minted with one is refused rather than valid for tens of thousands of years.
-spec max_lifetime() -> pos_integer().
max_lifetime() -> ?MAX_LIFETIME.

%% @doc A token from the issuer's key, a key of a purpose that signs with the profile's identity algorithms, for the
%% audience's node_id, granting Capabilities until exp. nbf, nnc, fct and prf are optional.
%%
%% A window no verifier accepts is not minted: an exp more than max_lifetime/0 past now raises
%% {exp_beyond_max_lifetime, #{exp, now, at_most}}, and an nbf not before exp raises {window_never_opens, #{nbf, exp}}.
-spec create(macula_node_keys:node_key(), macula_node_keys:node_id(), [capability()],
             #{exp := non_neg_integer(), nbf => non_neg_integer(), nnc => binary(), fct => map(), prf => [binary()]}) ->
        binary().
create(#{purpose := Purpose, profile := Profile} = Key, <<_:256>> = Audience, Capabilities, #{exp := Exp} = Opts)
  when (Purpose =:= identity orelse Purpose =:= realm orelse Purpose =:= org orelse Purpose =:= foundation),
       is_list(Capabilities), is_integer(Exp), Exp >= 0 ->
    ok = window_opens(Exp, maps:get(nbf, Opts, none), erlang:system_time(second)),
    Header = #{<<"alg">> => alg(Profile), <<"typ">> => ?TYP, <<"ucv">> => ?UCV},
    Claims = maps:merge(optional_claims(Opts),
                        #{<<"iss">> => did_key(macula_node_keys:public_key(Key), Profile),
                          <<"aud">> => binary:encode_hex(Audience, lowercase),
                          <<"cap">> => [#{<<"with">> => With, <<"can">> => Can}
                                        || #{with := With, can := Can} <- Capabilities],
                          <<"exp">> => Exp}),
    Input = <<(base64url(json:encode(Header)))/binary, ".", (base64url(json:encode(Claims)))/binary>>,
    <<Input/binary, ".", (base64url(macula_node_keys:sign(Input, Key)))/binary>>.

window_opens(Exp, _Nbf, Now) when Exp > Now + ?MAX_LIFETIME ->
    error({exp_beyond_max_lifetime, #{exp => Exp, now => Now, at_most => Now + ?MAX_LIFETIME}});
window_opens(Exp, Nbf, _Now) when is_integer(Nbf), Nbf >= Exp ->
    error({window_never_opens, #{nbf => Nbf, exp => Exp}});
window_opens(_Exp, _Nbf, _Now) ->
    ok.

optional_claims(Opts) ->
    maps:from_list([{atom_to_binary(Name), Value} || Name := Value <- maps:with([nbf, nnc, fct, prf], Opts)]).

%%------------------------------------------------------------------
%% Authorizing a token
%%------------------------------------------------------------------

%% @doc Whether Token authorizes the verified caller under Policy, in the provider's profile at Now in seconds: its
%% claims when it does, or the first reason it does not. Never raises on the token.
%%
%% The context names the caller, the profile and the time, and, for a request, the realm id and the procedure or
%% topic it is for and the proofs that travelled with it. A topic is named in full, its realm's name first. A context
%% with no realm and no procedure or topic checks the token alone, as a gate that has no request in front of it.
-spec authorize(term(), policy(), #{caller := macula_node_keys:node_id(),
                                    profile := macula_crypto_profile:profile(), now := integer(),
                                    realm => <<_:256>>, procedure => binary(), topic => binary(),
                                    proofs => #{binary() => binary()}}) ->
        {ok, map()} | {error, refusal()}.
authorize(Token, Policy, #{caller := <<_:256>>, profile := Profile, now := Now} = Context) when is_integer(Now) ->
    first_refusal([fun(T) -> parsed(T) end,
                   fun(Parsed) -> algorithm_is(alg(Profile), Parsed) end,
                   fun(Parsed) -> signed_by_iss(Parsed, Profile) end,
                   fun(Checked) -> audience_is(Context, Checked) end,
                   fun(Checked) -> valid_at(Now, Checked) end,
                   fun(Checked) -> grants(Policy, Context, Checked) end,
                   fun(Checked) -> chained(Policy, Context, Checked) end,
                   fun(#{claims := Claims}) -> {ok, Claims} end], Token).

%%------------------------------------------------------------------
%% The chain
%%------------------------------------------------------------------

%% The token's own capability is granted by the token it proves from, up to the root, which must be the issuer the
%% policy names. A token with no proof IS the root. Every proof that travelled is used exactly once, so one left
%% over is refused: the caller sent what nothing reads.
chained(Policy, Context, #{granted := Granted} = Checked) ->
    walked(walk(Policy, Context, Checked, Granted, maps:get(proofs, Context, #{})), Checked).

walked({ok, Unused}, Checked) ->
    spent(maps:size(Unused) =:= 0, Checked);
walked({error, _} = Refusal, _Checked) ->
    Refusal.

spent(true, Checked) -> {ok, Checked};
spent(false, _Checked) -> {error, unreferenced_proof}.

walk(Policy, Context, Checked, Granted, Proofs) ->
    step(parents(Checked), Policy, Context, Checked, Granted, Proofs).

%% A token names at most one parent (D7): a chain, not a graph.
parents(#{claims := Claims}) ->
    proof_ids(maps:get(<<"prf">>, Claims, [])).

proof_ids([]) -> root;
proof_ids([Id]) when is_binary(Id) -> {parent, Id};
proof_ids(Ids) when is_list(Ids) -> {error, chain_not_linear};
proof_ids(_Other) -> {error, malformed}.

%% The root's issuer is the one the policy requires; a link's parent is looked up by the id its child names.
step(root, Policy, #{profile := Profile}, Checked, _Granted, Proofs) ->
    rooted(issued_by(Policy, Checked, Profile), Proofs);
step({parent, Id}, Policy, Context, Checked, Granted, Proofs) ->
    proved(maps:take(Id, Proofs), Policy, Context, Checked, Granted);
step({error, _} = Refusal, _Policy, _Context, _Checked, _Granted, _Proofs) ->
    Refusal.

rooted({ok, _Checked}, Proofs) -> {ok, Proofs};
rooted({error, _} = Refusal, _Proofs) -> Refusal.

proved(error, _Policy, _Context, _Checked, _Granted) ->
    {error, missing_proof};
proved({Proof, Rest}, Policy, Context, Checked, Granted) ->
    parent_link(chain_link(Proof, Context), Policy, Context, Checked, Granted, Rest).

%% A parent is checked as a token in its own right, then as this child's proof: its audience is the node_id of the
%% child's issuer key, its `can' is the child's, and its capability covers the child's (D7's narrowing).
parent_link({ok, Parent}, Policy, Context, Checked, Granted, Rest) ->
    delegated(first_refusal([fun(P) -> delegates_to(P, Checked, maps:get(profile, Context)) end,
                             fun(P) -> narrows_to(P, Granted) end], Parent),
              Policy, Context, Rest);
parent_link({error, _} = Refusal, _Policy, _Context, _Checked, _Granted, _Rest) ->
    Refusal.

delegated({ok, #{granted := ParentGrant} = Parent}, Policy, Context, Rest) ->
    walk(Policy, Context, Parent, ParentGrant, Rest);
delegated({error, _} = Refusal, _Policy, _Context, _Rest) ->
    Refusal.

%% One link of a chain, checked as a token: its parts, its algorithm, its signature over the bytes as received, and
%% its validity window. Its audience and capability are checked against the child that names it.
chain_link(Token, #{profile := Profile, now := Now}) ->
    first_refusal([fun(T) -> parsed(T) end,
                   fun(Parsed) -> algorithm_is(alg(Profile), Parsed) end,
                   fun(Parsed) -> signed_by_iss(Parsed, Profile) end,
                   fun(Checked) -> valid_at(Now, Checked) end], Token).

delegates_to(#{claims := #{<<"aud">> := Aud}} = Parent, #{issuer_key := ChildKey}, Profile) ->
    verdict(Aud =:= binary:encode_hex(macula_node_keys:node_id(ChildKey, Profile), lowercase),
            Parent, not_the_delegate).

%% Every capability the child hands on must be one this parent granted: same `can', and a `with' the parent's
%% covers. What the parent granted is what the step above it has to cover in turn.
narrows_to(#{claims := #{<<"cap">> := Caps}} = Parent, Children) ->
    narrowed([covering(Caps, Child) || Child <- Children], Caps, Parent).

covering(Caps, #{<<"with">> := ChildWith, <<"can">> := ChildCan}) ->
    first_covering([Cap || #{<<"with">> := With, <<"can">> := Can} = Cap <- Caps,
                           is_binary(With), Can =:= ChildCan, covers(With, ChildWith)],
                   Caps, ChildWith);
covering(_Caps, _Child) ->
    {error, malformed}.

first_covering([Cap | _Rest], _Caps, _ChildWith) -> {ok, Cap};
first_covering([], Caps, ChildWith) -> {error, why_not_narrowed(Caps, ChildWith)}.

narrowed(Covering, _Caps, Parent) ->
    all_covered([Refusal || {error, Refusal} <- Covering], [Cap || {ok, Cap} <- Covering], Parent).

all_covered([Refusal | _Rest], _Granted, _Parent) -> {error, Refusal};
all_covered([], Granted, Parent) -> {ok, Parent#{granted => Granted}}.

%% A parent that granted the same authority under another `can' changed it; one whose realm differs is another
%% realm's; otherwise it granted less than its child hands on.
why_not_narrowed(Caps, ChildWith) ->
    first_reason([reason_of(Cap, ChildWith) || Cap <- Caps]).

reason_of(#{<<"with">> := With, <<"can">> := _Can}, ChildWith) when is_binary(With) ->
    realm_or_narrowing(same_realm(With, ChildWith), covers(With, ChildWith));
reason_of(_Cap, _ChildWith) ->
    grants_more_than_proof.

realm_or_narrowing(false, _Covers) -> wrong_realm;
realm_or_narrowing(true, true) -> can_changed;
realm_or_narrowing(true, false) -> grants_more_than_proof.

first_reason([Reason | _Rest]) -> Reason;
first_reason([]) -> grants_more_than_proof.

same_realm(With, Other) ->
    realms_of(grant(With), grant(Other)).

realms_of({ok, Grant}, {ok, Other}) -> grant_realm(Grant) =:= grant_realm(Other);
realms_of(_Grant, _Other) -> false.

first_refusal([], Result) -> Result;
first_refusal([Step | Steps], Input) -> next(Step(Input), Steps).

next({ok, Output}, []) -> {ok, Output};
next({ok, Output}, Steps) -> first_refusal(Steps, Output);
next({error, _} = Refused, _Steps) -> Refused.

%% The three parts, their decoded header, claims and signature, and the header and payload as received, which the
%% signature covers.
parsed(Token) when is_binary(Token) ->
    parts(binary:split(Token, <<".">>, [global]));
parsed(_Token) ->
    {error, malformed}.

parts([Header, Payload, Signature]) ->
    decoded([json_object(Header), json_object(Payload), base64url_decode(Signature)],
            <<Header/binary, ".", Payload/binary>>);
parts(_Parts) ->
    {error, malformed}.

decoded([{ok, Header}, {ok, Claims}, {ok, Signature}], Input) when is_map(Header), is_map(Claims) ->
    {ok, #{header => Header, claims => Claims, signature => Signature, input => Input}};
decoded(_Decoded, _Input) ->
    {error, malformed}.

algorithm_is(Alg, #{header := #{<<"alg">> := Alg, <<"typ">> := ?TYP, <<"ucv">> := ?UCV}} = Parsed) ->
    {ok, Parsed};
algorithm_is(_Alg, #{header := #{<<"alg">> := Other}}) when is_binary(Other) ->
    {error, wrong_algorithm};
algorithm_is(_Alg, _Parsed) ->
    {error, malformed}.

%% Every claim has its shape before anything is verified, and the signature is checked before any claim is acted on.
signed_by_iss(#{claims := #{<<"iss">> := Iss, <<"aud">> := Aud, <<"cap">> := Caps, <<"exp">> := Exp}} = Parsed,
              Profile)
  when is_binary(Iss), is_binary(Aud), is_list(Caps), is_integer(Exp) ->
    verified(carried_key(Iss, Profile), Parsed, Profile);
signed_by_iss(_Parsed, _Profile) ->
    {error, malformed}.

verified({ok, Carried}, #{input := Input, signature := Signature} = Parsed, Profile) ->
    signature_verdict(macula_node_keys:verify(Input, Signature, Carried, Profile), Parsed#{issuer_key => Carried});
verified(error, _Parsed, _Profile) ->
    {error, malformed}.

signature_verdict(true, Checked) -> {ok, Checked};
signature_verdict(false, _Checked) -> {error, signature_invalid}.

issued_by({ucan_required, NodeId}, #{issuer_key := Carried} = Checked, Profile) ->
    verdict(macula_node_keys:node_id(Carried, Profile) =:= NodeId, Checked, not_the_issuer);
issued_by({realm_member_required, KeyId, _Can}, #{issuer_key := Carried} = Checked, Profile) ->
    verdict(macula_node_keys:key_id(Carried, Profile) =:= KeyId, Checked, not_the_issuer).

audience_is(#{caller := Caller}, #{claims := #{<<"aud">> := Aud}} = Checked) ->
    verdict(Aud =:= binary:encode_hex(Caller, lowercase), Checked, not_the_audience).

valid_at(Now, #{claims := #{<<"exp">> := Exp}}) when Now >= Exp ->
    {error, expired};
valid_at(Now, #{claims := #{<<"exp">> := Exp}}) when Exp > Now + ?MAX_LIFETIME ->
    {error, exp_beyond_max_lifetime};
valid_at(Now, #{claims := #{<<"nbf">> := Nbf}}) when is_integer(Nbf), Now < Nbf ->
    {error, not_yet_valid};
valid_at(_Now, #{claims := #{<<"nbf">> := Nbf}}) when not is_integer(Nbf) ->
    {error, malformed};
valid_at(_Now, Checked) ->
    {ok, Checked}.

%% The capability the request needs: one the token grants, whose `can' is the policy's where it names one, and whose
%% `with' covers the request's realm and procedure or topic. The realm id a request carries is SHA-256 over the
%% grant's realm name (D7), so a grant is checked against a request with nothing looked up. A context with no request
%% checks the `can' alone, as before. A request is for one procedure or one topic: a context naming both is malformed.
grants(_Policy, #{realm := <<_:256>>, procedure := _, topic := _}, _Checked) ->
    {error, malformed};
grants(Policy, #{realm := <<_:256>> = Realm, procedure := Procedure}, #{claims := #{<<"cap">> := Caps}} = Checked) ->
    requested(request_grant(Realm, Procedure), can_of(Policy), Caps, Checked);
grants(Policy, #{realm := <<_:256>> = Realm, topic := Topic}, #{claims := #{<<"cap">> := Caps}} = Checked) ->
    requested(request_topic(Realm, Topic), can_of(Policy), Caps, Checked);
grants({ucan_required, _NodeId}, _Context, #{claims := #{<<"cap">> := Caps}} = Checked) ->
    {ok, Checked#{granted => Caps}};
grants({realm_member_required, _KeyId, Can}, _Context, #{claims := #{<<"cap">> := Caps}} = Checked) ->
    granted([Cap || #{<<"can">> := Granted} = Cap <- Caps, Granted =:= Can], no_request, Checked).

can_of({ucan_required, _NodeId}) -> any;
can_of({realm_member_required, _KeyId, Can}) -> Can.

%% The grant a request asks for: one procedure or one topic of the realm whose id it carries. A procedure's realm
%% name comes from the token's own grants, since a realm id is a hash and cannot be read back into a name; a topic
%% names its realm itself.
requested({error, _} = Refusal, _Can, _Caps, _Checked) ->
    Refusal;
requested({ok, Realm, Target}, Can, Caps, Checked) ->
    granted([Cap || #{<<"with">> := With, <<"can">> := Granted} = Cap <- Caps,
                    is_binary(With), can_matches(Can, Granted),
                    realm_matches(With, Realm), covers(With, target_grant(Target, With))],
            Realm, Checked).

target_grant({procedure, Procedure}, With) -> procedure_grant(With, Procedure);
target_grant({topic, Topic}, _With) -> <<"mri:topic:", Topic/binary>>.

can_matches(any, _Granted) -> true;
can_matches(Can, Granted) -> Can =:= Granted.

%% The request's own grant, in the realm the `with' names: the name is checked against the request's realm id.
procedure_grant(With, Procedure) ->
    procedure_grant_of(grant(With), Procedure).

procedure_grant_of({ok, Grant}, Procedure) -> <<"mri:proc:", (grant_realm(Grant))/binary, "/", Procedure/binary>>;
procedure_grant_of({error, _}, _Procedure) -> <<>>.

realm_matches(With, Realm) ->
    realm_hash(grant(With)) =:= Realm.

realm_hash({ok, Grant}) -> crypto:hash(sha256, grant_realm(Grant));
realm_hash({error, _}) -> nomatch.

granted([Cap | _Rest], _Realm, Checked) ->
    {ok, Checked#{granted => [Cap]}};
granted([], Realm, #{claims := #{<<"cap">> := Caps}}) ->
    {error, why_not_granted(Caps, Realm)}.

%% Why no capability answered the request: a grant that is not well formed says so, a grant in another realm says
%% so, and anything else is simply not granted.
why_not_granted(Caps, Realm) ->
    first_reason([grant_refusal(grant(With), Realm) || #{<<"with">> := With} <- Caps, is_binary(With)]
                 ++ [missing_capability]).

grant_refusal({error, malformed}, _Realm) -> missing_capability;
grant_refusal({error, Refusal}, _Realm) -> Refusal;
grant_refusal({ok, Grant}, <<_:256>> = Realm) -> realm_verdict(realm_hash({ok, Grant}) =:= Realm);
grant_refusal({ok, _Grant}, no_request) -> missing_capability.

realm_verdict(true) -> missing_capability;
realm_verdict(false) -> wrong_realm.

%% The request's realm and procedure, refused when the procedure has no org namespace (D25), or names a prefix: a
%% request is for one procedure.
request_grant(Realm, Procedure) when is_binary(Procedure) ->
    request_org(macula_record:procedure_org(Procedure), name_form(Procedure), Realm, Procedure);
request_grant(_Realm, _Procedure) ->
    {error, malformed}.

request_org({org, _Org}, {exact, _Name}, Realm, Procedure) -> {ok, Realm, {procedure, Procedure}};
request_org({org, _Org}, _NotExact, _Realm, _Procedure) -> {error, malformed};
request_org(_None, _Form, _Realm, _Procedure) -> {error, procedure_without_org}.

%% The request's topic, named in full: refused when it has no realm segment and a path below it (a system topic, say),
%% when it names a prefix, and when its realm is not the one whose id the request carries.
request_topic(Realm, Topic) when is_binary(Topic) ->
    topic_request(binary:split(Topic, <<"/">>), Realm, Topic);
request_topic(_Realm, _Topic) ->
    {error, malformed}.

topic_request([_RealmName, Path], Realm, Topic) when Path =/= <<>> ->
    topic_in_realm(grant(<<"mri:topic:", Topic/binary>>), Realm, Topic);
topic_request(_NoPath, _Realm, _Topic) ->
    {error, topic_without_realm}.

topic_in_realm({ok, {topic, _Name, _Path} = Grant}, Realm, Topic) ->
    topic_realm_verdict(realm_hash({ok, Grant}) =:= Realm, Realm, Topic);
topic_in_realm({ok, {topic_prefix, _Name, _Base}}, _Realm, _Topic) ->
    {error, malformed};
topic_in_realm({error, _} = Refusal, _Realm, _Topic) ->
    Refusal.

topic_realm_verdict(true, Realm, Topic) -> {ok, Realm, {topic, Topic}};
topic_realm_verdict(false, _Realm, _Topic) -> {error, wrong_realm}.

verdict(true, Checked, _Refusal) -> {ok, Checked};
verdict(false, _Checked, Refusal) -> {error, Refusal}.

%%------------------------------------------------------------------
%% Proofs and grants
%%------------------------------------------------------------------

%% @doc The id a child's `prf' names a parent token by: the lowercase hex of the SHA-384 of that token's bytes as
%% they travel, the three base64url parts and their dots. A token re-encoded on the way has another id (D7, D24).
-spec proof_id(binary()) -> binary().
proof_id(Token) when is_binary(Token) ->
    binary:encode_hex(crypto:hash(sha384, Token), lowercase).

%% @doc Whether a grant covers another grant or a request, by D7's narrowing matrix: a realm grant covers its realm,
%% an org grant covers that org, its procedures and their prefixes, a prefix covers every name and prefix below it
%% and itself, and a procedure or topic grant covers only itself. A procedure grant never covers a topic, nor a topic
%% grant a procedure, and an org grant covers no topic: a topic's org slot carries macula_topic's tier sentinels, so
%% topics are granted by realm, topic or topic prefix. False for a grant that is not an MRI of the four forms, whose
%% realm name is not canonical, whose procedure has no org namespace, or whose `*' is not a prefix's last segment.
-spec covers(binary(), binary()) -> boolean().
covers(Parent, Child) ->
    covered(grant(Parent), grant(Child)).

covered({ok, {realm, Realm}}, {ok, Grant}) ->
    Realm =:= grant_realm(Grant);
covered({ok, {org, Realm, Org}}, {ok, {org, Realm, Org}}) ->
    true;
covered({ok, {org, Realm, Org}}, {ok, {proc, Realm, Procedure}}) ->
    {org, Org} =:= macula_record:procedure_org(Procedure);
covered({ok, {org, Realm, Org}}, {ok, {proc_prefix, Realm, Base}}) ->
    hd(binary:split(Base, <<"/">>)) =:= Org;
covered({ok, {proc, Realm, Procedure}}, {ok, {proc, Realm, Procedure}}) ->
    true;
covered({ok, {proc_prefix, Realm, Base}}, {ok, {proc, Realm, Procedure}}) ->
    below(Base, Procedure);
covered({ok, {proc_prefix, Realm, Base}}, {ok, {proc_prefix, Realm, Child}}) ->
    Base =:= Child orelse below(Base, Child);
covered({ok, {topic, Realm, Path}}, {ok, {topic, Realm, Path}}) ->
    true;
covered({ok, {topic_prefix, Realm, Base}}, {ok, {topic, Realm, Path}}) ->
    below(Base, Path);
covered({ok, {topic_prefix, Realm, Base}}, {ok, {topic_prefix, Realm, Child}}) ->
    Base =:= Child orelse below(Base, Child);
covered(_Parent, _Child) ->
    false.

%% A name below a prefix's segments: those segments, a `/', and at least one byte more. Matching whole segments is
%% what the `/' does: `acme/ord' is not below `acme/orders'.
below(Base, Name) when byte_size(Name) > byte_size(Base) + 1 ->
    Size = byte_size(Base),
    binary:part(Name, 0, Size + 1) =:= <<Base/binary, "/">>;
below(_Base, _Name) ->
    false.

grant_realm({realm, Realm}) -> Realm;
grant_realm({org, Realm, _Org}) -> Realm;
grant_realm({proc, Realm, _Procedure}) -> Realm;
grant_realm({proc_prefix, Realm, _Base}) -> Realm;
grant_realm({topic, Realm, _Path}) -> Realm;
grant_realm({topic_prefix, Realm, _Base}) -> Realm.

%% A `with' parsed into its grant, or why it is not one.
-spec grant(term()) -> {ok, grant()} | {error, refusal()}.
grant(<<"mri:realm:", Realm/binary>>) ->
    named_realm(Realm, fun(Name) -> {realm, Name} end);
grant(<<"mri:org:", Rest/binary>>) ->
    org_grant(binary:split(Rest, <<"/">>));
grant(<<"mri:proc:", Rest/binary>>) ->
    proc_grant(binary:split(Rest, <<"/">>));
grant(<<"mri:topic:", Rest/binary>>) ->
    topic_grant(binary:split(Rest, <<"/">>));
grant(_Other) ->
    {error, malformed}.

org_grant([Realm, Org]) when Org =/= <<>> ->
    named_realm(Realm, fun(Name) -> {org, Name, Org} end);
org_grant(_Split) ->
    {error, malformed}.

proc_grant([Realm, Procedure]) ->
    named_realm(Realm, fun(Name) -> with_org(macula_record:procedure_org(Procedure), Name, name_form(Procedure)) end);
proc_grant(_Split) ->
    {error, malformed}.

%% A prefix's org is its first segment, as an exact procedure's is: `acme/*' is in org acme, and `_/*' in none.
with_org(_Org, _Realm, malformed) -> {error, malformed};
with_org({org, _Org}, Realm, {exact, Procedure}) -> {proc, Realm, Procedure};
with_org({org, _Org}, Realm, {prefix, Base}) -> {proc_prefix, Realm, Base};
with_org(none, _Realm, _Form) -> {error, procedure_without_org};
with_org({error, malformed}, _Realm, _Form) -> {error, procedure_without_org}.

%% A topic grant names its realm and at least a path below it.
topic_grant([Realm, Path]) when Path =/= <<>> ->
    named_realm(Realm, fun(Name) -> topic_of(name_form(Path), Name) end);
topic_grant(_Split) ->
    {error, malformed}.

topic_of({exact, Path}, Realm) -> {topic, Realm, Path};
topic_of({prefix, Base}, Realm) -> {topic_prefix, Realm, Base};
topic_of(malformed, _Realm) -> {error, malformed}.

%% A procedure name or a topic path, exact or a prefix: a `*' is only ever a whole last segment, after at least one
%% segment, none of them empty. Elsewhere it is no grant at all, never a literal.
name_form(Name) ->
    prefix_or_exact(lists:reverse(binary:split(Name, <<"/">>, [global])), Name).

prefix_or_exact([<<"*">> | [_ | _] = Before], _Name) ->
    prefix_of(lists:all(fun prefix_segment/1, Before), lists:reverse(Before));
prefix_or_exact(Segments, Name) ->
    exact_of(lists:member(<<"*">>, Segments), Name).

prefix_of(true, Segments) -> {prefix, iolist_to_binary(lists:join(<<"/">>, Segments))};
prefix_of(false, _Segments) -> malformed.

prefix_segment(Segment) -> Segment =/= <<>> andalso Segment =/= <<"*">>.

exact_of(false, Name) -> {exact, Name};
exact_of(true, _Name) -> malformed.

%% A realm name is the bytes a realm id hashes (D7): at least one segment, segments separated by single dots, each
%% of a-z, 0-9, hyphen or underscore. Case is never folded, so a name outside the form is refused, not normalised.
named_realm(Realm, Grant) ->
    canonical_realm(canonical_realm_name(Realm), Realm, Grant).

canonical_realm(true, Realm, Grant) -> grant_or_refusal(Grant(Realm));
canonical_realm(false, _Realm, _Grant) -> {error, realm_name_not_canonical}.

grant_or_refusal({error, _} = Refusal) -> Refusal;
grant_or_refusal(Grant) -> {ok, Grant}.

canonical_realm_name(Realm) when is_binary(Realm), Realm =/= <<>> ->
    lists:all(fun canonical_segment/1, binary:split(Realm, <<".">>, [global]));
canonical_realm_name(_Realm) ->
    false.

canonical_segment(<<>>) ->
    false;
canonical_segment(Segment) ->
    lists:all(fun canonical_char/1, binary_to_list(Segment)).

canonical_char(Char) when Char >= $a, Char =< $z -> true;
canonical_char(Char) when Char >= $0, Char =< $9 -> true;
canonical_char($-) -> true;
canonical_char($_) -> true;
canonical_char(_Char) -> false.

%%------------------------------------------------------------------
%% did:key
%%------------------------------------------------------------------

%% @doc The did:key for a key as carried in a profile.
-spec did_key(binary(), macula_crypto_profile:profile()) -> binary().
did_key(Carried, Profile) when is_binary(Carried) ->
    <<"did:key:z", (base58btc_encode(<<(varint(codec(Profile)))/binary, Carried/binary>>))/binary>>.

%% @doc The longest base58btc text after "did:key:z" that a did:key for a carried key can have (macula#87): a
%% pq_hybrid key, the longest, is the 3-byte codec varint, the 2,592-byte ML-DSA-87 key and a DER RSA-4096 public key
%% of about 526 bytes, some 4,270 characters. Base58 decodes in time quadratic in its length, ahead of the signature
%% check, so a longer did:key is refused before it is decoded. Every SDK holds the same bound (ucan_v1.json,
%% did_key_length).
-spec max_did_key_encoded() -> pos_integer().
max_did_key_encoded() -> ?MAX_DID_KEY_ENCODED.

%% @doc The key a did:key carries, when it is a key in its one carried form for the profile (D13). A did:key longer
%% than max_did_key_encoded/0 is refused before it is decoded.
-spec carried_key(binary(), macula_crypto_profile:profile()) -> {ok, binary()} | error.
carried_key(<<"did:key:z", Encoded/binary>>, _Profile) when byte_size(Encoded) > ?MAX_DID_KEY_ENCODED ->
    error;
carried_key(<<"did:key:z", Encoded/binary>>, Profile) ->
    carried_key_of(varint(codec(Profile)), base58btc_decode(Encoded), Profile);
carried_key(_Other, _Profile) ->
    error.

carried_key_of(Prefix, Bytes, Profile) when byte_size(Bytes) > byte_size(Prefix) ->
    Size = byte_size(Prefix),
    <<Head:Size/binary, Carried/binary>> = Bytes,
    prefixed(Head =:= Prefix, Carried, Profile);
carried_key_of(_Prefix, _Bytes, _Profile) ->
    error.

prefixed(true, Carried, Profile) -> well_formed(macula_node_keys:carried_key_well_formed(Carried, Profile), Carried);
prefixed(false, _Carried, _Profile) -> error.

well_formed(true, Carried) -> {ok, Carried};
well_formed(false, _Carried) -> error.

codec(pq_pure) -> ?MLDSA87_PUB;
codec(pq_hybrid) -> ?MLDSA87_RSA4096_PUB.

%% An unsigned LEB128 varint, as multicodec prefixes are written.
varint(N) when N < 128 -> <<N>>;
varint(N) -> <<(128 bor (N band 127)), (varint(N bsr 7))/binary>>.

%%------------------------------------------------------------------
%% Encodings
%%------------------------------------------------------------------

%% Base58 with the Bitcoin alphabet, as multibase's base58btc: each leading zero byte is a leading 1.
base58btc_encode(Bytes) ->
    Zeros = leading_zeros(Bytes),
    <<_:Zeros/binary, Rest/binary>> = Bytes,
    list_to_binary([lists:duplicate(Zeros, $1) | base58_digits(binary:decode_unsigned(<<0, Rest/binary>>), [])]).

leading_zeros(<<0, Rest/binary>>) -> 1 + leading_zeros(Rest);
leading_zeros(_Bytes) -> 0.

base58_digits(0, Digits) -> Digits;
base58_digits(N, Digits) -> base58_digits(N div 58, [lists:nth(N rem 58 + 1, ?BASE58) | Digits]).

%% The bytes a base58btc text encodes, or an empty binary for one that holds a character outside the alphabet, which
%% no carried key's did:key decodes to.
base58btc_decode(Text) ->
    Ones = leading_ones(Text),
    <<_:Ones/binary, Rest/binary>> = Text,
    base58_bytes(Ones, base58_value(Rest, 0)).

leading_ones(<<$1, Rest/binary>>) -> 1 + leading_ones(Rest);
leading_ones(_Text) -> 0.

base58_value(<<>>, N) -> N;
base58_value(<<C, Rest/binary>>, N) -> base58_digit(binary:match(<<?BASE58>>, <<C>>), Rest, N).

base58_digit(nomatch, _Rest, _N) -> invalid;
base58_digit({Index, 1}, Rest, N) -> base58_value(Rest, N * 58 + Index).

base58_bytes(_Ones, invalid) -> <<>>;
base58_bytes(Ones, 0) -> binary:copy(<<0>>, Ones);
base58_bytes(Ones, N) -> <<(binary:copy(<<0>>, Ones))/binary, (binary:encode_unsigned(N))/binary>>.

alg(pq_pure) -> <<"ML-DSA-87">>;
alg(pq_hybrid) -> <<"ML-DSA-87-PS384">>.

base64url(Data) ->
    base64:encode(iolist_to_binary(Data), #{mode => urlsafe, padding => false}).

base64url_decode(Text) ->
    try base64:decode(Text, #{mode => urlsafe, padding => false}) of
        Bytes -> {ok, Bytes}
    catch
        error:_ -> error
    end.

json_object(Segment) ->
    json_of(base64url_decode(Segment)).

json_of({ok, Bytes}) ->
    try json:decode(Bytes) of
        Object when is_map(Object) -> {ok, Object};
        _NotAnObject -> error
    catch
        error:_ -> error
    end;
json_of(error) ->
    error.
