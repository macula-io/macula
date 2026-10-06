%% The UCAN vectors (test/vectors/UCAN_V1.md): tokens macula_ucan minted once, in both crypto profiles, each with the
%% policy and context it is authorized under and the verdict every SDK must reach. The file is committed as
%% generated, since ML-DSA-87 and RSA-PSS signing are randomized; this module re-derives every verdict, proof id,
%% did:key and grant from it on each run, so the file can never drift from macula_ucan.
-module(macula_ucan_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

vectors_test_() ->
    Doc = vectors(),
    Profiles = maps:get(<<"profiles">>, Doc),
    [{"both profiles", ?_assertEqual([<<"pq_hybrid">>, <<"pq_pure">>], lists:sort(maps:keys(Profiles)))}]
    ++ [{case_name(Profile, C), fun() -> case_checked(profile(Profile), C) end}
        || Profile := P <- Profiles, C <- maps:get(<<"cases">>, P)]
    ++ [{binary_to_list(Profile) ++ " proof ids", fun() -> proof_ids_checked(maps:get(<<"proof_ids">>, P)) end}
        || Profile := P <- Profiles]
    ++ [{binary_to_list(Profile) ++ " keys", fun() -> keys_checked(profile(Profile), P) end}
        || Profile := P <- Profiles]
    ++ [{"covers", fun() -> covers_checked(maps:get(<<"covers">>, Doc)) end},
        {"every refusal is pinned", fun() -> refusals_pinned(Profiles) end}].

%% A profile by its name in the file, never through binary_to_existing_atom: whether that atom exists yet depends on
%% which modules happen to have loaded.
profile(<<"pq_pure">>) -> pq_pure;
profile(<<"pq_hybrid">>) -> pq_hybrid.

case_name(Profile, #{<<"name">> := Name}) ->
    binary_to_list(<<Profile/binary, " ", Name/binary>>).

%% The verdict macula_ucan reaches now is the one the file pins.
case_checked(Profile, #{<<"token">> := Token, <<"proofs">> := Proofs, <<"policy">> := Policy,
                        <<"context">> := Context, <<"verdict">> := Verdict}) ->
    ?assertEqual(Verdict, verdict(macula_ucan:authorize(Token, policy(Policy), context(Profile, Context, Proofs)))).

verdict({ok, _Claims}) -> <<"ok">>;
verdict({error, Refusal}) -> atom_to_binary(Refusal).

policy(#{<<"kind">> := <<"ucan_required">>, <<"issuer">> := Issuer}) ->
    {ucan_required, binary:decode_hex(Issuer)};
policy(#{<<"kind">> := <<"realm_member_required">>, <<"key_id">> := KeyId, <<"can">> := Can}) ->
    {realm_member_required, binary:decode_hex(KeyId), Can}.

%% A context names the caller and the time, and for a request its realm and procedure; the proofs that travelled
%% are keyed by proof id, as the station link keys them.
context(Profile, Context, Proofs) ->
    Base = #{caller => binary:decode_hex(maps:get(<<"caller">>, Context)), profile => Profile,
             now => maps:get(<<"now">>, Context),
             proofs => maps:from_list([{macula_ucan:proof_id(P), P} || P <- Proofs])},
    request(Context, Base).

request(#{<<"realm">> := Realm, <<"procedure">> := Procedure}, Base) ->
    Base#{realm => binary:decode_hex(Realm), procedure => Procedure};
request(_NoRequest, Base) ->
    Base.

proof_ids_checked(ProofIds) ->
    [?assertEqual(Id, macula_ucan:proof_id(Token)) || #{<<"token">> := Token, <<"proof_id">> := Id} <- ProofIds].

%% Each key's did:key carries the key it names: its key id, and its node_id where it has one, derive from the key
%% the did:key decodes to.
keys_checked(Profile, #{<<"keys">> := Keys}) ->
    [begin
         {ok, Carried} = macula_ucan:carried_key(DidKey, Profile),
         ?assertEqual(DidKey, macula_ucan:did_key(Carried, Profile)),
         ?assertEqual(binary:decode_hex(maps:get(<<"key_id">>, K)), macula_node_keys:key_id(Carried, Profile)),
         node_id_checked(maps:find(<<"node_id">>, K), Carried, Profile)
     end || _Name := #{<<"did_key">> := DidKey} = K <- Keys].

node_id_checked({ok, NodeId}, Carried, Profile) ->
    ?assertEqual(binary:decode_hex(NodeId), macula_node_keys:node_id(Carried, Profile));
node_id_checked(error, _Carried, _Profile) ->
    ok.

covers_checked(Covers) ->
    [?assertEqual({Parent, Child, Covers1}, {Parent, Child, macula_ucan:covers(Parent, Child)})
     || #{<<"parent">> := Parent, <<"child">> := Child, <<"covers">> := Covers1} <- Covers].

%% Every refusal macula_ucan:authorize/3 can reach is pinned in both profiles, so an SDK that passes the file has
%% met each one. The list is macula_ucan's refusal() type: a refusal added there is added here and given a case.
refusals_pinned(Profiles) ->
    Refusals = [malformed, wrong_algorithm, signature_invalid, not_the_issuer, not_the_audience, expired,
                exp_beyond_max_lifetime, not_yet_valid, missing_capability, missing_proof, unreferenced_proof, not_the_delegate,
                chain_not_linear, grants_more_than_proof, can_changed, wrong_realm, realm_name_not_canonical,
                procedure_without_org],
    [?assertEqual({Profile, []},
                  {Profile, [R || R <- Refusals,
                                  not lists:member(atom_to_binary(R),
                                                   [V || #{<<"verdict">> := V} <- maps:get(<<"cases">>, P)])]})
     || Profile := P <- Profiles].

vectors() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file() ->
    first_existing(["test/vectors/ucan_v1.json", "../../test/vectors/ucan_v1.json"]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", "ucan_v1.json"])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
