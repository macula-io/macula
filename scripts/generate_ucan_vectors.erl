%% Writes test/vectors/ucan_v1.json: UCANs macula_ucan mints, each with the policy and context it is authorized
%% under and the verdict macula_ucan:authorize/3 reaches, in both crypto profiles; the proof id of each proof; and
%% D7's narrowing matrix. test/vectors/UCAN_V1.md says what an SDK must pass. Run through
%% scripts/generate-ucan-vectors.sh with the compiled tree on the code path.
%%
%% ML-DSA-87 signing is hedged and pq_hybrid's RSA-PSS half is randomized, so a second run mints other tokens with
%% the same verdicts: the committed file is the vector, generated once. macula_ucan_vectors_tests re-derives every
%% verdict from it on each run, so it cannot drift from macula_ucan.
Now = 1790000000,
Hour = 3600,
MaxLifetime = macula_ucan:max_lifetime(),
RealmName = <<"io.macula">>,
Realm = crypto:hash(sha256, RealmName),
Procedure = <<"acme/count_v1">>,
Can = <<"invoke">>,
Proc = fun(Name, P) -> <<"mri:proc:", Name/binary, "/", P/binary>> end,
Hex = fun(Bin) -> binary:encode_hex(Bin, lowercase) end,
Verdict = fun({ok, _Claims}) -> <<"ok">>; ({error, Refusal}) -> atom_to_binary(Refusal) end,
Tamper = fun(Token) ->
    [H, P, S] = binary:split(Token, <<".">>, [global]),
    Sig = base64:decode(S, #{mode => urlsafe, padding => false}),
    <<First, Rest/binary>> = Sig,
    Flipped = base64:encode(<<(First bxor 1), Rest/binary>>, #{mode => urlsafe, padding => false}),
    <<H/binary, ".", P/binary, ".", Flipped/binary>>
end,
Profile = fun(P) ->
    Key = fun(Purpose) -> {ok, K} = macula_node_keys:generate(Purpose, P), K end,
    [Root, OtherRoot, Alice, Bob, Mallory] = [Key(identity) || _ <- lists:seq(1, 5)],
    RealmKey = Key(realm),
    Other = case P of pq_pure -> pq_hybrid; pq_hybrid -> pq_pure end,
    {ok, Foreign} = macula_node_keys:generate(identity, Other),
    Node = fun(K) -> {ok, Id} = macula_node_keys:node_id(K), Id end,
    KeyId = fun(K) -> macula_node_keys:key_id(macula_node_keys:public_key(K), P) end,
    Mint = fun(Issuer, Audience, Caps, Opts) ->
        macula_ucan:create(Issuer, Node(Audience), [#{with => W, can => C} || {W, C} <- Caps],
                           maps:merge(#{exp => Now + Hour}, Opts))
    end,
    %% A token signed as create/4 signs one, over claims create/4 will not mint: without exp, or with an exp beyond
    %% the max lifetime, as another SDK or an older macula could have minted it.
    MintRaw = fun(Issuer, Audience, Caps, Extra) ->
        B64 = fun(Bin) -> base64:encode(Bin, #{mode => urlsafe, padding => false}) end,
        Header = #{<<"alg">> => case P of pq_pure -> <<"ML-DSA-87">>; pq_hybrid -> <<"ML-DSA-87-PS384">> end,
                   <<"typ">> => <<"JWT">>, <<"ucv">> => <<"0.10.0">>},
        Claims = Extra#{<<"iss">> => macula_ucan:did_key(macula_node_keys:public_key(Issuer), P),
                        <<"aud">> => Hex(Node(Audience)),
                        <<"cap">> => [#{<<"with">> => W, <<"can">> => C} || {W, C} <- Caps]},
        Input = <<(B64(iolist_to_binary(json:encode(Header))))/binary, ".",
                  (B64(iolist_to_binary(json:encode(Claims))))/binary>>,
        <<Input/binary, ".", (B64(macula_node_keys:sign(Input, Issuer)))/binary>>
    end,
    MintWithoutExp = fun(Issuer, Audience, Caps) -> MintRaw(Issuer, Audience, Caps, #{}) end,
    ProcCap = {Proc(RealmName, Procedure), Can},
    OrgCap = {<<"mri:org:", RealmName/binary, "/acme">>, Can},
    RealmCap = {<<"mri:realm:", RealmName/binary>>, Can},
    UcanRequired = fun(K) -> #{<<"kind">> => <<"ucan_required">>, <<"issuer">> => Hex(Node(K))} end,
    RequestContext = fun(Caller) ->
        #{<<"caller">> => Hex(Node(Caller)), <<"now">> => Now, <<"realm">> => Hex(Realm), <<"procedure">> => Procedure}
    end,
    %% A chain: Root grants Alice the org, Alice hands Bob the procedure.
    RootToAlice = Mint(Root, Alice, [OrgCap], #{}),
    AliceToBob = Mint(Alice, Bob, [ProcCap], #{prf => [macula_ucan:proof_id(RootToAlice)]}),
    RootToMallory = Mint(Root, Mallory, [OrgCap], #{}),
    OtherToAlice = Mint(OtherRoot, Alice, [OrgCap], #{}),
    RootToAliceOther = Mint(Root, Alice, [{Proc(RealmName, <<"acme/other_v1">>), Can}], #{}),
    RootToAliceRead = Mint(Root, Alice, [{element(1, OrgCap), <<"read">>}], #{}),
    RootToAliceInMs = MintRaw(Root, Alice, [OrgCap], #{<<"exp">> => (Now + Hour) * 1000}),
    Cases = [
        {<<"ok_procedure_grant">>, Mint(Root, Alice, [ProcCap], #{}), [], UcanRequired(Root), RequestContext(Alice)},
        {<<"ok_org_grant">>, Mint(Root, Alice, [OrgCap], #{}), [], UcanRequired(Root), RequestContext(Alice)},
        {<<"ok_realm_grant">>, Mint(Root, Alice, [RealmCap], #{}), [], UcanRequired(Root), RequestContext(Alice)},
        {<<"ok_with_optional_claims">>,
         Mint(Root, Alice, [ProcCap], #{nbf => Now - 60, nnc => <<"n-1">>, fct => #{<<"note">> => <<"x">>}}), [],
         UcanRequired(Root), RequestContext(Alice)},
        {<<"ok_delegated_chain">>, AliceToBob, [RootToAlice], UcanRequired(Root), RequestContext(Bob)},
        %% The capability that answers the request is the first in cap order that covers it, and it alone is carried
        %% up the chain: the org grant for beta, which the parent does not cover, is never tried.
        {<<"ok_delegated_chain_with_an_extra_capability">>,
         Mint(Alice, Bob, [{<<"mri:org:", RealmName/binary, "/beta">>, Can}, ProcCap],
              #{prf => [macula_ucan:proof_id(RootToAlice)]}),
         [RootToAlice], UcanRequired(Root), RequestContext(Bob)},
        {<<"ok_token_alone">>, Mint(Root, Alice, [ProcCap], #{}), [], UcanRequired(Root),
         #{<<"caller">> => Hex(Node(Alice)), <<"now">> => Now}},
        {<<"ok_realm_member">>, Mint(RealmKey, Alice, [RealmCap], #{}), [],
         #{<<"kind">> => <<"realm_member_required">>, <<"key_id">> => Hex(KeyId(RealmKey)), <<"can">> => Can},
         RequestContext(Alice)},
        {<<"malformed">>, <<"not.a.token">>, [], UcanRequired(Root), RequestContext(Alice)},
        %% exp is required: a token without one never lives for ever, it is malformed.
        {<<"malformed_missing_exp">>, MintWithoutExp(Root, Alice, [ProcCap]), [], UcanRequired(Root),
         RequestContext(Alice)},
        {<<"wrong_algorithm">>, Mint(Foreign, Alice, [ProcCap], #{}), [], UcanRequired(Root), RequestContext(Alice)},
        {<<"signature_invalid">>, Tamper(Mint(Root, Alice, [ProcCap], #{})), [], UcanRequired(Root),
         RequestContext(Alice)},
        {<<"not_the_issuer">>, Mint(OtherRoot, Alice, [ProcCap], #{}), [], UcanRequired(Root), RequestContext(Alice)},
        {<<"not_the_issuer_at_the_chain_root">>,
         Mint(Alice, Bob, [ProcCap], #{prf => [macula_ucan:proof_id(OtherToAlice)]}), [OtherToAlice],
         UcanRequired(Root), RequestContext(Bob)},
        {<<"not_the_audience">>, Mint(Root, Alice, [ProcCap], #{}), [], UcanRequired(Root), RequestContext(Mallory)},
        {<<"expired">>, Mint(Root, Alice, [ProcCap], #{exp => Now - 1}), [], UcanRequired(Root),
         RequestContext(Alice)},
        {<<"expired_at_exp">>, Mint(Root, Alice, [ProcCap], #{exp => Now}), [], UcanRequired(Root),
         RequestContext(Alice)},
        %% UCAN_V1 has no revocation: an exp more than the max lifetime (ten years of 365.25 days) past now is
        %% refused, beside expired in the check order, so a token minted with an exp in milliseconds authorizes
        %% nothing. An exp exactly at the bound is valid.
        {<<"ok_exp_at_max_lifetime">>, MintRaw(Root, Alice, [ProcCap], #{<<"exp">> => Now + MaxLifetime}), [],
         UcanRequired(Root), RequestContext(Alice)},
        {<<"exp_beyond_max_lifetime">>, MintRaw(Root, Alice, [ProcCap], #{<<"exp">> => Now + MaxLifetime + 1}), [],
         UcanRequired(Root), RequestContext(Alice)},
        {<<"exp_beyond_max_lifetime_in_milliseconds">>,
         MintRaw(Root, Alice, [ProcCap], #{<<"exp">> => (Now + Hour) * 1000}), [], UcanRequired(Root),
         RequestContext(Alice)},
        {<<"exp_beyond_max_lifetime_in_a_proof">>,
         Mint(Alice, Bob, [ProcCap], #{prf => [macula_ucan:proof_id(RootToAliceInMs)]}), [RootToAliceInMs],
         UcanRequired(Root), RequestContext(Bob)},
        %% The first refusal wins: the validity window is checked before the proofs are, so an expired token with a
        %% proof nothing names is expired, not unreferenced_proof.
        {<<"expired_before_unreferenced_proof">>, Mint(Root, Alice, [ProcCap], #{exp => Now - 1}), [RootToMallory],
         UcanRequired(Root), RequestContext(Alice)},
        {<<"not_yet_valid">>, Mint(Root, Alice, [ProcCap], #{nbf => Now + 60}), [], UcanRequired(Root),
         RequestContext(Alice)},
        {<<"missing_capability">>, Mint(Root, Alice, [{Proc(RealmName, <<"acme/other_v1">>), Can}], #{}), [],
         UcanRequired(Root), RequestContext(Alice)},
        {<<"missing_capability_wrong_can">>, Mint(RealmKey, Alice, [{element(1, RealmCap), <<"read">>}], #{}), [],
         #{<<"kind">> => <<"realm_member_required">>, <<"key_id">> => Hex(KeyId(RealmKey)), <<"can">> => Can},
         RequestContext(Alice)},
        {<<"wrong_realm">>, Mint(Root, Alice, [{Proc(<<"other.realm">>, Procedure), Can}], #{}), [],
         UcanRequired(Root), RequestContext(Alice)},
        {<<"realm_name_not_canonical">>, Mint(Root, Alice, [{Proc(<<"IO.macula">>, Procedure), Can}], #{}), [],
         UcanRequired(Root), RequestContext(Alice)},
        {<<"procedure_without_org">>, Mint(Root, Alice, [RealmCap], #{}), [], UcanRequired(Root),
         (RequestContext(Alice))#{<<"procedure">> => <<"count_v1">>}},
        {<<"missing_proof">>, AliceToBob, [], UcanRequired(Root), RequestContext(Bob)},
        {<<"unreferenced_proof">>, Mint(Root, Alice, [ProcCap], #{}), [RootToMallory], UcanRequired(Root),
         RequestContext(Alice)},
        {<<"not_the_delegate">>,
         Mint(Alice, Bob, [ProcCap], #{prf => [macula_ucan:proof_id(RootToMallory)]}), [RootToMallory],
         UcanRequired(Root), RequestContext(Bob)},
        {<<"chain_not_linear">>,
         Mint(Alice, Bob, [ProcCap], #{prf => [macula_ucan:proof_id(RootToAlice), macula_ucan:proof_id(RootToMallory)]}),
         [RootToAlice, RootToMallory], UcanRequired(Root), RequestContext(Bob)},
        {<<"grants_more_than_proof">>,
         Mint(Alice, Bob, [ProcCap], #{prf => [macula_ucan:proof_id(RootToAliceOther)]}), [RootToAliceOther],
         UcanRequired(Root), RequestContext(Bob)},
        {<<"can_changed">>,
         Mint(Alice, Bob, [ProcCap], #{prf => [macula_ucan:proof_id(RootToAliceRead)]}), [RootToAliceRead],
         UcanRequired(Root), RequestContext(Bob)}
    ],
    Policy = fun(#{<<"kind">> := <<"ucan_required">>, <<"issuer">> := I}) ->
                     {ucan_required, binary:decode_hex(I)};
                (#{<<"kind">> := <<"realm_member_required">>, <<"key_id">> := K, <<"can">> := C}) ->
                     {realm_member_required, binary:decode_hex(K), C}
             end,
    Context = fun(Ctx, Proofs) ->
        Base = #{caller => binary:decode_hex(maps:get(<<"caller">>, Ctx)), profile => P, now => maps:get(<<"now">>, Ctx),
                 proofs => maps:from_list([{macula_ucan:proof_id(Pr), Pr} || Pr <- Proofs])},
        case Ctx of
            #{<<"realm">> := R, <<"procedure">> := Pc} -> Base#{realm => binary:decode_hex(R), procedure => Pc};
            _ -> Base
        end
    end,
    Entries = [#{<<"name">> => Name, <<"token">> => Token, <<"proofs">> => Proofs,
                 <<"policy">> => Pol, <<"context">> => Ctx,
                 <<"verdict">> => Verdict(macula_ucan:authorize(Token, Policy(Pol), Context(Ctx, Proofs)))}
               || {Name, Token, Proofs, Pol, Ctx} <- Cases],
    Mismatched = [{N, V} || #{<<"name">> := N, <<"verdict">> := V} <- Entries,
                            V =/= <<"ok">> andalso binary:longest_common_prefix([N, V]) =/= byte_size(V)
                                andalso not lists:member(N, [<<"not_the_issuer_at_the_chain_root">>,
                                                               <<"missing_capability_wrong_can">>,
                                                               <<"expired_at_exp">>])],
    Mismatched =:= [] orelse error({verdict_not_the_case_named, P, Mismatched}),
    [error({case_not_ok, P, N, V}) || #{<<"name">> := <<"ok_", _/binary>> = N, <<"verdict">> := V} <- Entries,
                                      V =/= <<"ok">>],
    %% A realm key has no node_id: it is named by its key id.
    Described = fun(K) ->
        Base = #{<<"key_id">> => Hex(KeyId(K)), <<"did_key">> => macula_ucan:did_key(macula_node_keys:public_key(K), P)},
        case macula_node_keys:node_id(K) of
            {ok, Id} -> Base#{<<"node_id">> => Hex(Id)};
            {error, _} -> Base
        end
    end,
    Keys = maps:from_list([{Name, Described(K)}
                           || {Name, K} <- [{<<"root">>, Root}, {<<"other_root">>, OtherRoot}, {<<"alice">>, Alice},
                                            {<<"bob">>, Bob}, {<<"mallory">>, Mallory}, {<<"realm">>, RealmKey}]]),
    ProofIds = [#{<<"token">> => T, <<"proof_id">> => macula_ucan:proof_id(T)} || T <- [RootToAlice, RootToMallory]],
    #{<<"keys">> => Keys, <<"cases">> => Entries, <<"proof_ids">> => ProofIds}
end,
Grants = [<<"mri:realm:io.macula">>, <<"mri:org:io.macula/acme">>, <<"mri:proc:io.macula/acme/count_v1">>,
          <<"mri:proc:io.macula/acme/other_v1">>, <<"mri:org:io.macula/beta">>, <<"mri:proc:io.macula/beta/count_v1">>,
          <<"mri:realm:other.realm">>, <<"mri:proc:other.realm/acme/count_v1">>, <<"mri:realm:IO.macula">>,
          <<"mri:proc:io.macula/count_v1">>, <<"mri:org:io.macula/">>, <<"https://io.macula">>],
Covers = [#{<<"parent">> => A, <<"child">> => B, <<"covers">> => macula_ucan:covers(A, B)} || A <- Grants, B <- Grants],
Doc = #{<<"scheme">> => <<"ucan 0.10.0, macula 12 (D7)">>,
        <<"spec">> => <<"test/vectors/UCAN_V1.md">>,
        <<"generator">> => <<"scripts/generate-ucan-vectors.sh">>,
        <<"realm_name">> => RealmName,
        <<"profiles">> => #{<<"pq_pure">> => Profile(pq_pure), <<"pq_hybrid">> => Profile(pq_hybrid)},
        <<"covers">> => Covers,
        %% macula#87: the longest did:key text every SDK decodes, and one past it, refused before any decode.
        <<"did_key_length">> =>
            #{<<"max_encoded_chars">> => macula_ucan:max_did_key_encoded(),
              <<"over_bound">> => <<"did:key:z", (binary:copy(<<"2">>, macula_ucan:max_did_key_encoded() + 1))/binary>>,
              <<"verdict">> => <<"malformed">>}},
ok = file:write_file("test/vectors/ucan_v1.json", [json:format(Doc), "\n"]),
io:format("wrote test/vectors/ucan_v1.json~n"),
halt().
