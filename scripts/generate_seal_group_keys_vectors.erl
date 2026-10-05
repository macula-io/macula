%% Writes test/vectors/e2e_seal_v1_group_keys.json: the sealed-group vectors (test/vectors/E2E_SEAL_V1.md, "Sealed
%% groups"). The group_keys_v1 CALL and RESULT plaintexts are deterministic, and macula_seal_group_keys_vectors_tests
%% re-encodes them exactly. The UCANs are signed once: ML-DSA-87 signing is hedged and pq_hybrid's RSA-PSS half is
%% randomized, so a second run mints other tokens with the same verdicts, and the committed file is the vector. Run
%% through scripts/generate-seal-group-keys-vectors.sh with the compiled tree on the code path.
Now = 1790000000,
NowMs = Now * 1000,
Hour = 3600,
RealmName = <<"io.macula">>,
Realm = crypto:hash(sha256, RealmName),
Org = <<"acme">>,
Prefix = <<RealmName/binary, "/", Org/binary, "/chat">>,
Procedure = <<Org/binary, "/group_keys_v1">>,
Can = <<"group_keys">>,
Hex = fun(Bin) -> binary:encode_hex(Bin, lowercase) end,
Minute = 60000,
Rotate = 15 * Minute,
%% Two contiguous epochs, as a pull inside the first's ahead window returns them. Fixed bytes: these vectors pin the
%% encoding, and the epoch keys and ids are random in use.
Epoch = fun(N, IssuedAt) ->
    #{id => binary:copy(<<N>>, 8), key => binary:copy(<<(16#A0 + N)>>, 32), issued_at => IssuedAt,
      publish_until => IssuedAt + Rotate, accept_until => IssuedAt + Rotate + 65 * Minute}
end,
Epochs = [Epoch(1, NowMs), Epoch(2, NowMs + Rotate)],
Plain = fun(Payload) -> {ok, P} = macula_frame:payload_plain(Payload), Hex(P) end,
Call = fun(EpochField) -> Plain(#{{text, <<"prefix">>} => {text, Prefix}, {text, <<"epoch">>} => EpochField}) end,
EpochJson = fun(#{id := Id, key := Key, issued_at := I, publish_until := P, accept_until := A}) ->
    #{<<"id">> => Hex(Id), <<"key">> => Hex(Key), <<"issued_at">> => I, <<"publish_until">> => P,
      <<"accept_until">> => A}
end,
Payloads = #{
    <<"call_current">> => #{<<"prefix">> => Prefix, <<"epoch">> => <<"current">>,
                            <<"cbor">> => Call({text, <<"current">>})},
    <<"call_by_id">> => #{<<"prefix">> => Prefix, <<"epoch_id">> => Hex(maps:get(id, hd(Epochs))),
                          <<"cbor">> => Call(maps:get(id, hd(Epochs)))},
    <<"result">> => #{<<"prefix">> => Prefix, <<"policy">> => <<"required">>,
                      <<"epochs">> => [EpochJson(E) || E <- Epochs],
                      <<"cbor">> => Plain(#{prefix => {text, Prefix}, policy => {text, <<"required">>},
                                            epochs => Epochs})}},
Verdict = fun({ok, _Claims}) -> <<"ok">>; ({error, Refusal}) -> atom_to_binary(Refusal) end,
Profile = fun(P) ->
    Key = fun(Purpose) -> {ok, K} = macula_node_keys:generate(Purpose, P), K end,
    [OrgKey, OtherOrgKey] = [Key(org) || _ <- [1, 2]],
    [Member, Stranger] = [Key(identity) || _ <- [1, 2]],
    Node = fun(K) -> {ok, Id} = macula_node_keys:node_id(K), Id end,
    KeyId = fun(K) -> macula_node_keys:key_id(macula_node_keys:public_key(K), P) end,
    Mint = fun(Issuer, With, C, Opts) ->
        macula_ucan:create(Issuer, Node(Member), [#{with => With, can => C}], maps:merge(#{exp => Now + Hour}, Opts))
    end,
    ProcGrant = <<"mri:proc:", RealmName/binary, "/", Procedure/binary>>,
    OrgGrant = <<"mri:org:", RealmName/binary, "/", Org/binary>>,
    Policy = #{<<"kind">> => <<"realm_member_required">>, <<"key_id">> => Hex(KeyId(OrgKey)), <<"can">> => Can},
    Context = fun(Caller) ->
        #{<<"caller">> => Hex(Node(Caller)), <<"now">> => Now, <<"realm">> => Hex(Realm), <<"procedure">> => Procedure}
    end,
    Cases = [
        {<<"ok_procedure_grant">>, Mint(OrgKey, ProcGrant, Can, #{}), Context(Member)},
        {<<"ok_org_grant">>, Mint(OrgKey, OrgGrant, Can, #{}), Context(Member)},
        {<<"missing_capability">>, Mint(OrgKey, ProcGrant, <<"invoke">>, #{}), Context(Member)},
        {<<"not_the_issuer">>, Mint(OtherOrgKey, ProcGrant, Can, #{}), Context(Member)},
        {<<"not_the_audience">>, Mint(OrgKey, ProcGrant, Can, #{}), Context(Stranger)},
        {<<"expired">>, Mint(OrgKey, ProcGrant, Can, #{exp => Now - 1}), Context(Member)}
    ],
    Entries = [#{<<"name">> => Name, <<"token">> => Token, <<"policy">> => Policy, <<"context">> => Ctx,
                 <<"verdict">> => Verdict(macula_ucan:authorize(Token, {realm_member_required, KeyId(OrgKey), Can},
                                                                #{caller => binary:decode_hex(maps:get(<<"caller">>, Ctx)),
                                                                  profile => P, now => Now, proofs => #{},
                                                                  realm => Realm, procedure => Procedure}))}
               || {Name, Token, Ctx} <- Cases],
    [error({case_not_named_by_its_verdict, P, N, V})
     || #{<<"name">> := N, <<"verdict">> := V} <- Entries,
        not (V =:= <<"ok">> andalso binary:part(N, 0, 3) =:= <<"ok_">>), V =/= N],
    Keys = #{<<"org">> => #{<<"key_id">> => Hex(KeyId(OrgKey)),
                            <<"did_key">> => macula_ucan:did_key(macula_node_keys:public_key(OrgKey), P)},
             <<"member">> => #{<<"node_id">> => Hex(Node(Member))},
             <<"stranger">> => #{<<"node_id">> => Hex(Node(Stranger))}},
    #{<<"keys">> => Keys, <<"cases">> => Entries}
end,
Doc = #{<<"scheme">> => <<"sealed groups, macula 13.2 (docs/design/DESIGN_E2E_SEALED_PUBSUB.md)">>,
        <<"spec">> => <<"test/vectors/E2E_SEAL_V1.md">>,
        <<"generator">> => <<"scripts/generate-seal-group-keys-vectors.sh">>,
        <<"realm_name">> => RealmName,
        <<"payloads">> => Payloads,
        <<"refusals">> => [<<"not_a_member">>, <<"membership_unknown">>, <<"unknown_epoch">>, <<"epoch_expired">>,
                           <<"unknown_group">>],
        <<"profiles">> => #{<<"pq_pure">> => Profile(pq_pure), <<"pq_hybrid">> => Profile(pq_hybrid)}},
ok = file:write_file("test/vectors/e2e_seal_v1_group_keys.json", [json:format(Doc), "\n"]),
io:format("wrote test/vectors/e2e_seal_v1_group_keys.json~n"),
halt().
