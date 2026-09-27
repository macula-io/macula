%% Writes test/vectors/e2e_seal_v1_advertisements.json: signed procedure advertisements in both crypto profiles, each
%% with the verdict macula_record:verify/3 reaches on its wire bytes at the vector's clock (E2E design, amendment A1). A
%% keyed advertisement names the provider's KEM key as carried and its kem_key_id; the two travel only as a pair of a
%% key a profile carries and that key's id, and every other shape is refused. test/vectors/E2E_SEAL_V1.md says what an
%% SDK's record verifier must do with them. Run through scripts/generate-seal-advertisement-vectors.sh with the
%% compiled tree on the code path.
%%
%% sign/2 refuses a payload verify/3 would refuse, so the refused cases are signed here the way sign/2 signs: the
%% record's fields under the record label, with the provider's key. ML-DSA-87 signing is hedged and pq_hybrid's RSA-PSS
%% half is randomized, so a second run signs other bytes with the same verdicts: the committed file is the vector,
%% generated once. macula_seal_advertisement_vectors_tests re-derives every verdict from it on each run.
{ok, _} = application:ensure_all_started(crypto),
Label = <<"MACULA-PQ-RECORD-V1">>,
Hex = fun(Bin) -> binary:encode_hex(Bin, lowercase) end,
Realm = crypto:hash(sha256, <<"io.macula">>),
Station = crypto:hash(sha256, <<"station">>),
Procedure = <<"acme/sealed_echo_v1">>,
Verdict = fun({ok, _Record}) -> <<"accepted">>; ({error, Refusal}) -> atom_to_binary(Refusal) end,
Profile = fun(P) ->
    {ok, Provider} = macula_node_keys:generate(identity, P),
    {ok, NodeId} = macula_node_keys:node_id(Provider),
    {KemPublic, _KemPrivate} = macula_seal:generate_key(P),
    KemKey = macula_seal:key_as_carried(KemPublic),
    KemKeyId = macula_seal:key_id(KemKey),
    {OtherKemPublic, _} = macula_seal:generate_key(P),
    OtherKemKeyId = macula_seal:key_id(macula_seal:key_as_carried(OtherKemPublic)),
    Keyed = macula_record:procedure_advertisement(NodeId, Realm, Procedure, Station, #{kem_key => KemKey}),
    #{payload := KeyedPayload, created_at := Created} = Keyed,
    Now = Created + 1_000,
    %% Signed as sign/2 signs, over a payload sign/2 refuses.
    SignedAs = fun(Payload) ->
        #{type := Type, version := Version, expires_at := Expires} = Keyed,
        Fields = #{{text, <<"type">>} => Type, {text, <<"version">>} => Version, {text, <<"created_at">>} => Created,
                   {text, <<"expires_at">>} => Expires, {text, <<"payload">>} => Payload},
        macula_record:encode(macula_signed_object:sign(Label, Fields, Provider))
    end,
    Case = fun(Name, Why, Bytes) ->
        Result = macula_record:verify(Bytes, P, Now),
        Read = case Result of
            {ok, Record} ->
                #{kem_key := K, kem_key_id := Id} = macula_record:read_procedure_advertisement(Record),
                #{<<"kem_key">> => Hex(K), <<"kem_key_id">> => Hex(Id)};
            {error, _} ->
                #{}
        end,
        maps:merge(#{<<"name">> => Name, <<"why">> => Why, <<"record">> => Hex(Bytes),
                     <<"verdict">> => Verdict(Result)}, Read)
    end,
    %% The signing here is not what the refused cases are refused for: the keyed payload, signed here, is accepted.
    {ok, _} = macula_record:verify(SignedAs(KeyedPayload), P, Now),
    Without = fun(Name) -> maps:remove({text, Name}, KeyedPayload) end,
    With = fun(Name, Value) -> KeyedPayload#{{text, Name} => Value} end,
    ShortKey = binary:part(KemKey, 0, byte_size(KemKey) - 1),
    Cases = [
        Case(<<"keyed">>, <<"the key as carried and its own id">>,
             macula_record:encode(macula_record:sign(Keyed, Provider))),
        Case(<<"kem_key_id_of_another_key">>, <<"kem_key_id is another key's id">>,
             SignedAs(With(<<"kem_key_id">>, OtherKemKeyId))),
        Case(<<"kem_key_alone">>, <<"kem_key without kem_key_id">>, SignedAs(Without(<<"kem_key_id">>))),
        Case(<<"kem_key_id_alone">>, <<"kem_key_id without kem_key">>, SignedAs(Without(<<"kem_key">>))),
        Case(<<"kem_key_of_no_profile_size">>, <<"a key one byte short of this profile's, with its own id">>,
             SignedAs(KeyedPayload#{{text, <<"kem_key">>} => ShortKey,
                                    {text, <<"kem_key_id">>} => macula_seal:key_id(ShortKey)}))
    ],
    {atom_to_binary(P),
     #{<<"signer_public_key">> => Hex(macula_node_keys:public_key(Provider)),
       <<"signer_node_id">> => Hex(NodeId),
       <<"now_ms">> => Now,
       <<"kem_key_size">> => byte_size(KemKey),
       <<"cases">> => Cases}}
end,
Vectors = #{<<"scheme">> => 1,
            <<"spec">> => <<"test/vectors/E2E_SEAL_V1.md">>,
            <<"design">> => <<"plans/DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md, amendment A1">>,
            <<"generator">> => <<"scripts/generate-seal-advertisement-vectors.sh">>,
            <<"realm_id">> => Hex(Realm),
            <<"procedure">> => Procedure,
            <<"serving_station">> => Hex(Station),
            <<"profiles">> => maps:from_list([Profile(P) || P <- [pq_pure, pq_hybrid]])},
ok = file:write_file("test/vectors/e2e_seal_v1_advertisements.json",
                     [json:format(Vectors), "\n"]),
io:format("wrote test/vectors/e2e_seal_v1_advertisements.json~n"),
halt(0).
