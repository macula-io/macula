%% The sealed-group vectors (test/vectors/E2E_SEAL_V1.md, "Sealed groups"): the `<org>/group_keys_v1' CALL and RESULT
%% plaintexts, byte for byte, so every SDK's distributor and member agree on them, and one org-issued `group_keys'
%% UCAN per profile with the verdict every SDK's distributor must reach under its advertise policy. The payloads are
%% deterministic and re-encoded here exactly; the UCANs are signed once, randomized, and their verdicts re-derived on
%% each run, so the file can never drift from macula.
-module(macula_seal_group_keys_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

vectors_test_() ->
    Doc = vectors(),
    #{<<"payloads">> := Payloads, <<"profiles">> := Profiles} = Doc,
    [{"a current pull's CALL is these bytes", fun() -> call_checked(maps:get(<<"call_current">>, Payloads)) end},
     {"a pull by id's CALL is these bytes", fun() -> call_checked(maps:get(<<"call_by_id">>, Payloads)) end},
     {"the RESULT is these bytes, as the distributor builds it", fun() -> result_checked(maps:get(<<"result">>, Payloads)) end},
     {"a member's keyring reads the RESULT's bytes", fun() -> result_read(maps:get(<<"result">>, Payloads)) end},
     {"the refusals are the closed set", ?_assertEqual(refusals(), maps:get(<<"refusals">>, Doc))},
     {"both profiles", ?_assertEqual([<<"pq_hybrid">>, <<"pq_pure">>], lists:sort(maps:keys(Profiles)))}]
    ++ [{binary_to_list(<<Profile/binary, " ", Name/binary>>), fun() -> grant_checked(profile(Profile), C) end}
        || Profile := #{<<"cases">> := Cases} <- Profiles, #{<<"name">> := Name} = C <- Cases].

vectors() ->
    {ok, Bin} = file:read_file(vector_file()),
    json:decode(Bin).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file() ->
    first_existing(["test/vectors/e2e_seal_v1_group_keys.json", "../../test/vectors/e2e_seal_v1_group_keys.json"]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", "e2e_seal_v1_group_keys.json"])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest);
first_existing([]) ->
    error(no_group_keys_vector_file).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).

profile(<<"pq_pure">>) -> pq_pure;
profile(<<"pq_hybrid">>) -> pq_hybrid.

refusals() ->
    [<<"not_a_member">>, <<"membership_unknown">>, <<"unknown_epoch">>, <<"epoch_expired">>, <<"unknown_group">>].

%% A member's pull, as macula_group_keyring sends it.
call_checked(#{<<"prefix">> := Prefix, <<"cbor">> := Cbor} = Call) ->
    Epoch = epoch_field(Call),
    {ok, Plain} = macula_frame:payload_plain(#{{text, <<"prefix">>} => {text, Prefix}, {text, <<"epoch">>} => Epoch}),
    ?assertEqual(Cbor, hex(Plain)).

epoch_field(#{<<"epoch">> := <<"current">>}) -> {text, <<"current">>};
epoch_field(#{<<"epoch_id">> := Id}) -> binary:decode_hex(Id).

%% The distributor's reply, as macula_group_keys builds it: atom keys, text tagged, epochs as maps.
result_checked(#{<<"prefix">> := Prefix, <<"policy">> := Policy, <<"epochs">> := Epochs, <<"cbor">> := Cbor}) ->
    Reply = #{prefix => {text, Prefix}, policy => {text, Policy}, epochs => [epoch(E) || E <- Epochs]},
    {ok, Plain} = macula_frame:payload_plain(Reply),
    ?assertEqual(Cbor, hex(Plain)).

epoch(#{<<"id">> := Id, <<"key">> := Key, <<"issued_at">> := I, <<"publish_until">> := P, <<"accept_until">> := A}) ->
    #{id => binary:decode_hex(Id), key => binary:decode_hex(Key), issued_at => I, publish_until => P,
      accept_until => A}.

%% The RESULT's bytes, opened as a member's link hands them over, are read by the keyring whole: the policy, and the
%% current epoch at a time inside it.
result_read(#{<<"cbor">> := Cbor, <<"policy">> := Policy, <<"epochs">> := [First | _]}) ->
    {ok, Reply} = macula_frame:plain_payload(binary:decode_hex(Cbor)),
    #{<<"issued_at">> := IssuedAt, <<"id">> := Id} = First,
    Prefix = macula_record:payload_field(Reply, <<"prefix">>),
    {ok, Pid} = macula_group_keyring:start_link(
                  #{now => fun() -> IssuedAt end, schedule => fun(_, _) -> ok end,
                    call => fun(_Realm, _Procedure, _Payload, _Opts) -> {ok, Reply, #{sealed => 1, provider => <<9:256>>}} end}),
    Keyring = macula_group_keyring:handle(Pid),
    try
        ?assertEqual({ok, binary_to_atom(Policy)}, macula_group_keyring:join(Keyring, <<7:256>>, Prefix, #{})),
        {ok, #{id := Held}} = macula_group_keyring:publish_epoch(Keyring, <<7:256>>, Prefix),
        ?assertEqual(binary:decode_hex(Id), Held)
    after
        unlink(Pid),
        exit(Pid, shutdown)
    end.

%% The verdict macula_ucan reaches under the distributor's advertise policy is the one the file pins.
grant_checked(Profile, #{<<"token">> := Token, <<"policy">> := #{<<"key_id">> := KeyId, <<"can">> := Can},
                         <<"context">> := Context, <<"verdict">> := Verdict}) ->
    ?assertEqual(Verdict, verdict(macula_ucan:authorize(Token, {realm_member_required, binary:decode_hex(KeyId), Can},
                                                        context(Profile, Context)))).

context(Profile, #{<<"caller">> := Caller, <<"now">> := Now, <<"realm">> := Realm, <<"procedure">> := Procedure}) ->
    #{caller => binary:decode_hex(Caller), profile => Profile, now => Now, proofs => #{},
      realm => binary:decode_hex(Realm), procedure => Procedure}.

verdict({ok, _Claims}) -> <<"ok">>;
verdict({error, Refusal}) -> atom_to_binary(Refusal).

hex(Bin) -> binary:encode_hex(Bin, lowercase).
