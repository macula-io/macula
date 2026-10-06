%% Writes test/vectors/realm_member_endorsement_v1.json: realm member endorsements signed by a realm key in both crypto
%% profiles, each with the member it is checked for, the time to check it at (now_ms) and the verdict
%% macula_hyparview_endorsement:verify_endorsement/4 reaches: {ok, Roles} as the roles, or the refusal's name. The
%% times cover a historical check (now_ms inside a window that has long ended by the time anyone reads the file), both
%% ends of the window, and the refusals in the order the verifier checks them. Run through
%% scripts/generate-realm-member-endorsement-vectors.sh with the compiled tree on the code path.
%%
%% ML-DSA-87 signing is hedged and pq_hybrid's RSA-PSS half is randomized, so a second run signs other bytes with the
%% same verdicts: the committed file is the vector, generated once. macula_realm_member_endorsement_vectors_tests
%% re-derives every verdict from it on each run.
{ok, _} = application:ensure_all_started(crypto),
Hex = fun(Bin) -> binary:encode_hex(Bin, lowercase) end,
Minute = 60 * 1000,
Hour = 60 * Minute,
Day = 24 * Hour,
%% Synthetic ids: the realm and its members are named for the vector, not taken from any running realm.
Id = fun(Name) -> crypto:hash(sha256, <<"macula realm member endorsement vector: ", Name/binary>>) end,
Realm = Id(<<"realm">>),
OtherRealm = Id(<<"another realm">>),
Member = Id(<<"member">>),
OtherMember = Id(<<"another member">>),
Roles = [<<"peer">>, <<"directory">>],
Profile = fun(P) ->
    {ok, RealmKey} = macula_node_keys:generate(realm, P),
    {ok, OtherKey} = macula_node_keys:generate(realm, P),
    Trust = #{profile => P, realm => Realm, realm_key_id => macula_node_keys:key_id(RealmKey)},
    G = erlang:system_time(millisecond),
    %% The record lives 2 days from G; its window runs from G + 1 hour to G + 1 day unless a case sets another.
    Unsigned = fun(R, Window) ->
        macula_record:realm_member_endorsement(R, #{realm => R, member_node => Member, roles => Roles},
                                               maps:merge(#{valid_from => G + Hour, valid_until => G + Day,
                                                            ttl_ms => 2 * Day}, Window))
    end,
    Forged = fun(Field, Value) ->
        #{payload := P0} = U = Unsigned(Realm, #{}),
        U#{payload := P0#{{text, Field} := Value}}
    end,
    Signed = fun(Record, Key) -> macula_record:encode(macula_record:sign(Record, Key)) end,
    #{key := RealmPublic, expires_at := Expires} = OkSigned = macula_record:sign(Unsigned(Realm, #{}), RealmKey),
    Ok = macula_record:encode(OkSigned),
    Case = fun(Name, Wire, For, Now) ->
        Verdict = case macula_hyparview_endorsement:verify_endorsement(Wire, Trust, For, Now) of
                      {ok, Got} -> #{<<"roles">> => Got};
                      {error, Reason} -> #{<<"refused">> => atom_to_binary(Reason)}
                  end,
        Verdict#{<<"name">> => Name, <<"record">> => Hex(Wire), <<"member">> => Hex(For), <<"now_ms">> => Now}
    end,
    {atom_to_binary(P),
     #{<<"realm_key">> => Hex(RealmPublic),
       <<"cases">> =>
           [Case(<<"inside_the_window">>, Ok, Member, G + 2 * Hour),
            Case(<<"at_valid_from">>, Ok, Member, G + Hour),
            Case(<<"at_valid_until">>, Ok, Member, G + Day),
            Case(<<"before_valid_from">>, Ok, Member, G + Hour - 1),
            Case(<<"after_valid_until">>, Ok, Member, G + Day + 1),
            %% Past the record's own expires_at and the clock tolerance after it: the record, not the window, refuses.
            Case(<<"past_the_record_expiry">>, Ok, Member, Expires + 5 * Minute + 1),
            Case(<<"another_member">>, Ok, OtherMember, G + 2 * Hour),
            Case(<<"another_realm">>, Signed(Unsigned(OtherRealm, #{}), RealmKey), Member, G + 2 * Hour),
            Case(<<"another_realm_key">>, Signed(Unsigned(Realm, #{}), OtherKey), Member, G + 2 * Hour),
            Case(<<"another_record_type">>,
                 Signed(macula_record:org_directory(Realm, <<"acme">>, Id(<<"org key">>)), RealmKey), Member,
                 G + 2 * Hour),
            Case(<<"window_over_30_days">>, Signed(Forged(<<"valid_until">>, G + Hour + 30 * Day + 1), RealmKey),
                 Member, G + 2 * Hour),
            Case(<<"window_reversed">>, Signed(Forged(<<"valid_until">>, G + Hour - 1), RealmKey), Member,
                 G + 2 * Hour),
            Case(<<"roles_not_a_list">>, Signed(Forged(<<"roles">>, {text, <<"peer">>}), RealmKey), Member,
                 G + 2 * Hour)]}}
end,
Vectors = #{<<"scheme">> => 1,
            <<"generator">> => <<"scripts/generate-realm-member-endorsement-vectors.sh">>,
            <<"verifier">> => <<"macula_hyparview_endorsement:verify_endorsement/4">>,
            <<"realm">> => Hex(Realm),
            <<"note">> => <<"each case's verdict is roles (admitted, with the endorsed roles) or refused (the refusal's name)">>,
            <<"profiles">> => maps:from_list([Profile(P) || P <- [pq_pure, pq_hybrid]])},
ok = file:write_file("test/vectors/realm_member_endorsement_v1.json", [json:format(Vectors), "\n"]),
io:format("wrote test/vectors/realm_member_endorsement_v1.json~n"),
halt(0).
