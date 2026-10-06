%% Writes test/vectors/app_record_v1.json (macula#75): app records in both crypto profiles, each with the realm key a
%% reader trusts, the clock to check it at, and what verify/3, verify_app/3 and read_app_record/1 must give. Run through
%% scripts/generate-app-record-vectors.sh with the compiled tree on the code path.
%%
%% The malformed cases are signed by hand, as a hostile org could: sign/2 refuses them, a reader must too. Signing is
%% randomized, so a second run signs other bytes with the same outcomes: the committed file is the vector, generated
%% once. macula_app_record_vectors_tests re-derives every outcome from it on each run.
{ok, _} = application:ensure_all_started(crypto),
Hex = fun(Bin) -> binary:encode_hex(Bin, lowercase) end,
Minute = 60_000,
RealmId = crypto:hash(sha256, <<"io.example">>),
Services = [#{name => <<"forecast">>, procedures => [<<"acme/get_forecast_v1">>, <<"acme/watch_forecast_v1">>]},
            #{name => <<"alerts">>, procedures => []}],
Profile = fun(P) ->
    Key = fun(Purpose) -> {ok, K} = macula_node_keys:generate(Purpose, P), K end,
    Realm = Key(realm),
    Org = Key(org),
    Directory = fun(Signer, OrgName, OrgKey, Ttl) ->
        macula_record:encode(macula_record:sign(
            macula_record:org_directory(RealmId, OrgName, macula_node_keys:key_id(OrgKey), #{ttl_ms => Ttl}), Signer))
    end,
    GoodDir = Directory(Realm, <<"acme">>, Org, 6 * 60 * Minute),
    App = fun(Dir, Opts) ->
        macula_record:app_record(macula_node_keys:key_id(Org), RealmId, <<"acme">>, <<"weather">>,
                                 maps:merge(#{version => <<"1.4.0">>, services => Services, org_directory => Dir,
                                              ttl_ms => 30 * Minute}, Opts))
    end,
    Signed = fun(Unsigned) -> {macula_record:encode(macula_record:sign(Unsigned, Org)), maps:get(created_at, Unsigned)} end,
    %% A record whose payload the builder would not make, signed by the org key as it is.
    Forged = fun(Changes) ->
        #{payload := P0} = U = App(GoodDir, #{}),
        Fields = #{{text, <<"type">>} => 16#17, {text, <<"version">>} => maps:get(version, U),
                   {text, <<"created_at">>} => maps:get(created_at, U),
                   {text, <<"expires_at">>} => maps:get(expires_at, U),
                   {text, <<"payload">>} => maps:merge(P0, Changes)},
        {macula_signed_object:encode(macula_signed_object:sign(<<"MACULA-PQ-RECORD-V1">>, Fields, Org)),
         maps:get(created_at, U)}
    end,
    Service = fun(Name, Procedures) ->
        #{{text, <<"name">>} => {text, Name}, {text, <<"procedures">>} => [{text, Pr} || Pr <- Procedures]}
    end,
    Trust = #{profile => P, realm_key => macula_node_keys:public_key(Realm)},
    Outcome = fun({ok, _}) -> <<"ok">>; (ok) -> <<"ok">>; ({error, R}) -> atom_to_binary(R) end,
    %% The carried directory is in the record already; the reading names it by its sha256.
    Read = fun(#{realm_id := RId, org_key := OKey, org_directory := Dir} = R) ->
        (maps:remove(org_directory, R))#{realm_id := Hex(RId), org_key := Hex(OKey),
                                         org_directory_sha256 => Hex(crypto:hash(sha256, Dir))}
    end,
    Case = fun(Name, {Bytes, Created}, Later) ->
        Now = Created + Later,
        Verified = macula_record:verify(Bytes, P, Now),
        App1 = case Verified of
                   {ok, R} -> Outcome(macula_record:verify_app(R, Trust, Now));
                   _ -> null
               end,
        Reading = case Verified of
                      {ok, R2} -> Read(macula_record:read_app_record(R2));
                      _ -> null
                  end,
        #{<<"name">> => Name, <<"record">> => Hex(Bytes), <<"now_ms">> => Now,
          <<"verify">> => Outcome(Verified), <<"verify_app">> => App1, <<"reading">> => Reading}
    end,
    {atom_to_binary(P),
     #{<<"realm_key">> => Hex(macula_node_keys:public_key(Realm)),
       <<"realm_key_id">> => Hex(macula_node_keys:key_id(Realm)),
       <<"org_key_id">> => Hex(macula_node_keys:key_id(Org)),
       <<"app_key">> => Hex(macula_record:app_key(RealmId, <<"acme">>, <<"weather">>)),
       <<"cases">> =>
           [Case(<<"valid">>, Signed(App(GoodDir, #{})), 1_000),
            Case(<<"directory_names_another_org_key">>, Signed(App(Directory(Realm, <<"acme">>, Key(org), 6 * 60 * Minute), #{})), 1_000),
            Case(<<"directory_from_another_realm_key">>, Signed(App(Directory(Key(realm), <<"acme">>, Org, 6 * 60 * Minute), #{})), 1_000),
            Case(<<"directory_for_another_org">>, Signed(App(Directory(Realm, <<"acmecorp">>, Org, 6 * 60 * Minute), #{})), 1_000),
            Case(<<"outlives_directory">>, Signed(App(Directory(Realm, <<"acme">>, Org, 2 * Minute), #{})), 1_000),
            Case(<<"expired">>, Signed(App(GoodDir, #{})), 40 * Minute),
            Case(<<"node_namespace_org">>,
                 Forged(#{{text, <<"org_name">>} => {text, <<"~", (Hex(binary:copy(<<1>>, 32)))/binary>>},
                          {text, <<"services">>} => []}), 1_000),
            Case(<<"foreign_namespace_procedure">>,
                 Forged(#{{text, <<"services">>} => [Service(<<"s">>, [<<"globex/get_forecast_v1">>])]}), 1_000),
            Case(<<"duplicate_service_name">>,
                 Forged(#{{text, <<"services">>} => [Service(<<"s">>, []), Service(<<"s">>, [])]}), 1_000),
            Case(<<"version_past_64_bytes">>,
                 Forged(#{{text, <<"version">>} => {text, binary:copy(<<"9">>, 65)}}), 1_000)]}}
end,
Vectors = #{<<"scheme">> => 1,
            <<"generator">> => <<"scripts/generate-app-record-vectors.sh">>,
            <<"realm_name">> => <<"io.example">>,
            <<"note">> => <<"verify is verify/3 at now_ms; verify_app is verify_app/3 against realm_key, null when verify refused; reading is read_app_record/1, null when verify refused">>,
            <<"profiles">> => maps:from_list([Profile(P) || P <- [pq_pure, pq_hybrid]])},
ok = file:write_file("test/vectors/app_record_v1.json", [json:format(Vectors), "\n"]),
io:format("wrote test/vectors/app_record_v1.json~n"),
halt(0).
