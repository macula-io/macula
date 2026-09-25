%% Writes test/fixtures/own_namespace/: signed procedure advertisements in a node's own namespace
%% (D25 item 6, revised 2026-09-24) and the verdicts every SDK must reach on them, one directory per
%% crypto profile. Run through scripts/generate-own-namespace-fixtures.sh with the compiled tree on the
%% code path. Throwaway identities; the evaluation time is recorded, since advertisements expire.
Dir = "test/fixtures/own_namespace",
Realm = <<7:256>>,
Station = <<9:256>>,
Case = fun(Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Node} = macula_node_keys:node_id(Key),
    Hex = binary:encode_hex(Node, lowercase),
    Other = binary:encode_hex(<<1:256>>, lowercase),
    Auth = #{org_directory => <<"d">>, procedure_delegation => <<"p">>},
    Cases = [{<<"own_ok">>, <<"~", Hex/binary, "/ring">>, #{}},
             {<<"own_other_node">>, <<"~", Other/binary, "/ring">>, #{}},
             {<<"own_with_authorization">>, <<"~", Hex/binary, "/ring">>, #{authorization => Auth}},
             {<<"own_uppercase_hex">>, <<"~", (string:uppercase(Hex))/binary, "/ring">>, #{}},
             {<<"own_short_hex">>, <<"~", (binary:part(Hex, 0, 62))/binary, "/ring">>, #{}},
             {<<"org_without_chain">>, <<"acme/ring">>, #{}},
             {<<"own_hex_without_a_name">>, <<"~", Hex/binary>>, #{}}],
    ok = filelib:ensure_path(filename:join(Dir, atom_to_list(Profile))),
    [begin
         Signed = macula_record:refresh(
                    macula_record:procedure_advertisement(Node, Realm, Procedure, Station, Opts), Key),
         Bytes = macula_record:encode(Signed),
         File = filename:join([Dir, atom_to_list(Profile), <<Name/binary, ".bin">>]),
         ok = file:write_file(File, Bytes),
         Now = macula_record:expires_at(Signed) - 60000,
         Verdict = fun(ok) -> <<"ok">>; ({error, R}) -> atom_to_binary(R) end,
         #{<<"file">> => iolist_to_binary([atom_to_list(Profile), "/", Name, ".bin"]),
           <<"profile">> => atom_to_binary(Profile),
           <<"procedure">> => Procedure,
           <<"now_ms">> => Now,
           <<"own_namespace">> => Verdict(macula_record:own_namespace(Signed)),
           <<"verify_authorization">> =>
               Verdict(macula_record:verify_authorization(Signed, #{profile => Profile}, Now))}
     end || {Name, Procedure, Opts} <- Cases]
end,
Entries = lists:append([Case(P) || P <- [pq_pure, pq_hybrid]]),
ok = file:write_file(filename:join(Dir, "verdicts.json"), [json:format(Entries), "\n"]),
io:format("wrote ~b fixtures~n", [length(Entries)]),
halt().
