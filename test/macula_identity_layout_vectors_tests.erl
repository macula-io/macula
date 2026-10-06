%% Runs every case in test/vectors/identity_layout_v1.json (macula#76), the vector the Go, Rust, .NET and TypeScript
%% SDKs run too: the file of each name and profile, the names refused, and the move of the old identity.key into the
%% layout. Every case works in its own directory, so none reads or writes the identity of the account running it.
-module(macula_identity_layout_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

every_path_is_the_vectors_file_test_() ->
    #{<<"paths">> := Paths, <<"identity_dir">> := IdentityDir} = vectors(),
    [{<<Name/binary, " ", Profile/binary>>,
      fun() ->
          ?assertEqual({ok, binary_to_list(File)},
                       macula_node_keys:identity_path(binary_to_list(IdentityDir), Name, binary_to_atom(Profile)))
      end}
     || #{<<"name">> := Name, <<"profile">> := Profile, <<"file">> := File} <- Paths].

every_refused_name_is_refused_and_writes_nothing_test_() ->
    #{<<"refused_names">> := Names} = vectors(),
    [{Name, fun() -> in_dir(fun(_Dir, IdentityDir) -> refused(Name, IdentityDir) end) end} || Name <- Names].

refused(Name, IdentityDir) ->
    ?assertEqual({error, {identity_name, invalid}}, macula_node_keys:identity_path(IdentityDir, Name, profile())),
    ?assertEqual({error, {identity_name, invalid}}, macula_node_keys:stored_identity(Name, profile())),
    ?assertNot(filelib:is_dir(IdentityDir)).

every_old_key_case_reaches_its_outcome_test_() ->
    #{<<"old_key_cases">> := Cases, <<"old_key_file">> := OldKeyFile, <<"default_name">> := Default} = vectors(),
    [{Name, {timeout, 60, fun() -> in_dir(fun(Dir, _IdentityDir) -> old_key_case(Case, Dir, OldKeyFile, Default) end) end}}
     || #{<<"name">> := Name} = Case <- Cases].

old_key_case(#{<<"old_key">> := OldKey, <<"in_place">> := InPlace, <<"outcome">> := Outcome}, Dir, OldKeyFile,
             Default) ->
    Old = filename:join(Dir, binary_to_list(OldKeyFile)),
    KeyProfile = old_key(OldKey, Old),
    {ok, To} = macula_node_keys:identity_path(macula_node_keys:identity_dir(), Default, KeyProfile),
    ok = in_place(InPlace, Old, To, KeyProfile),
    {ok, OldBytes} = file:read_file(Old),
    {ok, InPlaceBefore} = read_or_none(To),
    reached(Outcome, macula_node_keys:stored_identity(Default, profile()), Old, To, OldBytes, InPlaceBefore).

%% The old key file the case starts from, and the profile its move goes under.
old_key(<<"own_profile">>, Old) -> saved(Old, profile());
old_key(<<"other_profile">>, Old) -> saved(Old, other_profile());
old_key(<<"not_a_key">>, Old) ->
    ok = file:write_file(Old, <<"this is not a macula node key">>),
    ok = file:change_mode(Old, 8#0600),
    profile().

saved(Old, Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    ok = macula_node_keys:save(Old, Key),
    Profile.

in_place(<<"nothing">>, _Old, _To, _Profile) ->
    ok;
in_place(<<"same_file">>, Old, To, _Profile) ->
    ok = filelib:ensure_dir(To),
    file:make_link(Old, To);
in_place(<<"another_key">>, _Old, To, Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    macula_node_keys:save(To, Key).

reached(<<"moved">>, Result, Old, To, OldBytes, _InPlaceBefore) ->
    ?assertMatch({ok, _, _}, Result),
    ?assertNot(filelib:is_regular(Old)),
    ?assertEqual({ok, OldBytes}, file:read_file(To));
reached(<<"place_taken">>, Result, Old, To, OldBytes, InPlaceBefore) ->
    ?assertEqual({error, {old_identity_key, #{from => Old, to => To}, place_taken}}, Result),
    ?assertEqual({ok, OldBytes}, file:read_file(Old)),
    ?assertEqual({ok, InPlaceBefore}, read_or_none(To));
reached(Reason, Result, Old, _To, OldBytes, _InPlaceBefore) ->
    ?assertEqual({error, {old_identity_key, #{from => Old}, binary_to_atom(Reason)}}, Result),
    ?assertEqual({ok, OldBytes}, file:read_file(Old)).

read_or_none(Path) ->
    read_or_none_result(file:read_file(Path)).

read_or_none_result({ok, _} = Read) -> Read;
read_or_none_result({error, enoent}) -> {ok, none}.

%% A case in its own directory, with the identity directory pointed into it and restored after.
in_dir(Fun) ->
    macula_test_tmp:with_dir("macula-identity-layout",
                             fun(Dir) ->
                                 IdentityDir = filename:join(Dir, "identity"),
                                 Before = application:get_env(macula, identity_dir),
                                 ok = application:set_env(macula, identity_dir, IdentityDir),
                                 try Fun(Dir, IdentityDir) after restore(Before) end
                             end).

restore({ok, Was}) -> application:set_env(macula, identity_dir, Was);
restore(undefined) -> application:unset_env(macula, identity_dir).

profile() ->
    _ = application:load(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

other_profile() ->
    [Other] = macula_crypto_profile:profiles() -- [profile()],
    Other.

vectors() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file() ->
    Name = "identity_layout_v1.json",
    first_existing([filename:join("test/vectors", Name), filename:join("../../test/vectors", Name)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Name])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
