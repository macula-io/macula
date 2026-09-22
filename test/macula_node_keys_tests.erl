%% EUnit tests for macula_node_keys: a node's keys per purpose and profile, stored as plan decision D6 describes.
-module(macula_node_keys_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).

%%------------------------------------------------------------------
%% Generation
%%------------------------------------------------------------------

us_identity_key_is_one_mldsa87_pair_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    ?assertMatch(#{purpose := identity, profile := pq_pure,
                   components := [#{algorithm := mldsa87}]}, Key),
    [#{public := Public, private := Private}] = maps:get(components, Key),
    ?assertEqual({2592, 4896}, {byte_size(Public), byte_size(Private)}).

us_connect_key_is_one_mldsa87_pair_test() ->
    {ok, Key} = macula_node_keys:generate(connect, pq_pure),
    ?assertMatch(#{purpose := connect, components := [#{algorithm := mldsa87}]}, Key).

tls_key_is_mldsa87_alone_in_both_profiles_test_() ->
    [?_assertMatch({ok, #{purpose := tls, profile := Profile, components := [#{algorithm := mldsa87}]}},
                   macula_node_keys:generate(tls, Profile))
     || Profile <- [pq_pure, pq_hybrid]].

eu_identity_and_connect_keys_pair_mldsa87_with_rsa_pss_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        [begin
             {ok, Key} = macula_node_keys:generate(Purpose, pq_hybrid),
             ?assertMatch(#{components := [#{algorithm := mldsa87}, #{algorithm := rsa_pss}]}, Key)
         end
         || Purpose <- [identity, connect]]
    end}.

unknown_purpose_is_refused_test() ->
    ?assertEqual({error, {unknown_purpose, signing}},
                 macula_node_keys:generate(signing, pq_pure)).

unknown_profile_is_refused_test() ->
    ?assertEqual({error, {crypto_profile_unknown, rsa_only}},
                 macula_node_keys:generate(identity, rsa_only)).

%%------------------------------------------------------------------
%% Save and load
%%------------------------------------------------------------------

us_keys_survive_save_and_load_test_() ->
    [?_test(assert_round_trip(Purpose, pq_pure)) || Purpose <- [identity, connect, tls]].

eu_keys_survive_save_and_load_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        [assert_round_trip(Purpose, pq_hybrid) || Purpose <- [identity, tls]]
    end}.

saved_file_is_readable_by_its_owner_only_test() ->
    with_tmp_path("identity.key", fun(Path) ->
        {ok, Key} = macula_node_keys:generate(identity, pq_pure),
        ok = macula_node_keys:save(Path, Key),
        {ok, #file_info{mode = Mode}} = file:read_file_info(Path),
        ?assertEqual(8#0600, Mode band 8#0777)
    end).

key_file_its_group_or_others_can_read_is_refused_test_() ->
    [?_assertEqual({error, key_file_permissions}, load_with_mode(Mode))
     || Mode <- [8#0640, 8#0604, 8#0660, 8#0606, 8#0644]].

key_file_readable_by_its_owner_only_loads_test_() ->
    [?_assertMatch({ok, #{purpose := identity}}, load_with_mode(Mode)) || Mode <- [8#0600, 8#0400]].

missing_file_returns_enoent_test() ->
    ?assertEqual({error, enoent},
                 macula_node_keys:load("/nonexistent/xyz/identity.key", identity, pq_pure)).

%%------------------------------------------------------------------
%% What load refuses
%%------------------------------------------------------------------

stored_mldsa87_public_key_that_differs_from_the_derived_one_is_refused_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    [Component = #{public := Public}] = maps:get(components, Key),
    Tampered = Key#{components := [Component#{public := flip_byte(Public, 100)}]},
    ?assertEqual({error, public_key_mismatch}, save_and_load(Tampered, identity, pq_pure)).

stored_rsa_public_key_that_differs_from_the_derived_one_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Key} = macula_node_keys:generate(identity, pq_hybrid),
        {ok, Other} = macula_node_keys:generate(connect, pq_hybrid),
        [MlDsa, Rsa] = maps:get(components, Key),
        [_, #{public := OtherRsaPublic}] = maps:get(components, Other),
        Tampered = Key#{components := [MlDsa, Rsa#{public := OtherRsaPublic}]},
        ?assertEqual({error, public_key_mismatch}, save_and_load(Tampered, identity, pq_hybrid))
    end}.

%% An expanded ML-DSA-87 private key is rho, K and tr (bytes 0 to 127), then s1, s2 and t0 (t0 from byte 1,568). The
%% public key derives from rho, s1 and s2 alone, so a corrupted tr or t0 leaves it unchanged: the key is refused
%% because tr must hash the public key and t0 must be the low bits of t, and neither holds.
corrupted_mldsa87_tr_is_refused_test() ->
    ?assertEqual({error, private_key_invalid}, load_with_private_byte_flipped(64)).

corrupted_mldsa87_t0_is_refused_test() ->
    ?assertEqual({error, private_key_invalid}, load_with_private_byte_flipped(2000)).

truncated_mldsa87_private_key_is_refused_test() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    [Component = #{private := <<Short:4000/binary, _/binary>>}] = maps:get(components, Key),
    Truncated = Key#{components := [Component#{private := Short}]},
    ?assertEqual({error, private_key_invalid}, save_and_load(Truncated, identity, pq_pure)).

key_saved_for_another_purpose_is_refused_test() ->
    {ok, Key} = macula_node_keys:generate(connect, pq_pure),
    ?assertEqual({error, {wrong_purpose, connect}}, save_and_load(Key, identity, pq_pure)).

key_saved_for_another_profile_is_refused_test() ->
    {ok, Key} = macula_node_keys:generate(tls, pq_pure),
    ?assertEqual({error, {wrong_profile, pq_pure}}, save_and_load(Key, tls, pq_hybrid)).

key_whose_algorithms_do_not_match_its_purpose_and_profile_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, EuKey} = macula_node_keys:generate(identity, pq_hybrid),
        Relabelled = EuKey#{profile := pq_pure},
        ?assertEqual({error, {wrong_algorithms, [mldsa87, rsa_pss]}},
                     save_and_load(Relabelled, identity, pq_pure))
    end}.

ed25519_key_file_is_refused_test() ->
    with_tmp_path("identity.key", fun(Path) ->
        ok = macula_identity:save(Path, macula_identity:generate()),
        ?assertEqual({error, bad_key_file}, macula_node_keys:load(Path, identity, pq_pure))
    end).

file_with_trailing_bytes_is_refused_test() ->
    with_tmp_path("identity.key", fun(Path) ->
        {ok, Key} = macula_node_keys:generate(identity, pq_pure),
        ok = macula_node_keys:save(Path, Key),
        {ok, Bin} = file:read_file(Path),
        ok = file:write_file(Path, <<Bin/binary, 0>>),
        ?assertEqual({error, bad_key_file}, macula_node_keys:load(Path, identity, pq_pure))
    end).

truncated_file_is_refused_test() ->
    with_tmp_path("identity.key", fun(Path) ->
        {ok, Key} = macula_node_keys:generate(identity, pq_pure),
        ok = macula_node_keys:save(Path, Key),
        {ok, <<Head:1000/binary, _/binary>>} = file:read_file(Path),
        ok = file:write_file(Path, Head),
        ?assertEqual({error, bad_key_file}, macula_node_keys:load(Path, identity, pq_pure))
    end).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

assert_round_trip(Purpose, Profile) ->
    {ok, Key} = macula_node_keys:generate(Purpose, Profile),
    ?assertEqual({ok, Key}, save_and_load(Key, Purpose, Profile)).

save_and_load(Key, Purpose, Profile) ->
    with_tmp_path("node.key", fun(Path) ->
        ok = macula_node_keys:save(Path, Key),
        macula_node_keys:load(Path, Purpose, Profile)
    end).

load_with_mode(Mode) ->
    with_tmp_path("identity.key", fun(Path) ->
        {ok, Key} = macula_node_keys:generate(identity, pq_pure),
        ok = macula_node_keys:save(Path, Key),
        ok = file:change_mode(Path, Mode),
        macula_node_keys:load(Path, identity, pq_pure)
    end).

load_with_private_byte_flipped(Offset) ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    [Component = #{private := Private}] = maps:get(components, Key),
    save_and_load(Key#{components := [Component#{private := flip_byte(Private, Offset)}]}, identity, pq_pure).

flip_byte(Bin, Offset) ->
    <<Head:Offset/binary, Byte, Tail/binary>> = Bin,
    <<Head/binary, (Byte bxor 1), Tail/binary>>.

%% Fun called with the path Name in a new directory, removed once Fun returns or raises.
with_tmp_path(Name, Fun) ->
    macula_test_tmp:with_dir("macula_node_keys_tests", fun(Dir) -> Fun(filename:join(Dir, Name)) end).
