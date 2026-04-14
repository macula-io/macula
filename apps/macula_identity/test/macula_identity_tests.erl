%% EUnit tests for macula_identity.
-module(macula_identity_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%%------------------------------------------------------------------
%% Generation
%%------------------------------------------------------------------

generate_returns_32byte_keys_test() ->
    Kp = macula_identity:generate(),
    ?assertMatch(#{public := _, private := _}, Kp),
    ?assertEqual(32, byte_size(macula_identity:public(Kp))),
    ?assertEqual(32, byte_size(macula_identity:private(Kp))).

generated_keys_are_random_test() ->
    ?assertNotEqual(macula_identity:generate(), macula_identity:generate()).

%%------------------------------------------------------------------
%% Sign / verify
%%------------------------------------------------------------------

sign_produces_64_byte_signature_test() ->
    Kp = macula_identity:generate(),
    Sig = macula_identity:sign(<<"msg">>, Kp),
    ?assertEqual(64, byte_size(Sig)).

sign_verify_roundtrip_test() ->
    Kp = macula_identity:generate(),
    Msg = <<"hello macula">>,
    Sig = macula_identity:sign(Msg, Kp),
    ?assert(macula_identity:verify(Msg, Sig, macula_identity:public(Kp))).

sign_verify_accepts_iolist_message_test() ->
    Kp = macula_identity:generate(),
    Iolist = [<<"hello">>, $\s, <<"macula">>],
    Flat   = iolist_to_binary(Iolist),
    Sig    = macula_identity:sign(Iolist, Kp),
    ?assert(macula_identity:verify(Flat, Sig, macula_identity:public(Kp))).

verify_rejects_tampered_message_test() ->
    Kp = macula_identity:generate(),
    Sig = macula_identity:sign(<<"original">>, Kp),
    ?assertNot(macula_identity:verify(<<"tampered">>, Sig,
                                      macula_identity:public(Kp))).

verify_rejects_wrong_pubkey_test() ->
    Kp1 = macula_identity:generate(),
    Kp2 = macula_identity:generate(),
    Msg = <<"signed-by-1">>,
    Sig = macula_identity:sign(Msg, Kp1),
    ?assertNot(macula_identity:verify(Msg, Sig, macula_identity:public(Kp2))).

sign_accepts_raw_private_key_test() ->
    Kp = macula_identity:generate(),
    Priv = macula_identity:private(Kp),
    Sig  = macula_identity:sign(<<"m">>, Priv),
    ?assert(macula_identity:verify(<<"m">>, Sig, macula_identity:public(Kp))).

%%------------------------------------------------------------------
%% NodeId
%%------------------------------------------------------------------

node_id_of_key_pair_is_public_key_test() ->
    Kp = macula_identity:generate(),
    ?assertEqual(macula_identity:public(Kp), macula_identity:node_id(Kp)).

node_id_of_pubkey_is_identity_test() ->
    Pub = crypto:strong_rand_bytes(32),
    ?assertEqual(Pub, macula_identity:node_id(Pub)).

%%------------------------------------------------------------------
%% Puzzle
%%------------------------------------------------------------------

puzzle_evidence_is_sha256_of_pubkey_test() ->
    Kp   = macula_identity:generate(),
    Pub  = macula_identity:public(Kp),
    Want = crypto:hash(sha256, Pub),
    ?assertEqual(Want, macula_identity:puzzle_evidence(Kp)),
    ?assertEqual(Want, macula_identity:puzzle_evidence(Pub)).

puzzle_difficulty_zero_always_valid_test() ->
    ?assert(macula_identity:puzzle_valid(macula_identity:generate(), 0)).

puzzle_validity_is_deterministic_test() ->
    Kp = macula_identity:generate(),
    V1 = macula_identity:puzzle_valid(Kp, 4),
    V2 = macula_identity:puzzle_valid(Kp, 4),
    ?assertEqual(V1, V2).

grind_produces_valid_puzzle_test_() ->
    %% Difficulty 10 means ~1024 attempts expected; comfortably within 30s.
    {timeout, 30,
     fun() ->
         Kp = macula_identity:generate(#{puzzle => true, difficulty => 10}),
         ?assert(macula_identity:puzzle_valid(Kp, 10))
     end}.

puzzle_higher_difficulty_implies_lower_difficulty_test_() ->
    {timeout, 30,
     fun() ->
         Kp = macula_identity:generate(#{puzzle => true, difficulty => 10}),
         ?assert(macula_identity:puzzle_valid(Kp,  0)),
         ?assert(macula_identity:puzzle_valid(Kp,  5)),
         ?assert(macula_identity:puzzle_valid(Kp, 10))
     end}.

%%------------------------------------------------------------------
%% Persistence
%%------------------------------------------------------------------

save_load_roundtrip_test() ->
    Path = mktmp("identity.key"),
    Kp   = macula_identity:generate(),
    ok = macula_identity:save(Path, Kp),
    ?assertEqual({ok, Kp}, macula_identity:load(Path)).

load_rejects_bad_format_test() ->
    Path = mktmp("bad.key"),
    ok = file:write_file(Path, <<"not a valid key">>),
    ?assertEqual({error, bad_key_file}, macula_identity:load(Path)).

load_returns_enoent_for_missing_file_test() ->
    ?assertEqual({error, enoent}, macula_identity:load("/nonexistent/xyz/key")).

saved_file_has_restrictive_permissions_test() ->
    Path = mktmp("identity.key"),
    Kp   = macula_identity:generate(),
    ok = macula_identity:save(Path, Kp),
    {ok, #file_info{mode = Mode}} = file:read_file_info(Path),
    ?assertEqual(8#0600, Mode band 8#0777).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

mktmp(Name) ->
    Dir  = filename:join([
        "/tmp",
        "macula_identity_tests",
        integer_to_list(erlang:unique_integer([positive]))
    ]),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    filename:join(Dir, Name).
