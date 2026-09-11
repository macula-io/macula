%% EUnit tests for macula_identity.
-module(macula_identity_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

-define(REQUIRED_MODE, <<"no access for group or others (0600 or 0400)">>).

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
    ok = file:change_mode(Path, 8#600),
    ?assertEqual({error, bad_key_file}, macula_identity:load(Path)).

load_returns_enoent_for_missing_file_test() ->
    ?assertEqual({error, enoent}, macula_identity:load("/nonexistent/xyz/key")).

saved_file_has_restrictive_permissions_test() ->
    Path = mktmp("identity.key"),
    Kp   = macula_identity:generate(),
    ok = macula_identity:save(Path, Kp),
    {ok, #file_info{mode = Mode}} = file:read_file_info(Path),
    ?assertEqual(8#0600, Mode band 8#0777).

%% Regression: a bare `ok = filelib:ensure_dir(Path)' match used to crash
%% this function with an unhandled MatchError whenever ensure_dir failed,
%% instead of returning `{error, Reason}' as this function's own -spec
%% promises. Found live: macula-realm's MaculaRealm.Mesh calls this from
%% a required supervised GenServer with no try/catch around it (reasonably
%% trusting the documented contract) -- the crash took the entire hosting
%% OTP application down with it, Ecto.Repo included, whenever the
%% configured path wasn't writable. A regular file standing where a
%% directory is expected reproduces the same ensure_dir failure portably,
%% without depending on OS permissions or running as non-root.
save_returns_error_instead_of_crashing_when_ensure_dir_fails_test() ->
    Blocker = mktmp("not_a_directory"),
    ok = file:write_file(Blocker, <<"not a directory">>),
    Path = filename:join(Blocker, "identity.key"),
    Kp = macula_identity:generate(),
    ?assertMatch({error, _}, macula_identity:save(Path, Kp)).

%% A key file its group or others can access is refused, whatever its
%% content, with an error naming the file, its mode and what is required.
load_refuses_key_file_group_or_others_can_access_test_() ->
    [{"load refuses a key file with mode " ++ Octal,
      fun() ->
          Path = saved_key_with_mode(Mode),
          ?assertEqual({error, {file_permissions, #{file => Path,
                                                    mode => list_to_binary(Octal),
                                                    required => ?REQUIRED_MODE}}},
                       macula_identity:load(Path))
      end}
     || {Mode, Octal} <- [{8#640, "0640"}, {8#604, "0604"}, {8#660, "0660"},
                          {8#606, "0606"}, {8#644, "0644"}]].

load_accepts_key_file_only_its_owner_can_read_test_() ->
    [{"load accepts a key file with mode " ++ Octal,
      fun() ->
          ?assertMatch({ok, #{public := _, private := _}},
                       macula_identity:load(saved_key_with_mode(Mode)))
      end}
     || {Mode, Octal} <- [{8#600, "0600"}, {8#400, "0400"}]].

load_follows_symlink_to_key_file_test() ->
    Target = mktmp("identity.key"),
    Kp = macula_identity:generate(),
    ok = macula_identity:save(Target, Kp),
    Link = filename:join(filename:dirname(Target), "linked.key"),
    ok = file:make_symlink(Target, Link),
    ?assertEqual({ok, Kp}, macula_identity:load(Link)).

load_refuses_directory_test() ->
    Dir = filename:dirname(mktmp("unused")),
    ?assertEqual({error, {file_type, #{file => Dir, type => directory, required => regular}}},
                 macula_identity:load(Dir)).

save_never_writes_through_symlink_at_path_plus_tmp_test() ->
    Path = mktmp("identity.key"),
    Target = filename:join(filename:dirname(Path), "elsewhere"),
    ok = file:write_file(Target, <<"unrelated">>),
    ok = file:make_symlink(Target, Path ++ ".tmp"),
    ok = macula_identity:save(Path, macula_identity:generate()),
    ?assertEqual({ok, <<"unrelated">>}, file:read_file(Target)),
    {ok, #file_info{type = Type}} = file:read_link_info(Path),
    ?assertEqual(regular, Type).

save_creates_missing_key_directory_only_its_owner_can_use_test() ->
    Dir = filename:join(filename:dirname(mktmp("unused")), "keys"),
    ok = macula_identity:save(filename:join(Dir, "identity.key"), macula_identity:generate()),
    {ok, #file_info{mode = Mode}} = file:read_file_info(Dir),
    ?assertEqual(8#700, Mode band 8#777).

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

saved_key_with_mode(Mode) ->
    Path = mktmp("identity.key"),
    ok = macula_identity:save(Path, macula_identity:generate()),
    ok = file:change_mode(Path, Mode),
    Path.
