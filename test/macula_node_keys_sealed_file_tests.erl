%% EUnit tests for sealed key files (MACULA-SEALED-KEY-V1, docs/design/DESIGN_SEALED_KEY_FILE.md): the format, the
%% KEK refusals, the two modes, sealing a file in place, and the vector macula-go and macula-rust run too.
%%
%% A KEK sits on a different filesystem from the key file it unseals, which is what the format requires and what the
%% fleet does (a tmpfs directory bind-mounted into the service). The suite writes its key files under the system
%% temporary directory and its KEKs under /dev/shm, and asserts first that the two really are different devices: on a
%% box where they are not, every load here would refuse, and a suite that quietly went green would be worse than one
%% that fails.
-module(macula_node_keys_sealed_file_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).

-define(SEALED_MAGIC, "MACULA-SEALED-KEY-V1\0").
-define(HEADER_BYTES, 100).
-define(KEK_DIR, "/dev/shm").

%%------------------------------------------------------------------
%% Setup: two filesystems
%%------------------------------------------------------------------

two_filesystems_test() ->
    ?assertNotEqual(key_device(tmp_dir()), key_device(?KEK_DIR)).

key_device(Dir) ->
    {ok, #file_info{major_device = Device}} = file:read_file_info(Dir),
    Device.

%%------------------------------------------------------------------
%% The format
%%------------------------------------------------------------------

sealing_keeps_the_key_and_its_identities_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Sealed = macula_node_keys:seal(Key, Kek),
         ?assertMatch(<<?SEALED_MAGIC, _/binary>>, Sealed),
         ?assertEqual({ok, Key}, macula_node_keys:unseal(Sealed, Kek)),
         ?assertEqual({ok, macula_node_keys:key_id(Key)}, macula_node_keys:node_id(Key)),
         ?assertEqual(macula_node_keys:node_id(Key), sealed_node_id(Sealed, Kek))
     end}.

every_purpose_and_profile_seals_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         [{timeout, ?EU_TIMEOUT, fun() -> seals(Key) end}
          || Purpose <- [identity, connect, tls, realm, org, foundation],
             Profile <- [pq_pure, pq_hybrid],
             Key <- [element(2, macula_node_keys:generate(Purpose, Profile))]]
     end}.

seals(Key) ->
    Kek = kek(),
    Sealed = macula_node_keys:seal(Key, Kek),
    ?assertEqual({ok, Key}, macula_node_keys:unseal(Sealed, Kek)),
    ?assertEqual(macula_node_keys:key_id(Key), sealed_key_id(Sealed)).

sealing_is_reproducible_under_one_nonce_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Nonce = nonce(),
         First = macula_node_keys:seal(Key, Kek, Nonce),
         ?assertEqual(First, macula_node_keys:seal(Key, Kek, Nonce)),
         ?assertNotEqual(First, macula_node_keys:seal(Key, Kek, nonce()))
     end}.

%% The header names the key inside, and the name is covered by the tag, so a file moved over another's is refused
%% rather than unsealed under the wrong identity.
a_header_naming_another_key_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Ours} = macula_node_keys:generate(identity, pq_pure),
         {ok, Theirs} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Sealed = crafted(macula_node_keys:key_id(Theirs), 1, 1, Ours, Kek),
         ?assertEqual({error, {sealed_key, wrong_key_id}}, macula_node_keys:unseal(Sealed, Kek))
     end}.

a_header_whose_purpose_disagrees_with_the_key_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Sealed = crafted(macula_node_keys:key_id(Key), 5, 1, Key, Kek),
         ?assertEqual({error, {sealed_key, header_mismatch}}, macula_node_keys:unseal(Sealed, Kek))
     end}.

%%------------------------------------------------------------------
%% Refusals from the bytes
%%------------------------------------------------------------------

a_file_sealed_to_another_kek_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Sealed = macula_node_keys:seal(Key, kek()),
         ?assertEqual({error, {sealed_key, wrong_kek}}, macula_node_keys:unseal(Sealed, kek()))
     end}.

an_altered_file_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Sealed = macula_node_keys:seal(Key, Kek),
         ?assertEqual({error, sealed_key_not_authentic}, macula_node_keys:unseal(flip(Sealed, ?HEADER_BYTES + 10), Kek))
     end}.

an_altered_header_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Sealed = macula_node_keys:seal(Key, Kek),
         ?assertEqual({error, sealed_key_not_authentic}, macula_node_keys:unseal(flip(Sealed, 97), Kek))
     end}.

a_file_shorter_than_its_header_is_refused_test() ->
    ?assertEqual({error, sealed_key_bad_file}, macula_node_keys:unseal(binary:part(header_only(), 0, 60), kek())),
    %% Its KEK id is zero, so it is refused by name before a byte of it is decrypted.
    ?assertEqual({error, {sealed_key, wrong_kek}}, macula_node_keys:unseal(header_only(), kek())).

a_plaintext_file_is_not_a_sealed_file_test() ->
    ?assertEqual({error, bad_key_file}, macula_node_keys:unseal(<<"macula-node-key-v1\0", 1, 1, 1, 0:512>>, kek())).

a_kek_of_the_wrong_length_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Sealed = macula_node_keys:seal(Key, kek()),
         ?assertEqual({error, {kek, wrong_length}}, macula_node_keys:unseal(Sealed, <<0:128>>))
     end}.

%%------------------------------------------------------------------
%% Loading a file under a mode
%%------------------------------------------------------------------

a_sealed_file_loads_under_sealed_required_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Path = key_file("sealed", macula_node_keys:seal(Key, Kek)),
         ?assertEqual({ok, Key}, macula_node_keys:load(Path, identity, pq_pure,
                                                       #{kek_file => kek_file(Kek), mode => sealed_required}))
     end}.

a_plaintext_file_under_sealed_required_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = plaintext_file("plain", Key),
         ?assertEqual({error, plaintext_key_refused},
                      macula_node_keys:load(Path, identity, pq_pure, #{mode => sealed_required}))
     end}.

a_plaintext_file_under_accept_loads_and_is_counted_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = plaintext_file("accept", Key),
         Before = macula_node_keys:plaintext_loads(),
         ?assertEqual({ok, Key}, macula_node_keys:load(Path, identity, pq_pure, #{mode => accept})),
         ?assertEqual(Before + 1, macula_node_keys:plaintext_loads())
     end}.

a_sealed_load_is_not_counted_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Path = key_file("uncounted", macula_node_keys:seal(Key, Kek)),
         Before = macula_node_keys:plaintext_loads(),
         {ok, _} = macula_node_keys:load(Path, identity, pq_pure, #{kek_file => kek_file(Kek)}),
         ?assertEqual(Before, macula_node_keys:plaintext_loads())
     end}.

%%------------------------------------------------------------------
%% The KEK
%%------------------------------------------------------------------

a_kek_on_the_key_file_s_own_device_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Dir = tmp_dir(),
         Beside = filename:join(Dir, "kek.beside"),
         Kek = write_kek(Beside),
         Path = key_file("beside", macula_node_keys:seal(Key, Kek)),
         ?assertEqual({error, {kek, Beside, same_filesystem}},
                      macula_node_keys:load(Path, identity, pq_pure, #{kek_file => Beside}))
     end}.

a_kek_group_or_others_can_read_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = it_kek_file("readable"),
         Kek = write_kek(Path),
         ok = file:change_mode(Path, 8#0644),
         Sealed = key_file("readable", macula_node_keys:seal(Key, Kek)),
         ?assertEqual({error, {kek, Path, not_owner_only}},
                      macula_node_keys:load(Sealed, identity, pq_pure, #{kek_file => Path}))
     end}.

a_kek_of_the_wrong_length_on_disk_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = it_kek_file("short"),
         ok = file:write_file(Path, <<0:128>>),
         ok = file:change_mode(Path, 8#0600),
         Sealed = key_file("short", macula_node_keys:seal(Key, kek())),
         ?assertEqual({error, {kek, Path, wrong_length}},
                      macula_node_keys:load(Sealed, identity, pq_pure, #{kek_file => Path}))
     end}.

no_kek_named_anywhere_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Sealed = key_file("unconfigured", macula_node_keys:seal(Key, kek())),
         without_kek(fun() ->
             ?assertEqual({error, {kek, not_configured}}, macula_node_keys:load(Sealed, identity, pq_pure))
         end)
     end}.

the_kek_environment_variable_names_the_file_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         KekPath = kek_file(Kek),
         Sealed = key_file("from_env", macula_node_keys:seal(Key, Kek)),
         without_kek(fun() ->
             true = os:putenv("MACULA_KEY_KEK_FILE", binary_to_list(KekPath)),
             ?assertEqual({ok, Key}, macula_node_keys:load(Sealed, identity, pq_pure))
         end)
     end}.

%%------------------------------------------------------------------
%% Sealing a file in place
%%------------------------------------------------------------------

sealing_a_file_in_place_replaces_it_and_leaves_no_plaintext_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Path = plaintext_file("inplace", Key),
         ?assertEqual(ok, macula_node_keys:seal_file(Path, identity, pq_pure, kek_file(Kek))),
         {ok, Bytes} = file:read_file(Path),
         ?assertMatch(<<?SEALED_MAGIC, _/binary>>, Bytes),
         ?assertEqual([], temporaries(Path)),
         ?assertEqual({ok, Key}, macula_node_keys:load(Path, identity, pq_pure,
                                                       #{kek_file => kek_file(Kek), mode => sealed_required}))
     end}.

sealing_a_file_twice_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Kek = kek(),
         Path = plaintext_file("twice", Key),
         ok = macula_node_keys:seal_file(Path, identity, pq_pure, kek_file(Kek)),
         ?assertEqual({error, already_sealed}, macula_node_keys:seal_file(Path, identity, pq_pure, kek_file(Kek)))
     end}.

sealing_a_file_whose_key_is_not_the_one_asked_for_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = plaintext_file("wrong_purpose", Key),
         ?assertEqual({error, {wrong_purpose, identity}},
                      macula_node_keys:seal_file(Path, org, pq_pure, kek_file(kek())))
     end}.

a_key_saved_under_sealed_required_is_sealed_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = filename:join(tmp_dir(), "saved.key"),
         KekPath = kek_file(kek()),
         with_mode(sealed_required, fun() ->
             with_kek_setting(KekPath, fun() ->
                 ?assertEqual(ok, macula_node_keys:save(Path, Key)),
                 {ok, Bytes} = file:read_file(Path),
                 ?assertMatch(<<?SEALED_MAGIC, _/binary>>, Bytes),
                 ?assertEqual({ok, Key}, macula_node_keys:load(Path, identity, pq_pure,
                                                               #{kek_file => KekPath, mode => sealed_required}))
             end)
         end)
     end}.

a_key_saved_under_accept_with_no_kek_is_plaintext_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = filename:join(tmp_dir(), "plain_saved.key"),
         without_kek(fun() ->
             with_mode(accept, fun() -> ?assertEqual(ok, macula_node_keys:save(Path, Key)) end)
         end),
         {ok, Bytes} = file:read_file(Path),
         ?assertMatch(<<"macula-node-key-v1\0", _/binary>>, Bytes)
     end}.

%% A node that has no key yet grinds one; under sealed_required it is stored sealed, because a plaintext file would be
%% refused at the next start.
a_ground_identity_under_sealed_required_is_stored_sealed_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         Path = filename:join(tmp_dir(), "ground.key"),
         KekPath = kek_file(kek()),
         with_mode(sealed_required, fun() ->
             with_kek_setting(KekPath, fun() ->
                 {ok, Key} = macula_node_keys:node_identity(Path, pq_pure),
                 {ok, Bytes} = file:read_file(Path),
                 ?assertMatch(<<?SEALED_MAGIC, _/binary>>, Bytes),
                 ?assertEqual({ok, Key}, macula_node_keys:load(Path, identity, pq_pure,
                                                               #{kek_file => KekPath, mode => sealed_required}))
             end)
         end)
     end}.

a_ground_identity_under_sealed_required_without_a_kek_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         Path = filename:join(tmp_dir(), "ground_unconfigured.key"),
         without_kek(fun() ->
             with_mode(sealed_required, fun() ->
                 ?assertEqual({error, {kek, not_configured}}, macula_node_keys:node_identity(Path, pq_pure)),
                 ?assertEqual(false, filelib:is_regular(Path))
             end)
         end)
     end}.

%% A configured KEK seals what the box writes, whatever the mode: a box between step 2 and step 4 of section 6 is in
%% `accept', and a key it writes there in plaintext would be refused by its own flip to `sealed_required'.
a_key_saved_with_a_kek_configured_is_sealed_even_under_accept_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = filename:join(tmp_dir(), "sealed_while_accepting.key"),
         KekPath = kek_file(kek()),
         with_mode(accept, fun() ->
             with_kek_setting(KekPath, fun() ->
                 ?assertEqual(ok, macula_node_keys:save(Path, Key)),
                 {ok, Bytes} = file:read_file(Path),
                 ?assertMatch(<<?SEALED_MAGIC, _/binary>>, Bytes),
                 ?assertEqual({ok, Key}, macula_node_keys:load(Path, identity, pq_pure, #{kek_file => KekPath}))
             end)
         end)
     end}.

a_ground_identity_with_a_kek_configured_is_sealed_even_under_accept_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         Path = filename:join(tmp_dir(), "ground_accepting.key"),
         KekPath = kek_file(kek()),
         with_mode(accept, fun() ->
             with_kek_setting(KekPath, fun() ->
                 {ok, Key} = macula_node_keys:node_identity(Path, pq_pure),
                 {ok, Bytes} = file:read_file(Path),
                 ?assertMatch(<<?SEALED_MAGIC, _/binary>>, Bytes),
                 ?assertEqual({ok, Key}, macula_node_keys:load(Path, identity, pq_pure, #{kek_file => KekPath}))
             end)
         end)
     end}.

an_unknown_key_file_mode_is_refused_not_crashed_on_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = plaintext_file("unknown_mode", Key),
         ?assertEqual({error, {key_file_mode, sealed_if_you_like}},
                      macula_node_keys:load(Path, identity, pq_pure, #{mode => sealed_if_you_like}))
     end}.

%% A short-lived loader is the ordinary case: a pool asks for its identity, and the process that asked can be gone a
%% moment later. The count is a counters ref kept in persistent_term, which no process owns, so it stays.
the_plaintext_count_outlives_the_process_that_loaded_the_key_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
         {ok, Key} = macula_node_keys:generate(identity, pq_pure),
         Path = plaintext_file("outlives", Key),
         Before = macula_node_keys:plaintext_loads(),
         {Pid, Ref} = spawn_monitor(fun() ->
                                       {ok, _} = macula_node_keys:load(Path, identity, pq_pure, #{mode => accept})
                                   end),
         receive {'DOWN', Ref, process, Pid, _} -> ok
         after 30000 -> ?assert(false)
         end,
         ?assertEqual(Before + 1, macula_node_keys:plaintext_loads())
     end}.

%%------------------------------------------------------------------
%% The shared vectors
%%------------------------------------------------------------------

the_vectors_open_and_seal_to_the_same_bytes_test_() ->
    [?_test(check_vector(V)) || V <- vectors()].

the_vectors_are_refused_when_altered_test_() ->
    [?_test(check_altered(V)) || V <- vectors()].

check_vector(#{<<"kek">> := KekHex, <<"kek_id">> := KekIdHex, <<"nonce">> := NonceHex,
               <<"header">> := HeaderHex, <<"plaintext">> := PlaintextHex, <<"sealed">> := SealedHex,
               <<"key_id">> := KeyIdHex}) ->
    Kek = binary:decode_hex(KekHex),
    Nonce = binary:decode_hex(NonceHex),
    Sealed = binary:decode_hex(SealedHex),
    ?assertEqual(KekIdHex, hex(macula_node_keys:kek_id(Kek))),
    ?assertEqual(HeaderHex, hex(binary:part(Sealed, 0, ?HEADER_BYTES))),
    ?assertMatch(<<?SEALED_MAGIC, _/binary>>, Sealed),
    {ok, Key} = macula_node_keys:unseal(Sealed, Kek),
    ?assertEqual(KeyIdHex, hex(macula_node_keys:key_id(Key))),
    ?assertEqual(binary:decode_hex(PlaintextHex), plaintext_of(Key)),
    ?assertEqual(Sealed, macula_node_keys:seal(Key, Kek, Nonce)),
    ?assertEqual({error, {sealed_key, wrong_kek}},
                 macula_node_keys:unseal(Sealed, crypto:hash(sha256, <<"another kek">>))).

check_altered(#{<<"kek">> := KekHex, <<"sealed">> := SealedHex, <<"flipped_byte_offset">> := Offset}) ->
    Kek = binary:decode_hex(KekHex),
    Sealed = binary:decode_hex(SealedHex),
    ?assertEqual({error, sealed_key_not_authentic}, macula_node_keys:unseal(flip(Sealed, Offset), Kek)),
    ?assertEqual({error, sealed_key_bad_file},
                 macula_node_keys:unseal(binary:part(Sealed, 0, ?HEADER_BYTES - 1), Kek)).

the_vectors_pin_down_both_key_id_derivations_test() ->
    Purposes = [P || #{<<"purpose">> := P} <- vectors()],
    ?assertEqual([<<"identity">>, <<"org">>], Purposes).

sealed_key_id(<<_Magic:21/binary, _Version:8, _Purpose:8, _Profile:8, KeyId:32/binary, _/binary>>) ->
    KeyId.

sealed_node_id(Sealed, Kek) ->
    {ok, Key} = macula_node_keys:unseal(Sealed, Kek),
    macula_node_keys:node_id(Key).

%% A sealed file whose header names what it is told to, and whose tag is valid over that header: what a thief would
%% have to write to pass one key off as another.
crafted(KeyId, PurposeTag, ProfileTag, Key, Kek) ->
    Nonce = nonce(),
    Header = <<?SEALED_MAGIC, 1:8, PurposeTag:8, ProfileTag:8, KeyId/binary,
               (macula_node_keys:kek_id(Kek))/binary, Nonce/binary>>,
    <<Header/binary, (macula_seal:seal(Kek, Nonce, Header, plaintext_of(Key)))/binary>>.

header_only() ->
    <<?SEALED_MAGIC, 1:8, 1:8, 1:8, 0:(32 * 8), 0:(32 * 8), 0:(12 * 8)>>.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

kek() -> crypto:strong_rand_bytes(32).

nonce() -> crypto:strong_rand_bytes(12).

flip(Bytes, Offset) ->
    <<Before:Offset/binary, Byte:8, Rest/binary>> = Bytes,
    <<Before/binary, (Byte bxor 16#ff):8, Rest/binary>>.

hex(Bin) -> binary:encode_hex(Bin, lowercase).

%% The key file bytes a key is saved as: written by the one writer, and read back.
plaintext_of(Key) ->
    Path = filename:join(tmp_dir(), "plaintext.key"),
    without_kek(fun() -> ok = macula_node_keys:save(Path, Key) end),
    {ok, Bytes} = file:read_file(Path),
    Bytes.

%% A directory of this call's own, so no test reads another's files.
tmp_dir() ->
    Dir = filename:join([tmp_root(), "macula-k1", integer_to_list(erlang:unique_integer([positive]))]),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.

tmp_root() ->
    os:getenv("TMPDIR", "/tmp").

plaintext_file(Name, Key) ->
    Path = filename:join(tmp_dir(), Name ++ ".key"),
    ok = macula_node_keys:save(Path, Key),
    Path.

key_file(Name, Bytes) ->
    Path = filename:join(tmp_dir(), Name ++ ".key"),
    ok = file:write_file(Path, Bytes),
    ok = file:change_mode(Path, 8#0600),
    Path.

write_kek(Path) ->
    Kek = kek(),
    ok = file:write_file(Path, Kek),
    ok = file:change_mode(Path, 8#0600),
    Kek.

%% A KEK file on the other filesystem, holding Kek.
kek_file(Kek) ->
    Path = it_kek_file(binary:encode_hex(crypto:strong_rand_bytes(8), lowercase)),
    ok = file:write_file(Path, Kek),
    ok = file:change_mode(Path, 8#0600),
    Path.

it_kek_file(Name) ->
    Dir = filename:join([?KEK_DIR, "macula-k1", integer_to_list(erlang:unique_integer([positive]))]),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    filename:join(Dir, Name).

%% The leftovers of a seal that did not finish, which a completed one must not leave.
temporaries(Path) ->
    filelib:wildcard(Path ++ ".sealed.*").

with_mode(Mode, Test) ->
    Previous = application:get_env(macula, key_file_mode),
    ok = application:set_env(macula, key_file_mode, Mode),
    try Test()
    after restore_env(macula, key_file_mode, Previous)
    end.

with_kek_setting(Path, Test) ->
    Previous = application:get_env(macula, key_kek_file),
    ok = application:set_env(macula, key_kek_file, Path),
    try Test()
    after restore_env(macula, key_kek_file, Previous)
    end.

%% No KEK named by the application setting or the environment: what a box that has not wired one in has.
without_kek(Test) ->
    PreviousSetting = application:get_env(macula, key_kek_file),
    PreviousEnv = os:getenv("MACULA_KEY_KEK_FILE"),
    ok = application:unset_env(macula, key_kek_file),
    true = os:unsetenv("MACULA_KEY_KEK_FILE"),
    try Test()
    after
        restore_env(macula, key_kek_file, PreviousSetting),
        restore_os_env(PreviousEnv)
    end.

restore_env(App, Key, undefined) -> application:unset_env(App, Key);
restore_env(App, Key, {ok, Value}) -> application:set_env(App, Key, Value).

restore_os_env(false) -> os:unsetenv("MACULA_KEY_KEK_FILE");
restore_os_env(Value) -> os:putenv("MACULA_KEY_KEK_FILE", Value).

vectors() ->
    #{<<"vectors">> := Vectors} = vector_json(),
    Vectors.

vector_json() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

vector_file() ->
    Name = "sealed_key_v1.json",
    first_existing([filename:join("test/vectors", Name), filename:join("../../test/vectors", Name)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Name])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
