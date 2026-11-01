%% @doc A node's keys, one per purpose, in the node's crypto profile, stored as plan decision D6 describes, and
%% signing with them as decisions D4 and D7 describe.
%%
%% A node holds an identity key and a CONNECT key, and a station instance also holds a TLS key. Each key serves
%% exactly one purpose. A key is a list of components in the order of the profile's signature: ML-DSA-87 first,
%% then the classical half when the profile's signature for that purpose is hybrid. The TLS key is ML-DSA-87
%% alone in both profiles.
%%
%% A realm, an org and a foundation each hold a key of that purpose, which signs their records with the identity
%% key's algorithms.
%%
%% ML-DSA-87 signs, verifies and derives public keys through macula-mldsa, in macula_crypto_nif (D7, as amended on
%% 2026-09-22); RSA-PSS stays on OTP crypto.
%%
%% An ML-DSA-87 component stores its public key and its private key as the 32-byte seed (D6); a key generated before
%% D6's amendment keeps the 4,896-byte expanded form OTP made, which loads and signs as before. An RSA-PSS component
%% stores a DER-encoded RSAPrivateKey and RSAPublicKey. On load, every public key is derived again from its
%% private key and must equal the stored one, and every component passes a sign-and-verify round trip. A key file
%% its group or others can read is refused.
%%
%% A key file may also be sealed (MACULA-SEALED-KEY-V1, docs/design/DESIGN_SEALED_KEY_FILE.md): the same key bytes
%% sealed with AES-256-GCM under a key-encryption key the deployment supplies from off the key's volume, so that a
%% copied file is not a working identity. Sealing changes no identity, and a sealed file loads wherever a plaintext
%% one did. Under the key_file_mode setting `sealed_required' a plaintext file is refused; the default, `accept',
%% loads it and counts and logs the load.
%%
%% A process that holds keys shows them through redacted/1, and the primary logger filter redacted_log_event/2, which
%% install_log_redaction/0 puts in place, keeps their private halves out of crash and diagnostics reports.
%%
%% A key with one component signs with ML-DSA-87 alone, under an empty context. A hybrid key signs with the IETF LAMPS
%% composite id-MLDSA87-RSA4096-PSS-SHA512 (draft-ietf-lamps-pq-composite-sigs): both halves sign
%% M' = Prefix || Label || len(ctx) || ctx || SHA-512(M), with the label COMPSIG-MLDSA87-RSA4096-PSS-SHA512 and an
%% empty ctx, ML-DSA-87 with the label as its context string and RSA-PSS with SHA-384, MGF1 with SHA-384 and a
%% 48-byte salt. The signature is the ML-DSA-87 signature followed by the RSA-PSS signature, and the carried public
%% key is the ML-DSA-87 key followed by the DER RSAPublicKey. It is valid only if both halves verify. JOSE has no
%% name for it, so it travels under the alg ML-DSA-87-PS384 (D7).
%%
%% An identity key has a node_id: SHA-256 over the label MACULA-NODE-ID-V1, a zero byte, the length and ASCII name
%% of the profile, and the identity key as carried, and that node_id is its key id. Every other key has no
%% node_id, and its key id is SHA-256 over the label MACULA-KEY-ID-V1, a zero byte, the length and ASCII name of
%% the profile, and the key as carried.
%%
%% An identity key can be generated for a puzzle difficulty, so its node_id starts with that many zero bits. Each
%% try makes a new ML-DSA-87 half; a hybrid key keeps its RSA-PSS half across tries, since the node_id covers both
%% halves.
%%
%% See docs/design/DECISIONS_POST_QUANTUM.md, decisions D4, D5, D6 and D7.
-module(macula_node_keys).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

%% The id of the primary logger filter that redacts keys in log events.
-define(KEY_REDACTION, macula_key_redaction).

-export([
    generate/2,
    generate/3,
    node_identity/2,
    stored_identity/2,
    identity_env_checked/0,
    identity_dir/0,
    identity_path/3,
    save/2,
    load/3,
    load/4,
    seal/2,
    seal/3,
    unseal/2,
    seal_file/4,
    kek_id/1,
    key_file_mode/1,
    plaintext_loads/0,
    redacted/1,
    redacted_log_event/2,
    install_log_redaction/0,
    public_key/1,
    sign/2,
    verify/4,
    node_id/1,
    node_id/2,
    key_id/1,
    key_id/2,
    puzzle_solved/2,
    puzzle_difficulty/0,
    check_puzzle_difficulty/0,
    carried_key_well_formed/2,
    signature_bytes/1
]).

-ifdef(TEST).
-export([puzzle_candidate/1]).
-endif.

-export_type([purpose/0, algorithm/0, component/0, node_key/0, node_id/0, refusal/0]).

-type purpose()   :: identity | connect | tls | realm | org | foundation.
-type algorithm() :: mldsa87 | rsa_pss.
%% A node_id (D5): SHA-256 over the node_id label, the profile and the identity key as carried.
-type node_id()   :: <<_:256>>.
-type component() :: #{algorithm := algorithm(), public := binary(), private := binary()}.
-type node_key()  :: #{purpose    := purpose(),
                       profile    := macula_crypto_profile:profile(),
                       components := [component(), ...]}.
-type refusal()   :: bad_key_file
                   | key_file_permissions
                   | plaintext_key_refused
                   | already_sealed
                   | sealed_key_bad_file
                   | sealed_key_not_authentic
                   | sealed_key_read_back
                   | {sealed_key, wrong_kek | wrong_key_id | header_mismatch}
                   | {key_file_mode, term()}
                   | {key_id_mismatch, <<_:256>>}
                   | {kek, not_configured | wrong_length}
                   | {kek, file:name_all(), not_owner_only | same_filesystem | wrong_length | file:posix()}
                   | {unknown_purpose, term()}
                   | {crypto_profile_unknown, term()}
                   | {wrong_purpose, purpose()}
                   | {wrong_profile, macula_crypto_profile:profile()}
                   | {wrong_algorithms, [algorithm()]}
                   | {wrong_key_size, {pos_integer(), pos_integer()}}
                   | private_key_invalid
                   | public_key_mismatch
                   | round_trip_failed.

-define(KEY_FILE_MAGIC, "macula-node-key-v1\0").
-define(SEALED_KEY_MAGIC, "MACULA-SEALED-KEY-V1\0").
-define(SEALED_VERSION, 1).
-define(KEK_ID_LABEL, "MACULA-KEK-ID-V1").
-define(KEK_ENV, "MACULA_KEY_KEK_FILE").
-define(KEK_BYTES, 32).
-define(KEY_ID_BYTES, 32).
-define(NONCE_BYTES, 12).
-define(SEALED_HEADER_BYTES, (21 + 1 + 1 + 1 + 32 + 32 + 12)).
-define(PLAINTEXT_COUNTER, macula_node_keys_plaintext_counter).

%% The count of plaintext key loads is created when the code server loads this module, which it does once, so two
%% processes cannot race to create it and the count has no owning process to die with. It is kept in persistent_term
%% because a key file can be loaded before the application starts (see stored_identity/2), when there is no supervisor
%% to own a table.
-on_load(init_plaintext_counter/0).
-define(MLDSA87_PUBLIC_BYTES, 2592).
-define(MLDSA87_SIGNATURE_BYTES, 4627).
-define(COMPOSITE_PREFIX, "CompositeAlgorithmSignatures2025").
-define(COMPOSITE_LABEL, "COMPSIG-MLDSA87-RSA4096-PSS-SHA512").
-define(NODE_ID_LABEL, "MACULA-NODE-ID-V1").
-define(KEY_ID_LABEL, "MACULA-KEY-ID-V1").
-define(PUZZLE_DIFFICULTY, 8).

%%------------------------------------------------------------------
%% Generation
%%------------------------------------------------------------------

%% @doc Generate the key for a purpose in a profile.
-spec generate(purpose(), macula_crypto_profile:profile()) -> {ok, node_key()} | {error, refusal()}.
generate(Purpose, Profile) ->
    generated_key(expected_algorithms(Purpose, Profile), Purpose, Profile).

%% @doc Generate the key for a purpose in a profile, with options. With puzzle_difficulty, generate an identity key
%% whose node_id meets that difficulty (see puzzle_solved/2), in about 2^Difficulty tries.
-spec generate(purpose(), macula_crypto_profile:profile(), #{puzzle_difficulty => 0..256}) ->
        {ok, node_key()} | {error, refusal() | not_an_identity_key}.
generate(identity, Profile, #{puzzle_difficulty := Difficulty} = Options)
  when map_size(Options) =:= 1, is_integer(Difficulty), Difficulty >= 0, Difficulty =< 256 ->
    solved_key(generate(identity, Profile), Difficulty);
generate(Purpose, _Profile, #{puzzle_difficulty := _})
  when Purpose =:= connect; Purpose =:= tls; Purpose =:= realm; Purpose =:= org; Purpose =:= foundation ->
    {error, not_an_identity_key};
generate(Purpose, Profile, Options) when map_size(Options) =:= 0 ->
    generate(Purpose, Profile).

%%------------------------------------------------------------------
%% Persistence
%%------------------------------------------------------------------

%% @doc The identity a program under this account stores for `Name' in `Profile' (macula#76): loaded from
%% `identity_path(identity_dir(), Name, Profile)', or ground and stored there if there is none, with the path it is
%% stored at.
%%
%% A node identity belongs to one program running under one user account. A key works in one profile only, so a
%% program that runs in both profiles is two nodes, and its two keys sit side by side as `<name>.pq_pure.key' and
%% `<name>.pq_hybrid.key'. `Name' is `<<"default">>' unless the program names itself.
%%
%% The single `identity.key' of earlier releases, next to the identity directory, is moved first to
%% `default.<its profile>.key' and is never read in place. The move only ever creates: when that place holds another
%% key it is refused as `{old_identity_key, #{from, to}, place_taken}' and both files stay as they are. An old key that
%% will not load is refused as `{old_identity_key, #{from}, Reason}', untouched. A stored key that will not load is
%% refused as `{stored_key, Path, Reason}' and never replaced (see `node_identity/2').
%%
%% ⚠ THIS RUNS BEFORE THE APPLICATION IS STARTED. `macula_dist' is the `-proto_dist macula' driver, so net_kernel
%% calls its `listen/1' during KERNEL startup when the node is named on the command line, and it starts no
%% application: there is no `ensure_all_started' anywhere in `macula_dist_system'. A supervised owner process would
%% not exist to ask at that moment, which is why the serialisation is in the FILESYSTEM and not in a process.
-spec stored_identity(binary(), macula_crypto_profile:profile()) ->
        {ok, node_key(), file:filename()} | {error, term()}.
stored_identity(Name, Profile) ->
    in_layout(identity_env_checked(), Name, Profile).

in_layout(ok, Name, Profile) ->
    Dir = identity_dir(),
    named(identity_path(Dir, Name, Profile), Dir, Profile);
in_layout({error, _} = Refusal, _Name, _Profile) ->
    Refusal.

%% @doc `ok', unless the `node_identity_path' application env that macula#76 removed is still set: then
%% `{error, {node_identity_path_removed, #{node_identity_path, use_instead}}}', naming the setting and what replaces
%% it. A consumer that still sets it, as a station harness does with one key per station, would otherwise fall back to
%% the account's shared directory without a word: several stations, one key, one node_id, the unnoticed identity change
%% #76 exists to prevent. The application's start and every stored identity refuse with it.
-spec identity_env_checked() -> ok | {error, {node_identity_path_removed, map()}}.
identity_env_checked() ->
    removed_env(application:get_env(macula, node_identity_path)).

removed_env(undefined) ->
    ok;
removed_env({ok, Path}) ->
    {error, {node_identity_path_removed, #{node_identity_path => Path, use_instead => [identity_dir, identity_name]}}}.

named({ok, Path}, Dir, Profile) ->
    in_place(old_key_moved(filename:join(filename:dirname(Dir), "identity.key"), Dir), Path, Profile);
named({error, _} = Refusal, _Dir, _Profile) ->
    Refusal.

in_place(ok, Path, Profile) ->
    stored_at(node_identity(Path, Profile), Path);
in_place({error, _} = Refusal, _Path, _Profile) ->
    Refusal.

stored_at({ok, Key}, Path) -> {ok, Key, Path};
stored_at({error, Reason}, Path) -> {error, {stored_key, Path, Reason}}.

%% @doc Where identities are stored: the `identity_dir' macula application env, or `identity' in the platform's
%% per-user data directory. `filename:basedir/2' rather than a path of our own invention, so the files land where the
%% platform says a user's application data belongs.
-spec identity_dir() -> file:filename().
identity_dir() ->
    application:get_env(macula, identity_dir, filename:join(filename:basedir(user_data, "macula"), "identity")).

%% @doc The file of the identity `Name' in `Profile' under `Dir': `<Name>.<Profile>.key'. A name is 1 to 64 lowercase
%% letters, digits, `-' and `_', starting with a letter or digit, so it can name no other directory and no other
%% profile's key; any other is refused as `{identity_name, invalid}'.
-spec identity_path(file:filename(), term(), macula_crypto_profile:profile()) ->
        {ok, file:filename()} | {error, {identity_name, invalid}}.
identity_path(Dir, Name, Profile) when is_binary(Name) ->
    named_path(re:run(Name, "^[a-z0-9][a-z0-9_-]{0,63}$", [{capture, none}]), Dir, Name, Profile);
identity_path(_Dir, _Name, _Profile) ->
    {error, {identity_name, invalid}}.

named_path(match, Dir, Name, Profile) ->
    {ok, filename:join(Dir, binary_to_list(Name) ++ "." ++ atom_to_list(Profile) ++ ".key")};
named_path(nomatch, _Dir, _Name, _Profile) ->
    {error, {identity_name, invalid}}.

%% The old single key file, moved to `default.<its profile>.key' when it is there. Its profile is the one it loads in:
%% a key that loads in neither is refused, naming it.
old_key_moved(Old, Dir) ->
    old_key_found(file:read_file_info(Old), Old, Dir).

old_key_found({error, enoent}, _Old, _Dir) ->
    ok;
old_key_found({ok, _Info}, Old, Dir) ->
    [Profile | _] = macula_crypto_profile:profiles(),
    old_key_loaded(old_key_profile(load(Old, identity, Profile), Old), Old, Dir);
old_key_found({error, Reason}, Old, _Dir) ->
    {error, {old_identity_key, #{from => Old}, Reason}}.

old_key_profile({ok, #{profile := Profile}}, _Old) ->
    {ok, Profile};
old_key_profile({error, {wrong_profile, Found}}, Old) ->
    old_key_profile(load(Old, identity, Found), Old);
old_key_profile({error, _} = Refusal, _Old) ->
    Refusal.

old_key_loaded({ok, Profile}, Old, Dir) ->
    {ok, To} = identity_path(Dir, <<"default">>, Profile),
    old_key_linked(link_new(Old, To), Old, To);
old_key_loaded({error, Reason}, Old, _Dir) ->
    {error, {old_identity_key, #{from => Old}, Reason}}.

%% A link only ever creates, so a key already in the new place is never replaced. The same file already there is a
%% move cut short between the link and the removal of the old name, and is finished.
link_new(Old, To) ->
    linked_into(filelib:ensure_dir(To), Old, To).

linked_into(ok, Old, To) -> file:make_link(Old, To);
linked_into({error, _} = Failed, _Old, _To) -> Failed.

old_key_linked(ok, Old, _To) ->
    file:delete(Old);
old_key_linked({error, eexist}, Old, To) ->
    same_file(same_contents(file:read_file(Old), file:read_file(To)), Old, To);
old_key_linked({error, Reason}, Old, To) ->
    {error, {old_identity_key, #{from => Old, to => To}, Reason}}.

same_contents({ok, Bytes}, {ok, Bytes}) -> true;
same_contents(_Old, _To) -> false.

same_file(true, Old, _To) -> file:delete(Old);
same_file(false, Old, To) -> {error, {old_identity_key, #{from => Old, to => To}, place_taken}}.

%% @doc The identity key stored at `Path': loaded, or ground and stored there if there is none. Two callers that find
%% no file at once end up with the same key (see `claimed/4'), and a file that will not load is refused, never
%% replaced. Exported so a test can point a node at its own key file instead of the account's: a test that used the
%% account's would read, and on a fresh machine WRITE, the identity of whatever is running the suite.
-spec node_identity(file:name_all(), macula_crypto_profile:profile()) -> {ok, node_key()} | {error, term()}.
node_identity(Path, Profile) ->
    stored_or_ground(load(Path, identity, Profile), Path, Profile).

stored_or_ground({ok, _Key} = Stored, _Path, _Profile) ->
    Stored;
stored_or_ground({error, enoent}, Path, Profile) ->
    ground_and_store(generate(identity, Profile, #{puzzle_difficulty => puzzle_difficulty()}), Path, Profile);
%% ⛔ A FILE THAT EXISTS AND WILL NOT LOAD IS NEVER OVERWRITTEN. Wrong profile, wrong permissions, corrupt,
%% truncated: every one is refused and left exactly as it is. Grinding a replacement would be the single most
%% destructive thing this module could do — the node's permissions point at an id it could then no longer prove, and
%% the evidence of what went wrong would be gone. An operator can move the file aside deliberately; this cannot do it
%% for them.
%%
%% ⚠ THIS CLAUSE IS NOT WHAT MAKES THAT SAFE, AND SAYING SO MATTERS. `create_new/2' refuses to replace an existing
%% file, so even without this clause a ground replacement could not land. Both were mutated to find out: removing
%% either one alone leaves the file intact and every test green, and only removing BOTH destroys it. So the two are
%% independent guards and this one's real work is smaller and worth stating honestly — it returns the ORIGINAL
%% refusal, which says what is wrong with the file, instead of an `eexist' from the store followed by the same load
%% failing again, and it skips a pointless grind on the way.
stored_or_ground({error, _Refusal} = Refused, _Path, _Profile) ->
    Refused.

ground_and_store({ok, Key}, Path, Profile) ->
    claimed(create_new(Path, Key), Key, Path, Profile);
ground_and_store({error, _} = Failed, _Path, _Profile) ->
    Failed.

%% ⚠ THE LOSER OF A RACE USES THE WINNER'S KEY, and that is the whole point. Two pools, or a pool and a distribution
%% tunnel, can start at once: `macula_client:connect/2' is a plain `gen_server:start_link', so `init/1' runs in the
%% new process and nothing serialises two of them. Both would find no file and both would grind.
%%
%% `create_new/2' only ever creates, never replaces, so exactly one of them stores a key and the other is told
%% `eexist' and reads back what the winner stored. Both then hold the SAME key. A plain save would have let the later
%% writer replace the file while the earlier caller carried on holding the key it ground, which is two node_ids on one
%% node: the exact thing this design exists to prevent, arriving as a rare startup flake.
%%
%% That is not a guess about a race that might exist. Eight concurrent first starts are driven in
%% `macula_client_node_identity_tests', and with `create_new/2' mutated to replace rather than refuse they come back
%% with more than one node_id.
%%
%% The cost of losing is one wasted grind, about a second of CPU, once, and never a wrong identity.
claimed(ok, Key, _Path, _Profile) ->
    {ok, Key};
claimed({error, eexist}, _Ours, Path, Profile) ->
    load(Path, identity, Profile);
claimed({error, _} = Failed, _Key, _Path, _Profile) ->
    Failed.

%% Creates `Path' with `Blob', and REFUSES to replace an existing file.
%%
%% The temporary is named with both a VM-unique integer and the OS pid, so it is unique across two Erlang nodes
%% sharing one path as well as within one VM. The content is written there, in the same directory so the link below
%% stays on one filesystem, and two writers cannot corrupt each other's, and is linked into place only if nothing is there: `file:make_link/2' is atomic and answers `eexist'
%% rather than replacing. `save/2' cannot be used here — it renames, and a rename REPLACES.
%%
%% The link rather than a rename is also what stops a reader seeing a half-written file: the name appears only once
%% the content behind it is complete.
create_new(Path, Key) ->
    created(storage_bytes(Path, Key), Path).

created({ok, Blob}, Path) ->
    Tmp = iolist_to_binary([Path, ".new.", integer_to_binary(erlang:unique_integer([positive])),
                            ".", list_to_binary(os:getpid())]),
    linked(write_restricted(filelib:ensure_dir(Path), Tmp, Tmp, Blob), Tmp, Path);
created({error, _} = Failed, _Path) ->
    Failed.

linked(ok, Tmp, Path) ->
    Result = file:make_link(Tmp, Path),
    _ = file:delete(Tmp),
    Result;
linked({error, _} = Failed, Tmp, _Path) ->
    _ = file:delete(Tmp),
    Failed.

%% @doc Save a key atomically. The temporary file is restricted to its owner before the key is written into it. The
%% bytes are sealed when the key file mode is `sealed_required', and plaintext under `accept'.
-spec save(file:name_all(), node_key()) -> ok | {error, term()}.
save(Path, #{purpose := _, profile := _, components := [_ | _]} = Key) ->
    saved(storage_bytes(Path, Key), Path).

saved({ok, Blob}, Path) ->
    Tmp = iolist_to_binary([Path, ".tmp"]),
    write_restricted(filelib:ensure_dir(Path), Tmp, Path, Blob);
saved({error, _} = Failed, _Path) ->
    Failed.

%% @doc Load the key saved for a purpose in a profile, and check it before returning it. A key file its group or others
%% can read is refused. A sealed file is unsealed with the configured KEK; see load/4 for the options that name one.
-spec load(file:name_all(), purpose(), macula_crypto_profile:profile()) ->
        {ok, node_key()} | {error, refusal() | file:posix() | badarg | terminated | system_limit}.
load(Path, Purpose, Profile) ->
    load(Path, Purpose, Profile, #{}).

%% @doc Load a key file, with options. `kek_file' names the file the KEK is read from, `mode' overrides the
%% application's key_file_mode setting (`accept' or `sealed_required'), and `key_id' names the key the caller must get,
%% so a file holding any other is refused as {key_id_mismatch, Expected} rather than loaded.
-spec load(file:name_all(), purpose(), macula_crypto_profile:profile(), map()) ->
        {ok, node_key()} | {error, refusal() | file:posix() | badarg | terminated | system_limit}.
load(Path, Purpose, Profile, Options) ->
    loaded_key(key_file_mode(Options), Path, Purpose, Profile, Options).

%% The mode is checked once, here, before the file is read: a mode nobody recognises refuses a sealed file and a
%% plaintext one alike, instead of the sealed path having no opinion while the plaintext path refused.
loaded_key({invalid, Mode}, _Path, _Purpose, _Profile, _Options) ->
    {error, {key_file_mode, Mode}};
loaded_key(_Mode, Path, Purpose, Profile, Options) ->
    expected_key_id(checked_key(owner_only_read(file:read_file_info(Path), Path, Options), Purpose, Profile),
                    Options).

%% A caller that knows which key it must get names it, and a file holding any other is refused whatever the KEK
%% opens: the header's key id can only speak for the file it is in, not for the one the caller meant
%% (DESIGN_SEALED_KEY_FILE.md, section 6).
expected_key_id({ok, Key} = Loaded, #{key_id := Expected}) ->
    named_key_id(key_id(Key) =:= Expected, Expected, Loaded);
expected_key_id(Loaded, _Options) ->
    Loaded.

named_key_id(true, _Expected, Loaded) -> Loaded;
named_key_id(false, Expected, _Loaded) -> {error, {key_id_mismatch, Expected}}.

%%------------------------------------------------------------------
%% Sealed key files
%%------------------------------------------------------------------

%% @doc The id of a key-encryption key (DESIGN_SEALED_KEY_FILE.md, section 3): SHA-256 over the label
%% MACULA-KEK-ID-V1, a zero byte, and the KEK. It names which KEK a sealed file was sealed to, so a box that has been
%% given a new KEK refuses with that, and not as a tampered file.
-spec kek_id(<<_:256>>) -> <<_:256>>.
kek_id(Kek) when is_binary(Kek) ->
    crypto:hash(sha256, <<?KEK_ID_LABEL, 0:8, Kek/binary>>).

%% @doc The mode a key file is loaded under: `#{mode := Mode}' when the caller names one, else the macula
%% application's `key_file_mode' setting, whose default is `accept'. `sealed_required' refuses a plaintext key file.
-spec key_file_mode(map()) -> accept | sealed_required | {invalid, term()}.
key_file_mode(#{mode := Mode}) -> valid_mode(Mode);
key_file_mode(_Options) -> valid_mode(application:get_env(macula, key_file_mode, accept)).

valid_mode(accept) -> accept;
valid_mode(sealed_required) -> sealed_required;
valid_mode(Other) -> {invalid, Other}.

%% @doc How many plaintext key files have been loaded since this module was loaded. A box that has gone through the
%% fleet's sealing shows zero; a count that keeps moving is a key file that was never sealed (WP-K3's fleet check reads
%% it). The count is a counters ref, a resource no process owns, so it does not go back to zero when the process that
%% loaded the key exits.
-spec plaintext_loads() -> non_neg_integer().
plaintext_loads() ->
    counters:get(plaintext_counter(), 1).

plaintext_load_counted(Path) ->
    logger:notice(#{msg => {report, #{label => {macula_node_keys, plaintext_key_loaded}, path => Path}}}),
    counters:add(plaintext_counter(), 1, 1),
    ok.

plaintext_counter() ->
    persistent_term:get(?PLAINTEXT_COUNTER).

%% Created when the code server loads this module (see the on_load attribute above), which it does once, so two
%% processes cannot race to create it. A hot reload finds it present and keeps it, so an upgrade does not reset the
%% count either.
init_plaintext_counter() ->
    initialized_counter(held_plaintext_counter()).

held_plaintext_counter() ->
    case persistent_term:get(?PLAINTEXT_COUNTER, undefined) of
        undefined -> none;
        Ref -> {held, Ref}
    end.

initialized_counter(none) ->
    persistent_term:put(?PLAINTEXT_COUNTER, counters:new(1, [write_concurrency])),
    ok;
initialized_counter({held, _Ref}) ->
    ok.

%% @doc The sealed form of a key (MACULA-SEALED-KEY-V1): the header, then the plaintext key file sealed under `Kek'
%% with a fresh nonce. Sealing changes no identity: the key id inside stays the one the key already had.
-spec seal(node_key(), <<_:256>>) -> binary().
seal(Key, Kek) ->
    seal(Key, Kek, crypto:strong_rand_bytes(?NONCE_BYTES)).

%% @doc The sealed form of a key under a given nonce. This is the writer the cross-SDK vectors are made with, so that
%% another implementation reproduces them byte for byte.
-spec seal(node_key(), <<_:256>>, <<_:96>>) -> binary().
seal(#{purpose := Purpose, profile := Profile} = Key, Kek, Nonce) ->
    Header = sealed_header(Purpose, Profile, key_id(Key), kek_id(Kek), Nonce),
    <<Header/binary, (macula_seal:seal(Kek, Nonce, Header, encode(Key)))/binary>>.

%% @doc The key in a sealed file, or the refusal that names what is wrong with it: not a sealed file, a KEK other than
%% the one it was sealed to, a file altered since it was sealed, or a file whose header and key disagree.
-spec unseal(binary(), <<_:256>>) -> {ok, node_key()} | {error, refusal()}.
unseal(Sealed, Kek) when is_binary(Kek), byte_size(Kek) =:= ?KEK_BYTES ->
    sealed_file(Sealed, Kek);
unseal(_Sealed, _Kek) ->
    {error, {kek, wrong_length}}.

sealed_file(<<?SEALED_KEY_MAGIC, _/binary>> = Sealed, Kek) ->
    sealed_ciphertext(Sealed, Kek);
sealed_file(_NotSealed, _Kek) ->
    {error, bad_key_file}.

sealed_ciphertext(<<Header:(?SEALED_HEADER_BYTES)/binary, Ciphertext/binary>>, Kek) ->
    sealed_unsealed(sealed_header_fields(Header), Ciphertext, Header, Kek);
sealed_ciphertext(_TooShort, _Kek) ->
    {error, sealed_key_bad_file}.

sealed_unsealed(error, _Sealed, _Header, _Kek) ->
    {error, sealed_key_bad_file};
sealed_unsealed({ok, {Purpose, Profile, KeyId, KekId, Nonce}}, Sealed, Header, Kek) ->
    sealed_authentic(KekId =:= kek_id(Kek), {Purpose, Profile, KeyId}, Nonce, Sealed, Header, Kek).

sealed_authentic(false, _Named, _Nonce, _Sealed, _Header, _Kek) ->
    {error, {sealed_key, wrong_kek}};
sealed_authentic(true, {Purpose, Profile, KeyId}, Nonce, Sealed, Header, Kek) ->
    sealed_key(macula_seal:open(Kek, Nonce, Header, Sealed), Purpose, Profile, KeyId).

sealed_key({error, sealed_refused}, _Purpose, _Profile, _KeyId) ->
    {error, sealed_key_not_authentic};
sealed_key({ok, Plain}, Purpose, Profile, KeyId) ->
    sealed_named(decode(Plain), Purpose, Profile, KeyId).

sealed_named({ok, #{purpose := Purpose, profile := Profile} = Key}, Purpose, Profile, KeyId) ->
    sealed_key_id(KeyId =:= key_id(Key), Key);
sealed_named({ok, _Other}, _Purpose, _Profile, _KeyId) ->
    {error, {sealed_key, header_mismatch}};
sealed_named({error, _} = Error, _Purpose, _Profile, _KeyId) ->
    Error.

sealed_key_id(true, Key) -> {ok, Key};
sealed_key_id(false, _Key) -> {error, {sealed_key, wrong_key_id}}.

sealed_header(Purpose, Profile, KeyId, KekId, Nonce) ->
    <<?SEALED_KEY_MAGIC, ?SEALED_VERSION:8, (purpose_tag(Purpose)):8, (profile_tag(Profile)):8,
      KeyId:?KEY_ID_BYTES/binary, KekId:?KEY_ID_BYTES/binary, Nonce:?NONCE_BYTES/binary>>.

sealed_header_fields(<<?SEALED_KEY_MAGIC, ?SEALED_VERSION:8, PurposeTag:8, ProfileTag:8,
                       KeyId:?KEY_ID_BYTES/binary, KekId:?KEY_ID_BYTES/binary, Nonce:?NONCE_BYTES/binary>>) ->
    sealed_tags(tag_purpose(PurposeTag), tag_profile(ProfileTag), KeyId, KekId, Nonce);
sealed_header_fields(_Header) ->
    error.

sealed_tags({ok, Purpose}, {ok, Profile}, KeyId, KekId, Nonce) ->
    {ok, {Purpose, Profile, KeyId, KekId, Nonce}};
sealed_tags(_Purpose, _Profile, _KeyId, _KekId, _Nonce) ->
    error.

%% @doc Seal the key file at `Path' in place, with the KEK at `KekPath'. The sealed file is written beside the old
%% one, synced, read back and unsealed to the same key id, and only then renamed over it; a failure at any step leaves
%% the plaintext as it was. A file that is already sealed is refused rather than sealed again.
-spec seal_file(file:name_all(), purpose(), macula_crypto_profile:profile(), file:name_all()) -> ok | {error, term()}.
seal_file(Path, Purpose, Profile, KekPath) ->
    sealing(file:read_file(Path), Path, Purpose, Profile, KekPath).

sealing({ok, <<?SEALED_KEY_MAGIC, _/binary>>}, _Path, _Purpose, _Profile, _KekPath) ->
    {error, already_sealed};
sealing({ok, _Plain}, Path, Purpose, Profile, KekPath) ->
    with_plaintext_key(plaintext_key(Path, Purpose, Profile), Path, KekPath);
sealing({error, Reason}, _Path, _Purpose, _Profile, _KekPath) ->
    {error, Reason}.

with_plaintext_key({ok, Key}, Path, KekPath) ->
    with_sealing_kek(key_kek(KekPath, key_device(Path)), Key, Path);
with_plaintext_key({error, _} = Error, _Path, _KekPath) ->
    Error.

with_sealing_kek({ok, Kek}, Key, Path) ->
    write_then_replace(Path, seal(Key, Kek), Key, Kek);
with_sealing_kek({error, _} = Error, _Key, _Path) ->
    Error.

write_then_replace(Path, Sealed, Key, Kek) ->
    Tmp = temp_path(Path),
    replaced(write_synced(filelib:ensure_dir(Path), Tmp, Sealed), Tmp, Path, Key, Kek).

replaced(ok, Tmp, Path, Key, Kek) ->
    replaced_after_read_back(read_back(Tmp, Key, Kek), Tmp, Path);
replaced({error, _} = Error, Tmp, _Path, _Key, _Kek) ->
    _ = file:delete(Tmp),
    Error.

read_back(Tmp, Key, Kek) ->
    case file:read_file(Tmp) of
        {ok, Sealed} -> read_back_key(unseal(Sealed, Kek), Key);
        {error, Reason} -> {error, Reason}
    end.

read_back_key({ok, SealedKey}, Key) ->
    read_back_matches(key_id(SealedKey) =:= key_id(Key));
read_back_key({error, _} = Error, _Key) ->
    Error.

read_back_matches(true) -> ok;
read_back_matches(false) -> {error, sealed_key_read_back}.

replaced_after_read_back(ok, Tmp, Path) ->
    file:rename(Tmp, Path);
replaced_after_read_back({error, _} = Error, Tmp, _Path) ->
    _ = file:delete(Tmp),
    Error.

%% The plaintext key at Path, checked as load/4 checks it, but without the mode gate: sealing a file is what happens
%% before a box is set to sealed_required, and must work while it is still accept.
plaintext_key(Path, Purpose, Profile) ->
    checked_key(plaintext_read(file:read_file_info(Path), Path), Purpose, Profile).

plaintext_read({ok, #file_info{mode = Mode}}, Path) when Mode band 8#077 =:= 0 ->
    decode_file(file:read_file(Path));
plaintext_read({ok, #file_info{}}, _Path) ->
    {error, key_file_permissions};
plaintext_read({error, _} = Error, _Path) ->
    Error.

%% The KEK's path: what the caller named, else the macula application's key_kek_file setting, else the
%% MACULA_KEY_KEK_FILE the deployment sets in the service's environment.
kek_file(#{kek_file := Path}) ->
    {ok, Path};
kek_file(_Options) ->
    configured_kek(application:get_env(macula, key_kek_file), os:getenv(?KEK_ENV)).

configured_kek({ok, Path}, _EnvPath) -> {ok, Path};
configured_kek(undefined, false) -> {error, {kek, not_configured}};
configured_kek(undefined, EnvPath) -> {ok, EnvPath}.

%% The KEK at KekPath, refused when it is not the owner's, when it sits on the same device as the key file it would
%% unseal (a whole-disk copy would then carry both), or when it is not 32 bytes.
key_kek(KekPath, KeyDevice) ->
    kek_checks(file:read_file_info(KekPath), KekPath, KeyDevice).

kek_checks({ok, #file_info{mode = Mode} = Info}, KekPath, KeyDevice) when Mode band 8#077 =:= 0 ->
    kek_device(Info, KekPath, KeyDevice);
kek_checks({ok, #file_info{}}, KekPath, _KeyDevice) ->
    {error, {kek, KekPath, not_owner_only}};
kek_checks({error, Reason}, KekPath, _KeyDevice) ->
    {error, {kek, KekPath, Reason}}.

kek_device(#file_info{major_device = Device}, KekPath, Device) ->
    {error, {kek, KekPath, same_filesystem}};
kek_device(_Info, KekPath, _KeyDevice) ->
    kek_bytes(file:read_file(KekPath), KekPath).

kek_bytes({ok, <<_:32/binary>> = Kek}, _KekPath) -> {ok, Kek};
kek_bytes({ok, _ShortOrLong}, KekPath) -> {error, {kek, KekPath, wrong_length}};
kek_bytes({error, Reason}, KekPath) -> {error, {kek, KekPath, Reason}}.

%% The device a key file sits on, or the device of the directory it will be created in: a key file being written for
%% the first time has no device of its own yet.
key_device(Path) ->
    key_device_of(file:read_file_info(Path), Path).

key_device_of({ok, #file_info{major_device = Device}}, _Path) ->
    Device;
key_device_of({error, _}, Path) ->
    {ok, #file_info{major_device = Device}} = file:read_file_info(filename:dirname(Path)),
    Device.

%% Written, then synced, so that what is read back below is what is on the disk and not what is in the page cache.
write_synced(ok, Tmp, Blob) ->
    synced(filled(restrict(file:write_file(Tmp, <<>>, [raw, binary]), Tmp), Tmp, Blob), Tmp);
write_synced({error, _} = Error, _Tmp, _Blob) ->
    Error.

filled(ok, Tmp, Blob) -> file:write_file(Tmp, Blob, [raw, binary]);
filled({error, _} = Error, _Tmp, _Blob) -> Error.

synced(ok, Tmp) ->
    opened_for_sync(file:open(Tmp, [read, write, raw, binary]), Tmp);
synced({error, _} = Error, _Tmp) ->
    Error.

opened_for_sync({ok, Fd}, _Tmp) ->
    Result = file:sync(Fd),
    ok = file:close(Fd),
    Result;
opened_for_sync({error, Reason}, _Tmp) ->
    {error, Reason}.

temp_path(Path) ->
    iolist_to_binary([Path, ".sealed.", integer_to_binary(erlang:unique_integer([positive])),
                      ".", list_to_binary(os:getpid())]).

%%------------------------------------------------------------------
%% Internals: reading a sealed or plaintext key file
%%------------------------------------------------------------------

%% A sealed file is unsealed with the KEK, which is checked against the device the key file sits on. A plaintext one
%% loads or is refused by the mode, and under `accept' the load is counted and logged.
key_file({ok, <<?SEALED_KEY_MAGIC, _/binary>> = Sealed}, #file_info{major_device = Device}, _Path, Options) ->
    sealed_at(Sealed, Device, Options);
key_file({ok, Plain}, _Info, Path, Options) ->
    plaintext_at(Plain, Path, Options);
key_file({error, _} = Error, _Info, _Path, _Options) ->
    Error.

sealed_at(Sealed, Device, Options) ->
    sealed_with_kek(kek_file(Options), Sealed, Device).

sealed_with_kek({ok, KekPath}, Sealed, Device) ->
    sealed_with(key_kek(KekPath, Device), Sealed);
sealed_with_kek({error, _} = Error, _Sealed, _Device) ->
    Error.

sealed_with({ok, Kek}, Sealed) ->
    unseal(Sealed, Kek);
sealed_with({error, _} = Error, _Sealed) ->
    Error.

plaintext_at(Plain, Path, Options) ->
    plaintext_allowed(key_file_mode(Options), Plain, Path).

plaintext_allowed(accept, Plain, Path) ->
    plaintext_load_counted(Path),
    decode(Plain);
plaintext_allowed(sealed_required, _Plain, _Path) ->
    {error, plaintext_key_refused};
plaintext_allowed({invalid, Mode}, _Plain, _Path) ->
    {error, {key_file_mode, Mode}}.

%% The bytes a key is stored as. A KEK that is configured seals them whatever the mode: a box in the middle of the
%% rollout (the KEK wired in and its keys sealed, still in `accept') would otherwise keep writing the connect and TLS
%% keys a statement issuer rewrites and every new ground identity in plaintext, and the flip to `sealed_required'
%% would refuse its own fresh files at the next start. With no KEK, `accept' writes plaintext and `sealed_required'
%% refuses rather than write a file the next start would refuse.
storage_bytes(Path, Key) ->
    stored_form(kek_file(#{}), key_file_mode(#{}), filelib:ensure_dir(Path), Path, Key).

stored_form(_Kek, _Mode, {error, _} = Error, _Path, _Key) ->
    Error;
stored_form({ok, KekPath}, Mode, ok, Path, Key) when Mode =:= accept; Mode =:= sealed_required ->
    sealed_key_bytes(key_kek(KekPath, key_device(Path)), Key);
stored_form({error, _}, accept, ok, _Path, Key) ->
    {ok, encode(Key)};
stored_form({error, _} = NoKek, sealed_required, ok, _Path, _Key) ->
    NoKek;
stored_form(_Kek, {invalid, Mode}, ok, _Path, _Key) ->
    {error, {key_file_mode, Mode}}.

sealed_key_bytes({ok, Kek}, Key) ->
    {ok, seal(Key, Kek)};
sealed_key_bytes({error, _} = Error, _Key) ->
    Error.

%%------------------------------------------------------------------
%% Redaction
%%------------------------------------------------------------------

%% @doc A term with the private half of every key it holds replaced by the atom `redacted', at any depth: the private
%% value of every map that holds both a public and a private value, as node key components and key pairs do, and every
%% sealed call's or stream's secret by name, in any map: its keys (`k_req', `k_rep', `k_c2p', `k_p2c') and a KEM
%% private key's halves (`mlkem_dk', `p384_priv'); and a sealed group's epoch key (the `key' of a map that is an epoch
%% of macula_group_epoch). A group's admission token is redacted by name too (`ucan', as the keyring keeps it in a
%% group, and `ucan_token', as a join or a subscribe passes it in its options): it is a bearer credential, and a
%% process that shows its state or its last message must not show one (macula#189). A function that captured values
%% is replaced by its printed form, since what it captured can hold a key.
-spec redacted(term()) -> term().
redacted(Term) ->
    redacted(Term, #{}).

%% @doc The primary logger filter the application installs. In a report event of the otp or macula domain, every key
%% is redacted as redacted/1 does, and a stack frame of a module in `Modules' shows its arity in place of its
%% arguments, since those can hold a key. Every other event passes unchanged.
-spec redacted_log_event(logger:log_event(), #{module() => true}) -> logger:log_event().
redacted_log_event(#{msg := {report, Report}, meta := #{domain := [Domain | _]}} = Event, Modules)
  when Domain =:= otp; Domain =:= macula ->
    Event#{msg := {report, redacted(Report, Modules)}};
redacted_log_event(Event, _Modules) ->
    Event.

%% @doc Put the primary logger filter redacted_log_event/2, with the macula application's modules, in place, unless the
%% node already holds it. Another value held under its id, as an earlier load can leave behind, is replaced. The
%% application's start and every pool call it, and nothing removes it, since a process that holds a key can outlive
%% the application. A filter a concurrent call adds first is read back and replaced unless it is this one. Returns ok.
-spec install_log_redaction() -> ok.
install_log_redaction() ->
    ok = application_loaded(application:load(macula)),
    {ok, Modules} = application:get_key(macula, modules),
    Filter = {fun macula_node_keys:redacted_log_event/2, maps:from_keys(Modules, true)},
    log_redaction(held_log_redaction(), Filter).

application_loaded(ok) -> ok;
application_loaded({error, {already_loaded, macula}}) -> ok.

held_log_redaction() ->
    lists:keyfind(?KEY_REDACTION, 1, maps:get(filters, logger:get_primary_config())).

log_redaction({?KEY_REDACTION, Filter}, Filter) ->
    ok;
log_redaction(_MissingOrOther, Filter) ->
    _ = logger:remove_primary_filter(?KEY_REDACTION),
    filter_added(logger:add_primary_filter(?KEY_REDACTION, Filter), Filter).

%% A concurrent call can add a filter under the id between the removal and the add: the one held is read back, and kept
%% only when it is this filter.
filter_added(ok, _Filter) -> ok;
filter_added({error, {already_exist, ?KEY_REDACTION}}, Filter) -> log_redaction(held_log_redaction(), Filter).

%%------------------------------------------------------------------
%% Signing
%%------------------------------------------------------------------

%% @doc The public key a node carries for this key (D13): the ML-DSA-87 key, followed by the DER RSAPublicKey when
%% the key is hybrid.
-spec public_key(node_key()) -> binary().
public_key(#{components := Components}) ->
    << <<Public/binary>> || #{public := Public} <- Components >>.

%% @doc Sign a message: ML-DSA-87 alone for a one-component key, the LAMPS composite id-MLDSA87-RSA4096-PSS-SHA512 for
%% a hybrid key.
-spec sign(iodata(), node_key()) -> binary().
sign(Message, #{components := [#{algorithm := mldsa87, private := Private}]}) ->
    mldsa87_signature(Private, iolist_to_binary(Message), <<>>);
sign(Message, #{profile := Profile,
                components := [#{algorithm := mldsa87, private := MlDsaPrivate},
                               #{algorithm := rsa_pss, private := RsaPrivate}]}) ->
    Representative = composite_representative(Message),
    {ok, #{digest := Digest} = Params} = composite_rsa_params(Profile),
    {ok, RsaKey} = decode_rsa_private(RsaPrivate),
    MlDsaSignature = mldsa87_signature(MlDsaPrivate, Representative, <<?COMPOSITE_LABEL>>),
    RsaSignature = crypto:sign(rsa, Digest, Representative, rsa_private_list(RsaKey), pss_options(Params)),
    <<MlDsaSignature/binary, RsaSignature/binary>>.

%% @doc Verify a signature with the public key a node carries, under a profile. Malformed input is refused, never
%% raised on. A signature is exactly signature_bytes/1 of the profile long, and one of another length is refused
%% before either half is verified.
-spec verify(iodata(), binary(), binary(), term()) -> boolean().
verify(Message, Signature, Public, pq_pure)
  when byte_size(Signature) =:= ?MLDSA87_SIGNATURE_BYTES, byte_size(Public) =:= ?MLDSA87_PUBLIC_BYTES ->
    verified_call(fun() -> mldsa87_verifies(Public, iolist_to_binary(Message), Signature, <<>>) end);
verify(Message, Signature, Public, pq_hybrid) when is_binary(Signature), is_binary(Public) ->
    composite_verified(byte_size(Signature) =:= signature_bytes(pq_hybrid), Message, Signature, Public);
verify(_Message, _Signature, _Public, _Profile) ->
    false.

%% A composite's RSA half is as long as the modulus. RSA would accept the same value in fewer bytes, with its leading
%% zero bytes dropped, and the other stacks refuse that, so the length is checked first.
composite_verified(true, Message, <<MlDsaSignature:?MLDSA87_SIGNATURE_BYTES/binary, RsaSignature/binary>>,
                   <<MlDsaPublic:?MLDSA87_PUBLIC_BYTES/binary, RsaPublicDer/binary>>) ->
    Representative = composite_representative(Message),
    MlDsaValid = mldsa87_verifies(MlDsaPublic, Representative, MlDsaSignature, <<?COMPOSITE_LABEL>>),
    rsa_half_verifies(MlDsaValid, decode_rsa_public(RsaPublicDer), RsaSignature, Representative,
                      composite_rsa_params(pq_hybrid));
composite_verified(_LengthHolds, _Message, _Signature, _Public) ->
    false.

%%------------------------------------------------------------------
%% Node identity
%%------------------------------------------------------------------

%% @doc The node_id of an identity key (D5).
-spec node_id(node_key()) -> {ok, node_id()} | {error, not_an_identity_key}.
node_id(#{purpose := identity, profile := Profile} = Key) ->
    {ok, node_id(public_key(Key), Profile)};
node_id(#{purpose := _OtherPurpose}) ->
    {error, not_an_identity_key}.

%% @doc The node_id derived from an identity key as carried, under a profile (D5). A node_id earns no trust on its
%% own: a verifier relies on it only after a signature by the same carried key has verified.
-spec node_id(binary(), macula_crypto_profile:profile()) -> node_id().
node_id(IdentityKey, Profile)
  when is_binary(IdentityKey), (Profile =:= pq_pure orelse Profile =:= pq_hybrid) ->
    Name = atom_to_binary(Profile),
    crypto:hash(sha256, <<?NODE_ID_LABEL, 0:8, (byte_size(Name)):8, Name/binary, IdentityKey/binary>>).

%% @doc The key id of a key (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Signed objects): the node_id of an identity
%% key, and for any other key the key id of the key as carried.
-spec key_id(node_key()) -> <<_:256>>.
key_id(#{purpose := identity, profile := Profile} = Key) ->
    node_id(public_key(Key), Profile);
key_id(#{purpose := _OtherPurpose, profile := Profile} = Key) ->
    key_id(public_key(Key), Profile).

%% @doc The key id of a key as carried that is not an identity key, under a profile: SHA-256 over the label
%% MACULA-KEY-ID-V1, a zero byte, the length and ASCII name of the profile, and the key. Like a node_id, a key id
%% earns no trust on its own.
-spec key_id(binary(), macula_crypto_profile:profile()) -> <<_:256>>.
key_id(Key, Profile) when is_binary(Key), (Profile =:= pq_pure orelse Profile =:= pq_hybrid) ->
    Name = atom_to_binary(Profile),
    crypto:hash(sha256, <<?KEY_ID_LABEL, 0:8, (byte_size(Name)):8, Name/binary, Key/binary>>).

%% @doc Whether a node_id meets a puzzle difficulty: its first Difficulty bits are zero.
-spec puzzle_solved(node_id(), 0..256) -> boolean().
puzzle_solved(<<_:256>> = NodeId, Difficulty) when is_integer(Difficulty), Difficulty >= 0, Difficulty =< 256 ->
    <<Prefix:Difficulty, _/bitstring>> = NodeId,
    Prefix =:= 0.

%% @doc The puzzle difficulty of identity keys: a node generates its identity key to meet it (generate/3), and stations
%% check it on the node_id derived from a client's identity key.
-spec puzzle_difficulty() -> 0..256.
puzzle_difficulty() ->
    ?PUZZLE_DIFFICULTY.

%% @doc ok when the macula application has no puzzle_difficulty setting; raises
%% {bad_config, {macula, puzzle_difficulty, {not_a_setting, Value}}} when it has one, whatever the value. The difficulty
%% is puzzle_difficulty/0, one constant for the fleet (D30), so a node that sets it would believe it chose a difficulty
%% that nothing reads. The macula application calls this when it starts.
-spec check_puzzle_difficulty() -> ok.
check_puzzle_difficulty() ->
    no_puzzle_difficulty_setting(application:get_env(macula, puzzle_difficulty)).

no_puzzle_difficulty_setting(undefined) -> ok;
no_puzzle_difficulty_setting({ok, Value}) -> erlang:error({bad_config, {macula, puzzle_difficulty, {not_a_setting, Value}}}).

%% @doc Whether bytes are a key in its one carried form for a profile (D13): the 2,592-byte ML-DSA-87 key, followed
%% in pq_hybrid by a DER RSAPublicKey that encodes back to the same bytes, with the profile's modulus size and
%% public exponent. It says nothing about who holds the key.
-spec carried_key_well_formed(binary(), macula_crypto_profile:profile()) -> boolean().
carried_key_well_formed(Key, pq_pure) when is_binary(Key) ->
    byte_size(Key) =:= ?MLDSA87_PUBLIC_BYTES;
carried_key_well_formed(<<_:?MLDSA87_PUBLIC_BYTES/binary, RsaDer/binary>>, pq_hybrid) ->
    rsa_public_well_formed(decode_rsa_public(RsaDer), composite_rsa_params(pq_hybrid));
carried_key_well_formed(_Key, _Profile) ->
    false.

%% @doc The size of a signature by a node key in a profile: the ML-DSA-87 signature, followed in pq_hybrid by an
%% RSA-PSS signature as long as the modulus.
-spec signature_bytes(macula_crypto_profile:profile()) -> pos_integer().
signature_bytes(pq_pure) ->
    ?MLDSA87_SIGNATURE_BYTES;
signature_bytes(pq_hybrid) ->
    {ok, #{modulus_bits := Bits}} = composite_rsa_params(pq_hybrid),
    ?MLDSA87_SIGNATURE_BYTES + Bits div 8.

%%------------------------------------------------------------------
%% Internals: the puzzle
%%------------------------------------------------------------------

solved_key({ok, Key}, Difficulty) -> grind(Key, Difficulty);
solved_key({error, _} = Error, _Difficulty) -> Error.

grind(Key, Difficulty) ->
    {ok, NodeId} = node_id(Key),
    ground_key(puzzle_solved(NodeId, Difficulty), Key, Difficulty).

ground_key(true, Key, _Difficulty) -> {ok, Key};
ground_key(false, Key, Difficulty) -> grind(puzzle_candidate(Key), Difficulty).

%% One try: a new ML-DSA-87 half, with the classical half, if any, kept.
puzzle_candidate(#{purpose := identity, components := [#{algorithm := mldsa87} | Classical]} = Key) ->
    Key#{components := [generate_component(mldsa87) | Classical]}.

%%------------------------------------------------------------------
%% Internals: algorithms per purpose
%%------------------------------------------------------------------

expected_algorithms(Purpose, Profile) ->
    purpose_algorithms(Purpose, macula_crypto_profile:definition(Profile)).

purpose_algorithms(Purpose, {ok, #{identity_signature := Algorithms}})
  when Purpose =:= identity; Purpose =:= realm; Purpose =:= org; Purpose =:= foundation ->
    {ok, Algorithms};
purpose_algorithms(connect, {ok, #{connect_proof_signature := Algorithms}}) ->
    {ok, Algorithms};
purpose_algorithms(tls, {ok, #{tls_signature_scheme := Scheme}}) ->
    {ok, [Scheme]};
purpose_algorithms(Purpose, {ok, _Definition}) ->
    {error, {unknown_purpose, Purpose}};
purpose_algorithms(_Purpose, {error, _} = Error) ->
    Error.

algorithm_name(mldsa87) -> mldsa87;
algorithm_name({rsa_pss, _Params}) -> rsa_pss.

generated_key({ok, Algorithms}, Purpose, Profile) ->
    {ok, #{purpose    => Purpose,
           profile    => Profile,
           components => [generate_component(Algorithm) || Algorithm <- Algorithms]}};
generated_key({error, _} = Error, _Purpose, _Profile) ->
    Error.

generate_component(mldsa87) ->
    {ok, {Public, Seed}} = macula_crypto_nif:mldsa_generate(mldsa87),
    #{algorithm => mldsa87, public => Public, private => Seed};
generate_component({rsa_pss, #{modulus_bits := Bits, public_exponent := Exponent}}) ->
    {[E, N], PrivateList} = crypto:generate_key(rsa, {Bits, Exponent}),
    #{algorithm => rsa_pss, public => rsa_public_der(E, N), private => rsa_private_der(PrivateList)}.

%%------------------------------------------------------------------
%% Internals: the composite
%%------------------------------------------------------------------

composite_representative(Message) ->
    <<?COMPOSITE_PREFIX, ?COMPOSITE_LABEL, 0:8, (crypto:hash(sha512, Message))/binary>>.

composite_rsa_params(Profile) ->
    hybrid_rsa_params(macula_crypto_profile:definition(Profile)).

hybrid_rsa_params({ok, #{identity_signature := [mldsa87, {rsa_pss, Params}]}}) -> {ok, Params};
hybrid_rsa_params(_Definition) -> error.

%% A carried RSA key is accepted in its one DER encoding only. `der_decode' also takes BER forms (a long-form length,
%% an INTEGER with redundant leading zeros), so the key is re-encoded and compared. The re-encoding is built here
%% directly from the two integers rather than through the generic ASN.1 encoder, which cost more per verify than any
%% other step outside the two signature checks; `macula_node_keys_carried_form_tests' holds it to the library's.
decode_rsa_public(Der) ->
    try public_key:der_decode('RSAPublicKey', Der) of
        #'RSAPublicKey'{modulus = N, publicExponent = E} = Key when is_integer(N), N > 0, is_integer(E), E > 0 ->
            canonical_rsa_public(rsa_public_der_canonical(N, E) =:= Der, Key);
        #'RSAPublicKey'{} ->
            error
    catch
        error:_ -> error
    end.

%% DER of RSAPublicKey ::= SEQUENCE { modulus INTEGER, publicExponent INTEGER } for positive integers: each INTEGER
%% in its fewest bytes, with one leading zero byte only where the top bit would otherwise read as a sign, and every
%% length in its shortest form.
rsa_public_der_canonical(N, E) ->
    der_tlv(16#30, <<(der_tlv(16#02, der_positive_integer(N)))/binary,
                     (der_tlv(16#02, der_positive_integer(E)))/binary>>).

der_positive_integer(I) ->
    sign_padded(binary:encode_unsigned(I)).

sign_padded(<<Top, _/binary>> = Bytes) when Top >= 16#80 -> <<0, Bytes/binary>>;
sign_padded(Bytes) -> Bytes.

der_tlv(Tag, Value) ->
    <<Tag, (der_length(byte_size(Value)))/binary, Value/binary>>.

der_length(Length) when Length < 16#80 ->
    <<Length>>;
der_length(Length) ->
    Bytes = binary:encode_unsigned(Length),
    <<(16#80 bor byte_size(Bytes)), Bytes/binary>>.

canonical_rsa_public(true, Key) -> {ok, Key};
canonical_rsa_public(false, _Key) -> error.

rsa_public_well_formed({ok, #'RSAPublicKey'{modulus = N, publicExponent = E}},
                       {ok, #{modulus_bits := Bits, public_exponent := Exponent}}) ->
    {bit_length(N), E} =:= {Bits, Exponent};
rsa_public_well_formed(_Decoded, _Params) ->
    false.

rsa_half_verifies(true, {ok, #'RSAPublicKey'{modulus = N, publicExponent = E}}, Signature, Representative,
                  {ok, #{modulus_bits := Bits, public_exponent := Exponent, digest := Digest} = Params}) ->
    rsa_key_verifies({bit_length(N), E} =:= {Bits, Exponent}, [E, N], Signature, Representative, Digest,
                     pss_options(Params));
rsa_half_verifies(_MlDsaValid, _RsaPublic, _Signature, _Representative, _Params) ->
    false.

rsa_key_verifies(true, RsaPublic, Signature, Representative, Digest, Options) ->
    verified_call(fun() -> crypto:verify(rsa, Digest, Representative, Signature, RsaPublic, Options) end);
rsa_key_verifies(false, _RsaPublic, _Signature, _Representative, _Digest, _Options) ->
    false.

%% ML-DSA-87 under a context string: empty for a signature by ML-DSA-87 alone, the label for a composite's half.
mldsa87_signature(Private, Message, Context) ->
    {ok, Signature} = macula_crypto_nif:mldsa_sign(mldsa87, mldsa87_private(Private), Message, Context),
    Signature.

%% A private key stored as its seed, or in the expanded form of a key generated before D6's amendment.
mldsa87_private(<<_:32/binary>> = Seed) -> {seed, Seed};
mldsa87_private(Expanded) -> {expanded, Expanded}.

mldsa87_verifies(Public, Message, Signature, Context) ->
    macula_crypto_nif:mldsa_verify(mldsa87, Public, Message, Signature, Context).

verified_call(Verify) ->
    try Verify() of
        Result -> Result =:= true
    catch
        error:_ -> false
    end.

%%------------------------------------------------------------------
%% Internals: checks on load
%%------------------------------------------------------------------

checked_key({ok, #{purpose := Purpose, profile := Profile} = Key}, Purpose, Profile) ->
    checked_algorithms(Key, expected_algorithms(Purpose, Profile));
checked_key({ok, #{purpose := Found}}, Purpose, _Profile) when Found =/= Purpose ->
    {error, {wrong_purpose, Found}};
checked_key({ok, #{profile := Found}}, _Purpose, _Profile) ->
    {error, {wrong_profile, Found}};
checked_key({error, _} = Error, _Purpose, _Profile) ->
    Error.

checked_algorithms(#{components := Components} = Key, {ok, Expected}) ->
    Found = [Algorithm || #{algorithm := Algorithm} <- Components],
    checked_components(Key, Found =:= [algorithm_name(A) || A <- Expected], Found, Expected);
checked_algorithms(_Key, {error, _} = Error) ->
    Error.

checked_components(#{components := Components} = Key, true, _Found, Expected) ->
    whole_key_round_trip(first_refusal([check_component(C, A) || {C, A} <- lists:zip(Components, Expected)], Key));
checked_components(_Key, false, Found, _Expected) ->
    {error, {wrong_algorithms, Found}}.

first_refusal(Results, Key) ->
    case [Refusal || {error, _} = Refusal <- Results] of
        []            -> {ok, Key};
        [Refusal | _] -> Refusal
    end.

check_component(#{algorithm := mldsa87, public := Public, private := Private}, mldsa87) ->
    mldsa87_public_matches(derive_mldsa87_public(Private), Public);
check_component(#{algorithm := rsa_pss, public := Public, private := Private}, {rsa_pss, Params}) ->
    rsa_public_matches(decode_rsa_private(Private), Public, Params).

%% macula-mldsa derives the public key from a seed, or from rho, s1 and s2 of an expanded key, whose tr must hash
%% that public key and whose t0 must be the low bits of t. A private key of any other size is refused.
derive_mldsa87_public(Private) ->
    derived_mldsa87_public(macula_crypto_nif:mldsa_public_key(mldsa87, mldsa87_private(Private))).

derived_mldsa87_public({ok, Public}) -> {ok, Public};
derived_mldsa87_public({error, _Reason}) -> {error, private_key_invalid}.

mldsa87_public_matches({ok, Public}, Public) ->
    ok;
mldsa87_public_matches({ok, _Derived}, _Public) ->
    {error, public_key_mismatch};
mldsa87_public_matches({error, _} = Error, _Public) ->
    Error.

decode_rsa_private(Der) ->
    try public_key:der_decode('RSAPrivateKey', Der) of
        #'RSAPrivateKey'{} = Key -> {ok, Key}
    catch
        error:_ -> {error, private_key_invalid}
    end.

rsa_public_matches({ok, #'RSAPrivateKey'{modulus = N, publicExponent = E} = Key}, Public, Params) ->
    rsa_size_matches(rsa_public_der(E, N) =:= Public, Key, Params);
rsa_public_matches({error, _} = Error, _Public, _Params) ->
    Error.

rsa_size_matches(false, _Key, _Params) ->
    {error, public_key_mismatch};
rsa_size_matches(true, #'RSAPrivateKey'{modulus = N, publicExponent = E},
                 #{modulus_bits := Bits, public_exponent := Exponent}) ->
    rsa_size_result({bit_length(N), E} =:= {Bits, Exponent}, N, E).

rsa_size_result(true, _N, _E) -> ok;
rsa_size_result(false, N, E) -> {error, {wrong_key_size, {bit_length(N), E}}}.

%% The round trip signs with the whole key only: a hybrid key signs its composite, never one half, so hybrid key
%% material makes no signature outside the hybrid (BSI TR-02102-1 section 5.3.4).
whole_key_round_trip({ok, Key}) ->
    Message = crypto:strong_rand_bytes(32),
    key_round_trip_result(signed(fun(M) -> sign(M, Key) end, Message), Message, Key);
whole_key_round_trip({error, _} = Error) ->
    Error.

key_round_trip_result({ok, Signature}, Message, Key) ->
    round_trip_verdict(verify(Message, Signature, public_key(Key), signature_shape(Key)), Key);
key_round_trip_result({error, _} = Error, _Message, _Key) ->
    Error.

round_trip_verdict(true, Key)   -> {ok, Key};
round_trip_verdict(false, _Key) -> {error, round_trip_failed}.

%% verify/4 checks by profile; a one-component key (every pq_pure key, and a pq_hybrid TLS key) is ML-DSA-87 alone.
signature_shape(#{components := [_]})    -> pq_pure;
signature_shape(#{components := [_, _]}) -> pq_hybrid.

signed(Sign, Message) ->
    try Sign(Message) of
        Signature -> {ok, Signature}
    catch
        error:_ -> {error, round_trip_failed}
    end.


%%------------------------------------------------------------------
%% Internals: RSA encoding
%%------------------------------------------------------------------

rsa_public_der(E, N) ->
    public_key:der_encode('RSAPublicKey',
                          #'RSAPublicKey'{modulus = unsigned(N), publicExponent = unsigned(E)}).

rsa_private_der([E, N, D, P1, P2, E1, E2, C]) ->
    public_key:der_encode('RSAPrivateKey',
                          #'RSAPrivateKey'{version         = 'two-prime',
                                           modulus         = unsigned(N),
                                           publicExponent  = unsigned(E),
                                           privateExponent = unsigned(D),
                                           prime1          = unsigned(P1),
                                           prime2          = unsigned(P2),
                                           exponent1       = unsigned(E1),
                                           exponent2       = unsigned(E2),
                                           coefficient     = unsigned(C),
                                           otherPrimeInfos = asn1_NOVALUE}).

rsa_private_list(#'RSAPrivateKey'{publicExponent = E, modulus = N, privateExponent = D, prime1 = P1,
                                  prime2 = P2, exponent1 = E1, exponent2 = E2, coefficient = C}) ->
    [E, N, D, P1, P2, E1, E2, C].

pss_options(#{mgf1_digest := Mgf1Digest, salt_bytes := SaltBytes}) ->
    [{rsa_padding, rsa_pkcs1_pss_padding}, {rsa_pss_saltlen, SaltBytes}, {rsa_mgf1_md, Mgf1Digest}].

unsigned(Bin) when is_binary(Bin) -> binary:decode_unsigned(Bin);
unsigned(Int) when is_integer(Int) -> Int.

%% The bits of a positive integer, from its byte length and its top byte, without writing all 4096 of them out.
bit_length(N) when N > 0 ->
    <<Top, _/binary>> = Bytes = binary:encode_unsigned(N),
    (byte_size(Bytes) - 1) * 8 + length(integer_to_list(Top, 2));
%% A modulus that is not positive has no size a key could have, and is refused as the wrong size, not crashed on.
bit_length(_NotPositive) ->
    0.

%%------------------------------------------------------------------
%% Internals: key file format
%%------------------------------------------------------------------

encode(#{purpose := Purpose, profile := Profile, components := Components}) ->
    Encoded = << <<(encode_component(Component))/binary>> || Component <- Components >>,
    <<?KEY_FILE_MAGIC, (purpose_tag(Purpose)):8, (profile_tag(Profile)):8, (length(Components)):8,
      Encoded/binary>>.

encode_component(#{algorithm := Algorithm, public := Public, private := Private}) ->
    <<(algorithm_tag(Algorithm)):8, (byte_size(Public)):32, Public/binary,
      (byte_size(Private)):32, Private/binary>>.

owner_only_read({ok, #file_info{mode = Mode} = Info}, Path, Options) when Mode band 8#077 =:= 0 ->
    key_file(file:read_file(Path), Info, Path, Options);
owner_only_read({ok, #file_info{}}, _Path, _Options) ->
    {error, key_file_permissions};
owner_only_read({error, _} = Error, _Path, _Options) ->
    Error.

decode_file({ok, Bin}) -> decode(Bin);
decode_file({error, _} = Error) -> Error.

decode(<<?KEY_FILE_MAGIC, PurposeTag:8, ProfileTag:8, Count:8, Rest/binary>>) ->
    decoded_key(tag_purpose(PurposeTag), tag_profile(ProfileTag), decode_components(Rest, []), Count);
decode(_Bin) ->
    {error, bad_key_file}.

decode_components(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
decode_components(<<Tag:8, PublicLen:32, Public:PublicLen/binary, PrivateLen:32, Private:PrivateLen/binary,
                    Rest/binary>>, Acc) ->
    decode_component(tag_algorithm(Tag), Public, Private, Rest, Acc);
decode_components(_Bin, _Acc) ->
    {error, bad_key_file}.

decode_component({ok, Algorithm}, Public, Private, Rest, Acc) ->
    decode_components(Rest, [#{algorithm => Algorithm, public => Public, private => Private} | Acc]);
decode_component(error, _Public, _Private, _Rest, _Acc) ->
    {error, bad_key_file}.

decoded_key({ok, Purpose}, {ok, Profile}, {ok, Components}, Count) when length(Components) =:= Count ->
    {ok, #{purpose => Purpose, profile => Profile, components => Components}};
decoded_key(_Purpose, _Profile, _Components, _Count) ->
    {error, bad_key_file}.

purpose_tag(identity) -> 1;
purpose_tag(connect)  -> 2;
purpose_tag(tls)      -> 3;
purpose_tag(realm)    -> 4;
purpose_tag(org)      -> 5;
purpose_tag(foundation) -> 6.

tag_purpose(1) -> {ok, identity};
tag_purpose(2) -> {ok, connect};
tag_purpose(3) -> {ok, tls};
tag_purpose(4) -> {ok, realm};
tag_purpose(5) -> {ok, org};
tag_purpose(6) -> {ok, foundation};
tag_purpose(_) -> error.

profile_tag(pq_pure)   -> 1;
profile_tag(pq_hybrid) -> 2.

tag_profile(1) -> {ok, pq_pure};
tag_profile(2) -> {ok, pq_hybrid};
tag_profile(_) -> error.

algorithm_tag(mldsa87) -> 1;
algorithm_tag(rsa_pss) -> 2.

tag_algorithm(1) -> {ok, mldsa87};
tag_algorithm(2) -> {ok, rsa_pss};
tag_algorithm(_) -> error.

%%------------------------------------------------------------------
%% Internals: redaction
%%------------------------------------------------------------------

redacted(#{public := _, private := _} = Map, Modules) ->
    maps:map(redacted_value(Modules), Map#{private := redacted});
redacted(#{id := <<_:64>>, key := <<_:256>>, accept_until := _} = Epoch, Modules) ->
    maps:map(redacted_value(Modules), Epoch#{key := redacted});
redacted(Map, Modules) when is_map(Map) ->
    maps:map(redacted_value(Modules), Map);
redacted([Head | Tail], Modules) ->
    [redacted(Head, Modules) | redacted(Tail, Modules)];
redacted({Module, Function, Arguments, Location}, Modules)
  when is_map_key(Module, Modules), is_atom(Function), length(Arguments) >= 0, is_list(Location) ->
    {Module, Function, length(Arguments), Location};
redacted(Tuple, Modules) when is_tuple(Tuple) ->
    list_to_tuple(redacted(tuple_to_list(Tuple), Modules));
redacted(Fun, _Modules) when is_function(Fun) ->
    fun_redacted(erlang:fun_info(Fun, env), Fun);
redacted(Other, _Modules) ->
    Other.

redacted_value(Modules) ->
    fun(Key, _Value) when Key =:= k_req; Key =:= k_rep; Key =:= k_c2p; Key =:= k_p2c;
                          Key =:= mlkem_dk; Key =:= p384_priv;
                          Key =:= ucan; Key =:= ucan_token ->
           redacted;
       (_Key, Value) ->
           redacted(Value, Modules)
    end.

%% A function that captured values shows as its printed form, the way a report prints it, since what it captured can
%% hold a key: a node identity key travels to a pool or an issuer as a function that returns it. A function that
%% captured nothing stays as it is.
fun_redacted({env, []}, Fun) -> Fun;
fun_redacted({env, _Captured}, Fun) -> erlang:fun_to_list(Fun).

%%------------------------------------------------------------------
%% Internals: restricted atomic write
%%------------------------------------------------------------------

write_restricted(ok, Tmp, Path, Blob) ->
    fill_and_rename(restrict(file:write_file(Tmp, <<>>, [raw, binary]), Tmp), Tmp, Path, Blob);
write_restricted({error, _} = Error, _Tmp, _Path, _Blob) ->
    Error.

restrict(ok, Tmp) -> file:change_mode(Tmp, 8#0600);
restrict({error, _} = Error, _Tmp) -> Error.

fill_and_rename(ok, Tmp, Path, Blob) ->
    rename_filled(file:write_file(Tmp, Blob, [raw, binary]), Tmp, Path);
fill_and_rename({error, _} = Error, _Tmp, _Path, _Blob) ->
    Error.

rename_filled(ok, Tmp, Path) -> file:rename(Tmp, Path);
rename_filled({error, _} = Error, _Tmp, _Path) -> Error.
