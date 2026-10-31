%% Writes test/vectors/sealed_key_v1.json, the sealed key file vectors that macula-go and macula-rust run too
%% (docs/design/DESIGN_SEALED_KEY_FILE.md, section 7). Key generation is random, so a run makes new keys; the committed
%% file is the vector, and the test only reads it. Run through scripts/generate-sealed-key-vectors.sh with the tree
%% compiled, because it uses macula_node_keys from _build.
%%
%% Two vectors, because one key cannot pin down two derivations: an identity key is named by its node_id and any other
%% purpose by the MACULA-KEY-ID-V1 label, and a reader given only the identity one could implement either and see it
%% pass.
-module(generate_sealed_key_vectors).

-export([main/0]).

-define(VECTOR_FILE, "test/vectors/sealed_key_v1.json").
-define(HEADER_BYTES, 100).

main() ->
    %% A configured KEK seals every key a tree writes, and a vector's plaintext has to be plaintext.
    _ = os:unsetenv("MACULA_KEY_KEK_FILE"),
    _ = application:unset_env(macula, key_kek_file),
    Vectors = [vector(<<"identity_pq_pure">>, identity, pq_pure),
               vector(<<"org_pq_pure">>, org, pq_pure),
               vector(<<"identity_pq_hybrid">>, identity, pq_hybrid)],
    ok = file:write_file(?VECTOR_FILE, json:encode(document(Vectors))),
    io:format("wrote ~s~n", [?VECTOR_FILE]),
    halt(0).

vector(Name, Purpose, Profile) ->
    {ok, Key} = macula_node_keys:generate(Purpose, Profile),
    Plaintext = plaintext_of(Key),
    Kek = crypto:strong_rand_bytes(32),
    Nonce = crypto:strong_rand_bytes(12),
    Sealed = macula_node_keys:seal(Key, Kek, Nonce),
    <<Header:?HEADER_BYTES/binary, _/binary>> = Sealed,
    #{name => Name,
      purpose => atom_to_binary(Purpose),
      profile => atom_to_binary(Profile),
      kek => hex(Kek),
      kek_id => hex(macula_node_keys:kek_id(Kek)),
      nonce => hex(Nonce),
      header => hex(Header),
      plaintext => hex(Plaintext),
      sealed => hex(Sealed),
      key_id => hex(macula_node_keys:key_id(Key)),
      flipped_byte_offset => 110,
      header_flip_offset => 24}.

document(Vectors) ->
    #{generator => <<"scripts/generate-sealed-key-vectors.sh">>,
      note => <<"A conforming reader unseals a vector's `sealed` with its `kek` to a key whose key_id is `key_id`; "
                "a conforming writer seals its `plaintext` with its `kek` and `nonce` to exactly `sealed`. `header` is "
                "the first 100 bytes of `sealed` and `kek_id` is SHA-256 over the label MACULA-KEK-ID-V1, a zero "
                "byte and the KEK. An identity key's key id is its node_id; any other purpose's is the "
                "MACULA-KEY-ID-V1 derivation. A flipped byte at `flipped_byte_offset`, a file shorter than its header, "
                "and a plaintext file under sealed_required are refused.">>,
      vectors => Vectors}.

%% The key file bytes a key is saved as, written by the one writer and read back.
plaintext_of(Key) ->
    Path = filename:join(temporary_dir(), "sealed_key_vector.key"),
    ok = macula_node_keys:save(Path, Key),
    {ok, Bytes} = file:read_file(Path),
    _ = file:delete(Path),
    Bytes.

%% The key is written in plaintext only when no KEK is configured, and a generator run must not inherit one.
temporary_dir() ->
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"), "macula-sealed-key-vectors"),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.

hex(Bin) -> binary:encode_hex(Bin, lowercase).
