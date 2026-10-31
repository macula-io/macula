%% Writes test/vectors/sealed_key_v1.json, the sealed key file vector that macula-go and macula-rust run too
%% (docs/design/DESIGN_SEALED_KEY_FILE.md, section 7). Key generation is random, so a run makes a new key; the committed
%% file is the vector, and the test only reads it. Run through scripts/generate-sealed-key-vectors.sh with the tree
%% compiled, because it uses macula_node_keys and macula_seal from _build.
-module(generate_sealed_key_vectors).

-export([main/0]).

-define(VECTOR_FILE, "test/vectors/sealed_key_v1.json").

main() ->
    {ok, Key} = macula_node_keys:generate(identity, pq_pure),
    Plaintext = plaintext_of(Key),
    Kek = crypto:strong_rand_bytes(32),
    Nonce = crypto:strong_rand_bytes(12),
    Sealed = macula_node_keys:seal(Key, Kek, Nonce),
    {ok, KeyId} = macula_node_keys:node_id(Key),
    ok = file:write_file(?VECTOR_FILE, json:encode(document(Kek, Nonce, Plaintext, Sealed, KeyId))),
    io:format("wrote ~s~n", [?VECTOR_FILE]),
    halt(0).

document(Kek, Nonce, Plaintext, Sealed, KeyId) ->
    #{generator => <<"scripts/generate-sealed-key-vectors.sh">>,
      note => <<"A conforming reader unseals `sealed` with `kek` to a key whose key_id is `key_id`; a conforming "
                "writer seals `plaintext` with `kek` and `nonce` to exactly `sealed`. A flipped byte at "
                "`flipped_byte_offset`, a file shorter than its 100-byte header, and a plaintext file under "
                "sealed_required are refused.">>,
      kek => hex(Kek),
      nonce => hex(Nonce),
      plaintext => hex(Plaintext),
      sealed => hex(Sealed),
      key_id => hex(KeyId),
      flipped_byte_offset => 110}.

%% The key file bytes the vector's key is saved as, written by the one writer and read back.
plaintext_of(Key) ->
    Path = filename:join(temporary_dir(), "sealed_key_vector.key"),
    ok = macula_node_keys:save(Path, Key),
    {ok, Bytes} = file:read_file(Path),
    _ = file:delete(Path),
    Bytes.

temporary_dir() ->
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"), "macula-sealed-key-vectors"),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.

hex(Bin) -> binary:encode_hex(Bin, lowercase).
