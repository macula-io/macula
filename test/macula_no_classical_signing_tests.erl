%% @doc No classical signature is made or checked anywhere in `macula'.
%%
%% WP 1.3's done line: every signature in `macula' uses the profile's
%% algorithm, and this test guards against regressions. D7, as amended on
%% 2026-09-22, puts every ML-DSA signature on `macula-mldsa'; the only
%% classical signing left is RSA-PSS as the EU composite's second half,
%% which stays on OTP `crypto' in `macula_node_keys', and nowhere else.
%% The same amendment makes an ML-DSA signature through OTP `crypto' a use
%% to remove as well: rule erl_crypto_mldsa.
%%
%% The rule is USE, not the word. Comments are stripped before matching,
%% so a sentence about Ed25519 is not a hit; each rule names a call, a
%% crate, a wire constant or an identifier that makes or checks a
%% classical signature, or carries a classical key. Both `src/' and
%% `native/' are scanned: the NIFs sign too.
%%
%% ⛔ A RATCHET, NOT AN EXEMPTION LIST. The uses still to be removed are
%% listed in known/0, each with the work item that removes it: the list is
%% that work, made executable. A use not listed fails, and a listed count
%% that no longer matches fails too, so each fix must strike its own
%% entry and the list only shrinks. And it cannot ship: while any entry is
%% left, a `vsn' without a pre-release suffix fails, so `macula' 12.0.0 is
%% released with no classical signing or not at all.
-module(macula_no_classical_signing_tests).

-include_lib("eunit/include/eunit.hrl").

%% {Id, What it catches, File kind, Regex}.
rules() ->
    [{erl_crypto_classical, "OTP crypto signing or verifying with EdDSA, ECDSA or DSA",
      erl, "crypto:(sign|verify)\\(\\s*(eddsa|ecdsa|dss)\\b"},
     {erl_crypto_rsa, "OTP crypto signing or verifying with RSA",
      erl, "crypto:(sign|verify)\\(\\s*rsa\\b"},
     {erl_keygen_classical, "OTP crypto generating an EdDSA or DSA signing key",
      erl, "crypto:generate_key\\(\\s*(eddsa|dss)\\b"},
     {erl_keygen_rsa, "OTP crypto generating an RSA key",
      erl, "crypto:generate_key\\(\\s*rsa\\b"},
     {erl_crypto_mldsa, "OTP crypto making, checking or generating an ML-DSA key (D7: macula-mldsa does)",
      erl, "crypto:(sign|verify|generate_key)\\(\\s*mldsa"},
     {erl_public_key, "public_key signing or verifying (classical X.509 today)",
      erl, "public_key:(sign|verify)\\("},
     {erl_wire_constant, "the EdDSA alg or the Ed25519 DID key type on the wire",
      erl, "<<\"(EdDSA|Ed25519VerificationKey2020)\">>"},
     {rust_dalek, "the ed25519-dalek crate",
      rs, "ed25519_dalek"},
     {cargo_dalek, "an ed25519-dalek dependency",
      toml, "^\\s*ed25519-dalek\\s*="},
     {rust_classical_alg, "a classical signature scheme or algorithm constant",
      rs, "(PKCS_ED25519|PKCS_ECDSA_|PKCS_RSA_|SignatureScheme::(ED25519|ED448|ECDSA_|RSA_)"
          "|signature::(ED25519|ECDSA_|RSA_))"},
     {rust_ed25519_oid, "an Ed25519 key recognised or wrapped by its OID",
      rs, "(\"1\\.3\\.101\\.112\"|0x2b, 0x65, 0x70)"},
     {rust_provider_verify, "TLS signatures checked with the crypto provider's algorithms, all classical",
      rs, "\\.signature_verification_algorithms\\b"},
     {rust_wire_constant, "the EdDSA alg or the Ed25519 DID key type on the wire",
      rs, "\"(EdDSA|Ed25519VerificationKey2020)\""}].

%% The one sanctioned classical signature: the EU composite's RSA-PSS
%% half, on OTP `crypto', in the module that holds the composite (D4, D7).
allowed() ->
    [{erl_crypto_rsa, "src/identity/macula_node_keys.erl"},
     {erl_keygen_rsa, "src/identity/macula_node_keys.erl"}].

%% {File, Rule, How many uses, The work item that removes them}.
known() ->
    [{"src/identity/macula_identity.erl", erl_keygen_classical, 1, "WP 1.3 residual"},
     {"src/identity/macula_identity.erl", erl_crypto_classical, 2, "WP 1.3 residual"},
     {"src/identity/macula_crypto_nif.erl", erl_keygen_classical, 2, "WP 1.3 residual"},
     {"src/identity/macula_crypto_nif.erl", erl_crypto_classical, 2, "WP 1.3 residual"},
     {"native/macula_crypto_nif/src/lib.rs", rust_dalek, 1, "WP 1.3 residual"},
     {"native/macula_crypto_nif/Cargo.toml", cargo_dalek, 1, "WP 1.3 residual"},
     {"src/auth/macula_ucan_nif.erl", erl_wire_constant, 1, "WP 1.4"},
     {"native/macula_ucan_nif/src/lib.rs", rust_dalek, 1, "WP 1.4"},
     {"native/macula_ucan_nif/src/lib.rs", rust_wire_constant, 1, "WP 1.4"},
     {"native/macula_ucan_nif/Cargo.toml", cargo_dalek, 1, "WP 1.4"},
     {"src/identity/macula_did_nif.erl", erl_wire_constant, 1, "WP 1.4"},
     {"native/macula_did_nif/src/lib.rs", rust_dalek, 1, "WP 1.4"},
     {"native/macula_did_nif/src/lib.rs", rust_wire_constant, 1, "WP 1.4"},
     {"native/macula_did_nif/Cargo.toml", cargo_dalek, 1, "WP 1.4"},
     {"native/macula_quic/src/cert.rs", rust_ed25519_oid, 2, "D12 TLS leaf"},
     {"native/macula_quic/src/cert.rs", rust_provider_verify, 2, "D12 TLS leaf"},
     {"native/macula_quic/src/config.rs", rust_provider_verify, 1, "D12 TLS leaf"}].

no_classical_signing_beyond_the_known_list_test_() ->
    {timeout, 60,
     fun() ->
         Found = counts(hits(root())),
         Known = lists:sort([{File, Id, N} || {File, Id, N, _} <- known()]),
         %% Printed in full: eunit shortens a long list in its assertion
         %% message, and the difference is what to do next.
         [io:format(user, "use not in known/0: ~s ~s x~b~n", [F, what(Id), N])
          || {F, Id, N} <- Found -- Known],
         [io:format(user, "known/0 entry no longer matches, strike or correct it: ~s ~s x~b~n",
                    [F, what(Id), N])
          || {F, Id, N} <- Known -- Found],
         ?assertEqual(Known, Found)
     end}.

%% While known/0 is not empty, only a pre-release may be built from this
%% tree.
the_known_list_cannot_ship_test() ->
    ok = case application:load(macula) of
             ok -> ok;
             {error, {already_loaded, macula}} -> ok
         end,
    {ok, Vsn} = application:get_key(macula, vsn),
    ?assertEqual(ok, release_check(Vsn, known())).

%% The release check, on the cases that matter.
the_release_check_refuses_a_release_with_entries_left_test() ->
    Entry = [{"src/x.erl", erl_crypto_classical, 1, "WP 1.3 residual"}],
    ?assertEqual(ok, release_check("12.0.0-alpha.1", Entry)),
    ?assertEqual(ok, release_check("12.0.0", [])),
    ?assertMatch({error, {release_with_classical_signing, "12.0.0", 1}},
                 release_check("12.0.0", Entry)).

release_check(Vsn, Known) ->
    case {lists:member($-, Vsn), Known} of
        {_, []} -> ok;
        {true, _} -> ok;
        {false, _} -> {error, {release_with_classical_signing, Vsn, length(Known)}}
    end.

%% The scanner can see what it is meant to: each rule matches its own
%% example, and comments are stripped from both languages. Without this,
%% an empty result could mean a broken regex.
the_scanner_sees_each_rule_test() ->
    Examples = #{erl_crypto_classical => "crypto:sign(eddsa, none, M, K)",
                 erl_crypto_rsa => "crypto:verify(rsa, sha384, M, S, K)",
                 erl_keygen_classical => "crypto:generate_key(eddsa, ed25519)",
                 erl_keygen_rsa => "crypto:generate_key(rsa, {4096, 65537})",
                 erl_crypto_mldsa => "crypto:sign(mldsa87, none, M, {expandedkey, K})",
                 erl_public_key => "public_key:sign(Msg, sha256, Key)",
                 erl_wire_constant => "#{<<\"alg\">> => <<\"EdDSA\">>}",
                 rust_dalek => "use ed25519_dalek::SigningKey;",
                 cargo_dalek => "ed25519-dalek = \"3.0\"",
                 rust_classical_alg => "let a = &rcgen::PKCS_ED25519;",
                 rust_ed25519_oid => "if oid != \"1.3.101.112\" {",
                 rust_provider_verify => "provider.signature_verification_algorithms",
                 rust_wire_constant => "alg: \"EdDSA\".to_string(),"},
    [?assertMatch({Id, [_]}, {Id, matching_rules(Kind, strip(Kind, maps:get(Id, Examples)))})
     || {Id, _, Kind, _} <- rules()],
    ?assertEqual([], matching_rules(erl, strip(erl, "%% crypto:sign(eddsa, none, M, K)"))),
    ?assertEqual([], matching_rules(rs, strip(rs, "// use ed25519_dalek::SigningKey;"))),
    ?assertEqual([], matching_rules(rs, strip(rs, "x(); // PKCS_ED25519"))).

hits(Root) ->
    Files = [{erl, F} || F <- filelib:wildcard(filename:join(Root, "src/**/*.erl"))]
         ++ [{rs, F} || F <- filelib:wildcard(filename:join(Root, "native/*/src/**/*.rs"))]
         ++ [{toml, F} || F <- filelib:wildcard(filename:join(Root, "native/*/Cargo.toml"))],
    lists:append([file_hits(Root, Kind, File) || {Kind, File} <- Files]).

file_hits(Root, Kind, File) ->
    {ok, Bin} = file:read_file(File),
    Relative = relative(Root, File),
    Lines = string:split(Bin, "\n", all),
    [{Relative, N, Id}
     || {N, Line} <- lists:zip(lists:seq(1, length(Lines)), Lines),
        Id <- matching_rules(Kind, strip(Kind, unicode:characters_to_list(Line))),
        not lists:member({Id, Relative}, allowed())].

matching_rules(Kind, Code) ->
    [Id || {Id, _, K, Regex} <- rules(), K =:= Kind, re:run(Code, Regex, [unicode]) =/= nomatch].

%% Drops a comment: `%' to the end of an Erlang line, `//' to the end of
%% a Rust line, `#' to the end of a TOML line. A `%' or `//' inside a
%% string would be dropped too; no rule looks for one.
strip(erl, Line) -> hd(string:split(Line, "%"));
strip(rs, Line) -> hd(string:split(Line, "//"));
strip(toml, Line) -> hd(string:split(Line, "#")).

%% Hits as {File, Rule, How many}, sorted: line numbers move with
%% unrelated edits, counts do not.
counts(Hits) ->
    Keys = [{File, Id} || {File, _Line, Id} <- Hits],
    lists:sort([{File, Id, length([K || K <- Keys, K =:= {File, Id}])}
                || {File, Id} <- lists:usort(Keys)]).

what(Id) ->
    {Id, What, _, _} = lists:keyfind(Id, 1, rules()),
    What.

root() ->
    filename:dirname(filename:dirname(?FILE)).

relative(Root, File) ->
    string:prefix(File, Root ++ "/").
