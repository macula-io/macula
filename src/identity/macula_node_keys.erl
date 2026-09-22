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
%% A process that holds keys shows them through redacted/1, and the primary logger filter redacted_log_event/2, which
%% install_log_redaction/0 puts in place, keeps their private halves out of crash and diagnostics reports.
%%
%% A key with one component signs with ML-DSA-87 alone. A hybrid key signs with Macula's composite
%% ML-DSA-87-PS384: both halves sign M' = Prefix || Label || len(ctx) || ctx || SHA-512(M), with an empty ctx and
%% ML-DSA-87 under an empty context; the signature is the ML-DSA-87 signature followed by the RSA-PSS signature, and
%% the carried public key is the ML-DSA-87 key followed by the DER RSAPublicKey. It is valid only if both halves
%% verify.
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
%% See plans/PLAN_POST_QUANTUM_SECURITY.md, decisions D4, D5, D6 and D7.
-module(macula_node_keys).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/file.hrl").

%% The id of the primary logger filter that redacts keys in log events.
-define(KEY_REDACTION, macula_key_redaction).

-export([
    generate/2,
    generate/3,
    save/2,
    load/3,
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
    carried_key_well_formed/2,
    signature_bytes/1
]).

-ifdef(TEST).
-export([puzzle_candidate/1]).
-endif.

-export_type([purpose/0, algorithm/0, component/0, node_key/0, refusal/0]).

-type purpose()   :: identity | connect | tls | realm | org | foundation.
-type algorithm() :: mldsa87 | rsa_pss.
-type component() :: #{algorithm := algorithm(), public := binary(), private := binary()}.
-type node_key()  :: #{purpose    := purpose(),
                       profile    := macula_crypto_profile:profile(),
                       components := [component(), ...]}.
-type refusal()   :: bad_key_file
                   | key_file_permissions
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
-define(MLDSA87_PUBLIC_BYTES, 2592).
-define(MLDSA87_SIGNATURE_BYTES, 4627).
-define(COMPOSITE_PREFIX, "CompositeAlgorithmSignatures2025").
-define(COMPOSITE_LABEL, "MACULA-ML-DSA-87-PS384").
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

%% @doc Save a key atomically. The temporary file is restricted to its owner before the key is written into it.
-spec save(file:name_all(), node_key()) -> ok | {error, term()}.
save(Path, #{purpose := _, profile := _, components := [_ | _]} = Key) ->
    Tmp = iolist_to_binary([Path, ".tmp"]),
    write_restricted(filelib:ensure_dir(Path), Tmp, Path, encode(Key)).

%% @doc Load the key saved for a purpose in a profile, and check it before returning it. A key file its group or others
%% can read is refused.
-spec load(file:name_all(), purpose(), macula_crypto_profile:profile()) ->
        {ok, node_key()} | {error, refusal() | file:posix() | badarg | terminated | system_limit}.
load(Path, Purpose, Profile) ->
    checked_key(owner_only_read(file:read_file_info(Path), Path), Purpose, Profile).

%%------------------------------------------------------------------
%% Redaction
%%------------------------------------------------------------------

%% @doc A term with the private half of every key it holds replaced by the atom `redacted', at any depth: the private
%% value of every map that holds both a public and a private value, as node key components and key pairs do. A
%% function that captured values is replaced by its printed form, since what it captured can hold a key.
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

%% @doc Sign a message: ML-DSA-87 alone for a one-component key, Macula's composite ML-DSA-87-PS384 for a hybrid key.
-spec sign(iodata(), node_key()) -> binary().
sign(Message, #{components := [#{algorithm := mldsa87, private := Private}]}) ->
    mldsa87_signature(Private, iolist_to_binary(Message));
sign(Message, #{profile := Profile,
                components := [#{algorithm := mldsa87, private := MlDsaPrivate},
                               #{algorithm := rsa_pss, private := RsaPrivate}]}) ->
    Representative = composite_representative(Message),
    {ok, #{digest := Digest} = Params} = composite_rsa_params(Profile),
    {ok, RsaKey} = decode_rsa_private(RsaPrivate),
    MlDsaSignature = mldsa87_signature(MlDsaPrivate, Representative),
    RsaSignature = crypto:sign(rsa, Digest, Representative, rsa_private_list(RsaKey), pss_options(Params)),
    <<MlDsaSignature/binary, RsaSignature/binary>>.

%% @doc Verify a signature with the public key a node carries, under a profile. Malformed input is refused, never
%% raised on. A signature is exactly signature_bytes/1 of the profile long, and one of another length is refused
%% before either half is verified.
-spec verify(iodata(), binary(), binary(), term()) -> boolean().
verify(Message, Signature, Public, pq_pure)
  when byte_size(Signature) =:= ?MLDSA87_SIGNATURE_BYTES, byte_size(Public) =:= ?MLDSA87_PUBLIC_BYTES ->
    verified_call(fun() -> mldsa87_verifies(Public, iolist_to_binary(Message), Signature) end);
verify(Message, Signature, Public, pq_hybrid) when is_binary(Signature), is_binary(Public) ->
    composite_verified(byte_size(Signature) =:= signature_bytes(pq_hybrid), Message, Signature, Public);
verify(_Message, _Signature, _Public, _Profile) ->
    false.

%% A composite's RSA half is as long as the modulus. RSA would accept the same value in fewer bytes, with its leading
%% zero bytes dropped, and the other stacks refuse that, so the length is checked first.
composite_verified(true, Message, <<MlDsaSignature:?MLDSA87_SIGNATURE_BYTES/binary, RsaSignature/binary>>,
                   <<MlDsaPublic:?MLDSA87_PUBLIC_BYTES/binary, RsaPublicDer/binary>>) ->
    Representative = composite_representative(Message),
    MlDsaValid = mldsa87_verifies(MlDsaPublic, Representative, MlDsaSignature),
    rsa_half_verifies(MlDsaValid, decode_rsa_public(RsaPublicDer), RsaSignature, Representative,
                      composite_rsa_params(pq_hybrid));
composite_verified(_LengthHolds, _Message, _Signature, _Public) ->
    false.

%%------------------------------------------------------------------
%% Node identity
%%------------------------------------------------------------------

%% @doc The node_id of an identity key (D5).
-spec node_id(node_key()) -> {ok, <<_:256>>} | {error, not_an_identity_key}.
node_id(#{purpose := identity, profile := Profile} = Key) ->
    {ok, node_id(public_key(Key), Profile)};
node_id(#{purpose := _OtherPurpose}) ->
    {error, not_an_identity_key}.

%% @doc The node_id derived from an identity key as carried, under a profile (D5). A node_id earns no trust on its
%% own: a verifier relies on it only after a signature by the same carried key has verified.
-spec node_id(binary(), macula_crypto_profile:profile()) -> <<_:256>>.
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
-spec puzzle_solved(<<_:256>>, 0..256) -> boolean().
puzzle_solved(<<_:256>> = NodeId, Difficulty) when is_integer(Difficulty), Difficulty >= 0, Difficulty =< 256 ->
    <<Prefix:Difficulty, _/bitstring>> = NodeId,
    Prefix =:= 0.

%% @doc The puzzle difficulty of identity keys: a node generates its identity key to meet it (generate/3), and stations
%% check it on the node_id derived from a client's identity key.
-spec puzzle_difficulty() -> 0..256.
puzzle_difficulty() ->
    ?PUZZLE_DIFFICULTY.

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

decode_rsa_public(Der) ->
    try public_key:der_decode('RSAPublicKey', Der) of
        #'RSAPublicKey'{} = Key -> canonical_rsa_public(public_key:der_encode('RSAPublicKey', Key) =:= Der, Key)
    catch
        error:_ -> error
    end.

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

%% ML-DSA-87 under the empty context, as both the pure signature and the composite's ML-DSA half use it.
mldsa87_signature(Private, Message) ->
    {ok, Signature} = macula_crypto_nif:mldsa_sign(mldsa87, mldsa87_private(Private), Message, <<>>),
    Signature.

%% A private key stored as its seed, or in the expanded form of a key generated before D6's amendment.
mldsa87_private(<<_:32/binary>> = Seed) -> {seed, Seed};
mldsa87_private(Expanded) -> {expanded, Expanded}.

mldsa87_verifies(Public, Message, Signature) ->
    macula_crypto_nif:mldsa_verify(mldsa87, Public, Message, Signature, <<>>).

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

bit_length(N) -> length(integer_to_list(N, 2)).

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

owner_only_read({ok, #file_info{mode = Mode}}, Path) when Mode band 8#077 =:= 0 ->
    decode_file(file:read_file(Path));
owner_only_read({ok, #file_info{}}, _Path) ->
    {error, key_file_permissions};
owner_only_read({error, _} = Error, _Path) ->
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
    fun(_Key, Value) -> redacted(Value, Modules) end.

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
