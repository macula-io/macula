%% @doc A node's keys, one per purpose, in the node's crypto profile, stored as plan decision D6 describes, and
%% signing with them as decisions D4 and D7 describe.
%%
%% A node holds an identity key and a CONNECT key, and a station instance also holds a TLS key. Each key serves
%% exactly one purpose. A key is a list of components in the order of the profile's signature: ML-DSA-87 first,
%% then the classical half when the profile's signature for that purpose is hybrid. The TLS key is ML-DSA-87
%% alone in both profiles.
%%
%% An ML-DSA-87 component stores its 4,896-byte expanded private key and its public key. An RSA-PSS component
%% stores a DER-encoded RSAPrivateKey and RSAPublicKey. On load, every public key is derived again from its
%% private key and must equal the stored one, and every component passes a sign-and-verify round trip.
%%
%% A key with one component signs with ML-DSA-87 alone. A hybrid key signs with Macula's composite
%% ML-DSA-87-PS384: both halves sign M' = Prefix || Label || len(ctx) || ctx || SHA-512(M), with an empty ctx and
%% ML-DSA-87 under an empty context; the signature is the ML-DSA-87 signature followed by the RSA-PSS signature, and
%% the carried public key is the ML-DSA-87 key followed by the DER RSAPublicKey. It is valid only if both halves
%% verify.
%%
%% An identity key has a node_id: SHA-256 over the label MACULA-NODE-ID-V1, a zero byte, the length and ASCII name
%% of the profile, and the identity key as carried. CONNECT and TLS keys have none.
%%
%% An identity key can be generated for a puzzle difficulty, so its node_id starts with that many zero bits. Each
%% try makes a new ML-DSA-87 half; a hybrid key keeps its RSA-PSS half across tries, since the node_id covers both
%% halves.
%%
%% See plans/PLAN_POST_QUANTUM_SECURITY.md, decisions D4, D5, D6 and D7.
-module(macula_node_keys).

-include_lib("public_key/include/public_key.hrl").

-export([
    generate/2,
    generate/3,
    save/2,
    load/3,
    public_key/1,
    sign/2,
    verify/4,
    node_id/1,
    node_id/2,
    puzzle_solved/2
]).

-ifdef(TEST).
-export([puzzle_candidate/1]).
-endif.

-export_type([purpose/0, algorithm/0, component/0, node_key/0, refusal/0]).

-type purpose()   :: identity | connect | tls.
-type algorithm() :: mldsa87 | rsa_pss.
-type component() :: #{algorithm := algorithm(), public := binary(), private := binary()}.
-type node_key()  :: #{purpose    := purpose(),
                       profile    := macula_crypto_profile:profile(),
                       components := [component(), ...]}.
-type refusal()   :: bad_key_file
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
-define(MLDSA87_EXPANDED_BYTES, 4896).
-define(MLDSA87_PUBLIC_BYTES, 2592).
-define(MLDSA87_SIGNATURE_BYTES, 4627).
-define(COMPOSITE_PREFIX, "CompositeAlgorithmSignatures2025").
-define(COMPOSITE_LABEL, "MACULA-ML-DSA-87-PS384").
-define(NODE_ID_LABEL, "MACULA-NODE-ID-V1").

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
generate(Purpose, _Profile, #{puzzle_difficulty := _}) when Purpose =:= connect; Purpose =:= tls ->
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

%% @doc Load the key saved for a purpose in a profile, and check it before returning it.
-spec load(file:name_all(), purpose(), macula_crypto_profile:profile()) ->
        {ok, node_key()} | {error, refusal() | file:posix() | badarg | terminated | system_limit}.
load(Path, Purpose, Profile) ->
    checked_key(decode_file(file:read_file(Path)), Purpose, Profile).

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
    crypto:sign(mldsa87, none, Message, Private);
sign(Message, #{profile := Profile,
                components := [#{algorithm := mldsa87, private := MlDsaPrivate},
                               #{algorithm := rsa_pss, private := RsaPrivate}]}) ->
    Representative = composite_representative(Message),
    {ok, #{digest := Digest} = Params} = composite_rsa_params(Profile),
    {ok, RsaKey} = decode_rsa_private(RsaPrivate),
    MlDsaSignature = crypto:sign(mldsa87, none, Representative, MlDsaPrivate),
    RsaSignature = crypto:sign(rsa, Digest, Representative, rsa_private_list(RsaKey), pss_options(Params)),
    <<MlDsaSignature/binary, RsaSignature/binary>>.

%% @doc Verify a signature with the public key a node carries, under a profile. Malformed input is refused, never
%% raised on.
-spec verify(iodata(), binary(), binary(), term()) -> boolean().
verify(Message, Signature, Public, pq_pure)
  when byte_size(Signature) =:= ?MLDSA87_SIGNATURE_BYTES, byte_size(Public) =:= ?MLDSA87_PUBLIC_BYTES ->
    verified_call(fun() -> crypto:verify(mldsa87, none, Message, Signature, Public) end);
verify(Message, <<MlDsaSignature:?MLDSA87_SIGNATURE_BYTES/binary, RsaSignature/binary>>,
       <<MlDsaPublic:?MLDSA87_PUBLIC_BYTES/binary, RsaPublicDer/binary>>, pq_hybrid) ->
    Representative = composite_representative(Message),
    MlDsaValid = verified_call(fun() ->
        crypto:verify(mldsa87, none, Representative, MlDsaSignature, MlDsaPublic)
    end),
    rsa_half_verifies(MlDsaValid, decode_rsa_public(RsaPublicDer), RsaSignature, Representative,
                      composite_rsa_params(pq_hybrid));
verify(_Message, _Signature, _Public, _Profile) ->
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

%% @doc Whether a node_id meets a puzzle difficulty: its first Difficulty bits are zero.
-spec puzzle_solved(<<_:256>>, 0..256) -> boolean().
puzzle_solved(<<_:256>> = NodeId, Difficulty) when is_integer(Difficulty), Difficulty >= 0, Difficulty =< 256 ->
    <<Prefix:Difficulty, _/bitstring>> = NodeId,
    Prefix =:= 0.

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

purpose_algorithms(identity, {ok, #{identity_signature := Algorithms}}) ->
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
    {Public, Private} = crypto:generate_key(mldsa87, []),
    #{algorithm => mldsa87, public => Public, private => Private};
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

derive_mldsa87_public(Private) when byte_size(Private) =:= ?MLDSA87_EXPANDED_BYTES ->
    try crypto:generate_key(mldsa87, [], Private) of
        {Public, _} -> {ok, Public}
    catch
        error:_ -> {error, private_key_invalid}
    end;
derive_mldsa87_public(_Private) ->
    {error, private_key_invalid}.

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
purpose_tag(tls)      -> 3.

tag_purpose(1) -> {ok, identity};
tag_purpose(2) -> {ok, connect};
tag_purpose(3) -> {ok, tls};
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
