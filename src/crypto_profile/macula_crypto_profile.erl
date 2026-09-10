%% @doc The post-quantum crypto profile a node runs.
%%
%% A realm runs one profile, and a station instance serves one profile.
%% The profile is set in the `macula' application environment under
%% `crypto_profile'. There is no default: the application refuses to
%% start without exactly one known profile.
%%
%% <ul>
%%   <li>`us_national_security': the CNSA 2.0 algorithms. ML-KEM-1024
%%       key exchange, ML-DSA-87 signatures, AES-256, no classical
%%       half.</li>
%%   <li>`eu': SecP384r1MLKEM1024 hybrid key exchange, ML-DSA-87 alone
%%       in TLS, and every other signature hybrid: ML-DSA-87 plus
%%       RSA-PSS-4096 with SHA-384, valid only if both halves
%%       verify.</li>
%% </ul>
%%
%% See plans/PLAN_POST_QUANTUM_SECURITY.md, decisions D1 to D5 and D24.
-module(macula_crypto_profile).

-export([
    profiles/0,
    definition/1,
    validate/1,
    configured/0
]).

-export_type([profile/0, definition/0, signature_algorithm/0, refusal/0]).

-type profile() :: us_national_security | eu.
-type rsa_pss_params() :: #{
    modulus_bits    := 4096,
    public_exponent := 65537,
    digest          := sha384,
    mgf1_digest     := sha384,
    salt_bytes      := 48
}.
-type signature_algorithm() :: mldsa87 | {rsa_pss, rsa_pss_params()}.
-type definition() :: #{
    profile                 := profile(),
    key_exchange_group      := mlkem1024 | secp384r1_mlkem1024,
    tls_signature_scheme    := mldsa87,
    tls_cipher_suite        := tls_aes_256_gcm_sha384,
    identity_signature      := [signature_algorithm(), ...],
    connect_proof_signature := [signature_algorithm(), ...],
    status_signature        := [signature_algorithm(), ...],
    binding_digest          := sha384,
    content_id_digest       := sha384,
    node_id_digest          := sha256
}.
-type refusal() :: crypto_profile_missing
                 | {crypto_profile_unknown, term()}
                 | {crypto_profile_not_single, list()}.

-define(APP, macula).
-define(ENV_KEY, crypto_profile).

-define(RSA_PSS_4096, {rsa_pss, #{modulus_bits    => 4096,
                                  public_exponent => 65537,
                                  digest          => sha384,
                                  mgf1_digest     => sha384,
                                  salt_bytes      => 48}}).

%%------------------------------------------------------------------
%% Profiles
%%------------------------------------------------------------------

%% @doc The profiles a node can run.
-spec profiles() -> [profile(), ...].
profiles() ->
    [us_national_security, eu].

%% @doc The algorithms of a profile. A signature is a list of
%% algorithms: ML-DSA-87 first, then the classical half of a hybrid
%% signature when the profile has one.
-spec definition(term()) ->
        {ok, definition()} | {error, {crypto_profile_unknown, term()}}.
definition(us_national_security) ->
    {ok, profile_definition(us_national_security, mlkem1024, [mldsa87])};
definition(eu) ->
    {ok, profile_definition(eu, secp384r1_mlkem1024, [mldsa87, ?RSA_PSS_4096])};
definition(Other) ->
    {error, {crypto_profile_unknown, Other}}.

%%------------------------------------------------------------------
%% Selection
%%------------------------------------------------------------------

%% @doc Check a configured value: it must be exactly one known profile.
%% A list is refused, because one node runs one profile.
-spec validate(term()) -> {ok, profile()} | {error, refusal()}.
validate(undefined) ->
    {error, crypto_profile_missing};
validate([First | _] = Profiles) when is_atom(First) ->
    {error, {crypto_profile_not_single, Profiles}};
validate(Value) ->
    known_profile(Value, lists:member(Value, profiles())).

%% @doc The profile set in the application environment, validated.
-spec configured() -> {ok, profile()} | {error, refusal()}.
configured() ->
    validate(application:get_env(?APP, ?ENV_KEY, undefined)).

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

profile_definition(Profile, Group, Signature) ->
    #{profile                 => Profile,
      key_exchange_group      => Group,
      tls_signature_scheme    => mldsa87,
      tls_cipher_suite        => tls_aes_256_gcm_sha384,
      identity_signature      => Signature,
      connect_proof_signature => Signature,
      status_signature        => Signature,
      binding_digest          => sha384,
      content_id_digest       => sha384,
      node_id_digest          => sha256}.

known_profile(Profile, true) -> {ok, Profile};
known_profile(Value, false)  -> {error, {crypto_profile_unknown, Value}}.
