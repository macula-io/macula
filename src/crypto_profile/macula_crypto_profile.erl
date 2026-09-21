%% @doc The post-quantum crypto profile a node runs.
%%
%% A realm runs one profile, and a station instance serves one profile.
%% The profile is set in the `macula' application environment under
%% `crypto_profile'. There is no default: the application refuses to
%% start without exactly one known profile.
%%
%% <ul>
%%   <li>`pq_pure': the CNSA 2.0 algorithms. ML-KEM-1024
%%       key exchange as its DECLARED TARGET, ML-DSA-87 signatures,
%%       AES-256, no classical half.</li>
%%   <li>`pq_hybrid': SecP384r1MLKEM1024 hybrid key exchange as its
%%       DECLARED TARGET, ML-DSA-87 alone
%%       in TLS, and every other signature hybrid: ML-DSA-87 plus
%%       RSA-PSS-4096 with SHA-384, valid only if both halves
%%       verify.</li>
%% </ul>
%%
%% ⚠ `key_exchange_group' IS A TARGET AND NOT A DESCRIPTION OF THE WIRE.
%% Neither profile's group is negotiated by anything this node runs. See
%% `definition/1' for what is actually negotiated and why the field is
%% kept anyway. Read a profile NAME as
%% naming the policy the profile serves, never as a statement about the
%% key exchange a connection got.
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

-type profile() :: pq_pure | pq_hybrid.
-type rsa_pss_params() :: #{
    modulus_bits    := 4096,
    public_exponent := 65537,
    digest          := sha384,
    mgf1_digest     := sha384,
    salt_bytes      := 48
}.
-type signature_algorithm() :: mldsa87 | {rsa_pss, rsa_pss_params()}.
-type definition() :: #{
    %% ⚠ DECLARED TARGET. NOT NEGOTIATED. NOT NEGOTIABLE TODAY.
    %%
    %% Nothing reads this field. What a connection actually negotiates is
    %% whatever the QUIC NIF's TLS provider defaults to: the NIF selects
    %% rustls on its `ring' feature and configures no group list at all
    %% (`native/macula_quic/Cargo.toml', and the provider calls in
    %% `config.rs' and `cert.rs'), so the offered groups are X25519,
    %% SECP256R1 and SECP384R1. All classical. The ring provider
    %% implements no ML-KEM.
    %%
    %% Neither declared group is reachable by configuration. `mlkem1024'
    %% exists in rustls's aws-lc-rs provider, which we do not link.
    %% `secp384r1_mlkem1024' exists in NO rustls we hold, under either
    %% provider: BSI TR-02102-2 INTENDS TO RECOMMEND SecP384r1MLKEM1024
    %% once the corresponding RFC is adopted, and until that happens
    %% nobody has implemented it. (Say "intends to recommend". BSI does
    %% not recommend it yet.)
    %%
    %% The field is kept, deliberately, as the target the EU profile is
    %% aiming at. Keeping a declared-but-inert field is exactly the defect
    %% macula#15 exists to punish, so it is kept ONLY with this said next
    %% to it, and honouring it is tracked as work rather than assumed.
    %%
    %% What this does NOT say: it makes no claim either way about the
    %% signature half of this map. Signatures are a separate question and
    %% a separate audit.
    key_exchange_group      := mlkem1024 | secp384r1_mlkem1024,
    tls_signature_scheme    := mldsa87,
    identity_signature      := [signature_algorithm(), ...],
    connect_proof_signature := [signature_algorithm(), ...]
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
    [pq_pure, pq_hybrid].

%% @doc The algorithms of a profile. A signature is a list of
%% algorithms: ML-DSA-87 first, then the classical half of a hybrid
%% signature when the profile has one.
%%
%% == What is actually negotiated ==
%%
%% `key_exchange_group' IS A DECLARED TARGET AND NOT A DESCRIPTION OF THE
%% WIRE. Nothing reads it. What a connection negotiates is whatever the
%% QUIC NIF's TLS provider defaults to, and the NIF selects rustls on its
%% `ring' feature and configures no group list, so the offered groups are
%% X25519, SECP256R1 and SECP384R1. All classical. The ring provider
%% implements no ML-KEM.
%%
%% Neither declared group is reachable by configuration today.
%% `mlkem1024' exists in rustls's aws-lc-rs provider, which this NIF does
%% not link. `secp384r1_mlkem1024' exists in no rustls under either
%% provider: BSI TR-02102-2 INTENDS TO RECOMMEND SecP384r1MLKEM1024 once
%% the corresponding RFC is adopted, and until then nobody has
%% implemented it.
%%
%% So a profile NAME names the policy the profile serves. It is never a
%% statement about the key exchange a connection got. Post-quantum
%% SIGNATURES are real; key exchange is classical.
%%
%% This makes no claim either way about the signature half of the map.
-spec definition(term()) ->
        {ok, definition()} | {error, {crypto_profile_unknown, term()}}.
definition(pq_pure) ->
    {ok, profile_definition(mlkem1024, [mldsa87])};
definition(pq_hybrid) ->
    {ok, profile_definition(secp384r1_mlkem1024, [mldsa87, ?RSA_PSS_4096])};
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

%% Every field here is read by something, except `key_exchange_group',
%% which is kept on purpose and carries its own warning in `definition()'.
%%
%% Six fields were removed because nothing had ever read them:
%% `tls_cipher_suite', `status_signature', `binding_digest',
%% `content_id_digest', `node_id_digest' and `profile' itself. Each stated a
%% value that the code hardcodes at its use site, so the profile could
%% disagree with the node and nothing would notice. `profile' went last: a
%% caller already has the profile in hand, since it is the argument to
%% `definition/1'. `definition_key_set_is_pinned_test' is what keeps this
%% true, and it is a KEY-SET PIN rather than a reader check: the list of
%% readers is maintained by hand.
profile_definition(Group, Signature) ->
    #{key_exchange_group      => Group,
      tls_signature_scheme    => mldsa87,
      identity_signature      => Signature,
      connect_proof_signature => Signature}.

known_profile(Profile, true) -> {ok, Profile};
known_profile(Value, false)  -> {error, {crypto_profile_unknown, Value}}.
