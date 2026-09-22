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
%% Nothing reads it: the QUIC NIF offers the same key exchange groups
%% whatever a node's profile. See `definition/1' for what is actually
%% negotiated and why the field is kept anyway. Read a profile NAME as
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
    %% ⚠ DECLARED TARGET. NOT READ BY ANYTHING.
    %%
    %% What a connection negotiates is decided by the `macula-pqc' crate,
    %% which every TLS configuration in `native/macula_quic' is built from,
    %% whatever the node's profile: SecP384r1MLKEM1024, then
    %% SecP256r1MLKEM768, nothing classical. Two nodes on this version
    %% negotiate SecP384r1MLKEM1024, which is `pq_hybrid''s declared group,
    %% because every node offers it and not because this field asked.
    %% `mlkem1024', `pq_pure''s declared group, is never negotiated: no pure
    %% ML-KEM group is offered. (BSI TR-02102-2 INTENDS TO RECOMMEND
    %% SecP384r1MLKEM1024 once the corresponding RFC is adopted. Say
    %% "intends to recommend". BSI does not recommend it yet.)
    %%
    %% The field is kept, deliberately, as the target each profile is
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
%% WIRE. Nothing reads it. The QUIC NIF takes every TLS configuration from
%% the `macula-pqc' crate, whatever the node's profile, and offers
%% SecP384r1MLKEM1024 then SecP256r1MLKEM768 and nothing classical, so two
%% nodes on this version negotiate SecP384r1MLKEM1024.
%%
%% The declared group of `pq_hybrid', `secp384r1_mlkem1024', is therefore
%% the one negotiated, but because every node offers it, not because the
%% profile asked for it. The declared group of `pq_pure', `mlkem1024', is
%% never negotiated: no pure ML-KEM group is offered.
%%
%% So a profile NAME names the policy the profile serves. It is never a
%% statement about the key exchange a connection got. Key exchange is
%% post-quantum on every link; the profile does not choose the group.
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
