%%%-------------------------------------------------------------------
%%% @doc Macula Self-Sovereign Certificate System
%%%
%%% Provides Ed25519 DID-anchored certificate generation and verification.
%%% Unlike traditional PKI, these certificates do not require external CAs.
%%%
%%% Certificate hierarchy:
%%% - Realm certificates are self-signed (root of trust for a namespace)
%%% - Instance certificates are signed by the realm certificate
%%%
%%% Example usage:
%%% ```
%%% %% Generate realm keypair and certificate
%%% {PubKey, PrivKey} = macula_cert:generate_keypair(),
%%% {ok, RealmCert} = macula_cert:generate_realm_cert(
%%%     <<"did:macula:io.example">>,
%%%     PubKey,
%%%     PrivKey
%%% ),
%%%
%%% %% Generate instance certificate signed by realm
%%% {InstPub, InstPriv} = macula_cert:generate_keypair(),
%%% {ok, InstCert} = macula_cert:generate_instance_cert(
%%%     <<"did:macula:io.example.app.node01">>,
%%%     InstPub,
%%%     RealmCert,
%%%     PrivKey
%%% ),
%%%
%%% %% Verify certificate chain
%%% ok = macula_cert:verify_cert(InstCert, RealmCert).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cert).

-include("macula_cert.hrl").

%% API - Keypair Generation
-export([generate_keypair/0]).

%% API - Certificate Generation
-export([generate_realm_cert/3, generate_realm_cert/4]).
-export([generate_instance_cert/4, generate_instance_cert/5]).
-export([sign_cert_request/3]).

%% API - Certificate Verification
-export([verify_cert/2, verify_self_signed/1]).
-export([is_expired/1, is_valid_now/1]).

%% API - Encoding/Decoding
-export([encode/1, decode/1]).
-export([to_map/1, from_map/1]).
-export([canonical_form/1]).

%% API - Utilities
-export([did_to_cn/1, cn_to_did/1]).
-export([generate_serial/0]).
-export([extract_realm_did/1]).

%%%===================================================================
%%% Keypair Generation
%%%===================================================================

%% @doc Generate a new Ed25519 keypair
%% Returns {PublicKey, PrivateKey} as raw binaries.
-spec generate_keypair() -> {PublicKey :: binary(), PrivateKey :: binary()}.
generate_keypair() ->
    crypto:generate_key(eddsa, ed25519).

%%%===================================================================
%%% Certificate Generation
%%%===================================================================

%% @doc Generate a self-signed realm certificate
%% Realm certificates are the root of trust for a namespace.
-spec generate_realm_cert(RealmDID :: binary(), PublicKey :: binary(),
                          PrivateKey :: binary()) ->
    {ok, macula_cert()} | {error, term()}.
generate_realm_cert(RealmDID, PublicKey, PrivateKey) ->
    generate_realm_cert(RealmDID, PublicKey, PrivateKey, ?REALM_VALIDITY_DAYS).

%% @doc Generate a self-signed realm certificate with custom validity
-spec generate_realm_cert(RealmDID :: binary(), PublicKey :: binary(),
                          PrivateKey :: binary(), ValidityDays :: pos_integer()) ->
    {ok, macula_cert()} | {error, term()}.
generate_realm_cert(RealmDID, PublicKey, PrivateKey, ValidityDays) ->
    case validate_keys(PublicKey, PrivateKey) of
        ok ->
            Now = erlang:system_time(second),
            CN = did_to_cn(RealmDID),
            Serial = generate_serial(),

            %% For self-signed, issuer = subject
            UnsignedCert = #macula_cert{
                subject_did = RealmDID,
                subject_cn = CN,
                issuer_did = RealmDID,
                issuer_cn = CN,
                not_before = Now,
                not_after = Now + (ValidityDays * 86400),
                public_key = PublicKey,
                signature = <<>>,  %% Will be set after signing
                serial = Serial,
                version = 1
            },

            %% Sign the canonical form
            Canonical = canonical_form(UnsignedCert),
            Signature = sign_data(Canonical, PrivateKey),

            {ok, UnsignedCert#macula_cert{signature = Signature}};

        {error, _} = Error ->
            Error
    end.

%% @doc Generate an instance certificate signed by a realm certificate
%% The instance DID must be under the realm's namespace.
-spec generate_instance_cert(InstanceDID :: binary(), InstancePubKey :: binary(),
                             RealmCert :: macula_cert(), RealmPrivKey :: binary()) ->
    {ok, macula_cert()} | {error, term()}.
generate_instance_cert(InstanceDID, InstancePubKey, RealmCert, RealmPrivKey) ->
    generate_instance_cert(InstanceDID, InstancePubKey, RealmCert, RealmPrivKey,
                           ?DEFAULT_VALIDITY_DAYS).

%% @doc Generate an instance certificate with custom validity
-spec generate_instance_cert(InstanceDID :: binary(), InstancePubKey :: binary(),
                             RealmCert :: macula_cert(), RealmPrivKey :: binary(),
                             ValidityDays :: pos_integer()) ->
    {ok, macula_cert()} | {error, term()}.
generate_instance_cert(InstanceDID, InstancePubKey, RealmCert, RealmPrivKey, ValidityDays) ->
    #macula_cert{subject_did = RealmDID, public_key = RealmPubKey} = RealmCert,

    %% Verify the realm certificate is valid
    case verify_self_signed(RealmCert) of
        ok ->
            %% Verify instance DID is under realm namespace
            case is_did_under_realm(InstanceDID, RealmDID) of
                true ->
                    %% Verify realm private key matches public key
                    case verify_keypair(RealmPubKey, RealmPrivKey) of
                        ok ->
                            do_generate_instance_cert(
                                InstanceDID, InstancePubKey, RealmCert,
                                RealmPrivKey, ValidityDays
                            );
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, {did_not_under_realm, InstanceDID, RealmDID}}
            end;
        {error, _} = Error ->
            Error
    end.

%% @private
do_generate_instance_cert(InstanceDID, InstancePubKey, RealmCert, RealmPrivKey, ValidityDays) ->
    #macula_cert{subject_did = RealmDID, subject_cn = RealmCN} = RealmCert,

    Now = erlang:system_time(second),
    CN = did_to_cn(InstanceDID),
    Serial = generate_serial(),

    UnsignedCert = #macula_cert{
        subject_did = InstanceDID,
        subject_cn = CN,
        issuer_did = RealmDID,
        issuer_cn = RealmCN,
        not_before = Now,
        not_after = Now + (ValidityDays * 86400),
        public_key = InstancePubKey,
        signature = <<>>,
        serial = Serial,
        version = 1
    },

    %% Sign with realm's private key
    Canonical = canonical_form(UnsignedCert),
    Signature = sign_data(Canonical, RealmPrivKey),

    {ok, UnsignedCert#macula_cert{signature = Signature}}.

%% @doc Sign a certificate request
%% Used when processing CSRs from remote instances.
-spec sign_cert_request(Request :: macula_cert_request(), RealmCert :: macula_cert(),
                        RealmPrivKey :: binary()) ->
    {ok, macula_cert()} | {error, term()}.
sign_cert_request(Request, RealmCert, RealmPrivKey) ->
    #macula_cert_request{
        subject_did = SubjectDID,
        public_key = PubKey,
        validity_days = ValidityDays
    } = Request,
    generate_instance_cert(SubjectDID, PubKey, RealmCert, RealmPrivKey, ValidityDays).

%%%===================================================================
%%% Certificate Verification
%%%===================================================================

%% @doc Verify an instance certificate against its issuer (realm) certificate
-spec verify_cert(InstanceCert :: macula_cert(), IssuerCert :: macula_cert()) ->
    ok | {error, term()}.
verify_cert(InstanceCert, IssuerCert) ->
    #macula_cert{
        issuer_did = IssuerDID,
        signature = Signature
    } = InstanceCert,

    #macula_cert{
        subject_did = ExpectedIssuerDID,
        public_key = IssuerPubKey
    } = IssuerCert,

    %% Verify issuer matches
    case IssuerDID =:= ExpectedIssuerDID of
        true ->
            %% Verify signature
            Canonical = canonical_form(InstanceCert),
            case verify_signature(Canonical, Signature, IssuerPubKey) of
                ok ->
                    %% Verify not expired
                    case is_valid_now(InstanceCert) of
                        true -> ok;
                        false -> {error, certificate_expired}
                    end;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {issuer_mismatch, IssuerDID, ExpectedIssuerDID}}
    end.

%% @doc Verify a self-signed (realm) certificate
-spec verify_self_signed(Cert :: macula_cert()) -> ok | {error, term()}.
verify_self_signed(Cert) ->
    #macula_cert{
        subject_did = SubjectDID,
        issuer_did = IssuerDID,
        public_key = PubKey,
        signature = Signature
    } = Cert,

    %% Self-signed means subject == issuer
    case SubjectDID =:= IssuerDID of
        true ->
            Canonical = canonical_form(Cert),
            case verify_signature(Canonical, Signature, PubKey) of
                ok ->
                    case is_valid_now(Cert) of
                        true -> ok;
                        false -> {error, certificate_expired}
                    end;
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, not_self_signed}
    end.

%% @doc Check if a certificate is expired
-spec is_expired(Cert :: macula_cert()) -> boolean().
is_expired(#macula_cert{not_after = NotAfter}) ->
    erlang:system_time(second) > NotAfter.

%% @doc Check if a certificate is currently valid (within validity period)
-spec is_valid_now(Cert :: macula_cert()) -> boolean().
is_valid_now(#macula_cert{not_before = NotBefore, not_after = NotAfter}) ->
    Now = erlang:system_time(second),
    Now >= NotBefore andalso Now =< NotAfter.

%%%===================================================================
%%% Encoding/Decoding
%%%===================================================================

%% @doc Encode certificate to binary format
%% Uses term_to_binary for now; can switch to CBOR/JSON for interop.
-spec encode(Cert :: macula_cert()) -> binary().
encode(Cert) ->
    term_to_binary(to_map(Cert), [compressed]).

%% @doc Decode certificate from binary format
-spec decode(Binary :: binary()) -> {ok, macula_cert()} | {error, term()}.
decode(Binary) ->
    handle_binary_decode(catch binary_to_term(Binary, [safe])).

%% @private Handle binary decode result
handle_binary_decode({'EXIT', _}) ->
    {error, invalid_certificate_format};
handle_binary_decode(Map) when is_map(Map) ->
    from_map(Map);
handle_binary_decode(_) ->
    {error, invalid_certificate_format}.

%% @doc Convert certificate record to map
-spec to_map(Cert :: macula_cert()) -> map().
to_map(#macula_cert{} = Cert) ->
    #{
        version => Cert#macula_cert.version,
        serial => Cert#macula_cert.serial,
        subject_did => Cert#macula_cert.subject_did,
        subject_cn => Cert#macula_cert.subject_cn,
        issuer_did => Cert#macula_cert.issuer_did,
        issuer_cn => Cert#macula_cert.issuer_cn,
        not_before => Cert#macula_cert.not_before,
        not_after => Cert#macula_cert.not_after,
        public_key => Cert#macula_cert.public_key,
        signature => Cert#macula_cert.signature,
        extensions => Cert#macula_cert.extensions
    }.

%% @doc Convert map to certificate record
-spec from_map(Map :: map()) -> {ok, macula_cert()} | {error, term()}.
from_map(Map) when is_map(Map) ->
    build_cert_from_map(catch build_cert_record(Map));
from_map(_) ->
    {error, invalid_map}.

%% @private Build certificate record from map (may throw on missing keys)
build_cert_record(Map) ->
    #macula_cert{
        version = maps:get(version, Map, 1),
        serial = maps:get(serial, Map),
        subject_did = maps:get(subject_did, Map),
        subject_cn = maps:get(subject_cn, Map),
        issuer_did = maps:get(issuer_did, Map),
        issuer_cn = maps:get(issuer_cn, Map),
        not_before = maps:get(not_before, Map),
        not_after = maps:get(not_after, Map),
        public_key = maps:get(public_key, Map),
        signature = maps:get(signature, Map),
        extensions = maps:get(extensions, Map, #{})
    }.

%% @private Handle certificate build result
build_cert_from_map({'EXIT', _}) ->
    {error, missing_required_fields};
build_cert_from_map(#macula_cert{} = Cert) ->
    {ok, Cert}.

%% @doc Generate canonical form for signing/verification
%% The canonical form excludes the signature field and is deterministic.
-spec canonical_form(Cert :: macula_cert()) -> binary().
canonical_form(#macula_cert{} = Cert) ->
    %% Build a deterministic binary representation
    %% Order matters for reproducibility
    Data = [
        <<"v">>, integer_to_binary(Cert#macula_cert.version),
        <<"s">>, Cert#macula_cert.serial,
        <<"sd">>, Cert#macula_cert.subject_did,
        <<"sc">>, Cert#macula_cert.subject_cn,
        <<"id">>, Cert#macula_cert.issuer_did,
        <<"ic">>, Cert#macula_cert.issuer_cn,
        <<"nb">>, integer_to_binary(Cert#macula_cert.not_before),
        <<"na">>, integer_to_binary(Cert#macula_cert.not_after),
        <<"pk">>, Cert#macula_cert.public_key
        %% Note: signature is NOT included in canonical form
        %% Note: extensions are NOT included (for simplicity in v1)
    ],
    iolist_to_binary(Data).

%%%===================================================================
%%% Utility Functions
%%%===================================================================

%% @doc Convert DID to common name format
%% Example: <<"did:macula:io.example.app">> -> <<"app.io.example">>
-spec did_to_cn(DID :: binary()) -> binary().
did_to_cn(<<"did:macula:", Identity/binary>>) ->
    %% Reverse the dot-separated parts for CN format
    Parts = binary:split(Identity, <<".">>, [global]),
    ReversedParts = lists:reverse(Parts),
    iolist_to_binary(lists:join(<<".">>, ReversedParts));
did_to_cn(DID) ->
    %% If not in did:macula: format, return as-is
    DID.

%% @doc Convert common name to DID format
%% Example: <<"app.io.example">> -> <<"did:macula:io.example.app">>
-spec cn_to_did(CN :: binary()) -> binary().
cn_to_did(CN) ->
    Parts = binary:split(CN, <<".">>, [global]),
    ReversedParts = lists:reverse(Parts),
    Identity = iolist_to_binary(lists:join(<<".">>, ReversedParts)),
    <<"did:macula:", Identity/binary>>.

%% @doc Generate a random certificate serial number
-spec generate_serial() -> binary().
generate_serial() ->
    crypto:strong_rand_bytes(?CERT_SERIAL_SIZE).

%% @doc Extract realm DID from an instance DID
%% Example: <<"did:macula:io.example.app.node01">> -> <<"did:macula:io.example">>
-spec extract_realm_did(InstanceDID :: binary()) -> binary().
extract_realm_did(<<"did:macula:", Identity/binary>>) ->
    Parts = binary:split(Identity, <<".">>, [global]),
    %% Realm is typically first 2 parts (e.g., io.example)
    case length(Parts) >= 2 of
        true ->
            [P1, P2 | _] = Parts,
            <<"did:macula:", P1/binary, ".", P2/binary>>;
        false ->
            %% Return as-is if not enough parts
            <<"did:macula:", Identity/binary>>
    end;
extract_realm_did(DID) ->
    DID.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Validate keypair
-spec validate_keys(PublicKey :: binary(), PrivateKey :: binary()) ->
    ok | {error, term()}.
validate_keys(PublicKey, PrivateKey) ->
    case byte_size(PublicKey) of
        ?ED25519_PUBLIC_KEY_SIZE ->
            case validate_private_key_size(PrivateKey) of
                ok -> ok;
                Error -> Error
            end;
        _ ->
            {error, invalid_public_key}
    end.

%% @private
validate_private_key_size(PrivKey) when byte_size(PrivKey) =:= 64 -> ok;
validate_private_key_size(PrivKey) when byte_size(PrivKey) =:= 32 -> ok;
validate_private_key_size(_) -> {error, invalid_private_key}.

%% @private Verify that a private key corresponds to a public key
-spec verify_keypair(PublicKey :: binary(), PrivateKey :: binary()) ->
    ok | {error, term()}.
verify_keypair(PublicKey, PrivateKey) ->
    %% Sign some test data and verify with public key
    TestData = <<"keypair_verification_test">>,
    Signature = sign_data(TestData, PrivateKey),
    verify_signature(TestData, Signature, PublicKey).

%% @private Check if instance DID is under realm namespace
-spec is_did_under_realm(InstanceDID :: binary(), RealmDID :: binary()) -> boolean().
is_did_under_realm(InstanceDID, RealmDID) ->
    %% Instance DID must start with realm DID
    RealmPrefix = case RealmDID of
        <<"did:macula:", Identity/binary>> -> Identity;
        _ -> RealmDID
    end,
    InstanceIdentity = case InstanceDID of
        <<"did:macula:", Id/binary>> -> Id;
        _ -> InstanceDID
    end,
    %% Check prefix match
    PrefixSize = byte_size(RealmPrefix),
    case InstanceIdentity of
        <<RealmPrefix:PrefixSize/binary, ".", _/binary>> -> true;
        RealmPrefix -> true;  %% Exact match
        _ -> false
    end.

%% @private Sign data with Ed25519 private key
-spec sign_data(Data :: binary(), PrivateKey :: binary()) -> binary().
sign_data(Data, PrivateKey) ->
    Hash = crypto:hash(sha256, Data),
    crypto:sign(eddsa, none, Hash, [PrivateKey, ed25519]).

%% @private Verify Ed25519 signature
-spec verify_signature(Data :: binary(), Signature :: binary(), PublicKey :: binary()) ->
    ok | {error, invalid_signature}.
verify_signature(Data, Signature, PublicKey) ->
    Hash = crypto:hash(sha256, Data),
    case crypto:verify(eddsa, none, Hash, Signature, [PublicKey, ed25519]) of
        true -> ok;
        false -> {error, invalid_signature}
    end.
