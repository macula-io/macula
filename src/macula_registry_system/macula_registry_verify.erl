%%%-------------------------------------------------------------------
%%% @doc Macula Registry Signature Verification
%%%
%%% Provides Ed25519 digital signature operations for package verification:
%%% - Keypair generation
%%% - Package signing
%%% - Signature verification
%%% - Public key validation
%%%
%%% All functions are stateless and can be called directly.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_registry_verify).

%% API
-export([generate_keypair/0]).
-export([sign_package/3, sign_data/2]).
-export([verify_signature/3, verify_package/4]).
-export([validate_public_key/1, validate_private_key/1]).
-export([encode_public_key/1, decode_public_key/1]).
-export([compute_checksum/1]).

%% Ed25519 key sizes
-define(ED25519_PUBLIC_KEY_SIZE, 32).
-define(ED25519_PRIVATE_KEY_SIZE, 64).  %% 32 bytes seed + 32 bytes public

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Generate a new Ed25519 keypair
%% Returns {PublicKey, PrivateKey} as raw binaries
-spec generate_keypair() -> {PublicKey :: binary(), PrivateKey :: binary()}.
generate_keypair() ->
    %% crypto:generate_key/2 returns {PubKey, PrivKey} tuple for eddsa
    {PublicKey, PrivateKey} = crypto:generate_key(eddsa, ed25519),
    {PublicKey, PrivateKey}.

%% @doc Sign package data (manifest + archive)
%% The signature covers the SHA-256 hash of (manifest_binary ++ beam_archive)
-spec sign_package(ManifestBin :: binary(), BeamArchive :: binary(), PrivateKey :: binary()) ->
    {ok, Signature :: binary()} | {error, term()}.
sign_package(ManifestBin, BeamArchive, PrivateKey) ->
    case validate_private_key(PrivateKey) of
        ok ->
            %% Combine manifest and archive for signing
            DataToSign = <<ManifestBin/binary, BeamArchive/binary>>,
            {ok, sign_data(DataToSign, PrivateKey)};
        {error, _} = Error ->
            Error
    end.

%% @doc Sign arbitrary data with Ed25519 private key
-spec sign_data(Data :: binary(), PrivateKey :: binary()) -> Signature :: binary().
sign_data(Data, PrivateKey) ->
    %% Hash the data first for consistent signing regardless of size
    Hash = crypto:hash(sha256, Data),
    crypto:sign(eddsa, none, Hash, [PrivateKey, ed25519]).

%% @doc Verify a signature against data and public key
-spec verify_signature(Data :: binary(), Signature :: binary(), PublicKey :: binary()) ->
    ok | {error, invalid_signature}.
verify_signature(Data, Signature, PublicKey) ->
    Hash = crypto:hash(sha256, Data),
    case crypto:verify(eddsa, none, Hash, Signature, [PublicKey, ed25519]) of
        true -> ok;
        false -> {error, invalid_signature}
    end.

%% @doc Verify package signature
%% Reconstructs the signed data from manifest and archive, then verifies
-spec verify_package(ManifestBin :: binary(), BeamArchive :: binary(),
                     Signature :: binary(), PublicKey :: binary()) ->
    ok | {error, term()}.
verify_package(ManifestBin, BeamArchive, Signature, PublicKey) ->
    case validate_public_key(PublicKey) of
        ok ->
            DataToVerify = <<ManifestBin/binary, BeamArchive/binary>>,
            verify_signature(DataToVerify, Signature, PublicKey);
        {error, _} = Error ->
            Error
    end.

%% @doc Validate that a binary is a valid Ed25519 public key
-spec validate_public_key(PublicKey :: binary()) -> ok | {error, invalid_key}.
validate_public_key(PublicKey) when is_binary(PublicKey) ->
    case byte_size(PublicKey) of
        ?ED25519_PUBLIC_KEY_SIZE -> ok;
        _ -> {error, invalid_key}
    end;
validate_public_key(_) ->
    {error, invalid_key}.

%% @doc Validate that a binary is a valid Ed25519 private key
-spec validate_private_key(PrivateKey :: binary()) -> ok | {error, invalid_key}.
validate_private_key(PrivateKey) when is_binary(PrivateKey) ->
    case byte_size(PrivateKey) of
        ?ED25519_PRIVATE_KEY_SIZE -> ok;
        ?ED25519_PUBLIC_KEY_SIZE -> ok;  %% Some APIs use 32-byte seed
        _ -> {error, invalid_key}
    end;
validate_private_key(_) ->
    {error, invalid_key}.

%% @doc Encode public key as hex string for display/storage
-spec encode_public_key(PublicKey :: binary()) -> binary().
encode_public_key(PublicKey) ->
    binary:encode_hex(PublicKey, lowercase).

%% @doc Decode hex-encoded public key back to binary
-spec decode_public_key(HexKey :: binary()) -> {ok, binary()} | {error, invalid_format}.
decode_public_key(HexKey) when is_binary(HexKey) ->
    try
        Decoded = binary:decode_hex(HexKey),
        case validate_public_key(Decoded) of
            ok -> {ok, Decoded};
            Error -> Error
        end
    catch
        _:_ -> {error, invalid_format}
    end;
decode_public_key(_) ->
    {error, invalid_format}.

%% @doc Compute SHA-256 checksum of data
-spec compute_checksum(Data :: binary()) -> binary().
compute_checksum(Data) ->
    crypto:hash(sha256, Data).
