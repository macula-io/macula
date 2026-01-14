%%%-------------------------------------------------------------------
%%% @doc Macula Certificate System Records
%%%
%%% Defines record structures for self-sovereign certificates anchored
%%% to DIDs (Decentralized Identifiers). These certificates use Ed25519
%%% signatures and do not require external Certificate Authorities.
%%%
%%% Certificate hierarchy:
%%% - Realm certificates are self-signed (root of trust)
%%% - Instance certificates are signed by realm certificates
%%%
%%% To use: -include("macula_cert.hrl").
%%% @end
%%%-------------------------------------------------------------------

-ifndef(MACULA_CERT_HRL).
-define(MACULA_CERT_HRL, true).

%%%===================================================================
%%% Certificate Record
%%%===================================================================

%% @doc Macula certificate structure
%%
%% A self-sovereign certificate anchored to a DID:
%% - subject: The entity this certificate represents
%% - issuer: The entity that signed this certificate
%% - not_before/not_after: Validity period (Unix timestamps)
%% - public_key: Subject's Ed25519 public key (32 bytes)
%% - signature: Issuer's Ed25519 signature over canonical form (64 bytes)
%% - serial: Unique identifier for this certificate
%% - version: Certificate format version
-record(macula_cert, {
    %% Subject identity
    subject_did :: binary(),           %% e.g., <<"did:macula:io.example.app.node01">>
    subject_cn :: binary(),            %% Common name, e.g., <<"app.node01.io.example">>

    %% Issuer identity
    issuer_did :: binary(),            %% e.g., <<"did:macula:io.example">>
    issuer_cn :: binary(),             %% Common name of issuer

    %% Validity period (Unix timestamps in seconds)
    not_before :: non_neg_integer(),   %% Certificate valid from
    not_after :: non_neg_integer(),    %% Certificate valid until

    %% Cryptographic material
    public_key :: binary(),            %% Subject's Ed25519 public key (32 bytes)
    signature :: binary(),             %% Ed25519 signature (64 bytes)

    %% Metadata
    serial :: binary(),                %% Unique certificate ID (16 bytes random)
    version = 1 :: pos_integer(),      %% Format version

    %% Optional extensions
    extensions = #{} :: map()          %% Future extensions (capabilities, constraints)
}).

-type macula_cert() :: #macula_cert{}.

%%%===================================================================
%%% Certificate Request Record
%%%===================================================================

%% @doc Certificate signing request
%%
%% Used when an instance requests a certificate from a realm authority.
-record(macula_cert_request, {
    subject_did :: binary(),           %% Requested DID
    subject_cn :: binary(),            %% Requested common name
    public_key :: binary(),            %% Requester's public key
    validity_days = 90 :: pos_integer() %% Requested validity period
}).

-type macula_cert_request() :: #macula_cert_request{}.

%%%===================================================================
%%% Trust Entry Record
%%%===================================================================

%% @doc Trust store entry for a realm certificate
-record(trust_entry, {
    realm_did :: binary(),             %% Realm DID (key)
    cert :: #macula_cert{},            %% Realm's self-signed certificate
    added_at :: non_neg_integer(),     %% When trust was established (Unix timestamp)
    verified :: boolean(),             %% Whether manually verified
    notes :: binary()                  %% Optional notes about trust decision
}).

-type trust_entry() :: #trust_entry{}.

%%%===================================================================
%%% Constants
%%%===================================================================

%% Ed25519 key sizes
-define(ED25519_PUBLIC_KEY_SIZE, 32).
-define(ED25519_SIGNATURE_SIZE, 64).

%% Certificate serial number size
-define(CERT_SERIAL_SIZE, 16).

%% Default validity period (90 days)
-define(DEFAULT_VALIDITY_DAYS, 90).

%% Realm certificate validity (1 year)
-define(REALM_VALIDITY_DAYS, 365).

-endif. %% MACULA_CERT_HRL
