%% @doc PKARR-compatible signed records (envelope + node_record + tombstone).
%%
%% A record is a map carrying:
%% <ul>
%%   <li>`type' (uint) — record type tag (`0x01' = node_record, `0x0C' = tombstone)</li>
%%   <li>`key' (32B) — owning Ed25519 pubkey</li>
%%   <li>`version' (16B) — UUIDv7</li>
%%   <li>`created_at' / `expires_at' (ms since epoch)</li>
%%   <li>`payload' (map) — type-specific</li>
%%   <li>`signature' (64B) — Ed25519 signature; present after `sign/2'</li>
%% </ul>
%%
%% On the wire records are CBOR maps with single-letter keys
%% (`t', `k', `v', `c', `x', `p', `s') per `Part 6 §9'.
%% Signatures are Ed25519 over `"macula-v2-record\0" ++ canonical_cbor(unsigned)'.
-module(macula_record).

-export([
    %% Constructors
    node_record/3, node_record/4,
    realm_directory/3, realm_directory/4,
    realm_stations/2, realm_stations/3,
    realm_member_endorsement/2, realm_member_endorsement/3,
    procedure_advertisement/3, procedure_advertisement/4,
    content_announcement/3, content_announcement/4,
    foundation_seed_list/2, foundation_seed_list/3,
    foundation_parameter/3, foundation_parameter/4,
    foundation_realm_trust_list/2, foundation_realm_trust_list/3,
    foundation_t3_attestation/3, foundation_t3_attestation/4,
    tombstone/3, tombstone/4,

    %% Generic builder for domain-defined record types (tag 0x20-0xFF).
    %% Domain code (e.g. realm fact types) supplies its own type tag,
    %% storage key, and CBOR payload map; SDK only signs + ships.
    envelope/4,

    %% Sign / verify
    sign/2, verify/1,

    %% Owner refresh — new version + timestamps, re-sign (Part 3 §11)
    refresh/2,

    %% Wire codec
    encode/1, decode/1,

    %% Accessors
    type/1, key/1, version/1, created_at/1, expires_at/1,
    payload/1, signature/1,

    %% DHT storage-key derivation (Part 3 §3.3)
    storage_key/1
]).

-export_type([
    record/0,
    type_tag/0,
    version/0,
    node_record_opts/0,
    realm_directory_opts/0,
    realm_station_entry/0,
    realm_stations_opts/0,
    realm_member_endorsement_opts/0,
    procedure_advertisement_opts/0,
    content_announcement_opts/0,
    foundation_seed/0,
    foundation_seed_list_opts/0,
    foundation_parameter_opts/0,
    foundation_realm_trust_list_opts/0,
    foundation_t3_attestation_opts/0,
    tombstone_opts/0
]).

%% Domain separation prefix for record signatures (Part 6 §10.2).
-define(SIG_DOMAIN, "macula-v2-record\0").

%% Type tag allocation (1 byte):
%%   0x01-0x1F  reserved for macula infrastructure types (this module)
%%   0x20-0xFF  domain-defined; callers manage their own registry
%%              and use envelope/4 to build records.
-define(TYPE_NODE_RECORD,                  16#01).
-define(TYPE_REALM_DIRECTORY,              16#03).
-define(TYPE_REALM_STATIONS,               16#04).
-define(TYPE_REALM_MEMBER_ENDORSEMENT,     16#05).
-define(TYPE_PROCEDURE_ADVERTISEMENT,      16#06).
-define(TYPE_TOMBSTONE,                    16#0C).
-define(TYPE_FOUNDATION_SEED_LIST,         16#0D).
-define(TYPE_FOUNDATION_PARAMETER,         16#0E).
-define(TYPE_FOUNDATION_REALM_TRUST_LIST,  16#0F).
-define(TYPE_FOUNDATION_T3_ATTESTATION,    16#10).
-define(TYPE_CONTENT_ANNOUNCEMENT,         16#11).
-define(DOMAIN_TYPE_MIN,                   16#20).

%% Domain separation for derived storage keys (Part 3 §3.3).
-define(STORAGE_DOMAIN_STATION_SET,    <<"station_set">>).
-define(STORAGE_DOMAIN_MEMBER_ENDORSE, <<"member_endorsement">>).
-define(STORAGE_DOMAIN_FOUND_SEED,     <<"foundation_seed_list">>).
-define(STORAGE_DOMAIN_FOUND_PARAM,    <<"foundation_parameter">>).
-define(STORAGE_DOMAIN_FOUND_TRUST,    <<"foundation_realm_trust_list">>).
-define(STORAGE_DOMAIN_FOUND_ATTEST,   <<"foundation_t3_attestation">>).

%% Default endorsement validity window (30 days).
-define(DEFAULT_ENDORSEMENT_TTL_MS, 30 * 24 * 60 * 60 * 1000).

%% Default record TTL (Part 4 §11): expire 48h after creation.
-define(DEFAULT_TTL_MS, 48 * 60 * 60 * 1000).

-type type_tag() :: 1..16#FF.
-type version()  :: <<_:128>>.

-type record() :: #{
    type       := type_tag(),
    key        := <<_:256>>,
    version    := version(),
    created_at := pos_integer(),
    expires_at := pos_integer(),
    payload    := map(),
    signature  => <<_:512>>
}.

-type node_record_opts() :: #{
    station_id   => macula_identity:pubkey(),
    caps_hint    => binary(),
    display_name => binary(),
    ttl_ms       => pos_integer(),
    %% Optional self-described location + reach metadata.
    %% Subscribers (e.g. realm dashboards) read these from the
    %% record payload to render maps without polling a side-channel.
    %% All four geo fields travel together — supplying any without
    %% the others is allowed but generally reduces utility.
    hostname     => binary(),
    endpoint     => binary(),
    city         => binary(),
    country      => binary(),
    lat          => float() | integer(),
    lng          => float() | integer(),
    %% Actor discriminator — `<<"station">>' for relay identities,
    %% `<<"daemon">>' for client identities. Subscribers route on
    %% this to render presence events on different mesh channels.
    kind         => binary()
}.

-type realm_directory_opts() :: #{
    policy_url => binary(),
    ttl_ms     => pos_integer()
}.

-type realm_station_entry() :: #{
    station_id := macula_identity:pubkey(),
    roles      := [binary()]
}.

-type realm_stations_opts() :: #{ttl_ms => pos_integer()}.

-type realm_member_endorsement_opts() :: #{
    valid_from   => pos_integer(),
    valid_until  => pos_integer(),
    ttl_ms       => pos_integer()
}.

-type procedure_advertisement_opts() :: #{
    session_token_hint => binary(),
    rate_limit_qps     => non_neg_integer(),
    max_concurrency    => non_neg_integer(),
    ttl_ms             => pos_integer()
}.

-type content_announcement_opts() :: #{
    name        => binary(),
    size        => non_neg_integer(),
    chunk_count => non_neg_integer(),
    ttl_ms      => pos_integer()
}.

-type foundation_seed() :: #{
    node_id   := macula_identity:pubkey(),
    addresses := [map()],
    tier      := 3 | 4
}.

-type foundation_seed_list_opts() :: #{
    valid_from  => pos_integer(),
    valid_until => pos_integer(),
    ttl_ms      => pos_integer()
}.

-type foundation_parameter_value() ::
        integer() | binary() | [integer() | binary()] | boolean().

-type foundation_parameter_opts() :: #{
    valid_from    => pos_integer(),
    valid_until   => pos_integer(),
    prior_version => <<_:128>> | undefined,
    ttl_ms        => pos_integer()
}.

-type foundation_realm_trust_list_opts() :: #{
    realms_revoked => [macula_identity:pubkey()],
    valid_until    => pos_integer(),
    ttl_ms         => pos_integer()
}.

-type foundation_t3_attestation_opts() :: #{
    valid_until => pos_integer(),
    notes       => binary(),
    ttl_ms      => pos_integer()
}.

-type tombstone_opts() :: #{
    detail => binary(),
    ttl_ms => pos_integer()
}.

%%------------------------------------------------------------------
%% Constructors — node_record (Part 6 §9.2)
%%------------------------------------------------------------------

-spec node_record(macula_identity:pubkey(),
                  [macula_identity:pubkey()],
                  non_neg_integer()) -> record().
node_record(NodeId, Realms, Capabilities) ->
    node_record(NodeId, Realms, Capabilities, #{}).

-spec node_record(macula_identity:pubkey(),
                  [macula_identity:pubkey()],
                  non_neg_integer(),
                  node_record_opts()) -> record().
node_record(NodeId, Realms, Capabilities, Opts)
  when is_binary(NodeId), byte_size(NodeId) =:= 32,
       is_list(Realms),
       is_integer(Capabilities), Capabilities >= 0 ->
    StationId = maps:get(station_id, Opts, NodeId),
    Payload = node_payload(NodeId, StationId, Realms, Capabilities, Opts),
    envelope(?TYPE_NODE_RECORD, NodeId, Payload, Opts).

%%------------------------------------------------------------------
%% Constructors — realm_directory (Part 6 §9.4)
%%
%% A realm's "meta" record — name, admin key, optional policy URL.
%% Owning key is the RealmId; storage key is also the RealmId.
%%------------------------------------------------------------------

-spec realm_directory(macula_identity:pubkey(), binary(),
                      macula_identity:pubkey()) -> record().
realm_directory(RealmId, Name, AdminKey) ->
    realm_directory(RealmId, Name, AdminKey, #{}).

-spec realm_directory(macula_identity:pubkey(), binary(),
                      macula_identity:pubkey(),
                      realm_directory_opts()) -> record().
realm_directory(RealmId, Name, AdminKey, Opts)
  when is_binary(RealmId),  byte_size(RealmId)  =:= 32,
       is_binary(Name),
       is_binary(AdminKey), byte_size(AdminKey) =:= 32 ->
    Payload = realm_directory_payload(RealmId, Name, AdminKey, Opts),
    envelope(?TYPE_REALM_DIRECTORY, RealmId, Payload, Opts).

%%------------------------------------------------------------------
%% Constructors — realm_stations (Part 6 §9.5)
%%
%% Stored at storage key SHA-256("station_set" || RealmId) so a
%% fresh station can look up all stations serving a realm without
%% knowing the realm admin's NodeId. Envelope key remains the
%% RealmId (admin signs the record).
%%------------------------------------------------------------------

-spec realm_stations(macula_identity:pubkey(),
                     [realm_station_entry()]) -> record().
realm_stations(RealmId, Entries) ->
    realm_stations(RealmId, Entries, #{}).

-spec realm_stations(macula_identity:pubkey(),
                     [realm_station_entry()],
                     realm_stations_opts()) -> record().
realm_stations(RealmId, Entries, Opts)
  when is_binary(RealmId), byte_size(RealmId) =:= 32,
       is_list(Entries) ->
    Payload = realm_stations_payload(RealmId, Entries),
    envelope(?TYPE_REALM_STATIONS, RealmId, Payload, Opts).

%%------------------------------------------------------------------
%% Constructors — realm_member_endorsement (Part 6 §9.6)
%%
%% Admin-signed statement that `MemberNode' is authorised to act as
%% a member of the realm. Stored at a derived storage key so a new
%% station joining the realm can look it up by `{realm, node}' pair
%% without knowing the record version. Envelope key is the RealmId
%% (admin signs).
%%------------------------------------------------------------------

-spec realm_member_endorsement(macula_identity:pubkey(),
                               #{realm      := macula_identity:pubkey(),
                                 member_node := macula_identity:pubkey(),
                                 roles       := [binary()]}) -> record().
realm_member_endorsement(RealmId, Spec) ->
    realm_member_endorsement(RealmId, Spec, #{}).

-spec realm_member_endorsement(macula_identity:pubkey(),
                               #{realm       := macula_identity:pubkey(),
                                 member_node := macula_identity:pubkey(),
                                 roles       := [binary()]},
                               realm_member_endorsement_opts()) -> record().
realm_member_endorsement(RealmId,
                         #{realm := RealmId, member_node := Member,
                           roles := Roles} = _Spec, Opts)
  when is_binary(RealmId), byte_size(RealmId) =:= 32,
       is_binary(Member),  byte_size(Member)  =:= 32,
       is_list(Roles) ->
    NowMs = erlang:system_time(millisecond),
    ValidFrom  = maps:get(valid_from,  Opts, NowMs),
    ValidUntil = maps:get(valid_until, Opts,
                          NowMs + ?DEFAULT_ENDORSEMENT_TTL_MS),
    Payload = realm_member_endorsement_payload(RealmId, Member, Roles,
                                               ValidFrom, ValidUntil),
    envelope(?TYPE_REALM_MEMBER_ENDORSEMENT, RealmId, Payload, Opts).

%%------------------------------------------------------------------
%% Constructors — procedure_advertisement (Part 6 §9.7)
%%
%% Stored at storage key SHA-256(procedure_uri). Envelope key is
%% the advertiser's NodeId (advertiser signs the record).
%%------------------------------------------------------------------

-spec procedure_advertisement(macula_identity:pubkey(), binary(),
                              macula_identity:pubkey()) -> record().
procedure_advertisement(AdvertiserNode, ProcedureUri, ServingStation) ->
    procedure_advertisement(AdvertiserNode, ProcedureUri, ServingStation, #{}).

-spec procedure_advertisement(macula_identity:pubkey(), binary(),
                              macula_identity:pubkey(),
                              procedure_advertisement_opts()) -> record().
procedure_advertisement(AdvertiserNode, ProcedureUri, ServingStation, Opts)
  when is_binary(AdvertiserNode), byte_size(AdvertiserNode) =:= 32,
       is_binary(ProcedureUri),
       is_binary(ServingStation), byte_size(ServingStation) =:= 32 ->
    Payload = procedure_advertisement_payload(AdvertiserNode, ProcedureUri,
                                              ServingStation, Opts),
    envelope(?TYPE_PROCEDURE_ADVERTISEMENT, AdvertiserNode, Payload, Opts).

%%------------------------------------------------------------------
%% Constructors — content_announcement (Part 6 §9.x)
%%
%% Signed announcement that `AnnouncerNode' is hosting the content
%% identified by `MCID' (34-byte Macula Content IDentifier) and
%% reachable at `Endpoint'. Optional metadata fields carry the
%% manifest's display name, byte size, and chunk count so locators
%% can prioritise without fetching the manifest first.
%%------------------------------------------------------------------

-spec content_announcement(macula_identity:pubkey(), binary(), binary()) -> record().
content_announcement(AnnouncerNode, MCID, Endpoint) ->
    content_announcement(AnnouncerNode, MCID, Endpoint, #{}).

-spec content_announcement(macula_identity:pubkey(), binary(), binary(),
                            content_announcement_opts()) -> record().
content_announcement(AnnouncerNode, MCID, Endpoint, Opts)
  when is_binary(AnnouncerNode), byte_size(AnnouncerNode) =:= 32,
       is_binary(MCID), byte_size(MCID) =:= 34,
       is_binary(Endpoint) ->
    Payload = content_announcement_payload(AnnouncerNode, MCID, Endpoint, Opts),
    envelope(?TYPE_CONTENT_ANNOUNCEMENT, AnnouncerNode, Payload, Opts).

%%------------------------------------------------------------------
%% Constructors — foundation_seed_list (Part 6 §9.14)
%%
%% FROST-Ed25519 threshold-signed. `FoundationKey' is the aggregated
%% pubkey (32 bytes) — same as any Ed25519 key on the wire. Embedded
%% in station firmware via `macula_foundation'.
%%------------------------------------------------------------------

-spec foundation_seed_list(macula_identity:pubkey(),
                           [foundation_seed()]) -> record().
foundation_seed_list(FoundationKey, Seeds) ->
    foundation_seed_list(FoundationKey, Seeds, #{}).

-spec foundation_seed_list(macula_identity:pubkey(),
                           [foundation_seed()],
                           foundation_seed_list_opts()) -> record().
foundation_seed_list(FoundationKey, Seeds, Opts)
  when is_binary(FoundationKey), byte_size(FoundationKey) =:= 32,
       is_list(Seeds) ->
    NowMs = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    ValidFrom  = maps:get(valid_from,  Opts, NowMs),
    ValidUntil = maps:get(valid_until, Opts, NowMs + TtlMs),
    Envelope = envelope(?TYPE_FOUNDATION_SEED_LIST, FoundationKey, #{}, Opts),
    Payload = foundation_seed_list_payload(
                maps:get(version, Envelope),
                ValidFrom, ValidUntil, Seeds),
    Envelope#{payload => Payload}.

%%------------------------------------------------------------------
%% Constructors — foundation_parameter (Part 6 §9.15)
%%------------------------------------------------------------------

-spec foundation_parameter(macula_identity:pubkey(), binary(),
                           foundation_parameter_value()) -> record().
foundation_parameter(FoundationKey, Name, Value) ->
    foundation_parameter(FoundationKey, Name, Value, #{}).

-spec foundation_parameter(macula_identity:pubkey(), binary(),
                           foundation_parameter_value(),
                           foundation_parameter_opts()) -> record().
foundation_parameter(FoundationKey, Name, Value, Opts)
  when is_binary(FoundationKey), byte_size(FoundationKey) =:= 32,
       is_binary(Name) ->
    NowMs = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    ValidFrom  = maps:get(valid_from,  Opts, NowMs),
    ValidUntil = maps:get(valid_until, Opts, NowMs + TtlMs),
    PriorV     = maps:get(prior_version, Opts, undefined),
    Envelope = envelope(?TYPE_FOUNDATION_PARAMETER, FoundationKey, #{}, Opts),
    Payload = foundation_parameter_payload(
                Name, Value, maps:get(version, Envelope),
                ValidFrom, ValidUntil, PriorV),
    Envelope#{payload => Payload}.

%%------------------------------------------------------------------
%% Constructors — foundation_realm_trust_list (Part 6 §9.16)
%%------------------------------------------------------------------

-spec foundation_realm_trust_list(macula_identity:pubkey(),
                                  [macula_identity:pubkey()]) -> record().
foundation_realm_trust_list(FoundationKey, Trusted) ->
    foundation_realm_trust_list(FoundationKey, Trusted, #{}).

-spec foundation_realm_trust_list(macula_identity:pubkey(),
                                  [macula_identity:pubkey()],
                                  foundation_realm_trust_list_opts()) ->
          record().
foundation_realm_trust_list(FoundationKey, Trusted, Opts)
  when is_binary(FoundationKey), byte_size(FoundationKey) =:= 32,
       is_list(Trusted) ->
    NowMs = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    ValidUntil = maps:get(valid_until, Opts, NowMs + TtlMs),
    Revoked    = maps:get(realms_revoked, Opts, []),
    Envelope = envelope(?TYPE_FOUNDATION_REALM_TRUST_LIST, FoundationKey,
                        #{}, Opts),
    Payload = foundation_realm_trust_list_payload(
                Trusted, Revoked, maps:get(version, Envelope), ValidUntil),
    Envelope#{payload => Payload}.

%%------------------------------------------------------------------
%% Constructors — foundation_t3_attestation (Part 6 §9.17)
%%------------------------------------------------------------------

-spec foundation_t3_attestation(macula_identity:pubkey(),
                                macula_identity:pubkey(),
                                pos_integer()) -> record().
foundation_t3_attestation(FoundationKey, StationId, AuditDate) ->
    foundation_t3_attestation(FoundationKey, StationId, AuditDate, #{}).

-spec foundation_t3_attestation(macula_identity:pubkey(),
                                macula_identity:pubkey(),
                                pos_integer(),
                                foundation_t3_attestation_opts()) ->
          record().
foundation_t3_attestation(FoundationKey, StationId, AuditDate, Opts)
  when is_binary(FoundationKey), byte_size(FoundationKey) =:= 32,
       is_binary(StationId),     byte_size(StationId)     =:= 32,
       is_integer(AuditDate),    AuditDate > 0 ->
    NowMs = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    ValidUntil = maps:get(valid_until, Opts, NowMs + TtlMs),
    Notes      = maps:get(notes, Opts, undefined),
    Envelope = envelope(?TYPE_FOUNDATION_T3_ATTESTATION, FoundationKey,
                        #{}, Opts),
    Payload = foundation_t3_attestation_payload(
                StationId, AuditDate, ValidUntil, Notes),
    Envelope#{payload => Payload}.

%%------------------------------------------------------------------
%% Constructors — tombstone (Part 6 §9.13)
%%------------------------------------------------------------------

-spec tombstone(macula_identity:pubkey(), type_tag(), atom()) -> record().
tombstone(SupersededKey, SupersededType, Reason) ->
    tombstone(SupersededKey, SupersededType, Reason, #{}).

-spec tombstone(macula_identity:pubkey(), type_tag(), atom(), tombstone_opts()) ->
    record().
tombstone(SupersededKey, SupersededType, Reason, Opts)
  when is_binary(SupersededKey), byte_size(SupersededKey) =:= 32,
       is_integer(SupersededType), SupersededType > 0,
       is_atom(Reason) ->
    NowMs = erlang:system_time(millisecond),
    Detail = maps:get(detail, Opts, undefined),
    Payload = tombstone_payload(SupersededKey, SupersededType, NowMs, Reason, Detail),
    envelope(?TYPE_TOMBSTONE, SupersededKey, Payload, Opts).

%%------------------------------------------------------------------
%% Sign / verify
%%------------------------------------------------------------------

-spec sign(record(), macula_identity:key_pair() | macula_identity:privkey()) ->
    record().
sign(Record, Identity) ->
    Bytes = canonical_unsigned(Record),
    Sig = macula_identity:sign([?SIG_DOMAIN, Bytes], Identity),
    Record#{signature => Sig}.

-spec verify(record()) -> {ok, record()} | {error, term()}.
verify(#{signature := Sig, key := Pub} = Record)
  when is_binary(Sig), byte_size(Sig) =:= 64,
       is_binary(Pub), byte_size(Pub) =:= 32 ->
    Bytes = canonical_unsigned(Record),
    verify_signature(macula_identity:verify([?SIG_DOMAIN, Bytes], Sig, Pub),
                     Record);
verify(_) ->
    {error, bad_record}.

verify_signature(true, Record) ->
    expiry_check(Record);
verify_signature(false, _Record) ->
    {error, signature_invalid}.

%% @doc Rebuild a record with a fresh UUIDv7 version and a new
%% `created_at' / `expires_at' pair (preserving the original TTL),
%% then re-sign with `Identity'. Used by the owner's tRepublish
%% loop (Part 3 §11) to keep a record alive across churn without
%% changing its type, key, or payload.
%%
%% The new signature replaces any prior signature; callers can
%% safely pass an already-signed record — the prior signature is
%% stripped before re-signing.
-spec refresh(record(),
              macula_identity:key_pair() | macula_identity:privkey()) ->
          record().
refresh(Record, Identity) ->
    NowMs = erlang:system_time(millisecond),
    TtlMs = maps:get(expires_at, Record) - maps:get(created_at, Record),
    Fresh = #{
        type       => maps:get(type, Record),
        key        => maps:get(key, Record),
        version    => macula_record_uuid:v7(NowMs),
        created_at => NowMs,
        expires_at => NowMs + TtlMs,
        payload    => maps:get(payload, Record)
    },
    sign(Fresh, Identity).

expiry_check(#{expires_at := X} = Record) ->
    case erlang:system_time(millisecond) >= X of
        true  -> {error, expired};
        false -> {ok, Record}
    end.

%%------------------------------------------------------------------
%% Wire codec
%%------------------------------------------------------------------

-spec encode(record()) -> binary().
encode(#{signature := Sig} = Record) when is_binary(Sig), byte_size(Sig) =:= 64 ->
    macula_record_cbor:encode(to_envelope_map(Record)).

-spec decode(binary()) -> {ok, record()} | {error, term()}.
decode(Bin) when is_binary(Bin) ->
    decode_value(macula_record_cbor:decode(Bin)).

decode_value(Map) when is_map(Map) ->
    G = fun(Key) -> maps:get({text, Key}, Map, undefined) end,
    parse_envelope(G(<<"t">>), G(<<"k">>), G(<<"v">>), G(<<"c">>),
                   G(<<"x">>), G(<<"p">>), G(<<"u">>), G(<<"s">>));
decode_value(_Other) ->
    {error, bad_record}.

parse_envelope(T, K, V, C, X, P, U, S)
  when is_integer(T), T > 0,
       is_binary(K), byte_size(K) =:= 32,
       is_binary(V), byte_size(V) =:= 16,
       is_integer(C), C > 0,
       is_integer(X), X > 0,
       is_map(P),
       is_binary(S), byte_size(S) =:= 64 ->
    Base = #{type => T, key => K, version => V,
             created_at => C, expires_at => X,
             payload => P, signature => S},
    {ok, with_decoded_subject(Base, U)};
parse_envelope(_, _, _, _, _, _, _, undefined) ->
    {error, missing_signature};
parse_envelope(_, _, _, _, _, _, _, _) ->
    {error, bad_record}.

with_decoded_subject(R, undefined)               -> R;
with_decoded_subject(R, Sid) when is_binary(Sid) -> R#{subject_id => Sid};
with_decoded_subject(R, _Other)                  -> R.

%%------------------------------------------------------------------
%% Accessors
%%------------------------------------------------------------------

type(#{type := T}) -> T.
key(#{key := K}) -> K.
version(#{version := V}) -> V.
created_at(#{created_at := C}) -> C.
expires_at(#{expires_at := X}) -> X.
payload(#{payload := P}) -> P.
signature(#{signature := S}) -> S.

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

%% @doc Generic record builder. Used internally by every typed
%% constructor and exposed publicly for domain-defined record types
%% in the 0x20-0xFF tag range. Returns an UNSIGNED record map; pair
%% with `sign/2'.
%%
%% The `key' field is always the signer's 32-byte Ed25519 public key
%% (`verify/1' looks it up there). For facts where one signer
%% publishes about many subjects (e.g., a realm admin signing many
%% license records), pass a `subject_id' opt — `storage_key/1'
%% derives a per-subject 32-byte slot via
%% `BLAKE3(<<type, key, subject_id>>)'. Without `subject_id' the
%% storage key is `key' verbatim (one DHT slot per signer).
%%
%% Domain code names its own payload fields. Single-letter wire keys
%% (Part 6 §9) are an envelope-level concern; payloads use whatever
%% naming makes sense in the domain.
-spec envelope(type_tag(), <<_:256>>, map(), map()) -> record().
envelope(Type, Key, Payload, Opts)
  when is_integer(Type), Type > 0, Type =< 16#FF,
       is_binary(Key), byte_size(Key) =:= 32,
       is_map(Payload), is_map(Opts) ->
    NowMs = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    Base = #{
        type       => Type,
        key        => Key,
        version    => macula_record_uuid:v7(NowMs),
        created_at => NowMs,
        expires_at => NowMs + TtlMs,
        payload    => Payload
    },
    with_subject(Base, maps:get(subject_id, Opts, undefined)).

with_subject(Map, undefined)                       -> Map;
with_subject(Map, Sid) when is_binary(Sid)         -> Map#{subject_id => Sid}.

node_payload(NodeId, StationId, Realms, Caps, Opts) ->
    Base = #{
        {text, <<"node_id">>}      => NodeId,
        {text, <<"station_id">>}   => StationId,
        {text, <<"realms">>}       => Realms,
        {text, <<"capabilities">>} => Caps
    },
    M1 = with_text(Base,  <<"caps_hint">>,    maps:get(caps_hint,    Opts, undefined)),
    M2 = with_text(M1,    <<"display_name">>, maps:get(display_name, Opts, undefined)),
    M3 = with_text(M2,    <<"hostname">>,     maps:get(hostname,     Opts, undefined)),
    M4 = with_text(M3,    <<"endpoint">>,     maps:get(endpoint,     Opts, undefined)),
    M5 = with_text(M4,    <<"city">>,         maps:get(city,         Opts, undefined)),
    M6 = with_text(M5,    <<"country">>,      maps:get(country,      Opts, undefined)),
    M7 = with_geo(M6,     <<"lat">>,          maps:get(lat,          Opts, undefined)),
    M8 = with_geo(M7,     <<"lng">>,          maps:get(lng,          Opts, undefined)),
    %% `kind' is the actor discriminator — `station' for relay
    %% identities, `daemon' for client identities. Subscribers route
    %% on this to render presence events on different mesh channels.
    %% Records without `kind' predate the field and are treated as
    %% `station' by consumers.
    with_text(M8,         <<"kind">>,         maps:get(kind,         Opts, undefined)).

with_text(Map, _Key, undefined) -> Map;
with_text(Map,  Key, Bin) when is_binary(Bin) ->
    Map#{ {text, Key} => {text, Bin} }.

%% Geo coordinates travel as CBOR text strings — float canonicalisation
%% is fragile across language implementations, while a fixed-decimals
%% text rendering is stable. Subscribers parse with `binary_to_float/1'
%% (or `binary_to_integer/1' if the source was an integer like 0).
with_geo(Map, _Key, undefined) -> Map;
with_geo(Map,  Key, V) when is_float(V) ->
    Map#{ {text, Key} => {text, float_to_binary(V, [{decimals, 6}, compact])} };
with_geo(Map,  Key, V) when is_integer(V) ->
    Map#{ {text, Key} => {text, integer_to_binary(V)} }.

tombstone_payload(SupKey, SupType, ReplacedAt, Reason, Detail) ->
    Base = #{
        {text, <<"superseded_key">>}  => SupKey,
        {text, <<"superseded_type">>} => SupType,
        {text, <<"replaced_at">>}     => ReplacedAt,
        {text, <<"reason">>}          => {text, atom_to_binary(Reason, utf8)}
    },
    Base#{ {text, <<"detail">>} => detail_value(Detail) }.

detail_value(undefined) -> null;
detail_value(Bin) when is_binary(Bin) -> {text, Bin}.

realm_directory_payload(RealmId, Name, AdminKey, Opts) ->
    Base = #{
        {text, <<"realm_id">>}   => RealmId,
        {text, <<"name">>}       => {text, Name},
        {text, <<"admin_key">>}  => AdminKey,
        {text, <<"created_at">>} => erlang:system_time(millisecond)
    },
    with_text(Base, <<"policy_url">>, maps:get(policy_url, Opts, undefined)).

realm_stations_payload(RealmId, Entries) ->
    #{
        {text, <<"realm_id">>} => RealmId,
        {text, <<"stations">>} => [realm_station_entry(E) || E <- Entries]
    }.

realm_station_entry(#{station_id := SId, roles := Roles})
  when is_binary(SId), byte_size(SId) =:= 32, is_list(Roles) ->
    #{
        {text, <<"station_id">>} => SId,
        {text, <<"roles">>}      => [{text, R} || R <- Roles,
                                                  is_binary(R)]
    }.

realm_member_endorsement_payload(RealmId, Member, Roles,
                                 ValidFrom, ValidUntil) ->
    #{
        {text, <<"realm">>}       => RealmId,
        {text, <<"member_node">>} => Member,
        {text, <<"roles">>}       => [{text, R} || R <- Roles,
                                                    is_binary(R)],
        {text, <<"valid_from">>}  => ValidFrom,
        {text, <<"valid_until">>} => ValidUntil
    }.

procedure_advertisement_payload(AdvertiserNode, ProcedureUri,
                                ServingStation, Opts) ->
    Base = #{
        {text, <<"procedure_uri">>}   => {text, ProcedureUri},
        {text, <<"advertiser_node">>} => AdvertiserNode,
        {text, <<"serving_station">>} => ServingStation
    },
    M1 = with_text(Base, <<"session_token_hint">>,
                   maps:get(session_token_hint, Opts, undefined)),
    M2 = with_uint(M1, <<"rate_limit_qps">>,
                   maps:get(rate_limit_qps, Opts, undefined)),
    with_uint(M2, <<"max_concurrency">>,
              maps:get(max_concurrency, Opts, undefined)).

with_uint(Map, _Key, undefined) -> Map;
with_uint(Map,  Key, N) when is_integer(N), N >= 0 ->
    Map#{ {text, Key} => N }.

content_announcement_payload(AnnouncerNode, MCID, Endpoint, Opts) ->
    Base = #{
        {text, <<"announcer_node">>} => AnnouncerNode,
        {text, <<"mcid">>}           => MCID,
        {text, <<"endpoint">>}       => {text, Endpoint}
    },
    M1 = with_text(Base, <<"name">>,        maps:get(name, Opts, undefined)),
    M2 = with_uint(M1,   <<"size">>,        maps:get(size, Opts, undefined)),
    with_uint(M2,        <<"chunk_count">>, maps:get(chunk_count, Opts, undefined)).

foundation_seed_list_payload(Version, ValidFrom, ValidUntil, Seeds) ->
    #{
        {text, <<"version">>}     => Version,
        {text, <<"valid_from">>}  => ValidFrom,
        {text, <<"valid_until">>} => ValidUntil,
        {text, <<"seeds">>}       => [foundation_seed_entry(S) || S <- Seeds]
    }.

foundation_seed_entry(#{node_id := NodeId, addresses := Addrs, tier := Tier})
  when is_binary(NodeId), byte_size(NodeId) =:= 32,
       is_list(Addrs), (Tier =:= 3 orelse Tier =:= 4) ->
    #{
        {text, <<"node_id">>}   => NodeId,
        {text, <<"addresses">>} => [foundation_address(A) || A <- Addrs],
        {text, <<"tier">>}      => Tier
    }.

foundation_address(#{} = Addr) ->
    %% ip_address_rec() per Part 6 §9.18 — tolerated as an opaque map
    %% of text-keyed fields here; caller is responsible for building it.
    Addr.

foundation_parameter_payload(Name, Value, Version,
                             ValidFrom, ValidUntil, PriorV) ->
    Base = #{
        {text, <<"param_name">>}  => {text, Name},
        {text, <<"param_value">>} => parameter_value(Value),
        {text, <<"version">>}     => Version,
        {text, <<"valid_from">>}  => ValidFrom,
        {text, <<"valid_until">>} => ValidUntil
    },
    Base#{ {text, <<"prior_version">>} => prior_version_value(PriorV) }.

parameter_value(V) when is_integer(V); is_boolean(V) -> V;
parameter_value(V) when is_binary(V) -> {text, V};
parameter_value(V) when is_list(V)   -> [parameter_value(X) || X <- V].

prior_version_value(undefined) -> null;
prior_version_value(V) when is_binary(V), byte_size(V) =:= 16 -> V.

foundation_realm_trust_list_payload(Trusted, Revoked, Version, ValidUntil) ->
    #{
        {text, <<"realms_trusted">>} =>
            [R || R <- Trusted, is_binary(R), byte_size(R) =:= 32],
        {text, <<"realms_revoked">>} =>
            [R || R <- Revoked, is_binary(R), byte_size(R) =:= 32],
        {text, <<"version">>}     => Version,
        {text, <<"valid_until">>} => ValidUntil
    }.

foundation_t3_attestation_payload(StationId, AuditDate, ValidUntil, Notes) ->
    Base = #{
        {text, <<"station_id">>}    => StationId,
        {text, <<"tier_attested">>} => 3,
        {text, <<"audit_date">>}    => AuditDate,
        {text, <<"valid_until">>}   => ValidUntil
    },
    with_text(Base, <<"notes">>, Notes).

%%------------------------------------------------------------------
%% DHT storage-key derivation (Part 3 §3.3)
%%------------------------------------------------------------------

-spec storage_key(record()) -> <<_:256>>.
storage_key(#{type := Type, key := K})
  when Type =:= ?TYPE_NODE_RECORD;
       Type =:= ?TYPE_REALM_DIRECTORY;
       Type =:= ?TYPE_TOMBSTONE ->
    K;
storage_key(#{type := ?TYPE_REALM_STATIONS, key := RealmId}) ->
    crypto:hash(sha256, <<?STORAGE_DOMAIN_STATION_SET/binary, RealmId/binary>>);
storage_key(#{type := ?TYPE_REALM_MEMBER_ENDORSEMENT,
              key := RealmId, payload := P}) ->
    Member = maps:get({text, <<"member_node">>}, P),
    crypto:hash(sha256, <<?STORAGE_DOMAIN_MEMBER_ENDORSE/binary,
                          RealmId/binary, Member/binary>>);
storage_key(#{type := ?TYPE_PROCEDURE_ADVERTISEMENT, payload := P}) ->
    {text, Uri} = maps:get({text, <<"procedure_uri">>}, P),
    crypto:hash(sha256, Uri);
storage_key(#{type := ?TYPE_FOUNDATION_SEED_LIST, key := Fk}) ->
    crypto:hash(sha256, <<?STORAGE_DOMAIN_FOUND_SEED/binary, Fk/binary>>);
storage_key(#{type := ?TYPE_FOUNDATION_PARAMETER, key := Fk, payload := P}) ->
    {text, Name} = maps:get({text, <<"param_name">>}, P),
    crypto:hash(sha256, <<?STORAGE_DOMAIN_FOUND_PARAM/binary,
                          Fk/binary, Name/binary>>);
storage_key(#{type := ?TYPE_FOUNDATION_REALM_TRUST_LIST, key := Fk}) ->
    crypto:hash(sha256, <<?STORAGE_DOMAIN_FOUND_TRUST/binary, Fk/binary>>);
storage_key(#{type := ?TYPE_FOUNDATION_T3_ATTESTATION, payload := P}) ->
    Sid = maps:get({text, <<"station_id">>}, P),
    crypto:hash(sha256, <<?STORAGE_DOMAIN_FOUND_ATTEST/binary, Sid/binary>>);
%% Domain-defined types (0x20-0xFF). When the envelope carries a
%% `subject_id' the storage key is derived from <<type, signer_key,
%% subject_id>> so one signer can publish many records under
%% distinct DHT slots. Without `subject_id' the storage key is the
%% signer's pubkey (one slot per signer).
storage_key(#{type := T, key := K, subject_id := Sid})
  when is_integer(T), T >= ?DOMAIN_TYPE_MIN, T =< 16#FF,
       is_binary(K), byte_size(K) =:= 32,
       is_binary(Sid) ->
    crypto:hash(sha256, <<T:8, K/binary, Sid/binary>>);
storage_key(#{type := T, key := K})
  when is_integer(T), T >= ?DOMAIN_TYPE_MIN, T =< 16#FF,
       is_binary(K), byte_size(K) =:= 32 ->
    K.

%% Build the envelope CBOR map. Includes signature when present.
to_envelope_map(#{type := T, key := K, version := V,
                  created_at := C, expires_at := X,
                  payload := P} = R) ->
    Base = #{
        {text, <<"t">>} => T,
        {text, <<"k">>} => K,
        {text, <<"v">>} => V,
        {text, <<"c">>} => C,
        {text, <<"x">>} => X,
        {text, <<"p">>} => P
    },
    M1 = add_subject(Base, maps:get(subject_id, R, undefined)),
    add_signature(M1, maps:get(signature, R, undefined)).

add_subject(M, undefined) -> M;
add_subject(M, Sid) when is_binary(Sid) ->
    M#{ {text, <<"u">>} => Sid }.    %% u = subject_id (Part 6 §9 extension)

add_signature(M, undefined) -> M;
add_signature(M, Sig) when is_binary(Sig) ->
    M#{ {text, <<"s">>} => Sig }.

%% Canonical CBOR of the record minus its signature — what gets signed/verified.
canonical_unsigned(Record) ->
    macula_record_cbor:encode(to_envelope_map(maps:without([signature], Record))).
