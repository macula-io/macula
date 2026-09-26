%% @doc Records in the signed-object format of DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md.
%%
%% A record is the signed object {key, tbs, signature} under the label MACULA-PQ-RECORD-V1. Its tbs holds type, alg,
%% version, created_at, expires_at and payload, and subject only on a domain type (tags 0x20 to 0xFF). A constructor
%% returns an unsigned record. sign/2 takes the signer's key, refuses a key whose purpose does not fit the type, and
%% adds key, key_id, alg, tbs and signature. verify/2,3 reads a record in the design's order and keeps its tbs bytes,
%% so encode/1 sends them unchanged.
%%
%% A record is named by the key id of its key: the node_id for node records, procedure advertisements, content
%% announcements and station endpoints, and the MACULA-KEY-ID-V1 key id for realm, org and foundation records and for
%% every domain type. A tombstone is named as the type it withdraws. A node record is stored under its node_id, and
%% every other record under SHA-256 over MACULA-PQ-STORAGE-KEY-V1, a zero byte, the type and the type's fields.
%%
%% A procedure advertisement carries its provider authorization inside its payload. verify/2,3 treats it as opaque,
%% so a station that stores records never parses it; verify_authorization/3 is the caller's check.
-module(macula_record).

-export([
    node_record/3, node_record/4,
    realm_directory/3, realm_directory/4,
    realm_stations/2, realm_stations/3,
    realm_member_endorsement/2, realm_member_endorsement/3, max_endorsement_window_ms/0,
    org_directory/3, org_directory/4,
    procedure_delegation/2, procedure_delegation/3,
    procedure_advertisement/4, procedure_advertisement/5,
    content_announcement/3,
    foundation_seed_list/1, foundation_seed_list/2,
    foundation_parameter/2, foundation_parameter/3,
    foundation_realm_trust_list/1, foundation_realm_trust_list/2,
    foundation_t3_attestation/2, foundation_t3_attestation/3,
    station_endpoint/1, station_endpoint/2,
    tombstone/2, tombstone/3,
    envelope/3
]).
-export([sign/2, verify/2, verify/3, signer_entry/3, signer_entry/4, refresh/2, refresh/3, encode/1, node_signed/1,
         payload_bounded/1, wire_bounded/1,
         domain_type/1, domain_record_checked/1]).
-export([type/1, key/1, key_id/1, version/1, created_at/1, expires_at/1, payload/1, signature/1]).
-export([payload_field/2, type_procedure_advertisement/0, procedure_advertisement_max_lifetime_ms/0,
         own_namespace/1]).
-export([read_node_record/1, read_procedure_advertisement/1, read_station_endpoint/1, read_tombstone/1,
         read_org_directory/1, read_procedure_delegation/1, read_content_announcement/1,
         read_foundation_realm_trust_list/1]).
-export([procedure_org/1, verify_authorization/3]).
-export([storage_key/1, procedure_key/2, content_key/1, station_endpoint_key/1, org_directory_key/2,
         procedure_delegation_key/2, foundation_realm_trust_list_key/1]).

-export_type([m_record/0, type_tag/0, version/0, refusal/0, reason/0, authorization/0, trust/0,
              authorization_refusal/0, node_record_opts/0, realm_directory_opts/0, realm_station_entry/0,
              realm_stations_opts/0, realm_member_endorsement_opts/0, procedure_advertisement_opts/0,
              content_announcement_opts/0, foundation_seed/0, foundation_seed_list_opts/0,
              foundation_parameter_value/0, foundation_parameter_opts/0, foundation_realm_trust_list_entry/0,
              foundation_realm_trust_list_opts/0,
              foundation_t3_attestation_opts/0, tombstone_opts/0, station_endpoint_opts/0, signer_entry_stats/0]).

-type type_tag() :: 1..16#FF.
-type version() :: <<_:128>>.

%% An unsigned record holds type, version, created_at, expires_at, payload and, on a domain type, subject. A signed or
%% verified record also holds key, key_id, alg, tbs and signature.
-type m_record() :: #{
    type := type_tag(),
    version := version(),
    created_at := non_neg_integer(),
    expires_at := non_neg_integer(),
    payload := map(),
    subject => binary(),
    key => binary(),
    key_id => <<_:256>>,
    alg => binary(),
    tbs => binary(),
    signature => binary()
}.

-type refusal() :: record_too_large | malformed | signature_invalid | alg_mismatch | not_yet_valid | expired
                 | key_id_mismatch | lifetime_too_long | lifetime_reversed.
-type reason() :: shutdown | moved | revoked.
-type authorization() :: #{org_directory := binary(), procedure_delegation := binary()}.
%% The realm trust a provider authorization is checked against: the caller's crypto profile, and either the carried
%% realm key it pins for one realm, or the foundation realm trust list's pairs of realm id to realm key id, for the
%% advertisement's realm.
-type trust() :: #{profile := macula_crypto_profile:profile(),
                   realm_key => binary(),
                   realm_pairs => #{<<_:256>> => <<_:256>>}}.
-type authorization_refusal() :: malformed | no_authorization | authorization_not_allowed
                               | authorization_form_unsupported | no_realm_key | org_directory_invalid
                               | org_directory_wrong_realm | org_directory_wrong_org | delegation_invalid
                               | delegation_mismatch | authorization_outlived.

-type node_record_opts() :: #{
    station_id   => <<_:256>>,
    caps_hint    => binary(),
    display_name => binary(),
    ttl_ms       => pos_integer(),
    hostname     => binary(),
    endpoint     => binary(),
    city         => binary(),
    country      => binary(),
    lat          => float() | integer(),
    lng          => float() | integer(),
    %% `station' for relay identities, `daemon' for client identities.
    kind         => binary(),
    %% node_ids of the peer stations this node holds an overlay session with.
    peers        => [<<_:256>>]
}.
-type realm_directory_opts() :: #{policy_url => binary(), ttl_ms => pos_integer()}.
-type realm_station_entry() :: #{station_id := <<_:256>>, roles := [binary()]}.
-type realm_stations_opts() :: #{ttl_ms => pos_integer()}.
-type realm_member_endorsement_opts() :: #{valid_from => pos_integer(), valid_until => pos_integer(),
                                           ttl_ms => pos_integer()}.
-type procedure_advertisement_opts() :: #{authorization => map(), ttl_ms => pos_integer(), kem_key => binary()}.
-type content_announcement_opts() :: #{realm_id := <<_:256>>, serving_station := <<_:256>>, procedure := binary(),
                                       name => binary(), size => non_neg_integer(), chunk_count => non_neg_integer(),
                                       ttl_ms => pos_integer()}.
-type foundation_seed() :: #{node_id := <<_:256>>, addresses := [map()], tier := 3 | 4}.
-type foundation_seed_list_opts() :: #{valid_from => pos_integer(), valid_until => pos_integer(),
                                       ttl_ms => pos_integer()}.
-type foundation_parameter_value() :: integer() | binary() | [integer() | binary()].
-type foundation_parameter_opts() :: #{valid_from => pos_integer(), valid_until => pos_integer(),
                                       prior_version => version(), ttl_ms => pos_integer()}.
%% One entry of a foundation realm trust list: a realm id paired with the realm key id that signs the realm's
%% records (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Foundation realm trust list).
-type foundation_realm_trust_list_entry() :: #{realm_id := <<_:256>>, realm_key_id := <<_:256>>}.
-type foundation_realm_trust_list_opts() :: #{ttl_ms => pos_integer()}.
-type foundation_t3_attestation_opts() :: #{valid_until => pos_integer(), notes => binary(), ttl_ms => pos_integer()}.
-type tombstone_opts() :: #{detail => binary(), ttl_ms => pos_integer()}.
-type station_endpoint_opts() :: #{host_advertised => [binary()], alpn => binary(), ttl_ms => pos_integer()}.

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(STORAGE_KEY_LABEL, "MACULA-PQ-STORAGE-KEY-V1").
-define(MAX_RECORD_BYTES, 256 * 1024).
%% The longest coordinate text a node record's reader parses: a coordinate needs far fewer bytes.
-define(MAX_GEO_TEXT_BYTES, 32).
%% How far from zero a latitude and a longitude reach, both inclusive.
-define(LAT_BOUND, 90).
-define(LNG_BOUND, 180).
-define(CLOCK_TOLERANCE_MS, 5 * 60 * 1000).
%% A protocol integer in a signed structure stays below 2^53 (the decoding rule).
-define(MAX_PROTOCOL_INT, 1 bsl 53).

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
-define(TYPE_STATION_ENDPOINT,             16#12).
-define(TYPE_ORG_DIRECTORY,                16#15).
-define(TYPE_PROCEDURE_DELEGATION,         16#16).
-define(DOMAIN_TYPE_MIN,                   16#20).

%% station_endpoint record TTL (Part 4 §11): short enough to drop stale
%% stations within minutes, long enough that a one-minute refresh keeps
%% it live.
-define(STATION_ENDPOINT_TTL_MS, 5 * 60 * 1000).
%% A realm member endorsement's window, valid_from to valid_until, is at most 30 days, so one endorsement admits its
%% member for at most that long. A builder given no valid_until takes the whole window.
-define(MAX_ENDORSEMENT_WINDOW_MS, 30 * 24 * 60 * 60 * 1000).
-define(DEFAULT_TTL_MS, 48 * 60 * 60 * 1000).
%% The longest a record of a type lives, created_at to expires_at. A node record and a content announcement 48 hours; a
%% procedure advertisement 5 minutes, renewed at half that or sooner, so a provider that stops is gone within minutes; a
%% station endpoint 5 minutes; realm stations and an org directory 6 hours; a procedure delegation 30 minutes (D32); a
%% realm member endorsement 30 days; a domain record 7 days (D28); and any other type 30 days, so no record keeps a key trusted
%% without end. A tombstone lives at most its withdrawn type's maximum plus twice the clock tolerance.
-define(NODE_RECORD_MAX_LIFETIME_MS, 48 * 60 * 60 * 1000).
-define(CONTENT_ANNOUNCEMENT_MAX_LIFETIME_MS, 48 * 60 * 60 * 1000).
-define(PROCEDURE_ADVERTISEMENT_MAX_LIFETIME_MS, 5 * 60 * 1000).
-define(REALM_AND_ORG_MAX_LIFETIME_MS, 6 * 60 * 60 * 1000).
%% D32 (macula#38): a procedure delegation lives at most 30 minutes, which is
%% how soon a revoked provider is refused by every verifier: the realm stops
%% reissuing, and a longer one is refused at signing and at verifying alike, so
%% neither a realm by mistake nor a stolen org key can mint around the bound.
-define(PROCEDURE_DELEGATION_MAX_LIFETIME_MS, 30 * 60 * 1000).
-define(DOMAIN_RECORD_MAX_LIFETIME_MS, 7 * 24 * 60 * 60 * 1000).
-define(DEFAULT_MAX_LIFETIME_MS, 30 * 24 * 60 * 60 * 1000).
%% A payload sits in a record's tbs map, and the decoding rule accepts 64 levels, so a payload nests at most 63.
-define(MAX_PAYLOAD_NESTING, 63).

%%------------------------------------------------------------------
%% Constructors
%%------------------------------------------------------------------

%% @doc A node record about the node NodeId, which signs it.
-spec node_record(<<_:256>>, [<<_:256>>], non_neg_integer()) -> m_record().
node_record(NodeId, Realms, Capabilities) ->
    node_record(NodeId, Realms, Capabilities, #{}).

-spec node_record(<<_:256>>, [<<_:256>>], non_neg_integer(), node_record_opts()) -> m_record().
node_record(NodeId, Realms, Capabilities, Opts)
  when is_binary(NodeId), byte_size(NodeId) =:= 32, is_list(Realms), is_integer(Capabilities), Capabilities >= 0 ->
    StationId = maps:get(station_id, Opts, NodeId),
    unsigned(?TYPE_NODE_RECORD, node_payload(NodeId, StationId, Realms, Capabilities, Opts), Opts).

%% @doc A realm's directory record, signed by the realm key: its name and the key id of its admin key.
-spec realm_directory(<<_:256>>, binary(), <<_:256>>) -> m_record().
realm_directory(RealmId, Name, AdminKeyId) ->
    realm_directory(RealmId, Name, AdminKeyId, #{}).

-spec realm_directory(<<_:256>>, binary(), <<_:256>>, realm_directory_opts()) -> m_record().
realm_directory(RealmId, Name, AdminKeyId, Opts)
  when is_binary(RealmId), byte_size(RealmId) =:= 32, is_binary(Name),
       is_binary(AdminKeyId), byte_size(AdminKeyId) =:= 32 ->
    unsigned(?TYPE_REALM_DIRECTORY, realm_directory_payload(RealmId, Name, AdminKeyId, Opts), Opts).

%% @doc The stations serving a realm, signed by the realm key.
-spec realm_stations(<<_:256>>, [realm_station_entry()]) -> m_record().
realm_stations(RealmId, Entries) ->
    realm_stations(RealmId, Entries, #{}).

-spec realm_stations(<<_:256>>, [realm_station_entry()], realm_stations_opts()) -> m_record().
realm_stations(RealmId, Entries, Opts) when is_binary(RealmId), byte_size(RealmId) =:= 32, is_list(Entries) ->
    unsigned(?TYPE_REALM_STATIONS, realm_stations_payload(RealmId, Entries), Opts).

%% @doc A realm's statement, signed by the realm key, that a node is a member with roles. Its window, valid_from to
%% valid_until, is at most 30 days and never ends before it starts: a longer one raises a badmatch on
%% `{error, endorsement_window_too_long}', and a reversed one on `{error, endorsement_window_reversed}'.
-spec realm_member_endorsement(<<_:256>>, #{realm := <<_:256>>, member_node := <<_:256>>, roles := [binary()]}) ->
          m_record().
realm_member_endorsement(RealmId, Spec) ->
    realm_member_endorsement(RealmId, Spec, #{}).

-spec realm_member_endorsement(<<_:256>>, #{realm := <<_:256>>, member_node := <<_:256>>, roles := [binary()]},
                               realm_member_endorsement_opts()) -> m_record().
realm_member_endorsement(RealmId, #{realm := RealmId, member_node := Member, roles := Roles}, Opts)
  when is_binary(RealmId), byte_size(RealmId) =:= 32, is_binary(Member), byte_size(Member) =:= 32,
       is_list(Roles) ->
    Now = erlang:system_time(millisecond),
    ValidFrom = maps:get(valid_from, Opts, Now),
    ValidUntil = maps:get(valid_until, Opts, ValidFrom + ?MAX_ENDORSEMENT_WINDOW_MS),
    ok = endorsement_window(ValidFrom, ValidUntil),
    unsigned(?TYPE_REALM_MEMBER_ENDORSEMENT,
             realm_member_endorsement_payload(RealmId, Member, Roles, ValidFrom, ValidUntil), Opts).

endorsement_window(From, Until) when Until < From -> {error, endorsement_window_reversed};
endorsement_window(From, Until) when Until - From > ?MAX_ENDORSEMENT_WINDOW_MS -> {error, endorsement_window_too_long};
endorsement_window(_From, _Until) -> ok.

%% @doc The longest window a realm member endorsement may have, valid_from to valid_until, in milliseconds: 30 days.
%% Its builder refuses a longer one, and macula_hyparview_endorsement:verify_endorsement/3 refuses one it receives.
-spec max_endorsement_window_ms() -> pos_integer().
max_endorsement_window_ms() ->
    ?MAX_ENDORSEMENT_WINDOW_MS.

%% @doc A realm's statement, signed by the realm key, that the org OrgName is held by the key with key id OrgKeyId.
-spec org_directory(<<_:256>>, binary(), <<_:256>>) -> m_record().
org_directory(RealmId, OrgName, OrgKeyId) ->
    org_directory(RealmId, OrgName, OrgKeyId, #{}).

-spec org_directory(<<_:256>>, binary(), <<_:256>>, map()) -> m_record().
org_directory(RealmId, OrgName, OrgKeyId, Opts)
  when is_binary(RealmId), byte_size(RealmId) =:= 32, is_binary(OrgName),
       is_binary(OrgKeyId), byte_size(OrgKeyId) =:= 32 ->
    Payload = #{{text, <<"realm_id">>} => RealmId,
                {text, <<"org_name">>} => {text, OrgName},
                {text, <<"org_key">>} => OrgKeyId},
    unsigned(?TYPE_ORG_DIRECTORY, Payload, Opts).

%% @doc An org's grant, signed by its org key, that the node Advertiser may serve procedures under the org.
-spec procedure_delegation(<<_:256>>, <<_:256>>) -> m_record().
procedure_delegation(OrgKeyId, Advertiser) ->
    procedure_delegation(OrgKeyId, Advertiser, #{}).

-spec procedure_delegation(<<_:256>>, <<_:256>>, map()) -> m_record().
procedure_delegation(OrgKeyId, Advertiser, Opts)
  when is_binary(OrgKeyId), byte_size(OrgKeyId) =:= 32, is_binary(Advertiser), byte_size(Advertiser) =:= 32 ->
    Payload = #{{text, <<"org_key">>} => OrgKeyId, {text, <<"advertiser">>} => Advertiser},
    unsigned(?TYPE_PROCEDURE_DELEGATION, Payload, Opts).

%% @doc A provider's advertisement of a procedure in a realm, signed by the provider. For a procedure with an org
%% namespace the authorization option carries the provider authorization: org_directory and procedure_delegation as
%% the records' wire form, the only authorization form. The builder refuses any other. The kem_key option names the
%% provider's KEM key as carried (E2E design, amendment A1), which the advertisement carries with its kem_key_id: a
%% caller seals its request to that key.
-spec procedure_advertisement(<<_:256>>, <<_:256>>, binary(), <<_:256>>) -> m_record().
procedure_advertisement(AdvertiserNode, RealmId, Procedure, ServingStation) ->
    procedure_advertisement(AdvertiserNode, RealmId, Procedure, ServingStation, #{}).

-spec procedure_advertisement(<<_:256>>, <<_:256>>, binary(), <<_:256>>, procedure_advertisement_opts()) ->
          m_record().
procedure_advertisement(AdvertiserNode, RealmId, Procedure, ServingStation, Opts)
  when is_binary(AdvertiserNode), byte_size(AdvertiserNode) =:= 32, is_binary(RealmId), byte_size(RealmId) =:= 32,
       is_binary(Procedure), is_binary(ServingStation), byte_size(ServingStation) =:= 32, is_map(Opts) ->
    Payload = #{{text, <<"realm_id">>} => RealmId,
                {text, <<"procedure">>} => {text, Procedure},
                {text, <<"advertiser_node">>} => AdvertiserNode,
                {text, <<"serving_station">>} => ServingStation},
    unsigned(?TYPE_PROCEDURE_ADVERTISEMENT,
             with_kem_key(with_authorization(Payload, maps:get(authorization, Opts, undefined)),
                          maps:get(kem_key, Opts, undefined)), Opts).

%% @doc A node's announcement, signed by the node, that it shares the content with this tag 2 content id, naming where
%% it is served (D27): the realm, the station the node is reachable through, and the node's content procedure.
-spec content_announcement(<<_:256>>, <<_:400>>, content_announcement_opts()) -> m_record().
content_announcement(AnnouncerNode, <<2, _Codec:8, _Hash:48/binary>> = MCID,
                     #{realm_id := <<_:256>>, serving_station := <<_:256>>, procedure := Procedure} = Opts)
  when is_binary(AnnouncerNode), byte_size(AnnouncerNode) =:= 32, is_binary(Procedure) ->
    unsigned(?TYPE_CONTENT_ANNOUNCEMENT, content_announcement_payload(AnnouncerNode, MCID, Opts), Opts).

%% @doc A foundation's seed list, signed by a foundation key.
-spec foundation_seed_list([foundation_seed()]) -> m_record().
foundation_seed_list(Seeds) ->
    foundation_seed_list(Seeds, #{}).

-spec foundation_seed_list([foundation_seed()], foundation_seed_list_opts()) -> m_record().
foundation_seed_list(Seeds, Opts) when is_list(Seeds), is_map(Opts) ->
    Now = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    #{version := Version} = Unsigned = unsigned(?TYPE_FOUNDATION_SEED_LIST, #{}, Opts),
    Payload = foundation_seed_list_payload(Version, maps:get(valid_from, Opts, Now),
                                           maps:get(valid_until, Opts, Now + TtlMs), Seeds),
    Unsigned#{payload := Payload}.

%% @doc A foundation parameter, signed by a foundation key.
-spec foundation_parameter(binary(), foundation_parameter_value()) -> m_record().
foundation_parameter(Name, Value) ->
    foundation_parameter(Name, Value, #{}).

-spec foundation_parameter(binary(), foundation_parameter_value(), foundation_parameter_opts()) -> m_record().
foundation_parameter(Name, Value, Opts) when is_binary(Name), is_map(Opts) ->
    Now = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    #{version := Version} = Unsigned = unsigned(?TYPE_FOUNDATION_PARAMETER, #{}, Opts),
    Payload = foundation_parameter_payload(Name, Value, Version, maps:get(valid_from, Opts, Now),
                                           maps:get(valid_until, Opts, Now + TtlMs),
                                           maps:get(prior_version, Opts, undefined)),
    Unsigned#{payload := Payload}.

%% @doc A foundation's list of trusted realms: each entry pairs a realm id with the realm key id that signs the
%% realm's records, signed by a foundation key. Its payload holds exactly `realms_trusted' (D28).
-spec foundation_realm_trust_list([foundation_realm_trust_list_entry()]) -> m_record().
foundation_realm_trust_list(Trusted) ->
    foundation_realm_trust_list(Trusted, #{}).

-spec foundation_realm_trust_list([foundation_realm_trust_list_entry()],
                                  foundation_realm_trust_list_opts()) -> m_record().
foundation_realm_trust_list(Trusted, Opts) when is_list(Trusted), is_map(Opts) ->
    Payload = #{{text, <<"realms_trusted">>} => [trust_list_entry(Entry) || Entry <- Trusted]},
    unsigned(?TYPE_FOUNDATION_REALM_TRUST_LIST, Payload, Opts).

trust_list_entry(#{realm_id := RealmId, realm_key_id := RealmKeyId} = Entry)
  when map_size(Entry) =:= 2, is_binary(RealmId), byte_size(RealmId) =:= 32,
       is_binary(RealmKeyId), byte_size(RealmKeyId) =:= 32 ->
    #{{text, <<"realm_id">>} => RealmId, {text, <<"realm_key_id">>} => RealmKeyId}.

%% @doc A foundation's tier 3 attestation of a station, signed by a foundation key.
-spec foundation_t3_attestation(<<_:256>>, pos_integer()) -> m_record().
foundation_t3_attestation(StationId, AuditDate) ->
    foundation_t3_attestation(StationId, AuditDate, #{}).

-spec foundation_t3_attestation(<<_:256>>, pos_integer(), foundation_t3_attestation_opts()) -> m_record().
foundation_t3_attestation(StationId, AuditDate, Opts)
  when is_binary(StationId), byte_size(StationId) =:= 32, is_integer(AuditDate), AuditDate > 0, is_map(Opts) ->
    Now = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    Payload = foundation_t3_attestation_payload(StationId, AuditDate, maps:get(valid_until, Opts, Now + TtlMs),
                                                maps:get(notes, Opts, undefined)),
    unsigned(?TYPE_FOUNDATION_T3_ATTESTATION, Payload, Opts).

%% @doc A station's dialable endpoint, signed by the station and stored under its node_id.
-spec station_endpoint(1..65535) -> m_record().
station_endpoint(QuicPort) ->
    station_endpoint(QuicPort, #{}).

-spec station_endpoint(1..65535, station_endpoint_opts()) -> m_record().
station_endpoint(QuicPort, Opts) when is_integer(QuicPort), QuicPort > 0, QuicPort =< 65535, is_map(Opts) ->
    Payload0 = #{{text, <<"quic_port">>} => QuicPort},
    Payload1 = with_host_list(Payload0, maps:get(host_advertised, Opts, undefined)),
    Payload = with_text(Payload1, <<"alpn">>, maps:get(alpn, Opts, undefined)),
    unsigned(?TYPE_STATION_ENDPOINT, Payload, maps:merge(#{ttl_ms => ?STATION_ENDPOINT_TTL_MS}, Opts)).

%% @doc A tombstone that withdraws a record: it names the record's type, version and slot fields, takes the record's
%% slot, and lives until the record has expired plus the clock tolerance, so no replica serves the record again after
%% the tombstone lapses. Sign it with the key that signed the record.
-spec tombstone(m_record(), reason()) -> m_record().
tombstone(Withdrawn, Reason) ->
    tombstone(Withdrawn, Reason, #{}).

-spec tombstone(m_record(), reason(), tombstone_opts()) -> m_record().
tombstone(#{type := Type, version := Version, expires_at := WithdrawnExpiry, payload := WithdrawnPayload} = Withdrawn,
          Reason, Opts)
  when Type =/= ?TYPE_TOMBSTONE, (Reason =:= shutdown orelse Reason =:= moved orelse Reason =:= revoked),
       is_map(Opts) ->
    Base = #{{text, <<"withdrawn_type">>} => Type,
             {text, <<"withdrawn_version">>} => Version,
             {text, <<"reason">>} => {text, atom_to_binary(Reason)}},
    Slot = slot_fields(slot_field_names(Type, maps:get(subject, Withdrawn, undefined)), WithdrawnPayload,
                       maps:get(subject, Withdrawn, undefined)),
    Payload = with_text(maps:merge(Base, Slot), <<"detail">>, maps:get(detail, Opts, undefined)),
    #{created_at := Created} = Unsigned = unsigned(?TYPE_TOMBSTONE, Payload, Opts),
    Unsigned#{expires_at := max(Created + maps:get(ttl_ms, Opts, ?CLOCK_TOLERANCE_MS),
                                WithdrawnExpiry + ?CLOCK_TOLERANCE_MS)}.

%% @doc An unsigned record of a domain type (tags 0x20 to 0xFF). The subject_id option names the record's subject, a
%% non-empty binary: an empty subject would name a slot apart from no subject.
-spec envelope(type_tag(), map(), map()) -> m_record().
envelope(Type, Payload, Opts)
  when is_integer(Type), Type >= ?DOMAIN_TYPE_MIN, Type =< 16#FF, is_map(Payload), is_map(Opts) ->
    with_subject(unsigned(Type, Payload, Opts), maps:get(subject_id, Opts, undefined)).

%%------------------------------------------------------------------
%% Signing, verifying and the wire form
%%------------------------------------------------------------------

%% @doc Sign a record with a key whose purpose fits its type. Raises key_purpose_mismatch for a key of another
%% purpose, key_id_mismatch when the payload names a signer other than this key, {malformed, Type} for a record whose
%% fields, subject or payload verify/3 would refuse, and record_too_large past 256 KiB. The field and payload checks
%% are verify/3's own, run before anything is signed, so sign/2 never returns a record verify/3 refuses apart from
%% the clock.
-spec sign(m_record(), macula_node_keys:node_key()) -> m_record().
sign(#{type := Type, payload := Payload} = Record, #{purpose := Purpose, profile := Profile} = Key) ->
    ok = purpose_fits(lists:member(Purpose, signer_purposes(Type, Payload)), {Type, Purpose}),
    ok = lifetime_checked(lifetime(Record), Type),
    Carried = macula_node_keys:public_key(Key),
    KeyId = key_id_of(signer_kind(Type, Payload), Carried, Profile),
    ok = signer_matches(named_signer(Type, Payload, KeyId), Type),
    Fields = tbs_fields(Record),
    ok = verifiable(read_tbs(Fields#{{text, <<"alg">>} => {text, macula_signed_object:alg(Profile)}}), Type),
    ok = verifiable(payload_ok(Type, Payload), Type),
    ok = size_fits(byte_size(macula_record_cbor:encode(Fields)) + byte_size(Carried)
                   + macula_node_keys:signature_bytes(Profile)),
    Object = macula_signed_object:sign(?LABEL, Fields, Key),
    ok = size_fits(byte_size(macula_signed_object:encode(Object))),
    #{tbs := Tbs, signature := Signature} = Object,
    Record#{key => Carried, key_id => KeyId, alg => macula_signed_object:alg(Profile), tbs => Tbs,
            signature => Signature}.

%% @doc Verify a record, given as its wire form or as its {key, tbs, signature} map, under the verifier's profile and
%% clock. Refusals are returned, never raised.
-spec verify(binary() | map(), macula_crypto_profile:profile()) -> {ok, m_record()} | {error, refusal()}.
verify(Signed, Profile) ->
    verify(Signed, Profile, erlang:system_time(millisecond)).

-spec verify(binary() | map(), macula_crypto_profile:profile(), integer()) -> {ok, m_record()} | {error, refusal()}.
verify(Bytes, _Profile, _Now) when is_binary(Bytes), byte_size(Bytes) > ?MAX_RECORD_BYTES ->
    {error, record_too_large};
verify(Bytes, Profile, Now) when is_binary(Bytes) ->
    decoded_object(macula_signed_object:decode(Bytes), Profile, Now);
verify(#{key := Key, tbs := Tbs, signature := Signature} = Object, Profile, Now)
  when map_size(Object) =:= 3, is_binary(Key), is_binary(Tbs), is_binary(Signature) ->
    sized_object(byte_size(macula_signed_object:encode(Object)), Object, Profile, Now);
verify(_Other, _Profile, _Now) ->
    {error, malformed}.

%% The most entries a slot holds: 64 places for checked signers and 16 for everyone else
%% (DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md, 2.3).
-define(SLOT_CAPACITY, 80).
%% The most entries under the expected key that signer_entry/4 verifies.
-define(SIGNER_ENTRY_VERIFIES, 4).

-type signer_entry_stats() :: #{matching := non_neg_integer(), verified := non_neg_integer(),
                                beyond_capacity := non_neg_integer()}.

%% @doc signer_entry/4 at the current time.
-spec signer_entry([binary() | map()], {key_id | node_id, <<_:256>>}, macula_crypto_profile:profile()) ->
        {{ok, m_record()} | {error, not_found | refusal()}, signer_entry_stats()}.
signer_entry(Entries, Expected, Profile) ->
    signer_entry(Entries, Expected, Profile, erlang:system_time(millisecond)).

%% @doc The entry one signer holds among the entries a lookup of a slot returns, as wire forms or {key, tbs, signature}
%% maps. Expected is {key_id, Id} for a key named by its MACULA-KEY-ID-V1 key id, or {node_id, Id} for an identity key.
%% Every entry is read as far as its carried key, whatever the answer's order or length, and no signature is checked
%% to select. Only the entries under the expected key are verified: highest claimed version first, stopping at the
%% first that verifies and names Id as its key_id, and at most 4. So an entry under another key costs no verify, and
%% forged entries under the expected key cost a bounded few. The outcome is that record, not_found when no entry is
%% under the key, or the refusal of the last entry verified. The stats count the entries under the key, the entries
%% verified, and the entries past the 80 a slot holds, which a station that keeps to its slot places never sends.
-spec signer_entry([binary() | map()], {key_id | node_id, <<_:256>>}, macula_crypto_profile:profile(), integer()) ->
        {{ok, m_record()} | {error, not_found | refusal()}, signer_entry_stats()}.
signer_entry(Entries, {Kind, <<_:256>> = Id}, Profile, Now)
  when is_list(Entries), (Kind =:= key_id orelse Kind =:= node_id), is_integer(Now) ->
    Matching = by_claimed_version([Object || Entry <- Entries, {ok, Object} <- [entry_object(Entry)],
                                             carried_id(Kind, Object, Profile) =:= Id]),
    {Outcome, Verified} = first_verified(lists:sublist(Matching, ?SIGNER_ENTRY_VERIFIES), Id, Profile, Now,
                                         {{error, not_found}, 0}),
    {Outcome, #{matching => length(Matching), verified => Verified,
                beyond_capacity => max(0, length(Entries) - ?SLOT_CAPACITY)}}.

%% An entry's outer object, decoded as far as its byte strings: nothing inside its tbs is read.
entry_object(Bytes) when is_binary(Bytes) ->
    macula_signed_object:decode(Bytes);
entry_object(#{key := Key, tbs := Tbs, signature := Signature} = Object)
  when map_size(Object) =:= 3, is_binary(Key), is_binary(Tbs), is_binary(Signature) ->
    {ok, Object};
entry_object(_NotAnObject) ->
    error.

carried_id(key_id, #{key := Key}, Profile) -> macula_node_keys:key_id(Key, Profile);
carried_id(node_id, #{key := Key}, Profile) -> macula_node_keys:node_id(Key, Profile);
carried_id(_Kind, _HeldObject, _Profile) -> none.

%% Entries under one key, highest claimed version first. The version is read from the tbs without verifying it, and an
%% entry whose version cannot be read comes last. Entries claiming the same version keep no chosen order, so more than
%% 4 of them forged ahead of the real entry make the lookup refuse. Only a misbehaving station sends them, since a store
%% keeps one entry per signer, and such a station could withhold the entry anyway.
by_claimed_version(Objects) ->
    Claimed = lists:keysort(1, [{claimed_version(Object), Object} || Object <- Objects]),
    [Object || {_Version, Object} <- lists:reverse(Claimed)].

claimed_version(#{tbs := Tbs}) ->
    version_claimed(macula_record_cbor:decode_strict(Tbs)).

version_claimed({ok, #{{text, <<"version">>} := <<_:128>> = Version}}) -> Version;
version_claimed(_Unreadable) -> <<>>.

%% Entries verified in order until one verifies and names Id as its key_id, with the count of entries verified.
first_verified([], _Id, _Profile, _Now, Last) ->
    Last;
first_verified([Object | Rest], Id, Profile, Now, {_Outcome, Verified}) ->
    entry_verified(verify(Object, Profile, Now), Rest, Id, Profile, Now, Verified + 1).

entry_verified({ok, #{key_id := Id} = Record}, _Rest, Id, _Profile, _Now, Verified) ->
    {{ok, Record}, Verified};
entry_verified({ok, _OtherSigner}, Rest, Id, Profile, Now, Verified) ->
    first_verified(Rest, Id, Profile, Now, {{error, key_id_mismatch}, Verified});
entry_verified({error, _} = Refusal, Rest, Id, Profile, Now, Verified) ->
    first_verified(Rest, Id, Profile, Now, {Refusal, Verified}).

%% @doc The record with a new version, created now, with the same lifetime, signed again with Key.
-spec refresh(m_record(), macula_node_keys:node_key()) -> m_record().
refresh(Record, Key) ->
    refreshed(Record, Key, erlang:system_time(millisecond), no_bound).

%% @doc The same, ending the record at NotAfter when that comes before its lifetime runs out: an ABSOLUTE time in
%% milliseconds, to cap a record against something else, such as a delegation's own expiry. A bound at or before
%% the clock this signs on returns `{error, not_after_passed}' and signs nothing.
%%
%% ⚠ ONE CLOCK READ DECIDES BOTH ENDS, and that is the point. Writing the bound into a record and refreshing it
%% afterwards does not work: refresh keeps a record's LIFETIME, so a bound written as `expires_at' is re-anchored
%% to the later clock read and the record ends at the bound plus its own age at signing time. That was the defect
%% here until 2026-09-22, and the gap it opened is not bounded by anything: it is however long the record sat
%% unsigned.
-spec refresh(m_record(), macula_node_keys:node_key(), non_neg_integer()) ->
        {ok, m_record()} | {error, not_after_passed}.
refresh(Record, Key, NotAfter) when is_integer(NotAfter) ->
    bounded_refresh(erlang:system_time(millisecond), Record, Key, NotAfter).

bounded_refresh(Now, _Record, _Key, NotAfter) when NotAfter =< Now ->
    {error, not_after_passed};
bounded_refresh(Now, Record, Key, NotAfter) ->
    {ok, refreshed(Record, Key, Now, NotAfter)}.

refreshed(#{created_at := Created, expires_at := Expires} = Record, Key, Now, Bound) ->
    Fresh = maps:with([type, payload, subject], Record),
    sign(Fresh#{version => macula_record_uuid:v7_monotonic(Now), created_at => Now,
                expires_at => ends_at(Now + (Expires - Created), Bound)},
         Key).

ends_at(Expires, no_bound) -> Expires;
ends_at(Expires, NotAfter) -> min(Expires, NotAfter).

%% @doc The wire form of a signed or verified record: its {key, tbs, signature} map, tbs unchanged.
-spec encode(m_record()) -> binary().
encode(#{key := Key, tbs := Tbs, signature := Signature}) ->
    macula_signed_object:encode(#{key => Key, tbs => Tbs, signature => Signature}).

%%------------------------------------------------------------------
%% Accessors
%%------------------------------------------------------------------

-spec type(m_record()) -> type_tag().
type(#{type := Type}) -> Type.

-spec key(m_record()) -> binary().
key(#{key := Key}) -> Key.

-spec key_id(m_record()) -> <<_:256>>.
key_id(#{key_id := KeyId}) -> KeyId.

-spec version(m_record()) -> version().
version(#{version := Version}) -> Version.

-spec created_at(m_record()) -> non_neg_integer().
created_at(#{created_at := Created}) -> Created.

-spec expires_at(m_record()) -> non_neg_integer().
expires_at(#{expires_at := Expires}) -> Expires.

-spec payload(m_record()) -> map().
payload(#{payload := Payload}) -> Payload.

-spec signature(m_record()) -> binary().
signature(#{signature := Signature}) -> Signature.

%% @doc The procedure advertisement type tag, for callers that look records up by type.
-spec type_procedure_advertisement() -> type_tag().
type_procedure_advertisement() -> ?TYPE_PROCEDURE_ADVERTISEMENT.

%% @doc A payload field, read whatever key form it arrived in: `{text, Name}', the binary, or an existing atom. A
%% text value is returned as its binary.
-spec payload_field(map(), binary()) -> term().
payload_field(Payload, Name) ->
    unwrap_text(first_present([{text, Name}, Name, safe_atom(Name)], Payload)).

%%------------------------------------------------------------------
%% Readers
%%------------------------------------------------------------------

-spec read_node_record(m_record()) -> map().
read_node_record(#{type := ?TYPE_NODE_RECORD, payload := P}) ->
    #{node_id      => payload_field(P, <<"node_id">>),
      station_id   => payload_field(P, <<"station_id">>),
      realms       => payload_field(P, <<"realms">>),
      capabilities => payload_field(P, <<"capabilities">>),
      kind         => payload_field(P, <<"kind">>),
      hostname     => payload_field(P, <<"hostname">>),
      endpoint     => payload_field(P, <<"endpoint">>),
      city         => payload_field(P, <<"city">>),
      country      => payload_field(P, <<"country">>),
      lat          => parse_geo(maps:get({text, <<"lat">>}, P, undefined), ?LAT_BOUND),
      lng          => parse_geo(maps:get({text, <<"lng">>}, P, undefined), ?LNG_BOUND),
      display_name => payload_field(P, <<"display_name">>),
      caps_hint    => payload_field(P, <<"caps_hint">>),
      peers        => payload_field(P, <<"peers">>),
      %% Stamped by a station's re-announce heartbeat: the station's own reported build.
      version      => payload_field(P, <<"version">>)}.

-spec read_procedure_advertisement(m_record()) -> map().
read_procedure_advertisement(#{type := ?TYPE_PROCEDURE_ADVERTISEMENT, payload := P}) ->
    Read = #{realm_id        => payload_field(P, <<"realm_id">>),
      procedure       => payload_field(P, <<"procedure">>),
      advertiser_node => payload_field(P, <<"advertiser_node">>),
      serving_station => payload_field(P, <<"serving_station">>),
      authorization   => read_authorization(maps:get({text, <<"authorization">>}, P, undefined))},
    maps:merge(Read, maps:from_list([{Field, maps:get({text, Name}, P)}
                                     || {Name, Field} <- [{<<"kem_key">>, kem_key}, {<<"kem_key_id">>, kem_key_id}],
                                        maps:is_key({text, Name}, P)])).

-spec read_station_endpoint(m_record()) -> #{quic_port := 1..65535, host_advertised := [binary()]}.
read_station_endpoint(#{type := ?TYPE_STATION_ENDPOINT, payload := P}) ->
    #{quic_port       => payload_field(P, <<"quic_port">>),
      host_advertised => host_list(payload_field(P, <<"host_advertised">>))}.

-spec read_tombstone(m_record()) -> map().
read_tombstone(#{type := ?TYPE_TOMBSTONE, payload := P}) ->
    Base = #{withdrawn_type    => payload_field(P, <<"withdrawn_type">>),
             withdrawn_version => payload_field(P, <<"withdrawn_version">>),
             reason            => payload_field(P, <<"reason">>),
             detail            => payload_field(P, <<"detail">>)},
    maps:merge(Base, maps:from_list([{Field, payload_field(P, Name)}
                                     || {Name, Field} <- slot_field_atoms(), maps:is_key({text, Name}, P)])).

-spec read_org_directory(m_record()) -> #{realm_id := <<_:256>>, org_name := binary(), org_key := <<_:256>>}.
read_org_directory(#{type := ?TYPE_ORG_DIRECTORY, payload := P}) ->
    #{realm_id => payload_field(P, <<"realm_id">>),
      org_name => payload_field(P, <<"org_name">>),
      org_key  => payload_field(P, <<"org_key">>)}.

-spec read_procedure_delegation(m_record()) -> #{org_key := <<_:256>>, advertiser := <<_:256>>}.
read_procedure_delegation(#{type := ?TYPE_PROCEDURE_DELEGATION, payload := P}) ->
    #{org_key    => payload_field(P, <<"org_key">>),
      advertiser => payload_field(P, <<"advertiser">>)}.

-spec read_content_announcement(m_record()) -> map().
read_content_announcement(#{type := ?TYPE_CONTENT_ANNOUNCEMENT, payload := P}) ->
    #{announcer_node => payload_field(P, <<"announcer_node">>),
      mcid           => payload_field(P, <<"mcid">>),
      realm_id       => payload_field(P, <<"realm_id">>),
      serving_station => payload_field(P, <<"serving_station">>),
      procedure      => payload_field(P, <<"procedure">>),
      name           => payload_field(P, <<"name">>),
      size           => payload_field(P, <<"size">>),
      chunk_count    => payload_field(P, <<"chunk_count">>)}.

%% @doc The trusted realms of a foundation realm trust list, as realm id to realm key id (D28).
-spec read_foundation_realm_trust_list(m_record()) -> #{<<_:256>> => <<_:256>>}.
read_foundation_realm_trust_list(#{type := ?TYPE_FOUNDATION_REALM_TRUST_LIST, payload := P}) ->
    maps:from_list(
      [{RealmId, RealmKeyId}
       || #{{text, <<"realm_id">>} := RealmId, {text, <<"realm_key_id">>} := RealmKeyId}
              <- maps:get({text, <<"realms_trusted">>}, P)]).

%%------------------------------------------------------------------
%% Provider authorization (D25 item 6)
%%------------------------------------------------------------------

%% @doc The longest a procedure advertisement lives, `created_at' to `expires_at'; every verifier refuses a longer one.
-spec procedure_advertisement_max_lifetime_ms() -> pos_integer().
procedure_advertisement_max_lifetime_ms() ->
    ?PROCEDURE_ADVERTISEMENT_MAX_LIFETIME_MS.

%% @doc Whether a procedure advertisement is in its advertiser's own namespace, and admissible there (D25 item 6,
%% revised 2026-09-24): a procedure `~<node_id>/<name>', where `<node_id>' is the 64 lowercase hex characters of the
%% advertisement's `advertiser_node', carrying no authorization. Verifying the advertisement binds `advertiser_node'
%% to its signer, so only that node can sign for its namespace. `not_own_namespace' for an org procedure, a procedure
%% without a namespace or another node's namespace; `malformed' for a `~' namespace that is not 64 lowercase hex;
%% `authorization_not_allowed' when one is attached. The one rule the SDK and the station's admissions share.
-spec own_namespace(m_record()) -> ok | {error, not_own_namespace | malformed | authorization_not_allowed}.
own_namespace(#{type := ?TYPE_PROCEDURE_ADVERTISEMENT} = Advertisement) ->
    #{procedure := Procedure, advertiser_node := Advertiser, authorization := Authorization} =
        read_procedure_advertisement(Advertisement),
    own_namespace_of(procedure_org(Procedure), Advertiser, Authorization);
own_namespace(_NotAnAdvertisement) ->
    {error, not_own_namespace}.

own_namespace_of({org, <<"~", Hex/binary>>}, Advertiser, Authorization) ->
    own_node(node_hex(Hex), Advertiser, Authorization);
own_namespace_of(_NotOwn, _Advertiser, _Authorization) ->
    {error, not_own_namespace}.

own_node({ok, Advertiser}, Advertiser, undefined) -> ok;
own_node({ok, Advertiser}, Advertiser, _Attached) -> {error, authorization_not_allowed};
own_node({ok, _AnotherNode}, _Advertiser, _Authorization) -> {error, not_own_namespace};
own_node(malformed, _Advertiser, _Authorization) -> {error, malformed}.

%% Exactly 64 lowercase hex characters, the one spelling of a node_id in a namespace, so a node has one namespace.
node_hex(Hex) when byte_size(Hex) =:= 64 ->
    lowercase_hex(Hex, binary:encode_hex(decoded_or_empty(Hex), lowercase));
node_hex(_Hex) ->
    malformed.

decoded_or_empty(Hex) ->
    try binary:decode_hex(Hex)
    catch error:badarg -> <<>>
    end.

lowercase_hex(Hex, Hex) -> {ok, binary:decode_hex(Hex)};
lowercase_hex(_Hex, _Other) -> malformed.

%% @doc A procedure's org namespace: the text before the first "/" of its name, when there is one and it is not "_".
-spec procedure_org(binary()) -> {org, binary()} | none | {error, malformed}.
procedure_org(Procedure) when is_binary(Procedure) ->
    org_of(binary:split(Procedure, <<"/">>)).

%% @doc The caller's check of a verified advertisement's provider authorization, against the realm trust it holds:
%% the realm-signed org directory and the org-signed procedure delegation, the only authorization form. The realm
%% key is the carried realm key the caller pins, or the realm key id the foundation realm trust list's pairs name
%% for the advertisement's realm_id (D28). A procedure with an org namespace needs an authorization for that org,
%% a procedure in its advertiser's own namespace (`~<node_id>/<name>', `own_namespace/1') carries none and needs no
%% realm key, a procedure without a namespace carries none, and the advertisement expires no later than any part of its
%% authorization. An authorization in any other form, a certificate chain included, is refused as
%% authorization_form_unsupported: 11.0.0 has no certificate form.
-spec verify_authorization(m_record(), trust(), integer()) -> ok | {error, authorization_refusal()}.
verify_authorization(#{type := ?TYPE_PROCEDURE_ADVERTISEMENT} = Advertisement, #{profile := _} = Trust, Now) ->
    #{procedure := Procedure, authorization := Authorization} = read_procedure_advertisement(Advertisement),
    authorization_for(procedure_org(Procedure), Authorization, Advertisement, Trust, Now).

%%------------------------------------------------------------------
%% Storage keys
%%------------------------------------------------------------------

%% @doc The 32-byte DHT storage key of a record. A record stored under its signer needs its key_id, so it must be
%% signed or verified; a record named by its payload does not.
-spec storage_key(m_record()) -> <<_:256>>.
storage_key(#{type := ?TYPE_TOMBSTONE, payload := P} = Record) ->
    slot(maps:get({text, <<"withdrawn_type">>}, P), P, maps:get({text, <<"subject">>}, P, undefined),
         maps:get(key_id, Record, undefined));
storage_key(#{type := Type, payload := P} = Record) ->
    slot(Type, P, maps:get(subject, Record, undefined), maps:get(key_id, Record, undefined)).

%% @doc The storage key of a procedure's advertisements, from the realm id and the procedure name.
-spec procedure_key(<<_:256>>, binary()) -> <<_:256>>.
procedure_key(<<_:256>> = RealmId, Procedure) when is_binary(Procedure) ->
    derived(?TYPE_PROCEDURE_ADVERTISEMENT, [RealmId, {field, Procedure}]).

%% @doc The storage key of a content id's announcements.
-spec content_key(<<_:400>>) -> <<_:256>>.
content_key(<<2, _Codec:8, _Hash:48/binary>> = MCID) ->
    derived(?TYPE_CONTENT_ANNOUNCEMENT, [{field, MCID}]).

%% @doc The storage key of a station's endpoint record, from the station's node_id.
-spec station_endpoint_key(<<_:256>>) -> <<_:256>>.
station_endpoint_key(<<_:256>> = NodeId) ->
    derived(?TYPE_STATION_ENDPOINT, [NodeId]).

%% @doc The storage key of an org directory record, from the realm id and the org name.
-spec org_directory_key(<<_:256>>, binary()) -> <<_:256>>.
org_directory_key(<<_:256>> = RealmId, OrgName) when is_binary(OrgName) ->
    derived(?TYPE_ORG_DIRECTORY, [RealmId, {field, OrgName}]).

%% @doc The storage key of a procedure delegation, from the org key id and the advertiser's node_id.
-spec procedure_delegation_key(<<_:256>>, <<_:256>>) -> <<_:256>>.
procedure_delegation_key(<<_:256>> = OrgKeyId, <<_:256>> = Advertiser) ->
    derived(?TYPE_PROCEDURE_DELEGATION, [OrgKeyId, Advertiser]).

%% @doc The storage key of a foundation's realm trust list, from the foundation key id. The station computes it
%% to fetch the list without holding its record.
-spec foundation_realm_trust_list_key(<<_:256>>) -> <<_:256>>.
foundation_realm_trust_list_key(<<_:256>> = FoundationKeyId) ->
    derived(?TYPE_FOUNDATION_REALM_TRUST_LIST, [FoundationKeyId]).

%%------------------------------------------------------------------
%% Internals: signing
%%------------------------------------------------------------------

unsigned(Type, Payload, Opts) ->
    Now = erlang:system_time(millisecond),
    #{type => Type, version => macula_record_uuid:v7_monotonic(Now), created_at => Now,
      expires_at => Now + maps:get(ttl_ms, Opts, default_ttl(Type)), payload => Payload}.

with_subject(Record, undefined) -> Record;
with_subject(Record, Subject) when is_binary(Subject), byte_size(Subject) > 0 -> Record#{subject => Subject}.

tbs_fields(#{type := Type, version := Version, created_at := Created, expires_at := Expires, payload := Payload} = R) ->
    Fields = #{{text, <<"type">>} => Type, {text, <<"version">>} => Version, {text, <<"created_at">>} => Created,
               {text, <<"expires_at">>} => Expires, {text, <<"payload">>} => Payload},
    with_tbs_subject(Fields, maps:get(subject, R, undefined)).

with_tbs_subject(Fields, undefined) -> Fields;
with_tbs_subject(Fields, Subject) -> Fields#{{text, <<"subject">>} => Subject}.

purpose_fits(true, _Detail) -> ok;
purpose_fits(false, Detail) -> erlang:error({key_purpose_mismatch, Detail}).

lifetime_checked(ok, _Type) -> ok;
lifetime_checked(Refusal, Type) -> erlang:error({Refusal, Type}).

signer_matches(true, _Type) -> ok;
signer_matches(false, Type) -> erlang:error({key_id_mismatch, Type}).

%% A record handed to sign/2 passes verify/3's own tbs reader and payload rules, or it is refused before anything is
%% signed.
verifiable({ok, _Read}, _Type) -> ok;
verifiable(true, _Type) -> ok;
verifiable(_Refused, Type) -> erlang:error({malformed, Type}).

size_fits(Bytes) when Bytes =< ?MAX_RECORD_BYTES -> ok;
size_fits(Bytes) -> erlang:error({record_too_large, Bytes}).

%% The purposes of the keys that may sign a type. A tombstone is signed like the type it withdraws.
signer_purposes(?TYPE_TOMBSTONE, #{{text, <<"withdrawn_type">>} := Withdrawn}) ->
    signer_purposes(Withdrawn, #{});
signer_purposes(Type, _Payload) when Type =:= ?TYPE_NODE_RECORD; Type =:= ?TYPE_PROCEDURE_ADVERTISEMENT;
                                     Type =:= ?TYPE_CONTENT_ANNOUNCEMENT; Type =:= ?TYPE_STATION_ENDPOINT ->
    [identity];
signer_purposes(Type, _Payload) when Type =:= ?TYPE_REALM_DIRECTORY; Type =:= ?TYPE_REALM_STATIONS;
                                     Type =:= ?TYPE_REALM_MEMBER_ENDORSEMENT; Type =:= ?TYPE_ORG_DIRECTORY ->
    [realm];
signer_purposes(?TYPE_PROCEDURE_DELEGATION, _Payload) ->
    [org];
signer_purposes(Type, _Payload) when Type >= ?TYPE_FOUNDATION_SEED_LIST, Type =< ?TYPE_FOUNDATION_T3_ATTESTATION ->
    [foundation];
signer_purposes(Type, _Payload) when Type >= ?DOMAIN_TYPE_MIN ->
    [identity, realm, org, foundation];
signer_purposes(_Type, _Payload) ->
    [].

%% Node-signed types are named by the node_id; every other type by the MACULA-KEY-ID-V1 key id.
signer_kind(?TYPE_TOMBSTONE, #{{text, <<"withdrawn_type">>} := Withdrawn}) ->
    signer_kind(Withdrawn, #{});
signer_kind(Type, _Payload) when Type =:= ?TYPE_NODE_RECORD; Type =:= ?TYPE_PROCEDURE_ADVERTISEMENT;
                                 Type =:= ?TYPE_CONTENT_ANNOUNCEMENT; Type =:= ?TYPE_STATION_ENDPOINT ->
    node;
signer_kind(_Type, _Payload) ->
    other.

key_id_of(node, Carried, Profile) -> macula_node_keys:node_id(Carried, Profile);
key_id_of(other, Carried, Profile) -> macula_node_keys:key_id(Carried, Profile).

%% Whether the payload field that names the signer, where a type has one, holds the signer's key id.
named_signer(Type, Payload, KeyId) ->
    signer_field_holds(signer_field(Type), Payload, KeyId).

signer_field(?TYPE_NODE_RECORD) -> <<"node_id">>;
signer_field(?TYPE_PROCEDURE_ADVERTISEMENT) -> <<"advertiser_node">>;
signer_field(?TYPE_CONTENT_ANNOUNCEMENT) -> <<"announcer_node">>;
signer_field(?TYPE_PROCEDURE_DELEGATION) -> <<"org_key">>;
signer_field(_Type) -> none.

signer_field_holds(none, _Payload, _KeyId) -> true;
signer_field_holds(Name, Payload, KeyId) -> maps:get({text, Name}, Payload, undefined) =:= KeyId.

%% @doc Whether a node signs this record about itself: a node record, a procedure advertisement or a content
%% announcement, whose payload names the signing node. A tombstone is not one: it withdraws a record, and whoever signs
%% it checks that the record was theirs.
-spec node_signed(term()) -> boolean().
node_signed(#{type := Type, payload := Payload}) when is_integer(Type), Type =/= ?TYPE_TOMBSTONE, is_map(Payload) ->
    names_its_node(Type);
node_signed(_NotARecord) ->
    false.

names_its_node(Type) ->
    signer_kind(Type, #{}) =:= node andalso signer_field(Type) =/= none.

%% The longest a record of Type lives. A tombstone's follows the type it withdraws, plus twice the clock tolerance: the
%% record it withdraws may be created up to the tolerance ahead, and the tombstone outlives it by the tolerance.
max_lifetime(?TYPE_NODE_RECORD, _Payload) -> ?NODE_RECORD_MAX_LIFETIME_MS;
max_lifetime(?TYPE_CONTENT_ANNOUNCEMENT, _Payload) -> ?CONTENT_ANNOUNCEMENT_MAX_LIFETIME_MS;
max_lifetime(?TYPE_PROCEDURE_ADVERTISEMENT, _Payload) -> ?PROCEDURE_ADVERTISEMENT_MAX_LIFETIME_MS;
max_lifetime(?TYPE_STATION_ENDPOINT, _Payload) -> ?STATION_ENDPOINT_TTL_MS;
max_lifetime(Type, _Payload) when Type =:= ?TYPE_REALM_STATIONS; Type =:= ?TYPE_ORG_DIRECTORY ->
    ?REALM_AND_ORG_MAX_LIFETIME_MS;
max_lifetime(?TYPE_PROCEDURE_DELEGATION, _Payload) -> ?PROCEDURE_DELEGATION_MAX_LIFETIME_MS;
max_lifetime(?TYPE_REALM_MEMBER_ENDORSEMENT, _Payload) -> ?MAX_ENDORSEMENT_WINDOW_MS;
max_lifetime(?TYPE_TOMBSTONE, #{{text, <<"withdrawn_type">>} := Withdrawn}) when Withdrawn =/= ?TYPE_TOMBSTONE ->
    max_lifetime(Withdrawn, #{}) + 2 * ?CLOCK_TOLERANCE_MS;
max_lifetime(Type, _Payload) when is_integer(Type), Type >= ?DOMAIN_TYPE_MIN -> ?DOMAIN_RECORD_MAX_LIFETIME_MS;
max_lifetime(_Type, _Payload) -> ?DEFAULT_MAX_LIFETIME_MS.

%% Whether a record's lifetime, created_at to expires_at, runs forward and fits its type's maximum.
lifetime(#{type := Type, created_at := Created, expires_at := Expires, payload := Payload}) ->
    lifetime_within(Expires - Created, max_lifetime(Type, Payload)).

lifetime_within(Lifetime, _Max) when Lifetime =< 0 -> lifetime_reversed;
lifetime_within(Lifetime, Max) when Lifetime > Max -> lifetime_too_long;
lifetime_within(_Lifetime, _Max) -> ok.

%% A builder given no ttl takes 48 hours, or its type's maximum when that is shorter.
default_ttl(Type) ->
    min(?DEFAULT_TTL_MS, max_lifetime(Type, #{})).

%% @doc Check a payload before anything is encoded: its external size is at most 256 KiB, and it nests at most 63 levels
%% of maps and lists, which a record's tbs leaves it under the decoder's 64. Returns record_too_large or malformed.
-spec payload_bounded(term()) -> ok | {error, record_too_large | malformed}.
payload_bounded(Payload) ->
    sized_payload(erlang:external_size(Payload) =< ?MAX_RECORD_BYTES, Payload).

sized_payload(false, _Payload) -> {error, record_too_large};
sized_payload(true, Payload) -> nested_payload(nesting(Payload, 0) =< ?MAX_PAYLOAD_NESTING).

nested_payload(true) -> ok;
nested_payload(false) -> {error, malformed}.

%% How deep a term nests maps and lists, counted no further than one level past the bound.
nesting(Map, Depth) when is_map(Map) -> deepest(maps:to_list(Map), Depth + 1, Depth + 1);
nesting(List, Depth) when is_list(List) -> deepest(List, Depth + 1, Depth + 1);
nesting({Key, Value}, Depth) -> max(nesting(Key, Depth), nesting(Value, Depth));
nesting(_Leaf, Depth) -> Depth.

deepest(_Terms, _Level, Deepest) when Deepest > ?MAX_PAYLOAD_NESTING -> Deepest;
deepest([Term | Terms], Level, Deepest) -> deepest(Terms, Level, max(Deepest, nesting(Term, Level)));
deepest(_EndOrImproperTail, _Level, Deepest) -> Deepest.

%% @doc Check a record given as its wire form, or as a signed map, before it is decoded or encoded: a wire form is a
%% binary of at most 256 KiB, and a signed map's key, tbs and signature are binaries of at most 256 KiB together.
%% Returns record_too_large or malformed.
-spec wire_bounded(term()) -> ok | {error, record_too_large | malformed}.
wire_bounded(Bytes) when is_binary(Bytes) ->
    sized_wire(byte_size(Bytes));
wire_bounded(#{key := Key, tbs := Tbs, signature := Signature})
  when is_binary(Key), is_binary(Tbs), is_binary(Signature) ->
    sized_wire(byte_size(Key) + byte_size(Tbs) + byte_size(Signature));
wire_bounded(_NotAWireRecord) ->
    {error, malformed}.

sized_wire(Bytes) when Bytes =< ?MAX_RECORD_BYTES -> ok;
sized_wire(_Bytes) -> {error, record_too_large}.

%% @doc Whether a record is of a domain type (tags 0x20 to 0xFF): its owner sets its payload rules, and its slot is its
%% signer's key id, with its subject when it has one.
-spec domain_type(term()) -> boolean().
domain_type(#{type := Type}) when is_integer(Type), Type >= ?DOMAIN_TYPE_MIN, Type =< 16#FF -> true;
domain_type(_Other) -> false.

%% @doc Check an unsigned domain record before a pool signs it as its node: a domain type, a payload map, a subject that
%% is absent or a non-empty binary, a lifetime that runs forward and fits the type's maximum, never shortened, and a
%% payload and subject of at most 256 KiB together that nest at most 63 levels. Returns the refusal by name.
-spec domain_record_checked(term()) ->
          ok | {error, not_a_domain_type | invalid_subject | lifetime_too_long | lifetime_reversed | record_too_large
                       | malformed}.
domain_record_checked(#{type := Type, created_at := Created, expires_at := Expires, payload := Payload} = Record)
  when is_integer(Type), Type >= ?DOMAIN_TYPE_MIN, Type =< 16#FF, is_integer(Created), is_integer(Expires),
       is_map(Payload) ->
    domain_subject_checked(maps:get(subject, Record, none), Record);
domain_record_checked(#{type := Type}) when is_integer(Type), (Type < ?DOMAIN_TYPE_MIN orelse Type > 16#FF) ->
    {error, not_a_domain_type};
domain_record_checked(_NotADomainRecord) ->
    {error, malformed}.

domain_subject_checked(none, Record) ->
    domain_lifetime_checked(lifetime(Record), Record);
domain_subject_checked(Subject, Record) when is_binary(Subject), byte_size(Subject) > 0 ->
    domain_lifetime_checked(lifetime(Record), Record);
domain_subject_checked(_NotASubject, _Record) ->
    {error, invalid_subject}.

domain_lifetime_checked(ok, #{payload := Payload} = Record) ->
    payload_bounded({Payload, maps:get(subject, Record, <<>>)});
domain_lifetime_checked(Refusal, _Record) ->
    {error, Refusal}.

%%------------------------------------------------------------------
%% Internals: verifying
%%------------------------------------------------------------------

decoded_object({ok, #{key := _} = Object}, Profile, Now) -> checked_object(Object, Profile, Now);
decoded_object(_HeldOrMalformed, _Profile, _Now) -> {error, malformed}.

sized_object(Size, _Object, _Profile, _Now) when Size > ?MAX_RECORD_BYTES -> {error, record_too_large};
sized_object(_Size, Object, Profile, Now) -> checked_object(Object, Profile, Now).

checked_object(#{signature := Signature} = Object, Profile, Now) ->
    signed_fields(macula_signed_object:verify(?LABEL, Object, Profile), Signature, Profile, Now).

signed_fields({ok, #{key := Key, tbs := Tbs, fields := Fields}}, Signature, Profile, Now) ->
    read_record(read_tbs(Fields), Key, Tbs, Signature, Profile, Now);
signed_fields({error, _} = Refused, _Signature, _Profile, _Now) ->
    Refused.

read_tbs(#{{text, <<"type">>} := Type, {text, <<"alg">>} := {text, Alg}, {text, <<"version">>} := Version,
           {text, <<"created_at">>} := Created, {text, <<"expires_at">>} := Expires,
           {text, <<"payload">>} := Payload} = Fields)
  when is_integer(Type), Type >= 1, Type =< 16#FF, is_binary(Alg), is_binary(Version), byte_size(Version) =:= 16,
       is_integer(Created), Created >= 0, Created < ?MAX_PROTOCOL_INT, is_integer(Expires), Expires >= 0,
       Expires < ?MAX_PROTOCOL_INT, is_map(Payload) ->
    Record = #{type => Type, alg => Alg, version => Version, created_at => Created, expires_at => Expires,
               payload => Payload},
    with_read_subject(map_size(Fields), maps:get({text, <<"subject">>}, Fields, undefined), Record);
read_tbs(_Fields) ->
    malformed.

with_read_subject(6, undefined, Record) ->
    {ok, Record};
with_read_subject(7, Subject, #{type := Type} = Record)
  when is_binary(Subject), byte_size(Subject) > 0, Type >= ?DOMAIN_TYPE_MIN ->
    {ok, Record#{subject => Subject}};
with_read_subject(_Size, _Subject, _Record) ->
    malformed.

read_record(malformed, _Key, _Tbs, _Signature, _Profile, _Now) ->
    {error, malformed};
read_record({ok, Record}, Key, Tbs, Signature, Profile, Now) ->
    clocked(clock(Record, Now), Record#{key => Key, tbs => Tbs, signature => Signature}, Profile).

clock(#{created_at := Created}, Now) when Created > Now + ?CLOCK_TOLERANCE_MS -> not_yet_valid;
clock(#{expires_at := Expires}, Now) when Expires + ?CLOCK_TOLERANCE_MS < Now -> expired;
clock(_Record, _Now) -> ok.

clocked(ok, Record, Profile) ->
    lived(lifetime(Record), Record, Profile);
clocked(Refusal, _Record, _Profile) ->
    {error, Refusal}.

lived(ok, #{type := Type, payload := Payload} = Record, Profile) ->
    payload_checked(payload_ok(Type, Payload), Record, Profile);
lived(Refusal, _Record, _Profile) ->
    {error, Refusal}.

payload_checked(false, _Record, _Profile) ->
    {error, malformed};
payload_checked(true, #{type := Type, payload := Payload, key := Key} = Record, Profile) ->
    KeyId = key_id_of(signer_kind(Type, Payload), Key, Profile),
    named(named_signer(Type, Payload, KeyId), Record#{key_id => KeyId}).

named(true, Record) -> {ok, Record};
named(false, _Record) -> {error, key_id_mismatch}.

%% The payload rules of each type: every field a storage key or a signer check reads is present, with its type, and
%% the payloads the design pins hold exactly their keys. A domain type's owner sets its rules.
payload_ok(?TYPE_NODE_RECORD, P) -> is_id(field(P, <<"node_id">>));
payload_ok(?TYPE_REALM_DIRECTORY, P) -> is_id(field(P, <<"realm_id">>));
payload_ok(?TYPE_REALM_STATIONS, P) -> is_id(field(P, <<"realm_id">>));
payload_ok(?TYPE_REALM_MEMBER_ENDORSEMENT, P) ->
    is_id(field(P, <<"realm_id">>)) andalso is_id(field(P, <<"member_node">>));
payload_ok(?TYPE_PROCEDURE_ADVERTISEMENT, P) -> advertisement_payload_ok(P);
payload_ok(?TYPE_TOMBSTONE, P) -> tombstone_payload_ok(P);
payload_ok(?TYPE_FOUNDATION_SEED_LIST, _P) -> true;
payload_ok(?TYPE_FOUNDATION_PARAMETER, P) -> is_text(field(P, <<"param_name">>));
payload_ok(?TYPE_FOUNDATION_REALM_TRUST_LIST, P) -> trust_list_payload_ok(P);
payload_ok(?TYPE_FOUNDATION_T3_ATTESTATION, P) -> is_id(field(P, <<"station_id">>));
payload_ok(?TYPE_CONTENT_ANNOUNCEMENT, P) ->
    is_id(field(P, <<"announcer_node">>)) andalso is_content_id(field(P, <<"mcid">>))
        andalso is_id(field(P, <<"realm_id">>)) andalso is_id(field(P, <<"serving_station">>))
        andalso is_text_field(field(P, <<"procedure">>));
payload_ok(?TYPE_STATION_ENDPOINT, _P) -> true;
payload_ok(?TYPE_ORG_DIRECTORY, P) ->
    is_id(field(P, <<"realm_id">>)) andalso is_text(field(P, <<"org_name">>)) andalso is_id(field(P, <<"org_key">>));
payload_ok(?TYPE_PROCEDURE_DELEGATION, P) -> is_id(field(P, <<"org_key">>)) andalso is_id(field(P, <<"advertiser">>));
payload_ok(Type, _P) when Type >= ?DOMAIN_TYPE_MIN -> true;
payload_ok(_UnknownType, _P) -> false.

advertisement_payload_ok(#{{text, <<"realm_id">>} := <<_:256>>, {text, <<"procedure">>} := {text, Procedure},
                           {text, <<"advertiser_node">>} := <<_:256>>, {text, <<"serving_station">>} := <<_:256>>} = P)
  when is_binary(Procedure) ->
    advertisement_size_ok(map_size(P), maps:get({text, <<"authorization">>}, P, absent), kem_key_pair(P));
advertisement_payload_ok(_P) ->
    false.

%% The four fields every advertisement carries, then an authorization map and a KEM key pair, each present or not.
advertisement_size_ok(Size, Authorization, KemKeyPair) when KemKeyPair =/= malformed ->
    Size =:= 4 + authorization_fields(Authorization) + KemKeyPair andalso authorization_ok(Authorization);
advertisement_size_ok(_Size, _Authorization, malformed) ->
    false.

authorization_fields(absent) -> 0;
authorization_fields(_Authorization) -> 1.

authorization_ok(absent) -> true;
authorization_ok(Authorization) -> is_map(Authorization).

%% A provider's KEM key and its id travel only as a pair (E2E design, amendment A1): the key as carried, 1568 bytes
%% (ML-KEM-1024) or 1665 (with a P-384 point), and the first 8 bytes of SHA-384 over it. The two fields count 2, their
%% absence 0; a lone field, a key of another length or an id that is not its key's is malformed.
kem_key_pair(#{{text, <<"kem_key">>} := KemKey, {text, <<"kem_key_id">>} := KemKeyId})
  when is_binary(KemKey), (byte_size(KemKey) =:= 1568 orelse byte_size(KemKey) =:= 1665), is_binary(KemKeyId) ->
    kem_key_matched(kem_key_id(KemKey) =:= KemKeyId);
kem_key_pair(P) ->
    kem_key_absent(maps:is_key({text, <<"kem_key">>}, P) orelse maps:is_key({text, <<"kem_key_id">>}, P)).

kem_key_matched(true) -> 2;
kem_key_matched(false) -> malformed.

kem_key_absent(false) -> 0;
kem_key_absent(true) -> malformed.

kem_key_id(KemKey) ->
    binary:part(crypto:hash(sha384, KemKey), 0, 8).

%% The foundation realm trust list payload holds exactly realms_trusted, an array of maps, each with exactly a
%% 32-byte realm_id and a 32-byte realm_key_id (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, D28).
trust_list_payload_ok(#{{text, <<"realms_trusted">>} := Trusted} = P)
  when map_size(P) =:= 1, is_list(Trusted) ->
    lists:all(fun trust_list_entry_ok/1, Trusted);
trust_list_payload_ok(_P) ->
    false.

trust_list_entry_ok(#{{text, <<"realm_id">>} := RealmId, {text, <<"realm_key_id">>} := RealmKeyId} = E)
  when map_size(E) =:= 2, is_binary(RealmId), byte_size(RealmId) =:= 32,
       is_binary(RealmKeyId), byte_size(RealmKeyId) =:= 32 ->
    true;
trust_list_entry_ok(_Entry) ->
    false.

tombstone_payload_ok(#{{text, <<"withdrawn_type">>} := Type, {text, <<"withdrawn_version">>} := <<_:128>>,
                       {text, <<"reason">>} := {text, Reason}} = P) when is_integer(Type), is_binary(Reason) ->
    Slot = maps:without([{text, <<"withdrawn_type">>}, {text, <<"withdrawn_version">>}, {text, <<"reason">>},
                         {text, <<"detail">>}], P),
    lists:member(Reason, [<<"shutdown">>, <<"moved">>, <<"revoked">>])
        andalso withdrawable(Type)
        andalso detail_ok(maps:get({text, <<"detail">>}, P, absent))
        andalso slot_ok(Type, Slot);
tombstone_payload_ok(_P) ->
    false.

%% A tombstone withdraws a record type, a tag from 1 to 255: a domain type, or a built-in type some key signs, other
%% than a tombstone. Any other integer names no record type.
withdrawable(Type) when Type >= ?DOMAIN_TYPE_MIN, Type =< 16#FF ->
    true;
withdrawable(Type) when Type >= 1, Type < ?DOMAIN_TYPE_MIN ->
    Type =/= ?TYPE_TOMBSTONE andalso signer_purposes(Type, #{}) =/= [];
withdrawable(_OutsideTheTypeRange) ->
    false.

detail_ok(absent) -> true;
detail_ok({text, Detail}) when is_binary(Detail) -> true;
detail_ok(_Other) -> false.

%% A domain record's subject is a non-empty binary, in the record and in its tombstone's slot fields alike.
non_empty_subject(Subject) -> is_binary(Subject) andalso byte_size(Subject) > 0.

slot_ok(Type, Slot) when Type >= ?DOMAIN_TYPE_MIN ->
    map_size(Slot) =:= 0
        orelse (map_size(Slot) =:= 1 andalso non_empty_subject(maps:get({text, <<"subject">>}, Slot, none)));
slot_ok(Type, Slot) ->
    Names = slot_field_names(Type, undefined),
    lists:sort(maps:keys(Slot)) =:= lists:sort([{text, Name} || Name <- Names])
        andalso lists:all(fun(Name) -> slot_value_ok(Name, maps:get({text, Name}, Slot)) end, Names).

slot_value_ok(Name, Value) when Name =:= <<"procedure">>; Name =:= <<"param_name">>; Name =:= <<"org_name">> ->
    is_text(Value);
slot_value_ok(<<"mcid">>, Value) ->
    is_content_id(Value);
slot_value_ok(_IdName, Value) ->
    is_id(Value).

field(Payload, Name) -> maps:get({text, Name}, Payload, undefined).

is_text_field({text, Bin}) when is_binary(Bin), Bin =/= <<>> -> true;
is_text_field(_Other) -> false.

is_id(<<_:256>>) -> true;
is_id(_Other) -> false.

is_text({text, Text}) when is_binary(Text) -> true;
is_text(_Other) -> false.

is_content_id(<<2, _Codec:8, _Hash:48/binary>>) -> true;
is_content_id(_Other) -> false.

%%------------------------------------------------------------------
%% Internals: slots and storage keys
%%------------------------------------------------------------------

%% The payload fields a type's storage key derives from, other than the signer's key id.
slot_field_names(?TYPE_REALM_DIRECTORY, _Subject) -> [<<"realm_id">>];
slot_field_names(?TYPE_REALM_STATIONS, _Subject) -> [<<"realm_id">>];
slot_field_names(?TYPE_REALM_MEMBER_ENDORSEMENT, _Subject) -> [<<"realm_id">>, <<"member_node">>];
slot_field_names(?TYPE_PROCEDURE_ADVERTISEMENT, _Subject) -> [<<"realm_id">>, <<"procedure">>];
slot_field_names(?TYPE_FOUNDATION_PARAMETER, _Subject) -> [<<"param_name">>];
slot_field_names(?TYPE_FOUNDATION_T3_ATTESTATION, _Subject) -> [<<"station_id">>];
slot_field_names(?TYPE_CONTENT_ANNOUNCEMENT, _Subject) -> [<<"mcid">>];
slot_field_names(?TYPE_ORG_DIRECTORY, _Subject) -> [<<"realm_id">>, <<"org_name">>];
slot_field_names(?TYPE_PROCEDURE_DELEGATION, _Subject) -> [<<"advertiser">>];
slot_field_names(Type, Subject) when Type >= ?DOMAIN_TYPE_MIN, is_binary(Subject) -> [<<"subject">>];
slot_field_names(_SignerSlot, _Subject) -> [].

slot_fields(Names, Payload, Subject) ->
    maps:from_list([{{text, Name}, slot_value(Name, Payload, Subject)} || Name <- Names]).

slot_value(<<"subject">>, _Payload, Subject) -> Subject;
slot_value(Name, Payload, _Subject) -> maps:get({text, Name}, Payload).

slot_field_atoms() ->
    [{<<"realm_id">>, realm_id}, {<<"member_node">>, member_node}, {<<"procedure">>, procedure},
     {<<"param_name">>, param_name}, {<<"station_id">>, station_id}, {<<"mcid">>, mcid},
     {<<"org_name">>, org_name}, {<<"advertiser">>, advertiser}, {<<"subject">>, subject}].

slot(?TYPE_NODE_RECORD, _P, _Subject, <<_:256>> = KeyId) ->
    KeyId;
slot(?TYPE_REALM_DIRECTORY, P, _Subject, _KeyId) ->
    derived(?TYPE_REALM_DIRECTORY, [field(P, <<"realm_id">>)]);
slot(?TYPE_REALM_STATIONS, P, _Subject, _KeyId) ->
    derived(?TYPE_REALM_STATIONS, [field(P, <<"realm_id">>)]);
slot(?TYPE_REALM_MEMBER_ENDORSEMENT, P, _Subject, _KeyId) ->
    derived(?TYPE_REALM_MEMBER_ENDORSEMENT, [field(P, <<"realm_id">>), field(P, <<"member_node">>)]);
slot(?TYPE_PROCEDURE_ADVERTISEMENT, P, _Subject, _KeyId) ->
    procedure_key(field(P, <<"realm_id">>), text(field(P, <<"procedure">>)));
slot(?TYPE_FOUNDATION_SEED_LIST, _P, _Subject, <<_:256>> = KeyId) ->
    derived(?TYPE_FOUNDATION_SEED_LIST, [KeyId]);
slot(?TYPE_FOUNDATION_PARAMETER, P, _Subject, <<_:256>> = KeyId) ->
    derived(?TYPE_FOUNDATION_PARAMETER, [KeyId, {field, text(field(P, <<"param_name">>))}]);
slot(?TYPE_FOUNDATION_REALM_TRUST_LIST, _P, _Subject, <<_:256>> = KeyId) ->
    derived(?TYPE_FOUNDATION_REALM_TRUST_LIST, [KeyId]);
slot(?TYPE_FOUNDATION_T3_ATTESTATION, P, _Subject, _KeyId) ->
    derived(?TYPE_FOUNDATION_T3_ATTESTATION, [field(P, <<"station_id">>)]);
slot(?TYPE_CONTENT_ANNOUNCEMENT, P, _Subject, _KeyId) ->
    content_key(field(P, <<"mcid">>));
slot(?TYPE_STATION_ENDPOINT, _P, _Subject, <<_:256>> = KeyId) ->
    station_endpoint_key(KeyId);
slot(?TYPE_ORG_DIRECTORY, P, _Subject, _KeyId) ->
    org_directory_key(field(P, <<"realm_id">>), text(field(P, <<"org_name">>)));
slot(?TYPE_PROCEDURE_DELEGATION, P, _Subject, <<_:256>> = KeyId) ->
    procedure_delegation_key(KeyId, field(P, <<"advertiser">>));
slot(Type, _P, undefined, <<_:256>> = KeyId) when Type >= ?DOMAIN_TYPE_MIN ->
    derived(Type, [KeyId]);
slot(Type, _P, Subject, <<_:256>> = KeyId) when Type >= ?DOMAIN_TYPE_MIN, is_binary(Subject) ->
    derived(Type, [KeyId, {field, Subject}]).

%% SHA-256 over the storage key label, a zero byte, the type and the fields: a 32-byte id as it is, any other field as
%% a 4-byte big-endian length and its bytes.
derived(Type, Fields) ->
    crypto:hash(sha256, [<<?STORAGE_KEY_LABEL, 0:8, Type:8>> | [field_bytes(Field) || Field <- Fields]]).

field_bytes(<<_:256>> = Id) -> Id;
field_bytes({field, Bytes}) when is_binary(Bytes) -> <<(byte_size(Bytes)):32, Bytes/binary>>.

text({text, Text}) -> Text.

%%------------------------------------------------------------------
%% Internals: provider authorization
%%------------------------------------------------------------------

org_of([_NameWithoutSlash]) -> none;
org_of([<<>>, _Rest]) -> {error, malformed};
org_of([<<"_">>, _Rest]) -> none;
org_of([Org, _Rest]) -> {org, Org}.

%% An authorization holds exactly org_directory and procedure_delegation, both bytes. That pair with a value that is not
%% bytes, or an authorization that is not a map, is malformed; any other map is a form 11.0.0 does not accept.
read_authorization(undefined) ->
    undefined;
read_authorization(#{{text, <<"org_directory">>} := Directory, {text, <<"procedure_delegation">>} := Delegation} = A)
  when map_size(A) =:= 2, is_binary(Directory), is_binary(Delegation) ->
    #{org_directory => Directory, procedure_delegation => Delegation};
read_authorization(#{{text, <<"org_directory">>} := _, {text, <<"procedure_delegation">>} := _} = A)
  when map_size(A) =:= 2 ->
    malformed;
read_authorization(A) when is_map(A) ->
    unsupported;
read_authorization(_NotAMap) ->
    malformed.

%% The builder writes only the delegation form.
%% The provider's KEM key and its id, which sign/2 checks as a pair like every verifier.
with_kem_key(Payload, undefined) ->
    Payload;
with_kem_key(Payload, KemKey) when is_binary(KemKey) ->
    Payload#{{text, <<"kem_key">>} => KemKey, {text, <<"kem_key_id">>} => kem_key_id(KemKey)}.

with_authorization(Payload, undefined) ->
    Payload;
with_authorization(Payload, #{org_directory := Directory, procedure_delegation := Delegation} = Authorization)
  when map_size(Authorization) =:= 2, is_binary(Directory), is_binary(Delegation) ->
    Payload#{{text, <<"authorization">>} => #{{text, <<"org_directory">>} => Directory,
                                              {text, <<"procedure_delegation">>} => Delegation}}.

authorization_for({error, malformed}, _Authorization, _Adv, _Trust, _Now) ->
    {error, malformed};
%% A node's own namespace is authorized by the advertisement's signature alone: no realm key is needed, since no
%% realm or org signs for it.
authorization_for({org, <<"~", _/binary>>}, _Authorization, Adv, _Trust, _Now) ->
    own_namespace(Adv);
authorization_for(none, undefined, _Adv, _Trust, _Now) ->
    ok;
authorization_for(none, _Present, _Adv, _Trust, _Now) ->
    {error, authorization_not_allowed};
authorization_for({org, _Org}, undefined, _Adv, _Trust, _Now) ->
    {error, no_authorization};
authorization_for({org, Org}, #{org_directory := Directory, procedure_delegation := Delegation}, Adv, Trust, Now)
  when is_binary(Directory), is_binary(Delegation) ->
    #{realm_id := RealmId} = read_procedure_advertisement(Adv),
    delegation_path(realm_trust_key(RealmId, Trust), Directory, Delegation, Org, Adv, Trust, Now);
authorization_for({org, _Org}, unsupported, _Adv, _Trust, _Now) ->
    {error, authorization_form_unsupported};
authorization_for({org, _Org}, _Malformed, _Adv, _Trust, _Now) ->
    {error, malformed}.

%% The realm key the trust holds for the advertisement's realm: a pinned carried key when the caller pins one, the
%% realm key id the foundation realm trust list's pairs name, or none. The carried key is compared as carried; the
%% pair is a key id and is compared as the org directory's signer key id (D28).
realm_trust_key(_RealmId, #{realm_key := RealmKey}) when is_binary(RealmKey) ->
    {carried, RealmKey};
realm_trust_key(RealmId, #{realm_pairs := Pairs}) ->
    case maps:find(RealmId, Pairs) of
        {ok, RealmKeyId} -> {key_id, RealmKeyId};
        error            -> none
    end;
realm_trust_key(_RealmId, _Trust) ->
    none.

delegation_path(none, _Directory, _Delegation, _Org, _Adv, _Trust, _Now) ->
    {error, no_realm_key};
delegation_path(Expected, Directory, Delegation, Org, Adv, #{profile := Profile}, Now) ->
    org_directory_read(verify(Directory, Profile, Now), Expected, Delegation, Org, Adv, Profile, Now).

org_directory_read({ok, #{type := ?TYPE_ORG_DIRECTORY, key := DirectoryKey, key_id := DirectoryKeyId} = Dir},
                   Expected, Delegation, Org, Adv, Profile, Now) ->
    #{realm_id := RealmId, org_name := OrgName, org_key := OrgKeyId} = read_org_directory(Dir),
    #{realm_id := AdvRealmId} = read_procedure_advertisement(Adv),
    org_directory_matched(directory_signer_matches(Expected, DirectoryKey, DirectoryKeyId)
                              andalso RealmId =:= AdvRealmId,
                          OrgName =:= Org, OrgKeyId, Dir, Delegation, Adv, Profile, Now);
org_directory_read(_Refused, _Expected, _Delegation, _Org, _Adv, _Profile, _Now) ->
    {error, org_directory_invalid}.

directory_signer_matches({carried, RealmKey}, DirectoryKey, _DirectoryKeyId) ->
    DirectoryKey =:= RealmKey;
directory_signer_matches({key_id, RealmKeyId}, _DirectoryKey, DirectoryKeyId) ->
    DirectoryKeyId =:= RealmKeyId.

org_directory_matched(false, _SameOrg, _OrgKeyId, _Dir, _Delegation, _Adv, _Profile, _Now) ->
    {error, org_directory_wrong_realm};
org_directory_matched(true, false, _OrgKeyId, _Dir, _Delegation, _Adv, _Profile, _Now) ->
    {error, org_directory_wrong_org};
org_directory_matched(true, true, OrgKeyId, Dir, Delegation, Adv, Profile, Now) ->
    delegation_read(verify(Delegation, Profile, Now), OrgKeyId, Dir, Adv).

delegation_read({ok, #{type := ?TYPE_PROCEDURE_DELEGATION, key_id := DelegationKeyId} = Del}, OrgKeyId, Dir, Adv) ->
    #{advertiser := Advertiser} = read_procedure_delegation(Del),
    #{advertiser_node := AdvertiserNode} = read_procedure_advertisement(Adv),
    delegation_matched(DelegationKeyId =:= OrgKeyId andalso Advertiser =:= AdvertiserNode,
                       min(expires_at(Dir), expires_at(Del)), Adv);
delegation_read(_Refused, _OrgKeyId, _Dir, _Adv) ->
    {error, delegation_invalid}.

delegation_matched(false, _Earliest, _Adv) -> {error, delegation_mismatch};
delegation_matched(true, Earliest, Adv) -> within(expires_at(Adv) =< Earliest).

within(true) -> ok;
within(false) -> {error, authorization_outlived}.

%%------------------------------------------------------------------
%% Internals: payloads
%%------------------------------------------------------------------

node_payload(NodeId, StationId, Realms, Capabilities, Opts) ->
    Base = #{{text, <<"node_id">>}      => NodeId,
             {text, <<"station_id">>}   => StationId,
             {text, <<"realms">>}       => Realms,
             {text, <<"capabilities">>} => Capabilities},
    M1 = with_text(Base, <<"caps_hint">>, maps:get(caps_hint, Opts, undefined)),
    M2 = with_text(M1, <<"display_name">>, maps:get(display_name, Opts, undefined)),
    M3 = with_text(M2, <<"hostname">>, maps:get(hostname, Opts, undefined)),
    M4 = with_text(M3, <<"endpoint">>, maps:get(endpoint, Opts, undefined)),
    M5 = with_text(M4, <<"city">>, maps:get(city, Opts, undefined)),
    M6 = with_text(M5, <<"country">>, maps:get(country, Opts, undefined)),
    M7 = with_geo(M6, <<"lat">>, lat, maps:get(lat, Opts, undefined), ?LAT_BOUND),
    M8 = with_geo(M7, <<"lng">>, lng, maps:get(lng, Opts, undefined), ?LNG_BOUND),
    M9 = with_peers(M8, maps:get(peers, Opts, undefined)),
    with_text(M9, <<"kind">>, maps:get(kind, Opts, undefined)).

with_text(Map, _Key, undefined) -> Map;
with_text(Map, Key, Bin) when is_binary(Bin) -> Map#{{text, Key} => {text, Bin}}.

%% Coordinates travel as text: a fixed-decimals rendering is stable across stacks, unlike float encodings. A coordinate
%% is a number within Bound of zero either way; any other value is refused by name, so the builder writes only text a
%% reader takes.
with_geo(Map, _Key, _Field, undefined, _Bound) ->
    Map;
with_geo(Map, Key, Field, V, Bound) ->
    ok = coordinate_checked(is_number(V) andalso abs(V) =< Bound, Field),
    Map#{{text, Key} => {text, geo_text(V)}}.

coordinate_checked(true, _Field) -> ok;
coordinate_checked(false, Field) -> {error, {invalid_coordinate, Field}}.

geo_text(V) when is_float(V) -> float_to_binary(V, [{decimals, 6}, compact]);
geo_text(V) when is_integer(V) -> integer_to_binary(V).

%% Sorted, so the same set of peers always encodes the same way.
with_peers(Map, undefined) -> Map;
with_peers(Map, Peers) when is_list(Peers) ->
    sorted_peers(Map, lists:usort([Peer || Peer <- Peers, is_binary(Peer), byte_size(Peer) =:= 32])).

sorted_peers(Map, []) -> Map;
sorted_peers(Map, Sorted) -> Map#{{text, <<"peers">>} => Sorted}.

with_host_list(Map, undefined) -> Map;
with_host_list(Map, Hosts) when is_list(Hosts) ->
    host_binaries(Map, [Host || Host <- Hosts, is_binary(Host)]).

host_binaries(Map, []) -> Map;
host_binaries(Map, Hosts) -> Map#{{text, <<"host_advertised">>} => Hosts}.

realm_directory_payload(RealmId, Name, AdminKeyId, Opts) ->
    Base = #{{text, <<"realm_id">>}   => RealmId,
             {text, <<"name">>}       => {text, Name},
             {text, <<"admin_key">>}  => AdminKeyId,
             {text, <<"created_at">>} => erlang:system_time(millisecond)},
    with_text(Base, <<"policy_url">>, maps:get(policy_url, Opts, undefined)).

realm_stations_payload(RealmId, Entries) ->
    #{{text, <<"realm_id">>} => RealmId,
      {text, <<"stations">>} => [realm_station_entry(Entry) || Entry <- Entries]}.

realm_station_entry(#{station_id := StationId, roles := Roles})
  when is_binary(StationId), byte_size(StationId) =:= 32, is_list(Roles) ->
    #{{text, <<"station_id">>} => StationId,
      {text, <<"roles">>}      => [{text, Role} || Role <- Roles, is_binary(Role)]}.

realm_member_endorsement_payload(RealmId, Member, Roles, ValidFrom, ValidUntil) ->
    #{{text, <<"realm_id">>}    => RealmId,
      {text, <<"member_node">>} => Member,
      {text, <<"roles">>}       => [{text, Role} || Role <- Roles, is_binary(Role)],
      {text, <<"valid_from">>}  => ValidFrom,
      {text, <<"valid_until">>} => ValidUntil}.

with_uint(Map, _Key, undefined) -> Map;
with_uint(Map, Key, N) when is_integer(N), N >= 0 -> Map#{{text, Key} => N}.

content_announcement_payload(AnnouncerNode, MCID, Opts) ->
    Base = #{{text, <<"announcer_node">>} => AnnouncerNode,
             {text, <<"mcid">>}           => MCID},
    M1 = with_text(Base, <<"name">>, maps:get(name, Opts, undefined)),
    M2 = with_uint(M1, <<"size">>, maps:get(size, Opts, undefined)),
    M3 = with_uint(M2, <<"chunk_count">>, maps:get(chunk_count, Opts, undefined)),
    %% D27: where the content is served, the realm and the sharer's own
    %% procedure a STREAM_OPEN routes by, and the station it is reachable
    %% through now, which a fetcher resolves to that station's own endpoint.
    M4 = with_id(M3, <<"realm_id">>, maps:get(realm_id, Opts, undefined)),
    M5 = with_id(M4, <<"serving_station">>, maps:get(serving_station, Opts, undefined)),
    with_text(M5, <<"procedure">>, maps:get(procedure, Opts, undefined)).

with_id(Map, _Key, undefined) -> Map;
with_id(Map, Key, <<_:256>> = Id) -> Map#{{text, Key} => Id}.

foundation_seed_list_payload(Version, ValidFrom, ValidUntil, Seeds) ->
    #{{text, <<"version">>}     => Version,
      {text, <<"valid_from">>}  => ValidFrom,
      {text, <<"valid_until">>} => ValidUntil,
      {text, <<"seeds">>}       => [foundation_seed_entry(Seed) || Seed <- Seeds]}.

foundation_seed_entry(#{node_id := NodeId, addresses := Addresses, tier := Tier})
  when is_binary(NodeId), byte_size(NodeId) =:= 32, is_list(Addresses), (Tier =:= 3 orelse Tier =:= 4) ->
    #{{text, <<"node_id">>}   => NodeId,
      {text, <<"addresses">>} => [Address || #{} = Address <- Addresses],
      {text, <<"tier">>}      => Tier}.

foundation_parameter_payload(Name, Value, Version, ValidFrom, ValidUntil, PriorVersion) ->
    Base = #{{text, <<"param_name">>}  => {text, Name},
             {text, <<"param_value">>} => parameter_value(Value),
             {text, <<"version">>}     => Version,
             {text, <<"valid_from">>}  => ValidFrom,
             {text, <<"valid_until">>} => ValidUntil},
    with_prior_version(Base, PriorVersion).

parameter_value(Value) when is_integer(Value) -> Value;
parameter_value(Value) when is_binary(Value) -> {text, Value};
parameter_value(Values) when is_list(Values) -> [parameter_value(Value) || Value <- Values].

with_prior_version(Map, undefined) -> Map;
with_prior_version(Map, <<_:128>> = Version) -> Map#{{text, <<"prior_version">>} => Version}.

foundation_t3_attestation_payload(StationId, AuditDate, ValidUntil, Notes) ->
    Base = #{{text, <<"station_id">>}    => StationId,
             {text, <<"tier_attested">>} => 3,
             {text, <<"audit_date">>}    => AuditDate,
             {text, <<"valid_until">>}   => ValidUntil},
    with_text(Base, <<"notes">>, Notes).

%%------------------------------------------------------------------
%% Internals: readers
%%------------------------------------------------------------------

safe_atom(Name) ->
    try binary_to_existing_atom(Name, utf8)
    catch _:_ -> undefined
    end.

first_present([Key | Keys], Payload) ->
    present(maps:find(Key, Payload), Keys, Payload);
first_present([], _Payload) ->
    undefined.

present({ok, Value}, _Keys, _Payload) -> Value;
present(error, Keys, Payload) -> first_present(Keys, Payload).

%% A text value is returned as its binary. An atom value, other than true, false, undefined and null, is returned as
%% its binary too, because a decode path may have turned a known text value into an existing atom.
unwrap_text({text, Bin}) -> Bin;
unwrap_text(Atom) when is_atom(Atom), Atom =/= true, Atom =/= false, Atom =/= undefined, Atom =/= null ->
    atom_to_binary(Atom, utf8);
unwrap_text(Value) -> Value.

host_list(undefined) -> [];
host_list(Hosts) when is_list(Hosts) -> [unwrap_text(Host) || Host <- Hosts];
host_list(Host) -> [unwrap_text(Host)].

%% A coordinate is text as both builders write it: an optional leading minus, digits, then optionally a dot and
%% digits, in at most 32 bytes, within Bound of zero either way. Any other value, text or not, reads as no coordinate,
%% so reading a verified record never raises on a coordinate its signer chose.
parse_geo({text, Text}, Bound) when is_binary(Text), byte_size(Text) =< ?MAX_GEO_TEXT_BYTES ->
    within_bound(geo_number(geo_shape(Text), Text), Bound);
parse_geo(_NotACoordinate, _Bound) ->
    undefined.

%% The shape of coordinate text: integer or decimal when it is exactly the grammar, and none otherwise.
geo_shape(<<"-", Unsigned/binary>>) -> unsigned_shape(Unsigned);
geo_shape(Unsigned) -> unsigned_shape(Unsigned).

unsigned_shape(Text) -> parts_shape(binary:split(Text, <<".">>)).

parts_shape([Whole]) -> shape_if(all_digits(Whole), integer);
parts_shape([Whole, Fraction]) -> shape_if(all_digits(Whole) andalso all_digits(Fraction), decimal).

shape_if(true, Shape) -> Shape;
shape_if(false, _Shape) -> none.

all_digits(<<>>) -> false;
all_digits(Bytes) -> lists:all(fun(Byte) -> Byte >= $0 andalso Byte =< $9 end, binary_to_list(Bytes)).

%% Text of the grammar's shape is exactly what binary_to_integer/1 and binary_to_float/1 take, so neither raises.
geo_number(integer, Text) -> binary_to_integer(Text);
geo_number(decimal, Text) -> binary_to_float(Text);
geo_number(none, _Text) -> undefined.

within_bound(Number, Bound) when is_number(Number), abs(Number) =< Bound -> Number;
within_bound(_OutOfBoundOrNone, _Bound) -> undefined.
