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

-include_lib("public_key/include/public_key.hrl").

-export([
    node_record/3, node_record/4,
    realm_directory/3, realm_directory/4,
    realm_stations/2, realm_stations/3,
    realm_member_endorsement/2, realm_member_endorsement/3, max_endorsement_window_ms/0,
    org_directory/3, org_directory/4,
    procedure_delegation/2, procedure_delegation/3,
    procedure_advertisement/4, procedure_advertisement/5,
    content_announcement/3, content_announcement/4,
    foundation_seed_list/1, foundation_seed_list/2,
    foundation_parameter/2, foundation_parameter/3,
    foundation_realm_trust_list/1, foundation_realm_trust_list/2,
    foundation_t3_attestation/2, foundation_t3_attestation/3,
    station_endpoint/1, station_endpoint/2,
    tombstone/2, tombstone/3,
    envelope/3
]).
-export([sign/2, verify/2, verify/3, refresh/2, encode/1, node_signed/1, payload_bounded/1, wire_bounded/1]).
-export([type/1, key/1, key_id/1, version/1, created_at/1, expires_at/1, payload/1, signature/1]).
-export([payload_field/2, type_procedure_advertisement/0]).
-export([read_node_record/1, read_procedure_advertisement/1, read_station_endpoint/1, read_tombstone/1,
         read_org_directory/1, read_procedure_delegation/1, read_content_announcement/1]).
-export([procedure_org/1, verify_authorization/3]).
-export([storage_key/1, procedure_key/2, content_key/1, station_endpoint_key/1, org_directory_key/2,
         procedure_delegation_key/2]).

-export_type([m_record/0, type_tag/0, version/0, refusal/0, reason/0, authorization/0, trust/0,
              authorization_refusal/0, node_record_opts/0, realm_directory_opts/0, realm_station_entry/0,
              realm_stations_opts/0, realm_member_endorsement_opts/0, procedure_advertisement_opts/0,
              content_announcement_opts/0, foundation_seed/0, foundation_seed_list_opts/0,
              foundation_parameter_value/0, foundation_parameter_opts/0, foundation_realm_trust_list_opts/0,
              foundation_t3_attestation_opts/0, tombstone_opts/0, station_endpoint_opts/0]).

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
-type authorization() :: #{org_directory := binary(), procedure_delegation := binary()}
                       | #{certificate_chain := [binary(), ...]}.
-type trust() :: #{profile := macula_crypto_profile:profile(), realm_key => binary(), realm_ca => binary()}.
-type authorization_refusal() :: malformed | no_authorization | authorization_not_allowed | no_realm_key
                               | no_realm_ca | org_directory_invalid | org_directory_wrong_realm
                               | org_directory_wrong_org | delegation_invalid | delegation_mismatch
                               | cert_chain_undecodable | cert_key_mismatch | cert_chain_untrusted
                               | cert_org_mismatch | authorization_outlived.

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
-type procedure_advertisement_opts() :: #{authorization => map(), ttl_ms => pos_integer()}.
-type content_announcement_opts() :: #{name => binary(), size => non_neg_integer(), chunk_count => non_neg_integer(),
                                       ttl_ms => pos_integer()}.
-type foundation_seed() :: #{node_id := <<_:256>>, addresses := [map()], tier := 3 | 4}.
-type foundation_seed_list_opts() :: #{valid_from => pos_integer(), valid_until => pos_integer(),
                                       ttl_ms => pos_integer()}.
-type foundation_parameter_value() :: integer() | binary() | [integer() | binary()].
-type foundation_parameter_opts() :: #{valid_from => pos_integer(), valid_until => pos_integer(),
                                       prior_version => version(), ttl_ms => pos_integer()}.
-type foundation_realm_trust_list_opts() :: #{realms_revoked => [<<_:256>>], valid_until => pos_integer(),
                                              ttl_ms => pos_integer()}.
-type foundation_t3_attestation_opts() :: #{valid_until => pos_integer(), notes => binary(), ttl_ms => pos_integer()}.
-type tombstone_opts() :: #{detail => binary(), ttl_ms => pos_integer()}.
-type station_endpoint_opts() :: #{host_advertised => [binary()], alpn => binary(), ttl_ms => pos_integer()}.

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(STORAGE_KEY_LABEL, "MACULA-PQ-STORAGE-KEY-V1").
-define(MAX_RECORD_BYTES, 256 * 1024).
%% The longest coordinate text a node record's reader parses: a finite coordinate needs far fewer bytes.
-define(MAX_GEO_TEXT_BYTES, 32).
-define(CLOCK_TOLERANCE_MS, 5 * 60 * 1000).
%% A protocol integer in a signed structure stays below 2^53 (the decoding rule).
-define(MAX_PROTOCOL_INT, 1 bsl 53).
-define(UNIX_EPOCH_GREGORIAN_SECONDS, 62167219200).

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
%% station endpoint 5 minutes; realm stations, an org directory and a procedure delegation 6 hours; a realm member
%% endorsement 30 days; a domain record 7 days (D28); and any other type 30 days, so no record keeps a key trusted
%% without end. A tombstone lives at most its withdrawn type's maximum plus twice the clock tolerance.
-define(NODE_RECORD_MAX_LIFETIME_MS, 48 * 60 * 60 * 1000).
-define(CONTENT_ANNOUNCEMENT_MAX_LIFETIME_MS, 48 * 60 * 60 * 1000).
-define(PROCEDURE_ADVERTISEMENT_MAX_LIFETIME_MS, 5 * 60 * 1000).
-define(REALM_AND_ORG_MAX_LIFETIME_MS, 6 * 60 * 60 * 1000).
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
%% the records' wire form, or certificate_chain as DER certificates, leaf first.
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
             with_authorization(Payload, maps:get(authorization, Opts, undefined)), Opts).

%% @doc A node's announcement, signed by the node, that it shares the content with this tag 2 content id.
-spec content_announcement(<<_:256>>, <<_:400>>, binary()) -> m_record().
content_announcement(AnnouncerNode, MCID, Endpoint) ->
    content_announcement(AnnouncerNode, MCID, Endpoint, #{}).

-spec content_announcement(<<_:256>>, <<_:400>>, binary(), content_announcement_opts()) -> m_record().
content_announcement(AnnouncerNode, <<2, _Codec:8, _Hash:48/binary>> = MCID, Endpoint, Opts)
  when is_binary(AnnouncerNode), byte_size(AnnouncerNode) =:= 32, is_binary(Endpoint) ->
    unsigned(?TYPE_CONTENT_ANNOUNCEMENT, content_announcement_payload(AnnouncerNode, MCID, Endpoint, Opts), Opts).

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

%% @doc A foundation's list of trusted realms, by realm key id, signed by a foundation key.
-spec foundation_realm_trust_list([<<_:256>>]) -> m_record().
foundation_realm_trust_list(Trusted) ->
    foundation_realm_trust_list(Trusted, #{}).

-spec foundation_realm_trust_list([<<_:256>>], foundation_realm_trust_list_opts()) -> m_record().
foundation_realm_trust_list(Trusted, Opts) when is_list(Trusted), is_map(Opts) ->
    Now = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    #{version := Version} = Unsigned = unsigned(?TYPE_FOUNDATION_REALM_TRUST_LIST, #{}, Opts),
    Payload = foundation_realm_trust_list_payload(Trusted, maps:get(realms_revoked, Opts, []), Version,
                                                  maps:get(valid_until, Opts, Now + TtlMs)),
    Unsigned#{payload := Payload}.

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

%% @doc An unsigned record of a domain type (tags 0x20 to 0xFF). The subject_id option names the record's subject.
-spec envelope(type_tag(), map(), map()) -> m_record().
envelope(Type, Payload, Opts)
  when is_integer(Type), Type >= ?DOMAIN_TYPE_MIN, Type =< 16#FF, is_map(Payload), is_map(Opts) ->
    with_subject(unsigned(Type, Payload, Opts), maps:get(subject_id, Opts, undefined)).

%%------------------------------------------------------------------
%% Signing, verifying and the wire form
%%------------------------------------------------------------------

%% @doc Sign a record with a key whose purpose fits its type. Raises key_purpose_mismatch for a key of another
%% purpose, key_id_mismatch when the payload names a signer other than this key, and record_too_large past 256 KiB.
-spec sign(m_record(), macula_node_keys:node_key()) -> m_record().
sign(#{type := Type, payload := Payload} = Record, #{purpose := Purpose, profile := Profile} = Key) ->
    ok = purpose_fits(lists:member(Purpose, signer_purposes(Type, Payload)), {Type, Purpose}),
    ok = lifetime_checked(lifetime(Record), Type),
    Carried = macula_node_keys:public_key(Key),
    KeyId = key_id_of(signer_kind(Type, Payload), Carried, Profile),
    ok = signer_matches(named_signer(Type, Payload, KeyId), Type),
    Fields = tbs_fields(Record),
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

%% @doc The record with a new version, created now, with the same lifetime, signed again with Key.
-spec refresh(m_record(), macula_node_keys:node_key()) -> m_record().
refresh(#{created_at := Created, expires_at := Expires} = Record, Key) ->
    Now = erlang:system_time(millisecond),
    Fresh = maps:with([type, payload, subject], Record),
    sign(Fresh#{version => macula_record_uuid:v7_monotonic(Now), created_at => Now,
                expires_at => Now + (Expires - Created)},
         Key).

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
      lat          => parse_geo(payload_field(P, <<"lat">>)),
      lng          => parse_geo(payload_field(P, <<"lng">>)),
      display_name => payload_field(P, <<"display_name">>),
      caps_hint    => payload_field(P, <<"caps_hint">>),
      peers        => payload_field(P, <<"peers">>),
      %% Stamped by a station's re-announce heartbeat: the station's own reported build.
      version      => payload_field(P, <<"version">>)}.

-spec read_procedure_advertisement(m_record()) -> map().
read_procedure_advertisement(#{type := ?TYPE_PROCEDURE_ADVERTISEMENT, payload := P}) ->
    #{realm_id        => payload_field(P, <<"realm_id">>),
      procedure       => payload_field(P, <<"procedure">>),
      advertiser_node => payload_field(P, <<"advertiser_node">>),
      serving_station => payload_field(P, <<"serving_station">>),
      authorization   => read_authorization(maps:get({text, <<"authorization">>}, P, undefined))}.

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
      endpoint       => payload_field(P, <<"endpoint">>),
      name           => payload_field(P, <<"name">>),
      size           => payload_field(P, <<"size">>),
      chunk_count    => payload_field(P, <<"chunk_count">>)}.

%%------------------------------------------------------------------
%% Provider authorization (D25 item 6)
%%------------------------------------------------------------------

%% @doc A procedure's org namespace: the text before the first "/" of its name, when there is one and it is not "_".
-spec procedure_org(binary()) -> {org, binary()} | none | {error, malformed}.
procedure_org(Procedure) when is_binary(Procedure) ->
    org_of(binary:split(Procedure, <<"/">>)).

%% @doc The caller's check of a verified advertisement's provider authorization, against the realm it trusts: the
%% realm key for the org directory and the delegation, the realm CA (PEM) for a certificate chain. A procedure with an
%% org namespace needs an authorization for that org, a procedure without one carries none, and the advertisement
%% expires no later than any part of its authorization.
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

%%------------------------------------------------------------------
%% Internals: signing
%%------------------------------------------------------------------

unsigned(Type, Payload, Opts) ->
    Now = erlang:system_time(millisecond),
    #{type => Type, version => macula_record_uuid:v7_monotonic(Now), created_at => Now,
      expires_at => Now + maps:get(ttl_ms, Opts, default_ttl(Type)), payload => Payload}.

with_subject(Record, undefined) -> Record;
with_subject(Record, Subject) when is_binary(Subject) -> Record#{subject => Subject}.

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
max_lifetime(Type, _Payload) when Type =:= ?TYPE_REALM_STATIONS; Type =:= ?TYPE_ORG_DIRECTORY;
                                  Type =:= ?TYPE_PROCEDURE_DELEGATION ->
    ?REALM_AND_ORG_MAX_LIFETIME_MS;
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
with_read_subject(7, Subject, #{type := Type} = Record) when is_binary(Subject), Type >= ?DOMAIN_TYPE_MIN ->
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
payload_ok(?TYPE_FOUNDATION_REALM_TRUST_LIST, _P) -> true;
payload_ok(?TYPE_FOUNDATION_T3_ATTESTATION, P) -> is_id(field(P, <<"station_id">>));
payload_ok(?TYPE_CONTENT_ANNOUNCEMENT, P) ->
    is_id(field(P, <<"announcer_node">>)) andalso is_content_id(field(P, <<"mcid">>));
payload_ok(?TYPE_STATION_ENDPOINT, _P) -> true;
payload_ok(?TYPE_ORG_DIRECTORY, P) ->
    is_id(field(P, <<"realm_id">>)) andalso is_text(field(P, <<"org_name">>)) andalso is_id(field(P, <<"org_key">>));
payload_ok(?TYPE_PROCEDURE_DELEGATION, P) -> is_id(field(P, <<"org_key">>)) andalso is_id(field(P, <<"advertiser">>));
payload_ok(Type, _P) when Type >= ?DOMAIN_TYPE_MIN -> true;
payload_ok(_UnknownType, _P) -> false.

advertisement_payload_ok(#{{text, <<"realm_id">>} := <<_:256>>, {text, <<"procedure">>} := {text, Procedure},
                           {text, <<"advertiser_node">>} := <<_:256>>, {text, <<"serving_station">>} := <<_:256>>} = P)
  when is_binary(Procedure) ->
    advertisement_size_ok(map_size(P), maps:get({text, <<"authorization">>}, P, absent));
advertisement_payload_ok(_P) ->
    false.

advertisement_size_ok(4, absent) -> true;
advertisement_size_ok(5, Authorization) when is_map(Authorization) -> true;
advertisement_size_ok(_Size, _Authorization) -> false.

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

withdrawable(Type) when Type >= ?DOMAIN_TYPE_MIN, Type =< 16#FF -> true;
withdrawable(Type) -> Type =/= ?TYPE_TOMBSTONE andalso signer_purposes(Type, #{}) =/= [].

detail_ok(absent) -> true;
detail_ok({text, Detail}) when is_binary(Detail) -> true;
detail_ok(_Other) -> false.

slot_ok(Type, Slot) when Type >= ?DOMAIN_TYPE_MIN ->
    map_size(Slot) =:= 0 orelse (map_size(Slot) =:= 1 andalso is_binary(maps:get({text, <<"subject">>}, Slot, none)));
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

read_authorization(undefined) ->
    undefined;
read_authorization(#{{text, <<"org_directory">>} := Directory, {text, <<"procedure_delegation">>} := Delegation} = A)
  when map_size(A) =:= 2 ->
    #{org_directory => Directory, procedure_delegation => Delegation};
read_authorization(#{{text, <<"certificate_chain">>} := Chain} = A) when map_size(A) =:= 1 ->
    #{certificate_chain => Chain};
read_authorization(_Other) ->
    malformed.

with_authorization(Payload, undefined) ->
    Payload;
with_authorization(Payload, Authorization) when is_map(Authorization) ->
    Payload#{{text, <<"authorization">>} => #{{text, atom_to_binary(Name)} => Value || Name := Value <- Authorization}}.

authorization_for({error, malformed}, _Authorization, _Adv, _Trust, _Now) ->
    {error, malformed};
authorization_for(none, undefined, _Adv, _Trust, _Now) ->
    ok;
authorization_for(none, _Present, _Adv, _Trust, _Now) ->
    {error, authorization_not_allowed};
authorization_for({org, _Org}, undefined, _Adv, _Trust, _Now) ->
    {error, no_authorization};
authorization_for({org, Org}, #{org_directory := Directory, procedure_delegation := Delegation}, Adv, Trust, Now)
  when is_binary(Directory), is_binary(Delegation) ->
    delegation_path(maps:get(realm_key, Trust, undefined), Directory, Delegation, Org, Adv, Trust, Now);
authorization_for({org, Org}, #{certificate_chain := [_ | _] = Chain}, Adv, Trust, _Now) ->
    certificate_path(maps:get(realm_ca, Trust, undefined), Chain, Org, Adv);
authorization_for({org, _Org}, _Other, _Adv, _Trust, _Now) ->
    {error, malformed}.

delegation_path(undefined, _Directory, _Delegation, _Org, _Adv, _Trust, _Now) ->
    {error, no_realm_key};
delegation_path(RealmKey, Directory, Delegation, Org, Adv, #{profile := Profile}, Now) ->
    org_directory_read(verify(Directory, Profile, Now), RealmKey, Delegation, Org, Adv, Profile, Now).

org_directory_read({ok, #{type := ?TYPE_ORG_DIRECTORY, key := DirectoryKey} = Dir}, RealmKey, Delegation, Org, Adv,
                   Profile, Now) ->
    #{realm_id := RealmId, org_name := OrgName, org_key := OrgKeyId} = read_org_directory(Dir),
    #{realm_id := AdvRealmId} = read_procedure_advertisement(Adv),
    org_directory_matched(DirectoryKey =:= RealmKey andalso RealmId =:= AdvRealmId, OrgName =:= Org, OrgKeyId, Dir,
                          Delegation, Adv, Profile, Now);
org_directory_read(_Refused, _RealmKey, _Delegation, _Org, _Adv, _Profile, _Now) ->
    {error, org_directory_invalid}.

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

certificate_path(undefined, _Chain, _Org, _Adv) ->
    {error, no_realm_ca};
certificate_path(RealmCaPem, Chain, Org, #{key := AdvKey} = Adv) ->
    chain_decoded(decode_chain(Chain), Chain, RealmCaPem, Org, AdvKey, Adv).

decode_chain(Ders) ->
    try [public_key:der_decode('Certificate', Der) || Der <- Ders] of
        Certificates -> {ok, Certificates}
    catch
        _:_ -> error
    end.

chain_decoded(error, _Chain, _RealmCaPem, _Org, _AdvKey, _Adv) ->
    {error, cert_chain_undecodable};
chain_decoded({ok, [Leaf | _] = Certificates}, Chain, RealmCaPem, Org, AdvKey, Adv) ->
    leaf_matched(leaf_key(Leaf) =:= {ok, AdvKey}, Certificates, Chain, RealmCaPem, Org, Adv).

leaf_matched(false, _Certificates, _Chain, _RealmCaPem, _Org, _Adv) ->
    {error, cert_key_mismatch};
leaf_matched(true, Certificates, [LeafDer | _] = Chain, RealmCaPem, Org, Adv) ->
    path_checked(validate_path(RealmCaPem, Chain), cert_org(LeafDer) =:= {ok, Org}, Certificates, Adv).

path_checked({error, _}, _SameOrg, _Certificates, _Adv) ->
    {error, cert_chain_untrusted};
path_checked(ok, false, _Certificates, _Adv) ->
    {error, cert_org_mismatch};
path_checked(ok, true, Certificates, Adv) ->
    within(expires_at(Adv) =< lists:min([not_after_ms(Certificate) || Certificate <- Certificates])).

%% The raw ML-DSA-87 key a leaf certifies. A composite key has no X.509 form yet (WP 3.1), so no leaf matches one.
leaf_key(#'Certificate'{tbsCertificate = #'TBSCertificate'{subjectPublicKeyInfo = Spki}}) ->
    spki_key(Spki).

spki_key(#'SubjectPublicKeyInfo'{algorithm = {'AlgorithmIdentifier', ?'id-ml-dsa-87', _}, subjectPublicKey = Key}) ->
    key_bytes(Key);
spki_key(_OtherAlgorithm) ->
    error.

key_bytes({0, Key}) when is_binary(Key) -> {ok, Key};
key_bytes(Key) when is_binary(Key) -> {ok, Key};
key_bytes(_Other) -> error.

not_after_ms(#'Certificate'{tbsCertificate = #'TBSCertificate'{validity = #'Validity'{notAfter = NotAfter}}}) ->
    time_ms(NotAfter).

time_ms({utcTime, [Y1, Y2 | Rest]}) -> gregorian_ms(utc_year(list_to_integer([Y1, Y2])), Rest);
time_ms({generalTime, [Y1, Y2, Y3, Y4 | Rest]}) -> gregorian_ms(list_to_integer([Y1, Y2, Y3, Y4]), Rest).

%% RFC 5280: a two-digit year of 50 or more is 19YY, below 50 is 20YY.
utc_year(Year) when Year >= 50 -> 1900 + Year;
utc_year(Year) -> 2000 + Year.

gregorian_ms(Year, [Mo1, Mo2, D1, D2, H1, H2, Mi1, Mi2, S1, S2, $Z]) ->
    Date = {Year, list_to_integer([Mo1, Mo2]), list_to_integer([D1, D2])},
    Time = {list_to_integer([H1, H2]), list_to_integer([Mi1, Mi2]), list_to_integer([S1, S2])},
    (calendar:datetime_to_gregorian_seconds({Date, Time}) - ?UNIX_EPOCH_GREGORIAN_SECONDS) * 1000.

%% Validate leaf -> ... -> realm CA. pkix_path_validation wants the chain from the anchor's direct child down to the
%% leaf, so the leaf-first chain is reversed.
validate_path(RealmCaPem, ChainDers) ->
    validate_path_anchor(realm_ca_der(RealmCaPem), ChainDers).

validate_path_anchor({ok, AnchorDer}, ChainDers) ->
    validate_path_result(public_key:pkix_path_validation(AnchorDer, lists:reverse(ChainDers), []));
validate_path_anchor({error, _} = Error, _ChainDers) ->
    Error.

validate_path_result({ok, _}) -> ok;
validate_path_result({error, Reason}) -> {error, {bad_cert, Reason}}.

realm_ca_der(Pem) when is_binary(Pem) ->
    realm_ca_der_result([Der || {'Certificate', Der, not_encrypted} <- public_key:pem_decode(Pem)]);
realm_ca_der(_Other) ->
    {error, no_realm_ca}.

realm_ca_der_result([Der | _]) -> {ok, Der};
realm_ca_der_result([]) -> {error, no_realm_ca}.

%% The organization (O) of a certificate's subject.
cert_org(Der) ->
    #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{subject = Subject}} = public_key:pkix_decode_cert(Der, otp),
    subject_org(Subject).

subject_org({rdnSequence, RDNs}) -> org_from_rdns(lists:append(RDNs));
subject_org(_Other) -> {error, no_subject}.

%% id-at-organizationName = OID {2,5,4,10}.
org_from_rdns([#'AttributeTypeAndValue'{type = {2, 5, 4, 10}, value = Value} | _]) -> {ok, rdn_string(Value)};
org_from_rdns([_ | Rest]) -> org_from_rdns(Rest);
org_from_rdns([]) -> {error, no_org_rdn}.

rdn_string({utf8String, String}) -> to_bin(String);
rdn_string({printableString, String}) -> to_bin(String);
rdn_string(String) when is_binary(String) -> String;
rdn_string(String) when is_list(String) -> list_to_binary(String).

to_bin(Bin) when is_binary(Bin) -> Bin;
to_bin(List) when is_list(List) -> unicode:characters_to_binary(List).

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
    M7 = with_geo(M6, <<"lat">>, maps:get(lat, Opts, undefined)),
    M8 = with_geo(M7, <<"lng">>, maps:get(lng, Opts, undefined)),
    M9 = with_peers(M8, maps:get(peers, Opts, undefined)),
    with_text(M9, <<"kind">>, maps:get(kind, Opts, undefined)).

with_text(Map, _Key, undefined) -> Map;
with_text(Map, Key, Bin) when is_binary(Bin) -> Map#{{text, Key} => {text, Bin}}.

%% Coordinates travel as text: a fixed-decimals rendering is stable across stacks, unlike float encodings.
with_geo(Map, _Key, undefined) -> Map;
with_geo(Map, Key, V) when is_float(V) -> Map#{{text, Key} => {text, float_to_binary(V, [{decimals, 6}, compact])}};
with_geo(Map, Key, V) when is_integer(V) -> Map#{{text, Key} => {text, integer_to_binary(V)}}.

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

content_announcement_payload(AnnouncerNode, MCID, Endpoint, Opts) ->
    Base = #{{text, <<"announcer_node">>} => AnnouncerNode,
             {text, <<"mcid">>}           => MCID,
             {text, <<"endpoint">>}       => {text, Endpoint}},
    M1 = with_text(Base, <<"name">>, maps:get(name, Opts, undefined)),
    M2 = with_uint(M1, <<"size">>, maps:get(size, Opts, undefined)),
    with_uint(M2, <<"chunk_count">>, maps:get(chunk_count, Opts, undefined)).

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

foundation_realm_trust_list_payload(Trusted, Revoked, Version, ValidUntil) ->
    #{{text, <<"realms_trusted">>} => [Realm || <<_:256>> = Realm <- Trusted],
      {text, <<"realms_revoked">>} => [Realm || <<_:256>> = Realm <- Revoked],
      {text, <<"version">>}        => Version,
      {text, <<"valid_until">>}    => ValidUntil}.

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

%% Coordinate text reads as the finite number it spells in full, a float or an integer, in at most 32 bytes. Any other
%% value, text or not, reads as no coordinate, so reading a verified record never raises on a coordinate its signer
%% chose.
parse_geo(Text) when is_binary(Text), byte_size(Text) =< ?MAX_GEO_TEXT_BYTES ->
    geo_float(string:to_float(Text), Text);
parse_geo(_NotACoordinate) ->
    undefined.

geo_float({Float, <<>>}, _Text) when is_float(Float) -> Float;
geo_float(_NotAFloat, Text) -> geo_integer(string:to_integer(Text)).

geo_integer({Integer, <<>>}) when is_integer(Integer) -> Integer;
geo_integer(_NotANumber) -> undefined.
