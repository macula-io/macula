%% @doc Records in the signed-object format of DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md.
%%
%% A record is the signed object {key, tbs, signature} under the label MACULA-PQ-RECORD-V1. Its tbs holds type, alg,
%% version, schema, lifetime_class, created_at, expires_at and payload, and subject on a domain type (tags 0x20 to 0xFF)
%% or a built-in tag wire 1.0 does not define. A constructor returns an unsigned record. sign/2 takes the signer's key,
%% refuses a key whose purpose does not fit the type, and adds key, key_id, alg, schema, lifetime_class, tbs and
%% signature. verify/2,3 reads a record as its reader, in the design's order, and keeps its tbs bytes, so encode/1
%% sends them unchanged; custodian_verify/3 is a station's check before it stores a record, which stores types,
%% schemas and content-id tags a reader does not know (wire 1.0, macula#100).
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
    app_record/5,
    procedure_advertisement/4, procedure_advertisement/5, max_serving_stations/0,
    content_announcement/3,
    foundation_seed_list/1, foundation_seed_list/2,
    foundation_parameter/2, foundation_parameter/3,
    foundation_realm_trust_list/1, foundation_realm_trust_list/2,
    foundation_t3_attestation/2, foundation_t3_attestation/3,
    station_endpoint/1, station_endpoint/2,
    station_authorization/2, station_authorization/3, max_station_authorization_window_ms/0,
    tombstone/2, tombstone/3,
    envelope/3
]).
-export([sign/2, verify/2, verify/3, custodian_verify/3, custodian_refresh/4, signer_entry/3, signer_entry/4, refresh/2, refresh/3, encode/1, node_signed/1,
         payload_bounded/1, wire_bounded/1,
         domain_type/1, domain_record_checked/1]).
-export([type/1, key/1, key_id/1, version/1, created_at/1, expires_at/1, payload/1, signature/1]).
-export([lifetime_classes/0, type_lifetime_class/1, type_successor/0]).
-export([payload_field/2, type_procedure_advertisement/0, type_org_directory/0, procedure_advertisement_max_lifetime_ms/0, clock_tolerance_ms/0,
         own_namespace/1]).
-export([decode_payload/1]).
-export([read_node_record/1, read_procedure_advertisement/1, read_station_endpoint/1,
         read_station_authorization/1, read_tombstone/1,
         read_org_directory/1, read_procedure_delegation/1, read_content_announcement/1,
         read_foundation_realm_trust_list/1, read_app_record/1]).
-export([procedure_org/1, verify_authorization/3, authorization_ends_at/3, verify_app/3, verify_membership/5]).
-export([storage_key/1, procedure_key/2, content_key/1, station_endpoint_key/1, org_directory_key/2,
         procedure_delegation_key/2, realm_member_endorsement_key/2, station_authorization_key/2,
         foundation_realm_trust_list_key/1,
         app_key/3]).

-export_type([m_record/0, type_tag/0, version/0, refusal/0, reason/0, authorization/0, trust/0,
              authorization_refusal/0, node_record_opts/0, realm_directory_opts/0, realm_station_entry/0,
              realm_stations_opts/0, realm_member_endorsement_opts/0, station_authorization_opts/0,
              procedure_advertisement_opts/0,
              content_announcement_opts/0, foundation_seed/0, foundation_seed_list_opts/0,
              foundation_parameter_value/0, foundation_parameter_opts/0, foundation_realm_trust_list_entry/0,
              foundation_realm_trust_list_opts/0,
              foundation_t3_attestation_opts/0, tombstone_opts/0, station_endpoint_opts/0, signer_entry_stats/0,
              refresh_delta/0]).

-type type_tag() :: 1..16#FF.
-type version() :: <<_:128>>.

%% The re-signed expiry delta a STORE_REFRESH carries (macula-station#107): a new version, created_at and expires_at, and the
%% signature that covers them over the record's unchanged payload. custodian_refresh/4 applies it to a held record.
-type refresh_delta() :: #{version := version(), created_at := non_neg_integer(),
                           expires_at := non_neg_integer(), signature := binary()}.

%% An unsigned record holds type, version, created_at, expires_at, payload and, on a domain type, subject. A signed or
%% verified record also holds key, key_id, alg, tbs and signature.
-type m_record() :: #{
    type := type_tag(),
    version := version(),
    created_at := non_neg_integer(),
    expires_at := non_neg_integer(),
    payload := map(),
    subject => binary(),
    schema => pos_integer(),
    lifetime_class => non_neg_integer(),
    key => binary(),
    key_id => <<_:256>>,
    alg => binary(),
    tbs => binary(),
    signature => binary()
}.

-type refusal() :: record_too_large | malformed | signature_invalid | not_yet_valid | expired
                 | key_id_mismatch | lifetime_too_long | lifetime_reversed | lifetime_class_unknown
                 | lifetime_class_mismatch | type_unknown | schema_unsupported | mcid_tag_unknown | alg_unsupported
                 | subject_missing | extension_unsupported.
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
                               | delegation_mismatch | authorization_outlived | org_key_mismatch.

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
-type station_authorization_opts() :: #{valid_from => pos_integer(), valid_until => pos_integer(),
                                        ttl_ms => pos_integer(), attestation_key_id => <<_:256>>,
                                        attestation_required => 1}.
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
%% `station_version' names the release the station runs (its app vsn), text of 1 to ?MAX_STATION_VERSION_BYTES bytes, so a
%% reader outside the fleet can tell it under the station's signature. It is self-attested: the signature says who
%% claims it, not that the running code matches.
-type station_endpoint_opts() :: #{host_advertised => [binary()], alpn => binary(), ttl_ms => pos_integer(),
                                   station_version => binary()}.

-define(LABEL, <<"MACULA-PQ-RECORD-V1">>).
-define(STORAGE_KEY_LABEL, "MACULA-PQ-STORAGE-KEY-V1").
-define(MAX_RECORD_BYTES, 256 * 1024).
%% The longest coordinate text a node record's reader parses: a coordinate needs far fewer bytes.
-define(MAX_GEO_TEXT_BYTES, 32).
%% The longest station release a station endpoint carries: a release is a version, and a reader outside the fleet
%% renders it, so it gets no room for anything else.
-define(MAX_STATION_VERSION_BYTES, 64).
%% An app record's own text (app name, version, service names) is 1 to this many bytes (macula#75).
-define(MAX_APP_TEXT_BYTES, 64).
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
-define(TYPE_STATION_AUTHORIZATION,        16#13).
-define(TYPE_ORG_DIRECTORY,                16#15).
-define(TYPE_PROCEDURE_DELEGATION,         16#16).
-define(TYPE_APP_RECORD,                   16#17).
%% Reserved for the successor (key-rotation) record (wire 1.0, macula#83). Its design comes later: a reader refuses it
%% as type_unknown and a custodian stores it like any built-in tag it does not know.
-define(TYPE_SUCCESSOR,                    16#14).
-define(DOMAIN_TYPE_MIN,                   16#20).
%% The schema version every built-in type signs under wire 1.0. A domain type's schema is its owner's.
-define(SCHEMA_V1, 1).
%% The most stations one advertisement names (macula-station#55, K > 1).
-define(MAX_SERVING_STATIONS, 8).
%% A schema version is 1 to 65535 (WIRE_1_0.md 5.1).
-define(MAX_SCHEMA, 65535).
%% The core keys of a record tbs; subject and ext are its only optional ones.
-define(TBS_CORE_KEYS, [{text, <<"type">>}, {text, <<"alg">>}, {text, <<"version">>}, {text, <<"schema">>},
                        {text, <<"lifetime_class">>}, {text, <<"created_at">>}, {text, <<"expires_at">>},
                        {text, <<"payload">>}]).

%% station_endpoint record TTL (Part 4 §11): short enough to drop stale
%% stations within minutes, long enough that a one-minute refresh keeps
%% it live.
-define(STATION_ENDPOINT_TTL_MS, 5 * 60 * 1000).
%% A realm member endorsement's window, valid_from to valid_until, is at most 24 hours, so one endorsement admits its
%% member for at most that long even where the realm's tombstone never arrives (macula#118; the removal bound of
%% DECISIONS_POST_QUANTUM.md, decided 2026-09-22). A builder given no valid_from starts the window one clock
%% tolerance in the past, so a verifier whose clock is behind the realm's does not refuse a fresh endorsement as not
%% yet valid; given no valid_until it takes the whole window.
-define(MAX_ENDORSEMENT_WINDOW_MS, 24 * 60 * 60 * 1000).
%% A station authorization's window, valid_from to valid_until, is at most 30 minutes: a deliberate tightening
%% for infrastructure identities (macula-station#63; the window bounds the worst-case refusal tail once the realm
%% stops reissuing). Its lifetime, created_at to expires_at, is the same bound.
-define(STATION_AUTHORIZATION_MAX_WINDOW_MS, 30 * 60 * 1000).
%% A station authorization's hostname is text of 1 to 253 bytes (the DNS name bound); a role is text of 1 to 64.
-define(MAX_STATION_HOSTNAME_BYTES, 253).
-define(MAX_STATION_ROLE_BYTES, 64).
-define(DEFAULT_TTL_MS, 48 * 60 * 60 * 1000).
%% The maxima of the frozen lifetime-class table (lifetime_classes/0), created_at to expires_at. A node record and a
%% content announcement 48 hours; a procedure advertisement 5 minutes, renewed at half that or sooner, so a provider
%% that stops is gone within minutes; a station endpoint 5 minutes; realm stations and an org directory 6 hours; a
%% procedure delegation 30 minutes (D32); a realm member endorsement 30 days; a domain record 7 days (D28); and the
%% foundation types and a realm directory 30 days, so no record keeps a key trusted without end. A tombstone lives at
%% most its withdrawn type's maximum plus twice the clock tolerance.
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
%% valid_until, is at most 24 hours and never ends before it starts: a longer one raises a badmatch on
%% `{error, endorsement_window_too_long}', and a reversed one on `{error, endorsement_window_reversed}'. Without a
%% valid_from the window starts one clock tolerance (clock_tolerance_ms/0) before now; without a valid_until it ends
%% 24 hours after valid_from. A caller that sets valid_until sets valid_from too: a valid_until 24 hours from now with
%% the default valid_from is a window 5 minutes too long.
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
    ValidFrom = maps:get(valid_from, Opts, Now - ?CLOCK_TOLERANCE_MS),
    ValidUntil = maps:get(valid_until, Opts, ValidFrom + ?MAX_ENDORSEMENT_WINDOW_MS),
    ok = endorsement_window(ValidFrom, ValidUntil),
    unsigned(?TYPE_REALM_MEMBER_ENDORSEMENT,
             realm_member_endorsement_payload(RealmId, Member, Roles, ValidFrom, ValidUntil), Opts).

endorsement_window(From, Until) when Until < From -> {error, endorsement_window_reversed};
endorsement_window(From, Until) when Until - From > ?MAX_ENDORSEMENT_WINDOW_MS -> {error, endorsement_window_too_long};
endorsement_window(_From, _Until) -> ok.

%% @doc The longest window a realm member endorsement may have, valid_from to valid_until, in milliseconds: 24 hours.
%% Its builder refuses a longer one, and macula_hyparview_endorsement:verify_endorsement/3 refuses one it receives.
-spec max_endorsement_window_ms() -> pos_integer().
max_endorsement_window_ms() ->
    ?MAX_ENDORSEMENT_WINDOW_MS.

%% @doc A realm's statement, signed by the realm key, that the node StationNode serves it as a station at Hostname,
%% in Roles, valid over a window from valid_from to valid_until of at most 30 minutes
%% (`?STATION_AUTHORIZATION_MAX_WINDOW_MS'): a longer one raises a badmatch on
%% `{error, station_authorization_window_too_long}', a reversed one on `{error, station_authorization_window_reversed}'.
%%
%% `attestation_key_id', when carried, is the full 32-byte SHA-256 of the station's hardware attestation public key
%% (never a prefix of it), and `attestation_required' = 1 says the realm requires a live attestation for this node.
%% The co-presence bound holds at the builder and at `payload_ok/2' alike: a record that demands an attestation but
%% names no key is refused; a key id without the flag is a legitimate optional attestation. The attestation statement
%% and its verification belong to macula-station#62; this record only carries the pair.
-spec station_authorization(<<_:256>>, #{realm := <<_:256>>, station_node := <<_:256>>, hostname := binary(),
                                         roles := [binary()]}) -> m_record().
station_authorization(RealmId, Spec) ->
    station_authorization(RealmId, Spec, #{}).

-spec station_authorization(<<_:256>>, #{realm := <<_:256>>, station_node := <<_:256>>, hostname := binary(),
                                         roles := [binary()]}, station_authorization_opts()) -> m_record().
station_authorization(RealmId, #{realm := RealmId, station_node := Node, hostname := Hostname, roles := Roles},
                      Opts)
  when is_binary(RealmId), byte_size(RealmId) =:= 32, is_binary(Node), byte_size(Node) =:= 32,
       is_binary(Hostname), is_list(Roles) ->
    Now = erlang:system_time(millisecond),
    ValidFrom = maps:get(valid_from, Opts, Now),
    ValidUntil = maps:get(valid_until, Opts, ValidFrom + ?STATION_AUTHORIZATION_MAX_WINDOW_MS),
    ok = station_authorization_window(ValidFrom, ValidUntil),
    ok = station_authorization_attestation_options(Opts),
    unsigned(?TYPE_STATION_AUTHORIZATION,
             station_authorization_payload(RealmId, Node, Hostname, Roles, ValidFrom, ValidUntil, Opts), Opts).

station_authorization_window(From, Until) when Until < From -> {error, station_authorization_window_reversed};
station_authorization_window(From, Until) when Until - From > ?STATION_AUTHORIZATION_MAX_WINDOW_MS ->
    {error, station_authorization_window_too_long};
station_authorization_window(_From, _Until) -> ok.

station_authorization_attestation_options(Opts) ->
    case attestation_pair_ok({maps:get(attestation_required, Opts, undefined),
                              maps:get(attestation_key_id, Opts, undefined)}) of
        true -> ok;
        false -> {error, station_authorization_attestation_bound}
    end.

%% The co-presence bound: refusal when a record demands an attestation but names no key, when the flag is not 1, or
%% when the key id is not a 32-byte id. Both absent, and a key id without the flag, are legitimate.
attestation_pair_ok({undefined, undefined}) -> true;
attestation_pair_ok({undefined, KeyId}) -> is_id(KeyId);
attestation_pair_ok({1, KeyId}) -> is_id(KeyId);
attestation_pair_ok(_Pair) -> false.

%% @doc The longest window a station authorization may have, valid_from to valid_until, in milliseconds: 30 minutes.
-spec max_station_authorization_window_ms() -> pos_integer().
max_station_authorization_window_ms() ->
    ?STATION_AUTHORIZATION_MAX_WINDOW_MS.

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

%% @doc An org's statement of one of its apps (macula#75), signed by the org key OrgKeyId: the app's version and its
%% services, each naming the procedures it serves, all in the org's own namespace. It carries the realm-signed org
%% directory that ties the org name to OrgKeyId, so verify_app/3 checks it without a lookup, as an advertisement's
%% authorization is checked. Stored under app_key/3 of the app's MRI parts; it lives at most 30 minutes, like the
%% procedure delegations it is issued with.
-spec app_record(<<_:256>>, <<_:256>>, binary(), binary(),
                 #{version := binary(), services := [#{name := binary(), procedures := [binary()]}],
                   org_directory := binary(), ttl_ms => pos_integer()}) -> m_record().
app_record(OrgKeyId, RealmId, OrgName, AppName,
           #{version := Version, services := Services, org_directory := Directory} = Opts)
  when is_binary(OrgKeyId), byte_size(OrgKeyId) =:= 32, is_binary(RealmId), byte_size(RealmId) =:= 32,
       is_binary(OrgName), is_binary(AppName), is_binary(Version), is_list(Services), is_binary(Directory) ->
    Payload = #{{text, <<"realm_id">>} => RealmId,
                {text, <<"org_name">>} => {text, OrgName},
                {text, <<"app_name">>} => {text, AppName},
                {text, <<"version">>} => {text, Version},
                {text, <<"org_key">>} => OrgKeyId,
                {text, <<"services">>} => [app_service(S) || S <- Services],
                {text, <<"authorization">>} => #{{text, <<"org_directory">>} => Directory}},
    unsigned(?TYPE_APP_RECORD, Payload, Opts).

app_service(#{name := Name, procedures := Procedures}) when is_binary(Name), is_list(Procedures) ->
    #{{text, <<"name">>} => {text, Name}, {text, <<"procedures">>} => [{text, P} || P <- Procedures]}.

%% @doc The most stations one advertisement names (macula-station#55, K > 1): a longer list is refused by the
%% builder, so a caller that pins a provider to stations can refuse one the record could not carry whole.
-spec max_serving_stations() -> pos_integer().
max_serving_stations() -> ?MAX_SERVING_STATIONS.

%% @doc A provider's advertisement of a procedure in a realm, signed by the provider, naming the 1 to 8 stations it
%% serves the procedure through (macula-station#55, K > 1): one record per provider and procedure, so the slot holds
%% the provider's whole set, sorted and without repeats. For a procedure with an org
%% namespace the authorization option carries the provider authorization: org_directory and procedure_delegation as
%% the records' wire form, the only authorization form. The builder refuses any other. The kem_key option names the
%% provider's KEM key as carried (E2E design, amendment A1), which the advertisement carries with its kem_key_id: a
%% caller seals its request to that key.
-spec procedure_advertisement(<<_:256>>, <<_:256>>, binary(), [<<_:256>>, ...]) -> m_record().
procedure_advertisement(AdvertiserNode, RealmId, Procedure, ServingStations) ->
    procedure_advertisement(AdvertiserNode, RealmId, Procedure, ServingStations, #{}).

-spec procedure_advertisement(<<_:256>>, <<_:256>>, binary(), [<<_:256>>, ...], procedure_advertisement_opts()) ->
          m_record().
procedure_advertisement(AdvertiserNode, RealmId, Procedure, ServingStations, Opts)
  when is_binary(AdvertiserNode), byte_size(AdvertiserNode) =:= 32, is_binary(RealmId), byte_size(RealmId) =:= 32,
       is_binary(Procedure), is_list(ServingStations), is_map(Opts) ->
    Stations = lists:usort(ServingStations),
    true = serving_stations_ok(Stations),
    Payload = #{{text, <<"realm_id">>} => RealmId,
                {text, <<"procedure">>} => {text, Procedure},
                {text, <<"advertiser_node">>} => AdvertiserNode,
                {text, <<"serving_stations">>} => Stations},
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
    Payload2 = with_text(Payload1, <<"alpn">>, maps:get(alpn, Opts, undefined)),
    Payload = with_text(Payload2, <<"station_version">>, version_named(maps:get(station_version, Opts, undefined))),
    unsigned(?TYPE_STATION_ENDPOINT, Payload, maps:merge(#{ttl_ms => ?STATION_ENDPOINT_TTL_MS}, Opts)).

%% A release names something, briefly: an empty one, or one past ?MAX_STATION_VERSION_BYTES, is refused.
version_named(undefined) -> undefined;
version_named(Release) when is_binary(Release), byte_size(Release) > 0, byte_size(Release) =< ?MAX_STATION_VERSION_BYTES ->
    Release.

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
%% non-empty binary: an empty subject would name a slot apart from no subject. The schema option names the version of
%% the owner's payload rules, 1 unless given: a domain type's schema is its owner's to read.
-spec envelope(type_tag(), map(), map()) -> m_record().
envelope(Type, Payload, Opts)
  when is_integer(Type), Type >= ?DOMAIN_TYPE_MIN, Type =< 16#FF, is_map(Payload), is_map(Opts) ->
    with_schema(with_subject(unsigned(Type, Payload, Opts), maps:get(subject_id, Opts, undefined)),
                maps:get(schema, Opts, ?SCHEMA_V1)).

with_schema(Record, Schema) when is_integer(Schema), Schema >= 1, Schema < ?MAX_PROTOCOL_INT ->
    Record#{schema => Schema}.

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
    #{{text, <<"schema">>} := Schema, {text, <<"lifetime_class">>} := Class} = Fields,
    Record#{key => Carried, key_id => KeyId, alg => macula_signed_object:alg(Profile), tbs => Tbs,
            signature => Signature, schema => Schema, lifetime_class => Class}.

%% @doc Verify a record as its reader, given as its wire form or as its {key, tbs, signature} map, under the verifier's
%% profile and clock. Refusals are returned, never raised. After every custodian_verify/3 check, a reader refuses, in
%% order (WIRE_1_0.md 5.2, steps 10 to 12): a type it does not know (type_unknown), a built-in type's schema other than
%% 1 (schema_unsupported), an ext entry with critical
%% 1 (extension_unsupported; records define no extension yet, and a critical 0 one is ignored), a payload that breaks
%% its type's rules (malformed), a content id with a tag other than 2 (mcid_tag_unknown), and a payload that names a
%% signer other than the key (key_id_mismatch).
-spec verify(binary() | map(), macula_crypto_profile:profile()) -> {ok, m_record()} | {error, refusal()}.
verify(Signed, Profile) ->
    verify(Signed, Profile, erlang:system_time(millisecond)).

-spec verify(binary() | map(), macula_crypto_profile:profile(), integer()) -> {ok, m_record()} | {error, refusal()}.
verify(Signed, Profile, Now) ->
    read_checked(custodian_verify(Signed, Profile, Now), Profile).

%% @doc Verify a record as its custodian, a station that stores records it need not understand (wire 1.0, macula#83 D4,
%% WIRE_1_0.md 5.2 steps 1 to 9). In order, first failure wins: the wire form fits 256 KiB (record_too_large); the
%% signed object decodes (malformed); its tbs decodes with a text alg (malformed), since alg selects the verifier; the
%% alg is the verifier's own profile's (alg_unsupported); the carried key is in that profile's form (malformed); the
%% signature verifies (signature_invalid); the tbs holds exactly its core fields, with subject and ext where present
%% (malformed); the clock (not_yet_valid, expired); the lifetime (lifetime_reversed, then lifetime_class_unknown for a
%% row outside the table, then lifetime_too_long against the row); a known type naming a row other than its own
%% (lifetime_class_mismatch); the slot: a known type's slot fields present and typed (malformed), and a built-in tag it
%% does not know carries a subject (subject_missing). The payload is read no further than the slot fields and a
%% tombstone's withdrawn type, so a type, schema or content-id tag a reader does not know is stored. The record comes
%% back with its key_id: for a built-in tag wire 1.0 does not define, as for a domain type, the MACULA-KEY-ID-V1 id of
%% its key whatever the key's purpose, never a node_id. storage_key/1 places it.
-spec custodian_verify(binary() | map(), macula_crypto_profile:profile(), integer()) ->
          {ok, m_record()} | {error, refusal()}.
custodian_verify(Signed, Profile, Now) ->
    custodian_checked(envelope_verified(Signed, Profile, Now), Profile).

envelope_verified(Bytes, _Profile, _Now) when is_binary(Bytes), byte_size(Bytes) > ?MAX_RECORD_BYTES ->
    {error, record_too_large};
envelope_verified(Bytes, Profile, Now) when is_binary(Bytes) ->
    decoded_object(macula_signed_object:decode(Bytes), Profile, Now);
envelope_verified(#{key := Key, tbs := Tbs, signature := Signature} = Object, Profile, Now)
  when map_size(Object) =:= 3, is_binary(Key), is_binary(Tbs), is_binary(Signature) ->
    sized_object(byte_size(macula_signed_object:encode(Object)), Object, Profile, Now);
envelope_verified(_Other, _Profile, _Now) ->
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
    Fresh = maps:with([type, payload, subject, schema], Record),
    sign(Fresh#{version => macula_record_uuid:v7_monotonic(Now), created_at => Now,
                expires_at => ends_at(Now + (Expires - Created), Bound)},
         Key).

ends_at(Expires, no_bound) -> Expires;
ends_at(Expires, NotAfter) -> min(Expires, NotAfter).

%% @doc Apply a STORE_REFRESH to a record a custodian already holds (macula-station#107, D28). A refresh carries only the
%% re-signed expiry delta the publisher produced with refresh/2,3 -- the new version, created_at, expires_at and
%% signature -- and NO payload. The custodian reconstructs the signed tbs over the HELD payload with the delta's
%% version and timestamps, exactly as sign/2 built it, and runs the delta's signature through the same
%% custodian_verify/3 a full STORE gets. When it verifies, the returned record is byte-for-byte the one a full STORE
%% of the publisher's refresh would have produced, so encode/1 of it sends the publisher's bytes unchanged.
%%
%% ANY failure returns `need_full', never a partial or stale record: a reconstructed tbs that does not verify (the
%% held payload is not the one the delta was signed over), a signature over timestamps outside the type's lifetime
%% class, an expired or not-yet-valid clock, or a created_at/expires_at outside the range a record may carry (a wire
%% integer reaches 2^64-1, a record timestamp is bounded by ?MAX_PROTOCOL_INT, so a decoded frame cannot crash the
%% custodian). The sender then sends a full STORE. Version monotonicity against the held entry is the slot's concern
%% (macula_dht_slots:later/2), applied by the caller as for a full STORE; this function reconstructs and verifies, it
%% does not place. Held is a verified record the caller looked up by {key, signer}; when the caller holds no base
%% entry it answers need_full without calling this.
%%
%% Held may also be the record's WIRE BYTES, the form a custodian holds in its slot. They are decoded without a
%% signature step -- they were verified at admission, and the delta's signature is re-verified over the tbs
%% reconstructed from them, so the forgery bar is a full STORE's and a wrong held record only answers need_full. This
%% is the only verify-free decode of a record and it never leaves this function: a caller cannot use it to obtain an
%% unverified m_record(). Held bytes that do not decode to a readable record answer need_full and never raise.
-spec custodian_refresh(m_record() | binary(), refresh_delta(), macula_crypto_profile:profile(), integer()) ->
          {ok, m_record()} | need_full.
custodian_refresh(#{key := HeldKey, type := Type, payload := Payload, lifetime_class := Class} = Held,
                  #{version := Version, created_at := Created, expires_at := Expires, signature := Signature},
                  Profile, Now)
  when is_binary(HeldKey), is_binary(Version), byte_size(Version) =:= 16,
       is_integer(Created), Created >= 0, Created < ?MAX_PROTOCOL_INT,
       is_integer(Expires), Expires >= 0, Expires < ?MAX_PROTOCOL_INT, is_binary(Signature) ->
    Fields = refresh_tbs_fields(Type, Payload, Class, maps:get(schema, Held, ?SCHEMA_V1),
                                maps:get(subject, Held, undefined), Version, Created, Expires),
    Tbs = reconstructed_tbs(Fields, Profile),
    refresh_outcome(custodian_verify(#{key => HeldKey, tbs => Tbs, signature => Signature}, Profile, Now));
%% A well-formed delta the first clause cannot admit because a timestamp is outside the record range a wire frame
%% may still carry (`non_neg' on the wire reaches 2^64-1): no record the publisher could sign has such a timestamp,
%% so answer need_full rather than crash. Keeps the contract total for every frame the station decodes. A delta of
%% another shape, or a held record that is not a verified one, is a caller fault and is left to fail.
custodian_refresh(#{key := _, type := _, payload := _, lifetime_class := _},
                  #{version := Version, created_at := Created, expires_at := Expires, signature := Signature},
                  _Profile, _Now)
  when is_binary(Version), is_integer(Created), Created >= 0, is_integer(Expires), Expires >= 0,
       is_binary(Signature) ->
    need_full;
%% Held as the wire bytes the custodian already holds: the form a station applies a store_refresh frame against
%% (macula-station#107). Decode the held envelope WITHOUT a signature step - these bytes were verified at admission,
%% and the map clause below re-verifies the delta's signature over the tbs reconstructed from them, so a held record
%% that is wrong or forged only yields need_full, never an admit. Then refresh the decoded record through the map
%% clauses, so the tbs reconstruction and the out-of-range-timestamp fallback stay one code path. Held bytes that do
%% not decode to a readable record answer need_full and never raise: the frame is reachable from the wire on the DHT
%% receive path. This verify-free decode lives only here, so no caller can mint an unverified m_record() from it.
custodian_refresh(HeldBytes, Delta, Profile, Now) when is_binary(HeldBytes) ->
    refresh_held(held_record(HeldBytes), Delta, Profile, Now).

%% Reconstruct a held record from its wire bytes without checking its signature, to the same map custodian_verify
%% builds minus the crypto: the record read from its tbs with the envelope's key. Total over any binary - decode and
%% the strict codec answer malformed rather than raise, and bytes past the record size, a held-only object (no key),
%% or an unreadable tbs all answer need_full.
-spec held_record(binary()) -> {ok, m_record()} | need_full.
held_record(Bytes) when byte_size(Bytes) =< ?MAX_RECORD_BYTES ->
    held_envelope(macula_signed_object:decode(Bytes));
held_record(_TooLarge) ->
    need_full.

held_envelope({ok, #{key := Key, tbs := Tbs}}) ->
    held_fields(macula_record_cbor:decode_strict(Tbs), Key);
held_envelope(_HeldOrMalformed) ->
    need_full.

held_fields({ok, Fields}, Key) ->
    held_keyed(read_tbs(Fields), Key);
held_fields({error, _Strict}, _Key) ->
    need_full.

held_keyed({ok, Record}, Key) ->
    {ok, Record#{key => Key}};
held_keyed(malformed, _Key) ->
    need_full.

%% Refresh a held record decoded from bytes through the map clauses, so there is one reconstruction and one totality.
refresh_held({ok, Held}, Delta, Profile, Now) ->
    custodian_refresh(Held, Delta, Profile, Now);
refresh_held(need_full, _Delta, _Profile, _Now) ->
    need_full.

%% The tbs fields of the refreshed record, reconstructed from what the custodian HOLDS: the held type, payload,
%% lifetime_class, schema and subject, with the delta's version and timestamps. The class is the HELD one, not
%% re-derived from the type (a custodian stores records whose built-in tag has no class row, and tombstones of them;
%% re-deriving would error on those, and the held class is what the publisher's signature covers anyway, exact for a
%% known type by the lifetime_class_mismatch check at store time). A held record carrying an `ext' entry, which this
%% SDK never signs but a custodian stores, is not reconstructed field-for-field here (as sign/2 does not carry ext),
%% so a refresh of one fails to verify and answers need_full every cycle: waste, never loss.
refresh_tbs_fields(Type, Payload, Class, Schema, Subject, Version, Created, Expires) ->
    Fields = #{{text, <<"type">>} => Type, {text, <<"version">>} => Version, {text, <<"schema">>} => Schema,
               {text, <<"lifetime_class">>} => Class, {text, <<"created_at">>} => Created,
               {text, <<"expires_at">>} => Expires, {text, <<"payload">>} => Payload},
    with_tbs_subject(Fields, Subject).

%% The tbs bytes sign/2 would have produced for these fields: the core fields with the verifier's alg, encoded by the
%% one deterministic record codec. Must agree with macula_signed_object:sign/3, which builds the tbs the same way.
reconstructed_tbs(Fields, Profile) ->
    macula_record_cbor:encode(Fields#{{text, <<"alg">>} => {text, macula_signed_object:alg(Profile)}}).

refresh_outcome({ok, Record}) -> {ok, Record};
refresh_outcome({error, _Refusal}) -> need_full.

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

%% @doc The org directory type tag, for a caller that must tell a directory from the tombstone that withdrew it before
%% it reads the directory's org key.
-spec type_org_directory() -> type_tag().
type_org_directory() -> ?TYPE_ORG_DIRECTORY.

%% @doc A payload field, read whatever key form it arrived in: `{text, Name}', the binary, or an existing atom. A
%% text value is returned as its binary.
-spec payload_field(map(), binary()) -> term().
payload_field(Payload, Name) ->
    unwrap_text(first_present([{text, Name}, Name, safe_atom(Name)], Payload)).

%% @doc Deep normalization of a payload that arrived in wire form, the
%% exact inverse of `macula_frame:to_wire/1' over the payload term:
%% `{text, K}' map keys become binary keys, `{text, V}' values become
%% their binaries, `null' becomes `undefined', and maps and lists are
%% walked recursively. Binaries and integers are already canonical and
%% pass through. A term with no wire-form artifacts comes back equal to
%% itself, so the call is idempotent on already-decoded payloads.
%%
%% Handlers receive call args and pubsub facts in wire form
%% (`macula_subscriber' delivers what the frame carried), and consumers
%% used to hand-roll this unwrapping — badly: mcl-sec-guard matched atom
%% keys on the wire form and silently discarded every fact
%% (macula-services/mcl-sec-guard#2). One shared, tested inverse beats
%% per-consumer copies that each get it slightly wrong.
-spec decode_payload(term()) -> term().
decode_payload(M) when is_map(M) ->
    maps:fold(fun(K, V, Acc) ->
                  Acc#{decode_key(K) => decode_payload(V)}
              end, #{}, M);
decode_payload(L) when is_list(L) ->
    [decode_payload(E) || E <- L];
decode_payload(null) ->
    undefined;
decode_payload({text, B}) when is_binary(B) ->
    B;
decode_payload(V) ->
    V.

decode_key({text, K}) when is_binary(K) -> K;
decode_key(K) -> K.

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
      serving_stations => payload_field(P, <<"serving_stations">>),
      authorization   => read_authorization(maps:get({text, <<"authorization">>}, P, undefined))},
    maps:merge(Read, maps:from_list([{Field, maps:get({text, Name}, P)}
                                     || {Name, Field} <- [{<<"kem_key">>, kem_key}, {<<"kem_key_id">>, kem_key_id}],
                                        maps:is_key({text, Name}, P)])).

%% `station_version' only when the record carries one as text of 1 to ?MAX_STATION_VERSION_BYTES bytes, the constructor's own
%% rule: a record published before it existed reads exactly as it did, and bytes, empty text or text past the bound read
%% as no release.
-spec read_station_endpoint(m_record()) ->
          #{quic_port := 1..65535, host_advertised := [binary()], station_version => binary()}.
read_station_endpoint(#{type := ?TYPE_STATION_ENDPOINT, payload := P}) ->
    Read = #{quic_port       => payload_field(P, <<"quic_port">>),
             host_advertised => host_list(payload_field(P, <<"host_advertised">>))},
    maps:merge(Read, maps:from_list([{station_version, Release}
                                     || {text, Release} <- [maps:get({text, <<"station_version">>}, P, undefined)],
                                        byte_size(Release) > 0, byte_size(Release) =< ?MAX_STATION_VERSION_BYTES])).

-spec read_station_authorization(m_record()) ->
          #{realm_id := <<_:256>>, station_node := <<_:256>>, hostname := binary(), roles := [binary()],
            valid_from := non_neg_integer(), valid_until := non_neg_integer(),
            attestation_key_id => <<_:256>>, attestation_required => 1}.
read_station_authorization(#{type := ?TYPE_STATION_AUTHORIZATION, payload := P}) ->
    Base = #{realm_id     => payload_field(P, <<"realm_id">>),
             station_node => payload_field(P, <<"station_node">>),
             hostname     => payload_field(P, <<"hostname">>),
             roles        => [Role || {text, Role} <- payload_field(P, <<"roles">>)],
             valid_from   => payload_field(P, <<"valid_from">>),
             valid_until  => payload_field(P, <<"valid_until">>)},
    maps:merge(Base, maps:from_list(
                       [{Field, maps:get({text, Name}, P)}
                        || {Name, Field} <- [{<<"attestation_key_id">>, attestation_key_id},
                                             {<<"attestation_required">>, attestation_required}],
                           maps:is_key({text, Name}, P)])).

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

-spec read_app_record(m_record()) ->
          #{realm_id := <<_:256>>, org_name := binary(), app_name := binary(), version := binary(),
            org_key := <<_:256>>, services := [#{name := binary(), procedures := [binary()]}],
            org_directory := binary()}.
read_app_record(#{type := ?TYPE_APP_RECORD, payload := P}) ->
    #{{text, <<"org_directory">>} := Directory} = maps:get({text, <<"authorization">>}, P),
    #{realm_id      => payload_field(P, <<"realm_id">>),
      org_name      => payload_field(P, <<"org_name">>),
      app_name      => payload_field(P, <<"app_name">>),
      version       => payload_field(P, <<"version">>),
      org_key       => payload_field(P, <<"org_key">>),
      services      => [#{name => text(N), procedures => [text(Pr) || Pr <- Prs]}
                        || #{{text, <<"name">>} := N, {text, <<"procedures">>} := Prs}
                               <- maps:get({text, <<"services">>}, P)],
      org_directory => Directory}.

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

%% @doc How far a record's clocks may disagree with a verifier's: a record is accepted this long before its created_at
%% and after its expires_at (D22).
-spec clock_tolerance_ms() -> pos_integer().
clock_tolerance_ms() ->
    ?CLOCK_TOLERANCE_MS.

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

%% @doc The millisecond a verified advertisement's provider authorization ends: the earlier expiry of the org directory
%% and the procedure delegation it carries, which is how long a caller may hold a stream opened under it (macula#120).
%% It is longer than the advertisement's own life, which the provider renews within it. An advertisement without an
%% authorization (no org namespace, or the advertiser's own) has none, and one whose directory or delegation no longer
%% verifies has ended: 0. It is meaningful for an advertisement verify_authorization/3 accepted.
-spec authorization_ends_at(m_record(), macula_crypto_profile:profile(), integer()) -> non_neg_integer() | none.
authorization_ends_at(#{type := ?TYPE_PROCEDURE_ADVERTISEMENT} = Advertisement, Profile, Now) ->
    #{authorization := Authorization} = read_procedure_advertisement(Advertisement),
    authorization_end(Authorization, Profile, Now).

authorization_end(#{org_directory := Directory, procedure_delegation := Delegation}, Profile, Now) ->
    earliest_expiry(verify(Directory, Profile, Now), verify(Delegation, Profile, Now));
authorization_end(_None, _Profile, _Now) ->
    none.

earliest_expiry({ok, Directory}, {ok, Delegation}) -> min(expires_at(Directory), expires_at(Delegation));
earliest_expiry(_Directory, _Delegation) -> 0.

%% @doc The caller's check of a verified app record against the realm trust it holds (macula#75), pure: the org
%% directory the record carries verifies, is signed by the realm key (carried or named by the trust list's pairs, as for
%% verify_authorization/3), names the record's realm and org, and holds the org key that signed the record; and the
%% record expires no later than that directory. The shape rules (an org namespace, every procedure in it) are payload
%% rules: verify/3 has already refused a record that breaks them.
-spec verify_app(m_record(), trust(), integer()) -> ok | {error, authorization_refusal()}.
verify_app(#{type := ?TYPE_APP_RECORD} = App, #{profile := Profile} = Trust, Now) ->
    #{realm_id := RealmId, org_directory := Directory} = read_app_record(App),
    app_directory(realm_trust_key(RealmId, Trust), verify(Directory, Profile, Now), App).

app_directory(none, _Directory, _App) ->
    {error, no_realm_key};
app_directory(Expected, {ok, #{type := ?TYPE_ORG_DIRECTORY, key := DirectoryKey, key_id := DirectoryKeyId} = Dir},
              App) ->
    #{realm_id := DirRealmId, org_name := DirOrg, org_key := DirOrgKey} = read_org_directory(Dir),
    #{realm_id := RealmId, org_name := OrgName, org_key := OrgKey} = read_app_record(App),
    app_directory_matched(directory_signer_matches(Expected, DirectoryKey, DirectoryKeyId) andalso
                              DirRealmId =:= RealmId,
                          DirOrg =:= OrgName, DirOrgKey =:= OrgKey, expires_at(App) =< expires_at(Dir));
app_directory(_Expected, _Refused, _App) ->
    {error, org_directory_invalid}.

app_directory_matched(false, _SameOrg, _SameKey, _Within) -> {error, org_directory_wrong_realm};
app_directory_matched(true, false, _SameKey, _Within) -> {error, org_directory_wrong_org};
app_directory_matched(true, true, false, _Within) -> {error, org_key_mismatch};
app_directory_matched(true, true, true, Within) -> within(Within).

%% @doc The caller's check that a node is an admitted member of an org in a realm, pure: the same chain an advertisement
%% carries (verify_authorization/3), asked about a node instead of an advertisement. The org directory verifies, is
%% signed by the realm key the trust holds (carried or named by the trust list's pairs), and names the realm and the
%% org; the delegation verifies, is signed by the org key that directory holds, and names the node. The answer is the
%% earlier of the two expiries, after which the question must be asked again: that is how a realm's revocation, which
%% stops reissuing the delegation and tombstones it, reaches a caller that caches the answer.
%%
%% It exists for a subscriber: a delivery's publisher is the node that signed the publication, and nothing in the wire
%% says that node may speak for an org, so a service that must hear only an org's own nodes asks this about it.
-spec verify_membership(binary(), binary(), #{realm_id := <<_:256>>, org := binary(), node_id := <<_:256>>}, trust(),
                        integer()) -> {ok, non_neg_integer()} | {error, authorization_refusal()}.
verify_membership(Directory, Delegation, #{realm_id := RealmId} = Member, #{profile := Profile} = Trust, Now)
  when is_binary(Directory), is_binary(Delegation) ->
    member_directory(realm_trust_key(RealmId, Trust), verify(Directory, Profile, Now), Delegation, Member, Profile,
                     Now).

member_directory(none, _Directory, _Delegation, _Member, _Profile, _Now) ->
    {error, no_realm_key};
member_directory(Expected, {ok, #{type := ?TYPE_ORG_DIRECTORY, key := DirectoryKey, key_id := DirectoryKeyId} = Dir},
                 Delegation, #{realm_id := RealmId, org := Org} = Member, Profile, Now) ->
    #{realm_id := DirRealmId, org_name := DirOrg, org_key := OrgKeyId} = read_org_directory(Dir),
    member_org(directory_signer_matches(Expected, DirectoryKey, DirectoryKeyId) andalso DirRealmId =:= RealmId,
               DirOrg =:= Org, OrgKeyId, Dir, Delegation, Member, Profile, Now);
member_directory(_Expected, _Refused, _Delegation, _Member, _Profile, _Now) ->
    {error, org_directory_invalid}.

member_org(false, _SameOrg, _OrgKeyId, _Dir, _Delegation, _Member, _Profile, _Now) ->
    {error, org_directory_wrong_realm};
member_org(true, false, _OrgKeyId, _Dir, _Delegation, _Member, _Profile, _Now) ->
    {error, org_directory_wrong_org};
member_org(true, true, OrgKeyId, Dir, Delegation, Member, Profile, Now) ->
    member_delegation(verify(Delegation, Profile, Now), OrgKeyId, Dir, Member).

%% The delegation must be signed by THE org key the directory names, not merely by an org key: any node can mint one.
member_delegation({ok, #{type := ?TYPE_PROCEDURE_DELEGATION, key_id := DelegationKeyId} = Del}, OrgKeyId, Dir,
                  #{node_id := NodeId}) ->
    #{advertiser := Advertiser} = read_procedure_delegation(Del),
    member_matched(DelegationKeyId =:= OrgKeyId andalso Advertiser =:= NodeId,
                   min(expires_at(Dir), expires_at(Del)));
member_delegation(_Refused, _OrgKeyId, _Dir, _Member) ->
    {error, delegation_invalid}.

member_matched(false, _Earliest) -> {error, delegation_mismatch};
member_matched(true, Earliest) -> {ok, Earliest}.

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

%% @doc The storage key of an app record, from the parts of the app's MRI `mri:app:<realm>/<org>/<app>': the realm id
%% (macula_realm:id/1 of the realm name), the org name and the app name.
-spec app_key(<<_:256>>, binary(), binary()) -> <<_:256>>.
app_key(<<_:256>> = RealmId, OrgName, AppName) when is_binary(OrgName), is_binary(AppName) ->
    derived(?TYPE_APP_RECORD, [RealmId, {field, OrgName}, {field, AppName}]).

%% @doc The storage key of a realm member endorsement, from the realm id and the member's node_id: the slot that holds
%% the realm's endorsement of that member and, once the realm revokes it, the realm's tombstone of it. A lookup of this
%% key is what macula_hyparview_endorsement:slot_endorsement/3,4 reads.
-spec realm_member_endorsement_key(<<_:256>>, <<_:256>>) -> <<_:256>>.
realm_member_endorsement_key(<<_:256>> = RealmId, <<_:256>> = Member) ->
    derived(?TYPE_REALM_MEMBER_ENDORSEMENT, [RealmId, Member]).

%% @doc The storage key of a station's authorization, from the realm id and the station's node_id: the slot that
%% holds the realm's authorization of that station and, once the realm revokes it, the realm's tombstone of it.
-spec station_authorization_key(<<_:256>>, <<_:256>>) -> <<_:256>>.
station_authorization_key(<<_:256>> = RealmId, <<_:256>> = Node) ->
    derived(?TYPE_STATION_AUTHORIZATION, [RealmId, Node]).

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
    Fields = #{{text, <<"type">>} => Type, {text, <<"version">>} => Version,
               {text, <<"schema">>} => maps:get(schema, R, ?SCHEMA_V1),
               {text, <<"lifetime_class">>} => signed_class(record_class(Type, Payload), Type),
               {text, <<"created_at">>} => Created, {text, <<"expires_at">>} => Expires, {text, <<"payload">>} => Payload},
    with_tbs_subject(Fields, maps:get(subject, R, undefined)).

signed_class(none, Type) -> erlang:error({malformed, Type});
signed_class(Class, _Type) -> Class.

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
                                     Type =:= ?TYPE_REALM_MEMBER_ENDORSEMENT; Type =:= ?TYPE_ORG_DIRECTORY;
                                     Type =:= ?TYPE_STATION_AUTHORIZATION ->
    [realm];
signer_purposes(Type, _Payload) when Type =:= ?TYPE_PROCEDURE_DELEGATION; Type =:= ?TYPE_APP_RECORD ->
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
signer_field(?TYPE_APP_RECORD) -> <<"org_key">>;
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

%% @doc The frozen lifetime-class table of wire 1.0 (macula#100): a class names the longest a record lives, created_at
%% to expires_at, in milliseconds. Every record names its class in its tbs, so a custodian bounds a type it does not
%% know. The table only grows, and under an LTS line a class may tighten, never loosen.
-spec lifetime_classes() -> #{non_neg_integer() => pos_integer()}.
lifetime_classes() ->
    #{0 => ?PROCEDURE_ADVERTISEMENT_MAX_LIFETIME_MS, 1 => ?PROCEDURE_DELEGATION_MAX_LIFETIME_MS,
      2 => ?REALM_AND_ORG_MAX_LIFETIME_MS, 3 => ?NODE_RECORD_MAX_LIFETIME_MS, 4 => ?DOMAIN_RECORD_MAX_LIFETIME_MS,
      5 => ?DEFAULT_MAX_LIFETIME_MS}.

%% @doc The type tag reserved for the successor (key-rotation) record (macula#83). No reader takes it yet.
-spec type_successor() -> type_tag().
type_successor() -> ?TYPE_SUCCESSOR.

%% @doc The lifetime class a type's records name in their tbs: one for each 1.0 built-in type other than the tombstone
%% (which names its withdrawn type's), the reserved successor tag and the domain types; none for any other tag, whose
%% records are bounded by the class they name.
-spec type_lifetime_class(type_tag()) -> non_neg_integer() | none.
type_lifetime_class(Type) -> type_class(Type).

type_class(Type) when Type =:= ?TYPE_PROCEDURE_ADVERTISEMENT; Type =:= ?TYPE_STATION_ENDPOINT -> 0;
type_class(Type) when Type =:= ?TYPE_PROCEDURE_DELEGATION; Type =:= ?TYPE_APP_RECORD;
                      Type =:= ?TYPE_STATION_AUTHORIZATION ->
    1;
type_class(Type) when Type =:= ?TYPE_REALM_STATIONS; Type =:= ?TYPE_ORG_DIRECTORY -> 2;
type_class(Type) when Type =:= ?TYPE_NODE_RECORD; Type =:= ?TYPE_CONTENT_ANNOUNCEMENT -> 3;
type_class(Type) when is_integer(Type), Type >= ?DOMAIN_TYPE_MIN, Type =< 16#FF -> 4;
type_class(Type) when Type =:= ?TYPE_REALM_DIRECTORY; Type =:= ?TYPE_REALM_MEMBER_ENDORSEMENT;
                      Type =:= ?TYPE_SUCCESSOR ->
    5;
type_class(Type) when is_integer(Type), Type >= ?TYPE_FOUNDATION_SEED_LIST, Type =< ?TYPE_FOUNDATION_T3_ATTESTATION -> 5;
type_class(_Other) -> none.

%% A tombstone takes the class of the type it withdraws.
record_class(?TYPE_TOMBSTONE, #{{text, <<"withdrawn_type">>} := Withdrawn}) when Withdrawn =/= ?TYPE_TOMBSTONE ->
    type_class(Withdrawn);
record_class(?TYPE_TOMBSTONE, _Payload) ->
    none;
record_class(Type, _Payload) ->
    type_class(Type).

%% A tombstone outlives its class by twice the clock tolerance: the record it withdraws may be created up to the
%% tolerance ahead, and the tombstone outlives it by the tolerance.
class_lifetime(Class, ?TYPE_TOMBSTONE) -> maps:get(Class, lifetime_classes()) + 2 * ?CLOCK_TOLERANCE_MS;
class_lifetime(Class, _Type) -> maps:get(Class, lifetime_classes()).

%% The longest a record of Type lives, by its type's class. A type with no class takes the longest class.
max_lifetime(Type, Payload) ->
    class_lifetime(defined_class(record_class(Type, Payload)), Type).

defined_class(none) -> 5;
defined_class(Class) -> Class.

%% Whether a record's lifetime, created_at to expires_at, runs forward and fits its class. A record read from a tbs
%% names its class, which must be in the table and be its type's own where the type has one; a record built here
%% takes its type's.
lifetime(#{type := Type, created_at := Created, expires_at := Expires, lifetime_class := Class}) ->
    class_lived(Expires - Created, maps:is_key(Class, lifetime_classes()), Class, Type);
lifetime(#{type := Type, created_at := Created, expires_at := Expires, payload := Payload}) ->
    lifetime_within(Expires - Created, max_lifetime(Type, Payload)).

%% WIRE_1_0.md 5.2 step 8: a reversed lifetime, then a row outside the table, then a lifetime past the row's maximum.
class_lived(Lifetime, _Known, _Class, _Type) when Lifetime =< 0 -> lifetime_reversed;
class_lived(_Lifetime, false, _Class, _Type) -> lifetime_class_unknown;
class_lived(Lifetime, true, Class, Type) -> lifetime_within(Lifetime, class_lifetime(Class, Type)).

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

%% The tbs is decoded before the signature is checked, since alg selects the verifier (WIRE_1_0.md 5.2): nothing in
%% it but alg is acted on before the signature verifies.
checked_object(#{tbs := Tbs, signature := Signature} = Object, Profile, Now) ->
    alg_read(tbs_alg(macula_record_cbor:decode_strict(Tbs)), Object, Signature, Profile, Now).

tbs_alg({ok, #{{text, <<"alg">>} := {text, Alg}}}) when is_binary(Alg) -> {ok, Alg};
tbs_alg(_NoTextAlg) -> malformed.

alg_read(malformed, _Object, _Signature, _Profile, _Now) ->
    {error, malformed};
%% The allowlist is the verifier's own profile's alg: the other profile's is alg_unsupported before any crypto, and a
%% key not in the profile's carried form is malformed (WIRE_1_0.md 5.2 step 4).
alg_read({ok, Alg}, Object, Signature, Profile, Now) ->
    alg_registered(Alg =:= macula_signed_object:alg(Profile), Object, Signature, Profile, Now).

alg_registered(false, _Object, _Signature, _Profile, _Now) ->
    {error, alg_unsupported};
alg_registered(true, Object, Signature, Profile, Now) ->
    signed_fields(macula_signed_object:verify(?LABEL, Object, Profile), Signature, Profile, Now).

signed_fields({ok, #{key := Key, tbs := Tbs, fields := Fields}}, Signature, Profile, Now) ->
    read_record(read_tbs(Fields), Key, Tbs, Signature, Profile, Now);
signed_fields({error, _} = Refused, _Signature, _Profile, _Now) ->
    Refused.

%% A tbs holds exactly type, alg, version, schema, lifetime_class, created_at, expires_at and payload, subject on a
%% type that may carry one (a domain type, or a built-in tag that is not a 1.0 type), and ext where present. Any other
%% field is malformed.
read_tbs(#{{text, <<"type">>} := Type, {text, <<"alg">>} := {text, Alg}, {text, <<"version">>} := Version,
           {text, <<"schema">>} := Schema, {text, <<"lifetime_class">>} := Class,
           {text, <<"created_at">>} := Created, {text, <<"expires_at">>} := Expires,
           {text, <<"payload">>} := Payload} = Fields)
  when is_integer(Type), Type >= 1, Type =< 16#FF, is_binary(Alg), is_binary(Version), byte_size(Version) =:= 16,
       is_integer(Schema), Schema >= 1, Schema =< ?MAX_SCHEMA, is_integer(Class), Class >= 0,
       Class < ?MAX_PROTOCOL_INT,
       is_integer(Created), Created >= 0, Created < ?MAX_PROTOCOL_INT, is_integer(Expires), Expires >= 0,
       Expires < ?MAX_PROTOCOL_INT, is_map(Payload) ->
    Record = #{type => Type, alg => Alg, version => Version, schema => Schema, lifetime_class => Class,
               created_at => Created, expires_at => Expires, payload => Payload},
    optional_read(maps:without(?TBS_CORE_KEYS, Fields), Record);
read_tbs(_Fields) ->
    malformed.

optional_read(Optional, Record) ->
    optional_known(maps:size(maps:without([{text, <<"subject">>}, {text, <<"ext">>}], Optional)) =:= 0,
                   maps:get({text, <<"subject">>}, Optional, undefined), maps:get({text, <<"ext">>}, Optional, undefined),
                   Record).

optional_known(false, _Subject, _Ext, _Record) -> malformed;
optional_known(true, Subject, Ext, Record) -> with_ext(ext_shaped(Ext), Ext, subject_read(Subject, Record)).

subject_read(undefined, Record) ->
    {ok, Record};
subject_read(Subject, #{type := Type} = Record) when is_binary(Subject), byte_size(Subject) > 0 ->
    subject_carried(subject_type(Type), Record#{subject => Subject});
subject_read(_NotASubject, _Record) ->
    malformed.

subject_carried(true, Record) -> {ok, Record};
subject_carried(false, _Record) -> malformed.

with_ext(_Shaped, _Ext, malformed) -> malformed;
with_ext(false, _Ext, _Read) -> malformed;
with_ext(true, undefined, Read) -> Read;
with_ext(true, Ext, {ok, Record}) -> {ok, Record#{ext => Ext}}.

%% An ext map (WIRE_1_0.md 3.2): 1 to 16 entries, each a name of 1 to 64 bytes of [a-z0-9_.] to exactly
%% {critical: 0 or 1, value: any}. Absent is fine; empty is malformed, so one object has one encoding.
ext_shaped(undefined) ->
    true;
ext_shaped(Ext) when is_map(Ext), map_size(Ext) >= 1, map_size(Ext) =< 16 ->
    lists:all(fun ext_entry_shaped/1, maps:to_list(Ext));
ext_shaped(_NotAnExt) ->
    false.

ext_entry_shaped({{text, Name}, #{{text, <<"critical">>} := Critical, {text, <<"value">>} := _} = Entry})
  when is_binary(Name), byte_size(Name) >= 1, byte_size(Name) =< 64, map_size(Entry) =:= 2,
       (Critical =:= 0 orelse Critical =:= 1) ->
    lists:all(fun(C) -> (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse C =:= $_ orelse C =:= $. end,
              binary_to_list(Name));
ext_entry_shaped(_Entry) ->
    false.

%% An ext entry this reader does not know: every entry, since records define no extension yet. A critical one refuses
%% the record; a non-critical one is ignored.
critical_ext(#{ext := Ext}) ->
    lists:any(fun(#{{text, <<"critical">>} := Critical}) -> Critical =:= 1 end, maps:values(Ext));
critical_ext(_NoExt) ->
    false.

%% The built-in types of wire 1.0: their payload rules and slots are this module's. Every other built-in tag is one a
%% custodian stores under its signer and subject.
built_in_1_0(Type) ->
    lists:member(Type, [?TYPE_NODE_RECORD, ?TYPE_REALM_DIRECTORY, ?TYPE_REALM_STATIONS, ?TYPE_REALM_MEMBER_ENDORSEMENT,
                        ?TYPE_PROCEDURE_ADVERTISEMENT, ?TYPE_TOMBSTONE, ?TYPE_FOUNDATION_SEED_LIST,
                        ?TYPE_FOUNDATION_PARAMETER, ?TYPE_FOUNDATION_REALM_TRUST_LIST, ?TYPE_FOUNDATION_T3_ATTESTATION,
                        ?TYPE_CONTENT_ANNOUNCEMENT, ?TYPE_STATION_ENDPOINT, ?TYPE_ORG_DIRECTORY,
                        ?TYPE_PROCEDURE_DELEGATION, ?TYPE_APP_RECORD, ?TYPE_STATION_AUTHORIZATION]).

domain_tag(Type) -> is_integer(Type) andalso Type >= ?DOMAIN_TYPE_MIN andalso Type =< 16#FF.

%% A type that may carry a subject: a domain type, or a built-in tag that is not a 1.0 type, whose slot is its
%% signer's key id and its subject (macula#83 A3).
subject_type(Type) -> domain_tag(Type) orelse not built_in_1_0(Type).

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

lived(ok, #{type := Type, payload := Payload, lifetime_class := Class} = Record, _Profile) ->
    rowed(row_matches(record_class(Type, Payload), Class), Record);
lived(Refusal, _Record, _Profile) ->
    {error, Refusal}.

%% WIRE_1_0.md 5.2, after step 8: a known type names exactly its row (lifetime_class_mismatch); a tag with no row is
%% bounded by the row it names.
rowed(true, Record) -> {ok, Record};
rowed(false, _Record) -> {error, lifetime_class_mismatch}.

%% A reader's checks past the custodian's (WIRE_1_0.md 5.2, steps 10 to 12): the type, the schema; an unknown critical
%% extension; the payload rules, the content id's tag and the named signer, in that order.
read_checked({ok, #{type := Type, payload := Payload} = Record}, Profile) ->
    type_read(known_type(Type, Payload), Record, Profile);
read_checked({error, _} = Refused, _Profile) ->
    Refused.

%% A tombstone is known as far as the type it withdraws (WIRE_1_0.md 5.1): a custodian stores one withdrawing a tag it
%% does not know, and a reader that does not know that tag refuses it.
known_type(?TYPE_TOMBSTONE, #{{text, <<"withdrawn_type">>} := Withdrawn}) when Withdrawn =/= ?TYPE_TOMBSTONE ->
    known_type(Withdrawn, #{});
known_type(Type, _Payload) ->
    built_in_1_0(Type) orelse domain_tag(Type).

type_read(false, _Record, _Profile) -> {error, type_unknown};
type_read(true, #{type := Type, schema := Schema} = Record, Profile) ->
    schema_read(domain_tag(Type) orelse Schema =:= ?SCHEMA_V1, Record, Profile).

schema_read(false, _Record, _Profile) -> {error, schema_unsupported};
schema_read(true, Record, Profile) ->
    ext_read(critical_ext(Record), Record, Profile).

%% A type with a row names exactly that row; a tombstone that withdraws no type with one names any.
row_matches(none, _Class) -> true;
row_matches(Row, Class) -> Row =:= Class.

ext_read(true, _Record, _Profile) -> {error, extension_unsupported};
ext_read(false, #{type := Type, payload := Payload} = Record, Profile) ->
    payload_read(payload_ok(Type, Payload), Record, Profile).

payload_read(false, _Record, _Profile) ->
    {error, malformed};
payload_read(true, #{type := Type, payload := Payload} = Record, Profile) ->
    content_id_read(content_id_known(Type, field(Payload, <<"mcid">>)), Record, Profile).

%% Content ids are read by their tag byte; tag 2 is the one wire 1.0 knows. A custodian stores the others.
content_id_known(?TYPE_CONTENT_ANNOUNCEMENT, <<Tag, _/binary>>) -> Tag =:= 2;
content_id_known(_Type, _Mcid) -> true.

content_id_read(false, _Record, _Profile) -> {error, mcid_tag_unknown};
content_id_read(true, Record, Profile) ->
    payload_checked(true, Record, Profile).

%% A custodian's check past the lifetime: the record is placeable, and nothing else in its payload is read.
custodian_checked({ok, #{type := Type, payload := Payload, key := Key} = Record}, Profile) ->
    placed(placeable(Type, Payload, maps:get(subject, Record, undefined)),
           Record#{key_id => key_id_of(signer_kind(Type, Payload), Key, Profile)});
custodian_checked({error, _} = Refused, _Profile) ->
    Refused.

placed(true, Record) -> {ok, Record};
placed(subject_missing, _Record) -> {error, subject_missing};
placed(false, _Record) -> {error, malformed}.

%% Whether a custodian can place a record: a 1.0 type's slot fields are present and typed (a tombstone's are the
%% withdrawn type's), a domain type is placed by its signer and optional subject, and any other built-in tag carries
%% a subject.
placeable(?TYPE_TOMBSTONE, #{{text, <<"withdrawn_type">>} := Withdrawn} = Payload, _Subject)
  when is_integer(Withdrawn), Withdrawn >= 1, Withdrawn =< 16#FF, Withdrawn =/= ?TYPE_TOMBSTONE ->
    placeable(Withdrawn, Payload, maps:get({text, <<"subject">>}, Payload, undefined));
placeable(?TYPE_TOMBSTONE, _Payload, _Subject) ->
    false;
placeable(Type, Payload, Subject) ->
    slot_placeable(built_in_1_0(Type), domain_tag(Type), Type, Payload, Subject).

slot_placeable(true, _Domain, Type, Payload, _Subject) ->
    lists:all(fun(Name) -> custodian_slot_value_ok(Name, field(Payload, Name)) end, slot_field_names(Type, undefined));
slot_placeable(false, true, _Type, _Payload, Subject) ->
    Subject =:= undefined orelse non_empty_subject(Subject);
slot_placeable(false, false, _Type, _Payload, undefined) ->
    subject_missing;
slot_placeable(false, false, _Type, _Payload, Subject) ->
    non_empty_subject(Subject).

%% A content id is placed by its bytes whatever its tag, so a tag a reader does not know is still stored.
custodian_slot_value_ok(<<"mcid">>, Value) -> is_binary(Value) andalso byte_size(Value) > 0;
custodian_slot_value_ok(Name, Value) -> slot_value_ok(Name, Value).

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
payload_ok(?TYPE_STATION_AUTHORIZATION, P) -> station_authorization_payload_ok(P);
payload_ok(?TYPE_PROCEDURE_ADVERTISEMENT, P) -> advertisement_payload_ok(P);
payload_ok(?TYPE_TOMBSTONE, P) -> tombstone_payload_ok(P);
payload_ok(?TYPE_FOUNDATION_SEED_LIST, _P) -> true;
payload_ok(?TYPE_FOUNDATION_PARAMETER, P) -> is_text(field(P, <<"param_name">>));
payload_ok(?TYPE_FOUNDATION_REALM_TRUST_LIST, P) -> trust_list_payload_ok(P);
payload_ok(?TYPE_FOUNDATION_T3_ATTESTATION, P) -> is_id(field(P, <<"station_id">>));
payload_ok(?TYPE_CONTENT_ANNOUNCEMENT, P) ->
    is_id(field(P, <<"announcer_node">>)) andalso content_id_shaped(field(P, <<"mcid">>))
        andalso is_id(field(P, <<"realm_id">>)) andalso is_id(field(P, <<"serving_station">>))
        andalso is_text_field(field(P, <<"procedure">>));
payload_ok(?TYPE_STATION_ENDPOINT, _P) -> true;
payload_ok(?TYPE_ORG_DIRECTORY, P) ->
    is_id(field(P, <<"realm_id">>)) andalso is_text(field(P, <<"org_name">>)) andalso is_id(field(P, <<"org_key">>));
payload_ok(?TYPE_PROCEDURE_DELEGATION, P) -> is_id(field(P, <<"org_key">>)) andalso is_id(field(P, <<"advertiser">>));
payload_ok(?TYPE_APP_RECORD, P) -> app_payload_ok(P);
payload_ok(Type, _P) when Type >= ?DOMAIN_TYPE_MIN -> true;
payload_ok(_UnknownType, _P) -> false.

%% A station authorization holds the fields the design pins: realm_id and station_node (32-byte ids), hostname
%% (text of 1 to ?MAX_STATION_HOSTNAME_BYTES bytes), roles (texts of 1 to ?MAX_STATION_ROLE_BYTES bytes each), and
%% valid_from/valid_until (protocol integers, ending at or after they start, within the 30-minute window), plus the
%% optional attestation pair under the co-presence bound.
station_authorization_payload_ok(P) ->
    is_id(field(P, <<"realm_id">>)) andalso is_id(field(P, <<"station_node">>))
        andalso station_authorization_hostname_ok(field(P, <<"hostname">>))
        andalso station_authorization_roles_ok(field(P, <<"roles">>))
        andalso station_authorization_window_ok(field(P, <<"valid_from">>), field(P, <<"valid_until">>))
        andalso attestation_pair_ok({field(P, <<"attestation_required">>),
                                     field(P, <<"attestation_key_id">>)}).

station_authorization_hostname_ok({text, Host}) when is_binary(Host), byte_size(Host) > 0,
                                                     byte_size(Host) =< ?MAX_STATION_HOSTNAME_BYTES ->
    true;
station_authorization_hostname_ok(_Other) -> false.

station_authorization_roles_ok(Roles) when is_list(Roles) ->
    lists:all(fun({text, Role}) when is_binary(Role), byte_size(Role) > 0,
                                       byte_size(Role) =< ?MAX_STATION_ROLE_BYTES -> true;
                 (_Other) -> false
              end, Roles);
station_authorization_roles_ok(_Other) -> false.

%% The payload's own window: integers in the protocol range, ending at or after they start and inside the maximum.
station_authorization_window_ok(From, Until)
  when is_integer(From), From >= 0, From < ?MAX_PROTOCOL_INT,
       is_integer(Until), Until >= 0, Until < ?MAX_PROTOCOL_INT,
       Until >= From, Until - From =< ?STATION_AUTHORIZATION_MAX_WINDOW_MS -> true;
station_authorization_window_ok(_From, _Until) -> false.

%% An app record holds exactly its seven fields (macula#75). Its org is a real org namespace, not `_' and not a node's
%% `~<node_id>', and every procedure it lists is in that namespace, so an org cannot present another org's providers, or
%% a bare node's, as part of its app. Names and version are text of 1 to ?MAX_APP_TEXT_BYTES bytes; service names are
%% unique; the authorization is the carried org directory, as bytes, and nothing else.
app_payload_ok(#{{text, <<"realm_id">>} := <<_:256>>, {text, <<"org_name">>} := {text, OrgName},
                 {text, <<"app_name">>} := {text, AppName}, {text, <<"version">>} := {text, Version},
                 {text, <<"org_key">>} := <<_:256>>, {text, <<"services">>} := Services,
                 {text, <<"authorization">>} := #{{text, <<"org_directory">>} := Directory} = Authorization} = P)
  when map_size(P) =:= 7, map_size(Authorization) =:= 1, is_binary(Directory), is_list(Services) ->
    app_org(OrgName) andalso app_text(AppName) andalso nomatch =:= binary:match(AppName, <<"/">>)
        andalso app_text(Version) andalso app_services(Services, OrgName);
app_payload_ok(_P) ->
    false.

app_org(<<"_">>) -> false;
app_org(<<"~", _/binary>>) -> false;
app_org(OrgName) -> app_text(OrgName) andalso nomatch =:= binary:match(OrgName, <<"/">>).

app_text(Text) -> is_binary(Text) andalso byte_size(Text) > 0 andalso byte_size(Text) =< ?MAX_APP_TEXT_BYTES.

app_services(Services, OrgName) ->
    Names = [app_service_name(S, OrgName) || S <- Services],
    not lists:member(invalid, Names) andalso length(lists:usort(Names)) =:= length(Names).

app_service_name(#{{text, <<"name">>} := {text, Name}, {text, <<"procedures">>} := Procedures} = S, OrgName)
  when map_size(S) =:= 2, is_list(Procedures) ->
    app_service_named(app_text(Name) andalso lists:all(fun(Pr) -> in_org(Pr, OrgName) end, Procedures), Name);
app_service_name(_Malformed, _OrgName) ->
    invalid.

app_service_named(true, Name) -> Name;
app_service_named(false, _Name) -> invalid.

in_org({text, Procedure}, OrgName) when is_binary(Procedure) -> procedure_org(Procedure) =:= {org, OrgName};
in_org(_NotText, _OrgName) -> false.

advertisement_payload_ok(#{{text, <<"realm_id">>} := <<_:256>>, {text, <<"procedure">>} := {text, Procedure},
                           {text, <<"advertiser_node">>} := <<_:256>>, {text, <<"serving_stations">>} := Stations} = P)
  when is_binary(Procedure) ->
    serving_stations_ok(Stations)
        andalso advertisement_size_ok(map_size(P), maps:get({text, <<"authorization">>}, P, absent), kem_key_pair(P));
advertisement_payload_ok(_P) ->
    false.

%% An advertisement names 1 to 8 serving stations, 32-byte node_ids in strictly ascending order, so one set of stations
%% has one encoding and no station is named twice (macula-station#55).
serving_stations_ok(Stations) when is_list(Stations), Stations =/= [], length(Stations) =< ?MAX_SERVING_STATIONS ->
    lists:all(fun is_id/1, Stations) andalso strictly_ascending(Stations);
serving_stations_ok(_NotAStationList) ->
    false.

strictly_ascending([A, B | Rest]) -> A < B andalso strictly_ascending([B | Rest]);
strictly_ascending(_ShortList) -> true.

%% The four fields every advertisement carries, then an authorization map and a KEM key pair, each present or not.
advertisement_size_ok(Size, Authorization, KemKeyPair) when KemKeyPair =/= malformed ->
    Size =:= 4 + authorization_fields(Authorization) + KemKeyPair andalso authorization_ok(Authorization);
advertisement_size_ok(_Size, _Authorization, malformed) ->
    false.

authorization_fields(absent) -> 0;
authorization_fields(_Authorization) -> 1.

authorization_ok(absent) -> true;
authorization_ok(Authorization) -> is_map(Authorization).

%% A provider's KEM key and its id travel only as a pair (E2E design, amendment A1): the key as carried, of a profile's
%% size, and its id, both as macula_seal defines them, so the id an advertisement names is the id every sealed payload
%% and every refusal names. The two fields count 2, their absence 0; a lone field, a key of another size or an id
%% that is not its key's is malformed.
kem_key_pair(#{{text, <<"kem_key">>} := KemKey, {text, <<"kem_key_id">>} := KemKeyId})
  when is_binary(KemKey), is_binary(KemKeyId) ->
    kem_key_matched(lists:member(byte_size(KemKey), kem_key_sizes()) andalso kem_key_id(KemKey) =:= KemKeyId);
kem_key_pair(P) ->
    kem_key_absent(maps:is_key({text, <<"kem_key">>}, P) orelse maps:is_key({text, <<"kem_key_id">>}, P)).

kem_key_matched(true) -> 2;
kem_key_matched(false) -> malformed.

kem_key_absent(false) -> 0;
kem_key_absent(true) -> malformed.

kem_key_id(KemKey) ->
    macula_seal:key_id(KemKey).

kem_key_sizes() ->
    [macula_seal:carried_key_size(Profile) || Profile <- [pq_pure, pq_hybrid]].

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

slot_value_ok(Name, Value) when Name =:= <<"procedure">>; Name =:= <<"param_name">>; Name =:= <<"org_name">>;
                                 Name =:= <<"app_name">> ->
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

%% A content id as a reader's payload rules take it: tag 2 with its 48-byte digest, or any other tag with a codec,
%% which the reader then refuses by its tag (mcid_tag_unknown).
content_id_shaped(<<2, _/binary>> = Mcid) -> is_content_id(Mcid);
content_id_shaped(<<_Tag, _Codec, _/binary>>) -> true;
content_id_shaped(_Other) -> false.

%%------------------------------------------------------------------
%% Internals: slots and storage keys
%%------------------------------------------------------------------

%% The payload fields a type's storage key derives from, other than the signer's key id.
slot_field_names(?TYPE_REALM_DIRECTORY, _Subject) -> [<<"realm_id">>];
slot_field_names(?TYPE_REALM_STATIONS, _Subject) -> [<<"realm_id">>];
slot_field_names(?TYPE_REALM_MEMBER_ENDORSEMENT, _Subject) -> [<<"realm_id">>, <<"member_node">>];
slot_field_names(?TYPE_STATION_AUTHORIZATION, _Subject) -> [<<"realm_id">>, <<"station_node">>];
slot_field_names(?TYPE_PROCEDURE_ADVERTISEMENT, _Subject) -> [<<"realm_id">>, <<"procedure">>];
slot_field_names(?TYPE_FOUNDATION_PARAMETER, _Subject) -> [<<"param_name">>];
slot_field_names(?TYPE_FOUNDATION_T3_ATTESTATION, _Subject) -> [<<"station_id">>];
slot_field_names(?TYPE_CONTENT_ANNOUNCEMENT, _Subject) -> [<<"mcid">>];
slot_field_names(?TYPE_ORG_DIRECTORY, _Subject) -> [<<"realm_id">>, <<"org_name">>];
slot_field_names(?TYPE_PROCEDURE_DELEGATION, _Subject) -> [<<"advertiser">>];
slot_field_names(?TYPE_APP_RECORD, _Subject) -> [<<"realm_id">>, <<"org_name">>, <<"app_name">>];
slot_field_names(Type, Subject) when Type >= ?DOMAIN_TYPE_MIN, is_binary(Subject) -> [<<"subject">>];
slot_field_names(_SignerSlot, _Subject) -> [].

slot_fields(Names, Payload, Subject) ->
    maps:from_list([{{text, Name}, slot_value(Name, Payload, Subject)} || Name <- Names]).

slot_value(<<"subject">>, _Payload, Subject) -> Subject;
slot_value(Name, Payload, _Subject) -> maps:get({text, Name}, Payload).

slot_field_atoms() ->
    [{<<"realm_id">>, realm_id}, {<<"member_node">>, member_node}, {<<"station_node">>, station_node},
     {<<"procedure">>, procedure},
     {<<"param_name">>, param_name}, {<<"station_id">>, station_id}, {<<"mcid">>, mcid},
     {<<"org_name">>, org_name}, {<<"advertiser">>, advertiser}, {<<"app_name">>, app_name},
     {<<"subject">>, subject}].

slot(?TYPE_NODE_RECORD, _P, _Subject, <<_:256>> = KeyId) ->
    KeyId;
slot(?TYPE_REALM_DIRECTORY, P, _Subject, _KeyId) ->
    derived(?TYPE_REALM_DIRECTORY, [field(P, <<"realm_id">>)]);
slot(?TYPE_REALM_STATIONS, P, _Subject, _KeyId) ->
    derived(?TYPE_REALM_STATIONS, [field(P, <<"realm_id">>)]);
slot(?TYPE_REALM_MEMBER_ENDORSEMENT, P, _Subject, _KeyId) ->
    derived(?TYPE_REALM_MEMBER_ENDORSEMENT, [field(P, <<"realm_id">>), field(P, <<"member_node">>)]);
slot(?TYPE_STATION_AUTHORIZATION, P, _Subject, _KeyId) ->
    derived(?TYPE_STATION_AUTHORIZATION, [field(P, <<"realm_id">>), field(P, <<"station_node">>)]);
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
    derived(?TYPE_CONTENT_ANNOUNCEMENT, [{field, field(P, <<"mcid">>)}]);
slot(?TYPE_STATION_ENDPOINT, _P, _Subject, <<_:256>> = KeyId) ->
    station_endpoint_key(KeyId);
slot(?TYPE_ORG_DIRECTORY, P, _Subject, _KeyId) ->
    org_directory_key(field(P, <<"realm_id">>), text(field(P, <<"org_name">>)));
slot(?TYPE_PROCEDURE_DELEGATION, P, _Subject, <<_:256>> = KeyId) ->
    procedure_delegation_key(KeyId, field(P, <<"advertiser">>));
slot(?TYPE_APP_RECORD, P, _Subject, _KeyId) ->
    app_key(field(P, <<"realm_id">>), text(field(P, <<"org_name">>)), text(field(P, <<"app_name">>)));
slot(Type, _P, undefined, <<_:256>> = KeyId) when Type >= ?DOMAIN_TYPE_MIN ->
    derived(Type, [KeyId]);
%% A domain type with a subject, and any built-in tag wire 1.0 does not define: H(type | signer key id | subject).
slot(Type, _P, Subject, <<_:256>> = KeyId) when is_binary(Subject) ->
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

station_authorization_payload(RealmId, Node, Hostname, Roles, ValidFrom, ValidUntil, Opts) ->
    Base = #{{text, <<"realm_id">>}     => RealmId,
             {text, <<"station_node">>} => Node,
             {text, <<"hostname">>}     => {text, Hostname},
             {text, <<"roles">>}        => [{text, Role} || Role <- Roles, is_binary(Role)],
             {text, <<"valid_from">>}   => ValidFrom,
             {text, <<"valid_until">>}  => ValidUntil},
    with_id(with_uint(Base, <<"attestation_required">>, maps:get(attestation_required, Opts, undefined)),
            <<"attestation_key_id">>, maps:get(attestation_key_id, Opts, undefined)).

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
