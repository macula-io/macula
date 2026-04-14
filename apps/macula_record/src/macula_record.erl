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
    procedure_advertisement/3, procedure_advertisement/4,
    tombstone/3, tombstone/4,

    %% Sign / verify
    sign/2, verify/1,

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
    procedure_advertisement_opts/0,
    tombstone_opts/0
]).

%% Domain separation prefix for record signatures (Part 6 §10.2).
-define(SIG_DOMAIN, "macula-v2-record\0").

-define(TYPE_NODE_RECORD,             16#01).
-define(TYPE_REALM_DIRECTORY,          16#03).
-define(TYPE_REALM_STATIONS,           16#04).
-define(TYPE_PROCEDURE_ADVERTISEMENT,  16#06).
-define(TYPE_TOMBSTONE,                16#0C).

%% Domain separation for derived storage keys (Part 3 §3.3).
-define(STORAGE_DOMAIN_STATION_SET, <<"station_set">>).

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
    ttl_ms       => pos_integer()
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

-type procedure_advertisement_opts() :: #{
    session_token_hint => binary(),
    rate_limit_qps     => non_neg_integer(),
    max_concurrency    => non_neg_integer(),
    ttl_ms             => pos_integer()
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
                   G(<<"x">>), G(<<"p">>), G(<<"s">>));
decode_value(_Other) ->
    {error, bad_record}.

parse_envelope(T, K, V, C, X, P, S)
  when is_integer(T), T > 0,
       is_binary(K), byte_size(K) =:= 32,
       is_binary(V), byte_size(V) =:= 16,
       is_integer(C), C > 0,
       is_integer(X), X > 0,
       is_map(P),
       is_binary(S), byte_size(S) =:= 64 ->
    {ok, #{type => T, key => K, version => V,
           created_at => C, expires_at => X,
           payload => P, signature => S}};
parse_envelope(_, _, _, _, _, _, undefined) ->
    {error, missing_signature};
parse_envelope(_, _, _, _, _, _, _) ->
    {error, bad_record}.

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

envelope(Type, Key, Payload, Opts) ->
    NowMs = erlang:system_time(millisecond),
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    #{
        type       => Type,
        key        => Key,
        version    => macula_record_uuid:v7(NowMs),
        created_at => NowMs,
        expires_at => NowMs + TtlMs,
        payload    => Payload
    }.

node_payload(NodeId, StationId, Realms, Caps, Opts) ->
    Base = #{
        {text, <<"node_id">>}      => NodeId,
        {text, <<"station_id">>}   => StationId,
        {text, <<"realms">>}       => Realms,
        {text, <<"capabilities">>} => Caps
    },
    M1 = with_text(Base, <<"caps_hint">>,    maps:get(caps_hint,    Opts, undefined)),
    with_text(M1,    <<"display_name">>, maps:get(display_name, Opts, undefined)).

with_text(Map, _Key, undefined) -> Map;
with_text(Map,  Key, Bin) when is_binary(Bin) ->
    Map#{ {text, Key} => {text, Bin} }.

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
storage_key(#{type := ?TYPE_PROCEDURE_ADVERTISEMENT, payload := P}) ->
    {text, Uri} = maps:get({text, <<"procedure_uri">>}, P),
    crypto:hash(sha256, Uri).

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
    add_signature(Base, maps:get(signature, R, undefined)).

add_signature(M, undefined) -> M;
add_signature(M, Sig) when is_binary(Sig) ->
    M#{ {text, <<"s">>} => Sig }.

%% Canonical CBOR of the record minus its signature — what gets signed/verified.
canonical_unsigned(Record) ->
    macula_record_cbor:encode(to_envelope_map(maps:without([signature], Record))).
