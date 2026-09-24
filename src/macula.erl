%%%-------------------------------------------------------------------
%%% @doc Macula SDK — Public API for mesh applications.
%%%
%%% This is the main entry point for applications using the Macula SDK.
%%%
%%% Apps connect via `connect/2', which returns a `macula_client'
%%% pool that internally wraps N peering links to N stations.
%%% `publish/4,5', `subscribe/4,5', `unsubscribe/2', `call/5,6', `providers/3,4',
%%% `advertise/5', `unadvertise/3', `call_stream/5',
%%% `advertise_stream/5', and `unadvertise_stream/3' route through
%%% the pool with realm-per-call semantics. See `macula_pubsub' for
%%% the slice module of the publish/subscribe surface.
%%%
%%% LOCAL streaming (`call_stream/2,3', `open_stream/3,4',
%%% `advertise_stream/2,3', `unadvertise_stream/1') dispatches
%%% in-process via `macula_stream_local' — for unit tests and
%%% same-BEAM pairs.
%%%
%%% Erlang distribution over the mesh ships via `join_mesh/1' (V2
%%% pool carrier) or `join_dist_relay/1' (dedicated dist relay). See
%%% `macula_dist_pool' / `macula_dist_relay_client'.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula).

-include_lib("kernel/include/logger.hrl").

%% Connection
-export([connect/2, close/1, child_spec/3, status/1, links/1]).

%% Reading fields of peer-supplied maps (D26)
-export([field/2, field/3, text/1]).

%% Pub/Sub — realm-per-call against a V2 pool
-export([subscribe/4, subscribe/5,
         subscribe_callback/4,
         unsubscribe/2,
         publish/4, publish/5]).

%% RPC — realm-per-call against a V2 pool
-export([call/5,
         call/6,
         providers/3,
         providers/4,
         call_station/7,
         call_station/8,
         advertise/5,
         unadvertise/3]).
%% The pool's own D25 provider authorization, resolved from the DHT and
%% verified against the realm key the pool pins — the `authorization'
%% opt a direct-dial record publish needs.
-export([provider_authorization/3, provider_authorization/4]).

%% Signed DHT records — realm-agnostic infrastructure procedures
%% (`_dht.put_record', `_dht.find_record', `_dht.find_records_by_type',
%% `_dht.records.<type>.stored'). The all-zeros realm is the SDK
%% convention for protocol-internal traffic.
-export([put_record/2,
         find_record/2,
         find_record/3,
         find_records/2,
         find_records/3,
         find_records_by_type/2,
         subscribe_records/3,
         unsubscribe_records/2]).
-export([sign_node_record/2, sign_node_record/3, sign_domain_record/2,
         withdraw_node_record/3, parse_stations/1]).

%% Content-addressed blob storage. `_content.put_block' /
%% `_content.get_block' RPCs against the relay's local content
%% store. MCID is a 50-byte binary: the hash tag (2, SHA-384), the
%% codec (16#55 raw, 16#56 manifest), then the 48-byte SHA-384 hash. The
%% relay validates the payload's hash on `put_block' and rejects mismatches.
-export([put_content/2,
         put_content_station/4, put_content_station/5,
         get_content/2,
         get_content_station/4, get_content_station/5,
         find_content_providers/2]).

%% Streaming RPC (LOCAL in-process + V2 pool, see PLAN_MACULA_STREAMING.md)
-export([
    call_stream/2, call_stream/3, call_stream/5, call_stream_station/7,
    open_stream/3, open_stream/4,
    advertise_stream/2, advertise_stream/3, advertise_stream/5,
    advertise_stream/6,
    unadvertise_stream/1, unadvertise_stream/3,
    send/2, send/3,
    recv/1, recv/2,
    close_stream/1, close_send/1,
    await_reply/1, await_reply/2,
    set_reply/2, abort/3
]).

%% Cluster (LAN)
-export([ensure_distributed/0, monitor_nodes/0, unmonitor_nodes/0]).

%% Mesh Distribution
-export([join_mesh/1, join_dist_relay/1, dist_relay_client/0]).

-ifdef(TEST).
-export([join_pool_args/1]).
%% Exports for unit tests — pure helpers that are otherwise private.
%% `verify_block_hash/2' moved to `macula_content_transfer' (Phase 1,
%% PLAN_PUSH_UPLOAD.md) along with the rest of the content-stream
%% transfer internals — see `macula_content_transfer:verify_block_hash/2'.
-export([decode_provider/2]).
-endif.

%% Types
-export_type([pool/0, realm/0, provider_io/0,
              topic/0, procedure/0,
              stream/0, stream_mode/0, stream_handler/0,
              m_record/0, record_type/0, record_key/0,
              mcid/0]).

-type pool()   :: macula_client:pool().
-type realm()  :: <<_:256>>.            %% 32-byte realm tag.
-type topic() :: binary().
-type procedure() :: binary().

%% The I/O a provider-advertisement resolution runs on, overrideable per
%% call in `advertise/5''s and `provider_authorization/4''s `Opts' (the
%% same seam discipline as `macula_direct_dial:dial_io/0'): each entry
%% defaults to the facade's own function. Tests inject stubs here rather
%% than replacing a module other processes call.
-type provider_io() :: #{
    status           => fun((pool()) -> {ok, macula_client:status()} | {error, term()}),
    find_record      => fun((pool(), record_key()) -> {ok, m_record()} | {error, term()}),
    sign_node_record => fun((pool(), m_record(), #{not_after := integer()}) ->
                                {ok, m_record()} | {error, term()}),
    realm_key        => fun((pool(), realm()) -> {ok, binary()} | none)
}.

-type stream() :: pid().
-type stream_mode() :: server_stream | client_stream | bidi.
-type stream_handler() :: fun((stream(), term()) -> any()).

-type m_record()    :: macula_record:m_record().
-type record_type() :: macula_record:type_tag().
-type record_key()  :: <<_:256>>.   %% DHT storage key — `macula_record:storage_key/1' output.

%%%===================================================================
%%% Connection — V2 (pool, since 3.11.0)
%%%===================================================================

%% @doc Connect to the Macula relay mesh and return a pool handle.
%%
%% `Seeds' is a list of station endpoints: `#{host, port, expected_node_id}'
%% maps, or URL binaries or strings. Every seed names the node_id it
%% expects, in the seed or in the `expected_node_id' option; otherwise no
%% pool starts and `{error, {seeds, expected_node_id_required}}' is
%% returned. The pool spawns one peering link per seed and routes ops with
%% replication, replay, and event dedup. Returns immediately; link
%% handshakes complete asynchronously.
%%
%% Honored opts (full reference: `macula_client:opts()'):
%% <ul>
%%   <li>`node_identity': the pool's node identity key, in the node's crypto
%%       profile. If absent, the pool uses the node's one stored identity
%%       (`macula_node_keys:node_identity/1', at the `node_identity_path'
%%       application env): loaded if stored, ground and stored once if not,
%%       and refused, never replaced, if the file exists and will not load.
%%       Every pool on the node, and every restart, is then the same node.</li>
%%   <li>`realm_trust': the realm keys the pool pins, one per realm id, as
%%       `#{RealmId => RealmKey}', each realm's public key as carried. A call
%%       trusts an org namespaced advertisement only through the key pinned
%%       for its realm. Refused as `{error, {realm_trust, invalid}}' unless
%%       every id is 32 bytes and every key is well formed for the node's
%%       crypto profile, and as `{error, {realm_trust, profile_mismatch}}'
%%       for a key of the other profile.</li>
%%   <li>`replication_factor' — links per PUBLISH (default 2, since 10.19.0).</li>
%%   <li>`capabilities' — per-link bitfield (default 0).</li>
%%   <li>`alpn' — QUIC ALPN list (default `[<<"macula">>]').</li>
%%   <li>`connect_timeout_ms' — per-link CONNECT/HELLO deadline (default 30_000).</li>
%%   <li>`dedup_sweep_ms': how often the inbound publication dedup table is swept.</li>
%%   <li>`verify' — ⚠ REFUSED in any value, here and on every seed in
%%       `Seeds', with `{error, {refused, {verify, one_verification_mode}}}'.
%%       A link trusts a station in one way only: its handshake signature
%%       under the key of its ML-DSA-87 certificate, and its identity
%%       through the handshake. There is no chain to check.</li>
%%   <li>`expected_node_id' — the station node_id the handshake must
%%       prove, for every link this pool dials.</li>
%%   <li>`pin_tls_cert' — ⚠ `true' is REFUSED, here and on every seed in
%%       `Seeds', with
%%       `{error, {refused, {pin_tls_cert, no_pin_primitive_for_mldsa87_identity}}}'.
%%       No pin primitive can express an ML-DSA-87 identity, so the option
%%       could never be honoured; it pinned nothing at any value. `false'
%%       and an absent key pass and change nothing. See macula#15.</li>
%% </ul>
%%
%% Legacy opts silently dropped (with a one-shot `logger:notice'):
%% `relays' (use the `Seeds' positional argument), `realm' (V2 is
%% realm-per-call), `site' (no V2 analog), `connections' (one link
%% per seed; add more seeds to grow the pool).
%%
%% See `macula_client' for the canonical pool implementation and
%% `macula_pubsub' for the slice module.
-spec connect([macula_client:seed()], macula_client:opts()) ->
    {ok, pool()} | {error, term()}.
connect(Seeds, Opts) when is_list(Seeds), is_map(Opts) ->
    refused(seeds_checked(Seeds, Opts),
            fun() -> macula_client:connect(Seeds, Opts) end).

%% @doc Stop a V2 pool. Every subscriber receives a final
%% `{macula_event_gone, SubRef, pool_closed}' message.
-spec close(pool()) -> ok.
close(Pool) when is_pid(Pool) ->
    macula_client:close(Pool).

%% @doc OTP child spec to drop a V2 pool into a caller's supervision
%% tree. Give `node_identity' as a loader `{Module, Function, Args}' that
%% returns `{ok, Key}'. Its `Args' say where the key is and never hold the
%% key, because a supervisor logs them when a start fails. See
%% `macula_client:child_spec/3'.
-spec child_spec(term(), [macula_client:seed()], macula_client:opts()) ->
    supervisor:child_spec().
child_spec(Id, Seeds, Opts) ->
    macula_client:child_spec(Id, Seeds, Opts).

%% @doc Aggregate health snapshot of a V2 pool. Suitable for
%% `/health' or `/status' endpoints; not for hot-loop polling. See
%% `macula_client:status/1' for the full shape.
-spec status(pool()) -> {ok, macula_client:status()}.
status(Pool) when is_pid(Pool) ->
    macula_client:status(Pool).

%% @doc Per-link snapshot of a V2 pool — one entry per spawned link
%% with its peer station `node_id' (pubkey), dial `host', `pid', and
%% `connected' flag. Use this to resolve a specific station (by pubkey
%% or hostname) to its link for targeted, per-station operations. See
%% `macula_client:links/1' for the `link_info()' shape.
-spec links(pool()) -> {ok, [macula_client:link_info()]}.
links(Pool) when is_pid(Pool) ->
    macula_client:links(Pool).

%%%===================================================================
%%% Pub/Sub — realm-per-call against a V2 pool
%%%===================================================================

%% @doc Publish to `(Realm, Topic)' on `Pool'. Equivalent to
%% `publish/5' with empty opts.
-spec publish(pool(), realm(), topic(), term()) -> ok | {error, term()}.
publish(Pool, Realm, Topic, Payload) ->
    macula_pubsub:publish(Pool, Realm, Topic, Payload).

%% @doc Publish to `(Realm, Topic)' on `Pool' with options. See
%% `macula_pubsub:publish/5' for honored opts.
-spec publish(pool(), realm(), topic(), term(), map()) ->
    ok | {error, term()}.
publish(Pool, Realm, Topic, Payload, Opts) ->
    macula_pubsub:publish(Pool, Realm, Topic, Payload, Opts).

%% @doc Subscribe `Subscriber' to `(Realm, Topic)' on `Pool'.
%% Equivalent to `subscribe/5' with empty opts.
-spec subscribe(pool(), realm(), topic(), pid()) -> {ok, reference()} | {error, {text_too_long | invalid_text, topic}}.
subscribe(Pool, Realm, Topic, Subscriber) ->
    macula_pubsub:subscribe(Pool, Realm, Topic, Subscriber).

%% @doc Subscribe `Subscriber' to `(Realm, Topic)' on `Pool' with
%% options. The `delivery' option chooses how a single publisher's
%% out-of-order arrivals are handled:
%% <ul>
%%   <li>`ordered' (default) — per-publisher FIFO by seq; out-of-order
%%       arrivals are buffered and released in order, a genuinely
%%       missing seq skipped after `order_timeout_ms' (a `connect/2'
%%       option, default 250ms). A new publisher's first facts are
%%       held for up to `order_timeout_ms', so its order starts at the
%%       lowest seq seen.</li>
%%   <li>`latest_only' — deliver only seqs newer than the highest seen
%%       for that publisher (drop stale); no buffering, no delay.</li>
%%   <li>`as_arrives' — deliver in raw arrival order; the consumer
%%       orders it itself.</li>
%% </ul>
%% Ordering state is kept per publisher, and apart for EVENTs whose
%% publisher signature did not verify.
%% See `macula_pubsub:subscribe/5'.
-spec subscribe(pool(), realm(), topic(), pid(), map()) ->
    {ok, reference()} | {error, {text_too_long | invalid_text, topic}}.
subscribe(Pool, Realm, Topic, Subscriber, Opts) ->
    macula_pubsub:subscribe(Pool, Realm, Topic, Subscriber, Opts).

%% @doc Subscribe with a callback function. The SDK spawns a small
%% receiver process internally and invokes the callback once per
%% inbound event. See `macula_pubsub:subscribe_callback/4'.
-spec subscribe_callback(pool(), realm(), topic(),
                          macula_pubsub:callback()) ->
    {ok, reference()} | {error, term()}.
subscribe_callback(Pool, Realm, Topic, Callback) ->
    macula_pubsub:subscribe_callback(Pool, Realm, Topic, Callback).

%% @doc Drop a pool subscription. Idempotent.
-spec unsubscribe(pool(), reference()) -> ok.
unsubscribe(Pool, SubRef) when is_pid(Pool), is_reference(SubRef) ->
    macula_pubsub:unsubscribe(Pool, SubRef).

%%%===================================================================
%%% RPC — realm-per-call against a V2 pool
%%%===================================================================

%% @doc Call `Procedure' in `Realm' at the provider that serves it: resolve
%% the procedure's verified advertisements, authorized against the realm key
%% the pool pinned for `Realm', reach the station a candidate names
%% directly, and call its provider there. See
%% `macula_direct_dial:call/5'. A procedure the pool's linked stations serve
%% themselves, such as `_dht.*', goes through
%% `macula_client:call_linked_station/5'.
-spec call(pool(), realm(), procedure(), term(), 1..600_000) ->
    {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs) ->
    macula_direct_dial:call(Pool, Realm, Procedure, Payload, TimeoutMs).

%% @doc As `call/5', with `Opts'. `#{provider => NodeId}' calls THAT
%% provider of `Procedure' and no other: the procedure is resolved and its
%% advertisements trust-checked exactly as `call/5' does, and only the named
%% provider's are tried. A provider with no trusted advertisement by the
%% deadline is `{error, {unresolved, provider_not_advertised}}'. To have
%% every provider of a procedure answer, list them with `providers/3,4' and
%% make one call per provider. See `macula_direct_dial:call/6'.
-spec call(pool(), realm(), procedure(), term(), 1..600_000,
           #{provider => <<_:256>>}) ->
    {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs, Opts) when is_map(Opts) ->
    macula_direct_dial:call(Pool, Realm, Procedure, Payload, TimeoutMs, Opts).

%% @doc As `providers/4', within 5 seconds.
-spec providers(pool(), realm(), procedure()) ->
    {ok, [#{provider := <<_:256>>, station := <<_:256>>}]} | {error, term()}.
providers(Pool, Realm, Procedure) ->
    providers(Pool, Realm, Procedure, 5_000).

%% @doc Who provides `Procedure' in `Realm': each provider whose
%% advertisement passes the same trust check `call/5' applies, with the
%% station it serves from, in one DHT lookup bounded by `TimeoutMs'. A
%% provider whose record has not replicated yet is not listed; ask again
%% for a fresher answer. See `macula_direct_dial:providers/4'.
-spec providers(pool(), realm(), procedure(), 1..600_000) ->
    {ok, [#{provider := <<_:256>>, station := <<_:256>>}]} | {error, term()}.
providers(Pool, Realm, Procedure, TimeoutMs) ->
    macula_direct_dial:providers(Pool, Realm, Procedure, TimeoutMs).

%% @doc Issue a CALL to `Target', a provider's node_id, at ONE specific
%% station, dialing it directly even if it is not in the pool's seed set.
%% `Station' is a seed URL (e.g. `<<"quic://[::1]:4433">>'). The pool reuses
%% an existing link or dials and monitors a new one, waits for the
%% handshake, and calls there. This is the direct-dial data path: resolve a
%% provider's serving_station and its endpoint, then reach it in one hop.
%% See `macula_client:call_station/7'.
-spec call_station(pool(), macula_client:seed(), <<_:256>>, realm(), procedure(),
                   term(), 1..600_000) -> {ok, term()} | {error, term()}.
call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs) ->
    refused(trust_options_checked(Station),
            fun() -> macula_client:call_station(Pool, Station, Target, Realm,
                                                Procedure, Payload, TimeoutMs) end).

%% @doc As `call_station/7', presenting a capability token to a gated
%% provider via `Opts' (`#{ucan_token => Token}'). Empty/absent = none.
%% Slice 7b dual-trust. `Opts' also carries the station this dial must
%% prove, `expected_node_id' (see `macula_client:call_station/9'), and may
%% set `dial_timeout_ms', how much of `TimeoutMs' the wait for a fresh
%% link's handshake may take (default: all of it). `pin_tls_cert => true'
%% is REFUSED with
%% `{error, {refused, {pin_tls_cert, no_pin_primitive_for_mldsa87_identity}}}',
%% and `verify' in any value with
%% `{error, {refused, {verify, one_verification_mode}}}' (see
%% `trust_options_checked/1').
-spec call_station(pool(), macula_client:seed(), <<_:256>>, realm(), procedure(),
                   term(), 1..600_000, map()) ->
    {ok, term()} | {error, term()}.
call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs, Opts) ->
    refused(target_checked(Station, Opts),
            fun() -> do_call_station(Pool, Station, Target, Realm, Procedure,
                                     Payload, TimeoutMs, Opts) end).

do_call_station(Pool, Station, Target, Realm, Procedure, Payload, TimeoutMs, Opts) ->
    Ucan = maps:get(ucan_token, Opts, <<>>),
    LinkOpts = maps:with([expected_node_id], Opts),
    DialTimeoutMs = maps:get(dial_timeout_ms, Opts, TimeoutMs),
    macula_client:call_station(Pool, Station, Target, Realm, Procedure, Payload,
                               TimeoutMs, Ucan, LinkOpts, DialTimeoutMs).

%% @doc Register a procedure handler on a V2 pool and advertise it:
%% the pool resolves its own D25 provider authorization — the
%% realm-signed `org_directory' and the org-signed
%% `procedure_delegation' that names the pool's node id, both fetched
%% from the DHT — verifies it against the pinned realm key, and hands
%% every link the advertisement to sign: each link signs its own,
%% naming the station it is connected to as `serving_station', and
%% sends it as an ADVERTISE frame (signed again on reconnect and on
%% link respawn, never past the chain's earlier expiry). The
%% procedure must carry an org namespace, and the pool must run a
%% provisioned identity whose delegation the org has published, and
%% pin the realm's key (`realm_trust' at connect); a missing piece
%% fails fast with `{error, {provider_authorization, _}}'.
%%
%% `Opts' `auth' sets the procedure's policy: `open' (default, serve
%% any identified caller), `{ucan_required, IssuerNodeId}' (gated to
%% tokens from one known node), or `{realm_member_required, RealmKeyId,
%% RequiredCan}' (gated to realm membership at a specific tier) -- see
%% `macula_client:auth_policy()' for the full set. `Opts' may also
%% carry `advertise', an arity-6 override for the pool fan-out (see
%% `macula_response:advertise_opts()'), and the `provider_io/0' seam
%% entries the resolution reads its DHT calls from.
-spec advertise(pool(), realm(), procedure(),
                macula_client:handler(), map()) ->
    ok | {error, term()}.
advertise(Pool, Realm, Procedure, Handler, Opts)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32 ->
    Policy = maps:get(auth, Opts, open),
    Advertise = maps:get(advertise, Opts, fun macula_client:advertise/6),
    advertise_authorized(Pool, Realm, Procedure, Opts, fun(Spec) ->
        Advertise(Pool, Realm, Procedure, Handler, Policy, Spec)
    end).

%% @doc Stop advertising a procedure on a V2 pool.
-spec unadvertise(pool(), realm(), procedure()) -> ok.
unadvertise(Pool, Realm, Procedure) ->
    macula_client:unadvertise(Pool, Realm, Procedure).

%%%===================================================================
%%% Signed DHT records (v3.3.0)
%%%===================================================================
%%%
%%% Records are typed payloads signed in the signed-object format of
%%% DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md and stored in the relay
%%% mesh's distributed hash table. A record travels as its wire form:
%%% stations store and forward its bytes as received. A record reaches
%%% a caller only after it verifies under the node's crypto profile
%%% (`macula_record:verify/2'): `find_record/2' returns the refusal of
%%% one that does not, and the list functions and `subscribe_records/3'
%%% drop it.
%%%
%%% See `macula_record' for the record shape, the typed
%%% constructors (`node_record/3', `realm_directory/3',
%%% `realm_stations/2', `procedure_advertisement/4',
%%% `content_announcement/3', `tombstone/2', and the foundation_*
%%% constructors), and `storage_key/1' for the DHT addressing rule.
%%%
%%% Two complementary retrieval paths:
%%%
%%%   - `find_record/2'           — fetch one record by its
%%%                                 `storage_key/1' output
%%%   - `find_records_by_type/2'  — list every record of a given
%%%                                 type tag
%%%
%%% Plus a live-update channel:
%%%
%%%   - `subscribe_records/3'     — receive new records of a type
%%%                                 as they are stored

%% Procedure + topic shape — hidden from API consumers but exposed
%% as documentation. The relay backend (hecate-station and successors)
%% MUST advertise these procedures and publish on the per-type
%% record-stored topic for the SDK to function. DHT traffic travels
%% under the all-zeros realm tag (protocol-internal infrastructure;
%% the same convention `macula_dist_pool' uses for tunnel frames).
-define(DHT_REALM,                     <<0:256>>).
-define(DHT_PUT_RECORD_PROC,           <<"_dht.put_record">>).
-define(DHT_FIND_RECORD_PROC,          <<"_dht.find_record">>).
-define(DHT_FIND_RECORDS_PROC,         <<"_dht.find_records">>).
-define(DHT_FIND_RECORDS_BY_TYPE_PROC, <<"_dht.find_records_by_type">>).
-define(DHT_RECORD_TIMEOUT_MS,         5_000).
-define(TYPE_CONTENT_ANNOUNCEMENT,     16#11).

%% `_content.*' procedure names, realm, and timeouts moved to
%% `macula_content_transfer' (Phase 1, PLAN_PUSH_UPLOAD.md) along with
%% the transfer logic that used them.

%% @doc Store a signed record in the mesh DHT via a V2 pool.
%%
%% Build the record via the typed constructors in `macula_record'
%% (`node_record/3,4', `content_announcement/3,4', `tombstone/2,3',
%% `realm_directory/3,4', `procedure_advertisement/4,5', etc.) and sign
%% it. A record this node signs about itself is signed by its pool,
%% which holds the node identity key: through
%% `macula_client:sign_node_record/2', a domain record (tags 0x20 to
%% 0xFF) through `macula_client:sign_domain_record/2', and the
%% tombstone of either through `macula_client:withdraw_node_record/3'.
%% A realm-, org- or
%% foundation-signed record is signed with `macula_record:sign/2' and
%% its signer's key. Pass the signed record or its wire form
%% (`macula_record:encode/1'). The record travels as its
%% wire form; the station verifies it on receipt and stores it under
%% `macula_record:storage_key/1', propagating to the K-nearest peers.
-spec put_record(pool(), m_record() | binary()) -> ok | {error, term()}.
put_record(Pool, #{key := _, tbs := _, signature := _} = Signed) when is_pid(Pool) ->
    put_record(Pool, macula_record:encode(Signed));
put_record(Pool, Wire) when is_pid(Pool), is_binary(Wire) ->
    %% Record bytes paced per pool, so a bulk writer stays under the
    %% station's STORE allowance (D28, 3.5) instead of running into stored 0.
    %% One bucket per pool is conservative for a pool of several links: its
    %% calls fan out one link at a time, and the per-link pacer in
    %% macula_station_link:put_record/3 paces a caller that writes straight
    %% through a link.
    ok = macula_store_pacer:await(Pool, byte_size(Wire)),
    classify_put(macula_client:call_linked_station(Pool, ?DHT_REALM,
                                    ?DHT_PUT_RECORD_PROC,
                                    Wire, ?DHT_RECORD_TIMEOUT_MS)).

classify_put({ok, ok})                    -> ok;
%% The reply's atom crossed the wire as text and decoded back to the
%% codec's {text, _} marker (D26 makes no atom on the wire).
classify_put({ok, {text, <<"ok">>}})      -> ok;
classify_put({ok, Reply})                 -> {error, {unexpected_reply, Reply}};
classify_put({error, _} = E)              -> E.

%% @doc Fetch a record from the mesh DHT by its
%% `macula_record:storage_key/1'.
%%
%% Returns `{error, not_found}' when no record exists at the key. The
%% record is verified under the node's crypto profile before it is
%% returned; one that does not verify returns its refusal from
%% `macula_record:verify/2', such as `{error, expired}'.
-spec find_record(pool(), record_key()) ->
    {ok, m_record()} | {error, not_found | term()}.
find_record(Pool, Key) ->
    find_record(Pool, Key, ?DHT_RECORD_TIMEOUT_MS).

%% @doc As `find_record/2', waiting at most `TimeoutMs' for the reply, for a
%% caller that bounds its work by a deadline of its own.
-spec find_record(pool(), record_key(), pos_integer()) ->
    {ok, m_record()} | {error, not_found | term()}.
find_record(Pool, Key, TimeoutMs)
  when is_pid(Pool), is_binary(Key), byte_size(Key) =:= 32,
       is_integer(TimeoutMs), TimeoutMs > 0 ->
    classify_find(macula_client:call_linked_station(Pool, ?DHT_REALM,
                                     ?DHT_FIND_RECORD_PROC,
                                     #{key => Key}, TimeoutMs)).

classify_find({ok, Wire}) when is_binary(Wire) ->
    with_profile(fun(Profile) -> macula_record:verify(Wire, Profile) end);
classify_find({ok, not_found})                -> {error, not_found};
%% The reply's atom crossed the wire as text and decoded back to the
%% codec's {text, _} marker (D26 makes no atom on the wire).
classify_find({ok, {text, <<"not_found">>}}) -> {error, not_found};
classify_find({ok, Reply})                    -> {error, {unexpected_reply, Reply}};
classify_find({error, _} = E)                 -> E.

%% @doc Fetch EVERY record stored at `Key' — the full multi-value
%% set, e.g. every `procedure_advertisement' under one procedure's
%% storage key. Where `find_record/2' returns the first record (or
%% `not_found'), this returns the whole list, empty when none.
%%
%% The relay's local store is a signer-deduped multiset: one record
%% per signing key at a storage key, so N providers of one procedure
%% return N records. Each record is verified under the node's crypto
%% profile, and a record that does not verify is dropped.
-spec find_records(pool(), record_key()) ->
    {ok, [m_record()]} | {error, term()}.
find_records(Pool, Key) ->
    find_records(Pool, Key, ?DHT_RECORD_TIMEOUT_MS).

%% @doc As `find_records/2', waiting at most `TimeoutMs' for the reply, for a
%% caller that bounds its work by a deadline of its own.
-spec find_records(pool(), record_key(), pos_integer()) ->
    {ok, [m_record()]} | {error, term()}.
find_records(Pool, Key, TimeoutMs)
  when is_pid(Pool), is_binary(Key), byte_size(Key) =:= 32,
       is_integer(TimeoutMs), TimeoutMs > 0 ->
    classify_find_list(macula_client:call_linked_station(Pool, ?DHT_REALM,
                                          ?DHT_FIND_RECORDS_PROC,
                                          #{key => Key}, TimeoutMs)).

classify_find_list({ok, Wires}) when is_list(Wires) -> verified_records(Wires);
classify_find_list({ok, Reply})    -> {error, {unexpected_reply, Reply}};
classify_find_list({error, _} = E) -> E.

%% @doc Return every record of a given type currently visible from
%% the pool's connected stations, each verified under the node's
%% crypto profile; a record that does not verify is dropped.
%%
%% Coverage depends on each station's view of the DHT — a single
%% station sees its local replicas plus whatever its peers have
%% gossiped. Aggregating across the full mesh requires querying
%% multiple stations and deduplicating by record key.
-spec find_records_by_type(pool(), record_type()) ->
    {ok, [m_record()]} | {error, term()}.
find_records_by_type(Pool, Type)
  when is_pid(Pool), is_integer(Type), Type >= 0, Type =< 255 ->
    classify_list(macula_client:call_linked_station(Pool, ?DHT_REALM,
                                     ?DHT_FIND_RECORDS_BY_TYPE_PROC,
                                     #{type => Type},
                                     ?DHT_RECORD_TIMEOUT_MS)).

classify_list({ok, Wires}) when is_list(Wires) -> verified_records(Wires);
classify_list({ok, Reply})    -> {error, {unexpected_reply, Reply}};
classify_list({error, _} = E) -> E.

%% The records among Wires that verify under the node's crypto profile;
%% the rest are dropped.
verified_records(Wires) ->
    with_profile(fun(Profile) ->
                     {ok, [Record || Wire <- Wires, {ok, Record} <- [macula_record:verify(Wire, Profile)]]}
                 end).

%% Verify with the node's crypto profile, or return the refusal of a
%% node that has none.
with_profile(Verify) ->
    profile_verified(macula_crypto_profile:configured(), Verify).

profile_verified({ok, Profile}, Verify) -> Verify(Profile);
profile_verified({error, _} = Refusal, _Verify) -> Refusal.

%% @doc Subscribe to live record-stored events filtered by type.
%%
%% The callback receives each newly-stored record of the given type
%% that verifies under the node's crypto profile. Returns a
%% subscription reference for `unsubscribe_records/2'.
%% Topic shape is `_dht.records.<type>.stored', rendered with the
%% type tag as a decimal integer for log friendliness.
-spec subscribe_records(pool(), record_type(),
                        fun((m_record()) -> any())) ->
    {ok, reference()} | {error, term()}.
subscribe_records(Pool, Type, Callback)
  when is_pid(Pool), is_integer(Type), Type >= 0, Type =< 255,
       is_function(Callback, 1) ->
    Topic = record_stored_topic(Type),
    macula_pubsub:subscribe_callback(Pool, ?DHT_REALM, Topic,
                                     wrap_record_callback(Callback)).

%% @doc Cancel a `subscribe_records/3' subscription.
-spec unsubscribe_records(pool(), reference()) -> ok.
unsubscribe_records(Pool, Ref)
  when is_pid(Pool), is_reference(Ref) ->
    macula_pubsub:unsubscribe(Pool, Ref).

%%%===================================================================
%%% Signing records this node signs about itself — the pool holds the key
%%%===================================================================

%% @doc Sign a record this node signs about itself with the pool's node
%% identity key, in the pool's own process: a node record, a procedure
%% advertisement or a content announcement that names this node. See
%% `macula_client:sign_node_record/2' for the record checks and
%% refusals. `Opts' `not_after' bounds the signed record's expiry: the
%% pool judges it on its own clock, refuses a bound already passed as
%% `{error, not_after_passed}', ends the record at the bound when the
%% bound comes before the record's lifetime runs out, and keeps the
%% built lifetime otherwise.
-spec sign_node_record(pool(), m_record()) ->
    {ok, m_record()} | {error, term()}.
sign_node_record(Pool, Record) when is_pid(Pool) ->
    macula_client:sign_node_record(Pool, Record).

-spec sign_node_record(pool(), m_record(), #{not_after := integer()}) ->
    {ok, m_record()} | {error, term()}.
sign_node_record(Pool, Record, Opts) when is_pid(Pool), is_map(Opts) ->
    macula_client:sign_node_record(Pool, Record, Opts).

%% @doc Sign a domain record (tags 0x20 to 0xFF) as this node, with the
%% pool's node identity key, in the pool's own process. See
%% `macula_client:sign_domain_record/2'.
-spec sign_domain_record(pool(), m_record()) ->
    {ok, m_record()} | {error, term()}.
sign_domain_record(Pool, Record) when is_pid(Pool) ->
    macula_client:sign_domain_record(Pool, Record).

%% @doc Withdraw a record this node signed — a node record, a procedure
%% advertisement, a content announcement or a domain record — with a
%% tombstone signed by the pool's node identity key, in the pool's own
%% process. See `macula_client:withdraw_node_record/3'.
-spec withdraw_node_record(pool(), m_record() | binary(), macula_record:reason()) ->
    {ok, m_record()} | {error, term()}.
withdraw_node_record(Pool, Withdrawn, Reason) when is_pid(Pool) ->
    macula_client:withdraw_node_record(Pool, Withdrawn, Reason).

%% @doc Parse a `MACULA_STATIONS' seed list: comma-separated
%% `<node id>@<host>:<port>' entries, the node id as 64 lowercase hex
%% characters, the host a DNS name, an IPv4 address or a bracketed IPv6
%% address, the port 1 to 65535. Returns the seeds in order, each pinned
%% to its node id. A refusal names the entry's position and what is
%% wrong with it, and carries neither the value, nor a node id, nor a
%% host. See `macula_stations:parse/1'.
-spec parse_stations(binary()) ->
    {ok, [macula_stations:seed()]} | {error, term()}.
parse_stations(Value) when is_binary(Value) ->
    macula_stations:parse(Value).

record_stored_topic(Type) ->
    iolist_to_binary([<<"_dht.records.">>,
                      integer_to_binary(Type),
                      <<".stored">>]).

%% Adapt a 1-arg `(Record) -> any()' user callback to the 3-arg
%% `(Topic, Payload, Meta) -> any()' shape `macula_pubsub' delivers.
%%
%% PubSub delivers the payload as the record's wire form (the
%% substrate's `record_fanout' publishes `macula_record:encode/1'
%% output on the `_dht.records.<type>.stored' topic). The record is
%% verified here under the node's crypto profile, so the callback
%% receives only verified records. A payload that does not verify is
%% dropped silently: surfacing it would force every user to handle
%% refusals on a protocol-internal channel.
wrap_record_callback(Fun) ->
    fun(_Topic, Payload, _Meta) -> apply_callback_with_verified(Fun, Payload) end.

apply_callback_with_verified(Fun, Payload) ->
    callback_with(with_profile(fun(Profile) -> macula_record:verify(Payload, Profile) end), Fun).

callback_with({ok, Record}, Fun) -> Fun(Record), ok;
callback_with({error, _}, _Fun) -> ok.

%%%===================================================================
%%% Content-addressed blob storage (v4.2.7+)
%%%===================================================================

-type mcid() :: <<_:400>>.


%% @doc Store `Bytes' in the mesh's content store and return its MCID
%% (Macula Content ID, 50 bytes: tag 2 for SHA-384, codec, then the 48-byte hash).
%% Content that fits in one block (`byte_size(Bytes) =&lt;
%% macula_manifest:default_chunk_size/0', 256 KiB) is sent as a
%% single `_content.put_block', and the MCID is `&lt;&lt;2, 16#55,
%% SHA-384(Bytes)&gt;&gt;'. Larger content is split
%% into chunks (`macula_manifest:create/1'), each chunk sent
%% via its own `_content.put_block', then a `content_manifest' via
%% `_content.put_manifest'; the returned MCID is the manifest's
%% (`&lt;&lt;2, 16#56, _/binary&gt;&gt;'), Merkle-rooted over every chunk. Either
%% way the station verifies each block's hash before accepting it.
%%
%% The whole transfer — every block call plus the manifest call for
%% chunked content — rides one dedicated QUIC stream on one pinned
%% pool link (see PLAN_PER_STREAM_QUIC_ISOLATION.md Phase 2), so a
%% large blob transfer no longer head-of-line-blocks other RPC/PubSub
%% traffic on the same connection.
%%
%% A thin blocking wrapper over `macula_content_transfer:start_put/2' +
%% `await/1' — see that module for the addressable form (a live pid,
%% real cancel with a peer-visible abort, pause/resume/multi-stream as
%% later phases land). PLAN_PUSH_UPLOAD.md Phase 1.
-spec put_content(pool(), binary()) -> {ok, mcid()} | {error, term()}.
put_content(Pool, Bytes) when is_pid(Pool), is_binary(Bytes) ->
    {ok, Pid} = macula_content_transfer:start_put(Pool, Bytes),
    Result = macula_content_transfer:await(Pid),
    macula_content_transfer:cancel(Pid),
    Result.

%% @doc As `put_content/2', dialing `Station' directly (reusing a live
%% link or dialing + waiting up to `TimeoutMs' for one) instead of
%% picking from the pool's existing links — the content-transfer
%% counterpart to `call_station/7'. `Station' and `TimeoutMs' mean
%% exactly what they do there; the underlying block/manifest transfer
%% has its own internal timeouts regardless of `TimeoutMs', which
%% bounds only the connect wait. See `macula_direct_dial:put_content/4'
%% to resolve a station by identity and put in one call.
-spec put_content_station(pool(), macula_client:seed(), binary(),
                          pos_integer()) -> {ok, mcid()} | {error, term()}.
put_content_station(Pool, Station, Bytes, TimeoutMs) ->
    put_content_station(Pool, Station, Bytes, TimeoutMs, #{}).

%% @doc As `put_content_station/4', naming the station this dial must prove,
%% `expected_node_id' (see `call_station/8'). `pin_tls_cert => true' and
%% `verify' are REFUSED as `call_station/8' describes.
-spec put_content_station(pool(), macula_client:seed(), binary(),
                          pos_integer(), map()) ->
    {ok, mcid()} | {error, term()}.
put_content_station(Pool, Station, Bytes, TimeoutMs, Opts) ->
    refused(target_checked(Station, Opts),
            fun() -> do_put_content_station(Pool, Station, Bytes, TimeoutMs, Opts) end).

do_put_content_station(Pool, Station, Bytes, TimeoutMs, Opts) ->
    {ok, Pid} = macula_content_transfer:start_put_station(
                  Pool, Station, Bytes, TimeoutMs, Opts),
    Result = macula_content_transfer:await(Pid),
    macula_content_transfer:cancel(Pid),
    Result.

%% @doc Fetch the bytes for a previously-stored MCID. Returns
%% `{error, not_found}' if no provider in the pool's reach holds a
%% copy (for chunked content, if any single chunk is unreachable).
%% Dispatches on the MCID's codec byte: `16#55' (raw/single-block)
%% fetches one block, BLAKE3-verified by the station before it leaves
%% the store; `16#56' (manifest) fetches the manifest, then every
%% chunk in order, reassembles, and verifies the whole against the
%% manifest's size and Merkle root before returning.
%%
%% A thin blocking wrapper over `macula_content_transfer:start_get/2' +
%% `await/1' — see the note on `put_content/2'.
%% `MCID' must carry one of the two codec bytes `put_content/2' ever
%% mints (`16#55' single-block, `16#56' chunked manifest) — anything
%% else can't have come from this SDK's own put path (a corrupted
%% record, a caller's encoding bug, or hostile input on a path that
%% turns user-controlled bytes into an MCID) and is rejected here
%% rather than reaching `macula_content_transfer''s internal dispatch,
%% whose `is_chunked/2' clauses assume this shape and previously
%% crashed the calling process's linked worker on anything else.
-spec get_content(pool(), mcid()) ->
    {ok, binary()} | {error, not_found | invalid_mcid | term()}.
get_content(Pool, <<2, Codec, _:48/binary>> = MCID)
  when is_pid(Pool), (Codec =:= 16#55 orelse Codec =:= 16#56) ->
    {ok, Pid} = macula_content_transfer:start_get(Pool, MCID),
    Result = macula_content_transfer:await(Pid),
    macula_content_transfer:cancel(Pid),
    Result;
get_content(Pool, _MCID) when is_pid(Pool) ->
    {error, invalid_mcid}.

%% @doc As `get_content/2', dialing `Station' directly (reusing a live
%% link or dialing + waiting up to `TimeoutMs' for one) instead of
%% picking from the pool's existing links — the content-transfer
%% counterpart to `call_station/7'. `Station' and `TimeoutMs' mean
%% exactly what they do there; the underlying block/manifest transfer
%% has its own internal timeouts regardless of `TimeoutMs', which
%% bounds only the connect wait. See `find_content_providers/2' to
%% resolve a station to dial, or `macula_direct_dial:get_content/3' to
%% resolve-and-fetch in one call.
-spec get_content_station(pool(), macula_client:seed(), mcid(),
                          pos_integer()) ->
    {ok, binary()} | {error, not_found | term()}.
get_content_station(Pool, Station, MCID, TimeoutMs) ->
    get_content_station(Pool, Station, MCID, TimeoutMs, #{}).

%% @doc As `get_content_station/4', naming the station this dial must prove,
%% `expected_node_id' (see `call_station/8'). `pin_tls_cert => true' and
%% `verify' are REFUSED as `call_station/8' describes.
%% See `get_content/2' on why a malformed `MCID' is rejected here
%% rather than reaching `macula_content_transfer'.
-spec get_content_station(pool(), macula_client:seed(), mcid(),
                          pos_integer(), map()) ->
    {ok, binary()} | {error, not_found | invalid_mcid | term()}.
get_content_station(Pool, Station, <<2, Codec, _:48/binary>> = MCID, TimeoutMs, Opts)
  when Codec =:= 16#55 orelse Codec =:= 16#56 ->
    refused(target_checked(Station, Opts),
            fun() -> do_get_content_station(Pool, Station, MCID, TimeoutMs, Opts) end);
get_content_station(_Pool, _Station, _MCID, _TimeoutMs, _Opts) ->
    {error, invalid_mcid}.

do_get_content_station(Pool, Station, MCID, TimeoutMs, Opts) ->
    {ok, Pid} = macula_content_transfer:start_get_station(
                  Pool, Station, MCID, TimeoutMs, Opts),
    Result = macula_content_transfer:await(Pid),
    macula_content_transfer:cancel(Pid),
    Result.

%% @doc Resolve every host currently announcing an MCID: hosts that
%% stored a chunked put (`_content.put_manifest') and got
%% `content_announcement'd automatically by the station on receipt
%% (`macula_content_announcer'). `get_content/2' already reaches a
%% copy via the connected station's own 1-hop peer relay, so this is
%% for a caller that wants to know WHO holds an MCID, or to dial a
%% specific one directly with `get_content_station/4,5' — e.g. when the
%% connected station's relay hop budget does not reach the host (a
%% partial-mesh pair with no mutual peer), or to route around a
%% specific host deliberately.
%%
%% Each entry is verified under the node's crypto profile before its
%% `endpoint' is trusted: its signature, and that its signer is the
%% `announcer_node' it names, since `macula_record:verify/2' refuses a
%% record whose payload names another signer. Unverifiable records and
%% records of another type are dropped, not surfaced as errors.
%% Single-block content (put via `_content.put_block' alone) is not
%% announced: resolving its MCID returns `{ok, []}'.
-spec find_content_providers(pool(), mcid()) -> {ok, [map()]} | {error, term()}.
find_content_providers(Pool, <<2, _Codec:8, _Hash:48/binary>> = MCID) when is_pid(Pool) ->
    classify_find_providers(
      macula_client:call_linked_station(Pool, ?DHT_REALM, ?DHT_FIND_RECORDS_PROC,
                         #{key => macula_record:content_key(MCID)},
                         ?DHT_RECORD_TIMEOUT_MS)).

classify_find_providers({ok, Wires}) when is_list(Wires) ->
    with_profile(fun(Profile) -> {ok, content_providers(Wires, Profile)} end);
classify_find_providers({ok, Reply}) ->
    {error, {unexpected_reply, Reply}};
classify_find_providers({error, _} = E) ->
    E.

content_providers(Wires, Profile) ->
    lists:filtermap(fun(Wire) -> decode_provider(Wire, Profile) end, Wires).

%% A provider from a content announcement that verifies under Profile.
%% The verification covers the signer: a record merely stored under the
%% right key but signed by a node other than the `announcer_node' it
%% names is refused with key_id_mismatch, the same class of gap
%% `macula_direct_dial' closes for `station_endpoint'.
decode_provider(Signed, Profile) ->
    provider(macula_record:verify(Signed, Profile)).

provider({ok, #{type := ?TYPE_CONTENT_ANNOUNCEMENT} = Record}) ->
    {true, macula_record:read_content_announcement(Record)};
provider(_RefusedOrAnotherType) ->
    false.

%%%===================================================================
%%% Streaming RPC (v1.5.0+)
%%%===================================================================
%%%
%%% Streaming RPC ships in two phases (see PLAN_MACULA_STREAMING.md):
%%%
%%% Phase 1 (this release) — LOCAL dispatch only. Client and server
%%% halves both live in the same BEAM and are paired in-process. The
%%% public surface below is what cross-node streaming will use; only
%%% the transport behind the scenes changes in Phase 2.
%%%
%%% Phase 2 — wire STREAM_OPEN / STREAM_DATA / STREAM_END / STREAM_ERROR
%%% / STREAM_REPLY frames through QUIC, one QUIC stream per call_id.
%%%
%%% Patterns supported (gRPC taxonomy):
%%%   server_stream — single Args, streamed reply
%%%   client_stream — streamed Args, single reply
%%%   bidi          — duplex
%%%
%%% Example (server-stream):
%%%
%%%   ok = macula:advertise_stream(&lt;&lt;"foo.count"&gt;&gt;, server_stream,
%%%        fun(Stream, #{n := N}) ->
%%%             [ok = macula:send(Stream, integer_to_binary(I))
%%%              || I <- lists:seq(1, N)],
%%%             macula:close_stream(Stream)
%%%        end),
%%%   {ok, S} = macula:call_stream(&lt;&lt;"foo.count"&gt;&gt;, #{n => 5}),
%%%   drain(S).
%%%
%%% drain(S) ->
%%%     case macula:recv(S) of
%%%         {chunk, Bin} -> io:format("~s~n", [Bin]), drain(S);
%%%         eof -> ok
%%%     end.

%% @doc Open a LOCAL in-process server-stream call. Used for unit
%% tests and same-BEAM dispatch via `macula_stream_local'.
-spec call_stream(procedure(), term()) -> {ok, stream()} | {error, term()}.
call_stream(Procedure, Args) when is_binary(Procedure) ->
    call_stream(Procedure, Args, #{}).

%% @doc Open a LOCAL in-process server-stream call with options.
-spec call_stream(procedure(), term(), map()) ->
        {ok, stream()} | {error, term()}.
call_stream(Procedure, Args, Opts) when is_binary(Procedure), is_map(Opts) ->
    macula_stream_local:call_stream(Procedure, Args, Opts).

%% @doc Open a streaming RPC to `Procedure''s provider in `Realm': resolve
%% the provider through its `procedure_advertisement' and open the stream
%% at its serving station, naming the provider as the target, as
%% `call/5' does for a single-reply call. Same as
%% `macula_direct_dial:call_stream/5'. The returned stream is bound to
%% that station's link (errors with `peer_down' if the link dies; caller
%% re-opens).
%% `Opts' `ucan_token' presents a UCAN to a streaming procedure
%% advertised with an `auth' policy (see `advertise_stream/6').
%% An open whose signed STREAM_OPEN would be longer than
%% `max_stream_open_bytes' (1 MiB by default) returns
%% `{error, {open_too_large, Limit}}' without sending anything.
-spec call_stream(pool(), realm(), procedure(), term(), map()) ->
        {ok, stream()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, Opts)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure), is_map(Opts) ->
    macula_direct_dial:call_stream(Pool, Realm, Procedure, Args, Opts).

%% @doc Open a streaming RPC to `Target', a provider's node_id, by DIALING
%% a specific station directly (direct-dial): the streaming analogue of
%% `call_station/7'. Compose it with DHT resolution
%% (`find_records' -> `read_procedure_advertisement' -> `station_endpoint')
%% to reach a stream provider in one hop, exactly as a unary caller does.
%% `Opts' may set `dial_timeout_ms' (default 10_000) and a `mode'.
%% `Opts' also names the station this dial must prove, `expected_node_id'
%% (see `macula_client:call_station/8'); `pin_tls_cert => true' and `verify'
%% are REFUSED as `call_station/8' describes.
-spec call_stream_station(pool(), macula_client:seed(), <<_:256>>, realm(), procedure(),
                          term(), map()) -> {ok, stream()} | {error, term()}.
call_stream_station(Pool, Station, Target, Realm, Procedure, Args, Opts)
  when is_pid(Pool), is_binary(Target), byte_size(Target) =:= 32,
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure), is_map(Opts) ->
    refused(target_checked(Station, Opts),
            fun() -> macula_client:call_stream_station(
                       Pool, Station, Target, Realm, Procedure, Args, Opts) end).

%% @doc Open a LOCAL in-process client-stream or bidi call. Used
%% for unit tests and same-BEAM dispatch via `macula_stream_local'.
-spec open_stream(procedure(), term(), map()) ->
        {ok, stream()} | {error, term()}.
open_stream(Procedure, Args, Opts)
  when is_binary(Procedure), is_map(Opts) ->
    macula_stream_local:open_stream(Procedure, Args, Opts).

%% @doc Open a LOCAL in-process stream with explicit mode.
-spec open_stream(procedure(), term(), map(), stream_mode()) ->
        {ok, stream()} | {error, term()}.
open_stream(Procedure, Args, Opts, Mode)
  when is_binary(Procedure), is_map(Opts), is_atom(Mode) ->
    macula_stream_local:open_stream(Procedure, Args, Opts#{mode => Mode}).

%% @doc Advertise a LOCAL in-process streaming procedure
%% (default: server_stream).
-spec advertise_stream(procedure(), stream_handler()) -> ok | {error, term()}.
advertise_stream(Procedure, Handler)
  when is_binary(Procedure), is_function(Handler, 2) ->
    advertise_stream(Procedure, server_stream, Handler).

%% @doc Advertise a LOCAL in-process streaming procedure with mode.
-spec advertise_stream(procedure(), stream_mode(), stream_handler()) ->
        ok | {error, term()}.
advertise_stream(Procedure, Mode, Handler)
  when is_binary(Procedure), is_atom(Mode), is_function(Handler, 2) ->
    macula_stream_local:advertise(Procedure, Mode, Handler).

%% @doc Register a streaming procedure handler on a V2 pool. Fans out to
%% every healthy link and stores in pool state for replay on link
%% respawn. A caller reaches this provider only through a
%% `procedure_advertisement' record that names it; registering the
%% handler publishes none. See `macula_client:advertise_stream/5'.
-spec advertise_stream(pool(), realm(), procedure(),
                        stream_mode(), stream_handler()) ->
        ok | {error, term()}.
advertise_stream(Pool, Realm, Procedure, Mode, Handler)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (Mode =:= server_stream orelse Mode =:= client_stream
        orelse Mode =:= bidi),
       is_function(Handler, 2) ->
    advertise_stream(Pool, Realm, Procedure, Mode, Handler, #{}).

%% @doc As `advertise_stream/5', with `Opts'. `auth' sets the streaming
%% procedure's policy, the same set `advertise/5' takes: `open' (default),
%% `{ucan_required, IssuerNodeId}' or `{realm_member_required, RealmKeyId,
%% RequiredCan}'. A consumer presents its token with `call_stream/5''s
%% `ucan_token' opt.
-spec advertise_stream(pool(), realm(), procedure(),
                        stream_mode(), stream_handler(), map()) ->
        ok | {error, term()}.
advertise_stream(Pool, Realm, Procedure, Mode, Handler, Opts)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (Mode =:= server_stream orelse Mode =:= client_stream
        orelse Mode =:= bidi),
       is_function(Handler, 2), is_map(Opts) ->
    Policy = maps:get(auth, Opts, open),
    advertise_authorized(Pool, Realm, Procedure, Opts, fun(Spec) ->
        macula_client:advertise_stream(Pool, Realm, Procedure, Mode, Handler,
                                       Policy, Spec)
    end).

%% The provider-authorization resolution behind `advertise' and
%% `advertise_stream': the pool's own D25 chain, fetched from the DHT,
%% signed and verified before a single frame goes out. Any missing
%% piece fails fast under `provider_authorization'. What reaches the
%% pool is the advertisement spec, not the signed check: each link
%% signs its own advertisement naming the station it is connected to,
%% bounded by the chain (macula_station_link:advertisement_spec()).
advertise_authorized(Pool, Realm, Procedure, Opts, Fun) ->
    signed_provider_advertisement(Pool, Realm, Procedure, provider_io(Opts), Fun).

%% @doc Resolve this pool's own D25 provider authorization for an
%% org-namespaced `Procedure' under `Realm' — the realm-signed
%% `org_directory' and the org-signed `procedure_delegation' naming the
%% pool's node id, both fetched from the DHT and verified against the
%% realm key the pool pinned for `Realm' at connect — as the
%% `authorization' opt `macula_direct_dial:publish_advertisement/5'
%% and `macula_response:advertise_direct/6,7' take: a map of the two
%% records' encoded wire forms. `advertise/5' resolves this same chain
%% itself for the wire frame; a caller that publishes the direct-dial
%% record needs it explicitly, since the station refuses an
%% org-namespaced record without one (`no_authorization').
%%
%% Fails fast with `{error, {provider_authorization, _}}' when the
%% procedure has no org namespace, a chain piece is missing from the
%% DHT, the pool pinned no key for `Realm', or the chain does not
%% verify against that key — the same failures `advertise/5' reports.
-spec provider_authorization(pool(), realm(), procedure()) ->
    {ok, #{org_directory := binary(), procedure_delegation := binary()}} |
    {error, term()}.
provider_authorization(Pool, Realm, Procedure) ->
    provider_authorization(Pool, Realm, Procedure, #{}).

%% @doc As `provider_authorization/3', reading the resolution's DHT
%% calls from the `provider_io/0' seam entries in `Opts' (defaults to
%% the facade's own functions) — the seam `advertise/5' accepts too.
-spec provider_authorization(pool(), realm(), procedure(), provider_io()) ->
    {ok, #{org_directory := binary(), procedure_delegation := binary()}} |
    {error, term()}.
provider_authorization(Pool, Realm, Procedure, Opts) ->
    case signed_provider_advertisement(Pool, Realm, Procedure,
                                       provider_io(Opts),
                                       fun(#{authorization := A}) -> A end) of
        {error, _} = E -> E;
        Authorization -> {ok, Authorization}
    end.

provider_io(Opts) ->
    #{status           => maps:get(status, Opts, fun status/1),
      find_record      => maps:get(find_record, Opts, fun find_record/2),
      sign_node_record => maps:get(sign_node_record, Opts,
                                   fun sign_node_record/3),
      realm_key        => maps:get(realm_key, Opts,
                                   fun macula_client:realm_key/2)}.

%% The pool's own D25 chain, resolved from the DHT, signed and verified
%% against the realm key the pool pins, passed to `Fun' as the
%% advertisement spec: the verified authorization and the bound no
%% advertisement carrying it may pass. Any missing piece fails fast
%% under `provider_authorization'.
signed_provider_advertisement(Pool, Realm, Procedure, Io, Fun) ->
    case macula_record:procedure_org(Procedure) of
        none ->
            {error, {provider_authorization, no_org_namespace}};
        {error, malformed} ->
            {error, {provider_authorization, malformed_procedure}};
        {org, Org} ->
            case (maps:get(status, Io))(Pool) of
                {ok, #{self_node_id := NodeId}} ->
                    resolve_org_directory(Io, Pool, Realm, Org, Procedure,
                                          NodeId, Fun);
                {error, _} = E ->
                    E
            end
    end.

resolve_org_directory(Io, Pool, Realm, Org, Procedure, NodeId, Fun) ->
    Find = maps:get(find_record, Io),
    case Find(Pool, macula_record:org_directory_key(Realm, Org)) of
        {ok, OrgDir} ->
            #{org_key := OrgKeyId} =
                macula_record:read_org_directory(OrgDir),
            resolve_delegation(Io, Pool, Realm, Procedure, OrgDir, OrgKeyId,
                               NodeId, Fun);
        {error, not_found} ->
            {error, {provider_authorization, {org_directory, not_found}}};
        {error, _} = E ->
            {error, {provider_authorization, E}}
    end.

resolve_delegation(Io, Pool, Realm, Procedure, OrgDir, OrgKeyId, NodeId,
                   Fun) ->
    Find = maps:get(find_record, Io),
    case Find(Pool, macula_record:procedure_delegation_key(OrgKeyId, NodeId)) of
        {ok, Deleg} ->
            sign_provider_advertisement(Io, Pool, Realm, Procedure, OrgDir,
                                        Deleg, NodeId, Fun);
        {error, not_found} ->
            {error, {provider_authorization,
                     {procedure_delegation, not_found}}};
        {error, _} = E ->
            {error, {provider_authorization, E}}
    end.

%% The advertisement is signed under a bound: it must not outlive what
%% authorizes it. An advertisement is built with its type's own
%% lifetime, which knows nothing about the chain it carries, and both
%% verifiers refuse one that ends after the earlier of the org
%% directory's and the delegation's expiry —
%% `macula_record:delegation_matched/3' here, and macula-station's at
%% admission. Without the bound a provider spends the last stretch of
%% every authorization window signing advertisements its own pool
%% refuses, and goes dark before its delegation expires.
sign_provider_advertisement(Io, Pool, Realm, Procedure, OrgDir, Deleg,
                            NodeId, Fun) ->
    Authorization = #{org_directory        => macula_record:encode(OrgDir),
                      procedure_delegation => macula_record:encode(Deleg)},
    Unsigned = macula_record:procedure_advertisement(
                 NodeId, Realm, Procedure, NodeId,
                 #{authorization => Authorization}),
    NotAfter = min(macula_record:expires_at(OrgDir),
                   macula_record:expires_at(Deleg)),
    case (maps:get(sign_node_record, Io))(Pool, Unsigned,
                                          #{not_after => NotAfter}) of
        {ok, Signed} ->
            trusted_provider_advertisement(
              Io, Pool, Realm, Signed,
              fun(Verified) ->
                  #{authorization := A} =
                      macula_record:read_procedure_advertisement(Verified),
                  Fun(#{authorization => A, not_after => NotAfter})
              end);
        {error, _} = E ->
            {error, {provider_authorization, E}}
    end.

%% The advertisement goes out only after its authorization verifies
%% against the realm key this pool pins — a chain the pool cannot
%% check never reaches a station.
trusted_provider_advertisement(Io, Pool, Realm, Signed, Fun) ->
    case (maps:get(realm_key, Io))(Pool, Realm) of
        {ok, RealmKey} ->
            {ok, Profile} = macula_crypto_profile:configured(),
            Trust = #{profile => Profile, realm_key => RealmKey},
            case macula_record:verify_authorization(
                   Signed, Trust, erlang:system_time(millisecond)) of
                ok ->
                    Fun(Signed);
                {error, _} = E ->
                    {error, {provider_authorization, E}}
            end;
        none ->
            {error, {provider_authorization, no_realm_key}}
    end.

%% @doc Stop advertising a LOCAL streaming procedure.
-spec unadvertise_stream(procedure()) -> ok.
unadvertise_stream(Procedure) when is_binary(Procedure) ->
    macula_stream_local:unadvertise(Procedure).

%% @doc Stop advertising a streaming procedure on a V2 pool.
-spec unadvertise_stream(pool(), realm(), procedure()) -> ok.
unadvertise_stream(Pool, Realm, Procedure)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure) ->
    macula_client:unadvertise_stream(Pool, Realm, Procedure).

%% @doc Send a binary chunk on the stream.
-spec send(stream(), binary()) -> ok | {error, term()}.
send(Stream, Bin) when is_pid(Stream), is_binary(Bin) ->
    macula_stream:send(Stream, Bin).

%% @doc Send a chunk with explicit encoding.
-spec send(stream(), binary() | term(), raw | msgpack) -> ok | {error, term()}.
send(Stream, Body, Encoding) when is_pid(Stream) ->
    macula_stream:send(Stream, Body, Encoding).

%% @doc Receive the next chunk (blocks).
-spec recv(stream()) -> {chunk, binary()}
                      | {data, term()}
                      | eof
                      | {error, term()}.
recv(Stream) when is_pid(Stream) ->
    macula_stream:recv(Stream).

-spec recv(stream(), timeout()) -> {chunk, binary()}
                                 | {data, term()}
                                 | eof
                                 | {error, term()}.
recv(Stream, Timeout) when is_pid(Stream) ->
    macula_stream:recv(Stream, Timeout).

%% @doc Close a V1 stream (both sides). Renamed from `close/1' in
%% 3.11.0 because `close/1' now refers to the V2 pool surface.
-spec close_stream(stream()) -> ok.
close_stream(Stream) when is_pid(Stream) ->
    macula_stream:close(Stream).

%% @doc Half-close the write side; recv still drains.
-spec close_send(stream()) -> ok.
close_send(Stream) when is_pid(Stream) ->
    macula_stream:close_send(Stream).

%% @doc Wait for the terminal reply (client-stream / bidi).
-spec await_reply(stream()) -> {ok, term()} | {error, term()}.
await_reply(Stream) when is_pid(Stream) ->
    macula_stream:await_reply(Stream).

-spec await_reply(stream(), timeout()) -> {ok, term()} | {error, term()}.
await_reply(Stream, Timeout) when is_pid(Stream) ->
    macula_stream:await_reply(Stream, Timeout).

%% @doc Server-side: emit the terminal reply value.
-spec set_reply(stream(), term()) -> ok.
set_reply(Stream, Result) when is_pid(Stream) ->
    macula_stream:set_reply(Stream, Result).

%% @doc Abort the stream with an error frame.
-spec abort(stream(), binary(), binary()) -> ok | {error, {text_too_long | invalid_text, code}}.
abort(Stream, Code, Message)
  when is_pid(Stream), is_binary(Code), is_binary(Message) ->
    macula_stream:abort(Stream, Code, Message).

%%%===================================================================
%%% Cluster (LAN)
%%%===================================================================

%% @doc Ensure this node is running in distributed mode.
-spec ensure_distributed() -> ok | {error, term()}.
ensure_distributed() -> macula_cluster:ensure_distributed().

%% @doc Subscribe to node up/down events.
-spec monitor_nodes() -> ok.
monitor_nodes() -> macula_cluster:monitor_nodes().

%% @doc Unsubscribe from node up/down events.
-spec unmonitor_nodes() -> ok.
unmonitor_nodes() -> macula_cluster:unmonitor_nodes().

%%%===================================================================
%%% Mesh Distribution
%%%===================================================================

%% @doc Join the Macula relay mesh with Erlang distribution.
%%
%% After calling this, standard OTP distribution works across firewalls.
%% `Opts' takes:
%% <ul>
%%   <li>`relays' (required): the V2 pool's seeds, each a map with `host',
%%       `port' and `expected_node_id', the relay's 32-byte node_id, which
%%       every dial checks (D16). A relay without one refuses the join with
%%       `{error, {relays, expected_node_id_required}}' before any pool
%%       starts.</li>
%%   <li>`node_identity': the V2 pool's node identity key,
%%       `macula_node_keys:node_key()'. Default: the node's one stored
%%       identity, as for `connect/2'.</li>
%% </ul>
%%
%% Internally builds a V2 `macula_client:pool()' and registers it
%% with `macula_dist_pool' as the carrier for `_dist.tunnel.*'
%% traffic. Dist tunnel frames travel under the all-zeros realm
%% (protocol-internal infrastructure, not bound to any user realm).
-spec join_mesh(map()) -> ok | {error, term()}.
join_mesh(Opts) ->
    joined(join_pool_args(Opts)).

joined({ok, Relays, PoolOpts}) ->
    on_pool_for_join(macula_client:connect(Relays, PoolOpts));
joined({error, _} = Refusal) ->
    Refusal.

%% The seeds and options of the pool a join starts: the relays, when every one names the node_id it expects, and the
%% node identity key when one is given.
join_pool_args(#{relays := Relays} = Opts) ->
    joinable(seeds_checked(Relays, Opts), Relays, Opts).

%% A relay carrying `pin_tls_cert => true' or `verify' is refused here
%% rather than at link start, so a join reports it instead of leaving a pool respawning a
%% seed that can never start.
joinable(ok, Relays, Opts) ->
    pinned_relays(lists:all(fun pinned_relay/1, Relays), Relays,
                  maps:with([node_identity], Opts));
joinable({error, _} = Refusal, _Relays, _Opts) ->
    Refusal.

pinned_relays(true, Relays, PoolOpts) -> {ok, Relays, PoolOpts};
pinned_relays(false, _Relays, _PoolOpts) -> {error, {relays, expected_node_id_required}}.

pinned_relay(#{host := _, port := _, expected_node_id := <<_:256>>}) -> true;
pinned_relay(_Relay) -> false.

on_pool_for_join({ok, Pool}) ->
    wait_for_pool(Pool, 30),
    os:putenv("MACULA_DIST_MODE", "relay"),
    macula_dist_pool:register_mesh_pool(Pool),
    macula_dist_pool:advertise_dist_accept(),
    ?LOG_INFO("[macula] Joined mesh — distribution enabled"),
    ok;
on_pool_for_join({error, Reason}) ->
    ?LOG_ERROR("[macula] Failed to join mesh: ~p", [Reason]),
    {error, Reason}.

%% @doc Enable Erlang distribution over a dedicated dist relay
%% (`macula-io/macula-dist-relay').
%%
%% Different from `join_mesh/1':
%% - Connects to a dist relay (port 4434, ALPN `macula-dist'), NOT the
%%   pub/sub station mesh
%% - No mesh_client, no pub/sub subscriptions — only dist traffic
%% - Uses raw QUIC stream routing with no MessagePack overhead
%%
%% Options:
%% - `url' (required): `&lt;&lt;"quic://relay.example.com:4434"&gt;&gt;'
%%
%% After this returns `ok', standard OTP distribution (`rpc:call/4',
%% `gen_server:call/3' across nodes, `pg' groups, etc.) works across
%% firewalls via the dist relay.
%%
%% The relay client runs as a temporary child of the macula application
%% supervisor. It does not reconnect: when the relay closes the
%% connection the client ends and is not restarted. Monitor the pid from
%% `dist_relay_client/0' to learn that, then call this function again.
%% Returns `{error, macula_not_started}' when the macula application is
%% not running.
-spec join_dist_relay(map()) -> ok | {error, term()}.
join_dist_relay(Opts) ->
    Url = maps:get(url, Opts),
    NodeName = atom_to_binary(node()),
    start_dist_relay_client(whereis(macula_root), Url, NodeName).

start_dist_relay_client(undefined, _Url, _NodeName) ->
    {error, macula_not_started};
start_dist_relay_client(_Root, Url, NodeName) ->
    ChildSpec = macula_dist_relay_client:child_spec(Url, NodeName),
    case supervisor:start_child(macula_root, ChildSpec) of
        {ok, _Pid} ->
            os:putenv("MACULA_DIST_MODE", "dist_relay"),
            ?LOG_INFO("[macula] Joined dist relay ~s, distribution enabled", [Url]),
            ok;
        {error, {already_started, _Pid}} ->
            os:putenv("MACULA_DIST_MODE", "dist_relay"),
            ?LOG_INFO("[macula] dist_relay_client already running, mode set"),
            ok;
        {error, Reason} ->
            Refusal = child_start_refusal(Reason),
            ?LOG_ERROR("[macula] Failed to join dist relay: ~p", [Refusal]),
            {error, Refusal}
    end.

%% `supervisor:start_child/2' wraps a child's own refusal with the child
%% spec that failed to start. A caller of `join_dist_relay/1' wants the
%% refusal, not the spec it already gave us: the client refuses when its
%% operator has not accepted an unidentified peer
%% (`macula_dist:unidentified_peer_refusal/0'), and that term says so.
child_start_refusal({Refusal, Child}) when is_tuple(Child), element(1, Child) =:= child ->
    Refusal;
child_start_refusal(Reason) ->
    Reason.

%% @doc The dist relay client that `join_dist_relay/1' started, if it
%% is running.
%%
%% The client exits with `{relay_closed, Reason}' when the relay closes
%% the connection and is not restarted. Monitor the returned pid and call
%% `join_dist_relay/1' again after it goes down.
-spec dist_relay_client() -> {ok, pid()} | {error, not_joined}.
dist_relay_client() ->
    dist_relay_client_result(macula_dist_relay_client:whereis_client()).

dist_relay_client_result(undefined) -> {error, not_joined};
dist_relay_client_result(Pid) -> {ok, Pid}.

%% @private Wait until the V2 pool has at least one healthy
%% station_link (CONNECT/HELLO completed). One-second polling, capped
%% at `Retries' iterations.
wait_for_pool(_Pool, 0) ->
    ?LOG_WARNING("[macula] Mesh pool not ready after timeout");
wait_for_pool(Pool, Retries) ->
    on_pool_status(macula_client:status(Pool), Pool, Retries).

on_pool_status({ok, #{healthy_links := N}}, _Pool, _Retries) when N > 0 ->
    ?LOG_INFO("[macula] Mesh pool connected (~p healthy link(s))", [N]),
    ok;
on_pool_status(_Other, Pool, Retries) ->
    timer:sleep(1000),
    wait_for_pool(Pool, Retries - 1).

%%%===================================================================
%%% Peer-supplied maps (D26)
%%%===================================================================

%% @doc A field of a map a peer supplied, or `undefined' when it is absent. See `field/3'.
-spec field(atom() | binary(), map()) -> term().
field(Name, Map) ->
    field(Name, Map, undefined).

%% @doc A field of a map a peer supplied (D26), or `Default' when it is absent. A map from the codec carries its text
%% keys as `{text, Bin}'; a map handed over in process may carry atom or binary keys. The lookup tries `{text, Name}',
%% then the atom, then the binary, so a handler reads both kinds of map the same way. Looking up a binary name never
%% creates an atom.
-spec field(atom() | binary(), map(), term()) -> term().
field(Name, Map, Default) when (is_atom(Name) orelse is_binary(Name)), is_map(Map) ->
    first_found(field_keys(Name), Map, Default).

%% @doc The binary of a text value a peer supplied (D26): the binary of `{text, Bin}', a binary unchanged, and
%% `badarg' for anything else.
-spec text({text, binary()} | binary()) -> binary().
text({text, Bin}) when is_binary(Bin) -> Bin;
text(Bin) when is_binary(Bin) -> Bin;
text(Other) -> erlang:error(badarg, [Other]).

field_keys(Name) when is_atom(Name) ->
    Bin = atom_to_binary(Name),
    [{text, Bin}, Name, Bin];
field_keys(Name) when is_binary(Name) ->
    [{text, Name}] ++ existing_atom(Name) ++ [Name].

existing_atom(Name) ->
    try [binary_to_existing_atom(Name)]
    catch error:badarg -> []
    end.

first_found([], _Map, Default) -> Default;
first_found([Key | Keys], Map, Default) -> found(maps:find(Key, Map), Keys, Map, Default).

found({ok, Value}, _Keys, _Map, _Default) -> Value;
found(error, Keys, Map, Default) -> first_found(Keys, Map, Default).

%%------------------------------------------------------------------
%% `pin_tls_cert' and `verify' are refused, not accepted and ignored
%%------------------------------------------------------------------

%% Every public entry point that takes a seed, a station or a per-dial TLS
%% trust map runs this before it does anything else: `connect/2',
%% `call_station/7,8', `call_stream_station/7', `put_content_station/5',
%% `get_content_station/5' and `join_mesh/1'. `pin_tls_cert => true' is
%% refused, and `verify' in any value; `pin_tls_cert => false' and absence
%% pass through.
%%
%% WHY NEITHER CAN BE HONOURED, so the next reader does not re-litigate it.
%% A dial trusts a station in exactly one way (plan decisions D12 and D16):
%% the station presents one self-signed ML-DSA-87 certificate on its TLS
%% key, its handshake signature must verify under that key, and its
%% identity is proved by the signed handshake against `expected_node_id',
%% which is mandatory on a client dial. `pin_tls_cert' once mapped
%% `expected_node_id' onto a pinned Ed25519 key, which stopped meaning
%% anything when a node_id became a hash over an ML-DSA-87 identity key and
%% the TLS key became a key of its own; the pin primitive is gone from the
%% QUIC NIF. `verify' chose between a webpki chain check, which no ML-DSA
%% certificate can pass because no authority issues one, and no check at
%% all, which the NIF no longer offers.
%%
%% So the choice is between accepting a value that does nothing and saying
%% so. An inert security option is worse than an absent one, because a
%% reader takes it for a check that is happening: that is macula#15, and it
%% reached hexdocs as a documented default of `true' that was never true.
%% `pin_tls_cert => false' still passes, as it did through 11.x; `verify'
%% is refused in any value, since 12.0.0 is where it went.
%%
%% ⚠ EVERY map a caller can put the keys in, not just `Opts'. The seed and
%% station maps are where the options historically lived, so checking only
%% `Opts' advertises a check that does not happen on the path a reader of
%% our own CHANGELOG would take.
%%
%% The refusal is wrapped in `refused' because nothing is sent and every
%% candidate refuses it identically, which is `request' scope in the dial
%% taxonomy. Left bare it would fall to a catch-all and be classified
%% `provider', meaning "may have reached a provider", which is the wrong
%% scope for a refusal that sent nothing.
%%
%% ⚠ MAPS ONLY. A seed may be a charlist (`seed()' includes `string()'),
%% so an `is_list' clause here would walk `"quic://..."' character by
%% character and be correct only by accident, every integer falling to the
%% catch-all. The list is mapped over by `seeds_checked/2', which knows it
%% has a list OF seeds rather than a seed that happens to be a list.
-spec trust_options_checked(map() | macula_client:seed()) ->
        ok | {error, {refused, {pin_tls_cert | verify, atom()}}}.
trust_options_checked(#{pin_tls_cert := true}) ->
    {error, {refused, {pin_tls_cert, no_pin_primitive_for_mldsa87_identity}}};
trust_options_checked(#{verify := _}) ->
    {error, {refused, {verify, one_verification_mode}}};
trust_options_checked(_NoRefusedOption) ->
    ok.

%% `connect/2' and `join_mesh/1': a LIST of seeds, plus the option map.
-spec seeds_checked([macula_client:seed()], map()) ->
        ok | {error, {refused, {pin_tls_cert | verify, atom()}}}.
seeds_checked(Seeds, Opts) when is_list(Seeds) ->
    first_error([trust_options_checked(S) || S <- Seeds] ++
                  [trust_options_checked(Opts)]).

%% One station or seed, plus the option map, in the order the public
%% functions receive them.
-spec target_checked(macula_client:seed(), map()) ->
        ok | {error, {refused, {pin_tls_cert | verify, atom()}}}.
target_checked(Target, Opts) ->
    first_error([trust_options_checked(Target), trust_options_checked(Opts)]).

%% Named `first_error' and not `first_refusal': `macula_station_link' has a
%% `first_refusal/1' of its own with a different shape (it folds `none' and
%% a refusal term, not `ok' and an `{error, _}'). Two functions of the same
%% name meaning different things in one module family is the small version
%% of a contract in two places.
first_error([{error, _} = Refusal | _Rest]) -> Refusal;
first_error([ok | Rest]) -> first_error(Rest);
first_error([]) -> ok.

%% Run the call, or return the refusal that stopped it.
refused(ok, Call) -> Call();
refused({error, _} = Refusal, _Call) -> Refusal.
