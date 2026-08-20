%%%-------------------------------------------------------------------
%%% @doc Macula SDK — Public API for mesh applications.
%%%
%%% This is the main entry point for applications using the Macula SDK.
%%%
%%% Apps connect via `connect/2', which returns a `macula_client'
%%% pool that internally wraps N peering links to N stations.
%%% `publish/4,5', `subscribe/4,5', `unsubscribe/2', `call/5',
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
%%% `macula_dist_pool' / `macula_dist_system'.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula).

-include_lib("kernel/include/logger.hrl").

%% Connection
-export([connect/2, close/1, child_spec/3, status/1, links/1]).

%% Pub/Sub — realm-per-call against a V2 pool
-export([subscribe/4, subscribe/5,
         subscribe_callback/4,
         unsubscribe/2,
         publish/4, publish/5]).

%% RPC — realm-per-call against a V2 pool
-export([call/5,
         call_station/6,
         call_station/7,
         advertise/5,
         unadvertise/3]).

%% Signed DHT records — realm-agnostic infrastructure procedures
%% (`_dht.put_record', `_dht.find_record', `_dht.find_records_by_type',
%% `_dht.records.<type>.stored'). The all-zeros realm is the SDK
%% convention for protocol-internal traffic.
-export([put_record/2,
         find_record/2,
         find_records/2,
         find_records_by_type/2,
         subscribe_records/3,
         unsubscribe_records/2]).

%% Content-addressed blob storage. `_content.put_block' /
%% `_content.get_block' RPCs against the relay's local content
%% store. MCID is a 34-byte binary: 1 codec byte, 1 algo byte
%% (BLAKE3 = 16#55), 32-byte BLAKE3 hash. The relay validates the
%% payload's hash on `put_block' and rejects mismatches.
-export([put_content/2,
         put_content_station/4, put_content_station/5,
         get_content/2,
         get_content_station/4, get_content_station/5,
         find_content_providers/2]).

%% Streaming RPC (LOCAL in-process + V2 pool, see PLAN_MACULA_STREAMING.md)
-export([
    call_stream/2, call_stream/3, call_stream/5, call_stream_station/6,
    open_stream/3, open_stream/4,
    advertise_stream/2, advertise_stream/3, advertise_stream/5,
    unadvertise_stream/1, unadvertise_stream/3,
    send/2, send/3,
    recv/1, recv/2,
    close_stream/1, close_send/1,
    await_reply/1, await_reply/2,
    set_reply/2, abort/3
]).

%% Cluster (LAN)
-export([ensure_distributed/0, get_cookie/0, set_cookie/1,
         monitor_nodes/0, unmonitor_nodes/0]).

%% Mesh Distribution
-export([join_mesh/1, join_dist_relay/1]).

-ifdef(TEST).
%% Exports for unit tests — pure helpers that are otherwise private.
-export([verify_block_hash/2, decode_provider/1]).
-endif.

%% Types
-export_type([pool/0, realm/0,
              topic/0, procedure/0,
              stream/0, stream_mode/0, stream_handler/0,
              m_record/0, record_type/0, record_key/0,
              mcid/0]).

-type pool()   :: macula_client:pool().
-type realm()  :: <<_:256>>.            %% 32-byte realm tag.
-type topic() :: binary().
-type procedure() :: binary().

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
%% `Seeds' is a list of relay endpoints (URL binaries/strings or
%% `#{host, port}' maps). The pool spawns one peering link per seed
%% and routes ops with replication, replay, and event dedup. Returns
%% immediately; link handshakes complete asynchronously.
%%
%% Honored opts (full reference: `macula_client:opts()'):
%% <ul>
%%   <li>`identity' — pool's Ed25519 keypair; auto-generated if absent.</li>
%%   <li>`replication_factor' — links per PUBLISH (default 1).</li>
%%   <li>`capabilities' — per-link bitfield (default 0).</li>
%%   <li>`alpn' — QUIC ALPN list (default `[<<"macula">>]').</li>
%%   <li>`connect_timeout_ms' — per-link CONNECT/HELLO deadline (default 30_000).</li>
%%   <li>`dedup_window_ms', `dedup_sweep_ms' — inbound-EVENT dedup tunables.</li>
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
    macula_client:connect(Seeds, Opts).

%% @doc Stop a V2 pool. Every subscriber receives a final
%% `{macula_event_gone, SubRef, pool_closed}' message.
-spec close(pool()) -> ok.
close(Pool) when is_pid(Pool) ->
    macula_client:close(Pool).

%% @doc OTP child spec to drop a V2 pool into a caller's supervision
%% tree.
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
-spec subscribe(pool(), realm(), topic(), pid()) -> {ok, reference()}.
subscribe(Pool, Realm, Topic, Subscriber) ->
    macula_pubsub:subscribe(Pool, Realm, Topic, Subscriber).

%% @doc Subscribe `Subscriber' to `(Realm, Topic)' on `Pool' with
%% options. The `delivery' option chooses how a single publisher's
%% out-of-order arrivals are handled:
%% <ul>
%%   <li>`ordered' (default) — per-publisher FIFO by seq; out-of-order
%%       arrivals are buffered and released in order, a genuinely
%%       missing seq skipped after `order_timeout_ms' (a `connect/2'
%%       option, default 250ms).</li>
%%   <li>`latest_only' — deliver only seqs newer than the highest seen
%%       for that publisher (drop stale); no buffering, no delay.</li>
%%   <li>`as_arrives' — deliver in raw arrival order; the consumer
%%       orders it itself.</li>
%% </ul>
%% See `macula_pubsub:subscribe/5'.
-spec subscribe(pool(), realm(), topic(), pid(), map()) ->
    {ok, reference()}.
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

%% @doc Issue a CALL frame against a V2 pool. First-success across
%% the pool's healthy links. See `macula_client:call/5'.
-spec call(pool(), realm(), procedure(), term(), pos_integer()) ->
    {ok, term()} | {error, term()}.
call(Pool, Realm, Procedure, Payload, TimeoutMs) ->
    macula_client:call(Pool, Realm, Procedure, Payload, TimeoutMs).

%% @doc Issue a CALL to ONE specific station, dialing it directly even
%% if it is not in the pool's seed set. `Station' is a seed URL (e.g.
%% `<<"quic://[::1]:4433">>'). The pool reuses an existing link or dials
%% and monitors a new one, waits for the handshake, and calls there.
%% This is the direct-dial data path: resolve a serving_station and its
%% endpoint, then reach it in one hop. See `macula_client:call_station/6'.
-spec call_station(pool(), macula_client:seed(), realm(), procedure(),
                   term(), pos_integer()) -> {ok, term()} | {error, term()}.
call_station(Pool, Station, Realm, Procedure, Payload, TimeoutMs) ->
    macula_client:call_station(Pool, Station, Realm, Procedure, Payload,
                               TimeoutMs).

%% @doc As `call_station/6', presenting a capability token to a gated
%% provider via `Opts' (`#{ucan_token => Token}'). Empty/absent = none.
%% Slice 7b dual-trust. `Opts' also carries the per-call TLS trust
%% override for this dial: `verify', `expected_node_id', and
%% `pin_tls_cert' (see `macula_client:call_station/8').
-spec call_station(pool(), macula_client:seed(), realm(), procedure(),
                   term(), pos_integer(), map()) ->
    {ok, term()} | {error, term()}.
call_station(Pool, Station, Realm, Procedure, Payload, TimeoutMs, Opts) ->
    Ucan = maps:get(ucan_token, Opts, <<>>),
    LinkOpts = maps:with([verify, expected_node_id, pin_tls_cert], Opts),
    macula_client:call_station(Pool, Station, Realm, Procedure, Payload,
                               TimeoutMs, Ucan, LinkOpts).

%% @doc Advertise a procedure handler on a V2 pool. Fans out to every
%% healthy link and stores in pool state for replay on link respawn.
%% See `macula_client:advertise/4'.
-spec advertise(pool(), realm(), procedure(),
                macula_client:handler(), map()) ->
    ok | {error, term()}.
advertise(Pool, Realm, Procedure, Handler, Opts)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32 ->
    %% `auth' opt sets the procedure's policy: `open' (default, serve any
    %% identified caller) or `{ucan_required, Issuer}' (gated). Slice 7b.
    Policy = maps:get(auth, Opts, open),
    macula_client:advertise(Pool, Realm, Procedure, Handler, Policy).

%% @doc Stop advertising a procedure on a V2 pool.
-spec unadvertise(pool(), realm(), procedure()) -> ok.
unadvertise(Pool, Realm, Procedure) ->
    macula_client:unadvertise(Pool, Realm, Procedure).

%%%===================================================================
%%% Signed DHT records (v3.3.0)
%%%===================================================================
%%%
%%% Records are typed, signed payloads stored in the relay mesh's
%%% distributed hash table. The record format follows Macula V2
%%% spec Part 6 §9 (PKARR-compatible CBOR with single-letter keys
%%% `t', `k', `v', `c', `x', `p', `s'), Part 6 §10.2 (signing
%%% domain `"macula-v2-record\\0" || canonical_cbor(unsigned)'),
%%% and Part 3 §3.3 (domain-separated storage keys).
%%%
%%% See `macula_record' for the record shape, the typed
%%% constructors (`node_record/3', `realm_directory/3',
%%% `realm_stations/2', `procedure_advertisement/3',
%%% `content_announcement/3', `tombstone/3', and the foundation_*
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

-define(CONTENT_REALM,                 <<0:256>>).
-define(CONTENT_PUT_BLOCK_PROC,        <<"_content.put_block">>).
-define(CONTENT_GET_BLOCK_PROC,        <<"_content.get_block">>).
-define(CONTENT_PUT_MANIFEST_PROC,     <<"_content.put_manifest">>).
-define(CONTENT_GET_MANIFEST_PROC,     <<"_content.get_manifest">>).
%% Bigger timeout than DHT records — chunks are 256 KiB and a put
%% writes through the file-backed store on the relay.
-define(CONTENT_BLOCK_TIMEOUT_MS,      15_000).
-define(CONTENT_MANIFEST_TIMEOUT_MS,   5_000).
-define(CONTENT_RETRY_BACKOFF_MS,      200).

%% @doc Store a signed record in the mesh DHT via a V2 pool.
%%
%% Build the record via the typed constructors in `macula_record'
%% (`node_record/3,4', `content_announcement/3,4', `tombstone/3,4',
%% `realm_directory/3,4', `procedure_advertisement/3,4', etc.) then
%% sign it with `macula_record:sign/2'. The relay validates the
%% signature on receipt; an invalid signature returns
%% `{error, bad_signature}'. Successful stores propagate to the
%% K-nearest peers in the DHT under the record's
%% `macula_record:storage_key/1'.
-spec put_record(pool(), m_record()) -> ok | {error, term()}.
put_record(Pool, Record) when is_pid(Pool), is_map(Record) ->
    classify_put(macula_client:call(Pool, ?DHT_REALM,
                                    ?DHT_PUT_RECORD_PROC,
                                    Record, ?DHT_RECORD_TIMEOUT_MS)).

classify_put({ok, ok})       -> ok;
classify_put({ok, Reply})    -> {error, {unexpected_reply, Reply}};
classify_put({error, _} = E) -> E.

%% @doc Fetch a record from the mesh DHT by its
%% `macula_record:storage_key/1'.
%%
%% Returns `{error, not_found}' when no record exists at the key.
%% The returned record's signature should be verified via
%% `macula_record:verify/1' before its payload is trusted.
-spec find_record(pool(), record_key()) ->
    {ok, m_record()} | {error, not_found | term()}.
find_record(Pool, Key)
  when is_pid(Pool), is_binary(Key), byte_size(Key) =:= 32 ->
    classify_find(macula_client:call(Pool, ?DHT_REALM,
                                     ?DHT_FIND_RECORD_PROC,
                                     #{key => Key},
                                     ?DHT_RECORD_TIMEOUT_MS)).

classify_find({ok, #{type := _, payload := _, signature := _} = Record}) ->
    {ok, Record};
classify_find({ok, not_found})     -> {error, not_found};
classify_find({ok, Reply})         -> {error, {unexpected_reply, Reply}};
classify_find({error, _} = E)      -> E.

%% @doc Fetch EVERY record stored at `Key' — the full multi-value
%% set, e.g. every `procedure_advertisement' under one procedure's
%% storage key. Where `find_record/2' returns the first record (or
%% `not_found'), this returns the whole list, empty when none.
%%
%% The relay's local store is a signer-deduped multiset: one record
%% per signing key at a storage key, so N providers of one procedure
%% return N records. Each returned record's signature should be
%% verified via `macula_record:verify/1' before its payload is
%% trusted.
-spec find_records(pool(), record_key()) ->
    {ok, [m_record()]} | {error, term()}.
find_records(Pool, Key)
  when is_pid(Pool), is_binary(Key), byte_size(Key) =:= 32 ->
    classify_find_list(macula_client:call(Pool, ?DHT_REALM,
                                          ?DHT_FIND_RECORDS_PROC,
                                          #{key => Key},
                                          ?DHT_RECORD_TIMEOUT_MS)).

classify_find_list({ok, Records}) when is_list(Records) -> {ok, Records};
classify_find_list({ok, Reply})    -> {error, {unexpected_reply, Reply}};
classify_find_list({error, _} = E) -> E.

%% @doc Return every record of a given type currently visible from
%% the pool's connected stations.
%%
%% Coverage depends on each station's view of the DHT — a single
%% station sees its local replicas plus whatever its peers have
%% gossiped. Aggregating across the full mesh requires querying
%% multiple stations and deduplicating by record key.
-spec find_records_by_type(pool(), record_type()) ->
    {ok, [m_record()]} | {error, term()}.
find_records_by_type(Pool, Type)
  when is_pid(Pool), is_integer(Type), Type >= 0, Type =< 255 ->
    classify_list(macula_client:call(Pool, ?DHT_REALM,
                                     ?DHT_FIND_RECORDS_BY_TYPE_PROC,
                                     #{type => Type},
                                     ?DHT_RECORD_TIMEOUT_MS)).

classify_list({ok, Records}) when is_list(Records) -> {ok, Records};
classify_list({ok, Reply})    -> {error, {unexpected_reply, Reply}};
classify_list({error, _} = E) -> E.

%% @doc Subscribe to live record-stored events filtered by type.
%%
%% The callback receives each newly-stored record of the given
%% type. Returns a subscription reference for `unsubscribe_records/2'.
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

record_stored_topic(Type) ->
    iolist_to_binary([<<"_dht.records.">>,
                      integer_to_binary(Type),
                      <<".stored">>]).

%% Adapt a 1-arg `(Record) -> any()' user callback to the 3-arg
%% `(Topic, Payload, Meta) -> any()' shape `macula_pubsub' delivers.
%%
%% PubSub delivers the payload as the wire-format encoded record
%% binary (the substrate's `record_fanout' publishes
%% `macula_record:encode/1' output on the `_dht.records.<type>.stored'
%% topic). Decode here so the user-supplied callback receives the
%% record map per the documented contract. Malformed payloads are
%% dropped silently — surfacing them to the callback would force
%% every user to handle decode errors for what is fundamentally a
%% protocol-internal channel.
wrap_record_callback(Fun) ->
    fun(_Topic, Payload, _Meta) -> apply_callback_with_decode(Fun, Payload) end.

apply_callback_with_decode(Fun, Payload) when is_binary(Payload) ->
    case macula_record:decode(Payload) of
        {ok, Record} -> Fun(Record), ok;
        _            -> ok
    end;
apply_callback_with_decode(Fun, Payload) when is_map(Payload) ->
    %% Already-decoded record (legacy callers / direct injection).
    Fun(Payload), ok;
apply_callback_with_decode(_Fun, _Other) ->
    ok.

%%%===================================================================
%%% Content-addressed blob storage (v4.2.7+)
%%%===================================================================

-type mcid() :: <<_:272>>.


%% @doc Store `Bytes' in the mesh's content store and return its MCID
%% (Macula Content ID — 34 bytes: version, codec, then a 32-byte hash).
%% Content that fits in one block (`byte_size(Bytes) =&lt;
%% macula_manifest:default_chunk_size/0', 256 KiB) is sent as a
%% single `_content.put_block' — the MCID is `&lt;&lt;1, 16#55,
%% BLAKE3(Bytes)&gt;&gt;', unchanged since v4.2.7. Larger content is split
%% into chunks (`macula_manifest:create/1'), each chunk sent
%% via its own `_content.put_block', then a `content_manifest' via
%% `_content.put_manifest'; the returned MCID is the manifest's
%% (`&lt;&lt;1, 16#56, _/binary&gt;&gt;'), Merkle-rooted over every chunk. Either
%% way the station verifies each block's hash before accepting it.
%%
%% The whole transfer — every block call plus the manifest call for
%% chunked content — rides one dedicated QUIC stream on one pinned
%% pool link (see PLAN_PER_STREAM_QUIC_ISOLATION.md Phase 2), so a
%% large blob transfer no longer head-of-line-blocks other RPC/PubSub
%% traffic on the same connection. Unlike the old per-block
%% `macula_client:call/5' routing, chunks of one transfer can no
%% longer land on different links — the link is chosen once, up
%% front, for the whole call.
-spec put_content(pool(), binary()) -> {ok, mcid()} | {error, term()}.
put_content(Pool, Bytes) when is_pid(Pool), is_binary(Bytes) ->
    with_content_stream(Pool, fun(LinkPid, Stream) ->
        put_content_by_size(byte_size(Bytes) =< macula_manifest:default_chunk_size(),
                            LinkPid, Stream, Bytes)
    end).

%% Pin one connected link and open a dedicated content stream on it
%% for the duration of `Fun', closing the stream afterwards
%% regardless of outcome.
with_content_stream(Pool, Fun) ->
    on_content_link_picked(macula_client:pick_connected_link(Pool), Fun).

on_content_link_picked({error, _} = E, _Fun) ->
    E;
on_content_link_picked({ok, LinkPid}, Fun) ->
    on_content_stream_opened(
      macula_station_link:open_content_stream(LinkPid), LinkPid, Fun).

on_content_stream_opened({error, _} = E, _LinkPid, _Fun) ->
    E;
on_content_stream_opened({ok, Stream}, LinkPid, Fun) ->
    Result = Fun(LinkPid, Stream),
    macula_station_link:close_content_stream(LinkPid, Stream),
    Result.

put_content_by_size(true, LinkPid, Stream, Bytes) ->
    put_single_block(LinkPid, Stream, Bytes);
put_content_by_size(false, LinkPid, Stream, Bytes) ->
    put_chunked(LinkPid, Stream, Bytes).

put_single_block(LinkPid, Stream, Bytes) ->
    Hash = macula_blake3_nif:hash(Bytes),
    MCID = <<1, 16#55, Hash/binary>>,
    classify_put_content(put_block(LinkPid, Stream, MCID, Bytes), MCID).

put_block(LinkPid, Stream, MCID, Bytes) ->
    call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_PUT_BLOCK_PROC,
                              #{mcid => MCID, payload => Bytes},
                              ?CONTENT_BLOCK_TIMEOUT_MS).

classify_put_content({ok, ok},                MCID) -> {ok, MCID};
classify_put_content({ok, hash_mismatch},     _MCID) -> {error, hash_mismatch};
classify_put_content({ok, Reply},             _MCID) -> {error, {unexpected_reply, Reply}};
classify_put_content({error, _} = E,          _MCID) -> E.

%% Split into chunks, upload each block, then the manifest. A chunk
%% failure short-circuits WITHOUT putting the manifest — a manifest
%% naming missing chunks would resolve but never reassemble, which is
%% worse than a clean error now.
put_chunked(LinkPid, Stream, Bytes) ->
    {ok, Manifest, Chunks} = macula_manifest:create(Bytes),
    put_chunks(LinkPid, Stream, Manifest, Chunks, 0).

put_chunks(LinkPid, Stream, Manifest, [], _Index) ->
    put_manifest(LinkPid, Stream, Manifest);
put_chunks(LinkPid, Stream, Manifest, [Chunk | Rest], Index) ->
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, Index, blake3),
    chunk_put_result(put_block(LinkPid, Stream, ChunkMcid, Chunk), LinkPid,
                     Stream, Manifest, Rest, Index + 1).

chunk_put_result({ok, ok}, LinkPid, Stream, Manifest, Rest, NextIndex) ->
    put_chunks(LinkPid, Stream, Manifest, Rest, NextIndex);
chunk_put_result({ok, hash_mismatch}, _LinkPid, _Stream, _Manifest, _Rest,
                 _NextIndex) ->
    {error, hash_mismatch};
chunk_put_result({ok, Reply}, _LinkPid, _Stream, _Manifest, _Rest, _NextIndex) ->
    {error, {unexpected_reply, Reply}};
chunk_put_result({error, _} = E, _LinkPid, _Stream, _Manifest, _Rest, _NextIndex) ->
    E.

put_manifest(LinkPid, Stream, #{mcid := MCID} = Manifest) ->
    classify_put_manifest(
      call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_PUT_MANIFEST_PROC,
                                #{manifest => Manifest},
                                ?CONTENT_MANIFEST_TIMEOUT_MS),
      MCID).

classify_put_manifest({ok, ok},      MCID) -> {ok, MCID};
classify_put_manifest({ok, Reply},  _MCID) -> {error, {unexpected_reply, Reply}};
classify_put_manifest({error, _} = E, _MCID) -> E.

%% @doc As `put_content/2', dialing `Station' directly (reusing a live
%% link or dialing + waiting up to `TimeoutMs' for one) instead of
%% picking from the pool's existing links — the content-transfer
%% counterpart to `call_station/6'. `Station' and `TimeoutMs' mean
%% exactly what they do there; the underlying block/manifest transfer
%% has its own internal timeouts regardless of `TimeoutMs', which
%% bounds only the connect wait. See `macula_direct_dial:put_content/4'
%% to resolve a station by identity and put in one call.
-spec put_content_station(pool(), macula_client:seed(), binary(),
                          pos_integer()) -> {ok, mcid()} | {error, term()}.
put_content_station(Pool, Station, Bytes, TimeoutMs) ->
    put_content_station(Pool, Station, Bytes, TimeoutMs, #{}).

%% @doc As `put_content_station/4', with a per-call TLS trust override
%% for this dial — `verify', `expected_node_id', `pin_tls_cert' (see
%% `call_station/8').
-spec put_content_station(pool(), macula_client:seed(), binary(),
                          pos_integer(), map()) ->
    {ok, mcid()} | {error, term()}.
put_content_station(Pool, Station, Bytes, TimeoutMs, Opts) ->
    LinkOpts = maps:with([verify, expected_node_id, pin_tls_cert], Opts),
    with_content_stream_station(Pool, Station, TimeoutMs, LinkOpts,
                                fun(LinkPid, Stream) ->
        put_content_by_size(byte_size(Bytes) =< macula_manifest:default_chunk_size(),
                            LinkPid, Stream, Bytes)
    end).

%% @doc Fetch the bytes for a previously-stored MCID. Returns
%% `{error, not_found}' if no provider in the pool's reach holds a
%% copy (for chunked content, if any single chunk is unreachable).
%% Dispatches on the MCID's codec byte: `16#55' (raw/single-block)
%% fetches one block, BLAKE3-verified by the station before it leaves
%% the store; `16#56' (manifest) fetches the manifest, then every
%% chunk in order, reassembles, and verifies the whole against the
%% manifest's size and Merkle root before returning.
-spec get_content(pool(), mcid()) ->
    {ok, binary()} | {error, not_found | term()}.
get_content(Pool, MCID) when is_pid(Pool) ->
    with_content_stream(Pool, fun(LinkPid, Stream) ->
        get_content_via(LinkPid, Stream, MCID)
    end).

%% @doc As `get_content/2', dialing `Station' directly (reusing a live
%% link or dialing + waiting up to `TimeoutMs' for one) instead of
%% picking from the pool's existing links — the content-transfer
%% counterpart to `call_station/6'. `Station' and `TimeoutMs' mean
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

%% @doc As `get_content_station/4', with a per-call TLS trust override
%% for this dial — `verify', `expected_node_id', `pin_tls_cert' (see
%% `call_station/8').
-spec get_content_station(pool(), macula_client:seed(), mcid(),
                          pos_integer(), map()) ->
    {ok, binary()} | {error, not_found | term()}.
get_content_station(Pool, Station, MCID, TimeoutMs, Opts) ->
    LinkOpts = maps:with([verify, expected_node_id, pin_tls_cert], Opts),
    with_content_stream_station(Pool, Station, TimeoutMs, LinkOpts,
                                fun(LinkPid, Stream) ->
        get_content_via(LinkPid, Stream, MCID)
    end).

get_content_via(LinkPid, Stream, <<1, 16#55, _:32/binary>> = MCID) ->
    classify_get_content(get_block(LinkPid, Stream, MCID), MCID);
get_content_via(LinkPid, Stream, <<1, 16#56, _:32/binary>> = MCID) ->
    get_chunked(LinkPid, Stream, MCID).

%% As `with_content_stream/2', but pins a link to a specific,
%% resolved station rather than picking from the pool's existing links.
with_content_stream_station(Pool, Station, TimeoutMs, LinkOpts, Fun) ->
    on_content_link_picked(
      macula_client:ensure_content_link(Pool, Station, LinkOpts, TimeoutMs),
      Fun).

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
%% Each entry's signature is verified, AND its signer must equal the
%% `announcer_node' it claims — same discipline as `station_endpoint'
%% resolution — before its `endpoint' is trusted; unverifiable,
%% signer-mismatched, or malformed records are dropped, not surfaced as
%% errors. Single-block content (put via `_content.put_block' alone) is
%% not announced — resolving its MCID returns `{ok, []}'.
-spec find_content_providers(pool(), mcid()) -> {ok, [map()]} | {error, term()}.
find_content_providers(Pool, MCID)
  when is_pid(Pool), is_binary(MCID), byte_size(MCID) =:= 34 ->
    classify_find_providers(
      macula_client:call(Pool, ?DHT_REALM, ?DHT_FIND_RECORDS_PROC,
                         #{key => macula_record:content_key(MCID)},
                         ?DHT_RECORD_TIMEOUT_MS)).

classify_find_providers({ok, Records}) when is_list(Records) ->
    {ok, decode_providers(Records)};
classify_find_providers({ok, Reply}) ->
    {error, {unexpected_reply, Reply}};
classify_find_providers({error, _} = E) ->
    E.

decode_providers(Records) ->
    lists:filtermap(fun decode_provider/1, Records).

decode_provider(#{key := Key} = Record) ->
    provider_verified(macula_record:verify(Record), Key, Record).

%% The record's own signature proves SOME identity signed it; the
%% signer must also equal the `announcer_node' the payload claims — a
%% record merely stored under the right key but self-signed by someone
%% else's identity would otherwise still be trusted (same class of gap
%% `macula_direct_dial:verify_and_build/2' closes for `station_endpoint').
provider_verified({ok, _}, Key, Record) ->
    try macula_record:read_content_announcement(Record) of
        #{announcer_node := Key, endpoint := _} = Provider -> {true, Provider};
        _Mismatched -> false
    catch _:_ -> false
    end;
provider_verified({error, _}, _Key, _Record) ->
    false.

get_block(LinkPid, Stream, MCID) ->
    call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_GET_BLOCK_PROC,
                              #{mcid => MCID}, ?CONTENT_BLOCK_TIMEOUT_MS).

classify_get_content({ok, not_found}, _MCID)        -> {error, not_found};
classify_get_content({ok, Bin}, MCID) when is_binary(Bin) ->
    verify_block_hash(MCID, Bin);
classify_get_content({ok, Reply}, _MCID)            -> {error, {unexpected_reply, Reply}};
classify_get_content({error, _} = E, _MCID)         -> E.

%% The station verified this block's hash at PUT time; a station
%% fetched FROM (especially via `get_content_station/5', deliberately
%% dialing a caller-chosen peer) is not necessarily the one that stored
%% it, so re-verify client-side rather than trusting whoever answered.
%% Chunked content already gets this from `macula_manifest:verify/2'
%% over the reassembled whole; single-block content had no client-side
%% check at all before this.
verify_block_hash(<<1, 16#55, Hash:32/binary>>, Bin) ->
    hash_result(macula_blake3_nif:hash(Bin) =:= Hash, Bin);
verify_block_hash(_MCID, _Bin) ->
    {error, invalid_mcid}.

hash_result(true, Bin)   -> {ok, Bin};
hash_result(false, _Bin) -> {error, hash_mismatch}.

get_chunked(LinkPid, Stream, MCID) ->
    classify_get_manifest(
      call_on_stream_with_retry(LinkPid, Stream, ?CONTENT_GET_MANIFEST_PROC,
                                #{mcid => MCID}, ?CONTENT_MANIFEST_TIMEOUT_MS),
      LinkPid, Stream).

classify_get_manifest({ok, not_found}, _LinkPid, _Stream) ->
    {error, not_found};
classify_get_manifest({ok, Wire}, LinkPid, Stream) when is_map(Wire) ->
    manifest_decoded(macula_manifest:from_wire(Wire), LinkPid, Stream);
classify_get_manifest({ok, Reply}, _LinkPid, _Stream) ->
    {error, {unexpected_reply, Reply}};
classify_get_manifest({error, _} = E, _LinkPid, _Stream) ->
    E.

manifest_decoded({error, invalid_manifest}, _LinkPid, _Stream) ->
    {error, invalid_manifest};
manifest_decoded({ok, #{chunk_count := N} = Manifest}, LinkPid, Stream) ->
    get_chunks(LinkPid, Stream, Manifest, 0, N, []).

get_chunks(_LinkPid, _Stream, Manifest, Index, N, Acc) when Index >= N ->
    reassembled(Manifest, iolist_to_binary(lists:reverse(Acc)));
get_chunks(LinkPid, Stream, Manifest, Index, N, Acc) ->
    {ok, ChunkMcid} = macula_manifest:chunk_mcid(Manifest, Index, blake3),
    chunk_get_result(get_block(LinkPid, Stream, ChunkMcid), LinkPid, Stream,
                     Manifest, Index, N, Acc).

chunk_get_result({ok, Bin}, LinkPid, Stream, Manifest, Index, N, Acc)
        when is_binary(Bin) ->
    get_chunks(LinkPid, Stream, Manifest, Index + 1, N, [Bin | Acc]);
chunk_get_result({ok, not_found}, _LinkPid, _Stream, _Manifest, _Index, _N,
                 _Acc) ->
    {error, not_found};
chunk_get_result({ok, Reply}, _LinkPid, _Stream, _Manifest, _Index, _N, _Acc) ->
    {error, {unexpected_reply, Reply}};
chunk_get_result({error, _} = E, _LinkPid, _Stream, _Manifest, _Index, _N,
                 _Acc) ->
    E.

reassembled(Manifest, Reassembled) ->
    verify_result(macula_manifest:verify(Manifest, Reassembled),
                  Reassembled).

verify_result(ok, Reassembled)      -> {ok, Reassembled};
verify_result({error, _} = E, _Bin) -> E.

%% A `_content.*' CALL on the transfer's pinned dedicated stream,
%% retried on a BOLT#4 error whose OWN retry policy says to
%% (`macula_bolt4:is_retryable/1' — e.g. `temporary_relay_failure' is
%% rated `same_path_after_backoff'). This is the spec's documented
%% contract, not a blind retry: a non-retryable error (or a transport
%% `{error, _}' outside the BOLT#4 taxonomy, e.g. `not_connected')
%% returns immediately. Bounded to 3 attempts total with a short linear
%% backoff — enough to absorb a transient relay hiccup without masking a
%% genuine, persistent failure as a hang.
%%
%% Content puts are the first CALL callers in this SDK to hit this in
%% practice: `_content.put_manifest' was observed to fail the first
%% attempt against a freshly-started content store and succeed on retry
%% (`_content.put_block' has not shown this). The station-side root
%% cause is not yet diagnosed; retrying is what the CALL's own error
%% code prescribes regardless, so content operations do it uniformly.
%%
%% Retries resend on the SAME stream/link, never re-picking a link the
%% way the old pool-routed `call_with_retry' could — a link healthy
%% enough to answer BOLT#4 `same_path_after_backoff' in the first
%% place is the right target to retry against, and switching links
%% mid-transfer would defeat the point of pinning one.
call_on_stream_with_retry(LinkPid, Stream, Procedure, Payload, TimeoutMs) ->
    call_on_stream_with_retry(LinkPid, Stream, Procedure, Payload, TimeoutMs, 3).

call_on_stream_with_retry(LinkPid, Stream, Procedure, Payload, TimeoutMs,
                          AttemptsLeft) ->
    retry_stream_result(
      macula_station_link:call_on_stream(LinkPid, Stream, ?CONTENT_REALM,
                                         Procedure, Payload, TimeoutMs),
      LinkPid, Stream, Procedure, Payload, TimeoutMs, AttemptsLeft).

retry_stream_result({error, {call_error, Code, _Name}} = E, LinkPid, Stream,
                    Procedure, Payload, TimeoutMs, AttemptsLeft)
        when AttemptsLeft > 1 ->
    retry_stream_if_retryable(macula_bolt4:is_retryable(Code), E, LinkPid,
                              Stream, Procedure, Payload, TimeoutMs,
                              AttemptsLeft);
retry_stream_result(Result, _LinkPid, _Stream, _Procedure, _Payload,
                    _TimeoutMs, _AttemptsLeft) ->
    Result.

retry_stream_if_retryable(true, _E, LinkPid, Stream, Procedure, Payload,
                          TimeoutMs, AttemptsLeft) ->
    timer:sleep(?CONTENT_RETRY_BACKOFF_MS),
    call_on_stream_with_retry(LinkPid, Stream, Procedure, Payload, TimeoutMs,
                              AttemptsLeft - 1);
retry_stream_if_retryable(false, E, _LinkPid, _Stream, _Procedure, _Payload,
                          _TimeoutMs, _AttemptsLeft) ->
    E.

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

%% @doc Open a streaming RPC against a V2 pool. Picks the first
%% currently-healthy link and opens the stream there; the returned
%% stream is sticky-to-link (errors with `peer_down' if the link
%% dies; caller re-opens). See `macula_client:call_stream/5'.
-spec call_stream(pool(), realm(), procedure(), term(), map()) ->
        {ok, stream()} | {error, term()}.
call_stream(Pool, Realm, Procedure, Args, Opts)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure), is_map(Opts) ->
    macula_client:call_stream(Pool, Realm, Procedure, Args, Opts).

%% @doc Open a streaming RPC by DIALING a specific station directly
%% (direct-dial), instead of routing through an existing pool link — the
%% streaming analogue of `call_station/6'. Compose it with DHT resolution
%% (`find_records' -> `read_procedure_advertisement' -> `station_endpoint')
%% to reach a stream provider in one hop, exactly as a unary caller does.
%% `Opts' may set `dial_timeout_ms' (default 10_000) and a `mode'.
-spec call_stream_station(pool(), macula_client:seed(), realm(), procedure(),
                          term(), map()) -> {ok, stream()} | {error, term()}.
call_stream_station(Pool, Station, Realm, Procedure, Args, Opts)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure), is_map(Opts) ->
    macula_client:call_stream_station(Pool, Station, Realm, Procedure, Args,
                                      Opts).

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

%% @doc Advertise a streaming procedure on a V2 pool. Fans out to
%% every healthy link and stores in pool state for replay on link
%% respawn. See `macula_client:advertise_stream/5'.
-spec advertise_stream(pool(), realm(), procedure(),
                        stream_mode(), stream_handler()) ->
        ok | {error, term()}.
advertise_stream(Pool, Realm, Procedure, Mode, Handler)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Procedure),
       (Mode =:= server_stream orelse Mode =:= client_stream
        orelse Mode =:= bidi),
       is_function(Handler, 2) ->
    macula_client:advertise_stream(Pool, Realm, Procedure, Mode, Handler).

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
-spec abort(stream(), binary(), binary()) -> ok.
abort(Stream, Code, Message)
  when is_pid(Stream), is_binary(Code), is_binary(Message) ->
    macula_stream:abort(Stream, Code, Message).

%%%===================================================================
%%% Cluster (LAN)
%%%===================================================================

%% @doc Ensure this node is running in distributed mode.
-spec ensure_distributed() -> ok | {error, term()}.
ensure_distributed() -> macula_cluster:ensure_distributed().

%% @doc Get the Erlang cluster cookie.
-spec get_cookie() -> atom().
get_cookie() -> macula_cluster:get_cookie().

%% @doc Set the Erlang cluster cookie.
-spec set_cookie(atom() | binary()) -> ok.
set_cookie(Cookie) -> macula_cluster:set_cookie(Cookie).

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
%%   <li>`relays' (required) — list of seed URLs for the V2 pool.</li>
%%   <li>`identity' — V2 pool's `macula_identity:key_pair()'.
%%       Default: auto-generated.</li>
%% </ul>
%%
%% Internally builds a V2 `macula_client:pool()' and registers it
%% with `macula_dist_pool' as the carrier for `_dist.tunnel.*'
%% traffic. Dist tunnel frames travel under the all-zeros realm
%% (protocol-internal infrastructure, not bound to any user realm).
-spec join_mesh(map()) -> ok | {error, term()}.
join_mesh(Opts) ->
    Relays = maps:get(relays, Opts),
    PoolOpts = pool_opts_for_join(Opts),
    on_pool_for_join(macula_client:connect(Relays, PoolOpts)).

pool_opts_for_join(Opts) ->
    case maps:find(identity, Opts) of
        {ok, Identity} -> #{identity => Identity};
        error          -> #{}
    end.

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
-spec join_dist_relay(map()) -> ok | {error, term()}.
join_dist_relay(Opts) ->
    Url = maps:get(url, Opts),
    NodeName = atom_to_binary(node()),
    case macula_dist_system:start_dist_relay_client(Url, NodeName) of
        {ok, _Pid} ->
            os:putenv("MACULA_DIST_MODE", "dist_relay"),
            ?LOG_INFO("[macula] Joined dist relay ~s — distribution enabled", [Url]),
            ok;
        {error, {already_started, _Pid}} ->
            os:putenv("MACULA_DIST_MODE", "dist_relay"),
            ?LOG_INFO("[macula] dist_relay_client already running — mode set"),
            ok;
        {error, Reason} = Err ->
            ?LOG_ERROR("[macula] Failed to join dist relay: ~p", [Reason]),
            Err
    end.

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
