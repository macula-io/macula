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
%%% `macula_dist_relay' / `macula_dist_system'.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula).

-include_lib("kernel/include/logger.hrl").

%% Connection
-export([connect/2, close/1, child_spec/3, status/1]).

%% Pub/Sub — realm-per-call against a V2 pool
-export([subscribe/4, subscribe/5,
         subscribe_callback/4,
         unsubscribe/2,
         publish/4, publish/5]).

%% RPC — realm-per-call against a V2 pool
-export([call/5,
         advertise/5,
         unadvertise/3]).

%% Signed DHT records — realm-agnostic infrastructure procedures
%% (`_dht.put_record', `_dht.find_record', `_dht.find_records_by_type',
%% `_dht.records.<type>.stored'). The all-zeros realm is the SDK
%% convention for protocol-internal traffic.
-export([put_record/2,
         find_record/2,
         find_records_by_type/2,
         subscribe_records/3,
         unsubscribe_records/2]).

%% Content-addressed blob storage. `_content.put_block' /
%% `_content.get_block' RPCs against the relay's local content
%% store. MCID is a 34-byte binary: 1 codec byte, 1 algo byte
%% (BLAKE3 = 16#55), 32-byte BLAKE3 hash. The relay validates the
%% payload's hash on `put_block' and rejects mismatches.
-export([put_content/2,
         get_content/2]).

%% Streaming RPC (LOCAL in-process + V2 pool, see PLAN_MACULA_STREAMING.md)
-export([
    call_stream/2, call_stream/3, call_stream/5,
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

%% Types
-export_type([pool/0, realm/0,
              topic/0, procedure/0,
              stream/0, stream_mode/0, stream_handler/0,
              record/0, record_type/0, record_key/0]).

-type pool()   :: macula_client:pool().
-type realm()  :: <<_:256>>.            %% 32-byte realm tag.
-type topic() :: binary().
-type procedure() :: binary().

-type stream() :: pid().
-type stream_mode() :: server_stream | client_stream | bidi.
-type stream_handler() :: fun((stream(), term()) -> any()).

-type record()      :: macula_record:record().
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
%% options. See `macula_pubsub:subscribe/5'.
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

%% @doc Advertise a procedure handler on a V2 pool. Fans out to every
%% healthy link and stores in pool state for replay on link respawn.
%% See `macula_client:advertise/4'.
-spec advertise(pool(), realm(), procedure(),
                macula_client:handler(), map()) ->
    ok | {error, term()}.
advertise(Pool, Realm, Procedure, Handler, _Opts)
  when is_pid(Pool), is_binary(Realm), byte_size(Realm) =:= 32 ->
    macula_client:advertise(Pool, Realm, Procedure, Handler).

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
%% the same convention `macula_dist_relay' uses for tunnel frames).
-define(DHT_REALM,                     <<0:256>>).
-define(DHT_PUT_RECORD_PROC,           <<"_dht.put_record">>).
-define(DHT_FIND_RECORD_PROC,          <<"_dht.find_record">>).
-define(DHT_FIND_RECORDS_BY_TYPE_PROC, <<"_dht.find_records_by_type">>).
-define(DHT_RECORD_TIMEOUT_MS,         5_000).

-define(CONTENT_REALM,                 <<0:256>>).
-define(CONTENT_PUT_BLOCK_PROC,        <<"_content.put_block">>).
-define(CONTENT_GET_BLOCK_PROC,        <<"_content.get_block">>).
%% Bigger timeout than DHT records — chunks are 256 KiB and a put
%% writes through the file-backed store on the relay.
-define(CONTENT_BLOCK_TIMEOUT_MS,      15_000).

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
-spec put_record(pool(), record()) -> ok | {error, term()}.
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
    {ok, record()} | {error, not_found | term()}.
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

%% @doc Return every record of a given type currently visible from
%% the pool's connected stations.
%%
%% Coverage depends on each station's view of the DHT — a single
%% station sees its local replicas plus whatever its peers have
%% gossiped. Aggregating across the full mesh requires querying
%% multiple stations and deduplicating by record key.
-spec find_records_by_type(pool(), record_type()) ->
    {ok, [record()]} | {error, term()}.
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
                        fun((record()) -> any())) ->
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


%% @doc Store `Bytes' in the relay's content store and return its
%% MCID (Macula Content ID — 34 bytes: codec, algo (BLAKE3 = 16#55),
%% then the 32-byte BLAKE3 hash). The blob is sent as a single
%% block; on receipt the relay verifies the payload's BLAKE3
%% matches the MCID before accepting.
%%
%% This is the v4.2.7 minimum-viable shape — single-block per blob,
%% no client-side chunking. A subsequent release will add chunked
%% manifests so blobs larger than the per-call payload budget can be
%% transferred via parallel `_content.put_block' calls. For blobs in
%% the kilobyte-to-low-megabyte range a single block is sufficient
%% (relay default chunk size is 256 KiB; oversized payloads will
%% surface as a CALL-deadline timeout rather than silent truncation).
-spec put_content(pool(), binary()) -> {ok, mcid()} | {error, term()}.
put_content(Pool, Bytes) when is_pid(Pool), is_binary(Bytes) ->
    Hash = macula_blake3_nif:hash(Bytes),
    MCID = <<1, 16#55, Hash/binary>>,
    classify_put_content(
      macula_client:call(Pool, ?CONTENT_REALM,
                         ?CONTENT_PUT_BLOCK_PROC,
                         #{mcid => MCID, payload => Bytes},
                         ?CONTENT_BLOCK_TIMEOUT_MS),
      MCID).

classify_put_content({ok, ok},                MCID) -> {ok, MCID};
classify_put_content({ok, hash_mismatch},     _MCID) -> {error, hash_mismatch};
classify_put_content({ok, Reply},             _MCID) -> {error, {unexpected_reply, Reply}};
classify_put_content({error, _} = E,          _MCID) -> E.

%% @doc Fetch the bytes for a previously-stored MCID. Returns
%% `{error, not_found}' if no provider in the pool's reach holds a
%% copy. The returned binary is BLAKE3-verified by the relay before
%% it leaves the store, so the caller does not need to re-verify.
-spec get_content(pool(), mcid()) ->
    {ok, binary()} | {error, not_found | term()}.
get_content(Pool, <<1, 16#55, _:32/binary>> = MCID) when is_pid(Pool) ->
    classify_get_content(
      macula_client:call(Pool, ?CONTENT_REALM,
                         ?CONTENT_GET_BLOCK_PROC,
                         #{mcid => MCID},
                         ?CONTENT_BLOCK_TIMEOUT_MS)).

classify_get_content({ok, not_found})           -> {error, not_found};
classify_get_content({ok, Bin}) when is_binary(Bin) -> {ok, Bin};
classify_get_content({ok, Reply})               -> {error, {unexpected_reply, Reply}};
classify_get_content({error, _} = E)            -> E.

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
%% with `macula_dist_relay' as the carrier for `_dist.tunnel.*'
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
    macula_dist_relay:register_mesh_pool(Pool),
    macula_dist_relay:advertise_dist_accept(),
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
