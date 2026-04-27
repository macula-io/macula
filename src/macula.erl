%%%-------------------------------------------------------------------
%%% @doc Macula SDK — Public API for mesh applications.
%%%
%%% This is the main entry point for applications using the Macula SDK.
%%%
%%% == V2 (canonical, since 3.11.0) ==
%%%
%%% Apps connect via `connect/2', which returns a `macula_client'
%%% pool that internally wraps N peering links to N stations.
%%% `publish/4', `publish/5', `subscribe/4', `subscribe/5', and
%%% `unsubscribe/2' route through the pool with realm-per-call
%%% semantics. See `macula_pubsub' for the slice module of the same
%%% surface.
%%%
%%% == V1 (legacy, retired at 4.0.0) ==
%%%
%%% Older apps used a single-connection client (`macula_mesh_client').
%%% V1 facade surfaces still here untouched: `subscribe/3',
%%% `publish/3', `call/3,4', `advertise/3,4', `unadvertise/2',
%%% `put_record/2', plus all stream and directed-RPC operations.
%%% V1 callers wishing to keep V1 semantics for connect/publish/4/
%%% unsubscribe/close after upgrading to 3.11.0 should call
%%% `macula_mesh_client' / `macula_stream_v1' directly — see the
%%% migration guide.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula).

-include_lib("kernel/include/logger.hrl").

%% Connection — V2 (pool)
-export([connect/2, close/1, child_spec/3]).
%% Connection — V1 (legacy)
-export([disconnect/1]).

%% Pub/Sub — V2 (realm-per-call) + V1 (no-realm)
-export([subscribe/3, subscribe/4, subscribe/5,
         unsubscribe/2,
         publish/3, publish/4, publish/5]).

%% RPC
-export([call/3, call/4, advertise/3, advertise/4, unadvertise/2]).

%% Signed DHT records (v3.2.0)
-export([put_record/2,
         find_record/2,
         find_records_by_type/2,
         subscribe_records/3,
         unsubscribe_records/2]).

%% Streaming RPC (v1.5.0+ — see PLAN_MACULA_STREAMING.md)
%% NOTE: V1 stream `close/1' renamed to `close_stream/1' in 3.11.0
%% to free `close/1' for the V2 pool surface.
-export([
    call_stream/2, call_stream/3, call_stream/4,
    open_stream/3, open_stream/4,
    advertise_stream/2, advertise_stream/3, advertise_stream/4,
    unadvertise_stream/1,
    send/2, send/3,
    recv/1, recv/2,
    close_stream/1, close_send/1,
    await_reply/1, await_reply/2,
    set_reply/2, abort/3
]).

%% Directed RPC (Mesh Name Service)
-export([call_node/4, call_node/5, resolve/2, list_nodes/1, list_nodes/2]).

%% Cluster (LAN)
-export([ensure_distributed/0, get_cookie/0, set_cookie/1,
         monitor_nodes/0, unmonitor_nodes/0]).

%% Mesh Distribution
-export([join_mesh/1, join_dist_relay/1]).

%% Types
-export_type([client/0, pool/0, realm/0,
              topic/0, procedure/0,
              stream/0, stream_mode/0, stream_handler/0,
              record/0, record_type/0, record_key/0]).

-type client() :: pid().              %% V1 client (`macula_mesh_client').
-type pool()   :: macula_client:pool(). %% V2 pool (`macula_client').
-type realm()  :: <<_:256>>.            %% V2 32-byte realm tag.
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
%% See `macula_client' for the canonical pool implementation and
%% `macula_pubsub' for the slice module.
%%
%% **Breaking change since 3.11.0**: this function previously
%% returned a `macula_mesh_client' single-connection client. V1
%% callers must now call `macula_mesh_client:start_link/1' directly
%% to retain V1 semantics. See `CHANGELOG.md' / migration guide.
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

%%%===================================================================
%%% Connection — V1 (legacy)
%%%===================================================================

%% @doc Disconnect a V1 `macula_mesh_client' client. **Legacy** —
%% V2 pools use `close/1'.
-spec disconnect(client()) -> ok.
disconnect(Client) when is_pid(Client) ->
    macula_mesh_client:stop(Client).

%%%===================================================================
%%% Pub/Sub — V1 (legacy)
%%%===================================================================

%% @doc V1 subscribe — single-connection client, no realm. **Legacy**.
%% V2 callers use `subscribe/4' or `subscribe/5'.
-spec subscribe(client(), topic(), fun((term()) -> ok) | pid()) ->
    {ok, reference()} | {error, term()}.
subscribe(Client, Topic, Callback) when is_pid(Client), is_binary(Topic) ->
    macula_mesh_client:subscribe(Client, Topic, Callback).

%% @doc V1 publish — single-connection client, no realm. **Legacy**.
%% V2 callers use `publish/4' or `publish/5'.
-spec publish(client(), topic(), term()) -> ok.
publish(Client, Topic, Data) when is_pid(Client), is_binary(Topic) ->
    macula_mesh_client:publish(Client, Topic, Data).

%%%===================================================================
%%% Pub/Sub — V2 (pool, realm-per-call, since 3.11.0)
%%%===================================================================

%% @doc Publish to `(Realm, Topic)' on `Pool'. Equivalent to
%% `publish/5' with empty opts.
%%
%% **Breaking change since 3.11.0**: this signature previously was
%% `publish(Client, Topic, Data, Opts)' (V1). V1 callers using the
%% 4-arg form must call `macula_mesh_client:publish/3' directly to
%% retain V1 semantics.
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

%% @doc Drop a V2 pool subscription. Idempotent.
%%
%% **Breaking change since 3.11.0**: this routes to the V2 pool
%% (`macula_client:unsubscribe/2'). V1 callers must call
%% `macula_mesh_client:unsubscribe/2' directly to drop a V1 sub.
-spec unsubscribe(pool(), reference()) -> ok.
unsubscribe(Pool, SubRef) when is_pid(Pool), is_reference(SubRef) ->
    macula_pubsub:unsubscribe(Pool, SubRef).

%%%===================================================================
%%% RPC
%%%===================================================================

%% @doc Call a remote procedure (default 5s timeout).
-spec call(client(), procedure(), term()) -> {ok, term()} | {error, term()}.
call(Client, Procedure, Args) ->
    call(Client, Procedure, Args, 5000).

%% @doc Call a remote procedure with timeout.
-spec call(client(), procedure(), term(), pos_integer()) ->
    {ok, term()} | {error, term()}.
call(Client, Procedure, Args, Timeout) when is_pid(Client), is_binary(Procedure) ->
    macula_mesh_client:call(Client, Procedure, Args, Timeout).

%% @doc Advertise an RPC procedure handler.
-spec advertise(client(), procedure(), fun()) -> {ok, reference()} | {error, term()}.
advertise(Client, Procedure, Handler) ->
    advertise(Client, Procedure, Handler, #{}).

%% @doc Advertise with options.
-spec advertise(client(), procedure(), fun(), map()) -> {ok, reference()} | {error, term()}.
advertise(Client, Procedure, Handler, _Opts) when is_pid(Client), is_binary(Procedure) ->
    macula_mesh_client:advertise(Client, Procedure, Handler).

%% @doc Stop advertising a procedure.
-spec unadvertise(client(), procedure()) -> ok | {error, term()}.
unadvertise(Client, Procedure) when is_pid(Client), is_binary(Procedure) ->
    macula_mesh_client:unadvertise(Client, Procedure).

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
%% record-stored topic for the SDK to function.
-define(DHT_PUT_RECORD_PROC,           <<"_dht.put_record">>).
-define(DHT_FIND_RECORD_PROC,          <<"_dht.find_record">>).
-define(DHT_FIND_RECORDS_BY_TYPE_PROC, <<"_dht.find_records_by_type">>).
-define(DHT_RECORD_TIMEOUT_MS,         5_000).

%% @doc Store a signed record in the mesh DHT.
%%
%% Build the record via the typed constructors in `macula_record'
%% (`node_record/3,4', `content_announcement/3,4', `tombstone/3,4',
%% `realm_directory/3,4', `procedure_advertisement/3,4', etc.) then
%% sign it with `macula_record:sign/2'. The relay validates the
%% signature on receipt; an invalid signature returns
%% `{error, bad_signature}'. Successful stores propagate to the
%% K-nearest peers in the DHT under the record's
%% `macula_record:storage_key/1'.
-spec put_record(client(), record()) -> ok | {error, term()}.
put_record(Client, Record) when is_pid(Client), is_map(Record) ->
    classify_put(macula_mesh_client:call(Client, ?DHT_PUT_RECORD_PROC,
                                         Record, ?DHT_RECORD_TIMEOUT_MS)).

classify_put({ok, ok})       -> ok;
classify_put({ok, Reply})    -> {error, {unexpected_reply, Reply}};
classify_put({error, _} = E) -> E.

%% @doc Fetch a record by its `macula_record:storage_key/1'.
%%
%% Returns `{error, not_found}' when no record exists at the key.
%% The returned record's signature should be verified via
%% `macula_record:verify/1' before its payload is trusted.
-spec find_record(client(), record_key()) ->
    {ok, record()} | {error, not_found | term()}.
find_record(Client, Key)
  when is_pid(Client), is_binary(Key), byte_size(Key) =:= 32 ->
    classify_find(macula_mesh_client:call(Client, ?DHT_FIND_RECORD_PROC,
                                          #{key => Key},
                                          ?DHT_RECORD_TIMEOUT_MS)).

classify_find({ok, #{type := _, payload := _, sig := _} = Record}) ->
    {ok, Record};
classify_find({ok, not_found})     -> {error, not_found};
classify_find({ok, Reply})         -> {error, {unexpected_reply, Reply}};
classify_find({error, _} = E)      -> E.

%% @doc Return every record of a given type currently visible from
%% the connected relay.
%%
%% Coverage depends on the relay's view of the DHT — a single relay
%% sees its local replicas plus whatever its peers have gossiped.
%% Aggregating across the full mesh requires querying multiple
%% relays and deduplicating by record key.
-spec find_records_by_type(client(), record_type()) ->
    {ok, [record()]} | {error, term()}.
find_records_by_type(Client, Type)
  when is_pid(Client), is_integer(Type), Type >= 0, Type =< 255 ->
    classify_list(macula_mesh_client:call(Client,
                                          ?DHT_FIND_RECORDS_BY_TYPE_PROC,
                                          #{type => Type},
                                          ?DHT_RECORD_TIMEOUT_MS)).

classify_list({ok, Records}) when is_list(Records) -> {ok, Records};
classify_list({ok, Reply})    -> {error, {unexpected_reply, Reply}};
classify_list({error, _} = E) -> E.

%% @doc Subscribe to live record-stored events filtered by type.
%%
%% The callback (or pid) receives each newly-stored record of the
%% given type as `{record, Record}' (pid form) or via direct
%% invocation (fun form). Returns a subscription reference for
%% `unsubscribe_records/2'. Topic shape is `_dht.records.<type>.stored',
%% rendered with the type tag as a decimal integer for log
%% friendliness.
-spec subscribe_records(client(), record_type(),
                        fun((record()) -> any()) | pid()) ->
    {ok, reference()} | {error, term()}.
subscribe_records(Client, Type, Callback)
  when is_pid(Client), is_integer(Type), Type >= 0, Type =< 255 ->
    Topic = record_stored_topic(Type),
    macula_mesh_client:subscribe(Client, Topic,
                                 wrap_record_callback(Callback)).

%% @doc Cancel a `subscribe_records/3' subscription.
-spec unsubscribe_records(client(), reference()) -> ok | {error, term()}.
unsubscribe_records(Client, Ref)
  when is_pid(Client), is_reference(Ref) ->
    macula_mesh_client:unsubscribe(Client, Ref).

record_stored_topic(Type) ->
    iolist_to_binary([<<"_dht.records.">>,
                      integer_to_binary(Type),
                      <<".stored">>]).

wrap_record_callback(Pid) when is_pid(Pid) ->
    fun(Record) -> Pid ! {record, Record}, ok end;
wrap_record_callback(Fun) when is_function(Fun, 1) ->
    fun(Record) -> Fun(Record), ok end.

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
%%%   ok = macula:advertise_stream(<<"foo.count">>, server_stream,
%%%        fun(Stream, #{n := N}) ->
%%%             [ok = macula:send(Stream, integer_to_binary(I))
%%%              || I <- lists:seq(1, N)],
%%%             macula:close_stream(Stream)
%%%        end),
%%%   {ok, S} = macula:call_stream(<<"foo.count">>, #{n => 5}),
%%%   drain(S).
%%%
%%% drain(S) ->
%%%     case macula:recv(S) of
%%%         {chunk, Bin} -> io:format("~s~n", [Bin]), drain(S);
%%%         eof -> ok
%%%     end.

%% @doc Open a server-stream call (default mode).
%%
%% Phase 1 shortcut — dispatches in-process via the local registry.
%% For cross-node streaming use the `(Client, Procedure, Args)' form.
-spec call_stream(procedure(), term()) -> {ok, stream()} | {error, term()}.
call_stream(Procedure, Args) when is_binary(Procedure) ->
    call_stream(Procedure, Args, #{}).

%% @doc Open a server-stream call with options.
%%
%% Two shapes:
%%   call_stream(Procedure, Args, Opts) — LOCAL dispatch only
%%   call_stream(Client, Procedure, Args) — REMOTE via mesh client
-spec call_stream(procedure() | client(), procedure() | term(), term() | map()) ->
        {ok, stream()} | {error, term()}.
call_stream(Procedure, Args, Opts) when is_binary(Procedure), is_map(Opts) ->
    macula_stream_local:call_stream(Procedure, Args, Opts);
call_stream(Client, Procedure, Args) when is_pid(Client), is_binary(Procedure) ->
    macula_mesh_client:call_stream(Client, Procedure, Args, #{}).

%% @doc Open a remote server-stream call against a mesh client, with opts.
-spec call_stream(client(), procedure(), term(), map()) ->
        {ok, stream()} | {error, term()}.
call_stream(Client, Procedure, Args, Opts)
  when is_pid(Client), is_binary(Procedure), is_map(Opts) ->
    macula_mesh_client:call_stream(Client, Procedure, Args, Opts).

%% @doc Open a client-stream or bidi call.
%%
%% Two shapes:
%%   open_stream(Procedure, Args, Opts) — LOCAL dispatch
%%   open_stream(Client, Procedure, Args) — REMOTE, default mode bidi
-spec open_stream(procedure() | client(), procedure() | term(), term() | map()) ->
        {ok, stream()} | {error, term()}.
open_stream(Procedure, Args, Opts) when is_binary(Procedure), is_map(Opts) ->
    macula_stream_local:open_stream(Procedure, Args, Opts);
open_stream(Client, Procedure, Args) when is_pid(Client), is_binary(Procedure) ->
    macula_mesh_client:open_stream(Client, Procedure, Args, #{}).

%% @doc Open a stream with explicit mode.
%%
%% Two shapes:
%%   open_stream(Procedure, Args, Opts, Mode) — LOCAL dispatch, explicit mode
%%   open_stream(Client, Procedure, Args, Opts) — REMOTE via mesh client
-spec open_stream(procedure() | client(), procedure() | term(),
                  term() | map(), stream_mode() | map()) ->
        {ok, stream()} | {error, term()}.
open_stream(Procedure, Args, Opts, Mode)
  when is_binary(Procedure), is_map(Opts), is_atom(Mode) ->
    macula_stream_local:open_stream(Procedure, Args, Opts#{mode => Mode});
open_stream(Client, Procedure, Args, Opts)
  when is_pid(Client), is_binary(Procedure), is_map(Opts) ->
    macula_mesh_client:open_stream(Client, Procedure, Args, Opts).

%% @doc Advertise a streaming procedure (default: server_stream).
%% LOCAL dispatch only — see `advertise_stream/3,4' for the Client form.
-spec advertise_stream(procedure(), stream_handler()) -> ok | {error, term()}.
advertise_stream(Procedure, Handler)
  when is_binary(Procedure), is_function(Handler, 2) ->
    advertise_stream(Procedure, server_stream, Handler).

%% @doc Advertise a streaming procedure.
%%
%% Two shapes:
%%   advertise_stream(Procedure, Mode, Handler)   — LOCAL dispatch
%%   advertise_stream(Client, Procedure, Handler) — REMOTE, default
%%                                                  mode server_stream
-spec advertise_stream(procedure() | client(),
                       stream_mode() | procedure(),
                       stream_handler()) ->
        ok | {error, term()}.
advertise_stream(Procedure, Mode, Handler)
  when is_binary(Procedure), is_atom(Mode), is_function(Handler, 2) ->
    advertise_stream(Procedure, Mode, Handler, #{});
advertise_stream(Client, Procedure, Handler)
  when is_pid(Client), is_binary(Procedure), is_function(Handler, 2) ->
    macula_mesh_client:advertise_stream(Client, Procedure, server_stream,
                                        Handler).

%% @doc Advertise with explicit mode / options.
%%
%% Two shapes:
%%   advertise_stream(Procedure, Mode, Handler, Opts)   — LOCAL
%%   advertise_stream(Client, Procedure, Mode, Handler) — REMOTE
-spec advertise_stream(procedure() | client(),
                       stream_mode() | procedure(),
                       stream_handler() | stream_mode(),
                       map() | stream_handler()) ->
        ok | {error, term()}.
advertise_stream(Procedure, Mode, Handler, Opts)
  when is_binary(Procedure), is_function(Handler, 2), is_map(Opts) ->
    macula_stream_local:advertise(Procedure, Mode, Handler);
advertise_stream(Client, Procedure, Mode, Handler)
  when is_pid(Client), is_binary(Procedure), is_atom(Mode),
       is_function(Handler, 2) ->
    macula_mesh_client:advertise_stream(Client, Procedure, Mode, Handler).

%% @doc Stop advertising a streaming procedure.
-spec unadvertise_stream(procedure()) -> ok.
unadvertise_stream(Procedure) when is_binary(Procedure) ->
    macula_stream_local:unadvertise(Procedure).

%% @doc Send a binary chunk on the stream.
-spec send(stream(), binary()) -> ok | {error, term()}.
send(Stream, Bin) when is_pid(Stream), is_binary(Bin) ->
    macula_stream_v1:send(Stream, Bin).

%% @doc Send a chunk with explicit encoding.
-spec send(stream(), binary() | term(), raw | msgpack) -> ok | {error, term()}.
send(Stream, Body, Encoding) when is_pid(Stream) ->
    macula_stream_v1:send(Stream, Body, Encoding).

%% @doc Receive the next chunk (blocks).
-spec recv(stream()) -> {chunk, binary()}
                      | {data, term()}
                      | eof
                      | {error, term()}.
recv(Stream) when is_pid(Stream) ->
    macula_stream_v1:recv(Stream).

-spec recv(stream(), timeout()) -> {chunk, binary()}
                                 | {data, term()}
                                 | eof
                                 | {error, term()}.
recv(Stream, Timeout) when is_pid(Stream) ->
    macula_stream_v1:recv(Stream, Timeout).

%% @doc Close a V1 stream (both sides). Renamed from `close/1' in
%% 3.11.0 because `close/1' now refers to the V2 pool surface.
-spec close_stream(stream()) -> ok.
close_stream(Stream) when is_pid(Stream) ->
    macula_stream_v1:close(Stream).

%% @doc Half-close the write side; recv still drains.
-spec close_send(stream()) -> ok.
close_send(Stream) when is_pid(Stream) ->
    macula_stream_v1:close_send(Stream).

%% @doc Wait for the terminal reply (client-stream / bidi).
-spec await_reply(stream()) -> {ok, term()} | {error, term()}.
await_reply(Stream) when is_pid(Stream) ->
    macula_stream_v1:await_reply(Stream).

-spec await_reply(stream(), timeout()) -> {ok, term()} | {error, term()}.
await_reply(Stream, Timeout) when is_pid(Stream) ->
    macula_stream_v1:await_reply(Stream, Timeout).

%% @doc Server-side: emit the terminal reply value.
-spec set_reply(stream(), term()) -> ok.
set_reply(Stream, Result) when is_pid(Stream) ->
    macula_stream_v1:set_reply(Stream, Result).

%% @doc Abort the stream with an error frame.
-spec abort(stream(), binary(), binary()) -> ok.
abort(Stream, Code, Message)
  when is_pid(Stream), is_binary(Code), is_binary(Message) ->
    macula_stream_v1:abort(Stream, Code, Message).

%%%===================================================================
%%% Directed RPC (Mesh Name Service)
%%%===================================================================

%% @doc Call a remote procedure on a specific target node (default 5s timeout).
%%
%% Target can be a mesh name, site_id, or node_id (all binaries).
%% The relay resolves the target and routes the CALL directly to that node.
-spec call_node(client(), binary(), procedure(), term()) ->
    {ok, term()} | {error, term()}.
call_node(Client, Target, Procedure, Args) ->
    call_node(Client, Target, Procedure, Args, 5000).

%% @doc Call a remote procedure on a specific target node with timeout.
-spec call_node(client(), binary(), procedure(), term(), pos_integer()) ->
    {ok, term()} | {error, term()}.
call_node(Client, Target, Procedure, Args, Timeout)
  when is_pid(Client), is_binary(Target), is_binary(Procedure) ->
    macula_mesh_client:call(Client, Procedure, Args, Timeout, #{target => Target}).

%% @doc Resolve a mesh name to node identity information.
%%
%% Returns the node's identity including name, site_id, city, endpoint,
%% and connected_at timestamp. Works for names, site_ids, or node_ids.
-spec resolve(client(), binary()) -> {ok, map()} | {error, term()}.
resolve(Client, Name) when is_pid(Client), is_binary(Name) ->
    macula_mesh_client:call(Client, <<"_mesh.resolve">>,
                           #{<<"name">> => Name}, 5000).

%% @doc List all nodes connected to the relay (default 5s timeout).
-spec list_nodes(client()) -> {ok, map()} | {error, term()}.
list_nodes(Client) ->
    list_nodes(Client, #{}).

%% @doc List all nodes connected to the relay with options.
-spec list_nodes(client(), map()) -> {ok, map()} | {error, term()}.
list_nodes(Client, _Opts) when is_pid(Client) ->
    macula_mesh_client:call(Client, <<"_mesh.list_nodes">>, #{}, 5000).

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
%% Options: relays (required list), realm, identity, site, tls_verify.
-spec join_mesh(map()) -> ok | {error, term()}.
join_mesh(Opts) ->
    Relays = maps:get(relays, Opts),
    ClientOpts = #{
        relays => Relays,
        realm => maps:get(realm, Opts, <<"io.macula">>),
        identity => maps:get(identity, Opts, atom_to_binary(node())),
        tls_verify => maps:get(tls_verify, Opts, none),
        site => maps:get(site, Opts, undefined)
    },
    case macula_mesh_client:start_link(ClientOpts) of
        {ok, Client} ->
            %% Wait for relay connection before advertising
            wait_for_relay(Client, 30),
            os:putenv("MACULA_DIST_MODE", "relay"),
            macula_dist_relay:register_mesh_client(Client),
            macula_dist_relay:advertise_dist_accept(),
            ?LOG_INFO("[macula] Joined mesh — distribution enabled"),
            ok;
        {error, Reason} ->
            ?LOG_ERROR("[macula] Failed to join mesh: ~p", [Reason]),
            {error, Reason}
    end.

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
%% - `url' (required): `<<"quic://relay.example.com:4434">>'
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

%% @private Wait until the mesh client has an active QUIC stream.
wait_for_relay(_Client, 0) ->
    ?LOG_WARNING("[macula] Relay connection not ready after timeout");
wait_for_relay(Client, Retries) ->
    case catch macula_mesh_client:is_connected(Client) of
        true ->
            ?LOG_INFO("[macula] Relay connected"),
            ok;
        _ ->
            timer:sleep(1000),
            wait_for_relay(Client, Retries - 1)
    end.
