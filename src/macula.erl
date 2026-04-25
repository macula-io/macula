%%%-------------------------------------------------------------------
%%% @doc Macula SDK — Public API for mesh applications.
%%%
%%% This is the main entry point for applications using the Macula SDK.
%%% All functions delegate to macula_mesh_client or macula_multi_relay
%%% for relay mesh communication.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula).

-include_lib("kernel/include/logger.hrl").

%% Connection
-export([connect/2, disconnect/1]).

%% Pub/Sub
-export([subscribe/3, unsubscribe/2, publish/3, publish/4]).

%% RPC
-export([call/3, call/4, advertise/3, advertise/4, unadvertise/2]).

%% Signed DHT records (v3.2.0)
-export([put_record/2,
         find_record/2,
         find_records_by_type/2,
         subscribe_records/3,
         unsubscribe_records/2]).

%% Streaming RPC (v1.5.0+ — see PLAN_MACULA_STREAMING.md)
-export([
    call_stream/2, call_stream/3, call_stream/4,
    open_stream/3, open_stream/4,
    advertise_stream/2, advertise_stream/3, advertise_stream/4,
    unadvertise_stream/1,
    send/2, send/3,
    recv/1, recv/2,
    close/1, close_send/1,
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
-export_type([client/0, topic/0, procedure/0,
              stream/0, stream_mode/0, stream_handler/0,
              record/0, record_type/0, record_key/0]).

-type client() :: pid().
-type topic() :: binary().
-type procedure() :: binary().

-type stream() :: pid().
-type stream_mode() :: server_stream | client_stream | bidi.
-type stream_handler() :: fun((stream(), term()) -> any()).

-type record()      :: macula_record:record().
-type record_type() :: macula_record:record_type().
-type record_key()  :: macula_record:record_key().

%%%===================================================================
%%% Connection
%%%===================================================================

%% @doc Connect to a Macula relay.
%%
%% Returns a client PID (macula_mesh_client gen_server) that you pass
%% to all other API functions. Accepts a single URL binary or a list.
-spec connect(binary() | [binary()], map()) -> {ok, client()} | {error, term()}.
connect(Url, Opts) when is_binary(Url) ->
    connect([Url], Opts);
connect(Relays, Opts) when is_list(Relays), is_map(Opts) ->
    Connections = maps:get(connections, Opts, 1),
    ClientOpts = Opts#{relays => Relays},
    case Connections of
        1 ->
            macula_mesh_client:start_link(ClientOpts);
        N when N > 1 ->
            macula_multi_relay:start_link(ClientOpts#{connections => N})
    end.

%% @doc Disconnect from the relay.
-spec disconnect(client()) -> ok.
disconnect(Client) when is_pid(Client) ->
    macula_mesh_client:stop(Client).

%%%===================================================================
%%% Pub/Sub
%%%===================================================================

%% @doc Subscribe to a topic.
%%
%% The callback receives the event payload (map or binary).
%% Returns a subscription reference for unsubscribing.
-spec subscribe(client(), topic(), fun((term()) -> ok) | pid()) ->
    {ok, reference()} | {error, term()}.
subscribe(Client, Topic, Callback) when is_pid(Client), is_binary(Topic) ->
    macula_mesh_client:subscribe(Client, Topic, Callback).

%% @doc Unsubscribe from a topic.
-spec unsubscribe(client(), reference()) -> ok | {error, term()}.
unsubscribe(Client, SubRef) when is_pid(Client) ->
    macula_mesh_client:unsubscribe(Client, SubRef).

%% @doc Publish an event to a topic (fire-and-forget).
-spec publish(client(), topic(), term()) -> ok.
publish(Client, Topic, Data) ->
    publish(Client, Topic, Data, #{}).

%% @doc Publish an event with options.
-spec publish(client(), topic(), term(), map()) -> ok.
publish(Client, Topic, Data, _Opts) when is_pid(Client), is_binary(Topic) ->
    macula_mesh_client:publish(Client, Topic, Data).

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
%%% Signed DHT records (v3.2.0)
%%%===================================================================
%%%
%%% Records are typed, signed, content-addressed payloads stored in
%%% the relay mesh's distributed hash table. Two complementary
%%% retrieval paths:
%%%
%%%   - `find_record/2'           — fetch one record by its content key
%%%   - `find_records_by_type/2'  — list every record of a given type tag
%%%
%%% Plus a live-update channel:
%%%
%%%   - `subscribe_records/3'     — receive new records of a type as
%%%                                 they are stored
%%%
%%% See `macula_record' for the record shape, signing scheme, and
%%% key derivation. Type tags are application-defined `0..255' bytes.
%%% The SDK treats payloads as opaque — applications layer their own
%%% schemas on top (e.g. `hecate' uses tag `16#02' for stations,
%%% `16#11' for content announcements).

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
%% The record must already be built and signed via
%% `macula_record:build/4'. The relay validates the signature on
%% receipt; an invalid signature returns `{error, bad_signature}'.
%% Successful stores propagate to the K-nearest peers in the DHT
%% under the record's content key.
-spec put_record(client(), record()) -> ok | {error, term()}.
put_record(Client, Record) when is_pid(Client), is_map(Record) ->
    classify_put(macula_mesh_client:call(Client, ?DHT_PUT_RECORD_PROC,
                                         Record, ?DHT_RECORD_TIMEOUT_MS)).

classify_put({ok, ok})       -> ok;
classify_put({ok, Reply})    -> {error, {unexpected_reply, Reply}};
classify_put({error, _} = E) -> E.

%% @doc Fetch a record by its content-address key.
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
%%%             macula:close(Stream)
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

%% @doc Close the stream (both sides).
-spec close(stream()) -> ok.
close(Stream) when is_pid(Stream) ->
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
