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

%% Streaming RPC (v1.5.0+ — see PLAN_MACULA_STREAMING.md)
-export([
    call_stream/2, call_stream/3,
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
              stream/0, stream_mode/0, stream_handler/0]).

-type client() :: pid().
-type topic() :: binary().
-type procedure() :: binary().

-type stream() :: pid().
-type stream_mode() :: server_stream | client_stream | bidi.
-type stream_handler() :: fun((stream(), term()) -> any()).

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
-spec call_stream(procedure(), term()) -> {ok, stream()} | {error, term()}.
call_stream(Procedure, Args) ->
    call_stream(Procedure, Args, #{}).

%% @doc Open a server-stream call with options.
-spec call_stream(procedure(), term(), map()) -> {ok, stream()} | {error, term()}.
call_stream(Procedure, Args, Opts) when is_binary(Procedure) ->
    macula_stream_local:call_stream(Procedure, Args, Opts).

%% @doc Open a client-stream or bidi call (mode in opts; default bidi).
-spec open_stream(procedure(), term(), map()) -> {ok, stream()} | {error, term()}.
open_stream(Procedure, Args, Opts) when is_binary(Procedure) ->
    macula_stream_local:open_stream(Procedure, Args, Opts).

%% @doc Open a stream with explicit mode.
-spec open_stream(procedure(), term(), map(), stream_mode()) ->
        {ok, stream()} | {error, term()}.
open_stream(Procedure, Args, Opts, Mode)
  when is_binary(Procedure), is_atom(Mode) ->
    macula_stream_local:open_stream(Procedure, Args, Opts#{mode => Mode}).

%% @doc Advertise a streaming procedure (default: server_stream).
-spec advertise_stream(procedure(), stream_handler()) -> ok | {error, term()}.
advertise_stream(Procedure, Handler) ->
    advertise_stream(Procedure, server_stream, Handler).

%% @doc Advertise a streaming procedure with explicit mode.
-spec advertise_stream(procedure(), stream_mode(), stream_handler()) ->
        ok | {error, term()}.
advertise_stream(Procedure, Mode, Handler) ->
    advertise_stream(Procedure, Mode, Handler, #{}).

%% @doc Advertise with options (reserved for Phase 2: ucan, etc.).
-spec advertise_stream(procedure(), stream_mode(), stream_handler(), map()) ->
        ok | {error, term()}.
advertise_stream(Procedure, Mode, Handler, _Opts)
  when is_binary(Procedure), is_function(Handler, 2) ->
    macula_stream_local:advertise(Procedure, Mode, Handler).

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
