%% @doc V2 station-client — outbound RPC over `macula_peering'.
%%
%% A `macula_station_client' is a `gen_server' that owns one
%% `macula_peering' connection to a single station endpoint. It
%% drives the CONNECT/HELLO handshake as the client side, then
%% sends application-layer CALL frames built with `macula_frame:call/1'
%% and matches inbound RESULT/ERROR frames against pending
%% `gen_server:call' callers using the 16-byte CALL id.
%%
%% This is the V2 counterpart to `macula_mesh_client', which speaks
%% the V1 relay protocol. V1 clients cannot drive V2 stations because
%% V2 stations dispatch the QUIC connection straight into
%% `macula_peering:accept/2' — V1 CONNECT frames never reach the
%% V2 handler registry. This module bridges the gap so that a V1-era
%% consumer (e.g. macula-realm's topology subscriber) can issue
%% `_dht.find_records_by_type' against a V2 station and receive its
%% signed RESULT.
%%
%% == Lifecycle ==
%%
%% <ol>
%%   <li>`start_link/1' — spawn worker, schedule connect.</li>
%%   <li>`connect_now/1' (cast) — build connect opts, call
%%       `macula_peering:connect/1', store the worker pid.</li>
%%   <li>Peering handshake completes → `{macula_peering, connected,
%%       Pid, PeerNodeId}' arrives → state moves to `connected'.</li>
%%   <li>`call/4' from caller → build CALL frame, sign happens inside
%%       peering, store `{from, deadline_timer}` keyed by CALL id, send
%%       frame via `macula_peering:send_frame/2'.</li>
%%   <li>RESULT or ERROR arrives as `{macula_peering, frame, Pid, Frame}'
%%       → look up `call_id', cancel timer, reply to caller.</li>
%%   <li>`{macula_peering, disconnected, Pid, Reason}' → fail all
%%       pending calls with `{error, {disconnected, Reason}}', stop the
%%       client (caller is responsible for restart / reconnect).</li>
%% </ol>
%%
%% == Reply taxonomy ==
%%
%% <table>
%%   <tr><th>Inbound frame</th><th>`call/4' returns</th></tr>
%%   <tr><td>RESULT(payload=`{error, Reason}')</td><td>`{ok, {error, Reason}}'</td></tr>
%%   <tr><td>RESULT(payload=Value)</td><td>`{ok, Value}'</td></tr>
%%   <tr><td>ERROR(code=C, name=N)</td><td>`{error, {call_error, C, N}}'</td></tr>
%%   <tr><td>(deadline elapses)</td><td>`{error, timeout}'</td></tr>
%%   <tr><td>(connection drops)</td><td>`{error, {disconnected, Reason}}'</td></tr>
%% </table>
%%
%% == Realm field ==
%%
%% V2 CALL frames carry a 32-byte `realm' id. Stations deployed today
%% advertise an empty `realms' list (realm-agnostic infrastructure)
%% and do not enforce a realm match on inbound CALLs — the dispatch
%% path verifies the signature and looks up the procedure, nothing
%% more. Callers therefore pass any 32-byte value; this module
%% defaults to all-zeros when no realm is configured.
-module(macula_station_client).
-behaviour(gen_server).

-export([
    start_link/1,
    stop/1,
    call/4,
    put_record/2, put_record/3,
    find_record/2, find_record/3,
    find_records_by_type/2, find_records_by_type/3,
    is_connected/1,
    peer_node_id/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([opts/0]).

-type url() :: binary() | string().

-type opts() :: #{
    %% Endpoint to dial. Either a URL (https://host:port) or a
    %% pre-parsed #{host, port} map.
    seed     := url() | #{host := binary() | string(),
                          port := inet:port_number()},
    %% Local Ed25519 keypair used to sign the CONNECT frame and any
    %% subsequent application frames. Auto-generated when absent.
    identity => macula_identity:key_pair(),
    %% 32-byte realm ID stamped on outbound CALL frames. Defaults to
    %% all-zeros for realm-agnostic clients.
    realm    => <<_:256>>,
    %% Capability bitfield announced in CONNECT (default 0).
    capabilities => non_neg_integer(),
    %% ALPN list passed through to QUIC (default [<<"macula">>]).
    alpn         => [binary()],
    %% Connect timeout in ms (default 30_000).
    connect_timeout_ms => non_neg_integer()
}.

-define(DEFAULT_REALM, <<0:256>>).
-define(DEFAULT_DEADLINE_MS, 5_000).
-define(CONNECT_RETRY_BACKOFF_MS, 1_000).

-record(state, {
    seed             :: #{host := binary() | string(),
                          port := inet:port_number()},
    identity         :: macula_identity:key_pair(),
    realm            :: <<_:256>>,
    capabilities     :: non_neg_integer(),
    alpn             :: [binary()],
    connect_timeout_ms :: non_neg_integer(),
    %% peering worker pid (`macula_peering_conn`). undefined while
    %% disconnected.
    peer_pid         :: pid() | undefined,
    %% peer's node id, set on `connected'.
    peer_node_id     :: macula_identity:pubkey() | undefined,
    %% map of CALL id (16 bytes) -> {From, TimerRef}.
    pending = #{}    :: #{<<_:128>> => {gen_server:from(), reference()}}
}).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start a station-client connected to `seed'.
%% Returns once the gen_server is alive; the QUIC handshake completes
%% asynchronously. Use `is_connected/1' to poll readiness or just
%% issue `call/4' (which blocks the caller until ready or until its
%% timeout elapses).
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Issue a CALL frame and block until the station replies, the
%% deadline elapses, or the connection drops.
%%
%% `Procedure' is the V2 procedure name, e.g.
%% `<<"_dht.find_records_by_type">>'. `Payload' is any term that
%% `macula_frame:call/1' accepts (typically a map).
-spec call(pid(), binary(), term(), pos_integer()) ->
    {ok, term()} | {error, term()}.
call(Pid, Procedure, Payload, TimeoutMs)
  when is_pid(Pid), is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0 ->
    %% gen_server timeout = TimeoutMs + 500 to give the server time to
    %% report a clean `{error, timeout}' rather than the caller seeing
    %% a hard `exit({timeout, ...})'.
    GenTimeout = TimeoutMs + 500,
    try
        gen_server:call(Pid, {call, Procedure, Payload, TimeoutMs}, GenTimeout)
    catch
        exit:{timeout, _}      -> {error, timeout};
        exit:{noproc, _}       -> {error, noproc};
        exit:{normal, _}       -> {error, gone}
    end.

%% @doc Convenience wrapper for `_dht.put_record'. The record must be
%% a fully-signed `macula_record:record()' map (build via
%% `macula_record:envelope/3,4' + `macula_record:sign/2'). Returns
%% `ok' on success, `{error, Reason}' on RPC failure or unexpected
%% reply.
%%
%% Stations replicate the put across the K-nearest peers in their
%% Kademlia routing table, so a single `put_record/2' call against
%% any one connected station propagates to the rest of the DHT.
-spec put_record(pid(), map()) -> ok | {error, term()}.
put_record(Pid, Record) ->
    put_record(Pid, Record, ?DEFAULT_DEADLINE_MS).

-spec put_record(pid(), map(), pos_integer()) -> ok | {error, term()}.
put_record(Pid, Record, TimeoutMs) when is_pid(Pid), is_map(Record) ->
    classify_put(call(Pid, <<"_dht.put_record">>, Record, TimeoutMs)).

classify_put({ok, ok})       -> ok;
classify_put({ok, Other})    -> {error, {unexpected_reply, Other}};
classify_put({error, _} = E) -> E.

%% @doc Convenience wrapper for `_dht.find_record'. Looks up a record
%% by its `macula_record:storage_key/1' (32-byte BLAKE3 digest).
%% Returns `{error, not_found}' when no record exists at the key.
%% Callers SHOULD verify the returned record's signature with
%% `macula_record:verify/1' before trusting its payload.
-spec find_record(pid(), <<_:256>>) ->
    {ok, map()} | {error, not_found | term()}.
find_record(Pid, Key) ->
    find_record(Pid, Key, ?DEFAULT_DEADLINE_MS).

-spec find_record(pid(), <<_:256>>, pos_integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_record(Pid, Key, TimeoutMs)
  when is_pid(Pid), is_binary(Key), byte_size(Key) =:= 32 ->
    classify_find(call(Pid, <<"_dht.find_record">>, #{key => Key}, TimeoutMs)).

classify_find({ok, #{type := _, payload := _, sig := _} = R}) -> {ok, R};
classify_find({ok, not_found})   -> {error, not_found};
classify_find({ok, Other})       -> {error, {unexpected_reply, Other}};
classify_find({error, _} = E)    -> E.

%% @doc Convenience wrapper for `_dht.find_records_by_type'. Returns
%% the decoded list of signed records (CBOR-decoded maps as produced
%% by `macula_record').
-spec find_records_by_type(pid(), 0..255) ->
    {ok, [map()]} | {error, term()}.
find_records_by_type(Pid, Type) ->
    find_records_by_type(Pid, Type, ?DEFAULT_DEADLINE_MS).

-spec find_records_by_type(pid(), 0..255, pos_integer()) ->
    {ok, [map()]} | {error, term()}.
find_records_by_type(Pid, Type, TimeoutMs)
  when is_integer(Type), Type >= 0, Type =< 255 ->
    classify_records(call(Pid, <<"_dht.find_records_by_type">>,
                          #{type => Type}, TimeoutMs)).

classify_records({ok, Records}) when is_list(Records) -> {ok, Records};
classify_records({ok, Other})                          -> {error, {unexpected_reply, Other}};
classify_records({error, _} = E)                       -> E.

-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    case gen_server:call(Pid, is_connected, 1_000) of
        true  -> true;
        false -> false
    end.

-spec peer_node_id(pid()) -> {ok, macula_identity:pubkey()} | {error, not_connected}.
peer_node_id(Pid) ->
    gen_server:call(Pid, peer_node_id, 1_000).

%%====================================================================
%% gen_server
%%====================================================================

init(Opts) ->
    Seed     = parse_seed(maps:get(seed, Opts)),
    Identity = maps:get(identity, Opts, macula_identity:generate()),
    Realm    = maps:get(realm, Opts, ?DEFAULT_REALM),
    Caps     = maps:get(capabilities, Opts, 0),
    Alpn     = maps:get(alpn, Opts, [<<"macula">>]),
    Tmo      = maps:get(connect_timeout_ms, Opts, 30_000),
    State    = #state{seed = Seed, identity = Identity, realm = Realm,
                      capabilities = Caps, alpn = Alpn,
                      connect_timeout_ms = Tmo},
    process_flag(trap_exit, true),
    self() ! attempt_connect,
    {ok, State}.

handle_call({call, _Proc, _Payload, _Tmo}, _From,
            #state{peer_pid = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call({call, Proc, Payload, Tmo}, From,
            #state{peer_pid = Pid, identity = Id, realm = Realm,
                   pending = P} = S) ->
    CallId = crypto:strong_rand_bytes(16),
    Caller = macula_identity:public(Id),
    DeadlineMs = erlang:system_time(millisecond) + Tmo,
    Frame = macula_frame:call(#{
        call_id     => CallId,
        procedure   => Proc,
        realm       => Realm,
        payload     => Payload,
        deadline_ms => DeadlineMs,
        caller      => Caller
    }),
    ok = macula_peering:send_frame(Pid, Frame),
    TRef = erlang:send_after(Tmo, self(), {call_timeout, CallId}),
    {noreply, S#state{pending = P#{CallId => {From, TRef}}}};
handle_call(is_connected, _From, #state{peer_pid = undefined} = S) ->
    {reply, false, S};
handle_call(is_connected, _From, #state{peer_node_id = undefined} = S) ->
    {reply, false, S};
handle_call(is_connected, _From, S) ->
    {reply, true, S};
handle_call(peer_node_id, _From, #state{peer_node_id = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call(peer_node_id, _From, #state{peer_node_id = Id} = S) ->
    {reply, {ok, Id}, S};
handle_call(_Req, _From, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Msg, S) -> {noreply, S}.

%%-------------------------------------------------------------------
%% Connect
%%-------------------------------------------------------------------

handle_info(attempt_connect, #state{seed = Seed, identity = Id, realm = Realm,
                                    capabilities = Caps, alpn = Alpn,
                                    connect_timeout_ms = Tmo} = S) ->
    Pub = macula_identity:public(Id),
    PeeringOpts = #{
        role            => client,
        target          => Seed#{alpn => Alpn, timeout_ms => Tmo},
        node_id         => Pub,
        identity        => Id,
        realms          => realms_for_connect(Realm),
        capabilities    => Caps,
        controlling_pid => self()
    },
    after_connect_request(macula_peering:connect(PeeringOpts), S);

handle_info({macula_peering, connected, Pid, PeerNodeId},
            #state{peer_pid = Pid} = S) ->
    {noreply, S#state{peer_node_id = PeerNodeId}};

handle_info({macula_peering, frame, Pid, Frame},
            #state{peer_pid = Pid} = S) ->
    {noreply, on_frame(Frame, S)};

handle_info({macula_peering, disconnected, Pid, Reason},
            #state{peer_pid = Pid} = S) ->
    NewS = fail_all_pending({disconnected, Reason}, S),
    %% Stop normally — the supervisor (or owning gen_server) decides
    %% whether to restart us.
    {stop, normal, NewS#state{peer_pid = undefined,
                              peer_node_id = undefined}};

handle_info({call_timeout, CallId}, #state{pending = P} = S) ->
    on_timeout(maps:take(CallId, P), S);

handle_info({'EXIT', Pid, Reason}, #state{peer_pid = Pid} = S) ->
    NewS = fail_all_pending({peering_exit, Reason}, S),
    {stop, normal, NewS#state{peer_pid = undefined,
                              peer_node_id = undefined}};

handle_info(_Other, S) ->
    {noreply, S}.

terminate(_Reason, #state{peer_pid = Pid}) when is_pid(Pid) ->
    catch macula_peering:close(Pid, client_stop),
    ok;
terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%====================================================================
%% Internals
%%====================================================================

after_connect_request({ok, Pid}, S) ->
    link(Pid),
    {noreply, S#state{peer_pid = Pid}};
after_connect_request({error, Reason}, S) ->
    macula_diagnostics:event(<<"_macula.station_client.connect_failed">>, #{
        reason => Reason,
        seed   => S#state.seed
    }),
    erlang:send_after(?CONNECT_RETRY_BACKOFF_MS, self(), attempt_connect),
    {noreply, S}.

%% RESULT
on_frame(#{frame_type := result, call_id := CallId, payload := Payload},
         #state{pending = P} = S) ->
    deliver_pending(maps:take(CallId, P), {ok, Payload}, S);
%% ERROR
on_frame(#{frame_type := error, call_id := CallId} = Frame,
         #state{pending = P} = S) ->
    Code = maps:get(code, Frame, 0),
    Name = maps:get(name, Frame, undefined),
    deliver_pending(maps:take(CallId, P),
                    {error, {call_error, Code, Name}}, S);
%% Anything else (HyParView, Plumtree, SWIM, content) — ignore for
%% the realm-topology client. A dedicated overlay subscriber lives
%% in a separate module.
on_frame(_Frame, S) ->
    S.

deliver_pending(error, _Reply, S) ->
    %% Unknown call_id (race with timeout, or duplicate reply).
    S;
deliver_pending({{From, TRef}, NewP}, Reply, S) ->
    _ = erlang:cancel_timer(TRef),
    gen_server:reply(From, Reply),
    S#state{pending = NewP}.

on_timeout(error, S) ->
    {noreply, S};
on_timeout({{From, _OldTRef}, NewP}, S) ->
    gen_server:reply(From, {error, timeout}),
    {noreply, S#state{pending = NewP}}.

fail_all_pending(Reason, #state{pending = P} = S) ->
    maps:foreach(fun(_CallId, {From, TRef}) ->
        _ = erlang:cancel_timer(TRef),
        gen_server:reply(From, {error, Reason})
    end, P),
    S#state{pending = #{}}.

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------

%% A realm-agnostic client (realm = all-zeros) should advertise an
%% empty realms list in CONNECT — the V2 station spec uses an empty
%% list to mean "no realm membership claimed".
realms_for_connect(<<0:256>>) -> [];
realms_for_connect(R) when is_binary(R), byte_size(R) =:= 32 -> [R].

parse_seed(#{host := _, port := _} = Map) ->
    Map;
parse_seed(Url) when is_binary(Url) ->
    parse_seed(binary_to_list(Url));
parse_seed(Url) when is_list(Url) ->
    case uri_string:parse(Url) of
        #{host := H, port := P} when is_integer(P) ->
            #{host => list_to_binary(H), port => P};
        #{host := H, scheme := "https"} ->
            #{host => list_to_binary(H), port => 4433};
        _ ->
            error({invalid_seed_url, Url})
    end.
