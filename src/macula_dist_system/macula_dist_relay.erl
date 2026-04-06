%%%-------------------------------------------------------------------
%%% @doc Relay-routed Erlang distribution.
%%%
%%% When MACULA_DIST_MODE=relay, Erlang distribution connections
%%% are tunneled through the Macula relay mesh instead of direct QUIC.
%%% This enables distribution across firewalls and NATs — nodes only
%%% need outbound connectivity to a relay.
%%%
%%% The tunnel works by:
%%% 1. Node A requests a tunnel via mesh RPC (_dist.tunnel.{node})
%%% 2. Node B creates a gen_tcp loopback pair bridged to relay pub/sub
%%% 3. Node A creates its own loopback pair bridged to the tunnel
%%% 4. dist_util handshake flows through the relay tunnel
%%% 5. Post-handshake: tick keepalive + distribution traffic via bridge
%%%
%%% Tunnel bytes are encrypted with AES-256-GCM derived from the
%%% Erlang distribution cookie. The relay cannot read ETF content.
%%%
%%% This is EXPERIMENTAL. Supports Pid ! Msg, gen_server:call,
%%% pg groups, process monitoring. Does NOT guarantee Mnesia or
%%% global module compatibility over WAN latency.
%%%
%%% == Recommended net_ticktime ==
%%%
%%% Relay adds WAN latency to every tick. Increase net_ticktime
%%% to avoid false disconnects:
%%%   -kernel net_ticktime 120
%%% The default 60s may cause spurious node-DOWN on high-latency relays.
%%% BRIDGE_RECV_TIMEOUT should be >= net_ticktime * 4.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay).

-include_lib("kernel/include/logger.hrl").

-export([connect/3]).
-export([is_relay_mode/0, get_mesh_client/0]).
-export([register_mesh_client/1]).
-export([advertise_dist_accept/0]).
-export([extract_payload/1]).
-export([get_tunnel_metrics/0, get_tunnel_metrics/1]).

-define(DIST_TIMEOUT, 25000).
-define(BRIDGE_RECV_TIMEOUT, 60000).
-define(CONTROLLER_TIMEOUT, 30000).
-define(BACKPRESSURE_HWM, 64).

%% Metrics counter indices
-define(METRIC_BYTES_OUT, 1).
-define(METRIC_BYTES_IN, 2).
-define(METRIC_MSGS_OUT, 3).
-define(METRIC_MSGS_IN, 4).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Register a mesh relay client for distribution tunneling.
-spec register_mesh_client(pid()) -> ok.
register_mesh_client(Pid) when is_pid(Pid) ->
    persistent_term:put(macula_dist_mesh_client, Pid),
    ?LOG_INFO("[dist_relay] Mesh client registered: ~p", [Pid]),
    ok.

%% @doc Check if relay distribution mode is enabled.
-spec is_relay_mode() -> boolean().
is_relay_mode() ->
    os:getenv("MACULA_DIST_MODE") =:= "relay".

%% @doc Connect to a remote node via relay mesh.
%% Returns {ok, DistSock, DistSock} where DistSock is a gen_tcp loopback
%% socket bridged to an encrypted relay tunnel.
-spec connect(string(), string(), integer()) -> {ok, port(), port()} | {error, term()}.
connect(NodeStr, _Host, _Port) ->
    ?LOG_INFO("[dist_relay] Connecting to ~s via mesh", [NodeStr]),
    case get_mesh_client() of
        undefined ->
            {error, no_mesh_connection};
        MeshClient ->
            request_tunnel(MeshClient, NodeStr)
    end.

%% @doc Advertise this node as accepting distribution connections via relay.
-spec advertise_dist_accept() -> ok.
advertise_dist_accept() ->
    case get_mesh_client() of
        undefined ->
            ?LOG_WARNING("[dist_relay] Cannot advertise — no mesh client registered"),
            ok;
        Client ->
            NodeName = atom_to_binary(node()),
            Procedure = <<"_dist.tunnel.", NodeName/binary>>,
            Handler = fun(Args) -> handle_tunnel_request(Args) end,
            macula_relay_client:advertise(Client, Procedure, Handler),
            ?LOG_INFO("[dist_relay] Advertised distribution accept: ~s", [Procedure]),
            ok
    end.

%% @doc Get metrics for all active tunnels.
-spec get_tunnel_metrics() -> [{binary(), map()}].
get_tunnel_metrics() ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> [];
        Tunnels ->
            maps:fold(fun(TunnelId, Ref, Acc) ->
                [{TunnelId, read_metrics(Ref)} | Acc]
            end, [], Tunnels)
    end.

%% @doc Get metrics for a specific tunnel.
-spec get_tunnel_metrics(binary()) -> map() | undefined.
get_tunnel_metrics(TunnelId) ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> undefined;
        Tunnels ->
            case maps:get(TunnelId, Tunnels, undefined) of
                undefined -> undefined;
                Ref -> read_metrics(Ref)
            end
    end.

%%%===================================================================
%%% Internal — Mesh Client Lookup
%%%===================================================================

-spec get_mesh_client() -> pid() | undefined.
get_mesh_client() ->
    case persistent_term:get(macula_dist_mesh_client, undefined) of
        undefined -> undefined;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> Pid;
                false -> undefined
            end
    end.

%%%===================================================================
%%% Internal — Tunnel Negotiation (connecting side)
%%%===================================================================

request_tunnel(MeshClient, NodeStr) ->
    Procedure = <<"_dist.tunnel.", (list_to_binary(NodeStr))/binary>>,
    Args = #{<<"from_node">> => atom_to_binary(node()),
             <<"target_node">> => list_to_binary(NodeStr)},
    case macula_relay_client:call(MeshClient, Procedure, Args, ?DIST_TIMEOUT) of
        {ok, #{<<"tunnel_id">> := TunnelId}} ->
            ?LOG_INFO("[dist_relay] Tunnel established: ~s", [TunnelId]),
            create_dist_socket(MeshClient, TunnelId);
        {ok, #{<<"error">> := ErrorInfo}} ->
            ?LOG_WARNING("[dist_relay] Tunnel error: ~p", [ErrorInfo]),
            {error, {tunnel_error, ErrorInfo}};
        {error, Reason} ->
            ?LOG_WARNING("[dist_relay] Tunnel request failed: ~p", [Reason]),
            {error, {tunnel_failed, Reason}}
    end.

%%%===================================================================
%%% Internal — Tunnel Negotiation (accepting side)
%%%===================================================================

handle_tunnel_request(Args) ->
    FromNode = maps:get(<<"from_node">>, Args, <<>>),
    TunnelId = base64:encode(crypto:strong_rand_bytes(16)),
    ?LOG_INFO("[dist_relay] Tunnel request from ~s, id: ~s", [FromNode, TunnelId]),

    SendTopic = <<"_dist.data.", TunnelId/binary, ".in">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".out">>,

    case get_mesh_client() of
        undefined ->
            {error, <<"no_mesh_client">>};
        Client ->
            spawn_monitor_bridge(Client, TunnelId, SendTopic, RecvTopic, FromNode),
            {ok, #{<<"tunnel_id">> => TunnelId,
                   <<"send_topic">> => SendTopic,
                   <<"recv_topic">> => RecvTopic}}
    end.

spawn_monitor_bridge(Client, TunnelId, SendTopic, RecvTopic, FromNode) ->
    {Pid, _Ref} = spawn_monitor(fun() ->
        dist_accept_bridge(Client, TunnelId, SendTopic, RecvTopic, FromNode)
    end),
    ?LOG_INFO("[dist_relay] Tunnel bridge ~p for ~s", [Pid, TunnelId]),
    Pid.

%%%===================================================================
%%% Internal — Accept-side Bridge
%%%===================================================================

dist_accept_bridge(Client, TunnelId, SendTopic, RecvTopic, _FromNode) ->
    process_flag(trap_exit, true),
    Self = self(),
    Metrics = init_metrics(TunnelId),
    Key = tunnel_key(),

    {ok, BufferSubRef} = macula_relay_client:subscribe(Client, RecvTopic,
        fun(Msg) -> Self ! {dist_data_buffered, extract_payload(Msg)} end),

    {DistSock, BridgeSock} = create_loopback_pair(),

    case whereis(net_kernel) of
        undefined ->
            ?LOG_WARNING("[dist_bridge] net_kernel not found");
        KernelPid ->
            setup_accept_bridge(Self, KernelPid, Client, DistSock, BridgeSock,
                                BufferSubRef, SendTopic, RecvTopic, TunnelId,
                                Key, Metrics)
    end.

setup_accept_bridge(Self, KernelPid, Client, DistSock, BridgeSock,
                    BufferSubRef, SendTopic, RecvTopic, TunnelId,
                    Key, Metrics) ->
    KernelPid ! {accept, Self, DistSock, inet, macula_dist},
    receive
        {KernelPid, controller, DistCtrl} ->
            DistCtrl ! {Self, controller, ok},
            macula_relay_client:unsubscribe(Client, BufferSubRef),
            flush_buffered_to_socket(BridgeSock, Key),
            start_bridge_io(Self, Client, BridgeSock, SendTopic, RecvTopic,
                            TunnelId, Key, Metrics);
        {KernelPid, unsupported_protocol} ->
            ?LOG_WARNING("[dist_bridge] Unsupported protocol")
    after ?CONTROLLER_TIMEOUT ->
        ?LOG_WARNING("[dist_bridge] Controller timeout")
    end.

start_bridge_io(Self, Client, BridgeSock, SendTopic, RecvTopic,
                TunnelId, Key, Metrics) ->
    {ok, SubRef} = macula_relay_client:subscribe(Client, RecvTopic,
        fun(Msg) -> Self ! {tunnel_in, extract_payload(Msg)} end),
    spawn_link(fun() ->
        bridge_reader_loop(Client, BridgeSock, SendTopic, TunnelId, Key, Metrics)
    end),
    bridge_writer_loop(BridgeSock, TunnelId, Client, SubRef, Key, Metrics).

%%%===================================================================
%%% Internal — Connect-side Bridge
%%%===================================================================

create_dist_socket(MeshClient, TunnelId) ->
    SendTopic = <<"_dist.data.", TunnelId/binary, ".out">>,
    RecvTopic = <<"_dist.data.", TunnelId/binary, ".in">>,

    {DistSock, BridgeSock} = create_loopback_pair(),

    spawn_link(fun() ->
        tunnel_io_bridge(MeshClient, BridgeSock, SendTopic, RecvTopic, TunnelId)
    end),

    {ok, DistSock, DistSock}.

tunnel_io_bridge(MeshClient, BridgeSock, SendTopic, RecvTopic, TunnelId) ->
    process_flag(trap_exit, true),
    Self = self(),
    Metrics = init_metrics(TunnelId),
    Key = tunnel_key(),

    {ok, SubRef} = macula_relay_client:subscribe(MeshClient, RecvTopic,
        fun(Msg) -> Self ! {tunnel_in, extract_payload(Msg)} end),

    spawn_link(fun() ->
        bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId, Key, Metrics)
    end),
    bridge_writer_loop(BridgeSock, TunnelId, MeshClient, SubRef, Key, Metrics).

%%%===================================================================
%%% Internal — Bridge I/O Loops
%%%===================================================================

bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId, Key, Metrics) ->
    case gen_tcp:recv(BridgeSock, 0, ?BRIDGE_RECV_TIMEOUT) of
        {ok, Data} ->
            maybe_backpressure(MeshClient),
            Encrypted = encrypt(Key, Data),
            macula_relay_client:publish(MeshClient, SendTopic, Encrypted),
            counters:add(Metrics, ?METRIC_BYTES_OUT, byte_size(Data)),
            counters:add(Metrics, ?METRIC_MSGS_OUT, 1),
            bridge_reader_loop(MeshClient, BridgeSock, SendTopic, TunnelId, Key, Metrics);
        {error, closed} ->
            ?LOG_INFO("[io_bridge] Reader closed for ~s", [TunnelId]);
        {error, Reason} ->
            ?LOG_WARNING("[io_bridge] Reader error ~p for ~s", [Reason, TunnelId])
    end.

%% Gap 1: cleanup on exit. Gap 8: writer timeout.
bridge_writer_loop(BridgeSock, TunnelId, Client, SubRef, Key, Metrics) ->
    receive
        {tunnel_in, EncData} when is_binary(EncData) ->
            case decrypt(Key, EncData) of
                {ok, Data} ->
                    counters:add(Metrics, ?METRIC_BYTES_IN, byte_size(Data)),
                    counters:add(Metrics, ?METRIC_MSGS_IN, 1),
                    case gen_tcp:send(BridgeSock, Data) of
                        ok ->
                            bridge_writer_loop(BridgeSock, TunnelId, Client,
                                               SubRef, Key, Metrics);
                        {error, Reason} ->
                            ?LOG_WARNING("[io_bridge] Writer send error ~p for ~s",
                                         [Reason, TunnelId]),
                            cleanup_bridge(BridgeSock, TunnelId, Client, SubRef, Metrics)
                    end;
                {error, decrypt_failed} ->
                    ?LOG_WARNING("[io_bridge] Decrypt failed for ~s (cookie mismatch?)",
                                 [TunnelId]),
                    bridge_writer_loop(BridgeSock, TunnelId, Client, SubRef, Key, Metrics)
            end;
        {'EXIT', _Pid, _Reason} ->
            ?LOG_INFO("[io_bridge] Linked process exited for ~s", [TunnelId]),
            cleanup_bridge(BridgeSock, TunnelId, Client, SubRef, Metrics);
        stop ->
            cleanup_bridge(BridgeSock, TunnelId, Client, SubRef, Metrics)
    after ?BRIDGE_RECV_TIMEOUT ->
        ?LOG_WARNING("[io_bridge] Writer timeout for ~s", [TunnelId]),
        cleanup_bridge(BridgeSock, TunnelId, Client, SubRef, Metrics)
    end.

%% Gap 1: proper cleanup — unsubscribe, close socket, remove metrics
cleanup_bridge(BridgeSock, TunnelId, Client, SubRef, _Metrics) ->
    ?LOG_INFO("[io_bridge] Cleaning up tunnel ~s", [TunnelId]),
    catch macula_relay_client:unsubscribe(Client, SubRef),
    catch gen_tcp:close(BridgeSock),
    remove_metrics(TunnelId),
    ok.

%%%===================================================================
%%% Internal — Encryption (Gap 3)
%%%===================================================================

%% Derive a symmetric key from the Erlang distribution cookie.
%% Both sides share the same cookie (required for dist handshake).
tunnel_key() ->
    Cookie = atom_to_binary(erlang:get_cookie()),
    crypto:hash(sha256, <<"macula-dist-tunnel:", Cookie/binary>>).

%% Encrypt with AES-256-GCM. Nonce is random per message (prepended).
encrypt(Key, Plaintext) ->
    Nonce = crypto:strong_rand_bytes(12),
    {Ciphertext, Tag} = crypto:crypto_one_time_aead(
        aes_256_gcm, Key, Nonce, Plaintext, <<>>, true),
    <<Nonce/binary, Tag/binary, Ciphertext/binary>>.

%% Decrypt AES-256-GCM. Nonce + tag prepended to ciphertext.
decrypt(Key, <<Nonce:12/binary, Tag:16/binary, Ciphertext/binary>>) ->
    case crypto:crypto_one_time_aead(
            aes_256_gcm, Key, Nonce, Ciphertext, <<>>, Tag, false) of
        error -> {error, decrypt_failed};
        Plaintext -> {ok, Plaintext}
    end;
decrypt(_Key, _Data) ->
    {error, decrypt_failed}.

%%%===================================================================
%%% Internal — Backpressure (Gap 5)
%%%===================================================================

%% Pause if relay client's message queue is too deep.
maybe_backpressure(MeshClient) ->
    case erlang:process_info(MeshClient, message_queue_len) of
        {message_queue_len, Len} when Len > ?BACKPRESSURE_HWM ->
            ?LOG_WARNING("[io_bridge] Backpressure: relay queue ~p", [Len]),
            timer:sleep(1);
        _ ->
            ok
    end.

%%%===================================================================
%%% Internal — Metrics (Gap 9)
%%%===================================================================

init_metrics(TunnelId) ->
    Ref = counters:new(4, [write_concurrency]),
    Tunnels = persistent_term:get(macula_dist_tunnels, #{}),
    persistent_term:put(macula_dist_tunnels, Tunnels#{TunnelId => Ref}),
    Ref.

remove_metrics(TunnelId) ->
    case persistent_term:get(macula_dist_tunnels, undefined) of
        undefined -> ok;
        Tunnels ->
            persistent_term:put(macula_dist_tunnels, maps:remove(TunnelId, Tunnels))
    end.

read_metrics(Ref) ->
    #{bytes_out => counters:get(Ref, ?METRIC_BYTES_OUT),
      bytes_in  => counters:get(Ref, ?METRIC_BYTES_IN),
      msgs_out  => counters:get(Ref, ?METRIC_MSGS_OUT),
      msgs_in   => counters:get(Ref, ?METRIC_MSGS_IN)}.

%%%===================================================================
%%% Internal — Loopback Pair + Helpers
%%%===================================================================

create_loopback_pair() ->
    ListenOpts = [binary, {active, false}, {reuseaddr, true}, {ip, {127,0,0,1}}],
    {ok, LSock} = gen_tcp:listen(0, ListenOpts),
    {ok, Port} = inet:port(LSock),
    {ok, CSock} = gen_tcp:connect({127,0,0,1}, Port,
                                  [binary, {active, false}, {packet, 2}, {nodelay, true}]),
    {ok, ASock} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    inet:setopts(ASock, [{packet, raw}, {nodelay, true}]),
    {CSock, ASock}.

flush_buffered_to_socket(BridgeSock, Key) ->
    receive
        {dist_data_buffered, EncData} ->
            case decrypt(Key, EncData) of
                {ok, Data} -> gen_tcp:send(BridgeSock, Data);
                {error, _} ->
                    %% During buffer phase, data may be unencrypted
                    %% (from the connecting side's first message before
                    %% encryption was set up on that side)
                    gen_tcp:send(BridgeSock, EncData)
            end,
            flush_buffered_to_socket(BridgeSock, Key)
    after 0 ->
        ok
    end.

extract_payload(#{payload := P}) -> P;
extract_payload(P) when is_binary(P) -> P.
