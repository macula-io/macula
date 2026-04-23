%%%-------------------------------------------------------------------
%%% @doc QUIC Distribution Carrier for Erlang.
%%%
%%% This module implements the Erlang distribution carrier interface
%%% using QUIC transport via quicer. It replaces inet_tcp_dist to enable
%%% distributed Erlang over QUIC with:
%%%
%%% - Built-in TLS 1.3 encryption (mandatory)
%%% - NAT-friendly UDP-based transport
%%% - Connection migration support
%%% - Stream multiplexing for message priorities
%%% - Decentralized discovery (no EPMD required)
%%%
%%% When MACULA_DIST_MODE=relay, distribution is tunneled through the
%%% Macula relay mesh via gen_tcp loopback pairs bridged to pub/sub.
%%% See `macula_dist_relay' for relay tunnel details.
%%%
%%% == Usage ==
%%%
%%% Start the VM with:
%%%   erl -proto_dist macula -no_epmd -start_epmd false
%%%
%%% Or in vm.args:
%%%   -proto_dist macula
%%%   -no_epmd
%%%   -start_epmd false
%%%   -macula_dist_port 4433
%%%
%%% @copyright 2025 Macula.io Apache-2.0
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

%% Distribution carrier callbacks (required by OTP)
-export([
    listen/1,
    accept/1,
    accept_connection/5,
    setup/5,
    close/1,
    select/1,
    childspecs/0
]).

%% Additional exports for integration
-export([
    is_node_name/1,
    splitname/1,
    address/0
]).

%% Internal exports (used by spawn_link)
-export([
    acceptor_loop/2,
    do_accept/3,
    do_setup/6
]).

-ifdef(TEST).
%% Exports for unit tests — pure helpers that are otherwise private.
-export([
    frame_outgoing/2,
    packet_mode/1,
    set_packet_mode/2,
    extract_frame/4,
    parse_frames/2
]).
-endif.

-define(FAMILY, inet).
-define(DRIVER, macula_dist).
-define(DEFAULT_PORT, 4433).
-define(ALPN, "macula-dist").
-define(HANDSHAKE_TIMEOUT, 30000).
-define(CONNECT_TIMEOUT, 15000).

%%%===================================================================
%%% Distribution Carrier Callbacks
%%%===================================================================

%% @doc Return child specifications for the distribution supervisor.
-spec childspecs() -> [supervisor:child_spec()].
childspecs() ->
    [].

%% @doc Listen for incoming distribution connections.
%%
%% Dispatches on the current mode (set via MACULA_DIST_MODE env var):
%% - `relay' (legacy): pub/sub bridge through the station — no local listener
%% - `dist_relay': tunnels via the dedicated dist relay — no local listener
%% - `direct' (default): raw QUIC listener on the dist port
-spec listen(atom()) -> {ok, {term(), #net_address{}, 1..3}} | {error, term()}.
listen(NodeName) ->
    do_listen(dist_mode(), NodeName).

do_listen(relay, NodeName) ->
    ?LOG_INFO("[dist] Relay mode (pub/sub bridge) — no QUIC listener"),
    {ok, {relay_mode, make_address(NodeName, 0), 1}};
do_listen(dist_relay, NodeName) ->
    ?LOG_INFO("[dist] dist_relay mode — tunnels via macula-dist-relay"),
    register_node_with_dist_relay(NodeName),
    {ok, {dist_relay_mode, make_address(NodeName, 0), 1}};
do_listen(direct, NodeName) ->
    Port = get_dist_port(NodeName),
    handle_quic_listen(start_quic_listener(Port), NodeName, Port).

handle_quic_listen({ok, Listener}, NodeName, Port) ->
    {ok, {Listener, make_address(NodeName, Port), 1}};
handle_quic_listen({error, Reason}, _NodeName, _Port) ->
    ?LOG_ERROR("[dist] Listen failed: ~p", [Reason]),
    {error, Reason}.

register_node_with_dist_relay(NodeName) ->
    %% The dist_relay_client started on node boot should be up by now.
    %% If it's not, log + continue — outgoing setup will fail cleanly
    %% with client_not_running which is better than blocking listen/1.
    maybe_announce_node(macula_dist_relay_client:whereis_client(), NodeName).

maybe_announce_node(undefined, NodeName) ->
    ?LOG_WARNING("[dist] dist_relay_client not running — dist over relay will "
                 "fail until macula:join_mesh/1 is called with dist_relay opt "
                 "(node=~p)", [NodeName]);
maybe_announce_node(_Pid, _NodeName) ->
    %% Client was started with the NodeName already (via start_link) and
    %% sent identify during init. Nothing more to do here.
    ok.

%% @doc Accept incoming connections. Called in a loop by net_kernel.
%%
%% In direct-QUIC mode: acceptor_loop runs accept on the listener handle.
%% In relay (pub/sub) / dist_relay modes: acceptor_loop blocks — inbound
%% connections arrive via the respective client which delivers the
%% `{accept, ...}' messages directly to net_kernel.
-spec accept(term()) -> pid().
accept(dist_relay_mode = Sentinel) ->
    register_kernel_with_dist_relay(macula_dist_relay_client:whereis_client()),
    spawn_link(?MODULE, acceptor_loop, [self(), Sentinel]);
accept(ListenerHandle) ->
    spawn_link(?MODULE, acceptor_loop, [self(), ListenerHandle]).

register_kernel_with_dist_relay(undefined) ->
    ?LOG_ERROR("[dist] Cannot accept in dist_relay mode — client not running");
register_kernel_with_dist_relay(Client) ->
    ok = macula_dist_relay_client:set_kernel(Client, self()).

%% @doc Accept a distribution connection from a remote node.
%% Socket is either {QuicConn, Stream} (direct QUIC) or a gen_tcp port (relay).
-spec accept_connection(pid(), term(), node(), term(), term()) -> pid().
accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) when is_port(Socket) ->
    spawn_link(?MODULE, do_accept, [
        {AcceptPid, Socket, Socket, MyNode, Allowed, SetupTime},
        self(),
        unused
    ]);
accept_connection(AcceptPid, {QuicConn, Stream}, MyNode, Allowed, SetupTime) ->
    spawn_link(?MODULE, do_accept, [
        {AcceptPid, QuicConn, Stream, MyNode, Allowed, SetupTime},
        self(),
        unused
    ]).

%% @doc Setup an outgoing distribution connection.
%% self() here is net_kernel — captured as Kernel for the spawned process.
-spec setup(node(), term(), atom(), term(), term()) -> pid().
setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    Kernel = self(),
    spawn_link(?MODULE, do_setup, [
        Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime
    ]).

%% @doc Close a distribution connection.
-spec close(term()) -> ok.
close(Socket) when is_port(Socket) ->
    catch gen_tcp:close(Socket),
    ok;
close({S, S}) when is_port(S) ->
    catch gen_tcp:close(S),
    ok;
close({QuicConn, _Stream}) ->
    catch macula_quic:close_connection(QuicConn),
    ok;
close(QuicConn) when is_reference(QuicConn) ->
    catch macula_quic:close_connection(QuicConn),
    ok;
close(_) ->
    ok.

%% @doc Check if this module should handle distribution to the given node.
-spec select(atom()) -> boolean().
select(Node) ->
    case splitname(Node) of
        {_Port, _Host} -> true;
        false -> false
    end.

%%%===================================================================
%%% Name Handling
%%%===================================================================

%% @doc Check if a string is a valid node name.
-spec is_node_name(string()) -> boolean().
is_node_name(Name) ->
    case splitname(Name) of
        {_Port, _Host} -> true;
        _ -> false
    end.

%% @doc Split a node name into port and host.
%% Supports two formats:
%%   - port@host (e.g., 4433@192.168.1.100) — explicit port
%%   - name@host (e.g., test@nanode1.example.com) — uses default port
-spec splitname(atom() | string()) -> {integer(), string()} | false.
splitname(NodeName) when is_atom(NodeName) ->
    splitname(atom_to_list(NodeName));
splitname(NodeName) when is_list(NodeName) ->
    case string:tokens(NodeName, "@") of
        [NameOrPort, Host] ->
            case catch list_to_integer(NameOrPort) of
                Port when is_integer(Port), Port > 0, Port < 65536 ->
                    {Port, Host};
                _ ->
                    DefaultPort = application:get_env(kernel, macula_dist_port, ?DEFAULT_PORT),
                    {DefaultPort, Host}
            end;
        _ ->
            false
    end.

%% @doc Return address information for this distribution.
-spec address() -> #net_address{}.
address() ->
    {ok, Host} = inet:gethostname(),
    Port = application:get_env(kernel, macula_dist_port, ?DEFAULT_PORT),
    make_address(Port, Host).

%%%===================================================================
%%% Internal — Mode Detection
%%%===================================================================

%% @private Distribution transport mode.
%% - `relay' (legacy): pub/sub bridge via station (macula-relay). MACULA_DIST_MODE=relay
%% - `dist_relay': dedicated dist relay (macula-dist-relay). MACULA_DIST_MODE=dist_relay
%% - `direct' (default): raw QUIC on the dist port
-spec dist_mode() -> relay | dist_relay | direct.
dist_mode() ->
    mode_from_env(os:getenv("MACULA_DIST_MODE")).

mode_from_env("relay") -> relay;
mode_from_env("dist_relay") -> dist_relay;
mode_from_env(_) -> direct.

%%%===================================================================
%%% Internal — QUIC Listener
%%%===================================================================

start_quic_listener(Port) ->
    {CertFile, KeyFile} = get_tls_certs(),
    ListenerOpts = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        {alpn, [?ALPN]},
        {idle_timeout_ms, 60000},
        {peer_bidi_stream_count, 100},
        {peer_unidi_stream_count, 100}
    ],
    case macula_quic:listen(Port, ListenerOpts) of
        {ok, Listener} ->
            ?LOG_INFO("[dist] Listening on UDP port ~p", [Port]),
            {ok, Listener};
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal — Acceptor Loop (flattened from 5-level nesting)
%%%===================================================================

%% @private Acceptor loop. In all currently-supported modes inbound
%% connections arrive via the relay or dist-relay client (message
%% delivery), not via direct QUIC accept. Direct-listener mode was
%% never implemented end-to-end (the Quinn NIF binding for blocking
%% accept doesn't exist) and the supervisor today only spawns the
%% relay or dist_relay variants, so the loop just blocks on `stop`.
acceptor_loop(_Kernel, relay_mode) ->
    receive stop -> ok end;
acceptor_loop(_Kernel, dist_relay_mode) ->
    receive stop -> ok end;
acceptor_loop(_Kernel, _Listener) ->
    ?LOG_WARNING("[dist] Direct-listener acceptor mode is not implemented; "
                 "use relay or dist_relay modes."),
    receive stop -> ok end.

%% Direct-listener acceptance helpers were removed alongside
%% macula_quic:accept/2 (which had only ever been a stub). Inbound
%% dist connections now arrive exclusively via relay or dist_relay
%% modes, where the relay client message-delivers them rather than
%% invoking acceptor_loop.

%%%===================================================================
%%% Internal — Accept Incoming Connection
%%%===================================================================

%% @private Handle incoming connection setup.
%% _Unused kept for arity compatibility with spawn_link call sites.
do_accept({AcceptPid, QuicConn, Stream, MyNode, Allowed, SetupTime}, Kernel, _Unused) ->
    Timer = dist_util:start_timer(SetupTime),

    receive
        {AcceptPid, controller, ok} -> ok
    after ?HANDSHAKE_TIMEOUT ->
        dist_util:shutdown(?MODULE, ?LINE, control_transfer_timeout)
    end,

    Socket = {QuicConn, Stream},
    HSData = make_hs_data(Kernel, MyNode, Socket, Timer, Allowed),
    dist_util:handshake_other_started(HSData).

%%%===================================================================
%%% Internal — Setup Outgoing Connection
%%%===================================================================

%% @private Setup outgoing distribution connection.
do_setup(Kernel, Node, Type, MyNode, _LongOrShortNames, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),
    case splitname(Node) of
        {Port, Host} ->
            do_setup_connect(Kernel, Node, Type, MyNode, Timer, Host, Port);
        false ->
            ?LOG_WARNING("[dist] Invalid node name: ~p", [Node]),
            dist_util:shutdown(?MODULE, ?LINE, invalid_node_name)
    end.

do_setup_connect(Kernel, Node, Type, MyNode, Timer, Host, Port) ->
    Mode = dist_mode(),
    ?LOG_INFO("[dist] setup ~p mode=~p host=~s port=~p", [Node, Mode, Host, Port]),
    handle_connect_result(connect_by_mode(Mode, Node, Host, Port),
                          Kernel, Node, Type, MyNode, Timer).

connect_by_mode(relay, Node, Host, Port) ->
    macula_dist_relay:connect(atom_to_list(Node), Host, Port);
connect_by_mode(dist_relay, Node, _Host, _Port) ->
    connect_via_dist_relay(Node);
connect_by_mode(direct, _Node, Host, Port) ->
    connect_quic(Host, Port).

connect_via_dist_relay(Node) ->
    handle_client_lookup(macula_dist_relay_client:whereis_client(), Node).

handle_client_lookup(undefined, _Node) ->
    {error, dist_relay_client_not_running};
handle_client_lookup(Client, Node) ->
    NodeBin = atom_to_binary(Node),
    macula_dist_relay_client:request_tunnel(Client, NodeBin).

handle_connect_result({ok, Conn, Stream}, Kernel, Node, Type, MyNode, Timer) ->
    ?LOG_INFO("[dist] Connected to ~p, starting handshake", [Node]),
    HSData = make_hs_data(Kernel, MyNode, {Conn, Stream}, Timer, undefined),
    dist_util:handshake_we_started(
        HSData#hs_data{other_node = Node, request_type = Type});
handle_connect_result({error, Reason}, _Kernel, Node, _Type, _MyNode, _Timer) ->
    ?LOG_WARNING("[dist] Connection to ~p failed: ~p", [Node, Reason]),
    dist_util:shutdown(?MODULE, ?LINE, {connect_failed, Reason}).

connect_quic(Host, Port) ->
    {CertFile, KeyFile} = get_tls_certs(),
    TlsOpts = macula_tls:quic_client_opts(),
    BaseOpts = [
        {alpn, [?ALPN]},
        {certfile, CertFile},
        {keyfile, KeyFile},
        {idle_timeout_ms, 60000}
    ],
    ConnOpts = merge_dist_opts(BaseOpts, TlsOpts),
    case macula_quic:connect(Host, Port, ConnOpts, ?CONNECT_TIMEOUT) of
        {ok, Conn} ->
            case macula_quic:open_stream(Conn, #{active => false}) of
                {ok, Stream} ->
                    {ok, Conn, Stream};
                {error, Reason} ->
                    macula_quic:close_connection(Conn),
                    {error, {stream_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal — Shared hs_data construction
%%%===================================================================

make_hs_data(Kernel, MyNode, Socket, Timer, Allowed) ->
    #hs_data{
        kernel_pid = Kernel,
        this_node = MyNode,
        socket = Socket,
        timer = Timer,
        this_flags = 0,
        other_flags = 0,
        allowed = Allowed,
        f_send = fun quic_send/2,
        f_recv = fun quic_recv/3,
        f_setopts_pre_nodeup = fun quic_setopts_pre_nodeup/1,
        f_setopts_post_nodeup = fun quic_setopts_post_nodeup/1,
        f_getll = fun quic_getll/1,
        f_address = fun quic_address/2,
        f_handshake_complete = fun quic_handshake_complete/3,
        mf_tick = fun quic_tick/1,
        mf_getstat = fun quic_getstat/1,
        mf_setopts = fun quic_setopts/2,
        mf_getopts = fun quic_getopts/2
    }.

%%%===================================================================
%%% Socket Operations — dispatch on gen_tcp port vs QUIC reference
%%%===================================================================

%% --- send ---
%%
%% QUIC streams are raw bytestreams — no packet framing is done by the
%% transport. dist_util expects `{packet, 2}' during handshake and
%% `{packet, 4}' post-nodeup (from gen_tcp's perspective). Without framing,
%% recv returns arbitrary-sized chunks that don't align with dist frames.
%%
%% We emulate the packet option per-stream in the dist controller's process
%% dictionary. Default is 2 (handshake mode); `quic_setopts_pre_nodeup'
%% upgrades to 4 before post-handshake traffic begins.

quic_send(Socket, Data) when is_port(Socket) ->
    gen_tcp:send(Socket, Data);
quic_send({S, S}, Data) when is_port(S) ->
    gen_tcp:send(S, Data);
quic_send({_Conn, Stream}, Data) ->
    Framed = frame_outgoing(packet_mode(Stream), Data),
    case macula_quic:send(Stream, Framed) of
        ok -> ok;
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

frame_outgoing(2, Data) ->
    Bin = iolist_to_binary(Data),
    <<(byte_size(Bin)):16, Bin/binary>>;
frame_outgoing(4, Data) ->
    Bin = iolist_to_binary(Data),
    <<(byte_size(Bin)):32, Bin/binary>>.

%% --- recv (dist_util expects {ok, List}) ---

quic_recv(Socket, Length, Timeout) when is_port(Socket) ->
    recv_tcp(Socket, Length, Timeout);
quic_recv({S, S}, Length, Timeout) when is_port(S) ->
    recv_tcp(S, Length, Timeout);
quic_recv({_Conn, Stream}, _Length, Timeout) ->
    recv_frame(Stream, packet_mode(Stream), recv_timeout(Timeout)).

%% Pull one complete length-prefixed frame. Accumulates bytes across
%% multiple QUIC data events via a per-stream buffer in the proc dict.
recv_frame(Stream, Mode, TimeoutMs) ->
    Buf = erlang:get({macula_dist_recv_buf, Stream}),
    extract_frame(Stream, Mode, init_buf(Buf), TimeoutMs).

init_buf(undefined) -> <<>>;
init_buf(B) when is_binary(B) -> B.

extract_frame(Stream, 2, <<L:16, Frame:L/binary, Rest/binary>>, _Timeout) ->
    stash_buf(Stream, Rest),
    {ok, binary_to_list(Frame)};
extract_frame(Stream, 4, <<L:32, Frame:L/binary, Rest/binary>>, _Timeout) ->
    stash_buf(Stream, Rest),
    {ok, binary_to_list(Frame)};
extract_frame(Stream, Mode, Buf, TimeoutMs) ->
    wait_more_bytes(Stream, Mode, Buf, TimeoutMs).

wait_more_bytes(Stream, Mode, Buf, TimeoutMs) ->
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            extract_frame(Stream, Mode, <<Buf/binary, Data/binary>>, TimeoutMs);
        {quic, stream_closed, Stream, _Flags} ->
            stash_buf(Stream, Buf),
            {error, closed};
        {quic, peer_send_shutdown, Stream, _Flags} ->
            stash_buf(Stream, Buf),
            {error, closed}
    after TimeoutMs ->
        stash_buf(Stream, Buf),
        {error, timeout}
    end.

stash_buf(Stream, <<>>) ->
    erlang:erase({macula_dist_recv_buf, Stream});
stash_buf(Stream, Buf) ->
    erlang:put({macula_dist_recv_buf, Stream}, Buf).

packet_mode(Stream) ->
    packet_mode_of(erlang:get({macula_dist_packet_mode, Stream})).

packet_mode_of(undefined) -> 2;
packet_mode_of(N) when N =:= 2; N =:= 4 -> N.

set_packet_mode(Stream, N) when N =:= 2; N =:= 4 ->
    erlang:put({macula_dist_packet_mode, Stream}, N),
    ok.

recv_tcp(Socket, Length, Timeout) ->
    case gen_tcp:recv(Socket, Length, recv_timeout(Timeout)) of
        {ok, Bin} when is_binary(Bin) -> {ok, binary_to_list(Bin)};
        Other -> Other
    end.

recv_timeout(infinity) -> 30000;
recv_timeout(T) -> T.

%% --- setopts (match inet_tcp_dist: {packet,4} post-handshake) ---

quic_setopts_pre_nodeup(Socket) when is_port(Socket) ->
    inet:setopts(Socket, [{active, false}, {packet, 4}]);
quic_setopts_pre_nodeup({S, S}) when is_port(S) ->
    inet:setopts(S, [{active, false}, {packet, 4}]);
quic_setopts_pre_nodeup({_Conn, Stream}) ->
    set_packet_mode(Stream, 4).

quic_setopts_post_nodeup(Socket) when is_port(Socket) ->
    inet:setopts(Socket, [{active, true}, {packet, 4}, {deliver, port}, binary]);
quic_setopts_post_nodeup({S, S}) when is_port(S) ->
    inet:setopts(S, [{active, true}, {packet, 4}, {deliver, port}, binary]);
quic_setopts_post_nodeup({_Conn, _Stream}) ->
    ok.

%% --- getll ---

quic_getll(Socket) when is_port(Socket) ->
    {ok, Socket};
quic_getll({S, S}) when is_port(S) ->
    {ok, S};
quic_getll({_Conn, _Stream}) ->
    %% dist_util passes the returned value to erlang:setnode as the DistCtrl.
    %% Must be a port or a pid that implements the dist controller
    %% protocol (dist_ctrl_get_data / dist_ctrl_put_data). We return self()
    %% — the dist controller process (do_setup or do_accept) whose
    %% post-handshake loop shepherds bytes between the QUIC stream and the
    %% distribution runtime via the dist_ctrl_* BIFs.
    %%
    %% Note: dist_util's default con_loop does NOT implement this
    %% shuffling — a custom loop is wired in via f_handshake_complete.
    {ok, self()}.

%% --- address ---

quic_address(Socket, Node) when is_port(Socket) ->
    make_net_address(inet:peername(Socket), Node);
quic_address({S, S}, Node) when is_port(S) ->
    make_net_address(inet:peername(S), Node);
quic_address({Conn, _Stream}, Node) ->
    make_net_address(macula_quic:peername(Conn), Node).

make_net_address({ok, {IP, Port}}, Node) ->
    #net_address{address = {IP, Port}, host = atom_to_list(Node),
                 protocol = ?DRIVER, family = ?FAMILY};
make_net_address(_, Node) ->
    #net_address{address = undefined, host = atom_to_list(Node),
                 protocol = ?DRIVER, family = ?FAMILY}.

%% --- handshake_complete ---
%%
%% dist_util calls this AFTER f_setopts_post_nodeup and BEFORE con_loop.
%% For QUIC carrier we hijack the controlling process here and never
%% return — dist_util's con_loop doesn't understand `dist_data'
%% notifications or raw stream reads, so we run our own loop that shuffles
%% bytes between the QUIC stream and the distribution runtime via the
%% `erlang:dist_ctrl_*' BIFs.
%%
%% For gen_tcp sockets (when Socket is a port) we fall through to the
%% default no-op; the native port handles dist framing itself.

quic_handshake_complete(Socket, _Node, _DHandle) when is_port(Socket) ->
    ok;
quic_handshake_complete({S, S}, _Node, _DHandle) when is_port(S) ->
    ok;
quic_handshake_complete({_Conn, Stream}, Node, DHandle) ->
    ?LOG_INFO("[dist] handshake complete for ~p — entering ctrl loop", [Node]),
    %% Ask the runtime to return {Size, Data} tuples from dist_ctrl_get_data.
    %% Returns the PREVIOUS value, so we ignore it rather than asserting.
    _ = erlang:dist_ctrl_set_opt(DHandle, get_size, true),
    ok = erlang:dist_ctrl_get_data_notification(DHandle),
    %% Handshake-phase recv buffer may still hold bytes that arrived AFTER
    %% the last handshake frame (peer's first post-nodeup packet-4 frame,
    %% typically). If we drop them here, BEAM's dist parser chokes on the
    %% following frame because the stream is truncated mid-header. Seed
    %% the ctrl_loop's buffer with whatever recv_frame stashed.
    Seed = take_stashed_bytes(Stream),
    Seeded = put_incoming(DHandle, Seed),
    %% Don't return — dist_util's con_loop cannot drive the
    %% `dist_data' / `dist_ctrl_get_data' protocol. Our loop replaces it.
    ctrl_loop(Node, Stream, DHandle, Seeded).

take_stashed_bytes(Stream) ->
    case erlang:erase({macula_dist_recv_buf, Stream}) of
        undefined -> <<>>;
        Bin when is_binary(Bin) -> Bin
    end.

%% @private Dist controller loop — flat clauses, one message class each.
%%
%% `InBuf' is the accumulator for inbound bytes from the QUIC stream.
%% A packet-4 frame may arrive split across several `{quic, ...}' events
%% so we buffer until we have a complete frame (or several).
ctrl_loop(Node, Stream, DHandle, InBuf) ->
    receive Msg -> handle_msg(Msg, Node, Stream, DHandle, InBuf) end.

handle_msg(dist_data, Node, Stream, DHandle, InBuf) ->
    drain_out(DHandle, Stream),
    ok = erlang:dist_ctrl_get_data_notification(DHandle),
    ctrl_loop(Node, Stream, DHandle, InBuf);
handle_msg({quic, Data, Stream, _Flags}, Node, Stream, DHandle, InBuf)
  when is_binary(Data) ->
    NewBuf = put_incoming(DHandle, <<InBuf/binary, Data/binary>>),
    ctrl_loop(Node, Stream, DHandle, NewBuf);
handle_msg({quic, peer_send_shutdown, Stream, _}, Node, Stream, _DH, _B) ->
    exit({shutdown, {Node, peer_send_shutdown}});
handle_msg({quic, stream_closed, Stream, _}, Node, Stream, _DH, _B) ->
    exit({shutdown, {Node, stream_closed}});
handle_msg({quic, transport_shutdown, _, _}, Node, _Stream, _DH, _B) ->
    exit({shutdown, {Node, transport_shutdown}});
handle_msg({quic, closed, _, _}, Node, _Stream, _DH, _B) ->
    exit({shutdown, {Node, closed}});
handle_msg({_Kernel, disconnect}, Node, _Stream, _DH, _B) ->
    exit({shutdown, {Node, disconnected}});
handle_msg({_Kernel, tick}, Node, Stream, DHandle, InBuf) ->
    send_tick(Stream),
    ctrl_loop(Node, Stream, DHandle, InBuf);
handle_msg({_Kernel, aux_tick}, Node, Stream, DHandle, InBuf) ->
    send_tick(Stream),
    ctrl_loop(Node, Stream, DHandle, InBuf);
handle_msg({From, Ref, {setopts, _Opts}}, Node, Stream, DHandle, InBuf) ->
    From ! {Ref, ok},
    ctrl_loop(Node, Stream, DHandle, InBuf);
handle_msg({From, Ref, {getopts, _Opts}}, Node, Stream, DHandle, InBuf) ->
    From ! {Ref, {ok, []}},
    ctrl_loop(Node, Stream, DHandle, InBuf);
handle_msg({From, get_status}, Node, Stream, DHandle, InBuf) ->
    {ok, R, W, _} = getstat_dist(DHandle),
    From ! {self(), get_status, {ok, R, W}},
    ctrl_loop(Node, Stream, DHandle, InBuf);
handle_msg(_Other, Node, Stream, DHandle, InBuf) ->
    ctrl_loop(Node, Stream, DHandle, InBuf).

%% Zero-length packet-4 frame — the dist-layer keepalive.
send_tick(Stream) ->
    _ = macula_quic:send(Stream, <<0:32>>),
    ok.

getstat_dist(DHandle) ->
    %% erlang:dist_get_stat returns {ok, Read, Write, PendingWrites}.
    erlang:dist_get_stat(DHandle).

%% --- outbound: drain runtime → QUIC stream ---

drain_out(DHandle, Stream) ->
    send_out(erlang:dist_ctrl_get_data(DHandle), DHandle, Stream).

send_out(none, _DHandle, _Stream) ->
    ok;
send_out({Size, Data}, DHandle, Stream) ->
    Frame = iolist_to_binary([<<Size:32>> | Data]),
    write_frame(macula_quic:send(Stream, Frame)),
    drain_out(DHandle, Stream).

write_frame(ok) -> ok;
write_frame({ok, _}) -> ok;
write_frame({error, Reason}) ->
    ?LOG_ERROR("[dist] ctrl_loop send failed: ~p", [Reason]),
    exit({dist_send_failed, Reason}).

%% --- inbound: QUIC stream → runtime. Return leftover tail. ---

put_incoming(DHandle, Buf) ->
    {Frames, Leftover} = parse_frames(Buf, []),
    lists:foreach(fun(F) -> deliver_frame(DHandle, F) end, Frames),
    Leftover.

%% Zero-length frames are dist-layer ticks — they must NOT be forwarded to
%% dist_ctrl_put_data, which rejects them as corrupt. (ssl_dist applies the
%% same `when 0 < Size' guard in read_application_dist_data.)
deliver_frame(_DHandle, <<>>) -> ok;
deliver_frame(DHandle, Frame) -> erlang:dist_ctrl_put_data(DHandle, Frame).

%% Parse as many complete `<<Size:32, Payload:Size/binary>>' frames as the
%% buffer holds. Returns `{FramesInOrder, Leftover}'.
parse_frames(<<Size:32, Payload:Size/binary, Rest/binary>>, Acc) ->
    parse_frames(Rest, [Payload | Acc]);
parse_frames(Partial, Acc) ->
    {lists:reverse(Acc), Partial}.

%% --- tick ---

quic_tick(Socket) when is_port(Socket) ->
    gen_tcp:send(Socket, <<>>);
quic_tick({S, S}) when is_port(S) ->
    gen_tcp:send(S, <<>>);
quic_tick({_Conn, Stream}) ->
    %% Dist tick = zero-payload packet. Frame with current packet mode
    %% (post-nodeup so always 4 here, but read the mode to stay honest).
    Frame = frame_outgoing(packet_mode(Stream), <<>>),
    macula_quic:send(Stream, Frame).

%% --- getstat (dist_util expects {ok, R, W, P} 4-tuple) ---

quic_getstat(Socket) when is_port(Socket) ->
    getstat_tcp(Socket);
quic_getstat({S, S}) when is_port(S) ->
    getstat_tcp(S);
quic_getstat({Conn, _Stream}) ->
    case macula_quic:getstat(Conn, [send_cnt, recv_cnt, send_pend]) of
        {ok, Stats} -> split_stat(Stats, 0, 0, 0);
        {error, _} -> {ok, 0, 0, 0}
    end.

getstat_tcp(Socket) ->
    case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
        {ok, Stats} -> split_stat(Stats, 0, 0, 0);
        Error -> Error
    end.

split_stat([{recv_cnt, R} | Rest], _, W, P) -> split_stat(Rest, R, W, P);
split_stat([{send_cnt, W} | Rest], R, _, P) -> split_stat(Rest, R, W, P);
split_stat([{send_pend, P} | Rest], R, W, _) -> split_stat(Rest, R, W, P);
split_stat([], R, W, P) -> {ok, R, W, P}.

%% --- setopts / getopts ---

quic_setopts(Socket, Opts) when is_port(Socket) ->
    inet:setopts(Socket, Opts);
quic_setopts({S, S}, Opts) when is_port(S) ->
    inet:setopts(S, Opts);
quic_setopts({_Conn, Stream}, Opts) ->
    lists:foreach(
        fun({active, Mode}) -> macula_quic:setopt(Stream, active, Mode);
           (_) -> ok
        end, Opts),
    ok.

quic_getopts(Socket, Opts) when is_port(Socket) ->
    inet:getopts(Socket, Opts);
quic_getopts({S, S}, Opts) when is_port(S) ->
    inet:getopts(S, Opts);
quic_getopts({_Conn, _Stream}, _Opts) ->
    {ok, [{active, true}]}.

%%%===================================================================
%%% Utility Functions
%%%===================================================================

merge_dist_opts(BaseOpts, TlsOpts) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            lists:keystore(Key, 1, Acc, {Key, Value})
        end, BaseOpts, TlsOpts).

get_dist_port(NodeName) when is_atom(NodeName) ->
    get_dist_port(atom_to_list(NodeName));
get_dist_port(NodeName) when is_list(NodeName) ->
    case splitname(NodeName) of
        {Port, _Host} ->
            Port;
        false ->
            case catch list_to_integer(NodeName) of
                Port when is_integer(Port), Port > 0, Port < 65536 -> Port;
                _ -> application:get_env(kernel, macula_dist_port, ?DEFAULT_PORT)
            end
    end.

get_tls_certs() ->
    CertDir = application:get_env(kernel, macula_dist_cert_dir, "/tmp/macula_dist"),
    CertFile = filename:join(CertDir, "cert.pem"),
    KeyFile = filename:join(CertDir, "key.pem"),
    case filelib:is_regular(CertFile) andalso filelib:is_regular(KeyFile) of
        true  -> {CertFile, KeyFile};
        false -> generate_self_signed_cert(CertDir, CertFile, KeyFile)
    end.

generate_self_signed_cert(_CertDir, CertFile, KeyFile) ->
    ok = filelib:ensure_dir(CertFile),
    Cmd = io_lib:format(
        "openssl req -x509 -newkey rsa:2048 -keyout ~s -out ~s "
        "-days 365 -nodes -subj '/CN=macula-dist' 2>/dev/null",
        [KeyFile, CertFile]),
    case os:cmd(lists:flatten(Cmd)) of
        "" -> ok;
        Error -> ?LOG_ERROR("[dist] Cert generation failed: ~s", [Error])
    end,
    {CertFile, KeyFile}.

make_address(NodeName, Port) when is_atom(NodeName) ->
    {ok, Host} = inet:gethostname(),
    make_address(Port, Host);
make_address(Port, Host) when is_integer(Port) ->
    #net_address{address = {Host, Port}, host = Host,
                 protocol = ?DRIVER, family = ?FAMILY}.
