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

-define(FAMILY, inet).
-define(DRIVER, macula_dist).
-define(DEFAULT_PORT, 4433).
-define(ALPN, "macula-dist").
-define(HANDSHAKE_TIMEOUT, 30000).
-define(CONNECT_TIMEOUT, 15000).
-define(HANDOFF_TIMEOUT, 5000).

%%%===================================================================
%%% Distribution Carrier Callbacks
%%%===================================================================

%% @doc Return child specifications for the distribution supervisor.
-spec childspecs() -> [supervisor:child_spec()].
childspecs() ->
    [].

%% @doc Listen for incoming distribution connections.
%% In relay mode, skip the QUIC listener — all connections go through the relay.
-spec listen(atom()) -> {ok, {term(), #net_address{}, 1..3}} | {error, term()}.
listen(NodeName) ->
    case macula_dist_relay:is_relay_mode() of
        true ->
            %% Relay mode: no QUIC listener needed. Connections arrive via relay tunnel.
            Port = 0,
            Address = make_address(NodeName, Port),
            ?LOG_INFO("[dist] Relay mode — no QUIC listener (tunnels via relay)"),
            {ok, {relay_mode, Address, 1}};
        false ->
            Port = get_dist_port(NodeName),
            case start_quic_listener(Port) of
                {ok, ListenerHandle} ->
                    Address = make_address(NodeName, Port),
                    {ok, {ListenerHandle, Address, 1}};
                {error, Reason} ->
                    ?LOG_ERROR("[dist] Listen failed: ~p", [Reason]),
                    {error, Reason}
            end
    end.

%% @doc Accept incoming connections. Called in a loop by net_kernel.
-spec accept(term()) -> pid().
accept(ListenerHandle) ->
    spawn_link(?MODULE, acceptor_loop, [self(), ListenerHandle]).

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

%% @private Acceptor loop — recursively accepts QUIC connections.
acceptor_loop(Kernel, Listener) ->
    case macula_quic:accept(Listener, #{active => false}) of
        {ok, Conn} ->
            handle_accepted_connection(Kernel, Conn);
        {error, closed} ->
            exit(normal);
        {error, Reason} ->
            ?LOG_WARNING("[dist] Accept failed: ~p", [Reason])
    end,
    acceptor_loop(Kernel, Listener).

handle_accepted_connection(Kernel, Conn) ->
    case macula_quic:handshake(Conn) of
        {ok, Conn} ->
            handle_quic_handshake(Kernel, Conn);
        {error, Reason} ->
            ?LOG_WARNING("[dist] QUIC handshake failed: ~p", [Reason]),
            macula_quic:close_connection(Conn)
    end.

handle_quic_handshake(Kernel, Conn) ->
    case accept_dist_stream(Conn) of
        {ok, Stream} ->
            notify_kernel_and_transfer(Kernel, Conn, Stream);
        {error, Reason} ->
            ?LOG_WARNING("[dist] Stream accept failed: ~p", [Reason]),
            macula_quic:close_connection(Conn)
    end.

notify_kernel_and_transfer(Kernel, Conn, Stream) ->
    Kernel ! {accept, self(), {Conn, Stream}, ?FAMILY, ?DRIVER},
    receive
        {Kernel, controller, DistCtrl} ->
            transfer_stream_ownership(Conn, Stream, DistCtrl);
        {Kernel, unsupported_protocol} ->
            close({Conn, Stream})
    after ?HANDSHAKE_TIMEOUT ->
        ?LOG_WARNING("[dist] Controller assignment timeout"),
        close({Conn, Stream})
    end.

transfer_stream_ownership(Conn, Stream, DistCtrl) ->
    case macula_quic:handoff_stream(Stream, DistCtrl, #{}) of
        ok ->
            case macula_quic:controlling_process(Conn, DistCtrl) of
                ok ->
                    DistCtrl ! {self(), controller, ok};
                {error, Reason} ->
                    ?LOG_WARNING("[dist] Connection transfer failed: ~p", [Reason]),
                    close({Conn, Stream})
            end;
        {error, Reason} ->
            ?LOG_WARNING("[dist] Stream handoff failed: ~p", [Reason]),
            close({Conn, Stream})
    end.

accept_dist_stream(Conn) ->
    macula_quic:accept_stream(Conn, #{active => true}, ?HANDSHAKE_TIMEOUT).

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

    wait_for_handoff(Stream),

    Socket = {QuicConn, Stream},
    HSData = make_hs_data(Kernel, MyNode, Socket, Timer, Allowed),
    dist_util:handshake_other_started(HSData).

%% QUIC needs handoff_done; gen_tcp does not.
wait_for_handoff(Stream) when is_port(Stream) ->
    ok;
wait_for_handoff(Stream) ->
    receive
        {handoff_done, Stream, _HandoffData} -> ok
    after ?HANDOFF_TIMEOUT ->
        ok
    end.

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
    ConnectFn = case macula_dist_relay:is_relay_mode() of
        true  -> fun() -> macula_dist_relay:connect(atom_to_list(Node), Host, Port) end;
        false -> fun() -> connect_quic(Host, Port) end
    end,
    case ConnectFn() of
        {ok, Conn, Stream} ->
            HSData = make_hs_data(Kernel, MyNode, {Conn, Stream}, Timer, undefined),
            dist_util:handshake_we_started(
                HSData#hs_data{other_node = Node, request_type = Type});
        {error, Reason} ->
            ?LOG_WARNING("[dist] Connection to ~p failed: ~p", [Node, Reason]),
            dist_util:shutdown(?MODULE, ?LINE, {connect_failed, Reason})
    end.

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

quic_send(Socket, Data) when is_port(Socket) ->
    gen_tcp:send(Socket, Data);
quic_send({S, S}, Data) when is_port(S) ->
    gen_tcp:send(S, Data);
quic_send({_Conn, Stream}, Data) ->
    case macula_quic:send(Stream, Data) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

%% --- recv (dist_util expects {ok, List}) ---

quic_recv(Socket, Length, Timeout) when is_port(Socket) ->
    recv_tcp(Socket, Length, Timeout);
quic_recv({S, S}, Length, Timeout) when is_port(S) ->
    recv_tcp(S, Length, Timeout);
quic_recv({_Conn, Stream}, _Length, Timeout) ->
    TimeoutMs = recv_timeout(Timeout),
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            {ok, binary_to_list(Data)};
        {quic, stream_closed, Stream, _Flags} ->
            {error, closed};
        {quic, peer_send_shutdown, Stream, _Flags} ->
            {error, closed}
    after TimeoutMs ->
        {error, timeout}
    end.

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
quic_setopts_pre_nodeup({_Conn, _Stream}) ->
    ok.

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
quic_getll({Conn, _Stream}) ->
    {ok, Conn}.

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

quic_handshake_complete(_Socket, _Node, _DHandle) ->
    ok.

%% --- tick ---

quic_tick(Socket) when is_port(Socket) ->
    gen_tcp:send(Socket, <<>>);
quic_tick({S, S}) when is_port(S) ->
    gen_tcp:send(S, <<>>);
quic_tick({_Conn, Stream}) ->
    macula_quic:send(Stream, <<>>).

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
