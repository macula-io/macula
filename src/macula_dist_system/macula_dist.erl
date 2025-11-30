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
%%% Architecture: net_kernel - macula_dist - quicer - UDP/QUIC - remote node
%%%
%%% Node naming convention: port@ip (e.g., 4433@192.168.1.100)
%%%
%%% @copyright 2025 Macula.io Apache-2.0
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist).

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

%% Internal exports
-export([
    acceptor_loop/2,
    do_accept/3,
    do_setup/5
]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-define(FAMILY, inet).
-define(DRIVER, macula_dist).
-define(DEFAULT_PORT, 4433).
-define(ALPN, "macula-dist").
-define(HANDSHAKE_TIMEOUT, 30000).  % 30 seconds
-define(CONNECT_TIMEOUT, 10000).    % 10 seconds

%% Stream IDs for different message types
-define(STREAM_CONTROL, 0).
-define(STREAM_DIST, 1).

%%%===================================================================
%%% Distribution Carrier Callbacks
%%%===================================================================

%% @doc Return child specifications for the distribution supervisor.
%% Called by net_sup during startup.
-spec childspecs() -> [supervisor:child_spec()].
childspecs() ->
    %% The dist controller processes are supervised by net_kernel
    %% We don't need additional supervised children
    [].

%% @doc Listen for incoming distribution connections.
%% Returns a "listen handle" used by accept/1.
-spec listen(atom()) -> {ok, {term(), #net_address{}, 1..3}} | {error, term()}.
listen(NodeName) ->
    Port = get_dist_port(NodeName),
    case start_quic_listener(Port) of
        {ok, ListenerHandle} ->
            Address = make_address(NodeName, Port),
            %% Return: {ListenerHandle, NetAddress, AddressFamily}
            %% AddressFamily: 1=inet, 2=inet6, 3=local
            {ok, {ListenerHandle, Address, 1}};
        {error, Reason} ->
            error_logger:error_msg("macula_dist: listen failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Accept incoming connections.
%% Called in a loop by net_kernel.
-spec accept(term()) -> pid().
accept(ListenerHandle) ->
    spawn_link(?MODULE, acceptor_loop, [self(), ListenerHandle]).

%% @doc Accept a distribution connection.
%% This is called when a connection is being accepted from a remote node.
-spec accept_connection(pid(), term(), node(), term(), term()) -> pid().
accept_connection(AcceptPid, {QuicConn, Stream}, MyNode, Allowed, SetupTime) ->
    spawn_link(?MODULE, do_accept, [
        {AcceptPid, QuicConn, Stream, MyNode, Allowed, SetupTime},
        self(),
        connection_id()
    ]).

%% @doc Setup an outgoing distribution connection.
%% Called when this node wants to connect to another node.
-spec setup(node(), term(), atom(), term(), term()) -> pid().
setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    spawn_link(?MODULE, do_setup, [
        Node, Type, MyNode, LongOrShortNames, SetupTime
    ]).

%% @doc Close a distribution connection.
-spec close(term()) -> ok.
close({QuicConn, _Stream}) ->
    catch quicer:close_connection(QuicConn),
    ok;
close(QuicConn) when is_reference(QuicConn) ->
    catch quicer:close_connection(QuicConn),
    ok;
close(_) ->
    ok.

%% @doc Check if this module should handle distribution to the given node.
%% Returns true if the node name is in port@host format.
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
%% Node name format: port@host (e.g., 4433@192.168.1.100)
-spec splitname(atom() | string()) -> {integer(), string()} | false.
splitname(NodeName) when is_atom(NodeName) ->
    splitname(atom_to_list(NodeName));
splitname(NodeName) when is_list(NodeName) ->
    case string:tokens(NodeName, "@") of
        [PortStr, Host] ->
            case catch list_to_integer(PortStr) of
                Port when is_integer(Port), Port > 0, Port < 65536 ->
                    {Port, Host};
                _ ->
                    false
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
%%% Internal Functions - Listener
%%%===================================================================

%% @private Start QUIC listener for distribution
start_quic_listener(Port) ->
    %% Get or generate TLS certificates
    {CertFile, KeyFile} = get_tls_certs(),

    ListenerOpts = [
        {certfile, CertFile},
        {keyfile, KeyFile},
        {alpn, [?ALPN]},
        {idle_timeout_ms, 60000},
        {peer_bidi_stream_count, 100},
        {peer_unidi_stream_count, 100}
    ],

    case quicer:listen(Port, ListenerOpts) of
        {ok, Listener} ->
            error_logger:info_msg("macula_dist: listening on UDP port ~p~n", [Port]),
            {ok, Listener};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Acceptor loop - waits for incoming connections
acceptor_loop(Kernel, Listener) ->
    %% Accept with passive mode for recv() to work
    %% Stream notifications still come via messages for async_accept_stream
    case quicer:accept(Listener, #{active => false}) of
        {ok, Conn} ->
            error_logger:info_msg("macula_dist: accepted connection ~p~n", [Conn]),
            case quicer:handshake(Conn) of
                {ok, Conn} ->
                    error_logger:info_msg("macula_dist: handshake complete~n"),
                    %% Open or accept a bidirectional stream for distribution
                    case accept_dist_stream(Conn) of
                        {ok, Stream} ->
                            error_logger:info_msg("macula_dist: got stream ~p~n", [Stream]),
                            %% Notify kernel of new connection immediately
                            Kernel ! {accept, self(), {Conn, Stream}, ?FAMILY, ?DRIVER},
                            receive
                                {Kernel, controller, DistCtrl} ->
                                    error_logger:info_msg("macula_dist: transferring to ~p~n", [DistCtrl]),
                                    %% Use handoff_stream to forward any queued active messages
                                    case quicer:handoff_stream(Stream, DistCtrl, #{}) of
                                        ok ->
                                            error_logger:info_msg("macula_dist: stream handoff ok~n"),
                                            %% Transfer connection ownership
                                            case quicer:controlling_process(Conn, DistCtrl) of
                                                ok ->
                                                    error_logger:info_msg("macula_dist: conn transfer ok~n"),
                                                    DistCtrl ! {self(), controller, ok};
                                                {error, ConnErr} ->
                                                    error_logger:warning_msg("macula_dist: conn transfer failed: ~p~n", [ConnErr]),
                                                    close({Conn, Stream})
                                            end;
                                        {error, StreamErr} ->
                                            error_logger:warning_msg("macula_dist: stream handoff failed: ~p~n", [StreamErr]),
                                            close({Conn, Stream})
                                    end;
                                {Kernel, unsupported_protocol} ->
                                    close({Conn, Stream})
                            end;
                        {error, StreamReason} ->
                            error_logger:warning_msg(
                                "macula_dist: stream accept failed: ~p~n",
                                [StreamReason]
                            ),
                            quicer:close_connection(Conn)
                    end;
                {error, HandshakeReason} ->
                    error_logger:warning_msg(
                        "macula_dist: handshake failed: ~p~n",
                        [HandshakeReason]
                    ),
                    quicer:close_connection(Conn)
            end;
        {error, closed} ->
            %% Listener was closed
            exit(normal);
        {error, AcceptReason} ->
            error_logger:warning_msg(
                "macula_dist: accept failed: ~p~n",
                [AcceptReason]
            )
    end,
    acceptor_loop(Kernel, Listener).

%% @private Accept distribution stream on connection
%% Use active mode so data arrives as messages (quicer recv doesn't work with passive)
accept_dist_stream(Conn) ->
    error_logger:info_msg("macula_dist: waiting for stream on connection~n"),
    %% Use active mode - data will arrive as {quic, Data, Stream, ...} messages
    case quicer:accept_stream(Conn, #{active => true}, ?HANDSHAKE_TIMEOUT) of
        {ok, Stream} ->
            error_logger:info_msg("macula_dist: accepted stream: ~p~n", [Stream]),
            {ok, Stream};
        {error, timeout} ->
            error_logger:warning_msg("macula_dist: stream accept timeout~n"),
            {error, stream_timeout};
        {error, Reason} ->
            error_logger:warning_msg("macula_dist: stream accept failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% Internal Functions - Accept Connection
%%%===================================================================

%% @private Handle incoming connection setup
do_accept({AcceptPid, QuicConn, Stream, MyNode, Allowed, SetupTime}, Kernel, _ConnId) ->
    Timer = dist_util:start_timer(SetupTime),
    error_logger:info_msg("macula_dist: do_accept started, waiting for controller~n"),

    %% Wait for control transfer from acceptor
    receive
        {AcceptPid, controller, ok} ->
            error_logger:info_msg("macula_dist: do_accept got controller message~n"),
            ok
    after ?HANDSHAKE_TIMEOUT ->
        error_logger:warning_msg("macula_dist: do_accept controller timeout~n"),
        dist_util:shutdown(?MODULE, 280, control_transfer_timeout)
    end,

    %% Wait for handoff_done from the handoff_stream call
    receive
        {handoff_done, Stream, HandoffData} ->
            error_logger:info_msg("macula_dist: do_accept got handoff_done: ~p~n", [HandoffData]),
            ok
    after 5000 ->
        error_logger:warning_msg("macula_dist: do_accept handoff_done timeout~n"),
        ok
    end,

    error_logger:info_msg("macula_dist: do_accept stream/conn ownership received~n"),

    Socket = {QuicConn, Stream},
    HSData = #hs_data{
        kernel_pid = Kernel,
        this_node = MyNode,
        socket = Socket,
        timer = Timer,
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
    },

    error_logger:info_msg("macula_dist: do_accept starting handshake~n"),
    %% Run distribution handshake
    dist_util:handshake_other_started(HSData).

%%%===================================================================
%%% Internal Functions - Setup Outgoing Connection
%%%===================================================================

%% @private Setup outgoing distribution connection
do_setup(Node, Type, MyNode, _LongOrShortNames, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),

    case splitname(Node) of
        {Port, Host} ->
            case connect_quic(Host, Port) of
                {ok, Conn, Stream} ->
                    setup_loop({Conn, Stream}, Node, Type, MyNode, Timer);
                {error, Reason} ->
                    error_logger:warning_msg(
                        "macula_dist: connection to ~p failed: ~p~n",
                        [Node, Reason]
                    ),
                    dist_util:shutdown(?MODULE, 318, {connect_failed, Reason})
            end;
        false ->
            error_logger:warning_msg(
                "macula_dist: invalid node name format: ~p~n",
                [Node]
            ),
            dist_util:shutdown(?MODULE, 325, invalid_node_name)
    end.

%% @private Connect to remote node via QUIC
%% Uses macula_tls for certificate verification settings (v0.11.0+)
connect_quic(Host, Port) ->
    {CertFile, KeyFile} = get_tls_certs(),

    %% Get TLS verification options from centralized module
    TlsOpts = macula_tls:quic_client_opts(),

    %% Merge with distribution-specific options
    BaseOpts = [
        {alpn, [?ALPN]},
        {certfile, CertFile},
        {keyfile, KeyFile},
        {idle_timeout_ms, 60000}
    ],

    ConnOpts = merge_dist_opts(BaseOpts, TlsOpts),

    error_logger:info_msg("macula_dist: connecting to ~s:~p~n", [Host, Port]),
    case quicer:connect(Host, Port, ConnOpts, ?CONNECT_TIMEOUT) of
        {ok, Conn} ->
            error_logger:info_msg("macula_dist: connected, opening stream~n"),
            %% Open bidirectional stream for distribution - use passive mode
            case quicer:start_stream(Conn, #{active => false}) of
                {ok, Stream} ->
                    error_logger:info_msg("macula_dist: stream opened: ~p~n", [Stream]),
                    {ok, Conn, Stream};
                {error, StreamReason} ->
                    error_logger:warning_msg("macula_dist: stream open failed: ~p~n", [StreamReason]),
                    quicer:close_connection(Conn),
                    {error, {stream_failed, StreamReason}}
            end;
        {error, Reason} ->
            error_logger:warning_msg("macula_dist: connect failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @private Complete connection setup
setup_loop({_Conn, _Stream} = Socket, Node, Type, MyNode, Timer) ->
    HSData = #hs_data{
        kernel_pid = self(),
        this_node = MyNode,
        other_node = Node,
        socket = Socket,
        timer = Timer,
        this_flags = 0,
        other_flags = 0,
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
        mf_getopts = fun quic_getopts/2,
        request_type = Type
    },

    dist_util:handshake_we_started(HSData).

%%%===================================================================
%%% QUIC Socket Operations
%%%===================================================================

%% @private Send data over QUIC stream
quic_send({_Conn, Stream}, Data) ->
    error_logger:info_msg("macula_dist: quic_send ~p bytes~n", [byte_size(iolist_to_binary(Data))]),
    case quicer:send(Stream, Data) of
        {ok, _} -> ok;
        {error, Reason} ->
            error_logger:warning_msg("macula_dist: quic_send error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @private Receive data from QUIC stream
%% Uses active mode - receives data from {quic, Data, Stream, Flags} messages
quic_recv({_Conn, Stream}, _Length, Timeout) ->
    TimeoutMs = case Timeout of
        infinity -> 30000;  % Use 30s default for QUIC
        T -> T
    end,
    %% In active mode, data arrives as messages
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            error_logger:info_msg("macula_dist: quic_recv got ~p bytes~n", [byte_size(Data)]),
            {ok, Data};
        {quic, stream_closed, Stream, _Flags} ->
            error_logger:info_msg("macula_dist: quic_recv stream closed~n"),
            {error, closed};
        {quic, peer_send_shutdown, Stream, _Flags} ->
            error_logger:info_msg("macula_dist: quic_recv peer shutdown~n"),
            {error, closed}
    after TimeoutMs ->
        error_logger:info_msg("macula_dist: quic_recv timeout after ~p ms~n", [TimeoutMs]),
        {error, timeout}
    end.

%% @private Set options before nodeup
%% Note: quicer doesn't support setopt(active) directly, streams stay in their initial mode
quic_setopts_pre_nodeup({_Conn, _Stream}) ->
    ok.

%% @private Set options after nodeup
%% Note: quicer doesn't support setopt(active) directly, streams stay in their initial mode
quic_setopts_post_nodeup({_Conn, _Stream}) ->
    ok.

%% @private Get low-level socket (for process linking)
quic_getll({Conn, _Stream}) ->
    {ok, Conn}.

%% @private Get address information
quic_address({Conn, _Stream}, Node) ->
    case quicer:peername(Conn) of
        {ok, {IP, Port}} ->
            #net_address{
                address = {IP, Port},
                host = atom_to_list(Node),
                protocol = ?DRIVER,
                family = ?FAMILY
            };
        {error, _} ->
            #net_address{
                address = undefined,
                host = atom_to_list(Node),
                protocol = ?DRIVER,
                family = ?FAMILY
            }
    end.

%% @private Called when distribution handshake completes
quic_handshake_complete(_Socket, _Node, _DHandle) ->
    ok.

%% @private Send tick (keepalive)
quic_tick({_Conn, Stream}) ->
    %% Send empty message as keepalive
    quicer:send(Stream, <<>>).

%% @private Get socket statistics
quic_getstat({Conn, _Stream}) ->
    case quicer:getstat(Conn, [send_cnt, recv_cnt, send_pend]) of
        {ok, Stats} -> {ok, Stats};
        {error, _} -> {ok, []}
    end.

%% @private Set socket options
quic_setopts({_Conn, Stream}, Opts) ->
    lists:foreach(
        fun({active, Mode}) -> quicer:setopt(Stream, active, Mode);
           (_) -> ok
        end,
        Opts
    ),
    ok.

%% @private Get socket options
quic_getopts({_Conn, _Stream}, _Opts) ->
    %% Return defaults for now
    {ok, [{active, true}]}.

%%%===================================================================
%%% Utility Functions
%%%===================================================================

%% @private Merge distribution connection options
%% TLS options from macula_tls take precedence over base options
merge_dist_opts(BaseOpts, TlsOpts) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            lists:keystore(Key, 1, Acc, {Key, Value})
        end,
        BaseOpts,
        TlsOpts
    ).

%% @private Get distribution port from node name or config
%% NodeName can be:
%% - Full name: '4433@192.168.1.100' (atom with port@host)
%% - Short name: '4433' (atom with just port - this is what listen/1 receives)
%% - String version of either
get_dist_port(NodeName) when is_atom(NodeName) ->
    get_dist_port(atom_to_list(NodeName));
get_dist_port(NodeName) when is_list(NodeName) ->
    %% First try full name format (port@host)
    case splitname(NodeName) of
        {Port, _Host} ->
            Port;
        false ->
            %% Try parsing as just a port number
            case catch list_to_integer(NodeName) of
                Port when is_integer(Port), Port > 0, Port < 65536 ->
                    Port;
                _ ->
                    application:get_env(kernel, macula_dist_port, ?DEFAULT_PORT)
            end
    end.

%% @private Get TLS certificates
get_tls_certs() ->
    CertDir = application:get_env(kernel, macula_dist_cert_dir, "/tmp/macula_dist"),
    CertFile = filename:join(CertDir, "cert.pem"),
    KeyFile = filename:join(CertDir, "key.pem"),

    %% Generate self-signed certs if they don't exist
    case filelib:is_regular(CertFile) andalso filelib:is_regular(KeyFile) of
        true ->
            {CertFile, KeyFile};
        false ->
            generate_self_signed_cert(CertDir, CertFile, KeyFile)
    end.

%% @private Generate self-signed certificate
generate_self_signed_cert(_CertDir, CertFile, KeyFile) ->
    ok = filelib:ensure_dir(CertFile),

    %% Use OpenSSL to generate self-signed cert
    Cmd = io_lib:format(
        "openssl req -x509 -newkey rsa:2048 -keyout ~s -out ~s "
        "-days 365 -nodes -subj '/CN=macula-dist' 2>/dev/null",
        [KeyFile, CertFile]
    ),

    case os:cmd(lists:flatten(Cmd)) of
        "" ->
            {CertFile, KeyFile};
        Error ->
            error_logger:error_msg("Failed to generate certs: ~s~n", [Error]),
            %% Return paths anyway - will fail later with clear error
            {CertFile, KeyFile}
    end.

%% @private Create net_address record
make_address(NodeName, Port) when is_atom(NodeName) ->
    {ok, Host} = inet:gethostname(),
    make_address(Port, Host);
make_address(Port, Host) when is_integer(Port) ->
    #net_address{
        address = {Host, Port},
        host = Host,
        protocol = ?DRIVER,
        family = ?FAMILY
    }.

%% @private Generate unique connection ID
connection_id() ->
    erlang:unique_integer([positive, monotonic]).
