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
%%% == Architecture ==
%%%
%%% The carrier implements the OTP distribution protocol:
%%%
%%%   net_kernel <-> macula_dist <-> quicer <-> UDP/QUIC <-> remote node
%%%
%%% Node naming convention: port@ip (e.g., 4433@192.168.1.100)
%%%
%%% @copyright 2025 Macula.io
%%% @license Apache-2.0
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
    do_setup/5,
    dist_controller_loop/2
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

%% Distribution flags for OTP 25+ compatibility
%% This includes all mandatory flags including DFLAG_V4_NC
-define(DIST_FLAGS, (
    ?DFLAG_PUBLISHED bor
    ?DFLAG_ATOM_CACHE bor
    ?DFLAG_EXTENDED_REFERENCES bor
    ?DFLAG_DIST_MONITOR bor
    ?DFLAG_FUN_TAGS bor
    ?DFLAG_DIST_MONITOR_NAME bor
    ?DFLAG_HIDDEN_ATOM_CACHE bor
    ?DFLAG_NEW_FUN_TAGS bor
    ?DFLAG_EXTENDED_PIDS_PORTS bor
    ?DFLAG_EXPORT_PTR_TAG bor
    ?DFLAG_BIT_BINARIES bor
    ?DFLAG_NEW_FLOATS bor
    ?DFLAG_UNICODE_IO bor
    ?DFLAG_DIST_HDR_ATOM_CACHE bor
    ?DFLAG_SMALL_ATOM_TAGS bor
    ?DFLAG_UTF8_ATOMS bor
    ?DFLAG_MAP_TAG bor
    ?DFLAG_BIG_CREATION bor
    ?DFLAG_SEND_SENDER bor
    ?DFLAG_BIG_SEQTRACE_LABELS bor
    ?DFLAG_EXIT_PAYLOAD bor
    ?DFLAG_FRAGMENTS bor
    ?DFLAG_HANDSHAKE_23 bor
    ?DFLAG_UNLINK_ID bor
    ?DFLAG_MANDATORY_25_DIGEST bor
    ?DFLAG_SPAWN bor
    ?DFLAG_V4_NC bor
    ?DFLAG_ALIAS
)).

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
    %% Accept connection - connection itself is passive, only stream is active
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
                                    %% Collect any buffered QUIC data before transferring control
                                    BufferedData = collect_quic_data(Stream, []),
                                    error_logger:info_msg("macula_dist: collected ~p buffered messages~n",
                                                         [length(BufferedData)]),
                                    %% Transfer stream ownership to controller
                                    StreamResult = quicer:controlling_process(Stream, DistCtrl),
                                    error_logger:info_msg("macula_dist: stream controlling_process result: ~p~n", [StreamResult]),
                                    ConnResult = quicer:controlling_process(Conn, DistCtrl),
                                    error_logger:info_msg("macula_dist: conn controlling_process result: ~p~n", [ConnResult]),
                                    %% Notify controller with buffered data
                                    DistCtrl ! {self(), controller, {ok, BufferedData}};
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
%% Use active mode - data arrives as {quic, Data, Stream, Flags} messages
accept_dist_stream(Conn) ->
    error_logger:info_msg("macula_dist: waiting for stream on connection~n"),
    %% Use active mode - data will arrive as messages
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

    %% Wait for control transfer from acceptor with buffered data
    receive
        {AcceptPid, controller, {ok, BufferedData}} ->
            error_logger:info_msg("macula_dist: do_accept got controller with ~p buffered msgs~n",
                                 [length(BufferedData)]),
            %% Store buffered data in process dictionary for quic_recv to use
            put(quic_buffered_data, BufferedData)
    after ?HANDSHAKE_TIMEOUT ->
        error_logger:warning_msg("macula_dist: do_accept controller timeout~n"),
        dist_util:shutdown(?MODULE, 280, control_transfer_timeout)
    end,

    error_logger:info_msg("macula_dist: do_accept stream/conn ownership received~n"),

    Socket = {QuicConn, Stream},
    HSData = #hs_data{
        kernel_pid = Kernel,
        this_node = MyNode,
        socket = Socket,
        timer = Timer,
        this_flags = ?DIST_FLAGS,
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
    %% Run distribution handshake with crash reporting
    try
        Result = dist_util:handshake_other_started(HSData),
        error_logger:info_msg("macula_dist: handshake_other_started returned: ~p~n", [Result]),
        %% After handshake completes, enter controller loop
        DHandle = get(dist_handle),
        case DHandle of
            undefined ->
                error_logger:error_msg("macula_dist: no DHandle after handshake!~n"),
                exit({error, no_dhandle});
            _ ->
                error_logger:info_msg("macula_dist: entering controller loop with DHandle=~p~n", [DHandle]),
                dist_controller_loop(Socket, DHandle)
        end
    catch
        Class:Reason:Stack ->
            error_logger:error_msg("macula_dist: handshake CRASHED ~p:~p~n~p~n", [Class, Reason, Stack]),
            exit({handshake_crash, Class, Reason})
    end.

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
connect_quic(Host, Port) ->
    {CertFile, KeyFile} = get_tls_certs(),

    ConnOpts = [
        {alpn, [?ALPN]},
        {certfile, CertFile},
        {keyfile, KeyFile},
        {verify, none},  % TODO: Implement proper certificate verification
        {idle_timeout_ms, 60000}
    ],

    error_logger:info_msg("macula_dist: connecting to ~s:~p~n", [Host, Port]),
    case quicer:connect(Host, Port, ConnOpts, ?CONNECT_TIMEOUT) of
        {ok, Conn} ->
            error_logger:info_msg("macula_dist: connected, opening stream~n"),
            %% Open bidirectional stream for distribution - use active mode
            case quicer:start_stream(Conn, #{active => true}) of
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
    Flags = ?DIST_FLAGS,
    error_logger:info_msg("macula_dist: setup_loop this_flags = ~.16B (~p)~n", [Flags, Flags]),
    HSData = #hs_data{
        kernel_pid = self(),
        this_node = MyNode,
        other_node = Node,
        socket = Socket,
        timer = Timer,
        this_flags = Flags,
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

    try
        Result = dist_util:handshake_we_started(HSData),
        error_logger:info_msg("macula_dist: handshake_we_started returned: ~p~n", [Result]),
        %% After handshake completes, enter controller loop
        DHandle = get(dist_handle),
        case DHandle of
            undefined ->
                error_logger:error_msg("macula_dist: no DHandle after setup handshake!~n"),
                exit({error, no_dhandle});
            _ ->
                error_logger:info_msg("macula_dist: setup entering controller loop with DHandle=~p~n", [DHandle]),
                dist_controller_loop(Socket, DHandle)
        end
    catch
        Class:Reason:Stack ->
            error_logger:error_msg("macula_dist: setup handshake CRASHED ~p:~p~n~p~n", [Class, Reason, Stack]),
            exit({handshake_crash, Class, Reason})
    end.

%%%===================================================================
%%% QUIC Socket Operations
%%%===================================================================

%% @private Send data over QUIC stream with {packet, 2} style framing
%% Prepends 2-byte big-endian length to the data
quic_send({_Conn, Stream}, Data) ->
    Bin = iolist_to_binary(Data),
    Len = byte_size(Bin),
    %% Frame the packet with 2-byte length prefix (big-endian)
    Framed = <<Len:16/big, Bin/binary>>,
    error_logger:info_msg("macula_dist: quic_send ~p bytes (framed: ~p)~n", [Len, byte_size(Framed)]),
    case quicer:send(Stream, Framed) of
        {ok, _} -> ok;
        %% Handle 3-tuple error format from quicer
        {error, ErrorCode, ErrorReason} ->
            error_logger:warning_msg("macula_dist: quic_send error: ~p:~p~n", [ErrorCode, ErrorReason]),
            {error, {ErrorCode, ErrorReason}};
        {error, Reason} ->
            error_logger:warning_msg("macula_dist: quic_send error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @private Receive data from QUIC stream with {packet, 2} style framing
%% Reads 2-byte length prefix, then the payload
%% Returns a LIST (not binary) because dist_util expects list for pattern matching
quic_recv({Conn, Stream}, _Length, Timeout) ->
    TimeoutMs = case Timeout of
        infinity -> 30000;  % Use 30s default for QUIC
        T -> T
    end,
    error_logger:info_msg("macula_dist: quic_recv waiting on stream ~p, pid ~p~n", [Stream, self()]),
    %% Use passive mode recv with framing
    case quic_recv_framed({Conn, Stream}, <<>>, TimeoutMs) of
        {ok, Packet} ->
            %% CRITICAL: dist_util expects a list, not a binary!
            %% Pattern match like {ok, [$N | Data]} requires a list
            PacketList = binary_to_list(Packet),
            error_logger:info_msg("macula_dist: quic_recv got packet ~p bytes as list~n",
                                  [byte_size(Packet)]),
            {ok, PacketList};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Collect data until we have a complete framed packet
%% Buffer accumulates raw bytes until we have length + payload
quic_recv_framed({Conn, Stream}, Buffer, Timeout) ->
    case Buffer of
        %% Have at least 2 bytes - can read length
        <<Len:16/big, Rest/binary>> when byte_size(Rest) >= Len ->
            %% Have complete packet
            <<Packet:Len/binary, _Remaining/binary>> = Rest,
            {ok, Packet};
        <<Len:16/big, Rest/binary>> ->
            %% Need more data for payload
            Needed = Len - byte_size(Rest),
            error_logger:info_msg("macula_dist: need ~p more bytes for payload~n", [Needed]),
            case quic_recv_raw({Conn, Stream}, Timeout) of
                {ok, Data} ->
                    quic_recv_framed({Conn, Stream}, <<Buffer/binary, Data/binary>>, Timeout);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ when byte_size(Buffer) < 2 ->
            %% Need more data to even read length
            case quic_recv_raw({Conn, Stream}, Timeout) of
                {ok, Data} ->
                    quic_recv_framed({Conn, Stream}, <<Buffer/binary, Data/binary>>, Timeout);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @private Receive raw bytes from QUIC stream (active mode)
%% Data arrives as {quic, Data, Stream, Flags} messages
%% First checks process dictionary for buffered data from control transfer
quic_recv_raw({_Conn, Stream}, Timeout) ->
    %% First check for buffered data from control transfer
    case get(quic_buffered_data) of
        [Data | Rest] when is_binary(Data) ->
            put(quic_buffered_data, Rest),
            error_logger:info_msg("macula_dist: quic_recv_raw using buffered ~p bytes~n", [byte_size(Data)]),
            {ok, Data};
        _ ->
            %% No buffered data, wait for new messages
            receive
                {quic, Data, Stream, _Flags} when is_binary(Data) ->
                    error_logger:info_msg("macula_dist: quic_recv_raw got ~p bytes~n", [byte_size(Data)]),
                    {ok, Data};
                {quic, stream_closed, Stream, _Flags} ->
                    error_logger:info_msg("macula_dist: quic_recv_raw stream closed~n"),
                    {error, closed};
                {quic, peer_send_shutdown, Stream, _Flags} ->
                    error_logger:info_msg("macula_dist: quic_recv_raw peer shutdown~n"),
                    {error, closed}
            after Timeout ->
                error_logger:info_msg("macula_dist: quic_recv_raw timeout after ~p ms~n", [Timeout]),
                {error, timeout}
            end
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
%% For QUIC, we use process-based distribution - return the controlling process
%% (the current process doing the handshake), not the QUIC connection ref.
%% erlang:setnode/3 accepts pid() for external/process-based distribution.
quic_getll({_Conn, _Stream}) ->
    {ok, self()}.

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
%% Store the DHandle in process dictionary so controller loop can use it
quic_handshake_complete(Socket, Node, DHandle) ->
    error_logger:info_msg("macula_dist: handshake_complete for ~p, DHandle=~p~n", [Node, DHandle]),
    put(dist_handle, DHandle),
    put(dist_socket, Socket),
    put(dist_node, Node),
    %% Request notification when VM has data to send
    erlang:dist_ctrl_get_data_notification(DHandle),
    ok.

%%%===================================================================
%%% Distribution Controller Loop
%%%===================================================================

%% @doc Distribution controller loop - handles bidirectional message passing
%% This loop runs after handshake completes and handles:
%% - {dist_ctrl, DHandle, Data} from VM -> send via QUIC
%% - {quic, Data, Stream, Flags} from QUIC -> deliver via dist_ctrl_put_data
%% - dist_data notification -> pull data from VM and send
dist_controller_loop({_Conn, Stream} = Socket, DHandle) ->
    receive
        %% VM notifies us it has data to send
        dist_data ->
            dist_controller_send_loop(Socket, DHandle),
            ?MODULE:dist_controller_loop(Socket, DHandle);

        %% Direct data from VM (older method)
        {dist_ctrl, DHandle, Data} ->
            send_dist_data(Stream, Data),
            ?MODULE:dist_controller_loop(Socket, DHandle);

        %% Incoming QUIC data - deliver to VM
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            deliver_dist_data(DHandle, Data),
            ?MODULE:dist_controller_loop(Socket, DHandle);

        %% Stream closed
        {quic, stream_closed, Stream, _Flags} ->
            error_logger:info_msg("macula_dist: stream closed, exiting controller~n"),
            exit(normal);

        %% Peer shutdown
        {quic, peer_send_shutdown, Stream, _Flags} ->
            error_logger:info_msg("macula_dist: peer shutdown, exiting controller~n"),
            exit(normal);

        %% Connection closed
        {quic, closed, _, _Flags} ->
            error_logger:info_msg("macula_dist: connection closed, exiting controller~n"),
            exit(normal);

        %% Unknown message
        Other ->
            error_logger:warning_msg("macula_dist: controller got unknown: ~p~n", [Other]),
            ?MODULE:dist_controller_loop(Socket, DHandle)
    end.

%% @private Pull all pending data from VM and send
dist_controller_send_loop({_Conn, Stream} = Socket, DHandle) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            %% Re-arm notification for next batch
            erlang:dist_ctrl_get_data_notification(DHandle),
            ok;
        Data ->
            send_dist_data(Stream, Data),
            %% Loop to get more data
            dist_controller_send_loop(Socket, DHandle)
    end.

%% @private Send distribution data over QUIC with framing
send_dist_data(Stream, Data) ->
    Bin = iolist_to_binary(Data),
    Len = byte_size(Bin),
    Framed = <<Len:16/big, Bin/binary>>,
    case quicer:send(Stream, Framed) of
        {ok, _} -> ok;
        {error, Code, Reason} ->
            error_logger:warning_msg("macula_dist: send error ~p:~p~n", [Code, Reason]),
            ok;
        {error, Reason} ->
            error_logger:warning_msg("macula_dist: send error ~p~n", [Reason]),
            ok
    end.

%% @private Deliver incoming data to VM
%% Data comes framed with 2-byte length prefix
deliver_dist_data(DHandle, Data) ->
    %% Get or initialize the receive buffer
    Buffer = case get(dist_recv_buffer) of
        undefined -> <<>>;
        B -> B
    end,
    NewBuffer = <<Buffer/binary, Data/binary>>,
    put(dist_recv_buffer, deliver_framed_packets(DHandle, NewBuffer)).

%% @private Extract and deliver complete framed packets
deliver_framed_packets(DHandle, <<Len:16/big, Rest/binary>>) when byte_size(Rest) >= Len ->
    <<Packet:Len/binary, Remaining/binary>> = Rest,
    erlang:dist_ctrl_put_data(DHandle, Packet),
    deliver_framed_packets(DHandle, Remaining);
deliver_framed_packets(_DHandle, Buffer) ->
    %% Not enough data for a complete packet
    Buffer.

%% @private Send tick (keepalive)
%% Must send a framed message to match quic_recv framing expectations
quic_tick({_Conn, Stream}) ->
    %% Send framed empty message (2-byte length = 0)
    case quicer:send(Stream, <<0:16/big>>) of
        {ok, _} -> ok;
        {error, _, _} -> ok;  % Ignore 3-tuple errors
        {error, _} -> ok      % Ignore regular errors
    end.

%% @private Get socket statistics
%% Must return {ok, RecvCnt, SendCnt, SendPend} - a 4-tuple!
%% NOT {ok, PropList} which is what quicer returns
quic_getstat({Conn, _Stream}) ->
    case quicer:getstat(Conn, [recv_cnt, send_cnt, send_pend]) of
        {ok, Stats} ->
            RecvCnt = proplists:get_value(recv_cnt, Stats, 0),
            SendCnt = proplists:get_value(send_cnt, Stats, 0),
            SendPend = proplists:get_value(send_pend, Stats, 0),
            {ok, RecvCnt, SendCnt, SendPend};
        {error, _} ->
            {ok, 0, 0, 0}
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
    %% Return defaults for active mode
    {ok, [{active, true}]}.

%% @private Collect any buffered QUIC data from the mailbox
%% This is called during control transfer to preserve data that arrived
%% between stream creation and ownership transfer
collect_quic_data(Stream, Acc) ->
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            error_logger:info_msg("macula_dist: collecting buffered data ~p bytes~n", [byte_size(Data)]),
            collect_quic_data(Stream, [Data | Acc])
    after 0 ->
        %% No more buffered data, return in order received
        lists:reverse(Acc)
    end.

%%%===================================================================
%%% Utility Functions
%%%===================================================================

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
