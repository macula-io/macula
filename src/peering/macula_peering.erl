%% @doc Macula peering — connection state machine API.
%%
%% Each peer connection is one `macula_peering_conn' gen_statem under the
%% `macula_peering_conn_sup' simple_one_for_one supervisor.
%%
%% Two entry points:
%% <ul>
%%   <li>`connect/1' — outbound dial; worker drives the QUIC connect.</li>
%%   <li>`accept/2' — inbound; caller transfers ownership of an
%%       already-established `macula_quic' connection (a `reference()')
%%       to a new worker.</li>
%% </ul>
%%
%% The caller passes a `controlling_pid' in opts; that pid receives
%% peering events as messages:
%% <ul>
%%   <li>`{macula_peering, connected, ConnPid, PeerNodeId}'</li>
%%   <li>`{macula_peering, frame, ConnPid, Frame}' (post-handshake)</li>
%%   <li>`{macula_peering, disconnected, ConnPid, Reason}'</li>
%% </ul>
-module(macula_peering).

-export([
    connect/1,
    accept/2,
    close/1, close/2,
    send_frame/2
]).

-type opts() :: macula_peering_conn:opts().
-export_type([opts/0]).

%%------------------------------------------------------------------
%% Public API
%%------------------------------------------------------------------

%% @doc Outbound connect. Spawns a worker that opens a QUIC connection to
%% `target' and runs the CONNECT/HELLO handshake.
-spec connect(opts()) -> {ok, pid()} | {error, term()}.
connect(Opts) ->
    macula_peering_conn_sup:start_conn(Opts#{role => client}).

%% @doc Inbound accept. Caller currently owns `Conn' (e.g. it's the listener
%% owner that just received `{quic, new_conn, Conn, _}'). The transfer of
%% ownership and the handshake start are sequenced atomically.
-spec accept(reference(), opts()) ->
    {ok, pid()} | {error, term()}.
accept(Conn, Opts) ->
    start_server_worker(Conn, Opts#{role => server, quic_conn => Conn}).

start_server_worker(Conn, Opts) ->
    handle_started(macula_peering_conn_sup:start_conn(Opts), Conn).

handle_started({ok, Pid}, Conn) ->
    ok = macula_quic:controlling_process(Conn, Pid),
    ok = gen_statem:cast(Pid, start_handshake),
    {ok, Pid};
handle_started(Err, _Conn) ->
    Err.

%% @doc Initiate a graceful close (sends GOODBYE, drains 5s, terminates).
-spec close(pid()) -> ok.
close(Pid) ->
    close(Pid, operator_stop).

-spec close(pid(), atom()) -> ok.
close(Pid, Reason) ->
    gen_statem:cast(Pid, {close, Reason}).

%% @doc Send a frame through the peer connection. Signs the frame with
%% the local identity if it isn't already signed. Fire-and-forget.
-spec send_frame(pid(), macula_frame:frame()) -> ok.
send_frame(Pid, Frame) when is_map(Frame) ->
    gen_statem:cast(Pid, {send_frame, Frame}).
