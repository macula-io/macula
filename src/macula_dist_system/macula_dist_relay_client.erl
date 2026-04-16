%%%-------------------------------------------------------------------
%%% @doc Client for the Macula dist relay (`macula-io/macula-dist-relay').
%%%
%%% Maintains a persistent QUIC connection to a dedicated dist relay
%%% and exposes a simple API for establishing point-to-point tunnels
%%% between Erlang nodes. Each tunnel is backed by a raw QUIC stream —
%%% no framing, no pub/sub, no application-level encryption.
%%%
%%% This replaces the pub/sub bridge approach in `macula_dist_relay'
%%% (the SDK's previous dist-over-mesh implementation) which forced
%%% dist bytes through the station's MessagePack/handler pipeline.
%%%
%%% == Protocol ==
%%%
%%% Control frames on stream 0 (length-prefixed MessagePack):
%%%
%%% ```
%%%   node → relay    identify{node_name}
%%%   relay → node    identified{status}
%%%   node → relay    tunnel_request{target}
%%%   relay → node    tunnel_ok{tunnel_id} | tunnel_error{reason}
%%%   relay → node    tunnel_notify{tunnel_id, source}
%%%   node → relay    tunnel_close{tunnel_id}
%%% '''
%%%
%%% Tunnel data streams (stream 1+) carry the raw dist wire bytes after
%%% a 32-byte tunnel_id prefix (written by the relay on stream open)
%%% that lets the client match new_stream events to pending tunnels.
%%%
%%% == Lifecycle ==
%%%
%%% 1. `start_link(RelayUrl, NodeName)' connects + sends identify
%%% 2. Await identified reply
%%% 3. `request_tunnel(TargetNode)' blocks until tunnel_ok + tunnel
%%%    stream arrive; returns `{ok, ConnRef, StreamRef}' for use as
%%%    the dist Socket in `macula_dist'
%%% 4. Incoming tunnels: tunnel_notify arrives on control, then a
%%%    new_stream event; client reads 32-byte prefix, matches to the
%%%    notified tunnel, hands the stream to `net_kernel' via the
%%%    standard `{accept, _, Socket, Family, Driver}' protocol.
%%%
%%% == Not yet implemented (Phase 2 MVP) ==
%%%
%%% - net_kernel handoff for incoming tunnels (needs `macula_dist'
%%%   integration on the accept side)
%%% - automatic reconnect on relay disconnect
%%% - multiple relay failover
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay_client).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/2, start_link/3]).
-export([request_tunnel/2, close_tunnel/2, status/1]).
-export([set_kernel/2, whereis_client/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TUNNEL_ID_SIZE, 32).  %% 32-byte hex string (16 raw bytes → encode_hex)
-define(IDENTIFY_TIMEOUT, 10_000).
-define(TUNNEL_TIMEOUT, 15_000).
-define(RELAY_ALPN, "macula-dist").

-record(pending_tunnel, {
    from            :: gen_server:from(),
    tunnel_id       :: binary(),
    started_ms      :: integer()
}).

-record(state, {
    conn                :: reference() | undefined,
    control             :: reference() | undefined,
    node_name           :: binary(),
    identified = false  :: boolean(),
    recv_buf = <<>>     :: binary(),
    %% net_kernel pid for inbound tunnel accept delivery.
    %% Set by macula_dist:accept/1 before any inbound tunnels are expected.
    kernel_pid          :: pid() | undefined,
    %% Streams we've received but haven't yet matched to a tunnel
    %% (waiting for the 32-byte prefix).
    unidentified_streams = #{} :: #{reference() => binary()},
    %% Outbound tunnels: tunnel_id → pending_tunnel (awaiting stream)
    pending_outbound = #{} :: #{binary() => #pending_tunnel{}},
    %% Inbound tunnels: tunnel_id → source_node (awaiting stream)
    pending_inbound = #{}  :: #{binary() => binary()},
    %% Active tunnels: stream → tunnel_id
    active_tunnels = #{}   :: #{reference() => binary()},
    %% Setup processes currently doing the net_kernel handoff for inbound
    %% tunnels. Monitored so we notice crashes during handshake.
    setups = #{}           :: #{reference() => binary()}  %% MonitorRef → TunnelId
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(binary() | string(), binary()) -> {ok, pid()} | {error, term()}.
start_link(RelayUrl, NodeName) ->
    start_link(RelayUrl, NodeName, #{}).

%% @doc Start the client with a locally-registered name so macula_dist
%% can find it without being passed the pid. Only one dist_relay_client
%% per node makes sense — a node connects to exactly one dist relay for
%% its cluster traffic.
-spec start_link(binary() | string(), binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(RelayUrl, NodeName, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {RelayUrl, NodeName, Opts}, []).

%% @doc Tell the client which process to deliver `{accept, ...}' messages
%% to when inbound tunnels arrive. Called by `macula_dist:accept/1' with
%% the net_kernel pid (`self()' at that call site).
-spec set_kernel(pid(), pid()) -> ok.
set_kernel(Client, Kernel) when is_pid(Client), is_pid(Kernel) ->
    gen_server:call(Client, {set_kernel, Kernel}).

%% @doc Locate the registered client, if any.
-spec whereis_client() -> pid() | undefined.
whereis_client() ->
    erlang:whereis(?MODULE).

%% @doc Request a tunnel to TargetNode. Blocks until the tunnel stream
%% is ready or the request fails/times out. Returns a {ConnRef, StreamRef}
%% tuple suitable for use as the dist Socket in `macula_dist'.
-spec request_tunnel(pid(), binary()) ->
    {ok, reference(), reference()} | {error, term()}.
request_tunnel(Client, TargetNode) when is_binary(TargetNode) ->
    gen_server:call(Client, {request_tunnel, TargetNode}, ?TUNNEL_TIMEOUT + 5_000).

-spec close_tunnel(pid(), binary()) -> ok.
close_tunnel(Client, TunnelId) ->
    gen_server:cast(Client, {close_tunnel, TunnelId}).

-spec status(pid()) -> map().
status(Client) ->
    gen_server:call(Client, status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({RelayUrl, NodeName, _Opts}) ->
    process_flag(trap_exit, true),
    State0 = #state{node_name = NodeName},
    start_connect(parse_url(RelayUrl), State0).

handle_call({request_tunnel, _Target}, _From, #state{identified = false} = State) ->
    {reply, {error, not_identified}, State};
handle_call({request_tunnel, Target}, From, State) ->
    send_tunnel_request(Target, From, State);
handle_call({set_kernel, Kernel}, _From, State) ->
    ?LOG_INFO("[dist_relay_client] kernel_pid set to ~p", [Kernel]),
    {reply, ok, State#state{kernel_pid = Kernel}};
handle_call(status, _From, State) ->
    {reply, status_map(State), State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({close_tunnel, TunnelId}, #state{control = Ctrl} = State) when Ctrl =/= undefined ->
    Frame = macula_dist_relay_protocol:encode(
        #{type => tunnel_close, tunnel_id => TunnelId}
    ),
    macula_quic:send(Ctrl, Frame),
    {noreply, remove_tunnel(TunnelId, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --- QUIC events ---

handle_info({quic, new_stream, Stream, _Props}, #state{control = undefined} = State) ->
    %% First stream arriving after connect = the relay accepting our
    %% control stream open. But our client OPENS the control stream,
    %% so new_stream events here are tunnel streams opened by the relay.
    %% Since we have no control stream yet, this is unexpected — ignore.
    ?LOG_WARNING("[dist_relay_client] Unexpected new_stream before control: ~p", [Stream]),
    {noreply, State};
handle_info({quic, new_stream, Stream, _Props}, State) ->
    ok = macula_quic:setopt(Stream, active, true),
    ?LOG_DEBUG("[dist_relay_client] Incoming tunnel stream ~p, awaiting prefix", [Stream]),
    Unident = (State#state.unidentified_streams)#{Stream => <<>>},
    {noreply, State#state{unidentified_streams = Unident}};

%% Data on the control stream
handle_info({quic, Data, Stream, _Flags},
            #state{control = Stream, recv_buf = Buf} = State)
  when is_binary(Data) ->
    NewBuf = <<Buf/binary, Data/binary>>,
    {Msgs, Remaining} = macula_dist_relay_protocol:decode_buffer(NewBuf),
    State2 = lists:foldl(fun handle_control_msg/2, State, Msgs),
    {noreply, State2#state{recv_buf = Remaining}};

%% Data on an unidentified tunnel stream — accumulate until prefix is complete
handle_info({quic, Data, Stream, _Flags}, State) when is_binary(Data) ->
    handle_tunnel_data(Stream, Data, State);

%% Stream/connection shutdown events
handle_info({quic, peer_send_shutdown, Stream, _}, State) ->
    {noreply, drop_stream(Stream, State)};
handle_info({quic, Closed, Ref, _}, State)
  when Closed =:= closed; Closed =:= shutdown; Closed =:= transport_shutdown ->
    handle_closure(Ref, Closed, State);

%% Inbound-tunnel setup process lifecycle
handle_info({'DOWN', MonRef, process, _Pid, Reason}, #state{setups = Setups} = State) ->
    handle_setup_down(maps:take(MonRef, Setups), Reason, State);

handle_info(Info, State) ->
    ?LOG_DEBUG("[dist_relay_client] Unhandled: ~p", [Info]),
    {noreply, State}.

handle_setup_down({TunnelId, Rest}, normal, State) ->
    ?LOG_DEBUG("[dist_relay_client] Setup for ~s completed", [TunnelId]),
    {noreply, State#state{setups = Rest}};
handle_setup_down({TunnelId, Rest}, Reason, State) ->
    ?LOG_WARNING("[dist_relay_client] Setup for ~s crashed: ~p", [TunnelId, Reason]),
    {noreply, State#state{setups = Rest}};
handle_setup_down(error, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) when Conn =/= undefined ->
    catch macula_quic:close_connection(Conn),
    ok;
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Connect + identify
%%====================================================================

start_connect({ok, Host, Port}, State) ->
    TlsOpts = macula_tls:quic_client_opts(),
    ConnOpts = [{alpn, [?RELAY_ALPN]}, {idle_timeout_ms, 60_000} | TlsOpts],
    handle_connect(macula_quic:connect(Host, Port, ConnOpts, 10_000), State);
start_connect({error, Reason}, _State) ->
    {stop, {bad_relay_url, Reason}}.

handle_connect({ok, Conn}, State) ->
    handle_open_control(macula_quic:open_stream(Conn), Conn, State);
handle_connect({error, Reason}, _State) ->
    {stop, {connect_failed, Reason}}.

handle_open_control({ok, Ctrl}, Conn, State) ->
    ok = macula_quic:setopt(Ctrl, active, true),
    send_identify(Ctrl, State#state.node_name),
    {ok, State#state{conn = Conn, control = Ctrl}};
handle_open_control({error, Reason}, Conn, _State) ->
    catch macula_quic:close_connection(Conn),
    {stop, {control_stream_failed, Reason}}.

send_identify(Ctrl, NodeName) ->
    Frame = macula_dist_relay_protocol:encode(
        #{type => identify, node_name => NodeName}
    ),
    macula_quic:send(Ctrl, Frame).

%%====================================================================
%% Tunnel request
%%====================================================================

send_tunnel_request(Target, From, #state{control = Ctrl} = State) ->
    Frame = macula_dist_relay_protocol:encode(
        #{type => tunnel_request, target => Target}
    ),
    macula_quic:send(Ctrl, Frame),
    %% We don't know the tunnel_id yet — the relay picks it. Store the
    %% From in a pending-awaiting-tunnel-ok queue keyed by Target; when
    %% tunnel_ok arrives we move the From into pending_outbound keyed by
    %% tunnel_id. Simpler: store in a FIFO list.
    Pending = State#state.pending_outbound,
    Placeholder = #pending_tunnel{
        from = From,
        tunnel_id = <<"__pending__", Target/binary>>,
        started_ms = erlang:monotonic_time(millisecond)
    },
    %% Use a synthetic key with Target so we can match the next tunnel_ok.
    Key = {awaiting_ok, Target, From},
    {noreply, State#state{pending_outbound = Pending#{Key => Placeholder}}}.

%%====================================================================
%% Control message handling
%%====================================================================

handle_control_msg(#{type := identified, status := ok}, State) ->
    ?LOG_INFO("[dist_relay_client] Identified as ~s", [State#state.node_name]),
    State#state{identified = true};

handle_control_msg(#{type := tunnel_ok, tunnel_id := TunnelId}, State) ->
    %% Match the first awaiting_ok entry and re-key under tunnel_id.
    %% Caller will be replied to once the stream's prefix is matched.
    rekey_pending(TunnelId, State);

handle_control_msg(#{type := tunnel_error, reason := Reason}, State) ->
    %% Fail the oldest awaiting_ok entry.
    fail_oldest_pending({error, {tunnel_error, Reason}}, State);

handle_control_msg(#{type := tunnel_notify, tunnel_id := TunnelId, source := Source}, State) ->
    ?LOG_INFO("[dist_relay_client] Incoming tunnel ~s from ~s", [TunnelId, Source]),
    Inbound = (State#state.pending_inbound)#{TunnelId => Source},
    try_match_unidentified(State#state{pending_inbound = Inbound});

handle_control_msg(Msg, State) ->
    ?LOG_WARNING("[dist_relay_client] Unknown control message: ~p", [Msg]),
    State.

%% Move a placeholder {awaiting_ok, Target, From} → keyed by tunnel_id.
%% Takes the oldest matching entry (FIFO).
rekey_pending(TunnelId, #state{pending_outbound = Pending} = State) ->
    case take_oldest_awaiting(Pending) of
        {ok, Key, #pending_tunnel{} = PT, Rest} ->
            Updated = PT#pending_tunnel{tunnel_id = TunnelId},
            NewPending = Rest#{TunnelId => Updated},
            ?LOG_DEBUG("[dist_relay_client] Re-keyed ~p to tunnel ~s",
                       [Key, TunnelId]),
            try_match_unidentified(State#state{pending_outbound = NewPending});
        not_found ->
            ?LOG_WARNING("[dist_relay_client] tunnel_ok without pending: ~s", [TunnelId]),
            State
    end.

fail_oldest_pending(Reply, #state{pending_outbound = Pending} = State) ->
    case take_oldest_awaiting(Pending) of
        {ok, _Key, #pending_tunnel{from = From}, Rest} ->
            gen_server:reply(From, Reply),
            State#state{pending_outbound = Rest};
        not_found ->
            ?LOG_WARNING("[dist_relay_client] tunnel_error with no pending"),
            State
    end.

take_oldest_awaiting(Pending) ->
    %% Oldest = lowest started_ms. Only awaiting_ok keys qualify.
    Awaiting = [{K, V} || {{awaiting_ok, _, _} = K, V} <- maps:to_list(Pending)],
    find_oldest(Awaiting, Pending).

find_oldest([], _Pending) ->
    not_found;
find_oldest(Awaiting, Pending) ->
    {Key, V} = lists:foldl(fun oldest/2, hd(Awaiting), tl(Awaiting)),
    {ok, Key, V, maps:remove(Key, Pending)}.

oldest({_K1, #pending_tunnel{started_ms = T1}} = A,
       {_K2, #pending_tunnel{started_ms = T2}} = B) ->
    case T1 =< T2 of
        true  -> A;
        false -> B
    end.

%%====================================================================
%% Tunnel stream prefix handling
%%====================================================================

handle_tunnel_data(Stream, Data, State) ->
    Unident = State#state.unidentified_streams,
    handle_stream_data(maps:find(Stream, Unident), Stream, Data, State).

handle_stream_data({ok, Accumulated}, Stream, Data,
                   #state{unidentified_streams = Unident} = State) ->
    Combined = <<Accumulated/binary, Data/binary>>,
    maybe_extract_prefix(Stream, Combined, Unident, State);
handle_stream_data(error, _Stream, _Data, State) ->
    %% Stream is already identified and owned by the dist controller.
    %% Bytes arriving here should not happen — once handed off, the
    %% controller owns the mailbox. Ignore.
    State2 = State,
    {noreply, State2}.

maybe_extract_prefix(Stream, Buf, Unident, State)
  when byte_size(Buf) < ?TUNNEL_ID_SIZE ->
    {noreply, State#state{unidentified_streams = Unident#{Stream => Buf}}};
maybe_extract_prefix(Stream, Buf, Unident, State) ->
    <<TunnelId:?TUNNEL_ID_SIZE/binary, Rest/binary>> = Buf,
    NewUnident = maps:remove(Stream, Unident),
    State2 = State#state{unidentified_streams = NewUnident},
    match_tunnel(TunnelId, Stream, Rest, State2).

%% Try to match a freshly-identified stream against pending outbound/inbound.
match_tunnel(TunnelId, Stream, LeftoverBytes, State) ->
    match_outbound(maps:take(TunnelId, State#state.pending_outbound),
                   TunnelId, Stream, LeftoverBytes, State).

match_outbound({#pending_tunnel{from = From}, Rest}, TunnelId, Stream, Leftover, State) ->
    %% Outbound match. Transfer stream ownership to the caller BEFORE
    %% replying so subsequent {quic, ...} messages go to the caller, not
    %% to this gen_server. Then re-inject any bytes that arrived with
    %% the prefix (typically the peer's first dist handshake frame) as
    %% a synthetic QUIC data event the caller will see when it starts
    %% reading.
    {CallerPid, _Tag} = From,
    ok = macula_quic:controlling_process(Stream, CallerPid),
    deliver_leftover(CallerPid, Stream, Leftover),
    gen_server:reply(From, {ok, State#state.conn, Stream}),
    Active = (State#state.active_tunnels)#{Stream => TunnelId},
    {noreply, State#state{pending_outbound = Rest, active_tunnels = Active}};
match_outbound(error, TunnelId, Stream, Leftover, State) ->
    match_inbound(maps:take(TunnelId, State#state.pending_inbound),
                  TunnelId, Stream, Leftover, State).

%% Re-inject leftover bytes as a QUIC data event to the new stream owner.
%% This is critical: if prefix + first dist frame arrive in the same QUIC
%% data message, the dist frame would be lost without this handoff.
deliver_leftover(_Pid, _Stream, <<>>) ->
    ok;
deliver_leftover(Pid, Stream, Leftover) ->
    Pid ! {quic, Leftover, Stream, #{}},
    ok.

match_inbound({_Source, Rest}, TunnelId, Stream,
              _Leftover, #state{kernel_pid = undefined} = State) ->
    ?LOG_WARNING("[dist_relay_client] Inbound tunnel ~s arrived before "
                 "kernel_pid set — dropping stream ~p", [TunnelId, Stream]),
    catch macula_quic:close_stream(Stream),
    {noreply, State#state{pending_inbound = Rest}};
match_inbound({_Source, Rest}, TunnelId, Stream, Leftover,
              #state{kernel_pid = Kernel, conn = Conn, setups = Setups} = State) ->
    %% Start a monitored setup process for the net_kernel accept handshake.
    %% The handshake requires synchronous receive — can't do it inside a
    %% gen_server. This is the "one-shot setup-and-handoff" exception
    %% documented in feedback_no_anonymous_spawn.md: the process exits
    %% after transferring stream ownership, and we monitor it for
    %% crash visibility.
    Socket = {Conn, Stream},
    {SetupPid, MonRef} = spawn_monitor(
        fun() -> inbound_accept_setup(Kernel, Socket, TunnelId) end),
    ?LOG_INFO("[dist_relay_client] Inbound tunnel ~s → setup ~p",
              [TunnelId, SetupPid]),
    ok = macula_quic:controlling_process(Stream, SetupPid),
    %% Hand over ownership along with any leftover bytes. Setup process
    %% will re-inject them to the dist controller after the net_kernel
    %% handshake.
    SetupPid ! {stream_owned, Stream, Leftover},
    Active = (State#state.active_tunnels)#{Stream => TunnelId},
    NewSetups = Setups#{MonRef => TunnelId},
    {noreply, State#state{
        pending_inbound = Rest,
        active_tunnels = Active,
        setups = NewSetups
    }};
match_inbound(error, TunnelId, _Stream, _Leftover, State) ->
    ?LOG_WARNING("[dist_relay_client] Stream prefix ~s matches no pending tunnel",
                 [TunnelId]),
    {noreply, State}.

%% Short-lived process that owns the stream through the net_kernel accept
%% handshake. Exits normally after transferring ownership to the dist
%% controller (and re-injecting any leftover bytes that came with the
%% tunnel prefix), or crashes (visible via DOWN in the client) if the
%% handshake fails.
inbound_accept_setup(Kernel, {_Conn, Stream} = Socket, TunnelId) ->
    Leftover = wait_for_ownership(Stream, TunnelId),
    Kernel ! {accept, self(), Socket, inet, macula_dist},
    handle_controller_assignment(Kernel, Stream, Leftover, TunnelId).

wait_for_ownership(Stream, TunnelId) ->
    receive
        {stream_owned, Stream, Data} -> Data
    after 5_000 ->
        exit({setup_timeout, stream_ownership, TunnelId})
    end.

handle_controller_assignment(Kernel, Stream, Leftover, TunnelId) ->
    receive
        {Kernel, controller, DistCtrl} ->
            ok = macula_quic:controlling_process(Stream, DistCtrl),
            deliver_leftover(DistCtrl, Stream, Leftover),
            DistCtrl ! {self(), controller, ok},
            ok;
        {Kernel, unsupported_protocol} ->
            exit({unsupported_protocol, TunnelId})
    after 30_000 ->
        exit({setup_timeout, controller_assignment, TunnelId})
    end.

%% Called after state changes that might enable a match — re-runs
%% the prefix extraction pass on any accumulated unidentified streams
%% whose buffer might now be matchable. In practice, the buffers only
%% grow when data arrives, so this is a no-op here. Kept as a seam
%% for future logic (e.g. timeout-driven cleanup).
try_match_unidentified(State) ->
    State.

%%====================================================================
%% Cleanup
%%====================================================================

drop_stream(Stream, State) ->
    Unident = maps:remove(Stream, State#state.unidentified_streams),
    Active = maps:remove(Stream, State#state.active_tunnels),
    State#state{unidentified_streams = Unident, active_tunnels = Active}.

remove_tunnel(TunnelId, State) ->
    %% Remove from pending_outbound (by tunnel_id key or awaiting_ok
    %% placeholder — caller already knows tunnel_id so normal case)
    NewPending = maps:remove(TunnelId, State#state.pending_outbound),
    NewInbound = maps:remove(TunnelId, State#state.pending_inbound),
    NewActive = maps:filter(fun(_S, TId) -> TId =/= TunnelId end,
                            State#state.active_tunnels),
    State#state{
        pending_outbound = NewPending,
        pending_inbound = NewInbound,
        active_tunnels = NewActive
    }.

handle_closure(Ref, Closed, #state{conn = Ref} = State) ->
    ?LOG_WARNING("[dist_relay_client] Relay connection ~p", [Closed]),
    {stop, {relay_closed, Closed}, State};
handle_closure(Ref, _Closed, State) ->
    {noreply, drop_stream(Ref, State)}.

%%====================================================================
%% Utilities
%%====================================================================

parse_url(Url) when is_binary(Url) -> parse_url(binary_to_list(Url));
parse_url("quic://" ++ Rest) -> parse_host_port(Rest);
parse_url(Other) -> parse_host_port(Other).

parse_host_port(Str) ->
    case string:tokens(Str, ":") of
        [Host, PortStr] ->
            parse_port(Host, catch list_to_integer(PortStr));
        [Host] ->
            {ok, Host, 4434};
        _ ->
            {error, {invalid_url, Str}}
    end.

parse_port(Host, Port) when is_integer(Port), Port > 0, Port < 65536 ->
    {ok, Host, Port};
parse_port(_Host, _) ->
    {error, invalid_port}.

status_map(State) ->
    #{
        identified => State#state.identified,
        node_name => State#state.node_name,
        pending_outbound => maps:size(State#state.pending_outbound),
        pending_inbound => maps:size(State#state.pending_inbound),
        active_tunnels => maps:size(State#state.active_tunnels),
        unidentified_streams => maps:size(State#state.unidentified_streams)
    }.
