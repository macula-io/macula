%%%-------------------------------------------------------------------
%%% @doc Client for the Macula dist relay (`macula-io/macula-dist-relay').
%%%
%%% Maintains a persistent QUIC connection to a dedicated dist relay
%%% and exposes a simple API for establishing point-to-point tunnels
%%% between Erlang nodes. Each tunnel is backed by a raw QUIC stream —
%%% no framing, no pub/sub, no application-level encryption.
%%%
%%% This replaces the pub/sub bridge approach in `macula_dist_pool'
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
%%%    stream arrive; returns `{ok, ConnRef, StreamRef, Received}' for use
%%%    as the dist Socket in `macula_dist', where `Received' holds the
%%%    tunnel's bytes the client read before handing the stream over
%%% 4. Incoming tunnels: tunnel_notify arrives on control, then a
%%%    new_stream event; client reads 32-byte prefix, matches to the
%%%    notified tunnel, hands the stream to `net_kernel' via the
%%%    standard `{accept, _, Socket, Family, Driver}' protocol, and the
%%%    tunnel's bytes read before the handoff reach the dist controller
%%%    in its controller-ok message.
%%%
%%% == Tunnel bytes at a handoff ==
%%%
%%% A tunnel stream changes owner once or twice: to the caller of
%%% `request_tunnel/2', or to a setup process and then the dist controller.
%%% The bytes an owner read before the handoff, including those that
%%% arrive while a tunnel waits for tunnel_ok or tunnel_notify, travel
%%% inside the handoff message, so the new owner has them before anything
%%% the stream delivers to it directly.
%%%
%%% == Control stream ==
%%%
%%% Control frames reach the relay in the order the client sends them.
%%% While the relay takes no data on the control stream, the client holds
%%% them and keeps serving tunnel requests, inbound tunnels and `status/1',
%%% which reports how many it holds as `held_control_frames'.
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

-export([start_link/2, start_link/3, child_spec/2]).
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
    setups = #{}           :: #{reference() => binary()},  %% MonitorRef → TunnelId
    %% Streams whose prefix has been extracted but no pending tunnel
    %% matches yet — because the control message (tunnel_ok or
    %% tunnel_notify) is still in flight. Re-matched once the control
    %% side registers the tunnel_id.
    orphan_streams = #{}   :: #{binary() => {reference(), binary()}},  %% TunnelId → {Stream, bytes read after the prefix}
    %% Control frames the control stream has not taken yet, oldest first.
    held_control = queue:new() :: queue:queue(binary())
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

%% @doc Child spec for running the client under `macula_root', as
%% `macula:join_dist_relay/1' does. Temporary: the client ends when the
%% relay closes the connection and has no reconnect, so a restart would
%% only repeat against a relay that is gone.
-spec child_spec(binary() | string(), binary()) -> supervisor:child_spec().
child_spec(RelayUrl, NodeName) ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, [RelayUrl, NodeName]},
      restart => temporary,
      shutdown => 5000,
      type => worker,
      modules => [?MODULE]}.

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
%% is ready or the request fails/times out. Returns the connection and the
%% stream, suitable for use as the dist Socket in `macula_dist', and the
%% tunnel's bytes this client read before handing the stream over: they
%% come before anything the stream delivers to the caller.
-spec request_tunnel(pid(), binary()) ->
    {ok, reference(), reference(), binary()} | {error, term()}.
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
    control_noreply(send_control(Frame, remove_tunnel(TunnelId, State)));
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --- QUIC events ---

handle_info({quic, new_stream, Stream, _Props}, #state{control = undefined} = State) ->
    %% Our client OPENS the control stream, so new_stream events here
    %% are relay-initiated tunnel streams. Without a control stream yet
    %% we can't correlate them, so this is unexpected.
    ?LOG_WARNING("[dist_relay_client] Unexpected new_stream before control: ~p", [Stream]),
    {noreply, State};
handle_info({quic, new_stream, Stream, _Props}, #state{conn = Conn} = State) ->
    ok = macula_quic:setopt(Stream, active, true),
    %% Re-arm accept so the next tunnel stream also gets a new_stream event.
    ok = macula_quic:async_accept_stream(Conn),
    ?LOG_INFO("[dist_relay_client] Incoming tunnel stream ~p, awaiting prefix", [Stream]),
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

%% The control stream ending means the relay is gone. The QUIC NIF reports
%% a lost connection as a failed read on the connection's streams
%% (`stream_closed'), and a relay that finishes the control stream
%% (`peer_send_shutdown') sends no more control frames.
handle_info({quic, stream_closed, Stream, Flags}, #state{control = Stream} = State) ->
    relay_lost({stream_closed, Flags}, State);
handle_info({quic, peer_send_shutdown, Stream, _}, #state{control = Stream} = State) ->
    relay_lost(peer_send_shutdown, State);
%% A failed write on the control stream leaves the relay as unreachable as
%% a closed control stream does.
handle_info({quic, send_failed, Stream, Reason}, #state{control = Stream} = State) ->
    relay_lost({send_failed, Reason}, State);
%% The control stream takes data again: send the held control frames.
handle_info({quic, send_ready, Stream, undefined}, #state{control = Stream} = State) ->
    control_noreply(flush_control(State));

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
    try macula_quic:close_connection(Conn) catch _:_ -> ok end,
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
    %% Register to receive {quic, new_stream, ...} events for tunnel
    %% streams the relay opens on our connection. Without this, inbound
    %% tunnel streams (and the streams the relay opens for our own
    %% outbound tunnel_requests) are silently dropped.
    ok = macula_quic:async_accept_stream(Conn),
    identify_sent(send_control(identify_frame(State#state.node_name),
                               State#state{conn = Conn, control = Ctrl}),
                  Conn);
handle_open_control({error, Reason}, Conn, _State) ->
    try macula_quic:close_connection(Conn) catch _:_ -> ok end,
    {stop, {control_stream_failed, Reason}}.

identify_sent({ok, State}, _Conn) ->
    {ok, State};
identify_sent({relay_lost, Why, _State}, Conn) ->
    try macula_quic:close_connection(Conn) catch _:_ -> ok end,
    {stop, {control_stream_failed, Why}}.

identify_frame(NodeName) ->
    macula_dist_relay_protocol:encode(#{type => identify, node_name => NodeName}).

%%====================================================================
%% Tunnel request
%%====================================================================

send_tunnel_request(Target, From, State) ->
    Frame = macula_dist_relay_protocol:encode(
        #{type => tunnel_request, target => Target}
    ),
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
    control_noreply(send_control(Frame,
                                 State#state{pending_outbound = Pending#{Key => Placeholder}})).

%%====================================================================
%% Control frames out
%%====================================================================

%% Queue a control frame behind those the control stream has not taken
%% yet, then send as many as it takes, oldest first. While it answers busy
%% the rest wait here for its send_ready, so frames reach the relay in the
%% order they were queued.
send_control(Frame, #state{held_control = Held} = State) ->
    flush_control(State#state{held_control = queue:in(Frame, Held)}).

flush_control(#state{control = Ctrl, held_control = Held} = State) ->
    flushed(queue:peek(Held), Ctrl, State).

flushed(empty, _Ctrl, State) ->
    {ok, State};
flushed({value, Frame}, Ctrl, State) ->
    control_taken(macula_quic:async_send(Ctrl, Frame), State).

control_taken(ok, #state{held_control = Held} = State) ->
    flush_control(State#state{held_control = queue:drop(Held)});
control_taken({error, busy}, State) ->
    {ok, State};
control_taken({error, Reason}, State) ->
    {relay_lost, {send_failed, Reason}, State}.

control_noreply({ok, State}) ->
    {noreply, State};
control_noreply({relay_lost, Why, State}) ->
    relay_lost(Why, State).

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
    %% A stream for this tunnel may have arrived before the notify —
    %% drain orphan_streams now that pending_inbound has the tunnel_id.
    drain_orphan_stream(TunnelId, State#state{pending_inbound = Inbound});

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
            %% A stream for this tunnel may already be orphaned waiting
            %% for the tunnel_ok — drain it now that pending_outbound
            %% has the tunnel_id.
            drain_orphan_stream(TunnelId,
                State#state{pending_outbound = NewPending});
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
handle_stream_data(error, Stream, Data, #state{orphan_streams = Orphans} = State) ->
    {noreply, read_after_prefix(orphan_tunnel(Stream, Orphans), Stream, Data, State)}.

%% The tunnel whose stream waits for tunnel_ok or tunnel_notify, if any.
orphan_tunnel(Stream, Orphans) ->
    [TunnelId || {TunnelId, {Waiting, _Received}} <- maps:to_list(Orphans), Waiting =:= Stream].

%% Bytes read while a stream waits for its tunnel stay with it, in order,
%% until the stream is handed over.
read_after_prefix([TunnelId], Stream, Data, #state{orphan_streams = Orphans} = State) ->
    #{TunnelId := {Stream, Received}} = Orphans,
    State#state{orphan_streams = Orphans#{TunnelId := {Stream, <<Received/binary, Data/binary>>}}};
read_after_prefix([], Stream, Data, State) ->
    ?LOG_WARNING("[dist_relay_client] ~p bytes for stream ~p, which this client no longer owns",
                 [byte_size(Data), Stream]),
    State.

maybe_extract_prefix(Stream, Buf, Unident, State)
  when byte_size(Buf) < ?TUNNEL_ID_SIZE ->
    {noreply, State#state{unidentified_streams = Unident#{Stream => Buf}}};
maybe_extract_prefix(Stream, Buf, Unident, State) ->
    <<TunnelId:?TUNNEL_ID_SIZE/binary, Rest/binary>> = Buf,
    NewUnident = maps:remove(Stream, Unident),
    State2 = State#state{unidentified_streams = NewUnident},
    match_tunnel(TunnelId, Stream, Rest, State2).

%% Try to match a freshly-identified stream against pending outbound/inbound.
match_tunnel(TunnelId, Stream, Received, State) ->
    match_outbound(maps:take(TunnelId, State#state.pending_outbound),
                   TunnelId, Stream, Received, State).

match_outbound({#pending_tunnel{from = From}, Rest}, TunnelId, Stream, Received, State) ->
    %% Outbound match. Transfer stream ownership to the caller BEFORE
    %% replying so later {quic, ...} messages go to the caller, not to
    %% this gen_server. The bytes read so far, typically the peer's first
    %% dist handshake frame, and those still queued here for the stream go
    %% inside the reply, ahead of everything the stream delivers to the
    %% caller. The stream's other messages queued here follow the reply.
    {CallerPid, _Tag} = From,
    ok = macula_quic:controlling_process(Stream, CallerPid),
    ok = macula_quic:setopt(Stream, active, true),
    {Queued, Events} = take_queued(Stream, Received),
    gen_server:reply(From, {ok, State#state.conn, Stream, Queued}),
    ok = forward(CallerPid, Events),
    Active = (State#state.active_tunnels)#{Stream => TunnelId},
    {noreply, State#state{pending_outbound = Rest, active_tunnels = Active}};
match_outbound(error, TunnelId, Stream, Received, State) ->
    match_inbound(maps:take(TunnelId, State#state.pending_inbound),
                  TunnelId, Stream, Received, State).

%% The bytes queued in this process for `Stream', after `Received', and the
%% stream's other queued {quic, ...} messages, such as its end, oldest first.
take_queued(Stream, Received) ->
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            take_queued(Stream, <<Received/binary, Data/binary>>)
    after 0 ->
        {Received, take_events(Stream, [])}
    end.

take_events(Stream, Events) ->
    receive
        {quic, Event, Stream, _Info} = Message when is_atom(Event) ->
            take_events(Stream, [Message | Events])
    after 0 ->
        lists:reverse(Events)
    end.

forward(Pid, Messages) ->
    lists:foreach(fun(Message) -> Pid ! Message end, Messages).

match_inbound({_Source, Rest}, TunnelId, Stream,
              _Received, #state{kernel_pid = undefined} = State) ->
    ?LOG_WARNING("[dist_relay_client] Inbound tunnel ~s arrived before "
                 "kernel_pid set — dropping stream ~p", [TunnelId, Stream]),
    try macula_quic:close_stream(Stream) catch _:_ -> ok end,
    {noreply, State#state{pending_inbound = Rest}};
match_inbound({_Source, Rest}, TunnelId, Stream, Received,
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
    ok = macula_quic:setopt(Stream, active, true),
    %% Hand over ownership with the bytes read so far, those still queued
    %% here, and the stream's other queued messages. The setup process
    %% passes them on to the dist controller.
    {Queued, Events} = take_queued(Stream, Received),
    SetupPid ! {stream_owned, Stream, Queued, Events},
    Active = (State#state.active_tunnels)#{Stream => TunnelId},
    NewSetups = Setups#{MonRef => TunnelId},
    {noreply, State#state{
        pending_inbound = Rest,
        active_tunnels = Active,
        setups = NewSetups
    }};
match_inbound(error, TunnelId, Stream, Received,
              #state{orphan_streams = Orphans} = State) ->
    %% Neither outbound nor inbound has registered this tunnel yet —
    %% control message is in flight. Stash the stream and retry the
    %% match when tunnel_ok / tunnel_notify arrives.
    ?LOG_INFO("[dist_relay_client] Orphan stream for tunnel ~s — awaiting control",
              [TunnelId]),
    {noreply, State#state{orphan_streams = Orphans#{TunnelId => {Stream, Received}}}}.

%% When a new tunnel_id appears in pending_outbound or pending_inbound,
%% check the orphan_streams buffer and complete the match if there's a
%% waiting stream.
drain_orphan_stream(TunnelId, #state{orphan_streams = Orphans} = State) ->
    dispatch_orphan(maps:take(TunnelId, Orphans), TunnelId, State).

dispatch_orphan({{Stream, Received}, Rest}, TunnelId, State) ->
    State2 = State#state{orphan_streams = Rest},
    {noreply, S3} = match_tunnel(TunnelId, Stream, Received, State2),
    S3;
dispatch_orphan(error, _TunnelId, State) ->
    State.

%% Short-lived process that owns the stream through the net_kernel accept
%% handshake. Exits normally after transferring ownership, and the tunnel's
%% bytes read so far, to the dist controller, or crashes (visible via DOWN
%% in the client) if the handshake fails.
inbound_accept_setup(Kernel, {_Conn, Stream} = Socket, TunnelId) ->
    Owned = wait_for_ownership(Stream, TunnelId),
    Kernel ! {accept, self(), Socket, inet, macula_dist},
    handle_controller_assignment(Kernel, Stream, Owned, TunnelId).

wait_for_ownership(Stream, TunnelId) ->
    receive
        {stream_owned, Stream, Received, Events} -> {Received, Events}
    after 5_000 ->
        exit({setup_timeout, stream_ownership, TunnelId})
    end.

handle_controller_assignment(Kernel, Stream, {Received, Events}, TunnelId) ->
    receive
        {Kernel, controller, DistCtrl} ->
            ok = macula_quic:controlling_process(Stream, DistCtrl),
            ok = macula_quic:setopt(Stream, active, true),
            %% The stream delivered bytes here from the moment the client
            %% handed it over. They follow the client's, and all of them go
            %% to the dist controller inside its controller-ok message,
            %% which it reads before any {quic, ...} message. Without them
            %% the peer's first handshake frame disappears and DistCtrl
            %% waits in recv_name until its setup time runs out.
            {Queued, Later} = take_queued(Stream, Received),
            DistCtrl ! {self(), controller, ok, Queued},
            ok = forward(DistCtrl, Events ++ Later);
        {Kernel, unsupported_protocol} ->
            exit({unsupported_protocol, TunnelId})
    after 30_000 ->
        exit({setup_timeout, controller_assignment, TunnelId})
    end.

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

%% No reconnect: the client ends, and `macula:join_dist_relay/1' starts a
%% new one. Callers learn of it by monitoring `macula:dist_relay_client/0'.
relay_lost(Why, State) ->
    ?LOG_WARNING("[dist_relay_client] Relay control stream ended: ~p", [Why]),
    {stop, {relay_closed, Why}, State}.

%%====================================================================
%% Utilities
%%====================================================================

parse_url(Url) when is_binary(Url) -> parse_url(binary_to_list(Url));
parse_url("quic://" ++ Rest) -> parse_host_port(Rest);
parse_url(Other) -> parse_host_port(Other).

parse_host_port(Str) ->
    case string:tokens(Str, ":") of
        [Host, PortStr] ->
            parse_port(Host, to_port_int(PortStr));
        [Host] ->
            {ok, Host, 4434};
        _ ->
            {error, {invalid_url, Str}}
    end.

to_port_int(PortStr) ->
    try list_to_integer(PortStr) catch _:_ -> error end.

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
        unidentified_streams => maps:size(State#state.unidentified_streams),
        held_control_frames => queue:len(State#state.held_control)
    }.
