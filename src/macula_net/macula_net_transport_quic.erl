%%%-------------------------------------------------------------------
%%% @doc QUIC transport plugin for macula-net.
%%%
%%% Implements {@link macula_net_transport} using the SDK's existing
%%% {@link macula_quic} primitives (Quinn-based Rust NIF). One bidi
%%% stream per outbound connection carries length-prefixed CBOR
%%% envelopes:
%%%
%%% ```
%%%   <<Len:32/big, Cbor:Len/binary>>
%%% ```
%%%
%%% == Phase 1 simplifications ==
%%%
%%% These will be addressed in Phase 4 hardening (PLAN_MACULA_NET.md §13):
%%% <ul>
%%%   <li>Self-signed throwaway TLS cert generated at startup. Identity
%%%       authentication happens at the macula-net envelope layer (sigs
%%%       on control messages), NOT at the TLS layer. Phase 4 swaps to
%%%       raw-pubkey TLS bound to the macula identity.</li>
%%%   <li>One outbound connection per peer; no connection pooling.</li>
%%%   <li>On disconnect, no automatic reconnect.</li>
%%%   <li>No backpressure beyond the QUIC flow-control window.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_transport_quic).

-behaviour(macula_net_transport).
-behaviour(gen_server).

-export([
    start_link/1,
    stop/0,
    set_handler/1,
    connect/3,
    disconnect/1,
    send/2
]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FRAME_HEADER_BYTES, 4).
-define(MAX_FRAME_BYTES, 16#100000).         %% 1 MiB cap
-define(STREAM_OPEN_TIMEOUT_MS, 5000).
-define(CONNECT_TIMEOUT_MS, 5000).

-record(out_link, {
    conn   :: reference(),
    stream :: reference()
}).

-record(in_state, {
    %% Inbound stream framing buffer (per stream).
    buf = <<>> :: binary()
}).

-record(state, {
    listener        :: reference() | undefined,
    listen_port     :: inet:port_number() | undefined,
    cert_path       :: string() | undefined,
    key_path        :: string() | undefined,
    handler         :: macula_net_transport:handler() | undefined,
    %% station_id() => #out_link{}
    out = #{}       :: #{macula_net_transport:station_id() => #out_link{}},
    %% inbound stream ref => #in_state{}
    in_streams = #{}:: #{reference() => #in_state{}}
}).

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Start the QUIC listener.
%%
%% `Opts' map keys:
%% <ul>
%%   <li>`port' — UDP port for the QUIC listener (mandatory)</li>
%%   <li>`bind' — bind address binary (default `<<"::">>')</li>
%%   <li>`alpn' — ALPN protocol id binary (default `<<"macula-net">>')</li>
%% </ul>
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(#{port := _} = Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], []).

-spec stop() -> ok.
stop() ->
    case whereis(?SERVER) of
        undefined -> ok;
        _ -> gen_server:stop(?SERVER)
    end.

%% @doc Register the inbound handler. Called for every received envelope.
%%
%% The handler is invoked as `Handler(Cbor, StreamRef)' where StreamRef
%% is the bidi-stream reference the frame arrived on. Non-host handlers
%% can ignore StreamRef; the host_attach_controller (Phase 3.5) uses it
%% to forward replies on the same stream a daemon dialed in on.
-spec set_handler(macula_net_transport:handler()) -> ok.
set_handler(Handler) when is_function(Handler, 2) ->
    gen_server:call(?SERVER, {set_handler, Handler}).

%% @doc Open an outbound QUIC connection + bidi stream to a peer station.
-spec connect(StationId :: macula_net_transport:station_id(),
              Host :: binary() | string(),
              Port :: inet:port_number()) -> ok | {error, term()}.
connect(StationId, Host, Port) ->
    gen_server:call(?SERVER, {connect, StationId, Host, Port},
                    ?CONNECT_TIMEOUT_MS + 1000).

-spec disconnect(macula_net_transport:station_id()) -> ok.
disconnect(StationId) ->
    gen_server:call(?SERVER, {disconnect, StationId}).

%% @doc Send a CBOR envelope to a known station (must be `connect'ed first).
-spec send(macula_net_transport:station_id(),
           macula_net_transport:cbor_envelope()) -> ok | {error, term()}.
send(StationId, Cbor) ->
    gen_server:call(?SERVER, {send, StationId, Cbor}).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([#{port := Port} = Opts]) ->
    process_flag(trap_exit, true),
    BindAddr = maps:get(bind, Opts, <<"::">>),
    Alpn     = maps:get(alpn, Opts, <<"macula-net">>),
    {ok, {CertPath, KeyPath}} = ensure_self_signed_cert(Port),
    ListenOpts = [
        {cert, CertPath},
        {key,  KeyPath},
        {alpn, [Alpn]},
        {idle_timeout_ms, 120000},
        {keep_alive_interval_ms, 30000}
    ],
    case macula_quic:listen(BindAddr, Port, ListenOpts) of
        {ok, Listener} ->
            ok = macula_quic:async_accept(Listener),
            {ok, #state{listener   = Listener,
                        listen_port = Port,
                        cert_path  = CertPath,
                        key_path   = KeyPath}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({set_handler, Handler}, _From, State) ->
    {reply, ok, State#state{handler = Handler}};

handle_call({connect, StationId, Host, Port}, _From, #state{out = Out} = State) ->
    handle_connect(maps:is_key(StationId, Out), StationId, Host, Port, State);

handle_call({disconnect, StationId}, _From, #state{out = Out} = State) ->
    {Reply, NewState} = drop_out_link(StationId, Out, State),
    {reply, Reply, NewState};

handle_call({send, StationId, Cbor}, _From, #state{out = Out} = State) ->
    {reply, do_send(maps:get(StationId, Out, undefined), Cbor), State};

handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% A new inbound QUIC connection arrived on the listener.
handle_info({quic, new_conn, Conn, _ConnInfo},
            #state{listener = Listener} = State) ->
    %% Hand off control to ourselves; arm the next accept.
    ok = macula_quic:controlling_process(Conn, self()),
    ok = macula_quic:async_accept_stream(Conn),
    ok = macula_quic:async_accept(Listener),
    {noreply, State};

%% A new bidi stream arrived on an inbound connection.
handle_info({quic, new_stream, Stream, _StreamInfo},
            #state{in_streams = InStreams} = State) ->
    ok = macula_quic:controlling_process(Stream, self()),
    ok = macula_quic:setopt(Stream, active, true),
    telemetry:execute([macula, net, transport, stream_opened],
                      #{count => 1}, #{direction => <<"inbound">>}),
    {noreply, State#state{in_streams = InStreams#{Stream => #in_state{}}}};

%% Bytes arrived on an inbound stream. macula_quic delivers data as
%% `{quic, Binary, StreamRef, Flags}' (mirroring quicer's shape), NOT
%% `{quic, data, ...}'. See native/macula_quic/src/message.rs.
handle_info({quic, Data, Stream, _Flags}, State)
  when is_binary(Data), is_reference(Stream) ->
    {noreply, deliver_buffered(Stream, Data, State)};

%% Stream / connection lifecycle: clean up tracking maps.
handle_info({quic, stream_closed, Stream, _Reason},
            #state{in_streams = InStreams} = State) ->
    telemetry:execute([macula, net, transport, stream_closed],
                      #{count => 1}, #{direction => <<"inbound">>}),
    {noreply, State#state{in_streams = maps:remove(Stream, InStreams)}};

handle_info({quic, conn_closed, _Conn, _Reason}, State) ->
    {noreply, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener = L, out = Out, cert_path = CP, key_path = KP}) ->
    case L of
        undefined -> ok;
        _ -> _ = macula_quic:close_listener(L), ok
    end,
    maps:foreach(
        fun(_K, #out_link{conn = Conn}) -> _ = macula_quic:close_connection(Conn), ok end,
        Out),
    %% Clean up the throwaway cert/key files.
    case CP of
        undefined -> ok;
        _ -> _ = file:delete(CP), ok
    end,
    case KP of
        undefined -> ok;
        _ -> _ = file:delete(KP), ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% =============================================================================
%% Connect / disconnect helpers
%% =============================================================================

handle_connect(true, _StationId, _Host, _Port, State) ->
    telemetry:execute([macula, net, transport, connect],
                      #{count => 1}, #{outcome => <<"already_connected">>}),
    {reply, {error, already_connected}, State};
handle_connect(false, StationId, Host, Port, State) ->
    after_quic_connect(macula_quic:connect(to_binary(Host), Port,
                                            default_connect_opts(),
                                            ?CONNECT_TIMEOUT_MS),
                        StationId, State).

after_quic_connect({error, Reason} = E, _StationId, State) ->
    telemetry:execute([macula, net, transport, connect],
                      #{count => 1},
                      #{outcome => connect_outcome(Reason)}),
    {reply, E, State};
after_quic_connect({ok, Conn}, StationId, State) ->
    ok = macula_quic:controlling_process(Conn, self()),
    after_open_stream(macula_quic:open_stream(Conn), Conn, StationId, State).

after_open_stream({error, Reason} = E, Conn, _StationId, State) ->
    _ = macula_quic:close_connection(Conn),
    telemetry:execute([macula, net, transport, connect],
                      #{count => 1},
                      #{outcome => connect_outcome(Reason)}),
    {reply, E, State};
after_open_stream({ok, Stream}, Conn, StationId,
                   #state{out = Out, in_streams = InStreams} = State) ->
    %% Phase 3.5 — activate recv on outbound streams so the host can
    %% send forwarded data back on the same bidi stream the daemon
    %% dialed in on. Phase 1/2 didn't surface this because traffic
    %% only flowed one way (Bob -> Helsinki -> TUN); 3.6 needs the
    %% return path. Track the stream in in_streams so deliver_buffered
    %% has a framing buffer for it.
    ok = macula_quic:setopt(Stream, active, true),
    Link = #out_link{conn = Conn, stream = Stream},
    telemetry:execute([macula, net, transport, connect],
                      #{count => 1}, #{outcome => <<"ok">>}),
    telemetry:execute([macula, net, transport, stream_opened],
                      #{count => 1}, #{direction => <<"outbound">>}),
    {reply, ok, State#state{
        out         = Out#{StationId => Link},
        in_streams  = InStreams#{Stream => #in_state{}}
    }}.

connect_outcome(timeout)              -> <<"timeout">>;
connect_outcome(connection_refused)   -> <<"refused">>;
connect_outcome(R) when is_atom(R)    -> atom_to_binary(R, utf8);
connect_outcome(R) when is_binary(R)  -> R;
connect_outcome(_)                    -> <<"error">>.

drop_out_link(StationId, Out, State) ->
    case maps:take(StationId, Out) of
        {#out_link{conn = Conn, stream = Stream}, NewOut} ->
            _ = macula_quic:close_stream(Stream),
            _ = macula_quic:close_connection(Conn),
            {ok, State#state{out = NewOut}};
        error ->
            {ok, State}
    end.

do_send(undefined, _Cbor) ->
    {error, not_connected};
do_send(#out_link{stream = Stream}, Cbor) ->
    Frame = <<(byte_size(Cbor)):32/big, Cbor/binary>>,
    macula_quic:send(Stream, Frame).

%% =============================================================================
%% Inbound framing
%% =============================================================================

deliver_buffered(Stream, NewBytes, #state{handler = Handler,
                                          in_streams = InStreams} = State) ->
    Existing = maps:get(Stream, InStreams, #in_state{}),
    Combined = <<(Existing#in_state.buf)/binary, NewBytes/binary>>,
    {Frames, Rest} = extract_frames(Combined, []),
    lists:foreach(fun(F) -> safe_invoke(Handler, F, Stream) end, Frames),
    State#state{in_streams = InStreams#{Stream => #in_state{buf = Rest}}}.

extract_frames(<<Len:32/big, Cbor:Len/binary, Rest/binary>>, Acc)
  when Len =< ?MAX_FRAME_BYTES ->
    extract_frames(Rest, [Cbor | Acc]);
extract_frames(<<Len:32/big, _/binary>> = _Bin, Acc) when Len > ?MAX_FRAME_BYTES ->
    %% Oversized frame — drop the whole buffer; recovery is connection
    %% reset territory. Phase 4 hardening.
    {lists:reverse(Acc), <<>>};
extract_frames(Bin, Acc) ->
    {lists:reverse(Acc), Bin}.

safe_invoke(undefined, _Cbor, _Stream) -> ok;
safe_invoke(Handler, Cbor, Stream) when is_function(Handler, 2) ->
    %% Per CLAUDE.md: avoid try/catch where possible. We accept it here
    %% because a user-supplied handler crashing must not take down the
    %% transport. This is the boundary; below it we let it crash.
    try Handler(Cbor, Stream)
    catch _:_ -> ok
    end.

%% =============================================================================
%% Utilities
%% =============================================================================

to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X)   -> list_to_binary(X).

default_connect_opts() ->
    [
        {alpn, [<<"macula-net">>]},
        %% Phase 1: skip server name verification — self-signed certs.
        %% Phase 4 hardening swaps to raw-pubkey TLS pinned to identity.
        {verify, none},
        {idle_timeout_ms, 120000},
        {keep_alive_interval_ms, 30000}
    ].

%% Generate a throwaway Ed25519 keypair + self-signed cert at boot.
%% Phase 4 swaps this for identity-bound certs.
ensure_self_signed_cert(Port) ->
    {Pub, Priv} = ephemeral_ed25519_keypair(),
    Sans = [<<"localhost">>, <<"::1">>, <<"127.0.0.1">>],
    {ok, {CertPem, KeyPem}} = macula_quic:generate_self_signed_cert(Pub, Priv, Sans),
    Tmp = case os:getenv("TMPDIR") of
              false -> "/tmp";
              V -> V
          end,
    %% Include the OS pid in the stem so multiple BEAM nodes sharing
    %% the same /tmp (e.g. several netns on one box) don't race on
    %% writing the cert/key files. Without this, Phase 2's three-netns
    %% demo hits "TLS keys may not be consistent: KeyMismatch".
    Stem = filename:join(Tmp, io_lib:format("macula-net-~p-~s",
                                             [Port, os:getpid()])),
    CertPath = lists:flatten([Stem, ".crt"]),
    KeyPath  = lists:flatten([Stem, ".key"]),
    ok = file:write_file(CertPath, CertPem),
    ok = file:write_file(KeyPath, KeyPem),
    {ok, {CertPath, KeyPath}}.

ephemeral_ed25519_keypair() ->
    %% crypto:generate_key/2 uses the BEAM's OpenSSL; suitable for a
    %% throwaway TLS cert in Phase 1.
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {iolist_to_binary(Pub), iolist_to_binary(Priv)}.
