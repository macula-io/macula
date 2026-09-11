%%%-------------------------------------------------------------------
%%% @doc Macula QUIC transport — Quinn-based Rust NIF.
%%%
%%% Provides QUIC listener, connection, and stream operations backed
%%% by Quinn (Rust). Listeners bind to specific IP addresses,
%%% enabling per-identity IPv6 binding for virtual relay identities.
%%%
%%% Active-mode messages delivered to owning process:
%%%   {quic, Data, StreamRef, Flags}       — stream data
%%%   {quic, new_conn, ConnRef, Info}      — new connection accepted
%%%   {quic, new_stream, StreamRef, Props} — new stream accepted
%%%   {quic, connected, Tag, ConnRef}      a dial from async_connect/4 connected
%%%   {quic, connect_failed, Tag, Reason}  a dial from async_connect/4 failed
%%%   {quic, peer_send_shutdown, StreamRef, undefined}
%%%   {quic, stream_closed, StreamRef, Flags}
%%%     Flags is `{reset, ErrorCode}' when the read failed because
%%%     the peer called `reset_stream/2' on their send side (a
%%%     deliberate, peer-visible abort) — `none' for every other
%%%     read failure (connection loss, zero-RTT rejection, ...).
%%%   {quic, shutdown, Handle, Reason}
%%%   {quic, send_ready, StreamRef, undefined}
%%%     The stream takes data again after async_send/2 answered
%%%     `{error, busy}' to this process; also sent when the stream
%%%     stops taking data, so the retry sees the failure.
%%%   {quic, send_failed, StreamRef, Reason}
%%%     A write on the stream failed; later sends return the error.
%%%     Handle it as a closed stream.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic).

-include_lib("kernel/include/logger.hrl").

-on_load(init/0).

-export([
    %% Listener
    listen/2,
    listen/3,
    async_accept/1,
    async_accept/2,
    close_listener/1,

    %% Connection
    connect/4,
    async_connect/4,
    cancel_connect/1,
    dial_tag/1,
    open_stream/1,
    async_open_stream/1,
    cancel_open_stream/1,
    stream_open_tag/1,

    %% Self-signed cert generation (pubkey-anchored)
    generate_self_signed_cert/3,
    close_connection/1,
    async_accept_stream/1,
    async_accept_stream/2,
    handshake/1,
    peername/1,
    max_datagram_size/1,

    %% Stream
    send/2,
    async_send/2,
    close_stream/1,
    reset_stream/2,
    setopt/3,
    controlling_process/2,

    %% Compat: generic close (tries stream → conn → listener)
    close/1,

    %% Dist compat (stream accept with opts, stream open with opts)
    accept_stream/3,
    open_stream/2,
    handoff_stream/3,

    %% Shutdown (maps to close with flags)
    async_shutdown_stream/3,
    async_shutdown_connection/3,

    %% Stats
    getstat/2
]).

-export_type([dial/0, stream_opening/0]).

%% A dial started by async_connect/4: its result tag and its handle.
-opaque dial() :: {macula_quic_dial, reference(), reference()}.

%% A stream open started by async_open_stream/1: its result tag and its
%% handle.
-opaque stream_opening() :: {macula_quic_stream_opening, reference(), reference()}.

%% The application error code on a stream whose open was cancelled after
%% the peer allowed it.
-define(OPEN_CANCELLED_CODE, 0).

%% Added to a dial's own timeout before connect/4 gives up waiting.
-define(DIAL_RESULT_GRACE_MS, 1_000).

%% A stream's and a connection's receive window, in bytes, unless listen/3
%% sets them.
-define(DEFAULT_STREAM_RECEIVE_WINDOW, 16 * 1024 * 1024).
-define(DEFAULT_RECEIVE_WINDOW, 64 * 1024 * 1024).

%% How long a closed stream's writer may keep writing the data queued before
%% the close, unless the macula application env quic_close_linger_ms is set.
-define(CLOSE_LINGER_MS, 30_000).

%%%===================================================================
%%% NIF Loading
%%%===================================================================

init() ->
    PrivDir = code:priv_dir(macula),
    SoName = filename:join(PrivDir, "macula_quic"),
    case erlang:load_nif(SoName, 0) of
        ok ->
            ?LOG_INFO("[macula_quic] Quinn NIF loaded from ~s", [SoName]),
            ok;
        {error, {reload, _}} ->
            ok;
        {error, Reason} ->
            ?LOG_WARNING("[macula_quic] NIF load failed: ~p (path: ~s)", [Reason, SoName]),
            {error, Reason}
    end.

%%%===================================================================
%%% Listener API
%%%===================================================================

%% @doc Listen on a port or {Address, Port} tuple.
-spec listen(inet:port_number() | {string() | binary(), inet:port_number()}, list()) ->
    {ok, reference()} | {error, term()}.
listen({Address, Port}, Opts) ->
    listen(Address, Port, Opts);
listen(Port, Opts) when is_integer(Port) ->
    listen(<<"::">>, Port, Opts).

%% @doc Listen on a specific bind address and port.
%% BindAddr is a binary: "0.0.0.0", "192.168.1.1", "2600:3c0e::100", etc.
%%
%% `stream_receive_window' and `receive_window' are the credit, in bytes,
%% a peer gets on one stream and across all of a connection's streams
%% before this side reads: 16 MiB and 64 MiB unless set. A value that is
%% not a positive integer returns `{error, {invalid_receive_window, Value}}'.
-spec listen(binary() | string(), inet:port_number(), list()) -> {ok, reference()} | {error, term()}.
listen(BindAddr, Port, Opts) when is_list(BindAddr) ->
    listen(list_to_binary(BindAddr), Port, Opts);
listen(BindAddr, Port, Opts) when is_binary(BindAddr) ->
    CertFile = to_binary(proplists:get_value(cert, Opts)),
    KeyFile = to_binary(proplists:get_value(key, Opts)),
    Alpn = [to_binary(A) || A <- proplists:get_value(alpn, Opts, ["macula"])],
    %% idle_timeout=300s tolerates short snapshot-RPC gaps without
    %% closing the conn; keep_alive=15s sends PING ~10× before any
    %% idle deadline lands. Pre-4.2.1 defaults (120s/30s) were
    %% triggering 50-60s peer_closed cycles on realm MeshSubscriber
    %% clients between snapshot ticks.
    IdleTimeoutMs = proplists:get_value(idle_timeout_ms, Opts, 300_000),
    KeepAliveMs = proplists:get_value(keep_alive_interval_ms, Opts, 15_000),
    BidiStreams = proplists:get_value(peer_bidi_stream_count, Opts, 100),
    UniStreams = proplists:get_value(peer_unidi_stream_count, Opts, 3),
    StreamWindow = proplists:get_value(stream_receive_window, Opts, ?DEFAULT_STREAM_RECEIVE_WINDOW),
    ConnectionWindow = proplists:get_value(receive_window, Opts, ?DEFAULT_RECEIVE_WINDOW),
    ?LOG_INFO("Starting listener on ~s:~p with idle_timeout=~pms, keep_alive=~pms",
              [BindAddr, Port, IdleTimeoutMs, KeepAliveMs]),
    listen_with_windows(receive_window_error([StreamWindow, ConnectionWindow]),
                        [BindAddr, Port, CertFile, KeyFile, Alpn, IdleTimeoutMs,
                         KeepAliveMs, BidiStreams, UniStreams, StreamWindow, ConnectionWindow]).

listen_with_windows(none, NifArgs) ->
    erlang:apply(fun nif_listen/11, NifArgs);
listen_with_windows(Error, _NifArgs) ->
    Error.

receive_window_error([]) ->
    none;
receive_window_error([Window | Rest]) when is_integer(Window), Window > 0 ->
    receive_window_error(Rest);
receive_window_error([Window | _]) ->
    {error, {invalid_receive_window, Window}}.

%% @doc Start accepting connections on a listener.
%% Delivers {quic, new_conn, ConnRef, Info} to the calling process.
-spec async_accept(reference()) -> ok | {error, term()}.
async_accept(Listener) ->
    async_accept(Listener, #{}).

-spec async_accept(reference(), map()) -> ok | {error, term()}.
async_accept(Listener, _Opts) ->
    nif_async_accept(Listener).

%% @doc Close a listener.
-spec close_listener(reference()) -> ok.
close_listener(Listener) ->
    nif_close_listener(Listener).

%%%===================================================================
%%% Connection API
%%%===================================================================

%% @doc Connect to a remote QUIC server. `Host' is a hostname or
%% IP-string; validation depends on `verify' / `verify_pubkey' opts.
%%
%% Trust modes (most to least authenticated):
%% <ul>
%%   <li>`{verify_pubkey, Pin}' — pin the leaf cert's Ed25519 SPKI to
%%       `Pin' (32 bytes). No CA chain. Overrides `verify'.</li>
%%   <li>`{verify, webpki}' — webpki roots + hostname check
%%       (Let's Encrypt-anchored station certs). THE DEFAULT since
%%       5.0.0; before that the default was `none'.</li>
%%   <li>`{verify, none}' — skip all server-cert verification.
%%       Development / self-signed labs only; a network MITM can
%%       impersonate the peer. Must now be opted into explicitly,
%%       and every such dial logs a warning.</li>
%% </ul>
%%
%% The calling process waits for the result, and the dial ends if that
%% process exits while it waits. Use `async_connect/4' to wait elsewhere.
-spec connect(Host, inet:port_number(), list(), timeout()) ->
    {ok, reference()} | {error, term()}
        when Host :: binary() | string().
connect(Host, Port, Opts, Timeout) ->
    %% A reference made here, and matched by every clause of the receive in
    %% await_dial/3, lets that receive skip every message already in this
    %% process's mailbox.
    Tag = make_ref(),
    await_dial(start_dial(Tag, Host, Port, Opts, Timeout), Tag, Timeout).

%% @doc Start a dial and return at once, instead of waiting as `connect/4'
%% does. The calling process owns the dial and later receives
%% `{quic, connected, Tag, ConnRef}' or `{quic, connect_failed, Tag, Reason}',
%% where `Tag' is `dial_tag(Dial)'. The dial ends early when
%% `cancel_connect/1' is called or when its owner exits. `Opts' and
%% `Timeout' are those of `connect/4'; options that cannot be used return
%% `{error, Reason}' at once.
-spec async_connect(Host, inet:port_number(), list(), timeout()) ->
    {ok, dial()} | {error, term()}
        when Host :: binary() | string().
async_connect(Host, Port, Opts, Timeout) ->
    start_dial(make_ref(), Host, Port, Opts, Timeout).

%% @doc End a dial. Afterwards no result for it is in, or will reach, the
%% caller's mailbox: a result sent before the cancel is taken out, and the
%% connection it carried is closed. Call it from the dial's owner.
-spec cancel_connect(dial()) -> ok.
cancel_connect({macula_quic_dial, Tag, Dial}) ->
    discard_dial_result(nif_cancel_connect(Dial), Tag).

%% @doc The reference in this dial's result message.
-spec dial_tag(dial()) -> reference().
dial_tag({macula_quic_dial, Tag, _Dial}) ->
    Tag.

start_dial(Tag, Host, Port, Opts, Timeout) ->
    HostBin = to_binary(Host),
    Alpn = [to_binary(A) || A <- proplists:get_value(alpn, Opts, ["macula"])],
    %% Secure by default: webpki verification unless the caller
    %% explicitly opts out with `{verify, none}' or pins a pubkey.
    Verify = proplists:get_value(verify, Opts, webpki) =/= none,
    %% `verify_pubkey' is a 32-byte Ed25519 pubkey to pin against the
    %% leaf cert SPKI. Empty binary disables pinning. Sovereign
    %% overlay path uses this to validate by pubkey alone (no CA).
    VerifyPubkey = proplists:get_value(verify_pubkey, Opts, <<>>),
    warn_if_unverified(Verify, VerifyPubkey, HostBin, Port),
    %% Mirror the listener defaults: 300s idle + 15s keep-alive.
    %% See the listen/3 doc above for why the previous 60s/20s pair
    %% killed long-lived realm-side station_link clients.
    IdleTimeoutMs = proplists:get_value(idle_timeout_ms, Opts, 300_000),
    KeepAliveMs = proplists:get_value(keep_alive_interval_ms, Opts, 15_000),
    dial_started(nif_async_connect(Tag, HostBin, Port, Alpn, Verify, VerifyPubkey,
                                   IdleTimeoutMs, KeepAliveMs, Timeout),
                 Tag).

dial_started({ok, Dial}, Tag) ->
    {ok, {macula_quic_dial, Tag, Dial}};
dial_started({error, _} = Error, _Tag) ->
    Error.

await_dial({ok, Dial}, Tag, Timeout) ->
    receive
        {quic, connected, Tag, Conn} -> {ok, Conn};
        {quic, connect_failed, Tag, Reason} -> {error, Reason}
    after dial_wait(Timeout) ->
        ok = cancel_connect(Dial),
        {error, <<"connection_timeout">>}
    end;
await_dial({error, _} = Error, _Tag, _Timeout) ->
    Error.

dial_wait(infinity) ->
    infinity;
dial_wait(TimeoutMs) ->
    TimeoutMs + ?DIAL_RESULT_GRACE_MS.

discard_dial_result(cancelled, _Tag) ->
    ok;
discard_dial_result(delivered, Tag) ->
    receive
        {quic, connected, Tag, Conn} ->
            _ = close_connection(Conn),
            ok;
        {quic, connect_failed, Tag, _Reason} ->
            ok
    after 0 ->
        ok
    end.

%% An unverified dial (no webpki, no pubkey pin) accepts any server
%% certificate — a network MITM can impersonate the peer. Legitimate
%% only for development and self-signed lab setups, so make every
%% occurrence visible in the logs.
warn_if_unverified(false, <<>>, Host, Port) ->
    ?LOG_WARNING("[macula_quic] UNVERIFIED dial to ~s:~p — TLS server "
                 "verification disabled ({verify, none}); vulnerable to "
                 "MITM. Use webpki or verify_pubkey outside development.",
                 [Host, Port]);
warn_if_unverified(_, _, _, _) ->
    ok.

%%%===================================================================
%%% Self-signed cert generation
%%%===================================================================

%% @doc Generate a self-signed X.509 cert from an Ed25519 keypair.
%% Returns `{ok, {CertPem, KeyPem}}' as PEM-encoded binaries
%% suitable for handing to `macula_quic:listen/3' via `cert' / `key'
%% opts (after writing to disk). The cert wraps the identity's
%% macula pubkey; no CA chain required. Used by station listeners
%% running pubkey-anchored peering.
-spec generate_self_signed_cert(Pubkey :: binary(),
                                Privkey :: binary(),
                                Sans :: [binary() | string()]) ->
    {ok, {CertPem :: binary(), KeyPem :: binary()}} | {error, term()}.
generate_self_signed_cert(Pubkey, Privkey, Sans)
        when is_binary(Pubkey), byte_size(Pubkey) =:= 32,
             is_binary(Privkey), byte_size(Privkey) =:= 32 ->
    SansCsv = iolist_to_binary(
                  lists:join(<<",">>, [to_binary(S) || S <- Sans])),
    nif_generate_self_signed_cert(Pubkey, Privkey, SansCsv).

%% @doc Open a new bidirectional stream, owned by the calling process.
%%
%% The calling process waits until the peer allows another stream or the
%% connection ends, for as long as that takes, and the open ends if that
%% process exits while it waits. Use `async_open_stream/1' to wait
%% elsewhere.
-spec open_stream(reference()) -> {ok, reference()} | {error, term()}.
open_stream(Conn) ->
    %% A reference made here, and matched by every clause of the receive in
    %% await_stream_open/2, lets that receive skip every message already in
    %% this process's mailbox.
    Tag = make_ref(),
    await_stream_open(start_stream_open(Tag, Conn), Tag).

%% @doc Start opening a bidirectional stream and return at once, instead of
%% waiting as `open_stream/1' does. The calling process owns the open and
%% later receives `{quic, stream_opened, Tag, StreamRef}', and owns that
%% stream, or `{quic, stream_open_failed, Tag, Reason}', where `Tag' is
%% `stream_open_tag(Opening)'. The open waits for as long as the peer allows
%% no further stream, and ends early when `cancel_open_stream/1' is called or
%% when its owner exits. On a closed connection it returns
%% `{error, already_closed}' at once.
-spec async_open_stream(reference()) -> {ok, stream_opening()} | {error, term()}.
async_open_stream(Conn) ->
    start_stream_open(make_ref(), Conn).

%% @doc End a stream open. Afterwards no result for it is in, or will reach,
%% the caller's mailbox: a result sent before the cancel is taken out, and
%% the stream it carried is reset. Call it from the open's owner.
-spec cancel_open_stream(stream_opening()) -> ok.
cancel_open_stream({macula_quic_stream_opening, Tag, Opening}) ->
    discard_stream_open_result(nif_cancel_open_stream(Opening), Tag).

%% @doc The reference in this open's result message.
-spec stream_open_tag(stream_opening()) -> reference().
stream_open_tag({macula_quic_stream_opening, Tag, _Opening}) ->
    Tag.

start_stream_open(Tag, Conn) ->
    stream_open_started(nif_async_open_stream(Conn, Tag), Tag).

stream_open_started({ok, Opening}, Tag) ->
    {ok, {macula_quic_stream_opening, Tag, Opening}};
stream_open_started({error, _} = Error, _Tag) ->
    Error.

%% `Opening' is used in the opened clause, so it stays referenced while this
%% process waits: a collected handle cancels its open.
await_stream_open({ok, Opening}, Tag) ->
    receive
        {quic, stream_opened, Tag, Stream} -> opened_stream(Opening, Stream);
        {quic, stream_open_failed, Tag, Reason} -> {error, Reason}
    end;
await_stream_open({error, _} = Error, _Tag) ->
    Error.

opened_stream({macula_quic_stream_opening, _Tag, _Handle}, Stream) ->
    {ok, Stream}.

discard_stream_open_result(cancelled, _Tag) ->
    ok;
discard_stream_open_result(delivered, Tag) ->
    receive
        {quic, stream_opened, Tag, Stream} ->
            reset_stream(Stream, ?OPEN_CANCELLED_CODE);
        {quic, stream_open_failed, Tag, _Reason} ->
            ok
    after 0 ->
        ok
    end.

%% @doc Close a connection.
-spec close_connection(reference()) -> ok.
close_connection(Conn) ->
    nif_close_connection(Conn).

%% @doc Start accepting streams on a connection.
%% Delivers {quic, new_stream, StreamRef, #{conn => ConnRef}} to the owning process.
-spec async_accept_stream(reference()) -> ok | {error, term()}.
async_accept_stream(Conn) ->
    async_accept_stream(Conn, #{}).

-spec async_accept_stream(reference(), map()) -> ok | {error, term()}.
async_accept_stream(Conn, _Opts) ->
    nif_async_accept_stream(Conn).

%% @doc Complete TLS handshake.
%% With Quinn, handshake completes during accept — this is a no-op for compat.
-spec handshake(reference()) -> ok | {ok, reference()} | {error, term()}.
handshake(Conn) ->
    {ok, Conn}.

%% @doc Get remote address of a connection.
-spec peername(reference()) -> {ok, {string(), inet:port_number()}} | {error, term()}.
peername(Conn) ->
    nif_peername(Conn).

%% @doc Path MTU as discovered by Quinn's DPLPMTUD on this connection.
%% Returns `{ok, Bytes}' once the path MTU has been established;
%% `{error, no_path_mtu}' early in the connection lifecycle (before the
%% first probe lands) or if the peer disabled datagrams. Phase 4.2.
-spec max_datagram_size(reference()) -> {ok, pos_integer()} | {error, term()}.
max_datagram_size(Conn) ->
    nif_max_datagram_size(Conn).

%%%===================================================================
%%% Stream API
%%%===================================================================

%% @doc Send data on a stream, waiting in the calling process until the data
%% is written or the write fails.
%%
%% A stream's writes run in a writer task on the QUIC runtime; the calling
%% process waits in a receive, not in a NIF. It waits for as long as the peer
%% withholds flow-control credit. For a bounded wait, use `async_send/2' with
%% its `busy' result and `send_ready' message, or `reset_stream/2'.
%%
%% Returns `ok'; `{error, already_closed}' after `close_stream/1' or
%% `reset_stream/2'; `{error, reset}' when `reset_stream/2' dropped the data;
%% `{error, closed}' when the stream ended without writing it; or the reason
%% the stream's writes failed.
-spec send(reference(), iodata()) -> ok | {error, term()}.
send(Stream, Data) ->
    %% A reference made here, and matched by the receive in await_sent/2,
    %% lets that receive skip every message already in the mailbox.
    Ref = make_ref(),
    await_sent(nif_send(Stream, iolist_to_binary(Data), Ref), Ref).

await_sent(ok, Ref) ->
    receive
        {quic, sent, Ref, Result} -> Result
    end;
await_sent({error, _} = Error, _Ref) ->
    Error.

%% @doc Queue data on a stream and return at once.
%%
%% Returns `ok' when the data is queued for the stream's writer task. When
%% the stream already has 1 MiB queued, queues nothing and returns
%% `{error, busy}'; the calling process later gets one
%% `{quic, send_ready, Stream, undefined}' message, meaning it may retry. That
%% message also comes when the stream's writes stop meanwhile, and the retry
%% then returns the reason. Returns `{error, already_closed}' after
%% `close_stream/1' or `reset_stream/2', and the reason once the stream's
%% writes have failed.
%%
%% When a write fails, the stream's owner gets one
%% `{quic, send_failed, Stream, Reason}' message.
-spec async_send(reference(), iodata()) -> ok | {error, term()}.
async_send(Stream, Data) ->
    nif_async_send(Stream, iolist_to_binary(Data)).

%% @doc Close a stream's sending side gracefully, and return at once.
%%
%% Data queued before the close is still written, and then a QUIC FIN ends
%% the stream: the peer's `RecvStream::read' resolves `{ok, none}'. When that
%% data cannot be written within the linger bound, the stream is reset with
%% application error code 1, which means the stream closed and its unwritten
%% data was dropped after the linger bound. The bound is the macula
%% application env `quic_close_linger_ms', 30000 by default, read when
%% `close_stream/1' is called. For an immediate, peer-visible abort see
%% `reset_stream/2'.
-spec close_stream(reference()) -> ok.
close_stream(Stream) ->
    nif_close_stream(Stream, application:get_env(macula, quic_close_linger_ms, ?CLOSE_LINGER_MS)).

%% @doc Abruptly reset a stream's send side with `ErrorCode' — a QUIC
%% RESET_STREAM frame, genuinely peer-visible at the transport level:
%% the peer's `RecvStream::read' fails with `{quic, stream_closed,
%% PeerStream, {reset, ErrorCode}}' instead of the clean EOF
%% `close_stream/1' produces. Returns at once: data queued on the stream is
%% dropped, and a `send/2' waiting for its write returns `{error, reset}'.
%% `ErrorCode' must fit a QUIC VarInt (`&lt; 2^62'); out-of-range values
%% answer `{error, error_code_out_of_range}'.
-spec reset_stream(reference(), non_neg_integer()) -> ok | {error, term()}.
reset_stream(Stream, ErrorCode)
  when is_reference(Stream), is_integer(ErrorCode), ErrorCode >= 0 ->
    nif_reset_stream(Stream, ErrorCode).

%% @doc Set active mode on a stream handle.
-spec setopt(reference(), active, boolean()) -> ok | {error, term()}.
setopt(Stream, active, Value) ->
    nif_setopt_active(Stream, Value).

%% @doc Transfer ownership of a handle to another process.
%% Works with both stream and connection handles.
-spec controlling_process(reference(), pid()) -> ok | {error, term()}.
controlling_process(Handle, Pid) ->
    try nif_controlling_process(Handle, Pid)
    catch error:badarg ->
        nif_controlling_process_conn(Handle, Pid)
    end.

%%%===================================================================
%%% Compat API
%%%===================================================================

%% @doc Generic close — tries stream, then connection, then listener.
-spec close(reference()) -> ok.
close(Ref) ->
    close_as(Ref, [fun close_stream/1,
                    fun nif_close_connection/1,
                    fun nif_close_listener/1]).

close_as(_Ref, []) -> ok;
close_as(Ref, [CloseFn | Rest]) ->
    try CloseFn(Ref) of ok -> ok
    catch _:_ -> close_as(Ref, Rest)
    end.

%% @doc Async shutdown stream. `Code' now genuinely reaches the wire
%% via `reset_stream/2' — previously a stub that silently discarded
%% both `Flag' and `Code' and always did a graceful `close_stream/1'.
%% `Flag' is unused (reserved; no caller has ever needed it, kept for
%% signature compatibility).
-spec async_shutdown_stream(reference(), integer(), integer()) -> ok | {error, term()}.
async_shutdown_stream(Stream, _Flag, Code) ->
    reset_stream(Stream, Code).

%% @doc Async shutdown connection.
-spec async_shutdown_connection(reference(), integer(), integer()) -> ok.
async_shutdown_connection(Conn, _Flag, _Code) ->
    nif_close_connection(Conn).

%% @doc Get connection stats. NOT IMPLEMENTED — answers
%% `{error, not_implemented}'.
%%
%% ⚠ This used to answer `{ok, [{S, 0} || S <- Stats]}' — plausible,
%% well-formed, permanently zero — and excuse itself with "zeroed values
%% are harmless (dist_util only uses these for liveness signals)". That
%% is precisely the use a hardcoded zero destroys. A counter that always
%% reads zero makes "nothing is moving" indistinguishable from "nobody
%% implemented the counter", so any liveness check built on it is green
%% forever and its author cannot tell.
%%
%% That is not hypothetical. On 2026-08-13 `station-it-milan' received
%% every packet sent to it, answered none for thirty hours, and every
%% signal derived from the BEAM read healthy. Anyone reaching for a
%% send-side counter to catch that would have found this one, and it
%% would have lied. Failing loudly is the only honest answer until the
%% NIF surfaces the real thing.
%%
%% Quinn HAS the numbers: `quinn::Connection::stats()' carries
%% `udp_tx{datagrams,bytes}', `udp_rx{...}' and
%% `path{rtt,lost_packets,black_holes_detected}', and
%% `nif_max_datagram_size' already calls `stats()' and discards all but
%% `path.current_mtu'. Surfacing the rest is an extension of a working
%% function — see macula-station `plans/PLAN_WIRE_LIVENESS_TRIPWIRE.md'
%% commit 5.
%%
%% The sole consumer, the getstat callback in `macula_dist', already has an
%% `{error, _} -> {ok, 0, 0, 0}' branch, so this changes no behaviour
%% there. It changes what the next caller is told.
-spec getstat(reference(), [atom()]) -> {error, not_implemented}.
getstat(_Conn, _Stats) ->
    {error, not_implemented}.

%%%===================================================================
%%% Dist Compat API
%%%===================================================================

%% @doc Accept stream with options and timeout (for macula_dist).
-spec accept_stream(reference(), map(), timeout()) -> {ok, reference()} | {error, term()}.
accept_stream(Conn, _Opts, _Timeout) ->
    async_accept_stream(Conn).

%% @doc Open stream with options map (for macula_dist).
-spec open_stream(reference(), map()) -> {ok, reference()} | {error, term()}.
open_stream(Conn, _Opts) ->
    open_stream(Conn).

%% @doc Hand off a stream to another process (for macula_dist).
-spec handoff_stream(reference(), pid(), map()) -> ok | {error, term()}.
handoff_stream(Stream, NewOwner, _Opts) ->
    controlling_process(Stream, NewOwner).

%%%===================================================================
%%% NIF Stubs
%%%===================================================================

nif_listen(_BindAddr, _Port, _CertFile, _KeyFile, _Alpn,
           _IdleTimeoutMs, _KeepAliveMs, _BidiStreams, _UniStreams,
           _StreamReceiveWindow, _ReceiveWindow) ->
    erlang:nif_error(nif_not_loaded).

nif_async_accept(_Listener) ->
    erlang:nif_error(nif_not_loaded).

nif_close_listener(_Listener) ->
    erlang:nif_error(nif_not_loaded).

nif_async_connect(_Tag, _Host, _Port, _Alpn, _Verify, _VerifyPubkey,
                  _IdleTimeoutMs, _KeepAliveMs, _TimeoutMs) ->
    erlang:nif_error(nif_not_loaded).

nif_cancel_connect(_Dial) ->
    erlang:nif_error(nif_not_loaded).

nif_generate_self_signed_cert(_Pubkey, _Privkey, _Sans) ->
    erlang:nif_error(nif_not_loaded).

nif_async_open_stream(_Conn, _Tag) ->
    erlang:nif_error(nif_not_loaded).

nif_cancel_open_stream(_Opening) ->
    erlang:nif_error(nif_not_loaded).

nif_close_connection(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_async_accept_stream(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_peername(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_max_datagram_size(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_send(_Stream, _Data, _Ref) ->
    erlang:nif_error(nif_not_loaded).

nif_async_send(_Stream, _Data) ->
    erlang:nif_error(nif_not_loaded).

nif_close_stream(_Stream, _LingerMs) ->
    erlang:nif_error(nif_not_loaded).

nif_reset_stream(_Stream, _ErrorCode) ->
    erlang:nif_error(nif_not_loaded).

nif_setopt_active(_Stream, _Value) ->
    erlang:nif_error(nif_not_loaded).

nif_controlling_process(_Handle, _Pid) ->
    erlang:nif_error(nif_not_loaded).

nif_controlling_process_conn(_Handle, _Pid) ->
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% Internal
%%%===================================================================

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A).
