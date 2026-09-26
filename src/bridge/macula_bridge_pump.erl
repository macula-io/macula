%%%-------------------------------------------------------------------
%%% @private
%%% @doc One bridged TCP connection: its socket on one side, its bidi mesh
%%% stream on the other, bytes pumped both ways.
%%%
%%% Every stream chunk starts with one tag byte:
%%% <ul>
%%%   <li>`0' then TCP bytes;</li>
%%%   <li>`1' then a 32-bit count of bytes the other side may send more
%%%       (credit);</li>
%%%   <li>`2' alone: the other side's application finished sending (its FIN).</li>
%%% </ul>
%%%
%%% Credit is the receiver's to grant, and counts what a chunk costs the
%%% receiver's stream to keep unread: its bytes, its tag and `?CHUNK_COST'
%%% for the memory that holds it (the stream counts about a hundred bytes a
%%% chunk), so a stall on many small chunks costs no more than on a few big
%%% ones. A side starts able to send `?INITIAL_CREDIT'; at the start the
%%% receiver grants the rest of its receive window, and then gives back every
%%% `?GRANT_EVERY' it has written to its socket. A side reads its socket only
%%% while it holds credit for one more chunk, one read at a time of at most
%%% one chunk and at most what the credit left pays for (the socket's
%%% `buffer', set before each read), so what is kept unread towards a
%%% receiver never exceeds its window. The receive window is `window_bytes';
%%% the serving end's is also held to one served session's share of its
%%% caller's budget for unread bytes (`macula_stream_sessions:session_share/0'),
%%% so one stalled session never has another refused. Credit past any window
%%% a receiver may have is malformed.
%%%
%%% The stream is read one chunk at a time: the reader takes the next chunk
%%% only once this loop has handled the last. A peer that sends without
%%% credit therefore fills the stream's own inbox, whose bound ends the
%%% session, and never this process's memory.
%%%
%%% A TCP FIN on one side travels in band (tag `2') and becomes a TCP write
%%% shutdown on the other; the stream stays open both ways meanwhile, so the
%%% half-closed side keeps crediting the answer it reads. The end of the
%%% other side's stream (a plain stream client's `close_send') is taken the
%%% same way. The connection ends, and the stream is closed, when both
%%% directions have finished.
%%%
%%% A TCP reset is not an end of input: it aborts the stream, and an aborted
%%% stream resets the socket on the other side (a close with zero linger),
%%% so neither application takes a cut-off exchange for a complete one. A
%%% stream the provider refuses (an unadmitted caller) closes the local
%%% connection at once and is logged by the refusal's name.
%%%
%%% `idle_ms' (default infinity, as TCP itself) ends a connection with no
%%% traffic either way. `write_timeout_ms' (default infinity) ends one whose
%%% socket has not taken a write for that long: a blocked write otherwise
%%% holds this loop, deaf to the stream's end, for as long as the
%%% application does not read.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge_pump).

-export([serve/4, connect/3, socket_opts/1]).

-define(DATA, 0).
-define(CREDIT, 1).
-define(FIN, 2).
-define(INITIAL_CREDIT, 64 * 1024).
%% A chunk's cost beyond its bytes: the tag byte, and the heap words a
%% stream's inbox holds it in (at most 104 bytes on a 64-bit VM, measured).
-define(CHUNK_COST, 129).
-define(GRANT_EVERY, 32 * 1024).
-define(MAX_CREDIT, 8 * 1024 * 1024).
-define(DEFAULT_CONNECT_TIMEOUT_MS, 5_000).

%% @doc The serving end: connect to `Target' and pump `Stream' through it.
%% Runs in the stream handler's own process, which owns the stream.
-spec serve(pid(), macula_bridge:target(), binary() | undefined, map()) -> ok.
serve(Stream, {Host, Port}, Caller, Opts) ->
    served(gen_tcp:connect(Host, Port, [{active, false} | socket_opts(Opts)],
                           maps:get(connect_timeout_ms, Opts, ?DEFAULT_CONNECT_TIMEOUT_MS)),
           Stream, {Host, Port}, Caller, Opts).

served({ok, Sock}, Stream, Target, Caller, Opts) ->
    logger:info("[macula_bridge] ~ts connected to ~0p", [caller_name(Caller), Target]),
    run(Stream, Sock, Opts, #{side => serve, caller => Caller});
served({error, Reason}, Stream, {Host, Port}, Caller, _Opts) ->
    logger:warning("[macula_bridge] the bridged service ~0p:~b does not answer ~ts: ~0p",
                   [Host, Port, caller_name(Caller), Reason]),
    _ = macula_stream:abort(Stream, <<"unavailable">>, <<"the bridged service does not answer">>),
    ok.

%% @doc The listening end: open a stream with `Open', given the call options
%% this connection's stream takes, and pump `Sock' through it. Runs in the
%% connection's own process, which owns the stream.
-spec connect(gen_tcp:socket(), macula_bridge:opener(), map()) -> ok.
connect(Sock, Open, Opts) ->
    CallOpts = (maps:with([ucan_token, dial_timeout_ms], Opts))#{mode => bidi, owner => self()},
    opened(Open(CallOpts), Sock, Opts).

opened({ok, Stream}, Sock, Opts) ->
    run(Stream, Sock, Opts, #{side => listen, caller => undefined});
opened({error, Reason}, Sock, _Opts) ->
    logger:warning("[macula_bridge] no stream for a local connection: ~0p", [Reason]),
    gen_tcp:close(Sock).

%% @doc The socket options both ends' sockets take: binary, no Nagle delay,
%% a reset reported as one, and reads of at most one chunk. `Opts' are the
%% resolved options (`macula_bridge' gives every pump its `chunk_bytes' and
%% `receive_window').
-spec socket_opts(map()) -> [gen_tcp:option()].
socket_opts(#{chunk_bytes := Chunk}) ->
    [binary, {exit_on_close, false}, {nodelay, true}, {show_econnreset, true}, {buffer, Chunk}].

caller_name(<<Prefix:8/binary, _/binary>>) -> ["caller ", binary:encode_hex(Prefix, lowercase)];
caller_name(_Unknown) -> "an unnamed caller".

%%--------------------------------------------------------------------
%% The pump
%%--------------------------------------------------------------------

run(Stream, Sock, #{receive_window := Window, chunk_bytes := Chunk} = Opts, Who) ->
    Pump = self(),
    Reader = spawn_link(fun() -> read(Stream, Pump) end),
    ok = sent_ok(macula_stream:send(Stream, <<?CREDIT, (Window - ?INITIAL_CREDIT):32>>)),
    ok = inet:setopts(Sock, write_timeout(maps:get(write_timeout_ms, Opts, infinity))),
    loop(rearmed(#{stream => Stream, sock => Sock, reader => Reader, who => Who,
                   chunk => Chunk,
                   idle => maps:get(idle_ms, Opts, infinity),
                   credit => ?INITIAL_CREDIT, received => 0, paused => false,
                   local_done => false, remote_done => false})).

write_timeout(infinity) -> [];
write_timeout(Ms) -> [{send_timeout, Ms}, {send_timeout_close, true}].

loop(#{local_done := true, remote_done := true, stream := Stream} = P) ->
    ok = macula_stream:close(Stream),
    finish(P);
loop(#{sock := Sock, idle := Idle} = P) ->
    receive
        {tcp, Sock, Bytes} -> loop(sent(Bytes, P));
        {tcp_closed, Sock} -> loop(local_finished(P));
        {tcp_error, Sock, Reason} -> fail(<<"transport">>, io_lib:format("~0p", [Reason]), P);
        {bridge_in, Chunk} -> in(Chunk, P);
        bridge_malformed -> fail(<<"malformed">>, <<"a stream chunk that is not bridge bytes">>, P);
        bridge_eof -> loop(remote_finished(P));
        {bridge_error, Reason} -> ended(Reason, P);
        _Other -> loop(P)
    after Idle ->
        fail(<<"idle">>, <<"no traffic within idle_ms">>, P)
    end.

%% TCP bytes out, in chunks of at most `chunk'; the socket is read again only
%% while credit lasts.
sent(Bytes, #{stream := Stream, chunk := Chunk, credit := Credit} = P) ->
    Chunks = send_chunks(Stream, Bytes, Chunk, 0),
    rearmed(P#{credit => Credit - byte_size(Bytes) - Chunks * ?CHUNK_COST}).

%% Sends `Bytes' in chunks of at most `Chunk', and says how many.
send_chunks(_Stream, <<>>, _Chunk, Sent) ->
    Sent;
send_chunks(Stream, Bytes, Chunk, Sent) when byte_size(Bytes) =< Chunk ->
    ok = sent_ok(macula_stream:send(Stream, <<?DATA, Bytes/binary>>)),
    Sent + 1;
send_chunks(Stream, Bytes, Chunk, Sent) ->
    <<Part:Chunk/binary, Rest/binary>> = Bytes,
    ok = sent_ok(macula_stream:send(Stream, <<?DATA, Part/binary>>)),
    send_chunks(Stream, Rest, Chunk, Sent + 1).

%% A send on a stream that has already ended is not this loop's failure: the
%% reader reports the end, and the loop ends on it. Any other refusal is a
%% fault, and crashes the pump (which ends the stream and the socket).
sent_ok(ok) -> ok;
sent_ok({error, Ended}) when Ended =:= closed; Ended =:= send_closed -> ok.

rearmed(#{local_done := true} = P) ->
    P;
rearmed(#{credit := Credit, chunk := Chunk, sock := Sock} = P) when Credit > ?CHUNK_COST ->
    ok = inet:setopts(Sock, [{buffer, min(Chunk, Credit - ?CHUNK_COST)}, {active, once}]),
    P#{paused => false};
rearmed(P) ->
    P#{paused => true}.

%% A chunk from the other side: bytes for the socket, credit, or the end of
%% what the other side's application sends. The reader is asked for the next
%% chunk only once this one is handled.
in(<<?DATA, _/binary>>, #{remote_done := true} = P) ->
    fail(<<"malformed">>, <<"bridge bytes after the end of the other side's input">>, P);
in(<<?DATA, Bytes/binary>>, #{sock := Sock} = P) ->
    written(gen_tcp:send(Sock, Bytes), byte_size(Bytes) + ?CHUNK_COST, P);
in(<<?CREDIT, N:32>>, #{credit := Credit} = P) when Credit + N > ?MAX_CREDIT ->
    fail(<<"malformed">>, <<"credit past any window">>, P);
in(<<?CREDIT, N:32>>, #{credit := Credit, paused := Paused} = P) ->
    loop(pulled(credited(Paused, P#{credit => Credit + N})));
in(<<?FIN>>, P) ->
    loop(pulled(remote_finished(P)));
in(_Unknown, P) ->
    fail(<<"malformed">>, <<"a bridge chunk with an unknown tag">>, P).

credited(true, P) -> rearmed(P);
credited(false, P) -> P.

pulled(#{reader := Reader} = P) ->
    Reader ! more,
    P.

written(ok, Cost, #{received := Received, stream := Stream} = P) ->
    Total = Received + Cost,
    loop(pulled(granted(Total >= ?GRANT_EVERY, Total, Stream, P)));
written({error, Reason}, _Cost, P) ->
    fail(<<"transport">>, io_lib:format("~0p", [Reason]), P).

granted(true, Total, Stream, P) ->
    ok = sent_ok(macula_stream:send(Stream, <<?CREDIT, Total:32>>)),
    P#{received => 0};
granted(false, Total, _Stream, P) ->
    P#{received => Total}.

%% The local application finished sending: so does this side, in band, and
%% the stream stays open for the credit the answer needs.
local_finished(#{stream := Stream} = P) ->
    ok = sent_ok(macula_stream:send(Stream, <<?FIN>>)),
    P#{local_done => true}.

%% The other side finished sending: so does this side of the socket.
remote_finished(#{sock := Sock} = P) ->
    _ = gen_tcp:shutdown(Sock, write),
    P#{remote_done => true}.

%% The stream ended in error: refused at open (logged on the listening side,
%% by the refusal's name, so an unadmitted caller is visible), or aborted.
%% Either way the exchange was cut off, and the socket is reset.
ended({Code, Message}, #{who := #{side := listen}} = P) when is_binary(Code) ->
    logger:warning("[macula_bridge] the provider ended the connection: ~ts (~ts)", [Code, Message]),
    reset(P);
ended(Reason, #{who := #{side := Side, caller := Caller}} = P) ->
    logger:info("[macula_bridge] a ~ts connection's stream (~ts) ended: ~0p", [Side, caller_name(Caller), Reason]),
    reset(P).

fail(Code, Message, #{stream := Stream} = P) ->
    _ = macula_stream:abort(Stream, Code, iolist_to_binary(Message)),
    reset(P).

reset(#{sock := Sock} = P) ->
    _ = inet:setopts(Sock, [{linger, {true, 0}}]),
    finish(P).

finish(#{sock := Sock, reader := Reader}) ->
    unlink(Reader),
    exit(Reader, shutdown),
    _ = gen_tcp:close(Sock),
    ok.

%%--------------------------------------------------------------------
%% The reader
%%--------------------------------------------------------------------

%% One chunk at a time: the next `recv' waits for the pump's `more'.
read(Stream, Pump) ->
    forwarded(macula_stream:recv(Stream, infinity), Stream, Pump).

forwarded({chunk, Chunk}, Stream, Pump) ->
    Pump ! {bridge_in, Chunk},
    receive more -> read(Stream, Pump) end;
forwarded({data, _Term}, _Stream, Pump) ->
    Pump ! bridge_malformed;
forwarded(eof, _Stream, Pump) ->
    Pump ! bridge_eof;
forwarded({error, Reason}, _Stream, Pump) ->
    Pump ! {bridge_error, Reason}.
