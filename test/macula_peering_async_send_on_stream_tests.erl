%%%-------------------------------------------------------------------
%%% @doc macula_peering:async_send_on_stream/2,3, driven over a loopback QUIC
%%% stream whose reader can stop reading.
%%%
%%% The variant never waits in the calling process, whatever the reader does.
%%% When the stream's send queue is full it returns {error, busy} and queues
%%% nothing, and the calling process later gets {quic, send_ready, Stream, _}.
%%% Tagged bytes it accepted end with exactly one notice to the calling
%%% process: {quic, send_complete, Stream, Tag} once they are written, or
%%% {quic, send_incomplete, Stream, {Tag, Reason}} when the stream is reset or
%%% fails first, never both. A caller that is not the stream's owner gets
%%% these messages, and the owner gets none of them. The bytes arrive as they
%%% were given, in order, as a relay passes a frame on. A relay writes only
%%% what macula_frame:parse_for_relay/2 accepted: relay_on_stream/2 and
%%% async_relay_on_stream/2,3 take its units, never bytes.
%%%
%%% Senders run in monitored processes, so a sender that fails fails its own
%%% test with the reason, and the other tests still run.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_async_send_on_stream_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KIB, 1024).
%% The listener's stream receive window: a reader that stops reading stops
%% crediting the sender after this much.
-define(WINDOW, (64 * ?KIB)).
%% Parenthesized: without them `X div ?BLOCK_BYTES' reads as `(X div 32) * 1024'.
-define(BLOCK_BYTES, (32 * ?KIB)).
-define(EVENT_MS, 5_000).
-define(PROBE_MS, 100).
-define(RACE_ROUNDS, 20).

async_send_on_stream_test_() ->
    {timeout, 180,
     {setup,
      fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
      fun(ok) -> ok end,
      [{"a caller whose reader stopped keeps answering within 100 ms, gets busy, then send_ready",
        {timeout, 30, fun a_stopped_reader_never_holds_the_caller/0}},
       {"each tagged block gets one send_complete, and only once its bytes are written",
        {timeout, 30, fun each_tagged_block_completes_once_written/0}},
       {"a reset with tagged blocks queued gives each exactly one notice, and a non-owner caller gets them",
        {timeout, 30, fun a_reset_gives_each_queued_tag_one_notice/0}},
       {"a reset racing a tagged write gives exactly one notice for that tag",
        {timeout, 60, fun a_reset_racing_a_write_gives_one_notice/0}},
       {"the bytes arrive as they were given, in order",
        {timeout, 30, fun the_bytes_arrive_as_given/0}},
       {"a relay writes only the frames the reader accepted, as the bytes received",
        {timeout, 30, fun a_relay_writes_only_the_frames_the_reader_accepted/0}}]}}.

%%%===================================================================
%%% Scenarios
%%%===================================================================

%% A sender process, not the stream's owner, sends until busy while this
%% process probes it. Once the reader reads again, the sender gets send_ready,
%% and every block it had queued arrives. The owner gets no send_ready.
a_stopped_reader_never_holds_the_caller() ->
    with_pair(fun(#{client_stream := Stream, server_stream := ServerStream}) ->
        Test = self(),
        Sender = spawn_monitor(fun() -> send_until_busy(Test, Stream, 0) end),
        {Accepted, Latencies} = probe_until_busy(Sender, []),
        AfterBusy = probe(Sender),
        Arrived = blocks_read(ServerStream, Accepted),
        Ready = from_sender(Sender, fun({ready, _Pid}) -> ready end),
        OwnerGot = receive {quic, send_ready, Stream, _} -> send_ready after 0 -> nothing end,
        ?assertEqual({true, true, ready, Accepted, nothing},
                     {Accepted > 0, lists:all(fun within_probe_limit/1, [AfterBusy | Latencies]),
                      Ready, Arrived, OwnerGot})
    end).

%% With the reader stopped, most tagged blocks stay queued and get no notice;
%% once it reads, every tag gets exactly one send_complete.
each_tagged_block_completes_once_written() ->
    with_pair(fun(#{client_stream := Stream, server_stream := ServerStream}) ->
        Accepted = tagged_until_busy(Stream, 1),
        Early = notices(Stream, 300),
        ok = macula_quic:setopt(ServerStream, active, true),
        Late = notices_until(Stream, Accepted - length(Early)),
        All = Early ++ Late,
        ?assertEqual({true, lists:seq(1, Accepted), []},
                     {length(Early) < Accepted,
                      lists:sort([Tag || {complete, Tag} <- All]),
                      [N || {incomplete, _, _} = N <- All]})
    end).

%% A sender process, not the owner, queues tagged blocks to a stopped reader
%% and resets the stream. Every accepted tag gets exactly one notice, the
%% queued ones send_incomplete with reason reset, and the owner gets none.
a_reset_gives_each_queued_tag_one_notice() ->
    with_pair(fun(#{client_stream := Stream}) ->
        Test = self(),
        Sender = spawn_monitor(fun() ->
                     Accepted = tagged_until_busy(Stream, 1),
                     ok = macula_quic:reset_stream(Stream, 7),
                     Test ! {reset_notices, self(), Accepted, notices(Stream, 1_000)}
                 end),
        {Accepted, Notices} = from_sender(Sender, fun({reset_notices, _Pid, A, N}) -> {A, N} end),
        OwnerGot = notices(Stream, 0),
        Incomplete = [Tag || {incomplete, Tag, reset} <- Notices],
        ?assertEqual({lists:seq(1, Accepted), true, true, []},
                     {lists:sort([tag(N) || N <- Notices]),
                      Incomplete =/= [],
                      length(Notices) =:= length(lists:usort([tag(N) || N <- Notices])),
                      OwnerGot})
    end).

%% A reading peer and a 256 KiB tagged block, reset at once or after a short
%% delay: its one notice is send_complete when its bytes were written and
%% send_incomplete otherwise, and never both.
a_reset_racing_a_write_gives_one_notice() ->
    Counts = [race_round(Round rem 4) || Round <- lists:seq(1, ?RACE_ROUNDS)],
    ?assertEqual(lists:duplicate(?RACE_ROUNDS, 1), Counts).

%% Numbered blocks, some tagged and some not, to a reading peer: the peer reads
%% exactly their bytes, in the order they were given.
the_bytes_arrive_as_given() ->
    with_pair(fun(#{client_stream := Stream, server_stream := ServerStream}) ->
        ok = macula_quic:setopt(ServerStream, active, true),
        Blocks = [block(N) || N <- lists:seq(1, 8)],
        Sent = [send_block(Stream, N, Block) || {N, Block} <- lists:zip(lists:seq(1, 8), Blocks)],
        Expected = iolist_to_binary([<<"open">> | Blocks]),
        Read = read_bytes(ServerStream, byte_size(Expected), <<>>,
                          erlang:monotonic_time(millisecond) + ?EVENT_MS),
        ?assertEqual({lists:duplicate(8, ok), Expected}, {Sent, Read})
    end).

%% A relay parses what it received with the reader and writes only the units
%% the reader gave it: the far side reads exactly the bytes of the frames that
%% passed, in order, and never the bytes of the frame the reader refused. The
%% refusal is named once, and a raw binary is not written.
a_relay_writes_only_the_frames_the_reader_accepted() ->
    with_pair(fun(#{client_stream := Stream, server_stream := ServerStream}) ->
        ok = macula_quic:setopt(ServerStream, active, true),
        [Good, Refused, Other] = relay_samples(),
        {ok, Items, <<>>} = macula_frame:parse_for_relay(<<Good/binary, Refused/binary, Other/binary>>, ?WINDOW),
        [First, Second] = [Unit || {Frame, Unit} <- Items, is_map(Frame)],
        ok = macula_peering:relay_on_stream(Stream, First),
        ok = macula_peering:async_relay_on_stream(Stream, Second, relayed),
        Expected = <<"open", Good/binary, Other/binary>>,
        Read = read_bytes(ServerStream, byte_size(Expected), <<>>, erlang:monotonic_time(millisecond) + ?EVENT_MS),
        Later = read_bytes(ServerStream, 1, <<>>, erlang:monotonic_time(millisecond) + 200),
        ?assertEqual({[refused], Expected, <<>>, [{complete, relayed}]},
                     {[refused || {refused, _} <- Items], Read, Later, notices_until(Stream, 1)}),
        ?assertError(function_clause, macula_peering:relay_on_stream(Stream, Good))
    end).

%% Three frames as their bytes: a provider's stream frame, a frame whose type
%% refuses its fields (the same frame without its signed object), and a
%% caller's stream frame.
relay_samples() ->
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key end,
    [Caller, Provider] = [Generate(), Generate()],
    Spec = #{request_id => <<7:128>>, realm => <<1:256>>, procedure => <<"acme/count_v1">>,
             target => macula_node_keys:key_id(Provider), deadline => 1789000600000, payload => #{}, mode => bidi},
    {ok, OpenFrame, <<>>} = macula_frame:decode(macula_frame:encode(macula_frame:stream_open(Spec, Caller))),
    {ok, Open} = macula_frame:verify_request(OpenFrame, pq_pure),
    Chunk = #{frame_type => stream_data, seq => 0, encoding => raw, body => <<"chunk">>},
    [macula_frame:encode(macula_frame:provider_stream(Chunk, Provider, Open)),
     macula_frame:encode(maps:remove(stream, macula_frame:provider_stream(Chunk#{seq => 1}, Provider, Open))),
     macula_frame:encode(macula_frame:caller_stream(Chunk, Caller, Open))].

%%%===================================================================
%%% Senders
%%%===================================================================

send_until_busy(Test, Stream, Accepted) ->
    answer_probes(),
    sent(macula_peering:async_send_on_stream(Stream, block(Accepted + 1)), Test, Stream, Accepted).

sent(ok, Test, Stream, Accepted) ->
    send_until_busy(Test, Stream, Accepted + 1);
sent({error, busy}, Test, Stream, Accepted) ->
    Test ! {busy, self(), Accepted},
    await_ready(Test, Stream).

await_ready(Test, Stream) ->
    receive
        {probe, From} ->
            From ! {probe_answer, self()},
            await_ready(Test, Stream);
        {quic, send_ready, Stream, _} ->
            Test ! {ready, self()}
    end.

answer_probes() ->
    receive
        {probe, From} ->
            From ! {probe_answer, self()},
            answer_probes()
    after 0 ->
        ok
    end.

%% Sends blocks tagged Tag, Tag + 1, ... until busy; returns how many were
%% accepted.
tagged_until_busy(Stream, Tag) ->
    tagged_sent(macula_peering:async_send_on_stream(Stream, block(Tag), Tag), Stream, Tag).

tagged_sent(ok, Stream, Tag) ->
    tagged_until_busy(Stream, Tag + 1);
tagged_sent({error, busy}, _Stream, Tag) ->
    Tag - 1.

%% Odd blocks go untagged, even blocks tagged with their number.
send_block(Stream, N, Block) when N rem 2 =:= 1 ->
    macula_peering:async_send_on_stream(Stream, Block);
send_block(Stream, N, Block) ->
    macula_peering:async_send_on_stream(Stream, Block, N).

race_round(DelayMs) ->
    with_pair(fun(#{client_stream := Stream, server_stream := ServerStream}) ->
        ok = macula_quic:setopt(ServerStream, active, true),
        Block = binary:copy(<<0>>, 256 * ?KIB),
        ok = macula_peering:async_send_on_stream(Stream, Block, race),
        timer:sleep(DelayMs),
        _ = macula_quic:reset_stream(Stream, 7),
        length(notices(Stream, 1_000))
    end).

%% The next message from the monitored Sender that Take accepts, taken apart
%% by Take; fails with the sender's exit reason when it ends first.
from_sender({Pid, Mon}, Take) ->
    receive
        {ready, Pid} = Msg -> Take(Msg);
        {reset_notices, Pid, _, _} = Msg -> Take(Msg);
        {'DOWN', Mon, process, Pid, Reason} -> error({sender_ended, Reason})
    after ?EVENT_MS ->
        error(no_message_from_sender)
    end.

%%%===================================================================
%%% Probes and notices
%%%===================================================================

probe_until_busy({Pid, Mon} = Sender, Latencies) ->
    receive
        {busy, Pid, Accepted} -> {Accepted, Latencies};
        {'DOWN', Mon, process, Pid, Reason} -> error({sender_ended, Reason})
    after 0 ->
        probe_until_busy(Sender, [probe(Sender) | Latencies])
    end.

%% How long Sender took to answer a probe, in milliseconds.
probe({Pid, Mon}) ->
    Started = erlang:monotonic_time(millisecond),
    Pid ! {probe, self()},
    receive
        {probe_answer, Pid} -> erlang:monotonic_time(millisecond) - Started;
        {'DOWN', Mon, process, Pid, Reason} -> error({sender_ended, Reason})
    after ?EVENT_MS ->
        no_answer
    end.

within_probe_limit(Ms) when is_integer(Ms) -> Ms =< ?PROBE_MS;
within_probe_limit(_NoAnswer) -> false.

%% The tag notices for Stream that arrive within Ms, in arrival order.
notices(Stream, Ms) ->
    collect_notices(Stream, erlang:monotonic_time(millisecond) + Ms, infinity, []).

%% The next Count tag notices for Stream, or those that came within EVENT_MS.
notices_until(Stream, Count) ->
    collect_notices(Stream, erlang:monotonic_time(millisecond) + ?EVENT_MS, Count, []).

collect_notices(_Stream, _Deadline, 0, Acc) ->
    lists:reverse(Acc);
collect_notices(Stream, Deadline, Left, Acc) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {quic, send_complete, Stream, Tag} ->
            collect_notices(Stream, Deadline, fewer(Left), [{complete, Tag} | Acc]);
        {quic, send_incomplete, Stream, {Tag, Reason}} ->
            collect_notices(Stream, Deadline, fewer(Left), [{incomplete, Tag, Reason} | Acc])
    after Wait ->
        lists:reverse(Acc)
    end.

fewer(infinity) -> infinity;
fewer(N) -> N - 1.

tag({complete, Tag}) -> Tag;
tag({incomplete, Tag, _Reason}) -> Tag.

%% How many whole blocks arrive on ServerStream once it reads again, reading
%% until Expected blocks' bytes are in or EVENT_MS passes. The stream's first
%% four bytes are the "open" that made the listener side accept it.
blocks_read(ServerStream, Expected) ->
    ok = macula_quic:setopt(ServerStream, active, true),
    <<"open", Blocks/binary>> = read_bytes(ServerStream, 4 + Expected * ?BLOCK_BYTES, <<>>,
                                           erlang:monotonic_time(millisecond) + ?EVENT_MS),
    byte_size(Blocks) div ?BLOCK_BYTES.

read_bytes(_Stream, Wanted, Acc, _Deadline) when byte_size(Acc) >= Wanted ->
    Acc;
read_bytes(Stream, Wanted, Acc, Deadline) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {quic, Bin, Stream, _Flags} when is_binary(Bin) ->
            read_bytes(Stream, Wanted, <<Acc/binary, Bin/binary>>, Deadline)
    after Wait ->
        Acc
    end.

%% A numbered block of BLOCK_BYTES.
block(N) ->
    <<N:32/big, (binary:copy(<<0>>, ?BLOCK_BYTES - 4))/binary>>.

%%%===================================================================
%%% Pair
%%%===================================================================

%% Runs Fun with a loopback listener whose stream receive window is WINDOW,
%% a client connection to it, and one stream the client opened and this
%% process owns, whose listener side this process accepted and does not read.
with_pair(Fun) ->
    Pair = pair(),
    try
        Fun(Pair)
    after
        stop_pair(Pair)
    end.

pair() ->
    Port = free_udp_port(),
    {PubBin, {ok, Listener}} = macula_test_tmp:with_dir("macula-async-send-on-stream",
                                                        fun(Dir) -> windowed_listener(Dir, Port) end),
    ok = macula_quic:async_accept(Listener),
    {ok, ClientConn} = macula_quic:connect(<<"127.0.0.1">>, Port,
                                            [{verify_pubkey, PubBin}, {alpn, [<<"macula">>]}],
                                            ?EVENT_MS),
    ServerConn = receive {quic, new_conn, C, _Info} -> C after ?EVENT_MS -> error(no_server_connection) end,
    ok = macula_quic:async_accept_stream(ServerConn),
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    ok = macula_quic:send(ClientStream, <<"open">>),
    ServerStream = receive {quic, new_stream, S, _Props} -> S after ?EVENT_MS -> error(no_server_stream) end,
    #{listener => Listener, client_conn => ClientConn, server_conn => ServerConn,
      client_stream => ClientStream, server_stream => ServerStream}.

windowed_listener(Dir, Port) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    PubBin = iolist_to_binary(Pub),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(PubBin, iolist_to_binary(Priv), [<<"127.0.0.1">>]),
    Cert = filename:join(Dir, "listener.crt"),
    Key = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    {PubBin, macula_quic:listen(<<"127.0.0.1">>, Port,
                                [{cert, Cert}, {key, Key}, {alpn, [<<"macula">>]},
                                 {stream_receive_window, ?WINDOW},
                                 {receive_window, 4 * ?WINDOW}])}.

stop_pair(#{listener := Listener, client_conn := ClientConn, server_conn := ServerConn}) ->
    _ = (catch macula_quic:close_connection(ClientConn)),
    _ = (catch macula_quic:close_connection(ServerConn)),
    _ = (catch macula_quic:close_listener(Listener)),
    drain().

drain() ->
    receive
        {quic, _, _, _} -> drain();
        {ready, _} -> drain();
        {busy, _, _} -> drain();
        {'DOWN', _, process, _, _} -> drain()
    after 50 ->
        ok
    end.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.
