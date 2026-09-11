%%%-------------------------------------------------------------------
%%% @doc Tests for how a macula_quic dial waits for its handshake.
%%%
%%% A dial that has not completed leaves the node's file IO answering,
%%% stops sending when the process that started it exits, and still ends
%%% with the same connection_timeout error when nothing answers.
%%% async_connect/4 returns before its dial ends and reports the result as
%%% a tagged message, and cancel_connect/1 leaves no result for its dial in
%%% the mailbox, whether it comes before or after that result.
%%%
%%% Most dials target a UDP socket this test owns and never answers from,
%%% so a dial stays in its handshake until its timeout. Dials pin a server
%%% key, so none of them logs an unverified-dial warning.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_async_connect_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HOST, <<"127.0.0.1">>).
-define(OPTS, [{verify_pubkey, <<0:256>>}, {alpn, [<<"macula">>]}]).

%%%===================================================================
%%% connect/4
%%%===================================================================

%% A raw file open and close answers at once while more dials than the
%% node has dirty IO schedulers are waiting for their handshakes.
file_io_answers_while_dials_wait_test_() ->
    {timeout, 30, fun file_io_answers_while_dials_wait/0}.

file_io_answers_while_dials_wait() ->
    {Sock, Port} = silent_udp_socket(),
    Dials = erlang:system_info(dirty_io_schedulers) + 2,
    Test = self(),
    Dialers = [spawn(fun() -> Test ! {dialled, self(), dial(Port, 3_000)} end)
               || _ <- lists:seq(1, Dials)],
    %% Time the open only once as many dials as the node has dirty IO
    %% schedulers are known to be dialing: their Initial packets have
    %% reached the socket.
    ok = dials_seen(Sock, erlang:system_info(dirty_io_schedulers), 5_000),
    {Micros, ok} = macula_test_tmp:with_dir("macula-quic-dial", fun timed_open_and_close/1),
    [await_dialled(Dialer) || Dialer <- Dialers],
    ok = gen_udp:close(Sock),
    flush_datagrams(Sock),
    ?assert(Micros < 500_000).

%% Wait until the socket has received Initial packets from N distinct
%% dials, told apart by their Destination Connection IDs. A dial repeats
%% its DCID when it resends its Initial, so resends are not counted again.
dials_seen(Sock, N, TimeoutMs) ->
    seen_dials(Sock, erlang:monotonic_time(millisecond) + TimeoutMs,
               sets:new([{version, 2}]), N).

seen_dials(Sock, Deadline, Seen, N) ->
    seen_enough(sets:size(Seen) >= N, Sock, Deadline, Seen, N).

seen_enough(true, _Sock, _Deadline, _Seen, _N) ->
    ok;
seen_enough(false, Sock, Deadline, Seen, N) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {udp, Sock, _Ip, _Port, Packet} ->
            seen_dials(Sock, Deadline, add_initial_dcid(Packet, Seen), N)
    after Left ->
        erlang:error({dials_seen, sets:size(Seen), expected_at_least, N})
    end.

%% A QUIC long header with packet type Initial: form and fixed bits set,
%% type 0, then the version and the length-prefixed DCID.
add_initial_dcid(<<1:1, 1:1, 0:2, _:4, _Version:32, Len:8, Dcid:Len/binary,
                   _/binary>>, Seen) ->
    sets:add_element(Dcid, Seen);
add_initial_dcid(_Packet, Seen) ->
    Seen.

%% A dial nothing answers ends with connection_timeout, soon after its
%% timeout.
unanswered_dial_ends_with_connection_timeout_test_() ->
    {timeout, 30, fun unanswered_dial_ends_with_connection_timeout/0}.

unanswered_dial_ends_with_connection_timeout() ->
    {Sock, Port} = silent_udp_socket(),
    {Micros, Result} = timer:tc(fun() -> dial(Port, 1_000) end),
    ok = gen_udp:close(Sock),
    flush_datagrams(Sock),
    ?assertEqual({error, <<"connection_timeout">>}, Result),
    ?assert(Micros < 2_000_000).

%% A dial stops sending when the process that started it exits.
dial_stops_when_owner_exits_test_() ->
    {timeout, 30, fun dial_stops_when_owner_exits/0}.

dial_stops_when_owner_exits() ->
    {Sock, Port} = silent_udp_socket(),
    Owner = spawn(fun() -> dial(Port, 30_000) end),
    ok = datagrams_at_least(Sock, 1, 5_000),
    exit(Owner, kill),
    timer:sleep(200),
    flush_datagrams(Sock),
    Later = datagrams_within(Sock, 3_000),
    ok = gen_udp:close(Sock),
    ?assertEqual(0, Later).

%%%===================================================================
%%% async_connect/4 and cancel_connect/1
%%%===================================================================

%% async_connect/4 returns before its dial ends, and the result arrives as
%% a message carrying the dial's tag.
dial_returns_at_once_test_() ->
    {timeout, 30, fun dial_returns_at_once/0}.

dial_returns_at_once() ->
    {Sock, Port} = silent_udp_socket(),
    {Micros, {ok, Dial}} = timer:tc(fun() -> async_dial(Port, 1_000) end),
    Tag = macula_quic:dial_tag(Dial),
    Reason = receive
                 {quic, connect_failed, Tag, R} -> R
             after 3_000 ->
                 no_result
             end,
    ok = gen_udp:close(Sock),
    flush_datagrams(Sock),
    ?assert(Micros < 50_000),
    ?assertEqual(<<"connection_timeout">>, Reason).

%% A cancel after the dial failed leaves no result for it in the mailbox.
cancel_after_a_failed_dial_leaves_no_result_test_() ->
    {timeout, 30, fun cancel_after_a_failed_dial_leaves_no_result/0}.

cancel_after_a_failed_dial_leaves_no_result() ->
    {Sock, Port} = silent_udp_socket(),
    {ok, Dial} = async_dial(Port, 200),
    Tag = macula_quic:dial_tag(Dial),
    ok = result_in_mailbox(Tag, 3_000),
    ok = macula_quic:cancel_connect(Dial),
    ok = gen_udp:close(Sock),
    flush_datagrams(Sock),
    ?assertEqual([], dial_results(Tag)).

%% A cancel after the dial connected leaves no result for it in the
%% mailbox.
cancel_after_a_connected_dial_leaves_no_result_test_() ->
    {timeout, 30,
     {setup, fun start_listener/0, fun stop_listener/1,
      fun(Listener) ->
          ?_test(cancel_after_a_connected_dial_leaves_no_result(Listener))
      end}}.

cancel_after_a_connected_dial_leaves_no_result(#{port := Port, pubkey := Pub}) ->
    {ok, Dial} = macula_quic:async_connect(
                   ?HOST, Port, [{verify_pubkey, Pub}, {alpn, [<<"macula">>]}], 5_000),
    Tag = macula_quic:dial_tag(Dial),
    ok = result_in_mailbox(Tag, 5_000),
    ok = macula_quic:cancel_connect(Dial),
    ?assertEqual([], dial_results(Tag)).

%% A cancel before the dial reports stops it: the socket receives nothing
%% more, and no result arrives.
cancel_before_a_result_stops_the_dial_test_() ->
    {timeout, 30, fun cancel_before_a_result_stops_the_dial/0}.

cancel_before_a_result_stops_the_dial() ->
    {Sock, Port} = silent_udp_socket(),
    {ok, Dial} = async_dial(Port, 30_000),
    ok = datagrams_at_least(Sock, 1, 5_000),
    ok = macula_quic:cancel_connect(Dial),
    timer:sleep(200),
    flush_datagrams(Sock),
    Later = datagrams_within(Sock, 3_000),
    ok = gen_udp:close(Sock),
    ?assertEqual(0, Later),
    ?assertEqual([], dial_results(macula_quic:dial_tag(Dial))).

%%%===================================================================
%%% Helpers
%%%===================================================================

dial(Port, TimeoutMs) ->
    macula_quic:connect(?HOST, Port, ?OPTS, TimeoutMs).

async_dial(Port, TimeoutMs) ->
    macula_quic:async_connect(?HOST, Port, ?OPTS, TimeoutMs).

await_dialled(Dialer) ->
    receive
        {dialled, Dialer, _Result} -> ok
    after 10_000 ->
        erlang:error({dial_did_not_end, Dialer})
    end.

%% How long opening and closing a new file in Dir takes, timing the file operations only.
timed_open_and_close(Dir) ->
    timer:tc(fun() -> open_and_close_a_file(filename:join(Dir, "opened")) end).

open_and_close_a_file(Path) ->
    {ok, Fd} = file:open(Path, [write, raw]),
    ok = file:close(Fd),
    file:delete(Path).

%% The results for Tag in this process's mailbox, left in place.
dial_results(Tag) ->
    {messages, Messages} = erlang:process_info(self(), messages),
    [M || {quic, Event, T, _} = M <- Messages, T =:= Tag,
          Event =:= connected orelse Event =:= connect_failed].

%% Wait until a result for Tag is in the mailbox, without taking it out.
result_in_mailbox(Tag, TimeoutMs) ->
    wait_for_result(Tag, erlang:monotonic_time(millisecond) + TimeoutMs).

wait_for_result(Tag, Deadline) ->
    wait_for_result(dial_results(Tag), Tag, Deadline).

wait_for_result([_ | _], _Tag, _Deadline) ->
    ok;
wait_for_result([], Tag, Deadline) ->
    wait_or_give_up(erlang:monotonic_time(millisecond) >= Deadline, Tag, Deadline).

wait_or_give_up(true, Tag, _Deadline) ->
    erlang:error({no_result, Tag});
wait_or_give_up(false, Tag, Deadline) ->
    timer:sleep(10),
    wait_for_result(Tag, Deadline).

silent_udp_socket() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}, {active, true}]),
    {ok, Port} = inet:port(Sock),
    {Sock, Port}.

%% Wait until the socket has received at least N datagrams.
datagrams_at_least(Sock, N, TimeoutMs) ->
    Got = count_datagrams(Sock, erlang:monotonic_time(millisecond) + TimeoutMs, 0, N),
    at_least(Got, N).

at_least(Got, N) when Got >= N -> ok;
at_least(Got, N) -> erlang:error({datagrams, Got, expected_at_least, N}).

flush_datagrams(Sock) ->
    receive
        {udp, Sock, _Ip, _Port, _Packet} -> flush_datagrams(Sock)
    after 0 ->
        ok
    end.

datagrams_within(Sock, WindowMs) ->
    count_datagrams(Sock, erlang:monotonic_time(millisecond) + WindowMs, 0, infinity).

count_datagrams(_Sock, _Deadline, Stop, Stop) ->
    Stop;
count_datagrams(Sock, Deadline, N, Stop) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {udp, Sock, _Ip, _Port, _Packet} -> count_datagrams(Sock, Deadline, N + 1, Stop)
    after Left ->
        N
    end.

%% A loopback QUIC listener with a self-signed certificate for a fresh key,
%% so a dial can pin that key and connect.
start_listener() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, KeyPem}} = macula_quic:generate_self_signed_cert(
                                iolist_to_binary(Pub), iolist_to_binary(Priv),
                                [<<"localhost">>, <<"127.0.0.1">>]),
    Port = free_udp_port(),
    {ok, Listener} = macula_test_tmp:with_dir("macula-quic-async-connect",
                                              fun(Dir) -> listen(Dir, Port, CertPem, KeyPem) end),
    ok = macula_quic:async_accept(Listener),
    #{listener => Listener, port => Port, pubkey => iolist_to_binary(Pub)}.

%% The listener reads its certificate and key files when it starts listening, so they last only that long.
listen(Dir, Port, CertPem, KeyPem) ->
    Cert = filename:join(Dir, "listener.crt"),
    Key = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    macula_quic:listen(?HOST, Port,
                       [{cert, Cert}, {key, Key},
                        {alpn, [<<"macula">>]},
                        {idle_timeout_ms, 30_000},
                        {keep_alive_interval_ms, 5_000}]).

stop_listener(#{listener := Listener}) ->
    _ = macula_quic:close_listener(Listener),
    drain_quic_messages().

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.

drain_quic_messages() ->
    receive
        {quic, _, _, _} -> drain_quic_messages()
    after 0 ->
        ok
    end.
