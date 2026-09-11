%%%-------------------------------------------------------------------
%%% @doc How a QUIC stream's writes behave when the peer grants no credit.
%%%
%%% Each scenario runs in a peer node of its own. A write that holds a
%%% scheduler, or a VM stuck behind one, then costs only that node: the test
%%% asks the node for the scenario's result with a timeout, and kills the
%%% node's OS process afterwards whatever happened.
%%%
%%% The listener in each scenario sets a 64 KiB stream receive window and the
%%% accepting side never reads its stream, so a sender runs out of credit
%%% after about 64 KiB. The accepting side's connection and stream handles
%%% stay referenced for the whole scenario: a collected connection handle
%%% closes its connection.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_send_backpressure_tests).

-include_lib("eunit/include/eunit.hrl").

%% Scenarios, called inside a peer node.
-export([async_send_without_credit/0,
         async_send_after_connection_loss/0,
         send_waiting_for_credit/0,
         close_during_waiting_send/0,
         reset_during_waiting_send/0,
         close_with_queued_data/1]).

-define(WINDOW, 65_536).
-define(CHUNK, 65_536).
-define(FAST_US, 50_000).
-define(SCENARIO_TIMEOUT_MS, 15_000).
-define(LINGER_MS, 300).

%% With a valid certificate and key, only the window can make listen/3 fail.
receive_window_option_test() ->
    {Cert, Key} = cert_files(),
    Result = macula_quic:listen(<<"127.0.0.1">>, free_udp_port(),
                                [{cert, Cert}, {key, Key}, {stream_receive_window, 0}]),
    ok = file:delete(Cert),
    ok = file:delete(Key),
    close_if_listening(Result),
    ?assertMatch({error, _}, Result).

close_if_listening({ok, Listener}) -> macula_quic:close_listener(Listener);
close_if_listening(_Error) -> ok.

send_backpressure_test_() ->
    [{"async_send/2 on a stream without credit returns within 50 ms, then busy",
      {timeout, 60, fun async_send_without_credit_returns_busy/0}},
     {"async_send/2 returns an error once the connection is lost",
      {timeout, 60, fun async_send_reports_connection_loss/0}},
     {"a send/2 waiting for credit waits in its own process",
      {timeout, 60, fun send_waits_in_its_process/0}},
     {"close_stream/1 during a send/2 waiting for credit returns within 50 ms",
      {timeout, 60, fun close_does_not_wait_for_a_pending_write/0}},
     {"reset_stream/2 during a send/2 waiting for credit returns within 50 ms, and that send/2 returns {error, reset}",
      {timeout, 60, fun reset_ends_a_waiting_send/0}},
     {"a closed stream whose queued data cannot be written is reset after the linger bound",
      {timeout, 60, fun closed_stream_is_reset_after_linger/0}}].

%%%===================================================================
%%% Test bodies, in the test VM
%%%===================================================================

async_send_without_credit_returns_busy() ->
    {ok, Calls} = in_peer(async_send_without_credit, []),
    ?assertEqual([], [Call || {Micros, _} = Call <- Calls, Micros >= ?FAST_US]),
    ?assertMatch({_, {error, busy}}, lists:last(Calls)).

async_send_reports_connection_loss() ->
    ?assertMatch({ok, {error, _}}, in_peer(async_send_after_connection_loss, [])).

send_waits_in_its_process() ->
    {ok, #{waiting := Waiting, status := Status, sent := Sent}} = in_peer(send_waiting_for_credit, []),
    ?assertEqual(waiting, Waiting),
    ?assertEqual({status, waiting}, Status),
    ?assertMatch({error, _}, Sent).

close_does_not_wait_for_a_pending_write() ->
    {ok, #{waiting := Waiting, close_us := Micros, close := Closed}} =
        in_peer(close_during_waiting_send, []),
    ?assertEqual(waiting, Waiting),
    ?assertEqual(ok, Closed),
    ?assert(Micros < ?FAST_US).

reset_ends_a_waiting_send() ->
    {ok, #{waiting := Waiting, reset_us := Micros, reset := Reset, sent := Sent}} =
        in_peer(reset_during_waiting_send, []),
    ?assertEqual(waiting, Waiting),
    ?assertEqual(ok, Reset),
    ?assert(Micros < ?FAST_US),
    ?assertEqual({error, reset}, Sent).

closed_stream_is_reset_after_linger() ->
    {ok, #{queued := Queued, event := Event}} = in_peer(close_with_queued_data, [?LINGER_MS]),
    ?assertMatch([{_, ok} | _], Queued),
    ?assertEqual({stream_closed, {reset, 1}}, Event).

%%%===================================================================
%%% Scenarios, in a peer node
%%%===================================================================

async_send_without_credit() ->
    #{client_stream := Stream} = Pair = pair(),
    Calls = async_until_not_ok(Stream, binary:copy(<<0>>, ?CHUNK), 64, []),
    kept(Pair, Calls).

async_send_after_connection_loss() ->
    #{client_stream := Stream, server_conn := ServerConn} = Pair = pair(),
    ok = macula_quic:close_connection(ServerConn),
    kept(Pair, poll_async_send(Stream, 200)).

send_waiting_for_credit() ->
    #{client_stream := Stream, client_conn := ClientConn} = Pair = pair(),
    Sender = waiting_sender(Stream),
    Waiting = still_waiting(),
    Status = process_status(Sender, 1_000),
    ok = macula_quic:close_connection(ClientConn),
    kept(Pair, #{waiting => Waiting, status => Status, sent => sent_result(2_000)}).

close_during_waiting_send() ->
    #{client_stream := Stream, client_conn := ClientConn} = Pair = pair(),
    _Sender = waiting_sender(Stream),
    Waiting = still_waiting(),
    {Micros, Closed} = timer:tc(macula_quic, close_stream, [Stream]),
    ok = macula_quic:close_connection(ClientConn),
    kept(Pair, #{waiting => Waiting, close_us => Micros, close => Closed}).

reset_during_waiting_send() ->
    #{client_stream := Stream} = Pair = pair(),
    _Sender = waiting_sender(Stream),
    Waiting = still_waiting(),
    {Micros, Reset} = timer:tc(macula_quic, reset_stream, [Stream, 7]),
    kept(Pair, #{waiting => Waiting, reset_us => Micros, reset => Reset, sent => sent_result(500)}).

close_with_queued_data(LingerMs) ->
    ok = application:set_env(macula, quic_close_linger_ms, LingerMs),
    #{client_stream := Stream, server_stream := ServerStream} = Pair = pair(),
    Queued = async_until_not_ok(Stream, binary:copy(<<0>>, ?CHUNK), 4, []),
    ok = macula_quic:close_stream(Stream),
    timer:sleep(LingerMs + 500),
    ok = macula_quic:setopt(ServerStream, active, true),
    kept(Pair, #{queued => Queued, event => stream_end(ServerStream, 3_000)}).

%%%===================================================================
%%% Scenario helpers
%%%===================================================================

%% A loopback listener with a 64 KiB stream receive window, a client
%% connection to it, and one stream the client opened and the listener side
%% accepted and never reads.
pair() ->
    {PubBin, Cert, Key} = identity_files(),
    Port = free_udp_port(),
    {ok, Listener} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                        [{cert, Cert}, {key, Key}, {alpn, [<<"macula">>]},
                                         {stream_receive_window, ?WINDOW},
                                         {receive_window, 4 * ?WINDOW}]),
    ok = file:delete(Cert),
    ok = file:delete(Key),
    ok = macula_quic:async_accept(Listener),
    {ok, ClientConn} = macula_quic:connect(<<"127.0.0.1">>, Port,
                                            [{verify_pubkey, PubBin}, {alpn, [<<"macula">>]}],
                                            5_000),
    ServerConn = receive {quic, new_conn, C, _Info} -> C after 5_000 -> error(no_server_connection) end,
    ok = macula_quic:async_accept_stream(ServerConn),
    {ok, ClientStream} = macula_quic:open_stream(ClientConn),
    %% The accepting side learns of the stream from its first bytes.
    ok = macula_quic:send(ClientStream, <<"open">>),
    ServerStream = receive {quic, new_stream, S, _Props} -> S after 5_000 -> error(no_server_stream) end,
    #{listener => Listener, client_conn => ClientConn, server_conn => ServerConn,
      client_stream => ClientStream, server_stream => ServerStream}.

%% A fresh self-signed identity: the public key a client pins, and the
%% certificate and key files a listener reads.
identity_files() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    PubBin = iolist_to_binary(Pub),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(PubBin, iolist_to_binary(Priv), [<<"127.0.0.1">>]),
    Base = lists:flatten(io_lib:format("/tmp/macula-quic-backpressure-~s-~p",
                                       [os:getpid(), erlang:unique_integer([positive])])),
    Cert = Base ++ ".crt",
    Key = Base ++ ".key",
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    {PubBin, Cert, Key}.

cert_files() ->
    {_PubBin, Cert, Key} = identity_files(),
    {Cert, Key}.

%% Returns Result with the pair's handles referenced until now.
kept(#{listener := _, server_conn := _, server_stream := _}, Result) ->
    Result.

async_until_not_ok(_Stream, _Chunk, 0, Acc) ->
    lists:reverse(Acc);
async_until_not_ok(Stream, Chunk, Left, Acc) ->
    {Micros, Result} = timer:tc(macula_quic, async_send, [Stream, Chunk]),
    async_next(Result, Stream, Chunk, Left - 1, [{Micros, Result} | Acc]).

async_next(ok, Stream, Chunk, Left, Acc) ->
    async_until_not_ok(Stream, Chunk, Left, Acc);
async_next(_NotOk, _Stream, _Chunk, _Left, Acc) ->
    lists:reverse(Acc).

poll_async_send(_Stream, 0) ->
    still_ok;
poll_async_send(Stream, Left) ->
    poll_next(macula_quic:async_send(Stream, <<"x">>), Stream, Left).

poll_next(ok, Stream, Left) ->
    timer:sleep(10),
    poll_async_send(Stream, Left - 1);
poll_next(NotOk, _Stream, _Left) ->
    NotOk.

%% A process that sends twice the window, and reports the result to the
%% scenario process as {sent, Result}.
waiting_sender(Stream) ->
    Scenario = self(),
    Chunk = binary:copy(<<0>>, 2 * ?WINDOW),
    Sender = spawn(fun() -> Scenario ! {sent, macula_quic:send(Stream, Chunk)} end),
    timer:sleep(300),
    Sender.

still_waiting() ->
    receive
        {sent, Result} -> {returned, Result}
    after 0 ->
        waiting
    end.

sent_result(Timeout) ->
    receive
        {sent, Result} -> Result
    after Timeout ->
        no_reply
    end.

%% Asked from a separate process, so a target held inside a NIF costs only
%% that process.
process_status(Pid, Timeout) ->
    Scenario = self(),
    spawn(fun() -> Scenario ! {status, Pid, erlang:process_info(Pid, status)} end),
    receive
        {status, Pid, Status} -> Status
    after Timeout ->
        no_answer
    end.

stream_end(Stream, Timeout) ->
    receive
        {quic, stream_closed, Stream, Detail} -> {stream_closed, Detail};
        {quic, peer_send_shutdown, Stream, _} -> peer_send_shutdown;
        {quic, Data, Stream, _Flags} when is_binary(Data) -> stream_end(Stream, Timeout)
    after Timeout ->
        no_end
    end.

%%%===================================================================
%%% Peer node
%%%===================================================================

in_peer(Scenario, Args) ->
    Started = peer:start_link(#{connection => standard_io,
                                args => ["-pa" | code:get_path()]}),
    Peer = element(2, Started),
    OsPid = peer:call(Peer, os, getpid, [], 5_000),
    try peer:call(Peer, ?MODULE, Scenario, Args, ?SCENARIO_TIMEOUT_MS) of
        Result -> {ok, Result}
    catch
        Class:Reason -> {error, {Class, Reason}}
    after
        _ = os:cmd("kill -9 " ++ OsPid),
        catch peer:stop(Peer)
    end.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.
