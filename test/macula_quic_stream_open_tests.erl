%%%-------------------------------------------------------------------
%%% @doc Tests for opening a QUIC stream when the peer allows no more.
%%%
%%% A listener that allows one concurrent stream per connection, and a
%%% client that has opened that one, stand for a peer at its stream limit.
%%% open_stream/1 then waits in its own process until the peer allows
%%% another stream, and the open ends when that process exits.
%%% async_open_stream/1 returns at once and reports the opened stream, or
%%% the failure, as a tagged message. cancel_open_stream/1 leaves no result
%%% for its open in the mailbox, whether it comes before or after that
%%% result, and a lost connection ends a waiting open with an error. Each
%%% scenario runs in a peer node of its own.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_stream_open_tests).

-include_lib("eunit/include/eunit.hrl").

%% Scenarios, run in a peer node.
-export([open_stream_at_stream_limit/0,
         async_open_at_stream_limit/0,
         cancel_before_the_stream_opens/0,
         cancel_after_the_stream_opened/0,
         open_after_connection_loss/0,
         open_by_an_exited_process/0]).

-define(FAST_US, 50_000).
-define(QUIET_MS, 500).
-define(EVENT_TIMEOUT_MS, 5_000).
-define(SCENARIO_TIMEOUT_MS, 15_000).

stream_open_test_() ->
    [{"open_stream/1 at the stream limit waits in its process until a stream is freed",
      {timeout, 60, fun open_stream_waits_in_its_process/0}},
     {"async_open_stream/1 at the stream limit returns within 50 ms, then reports the stream",
      {timeout, 60, fun async_open_stream_returns_at_once/0}},
     {"cancel_open_stream/1 before the stream opens leaves no result and takes no stream",
      {timeout, 60, fun cancel_before_open_leaves_no_result/0}},
     {"cancel_open_stream/1 after the stream opened takes its result out of the mailbox",
      {timeout, 60, fun cancel_after_open_takes_the_result/0}},
     {"a lost connection ends a waiting open with an error",
      {timeout, 60, fun lost_connection_fails_the_open/0}},
     {"an open ends when its process exits, and takes no stream",
      {timeout, 60, fun open_ends_with_its_process/0}}].

open_stream_waits_in_its_process() ->
    ?assertMatch({ok, {{status, waiting}, {ok, _}}},
                 in_peer(open_stream_at_stream_limit, [])).

async_open_stream_returns_at_once() ->
    ?assertEqual({ok, {true, none, usable}}, in_peer(async_open_at_stream_limit, [])).

cancel_before_open_leaves_no_result() ->
    ?assertEqual({ok, {none, stream_opened}}, in_peer(cancel_before_the_stream_opens, [])).

cancel_after_open_takes_the_result() ->
    ?assertEqual({ok, {true, none}}, in_peer(cancel_after_the_stream_opened, [])).

lost_connection_fails_the_open() ->
    ?assertMatch({ok, {stream_open_failed, {error, _}}},
                 in_peer(open_after_connection_loss, [])).

open_ends_with_its_process() ->
    ?assertEqual({ok, stream_opened}, in_peer(open_by_an_exited_process, [])).

%%%===================================================================
%%% Scenarios
%%%===================================================================

open_stream_at_stream_limit() ->
    Pair = pair(1),
    Scenario = self(),
    Conn = maps:get(client_conn, Pair),
    Opener = spawn(fun() -> Scenario ! {opened, macula_quic:open_stream(Conn)} end),
    timer:sleep(300),
    Status = process_status(Opener, 1_000),
    ok = free_the_open_stream(Pair),
    kept(Pair, {Status, opened_result(?EVENT_TIMEOUT_MS)}).

async_open_at_stream_limit() ->
    Pair = pair(1),
    {Micros, {ok, Opening}} =
        timer:tc(macula_quic, async_open_stream, [maps:get(client_conn, Pair)]),
    Before = awaited(Opening, ?QUIET_MS),
    ok = free_the_open_stream(Pair),
    After = awaited(Opening, ?EVENT_TIMEOUT_MS),
    kept(Pair, {Micros < ?FAST_US, Before, usable(After)}).

cancel_before_the_stream_opens() ->
    Pair = pair(1),
    Conn = maps:get(client_conn, Pair),
    {ok, Opening} = macula_quic:async_open_stream(Conn),
    Tag = macula_quic:stream_open_tag(Opening),
    ok = macula_quic:cancel_open_stream(Opening),
    ok = free_the_open_stream(Pair),
    Late = open_result(Tag, ?QUIET_MS),
    {ok, Next} = macula_quic:async_open_stream(Conn),
    kept(Pair, {Late, kind(awaited(Next, ?EVENT_TIMEOUT_MS))}).

cancel_after_the_stream_opened() ->
    Pair = pair(2),
    {ok, Opening} = macula_quic:async_open_stream(maps:get(client_conn, Pair)),
    Tag = macula_quic:stream_open_tag(Opening),
    InMailbox = result_in_mailbox(Tag, ?EVENT_TIMEOUT_MS),
    ok = macula_quic:cancel_open_stream(Opening),
    kept(Pair, {InMailbox, open_result(Tag, 0)}).

open_after_connection_loss() ->
    Pair = pair(1),
    Conn = maps:get(client_conn, Pair),
    {ok, Opening} = macula_quic:async_open_stream(Conn),
    Scenario = self(),
    _Opener = spawn(fun() -> Scenario ! {opened, macula_quic:open_stream(Conn)} end),
    timer:sleep(300),
    ok = macula_quic:close_connection(maps:get(server_conn, Pair)),
    Async = kind(awaited(Opening, ?EVENT_TIMEOUT_MS)),
    kept(Pair, {Async, opened_result(?EVENT_TIMEOUT_MS)}).

open_by_an_exited_process() ->
    Pair = pair(1),
    Conn = maps:get(client_conn, Pair),
    Opener = spawn(fun() -> macula_quic:open_stream(Conn) end),
    timer:sleep(300),
    exit(Opener, kill),
    timer:sleep(100),
    ok = free_the_open_stream(Pair),
    {ok, Next} = macula_quic:async_open_stream(Conn),
    kept(Pair, kind(awaited(Next, 1_000))).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% A listener that allows `StreamLimit' concurrent streams per connection,
%% a client connection to it, and one stream the client opened and the
%% listener side accepted.
pair(StreamLimit) ->
    Port = free_udp_port(),
    {PubBin, {ok, Listener}} = macula_test_tmp:with_dir("macula-quic-stream-open",
                                                        fun(Dir) -> limited_listener(Dir, Port, StreamLimit) end),
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

%% Ends the pair's stream on both sides, so the listener allows the client
%% another one.
free_the_open_stream(#{client_stream := ClientStream, server_stream := ServerStream}) ->
    ok = macula_quic:close_stream(ClientStream),
    ok = macula_quic:setopt(ServerStream, active, true),
    ok = read_to_end(ServerStream),
    macula_quic:close_stream(ServerStream).

read_to_end(Stream) ->
    receive
        {quic, peer_send_shutdown, Stream, _} -> ok;
        {quic, Data, Stream, _Flags} when is_binary(Data) -> read_to_end(Stream)
    after ?EVENT_TIMEOUT_MS ->
        error(stream_not_finished)
    end.

%% A listener that allows StreamLimit concurrent streams per connection, and the public key a client pins. The listener
%% reads its certificate and key files when it starts listening, so they last only that long.
limited_listener(Dir, Port, StreamLimit) ->
    {PubBin, Cert, Key} = identity_files(Dir),
    {PubBin, macula_quic:listen(<<"127.0.0.1">>, Port,
                                [{cert, Cert}, {key, Key}, {alpn, [<<"macula">>]},
                                 {peer_bidi_stream_count, StreamLimit}])}.

%% A fresh self-signed identity: the public key a client pins, and the
%% certificate and key files in Dir that a listener reads.
identity_files(Dir) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    PubBin = iolist_to_binary(Pub),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(PubBin, iolist_to_binary(Priv), [<<"127.0.0.1">>]),
    Cert = filename:join(Dir, "listener.crt"),
    Key = filename:join(Dir, "listener.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    {PubBin, Cert, Key}.

%% Returns Result with the pair's handles referenced until now.
kept(#{listener := _, client_conn := _, server_conn := _, server_stream := _}, Result) ->
    Result.

%% Waits for the result of `Opening'. The handle is used again after the
%% wait, so it stays referenced meanwhile: a collected handle cancels its
%% open.
awaited(Opening, Timeout) ->
    Result = open_result(macula_quic:stream_open_tag(Opening), Timeout),
    held(Opening, Result).

held(_Opening, Result) ->
    Result.

open_result(Tag, Timeout) ->
    receive
        {quic, stream_opened, Tag, Stream} -> {stream_opened, Stream};
        {quic, stream_open_failed, Tag, Reason} -> {stream_open_failed, Reason}
    after Timeout ->
        none
    end.

kind({Kind, _Detail}) -> Kind;
kind(none) -> none.

%% Whether an opened stream carries data to the listener side.
usable({stream_opened, Stream}) ->
    ok = macula_quic:send(Stream, <<"x">>),
    receive
        {quic, new_stream, _ServerStream, _Props} -> usable
    after ?EVENT_TIMEOUT_MS ->
        not_accepted
    end;
usable(Other) ->
    Other.

opened_result(Timeout) ->
    receive
        {opened, Result} -> Result
    after Timeout ->
        no_result
    end.

result_in_mailbox(Tag, TimeoutMs) ->
    wait_for_result(Tag, erlang:monotonic_time(millisecond) + TimeoutMs).

wait_for_result(Tag, Deadline) ->
    {messages, Messages} = erlang:process_info(self(), messages),
    Results = [M || {quic, Kind, T, _} = M <- Messages, T =:= Tag,
                    Kind =:= stream_opened orelse Kind =:= stream_open_failed],
    found_or_wait(Results, Tag, Deadline).

found_or_wait([_ | _], _Tag, _Deadline) ->
    true;
found_or_wait([], Tag, Deadline) ->
    give_up_or_wait(erlang:monotonic_time(millisecond) >= Deadline, Tag, Deadline).

give_up_or_wait(true, _Tag, _Deadline) ->
    false;
give_up_or_wait(false, Tag, Deadline) ->
    timer:sleep(10),
    wait_for_result(Tag, Deadline).

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
        try peer:stop(Peer) catch _:_ -> ok end
    end.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.
