%%%-------------------------------------------------------------------
%%% @doc Tests for a dist relay client whose relay stops reading its
%%% control stream.
%%%
%%% A relay run by this test identifies the client and announces enough
%%% inbound tunnels that their tunnel_close frames outgrow the control
%%% stream's 1 MiB send queue. It then stops reading the control stream,
%%% which has a 1 KiB receive window, while the client is asked to close
%%% them all. The client keeps taking requests, answers
%%% status/1 within 50 ms, reports the control frames it holds, and still
%%% hands an inbound tunnel to net_kernel. Once the relay reads again,
%%% every frame arrives, in the order the client was asked to send them.
%%% The scenario runs in a peer node of its own.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay_client_control_queue_tests).

-include_lib("eunit/include/eunit.hrl").

%% Scenario, run in a peer node.
-export([control_frames_while_the_relay_stops_reading/0]).

-define(RELAY_ALPN, <<"macula-dist">>).
-define(CONTROL_WINDOW, 1024).
%% About 1.6 MiB of tunnel_close frames: more than the stream's send queue
%% takes before it answers busy.
-define(FRAMES, 48_000).
%% Tunnels announced per write.
-define(ANNOUNCE_BATCH, 1_000).
-define(FAST_MS, 50).
-define(DRAIN_MS, 20_000).
-define(EVENT_TIMEOUT_MS, 15_000).
-define(SCENARIO_TIMEOUT_MS, 90_000).

control_queue_test_() ->
    {"a client whose relay stops reading keeps serving and later sends every frame in order",
     {timeout, 120, fun client_keeps_serving_and_sends_in_order/0}}.

client_keeps_serving_and_sends_in_order() ->
    ?assertEqual({ok, {drained, answered, holds_frames, accepted, all_in_order}},
                 in_peer(control_frames_while_the_relay_stops_reading, [])).

%%%===================================================================
%%% Scenario
%%%===================================================================

control_frames_while_the_relay_stops_reading() ->
    os:putenv("MACULA_TLS_MODE", "development"),
    {Listener, Port} = relay_listener(),
    ok = macula_quic:async_accept(Listener),
    {ok, Client} = macula_dist_relay_client:start_link(relay_url(Port), <<"queue@127.0.0.1">>),
    Conn = receive_event(new_conn),
    ok = macula_quic:async_accept_stream(Conn),
    Control = receive_event(new_stream),
    ok = macula_quic:setopt(Control, active, true),
    {[#{type := identify}], Rest} = control_messages(Control, <<>>, 1),
    ok = macula_quic:send(Control, encode(#{type => identified, status => ok})),
    ok = identified(Client, ?EVENT_TIMEOUT_MS),
    %% This process stands in for net_kernel.
    ok = macula_dist_relay_client:set_kernel(Client, self()),
    ok = announce_tunnels(Control),
    ok = pending_inbound(Client, ?FRAMES, erlang:monotonic_time(millisecond) + ?DRAIN_MS),
    ok = macula_quic:setopt(Control, active, false),
    _ = [macula_dist_relay_client:close_tunnel(Client, tunnel_id(N))
         || N <- lists:seq(1, ?FRAMES)],
    Drained = mailbox_drained(Client, erlang:monotonic_time(millisecond) + ?DRAIN_MS),
    Status = answer_within(fun() -> macula_dist_relay_client:status(Client) end, ?FAST_MS),
    Inbound = inbound_tunnel(Conn, Control),
    ok = macula_quic:setopt(Control, active, true),
    {Closed, _} = control_messages(Control, Rest, ?FRAMES),
    Expected = [#{type => tunnel_close, tunnel_id => tunnel_id(N)} || N <- lists:seq(1, ?FRAMES)],
    kept({Listener, Conn},
         {Drained, answered(Status), holds(Status), Inbound, in_order(Closed =:= Expected)}).

%%%===================================================================
%%% Helpers
%%%===================================================================

relay_listener() ->
    Port = free_udp_port(),
    {ok, Listener} = macula_test_tmp:with_dir("macula-relay-client-queue",
                                             fun(Dir) -> listener_in(Dir, Port) end),
    {Listener, Port}.

%% A relay listener on `Port' with a 1 KiB stream window, whose certificate
%% and key live in `Dir' while listen reads them.
listener_in(Dir, Port) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            iolist_to_binary(Pub), iolist_to_binary(Priv), [<<"localhost">>, <<"127.0.0.1">>]),
    Cert = filename:join(Dir, "relay.crt"),
    Key = filename:join(Dir, "relay.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    macula_quic:listen(<<"127.0.0.1">>, Port,
                       [{cert, Cert}, {key, Key},
                        {alpn, [?RELAY_ALPN]},
                        {stream_receive_window, ?CONTROL_WINDOW},
                        {receive_window, 4 * ?CONTROL_WINDOW}]).

%% Announces FRAMES inbound tunnels to the client, ANNOUNCE_BATCH per write.
announce_tunnels(Control) ->
    lists:foreach(fun(Batch) -> ok = macula_quic:send(Control, notifies(Batch)) end,
                  batches(lists:seq(1, ?FRAMES), ?ANNOUNCE_BATCH)).

notifies(Ids) ->
    iolist_to_binary([encode(#{type => tunnel_notify, tunnel_id => tunnel_id(N),
                               source => <<"peer@127.0.0.1">>})
                      || N <- Ids]).

batches(List, Size) when length(List) =< Size ->
    [List];
batches(List, Size) ->
    {Batch, Rest} = lists:split(Size, List),
    [Batch | batches(Rest, Size)].

%% Waits until the client holds `Count' announced inbound tunnels.
pending_inbound(Client, Count, Deadline) ->
    pending_count(maps:get(pending_inbound, macula_dist_relay_client:status(Client)),
                  Count, Client, Deadline).

pending_count(Count, Count, _Client, _Deadline) ->
    ok;
pending_count(Seen, Count, Client, Deadline) ->
    pending_or_give_up(erlang:monotonic_time(millisecond) >= Deadline, Seen, Count, Client,
                       Deadline).

pending_or_give_up(true, Seen, _Count, _Client, _Deadline) ->
    {pending_inbound, Seen};
pending_or_give_up(false, _Seen, Count, Client, Deadline) ->
    timer:sleep(10),
    pending_inbound(Client, Count, Deadline).

relay_url(Port) ->
    iolist_to_binary(io_lib:format("quic://127.0.0.1:~p", [Port])).

tunnel_id(N) ->
    <<"tunnel-", (integer_to_binary(N))/binary>>.

encode(Message) ->
    macula_dist_relay_protocol:encode(Message).

receive_event(Event) ->
    receive
        {quic, Event, Handle, _Info} -> Handle
    after ?EVENT_TIMEOUT_MS ->
        error({missed, Event})
    end.

%% The next `Count' control messages on `Control', and the bytes left over.
control_messages(Control, Buffer, Count) ->
    collect_messages(Control, macula_dist_relay_protocol:decode_buffer(Buffer), Count, []).

collect_messages(_Control, {Messages, Rest}, Count, Acc)
  when length(Messages) + length(Acc) >= Count ->
    {lists:reverse(Acc, Messages), Rest};
collect_messages(Control, {Messages, Rest}, Count, Acc) ->
    receive
        {quic, Data, Control, _Flags} when is_binary(Data) ->
            collect_messages(Control,
                             macula_dist_relay_protocol:decode_buffer(<<Rest/binary, Data/binary>>),
                             Count, lists:reverse(Messages, Acc))
    after ?EVENT_TIMEOUT_MS ->
        {lists:reverse(Acc, Messages), Rest}
    end.

identified(_Client, Left) when Left =< 0 ->
    not_identified;
identified(Client, Left) ->
    identified_status(macula_dist_relay_client:status(Client), Client, Left).

identified_status(#{identified := true}, _Client, _Left) ->
    ok;
identified_status(_Status, Client, Left) ->
    timer:sleep(10),
    identified(Client, Left - 10).

%% The relay announces an inbound tunnel and opens its stream. Whether the
%% client hands the tunnel to this process, standing in for net_kernel,
%% within 2 s.
inbound_tunnel(Conn, Control) ->
    TunnelId = binary:encode_hex(crypto:strong_rand_bytes(16)),
    ok = macula_quic:send(Control, encode(#{type => tunnel_notify, tunnel_id => TunnelId,
                                            source => <<"peer@127.0.0.1">>})),
    {ok, Stream} = macula_quic:open_stream(Conn),
    ok = macula_quic:send(Stream, TunnelId),
    receive
        {accept, _SetupPid, _Socket, inet, macula_dist} -> accepted
    after 2_000 ->
        not_accepted
    end.

%% Whether the client took every request out of its mailbox before
%% `Deadline'.
mailbox_drained(Client, Deadline) ->
    mailbox_length(erlang:process_info(Client, message_queue_len), Client, Deadline).

mailbox_length({message_queue_len, 0}, _Client, _Deadline) ->
    drained;
mailbox_length({message_queue_len, Len}, Client, Deadline) ->
    wait_or_report(erlang:monotonic_time(millisecond) >= Deadline, Len, Client, Deadline).

wait_or_report(true, Len, _Client, _Deadline) ->
    {not_drained, Len};
wait_or_report(false, _Len, Client, Deadline) ->
    timer:sleep(10),
    mailbox_drained(Client, Deadline).

%% Runs `Fun' in a separate process and returns its value if it comes
%% within `Ms'.
answer_within(Fun, Ms) ->
    Scenario = self(),
    Asker = spawn(fun() -> Scenario ! {answer, self(), Fun()} end),
    receive
        {answer, Asker, Value} -> {answered, Value}
    after Ms ->
        exit(Asker, kill),
        no_answer
    end.

answered({answered, _Status}) -> answered;
answered(no_answer) -> no_answer.

holds({answered, #{held_control_frames := Held}}) when is_integer(Held), Held > 0 ->
    holds_frames;
holds(Other) ->
    {holds_no_frames, Other}.

in_order(true) -> all_in_order;
in_order(false) -> not_all_in_order.

%% Returns Result with the relay's handles referenced until now.
kept({_Listener, _Conn}, Result) ->
    Result.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.

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
