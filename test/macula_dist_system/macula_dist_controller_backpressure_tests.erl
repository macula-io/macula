%%%-------------------------------------------------------------------
%%% @doc A distribution controller over QUIC keeps serving while its
%%% stream takes no more data, and the runtime holds the distribution
%%% data meanwhile.
%%%
%%% Two peer nodes use macula_dist in dist_relay mode through a relay run
%%% by this test. Once they are connected, the relay stops reading the
%%% sending node's tunnel stream, so that stream gets no flow-control
%%% credit, and a process on the sending node sends 64 messages of 1 MiB
%%% each. The sender stays blocked, the sending node's controller answers
%%% get_status within 50 ms, and once the relay reads again every message
%%% arrives, in order.
%%%
%%% macula_dist has no direct listener, so an inbound distribution
%%% connection only arrives through a relay. The relay client learns the
%%% net_kernel from macula_dist:accept/1, which runs at boot before any
%%% client exists, so the receiving node hands it over here.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_controller_backpressure_tests).

-include_lib("eunit/include/eunit.hrl").

%% Called on the peer nodes.
-export([join_relay/1,
         deliver_inbound_tunnels_to_net_kernel/0,
         connect/1,
         start_receiver/0,
         received/2,
         start_sender/3,
         sender_state/0,
         sender_finished/1,
         controller_answers/2]).

-define(RELAY_ALPN, <<"macula-dist">>).
-define(COOKIE, "macula_dist_controller_backpressure").
%% The relay's receive window on a stream: the credit the sending node
%% has left once the relay stops reading its tunnel stream.
-define(TUNNEL_WINDOW, 65536).
-define(MESSAGES, 64).
-define(MESSAGE_BYTES, 1024 * 1024).
-define(ANSWER_MS, 50).
-define(HELD_MS, 1000).
-define(DELIVERY_MS, 20000).
-define(EVENT_TIMEOUT_MS, 15000).
-define(CALL_TIMEOUT_MS, 30000).
-define(IDENTIFY_POLL_MS, 10).
-define(SENDER, macula_dist_backpressure_sender).
-define(RECEIVER, macula_dist_backpressure_receiver).

%% =============================================================================
%% The test
%% =============================================================================

controller_backpressure_test_() ->
    {timeout, 120,
     {"a busy dist controller answers get_status and its held data arrives in order",
      fun busy_controller_keeps_serving/0}}.

busy_controller_keeps_serving() ->
    Relay = start_relay(),
    A = start_node("dist_bp_a"),
    B = start_node("dist_bp_b"),
    try
        ok = on(A, join_relay, [relay_url(Relay)]),
        ok = on(B, join_relay, [relay_url(Relay)]),
        ok = on(B, deliver_inbound_tunnels_to_net_kernel, []),
        ?assert(on(A, connect, [node_of(B)])),
        ok = on(B, start_receiver, []),

        ok = relay_call(Relay, stall),
        ok = on(A, start_sender, [node_of(B), ?MESSAGES, ?MESSAGE_BYTES]),
        timer:sleep(?HELD_MS),
        ?assertEqual(sending, on(A, sender_state, [])),
        ?assertEqual(answered, on(A, controller_answers, [node_of(B), ?ANSWER_MS])),

        ok = relay_call(Relay, resume),
        ?assertEqual(lists:seq(1, ?MESSAGES),
                     on(B, received, [?MESSAGES, ?DELIVERY_MS])),
        ?assertEqual(finished, on(A, sender_finished, [?DELIVERY_MS]))
    after
        stop_node(A),
        stop_node(B),
        stop_relay(Relay)
    end.

%% =============================================================================
%% On the peer nodes
%% =============================================================================

%% Start macula, join the relay, and wait until the relay has identified
%% this node: the client refuses a tunnel request before that.
join_relay(Url) ->
    {ok, _} = application:ensure_all_started(macula),
    ok = macula:join_dist_relay(#{url => Url}),
    {ok, Client} = macula:dist_relay_client(),
    await_identified(Client, ?EVENT_TIMEOUT_MS div ?IDENTIFY_POLL_MS).

await_identified(Client, 0) ->
    exit({not_identified, macula_dist_relay_client:status(Client)});
await_identified(Client, Polls) ->
    identified(macula_dist_relay_client:status(Client), Client, Polls).

identified(#{identified := true}, _Client, _Polls) ->
    ok;
identified(#{identified := false}, Client, Polls) ->
    timer:sleep(?IDENTIFY_POLL_MS),
    await_identified(Client, Polls - 1).

%% What macula_dist:accept/1 does for a client that already runs.
deliver_inbound_tunnels_to_net_kernel() ->
    {ok, Client} = macula:dist_relay_client(),
    macula_dist_relay_client:set_kernel(Client, whereis(net_kernel)).

connect(Node) ->
    net_kernel:connect_node(Node).

start_receiver() ->
    true = register(?RECEIVER, spawn(fun() -> receiver([], none) end)),
    ok.

receiver(Seqs, Waiter) ->
    receive
        {Seq, Payload} when is_integer(Seq), is_binary(Payload) ->
            answer_waiter([Seq | Seqs], Waiter);
        {wait_for, Count, From, Ref} ->
            answer_waiter(Seqs, {Count, From, Ref})
    end.

answer_waiter(Seqs, {Count, From, Ref}) when length(Seqs) >= Count ->
    From ! {Ref, lists:reverse(Seqs)},
    receiver(Seqs, none);
answer_waiter(Seqs, Waiter) ->
    receiver(Seqs, Waiter).

%% The sequence numbers received, in arrival order, once `Count' arrived.
received(Count, TimeoutMs) ->
    Ref = make_ref(),
    ?RECEIVER ! {wait_for, Count, self(), Ref},
    receive
        {Ref, Seqs} -> Seqs
    after TimeoutMs ->
        {fewer_than, Count, within_ms, TimeoutMs}
    end.

start_sender(Node, Count, Bytes) ->
    Sender = spawn(fun() -> receive go -> send_numbered(Node, 1, Count, Bytes) end end),
    true = register(?SENDER, Sender),
    Sender ! go,
    ok.

send_numbered(_Node, Seq, Count, _Bytes) when Seq > Count ->
    ok;
send_numbered(Node, Seq, Count, Bytes) ->
    {?RECEIVER, Node} ! {Seq, binary:copy(<<Seq>>, Bytes)},
    send_numbered(Node, Seq + 1, Count, Bytes).

sender_state() ->
    sender_state_of(whereis(?SENDER)).

sender_state_of(undefined) -> finished;
sender_state_of(Sender) when is_pid(Sender) -> sending.

sender_finished(TimeoutMs) ->
    finished_within(whereis(?SENDER), TimeoutMs).

finished_within(undefined, _TimeoutMs) ->
    finished;
finished_within(Sender, TimeoutMs) ->
    Mon = erlang:monitor(process, Sender),
    receive
        {'DOWN', Mon, process, Sender, _Reason} -> finished
    after TimeoutMs ->
        erlang:demonitor(Mon, [flush]),
        sending
    end.

%% Whether this node's controller for `Node' answers get_status within `Ms'.
controller_answers(Node, Ms) ->
    {Node, Controller} = lists:keyfind(Node, 1, erlang:system_info(dist_ctrl)),
    Controller ! {self(), get_status},
    receive
        {Controller, get_status, {ok, _Read, _Written}} -> answered
    after Ms ->
        {no_answer_within_ms, Ms}
    end.

%% =============================================================================
%% The relay: a QUIC listener with the relay's ALPN. It identifies two nodes,
%% one at a time, builds the tunnel the first request asks for, and pipes
%% bytes between the tunnel's two streams. On request it stops reading, and
%% reads again, the stream from the node that asked for the tunnel.
%% =============================================================================

start_relay() ->
    Test = self(),
    Relay = spawn(fun() -> relay(Test) end),
    receive
        {relay_listening, Relay, Port, Files} ->
            #{pid => Relay, port => Port, files => Files}
    after ?EVENT_TIMEOUT_MS ->
        error(relay_did_not_listen)
    end.

relay(Test) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            iolist_to_binary(Pub), iolist_to_binary(Priv),
            [<<"localhost">>, <<"127.0.0.1">>]),
    Base = lists:flatten(io_lib:format("/tmp/macula-dist-controller-backpressure-~p",
                                       [erlang:unique_integer([positive])])),
    Cert = Base ++ ".crt",
    Key = Base ++ ".key",
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    Port = free_udp_port(),
    {ok, Listener} = macula_quic:listen(
        <<"127.0.0.1">>, Port,
        [{cert, Cert}, {key, Key},
         {alpn, [?RELAY_ALPN]},
         {idle_timeout_ms, 30000},
         {keep_alive_interval_ms, 5000},
         {stream_receive_window, ?TUNNEL_WINDOW},
         {receive_window, 4 * ?TUNNEL_WINDOW}]),
    Test ! {relay_listening, self(), Port, [Cert, Key]},
    Nodes = identify_nodes(Listener, 2, #{}),
    %% The listener and the connections stay referenced for as long as the
    %% relay runs: a collected handle closes what it refers to.
    pipe(tunnel(tunnel_request(Nodes), Nodes), {Listener, Nodes}).

identify_nodes(_Listener, 0, Nodes) ->
    Nodes;
identify_nodes(Listener, Count, Nodes) ->
    ok = macula_quic:async_accept(Listener),
    Conn = receive_event(new_conn),
    ok = macula_quic:async_accept_stream(Conn),
    Control = receive_event(new_stream),
    ok = macula_quic:setopt(Control, active, true),
    #{type := identify, node_name := Name} = control_message(Control, <<>>),
    ok = macula_quic:send(Control, encode(#{type => identified, status => ok})),
    identify_nodes(Listener, Count - 1, Nodes#{Name => {Conn, Control}}).

receive_event(Event) ->
    receive
        {quic, Event, Handle, _Info} -> Handle
    after ?EVENT_TIMEOUT_MS ->
        exit({relay_missed, Event})
    end.

control_message(Control, Buffer) ->
    receive
        {quic, Data, Control, _Flags} when is_binary(Data) ->
            first_message(
                macula_dist_relay_protocol:decode_buffer(<<Buffer/binary, Data/binary>>),
                Control)
    after ?EVENT_TIMEOUT_MS ->
        exit({relay_missed_control_message, Control})
    end.

first_message({[Message | _], _Rest}, _Control) -> Message;
first_message({[], Rest}, Control) -> control_message(Control, Rest).

%% The first tunnel request on any control stream, as {Source, Target}.
tunnel_request(Nodes) ->
    Names = maps:fold(fun(Name, {_Conn, Control}, Acc) -> Acc#{Control => Name} end,
                      #{}, Nodes),
    receive
        {quic, Data, Control, _Flags} when is_binary(Data), is_map_key(Control, Names) ->
            {[#{type := tunnel_request, target := Target}], _} =
                macula_dist_relay_protocol:decode_buffer(Data),
            {maps:get(Control, Names), Target}
    after ?EVENT_TIMEOUT_MS ->
        exit(relay_missed_tunnel_request)
    end.

%% Open the tunnel's stream on each node's connection, each starting with
%% the tunnel id, then tell both nodes, as the real relay does.
tunnel({Source, Target}, Nodes) ->
    {SourceConn, SourceControl} = maps:get(Source, Nodes),
    {TargetConn, TargetControl} = maps:get(Target, Nodes),
    TunnelId = binary:encode_hex(crypto:strong_rand_bytes(16)),
    SourceStream = prefixed_stream(SourceConn, TunnelId),
    TargetStream = prefixed_stream(TargetConn, TunnelId),
    ok = macula_quic:send(SourceControl,
                          encode(#{type => tunnel_ok, tunnel_id => TunnelId})),
    ok = macula_quic:send(TargetControl,
                          encode(#{type => tunnel_notify, tunnel_id => TunnelId,
                                   source => Source})),
    ok = macula_quic:setopt(SourceStream, active, true),
    ok = macula_quic:setopt(TargetStream, active, true),
    {SourceStream, TargetStream}.

prefixed_stream(Conn, TunnelId) ->
    {ok, Stream} = macula_quic:open_stream(Conn),
    ok = macula_quic:send(Stream, TunnelId),
    Stream.

%% Bytes from either stream go to the other. `stall' stops reading the
%% source node's stream; `resume' reads it again.
pipe({SourceStream, TargetStream} = Tunnel, Keep) ->
    receive
        {quic, Data, SourceStream, _Flags} when is_binary(Data) ->
            ok = macula_quic:send(TargetStream, Data),
            pipe(Tunnel, Keep);
        {quic, Data, TargetStream, _Flags} when is_binary(Data) ->
            ok = macula_quic:send(SourceStream, Data),
            pipe(Tunnel, Keep);
        {From, Ref, stall} ->
            From ! {Ref, macula_quic:setopt(SourceStream, active, false)},
            pipe(Tunnel, Keep);
        {From, Ref, resume} ->
            From ! {Ref, macula_quic:setopt(SourceStream, active, true)},
            pipe(Tunnel, Keep)
    end.

relay_call(#{pid := Relay}, Request) ->
    Ref = make_ref(),
    Relay ! {self(), Ref, Request},
    receive
        {Ref, Reply} -> Reply
    after ?EVENT_TIMEOUT_MS ->
        error({relay_did_not_answer, Request})
    end.

stop_relay(#{pid := Relay, files := Files}) ->
    exit(Relay, kill),
    lists:foreach(fun file:delete/1, Files).

relay_url(#{port := Port}) ->
    iolist_to_binary(io_lib:format("quic://127.0.0.1:~p", [Port])).

encode(Message) ->
    macula_dist_relay_protocol:encode(Message).

free_udp_port() ->
    {ok, Socket} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_udp:close(Socket),
    Port.

%% =============================================================================
%% Peer nodes
%% =============================================================================

start_node(Prefix) ->
    Name = Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive])),
    {ok, Peer, Node} = peer:start_link(#{
        name => Name,
        host => "127.0.0.1",
        longnames => true,
        connection => standard_io,
        args => ["-proto_dist", "macula", "-start_epmd", "false",
                 "-setcookie", ?COOKIE, "-pa" | code:get_path()],
        env => [{"MACULA_DIST_MODE", "dist_relay"},
                {"MACULA_TLS_MODE", "development"}]}),
    #{peer => Peer, node => Node,
      os_pid => peer:call(Peer, os, getpid, [], ?EVENT_TIMEOUT_MS)}.

%% Killed rather than stopped: a node whose controller is stuck may not stop.
stop_node(#{peer := Peer, os_pid := OsPid}) ->
    _ = os:cmd("kill -9 " ++ OsPid),
    try peer:stop(Peer) catch _:_ -> ok end.

node_of(#{node := Node}) ->
    Node.

on(#{peer := Peer}, Function, Args) ->
    peer:call(Peer, ?MODULE, Function, Args, ?CALL_TIMEOUT_MS).
