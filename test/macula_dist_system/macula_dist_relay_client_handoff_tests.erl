%%%-------------------------------------------------------------------
%%% @doc Tests for the bytes that reach a dist relay client's tunnel
%%% stream around the moment the client hands the stream over.
%%%
%%% A relay run by this test opens a tunnel stream to the client and writes
%%% the first byte of the tunnel id. While the client is suspended, the
%%% relay writes the rest of the tunnel id and then the tunnel's first
%%% bytes, each as a read of its own, so both wait in the client's mailbox.
%%% For an inbound tunnel the relay announced the tunnel before the stream;
%%% for an outbound tunnel it confirms the tunnel only after the bytes. In
%%% both cases the process that takes the stream receives the bytes, in
%%% order, with the handoff: the dist controller in the controller-ok
%%% message, the caller of request_tunnel/2 with the tunnel. The scenarios
%%% run in a peer node of their own.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay_client_handoff_tests).

-include_lib("eunit/include/eunit.hrl").

%% Scenarios, run in a peer node.
-export([inbound_bytes_queued_behind_the_prefix/0,
         outbound_bytes_before_tunnel_ok/0]).

-define(RELAY_ALPN, <<"macula-dist">>).
-define(PEER, <<"peer@127.0.0.1">>).
%% The first frame of a dist handshake, a node's name: 37 bytes.
-define(FIRST_FRAME, <<35:16, $N, 0:64, 0:32, 20:16, "handoff_a@127.0.0.10">>).
%% Long enough for a write to reach the client as a read of its own.
-define(READ_GAP_MS, 200).
-define(EVENT_TIMEOUT_MS, 15_000).
-define(SCENARIO_TIMEOUT_MS, 60_000).

handoff_test_() ->
    [{"bytes queued behind an inbound tunnel's prefix reach the dist controller",
      {timeout, 90, fun inbound_bytes_reach_the_dist_controller/0}},
     {"bytes that arrive before tunnel_ok reach the caller of request_tunnel",
      {timeout, 90, fun outbound_bytes_reach_the_caller/0}}].

inbound_bytes_reach_the_dist_controller() ->
    ?assertEqual({ok, ?FIRST_FRAME}, in_peer(inbound_bytes_queued_behind_the_prefix, [])).

outbound_bytes_reach_the_caller() ->
    ?assertEqual({ok, ?FIRST_FRAME}, in_peer(outbound_bytes_before_tunnel_ok, [])).

%%%===================================================================
%%% Scenarios
%%%===================================================================

inbound_bytes_queued_behind_the_prefix() ->
    #{client := Client, conn := Conn, control := Control} = Relay = identified_client(),
    %% This process stands in for net_kernel, and for the dist controller
    %% it names.
    ok = macula_dist_relay_client:set_kernel(Client, self()),
    TunnelId = tunnel_id(),
    ok = macula_quic:send(Control, encode(#{type => tunnel_notify, tunnel_id => TunnelId,
                                            source => ?PEER})),
    ok = status_reaches(Client, pending_inbound, 1),
    Stream = stream_with_first_prefix_byte(Client, Conn, TunnelId),
    ok = sys:suspend(Client),
    ok = rest_of_prefix_then_first_frame(Stream, TunnelId),
    ok = sys:resume(Client),
    kept(Relay, received_as_dist_controller()).

outbound_bytes_before_tunnel_ok() ->
    #{client := Client, conn := Conn, control := Control} = Relay = identified_client(),
    Scenario = self(),
    _Caller = spawn(fun() ->
                        Tunnel = macula_dist_relay_client:request_tunnel(Client, ?PEER),
                        Scenario ! {tunnel, received_as_caller(Tunnel)}
                    end),
    {[#{type := tunnel_request}], _} = control_messages(Control, <<>>, 1),
    TunnelId = tunnel_id(),
    Stream = stream_with_first_prefix_byte(Client, Conn, TunnelId),
    ok = sys:suspend(Client),
    ok = rest_of_prefix_then_first_frame(Stream, TunnelId),
    ok = macula_quic:send(Control, encode(#{type => tunnel_ok, tunnel_id => TunnelId})),
    timer:sleep(?READ_GAP_MS),
    ok = sys:resume(Client),
    kept(Relay, receive {tunnel, Received} -> Received after ?EVENT_TIMEOUT_MS -> no_tunnel end).

%%%===================================================================
%%% The process that takes the stream
%%%===================================================================

%% As net_kernel: take the accept and name this process the dist
%% controller. Returns the bytes it receives as the stream's owner.
received_as_dist_controller() ->
    receive
        {accept, SetupPid, {_Conn, Stream}, inet, macula_dist} ->
            SetupPid ! {self(), controller, self()},
            handed_over(SetupPid, Stream)
    after ?EVENT_TIMEOUT_MS ->
        not_accepted
    end.

handed_over(SetupPid, Stream) ->
    receive
        {SetupPid, controller, ok, Received} -> bytes_until_quiet(Stream, Received)
    after ?EVENT_TIMEOUT_MS ->
        {not_handed_over, any_bytes_until_quiet(<<>>)}
    end.

received_as_caller({ok, _Conn, Stream, Received}) ->
    bytes_until_quiet(Stream, Received);
received_as_caller(Other) ->
    {no_tunnel, Other, any_bytes_until_quiet(<<>>)}.

%% `Received' followed by the stream's data messages, until none arrives
%% for a while.
bytes_until_quiet(Stream, Received) ->
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            bytes_until_quiet(Stream, <<Received/binary, Data/binary>>)
    after 2 * ?READ_GAP_MS ->
        Received
    end.

%% Every stream's data messages, until none arrives for a while.
any_bytes_until_quiet(Received) ->
    receive
        {quic, Data, _Stream, _Flags} when is_binary(Data) ->
            any_bytes_until_quiet(<<Received/binary, Data/binary>>)
    after 2 * ?READ_GAP_MS ->
        Received
    end.

%%%===================================================================
%%% The relay
%%%===================================================================

%% A client identified by a relay run by this process, which receives the
%% relay side's connection events.
identified_client() ->
    os:putenv("MACULA_TLS_MODE", "development"),
    {Listener, Port} = relay_listener(),
    ok = macula_quic:async_accept(Listener),
    {ok, Client} = macula_dist_relay_client:start_link(relay_url(Port), <<"handoff@127.0.0.1">>),
    Conn = receive_event(new_conn),
    ok = macula_quic:async_accept_stream(Conn),
    Control = receive_event(new_stream),
    ok = macula_quic:setopt(Control, active, true),
    {[#{type := identify}], _} = control_messages(Control, <<>>, 1),
    ok = macula_quic:send(Control, encode(#{type => identified, status => ok})),
    ok = status_reaches(Client, identified, true),
    #{listener => Listener, client => Client, conn => Conn, control => Control}.

%% A tunnel stream the client has taken and read the first byte of the
%% tunnel id from: it waits for the rest.
stream_with_first_prefix_byte(Client, Conn, <<First, _/binary>>) ->
    {ok, Stream} = macula_quic:open_stream(Conn),
    ok = macula_quic:send(Stream, <<First>>),
    ok = status_reaches(Client, unidentified_streams, 1),
    timer:sleep(?READ_GAP_MS),
    Stream.

rest_of_prefix_then_first_frame(Stream, <<_First, Rest/binary>>) ->
    ok = macula_quic:send(Stream, Rest),
    timer:sleep(?READ_GAP_MS),
    ok = macula_quic:send(Stream, ?FIRST_FRAME),
    timer:sleep(?READ_GAP_MS).

relay_listener() ->
    Port = free_udp_port(),
    {ok, Listener} = macula_test_tmp:with_dir("macula-relay-client-handoff",
                                             fun(Dir) -> listener_in(Dir, Port) end),
    {Listener, Port}.

%% A relay listener on `Port' whose certificate and key live in `Dir' while
%% listen reads them.
listener_in(Dir, Port) ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            iolist_to_binary(Pub), iolist_to_binary(Priv), [<<"localhost">>, <<"127.0.0.1">>]),
    Cert = filename:join(Dir, "relay.crt"),
    Key = filename:join(Dir, "relay.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    macula_quic:listen(<<"127.0.0.1">>, Port, [{cert, Cert}, {key, Key}, {alpn, [?RELAY_ALPN]}]).

relay_url(Port) ->
    iolist_to_binary(io_lib:format("quic://127.0.0.1:~p", [Port])).

tunnel_id() ->
    binary:encode_hex(crypto:strong_rand_bytes(16)).

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

%% Waits until status/1 of `Client' reports `Value' for `Key'.
status_reaches(Client, Key, Value) ->
    status_reaches(Client, Key, Value, erlang:monotonic_time(millisecond) + ?EVENT_TIMEOUT_MS).

status_reaches(Client, Key, Value, Deadline) ->
    Status = macula_dist_relay_client:status(Client),
    reached(maps:get(Key, Status) =:= Value, erlang:monotonic_time(millisecond) >= Deadline,
            Status, {Client, Key, Value, Deadline}).

reached(true, _Late, _Status, _Wait) ->
    ok;
reached(false, true, Status, {_Client, Key, _Value, _Deadline}) ->
    {not_reached, Key, Status};
reached(false, false, _Status, {Client, Key, Value, Deadline}) ->
    timer:sleep(10),
    status_reaches(Client, Key, Value, Deadline).

%% Returns Result with the relay's handles referenced until now.
kept(#{listener := _Listener, conn := _Conn}, Result) ->
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
