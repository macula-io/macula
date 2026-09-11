%%%-------------------------------------------------------------------
%%% @doc Tests that a dist relay client forgets a tunnel once it ends.
%%%
%%% A relay run by this test gives the client an outbound tunnel, taken by
%%% the process that requested it, or an inbound tunnel, whose setup process
%%% asks a process standing in for net_kernel for a dist controller. When
%%% the process that holds a tunnel ends, normally or killed, or when the
%%% inbound setup process dies before naming a controller, the client drops
%%% the tunnel within a second: status/1 reports no active tunnels, and the
%%% relay receives exactly one tunnel_close for it. A tunnel closed with
%%% close_tunnel/2 whose caller then ends is closed once too. The scenarios
%%% run in a peer node of their own.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_relay_client_tunnel_end_tests).

-include_lib("eunit/include/eunit.hrl").

%% Scenarios, run in a peer node.
-export([outbound_tunnel_whose_caller_ends/0,
         outbound_tunnel_whose_caller_is_killed/0,
         outbound_tunnel_closed_then_its_caller_ends/0,
         inbound_tunnel_whose_controller_ends/0,
         inbound_tunnel_whose_setup_dies_before_the_controller/0]).

-define(RELAY_ALPN, <<"macula-dist">>).
-define(PEER, <<"peer@127.0.0.1">>).
%% How long the client may take to drop a tunnel whose holder ended.
-define(END_MS, 1_000).
-define(EVENT_TIMEOUT_MS, 15_000).
-define(SCENARIO_TIMEOUT_MS, 60_000).

tunnel_end_test_() ->
    [{"an outbound tunnel ends with the process that requested it",
      {timeout, 90, fun outbound_tunnel_ends_with_its_caller/0}},
     {"an outbound tunnel ends when the process that requested it is killed",
      {timeout, 90, fun outbound_tunnel_ends_when_its_caller_is_killed/0}},
     {"a tunnel closed with close_tunnel/2 and then left by its caller is closed once",
      {timeout, 90, fun closed_tunnel_is_closed_once/0}},
     {"an inbound tunnel ends with its dist controller",
      {timeout, 90, fun inbound_tunnel_ends_with_its_controller/0}},
     {"an inbound tunnel ends when its setup process dies before naming a controller",
      {timeout, 90, fun inbound_tunnel_ends_with_its_setup/0}}].

outbound_tunnel_ends_with_its_caller() ->
    ?assertEqual({ok, {tunnel_dropped, one_tunnel_close}},
                 in_peer(outbound_tunnel_whose_caller_ends, [])).

outbound_tunnel_ends_when_its_caller_is_killed() ->
    ?assertEqual({ok, {tunnel_dropped, one_tunnel_close}},
                 in_peer(outbound_tunnel_whose_caller_is_killed, [])).

closed_tunnel_is_closed_once() ->
    ?assertEqual({ok, {tunnel_dropped, one_tunnel_close}},
                 in_peer(outbound_tunnel_closed_then_its_caller_ends, [])).

inbound_tunnel_ends_with_its_controller() ->
    ?assertEqual({ok, {tunnel_dropped, one_tunnel_close}},
                 in_peer(inbound_tunnel_whose_controller_ends, [])).

inbound_tunnel_ends_with_its_setup() ->
    ?assertEqual({ok, {tunnel_dropped, one_tunnel_close}},
                 in_peer(inbound_tunnel_whose_setup_dies_before_the_controller, [])).

%%%===================================================================
%%% Scenarios
%%%===================================================================

outbound_tunnel_whose_caller_ends() ->
    outbound(fun(#{caller := Caller}) -> Caller ! stop end).

outbound_tunnel_whose_caller_is_killed() ->
    outbound(fun(#{caller := Caller}) -> exit(Caller, kill) end).

outbound_tunnel_closed_then_its_caller_ends() ->
    outbound(fun(#{client := Client, caller := Caller, tunnel_id := TunnelId}) ->
                 ok = macula_dist_relay_client:close_tunnel(Client, TunnelId),
                 Caller ! stop
             end).

inbound_tunnel_whose_controller_ends() ->
    inbound(fun(SetupPid, Client) ->
                Controller = spawn(fun() -> controller(SetupPid) end),
                SetupPid ! {self(), controller, Controller},
                ok = status_reaches(Client, active_tunnels, 1, ?EVENT_TIMEOUT_MS),
                Controller ! stop
            end).

inbound_tunnel_whose_setup_dies_before_the_controller() ->
    inbound(fun(SetupPid, _Client) -> exit(SetupPid, kill) end).

%% An outbound tunnel taken by a caller process, then ended by `End'.
outbound(End) ->
    #{client := Client, conn := Conn, control := Control} = Relay = identified_client(),
    Scenario = self(),
    Caller = spawn(fun() -> caller(Scenario, Client) end),
    {[#{type := tunnel_request}], Rest} = control_messages(Control, <<>>, 1),
    TunnelId = tunnel_id(),
    _Stream = prefixed_stream(Conn, TunnelId),
    ok = macula_quic:send(Control, encode(#{type => tunnel_ok, tunnel_id => TunnelId})),
    {ok, _Conn, _ClientStream, _Received} =
        receive {tunnel, Tunnel} -> Tunnel after ?EVENT_TIMEOUT_MS -> no_tunnel end,
    ok = status_reaches(Client, active_tunnels, 1, ?EVENT_TIMEOUT_MS),
    _ = End(#{client => Client, caller => Caller, tunnel_id => TunnelId}),
    kept(Relay, ended(Client, Control, Rest, TunnelId)).

%% An inbound tunnel whose setup process asks this process, standing in for
%% net_kernel, for a controller. `Accept' answers the setup process, or not,
%% and ends the tunnel.
inbound(Accept) ->
    #{client := Client, conn := Conn, control := Control} = Relay = identified_client(),
    ok = macula_dist_relay_client:set_kernel(Client, self()),
    TunnelId = tunnel_id(),
    ok = macula_quic:send(Control, encode(#{type => tunnel_notify, tunnel_id => TunnelId,
                                            source => ?PEER})),
    ok = status_reaches(Client, pending_inbound, 1, ?EVENT_TIMEOUT_MS),
    _Stream = prefixed_stream(Conn, TunnelId),
    receive
        {accept, SetupPid, _Socket, inet, macula_dist} -> _ = Accept(SetupPid, Client)
    after ?EVENT_TIMEOUT_MS ->
        error(not_accepted)
    end,
    kept(Relay, ended(Client, Control, <<>>, TunnelId)).

%%%===================================================================
%%% The processes that hold a tunnel
%%%===================================================================

caller(Scenario, Client) ->
    Scenario ! {tunnel, macula_dist_relay_client:request_tunnel(Client, ?PEER)},
    receive stop -> ok end.

controller(SetupPid) ->
    receive
        {SetupPid, controller, ok, _Received} -> ok
    end,
    receive stop -> ok end.

%% Whether the client dropped the tunnel within END_MS of it ending, and
%% how many tunnel_close frames for it the relay read.
ended(Client, Control, Rest, TunnelId) ->
    {dropped(status_reaches(Client, active_tunnels, 0, ?END_MS)),
     closes(length([Id || Id <- tunnel_closes(Control, Rest, ?END_MS), Id =:= TunnelId]))}.

dropped(ok) -> tunnel_dropped;
dropped(NotReached) -> NotReached.

closes(1) -> one_tunnel_close;
closes(Count) -> {tunnel_closes, Count}.

%% The tunnel ids of the tunnel_close frames the relay reads within `Ms'.
tunnel_closes(Control, Rest, Ms) ->
    {Messages, _} = messages_within(Control, macula_dist_relay_protocol:decode_buffer(Rest),
                                    erlang:monotonic_time(millisecond) + Ms, []),
    [TunnelId || #{type := tunnel_close, tunnel_id := TunnelId} <- Messages].

messages_within(Control, {Messages, Rest}, Deadline, Acc) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {quic, Data, Control, _Flags} when is_binary(Data) ->
            messages_within(Control,
                            macula_dist_relay_protocol:decode_buffer(<<Rest/binary, Data/binary>>),
                            Deadline, lists:reverse(Messages, Acc))
    after Left ->
        {lists:reverse(Acc, Messages), Rest}
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
    {ok, Client} = macula_dist_relay_client:start_link(relay_url(Port), <<"tunnel_end@127.0.0.1">>),
    Conn = receive_event(new_conn),
    ok = macula_quic:async_accept_stream(Conn),
    Control = receive_event(new_stream),
    ok = macula_quic:setopt(Control, active, true),
    {[#{type := identify}], _} = control_messages(Control, <<>>, 1),
    ok = macula_quic:send(Control, encode(#{type => identified, status => ok})),
    ok = status_reaches(Client, identified, true, ?EVENT_TIMEOUT_MS),
    #{listener => Listener, client => Client, conn => Conn, control => Control}.

prefixed_stream(Conn, TunnelId) ->
    {ok, Stream} = macula_quic:open_stream(Conn),
    ok = macula_quic:send(Stream, TunnelId),
    Stream.

relay_listener() ->
    Port = free_udp_port(),
    {ok, Listener} = macula_test_tmp:with_dir("macula-relay-client-tunnel-end",
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

%% Waits up to `Ms' until status/1 of `Client' reports `Value' for `Key'.
status_reaches(Client, Key, Value, Ms) ->
    status_until(Client, Key, Value, erlang:monotonic_time(millisecond) + Ms).

status_until(Client, Key, Value, Deadline) ->
    Status = macula_dist_relay_client:status(Client),
    reached(maps:get(Key, Status) =:= Value, erlang:monotonic_time(millisecond) >= Deadline,
            Status, {Client, Key, Value, Deadline}).

reached(true, _Late, _Status, _Wait) ->
    ok;
reached(false, true, Status, {_Client, Key, _Value, _Deadline}) ->
    {not_reached, Key, maps:get(Key, Status)};
reached(false, false, _Status, {Client, Key, Value, Deadline}) ->
    timer:sleep(10),
    status_until(Client, Key, Value, Deadline).

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
