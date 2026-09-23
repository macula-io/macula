%% Does a macula handshake complete when its datagrams are dropped?
%%
%% WHY THIS EXISTS: msquic hardcodes classical key exchange groups, and its own comment says why — ML-KEM key
%% shares make the Client/Server hello span multiple UDP datagrams, and their tests "hit a buffer space assertion
%% failure" or "time out on loss recovery". Our stations offer ML-KEM-1024 hybrids and nothing else, so ours is
%% the large handshake. The question is not whether QUIC handles loss in general; it is whether OUR handshake
%% completes when a datagram carrying it is dropped, deliberately and repeatedly.
%%
%% THE INSTRUMENT: a user-space UDP relay between client and listener. No root, unlike netem, and DETERMINISTIC:
%% it drops the Nth datagram rather than a percentage, which is what "the second datagram of a hello that spans
%% two" requires. It sees every datagram, so it measures the sizes on the wire in the same run that tests loss.
-module(lossy_handshake).

-export([run/0, once/1, once/2]).

-define(EVENT_MS, 20_000).

run() ->
    {ok, _} = application:ensure_all_started(macula),
    io:format("~n=== no loss: sizes on the wire ===~n"),
    report(once(none)),
    [begin
         io:format("~n=== drop datagram ~p from the client ===~n", [N]),
         report(once({drop_nth_from_client, N}))
     end || N <- [1, 2, 3]],
    [begin
         io:format("~n=== drop one datagram in ~p, both directions ===~n", [R]),
         report(once({drop_one_in, R}))
     end || R <- [4, 3, 2]],
    ok.

report(#{outcome := Outcome, from_client := C, from_server := S, dropped := D,
         sizes_client := SC, sizes_server := SS}) ->
    io:format("  outcome:        ~p~n", [Outcome]),
    io:format("  datagrams:      client ~p, server ~p, dropped ~p~n", [C, S, D]),
    io:format("  client sizes:   ~p~n", [lists:sublist(SC, 8)]),
    io:format("  server sizes:   ~p~n", [lists:sublist(SS, 8)]),
    io:format("  client total:   ~p bytes in ~p datagrams~n", [lists:sum(SC), length(SC)]),
    io:format("  server total:   ~p bytes in ~p datagrams~n", [lists:sum(SS), length(SS)]).

%% One handshake under one policy, torn down afterwards.
once(Policy) -> once(Policy, ?EVENT_MS).

once(Policy, Deadline) ->
    Dir = filename:join(os:getenv("TMPDIR", "/tmp"),
                        "macula-lossy-handshake-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = filelib:ensure_path(Dir),
    try
        {Listener, ListenPort} = listener(Dir),
        {Relay, RelayPort} = relay(ListenPort, Policy),
        Outcome = dial(RelayPort, Deadline),
        Stats = stats(Relay),
        Relay ! stop,
        _ = macula_quic:close(Listener),
        Stats#{outcome => Outcome}
    after
        _ = file:del_dir_r(Dir)
    end.

dial(Port, Deadline) ->
    outcome(macula_quic:connect(<<"127.0.0.1">>, Port, [{alpn, [<<"macula">>]}], Deadline)).

outcome({ok, Conn}) ->
    _ = macula_quic:close_connection(Conn),
    connected;
outcome({error, Reason}) ->
    {refused, Reason}.

listener(Dir) ->
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(crypto:strong_rand_bytes(32), [<<"127.0.0.1">>]),
    Cert = filename:join(Dir, "l.crt"),
    Key = filename:join(Dir, "l.key"),
    ok = file:write_file(Cert, CertPem),
    ok = file:write_file(Key, KeyPem),
    Port = free_udp_port(),
    {ok, L} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                 [{cert, Cert}, {key, Key}, {alpn, [<<"macula">>]}]),
    ok = macula_quic:async_accept(L),
    {L, Port}.

%%------------------------------------------------------------------
%% The relay
%%------------------------------------------------------------------

relay(ListenPort, Policy) ->
    Port = free_udp_port(),
    Owner = self(),
    Pid = spawn_link(fun() ->
        {ok, Socket} = gen_udp:open(Port, [binary, {active, true}, {ip, {127, 0, 0, 1}}]),
        Owner ! {relay_ready, self()},
        loop(#{socket => Socket, listen_port => ListenPort, policy => Policy, client => undefined,
               from_client => 0, from_server => 0, dropped => 0, sizes_client => [], sizes_server => []})
    end),
    receive {relay_ready, Pid} -> ok after 5_000 -> error(relay_did_not_start) end,
    {Pid, Port}.

loop(State) ->
    receive
        {udp, _Socket, Ip, Port, Data} -> loop(forwarded(Ip, Port, Data, State));
        {stats, From} -> From ! {stats, State}, loop(State);
        stop -> ok = gen_udp:close(maps:get(socket, State))
    end.

%% A datagram whose source is the listener's port is going back to the client; anything else is going to it.
forwarded(_Ip, Port, Data, #{listen_port := Port, client := Client} = S) ->
    delivered(Client, Data, server, S);
forwarded(Ip, Port, Data, #{listen_port := ListenPort} = S) ->
    delivered({{127, 0, 0, 1}, ListenPort}, Data, client, S#{client => {Ip, Port}}).

delivered(To, Data, Direction, S0) ->
    S = counted(Direction, byte_size(Data), S0),
    written(drop(Direction, S), To, Data, S).

written(true, _To, _Data, S) ->
    maps:update_with(dropped, fun(V) -> V + 1 end, S);
written(false, {Ip, Port}, Data, #{socket := Socket} = S) ->
    ok = gen_udp:send(Socket, Ip, Port, Data),
    S.

counted(client, Size, #{from_client := N, sizes_client := Sizes} = S) ->
    S#{from_client => N + 1, sizes_client => Sizes ++ [Size]};
counted(server, Size, #{from_server := N, sizes_server := Sizes} = S) ->
    S#{from_server => N + 1, sizes_server => Sizes ++ [Size]}.

%% The Nth datagram from the client, counted after it was counted, so N=1 is the very first.
drop(client, #{policy := {drop_nth_from_client, N}, from_client := N}) -> true;
drop(_Direction, #{policy := {drop_one_in, Rate}, from_client := C, from_server := Srv})
  when (C + Srv) rem Rate =:= 0 -> true;
drop(_Direction, _State) -> false.

stats(Relay) ->
    Relay ! {stats, self()},
    receive {stats, S} -> maps:with([from_client, from_server, dropped, sizes_client, sizes_server], S)
    after 5_000 -> error(no_stats) end.

free_udp_port() ->
    {ok, S} = gen_udp:open(0, [{ip, {127, 0, 0, 1}}]),
    {ok, P} = inet:port(S),
    ok = gen_udp:close(S),
    P.
