%% EUnit tests for macula_dist_tunnel_socket: the socket a distribution tunnel's TLS session runs on.
%%
%% D29 carries a distribution tunnel's data inside a TLS 1.3 session run end to end between the two nodes, because
%% the carrier underneath — a relay, or stations on the pool path — forwards the bytes and is not trusted with them.
%% OTP's `ssl' speaks to whatever transport a `cb_info' names, so this module makes a macula QUIC stream look like a
%% socket to it: owning the stream, turning its events into the tags `ssl' expects, and answering `recv' when `ssl'
%% is passive.
%%
%% The stream is real here, and so is the certificate: one made by `macula_quic:generate_self_signed_cert/2', which
%% is ML-DSA-87 with a key in RFC 9881's seed form. That combination is the thing measured before this was built —
%% OTP's TLS 1.3 signs a CertificateVerify with it — and it is why D29 needs no fallback.
-module(macula_dist_tunnel_socket_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EVENT_MS, 10_000).
-define(CB_INFO, macula_dist_tunnel_socket:cb_info()).

tunnel_socket_test_() ->
    {timeout, 120,
     {setup,
      fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
      fun(ok) -> ok end,
      [{"a TLS 1.3 session runs over a macula stream and carries bytes both ways",
        fun a_tls_session_carries_bytes_both_ways/0},
       {"the session is TLS 1.3 and no other version",
        fun the_session_is_tls_13/0},
       {"a passive read of a length waits until all of it has arrived",
        fun a_read_of_a_length_waits_for_all_of_it/0},
       {"the socket ends when its owner does, and the stream with it",
        fun the_socket_ends_with_its_owner/0},
       {"a spent run of active reads says so and then stays quiet",
        fun a_spent_run_says_so_and_stays_quiet/0}]}}.

%% The tunnel's whole point: what the two nodes say to each other crosses a stream that something else forwards.
a_tls_session_carries_bytes_both_ways() ->
    with_session(fun(#{client := Client, server := Server}) ->
        ok = ssl:send(Client, <<"dist bytes out">>),
        ?assertEqual({ok, <<"dist bytes out">>}, ssl:recv(Server, 0, ?EVENT_MS)),
        ok = ssl:send(Server, <<"dist bytes back">>),
        ?assertEqual({ok, <<"dist bytes back">>}, ssl:recv(Client, 0, ?EVENT_MS))
    end).

the_session_is_tls_13() ->
    with_session(fun(#{client := Client}) ->
        ?assertEqual({ok, [{protocol, 'tlsv1.3'}]},
                     ssl:connection_information(Client, [protocol]))
    end).

%% `ssl' reads a record's header and then its body by exact length, so a short read must wait rather than answer
%% with what it has. The bytes are sent in two pieces with the reader already waiting, which is the case a socket
%% that answered early would get wrong.
a_read_of_a_length_waits_for_all_of_it() ->
    Pair = pair(),
    try
        #{client_stream := ClientStream, server_stream := ServerStream} = Pair,
        {ok, Socket} = macula_dist_tunnel_socket:own(ServerStream),
        Reader = self(),
        spawn_link(fun() -> Reader ! {read, macula_dist_tunnel_socket:recv(Socket, 10, ?EVENT_MS)} end),
        ok = macula_quic:send(ClientStream, <<"12345">>),
        ?assertEqual(nothing_yet, read_result(200)),
        ok = macula_quic:send(ClientStream, <<"67890">>),
        ?assertEqual({ok, <<"1234567890">>}, read_result(?EVENT_MS))
    after
        stop_pair(Pair)
    end.

%% The socket serves one owner. When that owner goes, so does the socket, and the stream it held is closed rather
%% than left for whatever the far side still wants to write into it.
the_socket_ends_with_its_owner() ->
    Pair = pair(),
    try
        #{server_stream := ServerStream} = Pair,
        Parent = self(),
        Owner = spawn(fun() ->
                          {ok, Socket} = macula_dist_tunnel_socket:own(ServerStream),
                          Parent ! {socket, Socket},
                          receive stop -> ok end
                      end),
        Socket = receive {socket, S} -> S after ?EVENT_MS -> error(no_socket) end,
        Monitor = erlang:monitor(process, Socket),
        Owner ! stop,
        receive {'DOWN', Monitor, process, Socket, _Reason} -> ok
        after ?EVENT_MS -> error(socket_outlived_its_owner) end,
        ?assertEqual({error, closed}, macula_dist_tunnel_socket:send(Socket, <<"after">>))
    after
        stop_pair(Pair)
    end.

read_result(Ms) ->
    receive {read, Result} -> Result after Ms -> nothing_yet end.

%% `ssl' reads with `{active, N}' and re-arms when the run is spent, so a socket that never says the run ended
%% leaves it waiting for a message that is not coming. The session stalls, and it stalls after the handshake, which
%% is why no test that only forms a session catches it: one count of 100 covers a handshake and then some.
%%
%% One count and one write, so the test does not depend on whether two writes arrive as one event.
a_spent_run_says_so_and_stays_quiet() ->
    Pair = pair(),
    try
        #{client_stream := ClientStream, server_stream := ServerStream} = Pair,
        {ok, Socket} = macula_dist_tunnel_socket:own(ServerStream),
        ok = macula_dist_tunnel_socket:setopts(Socket, [{active, 1}]),
        ok = macula_quic:send(ClientStream, <<"first">>),
        ?assertEqual({macula_tunnel, Socket, <<"first">>}, next_message(?EVENT_MS)),
        ?assertEqual({macula_tunnel_passive, Socket}, next_message(?EVENT_MS)),
        %% The run is over, so what comes next waits in the socket rather than arriving unasked.
        ok = macula_quic:send(ClientStream, <<"second">>),
        ?assertEqual(nothing_yet, next_message(200)),
        %% ...and it is still there to be read.
        ?assertEqual({ok, <<"second">>}, macula_dist_tunnel_socket:recv(Socket, 0, ?EVENT_MS))
    after
        stop_pair(Pair)
    end.

next_message(Ms) ->
    receive Message -> Message after Ms -> nothing_yet end.

%%%===================================================================
%%% A TLS session over a loopback macula stream pair
%%%===================================================================

with_session(Fun) ->
    Pair = pair(),
    try
        Session = session(Pair),
        try
            Fun(Session)
        after
            _ = ssl:close(maps:get(client, Session)),
            _ = ssl:close(maps:get(server, Session))
        end
    after
        stop_pair(Pair)
    end.

%% The accepting side takes the station role, with the TLS key and certificate a station presents (D29); the dialing
%% side checks the leaf itself, since nothing issues an ML-DSA-87 certificate for a chain to end at.
session(#{client_stream := ClientStream, server_stream := ServerStream, cert := Cert, key := Key}) ->
    {ok, ClientSocket} = macula_dist_tunnel_socket:own(ClientStream),
    {ok, ServerSocket} = macula_dist_tunnel_socket:own(ServerStream),
    Parent = self(),
    Accepting = spawn_link(fun() -> accept_session(ServerSocket, {Cert, Key}, Parent) end),
    ok = macula_dist_tunnel_socket:controlling_process(ServerSocket, Accepting),
    Accepting ! go,
    {ok, Client} = ssl:connect(ClientSocket, client_opts(), ?EVENT_MS),
    Server = receive {accepted, S} -> S after ?EVENT_MS -> error(no_server_session) end,
    #{client => Client, server => Server, accepting => Accepting}.

%% An ssl socket closes when its owner exits, so the accepting process stays until the test is done with it.
accept_session(Socket, Files, Parent) ->
    receive go -> ok after ?EVENT_MS -> error(no_go) end,
    {ok, Server} = ssl:handshake(Socket, server_opts(Files), ?EVENT_MS),
    ok = ssl:controlling_process(Server, Parent),
    Parent ! {accepted, Server},
    receive done -> ok after ?EVENT_MS * 6 -> ok end.

server_opts({Cert, Key}) ->
    [binary, {active, false}, {versions, ['tlsv1.3']}, {cb_info, ?CB_INFO},
     {certfile, Cert}, {keyfile, Key}].

client_opts() ->
    [binary, {active, false}, {versions, ['tlsv1.3']}, {cb_info, ?CB_INFO},
     {verify, verify_peer}, {cacerts, []},
     {verify_fun, {fun leaf_seen/3, []}},
     {customize_hostname_check, [{match_fun, fun(_Ref, _Presented) -> true end}]},
     {server_name_indication, disable}].

%% The connection handshake inside the tunnel is what judges the leaf (D29, WP 1.5). Here it is only kept, so the
%% session can form over a certificate no authority issued.
leaf_seen(_Cert, {bad_cert, selfsigned_peer_cert}, State) -> {valid, State};
leaf_seen(_Cert, {bad_cert, _Reason}, State) -> {valid, State};
leaf_seen(_Cert, {extension, _Ext}, State) -> {unknown, State};
leaf_seen(_Cert, valid, State) -> {valid, State};
leaf_seen(_Cert, valid_peer, State) -> {valid, State}.

%%%===================================================================
%%% The loopback pair
%%%===================================================================

pair() ->
    macula_test_quic_pair:open("macula-tunnel-socket").

stop_pair(Pair) ->
    macula_test_quic_pair:close(Pair).
