%%%-------------------------------------------------------------------
%%% @doc Tests for how macula_peering_conn handles a peer that sends bytes
%%% that do not decode as frames, or frames with invalid fields.
%%%
%%% A server-role connection is driven by a raw QUIC peer on a loopback
%%% listener. During the handshake, before the peer has authenticated, and
%%% on the control stream once connected, a complete frame that is not CBOR
%%% ends the connection with the reason `{malformed, bad_frame}', and a
%%% length header above the frame cap ends it with
%%% `{malformed, frame_too_large}' as soon as the header arrives. During the
%%% handshake the cap is the handshake frame cap, 64 KiB, and a CONNECT of
%%% exactly that size is still read. The worker stops without waiting for
%%% more data or for the handshake timeout.
%%%
%%% A frame whose framing is intact but whose fields are invalid ends the
%%% connection during the handshake, with the reason
%%% `{malformed, {invalid_frame, Type, Field}}'. On the control stream, which
%%% carries frames a station relays for others, that frame is dropped, the
%%% controlling process is told `{macula_peering, invalid_frame, Pid, Type,
%%% Field}', and the frames after it are served. A frame of a type this node
%%% does not know, whether its name is an atom here or not, leaves the
%%% connection serving.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_malformed_tests).

-include_lib("eunit/include/eunit.hrl").

%% The frame cap in macula_frame, 16 MiB.
-define(MAX_FRAME_BYTES, 16#FFFFFF).
%% The handshake frame cap in macula_peering_conn, 64 KiB.
-define(HANDSHAKE_FRAME_BYTES, 64 * 1024).
-define(NOT_CBOR, <<10:32/big, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>).
%% Well within the 30 s handshake timeout.
-define(PROMPT_MS, 3_000).
-define(EVENT_MS, 5_000).
%% A frame type name that is not an atom anywhere, so it never decodes to one.
-define(NON_ATOM_TYPE, <<"zz_frame_type_that_is_no_atom">>).

malformed_test_() ->
    {timeout, 240,
     {setup, fun setup/0, fun cleanup/1,
      fun(Ctx) ->
          [{"a handshake frame that is not CBOR, followed by more data, ends the connection",
            {timeout, 30, fun() -> bad_handshake_frame(Ctx) end}},
           {"a handshake length header above the cap ends the connection from the header",
            {timeout, 30, fun() -> oversize_handshake_header(Ctx) end}},
           {"a handshake length header just over the handshake cap ends the connection from the header",
            {timeout, 30, fun() -> handshake_header_over_the_handshake_cap(Ctx) end}},
           {"a signed CONNECT exactly at the handshake cap completes the handshake",
            {timeout, 30, fun() -> connect_at_the_handshake_cap(Ctx) end}},
           {"a control stream frame that is not CBOR, followed by more data, ends the connection",
            {timeout, 30, fun() -> bad_control_frame(Ctx) end}},
           {"a control stream length header above the cap ends the connection from the header",
            {timeout, 30, fun() -> oversize_control_header(Ctx) end}},
           {"a signed CONNECT without puzzle_evidence ends the connection as an invalid frame",
            {timeout, 30, fun() -> invalid_handshake_frame(Ctx) end}},
           {"a CALL without caller on the control stream is dropped and the next frame is served",
            {timeout, 30, fun() -> invalid_control_frame_dropped(Ctx) end}},
           {"a frame of an unknown type on the control stream leaves the connection serving",
            {timeout, 30, fun() -> unknown_control_frame_passes(Ctx) end}},
           {"frames whose type is no atom here, as text or bytes, leave the connection serving",
            {timeout, 30, fun() -> non_atom_control_frames_pass(Ctx) end}},
           {"a STORE whose record does not decode is dropped as invalid and the next frame is served",
            {timeout, 30, fun() -> undecodable_record_refused(Ctx, store) end}},
           {"a REPLICATE whose record does not decode is dropped as invalid and the next frame is served",
            {timeout, 30, fun() -> undecodable_record_refused(Ctx, replicate) end}},
           {"a VALUE whose records do not decode is dropped as invalid and the next frame is served",
            {timeout, 30, fun() -> undecodable_record_refused(Ctx, value) end}}]
      end}}.

%%%===================================================================
%%% Scenarios
%%%===================================================================

bad_handshake_frame(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, ?NOT_CBOR),
        _ = macula_quic:send(Stream, binary:copy(<<0>>, 65_536)),
        ?assertEqual({malformed, bad_frame}, ended(Server))
    end).

oversize_handshake_header(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, <<(?MAX_FRAME_BYTES + 1):32/big>>),
        ?assertEqual({malformed, frame_too_large}, ended(Server))
    end).

handshake_header_over_the_handshake_cap(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, <<(?HANDSHAKE_FRAME_BYTES + 1):32/big>>),
        ?assertEqual({malformed, frame_too_large}, ended(Server))
    end).

connect_at_the_handshake_cap(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        Wire = signed_connect_of_size(?HANDSHAKE_FRAME_BYTES),
        ?assertEqual(?HANDSHAKE_FRAME_BYTES + 4, byte_size(Wire)),
        ok = macula_quic:send(Stream, Wire),
        ?assertEqual(ok, connected(Server))
    end).

bad_control_frame(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, signed_connect()),
        connected(Server),
        ok = macula_quic:send(Stream, ?NOT_CBOR),
        _ = macula_quic:send(Stream, binary:copy(<<0>>, 65_536)),
        ?assertEqual({malformed, bad_frame}, ended(Server))
    end).

oversize_control_header(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, signed_connect()),
        connected(Server),
        ok = macula_quic:send(Stream, <<(?MAX_FRAME_BYTES + 1):32/big>>),
        ?assertEqual({malformed, frame_too_large}, ended(Server))
    end).

invalid_handshake_frame(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, signed_connect_without(puzzle_evidence)),
        ?assertEqual({malformed, {invalid_frame, connect, puzzle_evidence}}, ended(Server))
    end).

invalid_control_frame_dropped(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, signed_connect()),
        connected(Server),
        ok = macula_quic:send(Stream, encode_signed(maps:remove(caller, call_frame()))),
        Next = call_frame(),
        ok = macula_quic:send(Stream, encode_signed(Next)),
        Reported = invalid_reported(Server),
        Served = served(Server, Next),
        ?assertEqual({{invalid_frame, call, caller}, served, running},
                     {Reported, Served, running(Server)})
    end).

unknown_control_frame_passes(Ctx) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, signed_connect()),
        connected(Server),
        ok = macula_quic:send(Stream, encode_signed((call_frame())#{frame_type => zz_future_frame})),
        Next = call_frame(),
        ok = macula_quic:send(Stream, encode_signed(Next)),
        Served = served(Server, Next),
        ?assertEqual({served, running}, {Served, running(Server)})
    end).

%% A type name that is no atom on this node decodes as `{text, Name}' when
%% sent as a text string, and as a plain binary when sent as bytes.
non_atom_control_frames_pass(Ctx) ->
    ?assertError(badarg, binary_to_existing_atom(?NON_ATOM_TYPE)),
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, signed_connect()),
        connected(Server),
        AsText = (call_frame())#{frame_type => {text, ?NON_ATOM_TYPE}},
        AsBytes = (call_frame())#{frame_type => ?NON_ATOM_TYPE},
        ok = macula_quic:send(Stream, encode_signed(AsText)),
        ok = macula_quic:send(Stream, encode_signed(AsBytes)),
        Next = call_frame(),
        ok = macula_quic:send(Stream, encode_signed(Next)),
        Served = served(Server, Next),
        ?assertEqual({served, running}, {Served, running(Server)})
    end).

%% A STORE, REPLICATE or VALUE whose record bytes do not decode as a record,
%% first bytes that are not CBOR and then an array over the element budget,
%% followed by a CALL.
undecodable_record_refused(Ctx, Type) ->
    with_server(Ctx, fun(Server, Stream) ->
        ok = macula_quic:send(Stream, signed_connect()),
        connected(Server),
        {Field, Frame} = record_frame(Type),
        ok = macula_quic:send(Stream, wire_with(Frame, Field, field_value(Field, not_cbor_record()))),
        ok = macula_quic:send(Stream, wire_with(Frame, Field, field_value(Field, over_budget_array()))),
        Next = call_frame(),
        ok = macula_quic:send(Stream, encode_signed(Next)),
        Reported = [invalid_reported(Server), invalid_reported(Server)],
        Served = served(Server, Next),
        Invalid = {invalid_frame, Type, Field},
        ?assertEqual({[Invalid, Invalid], served, not_forwarded, running},
                     {Reported, Served, forwarded(Server, Type), running(Server)})
    end).

%%%===================================================================
%%% Record frames
%%%===================================================================

%% The record field of Type, and a frame of that type holding a signed record.
record_frame(store) ->
    {record, macula_frame:store(#{record => sample_record()})};
record_frame(replicate) ->
    {record, macula_frame:replicate(#{record => sample_record(), new_custodian => false})};
record_frame(value) ->
    {records, macula_frame:value(#{key => crypto:strong_rand_bytes(32),
                                   records => [sample_record()]})}.

sample_record() ->
    Kp = macula_identity:generate(),
    macula_record:sign(macula_record:node_record(macula_identity:public(Kp), [], 0), Kp).

field_value(records, Bytes) -> [Bytes];
field_value(record, Bytes) -> Bytes.

%% Record bytes that are not CBOR.
not_cbor_record() ->
    <<255, 255, 255, 255>>.

%% An array of as many zeros as the element budget of
%% macula_cbor_nif:unpack_deterministic/1, 131,072: with the array itself,
%% one item more than the budget allows.
over_budget_array() ->
    Budget = 131072,
    <<16#9A, Budget:32/big, (binary:copy(<<0>>, Budget))/binary>>.

%% Frame on the wire with Field holding Value, which a builder would not put
%% there: the signed frame is encoded, and its field replaced in the CBOR map.
wire_with(Frame, Field, Value) ->
    <<_Len:32/big, Body/binary>> = encode_signed(Frame),
    Map = macula_cbor_nif:unpack_deterministic(Body),
    Bytes = macula_cbor_nif:pack_deterministic(Map#{{text, atom_to_binary(Field)} := Value}),
    <<(byte_size(Bytes)):32/big, Bytes/binary>>.

%% Whether Server has delivered a frame of Type, of the messages already here.
forwarded(Server, Type) ->
    receive
        {macula_peering, frame, Server, #{frame_type := Type}} -> forwarded;
        {macula_peering, dht_frame, Server, _NodeId, #{frame_type := Type}} -> forwarded
    after 0 ->
        not_forwarded
    end.

%%%===================================================================
%%% Fixture
%%%===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(macula),
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {CertPem, KeyPem}} =
        macula_quic:generate_self_signed_cert(
            iolist_to_binary(Pub), iolist_to_binary(Priv), [<<"localhost">>, <<"127.0.0.1">>]),
    #{cert_pem => CertPem, key_pem => KeyPem}.

cleanup(_Ctx) ->
    ok.

%% Runs Scenario with a server-role connection accepted from a raw QUIC
%% peer, and that peer's open stream. The listener is made in the test
%% process, which owns it and receives its connections.
with_server(#{cert_pem := CertPem, key_pem := KeyPem}, Scenario) ->
    macula_test_tmp:with_dir("macula-peering-malformed", fun(Dir) ->
        Cert = filename:join(Dir, "listener.crt"),
        Key = filename:join(Dir, "listener.key"),
        ok = file:write_file(Cert, CertPem),
        ok = file:write_file(Key, KeyPem),
        Port = free_udp_port(),
        {ok, Listener} = macula_quic:listen(<<"127.0.0.1">>, Port,
                                            [{cert, Cert}, {key, Key},
                                             {alpn, [<<"macula">>]},
                                             {idle_timeout_ms, 30_000},
                                             {keep_alive_interval_ms, 5_000}]),
        ok = macula_quic:async_accept(Listener),
        {ok, PeerConn} = macula_quic:connect("127.0.0.1", Port,
                                             [{alpn, [<<"macula">>]}, {verify, none}],
                                             ?EVENT_MS),
        {ok, Server} = macula_peering:accept(inbound_conn(), server_opts()),
        {ok, Stream} = macula_quic:open_stream(PeerConn),
        try
            Scenario(Server, Stream)
        after
            _ = (catch macula_peering:close(Server, test_cleanup)),
            _ = (catch macula_quic:close(PeerConn)),
            _ = (catch macula_quic:close_listener(Listener)),
            drain()
        end
    end).

inbound_conn() ->
    receive
        {quic, new_conn, Conn, _Info} -> Conn
    after ?EVENT_MS ->
        erlang:error(no_inbound_conn)
    end.

server_opts() ->
    #{identity        => macula_identity:generate(),
      realms          => [],
      capabilities    => 0,
      controlling_pid => self()}.

%% The CONNECT a peer sends to open the handshake.
signed_connect() ->
    Kp = macula_identity:generate(),
    macula_frame:encode(macula_frame:sign(connect_frame(macula_identity:public(Kp)), Kp)).

%% The same CONNECT without Key, signed as it is sent.
signed_connect_without(Key) ->
    Kp = macula_identity:generate(),
    Connect = maps:remove(Key, connect_frame(macula_identity:public(Kp))),
    macula_frame:encode(macula_frame:sign(Connect, Kp)).

%% A signed CONNECT whose body is exactly Bytes long. A padding field the
%% handshake does not read makes up the size: its byte string header grows
%% from one byte, when empty, to three.
signed_connect_of_size(Bytes) ->
    Kp = macula_identity:generate(),
    Unpadded = byte_size(padded_connect(Kp, <<>>)) - 4,
    padded_connect(Kp, binary:copy(<<0>>, Bytes - Unpadded - 2)).

padded_connect(Kp, Padding) ->
    Connect = (connect_frame(macula_identity:public(Kp)))#{padding => Padding},
    macula_frame:encode(macula_frame:sign(Connect, Kp)).

connect_frame(Pub) ->
    macula_frame:connect(#{
        node_id         => Pub,
        station_id      => Pub,
        realms          => [],
        capabilities    => 0,
        puzzle_evidence => macula_identity:puzzle_evidence(Pub)
    }).

%% A CALL as a station relays it from a caller.
call_frame() ->
    macula_frame:call(#{
        call_id     => crypto:strong_rand_bytes(16),
        procedure   => <<"io.macula.test.echo">>,
        realm       => crypto:strong_rand_bytes(32),
        payload     => #{},
        deadline_ms => erlang:system_time(millisecond) + ?EVENT_MS,
        caller      => macula_identity:public(macula_identity:generate())
    }).

encode_signed(Frame) ->
    macula_frame:encode(macula_frame:sign(Frame, macula_identity:generate())).

connected(Server) ->
    receive
        {macula_peering, connected, Server, _PeerNodeId} -> ok
    after ?EVENT_MS ->
        erlang:error(not_connected)
    end.

%% The reason Server gave for ending the connection, once it has also
%% stopped; or what it did instead within PROMPT_MS.
ended(Server) ->
    Mon = erlang:monitor(process, Server),
    receive
        {macula_peering, disconnected, Server, Reason} -> stopped(Mon, Server, Reason)
    after ?PROMPT_MS ->
        no_disconnect
    end.

stopped(Mon, Server, Reason) ->
    receive
        {'DOWN', Mon, process, Server, _Exit} -> Reason
    after ?PROMPT_MS ->
        {still_running, Reason}
    end.

%% The invalid frame Server told its controlling process about.
invalid_reported(Server) ->
    receive
        {macula_peering, invalid_frame, Server, Type, Field} -> {invalid_frame, Type, Field}
    after ?EVENT_MS ->
        not_reported
    end.

%% Whether the next CALL Server delivers is Frame.
served(Server, #{call_id := CallId}) ->
    receive
        {macula_peering, frame, Server, #{frame_type := call, call_id := CallId}} ->
            served;
        {macula_peering, frame, Server, #{frame_type := call} = Other} ->
            {served_instead, maps:get(call_id, Other)}
    after ?EVENT_MS ->
        not_served
    end.

running(Server) ->
    receive
        {macula_peering, disconnected, Server, Reason} -> {disconnected, Reason}
    after 0 ->
        alive(is_process_alive(Server))
    end.

alive(true) -> running;
alive(false) -> stopped.

drain() ->
    receive
        {quic, _, _, _} -> drain();
        {macula_peering, _, _, _} -> drain();
        {macula_peering, _, _, _, _} -> drain()
    after 0 ->
        ok
    end.

free_udp_port() ->
    {ok, Sock} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Sock),
    ok = gen_udp:close(Sock),
    Port.
