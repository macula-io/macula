%%%-------------------------------------------------------------------
%%% @doc Handshake v5 between two `macula_peering_conn' workers over a real Quinn loopback pair
%%% (plans/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md): the connection is authenticated once, by both proofs over the TLS
%%% session's exporter, and no frame after HELLO carries a neighbour signature. A client falls back to v4 once, only
%%% after unsupported_version, from a station never seen on v5; a station seen on v5 that answers v4 is refused as a
%%% downgrade until forgotten. The liveness probe on v5 is answered by the peer's connection, never by its controller.
%%%
%%% The loopback pair and its helpers are macula_peering_handshake_tests'. A pre-v5 station is stood in for by
%%% `pre_v5_station/1': the station's own challenge material, and macula_handshake's check of CONNECT with no exporter,
%%% which answers a v5 CONNECT as an old station does. Nothing in a connection caps the handshake version: such a cap
%%% would be a configured downgrade.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_v5_tests).

-include_lib("eunit/include/eunit.hrl").

-import(macula_peering_handshake_tests,
        [world/2, connect/2, await/2, ended/1, still_open/2, finish/2, sent/3, on_control_stream/2, ping/0,
         node_id/1, accept_one/1, station_opts/2, fixed/1]).

-define(T0, 1789000000000).
-define(MINUTE, 60_000).

v5_test_() ->
    {timeout, 900,
     {setup,
      fun macula_peering_handshake_tests:setup/0,
      fun macula_peering_handshake_tests:cleanup/1,
      fun(Ctx) ->
          [{"pq_hybrid: both ends connect on v5, and control frames travel unsigned both ways",
            {timeout, 120, fun() -> both_ends_connect_on_v5(Ctx, pq_hybrid) end}},
           {"pq_pure: both ends connect on v5",
            {timeout, 60, fun() -> both_ends_connect_on_v5(Ctx, pq_pure) end}},
           {"a control frame without a neighbour signature is delivered on v5",
            {timeout, 120, fun() -> an_unsigned_control_frame_is_delivered_on_v5(Ctx) end}},
           {"a neighbour-signed frame on v5 closes with malformed_frame",
            {timeout, 120, fun() -> a_neighbour_signed_frame_on_v5_closes(Ctx) end}},
           {"a v5 client against a v4-only station falls back once and connects on v4",
            {timeout, 60, fun() -> a_v5_client_falls_back_to_a_v4_only_station(Ctx) end}},
           {"a station connection given a version cap still answers v5",
            {timeout, 60, fun() -> a_version_cap_option_is_not_honoured(Ctx) end}},
           {"a station seen on v5 that answers v4 is refused, until forgotten",
            {timeout, 60, fun() -> a_station_seen_on_v5_that_answers_v4_is_refused_until_forgotten(Ctx) end}},
           {"a v4 handshake that completes to a node seen on v5 meanwhile is refused (macula#53)",
            {timeout, 60, fun() -> a_v4_handshake_completing_to_a_node_seen_on_v5_is_refused(Ctx) end}},
           {"control frames on a v4 connection are counted as the old path",
            {timeout, 60, fun() -> control_frames_on_v4_are_counted(Ctx) end}},
           {"past the station's session proof budget the client is refused and nothing is signed",
            {timeout, 60, fun() -> past_the_session_proof_budget_the_client_is_refused(Ctx) end}},
           {"v5 liveness: each connection answers the other's probe, and neither controller sees one",
            {timeout, 60, fun() -> v5_liveness_is_answered_by_the_connections(Ctx) end}},
           {"v5 liveness: a peer whose connection stops answering is reaped",
            {timeout, 60, fun() -> v5_liveness_reaps_a_peer_whose_connection_stopped(Ctx) end}}]
      end}}.

%%====================================================================
%% Tests
%%====================================================================

both_ends_connect_on_v5(Ctx, Profile) ->
    World = world(Ctx, #{profile => Profile}),
    Before = macula_peering:handshake_counters(),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    ?assertEqual(2, counted(v5_connections, Before)),
    ?assertEqual(0, counted(v4_connections, Before)),
    ?assert(macula_peer_versions:seen_v5(node_id(maps:get(station_key, World)))),
    ok = sent(Client, Station, [ping() || _ <- lists:seq(1, 3)]),
    ok = sent(Station, Client, [ping() || _ <- lists:seq(1, 3)]),
    ?assertEqual(0, counted(v4_control_frames, Before)),
    ?assertEqual({open, open}, {still_open(Client, 300), still_open(Station, 0)}),
    finish(World, [Client, Station]).

%% The frame v4 closes a pq_hybrid connection for (a control frame with no neighbour signature) is what v5 carries.
an_unsigned_control_frame_is_delivered_on_v5(Ctx) ->
    World = world(Ctx, #{profile => pq_hybrid}),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    ok = on_control_stream(Client, macula_frame:encode(ping())),
    ?assertMatch(#{frame_type := ping}, macula_peering_handshake_tests:frame_from(Station)),
    ?assertEqual(open, still_open(Station, 300)),
    finish(World, [Client, Station]).

a_neighbour_signed_frame_on_v5_closes(Ctx) ->
    #{client_key := ClientKey} = World = world(Ctx, #{profile => pq_hybrid}),
    Before = macula_peering:handshake_counters(),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    ?assertEqual(2, counted(v5_connections, Before)),
    Signed = macula_frame:sign_neighbour(ping(), ClientKey,
                                         #{connection => crypto:hash(sha384, <<"a challenge">>), seq => 0}),
    ok = on_control_stream(Client, macula_frame:encode(Signed)),
    ?assertEqual(malformed_frame, ended(Station)),
    finish(World, [Client, Station]).

%% The station answers a v5 CONNECT as a pre-v5 station does. The client counts the fallback, redials on a new QUIC
%% connection with a v4 CONNECT, and connects on v4; its next dial to that station within 10 minutes is v4.
a_v5_client_falls_back_to_a_v4_only_station(Ctx) ->
    #{station_key := StationKey} = World = world(Ctx, #{}),
    Before = macula_peering:handshake_counters(),
    Client = dial(World),
    ?assertEqual({refused, unsupported_version}, pre_v5_station(World)),
    Station = accept_one(station_opts(World, #{mode => off})),
    _ = {await(Client, connected), await(Station, connected)},
    ?assertEqual(1, counted(v4_fallbacks, Before)),
    ?assertEqual(2, counted(v4_connections, Before)),
    ?assertEqual(4, macula_peer_versions:dial_version(node_id(StationKey), ?T0 + ?MINUTE)),
    ?assertNot(macula_peer_versions:seen_v5(node_id(StationKey))),
    finish(World, [Client, Station]).

%% The same station identity, first on v5 and then answering only v4 (a rollback, or an attacker forcing a
%% downgrade): the client refuses it without a retry. Once forgotten, the client falls back and connects on v4.
a_station_seen_on_v5_that_answers_v4_is_refused_until_forgotten(Ctx) ->
    #{station_key := StationKey} = World = world(Ctx, #{}),
    {Client1, Station1} = connect(World, #{mode => off}),
    _ = {await(Client1, connected), await(Station1, connected)},
    ok = macula_peering:close(Client1),
    Before = macula_peering:handshake_counters(),
    Client2 = dial(World),
    ?assertEqual({refused, unsupported_version}, pre_v5_station(World)),
    ?assertEqual(v5_downgrade_refused, ended(Client2)),
    ?assertEqual(1, counted(v5_downgrade_refused, Before)),
    ?assertEqual(0, counted(v4_fallbacks, Before)),
    ok = macula_peering:forget_v5_peer(node_id(StationKey)),
    Client3 = dial(World),
    ?assertEqual({refused, unsupported_version}, pre_v5_station(World)),
    Station3 = accept_one(station_opts(World, #{mode => off})),
    _ = {await(Client3, connected), await(Station3, connected)},
    finish(World, [Station1, Client3, Station3]).

%% The option a pre-v5 stand-in once took is no part of a connection: given it, a station still answers v5.
a_version_cap_option_is_not_honoured(Ctx) ->
    World = world(Ctx, #{}),
    Before = macula_peering:handshake_counters(),
    Client = dial(World),
    Station = accept_one((station_opts(World, #{mode => off}))#{max_handshake_version => 4}),
    _ = {await(Client, connected), await(Station, connected)},
    ?assertEqual(2, counted(v5_connections, Before)),
    finish(World, [Client, Station]).

%% Two dials interleaving: this client is in the v4 cache for the station and sends a v4 CONNECT; before the station
%% answers, the node completes v5 on another connection. The v4 HELLO is refused, whichever finished first.
a_v4_handshake_completing_to_a_node_seen_on_v5_is_refused(Ctx) ->
    #{station_key := StationKey} = World = world(Ctx, #{}),
    StationNodeId = node_id(StationKey),
    {fall_back, _} = macula_peer_versions:unsupported_version(StationNodeId, ?T0 + ?MINUTE),
    Before = macula_peering:handshake_counters(),
    Client = dial(World),
    ?assertEqual({accepted, v4}, pre_v5_station(World, fun() -> macula_peer_versions:completed_v5(StationNodeId) end)),
    ?assertEqual(v5_downgrade_refused, ended(Client)),
    ?assertEqual(1, counted(v5_downgrade_refused, Before)),
    ?assertEqual(0, counted(v4_connections, Before)),
    finish(World, []).

control_frames_on_v4_are_counted(Ctx) ->
    World = world(Ctx, #{}),
    Before = macula_peering:handshake_counters(),
    {Client, Station} = connect(World, #{handshake => 4, mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    ok = sent(Client, Station, [ping() || _ <- lists:seq(1, 3)]),
    ?assertEqual(2, counted(v4_connections, Before)),
    ?assertEqual(3, counted(v4_control_frames, Before)),
    finish(World, [Client, Station]).

%% With one session proof a minute per client node, the client's is spent before it dials, in this minute and the
%% next, on the real clock the budget counts on. Its CONNECT proof verifies, the station signs nothing, and both ends
%% close: the station with session_proof_rate, the client refused.
past_the_session_proof_budget_the_client_is_refused(Ctx) ->
    with_session_proofs_per_node_per_minute(1, fun() ->
        #{client_key := ClientKey} = World = world(Ctx, #{}),
        Now = erlang:system_time(millisecond),
        ok = macula_session_proof_rate:allow(node_id(ClientKey), Now),
        ok = macula_session_proof_rate:allow(node_id(ClientKey), Now + ?MINUTE),
        Before = macula_peering:handshake_counters(),
        {Client, Station} = connect(World, #{mode => off}),
        ?assertEqual({refused, session_proof_rate}, ended(Client)),
        ?assertEqual(session_proof_rate, ended(Station)),
        ?assertEqual(1, counted(session_proof_rate, Before)),
        finish(World, [])
    end).

v5_liveness_is_answered_by_the_connections(Ctx) ->
    World = world(Ctx, #{}),
    {Client, Station} = connect(World, #{mode => off, liveness_interval_ms => 200, liveness_max_misses => 2}),
    _ = {await(Client, connected), await(Station, connected)},
    ?assertEqual(open, still_open(Client, 1_500)),
    ?assertEqual(none, any_frame()),
    finish(World, [Client, Station]).

v5_liveness_reaps_a_peer_whose_connection_stopped(Ctx) ->
    World = world(Ctx, #{}),
    Before = macula_peering:handshake_counters(),
    {Client, Station} = connect(World, #{mode => off, liveness_interval_ms => 200, liveness_max_misses => 2}),
    _ = {await(Client, connected), await(Station, connected)},
    ?assertEqual(2, counted(v5_connections, Before)),
    ok = sys:suspend(Station),
    ?assertEqual(peer_liveness_lost, ended(Client)),
    ok = sys:resume(Station),
    finish(World, [Station]).

%%====================================================================
%% Helpers
%%====================================================================

%% The budget is read when its process starts, so the test restarts it under the limit and again after.
with_session_proofs_per_node_per_minute(Limit, Test) ->
    ok = application:set_env(macula, session_proofs_per_node_per_minute, Limit),
    ok = restart_session_proof_rate(),
    try Test()
    after
        ok = application:unset_env(macula, session_proofs_per_node_per_minute),
        ok = restart_session_proof_rate()
    end.

restart_session_proof_rate() ->
    ok = supervisor:terminate_child(macula_peering_sup, macula_session_proof_rate),
    {ok, _} = supervisor:restart_child(macula_peering_sup, macula_session_proof_rate),
    ok.

%% A client connection dialing the world's listener, whose station connection the test accepts itself.
dial(#{client_key := ClientKey, client_issuer := ClientIssuer, station_key := StationKey, port := Port}) ->
    {ok, Client} = macula_peering:connect(
                     #{identity => ClientKey, issuer => ClientIssuer, capabilities => 3, controlling_pid => self(),
                       clock => fixed(?T0 + ?MINUTE),
                       target => #{host => <<"127.0.0.1">>, port => Port, timeout_ms => 5_000,
                                   expected_node_id => node_id(StationKey)}}),
    Client.

%% One connection answered as a station from before handshake v5 answers it: the station's own challenge material,
%% and CONNECT checked by macula_handshake with no exporter, which accepts only version 4. Returns how it answered.
pre_v5_station(World) ->
    pre_v5_station(World, fun() -> ok end).

%% BeforeHello runs after the station has read CONNECT and before it answers.
pre_v5_station(#{station_key := StationKey, station_issuer := StationIssuer}, BeforeHello) ->
    Conn = receive {quic, new_conn, C, _Info} -> C after 5_000 -> erlang:error(no_inbound_conn) end,
    ok = macula_quic:async_accept_stream(Conn),
    Stream = receive {quic, new_stream, S, _} -> S after 5_000 -> erlang:error(no_control_stream) end,
    ok = macula_quic:setopt(Stream, active, true),
    [Opener] = frames(Stream, 1),
    ok = macula_handshake:read_opener(Opener),
    {ok, Leaf} = macula_quic:presented_leaf(Conn),
    {ok, #{tls_binding := Binding, tls_status := Status}} =
        macula_statement_issuer:tls_material(StationIssuer, crypto:hash(sha384, Leaf)),
    Profile = maps:get(profile, StationKey),
    Challenge = macula_handshake:challenge(#{profile => Profile, identity_key => macula_node_keys:public_key(StationKey),
                                             tls_binding => Binding, tls_status => Status}),
    ok = macula_quic:send(Stream, macula_frame:encode_bytes(Challenge)),
    [Connect] = frames(Stream, 1),
    Session = #{profile => Profile, challenge => Challenge, leaf => Leaf, capabilities => 5, now => ?T0 + ?MINUTE,
                puzzle => #{difficulty => macula_node_keys:puzzle_difficulty(), mode => off}},
    {Verdict, Reason, Hello} = answered(macula_handshake:accept_connect(Connect, Session)),
    ok = BeforeHello(),
    ok = macula_quic:send(Stream, macula_frame:encode_bytes(Hello)),
    timer:sleep(200),
    ok = macula_quic:close_connection(Conn),
    {Verdict, Reason}.

answered({refused, Reason, Hello}) -> {refused, Reason, Hello};
answered({accepted, _Client, Hello}) -> {accepted, v4, Hello}.

frames(Stream, N) ->
    frames(Stream, N, <<>>).

frames(Stream, N, Buf) ->
    enough(macula_frame:parse_stream_bytes(Buf, 64 * 1024), Stream, N, Buf).

enough({ok, Frames, _Tail}, _Stream, N, _Buf) when length(Frames) >= N ->
    lists:sublist(Frames, N);
enough(_NotYet, Stream, N, Buf) ->
    receive {quic, Bin, Stream, _Flags} when is_binary(Bin) -> frames(Stream, N, <<Buf/binary, Bin/binary>>)
    after 5_000 -> erlang:error(no_handshake_frame)
    end.

counted(Counter, Before) ->
    maps:get(Counter, macula_peering:handshake_counters()) - maps:get(Counter, Before).

%% Any frame a connection handed its controlling process.
any_frame() ->
    receive {macula_peering, frame, _Conn, Frame} -> Frame after 0 -> none end.
