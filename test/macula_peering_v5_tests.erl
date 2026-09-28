%%%-------------------------------------------------------------------
%%% @doc Handshake v5 between two `macula_peering_conn' workers over a real Quinn loopback pair
%%% (plans/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md): the connection is authenticated once, by both proofs over the TLS
%%% session's exporter, and no frame after HELLO carries a neighbour signature. A client falls back to v4 once, only
%%% after unsupported_version, from a station never seen on v5; a station seen on v5 that answers v4 is refused as a
%%% downgrade until forgotten. The liveness probe on v5 is answered by the peer's connection, never by its controller.
%%%
%%% The loopback pair and its helpers are macula_peering_handshake_tests'.
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
           {"a station seen on v5 that answers v4 is refused, until forgotten",
            {timeout, 60, fun() -> a_station_seen_on_v5_that_answers_v4_is_refused_until_forgotten(Ctx) end}},
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
    {Client, Refusing} = connect(World, #{mode => off, max_handshake_version => 4}),
    Station = accept_one(station_opts(World, #{mode => off, max_handshake_version => 4})),
    _ = {await(Client, connected), await(Station, connected)},
    ?assertEqual(unsupported_version, ended(Refusing)),
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
    {Client2, Refusing} = connect(World, #{mode => off, max_handshake_version => 4}),
    ?assertEqual(v5_downgrade_refused, ended(Client2)),
    ?assertEqual(unsupported_version, ended(Refusing)),
    ?assertEqual(1, counted(v5_downgrade_refused, Before)),
    ?assertEqual(0, counted(v4_fallbacks, Before)),
    ok = macula_peering:forget_v5_peer(node_id(StationKey)),
    {Client3, Refusing3} = connect(World, #{mode => off, max_handshake_version => 4}),
    Station3 = accept_one(station_opts(World, #{mode => off, max_handshake_version => 4})),
    _ = {await(Client3, connected), await(Station3, connected)},
    ?assertEqual(unsupported_version, ended(Refusing3)),
    finish(World, [Station1, Client3, Station3]).

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
        ?assertEqual({refused, not_accepted}, ended(Client)),
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

counted(Counter, Before) ->
    maps:get(Counter, macula_peering:handshake_counters()) - maps:get(Counter, Before).

%% Any frame a connection handed its controlling process.
any_frame() ->
    receive {macula_peering, frame, _Conn, Frame} -> Frame after 0 -> none end.
