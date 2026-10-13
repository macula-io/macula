%%%-------------------------------------------------------------------
%%% @doc The wire 1.0 handshake between two `macula_peering_conn' workers over a real Quinn loopback pair
%%% (docs/design/WIRE_1_0.md section 4): version 5 only, the connection authenticated once by both proofs over the TLS
%%% session's exporter, no frame after HELLO carrying a neighbour signature, and each side's version floor for the
%%% other raised after completion and kept through the connection's `version_floor' store. The liveness probe is
%%% answered by the peer's connection, never by its controller.
%%%
%%% The loopback pair and its helpers are macula_peering_handshake_tests'. This module is also the floor store the
%%% tests hand a connection (floor/2, raise/3).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_v5_tests).

-include_lib("eunit/include/eunit.hrl").

%% The version_floor store the tests hand a connection.
-export([floor/2, raise/3]).

-import(macula_peering_handshake_tests,
        [world/2, connect/2, await/2, ended/1, still_open/2, finish/2, sent/3, on_control_stream/2, ping/0,
         node_id/1]).

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
           {"a control frame without a neighbour signature is delivered",
            {timeout, 120, fun() -> an_unsigned_control_frame_is_delivered_on_v5(Ctx) end}},
           {"a frame carrying a neighbour field closes with malformed_frame",
            {timeout, 120, fun() -> a_neighbour_signed_frame_on_v5_closes(Ctx) end}},
           {"each side raises its floor for the other in its store after completion",
            {timeout, 60, fun() -> each_side_raises_its_floor_in_its_store(Ctx) end}},
           {"a floor above 5 in the client's store refuses the station with version_below_floor",
            {timeout, 60, fun() -> a_stored_floor_above_the_version_refuses(Ctx) end}},
           {"a store that fails to persist keeps the connection and counts version_floor_unpersisted",
            {timeout, 60, fun() -> a_store_that_fails_keeps_the_connection(Ctx) end}},
           {"past the station's session proof budget the client is refused and nothing is signed",
            {timeout, 60, fun() -> past_the_session_proof_budget_the_client_is_refused(Ctx) end}},
           {"liveness: each connection answers the other's probe, and neither controller sees one",
            {timeout, 60, fun() -> v5_liveness_is_answered_by_the_connections(Ctx) end}},
           {"liveness: a peer whose connection stops answering is reaped",
            {timeout, 60, fun() -> v5_liveness_reaps_a_peer_whose_connection_stopped(Ctx) end}},
           {"liveness: each side's frame observer sees the probes it reads",
            {timeout, 60, fun() -> v5_liveness_frames_reach_the_frame_observer(Ctx) end}}]
      end}}.

both_ends_connect_on_v5(Ctx, Profile) ->
    #{client_key := ClientKey, station_key := StationKey} = World = world(Ctx, #{profile => Profile}),
    Before = macula_peering:handshake_counters(),
    {Client, Station} = connect(World, #{mode => off}),
    _ = {await(Client, connected), await(Station, connected)},
    ?assertEqual(2, counted(v5_connections, Before)),
    %% Without a store, each side holds the other's floor for the run.
    ?assertEqual(5, macula_peer_versions:floor(node_id(StationKey), none)),
    ?assertEqual(5, macula_peer_versions:floor(node_id(ClientKey), none)),
    ok = sent(Client, Station, [ping() || _ <- lists:seq(1, 3)]),
    ok = sent(Station, Client, [ping() || _ <- lists:seq(1, 3)]),
    ?assertEqual({open, open}, {still_open(Client, 300), still_open(Station, 0)}),
    finish(World, [Client, Station]).

%% A control frame with no neighbour signature is what every connection carries.
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
    <<_Length:32, Bytes/binary>> = macula_frame:encode(ping()),
    {ok, Map} = macula_record_cbor:decode_strict(Bytes),
    Held = macula_signed_object:sign_held(<<"MACULA-PQ-NEIGHBOUR-V1">>, #{{text, <<"seq">>} => 0}, ClientKey),
    Signed = Map#{{text, <<"neighbour">>} => #{{text, <<"tbs">>} => maps:get(tbs, Held),
                                               {text, <<"signature">>} => maps:get(signature, Held)}},
    ok = on_control_stream(Client, macula_frame:encode_bytes(macula_record_cbor:encode(Signed))),
    ?assertEqual(malformed_frame, ended(Station)),
    finish(World, [Client, Station]).

%% Each side's floor for the other rises to 5 once the handshake completes, in memory and in its store.
each_side_raises_its_floor_in_its_store(Ctx) ->
    #{client_key := ClientKey, station_key := StationKey} = World = world(Ctx, #{}),
    Store = {?MODULE, #{test => self(), floor => none, raise => ok}},
    {Client, Station} = connect(World, #{mode => off, client_version_floor => Store, station_version_floor => Store}),
    _ = {await(Client, connected), await(Station, connected)},
    Raised = lists:sort([receive {raised, NodeId, Version} -> {NodeId, Version} after 2_000 -> none end
                         || _ <- [client, station]]),
    ?assertEqual(lists:sort([{node_id(StationKey), 5}, {node_id(ClientKey), 5}]), Raised),
    finish(World, [Client, Station]).

%% A floor read from the store, above the only version there is, refuses the station before any CONNECT.
a_stored_floor_above_the_version_refuses(Ctx) ->
    World = world(Ctx, #{}),
    Before = macula_peering:handshake_counters(),
    {Client, Station} = connect(World, #{mode => off,
                                         client_version_floor => {?MODULE, #{test => self(), floor => 6,
                                                                             raise => ok}}}),
    ?assertEqual(version_below_floor, ended(Client)),
    ?assertEqual(1, counted(version_below_floor, Before)),
    finish(World, [Station]).

%% The store refuses the raise: the completed connection stays up, the floor holds in memory, and the failure is
%% counted.
a_store_that_fails_keeps_the_connection(Ctx) ->
    #{station_key := StationKey} = World = world(Ctx, #{}),
    Before = macula_peering:handshake_counters(),
    Store = {?MODULE, #{test => self(), floor => none, raise => {error, read_only}}},
    {Client, Station} = connect(World, #{mode => off, client_version_floor => Store}),
    _ = {await(Client, connected), await(Station, connected)},
    ?assertEqual(1, counted(version_floor_unpersisted, Before)),
    ?assertEqual(5, macula_peer_versions:floor(node_id(StationKey), none)),
    ?assertEqual(open, still_open(Client, 300)),
    finish(World, [Client, Station]).

%% The version_floor store the tests hand a connection: the floor it was given, and each raise told to the test.
floor(#{floor := Floor}, _NodeId) -> Floor.

raise(#{test := Test, raise := Answer}, NodeId, Version) ->
    Test ! {raised, NodeId, Version},
    Answer.

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

%% Every frame a connection reads on its control stream reaches its frame observer, the v5 liveness probe and its
%% answer included (macula#51). The client probes: the station reads its liveness_ping, the client the liveness_pong.
v5_liveness_frames_reach_the_frame_observer(Ctx) ->
    World = world(Ctx, #{}),
    Test = self(),
    Observer = fun(Side) -> fun(Dir, Type, _Size, _Us) -> Test ! {liveness_observed, Side, Dir, Type}, ok end end,
    {Client, Station} = connect(World, #{mode => off, liveness_interval_ms => 200, liveness_max_misses => 2,
                                         client_observer => Observer(client), station_observer => Observer(station)}),
    _ = {await(Client, connected), await(Station, connected)},
    timer:sleep(1_000),
    Seen = liveness_observed([]),
    [?assert(lists:member(Read, Seen), {Read, Seen}) || Read <- [{station, in, liveness_ping}, {client, in, liveness_pong}]],
    finish(World, [Client, Station]).

%%====================================================================
%% Helpers
%%====================================================================

%% The {Side, Direction, Type} of every liveness frame the observers reported so far.
liveness_observed(Seen) ->
    receive {liveness_observed, Side, Dir, Type} when Type =:= liveness_ping; Type =:= liveness_pong ->
                liveness_observed([{Side, Dir, Type} | Seen]);
            {liveness_observed, _Side, _Dir, _OtherType} ->
                liveness_observed(Seen)
    after 0 -> lists:usort(Seen)
    end.

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
