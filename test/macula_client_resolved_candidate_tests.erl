%% EUnit tests for the two decisions behind `macula_client:resolved_candidate/3'
%% and `macula_client:remember_resolved/5': how long the station that last
%% answered a procedure stays usable as a direct-dial head start, and whether
%% the pool still holds a live link to it.
%%
%% Both are driven here with explicit clock readings and explicit links,
%% because neither property can be shown by inspection. That an entry's life
%% is measured in ELAPSED MONOTONIC milliseconds rather than against a wall
%% clock is visible only by moving the reading, and that a remembered station
%% is dropped the moment its link goes is visible only by taking the link
%% away.
-module(macula_client_resolved_candidate_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<1:256>>).
-define(PROC, <<"io.macula.echo">>).
-define(STATION, <<2:256>>).
-define(OTHER_STATION, <<4:256>>).
-define(PROVIDER, <<3:256>>).
-define(SEED, <<"quic://[2a01:db8::1]:4433">>).

%%%===================================================================
%%% Handing a remembered station back
%%%===================================================================

%% The whole point of the head start: the candidate comes back WITH the seed
%% the pool's own live link to that station is keyed by, so the call names
%% the link the pool already holds and dials nothing.
a_remembered_station_comes_back_with_its_live_links_seed_test() ->
    Link = link_answering(?STATION),
    ?assertEqual({ok, candidate(), ?SEED},
                 usable_at(remembered_at(0, 1_000), 999, [{?SEED, Link}])),
    stop(Link).

%% Nothing was ever remembered for this procedure.
an_unremembered_procedure_has_no_head_start_test() ->
    Link = link_answering(?STATION),
    ?assertEqual(none, macula_client:still_usable(error, 0, [{?SEED, Link}])),
    stop(Link).

%%%===================================================================
%%% The horizon is elapsed time, and only elapsed time
%%%===================================================================

%% Remembered for 1000 ms at reading 0, so it is gone at reading 1000 even
%% though the link is still there. The bound is exclusive at its own edge:
%% `Now < Until'. Pinned deliberately, because an off-by-one here hands back
%% a candidate for one millisecond past the lifetime it was granted.
an_elapsed_horizon_ends_the_head_start_even_with_a_live_link_test() ->
    Link = link_answering(?STATION),
    Resolved = remembered_at(0, 1_000),
    ?assertMatch({ok, _, _}, usable_at(Resolved, 999, [{?SEED, Link}])),
    ?assertEqual(none, usable_at(Resolved, 1_000, [{?SEED, Link}])),
    ?assertEqual(none, usable_at(Resolved, 1_001, [{?SEED, Link}])),
    stop(Link).

%% A zero lifetime is remembered and is immediately past: an advertisement
%% already at its expiry earns no head start rather than a free one.
a_zero_lifetime_is_never_usable_test() ->
    Link = link_answering(?STATION),
    ?assertEqual(none, usable_at(remembered_at(500, 0), 500, [{?SEED, Link}])),
    stop(Link).

%% NEITHER function reads a clock. The horizon is fixed by the reading the
%% caller passes at remember time, and compared against the reading the
%% caller passes at read time, so real time passing between the two calls
%% changes nothing. That is what makes the pool's own monotonic anchoring
%% safe: a wall clock that steps between remembering and reading cannot
%% lengthen or shorten an entry, because no wall clock is consulted at all.
the_answer_depends_on_the_readings_given_and_not_on_real_time_test() ->
    Link = link_answering(?STATION),
    Resolved = remembered_at(0, 50),
    First = usable_at(Resolved, 10, [{?SEED, Link}]),
    timer:sleep(120),
    ?assertMatch({ok, _, _}, First),
    ?assertEqual(First, usable_at(Resolved, 10, [{?SEED, Link}])),
    ?assertEqual(none, usable_at(Resolved, 60, [{?SEED, Link}])),
    stop(Link).

%%%===================================================================
%%% The live link is the evidence
%%%===================================================================

%% A remembered station the pool no longer has a link to is not handed back,
%% however fresh its horizon. This is the half that makes skipping the
%% `station_endpoint' lookup honest: the lookup would have produced an
%% address to dial, and the live link is better evidence than any record
%% because it cannot be stale.
a_remembered_station_with_no_live_link_is_not_handed_back_test() ->
    ?assertEqual(none, usable_at(remembered_at(0, 60_000), 1, [])).

%% A pool with links, none of them to this station.
a_live_link_to_another_station_is_not_a_head_start_test() ->
    Other = link_answering(?OTHER_STATION),
    ?assertEqual(none, usable_at(remembered_at(0, 60_000), 1,
                                 [{<<"quic://[2a01:db8::9]:4433">>, Other}])),
    stop(Other).

%% A link that never answers must not wedge the pool or be mistaken for the
%% station: `safe_peer_node_id/1' absorbs it and the scan moves on, so a mute
%% link sitting in front of the real one costs a head start, not a pool.
a_mute_link_is_passed_over_for_the_station_behind_it_test() ->
    Mute = link_answering(never),
    Real = link_answering(?STATION),
    ?assertEqual({ok, candidate(), ?SEED},
                 usable_at(remembered_at(0, 60_000), 1,
                           [{<<"quic://[2a01:db8::9]:4433">>, Mute}, {?SEED, Real}])),
    stop(Mute),
    stop(Real).

%%%===================================================================
%%% What the map holds
%%%===================================================================

%% A new entry for a procedure replaces the old one rather than accumulating
%% beside it.
remembering_a_procedure_again_replaces_its_entry_test() ->
    Once = macula_client:remembered(key(?PROC), candidate(), 1_000, 0, #{}),
    Twice = macula_client:remembered(key(?PROC), candidate(?OTHER_STATION), 2_000, 0,
                                     Once),
    ?assertEqual(1, map_size(Twice)),
    ?assertEqual(#{candidate => candidate(?OTHER_STATION), until_mono => 2_000},
                 maps:get(key(?PROC), Twice)).

%% Entries whose horizon has elapsed are dropped as a new one arrives, so the
%% map tracks the procedures a pool currently calls rather than every
%% procedure it has ever called. Nothing else prunes it.
remembering_drops_the_entries_whose_horizon_has_elapsed_test() ->
    Stale = macula_client:remembered(key(<<"stale">>), candidate(), 100, 0, #{}),
    Live = macula_client:remembered(key(<<"live">>), candidate(), 5_000, 0, Stale),
    Now = macula_client:remembered(key(?PROC), candidate(), 6_000, 200, Live),
    ?assertEqual(lists:sort([key(<<"live">>), key(?PROC)]), lists:sort(maps:keys(Now))).

%% An entry still inside its horizon is kept when another arrives.
remembering_keeps_the_entries_still_inside_their_horizon_test() ->
    Live = macula_client:remembered(key(<<"live">>), candidate(), 5_000, 0, #{}),
    Now = macula_client:remembered(key(?PROC), candidate(), 6_000, 200, Live),
    ?assertEqual(#{candidate => candidate(), until_mono => 5_000},
                 maps:get(key(<<"live">>), Now)).

%%%===================================================================
%%% Helpers
%%%===================================================================

key() -> key(?PROC).
key(Procedure) -> {?REALM, Procedure}.

candidate() -> candidate(?STATION).
candidate(Station) ->
    #{provider => ?PROVIDER, version => <<7:128>>, station => Station}.

remembered_at(Now, TtlMs) ->
    macula_client:remembered(key(), candidate(), Now + TtlMs, Now, #{}).

usable_at(Resolved, Now, LiveLinks) ->
    macula_client:still_usable(maps:find(key(), Resolved), Now, LiveLinks).

%% A stand-in for a link process: it answers `peer_node_id' with the station
%% it is connected to, or never answers at all. Same shape the pool's own
%% probe-guard tests use.
link_answering(never) ->
    spawn(fun Loop() -> receive _ -> Loop() end end);
link_answering(NodeId) ->
    spawn(fun Loop() ->
              receive
                  {'$gen_call', From, peer_node_id} ->
                      gen_server:reply(From, {ok, NodeId}),
                      Loop();
                  _Other ->
                      Loop()
              end
          end).

stop(Pid) -> exit(Pid, kill).
