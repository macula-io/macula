%% EUnit tests for the client's station-authorization check (macula-station#63,
%% hook 3): what a slot read means, what each verdict means under each mode,
%% and that the check reads exactly the (realm, station) slot and counts its
%% outcome on the pool's status.
-module(macula_station_check_tests).

-include_lib("eunit/include/eunit.hrl").

realm() -> binary:copy(<<16#11>>, 32).
station() -> binary:copy(<<16#22>>, 32).
other_station() -> binary:copy(<<16#23>>, 32).
other_realm() -> binary:copy(<<16#12>>, 32).

authorization() ->
    macula_record:station_authorization(realm(), #{realm => realm(), station_node => station(),
                                                   hostname => <<"a.example">>, roles => [<<"station">>]}).

tombstone() ->
    macula_record:tombstone(authorization(), revoked).

%% A fake dial: the pool is this test process, so a check's cast lands in its
%% mailbox, and the read is the one the test hands in — either a read result
%% or a three-argument read fun.
dial(Read) when is_function(Read, 3) ->
    #{pool => self(), find_record => Read};
dial(Read) ->
    #{pool => self(), find_record => fun(_Pool, _Key, _TimeoutMs) -> Read end}.

with_mode(Mode, Fun) ->
    application:set_env(macula, station_authorization_mode, Mode),
    try Fun() after application:unset_env(macula, station_authorization_mode) end.

casted() ->
    receive {'$gen_cast', {station_check, Outcome}} -> Outcome
    after 0 -> no_cast
    end.

%%------------------------------------------------------------------
%% classify: what the slot read means
%%------------------------------------------------------------------

an_authorization_for_the_dialed_pair_is_live_test() ->
    ?assertEqual(live, macula_station_check:classify({ok, authorization()}, realm(), station())).

an_authorization_for_another_station_is_none_test() ->
    ?assertEqual(none, macula_station_check:classify({ok, authorization()}, realm(), other_station())).

an_authorization_for_another_realm_is_none_test() ->
    ?assertEqual(none, macula_station_check:classify({ok, authorization()}, other_realm(), station())).

the_realms_tombstone_over_the_type_is_tombstoned_test() ->
    ?assertEqual(tombstoned, macula_station_check:classify({ok, tombstone()}, realm(), station())).

a_tombstone_over_another_type_is_unreadable_test() ->
    Endpoint = macula_record:station_endpoint(4433),
    ?assertEqual(unreadable, macula_station_check:classify({ok, macula_record:tombstone(Endpoint, revoked)},
                                                           realm(), station())).

nothing_at_the_slot_is_none_test() ->
    ?assertEqual(none, macula_station_check:classify({error, not_found}, realm(), station())).

a_failed_lookup_is_unreadable_test() ->
    ?assertEqual(unreadable, macula_station_check:classify({error, timeout}, realm(), station())).

a_record_of_another_type_is_unreadable_test() ->
    ?assertEqual(unreadable, macula_station_check:classify({ok, macula_record:station_endpoint(4433)},
                                                           realm(), station())).

%%------------------------------------------------------------------
%% enforce: what a verdict means under each mode
%%------------------------------------------------------------------

live_admits_under_every_mode_test() ->
    [?assertEqual(admit, macula_station_check:enforce(Mode, live)) || Mode <- [open, log, closed]].

a_non_live_verdict_is_a_would_refuse_under_open_and_log_test() ->
    [?assertEqual({would_refuse, Verdict}, macula_station_check:enforce(Mode, Verdict))
     || Mode <- [open, log], Verdict <- [tombstoned, none, unreadable]].

under_closed_a_non_live_verdict_refuses_test() ->
    [?assertEqual({refuse, Verdict}, macula_station_check:enforce(closed, Verdict))
     || Verdict <- [tombstoned, none, unreadable]].

%%------------------------------------------------------------------
%% mode: fleet config, `open' unless set
%%------------------------------------------------------------------

the_mode_defaults_to_open_test() ->
    application:unset_env(macula, station_authorization_mode),
    ?assertEqual(open, macula_station_check:mode()).

the_mode_reads_from_config_test() ->
    with_mode(log, fun() -> ?assertEqual(log, macula_station_check:mode()) end),
    with_mode(closed, fun() -> ?assertEqual(closed, macula_station_check:mode()) end).

an_unknown_mode_reads_as_open_test() ->
    with_mode(bogus, fun() -> ?assertEqual(open, macula_station_check:mode()) end).

%%------------------------------------------------------------------
%% authorized: the slot it reads and the outcome it counts
%%------------------------------------------------------------------

authorized_reads_the_slot_for_exactly_the_dialed_pair_test() ->
    Self = self(),
    Read = fun(_Pool, Key, _TimeoutMs) -> Self ! {asked, Key}, {ok, authorization()} end,
    with_mode(open, fun() ->
        ?assertEqual(ok, macula_station_check:authorized(dial(Read), realm(), station(), 1000))
    end),
    receive {asked, Key} -> ?assertEqual(macula_record:station_authorization_key(realm(), station()), Key)
    after 100 -> ?assert(false)
    end,
    ?assertEqual(live, casted()).

authorized_admits_and_counts_a_live_authorization_test() ->
    with_mode(open, fun() ->
        ?assertEqual(ok, macula_station_check:authorized(dial({ok, authorization()}), realm(), station(), 1000))
    end),
    ?assertEqual(live, casted()).

authorized_admits_and_counts_a_would_refuse_under_open_test() ->
    with_mode(open, fun() ->
        ?assertEqual(ok, macula_station_check:authorized(dial({error, not_found}), realm(), station(), 1000))
    end),
    ?assertEqual(would_refuse, casted()).

authorized_admits_a_live_authorization_under_closed_test() ->
    with_mode(closed, fun() ->
        ?assertEqual(ok, macula_station_check:authorized(dial({ok, authorization()}), realm(), station(), 1000))
    end),
    ?assertEqual(live, casted()).

authorized_refuses_and_counts_under_closed_test() ->
    with_mode(closed, fun() ->
        ?assertEqual({error, {station_not_authorized, none}},
                     macula_station_check:authorized(dial({error, not_found}), realm(), station(), 1000))
    end),
    ?assertEqual(refused, casted()).

authorized_refuses_a_tombstoned_slot_under_closed_test() ->
    with_mode(closed, fun() ->
        ?assertEqual({error, {station_not_authorized, tombstoned}},
                     macula_station_check:authorized(dial({ok, tombstone()}), realm(), station(), 1000))
    end),
    ?assertEqual(refused, casted()).
