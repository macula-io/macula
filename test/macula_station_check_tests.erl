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

%% The realm's signing key, and a stranger's key of the same purpose: any
%% process can mint a key of purpose `realm'. Generated once per run.
realm_key() -> key(realm_key).
stranger_key() -> key(stranger_key).

key(Name) ->
    case persistent_term:get({?MODULE, Name}, undefined) of
        undefined ->
            {ok, Key} = macula_node_keys:generate(realm, pq_pure),
            persistent_term:put({?MODULE, Name}, Key),
            Key;
        Key ->
            Key
    end.

pinned() -> macula_node_keys:public_key(realm_key()).

unsigned_authorization(Opts) ->
    macula_record:station_authorization(realm(), #{realm => realm(), station_node => station(),
                                                   hostname => <<"a.example">>, roles => [<<"station">>]}, Opts).

authorization() -> authorization(realm_key()).
authorization(Key) -> macula_record:sign(unsigned_authorization(#{}), Key).

tombstone() -> tombstone(realm_key()).
tombstone(Key) -> macula_record:sign(macula_record:tombstone(authorization(), revoked), Key).

trust() -> trust(realm(), station()).
trust(Realm, Station) ->
    #{realm => Realm, station => Station, realm_key => pinned(), now => erlang:system_time(millisecond)}.

%% A fake dial: the pool is this test process, so a check's cast lands in its
%% mailbox, the read is the one the test hands in — either a read result or a
%% three-argument read fun — and the pool pinned the realm's key.
dial(Read) when is_function(Read, 3) ->
    #{pool => self(), find_record => Read, realm_key => {ok, pinned()}};
dial(Read) ->
    dial(fun(_Pool, _Key, _TimeoutMs) -> Read end).

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
    ?assertEqual(live, macula_station_check:classify({ok, authorization()}, trust())).

an_authorization_for_another_station_is_none_test() ->
    ?assertEqual(none, macula_station_check:classify({ok, authorization()}, trust(realm(), other_station()))).

an_authorization_for_another_realm_is_none_test() ->
    ?assertEqual(none, macula_station_check:classify({ok, authorization()}, trust(other_realm(), station()))).

the_realms_tombstone_over_the_type_is_tombstoned_test() ->
    ?assertEqual(tombstoned, macula_station_check:classify({ok, tombstone()}, trust())).

a_realm_tombstone_over_another_type_is_unreadable_test() ->
    Endorsement = macula_record:realm_member_endorsement(realm(), #{realm => realm(), member_node => station(),
                                                                    roles => [<<"member">>]}),
    ?assertEqual(unreadable,
                 macula_station_check:classify({ok, macula_record:sign(macula_record:tombstone(Endorsement, revoked),
                                                                       realm_key())}, trust())).

nothing_at_the_slot_is_none_test() ->
    ?assertEqual(none, macula_station_check:classify({error, not_found}, trust())).

a_failed_lookup_is_unreadable_test() ->
    ?assertEqual(unreadable, macula_station_check:classify({error, timeout}, trust())).

a_record_of_another_type_is_unreadable_test() ->
    ?assertEqual(unreadable, macula_station_check:classify({ok, macula_record:station_endpoint(4433)}, trust())).

%% The signer is the realm's or the record counts for nothing: a thief who
%% mints a key of purpose `realm' can neither resurrect a revoked station nor
%% revoke a legitimate one.
an_authorization_a_stranger_signed_is_not_live_test() ->
    ?assertEqual(unreadable, macula_station_check:classify({ok, authorization(stranger_key())}, trust())).

a_tombstone_a_stranger_signed_is_not_tombstoned_test() ->
    ?assertEqual(unreadable, macula_station_check:classify({ok, tombstone(stranger_key())}, trust())).

an_unsigned_authorization_is_not_live_test() ->
    ?assertEqual(unreadable, macula_station_check:classify({ok, unsigned_authorization(#{})}, trust())).

%% The validity window is read: before it opens or after it closes (past the
%% records' clock tolerance) the authorization is stale, not live.
an_authorization_whose_window_has_closed_is_stale_test() ->
    Now = erlang:system_time(millisecond),
    Past = macula_record:sign(unsigned_authorization(#{valid_from => Now - 3_600_000,
                                                       valid_until => Now - 3_000_000}), realm_key()),
    ?assertEqual(stale, macula_station_check:classify({ok, Past}, trust())).

an_authorization_whose_window_has_not_opened_is_stale_test() ->
    Now = erlang:system_time(millisecond),
    Future = macula_record:sign(unsigned_authorization(#{valid_from => Now + 3_000_000,
                                                         valid_until => Now + 3_600_000}), realm_key()),
    ?assertEqual(stale, macula_station_check:classify({ok, Future}, trust())).

%%------------------------------------------------------------------
%% enforce: what a verdict means under each mode
%%------------------------------------------------------------------

live_admits_under_every_mode_test() ->
    [?assertEqual(admit, macula_station_check:enforce(Mode, live)) || Mode <- [open, log, closed]].

a_non_live_verdict_is_a_would_refuse_under_open_and_log_test() ->
    [?assertEqual({would_refuse, Verdict}, macula_station_check:enforce(Mode, Verdict))
     || Mode <- [open, log], Verdict <- [tombstoned, stale, none, unanchored, unreadable]].

under_closed_a_non_live_verdict_refuses_test() ->
    [?assertEqual({refuse, Verdict}, macula_station_check:enforce(closed, Verdict))
     || Verdict <- [tombstoned, stale, none, unanchored, unreadable]].

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

%% A pool that pinned no key for the realm has nothing to judge a record by:
%% the slot is not read, and under `closed' the station is refused.
authorized_without_a_pinned_realm_key_is_unanchored_test() ->
    Self = self(),
    Read = fun(_Pool, _Key, _TimeoutMs) -> Self ! read, {ok, authorization()} end,
    Dial = (dial(Read))#{realm_key := none},
    with_mode(closed, fun() ->
        ?assertEqual({error, {station_not_authorized, unanchored}},
                     macula_station_check:authorized(Dial, realm(), station(), 1000))
    end),
    ?assertEqual(refused, casted()),
    receive read -> ?assert(false) after 0 -> ok end.

authorized_refuses_a_stranger_signed_authorization_under_closed_test() ->
    with_mode(closed, fun() ->
        ?assertEqual({error, {station_not_authorized, unreadable}},
                     macula_station_check:authorized(dial({ok, authorization(stranger_key())}), realm(), station(),
                                                     1000))
    end),
    ?assertEqual(refused, casted()).
