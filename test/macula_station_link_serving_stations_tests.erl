%% EUnit tests for the rule macula#201's amendment added: which station ids an
%% advertisement body may name, decided by the tier of the link that SIGNS it.
%%
%% The body is per signing link (that is how the code already worked); what is
%% new is the station list inside it. A public-signed body must never name a
%% fallback station; a fallback-signed body names its own station and may name
%% public ones, because a LAN caller resolving through the fallback has to be
%% given a station it can dial during the outage the fallback exists for.
%%
%% macula#116 stays exact: a pin names the stations that hold the procedure.
%% Naming fewer holders is allowed; naming a non-holder is not, which is why an
%% unclassified pinned station is left off rather than guessed onto the body.
-module(macula_station_link_serving_stations_tests).

-include_lib("eunit/include/eunit.hrl").

body_test_() ->
    [fun a_public_body_names_no_fallback_station/0,
     fun a_fallback_body_names_its_own_station_and_may_name_public_ones/0,
     fun a_public_body_with_no_fallback_configured_names_every_connected_station/0,
     fun a_pin_spanning_both_tiers_yields_two_truthful_bodies/0,
     fun a_pinned_station_the_pool_cannot_classify_is_left_off_a_public_body/0].

%% §9 scenario 10, the record side: nothing a public station stores names the
%% fallback station.
a_public_body_names_no_fallback_station() ->
    PoolStations = [{lan_station, fallback}, {other_public, public}],
    Body = macula_station_link:serving_stations(pub_station, PoolStations, #{}, public),
    ?assertEqual([other_public, pub_station], Body),
    ?assertNot(lists:member(lan_station, Body)).

%% §9 scenario 10, the outage side: the body the fallback station holds names
%% the station a LAN caller can actually reach.
a_fallback_body_names_its_own_station_and_may_name_public_ones() ->
    PoolStations = [{lan_station, fallback}, {other_public, public}],
    Body = macula_station_link:serving_stations(lan_station, PoolStations, #{}, fallback),
    ?assertEqual([lan_station, other_public], Body).

%% With no fallback configured nothing changes: today's body, in today's shape
%% (this link's station first, then up to seven of the pool's others, sorted).
a_public_body_with_no_fallback_configured_names_every_connected_station() ->
    PoolStations = [{a_public, public}, {b_public, public}],
    ?assertEqual([a_public, b_public, signing_station],
                 macula_station_link:serving_stations(signing_station, PoolStations, #{}, public)).

%% A pin spanning both tiers is answered per link: the public link's body names
%% the pinned public stations, the fallback link's body names its own station
%% too. Both are truthful — every station it names holds the procedure (#116).
a_pin_spanning_both_tiers_yields_two_truthful_bodies() ->
    Pinned = #{serving_stations => [pinned_public, pinned_fallback]},
    PoolStations = [{pinned_public, public}, {pinned_fallback, fallback}],
    ?assertEqual([pinned_public, pub_station],
                 macula_station_link:serving_stations(pub_station, PoolStations, Pinned, public)),
    ?assertEqual([lan_station, pinned_fallback, pinned_public],
                 macula_station_link:serving_stations(lan_station, PoolStations, Pinned, fallback)).

%% Fail closed on a public body: a pinned station whose link is down is not
%% registered there right now, and one the pool cannot classify is not provably
%% public. Leaving it off may cost a dial target; naming it could leak the LAN.
a_pinned_station_the_pool_cannot_classify_is_left_off_a_public_body() ->
    Pinned = #{serving_stations => [unclassifiable]},
    ?assertEqual([pub_station],
                 macula_station_link:serving_stations(pub_station, [], Pinned, public)),
    ?assertEqual([lan_station, unclassifiable],
                 macula_station_link:serving_stations(lan_station, [], Pinned, fallback)).
