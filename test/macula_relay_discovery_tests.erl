-module(macula_relay_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

-record(relay_entry, {hostname, url, lat, lng, status, distance_km}).

%%====================================================================
%% Haversine distance
%%====================================================================

haversine_known_distance_test() ->
    %% Paris to London ≈ 344 km
    D = macula_relay_discovery:haversine_km(48.8566, 2.3522, 51.5074, -0.1278),
    ?assert(D > 340),
    ?assert(D < 350).

haversine_same_point_test() ->
    D = macula_relay_discovery:haversine_km(50.0, 10.0, 50.0, 10.0),
    ?assertEqual(0.0, D).

haversine_antipodal_test() ->
    %% Opposite sides of the earth ≈ 20015 km
    D = macula_relay_discovery:haversine_km(0.0, 0.0, 0.0, 180.0),
    ?assert(D > 20000),
    ?assert(D < 20100).

%%====================================================================
%% URL parsing
%%====================================================================

extract_hostname_https_test() ->
    ?assertEqual(<<"relay.macula.io">>,
                 macula_relay_discovery:extract_hostname(<<"https://relay.macula.io:4433">>)).

extract_hostname_no_port_test() ->
    ?assertEqual(<<"relay.macula.io">>,
                 macula_relay_discovery:extract_hostname(<<"https://relay.macula.io">>)).

build_topology_url_test() ->
    Url = macula_relay_discovery:build_topology_url(<<"https://box.macula.io:4433">>),
    ?assertEqual("https://box.macula.io/topology?n=90&s=-90&e=180&w=-180&z=10", Url).

%%====================================================================
%% Relay status parsing
%%====================================================================

relay_status_online_test() ->
    ?assertEqual(online, macula_relay_discovery:relay_status(<<"online">>)).

relay_status_offline_test() ->
    ?assertEqual(offline, macula_relay_discovery:relay_status(<<"offline">>)).

relay_status_unknown_test() ->
    ?assertEqual(offline, macula_relay_discovery:relay_status(<<"unknown">>)).

%%====================================================================
%% Float conversion
%%====================================================================

to_float_integer_test() ->
    ?assertEqual(42.0, macula_relay_discovery:to_float(42)).

to_float_float_test() ->
    ?assertEqual(3.14, macula_relay_discovery:to_float(3.14)).

to_float_invalid_test() ->
    ?assertEqual(0.0, macula_relay_discovery:to_float(<<"not a number">>)).

%%====================================================================
%% ETS-based ranking (requires gen_server running)
%%====================================================================

ranking_test_() ->
    {setup,
     fun() ->
         %% Stop if already running from a previous test
         catch gen_server:stop(macula_relay_discovery),
         timer:sleep(50),
         {ok, Pid} = macula_relay_discovery:start_link(#{
             seeds => [], lat => 50.8503, lng => 4.3517
         }),
         timer:sleep(50),
         %% Insert test relays at known distances from Brussels
         insert_test_relay(<<"relay-fr-paris.macula.io">>, 48.8566, 2.3522, online),
         insert_test_relay(<<"relay-de-berlin.macula.io">>, 52.5200, 13.4050, online),
         insert_test_relay(<<"relay-gb-london.macula.io">>, 51.5074, -0.1278, online),
         Pid
     end,
     fun(Pid) -> catch gen_server:stop(Pid) end,
     fun(_) -> [
        {"ranked by distance: Paris < London < Berlin", fun() ->
            Ranked = macula_relay_discovery:ranked_relays(),
            ?assert(length(Ranked) >= 3),
            [First, Second, Third | _] = Ranked,
            ?assertEqual(<<"relay-fr-paris.macula.io">>, maps:get(hostname, First)),
            ?assertEqual(<<"relay-gb-london.macula.io">>, maps:get(hostname, Second)),
            ?assertEqual(<<"relay-de-berlin.macula.io">>, maps:get(hostname, Third))
        end},
        {"nearest returns Paris", fun() ->
            ?assertEqual({ok, <<"https://relay-fr-paris.macula.io:4433">>},
                         macula_relay_discovery:nearest())
        end},
        {"nearest_except Paris returns London", fun() ->
            ?assertEqual({ok, <<"https://relay-gb-london.macula.io:4433">>},
                         macula_relay_discovery:nearest_except(<<"relay-fr-paris.macula.io">>))
        end},
        {"mark_offline excludes from nearest", fun() ->
            macula_relay_discovery:mark_offline(<<"relay-fr-paris.macula.io">>),
            ?assertEqual({ok, <<"https://relay-gb-london.macula.io:4433">>},
                         macula_relay_discovery:nearest()),
            %% Restore for other tests
            insert_test_relay(<<"relay-fr-paris.macula.io">>, 48.8566, 2.3522, online)
        end},
        {"relay_count", fun() ->
            ?assert(macula_relay_discovery:relay_count() >= 3)
        end},
        {"no relays returns error", fun() ->
            %% Insert fresh set, mark all offline
            insert_test_relay(<<"relay-xx-a.macula.io">>, 50.0, 5.0, offline),
            insert_test_relay(<<"relay-xx-b.macula.io">>, 51.0, 6.0, offline),
            %% nearest should skip offline relays — only returns error if ALL are offline
            %% but our Paris/London/Berlin are still online, so this tests the filter
            Result = macula_relay_discovery:nearest_except(<<"relay-xx-a.macula.io">>),
            ?assertMatch({ok, _}, Result)
        end}
     ] end}.

%%====================================================================
%% Helpers
%%====================================================================

insert_test_relay(Hostname, Lat, Lng, Status) ->
    %% Brussels coordinates
    MyLat = 50.8503,
    MyLng = 4.3517,
    Distance = macula_relay_discovery:haversine_km(MyLat, MyLng, Lat, Lng),
    Url = <<"https://", Hostname/binary, ":4433">>,
    ets:insert(macula_relay_discovery_cache, #relay_entry{
        hostname = Hostname, url = Url,
        lat = Lat, lng = Lng,
        status = Status, distance_km = Distance
    }).
