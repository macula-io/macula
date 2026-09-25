%% `stations => [StationNodeId]' on an advertisement registers the procedure on
%% the links to those stations only, and a respawned link replays it only when
%% its station is one of them. A station the pool holds no link to is refused
%% by name, and nothing is registered or kept.
%%
%% The links never connect (their seeds are unreachable): a link registers a
%% procedure the pool hands it whether or not it is connected, which is what
%% dispatches a CALL once it does. Each link's station is the node_id its seed
%% pins.
-module(macula_advertise_stations_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(A, <<1:256>>).
-define(B, <<2:256>>).
-define(SEED_A, #{host => <<"127.0.0.1">>, port => 1, expected_node_id => ?A}).
-define(SEED_B, #{host => <<"127.0.0.1">>, port => 2, expected_node_id => ?B}).
-define(PROCEDURES_INDEX, macula_station_link:state_field_index(procedures)).
-define(STREAM_PROCEDURES_INDEX, macula_station_link:state_field_index(stream_procedures)).

stations_test_() ->
    [{timeout, 30, Test} || Test <- [fun an_advertisement_goes_to_the_named_stations_only/0,
                                      fun a_station_without_a_link_is_refused_and_nothing_is_kept/0,
                                      fun an_empty_or_malformed_list_is_refused/0,
                                      fun a_respawned_link_replays_only_what_names_its_station/0,
                                      fun a_stream_advertisement_goes_to_the_named_stations_only/0,
                                      fun without_stations_every_link_has_it/0]].

an_advertisement_goes_to_the_named_stations_only() ->
    with_pool(fun(Pool, Proc) ->
        ok = macula:advertise(Pool, ?REALM, Proc, fun handler/1, #{stations => [?A]}),
        ?assertEqual({true, false}, {holds(Pool, ?A, Proc), holds(Pool, ?B, Proc)})
    end).

a_station_without_a_link_is_refused_and_nothing_is_kept() ->
    with_pool(fun(Pool, Proc) ->
        ?assertEqual({error, {station_not_linked, <<3:256>>}},
                     macula:advertise(Pool, ?REALM, Proc, fun handler/1, #{stations => [?A, <<3:256>>]})),
        ?assertEqual({false, false}, {holds(Pool, ?A, Proc), holds(Pool, ?B, Proc)}),
        %% Not kept: a respawned link to A does not replay it.
        _ = respawned(Pool, ?A),
        ?assertNot(holds(Pool, ?A, Proc))
    end).

an_empty_or_malformed_list_is_refused() ->
    with_pool(fun(Pool, Proc) ->
        ?assertEqual({error, {stations, empty}},
                     macula:advertise(Pool, ?REALM, Proc, fun handler/1, #{stations => []})),
        [?assertEqual({error, {stations, malformed}},
                      macula:advertise(Pool, ?REALM, Proc, fun handler/1, #{stations => Bad}))
         || Bad <- [?A, [<<1:8>>], [?A | ?B], all]],
        ?assertEqual({false, false}, {holds(Pool, ?A, Proc), holds(Pool, ?B, Proc)})
    end).

a_respawned_link_replays_only_what_names_its_station() ->
    with_pool(fun(Pool, Proc) ->
        ok = macula:advertise(Pool, ?REALM, Proc, fun handler/1, #{stations => [?B]}),
        _ = respawned(Pool, ?A),
        _ = respawned(Pool, ?B),
        ?assertEqual({false, true}, {holds(Pool, ?A, Proc), holds(Pool, ?B, Proc)})
    end).

a_stream_advertisement_goes_to_the_named_stations_only() ->
    with_pool(fun(Pool, Proc) ->
        ok = macula:advertise_stream(Pool, ?REALM, Proc, server_stream, fun stream_handler/2,
                                     #{stations => [?B]}),
        ?assertEqual({false, true}, {holds_stream(Pool, ?A, Proc), holds_stream(Pool, ?B, Proc)}),
        ?assertEqual({error, {station_not_linked, <<3:256>>}},
                     macula:advertise_stream(Pool, ?REALM, <<Proc/binary, "_2">>, server_stream,
                                             fun stream_handler/2, #{stations => [<<3:256>>]}))
    end).

without_stations_every_link_has_it() ->
    with_pool(fun(Pool, Proc) ->
        ok = macula:advertise(Pool, ?REALM, Proc, fun handler/1, #{}),
        ?assertEqual({true, true}, {holds(Pool, ?A, Proc), holds(Pool, ?B, Proc)})
    end).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% A pool linked to A and B, and a procedure in its own namespace, which needs
%% no authorization chain.
with_pool(Test) ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([?SEED_A, ?SEED_B], #{}),
    try
        {ok, #{self_node_id := Me}} = macula:status(Pool),
        Test(Pool, <<"~", (binary:encode_hex(Me, lowercase))/binary, "/echo">>)
    after
        ok = macula_client:close(Pool)
    end.

handler(_Payload) -> {ok, <<>>}.

stream_handler(_Stream, _Args) -> ok.

holds(Pool, Station, Proc) ->
    is_map_key({?REALM, Proc}, element(?PROCEDURES_INDEX, sys:get_state(link_pid(Pool, Station)))).

holds_stream(Pool, Station, Proc) ->
    is_map_key({?REALM, Proc}, element(?STREAM_PROCEDURES_INDEX, sys:get_state(link_pid(Pool, Station)))).

link_pid(Pool, Station) ->
    {ok, Links} = macula_client:links(Pool),
    [Pid] = [P || #{seed := #{expected_node_id := S}, pid := P} <- Links, S =:= Station],
    Pid.

%% Kill the link to `Station' and wait for the pool to start it again.
respawned(Pool, Station) ->
    Old = link_pid(Pool, Station),
    Ref = erlang:monitor(process, Old),
    exit(Old, kill),
    receive {'DOWN', Ref, process, Old, _} -> ok after 2_000 -> error(link_not_down) end,
    wait_new(Pool, Station, Old, 100).

wait_new(_Pool, _Station, _Old, 0) ->
    error(no_respawn);
wait_new(Pool, Station, Old, N) ->
    case catch link_pid(Pool, Station) of
        Pid when is_pid(Pid), Pid =/= Old -> Pid;
        _ -> timer:sleep(50), wait_new(Pool, Station, Old, N - 1)
    end.
