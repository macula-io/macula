%% `macula:ensure_station_link/4' hands a consumer a pool link to one pinned
%% station: a live one the pool already holds, or one it dials, owned and
%% monitored by the pool either way. macula-realm's overlay dials a peer's
%% station this way and drives the link with the facade's overlay calls
%% (12.6.0 removed the internal it used, macula_client:ensure_station_link/4,
%% as dead). The stations here are unreachable, so a dial ends without a
%% handshake: what is under test is the pool's side of it.
-module(macula_ensure_station_link_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SEED, #{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<1:256>>}).
-define(STATION, #{host => <<"127.0.0.1">>, port => 2, expected_node_id => <<2:256>>}).

ensure_station_link_test_() ->
    [{timeout, 30, Test} || Test <- [fun a_station_without_a_handshake_answers_not_connected_within_the_timeout/0,
                                      fun the_link_is_the_pools_and_is_reused/0,
                                      fun a_station_without_a_pin_is_refused_and_nothing_is_dialled/0,
                                      fun the_overlay_calls_reach_the_link/0]].

a_station_without_a_handshake_answers_not_connected_within_the_timeout() ->
    with_pool(fun(Pool) ->
        T0 = erlang:monotonic_time(millisecond),
        ?assertEqual({error, not_connected}, macula:ensure_station_link(Pool, ?STATION, #{}, 300)),
        ?assert(erlang:monotonic_time(millisecond) - T0 < 2_000)
    end).

%% The dialled link joins the pool (it respawns and dies with it), and a
%% second call reaches the same link rather than dialling another.
the_link_is_the_pools_and_is_reused() ->
    with_pool(fun(Pool) ->
        _ = macula:ensure_station_link(Pool, ?STATION, #{}, 200),
        First = station_link(Pool),
        ?assert(is_pid(First)),
        _ = macula:ensure_station_link(Pool, ?STATION, #{}, 200),
        ?assertEqual(First, station_link(Pool)),
        ?assertEqual(2, length(links(Pool)))
    end).

a_station_without_a_pin_is_refused_and_nothing_is_dialled() ->
    with_pool(fun(Pool) ->
        ?assertMatch({error, _}, macula:ensure_station_link(Pool, maps:remove(expected_node_id, ?STATION), #{}, 200)),
        ?assertEqual(1, length(links(Pool)))
    end).

%% The facade's overlay calls drive the link a consumer was handed: a link
%% not yet connected takes a subscription and refuses a frame by name.
the_overlay_calls_reach_the_link() ->
    with_pool(fun(Pool) ->
        _ = macula:ensure_station_link(Pool, ?STATION, #{}, 200),
        Link = station_link(Pool),
        {ok, Ref} = macula:overlay_subscribe(Link, <<0:256>>, self()),
        ?assertEqual(ok, macula:overlay_unsubscribe(Link, Ref)),
        Frame = macula_frame:ping(#{nonce => crypto:strong_rand_bytes(16)}),
        ?assertEqual({error, not_connected}, macula:send_overlay_frame(Link, Frame)),
        ?assertEqual({error, not_connected}, macula:send_overlay_frame(Link, <<3:256>>, Frame))
    end).

%%%===================================================================
%%% Helpers
%%%===================================================================

with_pool(Test) ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([?SEED], #{}),
    try Test(Pool)
    after ok = macula_client:close(Pool)
    end.

links(Pool) ->
    {ok, Links} = macula_client:links(Pool),
    Links.

station_link(Pool) ->
    first([P || #{seed := #{port := 2}, pid := P} <- links(Pool)]).

first([Pid | _]) -> Pid;
first([]) -> undefined.
