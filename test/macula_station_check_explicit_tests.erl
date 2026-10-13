%% EUnit tests for hook 3 on the dials an application names itself
%% (macula-station#63): `call_station/8', `call_stream_station/7',
%% `ensure_station_link/4' and a content fetch's sharer. Each checks the
%% station's authorization for the realm before it dials; under `closed' a
%% station without a live one is refused before the pool is asked to dial.
%% Direct dial checks the stations it resolves itself, so it dials through
%% the unchecked variants and is not checked twice.
-module(macula_station_check_explicit_tests).

-include_lib("eunit/include/eunit.hrl").

realm() -> binary:copy(<<16#51>>, 32).
station() -> binary:copy(<<16#52>>, 32).
target() -> binary:copy(<<16#53>>, 32).

with_mode(Mode, Fun) ->
    application:set_env(macula, station_authorization_mode, Mode),
    try Fun() after application:unset_env(macula, station_authorization_mode) end.

%% A stand-in pool that pinned no realm key: it answers `realm_key' with
%% `none', swallows the check's count, and reports every other call to the
%% test, answering it `{error, stand_in}'.
pool() ->
    Test = self(),
    spawn_link(fun Loop() ->
                   receive
                       {'$gen_call', From, {realm_key, _Realm}} -> gen_server:reply(From, none), Loop();
                       {'$gen_cast', {station_check, _}} -> Loop();
                       {'$gen_call', From, Msg} -> Test ! {pool_asked, element(1, Msg)},
                                                   gen_server:reply(From, {error, stand_in}), Loop();
                       stop -> ok
                   end
               end).

asked() ->
    receive {pool_asked, What} -> What after 200 -> nothing end.

seed() -> #{host => <<"2001:db8::5">>, port => 4433, expected_node_id => station()}.

%%------------------------------------------------------------------
%% call_station/8
%%------------------------------------------------------------------

closed_refuses_call_station_to_an_unauthorized_station_before_dialling_test() ->
    with_mode(closed, fun() ->
        Pool = pool(),
        ?assertEqual({error, {station_not_authorized, unanchored}},
                     macula:call_station(Pool, seed(), target(), realm(), <<"acme/p">>, #{}, 2_000,
                                         #{confidential => off})),
        ?assertEqual(nothing, asked())
    end).

closed_refuses_call_station_to_a_station_it_cannot_name_test() ->
    with_mode(closed, fun() ->
        Pool = pool(),
        ?assertEqual({error, {station_not_authorized, none}},
                     macula:call_station(Pool, <<"quic://[2001:db8::5]:4433">>, target(), realm(),
                                         <<"acme/p">>, #{}, 2_000, #{confidential => off})),
        ?assertEqual(nothing, asked())
    end).

open_counts_and_dials_test() ->
    with_mode(open, fun() ->
        Pool = pool(),
        ?assertEqual({error, stand_in},
                     macula:call_station(Pool, seed(), target(), realm(), <<"acme/p">>, #{}, 2_000,
                                         #{confidential => off})),
        ?assertEqual(call_station, asked())
    end).

%%------------------------------------------------------------------
%% call_stream_station/7 and ensure_station_link/4
%%------------------------------------------------------------------

closed_refuses_call_stream_station_before_dialling_test() ->
    with_mode(closed, fun() ->
        Pool = pool(),
        ?assertEqual({error, {station_not_authorized, unanchored}},
                     macula:call_stream_station(Pool, seed(), target(), realm(), <<"acme/s">>, #{},
                                                #{confidential => off})),
        ?assertEqual(nothing, asked())
    end).

closed_refuses_ensure_station_link_with_no_realm_named_test() ->
    with_mode(closed, fun() ->
        Pool = pool(),
        ?assertEqual({error, {station_not_authorized, unanchored}},
                     macula:ensure_station_link(Pool, seed(), #{}, 2_000)),
        ?assertEqual(nothing, asked())
    end).

closed_checks_ensure_station_link_for_the_realm_it_names_test() ->
    with_mode(closed, fun() ->
        Pool = pool(),
        ?assertEqual({error, {station_not_authorized, unanchored}},
                     macula:ensure_station_link(Pool, seed(), #{realm => realm()}, 2_000)),
        ?assertEqual(nothing, asked())
    end).

open_ensure_station_link_dials_test() ->
    with_mode(open, fun() ->
        Pool = pool(),
        ?assertEqual({error, stand_in}, macula:ensure_station_link(Pool, seed(), #{realm => realm()}, 2_000)),
        ?assertEqual(ensure_station_link, asked())
    end).

%%------------------------------------------------------------------
%% Direct dial is checked once, by itself
%%------------------------------------------------------------------

direct_dial_dials_through_the_unchecked_variants_test() ->
    #{call_station := CallStation, call_stream_station := CallStream} = macula_direct_dial:default_dial_io(),
    ?assertEqual(fun macula:call_resolved_station/8, CallStation),
    ?assertEqual(fun macula:call_resolved_stream_station/7, CallStream).
