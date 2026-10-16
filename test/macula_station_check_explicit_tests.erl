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

%%------------------------------------------------------------------
%% A pool that pinned the realm's key: the slot is read and judged
%% (macula_station_check:explicit/4 -> authorized/4), and the station the
%% check reads is the one the link proves, the seed's pin before `Opts''
%%------------------------------------------------------------------

other_station() -> binary:copy(<<16#54>>, 32).

realm_key() ->
    case persistent_term:get({?MODULE, realm_key}, undefined) of
        undefined ->
            {ok, Profile} = macula_crypto_profile:configured(),
            {ok, Key} = macula_node_keys:generate(realm, Profile),
            persistent_term:put({?MODULE, realm_key}, Key),
            Key;
        Key ->
            Key
    end.

live_authorization() ->
    macula_record:sign(macula_record:station_authorization(realm(), #{realm => realm(), station_node => station(),
                                                                     hostname => <<"b.example">>,
                                                                     roles => [<<"station">>]}),
                       realm_key()).

%% A stand-in pool that pinned the realm key and whose station answers a
%% record lookup (`macula:find_record/3' asks the pool's linked station):
%% only station()'s slot holds the realm's live authorization.
with_pinned_pool(Fun) ->
    Test = self(),
    Pinned = macula_node_keys:public_key(realm_key()),
    Live = macula_record:station_authorization_key(realm(), station()),
    Wire = macula_record:encode(live_authorization()),
    Pool = spawn_link(fun Loop() ->
                          receive
                              {'$gen_call', From, {realm_key, _}} -> gen_server:reply(From, {ok, Pinned}), Loop();
                              {'$gen_cast', {station_check, _}} -> Loop();
                              {'$gen_call', From, {linked_station_call, _R, _P, #{key := Key}, _T}} ->
                                  gen_server:reply(From, {ok, dht_reply(found(Key, Live, Wire))}), Loop();
                              {'$gen_call', From, Msg} -> Test ! {pool_asked, element(1, Msg)},
                                                          gen_server:reply(From, {error, stand_in}), Loop()
                          end
                      end),
    Fun(Pool).

%% The station's `_dht.find_record' reply as the caller reads it: `macula:find_record/3'
%% goes through `dht_call/4' and `macula_dht_procedures:read_reply/2', which reads the one
%% key form a peer's payload arrives in (D26), so text reads as `{text, Bin}'. Built with
%% the module's own builder and put through the wire, as the dht conformance test does.
dht_reply(Result) ->
    {ok, Bytes} = macula_frame:payload_plain(macula_dht_procedures:reply(find_record, Result)),
    {ok, Value} = macula_record_cbor:decode_strict(Bytes),
    Value.

found(Key, Key, Wire) -> {record, Wire};
found(_Key, _Live, _Wire) -> not_found.

closed_dials_a_station_the_realm_authorized_test() ->
    with_mode(closed, fun() ->
        with_pinned_pool(fun(Pool) ->
            ?assertEqual({error, stand_in},
                         macula:call_station(Pool, seed(), target(), realm(), <<"acme/p">>, #{}, 2_000,
                                             #{confidential => off})),
            ?assertEqual(call_station, asked())
        end)
    end).

the_seed_pin_is_the_station_checked_before_the_options_pin_test() ->
    with_mode(closed, fun() ->
        with_pinned_pool(fun(Pool) ->
            %% The link proves the seed's pin (station(), authorized), not the
            %% options' (other_station(), nothing at its slot).
            ?assertEqual({error, stand_in},
                         macula:call_station(Pool, seed(), target(), realm(), <<"acme/p">>, #{}, 2_000,
                                             #{confidential => off, expected_node_id => other_station()})),
            ?assertEqual(call_station, asked()),
            Unauthorized = #{host => <<"2001:db8::6">>, port => 4433, expected_node_id => other_station()},
            ?assertEqual({error, {station_not_authorized, none}},
                         macula:call_station(Pool, Unauthorized, target(), realm(), <<"acme/p">>, #{}, 2_000,
                                             #{confidential => off, expected_node_id => station()})),
            ?assertEqual(nothing, asked())
        end)
    end).
