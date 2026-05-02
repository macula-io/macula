%%%-------------------------------------------------------------------
%%% @doc Tests for macula_route_packet's DHT mode (Phase 2 §4.4 /
%%% milestone 2.5). Confirms cache-first lookup, DHT-resolve on miss,
%%% connect_fn invocation, cache insertion, and dispatch byte-flow.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_route_packet_dht_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<16#11:256>>).
-define(TEST_SINK, macula_route_packet_dht_tests_sink).

%% =============================================================================
%% Mock DHT
%% =============================================================================

new_dht() ->
    case ets:info(mock_dht_dht) of
        undefined -> ets:new(mock_dht_dht, [set, public, named_table]);
        _ ->
            ets:delete_all_objects(mock_dht_dht),
            mock_dht_dht
    end.

put_dht(Tab, Record) ->
    Key = macula_record:storage_key(Record),
    true = ets:insert(Tab, {Key, Record}),
    Key.

find_fn(Tab) ->
    fun(Key) ->
        case ets:lookup(Tab, Key) of
            [{_, R}] -> {ok, R};
            []       -> {error, not_found}
        end
    end.

new_station() ->
    Kp = macula_identity:generate(),
    Pk = macula_identity:public(Kp),
    Addr = macula_address:derive(?REALM, Pk),
    #{kp => Kp, pk => Pk, addr => Addr}.

advertise(Tab, #{kp := Kp, pk := Pk, addr := Addr}, QuicPort, Hosts) ->
    Endpoint = macula_record:sign(
                 macula_record:station_endpoint(Pk, QuicPort, #{
                     host_advertised => Hosts}), Kp),
    Redirect = macula_record:sign(
                 macula_record:address_pubkey_map(Pk, Addr), Kp),
    put_dht(Tab, Endpoint),
    put_dht(Tab, Redirect),
    ok.

%% =============================================================================
%% IPv6 packet helper
%% =============================================================================

ipv6_packet(Src, Dst) ->
    Payload = <<1, 2, 3, 4>>,
    Hdr = <<6:4, 0:8, 0:20,
            (byte_size(Payload)):16, 59:8, 64:8,
            Src/binary, Dst/binary>>,
    <<Hdr/binary, Payload/binary>>.

%% =============================================================================
%% Tests
%% =============================================================================

route_packet_dht_test_() ->
    {inorder, [
        ?_test(cold_miss_resolves_connects_caches_sends()),
        ?_test(warm_hit_skips_dht()),
        ?_test(unknown_address_returns_no_route()),
        ?_test(spoofed_record_returns_bad_address_binding()),
        ?_test(empty_host_advertised_returns_no_route()),
        ?_test(connect_failure_propagates())
    ]}.

cleanup_state() ->
    catch macula_cache_route:stop(),
    case ets:info(macula_route_packet_table) of
        undefined -> ok;
        _ -> ets:delete(macula_route_packet_table)
    end,
    case ets:info(macula_cache_route_table) of
        undefined -> ok;
        _ -> ets:delete(macula_cache_route_table)
    end.

start_cache() ->
    {ok, _} = macula_cache_route:start_link(#{sweep_ms => 60_000}).

configure_dht(Tab) ->
    register_test_sink(),
    Resolver = #{
        realm_pubkey => ?REALM,
        find_fn      => find_fn(Tab),
        connect_fn   => fun(Pk, Host, Port) ->
            ?TEST_SINK ! {connect, Pk, Host, Port}, ok
        end,
        send_fn      => fun(Pk, Cbor) ->
            ?TEST_SINK ! {send, Pk, Cbor}, ok
        end
    },
    OwnAddr = <<16#fd, 16#aa:120>>,
    ok = macula_route_packet:configure(#{own_address => OwnAddr,
                                         resolver    => Resolver}),
    OwnAddr.

register_test_sink() ->
    catch unregister(?TEST_SINK),
    true = register(?TEST_SINK, self()).

drain(N) -> drain(N, []).
drain(0, Acc) -> lists:reverse(Acc);
drain(N, Acc) ->
    receive M -> drain(N-1, [M | Acc]) after 1000 -> lists:reverse(Acc) end.

cold_miss_resolves_connects_caches_sends() ->
    cleanup_state(),
    start_cache(),
    Tab = new_dht(),
    St  = new_station(),
    advertise(Tab, St, 4400, [<<"192.168.1.12">>]),

    OwnAddr = configure_dht(Tab),
    Pkt = ipv6_packet(OwnAddr, maps:get(addr, St)),
    {ok, StationId} = macula_route_packet:dispatch(Pkt),
    ?assertEqual(maps:get(pk, St), StationId),

    Msgs = drain(2),
    ?assertMatch([{connect, _, <<"192.168.1.12">>, 4400}, {send, _, _}], Msgs),
    [{connect, ConnPk, _, _}, {send, SendPk, Cbor}] = Msgs,
    ?assertEqual(maps:get(pk, St), ConnPk),
    ?assertEqual(maps:get(pk, St), SendPk),
    ?assert(is_binary(Cbor)),

    %% Cache should now hold an entry for the resolved address.
    ?assertMatch({ok, _}, macula_cache_route:lookup(maps:get(addr, St))),
    cleanup_state().

warm_hit_skips_dht() ->
    cleanup_state(),
    start_cache(),
    Tab = new_dht(),
    St  = new_station(),
    advertise(Tab, St, 4400, [<<"192.168.1.12">>]),
    OwnAddr = configure_dht(Tab),

    %% First send populates the cache. Then we wipe the DHT — a
    %% subsequent send should still succeed because the cache has it.
    Pkt = ipv6_packet(OwnAddr, maps:get(addr, St)),
    {ok, _} = macula_route_packet:dispatch(Pkt),
    _ = drain(2),

    ets:delete_all_objects(Tab),

    {ok, _} = macula_route_packet:dispatch(Pkt),
    %% Only one message expected — `send' (no second connect because
    %% the connect_fn isn't invoked again on cache hit).
    Msgs = drain(1),
    ?assertMatch([{send, _, _}], Msgs),
    cleanup_state().

unknown_address_returns_no_route() ->
    cleanup_state(),
    start_cache(),
    Tab = new_dht(),
    OwnAddr = configure_dht(Tab),
    UnknownAddr = <<16#fd, 16#bb:120>>,
    Pkt = ipv6_packet(OwnAddr, UnknownAddr),
    ?assertEqual({error, no_route}, macula_route_packet:dispatch(Pkt)),
    cleanup_state().

spoofed_record_returns_bad_address_binding() ->
    cleanup_state(),
    start_cache(),
    Tab = new_dht(),
    %% Attacker signs a redirect for a victim's address.
    #{addr := VictimAddr} = new_station(),
    AttackerKp = macula_identity:generate(),
    AttackerPk = macula_identity:public(AttackerKp),
    Spoof = macula_record:sign(
              macula_record:address_pubkey_map(AttackerPk, VictimAddr),
              AttackerKp),
    put_dht(Tab, Spoof),

    OwnAddr = configure_dht(Tab),
    Pkt = ipv6_packet(OwnAddr, VictimAddr),
    ?assertEqual({error, bad_address_binding},
                 macula_route_packet:dispatch(Pkt)),
    %% No connect / send must have happened.
    ?assertEqual([], drain_now()),
    cleanup_state().

empty_host_advertised_returns_no_route() ->
    cleanup_state(),
    start_cache(),
    Tab = new_dht(),
    St  = new_station(),
    advertise(Tab, St, 4400, []), %% empty host_advertised
    OwnAddr = configure_dht(Tab),
    Pkt = ipv6_packet(OwnAddr, maps:get(addr, St)),
    ?assertEqual({error, no_route}, macula_route_packet:dispatch(Pkt)),
    cleanup_state().

connect_failure_propagates() ->
    cleanup_state(),
    start_cache(),
    Tab = new_dht(),
    St  = new_station(),
    advertise(Tab, St, 4400, [<<"10.99.0.42">>]),

    register_test_sink(),
    Resolver = #{
        realm_pubkey => ?REALM,
        find_fn      => find_fn(Tab),
        connect_fn   => fun(_Pk, _Host, _Port) -> {error, econnrefused} end,
        send_fn      => fun(_Pk, _Cbor) -> ?TEST_SINK ! send_should_not_happen, ok end
    },
    OwnAddr = <<16#fd, 16#aa:120>>,
    ok = macula_route_packet:configure(#{own_address => OwnAddr,
                                         resolver    => Resolver}),
    Pkt = ipv6_packet(OwnAddr, maps:get(addr, St)),
    ?assertEqual({error, econnrefused}, macula_route_packet:dispatch(Pkt)),
    %% No send should have been attempted.
    ?assertEqual([], drain_now()),
    cleanup_state().

drain_now() ->
    receive M -> [M | drain_now()] after 0 -> [] end.
