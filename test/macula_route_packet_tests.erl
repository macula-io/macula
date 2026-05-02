%%% @doc Tests for the macula-net egress routing facade.
-module(macula_route_packet_tests).

-include_lib("eunit/include/eunit.hrl").

-define(OWN, <<16#fd, 1:40, 16#01:80>>).
-define(REMOTE_A, <<16#fd, 1:40, 16#aa:80>>).
-define(REMOTE_B, <<16#fd, 1:40, 16#bb:80>>).

ipv6_packet_to(Dst) ->
    Header = <<6:4, 0:8, 0:20,
               8:16, 59:8, 64:8,
               ?OWN/binary, Dst/binary>>,
    <<Header/binary, 1, 2, 3, 4, 5, 6, 7, 8>>.

setup() ->
    Self = self(),
    SendFun = fun(StationId, Cbor) ->
        Self ! {sent, StationId, Cbor},
        ok
    end,
    Stations = [
        #{address => ?REMOTE_A, station => <<"station-a">>, send => SendFun},
        #{address => ?REMOTE_B, station => <<"station-b">>, send => SendFun}
    ],
    ok = macula_route_packet:configure(#{own_address => ?OWN, stations => Stations}),
    SendFun.

cleanup() ->
    receive _ -> cleanup() after 0 -> ok end.

%% =============================================================================
%% Configure + lookup
%% =============================================================================

configure_then_lookup_hit_test() ->
    _ = setup(),
    {ok, #{station := <<"station-a">>}} = macula_route_packet:lookup(?REMOTE_A),
    cleanup().

lookup_miss_returns_not_found_test() ->
    _ = setup(),
    ?assertEqual(not_found, macula_route_packet:lookup(<<0:128>>)),
    cleanup().

routes_returns_configured_entries_test() ->
    _ = setup(),
    Routes = macula_route_packet:routes(),
    ?assertEqual(2, length(Routes)),
    Addresses = [maps:get(address, R) || R <- Routes],
    ?assert(lists:member(?REMOTE_A, Addresses)),
    ?assert(lists:member(?REMOTE_B, Addresses)),
    cleanup().

reconfigure_replaces_table_test() ->
    _ = setup(),
    ok = macula_route_packet:configure(#{own_address => ?OWN, stations => []}),
    ?assertEqual([], macula_route_packet:routes()),
    ?assertEqual(not_found, macula_route_packet:lookup(?REMOTE_A)),
    cleanup().

%% =============================================================================
%% Dispatch flow
%% =============================================================================

dispatch_known_destination_calls_send_test() ->
    _ = setup(),
    Packet = ipv6_packet_to(?REMOTE_A),
    {ok, <<"station-a">>} = macula_route_packet:dispatch(Packet),
    receive
        {sent, <<"station-a">>, Cbor} ->
            {ok, E} = macula_cbor_nif:unpack(Cbor),
            ?assertEqual(?OWN, maps:get(<<"src">>, E)),
            ?assertEqual(?REMOTE_A, maps:get(<<"dst">>, E)),
            ?assertEqual(<<"data">>, maps:get(<<"type">>, E)),
            ?assertEqual(1, maps:get(<<"v">>, E)),
            ?assertEqual(64, maps:get(<<"ttl">>, E)),
            ?assertEqual(Packet, maps:get(<<"payload">>, E))
    after 1000 ->
        ?assert(false)
    end,
    cleanup().

dispatch_unknown_destination_returns_no_route_test() ->
    _ = setup(),
    UnknownDst = <<16#fd, 1:40, 16#cc:80>>,
    Packet = ipv6_packet_to(UnknownDst),
    ?assertEqual({error, no_route}, macula_route_packet:dispatch(Packet)),
    cleanup().

dispatch_malformed_packet_returns_error_test() ->
    _ = setup(),
    ?assertEqual({error, malformed_packet}, macula_route_packet:dispatch(<<1, 2, 3>>)),
    cleanup().

dispatch_propagates_send_failure_test() ->
    FailSend = fun(_S, _C) -> {error, mocked_failure} end,
    Stations = [#{address => ?REMOTE_A, station => <<"x">>, send => FailSend}],
    ok = macula_route_packet:configure(#{own_address => ?OWN, stations => Stations}),
    ?assertEqual({error, mocked_failure},
                 macula_route_packet:dispatch(ipv6_packet_to(?REMOTE_A))),
    cleanup().

%% =============================================================================
%% Encapsulate
%% =============================================================================

encapsulate_round_trips_test() ->
    Cbor = macula_route_packet:encapsulate(<<"hello">>, ?OWN, ?REMOTE_A),
    {ok, E} = macula_cbor_nif:unpack(Cbor),
    ?assertEqual(?OWN, maps:get(<<"src">>, E)),
    ?assertEqual(?REMOTE_A, maps:get(<<"dst">>, E)),
    ?assertEqual(<<"hello">>, maps:get(<<"payload">>, E)),
    ?assertEqual(64, maps:get(<<"ttl">>, E)).
