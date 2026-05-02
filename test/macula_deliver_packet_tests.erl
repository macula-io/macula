%%% @doc Tests for the macula-net ingress delivery slice.
-module(macula_deliver_packet_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LOCAL_A, <<16#fd, 1:40, 16#aa:80>>).
-define(LOCAL_B, <<16#fd, 1:40, 16#bb:80>>).
-define(REMOTE,  <<16#fd, 1:40, 16#cc:80>>).

setup() ->
    Self = self(),
    Writer = fun(P) -> Self ! {tun_wrote, P}, ok end,
    ok = macula_deliver_packet:configure(#{
        local_addresses => [?LOCAL_A, ?LOCAL_B],
        tun_writer      => Writer
    }).

cleanup() ->
    receive _ -> cleanup() after 0 -> ok end.

envelope_to(Dst, Payload) ->
    macula_cbor_nif:pack(#{
        <<"v">>       => 1,
        <<"type">>    => <<"data">>,
        <<"src">>     => ?REMOTE,
        <<"dst">>     => Dst,
        <<"ttl">>     => 64,
        <<"payload">> => Payload
    }).

%% =============================================================================
%% Configuration guards
%% =============================================================================

handle_without_configure_returns_not_configured_test() ->
    catch ets:delete(macula_deliver_packet_table),
    ?assertEqual({error, not_configured},
                 macula_deliver_packet:handle_envelope(<<>>)).

%% =============================================================================
%% Happy path
%% =============================================================================

deliver_local_address_writes_to_tun_test() ->
    setup(),
    Payload = <<"a real ipv6 packet would go here">>,
    Cbor = envelope_to(?LOCAL_A, Payload),
    ok = macula_deliver_packet:handle_envelope(Cbor),
    receive
        {tun_wrote, Got} -> ?assertEqual(Payload, Got)
    after 1000 ->
        ?assert(false)
    end,
    cleanup().

deliver_either_local_address_succeeds_test() ->
    setup(),
    ok = macula_deliver_packet:handle_envelope(envelope_to(?LOCAL_A, <<"x">>)),
    receive {tun_wrote, <<"x">>} -> ok after 1000 -> ?assert(false) end,
    ok = macula_deliver_packet:handle_envelope(envelope_to(?LOCAL_B, <<"y">>)),
    receive {tun_wrote, <<"y">>} -> ok after 1000 -> ?assert(false) end,
    cleanup().

%% =============================================================================
%% Validation
%% =============================================================================

unknown_destination_returns_no_route_test() ->
    setup(),
    Cbor = envelope_to(?REMOTE, <<"x">>),
    ?assertEqual({error, no_route}, macula_deliver_packet:handle_envelope(Cbor)),
    cleanup().

malformed_cbor_returns_decode_failed_test() ->
    setup(),
    ?assertEqual({error, decode_failed},
                 macula_deliver_packet:handle_envelope(<<255, 255, 255>>)),
    cleanup().

unknown_version_returns_version_unsupported_test() ->
    setup(),
    Cbor = macula_cbor_nif:pack(#{
        <<"v">>       => 99,
        <<"type">>    => <<"data">>,
        <<"src">>     => ?REMOTE,
        <<"dst">>     => ?LOCAL_A,
        <<"ttl">>     => 64,
        <<"payload">> => <<"x">>
    }),
    ?assertEqual({error, version_unsupported},
                 macula_deliver_packet:handle_envelope(Cbor)),
    cleanup().

ctrl_message_does_not_error_test() ->
    setup(),
    Cbor = macula_cbor_nif:pack(#{
        <<"v">>       => 1,
        <<"type">>    => <<"ctrl">>,
        <<"src">>     => ?REMOTE,
        <<"dst">>     => ?LOCAL_A,
        <<"ttl">>     => 64,
        <<"payload">> => <<"placeholder">>
    }),
    ?assertEqual(ok, macula_deliver_packet:handle_envelope(Cbor)),
    receive
        {tun_wrote, _} -> ?assert(false)
    after 100 -> ok
    end,
    cleanup().

tun_writer_failure_propagates_test() ->
    Writer = fun(_P) -> {error, mocked_eio} end,
    ok = macula_deliver_packet:configure(#{
        local_addresses => [?LOCAL_A],
        tun_writer      => Writer
    }),
    Cbor = envelope_to(?LOCAL_A, <<"x">>),
    ?assertEqual({error, tun_write_failed},
                 macula_deliver_packet:handle_envelope(Cbor)),
    cleanup().

reconfigure_replaces_local_set_test() ->
    setup(),
    ok = macula_deliver_packet:configure(#{
        local_addresses => [?LOCAL_B],
        tun_writer      => fun(_) -> ok end
    }),
    ?assertEqual({error, no_route},
                 macula_deliver_packet:handle_envelope(envelope_to(?LOCAL_A, <<"x">>))),
    ?assertEqual(ok,
                 macula_deliver_packet:handle_envelope(envelope_to(?LOCAL_B, <<"y">>))),
    cleanup().
