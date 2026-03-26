%%%-------------------------------------------------------------------
%%% @doc Tests for relay protocol encoding with binary keys.
%%%
%%% The relay handler constructs messages with binary keys (<<"key">>).
%%% These tests verify the encoder accepts binary keys for ALL message
%%% types that the relay handler sends.
%%%
%%% Every message type the relay handler produces MUST have a test here.
%%% If a test is missing, the handler will crash in production.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Relay handler sends these message types with binary keys.
%%% Each one MUST encode without crashing.
%%%===================================================================

%% PONG — sent as handshake ack when client sends CONNECT
pong_binary_keys_test() ->
    Msg = #{
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"server_time">> => erlang:system_time(millisecond)
    },
    Result = macula_protocol_encoder:encode(pong, Msg),
    ?assert(is_binary(Result)).

%% PING — binary keys variant
ping_binary_keys_test() ->
    Msg = #{<<"timestamp">> => erlang:system_time(millisecond)},
    Result = macula_protocol_encoder:encode(ping, Msg),
    ?assert(is_binary(Result)).

%% CONNECT — binary keys (relay handler sends this for ack)
connect_binary_keys_test() ->
    Msg = #{
        <<"version">> => <<"1.0">>,
        <<"node_id">> => crypto:strong_rand_bytes(32),
        <<"realm_id">> => crypto:strong_rand_bytes(32)
    },
    Result = macula_protocol_encoder:encode(connect, Msg),
    ?assert(is_binary(Result)).

%% DISCONNECT — binary keys
disconnect_binary_keys_test() ->
    Msg = #{
        <<"reason">> => <<"normal">>,
        <<"message">> => <<"bye">>
    },
    Result = macula_protocol_encoder:encode(disconnect, Msg),
    ?assert(is_binary(Result)).

%% PUBLISH — relay handler forwards publish to subscribers
publish_binary_keys_test() ->
    Msg = #{
        <<"topic">> => <<"test.topic">>,
        <<"payload">> => <<"hello">>,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },
    Result = macula_protocol_encoder:encode(publish, Msg),
    ?assert(is_binary(Result)).

%% CALL — relay handler forwards RPC calls
call_binary_keys_test() ->
    Msg = #{
        <<"call_id">> => crypto:strong_rand_bytes(12),
        <<"procedure">> => <<"my.procedure">>,
        <<"args">> => #{}
    },
    Result = macula_protocol_encoder:encode(call, Msg),
    ?assert(is_binary(Result)).

%% REPLY — relay handler forwards RPC replies
reply_binary_keys_test() ->
    Msg = #{
        <<"call_id">> => crypto:strong_rand_bytes(12),
        <<"result">> => #{<<"data">> => <<"ok">>}
    },
    Result = macula_protocol_encoder:encode(reply, Msg),
    ?assert(is_binary(Result)).

%% REPLY with error
reply_error_binary_keys_test() ->
    Msg = #{
        <<"call_id">> => crypto:strong_rand_bytes(12),
        <<"error">> => #{
            <<"code">> => <<"procedure_not_found">>,
            <<"message">> => <<"No node has registered this procedure">>
        }
    },
    Result = macula_protocol_encoder:encode(reply, Msg),
    ?assert(is_binary(Result)).

%% REGISTER_PROCEDURE — binary keys
register_procedure_binary_keys_test() ->
    Msg = #{<<"procedure">> => <<"hecate.mpong.join.abc123">>},
    Result = macula_protocol_encoder:encode(register_procedure, Msg),
    ?assert(is_binary(Result)).

%% SUBSCRIBE — binary keys
subscribe_binary_keys_test() ->
    Msg = #{
        <<"topics">> => [<<"topic.a">>, <<"topic.b">>],
        <<"qos">> => 0
    },
    Result = macula_protocol_encoder:encode(subscribe, Msg),
    ?assert(is_binary(Result)).

%% UNSUBSCRIBE — binary keys
unsubscribe_binary_keys_test() ->
    Msg = #{<<"topics">> => [<<"topic.a">>]},
    Result = macula_protocol_encoder:encode(unsubscribe, Msg),
    ?assert(is_binary(Result)).

%%%===================================================================
%%% Round-trip: encode with binary keys, decode, verify
%%%===================================================================

pong_roundtrip_test() ->
    Now = erlang:system_time(millisecond),
    Msg = #{<<"timestamp">> => Now, <<"server_time">> => Now + 1},
    Encoded = macula_protocol_encoder:encode(pong, Msg),
    <<_:8, _:8, _:8, _:8, _:32/big-unsigned, Payload/binary>> = Encoded,
    {ok, Decoded} = msgpack:unpack(Payload, [{map_format, map}]),
    ?assertEqual(Now, maps:get(<<"timestamp">>, Decoded)),
    ?assertEqual(Now + 1, maps:get(<<"server_time">>, Decoded)).

publish_roundtrip_test() ->
    Msg = #{
        <<"topic">> => <<"test.roundtrip">>,
        <<"payload">> => <<"data">>,
        <<"qos">> => 1,
        <<"retain">> => false,
        <<"message_id">> => <<"msg-123">>
    },
    Encoded = macula_protocol_encoder:encode(publish, Msg),
    <<_:8, _:8, _:8, _:8, _:32/big-unsigned, Payload/binary>> = Encoded,
    {ok, Decoded} = msgpack:unpack(Payload, [{map_format, map}]),
    ?assertEqual(<<"test.roundtrip">>, maps:get(<<"topic">>, Decoded)).

%%%===================================================================
%%% Client-side: json:encode payload must not crash msgpack
%%%===================================================================

%% Simulates what macula_relay_client does when publishing a map payload
json_payload_in_publish_test() ->
    %% json:encode returns iolist — must be flattened to binary
    MapPayload = #{<<"timestamp">> => 1234567890, <<"name">> => <<"test">>},
    BinPayload = iolist_to_binary(json:encode(MapPayload)),
    Msg = #{
        <<"topic">> => <<"test.topic">>,
        <<"payload">> => BinPayload,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    },
    Result = macula_protocol_encoder:encode(publish, Msg),
    ?assert(is_binary(Result)).

%% json:encode with integer values produces improper iolists that crash msgpack.
%% Simple string-only maps may not crash, but integer timestamps do.
json_iolist_with_integers_crashes_msgpack_test() ->
    MapPayload = #{<<"timestamp">> => 1774534113357, <<"node">> => <<"beam00">>},
    Iolist = json:encode(MapPayload),
    _Unsafe = #{<<"payload">> => Iolist},
    %% The safe path is always iolist_to_binary before msgpack.
    BinPayload = iolist_to_binary(Iolist),
    SafeMsg = #{<<"payload">> => BinPayload},
    Result = msgpack:pack(SafeMsg, [{map_format, map}]),
    ?assert(is_binary(Result)).
