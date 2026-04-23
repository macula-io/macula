%%% Roundtrip + edge case tests for the CBOR pack/unpack NIF.
%%%
%%% Verifies:
%%% - Common Erlang term shapes survive round-trip with the documented
%%%   lossiness (atoms become binaries on decode; tuples become lists).
%%% - The protocol-encoder's typical payload shape (map of binary keys
%%%   to mixed-type values) survives byte-identically modulo lossiness.
%%% - Malformed CBOR returns {error, _} from unpack/1 rather than
%%%   crashing the caller.
-module(macula_cbor_nif_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% NIF loading
%%====================================================================

nif_loaded_test() ->
    %% If the NIF didn't load, every other test in this module is
    %% meaningless. Fail loudly here so failure attribution is clear.
    ?assert(macula_cbor_nif:is_nif_loaded()).

%%====================================================================
%% Primitive roundtrips
%%====================================================================

integer_roundtrip_test() ->
    {ok, 42} = macula_cbor_nif:unpack(macula_cbor_nif:pack(42)),
    {ok, 0}  = macula_cbor_nif:unpack(macula_cbor_nif:pack(0)),
    {ok, -1} = macula_cbor_nif:unpack(macula_cbor_nif:pack(-1)),
    {ok, 9007199254740992} =
        macula_cbor_nif:unpack(macula_cbor_nif:pack(9007199254740992)).

float_roundtrip_test() ->
    {ok, F} = macula_cbor_nif:unpack(macula_cbor_nif:pack(3.14)),
    ?assert(abs(F - 3.14) < 1.0e-9).

binary_roundtrip_test() ->
    {ok, <<"hello">>} = macula_cbor_nif:unpack(macula_cbor_nif:pack(<<"hello">>)),
    {ok, <<>>}        = macula_cbor_nif:unpack(macula_cbor_nif:pack(<<>>)),
    Big = crypto:strong_rand_bytes(4096),
    {ok, Big}         = macula_cbor_nif:unpack(macula_cbor_nif:pack(Big)).

bool_roundtrip_test() ->
    {ok, true}  = macula_cbor_nif:unpack(macula_cbor_nif:pack(true)),
    {ok, false} = macula_cbor_nif:unpack(macula_cbor_nif:pack(false)).

null_roundtrip_test() ->
    {ok, nil} = macula_cbor_nif:unpack(macula_cbor_nif:pack(nil)),
    {ok, nil} = macula_cbor_nif:unpack(macula_cbor_nif:pack(undefined)).

list_roundtrip_test() ->
    {ok, [1, 2, 3]} = macula_cbor_nif:unpack(macula_cbor_nif:pack([1, 2, 3])),
    {ok, []}        = macula_cbor_nif:unpack(macula_cbor_nif:pack([])).

nested_list_roundtrip_test() ->
    {ok, [[1, 2], [3, 4]]} =
        macula_cbor_nif:unpack(macula_cbor_nif:pack([[1, 2], [3, 4]])).

%%====================================================================
%% Maps (the protocol's primary payload shape)
%%====================================================================

empty_map_roundtrip_test() ->
    {ok, M} = macula_cbor_nif:unpack(macula_cbor_nif:pack(#{})),
    ?assertEqual(#{}, M).

simple_map_roundtrip_test() ->
    Original = #{<<"version">> => 1, <<"node_id">> => <<"node-abc">>},
    {ok, Decoded} = macula_cbor_nif:unpack(macula_cbor_nif:pack(Original)),
    ?assertEqual(Original, Decoded).

protocol_payload_roundtrip_test() ->
    Payload = #{
        <<"type">>          => <<"call">>,
        <<"call_id">>       => <<"abc-123">>,
        <<"procedure">>     => <<"io.macula/_realm/_realm/auth/check_health_v1">>,
        <<"args">>          => #{<<"realm">> => <<"io.macula">>},
        <<"timeout_ms">>    => 5000,
        <<"trace">>         => [<<"hop1">>, <<"hop2">>]
    },
    {ok, Decoded} = macula_cbor_nif:unpack(macula_cbor_nif:pack(Payload)),
    ?assertEqual(Payload, Decoded).

%%====================================================================
%% Documented lossiness
%%====================================================================

atom_keys_become_binaries_on_decode_test() ->
    Original = #{version => 1},
    {ok, Decoded} = macula_cbor_nif:unpack(macula_cbor_nif:pack(Original)),
    ?assertEqual(#{<<"version">> => 1}, Decoded).

tuple_becomes_list_on_decode_test() ->
    {ok, [1, 2, 3]} = macula_cbor_nif:unpack(macula_cbor_nif:pack({1, 2, 3})).

%%====================================================================
%% Error paths
%%====================================================================

unpack_rejects_garbage_test() ->
    {error, _} = macula_cbor_nif:unpack(<<255, 254, 253>>).

unpack_rejects_truncated_test() ->
    Full = macula_cbor_nif:pack(#{<<"a">> => <<"verylongvalue">>}),
    Truncated = binary:part(Full, 0, byte_size(Full) div 2),
    {error, _} = macula_cbor_nif:unpack(Truncated).

unpack_rejects_empty_test() ->
    {error, _} = macula_cbor_nif:unpack(<<>>).

%%====================================================================
%% Encoding self-check (RFC 8949 fixed prefixes)
%%====================================================================

encodes_zero_as_single_byte_test() ->
    ?assertEqual(<<0>>, macula_cbor_nif:pack(0)).

encodes_empty_array_correctly_test() ->
    ?assertEqual(<<16#80>>, macula_cbor_nif:pack([])).

encodes_empty_map_correctly_test() ->
    ?assertEqual(<<16#a0>>, macula_cbor_nif:pack(#{})).

encodes_bool_correctly_test() ->
    ?assertEqual(<<16#f5>>, macula_cbor_nif:pack(true)),
    ?assertEqual(<<16#f4>>, macula_cbor_nif:pack(false)),
    ?assertEqual(<<16#f6>>, macula_cbor_nif:pack(nil)).
