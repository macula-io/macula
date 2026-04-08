%%%-------------------------------------------------------------------
%%% @doc Tests for message traceroute — opt-in per-message hop tracking.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_traceroute_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Protocol round-trip: _trace survives encode/decode
%%====================================================================

publish_trace_roundtrip_test() ->
    Msg = #{
        topic => <<"test.topic">>,
        payload => <<"hello">>,
        qos => 0,
        retain => false,
        message_id => crypto:strong_rand_bytes(16),
        <<"_trace">> => []
    },
    Binary = macula_protocol_encoder:encode(publish, Msg),
    {ok, {publish, Decoded}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual([], maps:get(<<"_trace">>, Decoded)).

publish_trace_with_hops_roundtrip_test() ->
    Hops = [
        #{<<"relay">> => <<"relay-de-nuremberg.macula.io">>,
          <<"ts">> => 1712570000123,
          <<"dir">> => <<"fwd">>},
        #{<<"relay">> => <<"relay-fi-helsinki.macula.io">>,
          <<"ts">> => 1712570000456,
          <<"dir">> => <<"fwd">>}
    ],
    Msg = #{
        topic => <<"weather.de.berlin">>,
        payload => <<"{}">>,
        qos => 0,
        retain => false,
        message_id => crypto:strong_rand_bytes(16),
        <<"_trace">> => Hops
    },
    Binary = macula_protocol_encoder:encode(publish, Msg),
    {ok, {publish, Decoded}} = macula_protocol_decoder:decode(Binary),
    DecodedTrace = maps:get(<<"_trace">>, Decoded),
    ?assertEqual(2, length(DecodedTrace)),
    [Hop1, Hop2] = DecodedTrace,
    ?assertEqual(<<"relay-de-nuremberg.macula.io">>, maps:get(<<"relay">>, Hop1)),
    ?assertEqual(<<"fwd">>, maps:get(<<"dir">>, Hop1)),
    ?assertEqual(<<"relay-fi-helsinki.macula.io">>, maps:get(<<"relay">>, Hop2)).

publish_no_trace_roundtrip_test() ->
    Msg = #{
        topic => <<"test.topic">>,
        payload => <<"hello">>,
        qos => 0,
        retain => false,
        message_id => crypto:strong_rand_bytes(16)
    },
    Binary = macula_protocol_encoder:encode(publish, Msg),
    {ok, {publish, Decoded}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual(undefined, maps:get(<<"_trace">>, Decoded, undefined)).

call_trace_roundtrip_test() ->
    Msg = #{
        procedure => <<"my.procedure">>,
        args => <<"{}">>,
        call_id => crypto:strong_rand_bytes(16),
        <<"_trace">> => []
    },
    Binary = macula_protocol_encoder:encode(call, Msg),
    {ok, {call, Decoded}} = macula_protocol_decoder:decode(Binary),
    ?assertEqual([], maps:get(<<"_trace">>, Decoded)).

reply_trace_roundtrip_test() ->
    Hops = [
        #{<<"relay">> => <<"relay-de-nuremberg.macula.io">>,
          <<"ts">> => 1712570000123,
          <<"dir">> => <<"fwd">>},
        #{<<"relay">> => <<"relay-de-nuremberg.macula.io">>,
          <<"ts">> => 1712570000789,
          <<"dir">> => <<"ret">>}
    ],
    Msg = #{
        call_id => crypto:strong_rand_bytes(16),
        result => <<"ok">>,
        <<"_trace">> => Hops
    },
    Binary = macula_protocol_encoder:encode(reply, Msg),
    {ok, {reply, Decoded}} = macula_protocol_decoder:decode(Binary),
    DecodedTrace = maps:get(<<"_trace">>, Decoded),
    ?assertEqual(2, length(DecodedTrace)),
    [Fwd, Ret] = DecodedTrace,
    ?assertEqual(<<"fwd">>, maps:get(<<"dir">>, Fwd)),
    ?assertEqual(<<"ret">>, maps:get(<<"dir">>, Ret)).

%%====================================================================
%% Relay handler helpers
%%====================================================================

maybe_append_hop_undefined_test() ->
    ?assertEqual(undefined, macula_relay_handler:maybe_append_hop(undefined, <<"fwd">>)).

maybe_append_hop_appends_to_empty_test() ->
    Result = macula_relay_handler:maybe_append_hop([], <<"fwd">>),
    ?assertEqual(1, length(Result)),
    [Hop] = Result,
    ?assertEqual(<<"fwd">>, maps:get(<<"dir">>, Hop)),
    ?assert(is_binary(maps:get(<<"relay">>, Hop))),
    ?assert(is_integer(maps:get(<<"ts">>, Hop))).

maybe_append_hop_appends_to_existing_test() ->
    Existing = [#{<<"relay">> => <<"old">>, <<"ts">> => 100, <<"dir">> => <<"fwd">>}],
    Result = macula_relay_handler:maybe_append_hop(Existing, <<"ret">>),
    ?assertEqual(2, length(Result)),
    [First, Second] = Result,
    ?assertEqual(<<"old">>, maps:get(<<"relay">>, First)),
    ?assertEqual(<<"ret">>, maps:get(<<"dir">>, Second)).

extract_trace_present_test() ->
    Msg = #{<<"_trace">> => [#{<<"relay">> => <<"r1">>}]},
    ?assertEqual([#{<<"relay">> => <<"r1">>}], macula_relay_handler:extract_trace(Msg)).

extract_trace_absent_test() ->
    ?assertEqual(undefined, macula_relay_handler:extract_trace(#{})).

maybe_add_trace_undefined_test() ->
    Msg = #{<<"topic">> => <<"t">>},
    ?assertEqual(Msg, macula_relay_handler:maybe_add_trace(Msg, undefined)).

maybe_add_trace_with_trace_test() ->
    Msg = #{<<"topic">> => <<"t">>},
    Trace = [#{<<"relay">> => <<"r1">>}],
    Result = macula_relay_handler:maybe_add_trace(Msg, Trace),
    ?assertEqual(Trace, maps:get(<<"_trace">>, Result)),
    ?assertEqual(<<"t">>, maps:get(<<"topic">>, Result)).

