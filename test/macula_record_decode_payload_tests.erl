%% EUnit tests for macula_record:decode_payload/1 — the deep
%% normalization of a pubsub/call payload that arrived in wire form
%% (macula_frame:to_wire/1: atom keys become {text, K}, atom values
%% become {text, V}, undefined becomes null). Consumers used to hand-roll
%% this, badly: mcl-sec-guard pattern-matched atom keys on the delivered
%% wire form and silently discarded every fact (mcl-sec-guard#2).
-module(macula_record_decode_payload_tests).

-include_lib("eunit/include/eunit.hrl").

text_keys_become_binary_keys_recursively_test() ->
    Wire = #{{text, <<"procedure">>} => <<"mcl-echo/echo">>,
             {text, <<"denied_rate">>} => 11,
             {text, <<"top_callers">>} => [#{{text, <<"caller">>} => <<"00ff">>}]},
    ?assertEqual(#{<<"procedure">> => <<"mcl-echo/echo">>,
                   <<"denied_rate">> => 11,
                   <<"top_callers">> => [#{<<"caller">> => <<"00ff">>}]},
                 macula_record:decode_payload(Wire)).

binary_keys_and_values_pass_through_test() ->
    Already = #{<<"procedure">> => <<"mcl-echo/echo">>, <<"count">> => 3},
    ?assertEqual(Already, macula_record:decode_payload(Already)).

text_values_unwrap_and_null_becomes_undefined_test() ->
    Wire = #{{text, <<"auth">>} => {text, <<"open">>},
             {text, <<"missing">>} => null},
    ?assertEqual(#{<<"auth">> => <<"open">>, <<"missing">> => undefined},
                 macula_record:decode_payload(Wire)).

non_map_values_pass_through_test() ->
    ?assertEqual([1, <<"a">>, 2.5], macula_record:decode_payload([1, <<"a">>, 2.5])),
    ?assertEqual(<<"raw">>, macula_record:decode_payload(<<"raw">>)),
    ?assertEqual(42, macula_record:decode_payload(42)).
