%% @doc `adv_opts/1' forwards each opt `procedure_advertisement/4' reads
%% independently. Regression coverage for a bug where a single-clause
%% match on `cert_chain' alone meant a caller passing `ttl_ms' with no
%% `cert_chain' got `#{}' back, silently dropping `ttl_ms' too.
-module(macula_direct_dial_adv_opts_tests).
-include_lib("eunit/include/eunit.hrl").

ttl_ms_alone_is_forwarded_test() ->
    ?assertEqual(#{ttl_ms => 120_000},
                 macula_direct_dial:adv_opts(#{ttl_ms => 120_000})).

cert_chain_alone_is_forwarded_test() ->
    ?assertEqual(#{cert_chain => <<"pem">>},
                 macula_direct_dial:adv_opts(#{cert_chain => <<"pem">>})).

both_are_forwarded_together_test() ->
    ?assertEqual(#{cert_chain => <<"pem">>, ttl_ms => 120_000},
                 macula_direct_dial:adv_opts(#{cert_chain => <<"pem">>,
                                              ttl_ms => 120_000})).

unrecognized_opts_are_dropped_test() ->
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{reuse_sup => self()})).

non_binary_cert_chain_is_dropped_test() ->
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{cert_chain => not_a_binary})).

non_positive_ttl_ms_is_dropped_test() ->
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{ttl_ms => 0})),
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{ttl_ms => -1})).

empty_opts_test() ->
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{})).
