%% @doc `adv_opts/1' forwards each opt `procedure_advertisement/5' reads, independently: the provider authorization and
%% ttl_ms. Regression coverage for a bug where a single-clause match on one opt meant a caller passing `ttl_ms' alone
%% got `#{}' back, silently dropping `ttl_ms' too.
-module(macula_direct_dial_adv_opts_tests).
-include_lib("eunit/include/eunit.hrl").

-define(AUTHORIZATION, #{certificate_chain => [<<"der">>]}).

ttl_ms_alone_is_forwarded_test() ->
    ?assertEqual(#{ttl_ms => 120_000},
                 macula_direct_dial:adv_opts(#{ttl_ms => 120_000})).

authorization_alone_is_forwarded_test() ->
    ?assertEqual(#{authorization => ?AUTHORIZATION},
                 macula_direct_dial:adv_opts(#{authorization => ?AUTHORIZATION})).

both_are_forwarded_together_test() ->
    ?assertEqual(#{authorization => ?AUTHORIZATION, ttl_ms => 120_000},
                 macula_direct_dial:adv_opts(#{authorization => ?AUTHORIZATION, ttl_ms => 120_000})).

unrecognized_opts_are_dropped_test() ->
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{reuse_sup => self(), cert_chain => <<"pem">>})).

an_authorization_that_is_not_a_map_is_dropped_test() ->
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{authorization => <<"pem">>})).

non_positive_ttl_ms_is_dropped_test() ->
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{ttl_ms => 0})),
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{ttl_ms => -1})).

empty_opts_test() ->
    ?assertEqual(#{}, macula_direct_dial:adv_opts(#{})).
