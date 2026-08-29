-module(macula_topic_pattern_tests).
-include_lib("eunit/include/eunit.hrl").

all_concrete_exact_match_test() ->
    ?assert(macula_topic_pattern:matches([<<"acme">>, <<"svc.do">>],
                                        [<<"acme">>, <<"svc.do">>])).

all_concrete_mismatch_test() ->
    ?assertNot(macula_topic_pattern:matches([<<"acme">>, <<"svc.do">>],
                                           [<<"contoso">>, <<"svc.do">>])).

single_wildcard_trailing_test() ->
    ?assert(macula_topic_pattern:matches([<<"acme">>, <<"*">>],
                                        [<<"acme">>, <<"svc.do">>])),
    ?assertNot(macula_topic_pattern:matches([<<"acme">>, <<"*">>],
                                           [<<"contoso">>, <<"svc.do">>])).

single_wildcard_leading_test() ->
    ?assert(macula_topic_pattern:matches([<<"*">>, <<"svc.do">>],
                                        [<<"acme">>, <<"svc.do">>])),
    ?assert(macula_topic_pattern:matches([<<"*">>, <<"svc.do">>],
                                        [<<"contoso">>, <<"svc.do">>])),
    ?assertNot(macula_topic_pattern:matches([<<"*">>, <<"svc.do">>],
                                           [<<"acme">>, <<"other">>])).

all_wildcards_matches_anything_of_same_arity_test() ->
    ?assert(macula_topic_pattern:matches([<<"*">>, <<"*">>],
                                        [<<"acme">>, <<"svc.do">>])),
    ?assert(macula_topic_pattern:matches([<<"*">>, <<"*">>],
                                        [<<"contoso">>, <<"other">>])).

multi_segment_multi_wildcard_test() ->
    Pattern = [<<"*">>, <<"weatherapp">>, <<"forecast">>, <<"*">>],
    ?assert(macula_topic_pattern:matches(
              Pattern, [<<"acme">>, <<"weatherapp">>, <<"forecast">>, <<"get_v1">>])),
    ?assert(macula_topic_pattern:matches(
              Pattern, [<<"contoso">>, <<"weatherapp">>, <<"forecast">>, <<"get_v2">>])),
    ?assertNot(macula_topic_pattern:matches(
                 Pattern, [<<"acme">>, <<"otherapp">>, <<"forecast">>, <<"get_v1">>])).

arity_mismatch_is_never_a_match_test() ->
    ?assertNot(macula_topic_pattern:matches([<<"acme">>], [<<"acme">>, <<"svc.do">>])),
    ?assertNot(macula_topic_pattern:matches([<<"acme">>, <<"svc.do">>], [<<"acme">>])),
    ?assertNot(macula_topic_pattern:matches([], [<<"acme">>])).

empty_pattern_matches_empty_concrete_test() ->
    ?assert(macula_topic_pattern:matches([], [])).

%% A concrete address with more than one dynamic segment (macula_topic's
%% shape) works identically to hecate_om_capabilities' narrower 2-segment
%% one -- the whole point of this module being arity-agnostic.
four_segment_shape_test() ->
    ?assert(macula_topic_pattern:matches(
              [<<"acme">>, <<"*">>, <<"forecast">>, <<"get_v1">>],
              [<<"acme">>, <<"weatherapp">>, <<"forecast">>, <<"get_v1">>])).
