%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_names module.
%%% Tests name validation, normalization, and segment extraction.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_names_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% validate/1 Tests - Valid Names (No Wildcards)
%%%===================================================================

validate_simple_name_test() ->
    ?assertEqual(ok, macula_names:validate(<<"org.example.service">>)).

validate_single_segment_test() ->
    ?assertEqual(ok, macula_names:validate(<<"service">>)).

validate_two_segments_test() ->
    ?assertEqual(ok, macula_names:validate(<<"example.service">>)).

validate_many_segments_test() ->
    ?assertEqual(ok, macula_names:validate(<<"org.example.domain.service.procedure">>)).

validate_with_hyphens_test() ->
    ?assertEqual(ok, macula_names:validate(<<"org.example-service.method">>)).

validate_with_underscores_test() ->
    ?assertEqual(ok, macula_names:validate(<<"org.example_service.method">>)).

validate_with_numbers_test() ->
    ?assertEqual(ok, macula_names:validate(<<"org.service123.method456">>)).

validate_uppercase_test() ->
    ?assertEqual(ok, macula_names:validate(<<"org.Example.Service">>)).

validate_mixed_case_test() ->
    ?assertEqual(ok, macula_names:validate(<<"Org.EXAMPLE.service">>)).

validate_all_valid_chars_test() ->
    ?assertEqual(ok, macula_names:validate(<<"org.service-test_123.method">>)).

%%%===================================================================
%%% validate/1 Tests - Invalid Names (No Wildcards)
%%%===================================================================

validate_empty_name_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<>>)).

validate_leading_dot_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<".org.example.service">>)).

validate_trailing_dot_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org.example.service.">>)).

validate_double_dot_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org..example.service">>)).

validate_wildcard_star_rejected_test() ->
    %% By default, wildcards not allowed
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org.*.service">>)).

validate_wildcard_hash_rejected_test() ->
    %% By default, wildcards not allowed
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org.#">>)).

validate_space_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org.example service">>)).

validate_slash_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org/example/service">>)).

validate_at_sign_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org@example.service">>)).

validate_dollar_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org$example.service">>)).

validate_percent_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org%example.service">>)).

validate_ampersand_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org&example.service">>)).

validate_equals_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org=example.service">>)).

validate_plus_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org+example.service">>)).

validate_bracket_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org[example].service">>)).

validate_brace_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org{example}.service">>)).

validate_pipe_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org|example.service">>)).

validate_colon_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org:example.service">>)).

validate_semicolon_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org;example.service">>)).

validate_quote_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org\"example\".service">>)).

validate_apostrophe_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org'example'.service">>)).

validate_comma_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org,example,service">>)).

validate_question_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org?example.service">>)).

validate_exclamation_test() ->
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org!example.service">>)).

%%%===================================================================
%%% validate/2 Tests - With allow_wildcards Option
%%%===================================================================

validate_with_wildcards_star_test() ->
    Opts = #{allow_wildcards => true},
    ?assertEqual(ok, macula_names:validate(<<"org.*.service">>, Opts)).

validate_with_wildcards_hash_test() ->
    Opts = #{allow_wildcards => true},
    ?assertEqual(ok, macula_names:validate(<<"org.#">>, Opts)).

validate_with_wildcards_both_test() ->
    Opts = #{allow_wildcards => true},
    ?assertEqual(ok, macula_names:validate(<<"org.*.#">>, Opts)).

validate_with_wildcards_multiple_stars_test() ->
    Opts = #{allow_wildcards => true},
    ?assertEqual(ok, macula_names:validate(<<"*.*.service">>, Opts)).

validate_wildcards_disabled_test() ->
    Opts = #{allow_wildcards => false},
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org.*.service">>, Opts)).

validate_with_wildcards_still_rejects_invalid_test() ->
    Opts = #{allow_wildcards => true},
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org..service">>, Opts)),
    ?assertEqual({error, invalid_name}, macula_names:validate(<<".org.service">>, Opts)),
    ?assertEqual({error, invalid_name}, macula_names:validate(<<"org.service.">>, Opts)),
    ?assertEqual({error, invalid_name}, macula_names:validate(<<>>, Opts)).

validate_wildcard_in_segment_test() ->
    Opts = #{allow_wildcards => true},
    ?assertEqual(ok, macula_names:validate(<<"org.service*.method">>, Opts)).

validate_hash_in_segment_test() ->
    Opts = #{allow_wildcards => true},
    ?assertEqual(ok, macula_names:validate(<<"org.service#.method">>, Opts)).

%%%===================================================================
%%% normalize/1 Tests
%%%===================================================================

normalize_lowercase_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"Org.Example.Service">>)).

normalize_uppercase_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"ORG.EXAMPLE.SERVICE">>)).

normalize_trim_leading_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"  org.example.service">>)).

normalize_trim_trailing_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"org.example.service  ">>)).

normalize_trim_both_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"  org.example.service  ">>)).

normalize_trim_tabs_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"\torg.example.service\t">>)).

normalize_trim_newlines_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"\norg.example.service\n">>)).

normalize_remove_double_dots_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"org..example..service">>)).

normalize_remove_triple_dots_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"org...example...service">>)).

normalize_remove_many_dots_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"org......example.service">>)).

normalize_already_normalized_test() ->
    ?assertEqual(<<"org.example.service">>, macula_names:normalize(<<"org.example.service">>)).

normalize_empty_test() ->
    ?assertEqual(<<>>, macula_names:normalize(<<>>)).

normalize_whitespace_only_test() ->
    ?assertEqual(<<>>, macula_names:normalize(<<"   ">>)).

normalize_preserves_hyphens_test() ->
    ?assertEqual(<<"org.example-service.method">>, macula_names:normalize(<<"Org.Example-Service.Method">>)).

normalize_preserves_underscores_test() ->
    ?assertEqual(<<"org.example_service.method">>, macula_names:normalize(<<"Org.Example_Service.Method">>)).

normalize_preserves_numbers_test() ->
    ?assertEqual(<<"org.service123.method456">>, macula_names:normalize(<<"Org.Service123.Method456">>)).

normalize_complex_test() ->
    Input = <<"  Org..Example...Service  ">>,
    Expected = <<"org.example.service">>,
    ?assertEqual(Expected, macula_names:normalize(Input)).

%%%===================================================================
%%% namespace/1 Tests
%%%===================================================================

namespace_simple_test() ->
    ?assertEqual(<<"org">>, macula_names:namespace(<<"org.example.service">>)).

namespace_single_segment_test() ->
    ?assertEqual(<<"service">>, macula_names:namespace(<<"service">>)).

namespace_two_segments_test() ->
    ?assertEqual(<<"example">>, macula_names:namespace(<<"example.service">>)).

namespace_many_segments_test() ->
    ?assertEqual(<<"org">>, macula_names:namespace(<<"org.example.domain.service.method">>)).

namespace_empty_test() ->
    ?assertEqual(<<>>, macula_names:namespace(<<>>)).

namespace_with_numbers_test() ->
    ?assertEqual(<<"org123">>, macula_names:namespace(<<"org123.example.service">>)).

namespace_with_hyphens_test() ->
    ?assertEqual(<<"org-example">>, macula_names:namespace(<<"org-example.service.method">>)).

namespace_with_underscores_test() ->
    ?assertEqual(<<"org_example">>, macula_names:namespace(<<"org_example.service.method">>)).

namespace_wildcard_test() ->
    %% Namespace extraction doesn't validate, just splits
    ?assertEqual(<<"*">>, macula_names:namespace(<<"*.example.service">>)).

%%%===================================================================
%%% segment_count/1 Tests
%%%===================================================================

segment_count_empty_test() ->
    ?assertEqual(0, macula_names:segment_count(<<>>)).

segment_count_single_test() ->
    ?assertEqual(1, macula_names:segment_count(<<"service">>)).

segment_count_two_test() ->
    ?assertEqual(2, macula_names:segment_count(<<"example.service">>)).

segment_count_three_test() ->
    ?assertEqual(3, macula_names:segment_count(<<"org.example.service">>)).

segment_count_many_test() ->
    ?assertEqual(5, macula_names:segment_count(<<"org.example.domain.service.method">>)).

segment_count_with_double_dots_test() ->
    %% segment_count doesn't normalize, just counts
    %% "org..service" = 3 segments: ["org", "", "service"]
    ?assertEqual(3, macula_names:segment_count(<<"org..service">>)).

segment_count_leading_dot_test() ->
    %% ".org.service" = 3 segments: ["", "org", "service"]
    ?assertEqual(3, macula_names:segment_count(<<".org.service">>)).

segment_count_trailing_dot_test() ->
    %% "org.service." = 3 segments: ["org", "service", ""]
    ?assertEqual(3, macula_names:segment_count(<<"org.service.">>)).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

normalize_then_validate_test() ->
    Input = <<"  Org..Example..Service  ">>,
    Normalized = macula_names:normalize(Input),
    ?assertEqual(ok, macula_names:validate(Normalized)).

validate_then_namespace_test() ->
    Name = <<"org.example.service">>,
    ?assertEqual(ok, macula_names:validate(Name)),
    ?assertEqual(<<"org">>, macula_names:namespace(Name)).

full_workflow_test() ->
    %% User input (messy)
    UserInput = <<"  Org..Example..Service  ">>,

    %% Normalize
    Normalized = macula_names:normalize(UserInput),
    ?assertEqual(<<"org.example.service">>, Normalized),

    %% Validate
    ?assertEqual(ok, macula_names:validate(Normalized)),

    %% Extract namespace
    Namespace = macula_names:namespace(Normalized),
    ?assertEqual(<<"org">>, Namespace),

    %% Count segments
    Count = macula_names:segment_count(Normalized),
    ?assertEqual(3, Count).

validate_edge_cases_test() ->
    EdgeCases = [
        {<<"org.example.service">>, ok},
        {<<"a">>, ok},
        {<<"a.b">>, ok},
        {<<"a.b.c.d.e.f.g">>, ok},
        {<<"org123.service456.method789">>, ok},
        {<<"org-service.method-name.v1">>, ok},
        {<<"org_service.method_name.v1">>, ok},
        {<<>>, {error, invalid_name}},
        {<<".org">>, {error, invalid_name}},
        {<<"org.">>, {error, invalid_name}},
        {<<"org..service">>, {error, invalid_name}},
        {<<"org.service.">>, {error, invalid_name}},
        {<<".org.service">>, {error, invalid_name}},
        {<<"org.*.service">>, {error, invalid_name}},
        {<<"org.#">>, {error, invalid_name}}
    ],
    lists:foreach(fun({Name, Expected}) ->
        ?assertEqual(Expected, macula_names:validate(Name))
    end, EdgeCases).

wildcard_validation_test() ->
    Opts = #{allow_wildcards => true},
    WildcardCases = [
        {<<"org.*.service">>, ok},
        {<<"org.#">>, ok},
        {<<"*">>, ok},
        {<<"#">>, ok},
        {<<"*.*.service">>, ok},
        {<<"org.service*">>, ok},
        {<<"org.service#">>, ok},
        {<<>>, {error, invalid_name}},
        {<<".org.*">>, {error, invalid_name}},
        {<<"org.*.">>, {error, invalid_name}},
        {<<"org..#">>, {error, invalid_name}}
    ],
    lists:foreach(fun({Name, Expected}) ->
        ?assertEqual(Expected, macula_names:validate(Name, Opts))
    end, WildcardCases).

normalize_preserves_wildcards_test() ->
    %% Normalize should preserve wildcards
    ?assertEqual(<<"org.*.service">>, macula_names:normalize(<<"Org.*.Service">>)),
    ?assertEqual(<<"org.#">>, macula_names:normalize(<<"Org.#">>)),
    ?assertEqual(<<"*.service.#">>, macula_names:normalize(<<"*.Service.#">>)).

segment_count_various_lengths_test() ->
    Tests = [
        {<<"a">>, 1},
        {<<"a.b">>, 2},
        {<<"a.b.c">>, 3},
        {<<"a.b.c.d">>, 4},
        {<<"a.b.c.d.e">>, 5},
        {<<"a.b.c.d.e.f">>, 6},
        {<<>>, 0}
    ],
    lists:foreach(fun({Name, Expected}) ->
        ?assertEqual(Expected, macula_names:segment_count(Name))
    end, Tests).

namespace_extraction_test() ->
    Tests = [
        {<<"org.example.service">>, <<"org">>},
        {<<"com.example.service">>, <<"com">>},
        {<<"net.test.prod.service">>, <<"net">>},
        {<<"single">>, <<"single">>},
        {<<>>, <<>>}
    ],
    lists:foreach(fun({Name, Expected}) ->
        ?assertEqual(Expected, macula_names:namespace(Name))
    end, Tests).

normalize_double_dots_recursively_test() ->
    %% Test that multiple double dots are removed recursively
    Input = <<"org....example....service">>,  % 4 dots become 1
    Expected = <<"org.example.service">>,
    ?assertEqual(Expected, macula_names:normalize(Input)).

validate_segment_boundaries_test() ->
    %% Test segments with various valid character combinations
    ValidNames = [
        <<"a1">>,
        <<"a_b">>,
        <<"a-b">>,
        <<"a1_b2-c3">>,
        <<"ABC">>,
        <<"abc123_def-456">>
    ],
    lists:foreach(fun(Name) ->
        ?assertEqual(ok, macula_names:validate(Name))
    end, ValidNames).

validate_all_uppercase_test() ->
    ?assertEqual(ok, macula_names:validate(<<"ORG.EXAMPLE.SERVICE">>)).

validate_all_lowercase_test() ->
    ?assertEqual(ok, macula_names:validate(<<"org.example.service">>)).

validate_all_numbers_test() ->
    ?assertEqual(ok, macula_names:validate(<<"123.456.789">>)).

validate_mixed_alphanum_test() ->
    ?assertEqual(ok, macula_names:validate(<<"org1.example2.service3">>)).

empty_segment_after_normalization_test() ->
    %% After normalization, double dots are removed, so validation should pass
    Input = <<"org..example">>,
    Normalized = macula_names:normalize(Input),
    ?assertEqual(<<"org.example">>, Normalized),
    ?assertEqual(ok, macula_names:validate(Normalized)).

wildcard_and_regular_chars_test() ->
    Opts = #{allow_wildcards => true},
    ?assertEqual(ok, macula_names:validate(<<"org.service*123">>, Opts)),
    ?assertEqual(ok, macula_names:validate(<<"org.service#abc">>, Opts)).
