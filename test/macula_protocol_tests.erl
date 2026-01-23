%% @doc Unit tests for macula_protocol behaviour module.
%%
%% Tests cover:
%% - Identity validation (format, segments, characters)
%% - Capability validation
%% - API spec validation
%% - Utility functions
-module(macula_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Identity Validation Tests
%%====================================================================

validate_identity_valid_three_segments_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_identity(<<"io.macula.myapp">>)).

validate_identity_valid_four_segments_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_identity(<<"io.macula.rgfaber.myapp">>)).

validate_identity_valid_five_segments_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_identity(<<"io.macula.rgfaber.apps.weather">>)).

validate_identity_with_hyphens_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_identity(<<"io.macula.my-org.my-app">>)).

validate_identity_with_numbers_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_identity(<<"io.macula.org123.app456">>)).

validate_identity_too_few_segments_test() ->
    ?assertEqual({error, insufficient_segments},
                 macula_protocol:validate_identity(<<"io.macula">>)).

validate_identity_one_segment_test() ->
    ?assertEqual({error, insufficient_segments},
                 macula_protocol:validate_identity(<<"myapp">>)).

validate_identity_empty_test() ->
    ?assertEqual({error, invalid_identity},
                 macula_protocol:validate_identity(<<>>)).

validate_identity_not_binary_test() ->
    ?assertEqual({error, invalid_identity},
                 macula_protocol:validate_identity("io.macula.myapp")).

validate_identity_empty_segment_test() ->
    ?assertEqual({error, invalid_segment},
                 macula_protocol:validate_identity(<<"io..myapp">>)).

validate_identity_invalid_chars_test() ->
    ?assertEqual({error, invalid_segment},
                 macula_protocol:validate_identity(<<"io.macula.my_app">>)).

validate_identity_space_test() ->
    ?assertEqual({error, invalid_segment},
                 macula_protocol:validate_identity(<<"io.macula.my app">>)).

%%====================================================================
%% Capability Validation Tests
%%====================================================================

is_valid_capability_publish_test() ->
    ?assertEqual(true, macula_protocol:is_valid_capability(publish)).

is_valid_capability_subscribe_test() ->
    ?assertEqual(true, macula_protocol:is_valid_capability(subscribe)).

is_valid_capability_call_test() ->
    ?assertEqual(true, macula_protocol:is_valid_capability(call)).

is_valid_capability_register_test() ->
    ?assertEqual(true, macula_protocol:is_valid_capability(register)).

is_valid_capability_provide_content_test() ->
    ?assertEqual(true, macula_protocol:is_valid_capability(provide_content)).

is_valid_capability_consume_content_test() ->
    ?assertEqual(true, macula_protocol:is_valid_capability(consume_content)).

is_valid_capability_invalid_atom_test() ->
    ?assertEqual(false, macula_protocol:is_valid_capability(invalid)).

is_valid_capability_binary_test() ->
    ?assertEqual(false, macula_protocol:is_valid_capability(<<"publish">>)).

validate_capabilities_empty_list_test() ->
    ?assertEqual(ok, macula_protocol:validate_capabilities([])).

validate_capabilities_single_test() ->
    ?assertEqual(ok, macula_protocol:validate_capabilities([publish])).

validate_capabilities_multiple_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_capabilities([publish, subscribe, call])).

validate_capabilities_all_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_capabilities([
                     publish, subscribe, call, register,
                     provide_content, consume_content
                 ])).

validate_capabilities_invalid_test() ->
    ?assertEqual({error, invalid_capability},
                 macula_protocol:validate_capabilities([publish, invalid])).

validate_capabilities_not_list_test() ->
    ?assertEqual({error, invalid_capabilities_format},
                 macula_protocol:validate_capabilities(publish)).

%%====================================================================
%% API Spec Validation Tests
%%====================================================================

validate_api_spec_empty_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_api_spec(#{
                     topics => [],
                     procedures => []
                 })).

validate_api_spec_with_topics_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_api_spec(#{
                     topics => [<<"weather.updates">>, <<"weather.alerts">>],
                     procedures => []
                 })).

validate_api_spec_with_procedures_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_api_spec(#{
                     topics => [],
                     procedures => [<<"weather.getForecast">>, <<"weather.getHistory">>]
                 })).

validate_api_spec_with_content_types_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_api_spec(#{
                     topics => [],
                     procedures => [],
                     content_types => [<<"application/wasm">>, <<"application/octet-stream">>]
                 })).

validate_api_spec_full_test() ->
    ?assertEqual(ok,
                 macula_protocol:validate_api_spec(#{
                     topics => [<<"events.created">>],
                     procedures => [<<"doSomething">>],
                     content_types => [<<"text/plain">>]
                 })).

validate_api_spec_missing_fields_uses_defaults_test() ->
    %% Empty map should work - fields default to empty lists
    ?assertEqual(ok,
                 macula_protocol:validate_api_spec(#{})).

validate_api_spec_topics_not_binary_test() ->
    ?assertEqual({error, invalid_api_entries},
                 macula_protocol:validate_api_spec(#{
                     topics => ["not_binary"],
                     procedures => []
                 })).

validate_api_spec_procedures_not_binary_test() ->
    ?assertEqual({error, invalid_api_entries},
                 macula_protocol:validate_api_spec(#{
                     topics => [],
                     procedures => [not_binary]
                 })).

validate_api_spec_not_map_test() ->
    ?assertEqual({error, invalid_api_spec_format},
                 macula_protocol:validate_api_spec([topics, procedures])).
