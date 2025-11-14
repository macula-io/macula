%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for macula_utils module.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Descriptions
%%%===================================================================

%% This module tests all utility functions in macula_utils.erl:
%%   - ensure_binary/1 - Type conversion
%%   - generate_node_id/0 - Random ID generation
%%   - encode_json/1 - JSON encoding
%%   - decode_json/1 - JSON decoding
%%   - next_message_id/1 - Message ID generation
%%   - parse_url/1 - URL parsing
%%   - topic_matches/5 - Topic pattern matching
%%   - normalize_provider/1 - Provider normalization

%%%===================================================================
%%% Type Conversion Tests
%%%===================================================================

ensure_binary_with_binary_test() ->
    Input = <<"already_binary">>,
    ?assertEqual(Input, macula_utils:ensure_binary(Input)).

ensure_binary_with_list_test() ->
    Input = "hello",
    Expected = <<"hello">>,
    ?assertEqual(Expected, macula_utils:ensure_binary(Input)).

ensure_binary_with_atom_test() ->
    Input = hello,
    Expected = <<"hello">>,
    ?assertEqual(Expected, macula_utils:ensure_binary(Input)).

ensure_binary_empty_string_test() ->
    ?assertEqual(<<>>, macula_utils:ensure_binary("")).

ensure_binary_empty_binary_test() ->
    ?assertEqual(<<>>, macula_utils:ensure_binary(<<>>)).

%%%===================================================================
%%% ID Generation Tests
%%%===================================================================

generate_node_id_returns_binary_test() ->
    NodeId = macula_utils:generate_node_id(),
    ?assert(is_binary(NodeId)).

generate_node_id_correct_length_test() ->
    NodeId = macula_utils:generate_node_id(),
    ?assertEqual(32, byte_size(NodeId)).

generate_node_id_unique_test() ->
    %% Generate 10 IDs and ensure they're all unique
    Ids = [macula_utils:generate_node_id() || _ <- lists:seq(1, 10)],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(length(Ids), length(UniqueIds)).

%%%===================================================================
%%% JSON Encoding/Decoding Tests
%%%===================================================================

encode_decode_map_test() ->
    Input = #{<<"key">> => <<"value">>, <<"number">> => 42},
    Encoded = macula_utils:encode_json(Input),
    Decoded = macula_utils:decode_json(Encoded),
    ?assertEqual(Input, Decoded).

encode_decode_list_test() ->
    Input = [1, 2, 3, <<"hello">>],
    Encoded = macula_utils:encode_json(Input),
    Decoded = macula_utils:decode_json(Encoded),
    ?assertEqual(Input, Decoded).

encode_empty_map_test() ->
    Input = #{},
    Encoded = macula_utils:encode_json(Input),
    ?assert(is_binary(Encoded)).

encode_nested_structure_test() ->
    Input = #{
        <<"user">> => #{
            <<"name">> => <<"Alice">>,
            <<"age">> => 30
        }},
    Encoded = macula_utils:encode_json(Input),
    Decoded = macula_utils:decode_json(Encoded),
    ?assertEqual(Input, Decoded).

%%%===================================================================
%%% Message ID Tests
%%%===================================================================

next_message_id_returns_tuple_test() ->
    {MsgId, NewCounter} = macula_utils:next_message_id(0),
    ?assert(is_binary(MsgId)),
    ?assert(is_integer(NewCounter)),
    ?assertEqual(1, NewCounter).

next_message_id_increments_test() ->
    {_MsgId1, Counter1} = macula_utils:next_message_id(0),
    {_MsgId2, Counter2} = macula_utils:next_message_id(Counter1),
    {_MsgId3, Counter3} = macula_utils:next_message_id(Counter2),
    ?assertEqual(1, Counter1),
    ?assertEqual(2, Counter2),
    ?assertEqual(3, Counter3).

next_message_id_unique_test() ->
    %% Generate 5 sequential message IDs and ensure they're unique
    {MsgId1, C1} = macula_utils:next_message_id(0),
    {MsgId2, C2} = macula_utils:next_message_id(C1),
    {MsgId3, C3} = macula_utils:next_message_id(C2),
    {MsgId4, C4} = macula_utils:next_message_id(C3),
    {MsgId5, _C5} = macula_utils:next_message_id(C4),

    Ids = [MsgId1, MsgId2, MsgId3, MsgId4, MsgId5],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(5, length(UniqueIds)).

%%%===================================================================
%%% URL Parsing Tests
%%%===================================================================

parse_url_https_with_port_test() ->
    Url = <<"https://example.com:9443">>,
    {Host, Port} = macula_utils:parse_url(Url),
    ?assertEqual("example.com", Host),
    ?assertEqual(9443, Port).

parse_url_https_default_port_test() ->
    Url = <<"https://example.com">>,
    {Host, Port} = macula_utils:parse_url(Url),
    ?assertEqual("example.com", Host),
    ?assertEqual(443, Port).

parse_url_http_with_port_test() ->
    Url = <<"http://localhost:8080">>,
    {Host, Port} = macula_utils:parse_url(Url),
    ?assertEqual("localhost", Host),
    ?assertEqual(8080, Port).

parse_url_http_default_port_test() ->
    Url = <<"http://example.com">>,
    {Host, Port} = macula_utils:parse_url(Url),
    ?assertEqual("example.com", Host),
    ?assertEqual(80, Port).

parse_url_no_protocol_test() ->
    Url = <<"example.com:9443">>,
    {Host, Port} = macula_utils:parse_url(Url),
    ?assertEqual("example.com", Host),
    ?assertEqual(9443, Port).

parse_url_no_protocol_no_port_test() ->
    Url = <<"example.com">>,
    {Host, Port} = macula_utils:parse_url(Url),
    ?assertEqual("example.com", Host),
    ?assertEqual(443, Port).  % Default for HTTPS

%%%===================================================================
%%% Topic Matching Tests
%%%===================================================================

topic_matches_exact_test() ->
    Pattern = <<"energy.home.measured">>,
    Topic = <<"energy.home.measured">>,
    ?assert(macula_utils:topic_matches(Pattern, Topic, <<".">>, <<"*">>, <<"**">>)).

topic_matches_exact_mismatch_test() ->
    Pattern = <<"energy.home.measured">>,
    Topic = <<"energy.building.measured">>,
    ?assertNot(macula_utils:topic_matches(Pattern, Topic, <<".">>, <<"*">>, <<"**">>)).

topic_matches_single_wildcard_test() ->
    Pattern = <<"energy.*.measured">>,
    Topic = <<"energy.home.measured">>,
    ?assert(macula_utils:topic_matches(Pattern, Topic, <<".">>, <<"*">>, <<"**">>)).

topic_matches_single_wildcard_multiple_segments_test() ->
    Pattern = <<"energy.*.measured">>,
    Topic1 = <<"energy.home.measured">>,
    Topic2 = <<"energy.building.measured">>,
    Topic3 = <<"energy.vehicle.measured">>,
    ?assert(macula_utils:topic_matches(Pattern, Topic1, <<".">>, <<"*">>, <<"**">>)),
    ?assert(macula_utils:topic_matches(Pattern, Topic2, <<".">>, <<"*">>, <<"**">>)),
    ?assert(macula_utils:topic_matches(Pattern, Topic3, <<".">>, <<"*">>, <<"**">>)).

topic_matches_single_wildcard_mismatch_test() ->
    Pattern = <<"energy.*.measured">>,
    Topic = <<"energy.home.device.measured">>,  % Too many segments
    ?assertNot(macula_utils:topic_matches(Pattern, Topic, <<".">>, <<"*">>, <<"**">>)).

topic_matches_multi_wildcard_test() ->
    Pattern = <<"energy.**">>,
    Topic1 = <<"energy.home">>,
    Topic2 = <<"energy.home.measured">>,
    Topic3 = <<"energy.home.device.power">>,
    ?assert(macula_utils:topic_matches(Pattern, Topic1, <<".">>, <<"*">>, <<"**">>)),
    ?assert(macula_utils:topic_matches(Pattern, Topic2, <<".">>, <<"*">>, <<"**">>)),
    ?assert(macula_utils:topic_matches(Pattern, Topic3, <<".">>, <<"*">>, <<"**">>)).

topic_matches_multi_wildcard_middle_test() ->
    Pattern = <<"energy.**.measured">>,
    Topic1 = <<"energy.home.measured">>,
    Topic2 = <<"energy.home.device.measured">>,
    Topic3 = <<"energy.building.floor1.room5.measured">>,
    ?assert(macula_utils:topic_matches(Pattern, Topic1, <<".">>, <<"*">>, <<"**">>)),
    ?assert(macula_utils:topic_matches(Pattern, Topic2, <<".">>, <<"*">>, <<"**">>)),
    ?assert(macula_utils:topic_matches(Pattern, Topic3, <<".">>, <<"*">>, <<"**">>)).

topic_matches_multi_wildcard_mismatch_test() ->
    Pattern = <<"energy.**">>,
    Topic = <<"building.home.measured">>,  % Doesn't start with 'energy'
    ?assertNot(macula_utils:topic_matches(Pattern, Topic, <<".">>, <<"*">>, <<"**">>)).

topic_matches_multiple_wildcards_test() ->
    Pattern = <<"energy.*.**.measured">>,
    Topic1 = <<"energy.home.measured">>,
    Topic2 = <<"energy.home.device.power.measured">>,
    ?assert(macula_utils:topic_matches(Pattern, Topic1, <<".">>, <<"*">>, <<"**">>)),
    ?assert(macula_utils:topic_matches(Pattern, Topic2, <<".">>, <<"*">>, <<"**">>)).

topic_matches_custom_separator_test() ->
    Pattern = <<"energy/home/measured">>,
    Topic = <<"energy/home/measured">>,
    ?assert(macula_utils:topic_matches(Pattern, Topic, <<"/">>, <<"*">>, <<"**">>)).

topic_matches_custom_wildcards_test() ->
    Pattern = <<"energy.+.measured">>,
    Topic = <<"energy.home.measured">>,
    ?assert(macula_utils:topic_matches(Pattern, Topic, <<".">>, <<"+">>, <<"#">>)).

%%%===================================================================
%%% Provider Normalization Tests
%%%===================================================================

normalize_provider_all_fields_test() ->
    Provider = #{
        <<"node_id">> => <<"abc123">>,
        <<"endpoint">> => <<"https://example.com:9443">>,
        <<"metadata">> => #{<<"region">> => <<"us-east">>},
        <<"ttl">> => 600
    },
    Expected = #{
        node_id => <<"abc123">>,
        endpoint => <<"https://example.com:9443">>,
        metadata => #{<<"region">> => <<"us-east">>},
        ttl => 600
    },
    ?assertEqual(Expected, macula_utils:normalize_provider(Provider)).

normalize_provider_minimal_fields_test() ->
    Provider = #{
        <<"node_id">> => <<"xyz789">>,
        <<"endpoint">> => <<"https://localhost:9443">>
    },
    Normalized = macula_utils:normalize_provider(Provider),

    ?assertEqual(<<"xyz789">>, maps:get(node_id, Normalized)),
    ?assertEqual(<<"https://localhost:9443">>, maps:get(endpoint, Normalized)),
    ?assertEqual(#{}, maps:get(metadata, Normalized)),  % Default empty map
    ?assertEqual(300, maps:get(ttl, Normalized)).       % Default TTL

normalize_provider_with_metadata_no_ttl_test() ->
    Provider = #{
        <<"node_id">> => <<"node1">>,
        <<"endpoint">> => <<"https://node1.example.com:9443">>,
        <<"metadata">> => #{<<"version">> => <<"1.0">>}
    },
    Normalized = macula_utils:normalize_provider(Provider),

    ?assertEqual(<<"node1">>, maps:get(node_id, Normalized)),
    ?assertEqual(#{<<"version">> => <<"1.0">>}, maps:get(metadata, Normalized)),
    ?assertEqual(300, maps:get(ttl, Normalized)).  % Default TTL

%%%===================================================================
%%% Integration Tests
%%%===================================================================

roundtrip_ensure_binary_with_types_test() ->
    %% Test that we can convert different types and get consistent binaries
    Atom = hello,
    List = "hello",
    Binary = <<"hello">>,

    Result1 = macula_utils:ensure_binary(Atom),
    Result2 = macula_utils:ensure_binary(List),
    Result3 = macula_utils:ensure_binary(Binary),

    ?assertEqual(Result1, Result2),
    ?assertEqual(Result2, Result3),
    ?assertEqual(<<"hello">>, Result3).

json_encode_with_binary_conversion_test() ->
    %% Test that we can encode a map with atom keys after conversion
    Input = #{<<"name">> => macula_utils:ensure_binary(alice),
              <<"age">> => 30},
    Encoded = macula_utils:encode_json(Input),
    Decoded = macula_utils:decode_json(Encoded),
    ?assertEqual(Input, Decoded).

url_parsing_with_binary_input_test() ->
    %% Test that parse_url works with binaries (common use case)
    Urls = [
        {<<"https://node1.example.com:9443">>, {"node1.example.com", 9443}},
        {<<"http://localhost:8080">>, {"localhost", 8080}},
        {<<"https://10.0.0.1:443">>, {"10.0.0.1", 443}}
    ],

    lists:foreach(fun({Url, Expected}) ->
        ?assertEqual(Expected, macula_utils:parse_url(Url))
    end, Urls).
