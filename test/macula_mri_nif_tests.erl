%% @doc Tests for macula_mri_nif module.
%%
%% These tests verify both NIF and Erlang fallback implementations.
-module(macula_mri_nif_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generator
%%====================================================================

mri_nif_test_() ->
    [
     {"Parse MRI tests", fun parse_mri_tests/0},
     {"Validate realm tests", fun validate_realm_tests/0},
     {"Validate segment tests", fun validate_segment_tests/0},
     {"Builtin type tests", fun builtin_type_tests/0},
     {"Join path tests", fun join_path_tests/0},
     {"Format MRI tests", fun format_mri_tests/0},
     {"Find children tests", fun find_children_tests/0},
     {"Find descendants tests", fun find_descendants_tests/0},
     {"Trie index tests", fun trie_index_tests/0},
     {"Trie index dynamic tests", fun trie_index_dynamic_tests/0},
     {"NIF availability test", fun nif_availability_test/0}
    ].

%%====================================================================
%% Parse MRI Tests
%%====================================================================

parse_mri_tests() ->
    %% Basic realm-only MRI
    {ok, #{type := <<"realm">>, realm := <<"io.macula">>, path := []}} =
        macula_mri_nif:parse_mri(<<"mri:realm:io.macula">>),

    %% App with single path segment
    {ok, #{type := <<"app">>, realm := <<"io.macula">>, path := [<<"counter">>]}} =
        macula_mri_nif:parse_mri(<<"mri:app:io.macula/counter">>),

    %% Service with multiple path segments
    {ok, #{type := <<"service">>, realm := <<"io.acme">>, path := [<<"api">>, <<"v1">>]}} =
        macula_mri_nif:parse_mri(<<"mri:service:io.acme/api/v1">>),

    %% Content with deep path
    {ok, #{type := <<"content">>, realm := <<"io.example">>, path := [<<"a">>, <<"b">>, <<"c">>]}} =
        macula_mri_nif:parse_mri(<<"mri:content:io.example/a/b/c">>),

    %% Invalid format - missing mri: prefix
    {error, invalid_format} = macula_mri_nif:parse_mri(<<"app:io.macula/foo">>),

    %% Invalid format - missing type
    {error, invalid_format} = macula_mri_nif:parse_mri(<<"mri::io.macula">>),

    %% Invalid format - missing realm
    {error, invalid_format} = macula_mri_nif:parse_mri(<<"mri:app:">>),

    %% Invalid realm - no dot
    {error, invalid_realm} = macula_mri_nif:parse_mri(<<"mri:app:nodot">>),

    %% Invalid realm - uppercase
    {error, invalid_realm} = macula_mri_nif:parse_mri(<<"mri:app:IO.Macula">>),

    %% Invalid segment - uppercase
    {error, invalid_segment} = macula_mri_nif:parse_mri(<<"mri:app:io.macula/Counter">>),

    %% Invalid input type
    {error, invalid_format} = macula_mri_nif:parse_mri(not_a_binary),

    ok.

%%====================================================================
%% Validate Realm Tests
%%====================================================================

validate_realm_tests() ->
    %% Valid realms
    true = macula_mri_nif:validate_realm_format(<<"io.macula">>),
    true = macula_mri_nif:validate_realm_format(<<"com.example.app">>),
    true = macula_mri_nif:validate_realm_format(<<"org.erlang.otp">>),
    true = macula_mri_nif:validate_realm_format(<<"io.a1b2c3">>),

    %% Invalid realms
    false = macula_mri_nif:validate_realm_format(<<"nodot">>),
    false = macula_mri_nif:validate_realm_format(<<"IO.MACULA">>),
    false = macula_mri_nif:validate_realm_format(<<"io.macula!">>),
    false = macula_mri_nif:validate_realm_format(<<"io.mac ula">>),
    false = macula_mri_nif:validate_realm_format(<<>>),
    false = macula_mri_nif:validate_realm_format(not_a_binary),

    ok.

%%====================================================================
%% Validate Segment Tests
%%====================================================================

validate_segment_tests() ->
    %% Valid segments
    true = macula_mri_nif:validate_segment_chars(<<"counter">>),
    true = macula_mri_nif:validate_segment_chars(<<"my-app">>),
    true = macula_mri_nif:validate_segment_chars(<<"api_v1">>),
    true = macula_mri_nif:validate_segment_chars(<<"a1b2c3">>),
    true = macula_mri_nif:validate_segment_chars(<<"test-app_v2">>),

    %% Invalid segments
    false = macula_mri_nif:validate_segment_chars(<<"Counter">>),
    false = macula_mri_nif:validate_segment_chars(<<"my app">>),
    false = macula_mri_nif:validate_segment_chars(<<"app.name">>),
    false = macula_mri_nif:validate_segment_chars(<<"app/name">>),
    false = macula_mri_nif:validate_segment_chars(<<>>),
    false = macula_mri_nif:validate_segment_chars(not_a_binary),

    ok.

%%====================================================================
%% Builtin Type Tests
%%====================================================================

builtin_type_tests() ->
    %% Builtin types
    true = macula_mri_nif:is_builtin_type(<<"realm">>),
    true = macula_mri_nif:is_builtin_type(<<"org">>),
    true = macula_mri_nif:is_builtin_type(<<"user">>),
    true = macula_mri_nif:is_builtin_type(<<"app">>),
    true = macula_mri_nif:is_builtin_type(<<"service">>),
    true = macula_mri_nif:is_builtin_type(<<"artifact">>),
    true = macula_mri_nif:is_builtin_type(<<"license">>),
    true = macula_mri_nif:is_builtin_type(<<"cert">>),
    true = macula_mri_nif:is_builtin_type(<<"key">>),
    true = macula_mri_nif:is_builtin_type(<<"topic">>),
    true = macula_mri_nif:is_builtin_type(<<"proc">>),
    true = macula_mri_nif:is_builtin_type(<<"content">>),
    true = macula_mri_nif:is_builtin_type(<<"device">>),
    true = macula_mri_nif:is_builtin_type(<<"cluster">>),
    true = macula_mri_nif:is_builtin_type(<<"location">>),
    true = macula_mri_nif:is_builtin_type(<<"zone">>),
    true = macula_mri_nif:is_builtin_type(<<"network">>),
    true = macula_mri_nif:is_builtin_type(<<"model">>),
    true = macula_mri_nif:is_builtin_type(<<"dataset">>),
    true = macula_mri_nif:is_builtin_type(<<"config">>),
    true = macula_mri_nif:is_builtin_type(<<"class">>),
    true = macula_mri_nif:is_builtin_type(<<"taxonomy">>),

    %% Non-builtin types
    false = macula_mri_nif:is_builtin_type(<<"custom">>),
    false = macula_mri_nif:is_builtin_type(<<"unknown">>),
    false = macula_mri_nif:is_builtin_type(<<"REALM">>),
    false = macula_mri_nif:is_builtin_type(<<>>),
    false = macula_mri_nif:is_builtin_type(not_a_binary),

    ok.

%%====================================================================
%% Join Path Tests
%%====================================================================

join_path_tests() ->
    %% Empty list
    <<>> = macula_mri_nif:join_path_segments([]),

    %% Single segment
    <<"foo">> = macula_mri_nif:join_path_segments([<<"foo">>]),

    %% Two segments
    <<"foo/bar">> = macula_mri_nif:join_path_segments([<<"foo">>, <<"bar">>]),

    %% Multiple segments
    <<"a/b/c/d">> = macula_mri_nif:join_path_segments([<<"a">>, <<"b">>, <<"c">>, <<"d">>]),

    %% Segments with hyphens and underscores
    <<"my-app/api_v1">> = macula_mri_nif:join_path_segments([<<"my-app">>, <<"api_v1">>]),

    %% Invalid input
    <<>> = macula_mri_nif:join_path_segments(not_a_list),

    ok.

%%====================================================================
%% Format MRI Tests
%%====================================================================

format_mri_tests() ->
    %% Realm only
    {ok, <<"mri:realm:io.macula">>} =
        macula_mri_nif:format_mri(<<"realm">>, <<"io.macula">>, []),

    %% App with path
    {ok, <<"mri:app:io.macula/counter">>} =
        macula_mri_nif:format_mri(<<"app">>, <<"io.macula">>, [<<"counter">>]),

    %% Service with multiple path segments
    {ok, <<"mri:service:io.acme/api/v1">>} =
        macula_mri_nif:format_mri(<<"service">>, <<"io.acme">>, [<<"api">>, <<"v1">>]),

    %% Deep path
    {ok, <<"mri:content:io.example/a/b/c/d">>} =
        macula_mri_nif:format_mri(<<"content">>, <<"io.example">>, [<<"a">>, <<"b">>, <<"c">>, <<"d">>]),

    ok.

%%====================================================================
%% Find Children Tests
%%====================================================================

find_children_tests() ->
    AllMRIs = [
        {<<"io.macula">>, [], <<"mri:realm:io.macula">>},
        {<<"io.macula">>, [<<"apps">>], <<"mri:app:io.macula/apps">>},
        {<<"io.macula">>, [<<"apps">>, <<"counter">>], <<"mri:app:io.macula/apps/counter">>},
        {<<"io.macula">>, [<<"apps">>, <<"timer">>], <<"mri:app:io.macula/apps/timer">>},
        {<<"io.macula">>, [<<"apps">>, <<"counter">>, <<"v1">>], <<"mri:app:io.macula/apps/counter/v1">>},
        {<<"io.acme">>, [<<"apps">>], <<"mri:app:io.acme/apps">>},
        {<<"io.acme">>, [<<"apps">>, <<"widget">>], <<"mri:app:io.acme/apps/widget">>}
    ],

    %% Find children of root (realm only)
    [<<"mri:app:io.macula/apps">>] =
        macula_mri_nif:find_children(<<"io.macula">>, [], AllMRIs),

    %% Find children of apps folder
    Children = macula_mri_nif:find_children(<<"io.macula">>, [<<"apps">>], AllMRIs),
    2 = length(Children),
    true = lists:member(<<"mri:app:io.macula/apps/counter">>, Children),
    true = lists:member(<<"mri:app:io.macula/apps/timer">>, Children),

    %% Find children of counter (should find v1)
    [<<"mri:app:io.macula/apps/counter/v1">>] =
        macula_mri_nif:find_children(<<"io.macula">>, [<<"apps">>, <<"counter">>], AllMRIs),

    %% Find children in different realm
    [<<"mri:app:io.acme/apps/widget">>] =
        macula_mri_nif:find_children(<<"io.acme">>, [<<"apps">>], AllMRIs),

    %% No children
    [] = macula_mri_nif:find_children(<<"io.macula">>, [<<"apps">>, <<"counter">>, <<"v1">>], AllMRIs),

    %% Non-existent realm
    [] = macula_mri_nif:find_children(<<"io.nonexistent">>, [], AllMRIs),

    ok.

%%====================================================================
%% Find Descendants Tests
%%====================================================================

find_descendants_tests() ->
    AllMRIs = [
        {<<"io.macula">>, [], <<"mri:realm:io.macula">>},
        {<<"io.macula">>, [<<"apps">>], <<"mri:app:io.macula/apps">>},
        {<<"io.macula">>, [<<"apps">>, <<"counter">>], <<"mri:app:io.macula/apps/counter">>},
        {<<"io.macula">>, [<<"apps">>, <<"timer">>], <<"mri:app:io.macula/apps/timer">>},
        {<<"io.macula">>, [<<"apps">>, <<"counter">>, <<"v1">>], <<"mri:app:io.macula/apps/counter/v1">>},
        {<<"io.macula">>, [<<"apps">>, <<"counter">>, <<"v1">>, <<"api">>], <<"mri:app:io.macula/apps/counter/v1/api">>},
        {<<"io.acme">>, [<<"apps">>], <<"mri:app:io.acme/apps">>}
    ],

    %% Find all descendants of root
    Descendants = macula_mri_nif:find_descendants(<<"io.macula">>, [], AllMRIs),
    5 = length(Descendants),

    %% Find descendants of apps folder
    AppsDescendants = macula_mri_nif:find_descendants(<<"io.macula">>, [<<"apps">>], AllMRIs),
    4 = length(AppsDescendants),
    true = lists:member(<<"mri:app:io.macula/apps/counter">>, AppsDescendants),
    true = lists:member(<<"mri:app:io.macula/apps/timer">>, AppsDescendants),
    true = lists:member(<<"mri:app:io.macula/apps/counter/v1">>, AppsDescendants),
    true = lists:member(<<"mri:app:io.macula/apps/counter/v1/api">>, AppsDescendants),

    %% Find descendants of counter
    CounterDescendants = macula_mri_nif:find_descendants(<<"io.macula">>, [<<"apps">>, <<"counter">>], AllMRIs),
    2 = length(CounterDescendants),

    %% Leaf node has no descendants
    [] = macula_mri_nif:find_descendants(<<"io.macula">>, [<<"apps">>, <<"timer">>], AllMRIs),

    %% Different realm doesn't mix
    AcmeDescendants = macula_mri_nif:find_descendants(<<"io.acme">>, [], AllMRIs),
    1 = length(AcmeDescendants),

    ok.

%%====================================================================
%% Trie Index Tests
%%====================================================================

trie_index_tests() ->
    AllMRIs = [
        {<<"io.macula">>, [], <<"mri:realm:io.macula">>},
        {<<"io.macula">>, [<<"apps">>], <<"mri:app:io.macula/apps">>},
        {<<"io.macula">>, [<<"apps">>, <<"counter">>], <<"mri:app:io.macula/apps/counter">>},
        {<<"io.macula">>, [<<"apps">>, <<"timer">>], <<"mri:app:io.macula/apps/timer">>},
        {<<"io.macula">>, [<<"apps">>, <<"counter">>, <<"v1">>], <<"mri:app:io.macula/apps/counter/v1">>},
        {<<"io.macula">>, [<<"apps">>, <<"counter">>, <<"v1">>, <<"api">>], <<"mri:app:io.macula/apps/counter/v1/api">>},
        {<<"io.acme">>, [<<"apps">>], <<"mri:app:io.acme/apps">>},
        {<<"io.acme">>, [<<"apps">>, <<"widget">>], <<"mri:app:io.acme/apps/widget">>}
    ],

    %% Build index
    {ok, Index} = macula_mri_nif:build_path_index(AllMRIs),

    %% Check index size
    {ok, 8} = macula_mri_nif:index_size(Index),

    %% Find children of root in io.macula
    {ok, RootChildren} = macula_mri_nif:index_find_children(Index, <<"io.macula">>, []),
    1 = length(RootChildren),
    true = lists:member(<<"mri:app:io.macula/apps">>, RootChildren),

    %% Find children of apps folder
    {ok, AppsChildren} = macula_mri_nif:index_find_children(Index, <<"io.macula">>, [<<"apps">>]),
    2 = length(AppsChildren),
    true = lists:member(<<"mri:app:io.macula/apps/counter">>, AppsChildren),
    true = lists:member(<<"mri:app:io.macula/apps/timer">>, AppsChildren),

    %% Find children of counter (should find v1)
    {ok, CounterChildren} = macula_mri_nif:index_find_children(Index, <<"io.macula">>, [<<"apps">>, <<"counter">>]),
    1 = length(CounterChildren),
    [<<"mri:app:io.macula/apps/counter/v1">>] = CounterChildren,

    %% Find descendants of root
    {ok, RootDescendants} = macula_mri_nif:index_find_descendants(Index, <<"io.macula">>, []),
    5 = length(RootDescendants),

    %% Find descendants of apps folder
    {ok, AppsDescendants} = macula_mri_nif:index_find_descendants(Index, <<"io.macula">>, [<<"apps">>]),
    4 = length(AppsDescendants),
    true = lists:member(<<"mri:app:io.macula/apps/counter">>, AppsDescendants),
    true = lists:member(<<"mri:app:io.macula/apps/timer">>, AppsDescendants),
    true = lists:member(<<"mri:app:io.macula/apps/counter/v1">>, AppsDescendants),
    true = lists:member(<<"mri:app:io.macula/apps/counter/v1/api">>, AppsDescendants),

    %% Find descendants of counter
    {ok, CounterDescendants} = macula_mri_nif:index_find_descendants(Index, <<"io.macula">>, [<<"apps">>, <<"counter">>]),
    2 = length(CounterDescendants),

    %% Leaf node has no descendants
    {ok, []} = macula_mri_nif:index_find_descendants(Index, <<"io.macula">>, [<<"apps">>, <<"timer">>]),

    %% Different realm
    {ok, AcmeChildren} = macula_mri_nif:index_find_children(Index, <<"io.acme">>, [<<"apps">>]),
    1 = length(AcmeChildren),
    [<<"mri:app:io.acme/apps/widget">>] = AcmeChildren,

    %% Non-existent realm
    {ok, []} = macula_mri_nif:index_find_children(Index, <<"io.nonexistent">>, []),

    ok.

%%====================================================================
%% Trie Index Dynamic Update Tests
%%====================================================================

trie_index_dynamic_tests() ->
    %% Start with small index
    InitialMRIs = [
        {<<"io.macula">>, [<<"devices">>], <<"mri:device:io.macula/devices">>}
    ],
    {ok, Index} = macula_mri_nif:build_path_index(InitialMRIs),

    %% Check initial size
    {ok, 1} = macula_mri_nif:index_size(Index),

    %% Insert new device
    ok = macula_mri_nif:index_insert(Index, <<"io.macula">>, [<<"devices">>, <<"sensor1">>],
                                      <<"mri:device:io.macula/devices/sensor1">>),

    %% Verify insert - size should increase (for NIF) or check children
    {ok, Children1} = macula_mri_nif:index_find_children(Index, <<"io.macula">>, [<<"devices">>]),
    true = lists:member(<<"mri:device:io.macula/devices/sensor1">>, Children1),

    %% Insert another device
    ok = macula_mri_nif:index_insert(Index, <<"io.macula">>, [<<"devices">>, <<"sensor2">>],
                                      <<"mri:device:io.macula/devices/sensor2">>),

    {ok, Children2} = macula_mri_nif:index_find_children(Index, <<"io.macula">>, [<<"devices">>]),
    2 = length(Children2),

    %% Insert in different realm
    ok = macula_mri_nif:index_insert(Index, <<"io.acme">>, [<<"devices">>, <<"actuator1">>],
                                      <<"mri:device:io.acme/devices/actuator1">>),

    %% Verify realm isolation
    {ok, AcmeChildren} = macula_mri_nif:index_find_children(Index, <<"io.acme">>, [<<"devices">>]),
    1 = length(AcmeChildren),

    %% Remove a device
    ok = macula_mri_nif:index_remove(Index, <<"io.macula">>, [<<"devices">>, <<"sensor1">>]),

    %% Verify removal
    {ok, ChildrenAfterRemove} = macula_mri_nif:index_find_children(Index, <<"io.macula">>, [<<"devices">>]),
    1 = length(ChildrenAfterRemove),
    false = lists:member(<<"mri:device:io.macula/devices/sensor1">>, ChildrenAfterRemove),
    true = lists:member(<<"mri:device:io.macula/devices/sensor2">>, ChildrenAfterRemove),

    %% Remove non-existent path
    {error, not_found} = macula_mri_nif:index_remove(Index, <<"io.macula">>, [<<"nonexistent">>]),

    ok.

%%====================================================================
%% NIF Availability Test
%%====================================================================

nif_availability_test() ->
    %% This test just verifies the function returns a boolean
    %% The actual value depends on whether the NIF compiled successfully
    IsLoaded = macula_mri_nif:is_nif_loaded(),
    ?assert(is_boolean(IsLoaded)),

    %% Log the status for debugging
    case IsLoaded of
        true -> io:format("NIF is loaded~n");
        false -> io:format("Using Erlang fallbacks~n")
    end,

    ok.

%%====================================================================
%% Roundtrip Tests
%%====================================================================

roundtrip_test_() ->
    [
     {"Parse-Format roundtrip", fun roundtrip_tests/0}
    ].

roundtrip_tests() ->
    MRIs = [
        <<"mri:realm:io.macula">>,
        <<"mri:app:io.macula/counter">>,
        <<"mri:service:io.acme/api/v1">>,
        <<"mri:content:io.example/a/b/c">>
    ],
    lists:foreach(
        fun(MRI) ->
            {ok, #{type := Type, realm := Realm, path := Path}} =
                macula_mri_nif:parse_mri(MRI),
            {ok, Formatted} = macula_mri_nif:format_mri(Type, Realm, Path),
            ?assertEqual(MRI, Formatted)
        end,
        MRIs
    ),
    ok.
