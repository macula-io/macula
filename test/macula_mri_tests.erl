%%%-------------------------------------------------------------------
%%% @doc Tests for macula_mri module
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_tests).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Test Fixtures
%%===================================================================

setup() ->
    %% Ensure registry is running for type checks
    case whereis(macula_mri_registry) of
        undefined -> {ok, _} = macula_mri_registry:start_link([]);
        _ -> ok
    end.

cleanup(_) ->
    ok.

mri_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        %% Parsing Tests
        fun parse_valid_realm/0,
        fun parse_valid_org/0,
        fun parse_valid_user/0,
        fun parse_valid_service/0,
        fun parse_valid_content/0,
        fun parse_invalid_format/0,
        fun parse_unknown_type/0,
        fun parse_empty_input/0,
        %% Formatting Tests
        fun format_realm/0,
        fun format_org/0,
        fun format_user/0,
        fun format_service/0,
        fun format_roundtrip/0,
        %% Validation Tests
        fun validate_valid_mri/0,
        fun validate_invalid_type/0,
        fun validate_invalid_realm_format/0,
        fun validate_path_depth/0,
        fun validate_empty_segment/0,
        %% Accessor Tests
        fun type_accessor/0,
        fun realm_accessor/0,
        fun path_accessor/0,
        fun path_string_accessor/0,
        fun depth_accessor/0,
        %% Hierarchy Tests
        fun parent_realm_undefined/0,
        fun parent_org/0,
        fun parent_user/0,
        fun ancestors_realm/0,
        fun ancestors_org/0,
        fun ancestors_user/0,
        fun is_ancestor_check/0,
        %% Constructor Tests
        fun new_realm/0,
        fun new_org/0,
        fun new_user/0,
        fun new_app/0,
        fun new_service/0,
        fun new_from_map/0,
        fun new_from_components/0,
        %% Path Manipulation Tests
        fun append_segment/0,
        fun join_path/0,
        fun split_path/0,
        %% Khepri Path Tests
        fun to_khepri_path_realm/0,
        fun to_khepri_path_org/0,
        fun to_khepri_path_service/0,
        fun from_khepri_path_realm/0,
        fun from_khepri_path_org/0,
        fun khepri_path_roundtrip/0
     ]}.

%%===================================================================
%% Parsing Tests
%%===================================================================

parse_valid_realm() ->
    MRI = <<"mri:realm:io.macula">>,
    {ok, Result} = macula_mri:parse(MRI),
    ?assertEqual(realm, maps:get(type, Result)),
    ?assertEqual(<<"io.macula">>, maps:get(realm, Result)),
    ?assertEqual([], maps:get(path, Result)).

parse_valid_org() ->
    MRI = <<"mri:org:io.macula/acme">>,
    {ok, Result} = macula_mri:parse(MRI),
    ?assertEqual(org, maps:get(type, Result)),
    ?assertEqual(<<"io.macula">>, maps:get(realm, Result)),
    ?assertEqual([<<"acme">>], maps:get(path, Result)).

parse_valid_user() ->
    MRI = <<"mri:user:io.macula/acme/alice">>,
    {ok, Result} = macula_mri:parse(MRI),
    ?assertEqual(user, maps:get(type, Result)),
    ?assertEqual(<<"io.macula">>, maps:get(realm, Result)),
    ?assertEqual([<<"acme">>, <<"alice">>], maps:get(path, Result)).

parse_valid_service() ->
    MRI = <<"mri:service:io.macula/acme/myapp/api">>,
    {ok, Result} = macula_mri:parse(MRI),
    ?assertEqual(service, maps:get(type, Result)),
    ?assertEqual(<<"io.macula">>, maps:get(realm, Result)),
    ?assertEqual([<<"acme">>, <<"myapp">>, <<"api">>], maps:get(path, Result)).

parse_valid_content() ->
    MRI = <<"mri:content:io.macula/bafkreihabc123">>,
    {ok, Result} = macula_mri:parse(MRI),
    ?assertEqual(content, maps:get(type, Result)),
    ?assertEqual(<<"io.macula">>, maps:get(realm, Result)),
    ?assertEqual([<<"bafkreihabc123">>], maps:get(path, Result)).

parse_invalid_format() ->
    ?assertEqual({error, invalid_format}, macula_mri:parse(<<"uri:org:io.macula/acme">>)),
    ?assertEqual({error, invalid_format}, macula_mri:parse(<<"http://example.com">>)),
    ?assertEqual({error, invalid_format}, macula_mri:parse(<<"just-a-string">>)).

parse_unknown_type() ->
    ?assertEqual({error, {unknown_type, <<"unknown">>}}, macula_mri:parse(<<"mri:unknown:io.macula/acme">>)).

parse_empty_input() ->
    ?assertEqual({error, empty_mri}, macula_mri:parse(<<>>)),
    ?assertEqual({error, not_binary}, macula_mri:parse(not_binary)).

%%===================================================================
%% Formatting Tests
%%===================================================================

format_realm() ->
    Parsed = #{type => realm, realm => <<"io.macula">>, path => []},
    ?assertEqual(<<"mri:realm:io.macula">>, macula_mri:format(Parsed)).

format_org() ->
    Parsed = #{type => org, realm => <<"io.macula">>, path => [<<"acme">>]},
    ?assertEqual(<<"mri:org:io.macula/acme">>, macula_mri:format(Parsed)).

format_user() ->
    Parsed = #{type => user, realm => <<"io.macula">>, path => [<<"acme">>, <<"alice">>]},
    ?assertEqual(<<"mri:user:io.macula/acme/alice">>, macula_mri:format(Parsed)).

format_service() ->
    Parsed = #{type => service, realm => <<"io.macula">>, path => [<<"acme">>, <<"myapp">>, <<"api">>]},
    ?assertEqual(<<"mri:service:io.macula/acme/myapp/api">>, macula_mri:format(Parsed)).

format_roundtrip() ->
    MRI = <<"mri:service:io.macula/acme/myapp/backend">>,
    {ok, Parsed} = macula_mri:parse(MRI),
    Formatted = macula_mri:format(Parsed),
    ?assertEqual(MRI, Formatted).

%%===================================================================
%% Validation Tests
%%===================================================================

validate_valid_mri() ->
    ?assertEqual(ok, macula_mri:validate(<<"mri:realm:io.macula">>)),
    ?assertEqual(ok, macula_mri:validate(<<"mri:org:io.macula/acme">>)),
    ?assertEqual(ok, macula_mri:validate(<<"mri:user:io.macula/acme/alice">>)).

validate_invalid_type() ->
    ?assertMatch({error, {unknown_type, _}}, macula_mri:validate(<<"mri:foobar:io.macula/acme">>)).

validate_invalid_realm_format() ->
    %% Realm must contain a dot (DNS-like format)
    ?assertMatch({error, {invalid_realm_format, _}}, macula_mri:validate(<<"mri:org:macula/acme">>)),
    ?assertMatch({error, {invalid_realm_format, _}}, macula_mri:validate(<<"mri:org:INVALID/acme">>)).

validate_path_depth() ->
    %% Max depth is 10
    DeepPath = <<"mri:service:io.macula/a/b/c/d/e/f/g/h/i/j/k">>,  %% 11 segments
    ?assertMatch({error, {path_too_deep, _}}, macula_mri:validate(DeepPath)).

validate_empty_segment() ->
    ?assertMatch({error, empty_segment}, macula_mri:validate(<<"mri:org:io.macula//test">>)).

%%===================================================================
%% Accessor Tests
%%===================================================================

type_accessor() ->
    %% From binary
    ?assertEqual(org, macula_mri:type(<<"mri:org:io.macula/acme">>)),
    ?assertEqual(user, macula_mri:type(<<"mri:user:io.macula/acme/alice">>)),
    %% From map
    ?assertEqual(app, macula_mri:type(#{type => app, realm => <<"io.macula">>, path => []})).

realm_accessor() ->
    %% From binary
    ?assertEqual(<<"io.macula">>, macula_mri:realm(<<"mri:org:io.macula/acme">>)),
    ?assertEqual(<<"example.com">>, macula_mri:realm(<<"mri:realm:example.com">>)),
    %% From map
    ?assertEqual(<<"test.realm">>, macula_mri:realm(#{type => org, realm => <<"test.realm">>, path => []})).

path_accessor() ->
    ?assertEqual([<<"acme">>], macula_mri:path(<<"mri:org:io.macula/acme">>)),
    ?assertEqual([<<"acme">>, <<"alice">>], macula_mri:path(<<"mri:user:io.macula/acme/alice">>)),
    ?assertEqual([], macula_mri:path(<<"mri:realm:io.macula">>)).

path_string_accessor() ->
    ?assertEqual(<<"acme/alice">>, macula_mri:path_string(<<"mri:user:io.macula/acme/alice">>)),
    ?assertEqual(<<>>, macula_mri:path_string(<<"mri:realm:io.macula">>)).

depth_accessor() ->
    ?assertEqual(0, macula_mri:depth(<<"mri:realm:io.macula">>)),
    ?assertEqual(1, macula_mri:depth(<<"mri:org:io.macula/acme">>)),
    ?assertEqual(2, macula_mri:depth(<<"mri:user:io.macula/acme/alice">>)),
    ?assertEqual(3, macula_mri:depth(<<"mri:service:io.macula/acme/myapp/api">>)).

%%===================================================================
%% Hierarchy Tests
%%===================================================================

parent_realm_undefined() ->
    ?assertEqual(undefined, macula_mri:parent(<<"mri:realm:io.macula">>)).

parent_org() ->
    Parent = macula_mri:parent(<<"mri:org:io.macula/acme">>),
    ?assertEqual(<<"mri:realm:io.macula">>, Parent).

parent_user() ->
    Parent = macula_mri:parent(<<"mri:user:io.macula/acme/alice">>),
    ?assertEqual(<<"mri:org:io.macula/acme">>, Parent).

ancestors_realm() ->
    ?assertEqual([], macula_mri:ancestors(<<"mri:realm:io.macula">>)).

ancestors_org() ->
    Ancestors = macula_mri:ancestors(<<"mri:org:io.macula/acme">>),
    ?assertEqual([<<"mri:realm:io.macula">>], Ancestors).

ancestors_user() ->
    Ancestors = macula_mri:ancestors(<<"mri:user:io.macula/acme/alice">>),
    ?assertEqual(2, length(Ancestors)),
    ?assert(lists:member(<<"mri:realm:io.macula">>, Ancestors)),
    ?assert(lists:member(<<"mri:org:io.macula/acme">>, Ancestors)).

is_ancestor_check() ->
    MRI = <<"mri:user:io.macula/acme/alice">>,
    ?assert(macula_mri:is_ancestor(<<"mri:realm:io.macula">>, MRI)),
    ?assert(macula_mri:is_ancestor(<<"mri:org:io.macula/acme">>, MRI)),
    ?assertNot(macula_mri:is_ancestor(<<"mri:org:io.macula/other">>, MRI)).

%%===================================================================
%% Constructor Tests
%%===================================================================

new_realm() ->
    ?assertEqual(<<"mri:realm:io.macula">>, macula_mri:new_realm(<<"io.macula">>)),
    ?assertEqual(<<"mri:realm:example.com">>, macula_mri:new_realm(<<"example.com">>)).

new_org() ->
    ?assertEqual(<<"mri:org:io.macula/acme">>, macula_mri:new_org(<<"io.macula">>, <<"acme">>)).

new_user() ->
    ?assertEqual(<<"mri:user:io.macula/acme/alice">>,
                 macula_mri:new_user(<<"io.macula">>, <<"acme">>, <<"alice">>)).

new_app() ->
    ?assertEqual(<<"mri:app:io.macula/acme/myapp">>,
                 macula_mri:new_app(<<"io.macula">>, <<"acme">>, <<"myapp">>)).

new_service() ->
    ?assertEqual(<<"mri:service:io.macula/acme/myapp/api">>,
                 macula_mri:new_service(<<"io.macula">>, <<"acme">>, <<"myapp">>, <<"api">>)).

new_from_map() ->
    {ok, MRI} = macula_mri:new(#{type => org, realm => <<"io.macula">>, path => [<<"acme">>]}),
    ?assertEqual(<<"mri:org:io.macula/acme">>, MRI).

new_from_components() ->
    {ok, MRI} = macula_mri:new(user, <<"io.macula">>, [<<"acme">>, <<"bob">>]),
    ?assertEqual(<<"mri:user:io.macula/acme/bob">>, MRI).

%%===================================================================
%% Path Manipulation Tests
%%===================================================================

append_segment() ->
    MRI = <<"mri:org:io.macula/acme">>,
    NewMRI = macula_mri:append_segment(MRI, <<"subunit">>),
    ?assertEqual(<<"mri:org:io.macula/acme/subunit">>, NewMRI).

join_path() ->
    ?assertEqual(<<>>, macula_mri:join_path([])),
    ?assertEqual(<<"acme">>, macula_mri:join_path([<<"acme">>])),
    ?assertEqual(<<"acme/alice">>, macula_mri:join_path([<<"acme">>, <<"alice">>])),
    ?assertEqual(<<"a/b/c">>, macula_mri:join_path([<<"a">>, <<"b">>, <<"c">>])).

split_path() ->
    ?assertEqual([], macula_mri:split_path(<<>>)),
    ?assertEqual([<<"acme">>], macula_mri:split_path(<<"acme">>)),
    ?assertEqual([<<"acme">>, <<"alice">>], macula_mri:split_path(<<"acme/alice">>)).

%%===================================================================
%% Khepri Path Conversion Tests
%%===================================================================

to_khepri_path_realm() ->
    ?assertEqual([mri, realm, <<"io.macula">>],
                 macula_mri:to_khepri_path(<<"mri:realm:io.macula">>)).

to_khepri_path_org() ->
    ?assertEqual([mri, org, <<"io.macula">>, <<"acme">>],
                 macula_mri:to_khepri_path(<<"mri:org:io.macula/acme">>)).

to_khepri_path_service() ->
    Path = macula_mri:to_khepri_path(<<"mri:service:io.macula/acme/myapp/api">>),
    ?assertEqual([mri, service, <<"io.macula">>, <<"acme">>, <<"myapp">>, <<"api">>], Path).

from_khepri_path_realm() ->
    {ok, MRI} = macula_mri:from_khepri_path([mri, realm, <<"io.macula">>]),
    ?assertEqual(<<"mri:realm:io.macula">>, MRI).

from_khepri_path_org() ->
    {ok, MRI} = macula_mri:from_khepri_path([mri, org, <<"io.macula">>, <<"acme">>]),
    ?assertEqual(<<"mri:org:io.macula/acme">>, MRI).

khepri_path_roundtrip() ->
    MRI = <<"mri:service:io.macula/acme/myapp/api">>,
    Path = macula_mri:to_khepri_path(MRI),
    {ok, Restored} = macula_mri:from_khepri_path(Path),
    ?assertEqual(MRI, Restored).
