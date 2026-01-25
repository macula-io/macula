%%%-------------------------------------------------------------------
%%% @doc Tests for macula_mri_registry module
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Test Fixtures
%%===================================================================

setup() ->
    %% Start the registry for tests
    case whereis(macula_mri_registry) of
        undefined ->
            {ok, Pid} = macula_mri_registry:start_link([]),
            Pid;
        Pid ->
            Pid
    end.

cleanup(Pid) ->
    %% Don't stop if it was already running
    case is_process_alive(Pid) of
        true -> ok;
        false -> ok
    end.

registry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        fun builtin_types_valid/0,
        fun builtin_type_realm/0,
        fun builtin_type_org/0,
        fun builtin_type_user/0,
        fun builtin_type_app/0,
        fun builtin_type_service/0,
        fun builtin_type_artifact/0,
        fun builtin_type_content/0,
        fun builtin_type_device/0,
        fun builtin_type_model/0,
        fun builtin_type_class/0,
        fun invalid_type/0,
        fun register_custom_type/0,
        fun cannot_override_builtin/0,
        fun custom_type_with_realm/0,
        fun list_types/0,
        fun get_type_schema/0,
        fun path_schema/0,
        fun validate_path_for_type/0,
        fun unregister_custom_type/0
     ]}.

%%===================================================================
%% Built-in Type Tests
%%===================================================================

builtin_types_valid() ->
    %% All built-in types should be valid
    BuiltinTypes = [realm, org, user, app, service, artifact, license, cert, key,
                    topic, proc, content, device, cluster, location, zone, network,
                    model, dataset, config, class, taxonomy],
    lists:foreach(
        fun(Type) ->
            ?assert(macula_mri_registry:is_valid_type(Type))
        end,
        BuiltinTypes
    ).

builtin_type_realm() ->
    ?assert(macula_mri_registry:is_valid_type(realm)),
    ?assert(macula_mri_registry:is_valid_type(<<"realm">>)).

builtin_type_org() ->
    ?assert(macula_mri_registry:is_valid_type(org)),
    ?assert(macula_mri_registry:is_valid_type(<<"org">>)).

builtin_type_user() ->
    ?assert(macula_mri_registry:is_valid_type(user)),
    ?assert(macula_mri_registry:is_valid_type(<<"user">>)).

builtin_type_app() ->
    ?assert(macula_mri_registry:is_valid_type(app)),
    ?assert(macula_mri_registry:is_valid_type(<<"app">>)).

builtin_type_service() ->
    ?assert(macula_mri_registry:is_valid_type(service)),
    ?assert(macula_mri_registry:is_valid_type(<<"service">>)).

builtin_type_artifact() ->
    ?assert(macula_mri_registry:is_valid_type(artifact)),
    ?assert(macula_mri_registry:is_valid_type(<<"artifact">>)).

builtin_type_content() ->
    ?assert(macula_mri_registry:is_valid_type(content)),
    ?assert(macula_mri_registry:is_valid_type(<<"content">>)).

builtin_type_device() ->
    ?assert(macula_mri_registry:is_valid_type(device)),
    ?assert(macula_mri_registry:is_valid_type(<<"device">>)).

builtin_type_model() ->
    ?assert(macula_mri_registry:is_valid_type(model)),
    ?assert(macula_mri_registry:is_valid_type(<<"model">>)).

builtin_type_class() ->
    ?assert(macula_mri_registry:is_valid_type(class)),
    ?assert(macula_mri_registry:is_valid_type(<<"class">>)).

invalid_type() ->
    ?assertNot(macula_mri_registry:is_valid_type(unknown_type)),
    ?assertNot(macula_mri_registry:is_valid_type(<<"unknown_type">>)),
    ?assertNot(macula_mri_registry:is_valid_type(foobar)).

%%===================================================================
%% Custom Type Registration Tests
%%===================================================================

register_custom_type() ->
    TypeName = <<"custom_sensor">>,
    Schema = #{
        description => <<"IoT sensor type">>,
        path_schema => [org, sensor_id],
        parent_type => device
    },
    ?assertEqual(ok, macula_mri_registry:register_type(TypeName, Schema)),
    ?assert(macula_mri_registry:is_valid_type({custom, TypeName})),
    %% Cleanup
    macula_mri_registry:unregister_type(TypeName).

cannot_override_builtin() ->
    ?assertEqual({error, cannot_override_builtin},
                 macula_mri_registry:register_type(<<"realm">>, #{})),
    ?assertEqual({error, cannot_override_builtin},
                 macula_mri_registry:register_type(<<"org">>, #{})).

custom_type_with_realm() ->
    TypeName = <<"realm_specific">>,
    Schema = #{
        description => <<"Only valid in io.macula realm">>,
        realm => <<"io.macula">>
    },
    ?assertEqual(ok, macula_mri_registry:register_type(TypeName, Schema)),
    %% Valid in specified realm
    ?assert(macula_mri_registry:is_valid_type({custom, TypeName}, <<"io.macula">>)),
    %% Invalid in other realms
    ?assertNot(macula_mri_registry:is_valid_type({custom, TypeName}, <<"other.realm">>)),
    %% Cleanup
    macula_mri_registry:unregister_type(TypeName).

%%===================================================================
%% Type Listing Tests
%%===================================================================

list_types() ->
    Types = macula_mri_registry:list_types(),
    %% Should include all built-in types
    ?assert(lists:member(realm, Types)),
    ?assert(lists:member(org, Types)),
    ?assert(lists:member(user, Types)),
    ?assert(lists:member(app, Types)),
    ?assert(lists:member(service, Types)).

%%===================================================================
%% Schema Tests
%%===================================================================

get_type_schema() ->
    {ok, Schema} = macula_mri_registry:get_type_schema(org),
    ?assert(is_map(Schema)),
    ?assertEqual(org, maps:get(name, Schema)),
    ?assert(is_binary(maps:get(description, Schema))).

path_schema() ->
    {ok, PathSchema} = macula_mri_registry:path_schema(org),
    ?assertEqual([org], PathSchema),
    {ok, UserPathSchema} = macula_mri_registry:path_schema(user),
    ?assertEqual([org, user], UserPathSchema),
    {ok, ServicePathSchema} = macula_mri_registry:path_schema(service),
    ?assertEqual([org, app, service], ServicePathSchema).

validate_path_for_type() ->
    %% Valid path length for org (needs 1 segment)
    ?assertEqual(ok, macula_mri_registry:validate_path_for_type(org, [<<"acme">>])),
    %% Valid path length for user (needs 2 segments)
    ?assertEqual(ok, macula_mri_registry:validate_path_for_type(user, [<<"acme">>, <<"alice">>])),
    %% Too short
    ?assertMatch({error, {path_too_short, _, _, _, _}},
                 macula_mri_registry:validate_path_for_type(user, [<<"acme">>])),
    %% Too long
    ?assertMatch({error, {path_too_long, _, _, _, _}},
                 macula_mri_registry:validate_path_for_type(org, [<<"a">>, <<"b">>])).

%%===================================================================
%% Unregistration Tests
%%===================================================================

unregister_custom_type() ->
    TypeName = <<"to_unregister">>,
    ?assertEqual(ok, macula_mri_registry:register_type(TypeName, #{})),
    ?assert(macula_mri_registry:is_valid_type({custom, TypeName})),
    ?assertEqual(ok, macula_mri_registry:unregister_type(TypeName)),
    ?assertNot(macula_mri_registry:is_valid_type({custom, TypeName})).
