%%%-------------------------------------------------------------------
%%% @doc Tests for macula_mri_graph module (facade)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_graph_tests).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Test Fixtures
%%===================================================================

setup() ->
    %% Start required services
    case whereis(macula_mri_registry) of
        undefined -> {ok, _} = macula_mri_registry:start_link([]);
        _ -> ok
    end,
    case whereis(macula_mri_ets) of
        undefined -> {ok, Pid} = macula_mri_ets:start_link([]), Pid;
        Pid -> macula_mri_ets:clear(), Pid
    end.

cleanup(_Pid) ->
    macula_mri_ets:clear().

graph_facade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"adapter defaults to ETS", fun adapter_defaults/0},
        {"set and get adapter", fun set_adapter/0},
        {"create_relationship delegates", fun create_relationship_delegates/0},
        {"create_relationship with metadata", fun create_relationship_with_meta/0},
        {"delete_relationship delegates", fun delete_relationship_delegates/0},
        {"related_to delegates", fun related_to_delegates/0},
        {"related_from delegates", fun related_from_delegates/0},
        {"all_related delegates", fun all_related_delegates/0},
        {"traverse_transitive delegates", fun traverse_transitive_delegates/0},
        {"get_relationship delegates", fun get_relationship_delegates/0},
        {"instances_of delegates", fun instances_of_delegates/0},
        {"instances_of_transitive delegates", fun instances_of_transitive_delegates/0},
        {"classes_of delegates", fun classes_of_delegates/0},
        {"subclasses delegates", fun subclasses_delegates/0},
        {"superclasses delegates", fun superclasses_delegates/0},
        {"is_builtin_predicate", fun is_builtin_predicate/0},
        {"inverse_predicate", fun inverse_predicate/0}
     ]}.

%%===================================================================
%% Adapter Management Tests
%%===================================================================

adapter_defaults() ->
    ?assertEqual(macula_mri_ets, macula_mri_graph:adapter()).

set_adapter() ->
    Original = macula_mri_graph:adapter(),
    ?assertEqual(ok, macula_mri_graph:set_adapter(custom_graph_adapter)),
    ?assertEqual(custom_graph_adapter, macula_mri_graph:adapter()),
    %% Restore
    macula_mri_graph:set_adapter(Original).

%%===================================================================
%% Delegation Tests
%%===================================================================

create_relationship_delegates() ->
    Subject = <<"mri:user:io.graph.facade/org/alice">>,
    Object = <<"mri:org:io.graph.facade/org">>,
    macula_mri_ets:register(Subject, #{}),
    macula_mri_ets:register(Object, #{}),

    ?assertEqual(ok, macula_mri_graph:create_relationship(Subject, member_of, Object)),
    Related = macula_mri_graph:related_to(Subject, member_of),
    ?assertEqual([Object], Related).

create_relationship_with_meta() ->
    Subject = <<"mri:user:io.graph.meta/org/bob">>,
    Object = <<"mri:org:io.graph.meta/org">>,
    Meta = #{role => <<"admin">>},
    macula_mri_ets:register(Subject, #{}),
    macula_mri_ets:register(Object, #{}),

    ?assertEqual(ok, macula_mri_graph:create_relationship(Subject, member_of, Object, Meta)),
    {ok, Rel} = macula_mri_graph:get_relationship(Subject, member_of, Object),
    ?assertEqual(<<"admin">>, maps:get(role, maps:get(metadata, Rel))).

delete_relationship_delegates() ->
    Subject = <<"mri:user:io.graph.del/org/charlie">>,
    Object = <<"mri:org:io.graph.del/org">>,
    macula_mri_ets:register(Subject, #{}),
    macula_mri_ets:register(Object, #{}),
    macula_mri_graph:create_relationship(Subject, member_of, Object),

    ?assertEqual([Object], macula_mri_graph:related_to(Subject, member_of)),
    ?assertEqual(ok, macula_mri_graph:delete_relationship(Subject, member_of, Object)),
    ?assertEqual([], macula_mri_graph:related_to(Subject, member_of)).

related_to_delegates() ->
    Subject = <<"mri:app:io.graph.relto/org/app1">>,
    Dep = <<"mri:app:io.graph.relto/org/dep1">>,
    macula_mri_ets:register(Subject, #{}),
    macula_mri_ets:register(Dep, #{}),
    macula_mri_graph:create_relationship(Subject, depends_on, Dep),

    ?assertEqual([Dep], macula_mri_graph:related_to(Subject, depends_on)).

related_from_delegates() ->
    Object = <<"mri:org:io.graph.relfrom/org">>,
    Subject1 = <<"mri:user:io.graph.relfrom/org/u1">>,
    Subject2 = <<"mri:user:io.graph.relfrom/org/u2">>,
    macula_mri_ets:register(Object, #{}),
    macula_mri_ets:register(Subject1, #{}),
    macula_mri_ets:register(Subject2, #{}),
    macula_mri_graph:create_relationship(Subject1, member_of, Object),
    macula_mri_graph:create_relationship(Subject2, member_of, Object),

    Members = macula_mri_graph:related_from(Object, member_of),
    ?assertEqual(2, length(Members)).

all_related_delegates() ->
    Subject = <<"mri:app:io.graph.allrel/org/myapp">>,
    Dep = <<"mri:app:io.graph.allrel/org/dep">>,
    Owner = <<"mri:user:io.graph.allrel/org/owner">>,
    macula_mri_ets:register(Subject, #{}),
    macula_mri_ets:register(Dep, #{}),
    macula_mri_ets:register(Owner, #{}),
    macula_mri_graph:create_relationship(Subject, depends_on, Dep),
    macula_mri_graph:create_relationship(Subject, owned_by, Owner),

    AllRels = macula_mri_graph:all_related(Subject),
    ?assertEqual(2, length(AllRels)).

traverse_transitive_delegates() ->
    A = <<"mri:app:io.graph.trans/org/a">>,
    B = <<"mri:app:io.graph.trans/org/b">>,
    C = <<"mri:app:io.graph.trans/org/c">>,
    macula_mri_ets:register(A, #{}),
    macula_mri_ets:register(B, #{}),
    macula_mri_ets:register(C, #{}),
    macula_mri_graph:create_relationship(A, depends_on, B),
    macula_mri_graph:create_relationship(B, depends_on, C),

    TransDeps = macula_mri_graph:traverse_transitive(A, depends_on, forward),
    ?assert(lists:member(B, TransDeps)),
    ?assert(lists:member(C, TransDeps)).

get_relationship_delegates() ->
    Subject = <<"mri:user:io.graph.getrel/org/sub">>,
    Object = <<"mri:org:io.graph.getrel/org">>,
    macula_mri_ets:register(Subject, #{}),
    macula_mri_ets:register(Object, #{}),
    macula_mri_graph:create_relationship(Subject, member_of, Object, #{since => <<"2024">>}),

    {ok, Rel} = macula_mri_graph:get_relationship(Subject, member_of, Object),
    ?assertEqual(Subject, maps:get(subject, Rel)),
    ?assertEqual(Object, maps:get(object, Rel)),
    ?assertEqual(member_of, maps:get(predicate, Rel)).

%%===================================================================
%% Taxonomy Delegation Tests
%%===================================================================

instances_of_delegates() ->
    Class = <<"mri:class:io.graph.inst/device">>,
    Instance = <<"mri:device:io.graph.inst/org/dev1">>,
    macula_mri_ets:register(Class, #{}),
    macula_mri_ets:register(Instance, #{}),
    macula_mri_graph:create_relationship(Instance, instance_of, Class),

    ?assertEqual([Instance], macula_mri_graph:instances_of(Class)).

instances_of_transitive_delegates() ->
    Parent = <<"mri:class:io.graph.insttrans/vehicle">>,
    Child = <<"mri:class:io.graph.insttrans/car">>,
    Instance = <<"mri:device:io.graph.insttrans/org/mycar">>,
    macula_mri_ets:register(Parent, #{}),
    macula_mri_ets:register(Child, #{}),
    macula_mri_ets:register(Instance, #{}),
    macula_mri_graph:create_relationship(Child, subclass_of, Parent),
    macula_mri_graph:create_relationship(Instance, instance_of, Child),

    Instances = macula_mri_graph:instances_of_transitive(Parent),
    ?assert(lists:member(Instance, Instances)).

classes_of_delegates() ->
    Class = <<"mri:class:io.graph.classof/sensor">>,
    Instance = <<"mri:device:io.graph.classof/org/s1">>,
    macula_mri_ets:register(Class, #{}),
    macula_mri_ets:register(Instance, #{}),
    macula_mri_graph:create_relationship(Instance, instance_of, Class),

    ?assertEqual([Class], macula_mri_graph:classes_of(Instance)).

subclasses_delegates() ->
    Parent = <<"mri:class:io.graph.sub/animal">>,
    Child = <<"mri:class:io.graph.sub/mammal">>,
    macula_mri_ets:register(Parent, #{}),
    macula_mri_ets:register(Child, #{}),
    macula_mri_graph:create_relationship(Child, subclass_of, Parent),

    ?assertEqual([Child], macula_mri_graph:subclasses(Parent)).

superclasses_delegates() ->
    Child = <<"mri:class:io.graph.super/dog">>,
    Parent = <<"mri:class:io.graph.super/mammal">>,
    macula_mri_ets:register(Child, #{}),
    macula_mri_ets:register(Parent, #{}),
    macula_mri_graph:create_relationship(Child, subclass_of, Parent),

    ?assertEqual([Parent], macula_mri_graph:superclasses(Child)).

%%===================================================================
%% Predicate Utility Tests
%%===================================================================

is_builtin_predicate() ->
    %% Spatial
    ?assert(macula_mri_graph:is_builtin_predicate(located_at)),
    ?assert(macula_mri_graph:is_builtin_predicate(contains)),
    ?assert(macula_mri_graph:is_builtin_predicate(adjacent_to)),
    %% Organizational
    ?assert(macula_mri_graph:is_builtin_predicate(member_of)),
    ?assert(macula_mri_graph:is_builtin_predicate(manages)),
    ?assert(macula_mri_graph:is_builtin_predicate(owns)),
    %% Technical
    ?assert(macula_mri_graph:is_builtin_predicate(depends_on)),
    ?assert(macula_mri_graph:is_builtin_predicate(provides)),
    ?assert(macula_mri_graph:is_builtin_predicate(connects_to)),
    %% Taxonomic
    ?assert(macula_mri_graph:is_builtin_predicate(instance_of)),
    ?assert(macula_mri_graph:is_builtin_predicate(subclass_of)),
    %% Custom is not builtin
    ?assertNot(macula_mri_graph:is_builtin_predicate({custom, <<"my_predicate">>})),
    ?assertNot(macula_mri_graph:is_builtin_predicate(unknown_predicate)).

inverse_predicate() ->
    %% Spatial
    ?assertEqual(contains, macula_mri_graph:inverse_predicate(located_at)),
    ?assertEqual(located_at, macula_mri_graph:inverse_predicate(contains)),
    ?assertEqual(adjacent_to, macula_mri_graph:inverse_predicate(adjacent_to)), %% Symmetric
    %% Organizational
    ?assertEqual(has_member, macula_mri_graph:inverse_predicate(member_of)),
    ?assertEqual(member_of, macula_mri_graph:inverse_predicate(has_member)),
    ?assertEqual(managed_by, macula_mri_graph:inverse_predicate(manages)),
    ?assertEqual(owned_by, macula_mri_graph:inverse_predicate(owns)),
    %% Technical
    ?assertEqual(depended_on_by, macula_mri_graph:inverse_predicate(depends_on)),
    ?assertEqual(provided_by, macula_mri_graph:inverse_predicate(provides)),
    ?assertEqual(connects_to, macula_mri_graph:inverse_predicate(connects_to)), %% Symmetric
    %% Taxonomic
    ?assertEqual(has_instance, macula_mri_graph:inverse_predicate(instance_of)),
    ?assertEqual(superclass_of, macula_mri_graph:inverse_predicate(subclass_of)),
    %% Custom has no inverse
    ?assertEqual(undefined, macula_mri_graph:inverse_predicate({custom, <<"custom">>})),
    ?assertEqual(undefined, macula_mri_graph:inverse_predicate(unknown)).
