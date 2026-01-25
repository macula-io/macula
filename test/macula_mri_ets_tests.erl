%%%-------------------------------------------------------------------
%%% @doc Tests for macula_mri_ets module (ETS storage adapter)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_ets_tests).

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

ets_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        %% Store Tests
        {"register and lookup MRI", fun register_and_lookup/0},
        {"update MRI metadata", fun update_metadata/0},
        {"delete MRI", fun delete_mri/0},
        {"exists check", fun exists_check/0},
        {"lookup not found", fun lookup_not_found/0},
        {"list children", fun list_children/0},
        {"list descendants", fun list_descendants/0},
        {"list by type", fun list_by_type/0},
        {"list by realm", fun list_by_realm/0},
        {"import and export", fun import_export/0},
        %% Graph Tests
        {"create relationship", fun create_relationship/0},
        {"delete relationship", fun delete_relationship/0},
        {"related to query", fun related_to/0},
        {"related from query", fun related_from/0},
        {"all related query", fun all_related/0},
        {"traverse transitive forward", fun traverse_transitive_forward/0},
        {"traverse transitive reverse", fun traverse_transitive_reverse/0},
        {"get relationship metadata", fun get_relationship_metadata/0},
        %% Taxonomy Tests
        {"instances of class", fun instances_of/0},
        {"instances of transitive", fun instances_of_transitive/0},
        {"classes of instance", fun classes_of/0},
        {"subclasses", fun subclasses/0},
        {"superclasses", fun superclasses/0},
        %% Stats Tests
        {"stats returns counts", fun stats_counts/0}
     ]}.

%%===================================================================
%% Store Operation Tests
%%===================================================================

register_and_lookup() ->
    MRI = <<"mri:org:io.macula/acme">>,
    Metadata = #{name => <<"Acme Corp">>, active => true},
    ?assertEqual(ok, macula_mri_ets:register(MRI, Metadata)),
    {ok, Retrieved} = macula_mri_ets:lookup(MRI),
    ?assertEqual(<<"Acme Corp">>, maps:get(name, Retrieved)),
    ?assertEqual(true, maps:get(active, Retrieved)),
    ?assert(is_integer(maps:get(registered_at, Retrieved))).

update_metadata() ->
    MRI = <<"mri:org:io.macula/beta">>,
    ?assertEqual(ok, macula_mri_ets:register(MRI, #{version => 1})),
    {ok, V1} = macula_mri_ets:lookup(MRI),
    ?assertEqual(1, maps:get(version, V1)),
    ?assertEqual(ok, macula_mri_ets:update(MRI, #{version => 2})),
    {ok, V2} = macula_mri_ets:lookup(MRI),
    ?assertEqual(2, maps:get(version, V2)).

delete_mri() ->
    MRI = <<"mri:org:io.macula/gamma">>,
    ?assertEqual(ok, macula_mri_ets:register(MRI, #{})),
    ?assert(macula_mri_ets:exists(MRI)),
    ?assertEqual(ok, macula_mri_ets:delete(MRI)),
    ?assertNot(macula_mri_ets:exists(MRI)).

exists_check() ->
    MRI = <<"mri:org:io.macula/exists_test">>,
    ?assertNot(macula_mri_ets:exists(MRI)),
    ?assertEqual(ok, macula_mri_ets:register(MRI, #{})),
    ?assert(macula_mri_ets:exists(MRI)).

lookup_not_found() ->
    ?assertEqual({error, not_found}, macula_mri_ets:lookup(<<"mri:org:io.macula/nonexistent">>)).

list_children() ->
    %% Create parent and children
    Parent = <<"mri:org:io.macula/parent_org">>,
    Child1 = <<"mri:user:io.macula/parent_org/child1">>,
    Child2 = <<"mri:user:io.macula/parent_org/child2">>,
    Grandchild = <<"mri:app:io.macula/parent_org/child1/app1">>,

    ?assertEqual(ok, macula_mri_ets:register(Parent, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Child1, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Child2, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Grandchild, #{})),

    Children = macula_mri_ets:list_children(Parent),
    ?assertEqual(2, length(Children)),
    ?assert(lists:member(Child1, Children)),
    ?assert(lists:member(Child2, Children)),
    %% Grandchild should not be direct child
    ?assertNot(lists:member(Grandchild, Children)).

list_descendants() ->
    Parent = <<"mri:org:io.macula/desc_parent">>,
    Child = <<"mri:user:io.macula/desc_parent/child">>,
    Grandchild = <<"mri:app:io.macula/desc_parent/child/gc">>,

    ?assertEqual(ok, macula_mri_ets:register(Parent, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Child, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Grandchild, #{})),

    Descendants = macula_mri_ets:list_descendants(Parent),
    ?assertEqual(2, length(Descendants)),
    ?assert(lists:member(Child, Descendants)),
    ?assert(lists:member(Grandchild, Descendants)).

list_by_type() ->
    Realm = <<"io.test.type">>,
    Org1 = <<"mri:org:", Realm/binary, "/org1">>,
    Org2 = <<"mri:org:", Realm/binary, "/org2">>,
    User = <<"mri:user:", Realm/binary, "/org1/user1">>,

    ?assertEqual(ok, macula_mri_ets:register(Org1, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Org2, #{})),
    ?assertEqual(ok, macula_mri_ets:register(User, #{})),

    Orgs = macula_mri_ets:list_by_type(org, Realm),
    ?assertEqual(2, length(Orgs)),
    ?assert(lists:member(Org1, Orgs)),
    ?assert(lists:member(Org2, Orgs)).

list_by_realm() ->
    Realm = <<"io.test.realm">>,
    MRI1 = <<"mri:org:", Realm/binary, "/org1">>,
    MRI2 = <<"mri:user:", Realm/binary, "/org1/user1">>,

    ?assertEqual(ok, macula_mri_ets:register(MRI1, #{})),
    ?assertEqual(ok, macula_mri_ets:register(MRI2, #{})),

    MRIs = macula_mri_ets:list_by_realm(Realm),
    ?assertEqual(2, length(MRIs)),
    ?assert(lists:member(MRI1, MRIs)),
    ?assert(lists:member(MRI2, MRIs)).

import_export() ->
    Entries = [
        {<<"mri:org:io.import/org1">>, #{name => <<"Org 1">>}},
        {<<"mri:org:io.import/org2">>, #{name => <<"Org 2">>}}
    ],
    ?assertEqual(ok, macula_mri_ets:import(Entries)),

    {ok, Exported} = macula_mri_ets:export(),
    ?assert(length(Exported) >= 2),

    %% Verify imported entries exist
    {ok, Meta1} = macula_mri_ets:lookup(<<"mri:org:io.import/org1">>),
    ?assertEqual(<<"Org 1">>, maps:get(name, Meta1)).

%%===================================================================
%% Graph Operation Tests
%%===================================================================

create_relationship() ->
    Subject = <<"mri:user:io.graph/org/alice">>,
    Object = <<"mri:org:io.graph/org">>,

    ?assertEqual(ok, macula_mri_ets:register(Subject, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Object, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Subject, member_of, Object)),

    Related = macula_mri_ets:related_to(Subject, member_of),
    ?assertEqual([Object], Related).

delete_relationship() ->
    Subject = <<"mri:user:io.delrel/org/bob">>,
    Object = <<"mri:org:io.delrel/org">>,

    ?assertEqual(ok, macula_mri_ets:register(Subject, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Object, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Subject, member_of, Object)),

    ?assertEqual([Object], macula_mri_ets:related_to(Subject, member_of)),
    ?assertEqual(ok, macula_mri_ets:delete_relationship(Subject, member_of, Object)),
    ?assertEqual([], macula_mri_ets:related_to(Subject, member_of)).

related_to() ->
    Subject = <<"mri:app:io.relto/org/app1">>,
    Dep1 = <<"mri:app:io.relto/org/dep1">>,
    Dep2 = <<"mri:app:io.relto/org/dep2">>,

    ?assertEqual(ok, macula_mri_ets:register(Subject, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Dep1, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Dep2, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Subject, depends_on, Dep1)),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Subject, depends_on, Dep2)),

    Deps = macula_mri_ets:related_to(Subject, depends_on),
    ?assertEqual(2, length(Deps)),
    ?assert(lists:member(Dep1, Deps)),
    ?assert(lists:member(Dep2, Deps)).

related_from() ->
    Manager = <<"mri:user:io.relfrom/org/manager">>,
    Team1 = <<"mri:user:io.relfrom/org/team1">>,
    Team2 = <<"mri:user:io.relfrom/org/team2">>,

    ?assertEqual(ok, macula_mri_ets:register(Manager, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Team1, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Team2, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Team1, managed_by, Manager)),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Team2, managed_by, Manager)),

    ManagedBy = macula_mri_ets:related_from(Manager, managed_by),
    ?assertEqual(2, length(ManagedBy)),
    ?assert(lists:member(Team1, ManagedBy)),
    ?assert(lists:member(Team2, ManagedBy)).

all_related() ->
    Subject = <<"mri:app:io.allrel/org/myapp">>,
    Dep = <<"mri:app:io.allrel/org/dep">>,
    Owner = <<"mri:user:io.allrel/org/owner">>,

    ?assertEqual(ok, macula_mri_ets:register(Subject, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Dep, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Owner, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Subject, depends_on, Dep)),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Subject, owned_by, Owner)),

    AllRels = macula_mri_ets:all_related(Subject),
    ?assertEqual(2, length(AllRels)),
    ?assert(lists:member({depends_on, Dep}, AllRels)),
    ?assert(lists:member({owned_by, Owner}, AllRels)).

traverse_transitive_forward() ->
    %% A depends_on B depends_on C
    A = <<"mri:app:io.trans/org/a">>,
    B = <<"mri:app:io.trans/org/b">>,
    C = <<"mri:app:io.trans/org/c">>,

    ?assertEqual(ok, macula_mri_ets:register(A, #{})),
    ?assertEqual(ok, macula_mri_ets:register(B, #{})),
    ?assertEqual(ok, macula_mri_ets:register(C, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(A, depends_on, B)),
    ?assertEqual(ok, macula_mri_ets:create_relationship(B, depends_on, C)),

    %% Transitive: A should reach both B and C
    TransDeps = macula_mri_ets:traverse_transitive(A, depends_on, forward),
    ?assert(lists:member(B, TransDeps)),
    ?assert(lists:member(C, TransDeps)).

traverse_transitive_reverse() ->
    %% X depended_on_by Y depended_on_by Z
    X = <<"mri:app:io.transrev/org/x">>,
    Y = <<"mri:app:io.transrev/org/y">>,
    Z = <<"mri:app:io.transrev/org/z">>,

    ?assertEqual(ok, macula_mri_ets:register(X, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Y, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Z, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Y, depends_on, X)),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Z, depends_on, Y)),

    %% Reverse traversal from X should find Y and Z
    Dependents = macula_mri_ets:traverse_transitive(X, depends_on, reverse),
    ?assert(lists:member(Y, Dependents)),
    ?assert(lists:member(Z, Dependents)).

get_relationship_metadata() ->
    Subject = <<"mri:user:io.relmeta/org/sub">>,
    Object = <<"mri:org:io.relmeta/org">>,
    Meta = #{role => <<"admin">>, since => <<"2024-01-01">>},

    ?assertEqual(ok, macula_mri_ets:register(Subject, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Object, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Subject, member_of, Object, Meta)),

    {ok, Rel} = macula_mri_ets:get_relationship(Subject, member_of, Object),
    ?assertEqual(Subject, maps:get(subject, Rel)),
    ?assertEqual(member_of, maps:get(predicate, Rel)),
    ?assertEqual(Object, maps:get(object, Rel)),
    RelMeta = maps:get(metadata, Rel),
    ?assertEqual(<<"admin">>, maps:get(role, RelMeta)).

%%===================================================================
%% Taxonomy Tests
%%===================================================================

instances_of() ->
    Class = <<"mri:class:io.tax/sensor">>,
    Instance1 = <<"mri:device:io.tax/org/sensor1">>,
    Instance2 = <<"mri:device:io.tax/org/sensor2">>,

    ?assertEqual(ok, macula_mri_ets:register(Class, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Instance1, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Instance2, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Instance1, instance_of, Class)),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Instance2, instance_of, Class)),

    Instances = macula_mri_ets:instances_of(Class),
    ?assertEqual(2, length(Instances)),
    ?assert(lists:member(Instance1, Instances)),
    ?assert(lists:member(Instance2, Instances)).

instances_of_transitive() ->
    %% Vehicle -> Car -> Sedan
    Vehicle = <<"mri:class:io.taxtrans/vehicle">>,
    Car = <<"mri:class:io.taxtrans/car">>,
    Sedan = <<"mri:device:io.taxtrans/org/my_sedan">>,

    ?assertEqual(ok, macula_mri_ets:register(Vehicle, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Car, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Sedan, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Car, subclass_of, Vehicle)),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Sedan, instance_of, Car)),

    %% Sedan should be a transitive instance of Vehicle
    Instances = macula_mri_ets:instances_of_transitive(Vehicle),
    ?assert(lists:member(Sedan, Instances)).

classes_of() ->
    Class = <<"mri:class:io.classof/printer">>,
    Instance = <<"mri:device:io.classof/org/hp_printer">>,

    ?assertEqual(ok, macula_mri_ets:register(Class, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Instance, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Instance, instance_of, Class)),

    Classes = macula_mri_ets:classes_of(Instance),
    ?assertEqual([Class], Classes).

subclasses() ->
    Parent = <<"mri:class:io.sub/animal">>,
    Child1 = <<"mri:class:io.sub/mammal">>,
    Child2 = <<"mri:class:io.sub/bird">>,

    ?assertEqual(ok, macula_mri_ets:register(Parent, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Child1, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Child2, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Child1, subclass_of, Parent)),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Child2, subclass_of, Parent)),

    Subs = macula_mri_ets:subclasses(Parent),
    ?assertEqual(2, length(Subs)),
    ?assert(lists:member(Child1, Subs)),
    ?assert(lists:member(Child2, Subs)).

superclasses() ->
    Child = <<"mri:class:io.super/dog">>,
    Parent = <<"mri:class:io.super/mammal">>,

    ?assertEqual(ok, macula_mri_ets:register(Child, #{})),
    ?assertEqual(ok, macula_mri_ets:register(Parent, #{})),
    ?assertEqual(ok, macula_mri_ets:create_relationship(Child, subclass_of, Parent)),

    Supers = macula_mri_ets:superclasses(Child),
    ?assertEqual([Parent], Supers).

%%===================================================================
%% Stats Tests
%%===================================================================

stats_counts() ->
    Stats = macula_mri_ets:stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(mri_count, Stats)),
    ?assert(maps:is_key(relationship_count, Stats)),
    ?assert(is_integer(maps:get(mri_count, Stats))),
    ?assert(is_integer(maps:get(relationship_count, Stats))).
