%%%-------------------------------------------------------------------
%%% @doc Tests for macula_mri_store module (facade)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_store_tests).

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

store_facade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"adapter defaults to ETS", fun adapter_defaults/0},
        {"set and get adapter", fun set_adapter/0},
        {"register delegates to adapter", fun register_delegates/0},
        {"lookup delegates to adapter", fun lookup_delegates/0},
        {"update delegates to adapter", fun update_delegates/0},
        {"delete delegates to adapter", fun delete_delegates/0},
        {"exists delegates to adapter", fun exists_delegates/0},
        {"list_children delegates", fun list_children_delegates/0},
        {"list_descendants delegates", fun list_descendants_delegates/0},
        {"list_by_type delegates", fun list_by_type_delegates/0},
        {"list_by_realm delegates", fun list_by_realm_delegates/0},
        {"import delegates", fun import_delegates/0},
        {"export delegates", fun export_delegates/0}
     ]}.

%%===================================================================
%% Adapter Management Tests
%%===================================================================

adapter_defaults() ->
    ?assertEqual(macula_mri_ets, macula_mri_store:adapter()).

set_adapter() ->
    Original = macula_mri_store:adapter(),
    ?assertEqual(ok, macula_mri_store:set_adapter(some_custom_adapter)),
    ?assertEqual(some_custom_adapter, macula_mri_store:adapter()),
    %% Restore
    macula_mri_store:set_adapter(Original).

%%===================================================================
%% Delegation Tests
%%===================================================================

register_delegates() ->
    MRI = <<"mri:org:io.store.facade/acme">>,
    ?assertEqual(ok, macula_mri_store:register(MRI, #{name => <<"Acme">>})),
    ?assert(macula_mri_ets:exists(MRI)).

lookup_delegates() ->
    MRI = <<"mri:org:io.store.lookup/beta">>,
    macula_mri_ets:register(MRI, #{version => 42}),
    {ok, Meta} = macula_mri_store:lookup(MRI),
    ?assertEqual(42, maps:get(version, Meta)).

update_delegates() ->
    MRI = <<"mri:org:io.store.update/gamma">>,
    macula_mri_store:register(MRI, #{v => 1}),
    ?assertEqual(ok, macula_mri_store:update(MRI, #{v => 2})),
    {ok, Meta} = macula_mri_store:lookup(MRI),
    ?assertEqual(2, maps:get(v, Meta)).

delete_delegates() ->
    MRI = <<"mri:org:io.store.delete/delta">>,
    macula_mri_store:register(MRI, #{}),
    ?assert(macula_mri_store:exists(MRI)),
    ?assertEqual(ok, macula_mri_store:delete(MRI)),
    ?assertNot(macula_mri_store:exists(MRI)).

exists_delegates() ->
    MRI = <<"mri:org:io.store.exists/epsilon">>,
    ?assertNot(macula_mri_store:exists(MRI)),
    macula_mri_store:register(MRI, #{}),
    ?assert(macula_mri_store:exists(MRI)).

list_children_delegates() ->
    Parent = <<"mri:org:io.store.children/parent">>,
    Child = <<"mri:user:io.store.children/parent/child">>,
    macula_mri_store:register(Parent, #{}),
    macula_mri_store:register(Child, #{}),
    Children = macula_mri_store:list_children(Parent),
    ?assert(lists:member(Child, Children)).

list_descendants_delegates() ->
    Parent = <<"mri:org:io.store.desc/parent">>,
    Child = <<"mri:user:io.store.desc/parent/child">>,
    Grandchild = <<"mri:app:io.store.desc/parent/child/gc">>,
    macula_mri_store:register(Parent, #{}),
    macula_mri_store:register(Child, #{}),
    macula_mri_store:register(Grandchild, #{}),
    Descendants = macula_mri_store:list_descendants(Parent),
    ?assertEqual(2, length(Descendants)).

list_by_type_delegates() ->
    Realm = <<"io.store.bytype">>,
    MRI1 = <<"mri:org:", Realm/binary, "/o1">>,
    MRI2 = <<"mri:org:", Realm/binary, "/o2">>,
    macula_mri_store:register(MRI1, #{}),
    macula_mri_store:register(MRI2, #{}),
    Orgs = macula_mri_store:list_by_type(org, Realm),
    ?assertEqual(2, length(Orgs)).

list_by_realm_delegates() ->
    Realm = <<"io.store.byrealm">>,
    MRI1 = <<"mri:org:", Realm/binary, "/o1">>,
    MRI2 = <<"mri:user:", Realm/binary, "/o1/u1">>,
    macula_mri_store:register(MRI1, #{}),
    macula_mri_store:register(MRI2, #{}),
    MRIs = macula_mri_store:list_by_realm(Realm),
    ?assertEqual(2, length(MRIs)).

import_delegates() ->
    Entries = [
        {<<"mri:org:io.store.import/a">>, #{a => 1}},
        {<<"mri:org:io.store.import/b">>, #{b => 2}}
    ],
    ?assertEqual(ok, macula_mri_store:import(Entries)),
    ?assert(macula_mri_store:exists(<<"mri:org:io.store.import/a">>)),
    ?assert(macula_mri_store:exists(<<"mri:org:io.store.import/b">>)).

export_delegates() ->
    MRI = <<"mri:org:io.store.export/test">>,
    macula_mri_store:register(MRI, #{test => true}),
    {ok, Exported} = macula_mri_store:export(),
    ?assert(is_list(Exported)),
    %% Should contain at least our MRI
    ?assert(lists:any(fun({M, _}) -> M =:= MRI end, Exported)).
