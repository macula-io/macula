%%%-------------------------------------------------------------------
%%% @doc MRI Graph Behaviour
%%%
%%% Defines the interface for MRI relationship (graph) storage.
%%% Implementations must provide relationship CRUD and traversal.
%%%
%%% Relationships are triples: {Subject, Predicate, Object}
%%% Both forward (subject→object) and reverse (object→subject) queries
%%% should be efficient.
%%%
%%% Built-in predicates include:
%%% - Spatial: located_at, contains, adjacent_to
%%% - Organizational: member_of, has_member, manages, managed_by, owns, owned_by
%%% - Technical: depends_on, depended_on_by, provides, provided_by, connects_to
%%% - Data: trained_on, derived_from, source_of, version_of
%%% - Taxonomic: instance_of, has_instance, subclass_of, superclass_of
%%%
%%% Custom predicates use the format: {custom, <<"predicate_name">>}
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_graph).

%% Type exports
-export_type([predicate/0, direction/0, relationship/0]).

-type predicate() :: atom() | {custom, binary()}.
-type direction() :: forward | reverse.
-type relationship() :: #{
    subject := macula_mri:mri(),
    predicate := predicate(),
    object := macula_mri:mri(),
    metadata => map()
}.

%% Behaviour definition

%% Create a relationship between two MRIs
-callback create_relationship(
    Subject :: macula_mri:mri(),
    Predicate :: predicate(),
    Object :: macula_mri:mri()
) -> ok | {error, term()}.

%% Create a relationship with metadata
-callback create_relationship(
    Subject :: macula_mri:mri(),
    Predicate :: predicate(),
    Object :: macula_mri:mri(),
    Metadata :: map()
) -> ok | {error, term()}.

%% Delete a relationship
-callback delete_relationship(
    Subject :: macula_mri:mri(),
    Predicate :: predicate(),
    Object :: macula_mri:mri()
) -> ok | {error, term()}.

%% Query: what does Subject relate to via Predicate?
-callback related_to(
    Subject :: macula_mri:mri(),
    Predicate :: predicate()
) -> [macula_mri:mri()].

%% Query: what relates to Object via Predicate?
-callback related_from(
    Object :: macula_mri:mri(),
    Predicate :: predicate()
) -> [macula_mri:mri()].

%% Query: all relationships from a subject
-callback all_related(Subject :: macula_mri:mri()) ->
    [{predicate(), macula_mri:mri()}].

%% Transitive traversal (follow relationships recursively)
-callback traverse_transitive(
    Start :: macula_mri:mri(),
    Predicate :: predicate(),
    Direction :: direction()
) -> [macula_mri:mri()].

%% Get relationship with metadata
-callback get_relationship(
    Subject :: macula_mri:mri(),
    Predicate :: predicate(),
    Object :: macula_mri:mri()
) -> {ok, relationship()} | {error, not_found | term()}.

%% Optional callbacks for taxonomy operations
-callback instances_of(Class :: macula_mri:mri()) -> [macula_mri:mri()].
-callback instances_of_transitive(Class :: macula_mri:mri()) -> [macula_mri:mri()].
-callback classes_of(Instance :: macula_mri:mri()) -> [macula_mri:mri()].
-callback subclasses(Class :: macula_mri:mri()) -> [macula_mri:mri()].
-callback superclasses(Class :: macula_mri:mri()) -> [macula_mri:mri()].

-optional_callbacks([
    instances_of/1,
    instances_of_transitive/1,
    classes_of/1,
    subclasses/1,
    superclasses/1
]).

%% API for working with configured adapter
-export([adapter/0, set_adapter/1]).
-export([create_relationship/3, create_relationship/4, delete_relationship/3]).
-export([related_to/2, related_from/2, all_related/1]).
-export([traverse_transitive/3, get_relationship/3]).
-export([instances_of/1, instances_of_transitive/1, classes_of/1]).
-export([subclasses/1, superclasses/1]).

%% Predicate utilities
-export([is_builtin_predicate/1, inverse_predicate/1]).

-define(ADAPTER_KEY, {?MODULE, adapter}).
-define(DEFAULT_ADAPTER, macula_mri_ets).

%%===================================================================
%% Adapter Management
%%===================================================================

%% @doc Get the configured graph adapter module.
-spec adapter() -> module().
adapter() ->
    case persistent_term:get(?ADAPTER_KEY, undefined) of
        undefined -> ?DEFAULT_ADAPTER;
        Adapter -> Adapter
    end.

%% @doc Set the graph adapter module.
-spec set_adapter(module()) -> ok.
set_adapter(Module) when is_atom(Module) ->
    persistent_term:put(?ADAPTER_KEY, Module),
    ok.

%%===================================================================
%% Delegating API
%%===================================================================

%% @doc Create a relationship between two MRIs.
-spec create_relationship(macula_mri:mri(), predicate(), macula_mri:mri()) ->
    ok | {error, term()}.
create_relationship(Subject, Predicate, Object) ->
    (adapter()):create_relationship(Subject, Predicate, Object).

%% @doc Create a relationship with metadata.
-spec create_relationship(macula_mri:mri(), predicate(), macula_mri:mri(), map()) ->
    ok | {error, term()}.
create_relationship(Subject, Predicate, Object, Metadata) ->
    (adapter()):create_relationship(Subject, Predicate, Object, Metadata).

%% @doc Delete a relationship.
-spec delete_relationship(macula_mri:mri(), predicate(), macula_mri:mri()) ->
    ok | {error, term()}.
delete_relationship(Subject, Predicate, Object) ->
    (adapter()):delete_relationship(Subject, Predicate, Object).

%% @doc Query what Subject relates to via Predicate.
-spec related_to(macula_mri:mri(), predicate()) -> [macula_mri:mri()].
related_to(Subject, Predicate) ->
    (adapter()):related_to(Subject, Predicate).

%% @doc Query what relates to Object via Predicate.
-spec related_from(macula_mri:mri(), predicate()) -> [macula_mri:mri()].
related_from(Object, Predicate) ->
    (adapter()):related_from(Object, Predicate).

%% @doc Get all relationships from a subject.
-spec all_related(macula_mri:mri()) -> [{predicate(), macula_mri:mri()}].
all_related(Subject) ->
    (adapter()):all_related(Subject).

%% @doc Traverse relationships transitively.
-spec traverse_transitive(macula_mri:mri(), predicate(), direction()) ->
    [macula_mri:mri()].
traverse_transitive(Start, Predicate, Direction) ->
    (adapter()):traverse_transitive(Start, Predicate, Direction).

%% @doc Get a specific relationship with its metadata.
-spec get_relationship(macula_mri:mri(), predicate(), macula_mri:mri()) ->
    {ok, relationship()} | {error, not_found | term()}.
get_relationship(Subject, Predicate, Object) ->
    (adapter()):get_relationship(Subject, Predicate, Object).

%%===================================================================
%% Taxonomy Operations
%%===================================================================

%% @doc Get direct instances of a class.
-spec instances_of(macula_mri:mri()) -> [macula_mri:mri()].
instances_of(Class) ->
    Adapter = adapter(),
    case erlang:function_exported(Adapter, instances_of, 1) of
        true -> Adapter:instances_of(Class);
        false -> related_from(Class, instance_of)
    end.

%% @doc Get all instances of a class including subclasses (transitive).
-spec instances_of_transitive(macula_mri:mri()) -> [macula_mri:mri()].
instances_of_transitive(Class) ->
    Adapter = adapter(),
    case erlang:function_exported(Adapter, instances_of_transitive, 1) of
        true ->
            Adapter:instances_of_transitive(Class);
        false ->
            %% Default implementation: collect instances from class and all subclasses
            DirectInstances = instances_of(Class),
            SubclassInstances = lists:flatmap(
                fun(Subclass) -> instances_of_transitive(Subclass) end,
                subclasses(Class)
            ),
            lists:usort(DirectInstances ++ SubclassInstances)
    end.

%% @doc Get the classes an instance belongs to.
-spec classes_of(macula_mri:mri()) -> [macula_mri:mri()].
classes_of(Instance) ->
    Adapter = adapter(),
    case erlang:function_exported(Adapter, classes_of, 1) of
        true -> Adapter:classes_of(Instance);
        false -> related_to(Instance, instance_of)
    end.

%% @doc Get direct subclasses of a class.
-spec subclasses(macula_mri:mri()) -> [macula_mri:mri()].
subclasses(Class) ->
    Adapter = adapter(),
    case erlang:function_exported(Adapter, subclasses, 1) of
        true -> Adapter:subclasses(Class);
        false -> related_from(Class, subclass_of)
    end.

%% @doc Get direct superclasses of a class.
-spec superclasses(macula_mri:mri()) -> [macula_mri:mri()].
superclasses(Class) ->
    Adapter = adapter(),
    case erlang:function_exported(Adapter, superclasses, 1) of
        true -> Adapter:superclasses(Class);
        false -> related_to(Class, subclass_of)
    end.

%%===================================================================
%% Predicate Utilities
%%===================================================================

%% @doc Check if a predicate is a built-in predicate.
-spec is_builtin_predicate(predicate()) -> boolean().
is_builtin_predicate(located_at) -> true;
is_builtin_predicate(contains) -> true;
is_builtin_predicate(adjacent_to) -> true;
is_builtin_predicate(member_of) -> true;
is_builtin_predicate(has_member) -> true;
is_builtin_predicate(manages) -> true;
is_builtin_predicate(managed_by) -> true;
is_builtin_predicate(owns) -> true;
is_builtin_predicate(owned_by) -> true;
is_builtin_predicate(depends_on) -> true;
is_builtin_predicate(depended_on_by) -> true;
is_builtin_predicate(provides) -> true;
is_builtin_predicate(provided_by) -> true;
is_builtin_predicate(consumes) -> true;
is_builtin_predicate(consumed_by) -> true;
is_builtin_predicate(connects_to) -> true;
is_builtin_predicate(trained_on) -> true;
is_builtin_predicate(used_to_train) -> true;
is_builtin_predicate(derived_from) -> true;
is_builtin_predicate(source_of) -> true;
is_builtin_predicate(version_of) -> true;
is_builtin_predicate(instance_of) -> true;
is_builtin_predicate(has_instance) -> true;
is_builtin_predicate(subclass_of) -> true;
is_builtin_predicate(superclass_of) -> true;
is_builtin_predicate(categorized_as) -> true;
is_builtin_predicate(categorizes) -> true;
is_builtin_predicate({custom, _}) -> false;
is_builtin_predicate(_) -> false.

%% @doc Get the inverse of a predicate (for bidirectional queries).
-spec inverse_predicate(predicate()) -> predicate() | undefined.
inverse_predicate(located_at) -> contains;
inverse_predicate(contains) -> located_at;
inverse_predicate(adjacent_to) -> adjacent_to;  %% Symmetric
inverse_predicate(member_of) -> has_member;
inverse_predicate(has_member) -> member_of;
inverse_predicate(manages) -> managed_by;
inverse_predicate(managed_by) -> manages;
inverse_predicate(owns) -> owned_by;
inverse_predicate(owned_by) -> owns;
inverse_predicate(depends_on) -> depended_on_by;
inverse_predicate(depended_on_by) -> depends_on;
inverse_predicate(provides) -> provided_by;
inverse_predicate(provided_by) -> provides;
inverse_predicate(consumes) -> consumed_by;
inverse_predicate(consumed_by) -> consumes;
inverse_predicate(connects_to) -> connects_to;  %% Symmetric
inverse_predicate(trained_on) -> used_to_train;
inverse_predicate(used_to_train) -> trained_on;
inverse_predicate(derived_from) -> source_of;
inverse_predicate(source_of) -> derived_from;
inverse_predicate(version_of) -> undefined;  %% Not symmetric
inverse_predicate(instance_of) -> has_instance;
inverse_predicate(has_instance) -> instance_of;
inverse_predicate(subclass_of) -> superclass_of;
inverse_predicate(superclass_of) -> subclass_of;
inverse_predicate(categorized_as) -> categorizes;
inverse_predicate(categorizes) -> categorized_as;
inverse_predicate({custom, _}) -> undefined;
inverse_predicate(_) -> undefined.
