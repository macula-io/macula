%%%-------------------------------------------------------------------
%%% @doc MRI ETS Storage Adapter
%%%
%%% In-memory storage adapter using ETS tables.
%%% Implements both macula_mri_store and macula_mri_graph behaviours.
%%%
%%% Suitable for development, testing, and single-node deployments.
%%% For distributed deployments, use macula_mri_khepri.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_ets).

-behaviour(gen_server).

-compile({no_auto_import, [register/2]}).

%% macula_mri_store callbacks
-export([register/2, lookup/1, update/2, delete/1, exists/1]).
-export([list_children/1, list_descendants/1, list_by_type/2, list_by_realm/1]).
-export([import/1, export/0]).

%% macula_mri_graph callbacks
-export([create_relationship/3, create_relationship/4, delete_relationship/3]).
-export([related_to/2, related_from/2, all_related/1]).
-export([traverse_transitive/3, get_relationship/3]).
-export([instances_of/1, instances_of_transitive/1, classes_of/1]).
-export([subclasses/1, superclasses/1]).

%% API
-export([start_link/0, start_link/1, stop/0]).
-export([clear/0, stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(MRI_TABLE, macula_mri_ets_mris).
-define(REL_FORWARD_TABLE, macula_mri_ets_rel_forward).
-define(REL_REVERSE_TABLE, macula_mri_ets_rel_reverse).
-define(TYPE_INDEX_TABLE, macula_mri_ets_type_idx).
-define(REALM_INDEX_TABLE, macula_mri_ets_realm_idx).

-record(state, {}).

-record(mri_entry, {
    mri :: binary(),
    type :: atom(),
    realm :: binary(),
    path :: [binary()],
    metadata :: map(),
    registered_at :: integer()
}).

-record(rel_entry, {
    key :: {binary(), atom() | {custom, binary()}, binary()},  %% {Subject, Predicate, Object}
    subject :: binary(),
    predicate :: atom() | {custom, binary()},
    object :: binary(),
    metadata :: map(),
    created_at :: integer()
}).

%%===================================================================
%% API
%%===================================================================

%% @doc Start the ETS adapter.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Stop the ETS adapter.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Clear all data.
-spec clear() -> ok.
clear() ->
    gen_server:call(?SERVER, clear).

%% @doc Get statistics.
-spec stats() -> map().
stats() ->
    #{
        mri_count => ets:info(?MRI_TABLE, size),
        relationship_count => ets:info(?REL_FORWARD_TABLE, size)
    }.

%%===================================================================
%% macula_mri_store callbacks
%%===================================================================

-spec register(binary(), map()) -> ok | {error, term()}.
register(MRI, Metadata) when is_binary(MRI), is_map(Metadata) ->
    case macula_mri:parse(MRI) of
        {ok, #{type := Type, realm := Realm, path := Path}} ->
            Entry = #mri_entry{
                mri = MRI,
                type = Type,
                realm = Realm,
                path = Path,
                metadata = Metadata,
                registered_at = erlang:system_time(millisecond)
            },
            true = ets:insert(?MRI_TABLE, Entry),
            %% Update indexes
            true = ets:insert(?TYPE_INDEX_TABLE, {{Type, Realm, MRI}, true}),
            true = ets:insert(?REALM_INDEX_TABLE, {{Realm, MRI}, true}),
            ok;
        {error, _} = Err ->
            Err
    end.

-spec lookup(binary()) -> {ok, map()} | {error, not_found | term()}.
lookup(MRI) when is_binary(MRI) ->
    case ets:lookup(?MRI_TABLE, MRI) of
        [#mri_entry{metadata = Metadata, registered_at = Ts}] ->
            {ok, Metadata#{registered_at => Ts}};
        [] ->
            {error, not_found}
    end.

-spec update(binary(), map()) -> ok | {error, term()}.
update(MRI, Metadata) when is_binary(MRI), is_map(Metadata) ->
    case ets:lookup(?MRI_TABLE, MRI) of
        [#mri_entry{} = Entry] ->
            Updated = Entry#mri_entry{metadata = Metadata},
            true = ets:insert(?MRI_TABLE, Updated),
            ok;
        [] ->
            {error, not_found}
    end.

-spec delete(binary()) -> ok | {error, term()}.
delete(MRI) when is_binary(MRI) ->
    case ets:lookup(?MRI_TABLE, MRI) of
        [#mri_entry{type = Type, realm = Realm}] ->
            true = ets:delete(?MRI_TABLE, MRI),
            true = ets:delete(?TYPE_INDEX_TABLE, {Type, Realm, MRI}),
            true = ets:delete(?REALM_INDEX_TABLE, {Realm, MRI}),
            ok;
        [] ->
            {error, not_found}
    end.

-spec exists(binary()) -> boolean().
exists(MRI) when is_binary(MRI) ->
    ets:member(?MRI_TABLE, MRI).

-spec list_children(binary()) -> [binary()].
list_children(MRI) when is_binary(MRI) ->
    case macula_mri:parse(MRI) of
        {ok, #{realm := Realm, path := Path}} ->
            ParentDepth = length(Path),
            %% Find all MRIs that have this as parent
            ets:foldl(
                fun(#mri_entry{mri = ChildMRI, realm = R, path = P}, Acc)
                    when R =:= Realm, length(P) =:= ParentDepth + 1 ->
                        case lists:prefix(Path, P) of
                            true -> [ChildMRI | Acc];
                            false -> Acc
                        end;
                   (_, Acc) ->
                        Acc
                end,
                [],
                ?MRI_TABLE
            );
        {error, _} ->
            []
    end.

-spec list_descendants(binary()) -> [binary()].
list_descendants(MRI) when is_binary(MRI) ->
    case macula_mri:parse(MRI) of
        {ok, #{realm := Realm, path := Path}} ->
            ParentDepth = length(Path),
            %% Find all MRIs that have this as ancestor
            ets:foldl(
                fun(#mri_entry{mri = DescMRI, realm = R, path = P}, Acc)
                    when R =:= Realm, length(P) > ParentDepth ->
                        case lists:prefix(Path, P) of
                            true -> [DescMRI | Acc];
                            false -> Acc
                        end;
                   (_, Acc) ->
                        Acc
                end,
                [],
                ?MRI_TABLE
            );
        {error, _} ->
            []
    end.

-spec list_by_type(atom(), binary()) -> [binary()].
list_by_type(Type, Realm) when is_atom(Type), is_binary(Realm) ->
    Pattern = {{Type, Realm, '$1'}, '_'},
    [MRI || [MRI] <- ets:match(?TYPE_INDEX_TABLE, Pattern)].

-spec list_by_realm(binary()) -> [binary()].
list_by_realm(Realm) when is_binary(Realm) ->
    Pattern = {{Realm, '$1'}, '_'},
    [MRI || [MRI] <- ets:match(?REALM_INDEX_TABLE, Pattern)].

-spec import([{binary(), map()}]) -> ok | {error, term()}.
import(Entries) when is_list(Entries) ->
    lists:foreach(
        fun({MRI, Metadata}) -> register(MRI, Metadata) end,
        Entries
    ),
    ok.

-spec export() -> {ok, [{binary(), map()}]} | {error, term()}.
export() ->
    Entries = ets:foldl(
        fun(#mri_entry{mri = MRI, metadata = Meta}, Acc) ->
            [{MRI, Meta} | Acc]
        end,
        [],
        ?MRI_TABLE
    ),
    {ok, Entries}.

%%===================================================================
%% macula_mri_graph callbacks
%%===================================================================

-spec create_relationship(binary(), atom() | {custom, binary()}, binary()) ->
    ok | {error, term()}.
create_relationship(Subject, Predicate, Object) ->
    create_relationship(Subject, Predicate, Object, #{}).

-spec create_relationship(binary(), atom() | {custom, binary()}, binary(), map()) ->
    ok | {error, term()}.
create_relationship(Subject, Predicate, Object, Metadata)
    when is_binary(Subject), is_binary(Object), is_map(Metadata) ->
    Key = {Subject, Predicate, Object},
    Entry = #rel_entry{
        key = Key,
        subject = Subject,
        predicate = Predicate,
        object = Object,
        metadata = Metadata,
        created_at = erlang:system_time(millisecond)
    },
    %% Insert into both forward and reverse indexes
    true = ets:insert(?REL_FORWARD_TABLE, Entry),
    ReverseKey = {Object, Predicate, Subject},
    true = ets:insert(?REL_REVERSE_TABLE, Entry#rel_entry{key = ReverseKey}),
    ok.

-spec delete_relationship(binary(), atom() | {custom, binary()}, binary()) ->
    ok | {error, term()}.
delete_relationship(Subject, Predicate, Object)
    when is_binary(Subject), is_binary(Object) ->
    ForwardKey = {Subject, Predicate, Object},
    ReverseKey = {Object, Predicate, Subject},
    true = ets:delete(?REL_FORWARD_TABLE, ForwardKey),
    true = ets:delete(?REL_REVERSE_TABLE, ReverseKey),
    ok.

-spec related_to(binary(), atom() | {custom, binary()}) -> [binary()].
related_to(Subject, Predicate) when is_binary(Subject) ->
    Pattern = #rel_entry{key = {Subject, Predicate, '_'}, object = '$1', _ = '_'},
    [Object || [Object] <- ets:match(?REL_FORWARD_TABLE, Pattern)].

-spec related_from(binary(), atom() | {custom, binary()}) -> [binary()].
related_from(Object, Predicate) when is_binary(Object) ->
    %% Use reverse index: find all subjects that relate to this object
    Pattern = #rel_entry{key = {Object, Predicate, '_'}, subject = '$1', _ = '_'},
    [Subject || [Subject] <- ets:match(?REL_REVERSE_TABLE, Pattern)].

-spec all_related(binary()) -> [{atom() | {custom, binary()}, binary()}].
all_related(Subject) when is_binary(Subject) ->
    ets:foldl(
        fun(#rel_entry{subject = S, predicate = P, object = O}, Acc) when S =:= Subject ->
                [{P, O} | Acc];
           (_, Acc) ->
                Acc
        end,
        [],
        ?REL_FORWARD_TABLE
    ).

-spec traverse_transitive(binary(), atom() | {custom, binary()}, forward | reverse) ->
    [binary()].
traverse_transitive(Start, Predicate, Direction) when is_binary(Start) ->
    traverse_transitive(Start, Predicate, Direction, sets:new(), []).

traverse_transitive(Current, Predicate, Direction, Visited, Acc) ->
    case sets:is_element(Current, Visited) of
        true ->
            Acc;
        false ->
            NewVisited = sets:add_element(Current, Visited),
            Neighbors = case Direction of
                forward -> related_to(Current, Predicate);
                reverse -> related_from(Current, Predicate)
            end,
            NewAcc = Neighbors ++ Acc,
            lists:foldl(
                fun(Next, AccIn) ->
                    traverse_transitive(Next, Predicate, Direction, NewVisited, AccIn)
                end,
                NewAcc,
                Neighbors
            )
    end.

-spec get_relationship(binary(), atom() | {custom, binary()}, binary()) ->
    {ok, map()} | {error, not_found | term()}.
get_relationship(Subject, Predicate, Object)
    when is_binary(Subject), is_binary(Object) ->
    Key = {Subject, Predicate, Object},
    case ets:lookup(?REL_FORWARD_TABLE, Key) of
        [#rel_entry{subject = S, predicate = P, object = O, metadata = M, created_at = Ts}] ->
            {ok, #{subject => S, predicate => P, object => O, metadata => M, created_at => Ts}};
        [] ->
            {error, not_found}
    end.

%% Taxonomy helpers

-spec instances_of(binary()) -> [binary()].
instances_of(Class) when is_binary(Class) ->
    related_from(Class, instance_of).

-spec instances_of_transitive(binary()) -> [binary()].
instances_of_transitive(Class) when is_binary(Class) ->
    %% Get direct instances + instances of all subclasses
    DirectInstances = instances_of(Class),
    SubclassInstances = lists:flatmap(
        fun(Subclass) -> instances_of_transitive(Subclass) end,
        subclasses(Class)
    ),
    lists:usort(DirectInstances ++ SubclassInstances).

-spec classes_of(binary()) -> [binary()].
classes_of(Instance) when is_binary(Instance) ->
    related_to(Instance, instance_of).

-spec subclasses(binary()) -> [binary()].
subclasses(Class) when is_binary(Class) ->
    related_from(Class, subclass_of).

-spec superclasses(binary()) -> [binary()].
superclasses(Class) when is_binary(Class) ->
    related_to(Class, subclass_of).

%%===================================================================
%% gen_server callbacks
%%===================================================================

init(_Opts) ->
    %% Create ETS tables
    ets:new(?MRI_TABLE, [
        named_table, set, public,
        {keypos, #mri_entry.mri},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    ets:new(?REL_FORWARD_TABLE, [
        named_table, set, public,
        {keypos, #rel_entry.key},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    ets:new(?REL_REVERSE_TABLE, [
        named_table, set, public,
        {keypos, #rel_entry.key},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    ets:new(?TYPE_INDEX_TABLE, [
        named_table, set, public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    ets:new(?REALM_INDEX_TABLE, [
        named_table, set, public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    {ok, #state{}}.

handle_call(clear, _From, State) ->
    ets:delete_all_objects(?MRI_TABLE),
    ets:delete_all_objects(?REL_FORWARD_TABLE),
    ets:delete_all_objects(?REL_REVERSE_TABLE),
    ets:delete_all_objects(?TYPE_INDEX_TABLE),
    ets:delete_all_objects(?REALM_INDEX_TABLE),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Tables are owned by this process and will be deleted automatically
    ok.
