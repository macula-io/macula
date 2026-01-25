%%%-------------------------------------------------------------------
%%% @doc MRI Type Registry
%%%
%%% Manages the registry of valid MRI types with their schemas.
%%% Provides both built-in types and runtime registration of custom types.
%%%
%%% Built-in types are always valid. Custom types can be registered
%%% at runtime and optionally scoped to specific realms.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([is_valid_type/1, is_valid_type/2]).
-export([register_type/2, unregister_type/1]).
-export([get_type_schema/1, list_types/0, list_custom_types/0]).
-export([path_schema/1, validate_path_for_type/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, macula_mri_types).

%% Built-in types that are always valid
-define(BUILTIN_TYPES, [
    realm, org, user, app, service,
    artifact, license, cert, key,
    topic, proc, content,
    device, cluster, location, zone, network,
    model, dataset, config,
    class, taxonomy
]).

-record(state, {}).

-record(type_schema, {
    name :: binary() | atom(),
    description :: binary(),
    path_schema :: [atom()],
    parent_type :: atom() | undefined,
    realm :: binary() | undefined  %% nil means global
}).

%%===================================================================
%% API
%%===================================================================

%% @doc Start the registry as a linked process.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the registry with options.
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Check if a type is valid (built-in or registered).
-spec is_valid_type(atom() | binary()) -> boolean().
is_valid_type(Type) when is_atom(Type) ->
    is_builtin_type(Type) orelse is_registered_type(atom_to_binary(Type, utf8));
is_valid_type(TypeBin) when is_binary(TypeBin) ->
    is_builtin_type_bin(TypeBin) orelse is_registered_type(TypeBin);
is_valid_type({custom, TypeBin}) when is_binary(TypeBin) ->
    is_registered_type(TypeBin);
is_valid_type(_) ->
    false.

%% @doc Check if a type is valid for a specific realm.
-spec is_valid_type(atom() | binary(), binary()) -> boolean().
is_valid_type(Type, Realm) when is_atom(Type) ->
    is_builtin_type(Type) orelse is_registered_type_for_realm(atom_to_binary(Type, utf8), Realm);
is_valid_type(TypeBin, Realm) when is_binary(TypeBin) ->
    is_builtin_type_bin(TypeBin) orelse is_registered_type_for_realm(TypeBin, Realm);
is_valid_type({custom, TypeBin}, Realm) when is_binary(TypeBin) ->
    is_registered_type_for_realm(TypeBin, Realm);
is_valid_type(_, _) ->
    false.

%% @doc Register a custom type with schema.
%% Schema map can contain:
%% - description: binary() - Human-readable description
%% - path_schema: [atom()] - Path segment roles (e.g., [org, device_id])
%% - parent_type: atom() - Type of the parent MRI
%% - realm: binary() - Restrict to this realm (undefined = global)
-spec register_type(binary(), map()) -> ok | {error, term()}.
register_type(TypeBin, Schema) when is_binary(TypeBin), is_map(Schema) ->
    case is_builtin_type_bin(TypeBin) of
        true -> {error, cannot_override_builtin};
        false -> gen_server:call(?SERVER, {register_type, TypeBin, Schema})
    end;
register_type(_, _) ->
    {error, invalid_arguments}.

%% @doc Unregister a custom type.
-spec unregister_type(binary()) -> ok | {error, term()}.
unregister_type(TypeBin) when is_binary(TypeBin) ->
    case is_builtin_type_bin(TypeBin) of
        true -> {error, cannot_remove_builtin};
        false -> gen_server:call(?SERVER, {unregister_type, TypeBin})
    end;
unregister_type(_) ->
    {error, invalid_arguments}.

%% @doc Get the schema for a type.
-spec get_type_schema(atom() | binary()) -> {ok, map()} | {error, not_found}.
get_type_schema(Type) when is_atom(Type) ->
    get_type_schema(atom_to_binary(Type, utf8));
get_type_schema(TypeBin) when is_binary(TypeBin) ->
    case is_builtin_type_bin(TypeBin) of
        true -> {ok, builtin_schema(binary_to_atom(TypeBin, utf8))};
        false -> lookup_custom_schema(TypeBin)
    end;
get_type_schema(_) ->
    {error, invalid_type}.

%% @doc List all valid types (built-in + custom).
-spec list_types() -> [atom() | binary()].
list_types() ->
    Builtin = ?BUILTIN_TYPES,
    Custom = list_custom_types(),
    Builtin ++ Custom.

%% @doc List only custom registered types.
-spec list_custom_types() -> [binary()].
list_custom_types() ->
    case ets:info(?TABLE) of
        undefined -> [];
        _ -> [Name || #type_schema{name = Name} <- ets:tab2list(?TABLE)]
    end.

%% @doc Get the path schema for a type.
-spec path_schema(atom() | binary()) -> {ok, [atom()]} | {error, not_found}.
path_schema(Type) ->
    case get_type_schema(Type) of
        {ok, #{path_schema := Schema}} -> {ok, Schema};
        {ok, _} -> {ok, []};
        {error, _} = Err -> Err
    end.

%% @doc Validate that a path matches the expected schema for a type.
-spec validate_path_for_type(atom() | binary(), [binary()]) -> ok | {error, term()}.
validate_path_for_type(Type, Path) ->
    case path_schema(Type) of
        {ok, []} -> ok;  %% No schema means any path is valid
        {ok, Schema} -> validate_path_against_schema(Path, Schema);
        {error, _} = Err -> Err
    end.

%%===================================================================
%% gen_server callbacks
%%===================================================================

init(_Opts) ->
    %% Create ETS table for custom types
    ?TABLE = ets:new(?TABLE, [
        named_table,
        set,
        public,
        {keypos, #type_schema.name},
        {read_concurrency, true}
    ]),
    {ok, #state{}}.

handle_call({register_type, TypeBin, Schema}, _From, State) ->
    TypeSchema = #type_schema{
        name = TypeBin,
        description = maps:get(description, Schema, <<>>),
        path_schema = maps:get(path_schema, Schema, []),
        parent_type = maps:get(parent_type, Schema, undefined),
        realm = maps:get(realm, Schema, undefined)
    },
    true = ets:insert(?TABLE, TypeSchema),
    {reply, ok, State};

handle_call({unregister_type, TypeBin}, _From, State) ->
    true = ets:delete(?TABLE, TypeBin),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%===================================================================
%% Internal Functions
%%===================================================================

is_builtin_type(Type) when is_atom(Type) ->
    lists:member(Type, ?BUILTIN_TYPES).

is_builtin_type_bin(<<"realm">>) -> true;
is_builtin_type_bin(<<"org">>) -> true;
is_builtin_type_bin(<<"user">>) -> true;
is_builtin_type_bin(<<"app">>) -> true;
is_builtin_type_bin(<<"service">>) -> true;
is_builtin_type_bin(<<"artifact">>) -> true;
is_builtin_type_bin(<<"license">>) -> true;
is_builtin_type_bin(<<"cert">>) -> true;
is_builtin_type_bin(<<"key">>) -> true;
is_builtin_type_bin(<<"topic">>) -> true;
is_builtin_type_bin(<<"proc">>) -> true;
is_builtin_type_bin(<<"content">>) -> true;
is_builtin_type_bin(<<"device">>) -> true;
is_builtin_type_bin(<<"cluster">>) -> true;
is_builtin_type_bin(<<"location">>) -> true;
is_builtin_type_bin(<<"zone">>) -> true;
is_builtin_type_bin(<<"network">>) -> true;
is_builtin_type_bin(<<"model">>) -> true;
is_builtin_type_bin(<<"dataset">>) -> true;
is_builtin_type_bin(<<"config">>) -> true;
is_builtin_type_bin(<<"class">>) -> true;
is_builtin_type_bin(<<"taxonomy">>) -> true;
is_builtin_type_bin(_) -> false.

is_registered_type(TypeBin) ->
    case ets:info(?TABLE) of
        undefined -> false;
        _ -> ets:member(?TABLE, TypeBin)
    end.

is_registered_type_for_realm(TypeBin, Realm) ->
    case ets:info(?TABLE) of
        undefined ->
            false;
        _ ->
            case ets:lookup(?TABLE, TypeBin) of
                [#type_schema{realm = undefined}] -> true;  %% Global type
                [#type_schema{realm = Realm}] -> true;      %% Realm-specific match
                _ -> false
            end
    end.

lookup_custom_schema(TypeBin) ->
    case ets:info(?TABLE) of
        undefined ->
            {error, not_found};
        _ ->
            case ets:lookup(?TABLE, TypeBin) of
                [#type_schema{} = Schema] ->
                    {ok, schema_to_map(Schema)};
                [] ->
                    {error, not_found}
            end
    end.

schema_to_map(#type_schema{
    name = Name,
    description = Desc,
    path_schema = PathSchema,
    parent_type = ParentType,
    realm = Realm
}) ->
    #{
        name => Name,
        description => Desc,
        path_schema => PathSchema,
        parent_type => ParentType,
        realm => Realm
    }.

builtin_schema(realm) ->
    #{name => realm, description => <<"Top-level mesh domain">>, path_schema => []};
builtin_schema(org) ->
    #{name => org, description => <<"Organization within realm">>, path_schema => [org]};
builtin_schema(user) ->
    #{name => user, description => <<"User within organization">>, path_schema => [org, user]};
builtin_schema(app) ->
    #{name => app, description => <<"Application">>, path_schema => [org, app]};
builtin_schema(service) ->
    #{name => service, description => <<"Service within application">>, path_schema => [org, app, service]};
builtin_schema(artifact) ->
    #{name => artifact, description => <<"Published artifact">>, path_schema => [org, artifact_id]};
builtin_schema(license) ->
    #{name => license, description => <<"License grant">>, path_schema => [org, app, license_id]};
builtin_schema(cert) ->
    #{name => cert, description => <<"Certificate">>, path_schema => [org, cert_id]};
builtin_schema(key) ->
    #{name => key, description => <<"Signing key">>, path_schema => [org, key_id]};
builtin_schema(topic) ->
    #{name => topic, description => <<"PubSub topic">>, path_schema => [org, topic_path]};
builtin_schema(proc) ->
    #{name => proc, description => <<"RPC procedure">>, path_schema => [org, proc_path]};
builtin_schema(content) ->
    #{name => content, description => <<"Content-addressed blob">>, path_schema => [mcid]};
builtin_schema(device) ->
    #{name => device, description => <<"Physical device">>, path_schema => [org, device_id]};
builtin_schema(cluster) ->
    #{name => cluster, description => <<"Group of nodes">>, path_schema => [org, cluster_name]};
builtin_schema(location) ->
    #{name => location, description => <<"Geographical location">>, path_schema => [org, location_path]};
builtin_schema(zone) ->
    #{name => zone, description => <<"Logical/physical zone">>, path_schema => [org, zone_name]};
builtin_schema(network) ->
    #{name => network, description => <<"Network segment">>, path_schema => [org, network_name]};
builtin_schema(model) ->
    #{name => model, description => <<"AI/ML model">>, path_schema => [org, model_name]};
builtin_schema(dataset) ->
    #{name => dataset, description => <<"Training/inference dataset">>, path_schema => [org, dataset_name]};
builtin_schema(config) ->
    #{name => config, description => <<"Configuration resource">>, path_schema => [org, config_path]};
builtin_schema(class) ->
    #{name => class, description => <<"Classification class">>, path_schema => [class_path]};
builtin_schema(taxonomy) ->
    #{name => taxonomy, description => <<"Taxonomy definition">>, path_schema => [org, taxonomy_name]};
builtin_schema(_) ->
    #{}.

validate_path_against_schema(Path, Schema) when length(Path) =:= length(Schema) ->
    ok;
validate_path_against_schema(Path, Schema) when length(Path) < length(Schema) ->
    {error, {path_too_short, expected, length(Schema), got, length(Path)}};
validate_path_against_schema(Path, Schema) ->
    {error, {path_too_long, expected, length(Schema), got, length(Path)}}.
