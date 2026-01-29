%%%-------------------------------------------------------------------
%%% @doc Macula Resource Identifier (MRI) - Core Module
%%%
%%% Provides parsing, validation, formatting, and manipulation of MRIs.
%%% MRI format: `mri:{type}:{realm}/{path}'
%%%
%%% Example:
%%%   {ok, Parsed} = macula_mri:parse(MRI),
%%%   app = macula_mri:type(Parsed),
%%%   Realm = macula_mri:realm(Parsed),
%%%   Path = macula_mri:path(Parsed).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri).

%% Parsing and Formatting
-export([parse/1, format/1, validate/1, is_valid/1]).

%% Accessors
-export([type/1, realm/1, path/1, path_string/1]).

%% Hierarchy
-export([parent/1, parent_type/1, ancestors/1, is_ancestor/2, depth/1]).

%% Construction
-export([new/1, new/3]).
-export([new_realm/1, new_org/2, new_user/3, new_app/3, new_service/4]).
-export([new_instance/4, new_instance_qualified/5]).

%% Path manipulation
-export([append_segment/2, join_path/1, split_path/1]).

%% DHT topic/procedure derivation (for instances)
-export([derive_topic/2, derive_procedure/2, to_topic_prefix/1]).

%% Types
-export_type([mri/0, mri_map/0, mri_type/0, realm/0, path_segment/0]).

-type mri() :: binary().
-type mri_map() :: #{
    type := mri_type(),
    realm := realm(),
    path := [path_segment()]
}.
-type mri_type() :: atom() | {custom, binary()}.
-type realm() :: binary().
-type path_segment() :: binary().

-define(MRI_PREFIX, <<"mri:">>).
-define(MAX_PATH_DEPTH, 10).
-define(MAX_SEGMENT_LENGTH, 63).

%%===================================================================
%% Parsing and Formatting
%%===================================================================

%% @doc Parse an MRI binary into a map.
-spec parse(mri()) -> {ok, mri_map()} | {error, term()}.
parse(<<>>) ->
    {error, empty_mri};
parse(MRI) when is_binary(MRI) ->
    case binary:split(MRI, <<":">>, [global]) of
        [<<"mri">>, TypeBin, RealmAndPath] ->
            parse_type_and_path(TypeBin, RealmAndPath);
        _ ->
            {error, invalid_format}
    end;
parse(_) ->
    {error, not_binary}.

%% @doc Format an MRI map back to binary.
-spec format(mri_map()) -> mri().
format(#{type := Type, realm := Realm, path := []}) ->
    TypeBin = type_to_binary(Type),
    <<"mri:", TypeBin/binary, ":", Realm/binary>>;
format(#{type := Type, realm := Realm, path := Path}) ->
    TypeBin = type_to_binary(Type),
    PathBin = join_path(Path),
    <<"mri:", TypeBin/binary, ":", Realm/binary, "/", PathBin/binary>>.

%% @doc Validate an MRI, returning ok or error with reason.
-spec validate(mri() | mri_map()) -> ok | {error, term()}.
validate(MRI) when is_binary(MRI) ->
    case parse(MRI) of
        {ok, Parsed} -> validate_parsed(Parsed);
        {error, _} = Err -> Err
    end;
validate(#{type := _, realm := _, path := _} = Parsed) ->
    validate_parsed(Parsed);
validate(_) ->
    {error, invalid_input}.

%% @doc Check if an MRI is valid.
-spec is_valid(mri() | mri_map()) -> boolean().
is_valid(MRI) ->
    validate(MRI) =:= ok.

%%===================================================================
%% Accessors
%%===================================================================

%% @doc Get the type from an MRI (binary or parsed).
-spec type(mri() | mri_map()) -> mri_type().
type(#{type := Type}) ->
    Type;
type(MRI) when is_binary(MRI) ->
    {ok, #{type := Type}} = parse(MRI),
    Type.

%% @doc Get the realm from an MRI.
-spec realm(mri() | mri_map()) -> realm().
realm(#{realm := Realm}) ->
    Realm;
realm(MRI) when is_binary(MRI) ->
    {ok, #{realm := Realm}} = parse(MRI),
    Realm.

%% @doc Get the path segments from an MRI.
-spec path(mri() | mri_map()) -> [path_segment()].
path(#{path := Path}) ->
    Path;
path(MRI) when is_binary(MRI) ->
    {ok, #{path := Path}} = parse(MRI),
    Path.

%% @doc Get the path as a single joined string.
-spec path_string(mri() | mri_map()) -> binary().
path_string(MRI) ->
    join_path(path(MRI)).

%%===================================================================
%% Hierarchy
%%===================================================================

%% @doc Get the parent MRI, or undefined for realm-level MRIs.
-spec parent(mri() | mri_map()) -> mri() | undefined.
parent(MRI) when is_binary(MRI) ->
    {ok, Parsed} = parse(MRI),
    parent(Parsed);
parent(#{type := realm}) ->
    undefined;
parent(#{type := Type, realm := Realm, path := []}) ->
    %% No path segments - parent is realm
    case parent_type(Type) of
        undefined -> undefined;
        ParentType -> format(#{type => ParentType, realm => Realm, path => []})
    end;
parent(#{type := Type, realm := Realm, path := Path}) ->
    case drop_last(Path) of
        [] ->
            %% Path becomes empty - check if parent type exists
            case parent_type(Type) of
                undefined -> format(#{type => realm, realm => Realm, path => []});
                ParentType -> format(#{type => ParentType, realm => Realm, path => []})
            end;
        ShorterPath ->
            case parent_type(Type) of
                undefined -> format(#{type => Type, realm => Realm, path => ShorterPath});
                ParentType -> format(#{type => ParentType, realm => Realm, path => ShorterPath})
            end
    end.

%% @doc Get the parent type for a given type.
-spec parent_type(mri_type()) -> mri_type() | undefined.
parent_type(realm) -> undefined;
parent_type(org) -> realm;
parent_type(user) -> org;
parent_type(app) -> org;
parent_type(service) -> app;
parent_type(artifact) -> org;
parent_type(license) -> app;
parent_type(cert) -> org;
parent_type(key) -> org;
parent_type(topic) -> org;
parent_type(proc) -> org;
parent_type(content) -> org;
parent_type(device) -> org;
parent_type(cluster) -> org;
parent_type(location) -> org;
parent_type(zone) -> org;
parent_type(network) -> org;
parent_type(model) -> org;
parent_type(dataset) -> org;
parent_type(config) -> org;
parent_type(class) -> undefined;  %% Classes have their own hierarchy
parent_type(taxonomy) -> org;
parent_type({custom, _}) -> undefined;
parent_type(_) -> undefined.

%% @doc Get all ancestors of an MRI, from immediate parent to root.
-spec ancestors(mri() | mri_map()) -> [mri()].
ancestors(MRI) ->
    ancestors(MRI, []).

ancestors(MRI, Acc) ->
    case parent(MRI) of
        undefined -> lists:reverse(Acc);
        Parent -> ancestors(Parent, [Parent | Acc])
    end.

%% @doc Check if PotentialAncestor is an ancestor of MRI.
-spec is_ancestor(mri(), mri()) -> boolean().
is_ancestor(PotentialAncestor, MRI) when is_binary(PotentialAncestor), is_binary(MRI) ->
    lists:member(PotentialAncestor, ancestors(MRI)).

%% @doc Get the depth of an MRI (0 for realm, 1 for org, etc).
-spec depth(mri() | mri_map()) -> non_neg_integer().
depth(#{path := Path}) ->
    length(Path);
depth(MRI) when is_binary(MRI) ->
    {ok, Parsed} = parse(MRI),
    depth(Parsed).

%%===================================================================
%% Construction
%%===================================================================

%% @doc Create a new MRI from a map.
-spec new(map()) -> {ok, mri()} | {error, term()}.
new(#{type := Type, realm := Realm} = Opts) ->
    Path = maps:get(path, Opts, []),
    MRI = format(#{type => Type, realm => Realm, path => Path}),
    case validate(MRI) of
        ok -> {ok, MRI};
        {error, _} = Err -> Err
    end.

%% @doc Create a new MRI from type, realm, and path.
-spec new(mri_type(), realm(), [path_segment()]) -> {ok, mri()} | {error, term()}.
new(Type, Realm, Path) ->
    new(#{type => Type, realm => Realm, path => Path}).

%% @doc Create a realm MRI.
-spec new_realm(realm()) -> mri().
new_realm(Realm) ->
    format(#{type => realm, realm => Realm, path => []}).

%% @doc Create an org MRI.
-spec new_org(realm(), binary()) -> mri().
new_org(Realm, Org) ->
    format(#{type => org, realm => Realm, path => [Org]}).

%% @doc Create a user MRI.
-spec new_user(realm(), binary(), binary()) -> mri().
new_user(Realm, Org, User) ->
    format(#{type => user, realm => Realm, path => [Org, User]}).

%% @doc Create an app MRI.
-spec new_app(realm(), binary(), binary()) -> mri().
new_app(Realm, Org, App) ->
    format(#{type => app, realm => Realm, path => [Org, App]}).

%% @doc Create a service MRI.
-spec new_service(realm(), binary(), binary(), binary()) -> mri().
new_service(Realm, Org, App, Service) ->
    format(#{type => service, realm => Realm, path => [Org, App, Service]}).

%% @doc Create an instance MRI (running installation of an artifact).
%% Instance path: {org}/{device}/{instance_name}
%% Example: mri:instance:io.macula/acme/edge-01/counter
-spec new_instance(realm(), binary(), binary(), binary()) -> mri().
new_instance(Realm, Org, Device, InstanceName) ->
    format(#{type => instance, realm => Realm, path => [Org, Device, InstanceName]}).

%% @doc Create an instance MRI with qualifier (environment or replica index).
%% Example: mri:instance:io.macula/acme/edge-01/counter.prod
-spec new_instance_qualified(realm(), binary(), binary(), binary(), binary()) -> mri().
new_instance_qualified(Realm, Org, Device, ArtifactName, Qualifier) ->
    InstanceName = <<ArtifactName/binary, ".", Qualifier/binary>>,
    format(#{type => instance, realm => Realm, path => [Org, Device, InstanceName]}).

%%===================================================================
%% Path Manipulation
%%===================================================================

%% @doc Append a segment to an MRI's path.
-spec append_segment(mri() | mri_map(), path_segment()) -> mri().
append_segment(#{type := Type, realm := Realm, path := Path}, Segment) ->
    format(#{type => Type, realm => Realm, path => Path ++ [Segment]});
append_segment(MRI, Segment) when is_binary(MRI) ->
    {ok, Parsed} = parse(MRI),
    append_segment(Parsed, Segment).

%% @doc Join path segments with slashes.
-spec join_path([path_segment()]) -> binary().
join_path([]) ->
    <<>>;
join_path([Segment]) ->
    Segment;
join_path(Segments) ->
    lists:foldl(
        fun(Seg, <<>>) -> Seg;
           (Seg, Acc) -> <<Acc/binary, "/", Seg/binary>>
        end,
        <<>>,
        Segments
    ).

%% @doc Split a path string into segments.
-spec split_path(binary()) -> [path_segment()].
split_path(<<>>) ->
    [];
split_path(PathBin) ->
    binary:split(PathBin, <<"/">>, [global]).

%%===================================================================
%% DHT Topic/Procedure Derivation (for instances)
%%===================================================================

%% @doc Derive a DHT topic from an instance MRI and a declared topic name.
%%
%% Takes an instance MRI and a declared topic name, returns the full DHT topic.
%% The instance path becomes a prefix: realm/org/device/instance.declared_topic
%%
%% Example: InstanceMRI = "mri:instance:io.macula/acme/edge-01/counter.prod"
%%          DeclaredTopic = "orders.created"
%%          Result = "io.macula/acme/edge-01/counter.prod.orders.created"
%% @end
-spec derive_topic(mri(), binary()) -> binary().
derive_topic(InstanceMRI, DeclaredTopic) ->
    InstancePath = to_topic_prefix(InstanceMRI),
    <<InstancePath/binary, ".", DeclaredTopic/binary>>.

%% @doc Derive a DHT procedure from an instance MRI and a declared procedure name.
%%
%% Takes an instance MRI and a declared procedure name, returns the full DHT procedure.
%% The instance path becomes a prefix: realm/org/device/instance.declared_procedure
%%
%% Example: InstanceMRI = "mri:instance:io.macula/acme/edge-01/counter.prod"
%%          DeclaredProcedure = "place_order"
%%          Result = "io.macula/acme/edge-01/counter.prod.place_order"
%% @end
-spec derive_procedure(mri(), binary()) -> binary().
derive_procedure(InstanceMRI, DeclaredProcedure) ->
    InstancePath = to_topic_prefix(InstanceMRI),
    <<InstancePath/binary, ".", DeclaredProcedure/binary>>.

%% @doc Convert an MRI to a topic/procedure prefix (realm/path segments).
%%
%% Takes any MRI and extracts the realm and path as a topic-compatible prefix.
%% Format: realm/path_segment1/path_segment2/...
%%
%% Example: MRI = "mri:instance:io.macula/acme/edge-01/counter.prod"
%%          Result = "io.macula/acme/edge-01/counter.prod"
%% @end
-spec to_topic_prefix(mri()) -> binary().
to_topic_prefix(MRI) ->
    {ok, #{realm := Realm, path := Path}} = parse(MRI),
    PathBin = join_path(Path),
    <<Realm/binary, "/", PathBin/binary>>.

%%===================================================================
%% Internal Functions
%%===================================================================

parse_type_and_path(TypeBin, RealmAndPath) ->
    case binary:split(RealmAndPath, <<"/">>) of
        [Realm] ->
            %% No path (e.g., mri:realm:io.macula)
            parse_with_type(TypeBin, Realm, []);
        [Realm, PathBin] ->
            Path = split_path(PathBin),
            parse_with_type(TypeBin, Realm, Path)
    end.

parse_with_type(TypeBin, Realm, Path) ->
    case binary_to_type(TypeBin) of
        {ok, Type} ->
            {ok, #{type => Type, realm => Realm, path => Path}};
        {error, _} = Err ->
            Err
    end.

binary_to_type(<<"realm">>) -> {ok, realm};
binary_to_type(<<"org">>) -> {ok, org};
binary_to_type(<<"user">>) -> {ok, user};
binary_to_type(<<"app">>) -> {ok, app};
binary_to_type(<<"service">>) -> {ok, service};
binary_to_type(<<"artifact">>) -> {ok, artifact};
binary_to_type(<<"instance">>) -> {ok, instance};
binary_to_type(<<"license">>) -> {ok, license};
binary_to_type(<<"cert">>) -> {ok, cert};
binary_to_type(<<"key">>) -> {ok, key};
binary_to_type(<<"topic">>) -> {ok, topic};
binary_to_type(<<"proc">>) -> {ok, proc};
binary_to_type(<<"content">>) -> {ok, content};
binary_to_type(<<"device">>) -> {ok, device};
binary_to_type(<<"cluster">>) -> {ok, cluster};
binary_to_type(<<"location">>) -> {ok, location};
binary_to_type(<<"zone">>) -> {ok, zone};
binary_to_type(<<"network">>) -> {ok, network};
binary_to_type(<<"model">>) -> {ok, model};
binary_to_type(<<"dataset">>) -> {ok, dataset};
binary_to_type(<<"config">>) -> {ok, config};
binary_to_type(<<"class">>) -> {ok, class};
binary_to_type(<<"taxonomy">>) -> {ok, taxonomy};
binary_to_type(TypeBin) ->
    %% Check if it's a registered custom type
    case macula_mri_registry:is_valid_type(TypeBin) of
        true -> {ok, {custom, TypeBin}};
        false -> {error, {unknown_type, TypeBin}}
    end.

type_to_binary(realm) -> <<"realm">>;
type_to_binary(org) -> <<"org">>;
type_to_binary(user) -> <<"user">>;
type_to_binary(app) -> <<"app">>;
type_to_binary(service) -> <<"service">>;
type_to_binary(artifact) -> <<"artifact">>;
type_to_binary(instance) -> <<"instance">>;
type_to_binary(license) -> <<"license">>;
type_to_binary(cert) -> <<"cert">>;
type_to_binary(key) -> <<"key">>;
type_to_binary(topic) -> <<"topic">>;
type_to_binary(proc) -> <<"proc">>;
type_to_binary(content) -> <<"content">>;
type_to_binary(device) -> <<"device">>;
type_to_binary(cluster) -> <<"cluster">>;
type_to_binary(location) -> <<"location">>;
type_to_binary(zone) -> <<"zone">>;
type_to_binary(network) -> <<"network">>;
type_to_binary(model) -> <<"model">>;
type_to_binary(dataset) -> <<"dataset">>;
type_to_binary(config) -> <<"config">>;
type_to_binary(class) -> <<"class">>;
type_to_binary(taxonomy) -> <<"taxonomy">>;
type_to_binary({custom, Bin}) -> Bin;
type_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).

validate_parsed(#{type := Type, realm := Realm, path := Path}) ->
    validate_type(Type, Realm, Path).

validate_type(Type, Realm, Path) ->
    case macula_mri_registry:is_valid_type(Type) of
        true -> validate_realm(Realm, Path);
        false -> {error, {invalid_type, Type}}
    end.

validate_realm(Realm, Path) when is_binary(Realm), byte_size(Realm) > 0 ->
    validate_realm_format(Realm, Path);
validate_realm(_, _) ->
    {error, empty_realm}.

validate_realm_format(Realm, Path) ->
    %% Realm should be reverse-domain notation (e.g., io.macula)
    case is_valid_realm_format(Realm) of
        true -> validate_path(Path);
        false -> {error, {invalid_realm_format, Realm}}
    end.

is_valid_realm_format(Realm) ->
    %% Simple validation: contains at least one dot, alphanumeric + dots
    case binary:match(Realm, <<".">>) of
        nomatch -> false;
        _ ->
            %% Check all characters are valid
            lists:all(
                fun(C) ->
                    (C >= $a andalso C =< $z) orelse
                    (C >= $0 andalso C =< $9) orelse
                    C =:= $.
                end,
                binary_to_list(Realm)
            )
    end.

validate_path(Path) when length(Path) > ?MAX_PATH_DEPTH ->
    {error, {path_too_deep, length(Path)}};
validate_path(Path) ->
    validate_path_segments(Path).

validate_path_segments([]) ->
    ok;
validate_path_segments([Segment | Rest]) ->
    case validate_segment(Segment) of
        ok -> validate_path_segments(Rest);
        {error, _} = Err -> Err
    end.

validate_segment(Segment) when byte_size(Segment) > ?MAX_SEGMENT_LENGTH ->
    {error, {segment_too_long, Segment}};
validate_segment(Segment) when byte_size(Segment) =:= 0 ->
    {error, empty_segment};
validate_segment(Segment) ->
    %% Lowercase alphanumeric, hyphen, underscore
    %% No leading/trailing hyphen
    case is_valid_segment_format(Segment) of
        true -> ok;
        false -> {error, {invalid_segment_format, Segment}}
    end.

is_valid_segment_format(<<$-, _/binary>>) ->
    false;  %% No leading hyphen
is_valid_segment_format(Segment) ->
    case binary:last(Segment) of
        $- -> false;  %% No trailing hyphen
        _ -> validate_segment_chars(Segment)
    end.

validate_segment_chars(Segment) ->
    lists:all(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse
            (C >= $0 andalso C =< $9) orelse
            C =:= $- orelse
            C =:= $_
        end,
        binary_to_list(Segment)
    ).

drop_last([]) -> [];
drop_last([_]) -> [];
drop_last(List) -> lists:droplast(List).
