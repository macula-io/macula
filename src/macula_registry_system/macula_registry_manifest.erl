%%%-------------------------------------------------------------------
%%% @doc Macula Registry Package Manifest
%%%
%%% Handles parsing and validation of package manifests:
%%% - Manifest structure validation
%%% - Capability specification parsing
%%% - SemVer version validation
%%% - Dependency resolution
%%%
%%% All functions are stateless.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_registry_manifest).

%% API
-export([parse/1, validate/1]).
-export([get_name/1, get_version/1, get_capabilities/1, get_dependencies/1]).
-export([validate_version/1, compare_versions/2]).
-export([capability_matches/2]).
-export([to_binary/1, from_binary/1]).

%% Types
-export_type([manifest/0, capability/0, dependency/0]).

-type manifest() :: #{
    name := binary(),
    version := binary(),
    otp_release => binary(),
    macula_version => binary(),
    capabilities := [capability()],
    dependencies := [dependency()],
    entry_module := atom(),
    supervisor := atom(),
    description => binary(),
    license => binary(),
    authors => [binary()]
}.

-type capability() ::
    {network, [{connect, binary()}]} |
    {pubsub, [{publish | subscribe, binary()}]} |
    {rpc, [{register | call, binary()}]} |
    {nifs, [binary()]} |
    {file_access, [{read | write, binary()}]}.

-type dependency() :: #{
    name := binary(),
    version := binary(),
    registry => binary()
}.

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Parse a manifest from a map (with atom or binary keys)
-spec parse(map()) -> {ok, manifest()} | {error, term()}.
parse(Map) when is_map(Map) ->
    try
        Manifest = #{
            name => get_required(Map, name, <<"name">>),
            version => get_required(Map, version, <<"version">>),
            capabilities => parse_capabilities(get_optional(Map, capabilities, <<"capabilities">>, [])),
            dependencies => parse_dependencies(get_optional(Map, dependencies, <<"dependencies">>, [])),
            entry_module => get_atom(Map, entry_module, <<"entry_module">>, undefined),
            supervisor => get_atom(Map, supervisor, <<"supervisor">>, undefined)
        },
        %% Add optional fields
        Manifest2 = add_optional(Manifest, Map, otp_release, <<"otp_release">>),
        Manifest3 = add_optional(Manifest2, Map, macula_version, <<"macula_version">>),
        Manifest4 = add_optional(Manifest3, Map, description, <<"description">>),
        Manifest5 = add_optional(Manifest4, Map, license, <<"license">>),
        Manifest6 = add_optional(Manifest5, Map, authors, <<"authors">>),
        {ok, Manifest6}
    catch
        throw:{missing_field, Field} ->
            {error, {missing_required_field, Field}};
        throw:{invalid_field, Field, Reason} ->
            {error, {invalid_field, Field, Reason}}
    end;
parse(_) ->
    {error, invalid_manifest_format}.

%% @doc Validate a parsed manifest
-spec validate(manifest()) -> ok | {error, term()}.
validate(Manifest) ->
    Validations = [
        fun validate_name/1,
        fun validate_manifest_version/1,
        fun validate_capabilities/1,
        fun validate_deps/1
    ],
    run_validations(Validations, Manifest).

%% @doc Get package name from manifest
-spec get_name(manifest()) -> binary().
get_name(#{name := Name}) -> Name.

%% @doc Get package version from manifest
-spec get_version(manifest()) -> binary().
get_version(#{version := Version}) -> Version.

%% @doc Get capabilities from manifest
-spec get_capabilities(manifest()) -> [capability()].
get_capabilities(#{capabilities := Caps}) -> Caps.

%% @doc Get dependencies from manifest
-spec get_dependencies(manifest()) -> [dependency()].
get_dependencies(#{dependencies := Deps}) -> Deps.

%% @doc Validate SemVer version string
-spec validate_version(binary()) -> ok | {error, invalid_version}.
validate_version(Version) when is_binary(Version) ->
    case parse_semver(Version) of
        {ok, _, _, _} -> ok;
        error -> {error, invalid_version}
    end;
validate_version(_) ->
    {error, invalid_version}.

%% @doc Compare two SemVer versions
%% Returns: lt (less than), eq (equal), gt (greater than)
-spec compare_versions(binary(), binary()) -> lt | eq | gt | {error, term()}.
compare_versions(V1, V2) ->
    case {parse_semver(V1), parse_semver(V2)} of
        {{ok, Major1, Minor1, Patch1}, {ok, Major2, Minor2, Patch2}} ->
            compare_tuples({Major1, Minor1, Patch1}, {Major2, Minor2, Patch2});
        _ ->
            {error, invalid_version}
    end.

%% @doc Check if a capability pattern matches a specific capability
%% Pattern can include wildcards: * matches any single segment, ** matches any segments
-spec capability_matches(capability(), capability()) -> boolean().
capability_matches({Type, PatternSpecs}, {Type, ActualSpecs}) ->
    lists:all(fun(PatternSpec) ->
        lists:any(fun(ActualSpec) ->
            spec_matches(PatternSpec, ActualSpec)
        end, ActualSpecs)
    end, PatternSpecs);
capability_matches(_, _) ->
    false.

%% @doc Serialize manifest to binary (MessagePack)
-spec to_binary(manifest()) -> binary().
to_binary(Manifest) ->
    %% Convert atoms to binaries for serialization
    SerializableManifest = make_serializable(Manifest),
    msgpack:pack(SerializableManifest, [{map_format, map}]).

%% @doc Deserialize manifest from binary (MessagePack)
-spec from_binary(binary()) -> {ok, manifest()} | {error, term()}.
from_binary(Binary) when is_binary(Binary) ->
    case msgpack:unpack(Binary, [{map_format, map}]) of
        {ok, Map} -> parse(Map);
        {error, Reason} -> {error, {decode_error, Reason}}
    end;
from_binary(_) ->
    {error, invalid_format}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Get required field from map with atom or binary key
get_required(Map, AtomKey, BinKey) ->
    case maps:get(AtomKey, Map, undefined) of
        undefined ->
            case maps:get(BinKey, Map, undefined) of
                undefined -> throw({missing_field, AtomKey});
                Value -> Value
            end;
        Value -> Value
    end.

%% @private Get optional field from map with atom or binary key
get_optional(Map, AtomKey, BinKey, Default) ->
    case maps:get(AtomKey, Map, undefined) of
        undefined -> maps:get(BinKey, Map, Default);
        Value -> Value
    end.

%% @private Get atom from map, converting binary if needed
get_atom(Map, AtomKey, BinKey, Default) ->
    case get_optional(Map, AtomKey, BinKey, Default) of
        undefined -> undefined;
        Value when is_atom(Value) -> Value;
        Value when is_binary(Value) -> binary_to_atom(Value, utf8);
        _ -> throw({invalid_field, AtomKey, expected_atom})
    end.

%% @private Add optional field if present
add_optional(Manifest, Map, AtomKey, BinKey) ->
    case get_optional(Map, AtomKey, BinKey, undefined) of
        undefined -> Manifest;
        Value -> maps:put(AtomKey, Value, Manifest)
    end.

%% @private Parse capability list
parse_capabilities(Caps) when is_list(Caps) ->
    lists:map(fun parse_capability/1, Caps);
parse_capabilities(_) ->
    [].

%% @private Parse single capability
parse_capability({Type, Specs}) when is_atom(Type), is_list(Specs) ->
    {Type, Specs};
parse_capability(#{<<"type">> := TypeBin} = Map) ->
    Type = binary_to_atom(TypeBin, utf8),
    Specs = maps:get(<<"specs">>, Map, []),
    {Type, parse_cap_specs(Type, Specs)};
parse_capability(Map) when is_map(Map) ->
    %% Try atom keys
    case maps:get(type, Map, undefined) of
        undefined -> throw({invalid_field, capability, missing_type});
        Type ->
            Specs = maps:get(specs, Map, []),
            {Type, parse_cap_specs(Type, Specs)}
    end;
parse_capability(_) ->
    throw({invalid_field, capability, invalid_format}).

%% @private Parse capability specs based on type
parse_cap_specs(network, Specs) -> parse_network_specs(Specs);
parse_cap_specs(pubsub, Specs) -> parse_pubsub_specs(Specs);
parse_cap_specs(rpc, Specs) -> parse_rpc_specs(Specs);
parse_cap_specs(nifs, Specs) when is_list(Specs) -> Specs;
parse_cap_specs(file_access, Specs) -> parse_file_specs(Specs);
parse_cap_specs(_, Specs) -> Specs.

parse_network_specs(Specs) ->
    lists:map(fun
        ({connect, Pattern}) -> {connect, Pattern};
        (#{<<"connect">> := Pattern}) -> {connect, Pattern};
        (#{connect := Pattern}) -> {connect, Pattern}
    end, Specs).

parse_pubsub_specs(Specs) ->
    lists:map(fun
        ({Op, Pattern}) when Op =:= publish; Op =:= subscribe -> {Op, Pattern};
        (#{<<"publish">> := Pattern}) -> {publish, Pattern};
        (#{<<"subscribe">> := Pattern}) -> {subscribe, Pattern};
        (#{publish := Pattern}) -> {publish, Pattern};
        (#{subscribe := Pattern}) -> {subscribe, Pattern}
    end, Specs).

parse_rpc_specs(Specs) ->
    lists:map(fun
        ({Op, Pattern}) when Op =:= register; Op =:= call -> {Op, Pattern};
        (#{<<"register">> := Pattern}) -> {register, Pattern};
        (#{<<"call">> := Pattern}) -> {call, Pattern};
        (#{register := Pattern}) -> {register, Pattern};
        (#{call := Pattern}) -> {call, Pattern}
    end, Specs).

parse_file_specs(Specs) ->
    lists:map(fun
        ({Op, Pattern}) when Op =:= read; Op =:= write -> {Op, Pattern};
        (#{<<"read">> := Pattern}) -> {read, Pattern};
        (#{<<"write">> := Pattern}) -> {write, Pattern};
        (#{read := Pattern}) -> {read, Pattern};
        (#{write := Pattern}) -> {write, Pattern}
    end, Specs).

%% @private Parse dependency list
parse_dependencies(Deps) when is_list(Deps) ->
    lists:map(fun parse_dependency/1, Deps);
parse_dependencies(_) ->
    [].

%% @private Parse single dependency
parse_dependency(#{name := Name, version := Version} = Dep) ->
    #{name => Name, version => Version, registry => maps:get(registry, Dep, undefined)};
parse_dependency(#{<<"name">> := Name, <<"version">> := Version} = Dep) ->
    #{name => Name, version => Version, registry => maps:get(<<"registry">>, Dep, undefined)};
parse_dependency(_) ->
    throw({invalid_field, dependency, invalid_format}).

%% @private Validate package name
validate_name(#{name := Name}) when is_binary(Name), byte_size(Name) > 0 ->
    %% Name should be lowercase alphanumeric with underscores
    case re:run(Name, "^[a-z][a-z0-9_]*$", [{capture, none}]) of
        match -> ok;
        nomatch -> {error, {invalid_name, Name}}
    end;
validate_name(_) ->
    {error, {invalid_name, missing}}.

%% @private Validate manifest version field
validate_manifest_version(#{version := Version}) ->
    validate_version(Version);
validate_manifest_version(_) ->
    {error, {invalid_version, missing}}.

%% @private Validate capabilities format
validate_capabilities(#{capabilities := Caps}) when is_list(Caps) ->
    case lists:all(fun is_valid_capability/1, Caps) of
        true -> ok;
        false -> {error, invalid_capabilities}
    end;
validate_capabilities(_) ->
    ok.

%% @private Check if capability is valid
is_valid_capability({Type, Specs}) when is_atom(Type), is_list(Specs) -> true;
is_valid_capability(_) -> false.

%% @private Validate dependencies
validate_deps(#{dependencies := Deps}) when is_list(Deps) ->
    case lists:all(fun is_valid_dependency/1, Deps) of
        true -> ok;
        false -> {error, invalid_dependencies}
    end;
validate_deps(_) ->
    ok.

%% @private Check if dependency is valid
is_valid_dependency(#{name := N, version := V}) when is_binary(N), is_binary(V) -> true;
is_valid_dependency(_) -> false.

%% @private Run all validations
run_validations([], _Manifest) -> ok;
run_validations([Fun | Rest], Manifest) ->
    case Fun(Manifest) of
        ok -> run_validations(Rest, Manifest);
        {error, _} = Error -> Error
    end.

%% @private Parse SemVer version string
parse_semver(Version) when is_binary(Version) ->
    case binary:split(Version, <<".">>, [global]) of
        [MajorBin, MinorBin, PatchBin] ->
            parse_semver_parts(MajorBin, MinorBin, PatchBin);
        _ -> error
    end;
parse_semver(_) ->
    error.

%% @private Parse semver major.minor.patch parts
parse_semver_parts(MajorBin, MinorBin, PatchBin) ->
    %% Handle pre-release suffix (e.g., "1.0.0-alpha")
    PatchStr = case binary:split(PatchBin, <<"-">>) of
        [P | _] -> P;
        _ -> PatchBin
    end,
    parse_semver_integers(
        catch binary_to_integer(MajorBin),
        catch binary_to_integer(MinorBin),
        catch binary_to_integer(PatchStr)
    ).

%% @private Validate parsed integers
parse_semver_integers(Major, Minor, Patch)
  when is_integer(Major), is_integer(Minor), is_integer(Patch) ->
    {ok, Major, Minor, Patch};
parse_semver_integers(_, _, _) ->
    error.

%% @private Compare version tuples
compare_tuples({M1, _, _}, {M2, _, _}) when M1 < M2 -> lt;
compare_tuples({M1, _, _}, {M2, _, _}) when M1 > M2 -> gt;
compare_tuples({_, N1, _}, {_, N2, _}) when N1 < N2 -> lt;
compare_tuples({_, N1, _}, {_, N2, _}) when N1 > N2 -> gt;
compare_tuples({_, _, P1}, {_, _, P2}) when P1 < P2 -> lt;
compare_tuples({_, _, P1}, {_, _, P2}) when P1 > P2 -> gt;
compare_tuples(_, _) -> eq.

%% @private Check if spec matches pattern
spec_matches({Op, Pattern}, {Op, Actual}) ->
    pattern_matches(Pattern, Actual);
spec_matches(_, _) ->
    false.

%% @private Match pattern with wildcards
pattern_matches(<<"**">>, _) -> true;
pattern_matches(<<"*">>, _) -> true;
pattern_matches(Pattern, Actual) when Pattern =:= Actual -> true;
pattern_matches(Pattern, Actual) ->
    %% Handle patterns like "api.*" or "*.example.com"
    PatternParts = binary:split(Pattern, <<".">>, [global]),
    ActualParts = binary:split(Actual, <<".">>, [global]),
    match_parts(PatternParts, ActualParts).

%% @private Match pattern parts
match_parts([], []) -> true;
match_parts([<<"**">> | _], _) -> true;
match_parts([<<"*">> | PRest], [_ | ARest]) -> match_parts(PRest, ARest);
match_parts([P | PRest], [A | ARest]) when P =:= A -> match_parts(PRest, ARest);
match_parts(_, _) -> false.

%% @private Make manifest serializable (atoms to binaries)
make_serializable(Manifest) ->
    maps:fold(fun
        (entry_module, V, Acc) when is_atom(V) ->
            maps:put(<<"entry_module">>, atom_to_binary(V, utf8), Acc);
        (supervisor, V, Acc) when is_atom(V) ->
            maps:put(<<"supervisor">>, atom_to_binary(V, utf8), Acc);
        (capabilities, Caps, Acc) ->
            maps:put(<<"capabilities">>, serialize_capabilities(Caps), Acc);
        (K, V, Acc) when is_atom(K) ->
            maps:put(atom_to_binary(K, utf8), V, Acc);
        (K, V, Acc) ->
            maps:put(K, V, Acc)
    end, #{}, Manifest).

%% @private Serialize capabilities for MessagePack
serialize_capabilities(Caps) ->
    lists:map(fun({Type, Specs}) ->
        #{<<"type">> => atom_to_binary(Type, utf8),
          <<"specs">> => serialize_specs(Specs)}
    end, Caps).

serialize_specs(Specs) ->
    lists:map(fun
        ({Op, Pattern}) -> #{atom_to_binary(Op, utf8) => Pattern};
        (Other) -> Other
    end, Specs).
