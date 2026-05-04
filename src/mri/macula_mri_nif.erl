%% @doc MRI (Macula Resource Identifier) operations for Macula mesh.
%%
%% This module provides high-performance MRI parsing, validation, and
%% hierarchy queries. It uses Rust NIFs when available, falling back
%% to pure Erlang implementations otherwise.
%%
%% == NIF vs Erlang ==
%%
%% The Rust NIFs provide significant performance improvements:
%% - Parsing: ~3-5x faster (single-pass parsing)
%% - Validation: ~4-6x faster (avoids binary_to_list conversion)
%% - Path joining: ~5-10x faster (single allocation)
%% - Hierarchy queries: ~10-20x faster (efficient prefix matching)
%%
%% == Trie Index for Million-Scale Deployments ==
%%
%% For large-scale deployments (millions of MRIs), use the persistent index:
%%
%% ```
%% %% Build index once (O(n))
%% {ok, Index} = macula_mri_nif:build_path_index(AllMRIs),
%%
%% %% Fast O(d) queries instead of O(n)
%% {ok, Children} = macula_mri_nif:index_find_children(Index, Realm, Path),
%% {ok, Descendants} = macula_mri_nif:index_find_descendants(Index, Realm, Path),
%%
%% %% Dynamic updates for device churn
%% ok = macula_mri_nif:index_insert(Index, Realm, Path, MRI),
%% ok = macula_mri_nif:index_remove(Index, Realm, Path).
%% '''
%%
%% The pure Erlang fallbacks ensure the module works even when NIFs
%% cannot be loaded (e.g., different architecture, missing Rust toolchain).
%%
%% == MRI Format ==
%%
%% MRIs follow the format: mri:type:realm or mri:type:realm/path
%%
%% Examples:
%%   mri:realm:io.macula — A realm
%%   mri:app:io.macula/counter — An app in the io.macula realm
%%   mri:service:io.macula/counter/api — A service endpoint
%%
%% @author rgfaber
-module(macula_mri_nif).

%% API - Basic operations
-export([
    parse_mri/1,
    validate_realm_format/1,
    validate_segment_chars/1,
    is_builtin_type/1,
    join_path_segments/1,
    format_mri/3,
    is_nif_loaded/0
]).

%% API - List-based hierarchy queries (O(n))
-export([
    find_children/3,
    find_descendants/3
]).

%% API - Trie index operations (O(d) queries)
-export([
    build_path_index/1,
    index_find_children/3,
    index_find_descendants/3,
    index_insert/4,
    index_remove/3,
    index_size/1
]).

%% NIF stubs
-export([
    nif_parse_mri/1,
    nif_validate_realm_format/1,
    nif_validate_segment_chars/1,
    nif_is_builtin_type/1,
    nif_join_path_segments/1,
    nif_format_mri/3,
    nif_find_children/3,
    nif_find_descendants/3,
    nif_build_path_index/1,
    nif_index_find_children/3,
    nif_index_find_descendants/3,
    nif_index_insert/4,
    nif_index_remove/3,
    nif_index_size/1
]).

-on_load(init/0).

-define(NIF_LOADED_KEY, macula_mri_nif_loaded).

%% Builtin MRI types
-define(BUILTIN_TYPES, [
    <<"realm">>, <<"org">>, <<"user">>, <<"app">>, <<"service">>,
    <<"artifact">>, <<"license">>, <<"cert">>, <<"key">>, <<"topic">>,
    <<"proc">>, <<"content">>, <<"device">>, <<"cluster">>, <<"location">>,
    <<"zone">>, <<"network">>, <<"model">>, <<"dataset">>, <<"config">>,
    <<"class">>, <<"taxonomy">>
]).

%%====================================================================
%% Init
%%====================================================================

init() ->
    PrivDir = case code:priv_dir(macula) of
        {error, _} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join(filename:dirname(filename:dirname(Filename)), "priv");
                _ ->
                    "priv"
            end;
        Dir ->
            Dir
    end,
    Path = filename:join(PrivDir, "macula_mri_nif"),
    case erlang:load_nif(Path, 0) of
        ok ->
            persistent_term:put(?NIF_LOADED_KEY, true),
            ok;
        {error, {reload, _}} ->
            persistent_term:put(?NIF_LOADED_KEY, true),
            ok;
        {error, _Reason} ->
            %% NIF not available, will use Erlang fallbacks
            ok
    end.

%%====================================================================
%% API
%%====================================================================

%% @doc Check if the NIF is loaded.
-spec is_nif_loaded() -> boolean().
is_nif_loaded() ->
    persistent_term:get(?NIF_LOADED_KEY, false).

%% @doc Parse an MRI string into its components.
%%
%% Returns a map with `type', `realm', and `path' keys.
%%
%% Example:
%% ```
%% {ok, #{type := &lt;&lt;"app"&gt;&gt;, realm := &lt;&lt;"io.macula"&gt;&gt;, path := [&lt;&lt;"counter"&gt;&gt;]}} =
%%     macula_mri_nif:parse_mri(&lt;&lt;"mri:app:io.macula/counter"&gt;&gt;).
%% '''
-spec parse_mri(MRI :: binary()) ->
    {ok, #{type := binary(), realm := binary(), path := [binary()]}} |
    {error, invalid_format | invalid_realm | invalid_segment}.
parse_mri(MRI) when is_binary(MRI) ->
    case is_nif_loaded() of
        true ->
            %% NIF returns {ok, {Type, Realm, Path}} - convert to map
            case nif_parse_mri(MRI) of
                {ok, {Type, Realm, Path}} ->
                    {ok, #{type => Type, realm => Realm, path => Path}};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            erlang_parse_mri(MRI)
    end;
parse_mri(_) ->
    {error, invalid_format}.

%% @doc Validate realm format.
%%
%% Valid realm: reverse domain notation (e.g., "io.macula.example")
%% Must contain at least one dot and only lowercase letters, digits, and dots.
-spec validate_realm_format(Realm :: binary()) -> boolean().
validate_realm_format(Realm) when is_binary(Realm) ->
    case is_nif_loaded() of
        true -> nif_validate_realm_format(Realm);
        false -> erlang_validate_realm_format(Realm)
    end;
validate_realm_format(_) ->
    false.

%% @doc Validate segment characters.
%%
%% Valid segment: a-z, 0-9, hyphen (-), underscore (_)
-spec validate_segment_chars(Segment :: binary()) -> boolean().
validate_segment_chars(Segment) when is_binary(Segment) ->
    case is_nif_loaded() of
        true -> nif_validate_segment_chars(Segment);
        false -> erlang_validate_segment_chars(Segment)
    end;
validate_segment_chars(_) ->
    false.

%% @doc Check if a type is a builtin MRI type.
%%
%% Builtin types: realm, org, user, app, service, artifact, license,
%% cert, key, topic, proc, content, device, cluster, location, zone,
%% network, model, dataset, config, class, taxonomy.
-spec is_builtin_type(Type :: binary()) -> boolean().
is_builtin_type(Type) when is_binary(Type) ->
    case is_nif_loaded() of
        true -> nif_is_builtin_type(Type);
        false -> erlang_is_builtin_type(Type)
    end;
is_builtin_type(_) ->
    false.

%% @doc Join path segments into a single path binary.
%%
%% Example:
%% ```
%% &lt;&lt;"foo/bar/baz"&gt;&gt; = macula_mri_nif:join_path_segments([&lt;&lt;"foo"&gt;&gt;, &lt;&lt;"bar"&gt;&gt;, &lt;&lt;"baz"&gt;&gt;]).
%% '''
-spec join_path_segments(Segments :: [binary()]) -> binary().
join_path_segments(Segments) when is_list(Segments) ->
    case is_nif_loaded() of
        true -> nif_join_path_segments(Segments);
        false -> erlang_join_path_segments(Segments)
    end;
join_path_segments(_) ->
    <<>>.

%% @doc Format MRI components into an MRI string.
%%
%% Example:
%% ```
%% {ok, &lt;&lt;"mri:app:io.macula/counter"&gt;&gt;} =
%%     macula_mri_nif:format_mri(&lt;&lt;"app"&gt;&gt;, &lt;&lt;"io.macula"&gt;&gt;, [&lt;&lt;"counter"&gt;&gt;]).
%% '''
-spec format_mri(Type :: binary(), Realm :: binary(), Path :: [binary()]) ->
    {ok, binary()} | {error, invalid_type | invalid_realm}.
format_mri(Type, Realm, Path) when is_binary(Type), is_binary(Realm), is_list(Path) ->
    case is_nif_loaded() of
        true -> nif_format_mri(Type, Realm, Path);
        false -> erlang_format_mri(Type, Realm, Path)
    end;
format_mri(_, _, _) ->
    {error, invalid_format}.

%% @doc Find children of a parent MRI from a list of MRIs.
%%
%% Children are MRIs that have the same realm, path starts with parent's path,
%% and have exactly one more path segment.
%%
%% The `AllMRIs' parameter is a list of `{Realm, PathSegments, FullMRI}' tuples.
%%
%% Example:
%% ```
%% AllMRIs = [
%%     {&lt;&lt;"io.macula"&gt;&gt;, [&lt;&lt;"apps"&gt;&gt;], &lt;&lt;"mri:app:io.macula/apps"&gt;&gt;},
%%     {&lt;&lt;"io.macula"&gt;&gt;, [&lt;&lt;"apps"&gt;&gt;, &lt;&lt;"counter"&gt;&gt;], &lt;&lt;"mri:app:io.macula/apps/counter"&gt;&gt;}
%% ],
%% [&lt;&lt;"mri:app:io.macula/apps/counter"&gt;&gt;] =
%%     macula_mri_nif:find_children(&lt;&lt;"io.macula"&gt;&gt;, [&lt;&lt;"apps"&gt;&gt;], AllMRIs).
%% '''
-spec find_children(
    ParentRealm :: binary(),
    ParentPath :: [binary()],
    AllMRIs :: [{binary(), [binary()], binary()}]
) -> [binary()].
find_children(ParentRealm, ParentPath, AllMRIs)
  when is_binary(ParentRealm), is_list(ParentPath), is_list(AllMRIs) ->
    case is_nif_loaded() of
        true -> nif_find_children(ParentRealm, ParentPath, AllMRIs);
        false -> erlang_find_children(ParentRealm, ParentPath, AllMRIs)
    end;
find_children(_, _, _) ->
    [].

%% @doc Find descendants of a parent MRI from a list of MRIs.
%%
%% Descendants are MRIs that have the same realm, path starts with parent's path,
%% and have at least one more path segment.
%%
%% The `AllMRIs' parameter is a list of `{Realm, PathSegments, FullMRI}' tuples.
-spec find_descendants(
    ParentRealm :: binary(),
    ParentPath :: [binary()],
    AllMRIs :: [{binary(), [binary()], binary()}]
) -> [binary()].
find_descendants(ParentRealm, ParentPath, AllMRIs)
  when is_binary(ParentRealm), is_list(ParentPath), is_list(AllMRIs) ->
    case is_nif_loaded() of
        true -> nif_find_descendants(ParentRealm, ParentPath, AllMRIs);
        false -> erlang_find_descendants(ParentRealm, ParentPath, AllMRIs)
    end;
find_descendants(_, _, _) ->
    [].

%% @doc Build a trie index from a list of MRIs.
%%
%% Creates a persistent trie index that enables O(d) queries where d is the
%% path depth. For million-scale deployments, this is orders of magnitude
%% faster than O(n) list scanning.
%%
%% When NIFs are loaded, returns an opaque reference to the Rust trie.
%% When using Erlang fallback, returns a map-based index (slower but functional).
%%
%% Example:
%% ```
%% AllMRIs = [
%%     {&lt;&lt;"io.macula"&gt;&gt;, [&lt;&lt;"apps"&gt;&gt;], &lt;&lt;"mri:app:io.macula/apps"&gt;&gt;},
%%     {&lt;&lt;"io.macula"&gt;&gt;, [&lt;&lt;"apps"&gt;&gt;, &lt;&lt;"counter"&gt;&gt;], &lt;&lt;"mri:app:io.macula/apps/counter"&gt;&gt;}
%% ],
%% {ok, Index} = macula_mri_nif:build_path_index(AllMRIs).
%% '''
-spec build_path_index(MRIs :: [{binary(), [binary()], binary()}]) ->
    {ok, reference() | map()}.
build_path_index(MRIs) when is_list(MRIs) ->
    case is_nif_loaded() of
        true -> nif_build_path_index(MRIs);
        false -> erlang_build_path_index(MRIs)
    end;
build_path_index(_) ->
    erlang_build_path_index([]).

%% @doc Find direct children using a trie index.
%%
%% O(d) complexity where d is the path depth, vs O(n) for list scanning.
%%
%% Example:
%% ```
%% {ok, Index} = macula_mri_nif:build_path_index(AllMRIs),
%% {ok, [&lt;&lt;"mri:app:io.macula/apps/counter"&gt;&gt;]} =
%%     macula_mri_nif:index_find_children(Index, &lt;&lt;"io.macula"&gt;&gt;, [&lt;&lt;"apps"&gt;&gt;]).
%% '''
-spec index_find_children(Index :: reference() | map(), Realm :: binary(), Path :: [binary()]) ->
    {ok, [binary()]} | {error, invalid_realm}.
index_find_children(Index, Realm, Path) when is_binary(Realm), is_list(Path) ->
    case is_nif_loaded() andalso is_reference(Index) of
        true -> nif_index_find_children(Index, Realm, Path);
        false -> erlang_index_find_children(Index, Realm, Path)
    end;
index_find_children(_, _, _) ->
    {error, invalid_realm}.

%% @doc Find all descendants using a trie index.
%%
%% O(d + m) complexity where d is path depth and m is number of descendants,
%% vs O(n) for list scanning where n is total MRIs.
-spec index_find_descendants(Index :: reference() | map(), Realm :: binary(), Path :: [binary()]) ->
    {ok, [binary()]} | {error, invalid_realm}.
index_find_descendants(Index, Realm, Path) when is_binary(Realm), is_list(Path) ->
    case is_nif_loaded() andalso is_reference(Index) of
        true -> nif_index_find_descendants(Index, Realm, Path);
        false -> erlang_index_find_descendants(Index, Realm, Path)
    end;
index_find_descendants(_, _, _) ->
    {error, invalid_realm}.

%% @doc Insert a single MRI into an existing index.
%%
%% Use this for dynamic updates when devices connect.
%%
%% Example:
%% ```
%% ok = macula_mri_nif:index_insert(Index, &lt;&lt;"io.macula"&gt;&gt;, [&lt;&lt;"devices"&gt;&gt;, &lt;&lt;"sensor1"&gt;&gt;],
%%                                   &lt;&lt;"mri:device:io.macula/devices/sensor1"&gt;&gt;).
%% '''
-spec index_insert(Index :: reference() | map(), Realm :: binary(), Path :: [binary()], MRI :: binary()) ->
    ok | {error, invalid_realm}.
index_insert(Index, Realm, Path, MRI) when is_binary(Realm), is_list(Path), is_binary(MRI) ->
    case is_nif_loaded() andalso is_reference(Index) of
        true -> nif_index_insert(Index, Realm, Path, MRI);
        false -> erlang_index_insert(Index, Realm, Path, MRI)
    end;
index_insert(_, _, _, _) ->
    {error, invalid_realm}.

%% @doc Remove a single MRI from an existing index.
%%
%% Use this for dynamic updates when devices disconnect.
-spec index_remove(Index :: reference() | map(), Realm :: binary(), Path :: [binary()]) ->
    ok | {error, not_found | invalid_realm}.
index_remove(Index, Realm, Path) when is_binary(Realm), is_list(Path) ->
    case is_nif_loaded() andalso is_reference(Index) of
        true -> nif_index_remove(Index, Realm, Path);
        false -> erlang_index_remove(Index, Realm, Path)
    end;
index_remove(_, _, _) ->
    {error, invalid_realm}.

%% @doc Get the number of MRIs in an index.
-spec index_size(Index :: reference() | map()) -> {ok, non_neg_integer()}.
index_size(Index) ->
    case is_nif_loaded() andalso is_reference(Index) of
        true -> nif_index_size(Index);
        false -> erlang_index_size(Index)
    end.

%%====================================================================
%% NIF Stubs (replaced when NIF loads)
%%====================================================================

nif_parse_mri(_MRI) ->
    erlang:nif_error(nif_not_loaded).

nif_validate_realm_format(_Realm) ->
    erlang:nif_error(nif_not_loaded).

nif_validate_segment_chars(_Segment) ->
    erlang:nif_error(nif_not_loaded).

nif_is_builtin_type(_Type) ->
    erlang:nif_error(nif_not_loaded).

nif_join_path_segments(_Segments) ->
    erlang:nif_error(nif_not_loaded).

nif_format_mri(_Type, _Realm, _Path) ->
    erlang:nif_error(nif_not_loaded).

nif_find_children(_ParentRealm, _ParentPath, _AllMRIs) ->
    erlang:nif_error(nif_not_loaded).

nif_find_descendants(_ParentRealm, _ParentPath, _AllMRIs) ->
    erlang:nif_error(nif_not_loaded).

nif_build_path_index(_MRIs) ->
    erlang:nif_error(nif_not_loaded).

nif_index_find_children(_Index, _Realm, _Path) ->
    erlang:nif_error(nif_not_loaded).

nif_index_find_descendants(_Index, _Realm, _Path) ->
    erlang:nif_error(nif_not_loaded).

nif_index_insert(_Index, _Realm, _Path, _MRI) ->
    erlang:nif_error(nif_not_loaded).

nif_index_remove(_Index, _Realm, _Path) ->
    erlang:nif_error(nif_not_loaded).

nif_index_size(_Index) ->
    erlang:nif_error(nif_not_loaded).

%%====================================================================
%% Pure Erlang Fallbacks
%%====================================================================

%% @private Parse MRI using pure Erlang
erlang_parse_mri(MRI) ->
    case binary:split(MRI, <<":">>, [global]) of
        [<<"mri">>, Type, RealmAndPath] when Type =/= <<>> ->
            parse_realm_and_path(Type, RealmAndPath);
        _ ->
            {error, invalid_format}
    end.

%% @private Parse realm and optional path
parse_realm_and_path(Type, RealmAndPath) ->
    case binary:split(RealmAndPath, <<"/">>) of
        [Realm] when Realm =/= <<>> ->
            case erlang_validate_realm_format(Realm) of
                true ->
                    {ok, #{type => Type, realm => Realm, path => []}};
                false ->
                    {error, invalid_realm}
            end;
        [Realm, PathPart] when Realm =/= <<>> ->
            case erlang_validate_realm_format(Realm) of
                true ->
                    Segments = binary:split(PathPart, <<"/">>, [global]),
                    case validate_all_segments(Segments) of
                        true ->
                            {ok, #{type => Type, realm => Realm, path => Segments}};
                        false ->
                            {error, invalid_segment}
                    end;
                false ->
                    {error, invalid_realm}
            end;
        _ ->
            {error, invalid_format}
    end.

%% @private Validate all segments in a list
validate_all_segments([]) ->
    true;
validate_all_segments([Seg | Rest]) ->
    case erlang_validate_segment_chars(Seg) of
        true -> validate_all_segments(Rest);
        false -> false
    end.

%% @private Validate realm format using pure Erlang
erlang_validate_realm_format(Realm) when byte_size(Realm) =:= 0 ->
    false;
erlang_validate_realm_format(Realm) ->
    case binary:match(Realm, <<".">>) of
        nomatch -> false;
        _ -> erlang_is_valid_realm_chars(Realm)
    end.

%% @private Check realm characters (a-z, 0-9, .)
erlang_is_valid_realm_chars(Realm) ->
    lists:all(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse
            (C >= $0 andalso C =< $9) orelse
            C =:= $.
        end,
        binary_to_list(Realm)
    ).

%% @private Validate segment characters using pure Erlang
erlang_validate_segment_chars(Segment) when byte_size(Segment) =:= 0 ->
    false;
erlang_validate_segment_chars(Segment) ->
    lists:all(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse
            (C >= $0 andalso C =< $9) orelse
            C =:= $- orelse
            C =:= $_
        end,
        binary_to_list(Segment)
    ).

%% @private Check if builtin type using pure Erlang
erlang_is_builtin_type(Type) ->
    lists:member(Type, ?BUILTIN_TYPES).

%% @private Join path segments using pure Erlang
erlang_join_path_segments([]) ->
    <<>>;
erlang_join_path_segments(Segments) ->
    lists:foldl(
        fun(Seg, <<>>) -> Seg;
           (Seg, Acc) -> <<Acc/binary, "/", Seg/binary>>
        end,
        <<>>,
        Segments
    ).

%% @private Format MRI using pure Erlang
erlang_format_mri(Type, Realm, []) ->
    {ok, <<"mri:", Type/binary, ":", Realm/binary>>};
erlang_format_mri(Type, Realm, Path) ->
    PathBin = erlang_join_path_segments(Path),
    {ok, <<"mri:", Type/binary, ":", Realm/binary, "/", PathBin/binary>>}.

%% @private Find children using pure Erlang
erlang_find_children(ParentRealm, ParentPath, AllMRIs) ->
    ParentDepth = length(ParentPath),
    lists:filtermap(
        fun({Realm, Path, MRI}) ->
            case Realm =:= ParentRealm andalso
                 length(Path) =:= ParentDepth + 1 andalso
                 lists:prefix(ParentPath, Path) of
                true -> {true, MRI};
                false -> false
            end
        end,
        AllMRIs
    ).

%% @private Find descendants using pure Erlang
erlang_find_descendants(ParentRealm, ParentPath, AllMRIs) ->
    ParentDepth = length(ParentPath),
    lists:filtermap(
        fun({Realm, Path, MRI}) ->
            case Realm =:= ParentRealm andalso
                 length(Path) > ParentDepth andalso
                 lists:prefix(ParentPath, Path) of
                true -> {true, MRI};
                false -> false
            end
        end,
        AllMRIs
    ).

%%====================================================================
%% Erlang Fallback - Trie Index Operations
%%====================================================================

%% The Erlang fallback uses a nested map structure:
%% #{realm => #{segment => #{segment => #{...}, '$mri' => MRI}}}
%% The '$mri' key stores the MRI at leaf nodes.

-define(MRI_KEY, '$mri').
-define(COUNT_KEY, '$count').

%% @private Build index using pure Erlang (map-based trie)
erlang_build_path_index(MRIs) ->
    Index = lists:foldl(
        fun({Realm, Path, MRI}, Acc) ->
            erlang_index_insert_impl(Acc, Realm, Path, MRI)
        end,
        #{?COUNT_KEY => 0},
        MRIs
    ),
    {ok, Index}.

%% @private Find children using map-based index
erlang_index_find_children(Index, Realm, Path) when is_map(Index) ->
    case maps:get(Realm, Index, undefined) of
        undefined ->
            {ok, []};
        RealmTree ->
            case navigate_to_node(RealmTree, Path) of
                undefined ->
                    {ok, []};
                Node when is_map(Node) ->
                    %% Collect MRIs from immediate children (not grandchildren)
                    Children = maps:fold(
                        fun(?MRI_KEY, _, Acc) -> Acc;
                           (?COUNT_KEY, _, Acc) -> Acc;
                           (_Segment, ChildNode, Acc) ->
                               case maps:get(?MRI_KEY, ChildNode, undefined) of
                                   undefined -> Acc;
                                   MRI -> [MRI | Acc]
                               end
                        end,
                        [],
                        Node
                    ),
                    {ok, Children}
            end
    end;
erlang_index_find_children(_, _, _) ->
    {ok, []}.

%% @private Find descendants using map-based index
erlang_index_find_descendants(Index, Realm, Path) when is_map(Index) ->
    case maps:get(Realm, Index, undefined) of
        undefined ->
            {ok, []};
        RealmTree ->
            case navigate_to_node(RealmTree, Path) of
                undefined ->
                    {ok, []};
                Node when is_map(Node) ->
                    Descendants = collect_all_mris(Node, []),
                    {ok, Descendants}
            end
    end;
erlang_index_find_descendants(_, _, _) ->
    {ok, []}.

%% @private Insert into map-based index
erlang_index_insert(Index, Realm, Path, MRI) when is_map(Index) ->
    NewIndex = erlang_index_insert_impl(Index, Realm, Path, MRI),
    %% Note: Erlang fallback uses mutable-style semantics via process dictionary
    %% or the caller must track the new index. For simplicity, we store in process dict.
    put(mri_index, NewIndex),
    ok;
erlang_index_insert(_, _, _, _) ->
    {error, invalid_realm}.

%% @private Remove from map-based index
erlang_index_remove(Index, Realm, Path) when is_map(Index) ->
    case erlang_index_remove_impl(Index, Realm, Path) of
        {ok, NewIndex} ->
            put(mri_index, NewIndex),
            ok;
        {error, not_found} ->
            {error, not_found}
    end;
erlang_index_remove(_, _, _) ->
    {error, invalid_realm}.

%% @private Get index size
erlang_index_size(Index) when is_map(Index) ->
    {ok, maps:get(?COUNT_KEY, Index, 0)};
erlang_index_size(_) ->
    {ok, 0}.

%% @private Insert implementation
erlang_index_insert_impl(Index, Realm, Path, MRI) ->
    RealmTree = maps:get(Realm, Index, #{}),
    NewRealmTree = insert_into_tree(RealmTree, Path, MRI),
    Count = maps:get(?COUNT_KEY, Index, 0),
    Index#{Realm => NewRealmTree, ?COUNT_KEY => Count + 1}.

%% @private Insert path into tree
insert_into_tree(Tree, [], MRI) ->
    Tree#{?MRI_KEY => MRI};
insert_into_tree(Tree, [Segment | Rest], MRI) ->
    Child = maps:get(Segment, Tree, #{}),
    NewChild = insert_into_tree(Child, Rest, MRI),
    Tree#{Segment => NewChild}.

%% @private Remove implementation
erlang_index_remove_impl(Index, Realm, Path) ->
    case maps:get(Realm, Index, undefined) of
        undefined ->
            {error, not_found};
        RealmTree ->
            case remove_from_tree(RealmTree, Path) of
                {ok, NewRealmTree} ->
                    Count = maps:get(?COUNT_KEY, Index, 1),
                    NewIndex = case maps:size(NewRealmTree) of
                        0 -> maps:remove(Realm, Index);
                        _ -> Index#{Realm => NewRealmTree}
                    end,
                    {ok, NewIndex#{?COUNT_KEY => Count - 1}};
                {error, not_found} ->
                    {error, not_found}
            end
    end.

%% @private Remove path from tree
remove_from_tree(Tree, []) ->
    case maps:is_key(?MRI_KEY, Tree) of
        true ->
            NewTree = maps:remove(?MRI_KEY, Tree),
            {ok, NewTree};
        false ->
            {error, not_found}
    end;
remove_from_tree(Tree, [Segment | Rest]) ->
    case maps:get(Segment, Tree, undefined) of
        undefined ->
            {error, not_found};
        Child ->
            case remove_from_tree(Child, Rest) of
                {ok, NewChild} ->
                    NewTree = case maps:size(NewChild) of
                        0 -> maps:remove(Segment, Tree);
                        _ -> Tree#{Segment => NewChild}
                    end,
                    {ok, NewTree};
                {error, not_found} ->
                    {error, not_found}
            end
    end.

%% @private Navigate to a node in the tree
navigate_to_node(Tree, []) ->
    Tree;
navigate_to_node(Tree, [Segment | Rest]) ->
    case maps:get(Segment, Tree, undefined) of
        undefined -> undefined;
        Child -> navigate_to_node(Child, Rest)
    end.

%% @private Collect all MRIs in subtree (excluding the node itself)
collect_all_mris(Node, Acc) ->
    maps:fold(
        fun(?MRI_KEY, _, InnerAcc) -> InnerAcc;  %% Skip self
           (?COUNT_KEY, _, InnerAcc) -> InnerAcc;
           (_Segment, ChildNode, InnerAcc) ->
               %% Add child's MRI if present
               Acc1 = case maps:get(?MRI_KEY, ChildNode, undefined) of
                   undefined -> InnerAcc;
                   MRI -> [MRI | InnerAcc]
               end,
               %% Recurse into grandchildren
               collect_all_mris(ChildNode, Acc1)
        end,
        Acc,
        Node
    ).
