%%%-------------------------------------------------------------------
%%% @doc
%%% Macula URI parsing and construction.
%%% Format: macula://realm/node_id
%%% Example: macula://org.example.mesh/0123456789abcdef...
%%% @end
%%%-------------------------------------------------------------------
-module(macula_uri).

%% API
-export([
    new/2,
    parse/1,
    get_realm/1,
    get_node_id/1,
    is_valid/1,
    equals/2
]).

%% Types
-type uri() :: binary().
-type realm() :: binary().
-type node_id() :: binary().

-export_type([uri/0]).

-define(SCHEME, <<"macula://">>).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Construct Macula URI from realm and node ID.
-spec new(realm(), node_id()) -> uri().
new(Realm, NodeId) when is_binary(Realm), is_binary(NodeId) ->
    %% Convert node ID to hex for URI
    NodeIdHex = macula_id:to_hex(NodeId),
    <<?SCHEME/binary, Realm/binary, $/, NodeIdHex/binary>>.

%% @doc Parse Macula URI to extract realm and node ID.
-spec parse(uri()) -> {ok, realm(), node_id()} | {error, invalid_uri}.
parse(Uri) when is_binary(Uri) ->
    SchemeSize = byte_size(?SCHEME),
    SchemeMatch = binary:match(Uri, ?SCHEME),
    parse_scheme(SchemeMatch, SchemeSize, Uri);
parse(_) ->
    {error, invalid_uri}.

%% @private Scheme found at position 0 - extract rest and split
parse_scheme({0, SchemeSize}, SchemeSize, Uri) ->
    Rest = binary:part(Uri, SchemeSize, byte_size(Uri) - SchemeSize),
    SplitResult = binary:split(Rest, <<"/">>),
    parse_path(SplitResult);
%% @private Scheme not found or wrong position
parse_scheme(_, _, _) ->
    {error, invalid_uri}.

%% @private Valid path with non-empty realm and node ID hex
parse_path([Realm, NodeIdHex]) when Realm =/= <<>>, NodeIdHex =/= <<>> ->
    convert_node_id(NodeIdHex, Realm);
%% @private Invalid path format
parse_path(_) ->
    {error, invalid_uri}.

%% @private Convert hex to node ID (external URIs may have invalid hex)
convert_node_id(NodeIdHex, Realm) ->
    try macula_id:from_hex(NodeIdHex) of
        NodeId -> {ok, Realm, NodeId}
    catch
        _:_ -> {error, invalid_uri}
    end.

%% @doc Extract realm from URI.
-spec get_realm(uri()) -> {ok, realm()} | {error, invalid_uri}.
get_realm(Uri) ->
    ParseResult = parse(Uri),
    extract_realm(ParseResult).

%% @private Parse succeeded - extract realm
extract_realm({ok, Realm, _NodeId}) ->
    {ok, Realm};
%% @private Parse failed - propagate error
extract_realm({error, Reason}) ->
    {error, Reason}.

%% @doc Extract node ID from URI.
-spec get_node_id(uri()) -> {ok, node_id()} | {error, invalid_uri}.
get_node_id(Uri) ->
    ParseResult = parse(Uri),
    extract_node_id(ParseResult).

%% @private Parse succeeded - extract node ID
extract_node_id({ok, _Realm, NodeId}) ->
    {ok, NodeId};
%% @private Parse failed - propagate error
extract_node_id({error, Reason}) ->
    {error, Reason}.

%% @doc Check if URI is valid.
-spec is_valid(uri()) -> boolean().
is_valid(Uri) ->
    ParseResult = parse(Uri),
    check_valid(ParseResult).

%% @private Valid URI
check_valid({ok, _, _}) ->
    true;
%% @private Invalid URI
check_valid({error, _}) ->
    false.

%% @doc Check if two URIs are equal.
-spec equals(uri(), uri()) -> boolean().
equals(Uri1, Uri2) ->
    Uri1 =:= Uri2.
