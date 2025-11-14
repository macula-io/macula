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
    %% Check scheme using binary:match
    SchemeSize = byte_size(?SCHEME),
    case binary:match(Uri, ?SCHEME) of
        {0, SchemeSize} ->
            %% Scheme found at start, extract rest
            Rest = binary:part(Uri, SchemeSize, byte_size(Uri) - SchemeSize),
            case binary:split(Rest, <<"/">>) of
                [Realm, NodeIdHex] when Realm =/= <<>>, NodeIdHex =/= <<>> ->
                    %% Convert hex node ID to binary (catch invalid hex from external URI)
                    try macula_id:from_hex(NodeIdHex) of
                        NodeId -> {ok, Realm, NodeId}
                    catch
                        _:_ -> {error, invalid_uri}
                    end;
                _ ->
                    {error, invalid_uri}
            end;
        _ ->
            {error, invalid_uri}
    end;
parse(_) ->
    {error, invalid_uri}.

%% @doc Extract realm from URI.
-spec get_realm(uri()) -> {ok, realm()} | {error, invalid_uri}.
get_realm(Uri) ->
    case parse(Uri) of
        {ok, Realm, _NodeId} -> {ok, Realm};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Extract node ID from URI.
-spec get_node_id(uri()) -> {ok, node_id()} | {error, invalid_uri}.
get_node_id(Uri) ->
    case parse(Uri) of
        {ok, _Realm, NodeId} -> {ok, NodeId};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Check if URI is valid.
-spec is_valid(uri()) -> boolean().
is_valid(Uri) ->
    case parse(Uri) of
        {ok, _, _} -> true;
        {error, _} -> false
    end.

%% @doc Check if two URIs are equal.
-spec equals(uri(), uri()) -> boolean().
equals(Uri1, Uri2) ->
    Uri1 =:= Uri2.
