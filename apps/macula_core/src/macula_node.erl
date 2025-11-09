%%%-------------------------------------------------------------------
%%% @doc
%%% Node identity and metadata management.
%%% Represents a single node in the Macula mesh.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_node).

%% API
-export([
    new/1,
    new/2,
    get_id/1,
    get_realm/1,
    get_metadata/1,
    set_metadata/2,
    update_metadata/2,
    get_address/1,
    set_address/2,
    to_binary/1,
    from_binary/1,
    equals/2
]).

%% Types
-type node_id() :: binary().  % 32-byte unique identifier
-type realm() :: binary().
-type metadata() :: map().
-type address() :: {inet:ip_address(), inet:port_number()}.

-type macula_node() :: #{
    node_id := node_id(),
    realm := realm(),
    metadata => metadata(),
    address => address()
}.

-export_type([macula_node/0, node_id/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create new node with random ID.
-spec new(realm()) -> macula_node().
new(Realm) ->
    new(Realm, #{}).

%% @doc Create new node with random ID and metadata.
-spec new(realm(), metadata()) -> macula_node().
new(Realm, Metadata) when is_binary(Realm), is_map(Metadata) ->
    NodeId = crypto:strong_rand_bytes(32),  % 256-bit random ID
    #{
        node_id => NodeId,
        realm => Realm,
        metadata => Metadata
    }.

%% @doc Get node ID.
-spec get_id(macula_node()) -> node_id().
get_id(#{node_id := NodeId}) ->
    NodeId.

%% @doc Get realm.
-spec get_realm(macula_node()) -> realm().
get_realm(#{realm := Realm}) ->
    Realm.

%% @doc Get metadata.
-spec get_metadata(macula_node()) -> metadata().
get_metadata(#{metadata := Metadata}) ->
    Metadata;
get_metadata(_Node) ->
    #{}.

%% @doc Set metadata (replaces existing).
-spec set_metadata(macula_node(), metadata()) -> macula_node().
set_metadata(Node, Metadata) when is_map(Metadata) ->
    Node#{metadata => Metadata}.

%% @doc Update metadata (merges with existing).
-spec update_metadata(macula_node(), metadata()) -> macula_node().
update_metadata(Node, NewMetadata) when is_map(NewMetadata) ->
    ExistingMetadata = get_metadata(Node),
    MergedMetadata = maps:merge(ExistingMetadata, NewMetadata),
    Node#{metadata => MergedMetadata}.

%% @doc Get address.
-spec get_address(macula_node()) -> address() | undefined.
get_address(#{address := Address}) ->
    Address;
get_address(_Node) ->
    undefined.

%% @doc Set address.
-spec set_address(macula_node(), address()) -> macula_node().
set_address(Node, Address) ->
    Node#{address => Address}.

%% @doc Encode node to binary.
-spec to_binary(macula_node()) -> binary().
to_binary(Node) ->
    term_to_binary(Node).

%% @doc Decode node from binary.
-spec from_binary(binary()) -> {ok, macula_node()} | {error, term()}.
from_binary(Binary) when is_binary(Binary) ->
    try
        Node = binary_to_term(Binary, [safe]),
        case validate_node(Node) of
            ok -> {ok, Node};
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Error ->
            {error, {decode_failed, Error}}
    end.

%% @doc Check if two nodes are equal (by ID).
-spec equals(macula_node(), macula_node()) -> boolean().
equals(#{node_id := Id}, #{node_id := Id}) ->
    true;
equals(_Node1, _Node2) ->
    false.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Validate node structure.
-spec validate_node(term()) -> ok | {error, term()}.
validate_node(#{node_id := NodeId, realm := Realm})
    when is_binary(NodeId), is_binary(Realm), byte_size(NodeId) =:= 32 ->
    ok;
validate_node(_) ->
    {error, invalid_node}.
