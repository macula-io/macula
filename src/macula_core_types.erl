%%%-------------------------------------------------------------------
%%% @doc
%%% Core type definitions and encoding/decoding for Macula.
%%% Provides fundamental types like node IDs, realm IDs, and addresses.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_core_types).

%% API exports
-export([
    node_id/0,
    realm_id/1,
    encode_address/1,
    decode_address/1
]).

%%%===================================================================
%%% Type Specifications
%%%===================================================================

-type node_id() :: binary().  % 32-byte unique node identifier
-type realm_id() :: binary(). % 32-byte realm identifier
-type ip_address() :: inet:ip_address().
-type port_number() :: inet:port_number().
-type address() :: {ip_address(), port_number()}.

-export_type([node_id/0, realm_id/0, address/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Generate a unique node ID.
%% Uses cryptographically strong random bytes for uniqueness.
%% @end
-spec node_id() -> node_id().
node_id() ->
    crypto:strong_rand_bytes(32).

%% @doc Generate a deterministic realm ID from a realm name.
%% Same name always produces the same ID (uses SHA-256 hash).
%% @end
-spec realm_id(binary()) -> realm_id().
realm_id(RealmName) when is_binary(RealmName) ->
    crypto:hash(sha256, RealmName).

%% @doc Encode an IP address and port to binary format.
%% Format:
%%   - 1 byte: IP version (4 or 6)
%%   - 4 or 16 bytes: IP address
%%   - 2 bytes: port (big-endian)
%% @end
-spec encode_address(address()) -> binary().
encode_address({{A, B, C, D}, Port}) when is_integer(Port) ->
    %% IPv4
    <<4:8, A:8, B:8, C:8, D:8, Port:16>>;
encode_address({{A, B, C, D, E, F, G, H}, Port}) when is_integer(Port) ->
    %% IPv6
    <<6:8, A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, Port:16>>.

%% @doc Decode binary address format to {IP, Port} tuple.
%% @end
-spec decode_address(binary()) -> {ok, address()} | {error, invalid_address}.
decode_address(<<4:8, A:8, B:8, C:8, D:8, Port:16>>) ->
    %% IPv4
    {ok, {{A, B, C, D}, Port}};
decode_address(<<6:8, A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16, Port:16>>) ->
    %% IPv6
    {ok, {{A, B, C, D, E, F, G, H}, Port}};
decode_address(_) ->
    {error, invalid_address}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% None yet
