%%%-------------------------------------------------------------------
%%% @doc
%%% Realm management and validation.
%%% Realms provide logical isolation boundaries in the mesh.
%%% Realm names follow reverse DNS notation (e.g., "org.example.mesh").
%%% @end
%%%-------------------------------------------------------------------
-module(macula_realm).

%% API
-export([
    id/1,
    validate/1,
    normalize/1,
    equals/2,
    namespace/1,
    to_binary/1,
    from_binary/1
]).

%% Types
-type realm_name() :: binary().
-type realm_id() :: binary().  % 32-byte SHA-256 hash

-export_type([realm_name/0, realm_id/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Generate deterministic realm ID from name.
%% Uses SHA-256 hash for 256-bit realm IDs.
-spec id(realm_name()) -> realm_id().
id(RealmName) when is_binary(RealmName) ->
    crypto:hash(sha256, RealmName).

%% @doc Validate realm name format.
%% Rules:
%%   - Reverse DNS notation (org.example.mesh)
%%   - Lowercase alphanumeric, dots, hyphens, underscores
%%   - No leading/trailing dots
%%   - No consecutive dots
-spec validate(realm_name()) -> ok | {error, term()}.
validate(<<>>) ->
    {error, empty_realm};
validate(RealmName) when is_binary(RealmName) ->
    %% Check for leading/trailing dots
    case binary:first(RealmName) of
        $. -> {error, invalid_realm};
        _ ->
            case binary:last(RealmName) of
                $. -> {error, invalid_realm};
                _ ->
                    %% Check for double dots and invalid characters
                    validate_segments(RealmName)
            end
    end.

%% @doc Normalize realm name (lowercase, trim).
-spec normalize(realm_name()) -> realm_name().
normalize(RealmName) when is_binary(RealmName) ->
    %% Trim whitespace
    Trimmed = string:trim(RealmName),
    %% Convert to lowercase
    string:lowercase(Trimmed).

%% @doc Check if two realm names are equal.
-spec equals(realm_name(), realm_name()) -> boolean().
equals(Realm1, Realm2) ->
    Realm1 =:= Realm2.

%% @doc Extract namespace (top-level domain) from realm name.
%% Example: "org.example.mesh" -> "org"
-spec namespace(realm_name()) -> binary().
namespace(<<>>) ->
    <<>>;
namespace(RealmName) when is_binary(RealmName) ->
    case binary:split(RealmName, <<".">>) of
        [Namespace | _] -> Namespace
    end.

%% @doc Encode realm name to binary.
-spec to_binary(realm_name()) -> binary().
to_binary(RealmName) when is_binary(RealmName) ->
    %% Length prefix + realm name
    Len = byte_size(RealmName),
    <<Len:16, RealmName/binary>>.

%% @doc Decode realm name from binary.
-spec from_binary(binary()) -> {ok, realm_name()} | {error, term()}.
from_binary(<<Len:16, RealmName:Len/binary>>) ->
    case validate(RealmName) of
        ok -> {ok, RealmName};
        {error, Reason} -> {error, Reason}
    end;
from_binary(_) ->
    {error, invalid_binary}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Validate realm segments.
-spec validate_segments(binary()) -> ok | {error, invalid_realm}.
validate_segments(RealmName) ->
    Segments = binary:split(RealmName, <<".">>, [global]),
    case validate_segment_list(Segments) of
        ok -> ok;
        error -> {error, invalid_realm}
    end.

%% @doc Validate list of segments.
-spec validate_segment_list([binary()]) -> ok | error.
validate_segment_list([]) ->
    ok;
validate_segment_list([<<>> | _Rest]) ->
    %% Empty segment (double dot)
    error;
validate_segment_list([Segment | Rest]) ->
    case validate_segment_chars(Segment) of
        ok -> validate_segment_list(Rest);
        error -> error
    end.

%% @doc Validate characters in a single segment.
-spec validate_segment_chars(binary()) -> ok | error.
validate_segment_chars(<<>>) ->
    ok;
validate_segment_chars(<<Char, Rest/binary>>) ->
    case is_valid_char(Char) of
        true -> validate_segment_chars(Rest);
        false -> error
    end.

%% @doc Check if character is valid in realm name.
-spec is_valid_char(byte()) -> boolean().
is_valid_char(Char) when Char >= $a, Char =< $z -> true;  % lowercase
is_valid_char(Char) when Char >= $A, Char =< $Z -> true;  % uppercase
is_valid_char(Char) when Char >= $0, Char =< $9 -> true;  % digits
is_valid_char($-) -> true;  % hyphen
is_valid_char($_) -> true;  % underscore
is_valid_char(_) -> false.
