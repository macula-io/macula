%%%-------------------------------------------------------------------
%%% @doc
%%% URI validation and utilities for RPC procedures.
%%% Uses reverse DNS notation: org.domain.service.procedure
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_uri).

%% API
-export([
    validate/1,
    matches/2,
    normalize/1,
    namespace/1,
    segment_count/1
]).

%% Types
-type uri() :: binary().
-export_type([uri/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Validate URI syntax.
%% Valid URIs:
%% - Non-empty
%% - Segments separated by dots
%% - Segments contain alphanumeric, underscore, hyphen
%% - No leading or trailing dots
%% - No double dots
-spec validate(uri()) -> ok | {error, invalid_uri}.
validate(<<>>) ->
    {error, invalid_uri};
validate(Uri) ->
    %% Check for leading/trailing dots
    case binary:first(Uri) of
        $. -> {error, invalid_uri};
        _ ->
            case binary:last(Uri) of
                $. -> {error, invalid_uri};
                _ -> validate_segments(Uri)
            end
    end.

%% @doc Check if URI matches pattern.
%% For now, only exact matching (no wildcards).
%% Future: Could add wildcard patterns if needed.
-spec matches(uri(), uri()) -> boolean().
matches(Uri, Pattern) ->
    Uri =:= Pattern.

%% @doc Normalize URI (lowercase, trim, remove double dots).
-spec normalize(uri()) -> uri().
normalize(Uri) ->
    %% Trim whitespace
    Trimmed = string:trim(Uri),

    %% Lowercase
    Lower = string:lowercase(Trimmed),

    %% Remove double dots
    remove_double_dots(Lower).

%% @doc Extract namespace (first segment).
-spec namespace(uri()) -> binary().
namespace(<<>>) ->
    <<>>;
namespace(Uri) ->
    case binary:split(Uri, <<".">>) of
        [Namespace] -> Namespace;
        [Namespace, _Rest] -> Namespace
    end.

%% @doc Count number of segments in URI.
-spec segment_count(uri()) -> non_neg_integer().
segment_count(<<>>) ->
    0;
segment_count(Uri) ->
    Segments = binary:split(Uri, <<".">>, [global]),
    length(Segments).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Validate all segments contain valid characters.
validate_segments(Uri) ->
    Segments = binary:split(Uri, <<".">>, [global]),
    case lists:all(fun is_valid_segment/1, Segments) of
        true -> ok;
        false -> {error, invalid_uri}
    end.

%% @doc Check if segment contains only valid characters.
is_valid_segment(<<>>) ->
    false;  % Empty segment not allowed
is_valid_segment(Segment) ->
    %% Allow alphanumeric, underscore, hyphen
    is_valid_segment_chars(binary_to_list(Segment)).

is_valid_segment_chars([]) ->
    true;
is_valid_segment_chars([C | Rest]) when (C >= $a andalso C =< $z) orelse
                                          (C >= $A andalso C =< $Z) orelse
                                          (C >= $0 andalso C =< $9) orelse
                                          C =:= $_ orelse
                                          C =:= $- ->
    is_valid_segment_chars(Rest);
is_valid_segment_chars(_) ->
    false.

%% @doc Remove consecutive dots.
remove_double_dots(Binary) ->
    case binary:replace(Binary, <<"..">>, <<".">>, [global]) of
        Binary -> Binary;  % No change, done
        Modified -> remove_double_dots(Modified)  % Keep removing
    end.
