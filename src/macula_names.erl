%%%-------------------------------------------------------------------
%%% @doc
%%% Shared naming utilities for hierarchical dot-separated names.
%%% Used by both pub/sub topics and RPC procedure names.
%%% Supports DNS-style reverse notation: org.domain.service.method
%%% @end
%%%-------------------------------------------------------------------
-module(macula_names).

%% API
-export([
    validate/1,
    validate/2,
    normalize/1,
    namespace/1,
    segment_count/1
]).

%% Types
-type name() :: binary().
-type options() :: #{allow_wildcards => boolean()}.
-export_type([name/0, options/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Validate name syntax with default options (no wildcards).
-spec validate(name()) -> ok | {error, invalid_name}.
validate(Name) ->
    validate(Name, #{allow_wildcards => false}).

%% @doc Validate name syntax with options.
%% Valid names:
%% - Non-empty
%% - Segments separated by dots
%% - Segments contain alphanumeric, underscore, hyphen
%% - Optionally allow wildcards (* and #) for patterns
%% - No leading or trailing dots
%% - No double dots
-spec validate(name(), options()) -> ok | {error, invalid_name}.
validate(<<>>, _Opts) ->
    {error, invalid_name};
validate(Name, Opts) ->
    %% Check for leading/trailing dots
    case binary:first(Name) of
        $. -> {error, invalid_name};
        _ ->
            case binary:last(Name) of
                $. -> {error, invalid_name};
                _ -> validate_segments(Name, Opts)
            end
    end.

%% @doc Normalize name (lowercase, trim, remove double dots).
-spec normalize(name()) -> name().
normalize(Name) ->
    %% Trim whitespace
    Trimmed = string:trim(Name),

    %% Lowercase
    Lower = string:lowercase(Trimmed),

    %% Remove double dots
    remove_double_dots(Lower).

%% @doc Extract namespace (first segment).
-spec namespace(name()) -> binary().
namespace(<<>>) ->
    <<>>;
namespace(Name) ->
    case binary:split(Name, <<".">>) of
        [Namespace] -> Namespace;
        [Namespace, _Rest] -> Namespace
    end.

%% @doc Count number of segments in name.
-spec segment_count(name()) -> non_neg_integer().
segment_count(<<>>) ->
    0;
segment_count(Name) ->
    Segments = binary:split(Name, <<".">>, [global]),
    length(Segments).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Validate all segments contain valid characters.
validate_segments(Name, Opts) ->
    Segments = binary:split(Name, <<".">>, [global]),
    case lists:all(fun(Seg) -> is_valid_segment(Seg, Opts) end, Segments) of
        true -> ok;
        false -> {error, invalid_name}
    end.

%% @doc Check if segment contains only valid characters.
is_valid_segment(<<>>, _Opts) ->
    false;  % Empty segment not allowed
is_valid_segment(Segment, Opts) ->
    AllowWildcards = maps:get(allow_wildcards, Opts, false),
    is_valid_segment_chars(binary_to_list(Segment), AllowWildcards).

is_valid_segment_chars([], _AllowWildcards) ->
    true;
is_valid_segment_chars([C | Rest], AllowWildcards) when (C >= $a andalso C =< $z) orelse
                                                          (C >= $A andalso C =< $Z) orelse
                                                          (C >= $0 andalso C =< $9) orelse
                                                          C =:= $_ orelse
                                                          C =:= $- ->
    is_valid_segment_chars(Rest, AllowWildcards);
is_valid_segment_chars([C | Rest], true) when C =:= $* orelse C =:= $# ->
    %% Allow wildcards if option is set
    is_valid_segment_chars(Rest, true);
is_valid_segment_chars(_, _) ->
    false.

%% @doc Remove consecutive dots.
remove_double_dots(Binary) ->
    case binary:replace(Binary, <<"..">>, <<".">>, [global]) of
        Binary -> Binary;  % No change, done
        Modified -> remove_double_dots(Modified)  % Keep removing
    end.
