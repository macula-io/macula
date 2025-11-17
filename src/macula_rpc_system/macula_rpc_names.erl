%%%-------------------------------------------------------------------
%%% @doc
%%% Name validation and utilities for RPC procedures.
%%% Uses reverse DNS notation: org.domain.service.procedure
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_names).

%% API
-export([
    validate/1,
    matches/2,
    normalize/1,
    namespace/1,
    segment_count/1
]).

%% Types
-type name() :: binary().
-export_type([name/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Validate RPC procedure name syntax.
%% Valid names:
%% - Non-empty
%% - Segments separated by dots
%% - Segments contain alphanumeric, underscore, hyphen
%% - No leading or trailing dots
%% - No double dots
%% - No wildcards allowed (unlike topics)
-spec validate(name()) -> ok | {error, invalid_name}.
validate(Name) ->
    macula_names:validate(Name).

%% @doc Check if name matches pattern.
%% For now, only exact matching (no wildcards).
%% Future: Could add wildcard patterns if needed.
-spec matches(name(), name()) -> boolean().
matches(Name, Pattern) ->
    Name =:= Pattern.

%% @doc Normalize name (lowercase, trim, remove double dots).
-spec normalize(name()) -> name().
normalize(Name) ->
    macula_names:normalize(Name).

%% @doc Extract namespace (first segment).
-spec namespace(name()) -> binary().
namespace(Name) ->
    macula_names:namespace(Name).

%% @doc Count number of segments in name.
-spec segment_count(name()) -> non_neg_integer().
segment_count(Name) ->
    macula_names:segment_count(Name).
