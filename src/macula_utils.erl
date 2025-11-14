%%%-------------------------------------------------------------------
%%% @doc
%%% Common utility functions for Macula.
%%%
%%% This module contains pure utility functions used across the
%%% Macula codebase to improve testability and eliminate duplication.
%%%
%%% All functions in this module are pure (no side effects) and
%%% can be tested independently.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_utils).

%% API
-export([
    parse_url/1,
    parse_host_port/2,
    generate_node_id/0,
    next_message_id/1,
    ensure_binary/1,
    normalize_provider/1,
    topic_matches/5,
    encode_json/1,
    decode_json/1
]).

%%%===================================================================
%%% URL Parsing
%%%===================================================================

%% @doc Parse URL to extract host and port.
-spec parse_url(binary()) -> {string(), inet:port_number()}.
parse_url(Url) when is_binary(Url) ->
    parse_url(binary_to_list(Url));
parse_url("https://" ++ Rest) ->
    parse_host_port(Rest, 443);
parse_url("http://" ++ Rest) ->
    parse_host_port(Rest, 80);
parse_url(Rest) ->
    parse_host_port(Rest, 443).

%% @doc Parse host:port string with default port.
-spec parse_host_port(string(), inet:port_number()) -> {string(), inet:port_number()}.
parse_host_port(HostPort, DefaultPort) ->
    parse_host_port_parts(string:split(HostPort, ":"), DefaultPort).

parse_host_port_parts([Host], DefaultPort) ->
    {Host, DefaultPort};
parse_host_port_parts([Host, PortStr], _DefaultPort) ->
    Port = list_to_integer(string:trim(PortStr, trailing, "/")),
    {Host, Port}.

%%%===================================================================
%%% ID Generation
%%%===================================================================

%% @doc Generate a random node ID.
-spec generate_node_id() -> binary().
generate_node_id() ->
    %% 32-byte random ID
    crypto:strong_rand_bytes(32).

%% @doc Get next message ID from counter.
%% Returns {MessageId, NewCounter}.
-spec next_message_id(non_neg_integer()) -> {binary(), non_neg_integer()}.
next_message_id(Counter) ->
    NewCounter = Counter + 1,
    %% 16-byte message ID from counter
    MsgId = <<NewCounter:128>>,
    {MsgId, NewCounter}.

%%%===================================================================
%%% Type Conversion
%%%===================================================================

%% @doc Ensure value is binary.
-spec ensure_binary(binary() | list() | atom()) -> binary().
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(A) when is_atom(A) -> atom_to_binary(A).

%%%===================================================================
%%% Provider Normalization
%%%===================================================================

%% @doc Normalize provider map from binary keys to atom keys.
-spec normalize_provider(map()) -> map().
normalize_provider(Provider) when is_map(Provider) ->
    #{
        node_id => maps:get(<<"node_id">>, Provider),
        endpoint => maps:get(<<"endpoint">>, Provider),
        metadata => maps:get(<<"metadata">>, Provider, #{}),
        ttl => maps:get(<<"ttl">>, Provider, 300)
    }.

%%%===================================================================
%%% Topic Matching
%%%===================================================================

%% @doc Check if a published topic matches a subscription topic pattern.
%% Supports configurable wildcards (defaults: dot-separated with * and **):
%%   - WildcardSingle (e.g., '*') matches a single segment
%%   - WildcardMulti (e.g., '**') matches multiple segments
%%   - Separator (e.g., '.') splits topic into segments
%%   - exact match otherwise
-spec topic_matches(binary() | list(), binary() | list(), binary(), binary(), binary()) -> boolean().
topic_matches(Pattern, Topic, Separator, WildcardSingle, WildcardMulti) when is_binary(Pattern), is_binary(Topic) ->
    case {binary:match(Pattern, WildcardSingle), binary:match(Pattern, WildcardMulti)} of
        {nomatch, nomatch} ->
            %% No wildcards - exact match
            Pattern =:= Topic;
        _ ->
            %% Has wildcards - do pattern matching
            PatternParts = binary:split(Pattern, Separator, [global]),
            TopicParts = binary:split(Topic, Separator, [global]),
            match_parts(PatternParts, TopicParts, WildcardSingle, WildcardMulti)
    end;
topic_matches(Pattern, Topic, Separator, WildcardSingle, WildcardMulti) when is_list(Pattern) ->
    topic_matches(list_to_binary(Pattern), Topic, Separator, WildcardSingle, WildcardMulti);
topic_matches(Pattern, Topic, Separator, WildcardSingle, WildcardMulti) when is_list(Topic) ->
    topic_matches(Pattern, list_to_binary(Topic), Separator, WildcardSingle, WildcardMulti).

%% @doc Match topic parts against pattern parts.
-spec match_parts(list(binary()), list(binary()), binary(), binary()) -> boolean().
match_parts([], [], _WildcardSingle, _WildcardMulti) ->
    true;
match_parts([WildcardMulti], _TopicParts, _WildcardSingle, WildcardMulti) ->
    %% Multi-level wildcard at end matches everything remaining
    true;
match_parts([WildcardMulti | PatternRest], TopicParts, WildcardSingle, WildcardMulti) ->
    %% Multi-level wildcard in middle - try matching rest of pattern at each position
    try_multi_wildcard_match(PatternRest, TopicParts, WildcardSingle, WildcardMulti);
match_parts([WildcardSingle | PatternRest], [_TopicPart | TopicRest], WildcardSingle, WildcardMulti) ->
    %% Single-level wildcard matches one segment
    match_parts(PatternRest, TopicRest, WildcardSingle, WildcardMulti);
match_parts([PatternPart | PatternRest], [PatternPart | TopicRest], WildcardSingle, WildcardMulti) ->
    %% Exact match - continue matching
    match_parts(PatternRest, TopicRest, WildcardSingle, WildcardMulti);
match_parts(_Pattern, _Topic, _WildcardSingle, _WildcardMulti) ->
    %% Length mismatch
    false.

%% @private
%% @doc Try matching pattern against topic at each possible position (for ** wildcard).
-spec try_multi_wildcard_match(list(binary()), list(binary()), binary(), binary()) -> boolean().
try_multi_wildcard_match(Pattern, [], WildcardSingle, WildcardMulti) ->
    %% No more topic parts - check if pattern can match empty
    match_parts(Pattern, [], WildcardSingle, WildcardMulti);
try_multi_wildcard_match(Pattern, [_TopicPart | TopicRest] = Topic, WildcardSingle, WildcardMulti) ->
    %% Try matching at current position or skip topic part and try again
    match_parts(Pattern, Topic, WildcardSingle, WildcardMulti) orelse
        try_multi_wildcard_match(Pattern, TopicRest, WildcardSingle, WildcardMulti).

%%%===================================================================
%%% JSON Encoding/Decoding
%%%===================================================================

%% @doc Encode map/list to JSON binary.
-spec encode_json(map() | list()) -> binary().
encode_json(Data) ->
    iolist_to_binary(json:encode(Data)).

%% @doc Decode JSON binary to map/list.
-spec decode_json(binary()) -> map() | list().
decode_json(Binary) ->
    json:decode(Binary).
