%% @doc DID Parsing Cache Module.
%%
%% Provides high-performance caching for parsed DID (Decentralized Identifier)
%% results using `persistent_term' for fast reads. This avoids repeated parsing
%% of DIDs during authorization checks.
%%
%% == Usage ==
%%
%% ```
%% %% Parse and cache a DID (or get from cache)
%% {ok, Parsed} = macula_did_cache:get_or_parse(<<"did:macula:io.macula.rgfaber">>).
%% %% => {ok, #{<<"method">> => <<"macula">>,
%% %%           <<"identity">> => <<"io.macula.rgfaber">>,
%% %%           <<"parts">> => [<<"io">>, <<"macula">>, <<"rgfaber">>],
%% %%           <<"depth">> => 3}}
%%
%% %% Subsequent calls return cached result (no re-parsing)
%% {ok, Parsed} = macula_did_cache:get_or_parse(<<"did:macula:io.macula.rgfaber">>).
%%
%% %% Invalidate a specific DID from cache
%% ok = macula_did_cache:invalidate(<<"did:macula:io.macula.rgfaber">>).
%%
%% %% Clear entire cache
%% ok = macula_did_cache:clear().
%% '''
%%
%% == Performance ==
%%
%% `persistent_term' provides O(1) lookups with zero garbage collection impact,
%% making it ideal for frequently-accessed, rarely-changed data like parsed DIDs.
%%
%% Note: `persistent_term:put/2' triggers a global GC, so this cache is optimized
%% for read-heavy workloads where DIDs are parsed once and looked up many times.
%%
%% == Cache Key Format ==
%%
%% Keys are stored as `{macula_did_cache, DID}' tuples to avoid namespace conflicts.
%%
%% @author macula
-module(macula_did_cache).

%% API
-export([
    get_or_parse/1,
    invalidate/1,
    clear/0,
    cache_size/0
]).

%% Internal (exported for testing)
-export([
    cache_key/1
]).

%%====================================================================
%% Types
%%====================================================================

-type did() :: binary().
-type parsed_did() :: #{
    binary() => binary() | [binary()] | integer()
}.

-export_type([did/0, parsed_did/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get parsed DID from cache, or parse and cache if not present.
%%
%% This is the main entry point for cached DID parsing. If the DID has been
%% parsed before, returns the cached result. Otherwise, parses the DID,
%% caches the result, and returns it.
%%
%% Returns `{ok, ParsedDID}' on success, `{error, invalid_did}' if parsing fails.
-spec get_or_parse(DID :: did()) -> {ok, parsed_did()} | {error, invalid_did}.
get_or_parse(DID) when is_binary(DID) ->
    Key = cache_key(DID),
    case persistent_term:get(Key, undefined) of
        undefined ->
            parse_and_cache(DID, Key);
        Cached ->
            {ok, Cached}
    end;
get_or_parse(_) ->
    {error, invalid_did}.

%% @doc Invalidate (remove) a specific DID from the cache.
%%
%% Use this when a DID's parsing result may have changed (rare in practice,
%% as DIDs are typically immutable identifiers).
-spec invalidate(DID :: did()) -> ok.
invalidate(DID) when is_binary(DID) ->
    Key = cache_key(DID),
    _ = persistent_term:erase(Key),
    ok;
invalidate(_) ->
    ok.

%% @doc Clear all cached DIDs.
%%
%% Warning: This triggers a global GC for each erased term. Use sparingly.
-spec clear() -> ok.
clear() ->
    AllKeys = persistent_term:get(),
    lists:foreach(
        fun({Key, _Value}) ->
            case Key of
                {macula_did_cache, _DID} ->
                    persistent_term:erase(Key);
                _ ->
                    ok
            end
        end,
        AllKeys
    ),
    ok.

%% @doc Get the current number of cached DIDs.
%%
%% Useful for monitoring and debugging.
-spec cache_size() -> non_neg_integer().
cache_size() ->
    AllKeys = persistent_term:get(),
    length([Key || {Key, _Value} <- AllKeys, is_cache_key(Key)]).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Generate cache key for a DID.
-spec cache_key(DID :: did()) -> {macula_did_cache, did()}.
cache_key(DID) ->
    {macula_did_cache, DID}.

%% @private Check if a key is a DID cache key.
-spec is_cache_key(term()) -> boolean().
is_cache_key({macula_did_cache, _}) -> true;
is_cache_key(_) -> false.

%% @private Parse DID and cache the result.
-spec parse_and_cache(DID :: did(), Key :: {macula_did_cache, did()}) ->
    {ok, parsed_did()} | {error, invalid_did}.
parse_and_cache(DID, Key) ->
    case parse_did(DID) of
        {ok, Parsed} ->
            persistent_term:put(Key, Parsed),
            {ok, Parsed};
        {error, invalid_did} = Error ->
            Error
    end.

%%====================================================================
%% Inline DID Parsing (same as macula_authorization)
%%====================================================================

%% @private Parse a DID string and extract its components.
%% Identical to macula_authorization:parse_did/1 for consistency.
-spec parse_did(DID :: binary()) ->
    {ok, parsed_did()} | {error, invalid_did}.
parse_did(DID) when is_binary(DID) ->
    case binary:split(DID, <<":">>, [global]) of
        [<<"did">>, <<"macula">>, Identity] ->
            Parts = binary:split(Identity, <<".">>, [global]),
            {ok, #{
                <<"method">> => <<"macula">>,
                <<"identity">> => Identity,
                <<"parts">> => Parts,
                <<"depth">> => length(Parts)
            }};
        _ ->
            {error, invalid_did}
    end;
parse_did(_) ->
    {error, invalid_did}.
