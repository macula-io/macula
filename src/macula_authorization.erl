%% @doc Mesh Authorization Module for UCAN/DID-based access control.
%%
%% This module implements decentralized authorization for Macula mesh operations
%% using UCAN (User Controlled Authorization Networks) and DID (Decentralized
%% Identifiers). All DID parsing and UCAN validation is implemented inline
%% with no external dependencies.
%%
%% == Namespace Ownership Model ==
%%
%% DIDs map to namespaces they own:
%% - `did:macula:io.macula.rgfaber' owns `io.macula.rgfaber.*'
%% - Parent DIDs can access child namespaces (hierarchical)
%% - `did:macula:io.macula' can access everything in `io.macula.*'
%%
%% == Authorization Flow ==
%%
%% 1. Extract caller DID from connection/message
%% 2. Parse topic/procedure to extract namespace
%% 3. Check if caller owns namespace → Allow
%% 4. If not owner, check for valid UCAN grant → Allow/Deny
%%
%% == Public Topics ==
%%
%% Topics containing `.public.' segment are world-readable:
%% - `io.macula.rgfaber.public.announcements' → Anyone can subscribe
%% - Publishing still requires ownership or UCAN grant
%%
%% @author rgfaber
-module(macula_authorization).

%% Core authorization checks
-export([
    check_rpc_call/4,
    check_publish/4,
    check_subscribe/3,
    check_subscribe/4,
    check_announce/3
]).

%% Namespace operations
-export([
    extract_namespace/1,
    check_namespace_ownership/2,
    is_public_topic/1,
    is_ancestor_namespace/2
]).

%% UCAN operations
-export([
    validate_ucan_for_operation/4,
    check_capability_match/3
]).

%% DID operations
-export([
    resolve_caller_did/1,
    extract_identity_from_did/1
]).

%%====================================================================
%% Types
%%====================================================================

-type did() :: binary().
-type topic() :: binary().
-type procedure() :: binary().
-type namespace() :: binary().
-type ucan_token() :: binary() | undefined.
-type operation() :: binary().
-type auth_opts() :: #{atom() => term()}.

-type auth_result() :: {ok, authorized} | {error, auth_error()}.
-type auth_error() :: unauthorized
                    | invalid_ucan
                    | expired_ucan
                    | insufficient_capability
                    | invalid_did
                    | namespace_mismatch.

-export_type([did/0, auth_result/0, auth_error/0]).

%%====================================================================
%% Core Authorization Checks
%%====================================================================

%% @doc Check if caller is authorized to make an RPC call.
%%
%% Authorization succeeds if:
%% 1. Caller owns the procedure's namespace, OR
%% 2. Caller is ancestor of the namespace (parent access), OR
%% 3. Caller has a valid UCAN with `mesh:call' capability
-spec check_rpc_call(CallerDID :: did(), Procedure :: procedure(),
                     UcanToken :: ucan_token(), Opts :: auth_opts()) ->
    auth_result().
check_rpc_call(CallerDID, Procedure, UcanToken, _Opts) ->
    Namespace = extract_namespace(Procedure),
    check_with_ucan_fallback(CallerDID, Namespace, Procedure,
                             UcanToken, <<"mesh:call">>).

%% @doc Check if caller is authorized to publish to a topic.
%%
%% Publishing requires ownership or UCAN grant.
%% Public topics do NOT grant publish rights.
-spec check_publish(CallerDID :: did(), Topic :: topic(),
                    UcanToken :: ucan_token(), Opts :: auth_opts()) ->
    auth_result().
check_publish(CallerDID, Topic, UcanToken, _Opts) ->
    Namespace = extract_namespace(Topic),
    check_with_ucan_fallback(CallerDID, Namespace, Topic,
                             UcanToken, <<"mesh:publish">>).

%% @doc Check if caller is authorized to subscribe to a topic.
%%
%% Authorization succeeds if:
%% 1. Topic is public (contains `.public.'), OR
%% 2. Caller owns the namespace, OR
%% 3. Caller is ancestor, OR
%% 4. Caller has UCAN with `mesh:subscribe' capability
-spec check_subscribe(CallerDID :: did(), Topic :: topic(),
                      Opts :: auth_opts()) ->
    auth_result().
check_subscribe(_CallerDID, Topic, _Opts) when is_binary(Topic) ->
    case is_public_topic(Topic) of
        true ->
            {ok, authorized};
        false ->
            %% For subscriptions without UCAN, check ownership
            %% Note: UCAN check requires token which isn't in this signature
            %% Subscriptions to non-public topics require ownership
            {error, unauthorized}
    end.

%% @doc Check if caller is authorized to subscribe with UCAN support.
%% Full version with UCAN token parameter.
-spec check_subscribe(CallerDID :: did(), Topic :: topic(),
                      UcanToken :: ucan_token(), Opts :: auth_opts()) ->
    auth_result().
check_subscribe(CallerDID, Topic, UcanToken, _Opts) ->
    case is_public_topic(Topic) of
        true ->
            {ok, authorized};
        false ->
            Namespace = extract_namespace(Topic),
            check_with_ucan_fallback(CallerDID, Namespace, Topic,
                                     UcanToken, <<"mesh:subscribe">>)
    end.

%% @doc Check if caller is authorized to announce/declare a procedure.
%%
%% Announcing ALWAYS requires namespace ownership.
%% UCAN grants cannot give announce rights (prevents namespace hijacking).
-spec check_announce(CallerDID :: did(), Procedure :: procedure(),
                     Opts :: auth_opts()) ->
    auth_result().
check_announce(CallerDID, Procedure, _Opts) ->
    Namespace = extract_namespace(Procedure),
    case check_namespace_ownership(CallerDID, Namespace) of
        {ok, owner} -> {ok, authorized};
        {ok, ancestor} -> {ok, authorized};
        {error, not_owner} -> {error, unauthorized}
    end.

%%====================================================================
%% Namespace Operations
%%====================================================================

%% @doc Extract namespace from a topic or procedure.
%%
%% Namespace is the first 3 segments of a dotted path:
%% - `io.macula.rgfaber.place_order' → `io.macula.rgfaber'
%% - `io.macula.public.events' → `io.macula.public'
%% - Short topics return the topic itself as namespace.
-spec extract_namespace(TopicOrProcedure :: binary()) -> namespace().
extract_namespace(TopicOrProcedure) when is_binary(TopicOrProcedure) ->
    Parts = binary:split(TopicOrProcedure, <<".">>, [global]),
    case length(Parts) of
        N when N >= 3 ->
            [A, B, C | _] = Parts,
            <<A/binary, ".", B/binary, ".", C/binary>>;
        _ ->
            %% Short topic = namespace itself
            TopicOrProcedure
    end.

%% @doc Check if caller DID owns a namespace.
%%
%% Returns:
%% - `{ok, owner}' if DID identity matches namespace exactly
%% - `{ok, ancestor}' if DID identity is parent of namespace
%% - `{error, not_owner}' otherwise
-spec check_namespace_ownership(CallerDID :: did(), Namespace :: namespace()) ->
    {ok, owner | ancestor} | {error, not_owner}.
check_namespace_ownership(CallerDID, Namespace) ->
    case extract_identity_from_did(CallerDID) of
        {ok, Identity} ->
            check_identity_ownership(Identity, Namespace);
        {error, _} ->
            {error, not_owner}
    end.

%% @private Check identity ownership of namespace
-spec check_identity_ownership(Identity :: binary(), Namespace :: namespace()) ->
    {ok, owner | ancestor} | {error, not_owner}.
check_identity_ownership(Identity, Namespace) when Identity =:= Namespace ->
    {ok, owner};
check_identity_ownership(Identity, Namespace) ->
    case is_ancestor_namespace(Identity, Namespace) of
        true -> {ok, ancestor};
        false -> {error, not_owner}
    end.

%% @doc Check if one namespace is an ancestor of another.
%%
%% `io.macula' is ancestor of `io.macula.rgfaber'
%% `io.macula.rgfaber' is ancestor of `io.macula.rgfaber.services'
-spec is_ancestor_namespace(Parent :: namespace(), Child :: namespace()) -> boolean().
is_ancestor_namespace(Parent, Child) when Parent =:= Child ->
    false;
is_ancestor_namespace(Parent, Child) ->
    Prefix = <<Parent/binary, ".">>,
    case binary:match(Child, Prefix) of
        {0, _} -> true;
        _ -> false
    end.

%% @doc Check if a topic is public (contains `.public.' segment).
%%
%% Public topics allow subscription without ownership or UCAN.
-spec is_public_topic(Topic :: topic()) -> boolean().
is_public_topic(Topic) when is_binary(Topic) ->
    case binary:match(Topic, <<".public.">>) of
        {_, _} -> true;
        nomatch ->
            %% Also check if topic starts with public.
            case binary:match(Topic, <<"public.">>) of
                {0, _} -> true;
                _ -> false
            end
    end.

%%====================================================================
%% UCAN Operations
%%====================================================================

%% @doc Validate a UCAN token for a specific operation.
%%
%% Checks:
%% 1. Token is well-formed and not expired
%% 2. Audience matches caller DID
%% 3. Token has required capability for operation
-spec validate_ucan_for_operation(UcanToken :: ucan_token(),
                                   CallerDID :: did(),
                                   Resource :: binary(),
                                   Operation :: operation()) ->
    auth_result().
validate_ucan_for_operation(undefined, _CallerDID, _Resource, _Operation) ->
    {error, unauthorized};
validate_ucan_for_operation(UcanToken, CallerDID, Resource, Operation) ->
    %% First decode without verification to get audience
    case decode_ucan(UcanToken) of
        {ok, Payload} ->
            %% Check audience matches caller
            case maps:get(<<"aud">>, Payload, undefined) of
                CallerDID ->
                    %% Check capabilities
                    validate_ucan_capabilities(Payload, Resource, Operation);
                _ ->
                    {error, unauthorized}
            end;
        {error, _} ->
            {error, invalid_ucan}
    end.

%% @private Validate UCAN capabilities
-spec validate_ucan_capabilities(Payload :: map(), Resource :: binary(),
                                  Operation :: operation()) ->
    auth_result().
validate_ucan_capabilities(Payload, Resource, Operation) ->
    %% Check expiration
    case is_ucan_expired(Payload) of
        true ->
            {error, expired_ucan};
        false ->
            %% Check capabilities
            Capabilities = maps:get(<<"cap">>, Payload, []),
            case check_capability_list(Capabilities, Resource, Operation) of
                true -> {ok, authorized};
                false -> {error, insufficient_capability}
            end
    end.

%% @private Check if any capability in the list grants access
-spec check_capability_list(Capabilities :: [map()], Resource :: binary(),
                            Operation :: operation()) -> boolean().
check_capability_list([], _Resource, _Operation) ->
    false;
check_capability_list([Cap | Rest], Resource, Operation) ->
    case check_capability_match(Cap, Resource, Operation) of
        true -> true;
        false -> check_capability_list(Rest, Resource, Operation)
    end.

%% @doc Check if a capability grants access to a resource for an operation.
%%
%% Capability format: `#{<<"with">> => Resource, <<"can">> => Operation}'
%%
%% Wildcards supported:
%% - `io.macula.rgfaber.*' matches any resource in namespace
%% - `mesh:*' matches any mesh operation
-spec check_capability_match(Capability :: map(), Resource :: binary(),
                             Operation :: operation()) -> boolean().
check_capability_match(Capability, Resource, Operation) when is_map(Capability) ->
    CapWith = maps:get(<<"with">>, Capability, <<>>),
    CapCan = maps:get(<<"can">>, Capability, <<>>),
    match_resource(CapWith, Resource) andalso match_operation(CapCan, Operation);
check_capability_match(_, _, _) ->
    false.

%% @private Match resource pattern against actual resource
-spec match_resource(Pattern :: binary(), Resource :: binary()) -> boolean().
match_resource(Pattern, Resource) when Pattern =:= Resource ->
    true;
match_resource(Pattern, Resource) ->
    %% Check wildcard pattern
    case binary:match(Pattern, <<"*">>) of
        nomatch ->
            false;
        {Pos, 1} ->
            %% Pattern like "io.macula.rgfaber.*"
            Prefix = binary:part(Pattern, 0, Pos),
            case binary:match(Resource, Prefix) of
                {0, _} -> true;
                _ -> false
            end
    end.

%% @private Match operation pattern against actual operation
-spec match_operation(Pattern :: binary(), Operation :: binary()) -> boolean().
match_operation(Pattern, Operation) when Pattern =:= Operation ->
    true;
match_operation(<<"mesh:*">>, <<"mesh:", _/binary>>) ->
    true;
match_operation(<<"*">>, _Operation) ->
    true;
match_operation(_, _) ->
    false.

%%====================================================================
%% DID Operations
%%====================================================================

%% @doc Resolve and validate a caller DID.
%%
%% Parses the DID and returns the parsed components if valid.
%% Uses `macula_did_cache' for performance.
-spec resolve_caller_did(CallerDID :: did()) ->
    {ok, Components :: map()} | {error, invalid_did}.
resolve_caller_did(CallerDID) when is_binary(CallerDID) ->
    macula_did_cache:get_or_parse(CallerDID);
resolve_caller_did(_) ->
    {error, invalid_did}.

%% @doc Extract the identity portion from a DID.
%%
%% `did:macula:io.macula.rgfaber' → `io.macula.rgfaber'
%%
%% Uses `macula_did_cache' for performance - repeated lookups for the same
%% DID return cached results without re-parsing.
-spec extract_identity_from_did(DID :: did()) ->
    {ok, Identity :: binary()} | {error, invalid_did}.
extract_identity_from_did(DID) when is_binary(DID) ->
    case macula_did_cache:get_or_parse(DID) of
        {ok, #{<<"identity">> := Identity}} ->
            {ok, Identity};
        _ ->
            {error, invalid_did}
    end;
extract_identity_from_did(_) ->
    {error, invalid_did}.

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @private Check ownership first, fall back to UCAN validation
-spec check_with_ucan_fallback(CallerDID :: did(), Namespace :: namespace(),
                                Resource :: binary(), UcanToken :: ucan_token(),
                                Operation :: operation()) ->
    auth_result().
check_with_ucan_fallback(CallerDID, Namespace, Resource, UcanToken, Operation) ->
    case check_namespace_ownership(CallerDID, Namespace) of
        {ok, owner} ->
            {ok, authorized};
        {ok, ancestor} ->
            {ok, authorized};
        {error, not_owner} ->
            %% Try UCAN validation
            validate_ucan_for_operation(UcanToken, CallerDID, Resource, Operation)
    end.

%%====================================================================
%% DID Parsing (delegated to macula_did_cache)
%%====================================================================

%% DID parsing is now handled by macula_did_cache for performance.
%% See macula_did_cache:get_or_parse/1 for cached DID parsing.

%%====================================================================
%% Inline UCAN Decoding (no external dependency)
%%====================================================================

%% @private Decode a UCAN token (JWT format) without signature verification.
%% WARNING: This does NOT verify the signature - for authorization checks
%% the token signature should be verified separately.
-spec decode_ucan(Token :: binary()) -> {ok, Payload :: map()} | {error, term()}.
decode_ucan(Token) when is_binary(Token) ->
    case binary:split(Token, <<".">>, [global]) of
        [_HeaderB64, PayloadB64, _SignatureB64] ->
            case base64_url_decode(PayloadB64) of
                {ok, PayloadJson} ->
                    case json_decode(PayloadJson) of
                        {ok, Payload} when is_map(Payload) -> {ok, Payload};
                        _ -> {error, invalid_token}
                    end;
                _ ->
                    {error, invalid_token}
            end;
        _ ->
            {error, invalid_token}
    end;
decode_ucan(_) ->
    {error, invalid_token}.

%% @private Check if UCAN is expired based on payload exp field
-spec is_ucan_expired(Payload :: map()) -> boolean().
is_ucan_expired(Payload) when is_map(Payload) ->
    case maps:get(<<"exp">>, Payload, null) of
        null -> false;
        Exp when is_integer(Exp) ->
            Now = erlang:system_time(second),
            Now > Exp;
        _ -> false
    end.

%% @private URL-safe base64 decode
-spec base64_url_decode(Encoded :: binary()) -> {ok, binary()} | {error, term()}.
base64_url_decode(Encoded) ->
    try
        %% Convert URL-safe to standard base64
        B64Std = binary:replace(
            binary:replace(Encoded, <<"-">>, <<"+">>, [global]),
            <<"_">>, <<"/">>, [global]),
        %% Add padding if needed
        Padded = case byte_size(B64Std) rem 4 of
            0 -> B64Std;
            2 -> <<B64Std/binary, "==">>;
            3 -> <<B64Std/binary, "=">>
        end,
        {ok, base64:decode(Padded)}
    catch
        _:_ -> {error, invalid_base64}
    end.

%% @private Simple JSON decoding for UCAN payloads
-spec json_decode(Binary :: binary()) -> {ok, term()} | {error, term()}.
json_decode(Binary) when is_binary(Binary) ->
    try
        %% Use OTP 27+ built-in json module if available, otherwise simple parser
        case erlang:function_exported(json, decode, 1) of
            true ->
                {ok, json:decode(Binary)};
            false ->
                {ok, simple_json_decode(Binary)}
        end
    catch
        _:_ -> {error, invalid_json}
    end.

%% @private Simple JSON object decoder (for environments without json module)
-spec simple_json_decode(Binary :: binary()) -> map().
simple_json_decode(Binary) ->
    {Value, _Rest} = decode_value(skip_ws(Binary)),
    Value.

%% @private Decode JSON value
decode_value(<<"null", Rest/binary>>) -> {null, Rest};
decode_value(<<"true", Rest/binary>>) -> {true, Rest};
decode_value(<<"false", Rest/binary>>) -> {false, Rest};
decode_value(<<"\"", Rest/binary>>) -> decode_string(Rest, <<>>);
decode_value(<<"[", Rest/binary>>) -> decode_array(skip_ws(Rest), []);
decode_value(<<"{", Rest/binary>>) -> decode_object(skip_ws(Rest), #{});
decode_value(<<C, _/binary>> = Bin) when C =:= $- orelse (C >= $0 andalso C =< $9) ->
    decode_number(Bin).

%% @private Decode JSON string
decode_string(<<"\\\"", Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, "\"">>);
decode_string(<<"\\\\", Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, "\\">>);
decode_string(<<"\\n", Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, "\n">>);
decode_string(<<"\\t", Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, "\t">>);
decode_string(<<"\"", Rest/binary>>, Acc) -> {Acc, Rest};
decode_string(<<C, Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, C>>).

%% @private Decode JSON array
decode_array(<<"]", Rest/binary>>, Acc) -> {lists:reverse(Acc), Rest};
decode_array(Bin, Acc) ->
    {Value, Rest} = decode_value(Bin),
    Rest2 = skip_ws(Rest),
    case Rest2 of
        <<",", Rest3/binary>> -> decode_array(skip_ws(Rest3), [Value | Acc]);
        <<"]", Rest3/binary>> -> {lists:reverse([Value | Acc]), Rest3}
    end.

%% @private Decode JSON object
decode_object(<<"}", Rest/binary>>, Acc) -> {Acc, Rest};
decode_object(<<"\"", Rest/binary>>, Acc) ->
    {Key, Rest2} = decode_string(Rest, <<>>),
    <<":", Rest3/binary>> = skip_ws(Rest2),
    {Value, Rest4} = decode_value(skip_ws(Rest3)),
    Rest5 = skip_ws(Rest4),
    case Rest5 of
        <<",", Rest6/binary>> -> decode_object(skip_ws(Rest6), maps:put(Key, Value, Acc));
        <<"}", Rest6/binary>> -> {maps:put(Key, Value, Acc), Rest6}
    end.

%% @private Decode JSON number
decode_number(Bin) ->
    {NumStr, Rest} = take_number(Bin, <<>>),
    Num = case binary:match(NumStr, [<<".">>, <<"e">>, <<"E">>]) of
        nomatch -> binary_to_integer(NumStr);
        _ -> binary_to_float(NumStr)
    end,
    {Num, Rest}.

%% @private Take number characters
take_number(<<C, Rest/binary>>, Acc) when C =:= $- orelse C =:= $+ orelse C =:= $.
    orelse C =:= $e orelse C =:= $E orelse (C >= $0 andalso C =< $9) ->
    take_number(Rest, <<Acc/binary, C>>);
take_number(Rest, Acc) -> {Acc, Rest}.

%% @private Skip whitespace
skip_ws(<<C, Rest/binary>>) when C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r ->
    skip_ws(Rest);
skip_ws(Bin) -> Bin.
