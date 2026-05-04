%% @doc Decentralized Identifier (DID) operations for Macula mesh.
%%
%% This module provides DID document creation, parsing, and verification
%% for the Macula identity hierarchy. It uses Rust NIFs when available,
%% falling back to pure Erlang implementations otherwise.
%%
%% == DID Format ==
%%
%% Macula uses the `did:macula:` method with hierarchical identities:
%% - Realm: `did:macula:io.macula'
%% - Organization: `did:macula:io.macula.{org}'
%% - Application: `did:macula:io.macula.{org}.{app}'
%%
%% == DID Document Structure ==
%%
%% DID documents follow W3C DID Core specification with Ed25519 keys:
%% ```
%% #{
%%   &lt;&lt;"@context"&gt;&gt; => [&lt;&lt;"https://www.w3.org/ns/did/v1"&gt;&gt;],
%%   &lt;&lt;"id"&gt;&gt; => &lt;&lt;"did:macula:io.macula.acme"&gt;&gt;,
%%   &lt;&lt;"controller"&gt;&gt; => &lt;&lt;"did:macula:io.macula"&gt;&gt;,
%%   &lt;&lt;"verificationMethod"&gt;&gt; => [...],
%%   &lt;&lt;"authentication"&gt;&gt; => [...],
%%   &lt;&lt;"assertionMethod"&gt;&gt; => [...]
%% }
%% '''
%%
%% @author rgfaber
-module(macula_did_nif).

%% API
-export([
    create_document/2,
    parse_document/1,
    extract_public_key/1,
    get_did/1,
    get_controller/1,
    verify_controller/2,
    parse_did/1,
    is_descendant/2,
    is_nif_loaded/0
]).

%% NIF stubs
-export([
    nif_create_document/2,
    nif_parse_document/1,
    nif_extract_public_key/1,
    nif_get_did/1,
    nif_get_controller/1,
    nif_verify_controller/2,
    nif_parse_did/1,
    nif_is_descendant/2
]).

-on_load(init/0).

-define(NIF_LOADED_KEY, macula_did_nif_loaded).

%%====================================================================
%% Init
%%====================================================================

init() ->
    PrivDir = case code:priv_dir(macula) of
        {error, _} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join(filename:dirname(filename:dirname(Filename)), "priv");
                _ ->
                    "priv"
            end;
        Dir ->
            Dir
    end,
    Path = filename:join(PrivDir, "macula_did_nif"),
    case erlang:load_nif(Path, 0) of
        ok ->
            persistent_term:put(?NIF_LOADED_KEY, true),
            ok;
        {error, {reload, _}} ->
            persistent_term:put(?NIF_LOADED_KEY, true),
            ok;
        {error, _Reason} ->
            %% NIF not available, will use Erlang fallbacks
            ok
    end.

%%====================================================================
%% API
%%====================================================================

%% @doc Check if the NIF is loaded.
-spec is_nif_loaded() -> boolean().
is_nif_loaded() ->
    persistent_term:get(?NIF_LOADED_KEY, false).

%% @doc Create a new DID Document.
%% Returns a JSON-encoded DID document.
-spec create_document(Did :: binary(), PublicKey :: binary()) ->
    {ok, DocumentJson :: binary()} | {error, atom()}.
create_document(Did, PublicKey) ->
    case is_nif_loaded() of
        true -> transform_nif_result(nif_create_document(Did, PublicKey));
        false -> erlang_create_document(Did, PublicKey)
    end.

%% @doc Parse and validate a DID Document from JSON.
-spec parse_document(DocumentJson :: binary()) ->
    {ok, DocumentJson :: binary()} | {error, atom()}.
parse_document(DocumentJson) ->
    case is_nif_loaded() of
        true -> transform_nif_result(nif_parse_document(DocumentJson));
        false -> erlang_parse_document(DocumentJson)
    end.

%% @doc Extract the public key from a DID Document.
%% Returns the 32-byte Ed25519 public key.
-spec extract_public_key(DocumentJson :: binary()) ->
    {ok, PublicKey :: binary()} | {error, atom()}.
extract_public_key(DocumentJson) ->
    case is_nif_loaded() of
        true -> transform_nif_result(nif_extract_public_key(DocumentJson));
        false -> erlang_extract_public_key(DocumentJson)
    end.

%% @doc Get the DID from a DID Document.
-spec get_did(DocumentJson :: binary()) ->
    {ok, Did :: binary()} | {error, atom()}.
get_did(DocumentJson) ->
    case is_nif_loaded() of
        true -> transform_nif_result(nif_get_did(DocumentJson));
        false -> erlang_get_did(DocumentJson)
    end.

%% @doc Get the controller DID from a DID Document.
%% Returns the controller, or the DID itself if self-controlled.
-spec get_controller(DocumentJson :: binary()) ->
    {ok, Controller :: binary()} | {error, atom()}.
get_controller(DocumentJson) ->
    case is_nif_loaded() of
        true -> transform_nif_result(nif_get_controller(DocumentJson));
        false -> erlang_get_controller(DocumentJson)
    end.

%% @doc Verify that a DID Document is controlled by the expected controller.
-spec verify_controller(DocumentJson :: binary(), ExpectedController :: binary()) ->
    ok | {error, atom()}.
verify_controller(DocumentJson, ExpectedController) ->
    case is_nif_loaded() of
        true ->
            case nif_verify_controller(DocumentJson, ExpectedController) of
                ok -> ok;
                Err -> {error, Err}
            end;
        false ->
            erlang_verify_controller(DocumentJson, ExpectedController)
    end.

%% @doc Parse a DID string and extract its components.
%% Returns a map with method, identity, parts, and depth.
-spec parse_did(Did :: binary()) ->
    {ok, Components :: map()} | {error, atom()}.
parse_did(Did) ->
    case is_nif_loaded() of
        true ->
            case nif_parse_did(Did) of
                {ok, Json} -> {ok, json_decode(Json)};
                {invalid_did, _} -> {error, invalid_did}
            end;
        false ->
            erlang_parse_did(Did)
    end.

%% @private Transform NIF result to standard format
transform_nif_result({ok, Result}) -> {ok, Result};
transform_nif_result({invalid_did, _}) -> {error, invalid_did};
transform_nif_result({invalid_public_key, _}) -> {error, invalid_public_key};
transform_nif_result({invalid_document, _}) -> {error, invalid_document};
transform_nif_result({not_found, _}) -> {error, not_found};
transform_nif_result({controller_mismatch, _}) -> {error, controller_mismatch};
transform_nif_result({malformed_json, _}) -> {error, malformed_json}.

%% @doc Check if one DID is a descendant of another.
-spec is_descendant(ChildDid :: binary(), ParentDid :: binary()) -> boolean().
is_descendant(ChildDid, ParentDid) ->
    case is_nif_loaded() of
        true -> nif_is_descendant(ChildDid, ParentDid);
        false -> erlang_is_descendant(ChildDid, ParentDid)
    end.

%%====================================================================
%% NIF Stubs (replaced when NIF loads)
%%====================================================================

nif_create_document(_Did, _PublicKey) ->
    erlang:nif_error(nif_not_loaded).

nif_parse_document(_DocumentJson) ->
    erlang:nif_error(nif_not_loaded).

nif_extract_public_key(_DocumentJson) ->
    erlang:nif_error(nif_not_loaded).

nif_get_did(_DocumentJson) ->
    erlang:nif_error(nif_not_loaded).

nif_get_controller(_DocumentJson) ->
    erlang:nif_error(nif_not_loaded).

nif_verify_controller(_DocumentJson, _ExpectedController) ->
    erlang:nif_error(nif_not_loaded).

nif_parse_did(_Did) ->
    erlang:nif_error(nif_not_loaded).

nif_is_descendant(_ChildDid, _ParentDid) ->
    erlang:nif_error(nif_not_loaded).

%%====================================================================
%% Pure Erlang Fallbacks
%%====================================================================

%% @private Create DID document using pure Erlang
erlang_create_document(Did, PublicKey) when byte_size(PublicKey) =:= 32 ->
    case binary:match(Did, <<"did:macula:">>) of
        {0, _} ->
            DidStr = binary_to_list(Did),
            KeyId = <<Did/binary, "#key-1">>,

            %% Encode public key as multibase (z + base58btc of multicodec + key)
            MulticodecKey = <<16#ed, 16#01, PublicKey/binary>>,
            MultibaseKey = <<"z", (base58_encode(MulticodecKey))/binary>>,

            %% Determine controller (parent in hierarchy)
            Controller = get_parent_did_erlang(DidStr),

            Doc = #{
                <<"@context">> => [<<"https://www.w3.org/ns/did/v1">>],
                <<"id">> => Did,
                <<"controller">> => Controller,
                <<"verificationMethod">> => [#{
                    <<"id">> => KeyId,
                    <<"type">> => <<"Ed25519VerificationKey2020">>,
                    <<"controller">> => Did,
                    <<"publicKeyMultibase">> => MultibaseKey
                }],
                <<"authentication">> => [KeyId],
                <<"assertionMethod">> => [KeyId],
                <<"capabilityInvocation">> => [KeyId],
                <<"capabilityDelegation">> => [KeyId]
            },

            %% Remove null controller for root
            Doc2 = case Controller of
                null -> maps:remove(<<"controller">>, Doc);
                _ -> Doc
            end,

            {ok, json_encode(Doc2)};
        _ ->
            {error, invalid_did}
    end;
erlang_create_document(_Did, _PublicKey) ->
    {error, invalid_public_key}.

%% @private Parse DID document
erlang_parse_document(DocumentJson) ->
    try
        _ = json_decode(DocumentJson),
        {ok, DocumentJson}
    catch
        _:_ -> {error, invalid_document}
    end.

%% @private Extract public key from DID document
erlang_extract_public_key(DocumentJson) ->
    try
        Doc = json_decode(DocumentJson),
        [VM | _] = maps:get(<<"verificationMethod">>, Doc),
        MultibaseKey = maps:get(<<"publicKeyMultibase">>, VM),
        <<"z", Base58Key/binary>> = MultibaseKey,
        Decoded = base58_decode(Base58Key),
        %% Check multicodec prefix (0xed01 for Ed25519)
        <<16#ed, 16#01, PublicKey:32/binary>> = Decoded,
        {ok, PublicKey}
    catch
        _:_ -> {error, invalid_document}
    end.

%% @private Get DID from document
erlang_get_did(DocumentJson) ->
    try
        Doc = json_decode(DocumentJson),
        {ok, maps:get(<<"id">>, Doc)}
    catch
        _:_ -> {error, invalid_document}
    end.

%% @private Get controller from document
erlang_get_controller(DocumentJson) ->
    try
        Doc = json_decode(DocumentJson),
        Did = maps:get(<<"id">>, Doc),
        Controller = maps:get(<<"controller">>, Doc, Did),
        {ok, Controller}
    catch
        _:_ -> {error, invalid_document}
    end.

%% @private Verify controller
erlang_verify_controller(DocumentJson, ExpectedController) ->
    case erlang_get_controller(DocumentJson) of
        {ok, ExpectedController} -> ok;
        {ok, _Other} -> {error, controller_mismatch};
        Error -> Error
    end.

%% @private Parse DID string
erlang_parse_did(Did) ->
    case binary:split(Did, <<":">>, [global]) of
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
    end.

%% @private Check if child is descendant of parent
erlang_is_descendant(ChildDid, ParentDid) ->
    case {binary:match(ChildDid, <<"did:macula:">>),
          binary:match(ParentDid, <<"did:macula:">>)} of
        {{0, 11}, {0, 11}} ->
            ChildIdentity = binary:part(ChildDid, 11, byte_size(ChildDid) - 11),
            ParentIdentity = binary:part(ParentDid, 11, byte_size(ParentDid) - 11),
            case ChildIdentity =:= ParentIdentity of
                true -> false; % Same DID
                false ->
                    Prefix = <<ParentIdentity/binary, ".">>,
                    case binary:match(ChildIdentity, Prefix) of
                        {0, _} -> true;
                        _ -> false
                    end
            end;
        _ ->
            false
    end.

%% @private Get parent DID
get_parent_did_erlang(DidStr) ->
    case string:prefix(DidStr, "did:macula:") of
        nomatch -> null;
        Identity ->
            Parts = string:split(Identity, ".", all),
            case length(Parts) of
                N when N =< 2 -> null; % Root realm has no parent
                _ ->
                    ParentParts = lists:droplast(Parts),
                    ParentIdentity = string:join(ParentParts, "."),
                    list_to_binary("did:macula:" ++ ParentIdentity)
            end
    end.

%%====================================================================
%% JSON Helpers (minimal implementation)
%%====================================================================

%% @private Encode Erlang term to JSON binary
json_encode(Term) ->
    iolist_to_binary(encode_value(Term)).

encode_value(null) -> <<"null">>;
encode_value(true) -> <<"true">>;
encode_value(false) -> <<"false">>;
encode_value(N) when is_integer(N) -> integer_to_binary(N);
encode_value(N) when is_float(N) -> float_to_binary(N, [{decimals, 10}, compact]);
encode_value(S) when is_binary(S) -> encode_string(S);
encode_value(L) when is_list(L) -> encode_array(L);
encode_value(M) when is_map(M) -> encode_object(M).

encode_string(S) ->
    Escaped = binary:replace(
        binary:replace(
            binary:replace(
                binary:replace(S, <<"\\">>, <<"\\\\">>, [global]),
                <<"\"">>, <<"\\\"">>, [global]),
            <<"\n">>, <<"\\n">>, [global]),
        <<"\t">>, <<"\\t">>, [global]),
    <<"\"", Escaped/binary, "\"">>.

encode_array(L) ->
    Elements = lists:map(fun encode_value/1, L),
    <<"[", (iolist_to_binary(lists:join(<<",">>, Elements)))/binary, "]">>.

encode_object(M) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        [<<(encode_string(K))/binary, ":", (iolist_to_binary(encode_value(V)))/binary>> | Acc]
    end, [], M),
    <<"{", (iolist_to_binary(lists:join(<<",">>, Pairs)))/binary, "}">>.

%% @private Decode JSON binary to Erlang term
json_decode(Bin) ->
    {Value, _Rest} = decode_value(skip_ws(Bin)),
    Value.

decode_value(<<"null", Rest/binary>>) -> {null, Rest};
decode_value(<<"true", Rest/binary>>) -> {true, Rest};
decode_value(<<"false", Rest/binary>>) -> {false, Rest};
decode_value(<<"\"", Rest/binary>>) -> decode_string(Rest, <<>>);
decode_value(<<"[", Rest/binary>>) -> decode_array(skip_ws(Rest), []);
decode_value(<<"{", Rest/binary>>) -> decode_object(skip_ws(Rest), #{});
decode_value(<<C, _/binary>> = Bin) when C =:= $- orelse (C >= $0 andalso C =< $9) ->
    decode_number(Bin).

decode_string(<<"\\\"", Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, "\"">>);
decode_string(<<"\\\\", Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, "\\">>);
decode_string(<<"\\n", Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, "\n">>);
decode_string(<<"\\t", Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, "\t">>);
decode_string(<<"\"", Rest/binary>>, Acc) -> {Acc, Rest};
decode_string(<<C, Rest/binary>>, Acc) -> decode_string(Rest, <<Acc/binary, C>>).

decode_array(<<"]", Rest/binary>>, Acc) -> {lists:reverse(Acc), Rest};
decode_array(Bin, Acc) ->
    {Value, Rest} = decode_value(Bin),
    Rest2 = skip_ws(Rest),
    case Rest2 of
        <<",", Rest3/binary>> -> decode_array(skip_ws(Rest3), [Value | Acc]);
        <<"]", Rest3/binary>> -> {lists:reverse([Value | Acc]), Rest3}
    end.

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

decode_number(Bin) ->
    {NumStr, Rest} = take_number(Bin, <<>>),
    Num = case binary:match(NumStr, [<<".">>, <<"e">>, <<"E">>]) of
        nomatch -> binary_to_integer(NumStr);
        _ -> binary_to_float(NumStr)
    end,
    {Num, Rest}.

take_number(<<C, Rest/binary>>, Acc) when C =:= $- orelse C =:= $+ orelse C =:= $.
    orelse C =:= $e orelse C =:= $E orelse (C >= $0 andalso C =< $9) ->
    take_number(Rest, <<Acc/binary, C>>);
take_number(Rest, Acc) -> {Acc, Rest}.

skip_ws(<<C, Rest/binary>>) when C =:= $  orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r ->
    skip_ws(Rest);
skip_ws(Bin) -> Bin.

%%====================================================================
%% Base58 Helpers (Bitcoin alphabet)
%%====================================================================

-define(BASE58_ALPHABET, "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz").

base58_encode(Bin) ->
    LeadingZeros = count_leading_zeros(Bin, 0),
    Ones = list_to_binary(lists:duplicate(LeadingZeros, $1)),
    Encoded = base58_encode_int(binary:decode_unsigned(Bin, big), <<>>),
    <<Ones/binary, Encoded/binary>>.

base58_encode_int(0, Acc) -> Acc;
base58_encode_int(N, Acc) ->
    Char = lists:nth((N rem 58) + 1, ?BASE58_ALPHABET),
    base58_encode_int(N div 58, <<Char, Acc/binary>>).

count_leading_zeros(<<0, Rest/binary>>, Count) -> count_leading_zeros(Rest, Count + 1);
count_leading_zeros(_, Count) -> Count.

base58_decode(Bin) ->
    LeadingOnes = count_leading_ones(Bin, 0),
    Zeros = binary:copy(<<0>>, LeadingOnes),
    Decoded = base58_decode_int(Bin, 0),
    DecodedBin = binary:encode_unsigned(Decoded, big),
    <<Zeros/binary, DecodedBin/binary>>.

base58_decode_int(<<>>, Acc) -> Acc;
base58_decode_int(<<C, Rest/binary>>, Acc) ->
    Index = string:chr(?BASE58_ALPHABET, C) - 1,
    base58_decode_int(Rest, Acc * 58 + Index).

count_leading_ones(<<$1, Rest/binary>>, Count) -> count_leading_ones(Rest, Count + 1);
count_leading_ones(_, Count) -> Count.
