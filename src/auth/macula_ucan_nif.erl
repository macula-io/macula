%% @doc UCAN (User Controlled Authorization Networks) token operations.
%%
%% This module provides creation, verification, and manipulation of UCAN tokens
%% for decentralized authorization in the Macula mesh. UCANs are self-contained
%% capability tokens that support delegation.
%%
%% == Token Structure ==
%%
%% UCAN tokens follow JWT format: header.payload.signature
%%
%% Header:
%%   - alg: "EdDSA" (Ed25519)
%%   - typ: "JWT"
%%   - ucv: "0.10.0" (UCAN version)
%%
%% Payload:
%%   - iss: Issuer DID (did:macula:io.macula.org)
%%   - aud: Audience DID
%%   - cap: Capabilities [{with, can}, ...]
%%   - exp: Expiration (optional, unix timestamp)
%%   - nbf: Not before (optional, unix timestamp)
%%   - nnc: Nonce (optional, for uniqueness)
%%   - fct: Facts (optional, metadata)
%%   - prf: Proof chain (CIDs of parent tokens)
%%
%% @author rgfaber
-module(macula_ucan_nif).

%% API
-export([
    create/4,
    create/5,
    verify/2,
    decode/1,
    compute_cid/1,
    get_issuer/1,
    get_audience/1,
    get_capabilities/1,
    get_expiration/1,
    get_proofs/1,
    is_expired/1,
    is_nif_loaded/0
]).

%% NIF stubs
-export([
    nif_create/5,
    nif_verify/2,
    nif_decode/1,
    nif_compute_cid/1,
    nif_get_issuer/1,
    nif_get_audience/1
]).

-on_load(init/0).

-define(NIF_LOADED_KEY, macula_ucan_nif_loaded).

%%====================================================================
%% Types
%%====================================================================

-type did() :: binary().
-type capability() :: #{with := binary(), can := binary()}.
-type ucan_opts() :: #{
    exp => non_neg_integer(),
    nbf => non_neg_integer(),
    nnc => binary(),
    fct => map(),
    prf => [binary()]
}.

-export_type([did/0, capability/0, ucan_opts/0]).

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
    Path = filename:join(PrivDir, "macula_ucan_nif"),
    case erlang:load_nif(Path, 0) of
        ok ->
            persistent_term:put(?NIF_LOADED_KEY, true),
            ok;
        {error, {reload, _}} ->
            persistent_term:put(?NIF_LOADED_KEY, true),
            ok;
        {error, _Reason} ->
            ok
    end.

%%====================================================================
%% API
%%====================================================================

%% @doc Check if the NIF is loaded.
-spec is_nif_loaded() -> boolean().
is_nif_loaded() ->
    persistent_term:get(?NIF_LOADED_KEY, false).

%% @doc Create a new UCAN token.
%% @equiv create(Issuer, Audience, Capabilities, PrivateKey, #{})
-spec create(Issuer :: did(), Audience :: did(), Capabilities :: [capability()],
             PrivateKey :: binary()) ->
    {ok, Token :: binary()} | {error, term()}.
create(Issuer, Audience, Capabilities, PrivateKey) ->
    create(Issuer, Audience, Capabilities, PrivateKey, #{}).

%% @doc Create a new UCAN token with options.
%%
%% Options:
%%   - exp: Expiration timestamp (unix seconds)
%%   - nbf: Not before timestamp (unix seconds)
%%   - nnc: Nonce (for uniqueness)
%%   - fct: Facts map (metadata)
%%   - prf: Proof chain (list of CIDs of parent tokens)
-spec create(Issuer :: did(), Audience :: did(), Capabilities :: [capability()],
             PrivateKey :: binary(), Opts :: ucan_opts()) ->
    {ok, Token :: binary()} | {error, term()}.
create(Issuer, Audience, Capabilities, PrivateKey, Opts) ->
    case is_nif_loaded() of
        true ->
            CapsJson = encode_json(Capabilities),
            OptsJson = encode_json(Opts),
            case nif_create(Issuer, Audience, CapsJson, PrivateKey, OptsJson) of
                {ok, Token} -> {ok, Token};
                {invalid_private_key, _} -> {error, invalid_private_key};
                {malformed_json, _} -> {error, malformed_json}
            end;
        false ->
            erlang_create(Issuer, Audience, Capabilities, PrivateKey, Opts)
    end.

%% @doc Verify a UCAN token.
%% Checks signature, expiration, and not-before.
%% Returns the decoded payload on success.
-spec verify(Token :: binary(), PublicKey :: binary()) ->
    {ok, Payload :: map()} | {error, term()}.
verify(Token, PublicKey) ->
    case is_nif_loaded() of
        true ->
            case nif_verify(Token, PublicKey) of
                {ok, PayloadJson} -> {ok, decode_json(PayloadJson)};
                {invalid_token, _} -> {error, invalid_token};
                {invalid_signature, _} -> {error, invalid_signature};
                {invalid_public_key, _} -> {error, invalid_public_key};
                {expired, _} -> {error, expired};
                {not_yet_valid, _} -> {error, not_yet_valid}
            end;
        false ->
            erlang_verify(Token, PublicKey)
    end.

%% @doc Decode a UCAN token without verification.
%% WARNING: This does NOT verify the signature!
-spec decode(Token :: binary()) -> {ok, Payload :: map()} | {error, term()}.
decode(Token) ->
    case is_nif_loaded() of
        true ->
            case nif_decode(Token) of
                {ok, PayloadJson} -> {ok, decode_json(PayloadJson)};
                {invalid_token, _} -> {error, invalid_token}
            end;
        false ->
            erlang_decode(Token)
    end.

%% @doc Compute the CID (Content ID) of a UCAN token.
%% Used for proof chains.
-spec compute_cid(Token :: binary()) -> binary().
compute_cid(Token) ->
    case is_nif_loaded() of
        true -> nif_compute_cid(Token);
        false -> erlang_compute_cid(Token)
    end.

%% @doc Get the issuer DID from a UCAN token.
-spec get_issuer(Token :: binary()) -> {ok, did()} | {error, term()}.
get_issuer(Token) ->
    case is_nif_loaded() of
        true ->
            case nif_get_issuer(Token) of
                {ok, Issuer} -> {ok, Issuer};
                {invalid_token, _} -> {error, invalid_token}
            end;
        false -> erlang_get_field(Token, <<"iss">>)
    end.

%% @doc Get the audience DID from a UCAN token.
-spec get_audience(Token :: binary()) -> {ok, did()} | {error, term()}.
get_audience(Token) ->
    case is_nif_loaded() of
        true ->
            case nif_get_audience(Token) of
                {ok, Audience} -> {ok, Audience};
                {invalid_token, _} -> {error, invalid_token}
            end;
        false -> erlang_get_field(Token, <<"aud">>)
    end.

%% @doc Get capabilities from a UCAN token.
-spec get_capabilities(Token :: binary()) -> {ok, [capability()]} | {error, term()}.
get_capabilities(Token) ->
    erlang_get_field(Token, <<"cap">>).

%% @doc Get expiration timestamp from a UCAN token.
-spec get_expiration(Token :: binary()) -> {ok, non_neg_integer() | null} | {error, term()}.
get_expiration(Token) ->
    erlang_get_field(Token, <<"exp">>).

%% @doc Get proof chain from a UCAN token.
-spec get_proofs(Token :: binary()) -> {ok, [binary()]} | {error, term()}.
get_proofs(Token) ->
    erlang_get_field(Token, <<"prf">>).

%% @doc Check if a UCAN token is expired.
-spec is_expired(Token :: binary()) -> boolean() | {error, term()}.
is_expired(Token) ->
    case get_expiration(Token) of
        {ok, null} -> false;
        {ok, Exp} ->
            Now = erlang:system_time(second),
            Now > Exp;
        Error -> Error
    end.

%%====================================================================
%% NIF Stubs
%%====================================================================

nif_create(_Issuer, _Audience, _CapsJson, _PrivateKey, _OptsJson) ->
    erlang:nif_error(nif_not_loaded).

nif_verify(_Token, _PublicKey) ->
    erlang:nif_error(nif_not_loaded).

nif_decode(_Token) ->
    erlang:nif_error(nif_not_loaded).

nif_compute_cid(_Token) ->
    erlang:nif_error(nif_not_loaded).

nif_get_issuer(_Token) ->
    erlang:nif_error(nif_not_loaded).

nif_get_audience(_Token) ->
    erlang:nif_error(nif_not_loaded).

%%====================================================================
%% Pure Erlang Fallbacks
%%====================================================================

%% @private
erlang_create(Issuer, Audience, Capabilities, PrivateKey, Opts) when byte_size(PrivateKey) =:= 32 ->
    %% Build header
    Header = #{
        <<"alg">> => <<"EdDSA">>,
        <<"typ">> => <<"JWT">>,
        <<"ucv">> => <<"0.10.0">>
    },

    %% Build payload
    Payload0 = #{
        <<"iss">> => Issuer,
        <<"aud">> => Audience,
        <<"cap">> => Capabilities,
        <<"prf">> => maps:get(prf, Opts, [])
    },

    %% Add optional fields
    Payload1 = maybe_add(<<"exp">>, exp, Opts, Payload0),
    Payload2 = maybe_add(<<"nbf">>, nbf, Opts, Payload1),
    Payload3 = maybe_add(<<"nnc">>, nnc, Opts, Payload2),
    Payload = maybe_add(<<"fct">>, fct, Opts, Payload3),

    %% Encode
    HeaderB64 = base64_url_encode(encode_json(Header)),
    PayloadB64 = base64_url_encode(encode_json(Payload)),

    %% Sign
    SigningInput = <<HeaderB64/binary, ".", PayloadB64/binary>>,
    {ok, Signature} = macula_crypto_nif:sign(SigningInput, PrivateKey),
    SignatureB64 = base64_url_encode(Signature),

    %% Combine
    Token = <<HeaderB64/binary, ".", PayloadB64/binary, ".", SignatureB64/binary>>,
    {ok, Token};
erlang_create(_Issuer, _Audience, _Capabilities, _PrivateKey, _Opts) ->
    {error, invalid_private_key}.

%% @private
erlang_verify(Token, PublicKey) when byte_size(PublicKey) =:= 32 ->
    case split_token(Token) of
        {ok, HeaderB64, PayloadB64, SignatureB64} ->
            %% Decode payload for checks
            case base64_url_decode(PayloadB64) of
                {ok, PayloadJson} ->
                    Payload = decode_json(PayloadJson),
                    Now = erlang:system_time(second),

                    %% Check expiration
                    case check_expiration(Payload, Now) of
                        ok ->
                            %% Check not-before
                            case check_not_before(Payload, Now) of
                                ok ->
                                    %% Verify signature
                                    verify_signature(HeaderB64, PayloadB64, SignatureB64, PublicKey, Payload);
                                Error -> Error
                            end;
                        Error -> Error
                    end;
                _ -> {error, invalid_token}
            end;
        _ -> {error, invalid_token}
    end;
erlang_verify(_Token, _PublicKey) ->
    {error, invalid_public_key}.

%% @private Check token expiration
check_expiration(Payload, Now) ->
    case maps:get(<<"exp">>, Payload, null) of
        null -> ok;
        Exp when is_integer(Exp), Exp >= Now -> ok;
        _ -> {error, expired}
    end.

%% @private Check not-before
check_not_before(Payload, Now) ->
    case maps:get(<<"nbf">>, Payload, null) of
        null -> ok;
        Nbf when is_integer(Nbf), Nbf =< Now -> ok;
        _ -> {error, not_yet_valid}
    end.

%% @private Verify signature
verify_signature(HeaderB64, PayloadB64, SignatureB64, PublicKey, Payload) ->
    SigningInput = <<HeaderB64/binary, ".", PayloadB64/binary>>,
    case base64_url_decode(SignatureB64) of
        {ok, Signature} ->
            case macula_crypto_nif:verify(SigningInput, Signature, PublicKey) of
                true -> {ok, Payload};
                false -> {error, invalid_signature}
            end;
        _ -> {error, invalid_signature}
    end.

%% @private
erlang_decode(Token) ->
    case split_token(Token) of
        {ok, _HeaderB64, PayloadB64, _SignatureB64} ->
            case base64_url_decode(PayloadB64) of
                {ok, PayloadJson} -> {ok, decode_json(PayloadJson)};
                _ -> {error, invalid_token}
            end;
        _ -> {error, invalid_token}
    end.

%% @private
erlang_compute_cid(Token) ->
    Hash = crypto:hash(sha256, Token),
    base64_url_encode(Hash).

%% @private
erlang_get_field(Token, Field) ->
    case erlang_decode(Token) of
        {ok, Payload} ->
            case maps:find(Field, Payload) of
                {ok, Value} -> {ok, Value};
                error -> {ok, null}
            end;
        Error -> Error
    end.

%%====================================================================
%% Helpers
%%====================================================================

%% @private
split_token(Token) ->
    case binary:split(Token, <<".">>, [global]) of
        [Header, Payload, Signature] -> {ok, Header, Payload, Signature};
        _ -> {error, invalid_token}
    end.

%% @private
maybe_add(JsonKey, OptKey, Opts, Map) ->
    case maps:find(OptKey, Opts) of
        {ok, Value} -> maps:put(JsonKey, Value, Map);
        error -> Map
    end.

%% @private URL-safe base64 encode (no padding)
base64_url_encode(Data) when is_binary(Data) ->
    B64 = base64:encode(Data),
    B64_Url = binary:replace(binary:replace(B64, <<"+">>, <<"-">>, [global]), <<"/">>, <<"_">>, [global]),
    binary:replace(B64_Url, <<"=">>, <<>>, [global]).

%% @private URL-safe base64 decode
base64_url_decode(Encoded) ->
    try
        B64_Std = binary:replace(binary:replace(Encoded, <<"-">>, <<"+">>, [global]), <<"_">>, <<"/">>, [global]),
        Padded = case byte_size(B64_Std) rem 4 of
            0 -> B64_Std;
            2 -> <<B64_Std/binary, "==">>;
            3 -> <<B64_Std/binary, "=">>
        end,
        {ok, base64:decode(Padded)}
    catch
        _:_ -> {error, invalid_base64}
    end.

%% @private JSON encoding using term_to_binary for maps (simple implementation)
%% In production, use jsx or jiffy
encode_json(Term) ->
    encode_json_term(Term).

encode_json_term(Map) when is_map(Map) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        Key = if is_atom(K) -> atom_to_binary(K); true -> K end,
        [encode_json_pair(Key, V) | Acc]
    end, [], Map),
    <<"{", (iolist_to_binary(lists:join(<<",">>, Pairs)))/binary, "}">>;
encode_json_term(List) when is_list(List) ->
    Items = [encode_json_term(I) || I <- List],
    <<"[", (iolist_to_binary(lists:join(<<",">>, Items)))/binary, "]">>;
encode_json_term(Bin) when is_binary(Bin) ->
    %% Simple JSON string encoding (escape quotes)
    Escaped = binary:replace(Bin, <<"\"">>, <<"\\\"">>, [global]),
    <<"\"", Escaped/binary, "\"">>;
encode_json_term(Num) when is_integer(Num) ->
    integer_to_binary(Num);
encode_json_term(true) -> <<"true">>;
encode_json_term(false) -> <<"false">>;
encode_json_term(null) -> <<"null">>;
encode_json_term(Atom) when is_atom(Atom) ->
    encode_json_term(atom_to_binary(Atom)).

encode_json_pair(Key, Value) ->
    <<(encode_json_term(Key))/binary, ":", (encode_json_term(Value))/binary>>.

%% @private Simple JSON decoding
decode_json(Bin) ->
    %% Very basic JSON parsing - use jsx or jiffy in production
    try
        {ok, Tokens, _} = json_tokenize(Bin),
        {Value, []} = json_parse(Tokens),
        Value
    catch
        _:_ -> #{}
    end.

%% Minimal JSON tokenizer
json_tokenize(Bin) ->
    json_tokenize(Bin, []).

json_tokenize(<<>>, Acc) ->
    {ok, lists:reverse(Acc), <<>>};
json_tokenize(<<C, Rest/binary>>, Acc) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
    json_tokenize(Rest, Acc);
json_tokenize(<<"{", Rest/binary>>, Acc) ->
    json_tokenize(Rest, ['{' | Acc]);
json_tokenize(<<"}", Rest/binary>>, Acc) ->
    json_tokenize(Rest, ['}' | Acc]);
json_tokenize(<<"[", Rest/binary>>, Acc) ->
    json_tokenize(Rest, ['[' | Acc]);
json_tokenize(<<"]", Rest/binary>>, Acc) ->
    json_tokenize(Rest, [']' | Acc]);
json_tokenize(<<",", Rest/binary>>, Acc) ->
    json_tokenize(Rest, [',' | Acc]);
json_tokenize(<<":", Rest/binary>>, Acc) ->
    json_tokenize(Rest, [':' | Acc]);
json_tokenize(<<"\"", Rest/binary>>, Acc) ->
    {Str, Rest2} = json_string(Rest, <<>>),
    json_tokenize(Rest2, [{string, Str} | Acc]);
json_tokenize(<<"true", Rest/binary>>, Acc) ->
    json_tokenize(Rest, [true | Acc]);
json_tokenize(<<"false", Rest/binary>>, Acc) ->
    json_tokenize(Rest, [false | Acc]);
json_tokenize(<<"null", Rest/binary>>, Acc) ->
    json_tokenize(Rest, [null | Acc]);
json_tokenize(<<C, _/binary>> = Bin, Acc) when C >= $0, C =< $9; C =:= $- ->
    {Num, Rest} = json_number(Bin, <<>>),
    json_tokenize(Rest, [{number, Num} | Acc]).

json_string(<<"\\\"", Rest/binary>>, Acc) ->
    json_string(Rest, <<Acc/binary, "\"">>);
json_string(<<"\\\\", Rest/binary>>, Acc) ->
    json_string(Rest, <<Acc/binary, "\\">>);
json_string(<<"\"", Rest/binary>>, Acc) ->
    {Acc, Rest};
json_string(<<C, Rest/binary>>, Acc) ->
    json_string(Rest, <<Acc/binary, C>>).

json_number(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9; C =:= $-; C =:= $.; C =:= $e; C =:= $E; C =:= $+ ->
    json_number(Rest, <<Acc/binary, C>>);
json_number(Rest, Acc) ->
    Num = case binary:match(Acc, <<".">>) of
        nomatch -> binary_to_integer(Acc);
        _ -> binary_to_float(Acc)
    end,
    {Num, Rest}.

json_parse(['{' | Rest]) ->
    json_parse_object(Rest, #{});
json_parse(['[' | Rest]) ->
    json_parse_array(Rest, []);
json_parse([{string, S} | Rest]) ->
    {S, Rest};
json_parse([{number, N} | Rest]) ->
    {N, Rest};
json_parse([true | Rest]) ->
    {true, Rest};
json_parse([false | Rest]) ->
    {false, Rest};
json_parse([null | Rest]) ->
    {null, Rest}.

json_parse_object(['}' | Rest], Acc) ->
    {Acc, Rest};
json_parse_object([{string, Key}, ':' | Rest], Acc) ->
    {Value, Rest2} = json_parse(Rest),
    case Rest2 of
        [',' | Rest3] -> json_parse_object(Rest3, maps:put(Key, Value, Acc));
        ['}' | Rest3] -> {maps:put(Key, Value, Acc), Rest3}
    end.

json_parse_array([']' | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
json_parse_array(Tokens, Acc) ->
    {Value, Rest} = json_parse(Tokens),
    case Rest of
        [',' | Rest2] -> json_parse_array(Rest2, [Value | Acc]);
        [']' | Rest2] -> {lists:reverse([Value | Acc]), Rest2}
    end.
