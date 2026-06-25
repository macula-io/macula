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
    Path = filename:join(priv_dir(), "macula_ucan_nif"),
    load_nif_result(erlang:load_nif(Path, 0)).

priv_dir() ->
    priv_dir_or_fallback(code:priv_dir(macula)).

priv_dir_or_fallback({error, _}) ->
    priv_dir_from_module(code:which(?MODULE));
priv_dir_or_fallback(Dir) ->
    Dir.

priv_dir_from_module(Filename) when is_list(Filename) ->
    filename:join(filename:dirname(filename:dirname(Filename)), "priv");
priv_dir_from_module(_) ->
    "priv".

load_nif_result(ok) ->
    persistent_term:put(?NIF_LOADED_KEY, true),
    ok;
load_nif_result({error, {reload, _}}) ->
    persistent_term:put(?NIF_LOADED_KEY, true),
    ok;
load_nif_result({error, _Reason}) ->
    ok.

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
    create_dispatch(is_nif_loaded(), Issuer, Audience, Capabilities, PrivateKey, Opts).

create_dispatch(true, Issuer, Audience, Capabilities, PrivateKey, Opts) ->
    create_nif_result(
        nif_create(Issuer, Audience, encode_json(Capabilities), PrivateKey, encode_json(Opts)));
create_dispatch(false, Issuer, Audience, Capabilities, PrivateKey, Opts) ->
    erlang_create(Issuer, Audience, Capabilities, PrivateKey, Opts).

create_nif_result({ok, Token}) -> {ok, Token};
create_nif_result({invalid_private_key, _}) -> {error, invalid_private_key};
create_nif_result({malformed_json, _}) -> {error, malformed_json}.

%% @doc Verify a UCAN token.
%% Checks signature, expiration, and not-before.
%% Returns the decoded payload on success.
-spec verify(Token :: binary(), PublicKey :: binary()) ->
    {ok, Payload :: map()} | {error, term()}.
verify(Token, PublicKey) ->
    verify_dispatch(is_nif_loaded(), Token, PublicKey).

verify_dispatch(true, Token, PublicKey) ->
    verify_nif_result(nif_verify(Token, PublicKey));
verify_dispatch(false, Token, PublicKey) ->
    erlang_verify(Token, PublicKey).

verify_nif_result({ok, PayloadJson}) -> {ok, decode_json(PayloadJson)};
verify_nif_result({invalid_token, _}) -> {error, invalid_token};
verify_nif_result({invalid_signature, _}) -> {error, invalid_signature};
verify_nif_result({invalid_public_key, _}) -> {error, invalid_public_key};
verify_nif_result({expired, _}) -> {error, expired};
verify_nif_result({not_yet_valid, _}) -> {error, not_yet_valid}.

%% @doc Decode a UCAN token without verification.
%% WARNING: This does NOT verify the signature!
-spec decode(Token :: binary()) -> {ok, Payload :: map()} | {error, term()}.
decode(Token) ->
    decode_dispatch(is_nif_loaded(), Token).

decode_dispatch(true, Token) ->
    decode_nif_result(nif_decode(Token));
decode_dispatch(false, Token) ->
    erlang_decode(Token).

decode_nif_result({ok, PayloadJson}) -> {ok, decode_json(PayloadJson)};
decode_nif_result({invalid_token, _}) -> {error, invalid_token}.

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
    get_issuer_dispatch(is_nif_loaded(), Token).

get_issuer_dispatch(true, Token) ->
    token_field_result(nif_get_issuer(Token));
get_issuer_dispatch(false, Token) ->
    erlang_get_field(Token, <<"iss">>).

%% @private Shared NIF token-field result mapping ({ok,_} | {invalid_token,_}).
token_field_result({ok, Value}) -> {ok, Value};
token_field_result({invalid_token, _}) -> {error, invalid_token}.

%% @doc Get the audience DID from a UCAN token.
-spec get_audience(Token :: binary()) -> {ok, did()} | {error, term()}.
get_audience(Token) ->
    get_audience_dispatch(is_nif_loaded(), Token).

get_audience_dispatch(true, Token) ->
    token_field_result(nif_get_audience(Token));
get_audience_dispatch(false, Token) ->
    erlang_get_field(Token, <<"aud">>).

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
    verify_split(split_token(Token), PublicKey);
erlang_verify(_Token, _PublicKey) ->
    {error, invalid_public_key}.

verify_split({ok, HeaderB64, PayloadB64, SignatureB64}, PublicKey) ->
    verify_payload(base64_url_decode(PayloadB64), HeaderB64, PayloadB64, SignatureB64, PublicKey);
verify_split(_, _PublicKey) ->
    {error, invalid_token}.

verify_payload({ok, PayloadJson}, HeaderB64, PayloadB64, SignatureB64, PublicKey) ->
    Payload = decode_json(PayloadJson),
    Now = erlang:system_time(second),
    verify_exp(check_expiration(Payload, Now), Now,
               HeaderB64, PayloadB64, SignatureB64, PublicKey, Payload);
verify_payload(_, _HeaderB64, _PayloadB64, _SignatureB64, _PublicKey) ->
    {error, invalid_token}.

%% @private Short-circuit: only check not-before if expiration passed.
verify_exp({error, _} = Error, _Now, _H, _P, _S, _PK, _Payload) ->
    Error;
verify_exp(ok, Now, HeaderB64, PayloadB64, SignatureB64, PublicKey, Payload) ->
    verify_nbf(check_not_before(Payload, Now),
               HeaderB64, PayloadB64, SignatureB64, PublicKey, Payload).

verify_nbf({error, _} = Error, _H, _P, _S, _PK, _Payload) ->
    Error;
verify_nbf(ok, HeaderB64, PayloadB64, SignatureB64, PublicKey, Payload) ->
    verify_signature(HeaderB64, PayloadB64, SignatureB64, PublicKey, Payload).

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
    verify_sig_decoded(base64_url_decode(SignatureB64), SigningInput, PublicKey, Payload).

verify_sig_decoded({ok, Signature}, SigningInput, PublicKey, Payload) ->
    verify_sig_check(macula_crypto_nif:verify(SigningInput, Signature, PublicKey), Payload);
verify_sig_decoded(_, _SigningInput, _PublicKey, _Payload) ->
    {error, invalid_signature}.

verify_sig_check(true, Payload)   -> {ok, Payload};
verify_sig_check(false, _Payload) -> {error, invalid_signature}.

%% @private
erlang_decode(Token) ->
    decode_split(split_token(Token)).

decode_split({ok, _HeaderB64, PayloadB64, _SignatureB64}) ->
    decode_payload(base64_url_decode(PayloadB64));
decode_split(_) ->
    {error, invalid_token}.

decode_payload({ok, PayloadJson}) -> {ok, decode_json(PayloadJson)};
decode_payload(_) -> {error, invalid_token}.

%% @private
erlang_compute_cid(Token) ->
    Hash = crypto:hash(sha256, Token),
    base64_url_encode(Hash).

%% @private
erlang_get_field(Token, Field) ->
    get_field_decoded(erlang_decode(Token), Field).

get_field_decoded({ok, Payload}, Field) ->
    field_value(maps:find(Field, Payload));
get_field_decoded(Error, _Field) ->
    Error.

field_value({ok, Value}) -> {ok, Value};
field_value(error)       -> {ok, null}.

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
    Pairs = maps:fold(fun encode_json_fold/3, [], Map),
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

encode_json_fold(K, V, Acc) ->
    [encode_json_pair(json_key(K), V) | Acc].

json_key(K) when is_atom(K) -> atom_to_binary(K);
json_key(K)                 -> K.

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
