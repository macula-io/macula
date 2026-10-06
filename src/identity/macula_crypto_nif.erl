%% @doc Cryptographic operations for Macula mesh.
%%
%% This module provides ML-DSA signatures, SHA-256 hashing, base64
%% encoding and constant-time comparison, in a Rust NIF. It has no BLAKE3:
%% content ids are SHA-384 (D24), and the last BLAKE3 caller is gone
%% (macula-io/macula#46).
%%
%% == NIF vs Erlang ==
%%
%% Hashing, encoding and comparison fall back to pure Erlang when the NIF
%% cannot be loaded; the Rust NIF is faster (SHA-256 about 3x). ML-DSA has
%% no fallback, below.
%%
%% == ML-DSA ==
%%
%% ML-DSA (FIPS 204) signatures are made and checked by `macula-mldsa'
%% (D7), and have no Erlang fallback: without the NIF they raise. Sets
%% take OTP's names (mldsa44, mldsa65, mldsa87). A private key is
%% `{seed, Seed}', the 32-byte seed new keys are stored as (D6), or
%% `{expanded, Key}', the form OTP generates. Signing is hedged, with
%% randomness from the OS, and takes a FIPS 204 context string of at
%% most 255 bytes.
%%
%% @author rgfaber
-module(macula_crypto_nif).

%% API
-export([
    sha256/1,
    sha256_base64/1,
    base64_encode/1,
    base64_decode/1,
    secure_compare/2,
    is_nif_loaded/0,
    mldsa_generate/1,
    mldsa_public_key/2,
    mldsa_sign/4,
    mldsa_verify/5
]).

-export_type([mldsa_set/0, mldsa_private_key/0]).

-type mldsa_set() :: mldsa44 | mldsa65 | mldsa87.
-type mldsa_private_key() :: {seed, <<_:256>>} | {expanded, binary()}.

%% NIF stubs
-export([
    nif_effective_uid/0
]).

-on_load(init/0).

-define(NIF_LOADED_KEY, macula_crypto_nif_loaded).

%%====================================================================
%% Init
%%====================================================================

init() ->
    PrivDir = priv_dir(),
    Path = filename:join(PrivDir, "macula_crypto_nif"),
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

priv_dir() ->
    priv_dir(code:priv_dir(macula)).

priv_dir({error, _}) ->
    priv_dir_from_module(code:which(?MODULE));
priv_dir(Dir) ->
    Dir.

priv_dir_from_module(Filename) when is_list(Filename) ->
    filename:join(filename:dirname(filename:dirname(Filename)), "priv");
priv_dir_from_module(_) ->
    "priv".

%%====================================================================
%% API
%%====================================================================

%% @doc Check if the NIF is loaded.
-spec is_nif_loaded() -> boolean().
is_nif_loaded() ->
    persistent_term:get(?NIF_LOADED_KEY, false).

%% @doc Compute SHA-256 hash.
%% Returns 32-byte hash binary.
-spec sha256(Data :: binary()) -> Hash :: binary().
sha256(Data) ->
    case is_nif_loaded() of
        true -> nif_sha256(Data);
        false -> erlang_sha256(Data)
    end.

%% @doc Compute SHA-256 hash and encode as URL-safe base64.
%% Returns base64-encoded string (no padding).
-spec sha256_base64(Data :: binary()) -> Base64Hash :: binary().
sha256_base64(Data) ->
    case is_nif_loaded() of
        true -> nif_sha256_base64(Data);
        false -> erlang_sha256_base64(Data)
    end.

%% @doc Encode data as URL-safe base64 (no padding).
-spec base64_encode(Data :: binary()) -> Encoded :: binary().
base64_encode(Data) ->
    case is_nif_loaded() of
        true -> nif_base64_encode(Data);
        false -> erlang_base64_encode(Data)
    end.

%% @doc Decode URL-safe base64 data.
%% Returns `{ok, Data}' or `{error, invalid_base64}'.
-spec base64_decode(Encoded :: binary()) -> {ok, binary()} | {error, atom()}.
base64_decode(Encoded) ->
    case is_nif_loaded() of
        true -> base64_decode_result(nif_base64_decode(Encoded));
        false -> erlang_base64_decode(Encoded)
    end.

base64_decode_result({ok, Data}) -> {ok, Data};
base64_decode_result({error, _}) -> {error, invalid_base64}.

%% @doc Constant-time comparison of two binaries.
%% Important for security - prevents timing attacks.
-spec secure_compare(A :: binary(), B :: binary()) -> boolean().
secure_compare(A, B) ->
    case is_nif_loaded() of
        true -> nif_secure_compare(A, B);
        false -> erlang_secure_compare(A, B)
    end.

%% @doc A new ML-DSA key, kept as its 32-byte seed.
-spec mldsa_generate(mldsa_set()) ->
    {ok, {PublicKey :: binary(), Seed :: <<_:256>>}} | {error, randomness_unavailable}.
mldsa_generate(Set) ->
    nif_mldsa_generate(Set).

%% @doc The public key of an ML-DSA private key in either form. An
%% expanded key whose parts disagree is `inconsistent_private_key'.
-spec mldsa_public_key(mldsa_set(), mldsa_private_key()) ->
    {ok, PublicKey :: binary()} | {error, wrong_length | inconsistent_private_key}.
mldsa_public_key(Set, {Form, Key}) ->
    nif_mldsa_public_key(Set, Form, Key).

%% @doc A hedged ML-DSA signature over `Message' under the context
%% string `Context'.
-spec mldsa_sign(mldsa_set(), mldsa_private_key(), Message :: binary(), Context :: binary()) ->
    {ok, Signature :: binary()}
    | {error, wrong_length | context_too_long | randomness_unavailable}.
mldsa_sign(Set, {Form, Key}, Message, Context) ->
    nif_mldsa_sign(Set, Form, Key, Message, Context).

%% @doc Whether `Signature' is a valid ML-DSA signature over `Message'
%% under `Context'. False for anything FIPS 204 rejects, a context over
%% 255 bytes included.
-spec mldsa_verify(mldsa_set(), PublicKey :: binary(), Message :: binary(),
                   Signature :: binary(), Context :: binary()) -> boolean().
mldsa_verify(Set, PublicKey, Message, Signature, Context) ->
    nif_mldsa_verify(Set, PublicKey, Message, Signature, Context).

%%====================================================================
%% NIF Stubs (replaced when NIF loads)
%%====================================================================

nif_sha256(_Data) ->
    erlang:nif_error(nif_not_loaded).

nif_sha256_base64(_Data) ->
    erlang:nif_error(nif_not_loaded).

nif_base64_encode(_Data) ->
    erlang:nif_error(nif_not_loaded).

nif_base64_decode(_Encoded) ->
    erlang:nif_error(nif_not_loaded).

nif_secure_compare(_A, _B) ->
    erlang:nif_error(nif_not_loaded).

nif_mldsa_generate(_Set) ->
    erlang:nif_error(nif_not_loaded).

nif_mldsa_public_key(_Set, _Form, _Key) ->
    erlang:nif_error(nif_not_loaded).

nif_mldsa_sign(_Set, _Form, _Key, _Message, _Context) ->
    erlang:nif_error(nif_not_loaded).

nif_mldsa_verify(_Set, _PublicKey, _Message, _Signature, _Context) ->
    erlang:nif_error(nif_not_loaded).

%% The effective user id, or none on a host without user ids. No Erlang
%% fallback: macula_node_user raises rather than skip an owner check.
-spec nif_effective_uid() -> non_neg_integer() | none.
nif_effective_uid() ->
    erlang:nif_error(nif_not_loaded).

%%====================================================================
%% Pure Erlang Fallbacks
%%====================================================================

%% @private SHA-256 using Erlang crypto
erlang_sha256(Data) ->
    crypto:hash(sha256, Data).

%% @private SHA-256 + base64 encode
erlang_sha256_base64(Data) ->
    Hash = crypto:hash(sha256, Data),
    erlang_base64_encode(Hash).

%% @private URL-safe base64 encode (no padding)
erlang_base64_encode(Data) ->
    %% Standard base64 encode
    B64 = base64:encode(Data),
    %% Make URL-safe: + -> -, / -> _
    B64_Url = binary:replace(binary:replace(B64, <<"+">>, <<"-">>, [global]), <<"/">>, <<"_">>, [global]),
    %% Remove padding
    binary:replace(B64_Url, <<"=">>, <<>>, [global]).

%% @private URL-safe base64 decode
erlang_base64_decode(Encoded) ->
    try
        %% Restore standard base64: - -> +, _ -> /
        B64_Std = binary:replace(binary:replace(Encoded, <<"-">>, <<"+">>, [global]), <<"_">>, <<"/">>, [global]),
        %% Add padding if needed
        Padded = case byte_size(B64_Std) rem 4 of
            0 -> B64_Std;
            2 -> <<B64_Std/binary, "==">>;
            3 -> <<B64_Std/binary, "=">>
        end,
        {ok, base64:decode(Padded)}
    catch
        _:_ -> {error, invalid_base64}
    end.

%% @private Constant-time comparison
erlang_secure_compare(A, B) when byte_size(A) =/= byte_size(B) ->
    false;
erlang_secure_compare(A, B) ->
    %% Constant-time XOR comparison
    AList = binary_to_list(A),
    BList = binary_to_list(B),
    Result = lists:foldl(fun({X, Y}, Acc) -> Acc bor (X bxor Y) end, 0, lists:zip(AList, BList)),
    Result =:= 0.
