%% @doc The signed objects of the post-quantum records and frames, as DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md lays
%% them out.
%%
%% A signed object is {key, tbs, signature} when the signer's key travels with it, and {tbs, signature} when the
%% verifier already holds the key. Either way the signature covers Label || 0x00 || SHA-384(key as carried) || tbs,
%% so each signature is bound to its key, also when the key does not travel, and to the label of the structure it
%% signs.
%%
%% A verifier reads an object in order: the object holds exactly the keys of its shape, each a byte string; a carried
%% key is in the carried form of the verifier's profile; the signature verifies over tbs as received, with the
%% profile's algorithm; only then is tbs decoded under the decoding rule, and it must be a map whose alg names the
%% profile's algorithm. alg is checked and never selects an algorithm. Nothing is re-encoded.
-module(macula_signed_object).

-export([sign/3, sign_held/3, verify/3, verify_held/4, encode/1, decode/1, alg/1]).

-export_type([object/0, held_object/0, refusal/0]).

-type object() :: #{key := binary(), tbs := binary(), signature := binary()}.
-type held_object() :: #{tbs := binary(), signature := binary()}.
-type refusal() :: malformed | signature_invalid | alg_mismatch.

-define(ALG, {text, <<"alg">>}).

%%------------------------------------------------------------------
%% Signing
%%------------------------------------------------------------------

%% @doc Sign fields under a label with a key. The fields gain alg for the key's profile, and the object carries the
%% key as carried.
-spec sign(binary(), map(), macula_node_keys:node_key()) -> object().
sign(Label, Fields, #{profile := Profile} = Key) when is_binary(Label), is_map(Fields) ->
    Carried = macula_node_keys:public_key(Key),
    Tbs = macula_record_cbor:encode(Fields#{?ALG => {text, alg(Profile)}}),
    #{key => Carried, tbs => Tbs, signature => macula_node_keys:sign(signed_bytes(Label, Carried, Tbs), Key)}.

%% @doc Sign fields under a label with a key, for a verifier that already holds the key. The signature still covers
%% the key's hash.
-spec sign_held(binary(), map(), macula_node_keys:node_key()) -> held_object().
sign_held(Label, Fields, Key) ->
    maps:remove(key, sign(Label, Fields, Key)).

%%------------------------------------------------------------------
%% Verifying
%%------------------------------------------------------------------

%% @doc Verify an object that carries its key, under a label and the verifier's profile. Returns the key, the tbs
%% bytes as received and the decoded fields. Malformed input is refused, never raised on.
-spec verify(binary(), term(), macula_crypto_profile:profile()) ->
        {ok, #{key := binary(), tbs := binary(), fields := map()}} | {error, refusal()}.
verify(Label, #{key := Key, tbs := Tbs, signature := Signature} = Object, Profile)
  when is_binary(Label), map_size(Object) =:= 3, is_binary(Key), is_binary(Tbs), is_binary(Signature) ->
    carried_key(macula_node_keys:carried_key_well_formed(Key, Profile), Label, Object, Profile);
verify(_Label, _Object, _Profile) ->
    {error, malformed}.

%% @doc Verify an object whose key the verifier holds, under a label, that key and the verifier's profile. Returns
%% the tbs bytes as received and the decoded fields.
-spec verify_held(binary(), term(), binary(), macula_crypto_profile:profile()) ->
        {ok, #{tbs := binary(), fields := map()}} | {error, refusal()}.
verify_held(Label, #{tbs := Tbs, signature := Signature} = Object, Key, Profile)
  when is_binary(Label), map_size(Object) =:= 2, is_binary(Tbs), is_binary(Signature), is_binary(Key) ->
    fields(Label, Key, Tbs, Signature, Profile);
verify_held(_Label, _Object, _Key, _Profile) ->
    {error, malformed}.

%%------------------------------------------------------------------
%% Wire form
%%------------------------------------------------------------------

%% @doc The CBOR map of an object, with text keys.
-spec encode(object() | held_object()) -> binary().
encode(#{key := Key, tbs := Tbs, signature := Signature} = Object) when map_size(Object) =:= 3 ->
    macula_record_cbor:encode(#{{text, <<"key">>} => Key, {text, <<"tbs">>} => Tbs,
                                {text, <<"signature">>} => Signature});
encode(#{tbs := Tbs, signature := Signature} = Object) when map_size(Object) =:= 2 ->
    macula_record_cbor:encode(#{{text, <<"tbs">>} => Tbs, {text, <<"signature">>} => Signature}).

%% @doc Read an object from its CBOR map under the decoding rule: exactly the keys of one shape, each a byte string.
-spec decode(binary()) -> {ok, object() | held_object()} | {error, malformed}.
decode(Bytes) when is_binary(Bytes) ->
    shaped(macula_record_cbor:decode_strict(Bytes)).

%% @doc The alg a profile's signed objects name.
-spec alg(macula_crypto_profile:profile()) -> binary().
alg(pq_pure) -> <<"ML-DSA-87">>;
alg(pq_hybrid) -> <<"ML-DSA-87-PS384">>.

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

signed_bytes(Label, Key, Tbs) ->
    [Label, 0, crypto:hash(sha384, Key), Tbs].

carried_key(true, Label, #{key := Key, tbs := Tbs, signature := Signature}, Profile) ->
    with_key(fields(Label, Key, Tbs, Signature, Profile), Key);
carried_key(false, _Label, _Object, _Profile) ->
    {error, malformed}.

with_key({ok, Read}, Key) -> {ok, Read#{key => Key}};
with_key({error, _} = Error, _Key) -> Error.

fields(Label, Key, Tbs, Signature, Profile) ->
    signed(macula_node_keys:verify(signed_bytes(Label, Key, Tbs), Signature, Key, Profile), Tbs, Profile).

signed(true, Tbs, Profile) -> decoded(macula_record_cbor:decode_strict(Tbs), Tbs, Profile);
signed(false, _Tbs, _Profile) -> {error, signature_invalid}.

decoded({ok, #{?ALG := {text, Alg}} = Fields}, Tbs, Profile) when is_binary(Alg) ->
    alg_checked(Alg =:= alg(Profile), Tbs, Fields);
decoded(_NotAMapWithAlg, _Tbs, _Profile) ->
    {error, malformed}.

alg_checked(true, Tbs, Fields) -> {ok, #{tbs => Tbs, fields => Fields}};
alg_checked(false, _Tbs, _Fields) -> {error, alg_mismatch}.

shaped({ok, #{{text, <<"key">>} := Key, {text, <<"tbs">>} := Tbs, {text, <<"signature">>} := Signature} = Map})
  when map_size(Map) =:= 3, is_binary(Key), is_binary(Tbs), is_binary(Signature) ->
    {ok, #{key => Key, tbs => Tbs, signature => Signature}};
shaped({ok, #{{text, <<"tbs">>} := Tbs, {text, <<"signature">>} := Signature} = Map})
  when map_size(Map) =:= 2, is_binary(Tbs), is_binary(Signature) ->
    {ok, #{tbs => Tbs, signature => Signature}};
shaped(_Other) ->
    {error, malformed}.
