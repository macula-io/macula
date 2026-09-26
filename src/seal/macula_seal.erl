%% @doc End-to-end payload sealing, scheme 1: what a node needs to seal a
%% payload so that the stations relaying it cannot read it
%% (`plans/DESIGN_E2E_PAYLOAD_CONFIDENTIALITY.md'). The byte-exact
%% construction is `test/vectors/E2E_SEAL_V1.md', and every function here
%% reproduces that file's vectors, which an independent Rust implementation
%% generated.
%%
%% The key agreement is ML-KEM-1024 in `pq_pure', and ML-KEM-1024 with an
%% ephemeral P-384 ECDH in `pq_hybrid', combined with HKDF-SHA-384 over both
%% secrets, both ciphertexts and the recipient's key. Payloads are sealed with
%% AES-256-GCM. Everything here is a pure function on OTP `crypto'; the
%% frames that carry a sealed payload are built elsewhere.
-module(macula_seal).

-export([key_as_carried/1, key_hash/1, key_id/1, carried_key_size/1,
         sender_secret/2, recipient_secret/4,
         call_keys/3, stream_keys/2, event_key/2,
         request_aad/1, reply_aad/4, stream_aad/4, event_aad/5,
         stream_nonce/1, random_nonce/0,
         seal/4, open/4]).

-export_type([profile/0, public_key/0, private_key/0, parties/0, request/0]).

-type profile() :: pq_pure | pq_hybrid.
%% A recipient's KEM key: the ML-KEM-1024 encapsulation key, plus the
%% uncompressed P-384 point in pq_hybrid.
-type public_key() :: #{mlkem_ek := <<_:12544>>, p384_pub => <<_:776>>}.
%% Its private half as OTP holds it: the expanded ML-KEM decapsulation key,
%% plus the P-384 scalar in pq_hybrid.
-type private_key() :: #{mlkem_dk := <<_:25344>>, p384_priv => <<_:384>>}.
%% A request's request_id, caller and target.
-type parties() :: {binary(), <<_:256>>, <<_:256>>}.
-type request() :: #{frame_type := binary(), realm := <<_:256>>, procedure := binary(),
                     caller := <<_:256>>, target := <<_:256>>, request_id := binary(),
                     deadline := non_neg_integer()}.

-define(MLKEM_EK_BYTES, 1568).
-define(MLKEM_CT_BYTES, 1568).
-define(P384_POINT_BYTES, 97).
-define(NONCE_BYTES, 12).
-define(TAG_BYTES, 16).

%%====================================================================
%% The recipient's key
%%====================================================================

%% @doc A recipient's KEM key as it is carried and hashed: the ML-KEM key,
%% followed by the P-384 point in pq_hybrid.
-spec key_as_carried(public_key()) -> binary().
key_as_carried(#{mlkem_ek := Ek, p384_pub := P384}) -> <<Ek/binary, P384/binary>>;
key_as_carried(#{mlkem_ek := Ek}) -> Ek.

%% @doc How many bytes a KEM key as carried has in a profile: the ML-KEM-1024
%% encapsulation key, plus the uncompressed P-384 point in pq_hybrid.
-spec carried_key_size(profile()) -> pos_integer().
carried_key_size(pq_pure) -> ?MLKEM_EK_BYTES;
carried_key_size(pq_hybrid) -> ?MLKEM_EK_BYTES + ?P384_POINT_BYTES.

%% @doc The SHA-384 of a KEM key as carried, which the combiner binds.
-spec key_hash(binary()) -> <<_:384>>.
key_hash(Carried) -> crypto:hash(sha384, Carried).

%% @doc The 8-byte id a sealed payload names its recipient key by.
-spec key_id(binary()) -> <<_:64>>.
key_id(Carried) -> binary:part(key_hash(Carried), 0, 8).

%%====================================================================
%% The shared secret
%%====================================================================

%% @doc A fresh shared secret to `Recipient', and the `kem_ct' that carries
%% it: the sender's side, with fresh randomness each time.
-spec sender_secret(profile(), public_key()) -> {binary(), binary()}.
sender_secret(pq_pure, #{mlkem_ek := Ek} = Recipient) ->
    {SsMlkem, MlkemCt} = crypto:encapsulate_key(mlkem1024, Ek),
    {pure_secret(SsMlkem, MlkemCt, key_hash(key_as_carried(Recipient))), MlkemCt};
sender_secret(pq_hybrid, #{mlkem_ek := Ek, p384_pub := RecipientP384} = Recipient) ->
    {SsMlkem, MlkemCt} = crypto:encapsulate_key(mlkem1024, Ek),
    {EphPub, EphPriv} = crypto:generate_key(ecdh, secp384r1),
    SsEcdh = crypto:compute_key(ecdh, RecipientP384, EphPriv, secp384r1),
    {hybrid_secret(SsMlkem, SsEcdh, MlkemCt, EphPub, key_hash(key_as_carried(Recipient))),
     <<MlkemCt/binary, EphPub/binary>>}.

%% @doc The shared secret a `kem_ct' carries, recovered with the recipient's
%% private key: the recipient's side. `Carried' is the recipient's own key as
%% carried, which the combiner binds. A `kem_ct' of the wrong length, a P-384
%% point not on the curve, or an ECDH output of zero is refused.
-spec recipient_secret(profile(), private_key(), binary(), binary()) ->
    {ok, binary()} | {error, sealed_refused}.
recipient_secret(pq_pure, #{mlkem_dk := Dk}, Carried, <<MlkemCt:?MLKEM_CT_BYTES/binary>>) ->
    {ok, pure_secret(crypto:decapsulate_key(mlkem1024, Dk, MlkemCt), MlkemCt, key_hash(Carried))};
recipient_secret(pq_hybrid, #{mlkem_dk := Dk, p384_priv := P384Priv}, Carried,
                 <<MlkemCt:?MLKEM_CT_BYTES/binary, EphPub:?P384_POINT_BYTES/binary>>) ->
    hybrid_recovered(ecdh_secret(EphPub, P384Priv), crypto:decapsulate_key(mlkem1024, Dk, MlkemCt),
                     MlkemCt, EphPub, key_hash(Carried));
recipient_secret(_Profile, _Private, _Carried, _KemCt) ->
    {error, sealed_refused}.

hybrid_recovered({ok, SsEcdh}, SsMlkem, MlkemCt, EphPub, KeyHash) ->
    {ok, hybrid_secret(SsMlkem, SsEcdh, MlkemCt, EphPub, KeyHash)};
hybrid_recovered(error, _SsMlkem, _MlkemCt, _EphPub, _KeyHash) ->
    {error, sealed_refused}.

%% The ECDH secret with a peer's point: `crypto' refuses a point that is not
%% on the curve, and an all-zero output is refused here.
ecdh_secret(<<4, _:96/binary>> = Point, Priv) ->
    nonzero(catch crypto:compute_key(ecdh, Point, Priv, secp384r1));
ecdh_secret(_NotAnUncompressedPoint, _Priv) ->
    error.

nonzero(<<0:384>>) -> error;
nonzero(<<_:384>> = Secret) -> {ok, Secret};
nonzero(_Refused) -> error.

pure_secret(SsMlkem, MlkemCt, KeyHash) ->
    extract(<<"MACULA-E2E-PURE-V1">>, cbor([SsMlkem, MlkemCt, KeyHash])).

hybrid_secret(SsMlkem, SsEcdh, MlkemCt, EphPub, KeyHash) ->
    extract(<<"MACULA-E2E-HYBRID-V1">>, cbor([SsMlkem, SsEcdh, MlkemCt, EphPub, KeyHash])).

%%====================================================================
%% Keys
%%====================================================================

%% @doc The request and reply keys of one call or STREAM_OPEN.
-spec call_keys(binary(), binary(), parties()) -> {<<_:256>>, <<_:256>>}.
call_keys(Secret, FrameType, {RequestId, Caller, Target}) ->
    <<KReq:32/binary, KRep:32/binary>> =
        expand(Secret, cbor([text(<<"MACULA-E2E-CALL-V1">>), text(FrameType), RequestId, Caller, Target]), 64),
    {KReq, KRep}.

%% @doc The caller-to-provider and provider-to-caller keys of one stream.
-spec stream_keys(binary(), parties()) -> {<<_:256>>, <<_:256>>}.
stream_keys(Secret, {RequestId, Caller, Target}) ->
    <<KC2P:32/binary, KP2C:32/binary>> =
        expand(Secret, cbor([text(<<"MACULA-E2E-STREAM-V1">>), RequestId, Caller, Target]), 64),
    {KC2P, KP2C}.

%% @doc A publisher's subkey of a group epoch key: every publisher seals
%% under its own, so no two ever share a key.
-spec event_key(<<_:256>>, <<_:256>>) -> <<_:256>>.
event_key(GroupKey, Publisher) ->
    expand(extract(<<"MACULA-E2E-EVENT-V1">>, GroupKey),
           cbor([text(<<"MACULA-E2E-EVENT-V1">>), Publisher]), 32).

%%====================================================================
%% AAD and nonces
%%====================================================================

%% @doc What a request's sealed payload is bound to: its routing fields.
-spec request_aad(request()) -> binary().
request_aad(Request) ->
    cbor(request_fields(Request)).

%% @doc What a reply's sealed payload is bound to: its request's routing
%% fields, the reply's frame type, the request hash and the provider.
-spec reply_aad(request(), binary(), <<_:384>>, <<_:256>>) -> binary().
reply_aad(Request, ReplyFrameType, RequestHash, RespondedBy) ->
    cbor(request_fields(Request#{frame_type := ReplyFrameType}) ++ [RequestHash, RespondedBy]).

request_fields(#{frame_type := FrameType, realm := Realm, procedure := Procedure, caller := Caller,
                 target := Target, request_id := RequestId, deadline := Deadline}) ->
    [text(<<"MACULA-E2E-AAD-V1">>), text(FrameType), Realm, text(Procedure), Caller, Target, RequestId, Deadline].

%% @doc What a stream frame's sealed body is bound to. `Direction' is 0 from
%% caller to provider and 1 back.
-spec stream_aad(binary(), binary(), non_neg_integer(), 0 | 1) -> binary().
stream_aad(FrameType, RequestId, Seq, Direction) when Direction =:= 0; Direction =:= 1 ->
    cbor([text(<<"MACULA-E2E-STREAM-AAD-V1">>), text(FrameType), RequestId, Seq, Direction]).

%% @doc What an event's sealed payload is bound to.
-spec event_aad(<<_:256>>, binary(), <<_:256>>, non_neg_integer(), non_neg_integer()) -> binary().
event_aad(Realm, Topic, Publisher, Seq, PublishedAt) ->
    cbor([text(<<"MACULA-E2E-EVENT-AAD-V1">>), Realm, text(Topic), Publisher, Seq, PublishedAt]).

%% @doc A caller stream frame's nonce: its seq, as a 96-bit big-endian
%% integer.
-spec stream_nonce(non_neg_integer()) -> <<_:96>>.
stream_nonce(Seq) when is_integer(Seq), Seq >= 0, Seq < 1 bsl 64 ->
    <<Seq:96/big>>.

%% @doc A fresh random nonce, for a reply, a provider stream frame or an
%% event.
-spec random_nonce() -> <<_:96>>.
random_nonce() ->
    crypto:strong_rand_bytes(?NONCE_BYTES).

%%====================================================================
%% Sealing
%%====================================================================

%% @doc AES-256-GCM: the ciphertext with its 16-byte tag appended.
-spec seal(<<_:256>>, <<_:96>>, binary(), binary()) -> binary().
seal(<<_:256>> = Key, <<_:96>> = Nonce, Aad, Plain) ->
    {Ct, Tag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, Nonce, Plain, Aad, ?TAG_BYTES, true),
    <<Ct/binary, Tag/binary>>.

%% @doc The plaintext of a sealed payload, or `sealed_refused' when the key,
%% the nonce, the AAD or a single bit of it differ.
-spec open(<<_:256>>, <<_:96>>, binary(), binary()) -> {ok, binary()} | {error, sealed_refused}.
open(<<_:256>> = Key, <<_:96>> = Nonce, Aad, Sealed) when byte_size(Sealed) >= ?TAG_BYTES ->
    CtBytes = byte_size(Sealed) - ?TAG_BYTES,
    <<Ct:CtBytes/binary, Tag:?TAG_BYTES/binary>> = Sealed,
    opened(crypto:crypto_one_time_aead(aes_256_gcm, Key, Nonce, Ct, Aad, Tag, false));
open(_Key, _Nonce, _Aad, _TooShort) ->
    {error, sealed_refused}.

opened(error) -> {error, sealed_refused};
opened(Plain) when is_binary(Plain) -> {ok, Plain}.

%%====================================================================
%% HKDF-SHA-384 (RFC 5869) and the deterministic CBOR the labels use
%%====================================================================

extract(Salt, Ikm) ->
    crypto:mac(hmac, sha384, Salt, Ikm).

expand(Prk, Info, Length) ->
    expand(Prk, Info, Length, 1, <<>>, <<>>).

expand(_Prk, _Info, Length, _N, _Prev, Acc) when byte_size(Acc) >= Length ->
    binary:part(Acc, 0, Length);
expand(Prk, Info, Length, N, Prev, Acc) ->
    Block = crypto:mac(hmac, sha384, Prk, <<Prev/binary, Info/binary, N:8>>),
    expand(Prk, Info, Length, N + 1, Block, <<Acc/binary, Block/binary>>).

%% Labels and names are CBOR text, byte strings are CBOR bytes, integers are
%% CBOR unsigned integers, in `macula_record_cbor''s deterministic encoding.
cbor(Items) ->
    macula_record_cbor:encode(Items).

text(Bin) -> {text, Bin}.
