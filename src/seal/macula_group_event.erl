%% @doc An event sealed under a sealed group's epoch (plans/DESIGN_E2E_SEALED_PUBSUB.md §7, test/vectors/E2E_SEAL_V1.md).
%%
%% A publisher seals under its own subkey of the epoch key, `k_pub = event_key(k_g, publisher)', so no two publishers
%% ever share a key, with a fresh 96-bit random nonce, bound by the AAD to the publication's routing fields (realm,
%% topic, publisher, seq, published_at). The plaintext is the payload's deterministic CBOR, as the tbs would carry it
%% in the clear. The `sealed' map names the epoch by its id (`key_id') and carries the nonce; a PUBLISH carries it in
%% place of `payload' (`macula_frame:publish/2').
%%
%% Both functions are pure: which epoch to seal under, or to open with, is the keyring's (`macula_group_keyring').
-module(macula_group_event).

-export([seal/3, open/2]).

-type fields() :: #{publisher := <<_:256>>, realm := <<_:256>>, topic := binary(), seq := non_neg_integer(),
                    published_at := non_neg_integer()}.
%% What opening reads of a publication: the fields it was sealed with, and its seal. A verified publication is one.
-type sealed_event() :: #{publisher := <<_:256>>, realm := <<_:256>>, topic := binary(), seq := non_neg_integer(),
                          published_at := non_neg_integer(), sealed => macula_frame:sealed(), atom() => term()}.
-export_type([fields/0, sealed_event/0]).

%% @doc The `sealed' map of an event carrying `Payload', for a publication with `Fields', under `Epoch'.
-spec seal(macula_group_epoch:epoch(), fields(), term()) ->
          {ok, macula_frame:sealed()} | {error, {unsupported_payload_type, atom(), [term()]}}.
seal(#{id := Id, key := GroupKey}, #{publisher := Publisher} = Fields, Payload) ->
    sealed(macula_frame:payload_plain(Payload), Id, macula_seal:event_key(GroupKey, Publisher), aad(Fields)).

sealed({ok, Plain}, Id, Key, Aad) ->
    Nonce = macula_seal:random_nonce(),
    {ok, #{scheme => 1, key_id => Id, nonce => Nonce, ct => macula_seal:seal(Key, Nonce, Aad, Plain)}};
sealed({error, _} = Refused, _Id, _Key, _Aad) ->
    Refused.

%% @doc The payload of a verified publication sealed under `Epoch', in the shape a clear payload arrives in.
%% `tag_invalid' when it does not open: another epoch's key, a routing field that is not the one it was sealed with, or
%% a byte of it changed. `not_sealed' for a publication in the clear.
-spec open(macula_group_epoch:epoch(), sealed_event()) ->
          {ok, term()} | {error, tag_invalid | not_sealed}.
open(#{key := GroupKey}, #{sealed := #{nonce := Nonce, ct := Ct}, publisher := Publisher} = Publication) ->
    opened(macula_seal:open(macula_seal:event_key(GroupKey, Publisher), Nonce, aad(Publication), Ct));
open(_Epoch, _ClearPublication) ->
    {error, not_sealed}.

opened({ok, Plain}) -> payload(macula_frame:plain_payload(Plain));
opened({error, sealed_refused}) -> {error, tag_invalid}.

payload({ok, Payload}) -> {ok, Payload};
payload({error, sealed_refused}) -> {error, tag_invalid}.

aad(#{realm := Realm, topic := Topic, publisher := Publisher, seq := Seq, published_at := PublishedAt}) ->
    macula_seal:event_aad(Realm, Topic, Publisher, Seq, PublishedAt).
