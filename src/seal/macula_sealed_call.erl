%% @doc A sealed call (E2E seal scheme 1, design §5.1), one level above
%% `macula_seal': the caller seals a request to the provider's KEM key and
%% opens the reply, and the provider opens the request and seals the reply.
%%
%% Plaintext here is bytes. What a frame seals is the CBOR of its payload,
%% which `macula_frame' writes and reads; this module neither knows nor
%% cares, so a stream's STREAM_OPEN shares it.
%%
%% A request is sealed under `k_req' with the fixed nonce `0^96', safe
%% because a fresh encapsulation gives every request its own keys. A reply
%% is sealed under `k_rep' with a fresh random nonce, carried in `sealed',
%% because one request can be answered more than once (a D25 retry across a
%% provider restart).
-module(macula_sealed_call).

-export([seal_request/4, open_request/4, seal_reply/4, open_reply/4, clear_refusal/1, refused_key/1]).

-export_type([keys/0, holder/0, sealed/0, reply/0]).

-define(SCHEME, 1).
-define(REQUEST_NONCE, <<0:96>>).
%% The clear codes a provider may answer a sealed request with (E2E design §5.1): the admission refusals, which carry
%% no application data, a STREAM_OPEN's session admission (`too_many_sessions', `unavailable') included.
%% `sealed_refused' is read on its own.
-define(CLEAR_REFUSALS, [<<"expired">>, <<"not_yet_valid">>, <<"request_id_reused">>, <<"request_copy">>,
                         <<"reply_not_kept">>, <<"caller_quota">>, <<"share_full">>, <<"admission_full">>,
                         <<"too_many_sessions">>, <<"unavailable">>]).

%% A call's two keys, the request's and the reply's, and the id of the
%% provider's KEM key the call was sealed to, which the reply names too. A
%% STREAM_OPEN has its request key and the two stream keys instead of a reply
%% key (design §5.2): its later frames seal under the key for their
%% direction, and a stream's keys cannot seal a reply.
-type keys() :: #{k_req := <<_:256>>, key_id := <<_:64>>,
                  k_rep => <<_:256>>, k_c2p => <<_:256>>, k_p2c => <<_:256>>}.
%% What a provider holds: a lookup of its KEM private key and that key as
%% carried by the key's id, and the id of the key it advertises now.
-type holder() :: #{lookup := fun((<<_:64>>) -> {ok, macula_seal:private_key(), binary()} | error),
                    current_key_id := <<_:64>>}.
%% A sealed payload as a frame carries it.
-type sealed() :: #{scheme := 1, key_id := <<_:64>>, ct := binary(), kem_ct => binary(), nonce => <<_:96>>}.
%% The reply's own fields the reply's AAD binds.
-type reply() :: #{frame_type := binary(), request_hash := <<_:384>>, responded_by := <<_:256>>}.

%%====================================================================
%% The caller's side
%%====================================================================

%% @doc The key a provider's clear `sealed_refused' names in its detail (or
%% message, on a stream): the id it holds now, as 16 lowercase hex digits,
%% or `no_key' when the detail is anything else, which a provider that holds
%% no key sends.
-spec refused_key(term()) -> <<_:64>> | no_key.
refused_key(Detail) when is_binary(Detail), byte_size(Detail) =:= 16 ->
    hex_key(catch binary:decode_hex(Detail), Detail);
refused_key(_NoKey) ->
    no_key.

hex_key(<<_:64>> = KeyId, Detail) -> hex_named(binary:encode_hex(KeyId, lowercase) =:= Detail, KeyId);
hex_key(_NotHex, _Detail) -> no_key.

hex_named(true, KeyId) -> KeyId;
hex_named(false, _KeyId) -> no_key.

%% @doc Whether `Code' is one a provider may answer a sealed request, a CALL
%% or a STREAM_OPEN, with in the clear: an admission refusal, decided before
%% anything is opened and carrying no application data. A caller refuses any
%% other clear code on a sealed request as malformed.
-spec clear_refusal(binary()) -> boolean().
clear_refusal(Code) when is_binary(Code) ->
    lists:member(Code, ?CLEAR_REFUSALS).

%% @doc Seal `Plain' as `Request''s payload to the provider's KEM key, and
%% return the sealed payload and the call's keys, which open the reply.
-spec seal_request(macula_seal:profile(), macula_seal:public_key(), macula_seal:request(), binary()) ->
    {sealed(), keys()}.
seal_request(Profile, Recipient, Request, Plain) ->
    {Secret, KemCt} = macula_seal:sender_secret(Profile, Recipient),
    KeyId = macula_seal:key_id(macula_seal:key_as_carried(Recipient)),
    Keys = keys(Secret, KeyId, Request),
    Sealed = #{scheme => ?SCHEME, key_id => KeyId, kem_ct => KemCt,
               ct => macula_seal:seal(maps:get(k_req, Keys), ?REQUEST_NONCE, macula_seal:request_aad(Request), Plain)},
    {Sealed, Keys}.

%% @doc Open a sealed reply to `Request' with the call's keys. A reply naming
%% another key than the call's, or that does not open under this reply's
%% frame type, request hash and provider, is `sealed_refused'.
-spec open_reply(keys(), macula_seal:request(), reply(), sealed()) -> {ok, binary()} | {error, sealed_refused}.
open_reply(#{k_rep := KRep, key_id := KeyId}, Request, Reply,
           #{scheme := ?SCHEME, key_id := KeyId, nonce := Nonce, ct := Ct}) ->
    macula_seal:open(KRep, Nonce, reply_aad(Request, Reply), Ct);
open_reply(_Keys, _Request, _Reply, _NotASealedReply) ->
    {error, sealed_refused}.

%%====================================================================
%% The provider's side
%%====================================================================

%% @doc Open a sealed request with the provider's key it names, and return
%% the plaintext and the call's keys, which seal the reply. A request sealed
%% to a key the provider does not hold, or that does not open, is refused
%% naming the key the provider holds now, so the caller seals again to it.
-spec open_request(macula_seal:profile(), holder(), macula_seal:request(), sealed()) ->
    {ok, binary(), keys()} | {error, {sealed_refused, <<_:64>>}}.
open_request(Profile, #{lookup := Lookup, current_key_id := Current},
             Request, #{scheme := ?SCHEME, key_id := KeyId, kem_ct := KemCt, ct := Ct}) ->
    refused_naming(Current, opened_request(Lookup(KeyId), Profile, Request, KemCt, Ct));
open_request(_Profile, #{current_key_id := Current}, _Request, _NotASealedRequest) ->
    {error, {sealed_refused, Current}}.

opened_request({ok, Private, Carried}, Profile, Request, KemCt, Ct) ->
    keyed(macula_seal:recipient_secret(Profile, Private, Carried, KemCt), macula_seal:key_id(Carried), Request, Ct);
opened_request(error, _Profile, _Request, _KemCt, _Ct) ->
    {error, sealed_refused}.

keyed({ok, Secret}, KeyId, Request, Ct) ->
    Keys = keys(Secret, KeyId, Request),
    with_keys(macula_seal:open(maps:get(k_req, Keys), ?REQUEST_NONCE, macula_seal:request_aad(Request), Ct), Keys);
keyed({error, sealed_refused} = Refused, _KeyId, _Request, _Ct) ->
    Refused.

with_keys({ok, Plain}, Keys) -> {ok, Plain, Keys};
with_keys({error, sealed_refused} = Refused, _Keys) -> Refused.

refused_naming(_Current, {ok, _Plain, _Keys} = Opened) -> Opened;
refused_naming(Current, {error, sealed_refused}) -> {error, {sealed_refused, Current}}.

%% @doc Seal `Plain' as the reply to `Request' under the call's reply key,
%% with a fresh random nonce.
-spec seal_reply(keys(), macula_seal:request(), reply(), binary()) -> sealed().
seal_reply(#{k_rep := KRep, key_id := KeyId}, Request, Reply, Plain) ->
    Nonce = macula_seal:random_nonce(),
    #{scheme => ?SCHEME, key_id => KeyId, nonce => Nonce,
      ct => macula_seal:seal(KRep, Nonce, reply_aad(Request, Reply), Plain)}.

%%====================================================================
%% Internal
%%====================================================================

keys(Secret, KeyId, #{frame_type := <<"stream_open">>, request_id := RequestId, caller := Caller,
                      target := Target}) ->
    Parties = {RequestId, Caller, Target},
    {KReq, _KRep} = macula_seal:call_keys(Secret, <<"stream_open">>, Parties),
    {KC2P, KP2C} = macula_seal:stream_keys(Secret, Parties),
    #{k_req => KReq, k_c2p => KC2P, k_p2c => KP2C, key_id => KeyId};
keys(Secret, KeyId, #{frame_type := FrameType, request_id := RequestId, caller := Caller, target := Target}) ->
    {KReq, KRep} = macula_seal:call_keys(Secret, FrameType, {RequestId, Caller, Target}),
    #{k_req => KReq, k_rep => KRep, key_id => KeyId}.

reply_aad(Request, #{frame_type := ReplyFrameType, request_hash := RequestHash, responded_by := RespondedBy}) ->
    macula_seal:reply_aad(Request, ReplyFrameType, RequestHash, RespondedBy).
