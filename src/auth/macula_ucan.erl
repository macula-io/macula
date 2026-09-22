%% @doc UCAN tokens in the node's crypto profile (plan decision D7), and a provider's authorization of one (D7 check 2).
%%
%% A token is a JWT, header.payload.signature, each part base64url without padding. The header names the profile's
%% algorithm: RFC 9964's ML-DSA-87 in pq_pure, and ML-DSA-87-PS384, the IETF LAMPS composite
%% id-MLDSA87-RSA4096-PSS-SHA512, in pq_hybrid (JOSE has no name for it). The signature is the issuer's node key's
%% signature over the header and payload as they are sent, made by macula_node_keys:sign/2, so ML-DSA-87 is
%% macula-mldsa and the composite's RSA-PSS half is OTP's.
%%
%% The payload's iss is a did:key for the issuer's key as carried (D13): multibase base58btc over the varint of a
%% multicodec and the key. pq_pure uses mldsa-87-pub, 0x1212; pq_hybrid has no multicodec yet and uses Macula's own
%% key type, 0x300087, from the private-use range. aud is the audience's node_id in lowercase hex, since a token is
%% presented by the node it names, inside a request that node signed (D7 check 2). Every token has an exp, in
%% seconds.
%%
%% authorize/3 is the provider's check, after the request's own signature and target have verified: the header and
%% payload are verified over the bytes as received, never as re-encoded JSON, then the issuer, the audience, the
%% validity window and, for a membership policy, the capability. A token that names proofs is authorized only when its
%% own issuer is the one the policy requires; the chain through them is not followed.
-module(macula_ucan).

-export([create/4, authorize/3, did_key/2, carried_key/2]).

-ifdef(TEST).
-export([base58btc_encode/1, base58btc_decode/1]).
-endif.

-export_type([capability/0, policy/0, issuer_id/0, refusal/0]).

-type capability() :: #{with := binary(), can := binary()}.
%% The issuer a policy requires: a node's node_id for ucan_required, a realm key's key id for realm_member_required.
-type issuer_id()  :: <<_:256>>.
-type policy()     :: {ucan_required, issuer_id()} | {realm_member_required, issuer_id(), binary()}.
-type refusal()    :: malformed | wrong_algorithm | signature_invalid | not_the_issuer | not_the_audience | expired
                    | not_yet_valid | missing_capability.

-define(TYP, <<"JWT">>).
-define(UCV, <<"0.10.0">>).
-define(MLDSA87_PUB, 16#1212).
-define(MLDSA87_RSA4096_PUB, 16#300087).
-define(BASE58, "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz").

%%------------------------------------------------------------------
%% Making a token
%%------------------------------------------------------------------

%% @doc A token from the issuer's key, a key of a purpose that signs with the profile's identity algorithms, for the
%% audience's node_id, granting Capabilities until exp. nbf, nnc, fct and prf are optional.
-spec create(macula_node_keys:node_key(), macula_node_keys:node_id(), [capability()],
             #{exp := non_neg_integer(), nbf => non_neg_integer(), nnc => binary(), fct => map(), prf => [binary()]}) ->
        binary().
create(#{purpose := Purpose, profile := Profile} = Key, <<_:256>> = Audience, Capabilities, #{exp := Exp} = Opts)
  when (Purpose =:= identity orelse Purpose =:= realm orelse Purpose =:= org orelse Purpose =:= foundation),
       is_list(Capabilities), is_integer(Exp), Exp >= 0 ->
    Header = #{<<"alg">> => alg(Profile), <<"typ">> => ?TYP, <<"ucv">> => ?UCV},
    Claims = maps:merge(optional_claims(Opts),
                        #{<<"iss">> => did_key(macula_node_keys:public_key(Key), Profile),
                          <<"aud">> => binary:encode_hex(Audience, lowercase),
                          <<"cap">> => [#{<<"with">> => With, <<"can">> => Can}
                                        || #{with := With, can := Can} <- Capabilities],
                          <<"exp">> => Exp}),
    Input = <<(base64url(json:encode(Header)))/binary, ".", (base64url(json:encode(Claims)))/binary>>,
    <<Input/binary, ".", (base64url(macula_node_keys:sign(Input, Key)))/binary>>.

optional_claims(Opts) ->
    maps:from_list([{atom_to_binary(Name), Value} || Name := Value <- maps:with([nbf, nnc, fct, prf], Opts)]).

%%------------------------------------------------------------------
%% Authorizing a token
%%------------------------------------------------------------------

%% @doc Whether Token authorizes the verified caller under Policy, in the provider's profile at Now in seconds: its
%% claims when it does, or the first reason it does not. Never raises on the token.
-spec authorize(term(), policy(), #{caller := macula_node_keys:node_id(),
                                    profile := macula_crypto_profile:profile(), now := integer()}) ->
        {ok, map()} | {error, refusal()}.
authorize(Token, Policy, #{caller := <<_:256>>, profile := Profile, now := Now} = Context) when is_integer(Now) ->
    first_refusal([fun(T) -> parsed(T) end,
                   fun(Parsed) -> algorithm_is(alg(Profile), Parsed) end,
                   fun(Parsed) -> signed_by_iss(Parsed, Profile) end,
                   fun(Checked) -> issued_by(Policy, Checked, Profile) end,
                   fun(Checked) -> audience_is(Context, Checked) end,
                   fun(Checked) -> valid_at(Now, Checked) end,
                   fun(Checked) -> grants(Policy, Checked) end,
                   fun(#{claims := Claims}) -> {ok, Claims} end], Token).

first_refusal([], Result) -> Result;
first_refusal([Step | Steps], Input) -> next(Step(Input), Steps).

next({ok, Output}, []) -> {ok, Output};
next({ok, Output}, Steps) -> first_refusal(Steps, Output);
next({error, _} = Refused, _Steps) -> Refused.

%% The three parts, their decoded header, claims and signature, and the header and payload as received, which the
%% signature covers.
parsed(Token) when is_binary(Token) ->
    parts(binary:split(Token, <<".">>, [global]));
parsed(_Token) ->
    {error, malformed}.

parts([Header, Payload, Signature]) ->
    decoded([json_object(Header), json_object(Payload), base64url_decode(Signature)],
            <<Header/binary, ".", Payload/binary>>);
parts(_Parts) ->
    {error, malformed}.

decoded([{ok, Header}, {ok, Claims}, {ok, Signature}], Input) when is_map(Header), is_map(Claims) ->
    {ok, #{header => Header, claims => Claims, signature => Signature, input => Input}};
decoded(_Decoded, _Input) ->
    {error, malformed}.

algorithm_is(Alg, #{header := #{<<"alg">> := Alg, <<"typ">> := ?TYP, <<"ucv">> := ?UCV}} = Parsed) ->
    {ok, Parsed};
algorithm_is(_Alg, #{header := #{<<"alg">> := Other}}) when is_binary(Other) ->
    {error, wrong_algorithm};
algorithm_is(_Alg, _Parsed) ->
    {error, malformed}.

%% Every claim has its shape before anything is verified, and the signature is checked before any claim is acted on.
signed_by_iss(#{claims := #{<<"iss">> := Iss, <<"aud">> := Aud, <<"cap">> := Caps, <<"exp">> := Exp}} = Parsed,
              Profile)
  when is_binary(Iss), is_binary(Aud), is_list(Caps), is_integer(Exp) ->
    verified(carried_key(Iss, Profile), Parsed, Profile);
signed_by_iss(_Parsed, _Profile) ->
    {error, malformed}.

verified({ok, Carried}, #{input := Input, signature := Signature} = Parsed, Profile) ->
    signature_verdict(macula_node_keys:verify(Input, Signature, Carried, Profile), Parsed#{issuer_key => Carried});
verified(error, _Parsed, _Profile) ->
    {error, malformed}.

signature_verdict(true, Checked) -> {ok, Checked};
signature_verdict(false, _Checked) -> {error, signature_invalid}.

issued_by({ucan_required, NodeId}, #{issuer_key := Carried} = Checked, Profile) ->
    verdict(macula_node_keys:node_id(Carried, Profile) =:= NodeId, Checked, not_the_issuer);
issued_by({realm_member_required, KeyId, _Can}, #{issuer_key := Carried} = Checked, Profile) ->
    verdict(macula_node_keys:key_id(Carried, Profile) =:= KeyId, Checked, not_the_issuer).

audience_is(#{caller := Caller}, #{claims := #{<<"aud">> := Aud}} = Checked) ->
    verdict(Aud =:= binary:encode_hex(Caller, lowercase), Checked, not_the_audience).

valid_at(Now, #{claims := #{<<"exp">> := Exp}}) when Now >= Exp ->
    {error, expired};
valid_at(Now, #{claims := #{<<"nbf">> := Nbf}}) when is_integer(Nbf), Now < Nbf ->
    {error, not_yet_valid};
valid_at(_Now, #{claims := #{<<"nbf">> := Nbf}}) when not is_integer(Nbf) ->
    {error, malformed};
valid_at(_Now, Checked) ->
    {ok, Checked}.

grants({ucan_required, _NodeId}, Checked) ->
    {ok, Checked};
grants({realm_member_required, _KeyId, Can}, #{claims := #{<<"cap">> := Caps}} = Checked) ->
    verdict(lists:any(fun(#{<<"can">> := Granted}) -> Granted =:= Can; (_Other) -> false end, Caps), Checked,
            missing_capability).

verdict(true, Checked, _Refusal) -> {ok, Checked};
verdict(false, _Checked, Refusal) -> {error, Refusal}.

%%------------------------------------------------------------------
%% did:key
%%------------------------------------------------------------------

%% @doc The did:key for a key as carried in a profile.
-spec did_key(binary(), macula_crypto_profile:profile()) -> binary().
did_key(Carried, Profile) when is_binary(Carried) ->
    <<"did:key:z", (base58btc_encode(<<(varint(codec(Profile)))/binary, Carried/binary>>))/binary>>.

%% @doc The key a did:key carries, when it is a key in its one carried form for the profile (D13).
-spec carried_key(binary(), macula_crypto_profile:profile()) -> {ok, binary()} | error.
carried_key(<<"did:key:z", Encoded/binary>>, Profile) ->
    carried_key_of(varint(codec(Profile)), base58btc_decode(Encoded), Profile);
carried_key(_Other, _Profile) ->
    error.

carried_key_of(Prefix, Bytes, Profile) when byte_size(Bytes) > byte_size(Prefix) ->
    Size = byte_size(Prefix),
    <<Head:Size/binary, Carried/binary>> = Bytes,
    prefixed(Head =:= Prefix, Carried, Profile);
carried_key_of(_Prefix, _Bytes, _Profile) ->
    error.

prefixed(true, Carried, Profile) -> well_formed(macula_node_keys:carried_key_well_formed(Carried, Profile), Carried);
prefixed(false, _Carried, _Profile) -> error.

well_formed(true, Carried) -> {ok, Carried};
well_formed(false, _Carried) -> error.

codec(pq_pure) -> ?MLDSA87_PUB;
codec(pq_hybrid) -> ?MLDSA87_RSA4096_PUB.

%% An unsigned LEB128 varint, as multicodec prefixes are written.
varint(N) when N < 128 -> <<N>>;
varint(N) -> <<(128 bor (N band 127)), (varint(N bsr 7))/binary>>.

%%------------------------------------------------------------------
%% Encodings
%%------------------------------------------------------------------

%% Base58 with the Bitcoin alphabet, as multibase's base58btc: each leading zero byte is a leading 1.
base58btc_encode(Bytes) ->
    Zeros = leading_zeros(Bytes),
    <<_:Zeros/binary, Rest/binary>> = Bytes,
    list_to_binary([lists:duplicate(Zeros, $1) | base58_digits(binary:decode_unsigned(<<0, Rest/binary>>), [])]).

leading_zeros(<<0, Rest/binary>>) -> 1 + leading_zeros(Rest);
leading_zeros(_Bytes) -> 0.

base58_digits(0, Digits) -> Digits;
base58_digits(N, Digits) -> base58_digits(N div 58, [lists:nth(N rem 58 + 1, ?BASE58) | Digits]).

%% The bytes a base58btc text encodes, or an empty binary for one that holds a character outside the alphabet, which
%% no carried key's did:key decodes to.
base58btc_decode(Text) ->
    Ones = leading_ones(Text),
    <<_:Ones/binary, Rest/binary>> = Text,
    base58_bytes(Ones, base58_value(Rest, 0)).

leading_ones(<<$1, Rest/binary>>) -> 1 + leading_ones(Rest);
leading_ones(_Text) -> 0.

base58_value(<<>>, N) -> N;
base58_value(<<C, Rest/binary>>, N) -> base58_digit(binary:match(<<?BASE58>>, <<C>>), Rest, N).

base58_digit(nomatch, _Rest, _N) -> invalid;
base58_digit({Index, 1}, Rest, N) -> base58_value(Rest, N * 58 + Index).

base58_bytes(_Ones, invalid) -> <<>>;
base58_bytes(Ones, 0) -> binary:copy(<<0>>, Ones);
base58_bytes(Ones, N) -> <<(binary:copy(<<0>>, Ones))/binary, (binary:encode_unsigned(N))/binary>>.

alg(pq_pure) -> <<"ML-DSA-87">>;
alg(pq_hybrid) -> <<"ML-DSA-87-PS384">>.

base64url(Data) ->
    base64:encode(iolist_to_binary(Data), #{mode => urlsafe, padding => false}).

base64url_decode(Text) ->
    try base64:decode(Text, #{mode => urlsafe, padding => false}) of
        Bytes -> {ok, Bytes}
    catch
        error:_ -> error
    end.

json_object(Segment) ->
    json_of(base64url_decode(Segment)).

json_of({ok, Bytes}) ->
    try json:decode(Bytes) of
        Object when is_map(Object) -> {ok, Object};
        _NotAnObject -> error
    catch
        error:_ -> error
    end;
json_of(error) ->
    error.
