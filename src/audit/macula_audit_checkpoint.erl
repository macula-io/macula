%% @doc A checkpoint of an audit chain, and its check at RECEIPT (macula#175 WP1), as
%% docs/design/DESIGN_AUDIT_CHAIN.md lays it out.
%%
%% A checkpoint is the claim that everything up to entry `n' is fixed: its sequence number `cs', the entry count `n',
%% the head hash, the previous checkpoint's hash in `prev', and the interval `i' in seconds at which this node promises
%% the next one. It is signed with the node's identity key through `macula_signed_object' under a signing domain of its
%% own, so a checkpoint signature can never be read as a transport signature and the other way round.
%%
%% A checkpoint's own hash is the SHA-384 of its signed bytes (the `tbs' as received, which carry the fields and the
%% algorithm), so `prev' links checkpoints exactly as `p' links entries, and the verifier hashes what it was given
%% without re-encoding anything.
%%
%% One rule lives here rather than in the verifier because an anchor destination needs it the moment a checkpoint
%% arrives: it never accepts one that does not extend the last it holds, and it never accepts an old sequence again.
-module(macula_audit_checkpoint).

-export([sign/2, verify/2, encode_bytes/1, received/2, hash/1, cs/1, head/1, count/1, label/0]).

-export_type([checkpoint/0, refusal/0, fields/0]).

-define(LABEL, <<"MACULA-AUDIT-CHECKPOINT-V1">>).
-define(VERSION, 1).
-define(NODE_ID_BYTES, 32).
-define(HASH_BYTES, 48).

-define(TEXT, [{v, <<"v">>}, {node, <<"node">>}, {cs, <<"cs">>}, {n, <<"n">>},
               {head, <<"head">>}, {prev, <<"prev">>}, {i, <<"i">>}, {alg, <<"alg">>}]).

-type fields() :: #{v := 1, node := binary(), cs := pos_integer(), n := non_neg_integer(),
                    head := binary(), prev := binary(), i := pos_integer()}.

%% The signed object as it travels, with the facts a chain and a destination ask of it read out once.
-type checkpoint() :: #{bytes := binary(),
                        cs := pos_integer(),
                        n := non_neg_integer(),
                        head := binary(),
                        prev := binary(),
                        hash := binary(),
                        i := pos_integer(),
                        object := macula_signed_object:object()}.

-type refusal() :: {bad_field, atom()} | {malformed, term()} | macula_signed_object:refusal().

%%------------------------------------------------------------------
%% Signing
%%------------------------------------------------------------------

%% @doc Sign a checkpoint's fields with the node's identity key. `prev' is empty for the first checkpoint of a chain
%% and otherwise the previous checkpoint's hash; `i' is the interval the node promises for the next one.
-spec sign(fields(), macula_node_keys:node_key()) -> {ok, checkpoint()} | {error, refusal()}.
sign(Fields, Key) when is_map(Fields) ->
    case checked(Fields) of
        ok    -> {ok, checkpoint(macula_signed_object:sign(?LABEL, text_fields(Fields), Key))};
        Error -> Error
    end;
sign(_Fields, _Key) ->
    {error, {malformed, not_a_map}}.

%% @doc The signed object's bytes as they travel in an export, which is what a verifier is handed.
-spec encode_bytes(checkpoint()) -> binary().
encode_bytes(#{object := Object}) ->
    macula_signed_object:encode(Object).

%%------------------------------------------------------------------
%% Verifying
%%------------------------------------------------------------------

%% @doc Read a checkpoint from its signed bytes, under the verifier's profile and the checkpoint label. The signature
%% is checked before the fields are read, and the fields are then held to the shape the format gives them.
-spec verify(binary(), macula_crypto_profile:profile()) -> {ok, checkpoint()} | {error, refusal()}.
verify(Bytes, Profile) when is_binary(Bytes) ->
    case macula_signed_object:decode(Bytes) of
        {ok, Object} ->
            case macula_signed_object:verify(?LABEL, Object, Profile) of
                {ok, #{fields := Decoded}} -> from_fields(Decoded, Object);
                {error, _} = Error        -> Error
            end;
        {error, Why} ->
            {error, {malformed, Why}}
    end;
verify(_NotBytes, _Profile) ->
    {error, {malformed, not_bytes}}.

%% @doc The fork and replay check at receipt: the checkpoint just received, against the last one this destination
%% holds for the node. An unchanged checkpoint delivered twice is not an event; a repeated sequence with a different
%% hash, a sequence that goes backwards, or a checkpoint that does not extend the one held is refused.
-spec received(checkpoint() | undefined, checkpoint()) -> ok | {error, term()}.
received(undefined, _New) ->
    ok;
received(#{cs := Cs, hash := Hash}, #{cs := Cs, hash := Hash}) ->
    ok;
received(#{cs := Cs}, #{cs := Cs}) ->
    {error, {checkpoint_replayed, Cs}};
received(#{cs := Last}, #{cs := Cs}) when Cs < Last ->
    {error, {checkpoint_replayed, Cs}};
received(#{cs := Last, hash := LastHash, n := LastCount}, #{cs := Cs, prev := Prev, n := Count}) ->
    extending(Cs =:= Last + 1 andalso Prev =:= LastHash andalso Count >= LastCount, Cs).

extending(true, _Cs)  -> ok;
extending(false, Cs)  -> {error, {checkpoint_not_extending, Cs}}.

%%------------------------------------------------------------------
%% Reading a checkpoint
%%------------------------------------------------------------------

%% @doc The SHA-384 of the checkpoint's signed bytes: the hash a `prev' field and an anchor name.
-spec hash(checkpoint()) -> binary().
hash(#{hash := Hash}) ->
    Hash.

%% @doc The checkpoint's sequence number, 1, 2, 3, … with no gaps.
-spec cs(checkpoint()) -> pos_integer().
cs(#{cs := Cs}) ->
    Cs.

%% @doc The hash of the last entry this checkpoint covers.
-spec head(checkpoint()) -> binary().
head(#{head := Head}) ->
    Head.

%% @doc How many entries the chain had when this checkpoint was taken.
-spec count(checkpoint()) -> non_neg_integer().
count(#{n := Count}) ->
    Count.

%% @doc The signing domain a checkpoint is signed under.
-spec label() -> binary().
label() ->
    ?LABEL.

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

checkpoint(Object) ->
    Bytes = macula_signed_object:encode(Object),
    {ok, #{key := _Key, tbs := Tbs, signature := _Signature}} = macula_signed_object:decode(Bytes),
    {ok, #{fields := Decoded}} = macula_signed_object:verify(?LABEL, Object, pq_pure),
    Fields = atom_fields(Decoded),
    Fields#{bytes => Bytes,
            hash => crypto:hash(sha384, Tbs),
            object => Object}.

checked(Fields) ->
    case [Key || Key <- [v, node, cs, n, head, prev, i], not maps:is_key(Key, Fields)] of
        [Missing|_] -> {error, {bad_field, Missing}};
        []          -> well_typed(Fields)
    end.

well_typed(Fields) ->
    case [Key || Key <- maps:keys(Fields), Key =/= alg, not typed(Key, maps:get(Key, Fields))] of
        []        -> ok;
        [First|_] -> {error, {bad_field, First}}
    end.

typed(v, Value)      -> Value =:= ?VERSION;
typed(node, NodeId)  -> is_binary(NodeId) andalso byte_size(NodeId) =:= ?NODE_ID_BYTES;
typed(cs, Cs)        -> is_integer(Cs) andalso Cs >= 1;
typed(n, Count)      -> is_integer(Count) andalso Count >= 0;
typed(head, Head)    -> hash_or_empty(Head);
typed(prev, Prev)    -> hash_or_empty(Prev);
typed(i, Interval)   -> is_integer(Interval) andalso Interval >= 1;
typed(_Other, _Value) -> false.

%% A head and a prev are a SHA-384 hash, and empty before the first entry and the first checkpoint.
hash_or_empty(Value) ->
    is_binary(Value) andalso (byte_size(Value) =:= ?HASH_BYTES orelse Value =:= <<>>).

text_fields(Fields) ->
    maps:from_list([{{text, proplists:get_value(Key, ?TEXT)}, Value}
                    || {Key, Value} <- maps:to_list(Fields), Key =/= alg]).

atom_fields(Decoded) ->
    maps:from_list([{atom_key(Text), text_value_back(atom_key(Text), Value)}
                    || {{text, Text}, Value} <- maps:to_list(Decoded), Text =/= <<"alg">>]).

atom_key(Text) ->
    case [Key || {Key, Under} <- ?TEXT, Under =:= Text] of
        [Key] -> Key;
        []    -> alg
    end.

text_value_back(_Key, {text, Value}) -> Value;
text_value_back(_Key, Value) -> Value.

from_fields(Decoded, Object) ->
    case missing_text(Decoded) of
        [] ->
            Fields = atom_fields(Decoded),
            case well_typed(maps:without([alg], Fields)) of
                ok ->
                    {ok, #{bytes => macula_signed_object:encode(Object),
                           cs => maps:get(cs, Fields),
                           n => maps:get(n, Fields),
                           head => maps:get(head, Fields),
                           prev => maps:get(prev, Fields),
                           i => maps:get(i, Fields),
                           hash => crypto:hash(sha384, tbs(Object)),
                           object => Object}};
                {error, _} = Error ->
                    Error
            end;
        [Missing|_] ->
            {error, {bad_field, Missing}}
    end.

%% The signed bytes as received: the object carries them, so they are never re-encoded.
tbs(#{tbs := Tbs}) -> Tbs.

missing_text(Decoded) ->
    [Key || Key <- [v, node, cs, n, head, prev, i],
            not maps:is_key({text, proplists:get_value(Key, ?TEXT)}, Decoded)].
