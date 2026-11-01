%% @doc One entry of an audit chain (macula#175 WP1), as docs/design/DESIGN_AUDIT_CHAIN.md lays it out.
%%
%% An entry is a deterministic-CBOR map with text keys. Its bytes are the only authority: the chain link in `p' is the
%% SHA-384 of the previous entry's own bytes, and an entry's hash is the SHA-384 of its own bytes, so nothing is
%% re-encoded when a link or a hash is checked.
%%
%% No payload is carried. `s' is the node id the transport verified, where one was, and `d' is a digest taken over the
%% signed or sealed bytes of a request and never over plaintext, so sealed traffic stays sealed.
-module(macula_audit_entry).

-export([new/1, encode/1, decode/1, hash/1, kinds/0, seq/1]).

-export_type([entry/0, refusal/0]).

%% The chain format version this module writes.
-define(VERSION, 1).

%% A node id is 32 bytes (the SHA-256 of the node's identity key), a chain link and an entry hash are SHA-384.
-define(NODE_ID_BYTES, 32).
-define(HASH_BYTES, 48).
-define(MAX_REASON_BYTES, 512).
-define(MAX_DIGEST_BYTES, 64).

-define(KINDS, [<<"handshake_refused">>, <<"store_refused">>, <<"call_refused">>, <<"limits_changed">>,
                <<"key_rotated">>, <<"checkpoint_overdue">>, <<"events_dropped">>]).

%% The entry's fields, and the text key each travels under. The order here is the order of the format table.
-define(TEXT, [{v, <<"v">>}, {n, <<"n">>}, {k, <<"k">>}, {t, <<"t">>}, {s, <<"s">>},
               {r, <<"r">>}, {d, <<"d">>}, {c, <<"c">>}, {p, <<"p">>}]).
-define(REQUIRED, [v, n, k, t, c, p]).
-define(KNOWN, [v, n, k, t, s, r, d, c, p]).

-type entry() :: #{v := 1,
                   n := pos_integer(),
                   k := binary(),
                   t := non_neg_integer(),
                   s => binary(),
                   r => binary(),
                   d => binary(),
                   c := pos_integer(),
                   p := binary()}.

-type refusal() :: {missing_field, atom()}
                 | {unknown_field, atom()}
                 | {bad_field, atom()}
                 | {unknown_kind, binary()}
                 | {malformed, term()}.

%%------------------------------------------------------------------
%% Building
%%------------------------------------------------------------------

%% @doc An entry from its fields: `k', `n', `t' and `p' are required, `s', `r' and `d' are the fields a kind may carry,
%% and `c' is 1 unless the entry stands for a coalesced run. The version is written here and cannot be chosen.
-spec new(map()) -> {ok, entry()} | {error, refusal()}.
new(Fields) when is_map(Fields) ->
    Entry = Fields#{v => ?VERSION, c => maps:get(c, Fields, 1)},
    checked(Entry);
new(_NotAMap) ->
    {error, {malformed, not_a_map}}.

checked(Entry) ->
    case missing(Entry) of
        []        -> known(Entry);
        [First|_] -> {error, {missing_field, First}}
    end.

missing(Entry) ->
    [Key || Key <- ?REQUIRED, not maps:is_key(Key, Entry)].

known(Entry) ->
    case [Key || Key <- maps:keys(Entry), not lists:member(Key, ?KNOWN)] of
        [First|_] -> {error, {unknown_field, First}};
        []        -> typed(Entry)
    end.

typed(Entry) ->
    case [Key || Key <- maps:keys(Entry), not well_typed(Key, maps:get(Key, Entry))] of
        []        -> {ok, Entry};
        [First|_] -> {error, {bad_field, First}}
    end.

well_typed(v, Version)  -> Version =:= ?VERSION;
well_typed(n, Seq)      -> is_integer(Seq) andalso Seq >= 1;
well_typed(k, Kind)     -> is_binary(Kind) andalso lists:member(Kind, ?KINDS);
well_typed(t, Time)     -> is_integer(Time) andalso Time >= 0;
well_typed(s, NodeId)   -> is_binary(NodeId) andalso byte_size(NodeId) =:= ?NODE_ID_BYTES;
well_typed(r, Reason)   -> is_binary(Reason) andalso byte_size(Reason) >= 1
                               andalso byte_size(Reason) =< ?MAX_REASON_BYTES;
well_typed(d, Digest)   -> is_binary(Digest) andalso byte_size(Digest) >= 1
                               andalso byte_size(Digest) =< ?MAX_DIGEST_BYTES;
well_typed(c, Count)    -> is_integer(Count) andalso Count >= 1;
well_typed(p, Link)     -> is_binary(Link) andalso (byte_size(Link) =:= ?HASH_BYTES orelse Link =:= <<>>).

%%------------------------------------------------------------------
%% Wire form
%%------------------------------------------------------------------

%% @doc The entry's bytes: the deterministic-CBOR map of its fields, with text keys and nothing else.
-spec encode(entry()) -> binary().
encode(Entry) ->
    macula_record_cbor:encode(maps:from_list([{text_key(Key), text_value(Key, Value)}
                                              || {Key, Value} <- maps:to_list(Entry)])).

%% @doc Read an entry from its bytes, refusing a field set, a type or a kind that `new/1' would refuse.
-spec decode(binary()) -> {ok, entry()} | {error, refusal()}.
decode(Bytes) when is_binary(Bytes) ->
    case macula_record_cbor:decode_strict(Bytes) of
        {ok, Map} when is_map(Map) -> from_cbor(Map);
        {ok, _NotAMap}             -> {error, {malformed, not_a_map}};
        {error, Why}               -> {error, {malformed, Why}}
    end;
decode(_NotBytes) ->
    {error, {malformed, not_bytes}}.

%% @doc The hash of an entry's own bytes, which is what the next entry links to and what a checkpoint's head names.
-spec hash(binary()) -> binary().
hash(Bytes) when is_binary(Bytes) ->
    crypto:hash(sha384, Bytes).

%% @doc The event kinds an entry may name. A kind not on this list leaves no trace, which is why adding one is a
%% reviewed change (L3 of the threat model).
-spec kinds() -> [binary()].
kinds() ->
    ?KINDS.

%% @doc The entry's sequence number.
-spec seq(entry()) -> pos_integer().
seq(Entry) ->
    maps:get(n, Entry).

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

from_cbor(Map) ->
    keyed(maps:to_list(Map), #{}).

keyed([], Fields) ->
    checked(Fields);
keyed([{{text, Text}, Value} | Rest], Fields) ->
    case atom_key(Text) of
        {ok, Key}          -> keyed(Rest, Fields#{Key => text_value_back(Key, Value)});
        {error, _} = Error -> Error
    end;
keyed([{NotText, _Value} | _Rest], _Fields) ->
    {error, {malformed, {bad_key, NotText}}}.

atom_key(Text) ->
    case [Key || {Key, Under} <- ?TEXT, Under =:= Text] of
        [Key] -> {ok, Key};
        []    -> {error, {malformed, {unknown_key, Text}}}
    end.

text_key(Key) ->
    {text, proplists:get_value(Key, ?TEXT)}.

%% `k' and `r' travel as CBOR text strings; every other field travels as the bytes or the integer it is.
text_value(k, Value) -> {text, Value};
text_value(r, Value) -> {text, Value};
text_value(_Key, Value) -> Value.

text_value_back(k, {text, Value}) -> Value;
text_value_back(r, {text, Value}) -> Value;
text_value_back(_Key, Value) -> Value.
