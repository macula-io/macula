%% @doc The JSON-lines export of an audit chain (macula#175 WP1), as docs/design/DESIGN_AUDIT_CHAIN.md lays it out.
%%
%% One line of three shapes, in one order: the header once, then one line per entry in ascending sequence, then one
%% line per checkpoint in ascending checkpoint sequence. The bytes are the authority — an entry line carries the exact
%% CBOR it was hashed as, and the readable fields beside it are re-derived from those bytes on the way out and checked
%% against them on the way in.
%%
%% A SIEM can take the lines without any Macula code, which is the point: the verifier and a stranger's log store read
%% the same file.
-module(macula_audit_export).

-export([encode/3, parse/1]).

-export_type([export/0, entry_line/0]).

-define(VERSION, 1).

-type export() :: #{header := map(), entries := [entry_line()], checkpoints := [map()]}.
-type entry_line() :: #{bytes := binary(), n := pos_integer(), h := binary(), k := binary(),
                        t := non_neg_integer(), c := pos_integer(), p := binary(),
                        s => binary(), r => binary(), d => binary()}.

%% @doc The JSON lines for a chain: `Header' is `#{node, segment, sealed}', `EntryBytes' are the entries as the chain
%% wrote them, and `Checkpoints' are the signed checkpoints, both in chain order.
-spec encode(map(), [binary()], [macula_audit_checkpoint:checkpoint()]) -> {ok, iodata()} | {error, term()}.
encode(#{node := Node, segment := Segment, sealed := Sealed}, EntryBytes, Checkpoints)
  when is_binary(Node), is_integer(Segment), is_boolean(Sealed) ->
    case entry_lines(EntryBytes, []) of
        {ok, Lines} ->
            Header = #{<<"chain">> => #{<<"v">> => ?VERSION,
                                        <<"node">> => hex(Node),
                                        <<"segment">> => Segment,
                                        <<"sealed">> => Sealed}},
            {ok, [json(Header) | Lines] ++ checkpoint_lines(Checkpoints)};
        {error, _} = Error ->
            Error
    end;
encode(_Header, _EntryBytes, _Checkpoints) ->
    {error, {malformed, export_header}}.

%% @doc Read an export, holding it to the strict order: a second header, an entry after a checkpoint, a repeated or
%% descending sequence number, or a line that is not one of the three shapes is refused here. Sequence *gaps* are not
%% refused here — they are a finding for the verifier, which reports the first bad place with its name.
-spec parse(binary()) -> {ok, export()} | {error, term()}.
parse(Bytes) when is_binary(Bytes) ->
    case [Line || Line <- binary:split(Bytes, <<"\n">>, [global]), Line =/= <<>>] of
        []            -> {error, empty_export};
        [First|Rest]  -> with_header(decode(First), Rest, 1)
    end;
parse(_NotBytes) ->
    {error, not_bytes}.

%%------------------------------------------------------------------
%% Writing
%%------------------------------------------------------------------

entry_lines([], Acc) ->
    {ok, lists:reverse(Acc)};
entry_lines([Bytes|Rest], Acc) when is_binary(Bytes) ->
    case macula_audit_entry:decode(Bytes) of
        {ok, Entry} -> entry_lines(Rest, [json(entry_line(Bytes, Entry)) | Acc]);
        {error, Why} -> {error, {not_an_entry, Why}}
    end;
entry_lines(_NotBytes, _Acc) ->
    {error, {not_an_entry, not_bytes}}.

entry_line(Bytes, Entry) ->
    Line = #{<<"entry">> => hex(Bytes),
             <<"n">> => maps:get(n, Entry),
             <<"k">> => maps:get(k, Entry),
             <<"t">> => maps:get(t, Entry),
             <<"c">> => maps:get(c, Entry),
             <<"p">> => hex(maps:get(p, Entry)),
             <<"h">> => hex(macula_audit_entry:hash(Bytes))},
    lists:foldl(fun(Key, Acc) -> optional(Key, Entry, Acc) end, Line, [s, r, d]).

optional(Key, Entry, Line) ->
    case maps:find(Key, Entry) of
        {ok, Value} -> Line#{text(Key) => value(Key, Value)};
        error       -> Line
    end.

value(r, Reason) -> Reason;
value(_Key, Bytes) -> hex(Bytes).

text(s) -> <<"s">>;
text(r) -> <<"r">>;
text(d) -> <<"d">>.

checkpoint_lines(Checkpoints) ->
    [json(#{<<"checkpoint">> => hex(macula_audit_checkpoint:encode_bytes(Checkpoint)),
            <<"cs">> => macula_audit_checkpoint:cs(Checkpoint),
            <<"n">> => macula_audit_checkpoint:count(Checkpoint),
            <<"head">> => hex(macula_audit_checkpoint:head(Checkpoint)),
            <<"prev">> => hex(prev(Checkpoint))})
     || Checkpoint <- Checkpoints].

prev(Checkpoint) ->
    maps:get(prev, Checkpoint).

json(Map) ->
    [json:encode(Map), <<"\n">>].

hex(Bytes) ->
    binary:encode_hex(Bytes, lowercase).

%%------------------------------------------------------------------
%% Reading
%%------------------------------------------------------------------

decode(Line) ->
    try json:decode(Line) of
        Map when is_map(Map) -> {ok, Map};
        _Other               -> {error, not_a_map}
    catch
        _:_ -> {error, not_json}
    end.

with_header({ok, #{<<"chain">> := Chain}}, Rest, Index) when is_map(Chain) ->
    case header_fields(Chain) of
        {ok, Header} -> entries(Rest, [], Index, Header);
        {error, Why} -> {error, {malformed_header, Why}}
    end;
with_header({ok, _Other}, _Rest, Index) ->
    {error, {not_a_header, Index}};
with_header({error, Why}, _Rest, Index) ->
    {error, {malformed_line, Index, Why}}.

header_fields(#{<<"v">> := ?VERSION, <<"node">> := Node, <<"segment">> := Segment, <<"sealed">> := Sealed})
  when is_binary(Node), is_integer(Segment), is_boolean(Sealed) ->
    case unhex(Node) of
        {ok, NodeId} -> {ok, #{node => NodeId, segment => Segment, sealed => Sealed}};
        _            -> {error, node_not_hex}
    end;
header_fields(_Other) ->
    {error, fields}.

entries([], Entries, _Index, Header) ->
    checkpoint_run([], lists:reverse(Entries), Header, []);
entries([Line|Rest], Entries, Index, Header) ->
    case decode(Line) of
        {ok, #{<<"entry">> := _} = Line0} ->
            case entry_fields(Line0) of
                {ok, EntryLine} -> ascending_entry(EntryLine, Rest, Entries, Index, Header);
                {error, Why}    -> {error, {malformed_entry, Index + 1, Why}}
            end;
        {ok, #{<<"chain">> := _}} ->
            {error, {out_of_order, second_header}};
        {ok, _Other} ->
            %% The entry run has ended: what follows is the checkpoint run, and nothing may follow it out of order.
            checkpoint_run([Line|Rest], lists:reverse(Entries), Header, []);
        {error, Why} ->
            {error, {malformed_line, Index + 1, Why}}
    end.

%% Entries are read in ascending sequence with no repeats. A GAP is not refused here: that is a finding, and the
%% verifier names it. A repeat or a step backwards is refused here, because sorting it into place is how a spliced
%% export would pass.
ascending_entry(#{n := Seq} = EntryLine, Rest, [], Index, Header) when is_integer(Seq) ->
    entries(Rest, [EntryLine], Index + 1, Header);
ascending_entry(#{n := Seq} = EntryLine, Rest, [#{n := Last} | _] = Entries, Index, Header) ->
    case Seq > Last of
        true  -> entries(Rest, [EntryLine | Entries], Index + 1, Header);
        false -> {error, {out_of_order, {entry, Seq}}}
    end;
ascending_entry(_EntryLine, _Rest, _Entries, Index, _Header) ->
    {error, {malformed_entry, Index + 1, sequence_not_an_integer}}.

checkpoint_run([], Entries, Header, Checkpoints) ->
    {ok, #{header => Header, entries => Entries, checkpoints => lists:reverse(Checkpoints)}};
checkpoint_run([Line|Rest], Entries, Header, Checkpoints) ->
    case decode(Line) of
        {ok, #{<<"checkpoint">> := _} = Line0} ->
            case checkpoint_fields(Line0) of
                {ok, Checkpoint} -> ascending_checkpoint(Checkpoint, Rest, Entries, Header, Checkpoints);
                {error, Why}     -> {error, {malformed_checkpoint, Why}}
            end;
        {ok, #{<<"chain">> := _}} ->
            {error, {out_of_order, second_header}};
        {ok, _Other} ->
            {error, {out_of_order, entry_after_checkpoint}};
        {error, Why} ->
            {error, {malformed_line, Why}}
    end.

entry_fields(Line) ->
    with_many([<<"entry">>, <<"n">>, <<"k">>, <<"t">>, <<"c">>, <<"p">>, <<"h">>], Line,
              fun() -> entry_values(Line) end).

entry_values(Line) ->
    #{<<"entry">> := Entry, <<"n">> := Seq, <<"k">> := Kind, <<"t">> := Time, <<"c">> := Count,
      <<"p">> := Prev, <<"h">> := Hash} = Line,
    case {unhex(Entry), unhex(Prev), unhex(Hash)} of
        {{ok, EntryBytes}, {ok, PrevBytes}, {ok, HashBytes}} when is_integer(Seq), is_binary(Kind),
                                                                  is_integer(Time), is_integer(Count) ->
            with_optional(#{bytes => EntryBytes, n => Seq, k => Kind, t => Time, c => Count,
                            p => PrevBytes, h => HashBytes}, Line);
        _ ->
            {error, fields}
    end.

with_optional(EntryLine, Line) ->
    case optional_hexes(Line, [s, d]) of
        {ok, Digests} ->
            case maps:find(<<"r">>, Line) of
                {ok, Reason} when is_binary(Reason) -> {ok, maps:merge(EntryLine, Digests#{r => Reason})};
                {ok, _NotText}                      -> {error, reason_not_text};
                error                               -> {ok, maps:merge(EntryLine, Digests)}
            end;
        {error, Why} ->
            {error, Why}
    end.

optional_hexes(Line, Keys) ->
    lists:foldl(fun hex_field/2, {ok, #{}}, [{Key, maps:find(text(Key), Line)} || Key <- Keys]).

hex_field({Key, {ok, Value}}, {ok, Acc}) when is_binary(Value) ->
    case unhex(Value) of
        {ok, Bytes} -> {ok, Acc#{Key => Bytes}};
        error       -> {error, {not_hex, Key}}
    end;
hex_field({_Key, error}, {ok, Acc}) ->
    {ok, Acc};
hex_field({Key, _Other}, _Acc) ->
    {error, {bad_field, Key}}.

ascending_checkpoint(#{cs := _Cs} = Checkpoint, Rest, Entries, Header, []) ->
    checkpoint_run(Rest, Entries, Header, [Checkpoint]);
ascending_checkpoint(#{cs := Cs} = Checkpoint, Rest, Entries, Header, [#{cs := Last} | _] = Checkpoints) ->
    case is_integer(Cs) andalso Cs > Last of
        true  -> checkpoint_run(Rest, Entries, Header, [Checkpoint | Checkpoints]);
        false -> {error, {out_of_order, {checkpoint, Cs}}}
    end;
ascending_checkpoint(_Checkpoint, _Rest, _Entries, _Header, _Checkpoints) ->
    {error, malformed_checkpoint_sequence}.

checkpoint_fields(Line) ->
    with_many([<<"checkpoint">>, <<"cs">>, <<"n">>, <<"head">>, <<"prev">>], Line,
              fun() -> checkpoint_values(Line) end).

checkpoint_values(Line) ->
    #{<<"checkpoint">> := Object, <<"cs">> := Cs, <<"n">> := Count, <<"head">> := Head,
      <<"prev">> := Prev} = Line,
    case {unhex(Object), unhex(Head), unhex(Prev)} of
        {{ok, ObjectBytes}, {ok, HeadBytes}, {ok, PrevBytes}} when is_integer(Cs), is_integer(Count) ->
            {ok, #{bytes => ObjectBytes, cs => Cs, n => Count, head => HeadBytes, prev => PrevBytes}};
        _ ->
            {error, fields}
    end.

with_many(Keys, Line, Then) ->
    case [Key || Key <- Keys, not maps:is_key(Key, Line)] of
        []        -> Then();
        [Missing|_] -> {error, {missing, Missing}}
    end.

unhex(Hex) ->
    try binary:decode_hex(Hex) of
        Bytes -> {ok, Bytes}
    catch
        _:_ -> error
    end.
