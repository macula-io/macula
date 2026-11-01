%% @doc Check an exported audit chain against its checkpoints and the anchors a destination holds (macula#175 WP1), as
%% docs/design/DESIGN_AUDIT_CHAIN.md lays it out.
%%
%% Three answers, and the difference between them is the point: `{ok, Report}' when the chain is intact and watched,
%% `{overdue, Report}' when it is intact but nobody has anchored it inside the interval the node promised, and
%% `{error, Findings}' when it is broken. A chain nobody is watching is not a chain that passed.
%%
%% Only the first bad place is reported, never a count of everything after it. The only clock used is the anchor
%% destination's own receipt time; an entry's `t' is a hint the node writes about itself and never evidence, and nothing
%% in the export supplies a time at all.
%%
%% The anchors are what a destination holds for this node: `#{cs, n, head, prev, hash, received_at}', each one a
%% checkpoint it accepted, with the seconds of ITS clock when it did.
-module(macula_audit_verifier).

-export([verify/3]).

-export_type([finding/0, report/0]).

-type finding() :: {entry_hash_mismatch, pos_integer()}
                 | {fields_bytes_mismatch, pos_integer()}
                 | {sequence_gap, pos_integer(), pos_integer()}
                 | {prev_hash_mismatch, pos_integer()}
                 | {truncated_after_checkpoint, pos_integer(), non_neg_integer()}
                 | {anchor_mismatch, pos_integer()}
                 | {checkpoint_signature_invalid, pos_integer()}
                 | {checkpoint_not_extending, pos_integer()}
                 | {checkpoint_replayed, pos_integer()}
                 | {checkpoint_head_mismatch, pos_integer(), non_neg_integer()}
                 | {sealed_without_anchor, pos_integer()}
                 | {malformed_export, term()}.

-type report() :: map().

%% @doc Check an export — its JSON lines, or the same export already parsed — against the anchors for one node.
%%
%% `Opts' carries `profile' (the verifier's cryptography profile, `pq_pure' by default) and `now' (seconds since the
%% epoch, for the overdue test only; it defaults to the system clock).
-spec verify(binary() | map(), [map()], map()) ->
        {ok, report()} | {overdue, report()} | {error, [finding()]}.
verify(Export, Anchors, Opts) when is_binary(Export) ->
    case macula_audit_export:parse(Export) of
        {ok, Parsed} -> verify(Parsed, Anchors, Opts);
        {error, Why} -> {error, [{malformed_export, Why}]}
    end;
verify(#{header := Header, entries := Entries, checkpoints := Checkpoints}, Anchors, Opts)
  when is_list(Anchors), is_map(Opts) ->
    Profile = maps:get(profile, Opts, pq_pure),
    Now = maps:get(now, Opts, erlang:system_time(second)),
    case walk_entries(Entries) of
        {error, Finding} -> {error, [Finding]};
        {ok, Read}       -> walked(Checkpoints, Profile, Read, Header, Anchors, Now)
    end.

walked(Checkpoints, Profile, Read, Header, Anchors, Now) ->
    case walk_checkpoints(Checkpoints, Profile, Read) of
        {error, Finding} -> {error, [Finding]};
        {ok, Read1}      -> judged(Header, Read1, Anchors, Now)
    end.

%%------------------------------------------------------------------
%% The entries
%%------------------------------------------------------------------

walk_entries(Entries) ->
    walk_entries(Entries, #{count => 0, head => <<>>, hashes => #{}}).

walk_entries([], Read) ->
    {ok, Read};
walk_entries([#{bytes := Bytes, n := Seq, h := Recorded} = Line | Rest], Read) ->
    case macula_audit_entry:hash(Bytes) =:= Recorded of
        false -> {error, {entry_hash_mismatch, Seq}};
        true  -> readable(Bytes, Seq, Recorded, Line, Rest, Read)
    end.

readable(Bytes, Seq, Recorded, Line, Rest, Read) ->
    case macula_audit_entry:decode(Bytes) of
        {ok, Entry} -> sequenced(Entry, Recorded, Line, Rest, Read);
        {error, _}  -> {error, {fields_bytes_mismatch, Seq}}
    end.

sequenced(#{n := Seq} = Entry, Hash, Line, Rest, Read) ->
    case agrees(Entry, Line) of
        false -> {error, {fields_bytes_mismatch, Seq}};
        true  -> in_sequence(Entry, Hash, Rest, Read)
    end.

in_sequence(#{n := Seq, p := Prev}, Hash, Rest, #{count := Count, head := Head} = Read) ->
    case Seq =:= Count + 1 of
        false -> {error, {sequence_gap, Count + 1, Seq}};
        true  -> linked(Prev, Head, Seq, Hash, Rest, Read)
    end.

linked(Prev, Head, Seq, _Hash, _Rest, _Read) when Prev =/= Head ->
    {error, {prev_hash_mismatch, Seq}};
linked(_Prev, _Head, Seq, Hash, Rest, #{hashes := Hashes} = Read) ->
    walk_entries(Rest, Read#{count => Seq, head => Hash, hashes => Hashes#{Seq => Hash}}).

%% The readable fields beside an entry must say exactly what its own bytes say, in both directions.
agrees(Entry, Line) ->
    Fields = [{n, n}, {k, k}, {t, t}, {c, c}, {p, p}],
    lists:all(fun({Key, LineKey}) -> maps:get(Key, Entry) =:= maps:get(LineKey, Line) end, Fields)
        andalso optional_agrees(Entry, Line, [s, r, d]).

optional_agrees(Entry, Line, Keys) ->
    lists:all(fun(Key) -> maps:find(Key, Entry) =:= maps:find(Key, Line) end, Keys).

%%------------------------------------------------------------------
%% The checkpoints
%%------------------------------------------------------------------

walk_checkpoints(Checkpoints, Profile, Read) ->
    walk_checkpoints(Checkpoints, Profile, Read, undefined, []).

walk_checkpoints([], _Profile, Read, Last, Seen) ->
    {ok, Read#{last_checkpoint => Last, checkpoints => lists:reverse(Seen)}};
walk_checkpoints([#{bytes := Bytes, cs := LineCs} | Rest], Profile, Read, Last, Seen) ->
    case macula_audit_checkpoint:verify(Bytes, Profile) of
        {error, _} ->
            {error, {checkpoint_signature_invalid, LineCs}};
        {ok, Checkpoint} ->
            extending(Checkpoint, Rest, Profile, Read, Last, Seen)
    end.

extending(Checkpoint, Rest, Profile, Read, Last, Seen) ->
    Cs = macula_audit_checkpoint:cs(Checkpoint),
    case macula_audit_checkpoint:received(Last, Checkpoint) of
        {error, {checkpoint_replayed, _}}         -> {error, {checkpoint_replayed, Cs}};
        {error, {checkpoint_not_extending, _}}    -> {error, {checkpoint_not_extending, Cs}};
        ok                                        -> covered(Checkpoint, Rest, Profile, Read, Seen)
    end.

%% A checkpoint may not claim entries the export does not hold, and its head must be the hash of the entry it says.
covered(Checkpoint, Rest, Profile, Read, Seen) ->
    Cs = macula_audit_checkpoint:cs(Checkpoint),
    Count = macula_audit_checkpoint:count(Checkpoint),
    case Count =< maps:get(count, Read) of
        false -> {error, {truncated_after_checkpoint, Cs, Count}};
        true  -> head_checked(Checkpoint, Rest, Profile, Read, Seen)
    end.

head_checked(Checkpoint, Rest, Profile, Read, Seen) ->
    case covered_head(Checkpoint, Read) of
        true  -> walk_checkpoints(Rest, Profile, Read, Checkpoint, [Checkpoint | Seen]);
        false -> {error, {checkpoint_head_mismatch, macula_audit_checkpoint:cs(Checkpoint),
                          macula_audit_checkpoint:count(Checkpoint)}}
    end.

covered_head(#{n := 0, head := <<>>}, _Read) ->
    true;
covered_head(#{n := Count, head := Head}, #{hashes := Hashes}) ->
    maps:get(Count, Hashes, undefined) =:= Head;
covered_head(_Checkpoint, _Read) ->
    false.

%%------------------------------------------------------------------
%% The anchors, the seal and the clock
%%------------------------------------------------------------------

judged(Header, Read, Anchors, Now) ->
    case anchored(Read, Anchors) of
        {error, Finding} ->
            {error, [Finding]};
        ok ->
            report(Header, Read, Anchors, Now)
    end.

anchored(#{checkpoints := []}, []) ->
    ok;
anchored(#{checkpoints := []}, [Anchor | _]) ->
    %% An anchor exists for a checkpoint the export does not hold: what it covers was cut out of the chain.
    {error, {truncated_after_checkpoint, maps:get(cs, Anchor), maps:get(n, Anchor, 0)}};
anchored(#{checkpoints := Checkpoints}, Anchors) ->
    anchored_each(Anchors, Checkpoints).

anchored_each([], _Checkpoints) ->
    ok;
anchored_each([#{cs := Cs} = Anchor | Rest], Checkpoints) ->
    case [Checkpoint || #{cs := Found} = Checkpoint <- Checkpoints, Found =:= Cs] of
        [] ->
            {error, {truncated_after_checkpoint, Cs, maps:get(n, Anchor, 0)}};
        [Checkpoint] -> matching(Anchor, Checkpoint, Rest, Checkpoints)
    end.

matching(#{cs := Cs} = Anchor, Checkpoint, Rest, Checkpoints) ->
    case agrees_with_anchor(Anchor, Checkpoint) of
        true  -> anchored_each(Rest, Checkpoints);
        false -> {error, {anchor_mismatch, Cs}}
    end.

agrees_with_anchor(#{hash := Hash, head := Head}, Checkpoint) ->
    Hash =:= macula_audit_checkpoint:hash(Checkpoint)
        andalso Head =:= macula_audit_checkpoint:head(Checkpoint);
agrees_with_anchor(#{hash := Hash}, Checkpoint) ->
    Hash =:= macula_audit_checkpoint:hash(Checkpoint).

report(#{node := Node, segment := Segment, sealed := Sealed}, Read, Anchors, Now) ->
    Base = #{node => Node, segment => Segment, sealed => Sealed,
             entries => maps:get(count, Read),
             checkpoints => length(maps:get(checkpoints, Read)),
             last_checkpoint => maps:get(last_checkpoint, Read),
             anchors => length(Anchors)},
    case Sealed of
        true  -> sealed(Base, Read, Anchors);
        false -> watched(Base, Read, Anchors, Now)
    end.

%% A chain the exporter says has ENDED may not end without an anchor sealing its last checkpoint.
sealed(_Base, #{last_checkpoint := undefined}, _Anchors) ->
    {error, [{sealed_without_anchor, 0}]};
sealed(Base, #{last_checkpoint := Last}, Anchors) ->
    Cs = macula_audit_checkpoint:cs(Last),
    Hash = macula_audit_checkpoint:hash(Last),
    case [Anchor || #{cs := AnchorCs, hash := AnchorHash} = Anchor <- Anchors,
                    AnchorCs =:= Cs, AnchorHash =:= Hash] of
        []    -> {error, [{sealed_without_anchor, Cs}]};
        [_Seal| _] -> {ok, Base}
    end.

%% A chain still being written is judged by its newest anchor, against the interval its node promised.
watched(_Base, #{last_checkpoint := undefined}, _Anchors, _Now) ->
    %% No checkpoint at all: nothing has ever been anchored, so nothing is being watched.
    {overdue, #{overdue_by => undefined, interval => undefined}};
watched(Base, #{last_checkpoint := Last}, Anchors, Now) ->
    Interval = maps:get(i, Last),
    case newest_receipt(Anchors) of
        none       -> {overdue, Base#{overdue_by => undefined}};
        ReceivedAt -> aging(Base, Interval, Now - ReceivedAt)
    end.

aging(Base, Interval, Age) ->
    case Age > Interval of
        true  -> {overdue, Base#{overdue_by => Age, interval => Interval}};
        false -> {ok, Base#{overdue_by => 0, interval => Interval}}
    end.

newest_receipt([]) ->
    none;
newest_receipt(Anchors) ->
    case [maps:get(received_at, Anchor) || Anchor <- Anchors, maps:is_key(received_at, Anchor)] of
        []    -> none;
        Times -> lists:max(Times)
    end.
