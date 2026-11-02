%% @doc What this node's frame reader tolerated from newer peers instead of closing a link (docs/design/WIRE_1_0.md
%% section 8, macula#83 decisions 5, 6 and 8), counted node-wide so an operator sees when the fleet speaks something
%% this node does not:
%%
%% <ul>
%%   <li>wire_unknown_frame_type: a frame of a type this node does not know, dropped (no label);</li>
%%   <li>wire_ext_ignored: a non-critical extension this node does not know, ignored, labelled by the extension's
%%       name;</li>
%%   <li>wire_ext_refused: a critical extension this node does not know, whose frame was dropped, labelled by the
%%       extension's name;</li>
%%   <li>wire_enum_fallback: a per-hop enum value this node does not know, read as its fallback, labelled by the
%%       field's name.</li>
%% </ul>
%%
%% An extension name comes from a peer, so the labels it makes are bounded: past 64 distinct names under a counter, a
%% new one counts there under the label other.
%%
%% The table belongs to macula_peering_sup, which creates it with new_table/0, as macula_peer_versions' is. There is
%% no process here: every count goes to the table directly, so no reader queues on it.
-module(macula_wire_tolerance).

-export([new_table/0, count/1, count/2, counters/0]).

-define(TABLE, ?MODULE).
-define(MAX_EXT_LABELS, 64).
-define(OTHER, <<"other">>).

-type counter() :: wire_unknown_frame_type | wire_ext_ignored | wire_ext_refused | wire_enum_fallback.
-type label() :: binary() | atom().
-export_type([counter/0, label/0]).

%% Rows: {wire_unknown_frame_type, Count}, {{wire_ext_ignored | wire_ext_refused, Name}, Count},
%% {{wire_enum_fallback, Field}, Count}, and {{ext_labels, Counter}, N}, how many extension names have a row under
%% that counter.

%% @doc Create the table, owned by the calling process: macula_peering_sup, whose life is the run's.
-spec new_table() -> ok.
new_table() ->
    ?TABLE = ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    ok.

%% @doc Count one frame of a type this node does not know.
-spec count(wire_unknown_frame_type) -> ok.
count(wire_unknown_frame_type) ->
    counting(ets:whereis(?TABLE), wire_unknown_frame_type).

%% @doc Count one ignored or refused extension, by its name, or one enum fallback, by its field.
-spec count(wire_ext_ignored | wire_ext_refused | wire_enum_fallback, label()) -> ok.
count(Counter, Name) when (Counter =:= wire_ext_ignored orelse Counter =:= wire_ext_refused), is_binary(Name) ->
    counting(ets:whereis(?TABLE), Counter, Name);
count(wire_enum_fallback, Field) when is_atom(Field) ->
    counting(ets:whereis(?TABLE), {wire_enum_fallback, Field}).

%% @doc Every count since the table was created, keyed by counter, or by counter and label.
-spec counters() -> #{counter() | {counter(), label()} => non_neg_integer()}.
counters() ->
    counters_in(ets:whereis(?TABLE)).

%% The counts belong to the peering run: `macula_peering_sup' creates this table and its life is the run's. A reader
%% can outlive it -- the station's peer observer is in the station's own tree and still decodes frames while a station
%% shuts down -- and then there is no table to count in. A tolerance count is bookkeeping and never the decoder's
%% answer, so its absence must not fail the caller (macula#186). Read the same way `macula_realm_rights' reads its
%% own table, through `ets:whereis/1', and with nothing caught: a bad key still raises.
counting(undefined, _Key) ->
    ok;
counting(Table, Key) ->
    bump(Table, Key).

counting(undefined, _Counter, _Name) ->
    ok;
counting(Table, Counter, Name) ->
    bump(Table, {Counter, ext_label(Table, ets:member(Table, {Counter, Name}), Counter, Name)}).

counters_in(undefined) -> #{};
counters_in(Table) ->
    maps:from_list([Row || {Key, _} = Row <- ets:tab2list(Table), not is_labels_row(Key)]).

is_labels_row({ext_labels, _Counter}) -> true;
is_labels_row(_Key) -> false.

bump(Table, Key) ->
    _ = ets:update_counter(Table, Key, 1, {Key, 0}),
    ok.

%% A name already counted keeps its row; a new one takes a row while fewer than 64 names have one under its counter.
ext_label(_Table, true, _Counter, Name) -> Name;
ext_label(Table, false, Counter, Name) ->
    Labels = {ext_labels, Counter},
    new_ext_label(ets:update_counter(Table, Labels, 1, {Labels, 0}), Name).

new_ext_label(N, Name) when N =< ?MAX_EXT_LABELS -> Name;
new_ext_label(_N, _Name) -> ?OTHER.
