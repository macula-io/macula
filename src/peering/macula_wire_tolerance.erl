%% @doc What this node's frame reader tolerated from newer peers instead of closing a link (docs/design/WIRE_1_0.md
%% section 8, macula#83 decisions 5, 6 and 8), counted node-wide so an operator sees when the fleet speaks something
%% this node does not:
%%
%% <ul>
%%   <li>wire_unknown_frame_type: a frame of a type this node does not know, dropped (no label);</li>
%%   <li>wire_ext_ignored: a non-critical extension this node does not know, ignored, labelled by the extension's
%%       name;</li>
%%   <li>wire_enum_fallback: a per-hop enum value this node does not know, read as its fallback, labelled by the
%%       field's name.</li>
%% </ul>
%%
%% An extension name comes from a peer, so the labels it makes are bounded: past 64 distinct names, a new one counts
%% under the label other.
%%
%% The table belongs to macula_peering_sup, which creates it with new_table/0, as macula_peer_versions' is. There is
%% no process here: every count goes to the table directly, so no reader queues on it.
-module(macula_wire_tolerance).

-export([new_table/0, count/1, count/2, counters/0]).

-define(TABLE, ?MODULE).
-define(MAX_EXT_LABELS, 64).
-define(OTHER, <<"other">>).

-type counter() :: wire_unknown_frame_type | wire_ext_ignored | wire_enum_fallback.
-type label() :: binary() | atom().
-export_type([counter/0, label/0]).

%% Rows: {wire_unknown_frame_type, Count}, {{wire_ext_ignored, Name}, Count}, {{wire_enum_fallback, Field}, Count},
%% and {ext_labels, N}, how many extension names have a row.

%% @doc Create the table, owned by the calling process: macula_peering_sup, whose life is the run's.
-spec new_table() -> ok.
new_table() ->
    ?TABLE = ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    ok.

%% @doc Count one frame of a type this node does not know.
-spec count(wire_unknown_frame_type) -> ok.
count(wire_unknown_frame_type) ->
    bump(wire_unknown_frame_type).

%% @doc Count one ignored extension, by its name, or one enum fallback, by its field.
-spec count(wire_ext_ignored | wire_enum_fallback, label()) -> ok.
count(wire_ext_ignored, Name) when is_binary(Name) ->
    bump({wire_ext_ignored, ext_label(ets:member(?TABLE, {wire_ext_ignored, Name}), Name)});
count(wire_enum_fallback, Field) when is_atom(Field) ->
    bump({wire_enum_fallback, Field}).

%% @doc Every count since the table was created, keyed by counter, or by counter and label.
-spec counters() -> #{counter() | {counter(), label()} => non_neg_integer()}.
counters() ->
    maps:from_list([Row || {Key, _} = Row <- ets:tab2list(?TABLE), Key =/= ext_labels]).

bump(Key) ->
    _ = ets:update_counter(?TABLE, Key, 1, {Key, 0}),
    ok.

%% A name already counted keeps its row; a new one takes a row while fewer than 64 names have one.
ext_label(true, Name) -> Name;
ext_label(false, Name) -> new_ext_label(ets:update_counter(?TABLE, ext_labels, 1, {ext_labels, 0}), Name).

new_ext_label(N, Name) when N =< ?MAX_EXT_LABELS -> Name;
new_ext_label(_N, _Name) -> ?OTHER.
