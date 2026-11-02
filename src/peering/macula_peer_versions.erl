%% @doc The version floor each peer is held to, and the node-wide handshake counters (docs/design/WIRE_1_0.md
%% sections 4.6 and 8).
%%
%% A node records, per peer node_id, the highest handshake version it completed with, and never completes below it.
%% The floor is raised only after a handshake completes, and only upward. It survives restarts through the connection's
%% `version_floor' option, a store `{Mod, Arg}' with `Mod:floor(Arg, NodeId) -> Version | none' and
%% `Mod:raise(Arg, NodeId, Version) -> ok | {error, term()}'. Without one, a node keeps the floor for the run, as an SDK
%% host with no writable store does. A raise the store fails to persist keeps the completed connection and the floor in
%% memory, and counts and logs version_floor_unpersisted; it never crashes the node or closes the link.
%%
%% The table belongs to macula_peering_sup, which creates it with new_table/0 and holds every connection that reads it,
%% so the memory lasts as long as those connections can, whichever child of that supervisor dies (macula#50). There is
%% no process here: every read and write goes to the table directly, so no connection queues on it.
-module(macula_peer_versions).

-export([new_table/0, floor/2, raise/3, count/1, counters/0]).

-export_type([store/0, counter/0]).

-define(TABLE, ?MODULE).
-define(COUNTERS, [v5_connections, downgrade_detected, negotiation_mismatch, version_below_floor,
                   session_proof_invalid, session_proof_missing, session_proof_rate, exporter_unavailable,
                   version_floor_unpersisted]).

%% Where a node keeps its floors across restarts.
-type store() :: {module(), term()} | none.
-type counter() :: v5_connections | downgrade_detected | negotiation_mismatch | version_below_floor
                 | session_proof_invalid | session_proof_missing | session_proof_rate | exporter_unavailable
                 | version_floor_unpersisted.

-callback floor(Arg :: term(), NodeId :: <<_:256>>) -> macula_handshake:version() | pos_integer() | none.
-callback raise(Arg :: term(), NodeId :: <<_:256>>, Version :: pos_integer()) -> ok | {error, term()}.

%% Rows: {{floor, NodeId}, Version} for each peer completed with in this run or read from the store,
%% {{counter, Name}, Count}.

%% @doc Create the table, owned by the calling process: macula_peering_sup, whose life is the run's.
-spec new_table() -> ok.
new_table() ->
    ?TABLE = ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}]),
    ok.

%% @doc The floor held for NodeId: the one this run holds, else the store's, or none.
-spec floor(<<_:256>>, store()) -> pos_integer() | none.
floor(<<_:256>> = NodeId, Store) ->
    held(ets:whereis(?TABLE), NodeId, Store).

%% The floors belong to the peering run: `macula_peering_sup' creates this table and its life is the run's, so a
%% caller can outlive it. With no table this run holds no floor, and the store's -- which is what survives a restart
%% anyway -- is the whole answer (macula#186). Read through `ets:whereis/1' as `macula_realm_rights' reads its own
%% table, with nothing caught.
held(undefined, NodeId, Store) ->
    stored(Store, NodeId);
held(Table, NodeId, Store) ->
    read(ets:lookup(Table, {floor, NodeId}), NodeId, Store).

read([{_, Version}], _NodeId, _Store) -> Version;
read([], NodeId, Store) -> stored(Store, NodeId).

stored(none, _NodeId) -> none;
stored({Mod, Arg}, NodeId) -> Mod:floor(Arg, NodeId).

%% @doc A handshake with NodeId completed at Version: the floor rises to it when it is higher, in memory and in the
%% store. A store that fails keeps the floor in memory and counts version_floor_unpersisted.
-spec raise(<<_:256>>, pos_integer(), store()) -> ok.
raise(<<_:256>> = NodeId, Version, Store) when is_integer(Version), Version > 0 ->
    risen(above(Version, floor(NodeId, Store)), NodeId, Version, Store).

above(_Version, none) -> true;
above(Version, Floor) -> Version > Floor.

risen(false, _NodeId, _Version, _Store) ->
    ok;
risen(true, NodeId, Version, Store) ->
    ok = record(ets:whereis(?TABLE), NodeId, Version),
    persisted(persist(Store, NodeId, Version), NodeId, Version).

%% A raise the run has no table for still reaches the store, which is the copy that survives a restart anyway; only
%% the in-memory floor is skipped (macula#186).
record(undefined, _NodeId, _Version) ->
    ok;
record(Table, NodeId, Version) ->
    true = ets:insert(Table, {{floor, NodeId}, Version}),
    ok.

persist(none, _NodeId, _Version) -> ok;
persist({Mod, Arg}, NodeId, Version) -> Mod:raise(Arg, NodeId, Version).

persisted(ok, _NodeId, _Version) ->
    ok;
persisted({error, Reason}, NodeId, Version) ->
    ok = count(version_floor_unpersisted),
    macula_diagnostics:event(warning, <<"_macula.peering.version_floor_unpersisted">>,
                             #{node_id => binary:encode_hex(NodeId, lowercase), version => Version,
                               reason => Reason}).

%% @doc Count one handshake event, node-wide.
-spec count(counter()) -> ok.
count(Name) ->
    true = lists:member(Name, ?COUNTERS),
    bumped(ets:whereis(?TABLE), Name).

%% A handshake counter is bookkeeping and never a connection's answer, so with no table there is nothing to count and
%% the caller carries on (macula#186).
bumped(undefined, _Name) ->
    ok;
bumped(Table, Name) ->
    _ = ets:update_counter(Table, {counter, Name}, 1, {{counter, Name}, 0}),
    ok.

%% @doc Every handshake counter, zero included.
-spec counters() -> #{counter() => non_neg_integer()}.
counters() ->
    counters_in(ets:whereis(?TABLE)).

counters_in(undefined) -> maps:from_keys(?COUNTERS, 0);
counters_in(Table) ->
    maps:from_list([{Name, counted(ets:lookup(Table, {counter, Name}))} || Name <- ?COUNTERS]).

counted([{_, Count}]) -> Count;
counted([]) -> 0.
