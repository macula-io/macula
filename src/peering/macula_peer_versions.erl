%% @doc Which handshake version a client dials each node with, and the node-wide handshake counters
%% (docs/design/DESIGN_NEIGHBOUR_CHANNEL_BINDING.md sections 3, 4 and 6).
%%
%% A node never seen is dialled with version 5. A node that refused a v5 CONNECT with unsupported_version is dialled
%% with version 4 for the next 10 minutes, so a slow roll does not pay a failed v5 handshake on every new connection,
%% then with version 5 again. A node that completed one v5 handshake in this run is never dialled with version 4
%% again: a later unsupported_version from it is refused as a downgrade, which also refuses a station rolled back
%% below v5 until forget_v5_peer/1 is called for it or this node restarts. There is no timer on that memory, because
%% a timer is also an attacker's wait.
%%
%% Fallbacks are counted per node. The first is expected while the fleet rolls; from the second, the caller logs a
%% warning at most once a minute per node.
%%
%% The table belongs to macula_peering_sup, which creates it with new_table/0 and holds every connection that reads it,
%% so the memory lasts as long as those connections can, whichever child of that supervisor dies (macula#50). There is
%% no process here: every read and write goes to the table directly, so no connection queues on it.
-module(macula_peer_versions).

-export([new_table/0, dial_version/2, unsupported_version/2, completed_v5/1, v4_completed/2, v4_ended/2,
         refuse_downgrade/1, seen_v5/1, forget_v5_peer/1,
         fallback_warning/2, downgrade_warning/2, count/1, counters/0]).

-define(TABLE, ?MODULE).
-define(V4_CACHE_MS, 10 * 60000).
-define(WARNING_INTERVAL_MS, 60000).
-define(COUNTERS, [v4_connections, v5_connections, v4_control_frames, v4_fallbacks, v5_downgrade_refused,
                   v4_hello_to_v5_connect, session_proof_invalid, session_proof_missing, session_proof_rate,
                   exporter_unavailable]).

-type counter() :: v4_connections | v5_connections | v4_control_frames | v4_fallbacks | v5_downgrade_refused
                 | v4_hello_to_v5_connect | session_proof_invalid | session_proof_missing | session_proof_rate
                 | exporter_unavailable.
-export_type([counter/0]).

%% Rows: {{seen_v5, NodeId}}, {{v4_until, NodeId}, Ms}, {{fallbacks, NodeId}, Count}, {{warned, NodeId}, Ms},
%% {{downgrades, NodeId}, Count}, {{warned_downgrade, NodeId}, Ms}, {{v4_conn, NodeId, Pid}} for each v4 connection
%% this node dialled that is open, {{counter, Name}, Count}.

%% @doc Create the table, owned by the calling process: macula_peering_sup, whose life is the run's.
-spec new_table() -> ok.
new_table() ->
    ?TABLE = ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}]),
    ok.

%% @doc The version to put in CONNECT to NodeId at Now.
-spec dial_version(<<_:256>>, integer()) -> 4 | 5.
dial_version(NodeId, Now) ->
    version_for(seen_v5(NodeId), ets:lookup(?TABLE, {v4_until, NodeId}), Now).

%% A node seen on v5 is dialled with v5, whatever the cache holds.
version_for(true, _Cached, _Now) -> 5;
version_for(false, Cached, Now) -> version_for(Cached, Now).

version_for([{_, Until}], Now) when Now < Until -> 4;
version_for(_NoneOrExpired, _Now) -> 5.

%% @doc NodeId refused a v5 CONNECT with unsupported_version. A node seen on v5 in this run is refused as a downgrade;
%% any other falls back to version 4 for 10 minutes, with its fallback count.
-spec unsupported_version(<<_:256>>, integer()) -> downgrade_refused | {fall_back, pos_integer()}.
unsupported_version(NodeId, Now) ->
    refused_or_fallen_back(seen_v5(NodeId), NodeId, Now).

refused_or_fallen_back(true, NodeId, _Now) ->
    downgrade_refused(NodeId);
refused_or_fallen_back(false, NodeId, Now) ->
    true = ets:insert(?TABLE, {{v4_until, NodeId}, Now + ?V4_CACHE_MS}),
    ok = count(v4_fallbacks),
    {fall_back, ets:update_counter(?TABLE, {fallbacks, NodeId}, 1, {{fallbacks, NodeId}, 0})}.

%% @doc A v5 handshake with NodeId completed: it is never dialled with version 4 again in this run, and every v4
%% connection this node dialled to it that is still open is told to close as a downgrade ({v5_completed_elsewhere,
%% NodeId}). With v4_completed/2 registering before it reads, no interleaving keeps a v4 connection (macula#53).
-spec completed_v5(<<_:256>>) -> ok.
completed_v5(NodeId) ->
    true = ets:insert(?TABLE, {{seen_v5, NodeId}}),
    true = ets:delete(?TABLE, {v4_until, NodeId}),
    [gen_statem:cast(Pid, {v5_completed_elsewhere, NodeId}) || [Pid] <- ets:match(?TABLE, {{v4_conn, NodeId, '$1'}})],
    ok.

%% @doc A v4 handshake with NodeId, dialled by Pid, completed. Pid is registered first, then the memory is read: when
%% NodeId completed v5 meanwhile it is refused as a downgrade; otherwise it stays registered until v4_ended/2, so a v5
%% completion after it reaches it (completed_v5/1). Each table operation is atomic, so in every interleaving either
%% this read sees the v5 completion or that completion sees this registration (macula#53). That rests on each table
%% operation completing before the next one of the same process starts, on the table's own locking, rather than on any
%% cross-key ordering ETS documents. Only a node's own dials
%% register: a station's accepted connections are never reached.
-spec v4_completed(<<_:256>>, pid()) -> ok | downgrade_refused.
v4_completed(NodeId, Pid) ->
    true = ets:insert(?TABLE, {{v4_conn, NodeId, Pid}}),
    v4_verdict(seen_v5(NodeId), NodeId, Pid).

v4_verdict(true, NodeId, Pid) ->
    ok = v4_ended(NodeId, Pid),
    downgrade_refused(NodeId);
v4_verdict(false, _NodeId, _Pid) ->
    ok.

%% @doc The v4 connection Pid dialled to NodeId ended.
-spec v4_ended(<<_:256>>, pid()) -> ok.
v4_ended(NodeId, Pid) ->
    true = ets:delete(?TABLE, {v4_conn, NodeId, Pid}),
    ok.

%% @doc Count a refused downgrade for NodeId, as the connection that refuses it does.
-spec refuse_downgrade(<<_:256>>) -> downgrade_refused.
refuse_downgrade(NodeId) ->
    downgrade_refused(NodeId).

downgrade_refused(NodeId) ->
    ok = count(v5_downgrade_refused),
    _ = ets:update_counter(?TABLE, {downgrades, NodeId}, 1, {{downgrades, NodeId}, 0}),
    downgrade_refused.

-spec seen_v5(<<_:256>>) -> boolean().
seen_v5(NodeId) ->
    ets:member(?TABLE, {seen_v5, NodeId}).

%% @doc Forget that NodeId completed a v5 handshake, so a station deliberately rolled back below v5 is dialled again.
%% An operator action, never automatic.
-spec forget_v5_peer(<<_:256>>) -> ok.
forget_v5_peer(NodeId) ->
    true = ets:delete(?TABLE, {seen_v5, NodeId}),
    ok.

%% @doc Whether to log a warning for NodeId's fallbacks at Now: from its second fallback, at most once a minute.
-spec fallback_warning(<<_:256>>, integer()) -> no_warning | {warn, pos_integer()}.
fallback_warning(NodeId, Now) ->
    warning_due(fallbacks(NodeId), ets:lookup(?TABLE, {warned, NodeId}), NodeId, Now).

fallbacks(NodeId) ->
    row_count(ets:lookup(?TABLE, {fallbacks, NodeId})).

row_count([{_, Count}]) -> Count;
row_count([]) -> 0.

warning_due(Count, _Warned, _NodeId, _Now) when Count < 2 ->
    no_warning;
warning_due(Count, Warned, NodeId, Now) ->
    once_a_minute(Warned, {warned, NodeId}, Count, Now).

%% @doc Whether to log a warning for NodeId's refused downgrades at Now: from the first, at most once a minute, with
%% how many there have been.
-spec downgrade_warning(<<_:256>>, integer()) -> no_warning | {warn, pos_integer()}.
downgrade_warning(NodeId, Now) ->
    once_a_minute(ets:lookup(?TABLE, {warned_downgrade, NodeId}), {warned_downgrade, NodeId},
                  row_count(ets:lookup(?TABLE, {downgrades, NodeId})), Now).

once_a_minute([{_, At}], _Key, _Count, Now) when Now - At < ?WARNING_INTERVAL_MS ->
    no_warning;
once_a_minute(_NeverOrLongAgo, Key, Count, Now) ->
    true = ets:insert(?TABLE, {Key, Now}),
    {warn, Count}.

%% @doc Count one handshake event, node-wide.
-spec count(counter()) -> ok.
count(Name) when Name =:= v4_connections; Name =:= v5_connections; Name =:= v4_control_frames;
                 Name =:= v4_fallbacks; Name =:= v5_downgrade_refused; Name =:= v4_hello_to_v5_connect;
                 Name =:= session_proof_invalid; Name =:= session_proof_missing; Name =:= session_proof_rate;
                 Name =:= exporter_unavailable ->
    _ = ets:update_counter(?TABLE, {counter, Name}, 1, {{counter, Name}, 0}),
    ok.

%% @doc Every handshake counter, zero included.
-spec counters() -> #{counter() => non_neg_integer()}.
counters() ->
    maps:from_list([{Name, counted(ets:lookup(?TABLE, {counter, Name}))} || Name <- ?COUNTERS]).

counted([{_, Count}]) -> Count;
counted([]) -> 0.
