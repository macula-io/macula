%%%-------------------------------------------------------------------
%%% @doc The node's in-flight bound on received frame bytes.
%%%
%%% Every frame byte a node holds from a peer, from the moment it is read until
%%% the frame has been handled, is covered by a reservation. A reservation is
%%% made against one connection opened with `open_connection/2' and against
%%% the node, and it ends when its holder releases it or exits. Reserved bytes
%%% stay under the connection's limit, with its streams together under three
%%% quarters of it so its control stream keeps room, and under the node limit.
%%% A station link's control stream is admitted past the node limit up to a
%%% hard ceiling of 1.25 times it. At that ceiling, control bytes of at most
%%% 4 KiB, and every byte of a control frame of at most 4 KiB on the wire
%%% (`small_control'), come from a small fixed station reserve.
%%%
%%% `try_admit/4' reserves at once or answers `full', and never passes a reader
%%% already waiting for room. `admit/5' gives a reader that does not fit a
%%% place in line under a tag, and this process admits waiting readers in
%%% arrival order as room frees: it keeps each admitted reservation for its
%%% reader, tells the reader `{macula_peering_inflight, resume, Tag}', and the
%%% reader takes the reservation with `take_admitted/1'.
%%%
%%% A reservation is made, split, shrunk and released in the calling process
%%% against the tables. A charge adds to the connection's count, then the
%%% node's (or the station reserve's), then the role's, and writes the
%%% reservation row last; a refused add is taken back at once, in reverse. A
%%% release takes the reservation row first and only then lowers the counts,
%%% and a split or a shrink lowers a row before it writes another, so an
%%% interrupted update can only count too much, never too little. Only the
%%% holder releases, splits, shrinks or hands over a reservation; this process
%%% releases what an exited holder held.
%%%
%%% The tables are held by `macula_peering_inflight_keeper', so they outlive
%%% this process. When it starts again it monitors every holder, connection
%%% and waiting reader again; one that ended in between is released at once.
%%% With this process down, readers that do not fit stay paused, so an outage
%%% fails closed.
%%%
%%% A reservation older than `inflight_max_reservation_age_ms' is counted as
%%% expired, once, with an event and a warning logged at most once a minute,
%%% and it stays reserved: the accounting never counts fewer bytes than are
%%% held, and the node's policy ends such a holder.
%%%
%%% The settings are read from the macula application env when used and are
%%% checked when macula starts, see `check_limits/0'.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peering_inflight).

-behaviour(gen_server).

-export([start_link/0, new_table/0,
         open_connection/2, set_role/2, connection_settings/1,
         try_admit/4, admit/5, take_admitted/1, cancel_wait/1,
         release/1, hand_over/2, split/3, shrink/2,
         usage/0, received_at_us/1,
         check_limits/0, node_bytes/0, connection_bytes/0,
         decode_transient_bytes/1, settled_bytes/2, frame_cap/2, streams_to_reset/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export_type([reservation/0, kind/0, role/0, usage/0]).

-define(SERVER, ?MODULE).

%% Reservations and counts. Rows:
%% `{{conn, Conn}, Limit, Role, PauseLimitMs, Watermark}' for each opened
%% connection, `{{conn_bytes, Conn, control | stream}, Bytes}',
%% `{node_bytes, Bytes}', `{reserve_bytes, Bytes}', `{{role_bytes, Role},
%% Bytes}', `{waiting, N}', `{ceiling_pauses, N}', `{reserve_admits, N}',
%% `{expired, N}', `{{holder, Pid}, watched}', `{{gone, Conn}, true}',
%% `{{expired, Seq}, true}', `{{admitted, Pid, Tag}, Reservation}' for an
%% admission its reader has not taken yet, and `{{reservation, Seq}, Conn,
%% CountKind, Bytes, Role, Source, Holder, FrameType, ReservedAtMs}'.
-define(TABLE, macula_peering_inflight).
%% `{{ReservedAtMs, Seq}, FrameType}', oldest first.
-define(AGES, macula_peering_inflight_ages).
%% `{Seq, Pid, Conn, Kind, Bytes, FrameType, Tag}', first come first.
-define(WAITERS, macula_peering_inflight_waiters).

-define(KIB, 1024).
-define(MIB, (1 bsl 20)).
-define(DEFAULT_NODE_BYTES, 64 * ?MIB).
-define(DEFAULT_CONNECTION_BYTES, 64 * ?MIB).
-define(DEFAULT_STATION_RESERVE_BYTES, 4 * ?MIB).
-define(DEFAULT_MAX_RESERVATION_AGE_MS, 200_000).
-define(DEFAULT_PAUSE_LIMIT_MS, 20_000).
-define(DEFAULT_RESUME_WATERMARK, 75).
-define(MIN_LIMIT_BYTES, ?MIB).
-define(MIN_STATION_RESERVE_BYTES, 4 * ?KIB).
-define(SMALL_CONTROL_FRAME_BYTES, 4 * ?KIB).
%% The frame cap in macula_frame, on a frame's length header, and the size of
%% that header.
-define(MAX_FRAME_BYTES, 16#FFFFFF).
-define(FRAME_HEADER_BYTES, 4).
%% The CBOR element budget of a frame, macula_cbor_nif:element_budget/0.
-define(ELEMENT_BUDGET, 131072).
%% Measured with the element budget: decoding holds at most this much per
%% CBOR item until the decode ends, and a decoded frame keeps at most this
%% much per item besides its wire bytes.
-define(TRANSIENT_BYTES_PER_ITEM, 240).
-define(KEPT_BYTES_PER_ITEM, 40).
%% A reader waits at most this long for this process to take its place in
%% line before it stays paused.
-define(WAIT_TIMEOUT_MS, 1000).
-define(EXPIRY_LOG_INTERVAL_MS, 60_000).

-type kind() :: control | small_control | stream.
-type role() :: client | station.
-opaque reservation() :: {inflight_reservation, integer(), integer()}.
-type usage() :: #{node_limit := pos_integer(),
                   reserved := non_neg_integer(),
                   reserved_by_role := #{client := non_neg_integer(), station := non_neg_integer()},
                   waiting := non_neg_integer(),
                   ceiling_pauses := non_neg_integer(),
                   reserve_admits := non_neg_integer(),
                   expired := non_neg_integer(),
                   oldest := none | #{age_ms := non_neg_integer(), frame_type := atom()}}.

-record(state, {
    %% Expiries not logged yet, and when expiries were last logged.
    unlogged  = 0         :: non_neg_integer(),
    logged_at = undefined :: undefined | integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create the tables the reservations are kept in, owned by the calling
%% process. `macula_peering_inflight_keeper' creates them, so they outlive
%% this process.
-spec new_table() -> ets:table().
new_table() ->
    _ = ets:new(?AGES, [ordered_set, public, named_table]),
    _ = ets:new(?WAITERS, [ordered_set, public, named_table]),
    ets:new(?TABLE, [set, public, named_table]).

%% @doc Open `Conn' for in-flight counting with a byte limit, a role, a pause
%% limit and a resume watermark (percent of the stream share). A connection
%% whose stream share would not stay above the per-caller session budget
%% (`macula_stream_sessions:max_inbox_bytes_per_caller/0') is refused, so a
%% slow session ends with resource_exhausted before its link pauses.
-spec open_connection(pid(), #{connection_bytes => pos_integer(), role => role(),
                               pause_limit_ms => pos_integer(),
                               resume_watermark => 1..100}) ->
    ok | {error, {bad_config, map()}}.
open_connection(Conn, Opts) when is_pid(Conn), is_map(Opts) ->
    Limit = maps:get(connection_bytes, Opts, connection_bytes()),
    opened(session_budget_below(stream_share(Limit)), Conn, Limit, Opts).

opened({true, _Budget}, Conn, Limit, Opts) ->
    true = ets:insert(?TABLE, {{conn, Conn}, Limit, maps:get(role, Opts, client),
                               maps:get(pause_limit_ms, Opts, ?DEFAULT_PAUSE_LIMIT_MS),
                               maps:get(resume_watermark, Opts, ?DEFAULT_RESUME_WATERMARK)}),
    true = ets:delete(?TABLE, {gone, Conn}),
    watch(Conn),
    ok;
opened({false, Budget}, _Conn, Limit, _Opts) ->
    {error, {bad_config, #{max_served_inbox_bytes_per_caller => Budget, connection_bytes => Limit}}}.

%% @doc Name `Conn' a station link or a client link from now on. The station
%% role comes only from the node's own configuration or a verified identity.
-spec set_role(pid(), role()) -> ok.
set_role(Conn, Role) when Role =:= client; Role =:= station ->
    _ = ets:update_element(?TABLE, {conn, Conn}, {3, Role}),
    room(),
    ok.

%% @doc The settings `Conn' was opened with, and its role now.
-spec connection_settings(pid()) -> #{connection_bytes := pos_integer(), role := role(),
                                      pause_limit_ms := pos_integer(), resume_watermark := 1..100}.
connection_settings(Conn) ->
    [{_Key, Limit, Role, PauseLimitMs, Watermark}] = ets:lookup(?TABLE, {conn, Conn}),
    #{connection_bytes => Limit, role => Role, pause_limit_ms => PauseLimitMs,
      resume_watermark => Watermark}.

%% @doc Reserve `Bytes' of kind `Kind' on `Conn' for the calling process, or
%% answer `full' when the connection or the node has no room, or when a reader
%% is already waiting for room.
-spec try_admit(pid(), kind(), non_neg_integer(), atom()) ->
    {ok, reservation()} | full | {error, not_open}.
try_admit(Conn, Kind, Bytes, FrameType)
  when is_pid(Conn), (Kind =:= control orelse Kind =:= small_control orelse Kind =:= stream),
       is_integer(Bytes), Bytes >= 0 ->
    before_waiters(count(waiting) =:= 0, {Conn, Kind, Bytes, FrameType}).

before_waiters(true, {Conn, Kind, Bytes, FrameType}) ->
    charge(ets:lookup(?TABLE, {conn, Conn}), Kind, Bytes, FrameType, {self(), counted});
before_waiters(false, _Request) ->
    full.

%% @doc Reserve as `try_admit/4' does, or take a place in line under `Tag':
%% `queued'. Once there is room, in arrival order, this process makes the
%% reservation for the calling process, keeps it under `Tag', and tells the
%% calling process `{macula_peering_inflight, resume, Tag}'; `take_admitted/1'
%% takes it. When this process does not answer in time, the reader has no
%% place in line and stays paused: `{error, unavailable}'.
-spec admit(pid(), kind(), non_neg_integer(), atom(), term()) ->
    {ok, reservation()} | queued | {error, not_open | unavailable}.
admit(Conn, Kind, Bytes, FrameType, Tag) ->
    in_line(try_admit(Conn, Kind, Bytes, FrameType), {Conn, Kind, Bytes, FrameType, Tag}).

in_line(full, {Conn, Kind, Bytes, FrameType, Tag}) ->
    try gen_server:call(?SERVER, {wait, self(), Conn, Kind, Bytes, FrameType, Tag}, ?WAIT_TIMEOUT_MS)
    catch exit:{_Why, {gen_server, call, _Args}} -> {error, unavailable}
    end;
in_line(Answer, _Request) ->
    Answer.

%% @doc Take the reservation admitted for the calling process under `Tag', or
%% `none' when there is none.
-spec take_admitted(term()) -> {ok, reservation()} | none.
take_admitted(Tag) ->
    taken_admission(ets:take(?TABLE, {admitted, self(), Tag})).

taken_admission([{_Key, Reservation}]) -> {ok, Reservation};
taken_admission([]) -> none.

%% @doc Give up the calling process's place in line under `Tag', and release a
%% reservation already admitted under it and not taken.
-spec cancel_wait(term()) -> ok.
cancel_wait(Tag) ->
    gen_server:cast(?SERVER, {cancel, self(), Tag}).

%% @doc Release reservations the calling process holds. A reservation another
%% process holds, or one already released, is left as it is.
-spec release(reservation() | [reservation()]) -> ok.
release(Reservations) when is_list(Reservations) ->
    lists:foreach(fun release/1, Reservations);
release({inflight_reservation, Seq, _At}) ->
    released(ets:lookup(?TABLE, {reservation, Seq}), self()).

%% @doc Make `To' the holder of reservations the calling process holds.
-spec hand_over(reservation() | [reservation()], pid()) -> ok.
hand_over(Reservations, To) when is_list(Reservations) ->
    lists:foreach(fun(R) -> hand_over(R, To) end, Reservations);
hand_over({inflight_reservation, Seq, _At}, To) when is_pid(To) ->
    handed(ets:lookup(?TABLE, {reservation, Seq}), self(), To).

%% @doc Split `Bytes' off a reservation the calling process holds, as a new
%% reservation for `FrameType' of the same connection, kind and source, and
%% return it with the rest. The counts do not change.
-spec split(reservation(), non_neg_integer(), atom()) -> {reservation(), reservation()}.
split({inflight_reservation, Seq, _At} = Rest, Bytes, FrameType) when is_integer(Bytes), Bytes >= 0 ->
    split_row(ets:lookup(?TABLE, {reservation, Seq}), Rest, Bytes, FrameType, self()).

%% @doc Lower a reservation the calling process holds to `Bytes', giving the
%% rest back to the counts.
-spec shrink(reservation(), non_neg_integer()) -> reservation().
shrink({inflight_reservation, Seq, _At} = Reservation, Bytes) when is_integer(Bytes), Bytes >= 0 ->
    ok = shrunk(ets:lookup(?TABLE, {reservation, Seq}), Bytes, self()),
    Reservation.

%% @doc What the node holds reserved against what it may hold, read from the
%% tables without asking this process.
-spec usage() -> usage().
usage() ->
    #{node_limit       => node_bytes(),
      reserved         => count(node_bytes) + count(reserve_bytes),
      reserved_by_role => #{client => count({role_bytes, client}),
                            station => count({role_bytes, station})},
      waiting          => count(waiting),
      ceiling_pauses   => count(ceiling_pauses),
      reserve_admits   => count(reserve_admits),
      expired          => count(expired),
      oldest           => oldest(ets:first(?AGES))}.

%% @doc When the frame a reservation covers finished decoding, in
%% `erlang:monotonic_time(microsecond)'; for a list, its first reservation.
-spec received_at_us(reservation() | [reservation()]) -> integer().
received_at_us([Reservation | _]) ->
    received_at_us(Reservation);
received_at_us({inflight_reservation, _Seq, At}) ->
    At.

%% @doc Check every in-flight setting and its pairing with the per-caller
%% session budget, raising `{bad_config, {macula, Setting, Value}}' for a
%% setting out of its range, and `{bad_config, #{...}}' naming both settings
%% when the session budget is not below the default connection's stream share.
-spec check_limits() -> ok.
check_limits() ->
    _ = node_bytes(),
    Connection = connection_bytes(),
    _ = station_reserve_bytes(),
    _ = max_reservation_age_ms(),
    paired(session_budget_below(stream_share(Connection)), Connection).

paired({true, _Budget}, _Connection) ->
    ok;
paired({false, Budget}, Connection) ->
    erlang:error({bad_config, #{max_served_inbox_bytes_per_caller => Budget,
                                inflight_connection_bytes => Connection}}).

%% @doc The node limit: `inflight_node_bytes', at least 1 MiB, 64 MiB when unset.
-spec node_bytes() -> pos_integer().
node_bytes() ->
    setting(inflight_node_bytes, ?DEFAULT_NODE_BYTES, ?MIN_LIMIT_BYTES).

%% @doc The default connection limit: `inflight_connection_bytes', at least
%% 1 MiB, 64 MiB when unset.
-spec connection_bytes() -> pos_integer().
connection_bytes() ->
    setting(inflight_connection_bytes, ?DEFAULT_CONNECTION_BYTES, ?MIN_LIMIT_BYTES).

%% @doc The most a frame of `WireBytes' holds while it decodes, besides its
%% wire bytes: one transient allowance per CBOR item it can have.
-spec decode_transient_bytes(non_neg_integer()) -> non_neg_integer().
decode_transient_bytes(WireBytes) ->
    min(WireBytes, ?ELEMENT_BUDGET) * ?TRANSIENT_BYTES_PER_ITEM.

%% @doc What a decoded frame of `WireBytes' with `Items' CBOR items keeps.
-spec settled_bytes(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
settled_bytes(WireBytes, Items) ->
    WireBytes + Items * ?KEPT_BYTES_PER_ITEM.

%% @doc The largest frame, by its length header, that a stream (`stream') or
%% the control stream (`control') of `Conn' reads: the largest whose wire
%% bytes, the 4-byte header included, and decode transient fit in the stream
%% share, or in the connection limit, and never more than the frame cap.
-spec frame_cap(pid(), control | stream) -> pos_integer().
frame_cap(Conn, Kind) ->
    #{connection_bytes := Limit} = connection_settings(Conn),
    min(?MAX_FRAME_BYTES, largest_wire(room_for(Kind, Limit)) - ?FRAME_HEADER_BYTES).

room_for(control, Limit) -> Limit;
room_for(stream, Limit) -> stream_share(Limit).

%% The most wire bytes that fit in Room with their decode transient. Up to the
%% element budget a frame's transient grows with its size, past it the
%% transient is fixed.
largest_wire(Room) when Room >= ?ELEMENT_BUDGET * (?TRANSIENT_BYTES_PER_ITEM + 1) ->
    Room - ?ELEMENT_BUDGET * ?TRANSIENT_BYTES_PER_ITEM;
largest_wire(Room) ->
    Room div (?TRANSIENT_BYTES_PER_ITEM + 1).

%% @doc Which paused streams to reset while a connection's stream share stays
%% full past its pause limit: those whose reservations did not shrink since
%% the pause began, largest first, until the share used falls below the
%% watermark; when every stream made progress, only the largest. None while
%% the share used is below the watermark.
-spec streams_to_reset([#{stream := term(), reserved := non_neg_integer(),
                          reserved_at_pause := non_neg_integer()}],
                       #{share_used := non_neg_integer(), watermark := non_neg_integer()}) ->
    [term()].
streams_to_reset(Paused, #{share_used := Used, watermark := Watermark}) ->
    Largest = lists:sort(fun(A, B) -> maps:get(reserved, A) >= maps:get(reserved, B) end, Paused),
    reset_from([P || P <- Largest, maps:get(reserved, P) >= maps:get(reserved_at_pause, P)],
               Largest, Used, Watermark).

reset_from(_NoProgress, _Largest, Used, Watermark) when Used < Watermark ->
    [];
reset_from([], [Largest | _], _Used, _Watermark) ->
    [maps:get(stream, Largest)];
reset_from([], [], _Used, _Watermark) ->
    [];
reset_from(NoProgress, _Largest, Used, Watermark) ->
    until_below(NoProgress, Used, Watermark).

until_below([P | Rest], Used, Watermark) when Used >= Watermark ->
    [maps:get(stream, P) | until_below(Rest, Used - maps:get(reserved, P), Watermark)];
until_below(_Paused, _Used, _Watermark) ->
    [].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = watch_again(),
    _ = erlang:send_after(scan_interval(), self(), scan),
    self() ! room,
    {ok, #state{}}.

handle_call({wait, Pid, Conn, Kind, Bytes, FrameType, Tag}, From, S) ->
    Seq = erlang:unique_integer([monotonic, positive]),
    true = ets:insert(?WAITERS, {Seq, Pid, Conn, Kind, Bytes, FrameType, Tag}),
    ok = add(waiting, 1),
    _ = erlang:monitor(process, Pid),
    gen_server:reply(From, queued),
    ok = admit_waiters(),
    {noreply, S}.

handle_cast({watch, Pid}, S) ->
    _ = erlang:monitor(process, Pid),
    {noreply, S};
handle_cast(room, S) ->
    ok = admit_waiters(),
    {noreply, S};
handle_cast({cancel, Pid, Tag}, S) ->
    ok = left_line(Pid, Tag),
    ok = admit_waiters(),
    {noreply, S};
handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({'DOWN', _Mon, process, Pid, _Reason}, S) ->
    ok = release_held_by(Pid),
    ok = drop_waiter(Pid),
    ok = connection_down(ets:lookup(?TABLE, {conn, Pid}), Pid),
    ok = admit_waiters(),
    {noreply, S};
handle_info(room, S) ->
    ok = admit_waiters(),
    {noreply, S};
handle_info(scan, S) ->
    S1 = expire(ets:first(?AGES), erlang:monotonic_time(millisecond) - max_reservation_age_ms(), S),
    ok = clear_leftovers(),
    _ = erlang:send_after(scan_interval(), self(), scan),
    {noreply, S1};
handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

%%%===================================================================
%%% Internal: charging
%%%===================================================================

%% Pause is `counted' for a reader's own attempt, so a station control stream
%% refused at the ceiling is counted once, and `quiet' for this process
%% admitting a waiting reader.
charge([], _Kind, _Bytes, _FrameType, _Holder) ->
    {error, not_open};
charge([{{conn, Conn}, Limit, Role, _PauseLimitMs, _Watermark}], Kind, Bytes, FrameType, Holder) ->
    CountKind = count_kind(Kind),
    in_connection(add_within({conn_bytes, Conn, CountKind}, Bytes, kind_max(CountKind, Limit)),
                  {Conn, Limit, Role, Kind, Bytes, FrameType}, Holder).

count_kind(stream) -> stream;
count_kind(_Control) -> control.

kind_max(control, Limit) -> Limit;
kind_max(stream, Limit) -> stream_share(Limit).

in_connection(added, {Conn, Limit, _Role, _Kind, _Bytes, _FrameType} = Charge, Holder) ->
    under_limit(connection_total(Conn) =< Limit, Charge, Holder);
in_connection(refused, _Charge, _Holder) ->
    full.

under_limit(true, {_Conn, _Limit, Role, Kind, Bytes, _FrameType} = Charge, Holder) ->
    in_node(add_within(node_bytes, Bytes, node_max(Role, count_kind(Kind))), Charge, Holder);
under_limit(false, {Conn, _Limit, _Role, Kind, Bytes, _FrameType}, _Holder) ->
    ok = add({conn_bytes, Conn, count_kind(Kind)}, -Bytes),
    full.

node_max(station, control) -> ceiling();
node_max(_Role, _CountKind) -> node_bytes().

in_node(added, Charge, Holder) ->
    reserved(node, Charge, Holder);
in_node(refused, {_Conn, _Limit, station, Kind, Bytes, _FrameType} = Charge, Holder) ->
    reserve_if(from_reserve_allowed(Kind, Bytes), Charge, Holder);
in_node(refused, Charge, Holder) ->
    no_room(Charge, Holder).

from_reserve_allowed(small_control, _Bytes) -> true;
from_reserve_allowed(control, Bytes) -> Bytes =< ?SMALL_CONTROL_FRAME_BYTES;
from_reserve_allowed(stream, _Bytes) -> false.

reserve_if(true, {_Conn, _Limit, _Role, _Kind, Bytes, _FrameType} = Charge, Holder) ->
    from_reserve(add_within(reserve_bytes, Bytes, station_reserve_bytes()), Charge, Holder);
reserve_if(false, Charge, Holder) ->
    no_room(Charge, Holder).

from_reserve(added, Charge, Holder) ->
    ok = add(reserve_admits, 1),
    reserved(reserve, Charge, Holder);
from_reserve(refused, Charge, Holder) ->
    no_room(Charge, Holder).

no_room({Conn, _Limit, Role, Kind, Bytes, FrameType}, {_Pid, Pause}) ->
    ok = add({conn_bytes, Conn, count_kind(Kind)}, -Bytes),
    ok = ceiling_pause(Pause, Role, count_kind(Kind), FrameType),
    full.

ceiling_pause(counted, station, control, FrameType) ->
    ok = add(ceiling_pauses, 1),
    macula_diagnostics:event(<<"_macula.peering.inflight_ceiling_pause">>, #{frame_type => FrameType});
ceiling_pause(_Pause, _Role, _CountKind, _FrameType) ->
    ok.

reserved(Source, {Conn, _Limit, Role, Kind, Bytes, FrameType}, {Pid, _Pause}) ->
    ok = add({role_bytes, Role}, Bytes),
    {ok, new_reservation({Conn, count_kind(Kind), Bytes, Role, Source, Pid}, FrameType)}.

new_reservation({Conn, CountKind, Bytes, Role, Source, Pid}, FrameType) ->
    Seq = erlang:unique_integer([monotonic, positive]),
    At = erlang:monotonic_time(millisecond),
    true = ets:insert(?AGES, {{At, Seq}, FrameType}),
    true = ets:insert(?TABLE, {{reservation, Seq}, Conn, CountKind, Bytes, Role, Source, Pid, FrameType, At}),
    watch(Pid),
    {inflight_reservation, Seq, erlang:monotonic_time(microsecond)}.

connection_total(Conn) ->
    count({conn_bytes, Conn, control}) + count({conn_bytes, Conn, stream}).

stream_share(Limit) ->
    Limit * 3 div 4.

ceiling() ->
    node_bytes() * 5 div 4.

%% Adds Bytes to the count at Key, and takes them back at once when that took
%% it past Max: for that moment the count is over by only the refused bytes.
add_within(Key, Bytes, Max) ->
    within(ets:update_counter(?TABLE, Key, {2, Bytes}, {Key, 0}) =< Max, Key, Bytes).

within(true, _Key, _Bytes) ->
    added;
within(false, Key, Bytes) ->
    ok = add(Key, -Bytes),
    refused.

add(Key, Bytes) ->
    _ = ets:update_counter(?TABLE, Key, {2, Bytes}, {Key, 0}),
    ok.

count(Key) ->
    ets:lookup_element(?TABLE, Key, 2, 0).

%%%===================================================================
%%% Internal: releasing, splitting and handing over
%%%===================================================================

released([{Key, _Conn, _Kind, _Bytes, _Role, _Source, Holder, _FrameType, _At} = Row], Holder) ->
    taken(ets:take(?TABLE, Key), Row);
released(_NotHeldHere, _Pid) ->
    ok.

%% The row is taken first, so a second release, or this process releasing
%% for an exited holder at the same moment, gives nothing back twice.
taken([_Row], {{reservation, Seq}, Conn, CountKind, Bytes, Role, Source, _Holder, _FrameType, At}) ->
    true = ets:delete(?AGES, {At, Seq}),
    true = ets:delete(?TABLE, {expired, Seq}),
    ok = give_back(Conn, CountKind, Bytes, Role, Source),
    room();
taken([], _Row) ->
    ok.

give_back(Conn, CountKind, Bytes, Role, Source) ->
    ok = add({role_bytes, Role}, -Bytes),
    ok = add(source_key(Source), -Bytes),
    add({conn_bytes, Conn, CountKind}, -Bytes).

source_key(node) -> node_bytes;
source_key(reserve) -> reserve_bytes.

%% The held row is lowered before the split-off row is written, so a holder
%% that exits in between leaves bytes counted, never uncounted.
split_row([{Key, Conn, CountKind, Held, Role, Source, Holder, _Type, _At}], Rest, Bytes, FrameType, Holder)
  when Bytes =< Held ->
    true = ets:update_element(?TABLE, Key, {4, Held - Bytes}),
    {new_reservation({Conn, CountKind, Bytes, Role, Source, Holder}, FrameType), Rest};
split_row(_NotHeldHere, Rest, _Bytes, _FrameType, _Pid) ->
    erlang:error({not_held_or_too_small, Rest}).

%% The row is lowered before the counts, so an exit in between counts too much.
shrunk([{Key, Conn, CountKind, Held, Role, Source, Holder, _FrameType, _At}], Bytes, Holder)
  when Bytes =< Held ->
    true = ets:update_element(?TABLE, Key, {4, Bytes}),
    ok = give_back(Conn, CountKind, Held - Bytes, Role, Source),
    room();
shrunk(_NotHeldHere, _Bytes, _Pid) ->
    ok.

handed([{Key, _Conn, _Kind, _Bytes, _Role, _Source, Holder, _FrameType, _At}], Holder, To) ->
    _ = ets:update_element(?TABLE, Key, {7, To}),
    watch(To),
    ok;
handed(_NotHeldHere, _Pid, _To) ->
    ok.

%% This process is told only when a reader waits for the room a release frees.
room() ->
    tell_room(count(waiting) > 0).

tell_room(true) -> gen_server:cast(?SERVER, room);
tell_room(false) -> ok.

%% A process holding or waiting for a reservation is monitored once.
watch(Pid) ->
    tell_watch(ets:insert_new(?TABLE, {{holder, Pid}, watched}), Pid).

tell_watch(true, Pid) -> gen_server:cast(?SERVER, {watch, Pid});
tell_watch(false, _Pid) -> ok.

%%%===================================================================
%%% Internal: this process
%%%===================================================================

%% After a restart: monitors every holder, connection and waiting reader
%% again. One that ended while no process watched is released through its
%% monitor at once.
watch_again() ->
    Holders = ets:select(?TABLE, [{{{holder, '$1'}, '_'}, [], ['$1']},
                                  {{{conn, '$1'}, '_', '_', '_', '_'}, [], ['$1']}]),
    Waiters = ets:select(?WAITERS, [{{'_', '$1', '_', '_', '_', '_', '_'}, [], ['$1']}]),
    _ = [erlang:monitor(process, Pid) || Pid <- lists:usort(Holders ++ Waiters)],
    true = ets:insert(?TABLE, {waiting, ets:info(?WAITERS, size)}),
    ok.

%% An admission its reader did not take goes with the reservation it names.
release_held_by(Pid) ->
    Rows = ets:select(?TABLE, [{{{reservation, '_'}, '_', '_', '_', '_', '_', Pid, '_', '_'}, [], ['$_']}]),
    lists:foreach(fun({Key, _, _, _, _, _, _, _, _} = Row) -> taken(ets:take(?TABLE, Key), Row) end, Rows),
    true = ets:match_delete(?TABLE, {{admitted, Pid, '_'}, '_'}),
    true = ets:delete(?TABLE, {holder, Pid}),
    ok.

drop_waiter(Pid) ->
    Dropped = ets:select_delete(?WAITERS, [{{'_', Pid, '_', '_', '_', '_', '_'}, [], [true]}]),
    add(waiting, -Dropped).

%% A reader gives up its place in line under Tag, and the reservation admitted
%% under it and not taken is released.
left_line(Pid, Tag) ->
    Dropped = ets:select_delete(?WAITERS, [{{'_', Pid, '_', '_', '_', '_', '$1'},
                                            [{'=:=', '$1', {const, Tag}}], [true]}]),
    ok = add(waiting, -Dropped),
    admission_released(ets:take(?TABLE, {admitted, Pid, Tag})).

admission_released([{_Key, {inflight_reservation, Seq, _At}}]) ->
    row_released(ets:lookup(?TABLE, {reservation, Seq}));
admission_released([]) ->
    ok.

row_released([{Key, _, _, _, _, _, _, _, _} = Row]) ->
    taken(ets:take(?TABLE, Key), Row);
row_released([]) ->
    ok.

connection_down([], _Pid) ->
    ok;
connection_down([_Row], Conn) ->
    true = ets:insert(?TABLE, {{gone, Conn}, true}),
    ok.

%% Admits waiting readers in arrival order while the first one fits.
admit_waiters() ->
    admit_first(ets:first(?WAITERS)).

admit_first('$end_of_table') ->
    ok;
admit_first(Seq) ->
    [{Seq, Pid, Conn, Kind, Bytes, FrameType, Tag}] = ets:lookup(?WAITERS, Seq),
    first_admitted(charge(ets:lookup(?TABLE, {conn, Conn}), Kind, Bytes, FrameType, {Pid, quiet}),
                   Seq, Pid, Tag).

%% The reservation is kept for its reader before the reader is told.
first_admitted({ok, Reservation}, Seq, Pid, Tag) ->
    true = ets:delete(?WAITERS, Seq),
    ok = add(waiting, -1),
    true = ets:insert(?TABLE, {{admitted, Pid, Tag}, Reservation}),
    Pid ! {macula_peering_inflight, resume, Tag},
    admit_waiters();
first_admitted({error, not_open}, Seq, _Pid, _Tag) ->
    true = ets:delete(?WAITERS, Seq),
    ok = add(waiting, -1),
    admit_waiters();
first_admitted(full, _Seq, _Pid, _Tag) ->
    ok.

%% Counts each reservation past the maximum age once, oldest first, and keeps
%% it reserved.
expire('$end_of_table', _Before, S) ->
    report_expiries(S);
expire({At, _Seq}, Before, S) when At >= Before ->
    report_expiries(S);
expire({_At, Seq} = Key, Before, S) ->
    expire(ets:next(?AGES, Key), Before, expired_once(ets:insert_new(?TABLE, {{expired, Seq}, true}), Seq, S)).

expired_once(true, Seq, #state{unlogged = Unlogged} = S) ->
    ok = add(expired, 1),
    ok = expiry_event(ets:lookup(?TABLE, {reservation, Seq})),
    S#state{unlogged = Unlogged + 1};
expired_once(false, _Seq, S) ->
    S.

expiry_event([{_Key, _Conn, _Kind, Bytes, _Role, _Source, Holder, FrameType, _At}]) ->
    macula_diagnostics:event(warning, <<"_macula.peering.inflight_reservation_expired">>,
                             #{frame_type => FrameType, holder => list_to_binary(pid_to_list(Holder)),
                               bytes => Bytes});
expiry_event([]) ->
    ok.

report_expiries(#state{unlogged = 0} = S) ->
    S;
report_expiries(#state{logged_at = At} = S) ->
    logged_if(At =:= undefined orelse erlang:monotonic_time(millisecond) - At >= ?EXPIRY_LOG_INTERVAL_MS, S).

logged_if(true, #state{unlogged = Unlogged} = S) ->
    logger:warning("[macula_peering_inflight] reservations past the maximum age since the last report: ~p",
                   [Unlogged]),
    S#state{unlogged = 0, logged_at = erlang:monotonic_time(millisecond)};
logged_if(false, S) ->
    S.

%% A gone connection that holds nothing any more leaves no rows, and a node
%% that holds no reservation keeps no count that an interrupted update left.
clear_leftovers() ->
    Gone = ets:select(?TABLE, [{{{gone, '$1'}, '_'}, [], ['$1']}]),
    lists:foreach(fun clear_gone/1, Gone),
    cleared_when_empty(ets:select_count(?TABLE, [{{{reservation, '_'}, '_', '_', '_', '_', '_', '_', '_', '_'}, [], [true]}])).

clear_gone(Conn) ->
    empty_connection(connection_total(Conn), Conn).

empty_connection(0, Conn) ->
    true = ets:delete(?TABLE, {conn, Conn}),
    true = ets:delete(?TABLE, {conn_bytes, Conn, control}),
    true = ets:delete(?TABLE, {conn_bytes, Conn, stream}),
    true = ets:delete(?TABLE, {gone, Conn}),
    ok;
empty_connection(_Bytes, _Conn) ->
    ok.

cleared_when_empty(0) ->
    true = ets:insert(?TABLE, [{node_bytes, 0}, {reserve_bytes, 0},
                               {{role_bytes, client}, 0}, {{role_bytes, station}, 0}]),
    ok;
cleared_when_empty(_Reservations) ->
    ok.

scan_interval() ->
    max(50, min(1000, max_reservation_age_ms() div 4)).

oldest('$end_of_table') ->
    none;
oldest({At, _Seq} = Key) ->
    aged(ets:lookup(?AGES, Key), At).

aged([{_Key, FrameType}], At) ->
    #{age_ms => max(0, erlang:monotonic_time(millisecond) - At), frame_type => FrameType};
aged([], _At) ->
    oldest(ets:first(?AGES)).

%%%===================================================================
%%% Internal: settings
%%%===================================================================

station_reserve_bytes() ->
    setting(inflight_station_reserve_bytes, ?DEFAULT_STATION_RESERVE_BYTES, ?MIN_STATION_RESERVE_BYTES).

max_reservation_age_ms() ->
    setting(inflight_max_reservation_age_ms, ?DEFAULT_MAX_RESERVATION_AGE_MS, 1).

setting(Key, Default, Min) ->
    in_range(application:get_env(macula, Key), Key, Default, Min).

in_range(undefined, _Key, Default, _Min) ->
    Default;
in_range({ok, Value}, _Key, _Default, Min) when is_integer(Value), Value >= Min ->
    Value;
in_range({ok, Value}, Key, _Default, _Min) ->
    erlang:error({bad_config, {macula, Key, Value}}).

%% Whether the per-caller session budget stays below Share, and the budget.
session_budget_below(Share) ->
    Budget = macula_stream_sessions:max_inbox_bytes_per_caller(),
    {Budget < Share, Budget}.
