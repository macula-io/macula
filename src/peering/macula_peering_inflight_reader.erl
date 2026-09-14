%% @doc Reading a stream with the bytes it holds reserved.
%%
%% A reader reserves a stream's bytes as they arrive, and a frame's decode
%% transient just before the frame decodes. After decode the frame keeps a
%% reservation of its wire bytes and of what its decoded terms hold, which
%% travels with it as `{frame, Frame, Reservations}' until its handler releases
%% it. A frame whose fields are refused comes out as
%% `{invalid_frame, Type, Field}', its bytes already released.
%%
%% When a reservation does not fit, the reader makes its stream passive, keeps
%% the chunks already on their way unreserved, and waits in line for room.
%% `{macula_peering_inflight, resume, Stream}' then tells the process that runs
%% the reader to call `resume/1'. While a frame is incomplete and the reader
%% does not wait, each window of reading time has to bring the floor, or the
%% rest of the frame when that is less, or the stream is malformed as
%% body_stalled; time spent waiting does not count. A length header above the
%% frame cap is malformed as frame_too_large from the header alone, and a frame
%% that does not decode as bad_frame or too_many_elements. Frames read before a
%% malformed one still come out first, and the malformed answer follows
%% through `timeout/2'.
%%
%% The reader runs in the process that owns its stream. Its timers arrive there
%% as `{macula_peering_inflight, reader_timer, Stream, Timer}', for `timeout/2'.
%%
%% A control stream reader reads with its connection's control frame cap, and
%% a frame of at most 4 KiB decodes as small_control, so a station link reads
%% it at the ceiling from the station reserve. When a wait passes the pause
%% limit, a client link's reader answers `{busy, Reader}' and its connection
%% closes with REFUSED_BUSY. A station link's reader keeps waiting, and asks
%% again when it could not take a place in line.
%% @end
-module(macula_peering_inflight_reader).

-export([new/3, data/2, resume/1, timeout/2, close/1]).

-export_type([reader/0, item/0, answer/0, malformed/0]).

%% The largest frame a station link's control stream decodes from the station
%% reserve at the ceiling.
-define(SMALL_FRAME_BYTES, 4 * 1024).
-define(DEFAULT_BODY_WINDOW_MS, 10_000).
-define(DEFAULT_BODY_FLOOR_BYTES, 64 * 1024).

-type malformed() :: frame_too_large | body_stalled | bad_frame | too_many_elements.
-type item() :: {frame, macula_frame:frame(), [macula_peering_inflight:reservation()]}
              | {invalid_frame, macula_frame:frame_type() | unknown, atom()}.
-type answer() :: {[item()], reader()}
                | {malformed, malformed(), reader()}
                | {busy, reader()}.
%% A running timer, and the reference its message carries.
-type timer() :: {reference(), reference()}.

-record(reader, {
    stream         :: reference(),
    conn           :: pid(),
    cap            :: pos_integer(),
    window_ms      :: pos_integer(),
    floor_bytes    :: pos_integer(),
    %% Bytes received and reserved but not decoded yet, and the reservations
    %% that cover them with their sizes, newest first.
    buf = <<>>     :: binary(),
    chunks = []    :: [{non_neg_integer(), macula_peering_inflight:reservation()}],
    %% Chunks that arrived while the reader waited, in order, not reserved.
    unread = []    :: [binary()],
    %% What the reader waits for room for: the first unread chunk, or the
    %% decode transient of the whole frame of Wire bytes at the head of buf;
    %% queued with a place in line, unavailable when it could not ask for one.
    waiting = none :: none | {chunk | {transient, pos_integer()}, queued | unavailable},
    passive = false :: boolean(),
    %% The window of reading time of a frame in part: its timer, the bytes it
    %% needs and the bytes it has seen.
    window = none  :: none | {timer(), pos_integer(), non_neg_integer()},
    pause = none   :: none | timer(),
    failed = none  :: none | malformed()
}).

-opaque reader() :: #reader{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc A reader of `Stream' on the connection `Conn', which delivers from now
%% on. `body_window_ms' and `body_floor_bytes' are the window of reading time
%% and the floor, 10 s and 64 KiB when unset.
-spec new(reference(), pid(), #{kind := control,
                                body_window_ms => pos_integer(),
                                body_floor_bytes => pos_integer()}) -> reader().
new(Stream, Conn, #{kind := control} = Opts) ->
    _ = macula_quic:setopt(Stream, active, true),
    #reader{stream      = Stream,
            conn        = Conn,
            cap         = macula_peering_inflight:frame_cap(Conn, control),
            window_ms   = maps:get(body_window_ms, Opts, ?DEFAULT_BODY_WINDOW_MS),
            floor_bytes = maps:get(body_floor_bytes, Opts, ?DEFAULT_BODY_FLOOR_BYTES)}.

%% @doc Read a chunk the stream delivered.
-spec data(reader(), binary()) -> answer().
data(#reader{failed = none, waiting = none, unread = []} = R, Bin) when byte_size(Bin) > 0 ->
    read_unread(R#reader{unread = [Bin]}, []);
data(#reader{failed = none, waiting = {_What, _How}, unread = Unread} = R, Bin) when byte_size(Bin) > 0 ->
    {[], R#reader{unread = Unread ++ [Bin]}};
data(#reader{failed = none} = R, <<>>) ->
    {[], R};
data(R, Bin) when is_binary(Bin) ->
    {[], R}.

%% @doc Go on after `{macula_peering_inflight, resume, Stream}': take the
%% reservation the reader waited for, when it came, and read on.
-spec resume(reader()) -> answer().
resume(#reader{waiting = none} = R) ->
    {[], R};
resume(#reader{stream = Stream, waiting = {What, _How}} = R) ->
    resumed(macula_peering_inflight:take_admitted(Stream), What, R).

%% @doc Handle one of the reader's timers. A timer the reader no longer runs
%% changes nothing.
-spec timeout(reader(), {macula_peering_inflight, reader_timer, reference(), term()}) -> answer().
timeout(#reader{stream = Stream, failed = Reason} = R,
        {macula_peering_inflight, reader_timer, Stream, {malformed, Reason}}) ->
    {malformed, Reason, R};
timeout(#reader{stream = Stream, window = {{_TRef, Ref}, Need, Seen}} = R,
        {macula_peering_inflight, reader_timer, Stream, {body_window, Ref}}) ->
    window_ended(Seen >= Need, R#reader{window = none});
timeout(#reader{stream = Stream, pause = {_TRef, Ref}} = R,
        {macula_peering_inflight, reader_timer, Stream, {pause_limit, Ref}}) ->
    pause_ended(R#reader{pause = none});
timeout(#reader{stream = Stream} = R, {macula_peering_inflight, reader_timer, Stream, _Ended}) ->
    {[], R}.

%% @doc Stop reading: release what the reader holds, give up its place in
%% line, and let the stream deliver again, for a process that drops what
%% arrives from now on.
-spec close(reader()) -> ok.
close(#reader{stream = Stream} = R) ->
    _ = released(R),
    _ = macula_quic:setopt(Stream, active, true),
    ok.

%%%===================================================================
%%% Reading
%%%===================================================================

%% Reserves the first unread chunk and reads on, until no chunk is unread or
%% one waits for room.
read_unread(#reader{unread = []} = R, Items) ->
    {lists:reverse(Items), delivering(R)};
read_unread(#reader{conn = Conn, stream = Stream, unread = [Bin | _]} = R, Items) ->
    chunk_admitted(macula_peering_inflight:admit(Conn, control, byte_size(Bin), unread, Stream), R, Items).

chunk_admitted({ok, Reservation}, #reader{unread = [Bin | Rest]} = R, Items) ->
    read_on(appended(R#reader{unread = Rest}, Bin, Reservation), Items);
chunk_admitted(Answer, R, Items) ->
    {lists:reverse(Items), waits(R, chunk, Answer)}.

appended(#reader{buf = Buf, chunks = Chunks, window = Window} = R, Bin, Reservation) ->
    R#reader{buf    = <<Buf/binary, Bin/binary>>,
             chunks = [{byte_size(Bin), Reservation} | Chunks],
             window = seen(Window, byte_size(Bin))}.

seen(none, _Bytes) -> none;
seen({Timer, Need, Seen}, Bytes) -> {Timer, Need, Seen + Bytes}.

%% Reads the frames at the head of buf while the decode transient of each fits.
read_on(#reader{buf = Buf, cap = Cap} = R, Items) ->
    framed(macula_frame:frame_bytes(Buf, Cap), R, Items).

framed({complete, Wire}, R, Items) ->
    transient_admitted(transient(R, Wire), Wire, R, Items);
framed({more, Needed}, R, Items) ->
    read_unread(windowed(R, Needed), Items);
framed({error, frame_too_large}, R, Items) ->
    failed(frame_too_large, R, Items).

transient(#reader{conn = Conn, stream = Stream}, Wire) ->
    macula_peering_inflight:admit(Conn, transient_kind(Wire),
                                  macula_peering_inflight:decode_transient_bytes(Wire), decoding, Stream).

transient_kind(Wire) when Wire =< ?SMALL_FRAME_BYTES -> small_control;
transient_kind(_Wire) -> control.

transient_admitted({ok, Transient}, Wire, #reader{buf = Buf, cap = Cap} = R, Items) ->
    decoded(macula_frame:decode_frame(Buf, Cap), Wire, Transient, R, Items);
transient_admitted(Answer, Wire, R, Items) ->
    {lists:reverse(Items), waits(R, {transient, Wire}, Answer)}.

%% A decoded frame keeps its wire bytes, taken off the oldest chunk
%% reservations, and the part of its transient its terms hold; the rest of the
%% transient goes back. An invalid frame gives back both.
decoded({ok, Frame, Count, Rest}, Wire, Transient, #reader{chunks = Chunks} = R, Items) ->
    Type = macula_frame:frame_type(Frame),
    {Parts, Left} = taken(Wire, lists:reverse(Chunks), Type, []),
    Kept = macula_peering_inflight:settled_bytes(Wire, Count) - Wire,
    {Terms, Spare} = macula_peering_inflight:split(Transient, Kept, Type),
    ok = macula_peering_inflight:release(Spare),
    read_on(R#reader{buf = Rest, chunks = lists:reverse(Left)},
            [{frame, Frame, [Terms | Parts]} | Items]);
decoded({invalid, Invalid, _Count, Rest}, Wire, Transient, #reader{chunks = Chunks} = R, Items) ->
    {Parts, Left} = taken(Wire, lists:reverse(Chunks), invalid_frame, []),
    ok = macula_peering_inflight:release([Transient | Parts]),
    read_on(R#reader{buf = Rest, chunks = lists:reverse(Left)}, [Invalid | Items]);
decoded({error, Reason}, _Wire, Transient, R, Items) ->
    ok = macula_peering_inflight:release(Transient),
    failed(Reason, R, Items).

%% Takes Wire bytes off the front of the chunk reservations, oldest first, as
%% reservations of their own for a frame of Type.
taken(0, Chunks, _Type, Parts) ->
    {Parts, Chunks};
taken(Wire, [{Size, Reservation} | Rest], Type, Parts) when Size =< Wire ->
    taken(Wire - Size, Rest, Type, [Reservation | Parts]);
taken(Wire, [{Size, Reservation} | Rest], Type, Parts) ->
    {Part, Left} = macula_peering_inflight:split(Reservation, Wire, Type),
    {[Part | Parts], [{Size - Wire, Left} | Rest]}.

%% Nothing after a malformed frame can be read: the reader releases what it
%% holds and reads no more. Frames read before it go out first, and the
%% malformed answer follows through timeout/2.
failed(Reason, R, []) ->
    {malformed, Reason, stopped(R#reader{failed = Reason})};
failed(Reason, #reader{stream = Stream} = R, Items) ->
    self() ! {macula_peering_inflight, reader_timer, Stream, {malformed, Reason}},
    {lists:reverse(Items), stopped(R#reader{failed = Reason})}.

stopped(R) ->
    passive((released(R))#reader{passive = false}).

%% Releases what the reader holds, gives up its place in line and stops its
%% timers.
released(#reader{chunks = Chunks} = R) ->
    ok = macula_peering_inflight:release([Reservation || {_Size, Reservation} <- Chunks]),
    ok = left_line(R),
    cancel_pause(cancel_window(R#reader{buf = <<>>, chunks = [], unread = [], waiting = none})).

left_line(#reader{waiting = none}) ->
    ok;
left_line(#reader{stream = Stream}) ->
    macula_peering_inflight:cancel_wait(Stream).

%%%===================================================================
%%% Waiting and windows
%%%===================================================================

%% Waits for room: the stream stops delivering, the window of reading time
%% stops, and the pause limit runs for this wait.
waits(#reader{stream = Stream, conn = Conn} = R, What, Answer) ->
    #{pause_limit_ms := Ms} = macula_peering_inflight:connection_settings(Conn),
    Waiting = passive(cancel_window(cancel_pause(R))),
    Waiting#reader{waiting = {What, how(Answer)}, pause = timer(Stream, pause_limit, Ms)}.

how(queued) -> queued;
how({error, unavailable}) -> unavailable.

resumed(none, _What, R) ->
    {[], R};
resumed({ok, Reservation}, chunk, #reader{unread = [Bin | Rest]} = R) ->
    read_on(appended(cancel_pause(R#reader{waiting = none, unread = Rest}), Bin, Reservation), []);
resumed({ok, Transient}, {transient, Wire}, R) ->
    transient_admitted({ok, Transient}, Wire, cancel_pause(R#reader{waiting = none}), []).

pause_ended(#reader{conn = Conn} = R) ->
    #{role := Role} = macula_peering_inflight:connection_settings(Conn),
    paused_past_the_limit(Role, R).

%% A client link gives up; a station link keeps its place in line, or asks
%% again for one it could not take.
paused_past_the_limit(client, R) ->
    {busy, R};
paused_past_the_limit(station, #reader{waiting = {What, unavailable}} = R) ->
    asked_again(What, R#reader{waiting = none});
paused_past_the_limit(station, #reader{stream = Stream, conn = Conn} = R) ->
    #{pause_limit_ms := Ms} = macula_peering_inflight:connection_settings(Conn),
    {[], R#reader{pause = timer(Stream, pause_limit, Ms)}}.

asked_again(chunk, R) ->
    read_unread(R, []);
asked_again({transient, Wire}, R) ->
    transient_admitted(transient(R, Wire), Wire, R, []).

%% A frame in part at the head of buf has a window of reading time running; an
%% empty buf has none.
windowed(#reader{buf = <<>>} = R, _Needed) ->
    cancel_window(R);
windowed(#reader{window = none, stream = Stream, window_ms = Ms, floor_bytes = Floor} = R, Needed) ->
    R#reader{window = {timer(Stream, body_window, Ms), min(Floor, Needed), 0}};
windowed(R, _Needed) ->
    R.

%% A window that saw what it needed starts the next one for what is still
%% missing.
window_ended(true, #reader{buf = Buf, cap = Cap} = R) ->
    {[], rewindowed(macula_frame:frame_bytes(Buf, Cap), R)};
window_ended(false, R) ->
    failed(body_stalled, R, []).

rewindowed({more, Needed}, R) -> windowed(R, Needed);
rewindowed(_Whole, R) -> R.

timer(Stream, Name, Ms) ->
    Ref = make_ref(),
    {erlang:send_after(Ms, self(), {macula_peering_inflight, reader_timer, Stream, {Name, Ref}}), Ref}.

cancel_window(#reader{window = none} = R) ->
    R;
cancel_window(#reader{window = {{TRef, _Ref}, _Need, _Seen}} = R) ->
    _ = erlang:cancel_timer(TRef),
    R#reader{window = none}.

cancel_pause(#reader{pause = none} = R) ->
    R;
cancel_pause(#reader{pause = {TRef, _Ref}} = R) ->
    _ = erlang:cancel_timer(TRef),
    R#reader{pause = none}.

passive(#reader{passive = true} = R) ->
    R;
passive(#reader{stream = Stream} = R) ->
    _ = macula_quic:setopt(Stream, active, false),
    R#reader{passive = true}.

delivering(#reader{passive = false} = R) ->
    R;
delivering(#reader{stream = Stream} = R) ->
    _ = macula_quic:setopt(Stream, active, true),
    R#reader{passive = false}.
