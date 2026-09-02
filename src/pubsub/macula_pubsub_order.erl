%%% @doc Per-subscription delivery ordering for pubsub events.
%%%
%%% A publisher stamps every fact with a pool-monotonic `seq'
%%% (`macula_client'), contiguous within a publisher's lifetime (a
%%% refused publish does not burn a number). The mesh sends copies down
%%% several links at once, so a subscriber receives seqs out of order and
%%% `macula_client' dedups to the FIRST arrival — which scrambles a
%%% single publisher's stream. This module puts the deduped stream back
%%% into per-publisher order, offering three delivery modes a subscriber
%%% picks at `subscribe' time:
%%%
%%% <ul>
%%%   <li><strong>ordered</strong> (default) — per-publisher FIFO by
%%%       seq. Out-of-order arrivals are buffered and released in order;
%%%       a genuinely missing seq is skipped after a timeout (see
%%%       `flush/3'), trading a bounded delay for the lost fact.</li>
%%%   <li><strong>latest_only</strong> — deliver only if the seq exceeds
%%%       the highest already delivered for that publisher (drop stale).
%%%       No buffering, no head-of-line delay. For state-snapshot
%%%       consumers that want freshness over completeness.</li>
%%%   <li><strong>as_arrives</strong> — deliver immediately in arrival
%%%       order (the pre-1.x behaviour). Zero added latency; the consumer
%%%       handles ordering itself.</li>
%%% </ul>
%%%
%%% seq re-bases to wall-clock microseconds when a publisher's pool
%%% restarts, so a large forward jump (> `?EPOCH_JUMP') is read as a
%%% restart: the old expected counter is abandoned rather than waited on.
%%% A large BACKWARD jump is read the same way. A publisher that restarts
%%% with a counter re-seeded from zero instead of wall-clock (a
%%% macula-station's own `hecate_pubsub_server' before 10.17.0) would
%%% otherwise have every fact after the restart dropped as "past" until
%%% the counter climbed back over the old watermark -- silently, with the
%%% link, the wire subscription and dedup all healthy. That is how
%%% hecate-stations went deaf for 10+ hours after a fleet rollout on
%%% 2026-09-02. A backstep within the threshold is still a late duplicate.
%%%
%%% Pure and side-effect-free: it returns the events to deliver now, and
%%% the caller does the sending. `flush/3' is driven by the caller on a
%%% timer to release buffers whose gap has timed out.
-module(macula_pubsub_order).

-export([new/1, new/2, offer/5, flush/3, buffered/1, skips/1]).
-export_type([t/0, mode/0]).

%% A seq gap wider than this, in EITHER direction, is a publisher
%% restart (seq re-based to µs, or rewound to zero), not a run of lost
%% facts. Losing this many consecutive facts is an outage, not
%% reordering; a late duplicate never trails by this much.
-define(EPOCH_JUMP, 10000).

%% Per-publisher reorder-buffer cap. The flush timeout bounds a buffer
%% in TIME; this bounds it in COUNT for a publisher gapping under a high
%% rate — when exceeded, the head gap is skipped early rather than held.
-define(DEFAULT_MAX_BUFFER, 1024).

-type mode()  :: ordered | latest_only | as_arrives.
-type seq()   :: non_neg_integer().
-type event() :: term().

%% Per-publisher state. `ordered' uses `next' + `buf'; `latest_only'
%% uses `high'. Unused fields stay `undefined'.
-record(pub, {
    next :: seq() | undefined,
    buf  = #{}  :: #{seq() => {event(), integer()}},
    high :: seq() | undefined
}).

-opaque t() :: #{mode := mode(),
                 pubs := #{binary() => #pub{}},
                 skips := non_neg_integer(),
                 max := pos_integer()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(mode()) -> t().
new(Mode) ->
    new(Mode, ?DEFAULT_MAX_BUFFER).

-spec new(mode(), pos_integer()) -> t().
new(Mode, Max)
  when (Mode =:= ordered orelse Mode =:= latest_only orelse Mode =:= as_arrives),
       is_integer(Max), Max > 0 ->
    #{mode => Mode, pubs => #{}, skips => 0, max => Max}.

%% @doc Offer an arrived (deduped) event. Returns the events to deliver
%% now, in order, and the updated state. `NowMs' timestamps buffered
%% arrivals for `flush/3'.
-spec offer(t(), binary(), seq(), event(), integer()) -> {[event()], t()}.
offer(#{mode := as_arrives} = S, _Pub, _Seq, Ev, _Now) ->
    {[Ev], S};
offer(#{mode := latest_only, pubs := P} = S, Pub, Seq, Ev, _Now) ->
    offer_latest(maps:get(Pub, P, undefined), S, Pub, Seq, Ev);
offer(#{mode := ordered, pubs := P} = S, Pub, Seq, Ev, Now) ->
    offer_ordered(maps:get(Pub, P, undefined), S, Pub, Seq, Ev, Now).

%% @doc Release buffers whose head has waited past `TimeoutMs' for a
%% missing seq: skip the gap up to the smallest buffered seq and drain.
%% Returns the events to deliver now and the updated state (skip counter
%% advanced by one per gap given up on). A no-op for non-ordered modes.
-spec flush(t(), integer(), non_neg_integer()) -> {[event()], t()}.
flush(#{mode := ordered, pubs := P, skips := Sk} = S, Now, Timeout) ->
    {Evs, Pubs2, Sk2} = flush_pubs(maps:to_list(P), Now, Timeout, [], #{}, Sk),
    {Evs, S#{pubs := Pubs2, skips := Sk2}};
flush(S, _Now, _Timeout) ->
    {[], S}.

%% @doc Total events currently held in reorder buffers (introspection).
-spec buffered(t()) -> non_neg_integer().
buffered(#{pubs := P}) ->
    lists:sum([map_size(B) || #pub{buf = B} <- maps:values(P)]).

%% @doc Count of gaps skipped after timeout since `new/1' (telemetry:
%% the genuine per-publisher loss rate).
-spec skips(t()) -> non_neg_integer().
skips(#{skips := N}) -> N.

%%%===================================================================
%%% latest_only
%%%===================================================================

offer_latest(undefined, S, Pub, Seq, Ev) ->
    {[Ev], put_pub(S, Pub, #pub{high = Seq})};
%% Huge backward jump: the publisher restarted with a counter re-seeded
%% from zero. Take it as the new high-water mark (see `offer_ordered').
offer_latest(#pub{high = H}, S, Pub, Seq, Ev) when Seq + ?EPOCH_JUMP < H ->
    {[Ev], put_pub(S, Pub, #pub{high = Seq})};
offer_latest(#pub{high = H}, S, _Pub, Seq, _Ev) when Seq =< H ->
    {[], S};
offer_latest(#pub{} = Pst, S, Pub, Seq, Ev) ->
    {[Ev], put_pub(S, Pub, Pst#pub{high = Seq})}.

%%%===================================================================
%%% ordered
%%%===================================================================

%% First fact from this publisher: its seq is the base.
offer_ordered(undefined, S, Pub, Seq, Ev, Now) ->
    offer_ordered(#pub{next = Seq, buf = #{}}, S, Pub, Seq, Ev, Now);
%% In order: deliver, advance, drain any contiguous buffered tail.
offer_ordered(#pub{next = Next} = Pst, S, Pub, Seq, Ev, _Now)
  when Seq =:= Next ->
    {Evs, Pst2} = drain(Pst#pub{next = Next + 1}, [Ev]),
    {lists:reverse(Evs), put_pub(S, Pub, Pst2)};
%% Huge jump either way: the publisher restarted -- forward when its
%% seq re-based to wall-clock µs, backward when it re-seeded from zero.
%% Deliver whatever is buffered (old epoch, in seq order), then rebase.
offer_ordered(#pub{next = Next, buf = Buf}, S, Pub, Seq, Ev, _Now)
  when Seq > Next + ?EPOCH_JUMP; Seq + ?EPOCH_JUMP < Next ->
    Old = [E || {_Sq, {E, _Arr}} <- lists:keysort(1, maps:to_list(Buf))],
    {Old ++ [Ev], put_pub(S, Pub, #pub{next = Seq + 1, buf = #{}})};
%% Future within the same epoch: buffer it, then skip the head gap early
%% if the buffer is now over the count cap.
offer_ordered(#pub{next = Next, buf = Buf} = Pst, S, Pub, Seq, Ev, Now)
  when Seq > Next ->
    Pst2 = Pst#pub{buf = maps:put(Seq, {Ev, Now}, Buf)},
    cap_buffer(map_size(Pst2#pub.buf) > maps:get(max, S), Pst2, S, Pub);
%% Past: already delivered or skipped (also a late duplicate). Drop.
offer_ordered(#pub{}, S, _Pub, _Seq, _Ev, _Now) ->
    {[], S}.

%% Under the cap: just hold the buffered fact.
cap_buffer(false, Pst, S, Pub) ->
    {[], put_pub(S, Pub, Pst)};
%% Over the cap: give up on the head gap now, skipping to the smallest
%% buffered seq and draining its contiguous run (counts as one skip).
cap_buffer(true, #pub{buf = Buf} = Pst, #{skips := Sk} = S, Pub) ->
    MinSeq = lists:min(maps:keys(Buf)),
    {Evs, Pst2} = drain(Pst#pub{next = MinSeq}, []),
    {lists:reverse(Evs), put_pub(S#{skips := Sk + 1}, Pub, Pst2)}.

%% Pull contiguous seqs starting at `next' out of the buffer.
drain(#pub{next = Next, buf = Buf} = Pst, Acc) ->
    drain_step(maps:take(Next, Buf), Pst, Acc).

drain_step({{Ev, _Arr}, Buf2}, #pub{next = Next} = Pst, Acc) ->
    drain(Pst#pub{next = Next + 1, buf = Buf2}, [Ev | Acc]);
drain_step(error, Pst, Acc) ->
    {Acc, Pst}.

%%%===================================================================
%%% flush (timeout skip)
%%%===================================================================

flush_pubs([], _Now, _Timeout, Evs, Acc, Sk) ->
    {lists:reverse(Evs), Acc, Sk};
flush_pubs([{Pub, Pst} | Rest], Now, Timeout, Evs, Acc, Sk) ->
    {PubEvs, Pst2, Sk2} = flush_pub(Pst, Now, Timeout, [], Sk),
    flush_pubs(Rest, Now, Timeout, lists:reverse(PubEvs) ++ Evs,
               Acc#{Pub => Pst2}, Sk2).

flush_pub(#pub{buf = Buf} = Pst, _Now, _Timeout, EvAcc, Sk)
  when map_size(Buf) =:= 0 ->
    {lists:reverse(EvAcc), Pst, Sk};
flush_pub(#pub{buf = Buf} = Pst, Now, Timeout, EvAcc, Sk) ->
    Oldest = lists:min([Arr || {_Sq, {_E, Arr}} <- maps:to_list(Buf)]),
    flush_when_expired(Now - Oldest >= Timeout, Pst, Now, Timeout, EvAcc, Sk).

flush_when_expired(false, Pst, _Now, _Timeout, EvAcc, Sk) ->
    {lists:reverse(EvAcc), Pst, Sk};
flush_when_expired(true, #pub{buf = Buf} = Pst, Now, Timeout, EvAcc, Sk) ->
    MinSeq = lists:min(maps:keys(Buf)),
    %% `drain' returns events newest-first; `EvAcc' is kept newest-first
    %% and reversed once in the base clause, so prepend directly.
    {Evs, Pst2} = drain(Pst#pub{next = MinSeq}, []),
    flush_pub(Pst2, Now, Timeout, Evs ++ EvAcc, Sk + 1).

%%%===================================================================
%%% helpers
%%%===================================================================

put_pub(#{pubs := P} = S, Pub, Pst) ->
    S#{pubs := maps:put(Pub, Pst, P)}.
