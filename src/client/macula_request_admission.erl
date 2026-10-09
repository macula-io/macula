%% @private
%% @doc Admission of the verified requests one provider node receives.
%%
%% A provider runs a request once. Every copy of a CALL or STREAM_OPEN that
%% reaches it, over any station link, is judged here once its signature and
%% target have verified, in the order the design gives
%% (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Requests):
%%
%% - the deadline lies inside the provider's clock minus 5 minutes and its
%%   clock plus 10 minutes;
%% - (caller, request_id) has not been seen before, until the deadline plus
%%   5 minutes. A copy with the same request hash gets the stored reply while
%%   the reply is kept, or waits for it while the handler runs; a copy with
%%   another hash is refused.
%%
%% The records are bounded, and a full bound refuses a request rather than
%% evicting:
%%
%% - the requests in flight: a caller holds at most `caller_quota', a share
%%   at most `share'. These are concurrency bounds, not rate limits: the
%%   place is released the moment the reply is stored (`store_reply/5') or
%%   the handler ends without one (`release/2'). A share is one incoming
%%   connection's place: the normalized seed of its link, as the pool's
%%   new-peer budget counts peers. A STREAM_OPEN's place is released by the
%%   link when the session ends, so one caller streams many chunks on many
%%   opens without filling its quota;
%% - the run-once markers: one compact marker of (caller, request_id) is
%%   kept until the deadline plus 5 minutes, so a replay inside that window
%%   never starts a second handler. The marker set is bounded by
%%   `seen_bytes' (`marker_bytes/0' each) and NEVER evicts a marker, since
%%   evicting one is what re-runs a replay; a request that would not fit is
%%   refused `admission_full';
%% - stored reply bytes are at most `reply_bytes' per caller and at most
%%   `reply_bytes_total' for all callers together, since callers are cheap
%%   to make. A reply past either is not kept, and a copy of its request is
%%   then refused. A kept reply is dropped at the request's deadline, when
%%   its use ends; its marker stays to the end of the run-once window.
%%
%% Every bound refusal carries how long the caller should wait before it is
%% likely to fit: `{Kind, RetryAfterMs}' with the nearest in-flight deadline
%% minus now for the caller, share and cap bounds, and the nearest marker
%% expiry minus now for the marker budget. The station link sends it as
%% `retry_after_ms=<n>' in the ERROR frame's existing detail.
%%
%% One process owns the records and serializes every change, so the counts
%% and the byte budget always count exactly what is held. Times are
%% milliseconds of wall-clock time, passed in by the caller.
%%
%% A record past its window leaves before any request is judged, so a bound
%% only ever counts live records, and the pool sweeps (`expire/2') for the
%% callers that never ask again. The markers and the in-flight records are
%% held in order of expiry, so removing the expired ones costs only those,
%% never a pass over the set (macula#37: a provider's entries were removed
%% only by a sweep nothing ran, and a station's liveness pings kept its
%% caller quota full for good).
%%
%% Every refusal, and every reply not stored, is counted by kind and logged
%% at most once a minute per kind (`macula_refusal_report'), naming the callers
%% refused (the first 8 bytes of the node id, in hex) and the procedures they
%% asked for, most refused first; `refusal_sources/1' has the latest of those
%% lists by kind, and `refusals/1'
%% reads the counts.
-module(macula_request_admission).

-behaviour(gen_server).

-export([start_link/1, admit/4, admit/5, store_reply/5, release/2, sweep/2, expire/2, refusals/1,
         refusal_sources/1, refusal_line/3, stop/1, released_at/1, marker_bytes/0, held/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-export_type([limits/0, request/0, refusal/0, verdict/0]).

-define(DEADLINE_PAST_TOLERANCE_MS, 5 * 60000).
-define(DEADLINE_AHEAD_MAX_MS, 10 * 60000).
-define(KEPT_PAST_DEADLINE_MS, 5 * 60000).
-define(REFUSAL_REPORT_WINDOW_MS, 60000).

%% What one run-once marker counts as under `seen_bytes': the two fixed ids
%% (32 + 16 bytes), the request hash (48), the two times (8 + 8), the marker
%% record and its entry in the expiry index. Measured on OTP 28 over 100,000
%% and 50,000 markers as the flat size of the live admission state after the
%% in-flight records are released: 502.2 and 501.5 bytes per marker. Budgeted
%% at 512, so the figure cannot drift under the real cost.
-define(MARKER_BYTES, 512).
-define(DEFAULT_SEEN_BYTES, 64 * 1024 * 1024).

-type limits() :: #{caller_quota := pos_integer(), share := pos_integer(), cap := pos_integer(),
                    reply_bytes := pos_integer(), reply_bytes_total := pos_integer(),
                    seen_bytes := pos_integer()}.
-type request() :: #{caller := <<_:256>>, request_id := <<_:128>>, request_hash := <<_:384>>,
                     deadline := non_neg_integer(), _ => _}.
-type refusal() :: {expired, non_neg_integer()} | {not_yet_valid, non_neg_integer()} | request_id_reused
                 | reply_not_kept | request_copy
                 | {caller_quota, non_neg_integer()} | {share_full, non_neg_integer()}
                 | {admission_full, non_neg_integer()}.
-type verdict() :: new | {copy, pending | {reply, term()}} | {refused, refusal()}.

%% The run-once record of one admitted request, kept until the deadline plus
%% 5 minutes. Its reply is useful only until the deadline and is dropped then.
-record(marker, {
    hash        :: <<_:384>>,
    expires_at  :: integer(),
    reply_until :: integer(),
    reply = none :: none | not_kept | {reply, term(), non_neg_integer()}
}).

%% One request in flight: its place in a share, held until the reply is
%% stored or the handler ends (or the window ends, as a backstop). Its
%% deadline is what a bound refusal names as `retry_after_ms': the nearest
%% one is the soonest the work can finish.
-record(flight, {
    share      :: term(),
    deadline   :: integer(),
    expires_at :: integer()
}).

-record(state, {
    limits          :: limits(),
    markers = #{}   :: #{{<<_:256>>, <<_:128>>} => #marker{}},
    %% Every marker key under its expiry, earliest first.
    marker_expiry = gb_sets:empty() :: gb_sets:set({integer(), {<<_:256>>, <<_:128>>}}),
    flight = #{}    :: #{{<<_:256>>, <<_:128>>} => #flight{}},
    flight_expiry = gb_sets:empty() :: gb_sets:set({integer(), {<<_:256>>, <<_:128>>}}),
    %% Every in-flight key under its deadline, for the wait a bound refusal names.
    flight_due = gb_sets:empty() :: gb_sets:set({integer(), {<<_:256>>, <<_:128>>}}),
    callers = #{}   :: #{<<_:256>> => gb_sets:set({integer(), {<<_:256>>, <<_:128>>}})},
    shares = #{}    :: #{term() => gb_sets:set({integer(), {<<_:256>>, <<_:128>>}})},
    seen_bytes = 0  :: non_neg_integer(),
    %% Stored replies to drop at their deadline, earliest first.
    reply_due = gb_sets:empty() :: gb_sets:set({integer(), {<<_:256>>, <<_:128>>}}),
    reply_bytes = #{} :: #{<<_:256>> => pos_integer()},
    reply_total = 0 :: non_neg_integer(),
    refusals        :: macula_refusal_report:t()
}).

%% @doc Start the admission of one provider node, linked to its pool.
-spec start_link(limits()) -> {ok, pid()} | {error, term()}.
start_link(#{caller_quota := Quota, share := Share, cap := Cap, reply_bytes := ReplyBytes,
             reply_bytes_total := ReplyTotal} = Given)
  when is_integer(Quota), Quota > 0, is_integer(Share), Share > 0, is_integer(Cap), Cap > 0,
       is_integer(ReplyBytes), ReplyBytes > 0, is_integer(ReplyTotal), ReplyTotal > 0 ->
    Limits = maps:merge(#{seen_bytes => ?DEFAULT_SEEN_BYTES}, Given),
    case maps:get(seen_bytes, Limits) of
        SeenBytes when is_integer(SeenBytes), SeenBytes > 0 ->
            gen_server:start_link(?MODULE, Limits, []);
        BadSeenBytes ->
            {error, {invalid_admission_limit, seen_bytes, BadSeenBytes}}
    end.

%% @doc When admission releases the in-flight record of a request with this
%% signed `Deadline' (milliseconds of wall-clock time): the deadline plus the
%% 5 minutes an entry is kept past it. A station link stops a CALL's handler
%% then (macula#64 F6), so handler processes never outnumber entries.
-spec released_at(integer()) -> integer().
released_at(Deadline) ->
    Deadline + ?KEPT_PAST_DEADLINE_MS.

%% @doc The bytes one run-once marker counts as under `seen_bytes'.
-spec marker_bytes() -> pos_integer().
marker_bytes() ->
    ?MARKER_BYTES.

%% @doc Judge a verified request arriving on `Share' at `NowMs'.
-spec admit(pid(), request(), term(), integer()) -> verdict().
admit(Admission, Request, Share, NowMs) ->
    admit(Admission, Request, Share, NowMs, 5_000).

%% @doc As `admit/4', waiting at most `TimeoutMs' for the verdict. An admission
%% that does not answer in time, or has stopped, exits the caller as
%% `gen_server:call/3' does.
-spec admit(pid(), request(), term(), integer(), timeout()) -> verdict().
admit(Admission, #{caller := <<_:256>>, request_id := <<_:128>>, request_hash := <<_:384>>,
                   deadline := Deadline} = Request, Share, NowMs, TimeoutMs)
  when is_integer(Deadline), Deadline >= 0, is_integer(NowMs) ->
    gen_server:call(Admission,
                    {admit, maps:with([caller, request_id, request_hash, deadline, procedure], Request), Share, NowMs},
                    TimeoutMs).

%% @doc Store the signed reply of an admitted request, `Bytes' long, for its
%% copies, at `NowMs'. The request's in-flight place is released here, since
%% the reply is about to be sent. `not_kept' when a byte bound leaves no room
%% for it or the deadline has passed, and `gone' when the admission no longer
%% waits for this reply.
-spec store_reply(pid(), request(), term(), non_neg_integer(), integer()) -> kept | not_kept | gone.
store_reply(Admission, #{caller := <<_:256>> = Caller, request_id := <<_:128>> = RequestId,
                         request_hash := <<_:384>> = Hash}, Reply, Bytes, NowMs)
  when is_integer(Bytes), Bytes >= 0, is_integer(NowMs) ->
    gen_server:call(Admission, {store_reply, {Caller, RequestId}, Hash, Reply, Bytes, NowMs}).

%% @doc Release the in-flight place of an admitted request whose handler ended
%% without storing a reply: a crash, a kill at the window's end, or a handler
%% that answers nothing. The run-once marker stays until its window ends.
%% `gone' when the request held no place (already released, or swept).
-spec release(pid(), request()) -> ok | gone.
release(Admission, #{caller := <<_:256>> = Caller, request_id := <<_:128>> = RequestId}) ->
    gen_server:call(Admission, {release, {Caller, RequestId}}).

%% @doc The markers held and the requests in flight, for tests and operators.
-spec held(pid()) -> #{markers := non_neg_integer(), in_flight := non_neg_integer()}.
held(Admission) ->
    gen_server:call(Admission, held).

%% @doc Remove the records whose window passed before `NowMs', freeing their
%% places, and drop the stored replies whose deadline passed. Returns how many
%% run-once markers were removed.
-spec sweep(pid(), integer()) -> non_neg_integer().
sweep(Admission, NowMs) when is_integer(NowMs) ->
    gen_server:call(Admission, {sweep, NowMs}).

%% @doc As `sweep/2', returning at once: the pool's periodic sweep, which
%% must not wait on its admission.
-spec expire(pid(), integer()) -> ok.
expire(Admission, NowMs) when is_integer(NowMs) ->
    gen_server:cast(Admission, {sweep, NowMs}).

%% @doc The callers and procedures of the latest refusal report of each kind,
%% most refused first: `{CallerPrefixHex, Procedure | undefined}' with its
%% count, and `other' for sources past the window's bound.
-spec refusal_sources(pid()) -> #{atom() => [{{binary(), binary() | undefined} | other, pos_integer()}]}.
refusal_sources(Admission) ->
    gen_server:call(Admission, refusal_sources).

%% @doc Every refusal, and every reply not stored (`reply_not_stored'),
%% counted by kind.
-spec refusals(pid()) -> #{atom() => pos_integer()}.
refusals(Admission) ->
    gen_server:call(Admission, refusals).

-spec stop(pid()) -> ok.
stop(Admission) ->
    gen_server:stop(Admission).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Limits) ->
    {ok, #state{limits = Limits, refusals = macula_refusal_report:new(?REFUSAL_REPORT_WINDOW_MS)}}.

handle_call({admit, #{deadline := Deadline} = Request, Share, Now}, _From, S) ->
    {_Removed, Live} = swept(Now, S),
    {Verdict, NewS} = in_window(deadline_verdict(Deadline, Now), Request, Share, Now, Live),
    {reply, Verdict, refusal_counted(Verdict, source(Request), Now, NewS)};
handle_call({store_reply, Key, Hash, Reply, Bytes, Now}, _From, S) ->
    {Result, NewS} = stored(maps:find(Key, S#state.markers), Key, Hash, Reply, Bytes, Now, S),
    {reply, Result, reply_counted(Result, source(Key), Now, NewS)};
handle_call({release, Key}, _From, S) ->
    {Released, NewS} = released_flight(maps:is_key(Key, S#state.flight), Key, S),
    {reply, Released, NewS};
handle_call(refusals, _From, #state{refusals = Report} = S) ->
    {reply, macula_refusal_report:counts(Report), S};
handle_call(refusal_sources, _From, #state{refusals = Report} = S) ->
    {reply, macula_refusal_report:last_sources(Report), S};
handle_call(held, _From, S) ->
    {reply, #{markers => map_size(S#state.markers), in_flight => map_size(S#state.flight)}, S};
handle_call({sweep, Now}, _From, S) ->
    {Removed, NewS} = swept(Now, S),
    {reply, Removed, NewS}.

handle_cast({sweep, Now}, S) ->
    {_Removed, NewS} = swept(Now, S),
    {noreply, NewS};
handle_cast(_Message, S) ->
    {noreply, S}.

%%--------------------------------------------------------------------
%% Internals
%%--------------------------------------------------------------------

%% A refusal is counted under its kind alone, never a retry hint, so the
%% kinds counted stay a fixed set.
refusal_counted({refused, Refusal}, Source, Now, S) -> count_kind(refusal_kind(Refusal), Source, Now, S);
refusal_counted(_AdmittedOrCopy, _Source, _Now, S)   -> S.

reply_counted(not_kept, Source, Now, S)     -> count_kind(reply_not_stored, Source, Now, S);
reply_counted(_KeptOrGone, _Source, _Now, S) -> S.

%% Who a refusal is about: the caller's node id prefix, and the procedure when
%% the request names one (a reply is counted by its marker's key, which does not).
source(#{caller := Caller} = Request) -> {caller_prefix(Caller), maps:get(procedure, Request, undefined)};
source({Caller, _RequestId})          -> {caller_prefix(Caller), undefined}.

caller_prefix(<<Prefix:8/binary, _/binary>>) -> binary:encode_hex(Prefix, lowercase).

refusal_kind({expired, _PastMs})          -> expired;
refusal_kind({not_yet_valid, _AheadMs})   -> not_yet_valid;
refusal_kind({caller_quota, _WaitMs})     -> caller_quota;
refusal_kind({share_full, _WaitMs})       -> share_full;
refusal_kind({admission_full, _WaitMs})   -> admission_full;
refusal_kind(Kind)                        -> Kind.

count_kind(Kind, Source, Now, #state{refusals = Report} = S) ->
    S#state{refusals = logged(macula_refusal_report:refused(Report, Kind, Source, Now), Kind)}.

logged({report, Count, Sources, Report}, Kind) ->
    logger:warning("~ts", [refusal_line(Count, Kind, Sources)]),
    Report;
logged({quiet, Report}, _Kind) ->
    Report.

%% @doc The warning a refusal report logs: how many, of what kind, and from
%% whom on what, most refused first.
-spec refusal_line(pos_integer(), atom(), [{term(), pos_integer()}]) -> iolist().
refusal_line(Count, Kind, Sources) ->
    [io_lib:format("[macula_request_admission] ~b refused: ~p, from ", [Count, Kind]),
     lists:join(", ", [source_text(Source, N) || {Source, N} <- Sources])].

source_text({Prefix, undefined}, N) -> io_lib:format("~ts (~b)", [Prefix, N]);
source_text({Prefix, Procedure}, N) -> io_lib:format("~ts on ~ts (~b)", [Prefix, Procedure, N]);
source_text(other, N)               -> io_lib:format("others (~b)", [N]).

deadline_verdict(Deadline, Now) when Deadline < Now - ?DEADLINE_PAST_TOLERANCE_MS ->
    {expired, Now - ?DEADLINE_PAST_TOLERANCE_MS - Deadline};
deadline_verdict(Deadline, Now) when Deadline > Now + ?DEADLINE_AHEAD_MAX_MS ->
    {not_yet_valid, Deadline - Now - ?DEADLINE_AHEAD_MAX_MS};
deadline_verdict(_Deadline, _Now) ->
    inside.

in_window(inside, #{caller := Caller, request_id := RequestId} = Request, Share, Now, S) ->
    Key = {Caller, RequestId},
    seen_marker(maps:find(Key, S#state.markers), Key, Request, Share, Now, S);
in_window(Refusal, _Request, _Share, _Now, S) ->
    {{refused, Refusal}, S}.

seen_marker(error, Key, Request, Share, Now, S) ->
    admitted(first_full(bounds(Key, Share, Now, S)), Key, Request, Share, S);
seen_marker({ok, #marker{hash = Hash} = Marker}, Key, #{request_hash := Hash} = Request, _Share, Now, S) ->
    copy_or_wait(Key, Marker, Request, Now, S);
seen_marker({ok, _MarkerWithAnotherHash}, _Key, _Request, _Share, _Now, S) ->
    {{refused, request_id_reused}, S}.

%% A copy of a request already served gets the stored reply until its
%% deadline; past that the reply is dropped and the copy is refused, since
%% the run-once window still bars a second handler. While the handler runs,
%% the copy waits. A reply the byte bounds would not keep is refused too.
copy_or_wait(_Key, #marker{reply = {reply, Reply, _Bytes}, reply_until = Until}, _Request, Now, S)
  when Now =< Until ->
    {{copy, {reply, Reply}}, S};
copy_or_wait(_Key, #marker{reply = not_kept}, _Request, _Now, S) ->
    {{refused, reply_not_kept}, S};
copy_or_wait(Key, _Marker, _Request, _Now, S) ->
    copy_after_release(maps:is_key(Key, S#state.flight), S).

copy_after_release(true, S)  -> {{copy, pending}, S};
copy_after_release(false, S) -> {{refused, request_copy}, S}.

%% In order: the caller's in-flight quota, the share's, the set's cap, the
%% run-once byte budget. Each bound carries the earliest wait it can name:
%% the nearest in-flight deadline for the first three (the soonest the work
%% can finish and free the place), the nearest marker expiry for the budget.
bounds({Caller, _RequestId}, Share, Now,
       #state{limits = Limits, flight = Flight, callers = Callers, shares = Shares,
              flight_due = FlightDue, marker_expiry = MarkerExpiry, seen_bytes = SeenBytes}) ->
    #{caller_quota := Quota, share := ShareMax, cap := Cap, seen_bytes := SeenMax} = Limits,
    CallerHeld = maps:get(Caller, Callers, gb_sets:empty()),
    ShareHeld = maps:get(Share, Shares, gb_sets:empty()),
    [{bound(caller_quota, CallerHeld, Now), gb_sets:size(CallerHeld) >= Quota},
     {bound(share_full, ShareHeld, Now), gb_sets:size(ShareHeld) >= ShareMax},
     {bound(admission_full, FlightDue, Now), map_size(Flight) >= Cap},
     {bound(admission_full, MarkerExpiry, Now), SeenBytes + ?MARKER_BYTES > SeenMax}].

bound(Kind, Expiry, Now) ->
    {Kind, later(gb_sets:is_empty(Expiry), Expiry, Now)}.

%% How long until the earliest release this scope can name; 0 when it holds
%% nothing (a full bound always holds something, so this is only a guard).
later(true, _Expiry, _Now) -> 0;
later(false, Expiry, Now)  -> max(0, element(1, gb_sets:smallest(Expiry)) - Now).

first_full(Bounds) ->
    first_full_bound([Refusal || {Refusal, true} <- Bounds]).

first_full_bound([Refusal | _]) -> {refused, Refusal};
first_full_bound([])            -> room.

admitted(room, {Caller, _RequestId} = Key, #{request_hash := Hash, deadline := Deadline}, Share, S) ->
    ExpiresAt = released_at(Deadline),
    Marker = #marker{hash = Hash, expires_at = ExpiresAt, reply_until = Deadline},
    Flight = #flight{share = Share, deadline = Deadline, expires_at = ExpiresAt},
    {new, S#state{markers = maps:put(Key, Marker, S#state.markers),
                  marker_expiry = gb_sets:add_element({ExpiresAt, Key}, S#state.marker_expiry),
                  flight = maps:put(Key, Flight, S#state.flight),
                  flight_expiry = gb_sets:add_element({ExpiresAt, Key}, S#state.flight_expiry),
                  flight_due = gb_sets:add_element({Deadline, Key}, S#state.flight_due),
                  callers = held_in(Caller, Deadline, Key, S#state.callers),
                  shares = held_in(Share, Deadline, Key, S#state.shares),
                  seen_bytes = S#state.seen_bytes + ?MARKER_BYTES}};
admitted(Refused, _Key, _Request, _Share, S) ->
    {Refused, S}.

stored({ok, #marker{hash = Hash, reply = none, reply_until = Until} = Marker}, Key, Hash, Reply, Bytes, Now, S) ->
    {Result, S1} = store_marker(Now =< Until andalso room_for(Key, Bytes, S), Marker, Key, Reply, Bytes, Until, S),
    {Result, release_flight(Key, S1)};
stored(_NotWaitingForThisReply, _Key, _Hash, _Reply, _Bytes, _Now, S) ->
    {gone, S}.

%% The reply bytes of this caller and of all callers together are bounded;
%% callers are cheap to make, so the total is the one that needs the check.
room_for({Caller, _RequestId}, Bytes, #state{limits = Limits, reply_bytes = Held, reply_total = Total}) ->
    #{reply_bytes := Max, reply_bytes_total := TotalMax} = Limits,
    maps:get(Caller, Held, 0) + Bytes =< Max andalso Total + Bytes =< TotalMax.

store_marker(true, Marker, {Caller, _RequestId} = Key, Reply, Bytes, Until, S) ->
    {kept, S#state{markers = maps:put(Key, Marker#marker{reply = {reply, Reply, Bytes}}, S#state.markers),
                   reply_due = gb_sets:add_element({Until, Key}, S#state.reply_due),
                   reply_bytes = bump(Caller, Bytes, S#state.reply_bytes),
                   reply_total = S#state.reply_total + Bytes}};
store_marker(false, Marker, Key, _Reply, _Bytes, _Until, S) ->
    {not_kept, S#state{markers = maps:put(Key, Marker#marker{reply = not_kept}, S#state.markers)}}.

release_flight(Key, S) ->
    {_Released, NewS} = released_flight(maps:is_key(Key, S#state.flight), Key, S),
    NewS.

released_flight(false, _Key, S) ->
    {gone, S};
released_flight(true, {Caller, _RequestId} = Key, #state{flight = Flight} = S) ->
    {#flight{share = Share, deadline = Deadline, expires_at = ExpiresAt}, Remaining} = maps:take(Key, Flight),
    {ok, S#state{flight = Remaining,
                 flight_expiry = gb_sets:delete_any({ExpiresAt, Key}, S#state.flight_expiry),
                 flight_due = gb_sets:delete_any({Deadline, Key}, S#state.flight_due),
                 callers = left(Caller, Deadline, Key, S#state.callers),
                 shares = left(Share, Deadline, Key, S#state.shares)}}.

held_in(Scope, ExpiresAt, Key, Scopes) ->
    Scopes#{Scope => gb_sets:add_element({ExpiresAt, Key}, maps:get(Scope, Scopes, gb_sets:empty()))}.

%% A scope whose set falls empty leaves the map, so the maps hold only what is
%% counted.
left(Scope, ExpiresAt, Key, Scopes) ->
    left_scope(maps:find(Scope, Scopes), Scope, ExpiresAt, Key, Scopes).

left_scope(error, _Scope, _ExpiresAt, _Key, Scopes) ->
    Scopes;
left_scope({ok, Set}, Scope, ExpiresAt, Key, Scopes) ->
    Remaining = gb_sets:delete_any({ExpiresAt, Key}, Set),
    left_remaining(gb_sets:is_empty(Remaining), Remaining, Scope, Scopes).

left_remaining(true, _Remaining, Scope, Scopes)  -> maps:remove(Scope, Scopes);
left_remaining(false, Remaining, Scope, Scopes)  -> Scopes#{Scope => Remaining}.

%% The records past their window at `Now' leave, earliest first: the stored
%% replies whose deadline passed are dropped, then the markers and any
%% in-flight record they still hold. Only the markers are counted: they are
%% the durable records the bounds are about.
swept(Now, S) ->
    dropped_flights(swept_markers(Now, 0, dropped_replies(Now, S)), Now).

dropped_replies(Now, #state{reply_due = Due} = S) ->
    next_due(earliest(gb_sets:is_empty(Due), Due), Now, S).

earliest(true, _Set)  -> none;
earliest(false, Set)  -> gb_sets:smallest(Set).

next_due({Until, Key}, Now, S) when Until < Now ->
    dropped_replies(Now, due_cleared(Until, Key, S));
next_due(_LiveOrNone, _Now, S) ->
    S.

due_cleared(Until, Key, #state{reply_due = Due, markers = Markers} = S) ->
    due_reply_dropped(maps:find(Key, Markers), Key,
                      S#state{reply_due = gb_sets:delete_any({Until, Key}, Due)}).

due_reply_dropped({ok, #marker{reply = {reply, _Reply, Bytes}} = Marker}, Key, S) ->
    S#state{markers = maps:put(Key, Marker#marker{reply = none}, S#state.markers),
            reply_bytes = bump(element(1, Key), -Bytes, S#state.reply_bytes),
            reply_total = S#state.reply_total - Bytes};
due_reply_dropped(_GoneOrNoReply, _Key, S) ->
    S.

swept_markers(Now, Removed, #state{marker_expiry = Expiry} = S) ->
    next_expired(earliest(gb_sets:is_empty(Expiry), Expiry), Now, Removed, S).

next_expired({ExpiresAt, Key}, Now, Removed, S) when ExpiresAt < Now ->
    swept_markers(Now, Removed + 1, forget_marker(Key, S));
next_expired(_LiveOrNone, _Now, Removed, S) ->
    {Removed, S}.

%% Only a marker the set holds is forgotten: a key in the index but not in
%% the map is a broken invariant, and crashes here rather than leave the
%% sweep taking the same smallest key for ever (Fable QA on #37).
forget_marker({Caller, _RequestId} = Key, #state{markers = Markers} = S) ->
    marker_taken(maps:take(Key, Markers), Caller, Key, S).

marker_taken({#marker{expires_at = ExpiresAt, reply = Reply}, Remaining}, Caller, Key, S) ->
    released_marker(S#state{markers = Remaining,
                            marker_expiry = gb_sets:delete_any({ExpiresAt, Key}, S#state.marker_expiry),
                            seen_bytes = S#state.seen_bytes - ?MARKER_BYTES,
                            reply_bytes = bump(Caller, -reply_size(Reply), S#state.reply_bytes),
                            reply_total = S#state.reply_total - reply_size(Reply)}, Key);
marker_taken(error, _Caller, Key, _S) ->
    error({marker_missing, Key}).

released_marker(S, Key) ->
    release_flight(Key, S).

%% The backstop for a flight whose marker is gone (they expire together, so
%% this finds nothing while the invariants hold).
dropped_flights({Removed, S}, Now) ->
    {Removed, swept_flights(Now, S)}.

swept_flights(Now, #state{flight_expiry = Expiry} = S) ->
    next_flight(earliest(gb_sets:is_empty(Expiry), Expiry), Now, S).

next_flight({ExpiresAt, Key}, Now, S) when ExpiresAt < Now ->
    {_Released, NewS} = released_flight(maps:is_key(Key, S#state.flight), Key, S),
    swept_flights(Now, NewS);
next_flight(_LiveOrNone, _Now, S) ->
    S.

reply_size({reply, _Reply, Bytes}) -> Bytes;
reply_size(_PendingOrNotKept)      -> 0.

%% A count that falls to zero leaves the map, so the maps hold only what is
%% counted.
bump(Key, Delta, Counts) ->
    counted(Key, maps:get(Key, Counts, 0) + Delta, Counts).

counted(Key, Count, Counts) when Count > 0 -> Counts#{Key => Count};
counted(Key, _Zero, Counts)                -> maps:remove(Key, Counts).
