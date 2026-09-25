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
%% - (caller, request_id) has not been seen before. The entry is kept until
%%   the deadline plus 5 minutes. A copy with the same request hash waits for
%%   the reply, or gets the stored reply; a copy with another hash is refused.
%%
%% The entries are bounded, and a full bound refuses a request rather than
%% evicting an entry:
%%
%% - each caller holds at most `caller_quota' entries;
%% - each share holds at most `share' entries. A share is one incoming
%%   connection's place: the normalized seed of its link, as the pool's
%%   new-peer budget counts peers;
%% - the set holds at most `cap' entries. The pool sets it to `share' times
%%   the most distinct shares one entry lifetime can see, so the set cannot
%%   fill before every share has;
%% - stored reply bytes are at most `reply_bytes' per caller and at most
%%   `reply_bytes_total' for all callers together, since callers are cheap
%%   to make. A reply past either is not kept, and a copy of its request is
%%   then refused.
%%
%% One process owns the entries and serializes every change, so a quota, a
%% share and the set always count exactly the entries held. Times are
%% milliseconds of wall-clock time, passed in by the caller.
%%
%% An entry past its deadline plus 5 minutes leaves the set before any
%% request is judged, so a bound only ever counts live entries, and the
%% pool sweeps the set on a timer (`expire/2') for the callers that never
%% ask again. The entries are also held in order of expiry, so removing the
%% expired ones costs only those, never a pass over the set (macula#37: a
%% provider's entries were removed only by a sweep nothing ran, and a
%% station's liveness pings kept its caller quota full for good).
%%
%% Every refusal, and every reply not stored, is counted by kind and logged
%% at most once a minute per kind (`macula_refusal_report'), naming the callers
%% refused (the first 8 bytes of the node id, in hex) and the procedures they
%% asked for, most refused first; `refusal_sources/1' has the latest of those
%% lists by kind, and `refusals/1'
%% reads the counts.
-module(macula_request_admission).

-behaviour(gen_server).

-export([start_link/1, admit/4, admit/5, store_reply/5, sweep/2, expire/2, refusals/1, refusal_sources/1,
         refusal_line/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-export_type([limits/0, request/0, refusal/0, verdict/0]).

-define(DEADLINE_PAST_TOLERANCE_MS, 5 * 60000).
-define(DEADLINE_AHEAD_MAX_MS, 10 * 60000).
-define(KEPT_PAST_DEADLINE_MS, 5 * 60000).
-define(REFUSAL_REPORT_WINDOW_MS, 60000).

-type limits() :: #{caller_quota := pos_integer(), share := pos_integer(), cap := pos_integer(),
                    reply_bytes := pos_integer(), reply_bytes_total := pos_integer()}.
-type request() :: #{caller := <<_:256>>, request_id := <<_:128>>, request_hash := <<_:384>>,
                     deadline := non_neg_integer(), _ => _}.
-type refusal() :: {expired, pos_integer()} | {not_yet_valid, pos_integer()} | request_id_reused
                 | reply_not_kept | caller_quota | share_full | admission_full.
-type verdict() :: new | {copy, pending | {reply, term()}} | {refused, refusal()}.

-record(entry, {
    hash       :: <<_:384>>,
    expires_at :: integer(),
    share      :: term(),
    reply = pending :: pending | not_kept | {reply, term(), non_neg_integer()}
}).

-record(state, {
    limits          :: limits(),
    entries = #{}   :: #{{<<_:256>>, <<_:128>>} => #entry{}},
    %% Every entry's key under its expiry, earliest first.
    expiry = gb_sets:empty() :: gb_sets:set({integer(), {<<_:256>>, <<_:128>>}}),
    callers = #{}   :: #{<<_:256>> => pos_integer()},
    shares = #{}    :: #{term() => pos_integer()},
    reply_bytes = #{} :: #{<<_:256>> => pos_integer()},
    reply_total = 0 :: non_neg_integer(),
    refusals        :: macula_refusal_report:t()
}).

%% @doc Start the admission of one provider node, linked to its pool.
-spec start_link(limits()) -> {ok, pid()}.
start_link(#{caller_quota := Quota, share := Share, cap := Cap, reply_bytes := ReplyBytes,
             reply_bytes_total := ReplyTotal} = Limits)
  when is_integer(Quota), Quota > 0, is_integer(Share), Share > 0, is_integer(Cap), Cap > 0,
       is_integer(ReplyBytes), ReplyBytes > 0, is_integer(ReplyTotal), ReplyTotal > 0 ->
    gen_server:start_link(?MODULE, Limits, []).

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
%% copies, at `NowMs'. `not_kept' when a byte bound leaves no room for it, and
%% `gone' when the admission no longer waits for this reply.
-spec store_reply(pid(), request(), term(), non_neg_integer(), integer()) -> kept | not_kept | gone.
store_reply(Admission, #{caller := <<_:256>> = Caller, request_id := <<_:128>> = RequestId,
                         request_hash := <<_:384>> = Hash}, Reply, Bytes, NowMs)
  when is_integer(Bytes), Bytes >= 0, is_integer(NowMs) ->
    gen_server:call(Admission, {store_reply, {Caller, RequestId}, Hash, Reply, Bytes, NowMs}).

%% @doc Remove the entries whose deadline plus 5 minutes passed before
%% `NowMs', freeing their places. Returns how many were removed.
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
    {Verdict, NewS} = in_window(deadline_verdict(Deadline, Now), Request, Share, Live),
    {reply, Verdict, refusal_counted(Verdict, source(Request), Now, NewS)};
handle_call({store_reply, Key, Hash, Reply, Bytes, Now}, _From, S) ->
    {Result, NewS} = stored(maps:find(Key, S#state.entries), Key, Hash, Reply, Bytes, S),
    {reply, Result, reply_counted(Result, source(Key), Now, NewS)};
handle_call(refusals, _From, #state{refusals = Report} = S) ->
    {reply, macula_refusal_report:counts(Report), S};
handle_call(refusal_sources, _From, #state{refusals = Report} = S) ->
    {reply, macula_refusal_report:last_sources(Report), S};
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

%% A refusal is counted under its kind alone, never a deadline's
%% milliseconds, so the kinds counted stay a fixed set.
refusal_counted({refused, Refusal}, Source, Now, S) -> count_kind(refusal_kind(Refusal), Source, Now, S);
refusal_counted(_AdmittedOrCopy, _Source, _Now, S)   -> S.

reply_counted(not_kept, Source, Now, S)     -> count_kind(reply_not_stored, Source, Now, S);
reply_counted(_KeptOrGone, _Source, _Now, S) -> S.

%% Who a refusal is about: the caller's node id prefix, and the procedure when
%% the request names one (a reply is counted by its entry's key, which does not).
source(#{caller := Caller} = Request) -> {caller_prefix(Caller), maps:get(procedure, Request, undefined)};
source({Caller, _RequestId})          -> {caller_prefix(Caller), undefined}.

caller_prefix(<<Prefix:8/binary, _/binary>>) -> binary:encode_hex(Prefix, lowercase).

refusal_kind({expired, _PastMs})        -> expired;
refusal_kind({not_yet_valid, _AheadMs}) -> not_yet_valid;
refusal_kind(Kind)                      -> Kind.

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

in_window(inside, #{caller := Caller, request_id := RequestId} = Request, Share, S) ->
    Key = {Caller, RequestId},
    seen(held(maps:find(Key, S#state.entries)), Key, Request, Share, S);
in_window(Refusal, _Request, _Share, S) ->
    {{refused, Refusal}, S}.

held({ok, Entry}) -> {held, Entry};
held(error)       -> absent.

seen({held, #entry{hash = Hash, reply = pending}}, _Key, #{request_hash := Hash}, _Share, S) ->
    {{copy, pending}, S};
seen({held, #entry{hash = Hash, reply = {reply, Reply, _Bytes}}}, _Key, #{request_hash := Hash}, _Share, S) ->
    {{copy, {reply, Reply}}, S};
seen({held, #entry{hash = Hash, reply = not_kept}}, _Key, #{request_hash := Hash}, _Share, S) ->
    {{refused, reply_not_kept}, S};
seen({held, _EntryWithAnotherHash}, _Key, _Request, _Share, S) ->
    {{refused, request_id_reused}, S};
seen(absent, Key, Request, Share, S) ->
    admitted(first_full(bounds(Key, Share, S)), Key, Request, Share, S).

%% In order: the caller's quota, the share, the set.
bounds({Caller, _RequestId}, Share, #state{limits = Limits, entries = Entries, callers = Callers,
                                           shares = Shares}) ->
    #{caller_quota := Quota, share := ShareMax, cap := Cap} = Limits,
    [{caller_quota, maps:get(Caller, Callers, 0) >= Quota},
     {share_full, maps:get(Share, Shares, 0) >= ShareMax},
     {admission_full, map_size(Entries) >= Cap}].

first_full(Bounds) ->
    first_full_bound([Refusal || {Refusal, true} <- Bounds]).

first_full_bound([Refusal | _]) -> {refused, Refusal};
first_full_bound([])            -> room.

admitted(room, {Caller, _RequestId} = Key, #{request_hash := Hash, deadline := Deadline}, Share, S) ->
    ExpiresAt = Deadline + ?KEPT_PAST_DEADLINE_MS,
    Entry = #entry{hash = Hash, expires_at = ExpiresAt, share = Share},
    {new, S#state{entries = maps:put(Key, Entry, S#state.entries),
                  expiry = gb_sets:add_element({ExpiresAt, Key}, S#state.expiry),
                  callers = bump(Caller, 1, S#state.callers),
                  shares = bump(Share, 1, S#state.shares)}};
admitted(Refused, _Key, _Request, _Share, S) ->
    {Refused, S}.

stored({ok, #entry{hash = Hash, reply = pending} = Entry}, {Caller, _RequestId} = Key, Hash, Reply, Bytes,
       #state{limits = #{reply_bytes := Max, reply_bytes_total := TotalMax}, reply_bytes = Held,
              reply_total = Total} = S) ->
    kept_or_not(maps:get(Caller, Held, 0) + Bytes =< Max andalso Total + Bytes =< TotalMax,
                Entry, Key, Reply, Bytes, S);
stored(_NotWaitingForThisReply, _Key, _Hash, _Reply, _Bytes, S) ->
    {gone, S}.

kept_or_not(true, Entry, {Caller, _RequestId} = Key, Reply, Bytes, S) ->
    {kept, S#state{entries = maps:put(Key, Entry#entry{reply = {reply, Reply, Bytes}}, S#state.entries),
                   reply_bytes = bump(Caller, Bytes, S#state.reply_bytes),
                   reply_total = S#state.reply_total + Bytes}};
kept_or_not(false, Entry, Key, _Reply, _Bytes, S) ->
    {not_kept, S#state{entries = maps:put(Key, Entry#entry{reply = not_kept}, S#state.entries)}}.

%% The entries past their deadline plus 5 minutes at `Now' leave, earliest
%% first, and how many did.
swept(Now, S) ->
    swept(Now, 0, S).

swept(Now, Removed, #state{expiry = Expiry} = S) ->
    next_expired(earliest(gb_sets:is_empty(Expiry), Expiry), Now, Removed, S).

earliest(true, _Expiry) -> none;
earliest(false, Expiry) -> gb_sets:smallest(Expiry).

next_expired({ExpiresAt, Key}, Now, Removed, S) when ExpiresAt < Now ->
    swept(Now, Removed + 1, forget(Key, S));
next_expired(_LiveOrNone, _Now, Removed, S) ->
    {Removed, S}.

%% Only a key the set holds is forgotten: a key in the expiry index but not
%% in the entries is a broken invariant, and crashes here rather than leave
%% `swept/3' taking the same smallest key for ever (Fable QA on #37).
forget({Caller, _RequestId} = Key, #state{entries = Entries} = S) ->
    released(maps:take(Key, Entries), Key, Caller, S).

released({#entry{share = Share, reply = Reply, expires_at = ExpiresAt}, Entries}, Key, Caller, S) ->
    S#state{entries = Entries,
            expiry = gb_sets:delete({ExpiresAt, Key}, S#state.expiry),
            callers = bump(Caller, -1, S#state.callers),
            shares = bump(Share, -1, S#state.shares),
            reply_bytes = bump(Caller, -reply_size(Reply), S#state.reply_bytes),
            reply_total = S#state.reply_total - reply_size(Reply)}.

reply_size({reply, _Reply, Bytes}) -> Bytes;
reply_size(_PendingOrNotKept)      -> 0.

%% A count that falls to zero leaves the map, so the maps hold only what is
%% counted.
bump(Key, Delta, Counts) ->
    counted(Key, maps:get(Key, Counts, 0) + Delta, Counts).

counted(Key, Count, Counts) when Count > 0 -> Counts#{Key => Count};
counted(Key, _Zero, Counts)                -> maps:remove(Key, Counts).
