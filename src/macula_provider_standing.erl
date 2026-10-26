%% @doc The node's last reading of each provider's delegation slot, shared by every stream that follows that provider
%% (macula#120): a node holding N streams to one provider under one org key reads the slot once per re-check, not N
%% times.
%%
%% A follower asks read/3 with its key, {Provider, OrgKeyId}, the read it would make and its re-check interval. A
%% reading younger than half the interval is answered as it is, so what a follower acts on is never older than one
%% interval at its next ask: the withdrawal reaches it within one re-check plus a lookup. A missing or older one is
%% claimed by the first asker, atomically, and that asker makes the one read and stores its answer; every other asker
%% meanwhile waits for that answer rather than taking the last one, so no follower learns of a withdrawal later than
%% the one that read it. A read that raises is stored as lookup_failed, and an asker that waits longer than a lookup can
%% take answers lookup_failed: neither is evidence of revocation. A claim older than that wait, its claimant gone
%% mid-read, is claimed again by the next asker. Callers read and write the table themselves; this process owns
%% it and sweeps readings older than ten minutes. Without the table, between an owner's exit and its restart, read/3
%% reads directly.
-module(macula_provider_standing).
-behaviour(gen_server).

-export([start_link/0, read/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
%% Started from macula_root's child spec, which xref does not count as a call.
-ignore_xref([{macula_provider_standing, start_link, 0}]).

-define(TABLE, ?MODULE).
%% A reading older than this is swept.
-define(KEEP_MS, 600_000).
-define(SWEEP_MS, 60_000).
%% An asker waits this long at most for another asker's read, longer than a lookup's own deadline.
-define(AWAIT_MS, 15_000).
-define(AWAIT_POLL_MS, 20).

-type key() :: {<<_:256>>, <<_:256>>}.
-type standing() :: ok | {error, term()}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc The standing of the provider and org key `Key', from a reading younger than `MaxAgeMs', or from `Read()'.
-spec read(key(), fun(() -> standing()), pos_integer()) -> standing().
read(Key, Read, MaxAgeMs) when is_function(Read, 0), is_integer(MaxAgeMs), MaxAgeMs > 0 ->
    held(ets:whereis(?TABLE), Key, Read, MaxAgeMs).

held(undefined, _Key, Read, _MaxAgeMs) ->
    Read();
held(Table, Key, Read, MaxAgeMs) ->
    Now = now_ms(),
    reading(ets:lookup(Table, Key), Table, Key, Read, MaxAgeMs, Now).

%% A row is {Key, ReadAt, Standing}, where Standing is `reading' while its claimant reads, ReadAt then being the claim's
%% time.
reading([{Key, ClaimedAt, reading}], Table, Key, _Read, _MaxAgeMs, Now) when Now - ClaimedAt < ?AWAIT_MS ->
    awaited(Table, Key, Now + ?AWAIT_MS);
reading([{Key, ReadAt, Standing}], _Table, Key, _Read, MaxAgeMs, Now)
  when Standing =/= reading, Now - ReadAt < MaxAgeMs div 2 ->
    Standing;
reading([{Key, ReadAt, Standing}], Table, Key, Read, _MaxAgeMs, Now) ->
    claimed(ets:select_replace(Table, [{{Key, ReadAt, Standing}, [], [{{{const, Key}, Now, reading}}]}]), Table, Key,
            Read, Now);
reading([], Table, Key, Read, _MaxAgeMs, Now) ->
    claimed(boolean_to_count(ets:insert_new(Table, {Key, Now, reading})), Table, Key, Read, Now).

boolean_to_count(true) -> 1;
boolean_to_count(false) -> 0.

%% The asker whose claim landed owns the read; another asker got there first, and this one waits for its answer.
claimed(1, Table, Key, Read, _Now) -> stored(Table, Key, read_once(Read));
claimed(0, Table, Key, _Read, Now) -> awaited(Table, Key, Now + ?AWAIT_MS).

read_once(Read) ->
    try Read()
    catch _:_ -> {error, lookup_failed}
    end.

stored(Table, Key, Standing) ->
    true = ets:insert(Table, {Key, now_ms(), Standing}),
    Standing.

awaited(Table, Key, Deadline) ->
    awaited(ets:lookup(Table, Key), Table, Key, Deadline, now_ms()).

awaited([{Key, _ReadAt, reading}], Table, Key, Deadline, Now) when Now < Deadline ->
    timer:sleep(?AWAIT_POLL_MS),
    awaited(Table, Key, Deadline);
awaited([{Key, _ReadAt, reading}], _Table, Key, _Deadline, _Now) ->
    {error, lookup_failed};
awaited([{Key, _ReadAt, Standing}], _Table, Key, _Deadline, _Now) ->
    Standing;
awaited([], _Table, _Key, _Deadline, _Now) ->
    {error, lookup_failed}.

init([]) ->
    Table = ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    _ = erlang:send_after(?SWEEP_MS, self(), sweep),
    {ok, #{table => Table}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(sweep, #{table := Table} = State) ->
    _ = ets:select_delete(Table, [{{'_', '$1', '_'}, [{'<', '$1', now_ms() - ?KEEP_MS}], [true]}]),
    _ = erlang:send_after(?SWEEP_MS, self(), sweep),
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.

now_ms() ->
    erlang:system_time(millisecond).
