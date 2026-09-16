%% @doc A caller-side bucket of record bytes per connection, so a node that
%% writes many records stays under the station's STORE allowance (D28, 3.5):
%% 16 MiB at once, refilled at 1 MiB per second. `await/2' sleeps in the
%% calling process until the bucket allows `Bytes' and records them, so a
%% put path that paces here never runs into a station's `stored` 0 for the
%% byte allowance. The bucket is recomputed lazily on each check, like
%% `macula_dht_budget`'s, so an idle connection's bucket never grows past
%% its capacity and there is no timer per connection.
%%
%% A record is at most 256 KiB, so one await never sleeps more than about
%% 15 seconds past a full bucket, and an honest renewer's traffic (small
%% records renewed on their own cadence) never waits at all.
-module(macula_store_pacer).

-export([await/2, stats/1]).

-define(TABLE, macula_store_pacer).
-define(BUCKET_BYTES, (16 * 1024 * 1024)).
-define(REFILL_PER_SEC, (1 * 1024 * 1024)).

-export_type([connection/0]).

-type connection() :: pid() | term().

%% @doc Sleep in the calling process until `Bytes' fit the connection's
%% bucket, then record them. `Bytes' is the record's wire size.
-spec await(connection(), non_neg_integer()) -> ok.
await(Conn, Bytes) when is_integer(Bytes), Bytes >= 0 ->
    Now = now_ms(),
    await(Now, ensured(Conn, Now), Conn, Bytes).

await(Now, #{bucket := Bucket, refilled_at := At} = Entry, Conn, Bytes) ->
    wait_until(Bucket >= Bytes, Now, Bucket, At, Entry, Conn, Bytes).

wait_until(true, Now, Bucket, _At, Entry, Conn, Bytes) ->
    put_bucket(Conn, Entry#{bucket := Bucket - Bytes, refilled_at := Now});
wait_until(false, _Now, Bucket, At, Entry, Conn, Bytes) ->
    timer:sleep(max(1, ((Bytes - Bucket) * 1000) div ?REFILL_PER_SEC)),
    Next = now_ms(),
    Refilled = min(?BUCKET_BYTES, Bucket + (?REFILL_PER_SEC * (Next - At)) div 1000),
    await(Next, Entry#{bucket := Refilled, refilled_at := Next}, Conn, Bytes).

%% @doc The connection's current bucket, for tests and diagnostics.
-spec stats(connection()) -> #{bucket := non_neg_integer()}.
stats(Conn) ->
    #{bucket => maps:get(bucket, ensured(Conn, now_ms()))}.

ensured(Conn, Now) ->
    maybe_create(),
    case ets:lookup(?TABLE, Conn) of
        [{_, #{bucket := Bucket, refilled_at := At} = Entry}] ->
            Refilled = min(?BUCKET_BYTES, Bucket + (?REFILL_PER_SEC * (Now - At)) div 1000),
            Entry#{bucket := Refilled, refilled_at := Now};
        [] ->
            #{bucket => ?BUCKET_BYTES, refilled_at => Now}
    end.

put_bucket(Conn, Entry) ->
    ets:insert(?TABLE, {Conn, Entry}),
    ok.

maybe_create() ->
    case ets:info(?TABLE) of
        undefined ->
            ?TABLE = ets:new(?TABLE, [named_table, public, set,
                                      {write_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

now_ms() ->
    erlang:system_time(millisecond).
