%% @private
%% @doc Inbound EVENT dedup table for `macula_client'.
%%
%% The pool subscribes the same `(Realm, Topic)' against multiple
%% station links. When a publisher's frame plumtrees through the
%% station overlay, several stations may relay the same EVENT to the
%% pool. Dedup keys frames by `(Realm, Publisher, Seq)' — the tuple
%% the publisher stamps onto every PUBLISH frame — and accepts the
%% first sighting only. Only an event whose publisher signature verified
%% uses that key: `check_unverified/5' keys any other event on the same
%% tuple plus a digest of its topic and payload, in a separate key space,
%% so it only ever matches an identical copy of itself.
%%
%% The table is a plain `ets' set with `public' access; the pool
%% owns it and drops it on `terminate/2'. Periodic `sweep/2' prunes
%% entries older than the configured window so memory stays bounded
%% under steady traffic.
%%
%% **Assumption:** publishers maintain a monotonic per-link `seq'
%% across their lifetime. A publisher restart resets the counter and
%% can theoretically collide with a not-yet-swept entry — Phase 4
%% will introduce a `publisher_session_id' to harden this.
-module(macula_client_dedup).
-export([new/0, check/4, check_unverified/5, sweep/2]).

-export_type([table/0]).
-type table() :: ets:tid().

%% @doc Create a fresh dedup table. Pool owns the lifetime; the
%% table dies with the pool process.
-spec new() -> table().
new() ->
    ets:new(?MODULE, [set, public, {read_concurrency, true}]).

%% @doc Insert-if-absent. Returns `new' on first sighting of
%% `(Realm, Publisher, Seq)', `duplicate' on every subsequent. For an
%% event whose publisher signature verified.
%%
%% Uses `ets:insert_new/2' for atomicity — concurrent callers can
%% race without locks.
-spec check(table(), <<_:256>>, <<_:256>>, non_neg_integer()) ->
    new | duplicate.
check(Tab, Realm, Publisher, Seq)
  when is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Publisher), byte_size(Publisher) =:= 32,
       is_integer(Seq), Seq >= 0 ->
    insert_new(Tab, {Realm, Publisher, Seq}).

%% @doc As `check/4', for an event whose publisher signature did not
%% verify or that carried none. `Digest' covers the event's topic and
%% payload. The key lives apart from `check/4''s, so such an event never
%% matches a verified event's key and matches only an identical copy of
%% itself.
-spec check_unverified(table(), <<_:256>>, <<_:256>>, non_neg_integer(),
                       binary()) -> new | duplicate.
check_unverified(Tab, Realm, Publisher, Seq, Digest)
  when is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Publisher), byte_size(Publisher) =:= 32,
       is_integer(Seq), Seq >= 0, is_binary(Digest) ->
    insert_new(Tab, {unverified, Realm, Publisher, Seq, Digest}).

insert_new(Tab, Key) ->
    Now = erlang:system_time(millisecond),
    case ets:insert_new(Tab, {Key, Now}) of
        true  -> new;
        false -> duplicate
    end.

%% @doc Drop entries older than `WindowMs' (relative to wall clock).
%% Returns the number of entries removed. Cheap to call frequently —
%% ETS `select_delete' is in-place.
-spec sweep(table(), non_neg_integer()) -> non_neg_integer().
sweep(Tab, WindowMs) when is_integer(WindowMs), WindowMs >= 0 ->
    Cutoff = erlang:system_time(millisecond) - WindowMs,
    MS = [{{'_', '$1'}, [{'<', '$1', Cutoff}], [true]}],
    ets:select_delete(Tab, MS).
