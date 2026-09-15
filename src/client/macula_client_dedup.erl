%% @private
%% @doc Inbound publication dedup table for `macula_client'.
%%
%% The pool subscribes the same `(Realm, Topic)' against several station
%% links, so one publication can reach the pool over more than one link.
%% A link verifies each publication before it hands the event to the pool,
%% and the pool delivers each publication at most once: the table keys on
%% the publication's hash, the SHA-384 of its `tbs', and keeps each entry
%% until the publication expires, after which every verifier refuses it
%% (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, Publications). Expiry is
%% judged when the check runs, so a copy checked after its publication
%% expired is never new, even once a sweep has forgotten an earlier copy.
%%
%% The table is a plain `ets' set with `public' access; the pool owns it
%% and it dies with the pool. `sweep/2' drops the entries whose
%% publication has expired. A publication's `ttl_ms' is at most one hour
%% and its `published_at' at most 5 minutes ahead, so no entry is kept
%% longer than 70 minutes after it is recorded.
-module(macula_client_dedup).
-export([new/0, check/4, sweep/2]).

-export_type([table/0]).
-type table() :: ets:tid().

%% @doc Create a fresh dedup table. The pool owns its lifetime; the
%% table dies with the pool process.
-spec new() -> table().
new() ->
    ets:new(?MODULE, [set, public, {read_concurrency, true}]).

%% @doc Check a publication's hash at `NowMs'. Returns `expired' when the
%% publication's `ExpiresAt' is already past, and records nothing.
%% Otherwise returns `new' on the first sighting of the hash and
%% `duplicate' on every later one, until a sweep drops the entry. Both
%% times are milliseconds of wall-clock time.
%%
%% Uses `ets:insert_new/2' for atomicity: concurrent callers can race
%% without locks.
-spec check(table(), <<_:384>>, non_neg_integer(), integer()) ->
    new | duplicate | expired.
check(Tab, PublicationHash, ExpiresAt, NowMs)
  when is_binary(PublicationHash), byte_size(PublicationHash) =:= 48,
       is_integer(ExpiresAt), ExpiresAt >= 0, is_integer(NowMs) ->
    sighting(NowMs > ExpiresAt, Tab, PublicationHash, ExpiresAt).

sighting(true, _Tab, _PublicationHash, _ExpiresAt) ->
    expired;
sighting(false, Tab, PublicationHash, ExpiresAt) ->
    inserted(ets:insert_new(Tab, {PublicationHash, ExpiresAt})).

inserted(true)  -> new;
inserted(false) -> duplicate.

%% @doc Drop the entries whose publication expired before `NowMs', in
%% milliseconds of wall-clock time. Returns the number of entries removed.
%% Cheap to call frequently: ETS `select_delete' is in-place.
-spec sweep(table(), integer()) -> non_neg_integer().
sweep(Tab, NowMs) when is_integer(NowMs) ->
    ets:select_delete(Tab, [{{'_', '$1'}, [{'<', '$1', NowMs}], [true]}]).
