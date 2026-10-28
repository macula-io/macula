%% @doc Who may deliver to a subscription (macula#147, WP2).
%%
%% A subscriber declares the sources it accepts facts from, and its own SDK
%% refuses every fact from anywhere else. This is self-protection, not a grant:
%% nothing is minted, published, delegated or administered, because no third
%% party ever relies on another party's declaration.
%%
%% The declaration is `members | {org, Org} | {nodes, Ids}', or `open' as an
%% explicit opt-out. There is no default: a subscription that names none is
%% refused at `subscribe'. The threat model and the note that decided what the
%% model left open are on https://github.com/macula-io/macula/issues/147
%% (sections 2, 5, 6 and 7 of the model).
%%
%% == Where the check runs ==
%%
%% At the pool, on the `publisher' the delivering link verified, once per
%% publication and per matching subscription -- two subscriptions on one topic
%% may declare different sources. `{nodes, Ids}' costs nothing: it compares the
%% verified publisher against the list, and a node id is not a credential, so it
%% never expires.
%%
%% `members' and `{org, Org}' are a record lookup, and a DHT read never runs in
%% the pool process: `lookup/3' runs in a worker, and the pool keeps only the
%% verdict cache and the in-flight set. A verdict is cached under
%% `{Realm, Source, Publisher}' and is never honoured past the record that
%% produced it.
%%
%% == The bounds (threat model 6.3) ==
%%
%% Node keys are free, so an attacker can present every fact from a fresh
%% publisher and cost the subscriber one lookup each. Every bound here fails
%% closed, and each is counted separately -- refused is a verdict, dropped is a
%% budget, held is waiting -- because an operator can tell an attack from a
%% misdeclaration only if the three never merge.
-module(macula_subscriber_sources).

-export([declared/3,
         bounds/1,
         cache_key/3, cache_get/3, cache_put/6,
         refresh_due/2,
         window_allows/4,
         admits/2,
         lookup/3]).

-export_type([source/0, record_source/0, verdict/0, bounds/0, cache/0, key/0, context/0, outcome/0]).

-type realm()     :: <<_:256>>.
-type node_id()   :: <<_:256>>.
-type source()    :: record_source() | {nodes, [node_id(), ...]} | open.
%% The sources that rest on a record, and so on a lookup: everything else a
%% declaration may be.
-type record_source() :: members | {org, binary()}.

%% What a lookup answered: the source admits the publisher until the record that
%% decided it expires, the source does not admit it (a refusal, with its reason,
%% which the counters keep), or the pool could not find out -- which is not a
%% verdict and is never cached.
-type verdict()   :: admit | {refuse, term()}.
-type outcome()   :: {admit, non_neg_integer()} | {refuse, term()} | {unavailable, term()}.

%% The verdict cache as the pool keeps it: one entry per realm, source and
%% publisher, with the verdict, the moment it stops being honoured, when it was
%% last used (which is what eviction goes by), and when it is due to be refreshed.
-type cache()     :: #{key() => {verdict(), non_neg_integer(), non_neg_integer(), non_neg_integer()}}.
-type key()       :: {realm(), record_source(), node_id()}.

%% What a worker needs to resolve one publisher: the pool whose DHT the records
%% are read through, the realm, the realm key it pinned, and the moment.
-type context()   :: #{pool := pid(), realm := realm(), realm_key := binary(), now := integer()}.

-type bounds()    :: #{max_publishers     := pos_integer(),
                       held_per_key       := pos_integer(),
                       lookups_per_window := pos_integer(),
                       window_ms          := pos_integer(),
                       cache_ttl_ms       := pos_integer(),
                       refresh_margin_ms  := non_neg_integer(),
                       refusal_ttl_ms     := pos_integer(),
                       lookup_timeout_ms  := pos_integer()}.

%% Per-realm distinct-publisher ceiling (§6.3.2): the cache of one realm holds
%% live verdicts for at most this many publishers, and its least recently verified
%% entry goes when it is full.
-define(DEFAULT_MAX_PUBLISHERS, 4096).
%% Facts held behind one in-flight lookup (the note, decision 4): the bound that
%% keeps a held fact from being an unbounded queue.
-define(DEFAULT_HELD_PER_KEY, 32).
%% Lookups this realm's own deliveries may trigger per window (§6.3.3). The bound
%% that carries a rotating-id attacker, and what bounds the cost of eviction.
-define(DEFAULT_LOOKUPS_PER_WINDOW, 300).
-define(DEFAULT_WINDOW_MS, 60_000).
%% A cached verdict lives no longer than this and never past its record, so a
%% withdrawal is seen within the cache's life rather than at the record's 24 h
%% ceiling (§6.2).
-define(DEFAULT_CACHE_TTL_MS, 300_000).
%% A verdict still in use is refreshed this close to its expiry, so a steady
%% publisher never waits on a lookup (§6.3).
-define(DEFAULT_REFRESH_MARGIN_MS, 60_000).
%% A refusal is cached too, this long: a non-member presenting facts in a loop
%% costs one lookup a minute, not one per fact. Short, because the source may
%% admit that publisher a moment later.
-define(DEFAULT_REFUSAL_TTL_MS, 30_000).
%% A lookup that neither answers nor dies releases the facts waiting behind it.
-define(DEFAULT_LOOKUP_TIMEOUT_MS, 15_000).

%%------------------------------------------------------------------
%% The declaration, resolved at subscribe
%%------------------------------------------------------------------

%% @doc Resolve a subscription's declared sources against the realm keys the pool
%% pinned, before the subscription exists. `{nodes, Ids}' needs no key; `members'
%% and `{org, Org}' rest on a record verified under the realm key, so in a realm
%% whose key the pool never pinned they are refused rather than served open.
%%
%% There is no default: `undefined' is refused, and a subscription whose sources
%% could not be resolved is never registered.
-spec declared(term(), realm(), #{realm() => binary()}) ->
        {ok, source()} | {error, term()}.
declared(undefined, _Realm, _RealmKeys) ->
    {error, {sources, required}};
declared(open, _Realm, _RealmKeys) ->
    {ok, open};
declared(members, Realm, RealmKeys) ->
    member_source(Realm, RealmKeys, members);
declared({org, Org}, Realm, RealmKeys) when is_binary(Org), byte_size(Org) > 0 ->
    member_source(Realm, RealmKeys, {org, Org});
declared({org, _NotAnOrg}, _Realm, _RealmKeys) ->
    {error, {source_class, {org, invalid}}};
declared({nodes, [_ | _] = NodeIds}, _Realm, _RealmKeys) ->
    node_ids_or_error(NodeIds);
declared({nodes, []}, _Realm, _RealmKeys) ->
    {error, {source_class, {nodes, empty}}};
declared({nodes, _NotAList}, _Realm, _RealmKeys) ->
    {error, {source_class, {nodes, not_a_list}}};
declared(Declaration, _Realm, _RealmKeys) ->
    {error, {source_class, {unknown, Declaration}}}.

%% A source that rests on a record cannot be enforced in a realm whose key this
%% pool never pinned.
member_source(Realm, RealmKeys, Source) ->
    case maps:is_key(Realm, RealmKeys) of
        true  -> {ok, Source};
        false -> {error, {source_class, no_realm_key}}
    end.

%% Walked by pattern, never with `lists:all/2' or an `is_list' guard: `[_ | _]'
%% matches an improper cons too, `lists:all/2' over it raises (inside the pool
%% process), and on OTP 28 `is_list/1' answers true for an improper list, so no
%% guard catches it. An improper list has no `[]' to end on and falls out as
%% `not_a_list'. The call side is walked the same way
%% (`macula_client:caller_policy/3').
node_ids_or_error(NodeIds) ->
    node_ids_formed(NodeIds, NodeIds).

node_ids_formed([], NodeIds) ->
    {ok, {nodes, NodeIds}};
node_ids_formed([Id | Rest], NodeIds) ->
    node_id_formed(Id, Rest, NodeIds);
node_ids_formed(_NotAList, _NodeIds) ->
    {error, {source_class, {nodes, not_a_list}}}.

node_id_formed(Id, Rest, NodeIds) when is_binary(Id), byte_size(Id) =:= 32 ->
    node_ids_formed(Rest, NodeIds);
node_id_formed(_Id, _Rest, _NodeIds) ->
    {error, {source_class, {nodes, not_node_ids}}}.

%%------------------------------------------------------------------
%% Bounds: parsed once, at connect
%%------------------------------------------------------------------

%% @doc The check's bounds, from the pool's `subscribe_sources' option. An option
%% that names a bound this module does not know, or a bound that is not a positive
%% integer, does not start the pool: a check that silently ran on the wrong bound
%% looks exactly like one that ran on the right one.
-spec bounds(map()) -> {ok, bounds()} | {error, term()}.
bounds(Opts) when is_map(Opts) ->
    given_bounds(maps:get(subscribe_sources, Opts, #{}));
bounds(_NotAMap) ->
    {error, {sources, {subscribe_sources, invalid}}}.

given_bounds(Given) when is_map(Given) ->
    Defaults = #{max_publishers     => ?DEFAULT_MAX_PUBLISHERS,
                 held_per_key       => ?DEFAULT_HELD_PER_KEY,
                 lookups_per_window => ?DEFAULT_LOOKUPS_PER_WINDOW,
                 window_ms          => ?DEFAULT_WINDOW_MS,
                 cache_ttl_ms       => ?DEFAULT_CACHE_TTL_MS,
                 refresh_margin_ms  => ?DEFAULT_REFRESH_MARGIN_MS,
                 refusal_ttl_ms     => ?DEFAULT_REFUSAL_TTL_MS,
                 lookup_timeout_ms  => ?DEFAULT_LOOKUP_TIMEOUT_MS},
    known_bounds(maps:keys(Given), Defaults, Given);
given_bounds(_NotAMap) ->
    {error, {sources, {subscribe_sources, invalid}}}.

known_bounds([], Defaults, Given) ->
    bounded_values(maps:to_list(maps:merge(Defaults, Given)), maps:merge(Defaults, Given));
known_bounds([Key | Rest], Defaults, Given) ->
    case maps:is_key(Key, Defaults) of
        true  -> known_bounds(Rest, Defaults, Given);
        false -> {error, {sources, {subscribe_sources, {unknown, Key}}}}
    end.

bounded_values([], Bounds) ->
    {ok, Bounds};
bounded_values([{_Key, Value} | Rest], Bounds) when is_integer(Value), Value > 0 ->
    bounded_values(Rest, Bounds);
bounded_values([{Key, Value} | _Rest], _Bounds) ->
    {error, {sources, {subscribe_sources, {Key, Value}}}}.

%%------------------------------------------------------------------
%% The verdict cache
%%------------------------------------------------------------------

%% @doc The cache key of a verdict: one realm, one source, one publisher. A
%% verdict for one source is never a verdict for another.
-spec cache_key(realm(), record_source(), node_id()) -> key().
cache_key(Realm, Source, Publisher) ->
    {Realm, Source, Publisher}.

%% @doc The verdict this key holds at `Now'. An entry past its own expiry is a
%% miss and is dropped, so a record that has expired is never honoured. A hit is
%% marked as used, which is what eviction and the refresh go by.
-spec cache_get(cache(), key(), integer()) ->
        {ok, verdict(), non_neg_integer(), cache()} | {miss, cache()}.
cache_get(Cache, Key, Now) ->
    held_verdict(maps:find(Key, Cache), Cache, Key, Now).

%% The refresh moment comes back with the verdict: it is due once, per life of
%% the entry, however many facts arrive after it.
held_verdict({ok, {Verdict, ExpiresAt, _Used, RefreshAt}}, Cache, Key, Now) when ExpiresAt > Now ->
    {ok, Verdict, RefreshAt, Cache#{Key => {Verdict, ExpiresAt, Now, RefreshAt}}};
held_verdict({ok, _Expired}, Cache, Key, _Now) ->
    {miss, maps:remove(Key, Cache)};
held_verdict(error, Cache, _Key, _Now) ->
    {miss, Cache}.

%% @doc Record a verdict for `Key', honoured until `Until', but never past the
%% cache's own TTL and never past the record it came from. Expired entries of the
%% realm are dropped first, and if the realm is still at its cap its least
%% recently verified entry goes -- so the cap bounds live memory, and the lookup
%% window bounds how often that can happen.
-spec cache_put(cache(), key(), verdict(), non_neg_integer() | undefined, bounds(), integer()) ->
        cache().
cache_put(Cache, {Realm, _Source, _Publisher} = Key, Verdict, Until, Bounds, Now) ->
    ExpiresAt = cache_until(Until, Verdict, Bounds, Now),
    Kept = maps:filter(fun ({R, _S, _P}, {_V, EntryUntil, _Used, _RefreshAt}) ->
                               R =/= Realm orelse EntryUntil > Now
                       end, Cache),
    RefreshAt = refresh_at(ExpiresAt, Now, maps:get(refresh_margin_ms, Bounds)),
    within_cap(Kept#{Key => {Verdict, ExpiresAt, Now, RefreshAt}}, Realm, maps:get(max_publishers, Bounds)).

%% Refresh a margin before the entry expires, but never in the first half of its
%% life: a record living under the margin would otherwise be looked up once per
%% fact instead of once per life.
refresh_at(ExpiresAt, Now, Margin) ->
    Applied = min(Margin, (ExpiresAt - Now) div 2),
    ExpiresAt - Applied.

%% A verdict never outlives the cache TTL, and an admission never outlives the
%% record that admitted it. Only a refusal reaches the second clause without an
%% expiry: a refusal usually rests on no record at all.
cache_until(Until, admit, #{cache_ttl_ms := Ttl}, Now) when is_integer(Until) ->
    min(Until, Now + Ttl);
cache_until(_Until, admit, #{cache_ttl_ms := Ttl}, Now) ->
    Now + Ttl;
cache_until(_Until, {refuse, _Reason}, #{refusal_ttl_ms := Ttl}, Now) ->
    Now + Ttl.

within_cap(Cache, Realm, Max) ->
    case realm_count(Cache, Realm) =< Max of
        true  -> Cache;
        false -> within_cap(maps:remove(lru(Cache, Realm), Cache), Realm, Max)
    end.

realm_count(Cache, Realm) ->
    maps:fold(fun({R, _S, _P}, _Entry, Count) when R =:= Realm -> Count + 1;
                 (_OtherRealm, _Entry, Count) -> Count
              end, 0, Cache).

%% The realm's least recently verified entry: a rotating-id attacker pushes out
%% its own verdicts rather than a steady publisher's.
lru(Cache, Realm) ->
    lru_entry(maps:to_list(Cache), Realm, none).

lru_entry([], _Realm, none) ->
    none;
lru_entry([], _Realm, {Key, _Used}) ->
    Key;
lru_entry([{{R, _S, _P} = Key, {_V, _Until, Used, _RefreshAt}} | Rest], Realm, Best) when R =:= Realm ->
    lru_entry(Rest, Realm, oldest(Key, Used, Best));
lru_entry([_OtherRealm | Rest], Realm, Best) ->
    lru_entry(Rest, Realm, Best).

oldest(Key, Used, none) ->
    {Key, Used};
oldest(Key, Used, {_OtherKey, Oldest}) when Used < Oldest ->
    {Key, Used};
oldest(_Key, _Used, Best) ->
    Best.

%% @doc Whether a cached verdict still in use is due to be refreshed before it
%% expires, so a steady publisher never waits on a lookup.
-spec refresh_due(non_neg_integer(), integer()) -> boolean().
refresh_due(RefreshAt, Now) ->
    Now >= RefreshAt.

%%------------------------------------------------------------------
%% The lookup window (§6.3.3)
%%------------------------------------------------------------------

%% @doc Count one lookup against `Realm's window. Over the cap the fact is dropped
%% and counted without a lookup; the window resets on its own, so this is a rate
%% and not a lifetime budget.
-spec window_allows(#{realm() => {integer(), pos_integer()}}, realm(), integer(), bounds()) ->
        {ok, #{realm() => {integer(), pos_integer()}}} | over.
window_allows(Windows, Realm, Now, #{window_ms := WindowMs, lookups_per_window := Max}) ->
    counted_window(maps:get(Realm, Windows, none), Windows, Realm, Now, WindowMs, Max).

counted_window(none, Windows, Realm, Now, _WindowMs, _Max) ->
    {ok, Windows#{Realm => {Now, 1}}};
counted_window({StartedAt, _Count}, Windows, Realm, Now, WindowMs, _Max)
  when Now - StartedAt >= WindowMs ->
    {ok, Windows#{Realm => {Now, 1}}};
counted_window({_StartedAt, Count}, _Windows, _Realm, _Now, _WindowMs, Max) when Count >= Max ->
    over;
counted_window({StartedAt, Count}, Windows, Realm, _Now, _WindowMs, _Max) ->
    {ok, Windows#{Realm => {StartedAt, Count + 1}}}.

%%------------------------------------------------------------------
%% The named-nodes source, decided without a lookup
%%------------------------------------------------------------------

%% @doc Whether a `{nodes, Ids}' source admits `Publisher'. No lookup, no cache,
%% no expiry: a node id is not a credential, so this answer changes only when a
%% release changes the list (§6.2).
-spec admits(source(), node_id()) -> boolean().
admits({nodes, NodeIds}, Publisher) ->
    lists:member(Publisher, NodeIds);
admits(_NotANamedSource, _Publisher) ->
    false.

%%------------------------------------------------------------------
%% The lookup, run in a worker
%%------------------------------------------------------------------

%% @doc Resolve a publisher against a source that rests on a record. Runs in the
%% pool's worker, never in the pool: both reads are DHT calls, and a pool that
%% waited on one would stall delivery for every topic on it.
%%
%% `members' is the publisher's realm member endorsement, read from its slot under
%% the key the pool pinned -- the member itself, never a delegate, because
%% `check_member/3' requires `member_node' to be the publisher. `{org, Org}' is
%% `macula:org_member/4,5', which verifies the org directory and the publisher's
%% delegation and answers the earlier expiry of the two.
-spec lookup(record_source(), context(), node_id()) -> outcome().
lookup(members, #{pool := Pool, realm := Realm, realm_key := RealmKey, now := Now}, Publisher) ->
    Profile = configured_profile(),
    Trust = #{profile => Profile,
              realm => Realm,
              realm_key_id => macula_node_keys:key_id(RealmKey, Profile)},
    slot_verdict(Pool, macula_record:realm_member_endorsement_key(Realm, Publisher), Trust, Publisher, Now);
lookup({org, Org}, #{pool := Pool, realm := Realm}, Publisher) ->
    org_verdict(guarded(fun() -> macula:org_member(Pool, Realm, Org, Publisher) end)).

org_verdict({ok, {ok, ExpiresAt}}) ->
    {admit, ExpiresAt};
org_verdict({ok, {error, {not_a_member, Why}}}) ->
    {refuse, {not_a_member, Why}};
org_verdict({ok, {error, {unavailable, Why}}}) ->
    {unavailable, Why};
org_verdict({unavailable, _} = Unavailable) ->
    Unavailable.

slot_verdict(Pool, Slot, Trust, Publisher, Now) ->
    slot_read(guarded(fun() -> macula:find_records(Pool, Slot) end), Trust, Publisher, Now).

slot_read({ok, {ok, Records}}, Trust, Publisher, Now) when is_list(Records) ->
    {Outcome, Until, _Stats} =
        macula_hyparview_endorsement:slot_verdict([macula_record:encode(R) || R <- Records],
                                                 Trust, Publisher, Now),
    endorsement_verdict(Outcome, Until);
slot_read({ok, {error, Reason}}, _Trust, _Publisher, _Now) ->
    {unavailable, {slot, Reason}};
slot_read({unavailable, _} = Unavailable, _Trust, _Publisher, _Now) ->
    Unavailable.

endorsement_verdict({ok, _Roles}, Until) ->
    {admit, Until};
endorsement_verdict({error, Why}, _Until) ->
    {refuse, Why}.

%% A read that exits is "the pool could not find out", never a refusal the
%% publisher did not earn and never the subscriber's process dying.
guarded(Read) ->
    try Read() of
        Answer -> {ok, Answer}
    catch
        exit:Reason  -> {unavailable, {lookup_exited, Reason}};
        error:Reason -> {unavailable, {lookup_failed, Reason}}
    end.

configured_profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
