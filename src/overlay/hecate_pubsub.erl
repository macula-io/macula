%% @doc Realm-scoped PubSub state + dispatch (Part 6 §6).
%%
%% Holds the topic-to-subscriber index for one realm and converts
%% incoming SUBSCRIBE / UNSUBSCRIBE / EVENT frames into local
%% state mutations + delivery instructions. The wire layer that
%% actually transmits frames lives elsewhere — typically
%% `hecate_plumtree' for intra-realm fan-out.
%%
%% == Pipeline ==
%%
%% <ul>
%%   <li><strong>Local subscribe</strong> — `subscribe/3' adds the
%%       subscriber to the topic's set. The wrapper builds a
%%       SUBSCRIBE frame for upstream propagation if needed.</li>
%%   <li><strong>Local publish</strong> — `build_event/3' takes a
%%       PUBLISH spec and produces a signed EVENT frame the
%%       wrapper hands to Plumtree for fan-out. The publisher
%%       signs the EVENT once; intermediate hops do NOT re-sign,
%%       so every subscriber can verify authenticity end-to-end
%%       (Part 6 §6.4).</li>
%%   <li><strong>Receive EVENT</strong> — `deliver_event/2'
%%       returns the list of local subscribers whose subscription
%%       matches the event's topic + realm. The wrapper notifies
%%       each via the application channel.</li>
%%   <li><strong>Receive SUBSCRIBE / UNSUBSCRIBE</strong> —
%%       `process/3' updates local state.</li>
%% </ul>
%%
%% State is per realm: an instance handles one realm's topics
%% only. Cross-realm leakage is impossible — the realm is
%% baked into the state and every dispatch checks it.
%%
%% == Wildcard subscriptions (2026-08-29, station-local only) ==
%%
%% `subscribe/3' with a topic containing a literal `*' segment (see
%% `macula_topic_pattern:matches/2') registers a PATTERN instead of an
%% exact topic — kept in a SEPARATE `patterns' map, not mixed into
%% `subscriptions', so the common case (no wildcard subscribers in this
%% realm) pays zero extra cost on delivery: `subscribers/2' only scans
%% `patterns' when `map_size(patterns) > 0'.
%%
%% Deliberately NOT propagated cross-station: `topics/1' returns
%% `subscriptions''s keys only. `macula_station_peering_router' treats
%% every entry in `topics/1' as local interest worth re-subscribing on
%% every peer and folding into the Bloom-gossip summary — a Bloom filter
%% tests exact-string membership, so gossiping a raw `*'-bearing string
%% would be meaningless (it can only ever match itself, never the
%% concrete topics it was meant to stand in for) and would pollute the
%% gossip layer for no benefit. A wildcard subscriber therefore only
%% ever receives a publish that reaches THIS realm instance directly —
%% same station as the publisher, or already fanned here via the
%% ordinary (exact-topic) gossip/relay path. Mesh-wide wildcard
%% subscription (matching cross-station, not just locally) is a
%% separate, bigger piece of work — see
%% macula-station/plans/PLAN_ORG_SCOPED_DISPATCH_AND_WILDCARD_DISCOVERY.md,
%% slice 5.
%%
%% Reference: plans/PLAN_MACULA_V2_PART6_PROTOCOL.md §6;
%% plans/PLAN_PHASE_5_BREAKDOWN.md Session 5.5.
-module(hecate_pubsub).

-export([
    new/1,
    realm/1,
    subscribe/3,
    unsubscribe/3,
    purge_subscriber/2,
    is_subscribed/3,
    subscribers/2,
    topics/1,
    topic_count/1,
    subscriber_count/1,
    deliver_event/2,
    build_event/3,
    process/3
]).

-export_type([state/0, topic/0, subscriber/0]).

-type topic()      :: binary().
-type subscriber() :: macula_identity:pubkey().

-type state() :: #{
    realm         := <<_:256>>,
    subscriptions := #{topic() => sets:set(subscriber())},
    patterns      := #{topic() => sets:set(subscriber())}
}.

%%=====================================================================
%% Construction
%%=====================================================================

-spec new(<<_:256>>) -> state().
new(<<_:256>> = Realm) ->
    #{realm => Realm, subscriptions => #{}, patterns => #{}}.

%%=====================================================================
%% Inspection
%%=====================================================================

realm(#{realm := R}) -> R.

%% @doc Every subscriber that would receive a publish to `Topic' — exact
%% subscribers plus, when this realm has any registered, every wildcard
%% pattern that matches `Topic'. `Topic' itself is always concrete here
%% (a publish never carries a wildcard); a caller passing a `*'-bearing
%% string gets whatever literal entry (if any) happens to exist under
%% that exact string in `subscriptions' — patterns match AGAINST
%% concrete topics, not against each other.
-spec subscribers(state(), topic()) -> [subscriber()].
subscribers(#{subscriptions := S, patterns := P}, Topic) ->
    Exact = maps:get(Topic, S, sets:new()),
    sets:to_list(sets:union(Exact, pattern_subscribers(P, Topic))).

%% Fast path: the overwhelmingly common case is a realm with no
%% wildcard subscribers at all, and that case must cost nothing beyond
%% this one `map_size/1' check — see moduledoc.
pattern_subscribers(P, _Topic) when map_size(P) =:= 0 ->
    sets:new();
pattern_subscribers(P, Topic) ->
    TopicSegments = split_segments(Topic),
    maps:fold(fun(Pattern, Set, Acc) ->
                 merge_if_matches(split_segments(Pattern), TopicSegments, Set, Acc)
              end, sets:new(), P).

merge_if_matches(PatternSegments, TopicSegments, Set, Acc) ->
    union_when(macula_topic_pattern:matches(PatternSegments, TopicSegments), Set, Acc).

union_when(true,  Set, Acc) -> sets:union(Set, Acc);
union_when(false, _Set, Acc) -> Acc.

split_segments(Topic) -> binary:split(Topic, <<"/">>, [global]).

%% A topic containing a literal `*' segment is a pattern (see
%% `macula_topic_pattern''s own definition — a WHOLE segment, not a
%% substring, so e.g. `"foo*bar"' is an ordinary literal topic, not a
%% pattern).
-spec is_pattern(topic()) -> boolean().
is_pattern(Topic) -> lists:member(<<"*">>, split_segments(Topic)).

%% @doc Whether `Sub' is registered under the LITERAL string `Topic' —
%% exact or pattern, whichever map it actually lives in. Distinct from
%% "would `Sub' receive a publish to `Topic''" (that question is
%% `subscribers/2'): a subscriber registered under a pattern is not
%% `is_subscribed' for one of the concrete topics that pattern matches,
%% only for the pattern string itself.
-spec is_subscribed(state(), topic(), subscriber()) -> boolean().
is_subscribed(#{subscriptions := S, patterns := P}, Topic, Sub) ->
    map_has_sub(route_map(is_pattern(Topic), S, P), Topic, Sub).

map_has_sub(Map, Topic, Sub) ->
    case maps:find(Topic, Map) of
        {ok, Set} -> sets:is_element(Sub, Set);
        error     -> false
    end.

route_map(true,  _S, P) -> P;
route_map(false, S, _P) -> S.

%% Exact topics ONLY — deliberately excludes `patterns'. See moduledoc:
%% this feeds cross-station gossip re-subscription, where a raw
%% `*'-bearing string would be meaningless.
-spec topics(state()) -> [topic()].
topics(#{subscriptions := S}) -> maps:keys(S).

%% Observability counts include patterns — real, load-bearing state,
%% just not gossip-propagated. See `topics/1' for what IS propagated.
-spec topic_count(state()) -> non_neg_integer().
topic_count(#{subscriptions := S, patterns := P}) ->
    maps:size(S) + maps:size(P).

-spec subscriber_count(state()) -> non_neg_integer().
subscriber_count(#{subscriptions := S, patterns := P}) ->
    count_all(S) + count_all(P).

count_all(Map) ->
    maps:fold(fun(_, Set, Acc) -> Acc + sets:size(Set) end, 0, Map).

%%=====================================================================
%% Local subscribe / unsubscribe
%%=====================================================================

-spec subscribe(state(), topic(), subscriber()) -> state().
subscribe(State, Topic, Sub)
  when is_binary(Topic), is_binary(Sub), byte_size(Sub) =:= 32 ->
    route_subscribe(is_pattern(Topic), State, Topic, Sub).

route_subscribe(true, #{patterns := P} = State, Topic, Sub) ->
    State#{patterns := add_sub(P, Topic, Sub)};
route_subscribe(false, #{subscriptions := S} = State, Topic, Sub) ->
    State#{subscriptions := add_sub(S, Topic, Sub)}.

add_sub(Map, Topic, Sub) ->
    Existing = maps:get(Topic, Map, sets:new()),
    Map#{Topic => sets:add_element(Sub, Existing)}.

-spec unsubscribe(state(), topic(), subscriber()) -> state().
unsubscribe(State, Topic, Sub) ->
    route_unsubscribe(is_pattern(Topic), State, Topic, Sub).

route_unsubscribe(true, #{patterns := P} = State, Topic, Sub) ->
    State#{patterns := remove_sub(P, Topic, Sub)};
route_unsubscribe(false, #{subscriptions := S} = State, Topic, Sub) ->
    State#{subscriptions := remove_sub(S, Topic, Sub)}.

remove_sub(Map, Topic, Sub) ->
    case maps:find(Topic, Map) of
        error -> Map;
        {ok, Existing} -> drop_or_keep(Map, Topic, sets:del_element(Sub, Existing))
    end.

-spec drop_or_keep(#{topic() => sets:set(subscriber())}, topic(),
                   sets:set(subscriber())) ->
        #{topic() => sets:set(subscriber())}.
drop_or_keep(Map, Topic, Set) ->
    case sets:size(Set) of
        0 -> maps:remove(Topic, Map);
        _ -> Map#{Topic => Set}
    end.

%% @doc Remove `Sub' from every topic in this realm, dropping any
%% topic whose subscriber set becomes empty as a result — the same
%% `drop_or_keep/3' rule `unsubscribe/3' applies to one topic, fanned
%% out across all of them in one pass.
%%
%% For a peer or daemon that disconnects without sending UNSUBSCRIBE
%% for everything it held: without this, a topic whose only
%% subscriber was that departed connection never empties, so it never
%% leaves `topics/1' — and `macula_station_peering_router' (which
%% treats every entry in `topics/1' as local interest worth
%% re-subscribing on every peer, regardless of whether the original
%% subscriber was a peer-sourced entry) keeps re-propagating it
%% mesh-wide forever. See
%% macula-station/plans/DESIGN_SUBSCRIPTION_LIFECYCLE_GC.md.
-spec purge_subscriber(state(), subscriber()) -> state().
purge_subscriber(#{subscriptions := S, patterns := P} = State, Sub) ->
    State#{subscriptions := purge_from(S, Sub),
           patterns      := purge_from(P, Sub)}.

purge_from(Map, Sub) ->
    maps:fold(fun(Topic, Set, Acc) ->
                 drop_or_keep(Acc, Topic, sets:del_element(Sub, Set))
              end, Map, Map).

%%=====================================================================
%% Delivery
%%=====================================================================

%% @doc Match an incoming EVENT frame to local subscribers. Returns
%% an empty list if the realm doesn't match (defensive — the
%% transport should already route by realm) or no-one is
%% subscribed.
-spec deliver_event(state(), macula_frame:frame()) ->
        [subscriber()].
deliver_event(#{realm := R} = State,
              #{frame_type := event, realm := Eventr,
                topic := Topic}) when Eventr =:= R ->
    subscribers(State, Topic);
deliver_event(_State, _Frame) ->
    [].

%%=====================================================================
%% Construction helpers
%%
%% `build_event/3' takes a published payload + signing identity
%% and builds the signed EVENT frame the wrapper feeds into
%% Plumtree.
%%=====================================================================

-spec build_event(state(), macula_frame:publish_spec(),
                  macula_identity:key_pair()) ->
        macula_frame:frame().
build_event(#{realm := R}, #{topic := T, publisher := Pub,
                              seq := Seq, payload := Payload},
            Identity) ->
    macula_frame:sign(macula_frame:event(#{
        topic         => T,
        realm         => R,
        publisher     => Pub,
        seq           => Seq,
        payload       => Payload,
        delivered_via => plumtree
    }), Identity).

%%=====================================================================
%% Inbound dispatch
%%=====================================================================

-spec process(state(), macula_identity:pubkey(),
              macula_frame:frame()) ->
        {state(), [subscriber()]}.
process(State, _From, #{frame_type := subscribe} = F) ->
    on_subscribe(State, F);
process(State, _From, #{frame_type := unsubscribe} = F) ->
    on_unsubscribe(State, F);
process(State, _From, #{frame_type := event} = F) ->
    {State, deliver_event(State, F)};
process(State, _From, _Frame) ->
    {State, []}.

-spec on_subscribe(state(), macula_frame:frame()) ->
        {state(), []}.
on_subscribe(#{realm := R} = State,
             #{realm := Eventr, topic := T, subscriber := Sub})
  when Eventr =:= R ->
    %% [mpong-trace] temporary — diagnose state_broadcast_v1 routing.
    case T of
        <<"io.macula/beam-campus/hecate/mpong/", Suffix/binary>> ->
            SubHex = binary:encode_hex(binary:part(Sub, 0, 6)),
            logger:info("[mpong-trace] on_subscribe topic=mpong/~s sub=~s",
                        [Suffix, SubHex]);
        _ -> ok
    end,
    {subscribe(State, T, Sub), []};
on_subscribe(State, _Frame) ->
    {State, []}.

-spec on_unsubscribe(state(), macula_frame:frame()) ->
        {state(), []}.
on_unsubscribe(#{realm := R} = State,
               #{realm := Eventr, topic := T, subscriber := Sub})
  when Eventr =:= R ->
    {unsubscribe(State, T, Sub), []};
on_unsubscribe(State, _Frame) ->
    {State, []}.
