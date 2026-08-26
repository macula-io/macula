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
%% Reference: plans/PLAN_MACULA_V2_PART6_PROTOCOL.md §6;
%% plans/PLAN_PHASE_5_BREAKDOWN.md Session 5.5.
-module(hecate_pubsub).

-export([
    new/1,
    realm/1,
    subscribe/3,
    unsubscribe/3,
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
    subscriptions := #{topic() => sets:set(subscriber())}
}.

%%=====================================================================
%% Construction
%%=====================================================================

-spec new(<<_:256>>) -> state().
new(<<_:256>> = Realm) ->
    #{realm => Realm, subscriptions => #{}}.

%%=====================================================================
%% Inspection
%%=====================================================================

realm(#{realm := R}) -> R.

-spec subscribers(state(), topic()) -> [subscriber()].
subscribers(#{subscriptions := S}, Topic) ->
    case maps:find(Topic, S) of
        {ok, Set} -> sets:to_list(Set);
        error     -> []
    end.

-spec is_subscribed(state(), topic(), subscriber()) -> boolean().
is_subscribed(#{subscriptions := S}, Topic, Sub) ->
    case maps:find(Topic, S) of
        {ok, Set} -> sets:is_element(Sub, Set);
        error     -> false
    end.

-spec topics(state()) -> [topic()].
topics(#{subscriptions := S}) -> maps:keys(S).

-spec topic_count(state()) -> non_neg_integer().
topic_count(#{subscriptions := S}) -> maps:size(S).

-spec subscriber_count(state()) -> non_neg_integer().
subscriber_count(#{subscriptions := S}) ->
    maps:fold(fun(_, Set, Acc) -> Acc + sets:size(Set) end, 0, S).

%%=====================================================================
%% Local subscribe / unsubscribe
%%=====================================================================

-spec subscribe(state(), topic(), subscriber()) -> state().
subscribe(#{subscriptions := S} = State, Topic, Sub)
  when is_binary(Topic), is_binary(Sub), byte_size(Sub) =:= 32 ->
    Existing = maps:get(Topic, S, sets:new()),
    State#{subscriptions := S#{Topic => sets:add_element(Sub, Existing)}}.

-spec unsubscribe(state(), topic(), subscriber()) -> state().
unsubscribe(#{subscriptions := S} = State, Topic, Sub) ->
    case maps:find(Topic, S) of
        error -> State;
        {ok, Existing} ->
            Trimmed = sets:del_element(Sub, Existing),
            State#{subscriptions := drop_or_keep(S, Topic, Trimmed)}
    end.

-spec drop_or_keep(#{topic() => sets:set(subscriber())}, topic(),
                   sets:set(subscriber())) ->
        #{topic() => sets:set(subscriber())}.
drop_or_keep(Map, Topic, Set) ->
    case sets:size(Set) of
        0 -> maps:remove(Topic, Map);
        _ -> Map#{Topic => Set}
    end.

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
