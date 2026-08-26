%% @doc PubSub gen_server wrapping `hecate_pubsub' state for one realm
%% namespace.
%%
%% Activates the dormant `hecate_pubsub' pure-state module by giving
%% it a process identity. One server instance owns the
%% topic-to-subscriber index for a single realm tag; the realm is an
%% opaque 32-byte namespace key, not validated against any authority.
%% Multi-tenancy comes from running multiple servers under different
%% realm tags — the station does not arbitrate which realm tags are
%% "real" (Sprint A: realm identity lives outside infrastructure).
%%
%% == Phase 1 scope (this commit) ==
%%
%% State mutations + frame processing only. The publish path builds
%% the signed EVENT frame and returns the matched LOCAL subscribers,
%% but does NOT fan out across the cluster — that requires the
%% Plumtree wire layer (`hecate_plumtree') and the DHT topic-discovery
%% integration which land in subsequent commits.
%%
%% == Sequencing ==
%%
%% <ul>
%%   <li>This commit: server in isolation, no integration with
%%       station listener or DHT.</li>
%%   <li>Next: per-realm-namespace registry under
%%       `hecate_overlay_sup' so the listener can route inbound
%%       SUBSCRIBE / UNSUBSCRIBE / EVENT frames to the right
%%       server.</li>
%%   <li>Then: Plumtree fan-out for cross-station delivery.</li>
%%   <li>Then: DHT integration for topic-mesh discovery.</li>
%% </ul>
-module(hecate_pubsub_server).
-behaviour(gen_server).

-export([
    start_link/1,
    subscribe/3, unsubscribe/3, is_subscribed/3,
    subscribers/2, topics/1, topic_count/1, subscriber_count/1,
    realm/1,
    publish/3, deliver_event/2, process_frame/3,
    relay_publish/2,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export_type([opts/0]).

-type opts() :: #{realm := <<_:256>>, identity := macula_identity:key_pair()}.

-record(state, {
    realm    :: <<_:256>>,
    identity :: macula_identity:key_pair(),
    self_id  :: <<_:256>>,
    pubsub   :: hecate_pubsub:state(),
    next_seq :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(#{realm := <<_:256>>, identity := _} = Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec subscribe(pid(), binary(), <<_:256>>) -> ok.
subscribe(Pid, Topic, Sub) ->
    gen_server:call(Pid, {subscribe, Topic, Sub}).

-spec unsubscribe(pid(), binary(), <<_:256>>) -> ok.
unsubscribe(Pid, Topic, Sub) ->
    gen_server:call(Pid, {unsubscribe, Topic, Sub}).

-spec is_subscribed(pid(), binary(), <<_:256>>) -> boolean().
is_subscribed(Pid, Topic, Sub) ->
    gen_server:call(Pid, {is_subscribed, Topic, Sub}).

-spec subscribers(pid(), binary()) -> [<<_:256>>].
subscribers(Pid, Topic) ->
    gen_server:call(Pid, {subscribers, Topic}).

-spec topics(pid()) -> [binary()].
topics(Pid) ->
    gen_server:call(Pid, topics).

-spec topic_count(pid()) -> non_neg_integer().
topic_count(Pid) ->
    gen_server:call(Pid, topic_count).

-spec subscriber_count(pid()) -> non_neg_integer().
subscriber_count(Pid) ->
    gen_server:call(Pid, subscriber_count).

-spec realm(pid()) -> <<_:256>>.
realm(Pid) ->
    gen_server:call(Pid, realm).

%% @doc Build a signed EVENT frame for `Topic'/`Payload' and return it
%% together with the set of LOCAL subscribers that match. The caller
%% is responsible for handing the frame to the cross-station delivery
%% layer (Plumtree, future commit) and for delivering to the matched
%% local subscribers via the application channel.
-spec publish(pid(), binary(), binary()) ->
        {macula_frame:frame(), [<<_:256>>]}.
publish(Pid, Topic, Payload) ->
    gen_server:call(Pid, {publish, Topic, Payload}).

%% @doc Process an inbound EVENT frame received from the wire. Returns
%% the matched local subscribers; the caller delivers.
-spec deliver_event(pid(), macula_frame:frame()) -> [<<_:256>>].
deliver_event(Pid, Frame) ->
    gen_server:call(Pid, {deliver_event, Frame}).

%% @doc Generic frame dispatch — handles subscribe / unsubscribe /
%% event uniformly. Returns the matched subscribers for event frames,
%% empty list for subscribe / unsubscribe.
-spec process_frame(pid(), <<_:256>>, macula_frame:frame()) -> [<<_:256>>].
process_frame(Pid, From, Frame) ->
    gen_server:call(Pid, {process_frame, From, Frame}).

%% @doc Relay an inbound PUBLISH frame from a remote daemon. The
%% server builds an EVENT frame signed by THIS server's identity
%% (intermediate hop re-signing — Phase 1 simplification; Phase 2
%% tightens to publisher-end-to-end auth via UCAN), preserves the
%% original publisher pubkey + seq inside the EVENT, and returns
%% the matched local subscribers. The caller (typically the
%% peer observer) is responsible for sending `EventFrame' on each
%% subscriber's peering connection.
%%
%% Returns `{error, realm_mismatch}' when the publish frame's realm
%% does not match this server's realm. The registry routes by realm
%% so this should never fire in practice — defensive check.
-spec relay_publish(pid(), macula_frame:frame()) ->
        {macula_frame:frame(), [<<_:256>>]} | {error, realm_mismatch}.
relay_publish(Pid, Frame) ->
    gen_server:call(Pid, {relay_publish, Frame}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(#{realm := Realm, identity := Kp}) ->
    {ok, #state{
        realm    = Realm,
        identity = Kp,
        self_id  = macula_identity:public(Kp),
        pubsub   = hecate_pubsub:new(Realm),
        next_seq = 0
    }}.

handle_call({subscribe, Topic, Sub}, _From, S) ->
    PS2 = hecate_pubsub:subscribe(S#state.pubsub, Topic, Sub),
    {reply, ok, S#state{pubsub = PS2}};
handle_call({unsubscribe, Topic, Sub}, _From, S) ->
    PS2 = hecate_pubsub:unsubscribe(S#state.pubsub, Topic, Sub),
    {reply, ok, S#state{pubsub = PS2}};
handle_call({is_subscribed, Topic, Sub}, _From, S) ->
    {reply, hecate_pubsub:is_subscribed(S#state.pubsub, Topic, Sub), S};
handle_call({subscribers, Topic}, _From, S) ->
    {reply, hecate_pubsub:subscribers(S#state.pubsub, Topic), S};
handle_call(topics, _From, S) ->
    {reply, hecate_pubsub:topics(S#state.pubsub), S};
handle_call(topic_count, _From, S) ->
    {reply, hecate_pubsub:topic_count(S#state.pubsub), S};
handle_call(subscriber_count, _From, S) ->
    {reply, hecate_pubsub:subscriber_count(S#state.pubsub), S};
handle_call(realm, _From, S) ->
    {reply, S#state.realm, S};
handle_call({publish, Topic, Payload}, _From, S) ->
    Spec = #{topic           => Topic,
             realm           => S#state.realm,
             publisher       => S#state.self_id,
             seq             => S#state.next_seq,
             payload         => Payload,
             published_at_ms => erlang:system_time(millisecond)},
    Frame   = hecate_pubsub:build_event(S#state.pubsub, Spec, S#state.identity),
    Matched = hecate_pubsub:deliver_event(S#state.pubsub, Frame),
    {reply, {Frame, Matched}, S#state{next_seq = S#state.next_seq + 1}};
handle_call({deliver_event, Frame}, _From, S) ->
    {reply, hecate_pubsub:deliver_event(S#state.pubsub, Frame), S};
handle_call({process_frame, From, Frame}, _From, S) ->
    {PS2, Subs} = hecate_pubsub:process(S#state.pubsub, From, Frame),
    {reply, Subs, S#state{pubsub = PS2}};
handle_call({relay_publish, Frame}, _From, S) ->
    {reply, do_relay_publish(Frame, S), S};
handle_call(_Request, _From, S) ->
    {reply, {error, unknown_call}, S}.

%%====================================================================
%% Internals — publish relay
%%====================================================================

do_relay_publish(#{frame_type := publish, realm := R} = Frame,
                 #state{realm = R} = S) ->
    EventFrame = build_relay_event(Frame, S),
    Matched    = hecate_pubsub:deliver_event(S#state.pubsub, EventFrame),
    %% [mpong-trace] temporary — diagnose state_broadcast_v1 routing
    %% (see project_mpong_state_broadcast_bug memory). Remove after fix.
    case maps:get(topic, Frame, <<>>) of
        <<"io.macula/beam-campus/hecate/mpong/", Suffix/binary>> ->
            logger:info("[mpong-trace] do_relay_publish topic=mpong/~s matched=~p",
                        [Suffix, length(Matched)]);
        _ -> ok
    end,
    {EventFrame, Matched};
do_relay_publish(_Frame, _S) ->
    {error, realm_mismatch}.

build_relay_event(#{topic := T, realm := R, publisher := Pub,
                    seq := Seq, payload := Pl} = PubFrame,
                  #state{identity = Id}) ->
    Spec0 = #{topic         => T,
              realm         => R,
              publisher     => Pub,
              seq           => Seq,
              payload       => Pl,
              delivered_via => direct},
    %% Carry the publisher-end-to-end signature onto the EVENT if the
    %% inbound PUBLISH had one (pubsub Phase 2: a daemon with
    %% `pubsub_emit_publisher_sig' enabled attaches it). Consumers and
    %% relay stations then verify EVENT authenticity against the
    %% publisher, not the relaying station — so the EVENT can be
    %% relayed beyond one hop. We still attach our own per-hop
    %% signature below: pre-4.4.0 stations on a relay path verify
    %% against the conn NodeId, and that mismatch is still the loop
    %% kill for EVENTs that carry no `publisher_sig'. `canonical_unsigned/1'
    %% (macula >= 4.4.0) excludes `publisher_sig', so signing after
    %% adding it leaves the per-hop signature valid.
    Spec = case maps:get(publisher_sig, PubFrame, undefined) of
               <<Sig:64/binary>> -> Spec0#{publisher_sig => Sig};
               _                 -> Spec0
           end,
    macula_frame:sign(macula_frame:event(Spec), Id).

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.
