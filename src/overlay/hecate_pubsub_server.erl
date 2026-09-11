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
%% State mutations + frame processing only. The publish path signs a
%% PUBLISH with the server's node identity key, builds its EVENT and
%% returns the matched LOCAL subscribers,
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
    subscribe/3, unsubscribe/3, purge_subscriber/2, is_subscribed/3,
    subscribers/2, topics/1, patterns/1, topic_count/1, subscriber_count/1,
    realm/1,
    publish/3, deliver_event/2, process_frame/3,
    relay_publish/2,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export_type([opts/0]).

%% `identity' is an identity key in the node's configured crypto profile: the server signs its own PUBLISH frames with
%% it and verifies publications under that profile.
-type opts() :: #{realm := <<_:256>>, identity := macula_node_keys:node_key()}.

-record(state, {
    realm    :: <<_:256>>,
    identity :: macula_node_keys:node_key(),
    profile  :: macula_crypto_profile:profile(),
    pubsub   :: hecate_pubsub:state(),
    next_seq :: non_neg_integer()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the server for a realm. A server whose `identity' is not an identity key in the node's configured
%% crypto profile does not start, so one node never runs two profiles.
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(#{realm := <<_:256>>, identity := _} = Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec subscribe(pid(), binary(), <<_:256>>) -> ok.
subscribe(Pid, Topic, Sub) ->
    gen_server:call(Pid, {subscribe, Topic, Sub}).

-spec unsubscribe(pid(), binary(), <<_:256>>) -> ok.
unsubscribe(Pid, Topic, Sub) ->
    gen_server:call(Pid, {unsubscribe, Topic, Sub}).

%% @doc Remove `Sub' from every topic this server holds, dropping any
%% topic that empties out as a result. See
%% `hecate_pubsub:purge_subscriber/2'.
-spec purge_subscriber(pid(), <<_:256>>) -> ok.
purge_subscriber(Pid, Sub) ->
    gen_server:call(Pid, {purge_subscriber, Sub}).

-spec is_subscribed(pid(), binary(), <<_:256>>) -> boolean().
is_subscribed(Pid, Topic, Sub) ->
    gen_server:call(Pid, {is_subscribed, Topic, Sub}).

-spec subscribers(pid(), binary()) -> [<<_:256>>].
subscribers(Pid, Topic) ->
    gen_server:call(Pid, {subscribers, Topic}).

-spec topics(pid()) -> [binary()].
topics(Pid) ->
    gen_server:call(Pid, topics).

-spec patterns(pid()) -> [binary()].
patterns(Pid) ->
    gen_server:call(Pid, patterns).

-spec topic_count(pid()) -> non_neg_integer().
topic_count(Pid) ->
    gen_server:call(Pid, topic_count).

-spec subscriber_count(pid()) -> non_neg_integer().
subscriber_count(Pid) ->
    gen_server:call(Pid, subscriber_count).

-spec realm(pid()) -> <<_:256>>.
realm(Pid) ->
    gen_server:call(Pid, realm).

%% @doc Sign a PUBLISH for `Topic'/`Payload' with the server's node
%% identity key, build its EVENT, and return the EVENT together with
%% the set of LOCAL subscribers that match. The caller
%% is responsible for handing the frame to the cross-station delivery
%% layer (Plumtree, future commit) and for delivering to the matched
%% local subscribers via the application channel.
-spec publish(pid(), binary(), binary()) ->
        {macula_frame:frame(), [<<_:256>>]}.
publish(Pid, Topic, Payload) ->
    gen_server:call(Pid, {publish, Topic, Payload}).

%% @doc Process an inbound EVENT frame received from the wire. Its
%% publication is verified first; one that does not verify matches no
%% one. Returns the matched local subscribers; the caller delivers.
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
%% server verifies its publication once, under the server's profile,
%% and builds the EVENT from the same publication bytes, so the
%% publisher's signature goes end to end and no hop re-signs it (D17).
%% Returns the EVENT and the matched local subscribers. The caller
%% (typically the peer observer) is responsible for sending
%% `EventFrame' on each subscriber's peering connection.
%%
%% Returns the publication's refusal when it does not verify, and
%% `{error, realm_mismatch}' when its realm is not this server's (the
%% registry routes by realm, so that is a defensive check).
-spec relay_publish(pid(), macula_frame:frame()) ->
        {macula_frame:frame(), [<<_:256>>]} | {error, term()}.
relay_publish(Pid, Frame) ->
    gen_server:call(Pid, {relay_publish, Frame}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(#{realm := Realm, identity := Key}) ->
    started(identity_profile(Key, macula_crypto_profile:configured()), Realm, Key).

%% The server's key is an identity key in the node's configured profile, the one the pool reads, so one node never
%% runs two profiles.
identity_profile(#{purpose := identity, profile := Profile}, {ok, Profile}) ->
    {ok, Profile};
identity_profile(#{purpose := identity, profile := KeyProfile}, {ok, Configured}) ->
    {error, {identity_profile_mismatch, KeyProfile, Configured}};
identity_profile(#{purpose := identity}, {error, _} = Refusal) ->
    Refusal;
identity_profile(_NotAnIdentityKey, _Configured) ->
    {error, {identity, not_an_identity_key}}.

started({error, _} = Refusal, _Realm, _Key) ->
    Refusal;
started({ok, Profile}, Realm, Key) ->
    {ok, #state{
        realm    = Realm,
        identity = Key,
        profile  = Profile,
        pubsub   = hecate_pubsub:new(Realm, Profile),
        %% Seeded from wall-clock µs, never from 0 -- the same convention
        %% macula_client's publish_seq follows. Subscribers put a
        %% publisher's stream back in order with macula_pubsub_order,
        %% which reads a large forward jump as a restart. A counter that
        %% restarts at 0 instead rewinds below every subscriber's
        %% watermark, and each fact is then dropped as "past" until the
        %% counter climbs back over it: a station rollout blinded
        %% hecate-stations for 10+ hours this way on 2026-09-02, with the
        %% link, subscriptions and dedup all looking healthy.
        next_seq = erlang:system_time(microsecond)
    }}.

handle_call({subscribe, Topic, Sub}, _From, S) ->
    PS2 = hecate_pubsub:subscribe(S#state.pubsub, Topic, Sub),
    {reply, ok, S#state{pubsub = PS2}};
handle_call({unsubscribe, Topic, Sub}, _From, S) ->
    PS2 = hecate_pubsub:unsubscribe(S#state.pubsub, Topic, Sub),
    {reply, ok, S#state{pubsub = PS2}};
handle_call({purge_subscriber, Sub}, _From, S) ->
    PS2 = hecate_pubsub:purge_subscriber(S#state.pubsub, Sub),
    {reply, ok, S#state{pubsub = PS2}};
handle_call({is_subscribed, Topic, Sub}, _From, S) ->
    {reply, hecate_pubsub:is_subscribed(S#state.pubsub, Topic, Sub), S};
handle_call({subscribers, Topic}, _From, S) ->
    {reply, hecate_pubsub:subscribers(S#state.pubsub, Topic), S};
handle_call(topics, _From, S) ->
    {reply, hecate_pubsub:topics(S#state.pubsub), S};
handle_call(patterns, _From, S) ->
    {reply, hecate_pubsub:patterns(S#state.pubsub), S};
handle_call(topic_count, _From, S) ->
    {reply, hecate_pubsub:topic_count(S#state.pubsub), S};
handle_call(subscriber_count, _From, S) ->
    {reply, hecate_pubsub:subscriber_count(S#state.pubsub), S};
handle_call(realm, _From, S) ->
    {reply, S#state.realm, S};
handle_call({publish, Topic, Payload}, _From, S) ->
    Spec = #{realm        => S#state.realm,
             topic        => Topic,
             seq          => S#state.next_seq,
             published_at => erlang:system_time(millisecond),
             payload      => Payload},
    Event   = hecate_pubsub:build_event(macula_frame:publish(Spec, S#state.identity), plumtree),
    Matched = hecate_pubsub:subscribers(S#state.pubsub, Topic),
    {reply, {Event, Matched}, S#state{next_seq = S#state.next_seq + 1}};
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

%% A relayed PUBLISH is verified once, under the server's profile, before its EVENT is built from the same
%% publication bytes.
do_relay_publish(#{frame_type := publish} = Frame, #state{profile = Profile} = S) ->
    relayed(macula_frame:verify_publication(Frame, Profile, erlang:system_time(millisecond)), Frame, S);
do_relay_publish(_Frame, _S) ->
    {error, malformed_frame}.

relayed({ok, #{realm := R, topic := Topic}}, Frame, #state{realm = R} = S) ->
    Matched = hecate_pubsub:subscribers(S#state.pubsub, Topic),
    trace_mpong(Topic, Matched),
    {hecate_pubsub:build_event(Frame, direct), Matched};
relayed({ok, _AnotherRealm}, _Frame, _S) ->
    {error, realm_mismatch};
relayed({error, _} = Refusal, _Frame, _S) ->
    Refusal.

%% [mpong-trace] temporary: diagnose state_broadcast_v1 routing
%% (see project_mpong_state_broadcast_bug memory). Remove after fix.
trace_mpong(<<"io.macula/beam-campus/hecate/mpong/", Suffix/binary>>, Matched) ->
    logger:info("[mpong-trace] do_relay_publish topic=mpong/~s matched=~p", [Suffix, length(Matched)]);
trace_mpong(_Topic, _Matched) ->
    ok.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.
