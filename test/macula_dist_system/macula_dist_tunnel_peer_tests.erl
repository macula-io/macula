%% A distribution tunnel's subscription names its peer instead of accepting any
%% publisher (macula#156): the node the tunnel was negotiated with is the only
%% publisher its topics take, on both delivery channels, and anything else is
%% refused before the socket.
%%
%% The peer is the wire's at both ends. The accept side reads the CALL's
%% `caller', which the link merges into the handler's args
%% (`macula_station_link:with_caller/2'). The connect side reads the seal
%% report's `provider', and `macula_frame:verify_reply/3' fills that only from a
%% reply signed by the key of the node it claims to be and naming the call's
%% target as `responded_by' -- so the node that answered is the node the caller
%% addressed, and no other node can answer under its id
%% (`macula_frame_request_tests:a_reply_from_a_node_other_than_the_target_is_refused').
-module(macula_dist_tunnel_peer_tests).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DIST_REALM, <<0:256>>).
-define(PEER, <<7:256>>).
-define(OTHER, <<8:256>>).

%%------------------------------------------------------------------
%% The declaration
%%------------------------------------------------------------------

%% A tunnel names exactly one publisher, and it names it by node_id.
a_tunnel_declares_the_one_peer_it_was_negotiated_with_test() ->
    ?assertEqual(#{sources => {nodes, [?PEER]}},
                 macula_dist_pool:peer_source(?PEER)).

%%------------------------------------------------------------------
%% Its peer is taken on both channels, every other publisher is refused
%%------------------------------------------------------------------

a_tunnel_takes_its_peer_on_both_channels_and_refuses_every_other_test_() ->
    {timeout, 30, fun() ->
        {Pool, Topic} = subscribed_tunnel(),
        inject(Pool, Topic, ?PEER, 1, direct),
        inject(Pool, Topic, ?PEER, 2, plumtree),
        ?assertEqual([1, 2], collect(2, 1000)),
        inject(Pool, Topic, ?OTHER, 3, direct),
        inject(Pool, Topic, ?OTHER, 4, plumtree),
        ?assertEqual(ok, collect_none(300)),
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{not_listed => 2}, maps:get(source_refused, Status)),
        stop(Pool)
    end}.

%%------------------------------------------------------------------
%% The accept side: the caller the link authenticated, or no tunnel
%%------------------------------------------------------------------

%% A CALL's payload need not be a map, and the procedure is advertised `open', so
%% a probe with a payload of any other shape must not crash the handler: it is
%% refused by name, before anything reads a field out of it.
a_tunnel_request_whose_payload_is_not_a_map_is_refused_test() ->
    macula_dist_pool:register_mesh_pool(self()),
    try
        [?assertEqual({error, <<"bad_args">>},
                      macula_dist_pool:handle_tunnel_request(Shaped))
         || Shaped <- [<<"not a map">>, [], {a, tuple}, 42]]
    after
        persistent_term:erase(macula_dist_mesh_pool)
    end.

%% `caller' is a required, wire-authenticated field of a CALL, so args without
%% one are not a tunnel request this node can name a peer for. It is refused
%% rather than opened to any publisher.
a_tunnel_request_with_no_wire_authenticated_caller_is_refused_test() ->
    macula_dist_pool:register_mesh_pool(self()),
    try
        ?assertEqual({error, <<"no_caller">>},
                     macula_dist_pool:handle_tunnel_request(
                       #{<<"from_node">> => <<"a@host">>}))
    after
        persistent_term:erase(macula_dist_mesh_pool)
    end.

%% The positive half of the refusal above, and the half that makes the test
%% suite fail when the acceptance is weakened: a request that carries a caller
%% reaches a subscription naming exactly that node, on the very topic the reply
%% hands the peer, made by the code the tunnel path runs.
the_accept_side_subscribes_for_the_caller_it_was_given_test_() ->
    {timeout, 30, fun() ->
        %% With no distribution started the setup stops at its kernel lookup, so
        %% this exercises the subscription it makes and nothing beyond it.
        ?assertEqual(undefined, whereis(net_kernel)),
        {ok, Pool} = start_recording_pool(),
        macula_dist_pool:register_mesh_pool(Pool),
        try
            {ok, Reply} = macula_dist_pool:handle_tunnel_request(
                            #{caller => ?PEER, <<"from_node">> => <<"a@host">>}),
            ?assertEqual({maps:get(<<"recv_topic">>, Reply),
                          #{sources => {nodes, [?PEER]}}},
                         subscription_recorded(Pool, 1000))
        after
            persistent_term:erase(macula_dist_mesh_pool),
            stop_recording_pool(Pool)
        end
    end}.

%%------------------------------------------------------------------
%% The accept side: bytes that arrive before the kernel takes the socket
%%------------------------------------------------------------------

%% The reorder's own property, and the half that kills the old design: a tunnel
%% holds exactly ONE subscription, and it is the bridge's. The setup used to hold
%% a buffer subscription of its own and hand the topic over at the end of the
%% handshake, which is two subscriptions and a gap between them -- and two
%% subscriptions on one topic also double every fact, because the pool fans a
%% publication out to each.
a_tunnel_holds_one_subscription_and_the_bridge_owns_it_test_() ->
    {timeout, 60, fun() ->
        ?assertEqual(undefined, whereis(net_kernel)),
        {ok, _} = application:ensure_all_started(macula),
        {ok, Pool} = start_recording_pool(),
        register(net_kernel, self()),
        macula_dist_pool:register_mesh_pool(Pool),
        try
            {ok, _Reply} = macula_dist_pool:handle_tunnel_request(
                             #{caller => ?PEER, <<"from_node">> => <<"a@host">>}),
            {accept, Setup, _DistSock, inet, macula_dist} = await_accept(),
            {Count, Subscriber} = gen_server:call(Pool, subscriptions, 1000),
            ?assertEqual(1, Count),
            %% The one subscriber is the bridge, never the setup process that
            %% offers the socket to the kernel.
            ?assertNotEqual(Setup, Subscriber),
            ?assert(is_pid(Subscriber))
        after
            catch unregister(net_kernel),
            persistent_term:erase(macula_dist_mesh_pool),
            stop_recording_pool(Pool)
        end
    end}.

%% A tunnel's bytes are not lost while the kernel is still being asked for a
%% controller, and they are not doubled either (macula#157). The kernel is stood
%% in for by this process: it is registered under the name the setup looks up,
%% it withholds the controller until two facts are in flight, and it then takes
%% the socket and reads what the bridge wrote in the meantime.
%%
%% The reads below are `{packet, 2}', which is the dist wire's own framing, so
%% each recv is exactly one datagram the bridge wrote -- in order, and the third
%% read finding nothing is the "exactly once" half.
bytes_before_the_controller_attaches_reach_it_in_order_once_test_() ->
    {timeout, 60, fun() ->
        ?assertEqual(undefined, whereis(net_kernel)),
        {ok, _} = application:ensure_all_started(macula),
        drain(),
        {ok, Pool} = macula_client:connect([], #{}),
        register(net_kernel, self()),
        macula_dist_pool:register_mesh_pool(Pool),
        try
            {ok, Reply} = macula_dist_pool:handle_tunnel_request(
                            #{caller => ?PEER, <<"from_node">> => <<"a@host">>}),
            Topic = maps:get(<<"recv_topic">>, Reply),
            {accept, Setup, DistSock, inet, macula_dist} = await_accept(),
            %% Both facts are published while the kernel holds the controller.
            inject_tunnel_fact(Pool, Topic, <<"first">>, 1),
            inject_tunnel_fact(Pool, Topic, <<"second">>, 2),
            Setup ! {self(), controller, self()},
            receive
                {Setup, controller, ok, <<>>} -> ok
            after 5000 ->
                ?assert(false)
            end,
            ?assertEqual({ok, <<"first">>}, gen_tcp:recv(DistSock, 0, 3000)),
            ?assertEqual({ok, <<"second">>}, gen_tcp:recv(DistSock, 0, 3000)),
            ?assertEqual({error, timeout}, gen_tcp:recv(DistSock, 0, 300))
        after
            catch unregister(net_kernel),
            persistent_term:erase(macula_dist_mesh_pool),
            stop(Pool)
        end
    end}.

%% The same reorder's other side: a bridge that cannot start offers the kernel
%% nothing at all, so there is no socket left half-owned and no reader-less
%% handshake to hang on (macula#158, macula#157).
a_tunnel_whose_bridge_cannot_start_offers_the_kernel_nothing_test_() ->
    {timeout, 30, fun() ->
        ?assertEqual(undefined, whereis(net_kernel)),
        {ok, _} = application:ensure_all_started(macula),
        {ok, RefusingPool} = start_refusing_pool(),
        register(net_kernel, self()),
        macula_dist_pool:register_mesh_pool(RefusingPool),
        try
            ?assertMatch({ok, _},
                         macula_dist_pool:handle_tunnel_request(
                           #{caller => ?PEER, <<"from_node">> => <<"a@host">>})),
            receive
                {accept, _Setup, _Sock, inet, macula_dist} -> ?assert(false)
            after 700 ->
                ok
            end
        after
            catch unregister(net_kernel),
            persistent_term:erase(macula_dist_mesh_pool),
            stop_recording_pool(RefusingPool)
        end
    end}.

%%------------------------------------------------------------------
%% The connect side: the provider the report named, made by the real path
%%------------------------------------------------------------------

%% The subscription the connect path creates for a tunnel it opened names the
%% provider the seal report carried: that provider's publication passes the gate
%% and another node's is refused, on the topic the bridge reads.
the_connect_side_names_the_provider_the_report_carried_test_() ->
    {timeout, 30, fun() ->
        {ok, _} = application:ensure_all_started(macula),
        {ok, Pool} = macula_client:connect([], #{}),
        TunnelId = <<"ceres156tunnel">>,
        Reply = #{<<"tunnel_id">> => TunnelId},
        %% A received tunnel reply, as `macula:call/6' returns it when the report
        %% is asked for. `sealed' is in every report, 0 for a call sent clear.
        ?assertMatch({ok, _, _},
                     macula_dist_pool:on_tunnel_rpc_reply(
                       {ok, Reply, #{provider => ?PEER, sealed => 0}}, Pool)),
        Topic = <<"_dist.data.", TunnelId/binary, ".in">>,
        inject(Pool, Topic, ?PEER, 1, direct),
        inject(Pool, Topic, ?OTHER, 2, direct),
        %% The peer's publication is the one the gate admitted, and the other
        %% node's is the one it refused.
        {ok, Status} = macula_client:status(Pool),
        ?assertEqual(#{not_listed => 1}, maps:get(source_refused, Status)),
        stop(Pool)
    end}.

%%------------------------------------------------------------------
%% Fixtures
%%------------------------------------------------------------------

%% A pool with one subscription declaring this node's tunnel peer on ?TOPIC.
subscribed_tunnel() ->
    {ok, _} = application:ensure_all_started(macula),
    %% eunit runs a module's tests in one process, so a message an earlier test
    %% left behind is still in the mailbox when this one counts what it sees.
    drain(),
    {ok, Pool} = macula_client:connect([], #{}),
    Topic = <<"_dist.data.ceres156.recv">>,
    {ok, _} = macula_client:subscribe(Pool, ?DIST_REALM, Topic, self(),
                                      #{sources => {nodes, [?PEER]},
                                        delivery => as_arrives}),
    {Pool, Topic}.

drain() ->
    receive
        _Stale -> drain()
    after 0 ->
        ok
    end.

stop(Pool) ->
    try macula_client:close(Pool) catch _:_ -> ok end,
    ok.

%% A stand-in for a V2 pool: it answers the client calls the tunnel's setup makes
%% and records the topic and options it was subscribed with, which is the only
%% way to read a subscription's declaration from outside the pool.
start_recording_pool() ->
    gen_server:start(?MODULE, {recording_pool, accept}, []).

%% The same pool refusing every subscription: a bridge whose `init' cannot
%% subscribe is a bridge that cannot start.
start_refusing_pool() ->
    gen_server:start(?MODULE, {recording_pool, refuse}, []).

stop_recording_pool(Pool) ->
    try gen_server:stop(Pool, normal, 1000) catch _:_ -> ok end.

%% The socket the setup offers the kernel, once it has started the bridge.
await_accept() ->
    receive
        {accept, _Setup, _DistSock, inet, macula_dist} = Offered -> Offered
    after 5000 ->
        none
    end.

%% One fact published by the tunnel's peer, encrypted under the tunnel's own key
%% and framed as the dist wire frames a datagram, so the bridge writes it to the
%% socket exactly as it writes a peer's bytes.
inject_tunnel_fact(Pool, Topic, Datagram, Seq) ->
    Key = macula_dist_pool:tunnel_key(),
    Framed = <<(byte_size(Datagram)):16, Datagram/binary>>,
    Pool ! {macula_event, make_ref(), Topic, macula_dist_bridge:encrypt(Key, Framed),
            #{realm => ?DIST_REALM, publisher => ?PEER, publisher_verified => true,
              seq => Seq, delivered_via => direct,
              publication_hash => crypto:hash(sha384, <<?PEER/binary, Seq:64>>),
              expires_at => erlang:system_time(millisecond) + 60_000}},
    ok.

%% What the pool was last subscribed with, once it has been, or `none' when the
%% setup has not got that far inside the window.
subscription_recorded(_Pool, Timeout) when Timeout =< 0 ->
    none;
subscription_recorded(Pool, Timeout) ->
    case gen_server:call(Pool, subscription, 1000) of
        none ->
            timer:sleep(20),
            subscription_recorded(Pool, Timeout - 20);
        Recorded ->
            Recorded
    end.

%% gen_server callbacks for the recording pool. It counts subscriptions and keeps
%% the last one's subscriber, because "how many subscriptions a tunnel holds, and
%% whose they are" is the property the reorder in macula#157 changed.
init({recording_pool, Mode}) ->
    {ok, #{mode => Mode, count => 0, last => undefined}}.

handle_call({subscribe, _Realm, Topic, Subscriber, Opts}, _From, #{mode := Mode} = State) ->
    Recorded = #{count => maps:get(count, State) + 1,
                 last => {Topic, Opts, Subscriber}},
    {reply, subscribe_reply(Mode), maps:merge(State, Recorded)};
handle_call(subscription, _From, #{last := undefined} = State) ->
    {reply, none, State};
handle_call(subscription, _From, #{last := {Topic, Opts, _Subscriber}} = State) ->
    {reply, {Topic, Opts}, State};
handle_call(subscriptions, _From, #{count := Count, last := undefined} = State) ->
    {reply, {Count, none}, State};
handle_call(subscriptions, _From, #{count := Count, last := {_, _, Subscriber}} = State) ->
    {reply, {Count, Subscriber}, State};
handle_call({unsubscribe, _Ref}, _From, State) ->
    {reply, ok, State};
handle_call({publish, _Realm, _Topic, _Payload, _Opts}, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

%% A refusing pool answers the subscription with an error, which is a bridge whose
%% `init' cannot subscribe and therefore cannot start.
subscribe_reply(refuse) -> {error, <<"nope">>};
subscribe_reply(accept) -> {ok, make_ref()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Each (publisher, seq) stands in for one verified publication over the given
%% delivery channel, with its own publication hash and a minute to live.
inject(Pool, Topic, Publisher, Seq, DeliveredVia) ->
    Pool ! {macula_event, make_ref(), Topic, Seq,
            #{realm => ?DIST_REALM, publisher => Publisher, publisher_verified => true,
              seq => Seq, delivered_via => DeliveredVia,
              publication_hash => crypto:hash(sha384, <<Publisher/binary, Seq:64>>),
              expires_at => erlang:system_time(millisecond) + 60_000}},
    ok.

collect(N, Timeout) ->
    collect(N, Timeout, []).

collect(0, _Timeout, Acc) ->
    lists:reverse(Acc);
collect(N, Timeout, Acc) ->
    receive
        {macula_event, _Ref, _Topic, Payload, _Meta} -> collect(N - 1, Timeout, [Payload | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

%% Nothing is delivered within the window.
collect_none(Timeout) ->
    collect_one(Timeout).

collect_one(Timeout) ->
    receive
        {macula_event, _Ref, _Topic, Payload, _Meta} -> {unexpected, Payload}
    after Timeout ->
        ok
    end.
