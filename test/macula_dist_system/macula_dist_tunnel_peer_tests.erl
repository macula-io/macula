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

-include_lib("eunit/include/eunit.hrl").

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
