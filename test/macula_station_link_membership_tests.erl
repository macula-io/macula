%% EUnit tests for presenting a realm membership on a station link (macula#128; threat model macula-realm#97, carriers
%% in section 2 and requirement 6.1): the link's next SUBSCRIBE or PUBLISH in the realm carries the membership once per
%% connection as the per-hop extension `realm.membership', a new connection carries it again, and a membership
%% presented anew on a link already subscribed in the realm goes out at once, with every subscription the link holds in
%% the realm sent again (macula#133). The station reads it back from the decoded frame with
%% macula_realm_rights:presented/1.
%%
%% The link's peer is this test process, so every frame it sends arrives here as the peering cast.
-module(macula_station_link_membership_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(OTHER_REALM, <<8:256>>).
-define(TOPIC, <<"acme.counted">>).
-define(PROCEDURE, <<"acme.proc">>).
-define(AD, <<"an advertisement's wire form">>).
-define(PRESENTED, #{token => <<"membership token">>, proofs => [<<"proof one">>]}).
-define(RENEWED, #{token => <<"renewed token">>, proofs => []}).

membership_test_() ->
    {foreach, fun link/0, fun macula_station_link:stop/1,
     [fun(Link) -> {Name, fun() -> Case(connected(Link)) end} end
      || {Name, Case} <- [{"the first SUBSCRIBE in the realm carries the membership, and only that one",
                           fun the_first_subscribe_carries_it/1},
                          {"a SUBSCRIBE in another realm carries none", fun another_realm_carries_none/1},
                          {"the first ADVERTISE in the realm carries the membership, and only that one",
                           fun the_first_advertise_carries_it/1},
                          {"a provider that only advertises presents its token, and a rotation sends its advertisements again",
                           fun a_rotation_resends_the_advertisements/1},
                          {"the first PUBLISH carries it when no SUBSCRIBE came first", fun the_first_publish_carries_it/1},
                          {"a new connection carries it again", fun a_new_connection_carries_it_again/1},
                          {"a membership presented anew goes out at once, every subscription in the realm sent again",
                           fun a_renewed_membership_goes_out_at_once/1},
                          {"a link not subscribed in the realm waits for its next frame",
                           fun an_unsubscribed_link_waits/1},
                          {"the station reads what the link sent", fun the_station_reads_what_was_sent/1}]]}.

pool_test_() ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(macula) end, fun(_) -> ok end,
     [{"a token past the verified size is refused before it is sent", fun a_token_past_the_size_is_refused/0},
      {"the pool presents on every link and to a respawned one", fun the_pool_presents_on_every_link/0},
      {"a station dialled directly after the membership is presented carries it",
       fun a_direct_dial_carries_the_membership/0}]}.

the_first_subscribe_carries_it(Link) ->
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?PRESENTED),
    {ok, _} = macula_station_link:subscribe(Link, ?REALM, ?TOPIC, self()),
    ?assertEqual({ok, ?PRESENTED}, presented(sent(subscribe))),
    {ok, _} = macula_station_link:subscribe(Link, ?REALM, <<"acme.other">>, self()),
    ?assertEqual(none, presented(sent(subscribe))).

another_realm_carries_none(Link) ->
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?PRESENTED),
    {ok, _} = macula_station_link:subscribe(Link, ?OTHER_REALM, ?TOPIC, self()),
    ?assertEqual(none, presented(sent(subscribe))).

%% A provider that never subscribes or publishes: its first ADVERTISE is the realm's first word to its station.
the_first_advertise_carries_it(Link) ->
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?PRESENTED),
    ok = advertise(Link, ?PROCEDURE),
    ?assertEqual({ok, ?PRESENTED}, presented(sent(advertise))),
    ok = advertise(Link, <<"acme.other">>),
    ?assertEqual(none, presented(sent(advertise))).

%% A fresh token reaches the station before the old one ends, on the advertisements as on the subscriptions
%% (macula-realm#175 D3): the station purges what it admitted at `until', and a provider that only advertises has
%% nothing else to re-present with.
a_rotation_resends_the_advertisements(Link) ->
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?PRESENTED),
    ok = advertise(Link, ?PROCEDURE),
    ?assertEqual({ok, ?PRESENTED}, presented(sent(advertise))),
    ok = advertise(Link, <<"acme.other">>),
    ?assertEqual(none, presented(sent(advertise))),
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?RENEWED),
    ?assertEqual([none, {ok, ?RENEWED}], lists:sort([presented(sent(advertise)) || _ <- [1, 2]])),
    ?assertEqual(nothing, next_sent()).

advertise(Link, Procedure) ->
    macula_station_link:advertise(Link, ?REALM, Procedure, fun(_Request) -> {error, unavailable} end, open, ?AD).

the_first_publish_carries_it(Link) ->
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?PRESENTED),
    ok = macula_station_link:publish(Link, ?REALM, ?TOPIC, <<"one">>),
    ?assertEqual({ok, ?PRESENTED}, presented(sent(publish))),
    ok = macula_station_link:publish(Link, ?REALM, ?TOPIC, <<"two">>),
    ?assertEqual(none, presented(sent(publish))).

a_new_connection_carries_it_again(Link) ->
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?PRESENTED),
    {ok, _} = macula_station_link:subscribe(Link, ?REALM, ?TOPIC, self()),
    ?assertEqual({ok, ?PRESENTED}, presented(sent(subscribe))),
    Link ! {macula_peering, connected, self(), <<21:256>>},
    ?assertEqual({ok, ?PRESENTED}, presented(sent(subscribe))).

a_renewed_membership_goes_out_at_once(Link) ->
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?PRESENTED),
    {ok, _} = macula_station_link:subscribe(Link, ?REALM, ?TOPIC, self()),
    {ok, _} = macula_station_link:subscribe(Link, ?REALM, <<"acme.other">>, self()),
    ?assertEqual({ok, ?PRESENTED}, presented(sent(subscribe))),
    ?assertEqual(none, presented(sent(subscribe))),
    {ok, _} = macula_station_link:subscribe(Link, ?OTHER_REALM, <<"acme.elsewhere">>, self()),
    _ = sent(subscribe),
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?RENEWED),
    First = sent(subscribe),
    Second = sent(subscribe),
    ?assertMatch({#{topic := <<"acme.counted">>, realm := ?REALM}, #{topic := <<"acme.other">>, realm := ?REALM}},
                 {First, Second}),
    ?assertEqual({{ok, ?RENEWED}, none}, {presented(First), presented(Second)}),
    ?assertEqual(nothing, next_sent()).

an_unsubscribed_link_waits(Link) ->
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?PRESENTED),
    _ = sys:get_state(Link),
    ?assertEqual(nothing, next_sent()).

%% The frame the station receives is the decoded one: the extension's value comes back in the peer-value form.
the_station_reads_what_was_sent(Link) ->
    ok = macula_station_link:present_membership_async(Link, ?REALM, ?PRESENTED),
    {ok, _} = macula_station_link:subscribe(Link, ?REALM, ?TOPIC, self()),
    {ok, Received, <<>>} = macula_frame:decode(macula_frame:encode(sent(subscribe))),
    ?assertEqual({ok, ?PRESENTED}, macula_realm_rights:presented(Received)),
    ?assertEqual({error, malformed},
                 macula_realm_rights:presented(Received#{ext => #{<<"realm.membership">> =>
                                                                      #{critical => 0, value => <<"not a map">>}}})).

a_token_past_the_size_is_refused() ->
    Pool = pool(),
    try
        Large = binary:copy(<<0>>, macula_realm_rights:max_token_bytes() + 1),
        ?assertEqual({error, too_large}, macula:present_membership(Pool, ?REALM, Large, [])),
        Proofs = lists:duplicate(macula_realm_rights:max_proofs() + 1, <<"proof">>),
        ?assertEqual({error, too_large}, macula:present_membership(Pool, ?REALM, <<"token">>, Proofs))
    after
        macula_client:close(Pool)
    end.

the_pool_presents_on_every_link() ->
    Pool = pool(),
    try
        {ok, Links} = macula_client:links(Pool),
        ok = macula:present_membership(Pool, ?REALM, <<"membership token">>, [<<"proof one">>]),
        [?assertEqual(#{?REALM => #{presented => ?PRESENTED, sent => false}}, memberships(Pid))
         || #{pid := Pid} <- Links],
        [#{pid := First} | _] = Links,
        exit(First, kill),
        Respawned = respawned(Pool, [Pid || #{pid := Pid} <- Links], 50),
        ?assertEqual(#{?REALM => #{presented => ?PRESENTED, sent => false}}, memberships(Respawned))
    after
        macula_client:close(Pool)
    end.

a_direct_dial_carries_the_membership() ->
    Pool = pool(),
    try
        {ok, Before} = macula_client:links(Pool),
        ok = macula:present_membership(Pool, ?REALM, <<"membership token">>, [<<"proof one">>]),
        {error, not_connected} =
            macula_client:call_station(Pool, #{host => <<"127.0.0.1">>, port => 3, expected_node_id => <<3:256>>},
                                       <<3:256>>, ?REALM, <<"x.y">>, #{}, 100, <<>>, #{}, 100, clear),
        {ok, After} = macula_client:links(Pool),
        [Direct] = [Pid || #{pid := Pid} <- After, is_pid(Pid)] -- [Pid || #{pid := Pid} <- Before],
        ?assertEqual(#{?REALM => #{presented => ?PRESENTED, sent => false}}, memberships(Direct))
    after
        macula_client:close(Pool)
    end.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A link to an unreachable seed, connected below to a station whose peer is the test's own process.
link() ->
    ok = flushed(),
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    {ok, Link} = macula_station_link:start_link(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                  connect_timeout_ms => 2000,
                                                  node_identity => fun() -> Key end, issuer => Issuer,
                                                  admission => Admission, share => {seed, {<<"127.0.0.1">>, 1}},
                                                  expected_node_id => <<1:256>>}),
    Link.

connected(Link) ->
    ok = flushed(),
    Peer = self(),
    _ = sys:replace_state(Link, fun(S) -> setelement(macula_station_link:state_field_index(peer_pid), S, Peer) end),
    Link ! {macula_peering, connected, Peer, <<20:256>>},
    _ = sys:get_state(Link),
    Link.

pool() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Pool} = macula_client:connect([#{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<1:256>>},
                                        #{host => <<"127.0.0.1">>, port => 2, expected_node_id => <<2:256>>}],
                                       #{node_identity => Key}),
    Pool.

sent(Type) ->
    receive {'$gen_cast', {send_frame, _, #{frame_type := Type} = Frame}} -> Frame
    after 1_000 -> error({not_sent, Type})
    end.

next_sent() ->
    receive {'$gen_cast', {send_frame, _, Frame}} -> Frame after 200 -> nothing end.

presented(#{ext := #{<<"realm.membership">> := #{critical := 0, value := #{<<"token">> := Token,
                                                                         <<"proofs">> := Proofs}}}}) ->
    {ok, #{token => Token, proofs => Proofs}};
presented(Frame) ->
    ?assertNot(is_map_key(ext, Frame)),
    none.

memberships(Link) ->
    element(macula_station_link:state_field_index(memberships), sys:get_state(Link)).

%% The link the pool started in place of the ones it held before.
respawned(_Pool, _Before, 0) ->
    error(not_respawned);
respawned(Pool, Before, N) ->
    {ok, Links} = macula_client:links(Pool),
    new_link([Pid || #{pid := Pid} <- Links, is_pid(Pid)] -- Before, Pool, Before, N).

new_link([Pid], _Pool, _Before, _N) -> Pid;
new_link([], Pool, Before, N) -> timer:sleep(100), respawned(Pool, Before, N - 1).

flushed() ->
    receive _ -> flushed() after 50 -> ok end.
