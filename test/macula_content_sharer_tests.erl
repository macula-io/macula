%% EUnit tests for `macula_content_sharer': the process that shares a pool's content (D27). It keeps the content,
%% serves it on the node's own content procedure, and announces each root in the DHT naming the realm, the station the
%% node is reachable through now and the procedure; it renews the announcements, announces again when the station
%% changes, and withdraws them when the content is unshared. The mesh is replaced by an io map that records what the
%% sharer asked of it.
-module(macula_content_sharer_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(STATION_A, <<16#A1:256>>).
-define(STATION_B, <<16#B2:256>>).

sharer_test_() ->
    {foreach,
     fun() -> {ok, _} = application:ensure_all_started(macula), ok end,
     fun(_) -> ok end,
     [{"sharing registers the node's own-namespace procedure once and announces the root",
       fun() -> cleaned(fun sharing_registers_and_announces/0) end},
      {"an org names the procedure <org>/content_v1_<hex>", fun() -> cleaned(fun an_org_names_the_procedure/0) end},
      {"a second share in the realm announces without registering again", fun() -> cleaned(fun a_second_share_registers_nothing/0) end},
      {"what is shared is served, and what is not is not", fun() -> cleaned(fun what_is_shared_is_served/0) end},
      {"unsharing withdraws the announcement, and the last one drops the procedure",
       fun() -> cleaned(fun unsharing_withdraws/0) end},
      {"a new station is announced", fun() -> cleaned(fun a_new_station_is_announced/0) end},
      {"announcements are renewed before they expire", fun() -> cleaned(fun announcements_are_renewed/0) end},
      {"with no station connected the content is kept and announced once one is",
       fun() -> cleaned(fun no_station_yet_announces_later/0) end},
      {"the sharer ends with its pool", fun() -> cleaned(fun the_sharer_ends_with_its_pool/0) end},
      {"a pool with no station yet keeps the registration", fun() -> cleaned(fun no_station_keeps_registration/0) end},
      {"a renewal due while no station is connected is made once one is",
       fun() -> cleaned(fun a_renewal_due_in_an_outage_is_made_after_it/0) end},
      {"a realm serves only what is shared in it", fun() -> cleaned(fun a_realm_serves_only_its_own/0) end},
      {"sharing and serving do not wait on announcing", fun() -> cleaned(fun serving_does_not_wait_on_announcing/0) end},
      {"an announcement the DHT did not take is made again at the next station check",
       fun() -> cleaned(fun a_failed_put_is_made_again/0) end},
      {"unsharing while an announcement is in flight withdraws that announcement",
       fun() -> cleaned(fun unsharing_withdraws_an_announcement_in_flight/0) end}]}.

%% A station roll returns the same station: a renewal that fell in the outage must still be made, or the announcement
%% expires while the content is shared and its node online.
a_renewal_due_in_an_outage_is_made_after_it() ->
    {Sharer, Io, _Node} = sharer(#{announce_ttl_ms => 400, station_check_ms => 50}),
    {ok, MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    ok = wait_until(fun() -> length(announced_stations(Io, MCID)) =:= 1 end, 1_000),
    set_station(Io, none),
    timer:sleep(400),
    Before = length(announced_stations(Io, MCID)),
    set_station(Io, ?STATION_A),
    ok = wait_until(fun() -> length(announced_stations(Io, MCID)) > Before end, 1_000).

a_realm_serves_only_its_own() ->
    Other = <<8:256>>,
    {Sharer, Io, _Node} = sharer(#{}),
    {ok, MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    ?assertEqual(not_found, macula_content_sharer:lookup(Sharer, Other, root, MCID)),
    {ok, MCID} = macula_content_sharer:share(Sharer, Other, <<"hello">>, #{}),
    ok = wait_until(fun() -> lists:member(Other, announced_realms(Io, MCID)) end, 1_000),
    ok = macula_content_sharer:unshare(Sharer, ?REALM, MCID),
    ?assertEqual(not_found, macula_content_sharer:lookup(Sharer, ?REALM, root, MCID)),
    ?assertMatch({ok, #{kind := block}}, macula_content_sharer:lookup(Sharer, Other, root, MCID)),
    ?assertEqual([?REALM], [maps:get(realm_id, macula_record:read_content_announcement(W))
                            || {withdraw, W} <- asked(Io, withdraw)]).

serving_does_not_wait_on_announcing() ->
    {Sharer, _Io, _Node} = sharer(#{put_record => fun(_P, _Signed) -> timer:sleep(3_000), ok end}),
    T0 = erlang:monotonic_time(millisecond),
    {ok, MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    {ok, _} = macula_content_sharer:share(Sharer, ?REALM, <<"second">>, #{}),
    ?assertMatch({ok, _}, macula_content_sharer:lookup(Sharer, ?REALM, root, MCID)),
    ?assert(erlang:monotonic_time(millisecond) - T0 < 1_000).

a_failed_put_is_made_again() ->
    Test = self(),
    Tries = counters:new(1, []),
    Put = fun(_P, Signed) ->
              counters:add(Tries, 1, 1),
              Test ! {put, Signed},
              put_answer(counters:get(Tries, 1))
          end,
    {Sharer, _Io, _Node} = sharer(#{put_record => Put, station_check_ms => 50}),
    {ok, _MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    ok = wait_until(fun() -> counters:get(Tries, 1) >= 2 end, 1_000).

put_answer(1) -> {error, no_route};
put_answer(_) -> ok.

unsharing_withdraws_an_announcement_in_flight() ->
    Test = self(),
    Put = fun(_P, Signed) -> Test ! {putting, self(), Signed}, receive go -> ok end end,
    {Sharer, Io, _Node} = sharer(#{put_record => Put}),
    {ok, MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    {Putter, InFlight} = receive {putting, P, R} -> {P, R} after 1_000 -> error(no_put) end,
    ok = macula_content_sharer:unshare(Sharer, ?REALM, MCID),
    Putter ! go,
    ok = wait_until(fun() -> [W || {withdraw, W} <- asked(Io, withdraw), W =:= InFlight] =/= [] end, 1_000).

%% The pool keeps a registration no link could take yet, and replays it when one comes up: the share stands.
no_station_keeps_registration() ->
    {Sharer, _Io, _Node} = sharer(#{advertise_stream => fun(_P, _R, _Pr, _M, _H, _O) -> {error, no_healthy_station} end}),
    ?assertMatch({ok, _}, macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{})).

sharing_registers_and_announces() ->
    {Sharer, Io, Node} = sharer(#{}),
    {ok, MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    Own = <<"~", (hex(Node))/binary, "/content_v1">>,
    ?assertEqual([{advertise_stream, ?REALM, Own, server_stream}], asked(Io, advertise_stream)),
    ok = wait_until(fun() -> asked(Io, put_record) =/= [] end, 1_000),
    [{put_record, Announcement}] = asked(Io, put_record),
    ?assertMatch(#{announcer_node := Node, mcid := MCID, realm_id := ?REALM, serving_station := ?STATION_A,
                   procedure := Own, size := 5},
                 macula_record:read_content_announcement(Announcement)).

an_org_names_the_procedure() ->
    {Sharer, Io, Node} = sharer(#{}),
    {ok, _} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{org => <<"acme">>}),
    Proc = <<"acme/content_v1_", (hex(Node))/binary>>,
    ?assertEqual([{advertise_stream, ?REALM, Proc, server_stream}], asked(Io, advertise_stream)).

a_second_share_registers_nothing() ->
    {Sharer, Io, _Node} = sharer(#{}),
    {ok, A} = macula_content_sharer:share(Sharer, ?REALM, <<"a">>, #{}),
    {ok, B} = macula_content_sharer:share(Sharer, ?REALM, <<"b">>, #{}),
    ?assertEqual(1, length(asked(Io, advertise_stream))),
    ok = wait_until(fun() -> length(asked(Io, put_record)) =:= 2 end, 1_000),
    ?assertEqual(lists:sort([A, B]), lists:sort([maps:get(mcid, macula_record:read_content_announcement(R))
                                                  || {put_record, R} <- asked(Io, put_record)])).

what_is_shared_is_served() ->
    {Sharer, _Io, _Node} = sharer(#{}),
    {ok, MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    ?assertEqual({ok, #{kind => block, mcid => MCID, bytes => <<"hello">>}},
                 macula_content_sharer:lookup(Sharer, ?REALM, root, MCID)),
    ?assertEqual(not_found, macula_content_sharer:lookup(Sharer, ?REALM, root, <<2, 16#55, 0:384>>)).

unsharing_withdraws() ->
    {Sharer, Io, _Node} = sharer(#{}),
    {ok, A} = macula_content_sharer:share(Sharer, ?REALM, <<"a">>, #{}),
    {ok, B} = macula_content_sharer:share(Sharer, ?REALM, <<"b">>, #{}),
    ok = wait_until(fun() -> length(asked(Io, put_record)) =:= 2 end, 1_000),
    ok = macula_content_sharer:unshare(Sharer, ?REALM, A),
    [{withdraw, Withdrawn}] = asked(Io, withdraw),
    ?assertEqual(A, maps:get(mcid, macula_record:read_content_announcement(Withdrawn))),
    ?assertEqual(not_found, macula_content_sharer:lookup(Sharer, ?REALM, root, A)),
    ?assertEqual([], asked(Io, unadvertise_stream)),
    ok = macula_content_sharer:unshare(Sharer, ?REALM, B),
    ?assertMatch([{unadvertise_stream, ?REALM, _}], asked(Io, unadvertise_stream)),
    ?assertEqual(ok, macula_content_sharer:unshare(Sharer, ?REALM, B)).

a_new_station_is_announced() ->
    {Sharer, Io, _Node} = sharer(#{station_check_ms => 50}),
    {ok, MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    set_station(Io, ?STATION_B),
    ok = wait_until(fun() -> lists:member(?STATION_B, announced_stations(Io, MCID)) end, 2_000).

announcements_are_renewed() ->
    {Sharer, Io, _Node} = sharer(#{announce_ttl_ms => 400}),
    {ok, MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    ok = wait_until(fun() -> length(announced_stations(Io, MCID)) >= 3 end, 2_000).

no_station_yet_announces_later() ->
    {Sharer, Io, _Node} = sharer(#{station_check_ms => 50}),
    set_station(Io, none),
    {ok, MCID} = macula_content_sharer:share(Sharer, ?REALM, <<"hello">>, #{}),
    ?assertEqual([], asked(Io, put_record)),
    ?assertMatch({ok, _}, macula_content_sharer:lookup(Sharer, ?REALM, root, MCID)),
    set_station(Io, ?STATION_A),
    ok = wait_until(fun() -> announced_stations(Io, MCID) =/= [] end, 2_000).

the_sharer_ends_with_its_pool() ->
    Pool = spawn(fun() -> receive stop -> ok end end),
    {Sharer, _Io, _Node} = sharer(#{}, Pool),
    Mon = erlang:monitor(process, Sharer),
    Pool ! stop,
    receive {'DOWN', Mon, process, Sharer, _} -> ok after 2_000 -> error(sharer_outlived_its_pool) end.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

sharer(Opts) ->
    sharer(Opts, spawn(fun() -> receive stop -> ok end end)).

%% A sharer on a stand-in pool, with an io map recording each request, signing with a real key of the node.
sharer(Opts, Pool) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Node} = macula_node_keys:node_id(Key),
    Io = ets:new(io, [public, bag]),
    ets:insert(Io, {station, ?STATION_A}),
    Rec = fun(Entry) -> ets:insert(Io, {asked, erlang:unique_integer([monotonic]), Entry}), ok end,
    IoMap = #{status => fun(_P) -> {ok, #{self_node_id => Node}} end,
              links => fun(_P) -> {ok, station_links(Io)} end,
              advertise_stream => fun(_P, Realm, Proc, Mode, _Handler, _O) ->
                                      Rec({advertise_stream, Realm, Proc, Mode}) end,
              unadvertise_stream => fun(_P, Realm, Proc) -> Rec({unadvertise_stream, Realm, Proc}) end,
              sign_node_record => fun(_P, Unsigned, _O) -> {ok, macula_record:refresh(Unsigned, Key)} end,
              put_record => fun(_P, Signed) -> Rec({put_record, Signed}) end,
              withdraw_node_record => fun(_P, Withdrawn, _Reason) -> Rec({withdraw, Withdrawn}),
                                                                       {ok, Withdrawn} end},
    {ok, Sharer} = macula_content_sharer:start_link(Pool, maps:merge(IoMap, Opts)),
    put(started, [Sharer, Pool | get_or_empty(started)]),
    {Sharer, Io, Node}.

get_or_empty(Key) ->
    case get(Key) of undefined -> []; L -> L end.

%% Run a test body, then stop the sharers and stand-in pools it started, so none outlives its io table.
cleaned(Body) ->
    try Body()
    after
        [begin unlink(P), exit(P, kill) end || P <- get_or_empty(started)],
        erase(started)
    end.

station_links(Io) ->
    case ets:lookup(Io, station) of
        [{station, none}] -> [];
        [{station, S}] -> [#{connected => true, node_id => S}]
    end.

set_station(Io, Station) ->
    ets:delete(Io, station),
    ets:insert(Io, {station, Station}).

asked(Io, Kind) ->
    [Entry || {asked, _, Entry} <- lists:keysort(2, ets:lookup(Io, asked)), element(1, Entry) =:= Kind].

announced_realms(Io, MCID) ->
    [R || {put_record, Rec} <- asked(Io, put_record),
          #{mcid := M, realm_id := R} <- [macula_record:read_content_announcement(Rec)], M =:= MCID].

announced_stations(Io, MCID) ->
    [S || {put_record, R} <- asked(Io, put_record),
          #{mcid := M, serving_station := S} <- [macula_record:read_content_announcement(R)], M =:= MCID].

hex(Node) -> binary:encode_hex(Node, lowercase).

wait_until(_Check, Left) when Left =< 0 -> timeout;
wait_until(Check, Left) ->
    case Check() of
        true -> ok;
        false -> timer:sleep(50), wait_until(Check, Left - 50)
    end.
