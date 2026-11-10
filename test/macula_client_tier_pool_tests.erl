%% The two-tier rules driven through the POOL, not through the pure functions
%% (macula#201 step 1, Uranus r=1 finding 1): every rule below is asserted at
%% the place the pool uses it, so a mutation that bypasses `tier_targets/3',
%% `candidate_links/1', the RTT sort, the redial schedule, the steady-up reset
%% or the per-link advertisement body has nowhere left to hide.
%%
%% Real macula_station_link workers against unreachable seeds (the technique of
%% macula_client_per_link_advertise_tests): each is told it completed a
%% handshake with a station of its own, over a relay peer that tags every frame
%% it sends with the link it came from -- so an assertion can say WHICH link the
%% pool chose, not merely that some frame went out. Notices the links would send
%% (`macula_link_connected') are the links' own; the round trip and disconnect
%% notices are injected, since an unreachable seed has neither.
-module(macula_client_tier_pool_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(PROC, <<"acme.count_v1">>).
%% Nodes: 11 and 12 are public stations, 13 the LAN fallback.
-define(PUB_A_STATION, <<11:256>>).
-define(PUB_B_STATION, <<12:256>>).
-define(LAN_STATION, <<13:256>>).

publish_test_() ->
    {timeout, 30,
     [fun a_publish_reaches_only_the_active_tiers_links/0,
      fun a_publish_on_the_fallback_reaches_the_fallback_link/0,
      fun a_called_link_is_one_of_the_active_tier/0]}.

ranking_test_() ->
    {timeout, 20, [fun the_lowest_measured_round_trip_is_published_to_first/0]}.

advertisement_test_() ->
    {timeout, 30,
     [fun a_public_link_never_names_the_fallback_station/0,
      fun the_fallback_link_names_its_own_station/0]}.

status_test_() ->
    {timeout, 20,
     [fun the_status_names_the_tier_the_pool_is_on/0,
      fun every_link_names_its_configured_tier/0]}.

redial_test_() ->
    {timeout, 30,
     [fun a_tier_that_is_down_escalates_its_redial/0,
      fun a_link_that_stays_up_resets_its_backoff/0,
      fun a_link_that_dies_early_keeps_it/0]}.

rearm_test_() ->
    {timeout, 60,
     [fun a_fallback_retry_rearms_when_the_public_tier_dies/0,
      fun a_fallback_retry_rearms_when_the_public_tier_reports_down/0,
      fun a_public_retry_rearms_when_a_public_link_returns/0,
      fun an_unconfigured_public_retry_rearms_when_a_public_link_returns/0]}.

%%--------------------------------------------------------------------
%% Which links an originating operation may use
%%--------------------------------------------------------------------

%% §9 scenario 1 through the pool: with a public link up the pool is on the
%% public tier, so a publish goes to a public link and never to the LAN one.
%% The fallback link is measured FASTEST here, so only the tier filter can keep
%% the publish off it: any path that skipped the filter would choose it.
a_publish_reaches_only_the_active_tiers_links() ->
    Pool = pool(),
    #{public := Pub, lan := Lan} = connect_all(Pool),
    measured(Pool, Lan, 1),
    measured(Pool, hd(Pub), 1_000),
    measured(Pool, lists:last(Pub), 2_000),
    ?assertEqual(ok, macula_client:publish(Pool, ?REALM, <<"acme.things">>, #{<<"n">> => 1}, #{})),
    [Sent] = links_that_sent(Pool, publish),
    ?assert(lists:member(Sent, Pub)),
    ?assertNotEqual(Lan, Sent),
    ok = macula_client:close(Pool).

%% §9 scenario 3 through the pool: both public links down and the LAN link up
%% puts the pool on the fallback tier, and the publish lands there -- and only
%% there.
a_publish_on_the_fallback_reaches_the_fallback_link() ->
    Pool = pool(),
    #{public := Pub, lan := Lan} = connect_all(Pool),
    [disconnected(Pool, Pid) || Pid <- Pub],
    ?assertEqual(fallback, active_tier(Pool)),
    ?assertEqual(ok, macula_client:publish(Pool, ?REALM, <<"acme.things">>, #{<<"n">> => 2}, #{})),
    ?assertEqual([Lan], links_that_sent(Pool, publish)),
    ok = macula_client:close(Pool).

%% A call walks the candidate list link by link on a failure that means "not this
%% link" (`candidate' scope), so the tier filter has to hold there too. Here the
%% public link is one the pool BELIEVES is connected while the link itself never
%% completed a handshake: it answers `{error, not_connected}', which lets the
%% walk move on -- and the next link it may reach is the fallback one, which is
%% the whole point. A fallback link that was TRIED got the frame.
a_called_link_is_one_of_the_active_tier() ->
    Pool = pool(),
    %% The fallback link is really connected; the public one the pool believes is
    %% up never handshook, so a call there answers `{error, not_connected}'.
    _Lan = connect_to(Pool, link_for(Pool, 3), ?LAN_STATION),
    A = link_for(Pool, 1),
    reconnected(Pool, A, ?PUB_A_STATION),
    ?assertEqual(public, active_tier(Pool)),
    ?assertEqual({error, not_connected},
                 macula_client:call_linked_station(Pool, ?REALM, <<"acme.nobody">>, #{}, 150)),
    ?assertEqual([], links_that_sent(Pool, call)),
    ok = macula_client:close(Pool).

%%--------------------------------------------------------------------
%% §9 scenario 6 through the pool: measured round trip decides the target
%%--------------------------------------------------------------------

the_lowest_measured_round_trip_is_published_to_first() ->
    Pool = pool(),
    #{public := [A, B]} = connect_all(Pool),
    measured(Pool, A, 90_000),
    measured(Pool, B, 1_000),
    ?assertEqual(ok, macula_client:publish(Pool, ?REALM, <<"acme.things">>, #{<<"n">> => 3}, #{})),
    ?assertEqual([B], links_that_sent(Pool, publish)),
    %% Swapping the measurements swaps the choice: the order is the
    %% measurement, not the seed list (A is dialled first).
    measured(Pool, A, 500),
    measured(Pool, B, 80_000),
    ?assertEqual(ok, macula_client:publish(Pool, ?REALM, <<"acme.things">>, #{<<"n">> => 4}, #{})),
    ?assertEqual([A], links_that_sent(Pool, publish)),
    ok = macula_client:close(Pool).

%%--------------------------------------------------------------------
%% The advertisement body a link signs, by its own tier (amendment 2)
%%--------------------------------------------------------------------

%% A public link's body names public stations only: the fallback station's node
%% id never reaches a record a public station stores.
a_public_link_never_names_the_fallback_station() ->
    Pool = pool(),
    #{public := Pub} = connect_all(Pool),
    Bodies = advertisements(Pool),
    Named = [maps:get(serving_stations, Body) || {Pid, Body} <- Bodies, lists:member(Pid, Pub)],
    ?assertEqual(2, length(Named)),
    [?assertEqual(lists:sort([?PUB_A_STATION, ?PUB_B_STATION]), lists:sort(N)) || N <- Named],
    ok = macula_client:close(Pool).

%% The fallback station's own body is what a LAN caller resolves during an
%% outage, so it names itself AND a public station the caller can dial.
the_fallback_link_names_its_own_station() ->
    Pool = pool(),
    #{lan := Lan} = connect_all(Pool),
    Bodies = advertisements(Pool),
    [FallbackBody] = [Body || {Pid, Body} <- Bodies, Pid =:= Lan],
    ?assertEqual(lists:sort([?PUB_A_STATION, ?PUB_B_STATION, ?LAN_STATION]),
                 lists:sort(maps:get(serving_stations, FallbackBody))),
    ok = macula_client:close(Pool).

%%--------------------------------------------------------------------
%% What the pool says about its own tiers
%%--------------------------------------------------------------------

the_status_names_the_tier_the_pool_is_on() ->
    Pool = pool(),
    #{public := [A | _] = Pub, lan := Lan} = connect_all(Pool),
    ?assertEqual(public, active_tier(Pool)),
    [disconnected(Pool, Pid) || Pid <- Pub],
    ?assertEqual(fallback, active_tier(Pool)),
    %% A handshake brings the public tier back on its own: there is no timer to
    %% wait out, and the return does not need the fallback to go first.
    reconnected(Pool, A, ?PUB_A_STATION),
    ?assertEqual(public, active_tier(Pool)),
    ?assert(is_process_alive(Lan)),
    ok = macula_client:close(Pool).

every_link_names_its_configured_tier() ->
    Pool = pool(),
    _ = connect_all(Pool),
    {ok, Links} = macula_client:links(Pool),
    Tiers = maps:from_list([{port(Seed), Tier} || #{seed := Seed, tier := Tier} <- Links]),
    ?assertEqual(#{1 => public, 2 => public, 3 => fallback}, Tiers),
    ok = macula_client:close(Pool).

%%--------------------------------------------------------------------
%% §9 scenario 4: the redial schedule, at the pool that runs it
%%--------------------------------------------------------------------

%% A seed whose tier is NOT the active one retries on the schedule: each failed
%% attempt is counted, which is what makes the wait grow. A pool on the flat
%% delay for everything would count nothing.
a_tier_that_is_down_escalates_its_redial() ->
    Pool = pool(),
    #{lan := Lan} = connect_all(Pool),
    ?assertEqual(0, attempts(Pool, 3)),
    kill(Lan),
    ?assertEqual(1, attempts(Pool, 3)),
    kill(wait_for_link(Pool, 3)),
    ?assertEqual(2, attempts(Pool, 3)),
    ok = macula_client:close(Pool).

%% A link that stays up past the steady-up window has proved itself: its seed's
%% backoff starts again from the base, so the next failure counts from one.
a_link_that_stays_up_resets_its_backoff() ->
    Pool = pool(),
    #{lan := Lan} = connect_all(Pool),
    kill(Lan),
    ?assertEqual(1, attempts(Pool, 3)),
    _ = connect_to(Pool, wait_for_link(Pool, 3), ?LAN_STATION),
    timer:sleep(steady_up_ms() + 200),
    ?assertEqual(0, attempts(Pool, 3)),
    ok = macula_client:close(Pool).

%% The other half of the same rule: a link that connects and dies before the
%% window elapses resets nothing, so a connect-then-die loop escalates instead
%% of retrying at the base forever.
%%
%% The death is timed off a wait that is well INSIDE the window (a third of it)
%% and far outside any reset that fired at the handshake: a pool that reset the
%% moment a link connected cannot pass this by winning a race with the kill.
a_link_that_dies_early_keeps_it() ->
    Pool = pool(),
    #{lan := Lan} = connect_all(Pool),
    kill(Lan),
    ?assertEqual(1, attempts(Pool, 3)),
    Early = connect_to(Pool, wait_for_link(Pool, 3), ?LAN_STATION),
    timer:sleep(steady_up_ms() div 3),
    ?assertEqual(1, attempts(Pool, 3)),
    kill(Early),
    ?assertEqual(2, attempts(Pool, 3)),
    ok = macula_client:close(Pool).

%%--------------------------------------------------------------------
%% §3's re-arm rule: a tier that BECOMES the active one does not inherit the
%% wait the other tier earned. The pending retry restarts at the redial base and
%% its counter starts again from zero; the tier that just went keeps the wait it
%% earned, which is what makes an outage cheap.
%
%% All three tests use a schedule whose fourth failed retry waits at least 2 s,
%% and then let no more than 800 ms pass: "re-armed" and "left to run out" are
%% two orders of magnitude apart, so none of them can pass by winning a race.
%%--------------------------------------------------------------------

%% The public tier dies for real -- its links' DOWN, no goodbye notice -- while
%% the fallback seed waits out a backoff it earned behind an up public tier.
a_fallback_retry_rearms_when_the_public_tier_dies() ->
    Pool = rearm_pool(),
    #{public := Pub} = connect_all(Pool),
    ok = escalated(Pool, 3, 4),
    [kill(Pid) || Pid <- Pub],
    ok = await_tier(Pool, fallback),
    ?assert(is_pid(wait_for_link(Pool, 3, 32))),
    ?assertEqual(0, attempts(Pool, 3)),
    ok = macula_client:close(Pool).

%% The same flip through the notice a link sends just before it stops, which the
%% pool handles BEFORE that link's DOWN: a re-arm hooked only on the DOWN would
%% find the tier already changed and never restart the retry.
a_fallback_retry_rearms_when_the_public_tier_reports_down() ->
    Pool = rearm_pool(),
    #{public := Pub} = connect_all(Pool),
    ok = escalated(Pool, 3, 4),
    [disconnected(Pool, Pid) || Pid <- Pub],
    ok = await_tier(Pool, fallback),
    ?assert(is_pid(wait_for_link(Pool, 3, 32))),
    ?assertEqual(0, attempts(Pool, 3)),
    ok = macula_client:close(Pool).

%% And back the other way: a public link completing its handshake makes the
%% public tier active again, so a public seed waiting out a long retry is
%% dialled again on the base rather than left on the other tier's schedule.
a_public_retry_rearms_when_a_public_link_returns() ->
    Pool = rearm_pool(),
    #{public := [A | _] = Pub} = connect_all(Pool),
    kill(A),
    [disconnected(Pool, Pid) || Pid <- tl(Pub)],
    ok = await_tier(Pool, fallback),
    ok = escalated(Pool, 1, 4),
    reconnected(Pool, lists:last(Pub), ?PUB_B_STATION),
    ok = await_tier(Pool, public),
    ?assert(is_pid(wait_for_link(Pool, 1, 32))),
    ?assertEqual(0, attempts(Pool, 1)),
    ok = macula_client:close(Pool).

%% The same switch for a seed the pool was never configured with. A direct dial
%% (`ensure_station_link/4') puts a link in the pool without an entry in its
%% `roles', and everywhere else -- `respawn_plan/2', `active_tier/1',
%% `link_tiers/1' -- such a seed counts as public. The re-arm must count it the
%% same way: one rule, one answer. Before that, this seed alone kept the wait
%% the outage earned, because only `roles' was consulted for the pending list.
an_unconfigured_public_retry_rearms_when_a_public_link_returns() ->
    Pool = rearm_pool(),
    #{public := [A | _] = Pub} = connect_all(Pool),
    kill(A),
    [disconnected(Pool, Pid) || Pid <- tl(Pub)],
    ok = await_tier(Pool, fallback),
    ?assertEqual({error, not_connected},
                 macula_client:ensure_station_link(Pool, seed(4, 4), #{}, 200)),
    ok = escalated(Pool, 4, 4),
    reconnected(Pool, lists:last(Pub), ?PUB_B_STATION),
    ok = await_tier(Pool, public),
    ?assert(is_pid(wait_for_link(Pool, 4, 32))),
    ?assertEqual(0, attempts(Pool, 4)),
    ok = macula_client:close(Pool).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% Kill the seed's link until the pool counts `Target' backoff attempts on it,
%% waiting out the retry the pool arms between one kill and the next.
escalated(Pool, Port, 1) ->
    kill(held_link(Pool, Port)),
    counted(Pool, Port, 1);
escalated(Pool, Port, Target) ->
    ok = escalated(Pool, Port, Target - 1),
    kill(held_link(Pool, Port)),
    counted(Pool, Port, Target).

held_link(Pool, Port) -> wait_for_link(Pool, Port, 400).

counted(_Pool, _Port, _N, 0) ->
    error(attempt_never_counted);
counted(Pool, Port, N, Tries) ->
    case attempts(Pool, Port) of
        N -> ok;
        _ -> timer:sleep(25), counted(Pool, Port, N, Tries - 1)
    end.

counted(Pool, Port, N) -> counted(Pool, Port, N, 200).

await_tier(_Pool, _Tier, 0) ->
    error(tier_never_changed);
await_tier(Pool, Tier, Tries) ->
    case active_tier(Pool) of
        Tier -> ok;
        _ -> timer:sleep(25), await_tier(Pool, Tier, Tries - 1)
    end.

await_tier(Pool, Tier) -> await_tier(Pool, Tier, 40).

%% A pool on the public tier with a LAN fallback: two public seeds, one
%% fallback seed, all unreachable, and a redial schedule short enough to watch.
pool() -> pool(redial()).

pool(Redial) ->
    ok = flushed(),
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Pool} = macula_client:connect([seed(1, 1), seed(2, 2)],
                                       #{node_identity => Key,
                                         fallback_stations => [seed(3, 3)],
                                         replication_factor => 1,
                                         redial => Redial}),
    Pool.

redial() ->
    #{base_ms => 50, factor => 2, cap_ms => 1_000, steady_up_ms => steady_up_ms()}.

%% The re-arm tests' schedule: a floor of a couple of milliseconds to watch by,
%% and a growth steep enough that the fourth failed retry waits at least 2 s.
rearm_pool() ->
    pool(rearm_redial()).

rearm_redial() ->
    #{base_ms => 2, factor => 10, cap_ms => 60_000, steady_up_ms => steady_up_ms()}.

steady_up_ms() -> 400.

seed(Port, N) ->
    #{host => <<"127.0.0.1">>, port => Port, expected_node_id => <<N:256>>}.

port(Seed) -> maps:get(port, Seed).

%% Every link connected to a station of its own, as a completed handshake leaves
%% it: this also tells the POOL, which is what makes the tiers live.
connect_all(Pool) ->
    Pub = [connect_to(Pool, link_for(Pool, 1), ?PUB_A_STATION),
           connect_to(Pool, link_for(Pool, 2), ?PUB_B_STATION)],
    Lan = connect_to(Pool, link_for(Pool, 3), ?LAN_STATION),
    #{public => Pub, lan => Lan}.

%% The link of the seed on `Port', from the pool's own report.
link_for(Pool, Port) ->
    {ok, Links} = macula_client:links(Pool),
    [#{pid := Pid}] = [L || #{seed := Seed} = L <- Links, port(Seed) =:= Port],
    Pid.

%% A link that has respawned since the one this test had.
wait_for_link(Pool, Port) ->
    wait_for_link(Pool, Port, 40).

wait_for_link(_Pool, Port, 0) ->
    error({no_link, Port});
wait_for_link(Pool, Port, N) ->
    case [Pid || #{seed := Seed, pid := Pid} <- links(Pool), port(Seed) =:= Port, is_pid(Pid)] of
        [Pid | _] -> Pid;
        [] -> timer:sleep(25), wait_for_link(Pool, Port, N - 1)
    end.

links(Pool) ->
    {ok, Links} = macula_client:links(Pool),
    Links.

%% Make `Pid' believe it completed a handshake with `Station', over a peer that
%% tags every frame it sends with the link it came from -- so a test can tell
%% WHICH link the pool chose. The link tells its pool itself (`pool_connected'),
%% and the pool's answer is what the assertions then read, not the send.
connect_to(Pool, Pid, Station) ->
    Test = self(),
    Peer = spawn(fun() -> relay(Test, Pid) end),
    _ = sys:replace_state(Pid, fun(S) ->
            setelement(macula_station_link:state_field_index(peer_pid), S, Peer)
        end),
    Pid ! {macula_peering, connected, Peer, Station},
    Station = element(macula_station_link:state_field_index(peer_node_id), sys:get_state(Pid)),
    connected(Pool, Pid, Station),
    Pid.

%% Tagger for one link: every frame the link sends carries the link it came
%% from. It ends with its link, so a closed pool leaves no relay behind.
relay(Test, Link) ->
    relay(Test, Link, erlang:monitor(process, Link)).

relay(Test, Link, Mon) ->
    receive
        {'$gen_cast', {send_frame, _Peer, Frame}} ->
            Test ! {frame_from, Link, Frame},
            relay(Test, Link, Mon);
        {'DOWN', Mon, process, Link, _} ->
            ok;
        _Other ->
            relay(Test, Link, Mon)
    end.

%% Wait until the pool reports this link connected to this station.
connected(Pool, Pid, Station) ->
    connected(Pool, Pid, Station, 40).

connected(_Pool, _Pid, _Station, 0) ->
    error(link_never_connected);
connected(Pool, Pid, Station, N) ->
    case [L || #{pid := P} = L <- links(Pool), P =:= Pid,
                maps:get(node_id, L) =:= Station, maps:get(connected, L)] of
        [_] -> ok;
        [] -> timer:sleep(25), connected(Pool, Pid, Station, N - 1)
    end.

%% The pool's view of a link that ended, without the link itself ending: the
%% notice a link sends just before it stops, injected here because these links
%% do not stop.
disconnected(Pool, Pid) ->
    Pool ! {macula_link_disconnected, Pid, #{reason => <<"test_end">>}},
    wait_disconnected(Pool, Pid, 40).

wait_disconnected(_Pool, _Pid, 0) ->
    error(link_never_disconnected);
wait_disconnected(Pool, Pid, N) ->
    case [L || #{pid := P} = L <- links(Pool), P =:= Pid, not maps:get(connected, L)] of
        [_] -> ok;
        [] -> timer:sleep(25), wait_disconnected(Pool, Pid, N - 1)
    end.

%% The pool's handshake notice for a link it already holds: what a returned link
%% reports after a completed handshake, injected here so the test does not need
%% the link to dial again.
reconnected(Pool, Pid, Station) ->
    Pool ! {macula_link_connected, Pid, Station},
    connected(Pool, Pid, Station).

%% The pool's round trip notice for a link it already holds.
measured(Pool, Pid, Micros) ->
    true = is_process_alive(Pid),
    Pool ! {macula_link_rtt, Pid, {ok, Micros}},
    wait_rtt(Pool, Pid, Micros, 40).

wait_rtt(_Pool, _Pid, _Micros, 0) ->
    error(rtt_never_kept);
wait_rtt(Pool, Pid, Micros, N) ->
    Rtts = element(macula_client:state_field_index(rtts), sys:get_state(Pool)),
    case maps:get(Pid, Rtts, undefined) of
        Micros -> ok;
        _ -> timer:sleep(25), wait_rtt(Pool, Pid, Micros, N - 1)
    end.

active_tier(Pool) ->
    {ok, Status} = macula_client:status(Pool),
    maps:get(active_tier, Status).

%% How many backoff attempts the pool counts for the seed on `Port': zero when
%% the seed has none, which is also what a steady-up reset leaves behind.
attempts(Pool, Port) ->
    Attempts = element(macula_client:state_field_index(redial_attempts), sys:get_state(Pool)),
    maps:get(seed(Port, Port), Attempts, 0).    %% this module seeds port N with node id N

kill(Pid) ->
    Mon = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive {'DOWN', Mon, process, Pid, _} -> ok
    after 2_000 -> error(link_did_not_die)
    end.

%% Every link that sent a frame of `Type' to this process, since the last
%% frames were read.
links_that_sent(Pool, Type) ->
    Sent = [Pid || {Pid, #{frame_type := T}} <- frames(), T =:= Type],
    %% A frame from a pid this pool no longer holds is an earlier test's tail.
    Held = [Pid || #{pid := Pid} <- links(Pool)],
    lists:usort([Pid || Pid <- Sent, lists:member(Pid, Held)]).

frames() ->
    frames(400).

frames(Timeout) ->
    receive
        {frame_from, Pid, Frame} -> [{Pid, Frame} | frames(Timeout)]
    after Timeout -> []
    end.

%% Every ADVERTISE frame the registration sent, decoded: the link that signed it
%% and the body, whose `serving_stations' is what the assertion reads.
advertisements(Pool) ->
    flushed(),
    ok = macula_client:advertise(Pool, ?REALM, ?PROC, fun(_) -> {ok, counted} end, open, spec()),
    {ok, Profile} = macula_crypto_profile:configured(),
    [{Pid, advertisement_body(Profile, Frame)}
     || {Pid, #{frame_type := advertise} = Frame} <- frames()].

advertisement_body(Profile, #{advertisement := Encoded}) ->
    {ok, Record} = macula_record:verify(Encoded, Profile),
    macula_record:read_procedure_advertisement(Record).

spec() ->
    #{authorization => #{org_directory => <<"org directory wire">>,
                         procedure_delegation => <<"delegation wire">>},
      not_after => erlang:system_time(millisecond) + 3_600_000}.

%% An earlier test's links can still put from their own processes after this
%% one started: drained until the mailbox has been quiet for 200 ms.
flushed() ->
    receive {frame_from, _, _} -> flushed() after 200 -> ok end.
