%% macula#44: the pool answers from its own state, never by calling into a
%% link. A link spends its time signing and verifying frames, and while it is
%% in a sign or verify it answers nothing; the pool used to ask every link
%% `is_connected' (and `peer_node_id') from inside its own handle_call, on
%% every publish and every `links/1' and `status/1', so busy links stalled
%% every pool call (live on beam02 mcl-mpong: 99 x mcl_om's 5 s
%% sign_node_record timing out). The pool now keeps each link's connectedness
%% and station node id from the link's own notice at handshake, and drops it
%% on the link's DOWN.
%%
%% Real macula_station_link workers against unreachable seeds, each told it is
%% connected to a distinct station with this test process as its peer. One is
%% then suspended (sys:suspend/1): it answers no call at all, as a link deep
%% in a long sign or verify does not.
-module(macula_client_link_state_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(STATION_A, <<21:256>>).
-define(STATION_B, <<22:256>>).
-define(SEEDS, [#{host => <<"127.0.0.1">>, port => 1, expected_node_id => ?STATION_A},
                #{host => <<"127.0.0.1">>, port => 2, expected_node_id => ?STATION_B}]).
%% A pool call answers well inside this while a link is busy; a probe into
%% the busy link alone waits 1 s.
-define(PROMPT_MS, 200).

pool_calls_answer_promptly_while_a_link_is_busy_test_() ->
    {timeout, 20,
     fun() ->
         {Pool, [_LinkA, LinkB]} = connected_pool(),
         ok = sys:suspend(LinkB),
         try
             {LinksMs, {ok, Links}} = timed(fun() -> macula_client:links(Pool) end),
             ?assert(LinksMs < ?PROMPT_MS),
             ?assertEqual([{?STATION_A, true}, {?STATION_B, true}],
                          lists:sort([{N, C} || #{node_id := N, connected := C} <- Links])),

             {StatusMs, {ok, #{healthy_links := Healthy}}} = timed(fun() -> macula_client:status(Pool) end),
             ?assert(StatusMs < ?PROMPT_MS),
             ?assertEqual(2, Healthy),

             %% A publish goes to every link, the busy one included: that
             %% caller waits for it, but the pool picks its targets from its
             %% own state and keeps answering everyone else meanwhile.
             Self = self(),
             _ = spawn(fun() ->
                           Self ! {published, macula_client:publish(Pool, ?REALM, <<"acme.tick_v1">>, #{n => 1}, #{})}
                       end),
             timer:sleep(50),
             {DuringPublishMs, {ok, _}} = timed(fun() -> macula_client:status(Pool) end),
             ?assert(DuringPublishMs < ?PROMPT_MS),

             %% A pool call queued behind another caller's `links/1' is not
             %% held by it either.
             _ = spawn(fun() -> Self ! {links_answered, macula_client:links(Pool)} end),
             {SignMs, {ok, _Signed}} =
                 timed(fun() -> macula_client:sign_domain_record(Pool, macula_record:envelope(16#20, #{}, #{})) end),
             ?assert(SignMs < ?PROMPT_MS),
             ?assertMatch({ok, _}, receive {links_answered, R} -> R after 2_000 -> none end)
         after
             ok = sys:resume(LinkB),
             ok = macula_client:close(Pool)
         end
     end}.

%% A link that dies is no longer counted connected: the state goes with the
%% link's DOWN, not only with its disconnect notice, which a crash never sends.
a_crashed_link_is_no_longer_connected_test_() ->
    {timeout, 20,
     fun() ->
         {Pool, [_LinkA, LinkB]} = connected_pool(),
         Mon = erlang:monitor(process, LinkB),
         exit(LinkB, kill),
         receive {'DOWN', Mon, process, LinkB, _} -> ok after 2_000 -> error(link_did_not_die) end,
         {ok, #{healthy_links := Healthy}} = macula_client:status(Pool),
         ?assertEqual(1, Healthy),
         {ok, Links} = macula_client:links(Pool),
         ?assertEqual([?STATION_A], [N || #{connected := true, node_id := N} <- Links]),
         %% And the pool holds nothing for the dead link: the state went with
         %% its DOWN, so it cannot grow with every link that ever crashed.
         Connected = element(macula_client:state_field_index(connected), sys:get_state(Pool)),
         ?assertNot(is_map_key(LinkB, Connected)),
         ok = macula_client:close(Pool)
     end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A pool of two links, each connected to its seed's station with this
%% process as the peer, in seed order; waits until the pool counts both.
connected_pool() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Pool} = macula_client:connect(?SEEDS, #{node_identity => Key}),
    {ok, Links} = macula_client:links(Pool),
    ByStation = maps:from_list([{maps:get(expected_node_id, Seed), Pid} || #{seed := Seed, pid := Pid} <- Links]),
    Pids = [connected(maps:get(Station, ByStation), Station) || Station <- [?STATION_A, ?STATION_B]],
    ok = healthy_within(Pool, 2, 50),
    {Pool, Pids}.

connected(Link, Station) ->
    Peer = self(),
    _ = sys:replace_state(Link, fun(S) ->
            setelement(macula_station_link:state_field_index(peer_pid), S, Peer)
        end),
    Link ! {macula_peering, connected, Peer, Station},
    Station = element(macula_station_link:state_field_index(peer_node_id), sys:get_state(Link)),
    Link.

healthy_within(_Pool, _N, 0) ->
    error(links_not_counted_connected);
healthy_within(Pool, N, Tries) ->
    {ok, #{healthy_links := H}} = macula_client:status(Pool),
    healthy_or_wait(H >= N, Pool, N, Tries).

healthy_or_wait(true, _Pool, _N, _Tries) -> ok;
healthy_or_wait(false, Pool, N, Tries) -> timer:sleep(20), healthy_within(Pool, N, Tries - 1).

timed(Fun) ->
    Start = erlang:monotonic_time(millisecond),
    Result = Fun(),
    {erlang:monotonic_time(millisecond) - Start, Result}.
