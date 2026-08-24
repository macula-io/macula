%% Live-boot check for hecate-om's mesh-wrappers plan (piece D): does a
%% subscription actually survive a link respawn, end to end, the way
%% `macula_client'/`macula_client_replay' claim in their moduledocs —
%% not just "the code reads that way"?
%%
%% No real QUIC station needed: same technique as `macula_client_tests'
%% (real macula_station_link workers against an unreachable seed) plus
%% `macula_pubsub_connect_selfheal_tests' (killing a link to force the
%% pool's DOWN/respawn path). meck traces the underlying
%% `macula_station_link:subscribe/4' calls to prove `subs_to/2' really
%% re-issues the subscription against the new link, not just that no
%% error was raised.
-module(macula_link_respawn_replay_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
-define(SEED, #{host => <<"127.0.0.1">>, port => 1}).
-define(TOPIC, <<"resp.test_v1">>).

subscription_survives_link_respawn_test_() ->
    {timeout, 10,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         ok = meck:new(macula_station_link, [passthrough]),
         {ok, Pool} = macula_client:connect([?SEED], #{}),

         {ok, SubRef} = macula_client:subscribe(Pool, ?REALM, ?TOPIC,
                                                self(), #{}),

         {ok, [#{pid := OldPid}]} = macula_client:links(Pool),

         %% Kill the link — pool's on_down_routed schedules a respawn.
         LinkMon = erlang:monitor(process, OldPid),
         exit(OldPid, kill),
         receive
             {'DOWN', LinkMon, process, OldPid, _} -> ok
         after 2_000 -> erlang:error(link_did_not_die)
         end,

         %% The claim under test: a mere link death must NOT be treated
         %% as subscription loss. No macula_event_gone for this SubRef.
         receive
             {macula_event_gone, SubRef, Reason} ->
                 erlang:error({unexpected_gone, Reason})
         after 1_500 -> ok
         end,

         %% Wait past ?LINK_RESPAWN_DELAY_MS (1s) for the new link.
         NewPid = wait_for_new_link(Pool, OldPid, 30),
         ?assertNotEqual(OldPid, NewPid),

         %% Prove subs_to/2 actually replayed the subscription onto the
         %% NEW link — not just "no crash happened".
         History = meck:history(macula_station_link),
         Replayed = [call || {_Pid, {macula_station_link, subscribe,
                                     [P, ?REALM, ?TOPIC, PP]}, _Res} <- History,
                              P =:= NewPid, PP =:= Pool],
         ?assert(length(Replayed) >= 1),

         %% And prove it end-to-end: an event arriving via the new link
         %% still reaches the original subscriber under the same SubRef,
         %% with no re-subscribe from us.
         Pool ! {macula_event, make_ref(), ?TOPIC, #{probe => true},
                 #{realm => ?REALM, publisher => <<1:256>>, seq => 1,
                   delivered_via => direct}},
         receive
             {macula_event, R, T, P, _Meta} ->
                 ?assertEqual(SubRef, R),
                 ?assertEqual(?TOPIC, T),
                 ?assertEqual(#{probe => true}, P)
         after 2_000 -> erlang:error(event_not_delivered_after_respawn)
         end,

         meck:unload(macula_station_link),
         ok = macula_client:close(Pool),
         ok
     end}.

wait_for_new_link(_Pool, _OldPid, 0) ->
    erlang:error(no_respawn_observed);
wait_for_new_link(Pool, OldPid, N) ->
    {ok, Links} = macula_client:links(Pool),
    case [P || #{pid := P} <- Links, P =/= OldPid, is_pid(P),
                is_process_alive(P)] of
        [New | _] -> New;
        []        -> timer:sleep(200), wait_for_new_link(Pool, OldPid, N - 1)
    end.
