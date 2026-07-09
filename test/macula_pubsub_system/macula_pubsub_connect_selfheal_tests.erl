%% Regression tests for the `macula_station_link' connect watchdog —
%% the bounded, automatic self-heal for a link whose CONNECT/HELLO
%% handshake never completes.
%%
%% == Why this exists ==
%%
%% A subscriber (e.g. the realm's ClankerCab fleet subscriber) owns one
%% `macula_station_link' per relay. When the mesh churns (producer /
%% relay redeploy) the links drop and respawn. Live triage of the
%% clankercab-dark outage (2026-07-09) found EVERY realm link stuck
%% `alive-but-not-connected': `peer_pid' set to a live peering worker,
%% `peer_node_id' still undefined, 16 subscriptions queued, wedged since
%% boot with no `:DOWN' and no reconnect. The peering worker was blocked
%% in the QUIC dial NIF (which hung past its own timeout); nothing on the
%% link bounded the un-connected phase, so the handshake never completed,
%% never failed, and the owner was never told to reconnect — no
%% self-heal, dashboard dark indefinitely.
%%
%% The app-liveness probe does NOT cover this: it only arms AFTER
%% `connected'. The connect watchdog is the missing bound on the phase
%% BEFORE `connected'. On fire (still un-connected) it kills the wedged
%% worker and stops the link so the owner respawns a fresh dial —
%% bounded, automatic self-heal.
-module(macula_pubsub_connect_selfheal_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).
%% #state record layout — peer_pid at 7, peer_node_id at 8 (see
%% macula_station_link_tests.erl for the full map). These are the only
%% fields these tests patch, so the numeric coupling stays minimal.
-define(PEER_PID_INDEX, 7).
-define(PEER_NODE_INDEX, 8).

%%------------------------------------------------------------------
%% A wedged (never-connected) link recycles itself when the watchdog
%% fires: the wedged peering worker is killed, the link stops (so its
%% owner respawns a fresh dial), and queued subscribers are released
%% with `macula_event_gone'.
%%------------------------------------------------------------------

connect_watchdog_recycles_wedged_link_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         %% Short watchdog + a dead seed: the link never really connects.
         {ok, Pid} = macula_station_link:start_link(#{
             seed                => #{host => <<"127.0.0.1">>, port => 1},
             identity            => Identity,
             connect_watchdog_ms => 200
         }),

         %% A subscriber queued while the link is still un-connected.
         Test = self(),
         _SubProc = spawn(fun() ->
             {ok, SubRef} = macula_station_link:subscribe(
                              Pid, ?REALM, <<"fleet/leuven/summary">>, self()),
             Test ! {subbed, SubRef},
             receive
                 {macula_event_gone, R, Reason} ->
                     Test ! {gone, R, Reason}
             after 4_000 ->
                 Test ! no_gone
             end
         end),
         SubRef = receive {subbed, R} -> R after 1_000 ->
             erlang:error(subscribe_never_returned) end,

         %% Simulate the real wedge: a live peering worker that never
         %% completes HELLO, so `peer_node_id' stays undefined. This is
         %% exactly the state the live realm links were stuck in.
         FakePeer = spawn(fun() -> receive stop -> ok after 10_000 -> ok end end),
         PeerMon  = erlang:monitor(process, FakePeer),
         _ = sys:replace_state(Pid, fun(S) ->
             setelement(?PEER_PID_INDEX, S, FakePeer)
         end),
         %% peer_node_id must remain undefined (un-connected).
         ?assertEqual(undefined, element(?PEER_NODE_INDEX, sys:get_state(Pid))),

         LinkMon = erlang:monitor(process, Pid),

         %% Fire the watchdog exactly as the timer would.
         Pid ! connect_watchdog,

         %% The link recycles: stops normally so the owner respawns.
         receive
             {'DOWN', LinkMon, process, Pid, Reason} ->
                 ?assertEqual(normal, Reason)
         after 2_000 ->
             erlang:error(link_did_not_recycle)
         end,

         %% The wedged peering worker is killed (best-effort).
         receive
             {'DOWN', PeerMon, process, FakePeer, _} -> ok
         after 2_000 ->
             erlang:error(wedged_worker_not_killed)
         end,

         %% The queued subscriber is released, not left dangling.
         receive
             {gone, SubRef, connect_timeout} -> ok;
             {gone, SubRef, OtherReason} ->
                 erlang:error({unexpected_gone_reason, OtherReason});
             no_gone ->
                 erlang:error(subscriber_not_released)
         after 2_000 ->
             erlang:error(no_gone_message)
         end,
         ok
     end}.

%%------------------------------------------------------------------
%% Safety: a link that DID complete the handshake must never be
%% recycled by a late/racing watchdog message. The connected guard
%% swallows it and the link keeps serving.
%%------------------------------------------------------------------

connect_watchdog_does_not_recycle_connected_link_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_station_link:start_link(#{
             seed                => #{host => <<"127.0.0.1">>, port => 1},
             identity            => Identity,
             connect_watchdog_ms => 200
         }),
         FakePeer   = self(),
         PeerNodeId = macula_identity:public(macula_identity:generate()),
         _ = sys:replace_state(Pid, fun(S) ->
             setelement(?PEER_PID_INDEX, S, FakePeer)
         end),
         %% Complete the handshake — this cancels the real watchdog and
         %% sets peer_node_id.
         Pid ! {macula_peering, connected, FakePeer, PeerNodeId},
         %% Give the connected message time to process.
         ok = wait_until_connected(Pid, 20),

         LinkMon = erlang:monitor(process, Pid),
         %% A late/racing watchdog straggler must be a no-op now.
         Pid ! connect_watchdog,
         receive
             {'DOWN', LinkMon, process, Pid, _R} ->
                 erlang:error(connected_link_wrongly_recycled)
         after 500 ->
             ok
         end,
         ?assert(is_process_alive(Pid)),
         macula_station_link:stop(Pid),
         ok
     end}.

wait_until_connected(_Pid, 0) ->
    erlang:error(never_connected);
wait_until_connected(Pid, N) ->
    case element(?PEER_NODE_INDEX, sys:get_state(Pid)) of
        undefined -> timer:sleep(25), wait_until_connected(Pid, N - 1);
        _NodeId   -> ok
    end.
