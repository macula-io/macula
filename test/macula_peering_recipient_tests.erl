%%% @doc Bypass-recipient resolution for `macula_peering_conn'.
%%%
%%% Regression cover for the silent pubsub/DHT black-hole: the
%%% category-bypass guard used to be `is_pid(Pid)' alone, against a
%%% recipient captured once at `init/1'. `is_pid/1' is true for a DEAD
%%% pid, so when a station's frame dispatcher crash-restarted, every
%%% already-established peering connection kept posting frames to the
%%% dead pid. Messages to a dead pid are discarded by the VM, so the
%%% connection went pubsub-silent for its whole remaining life with no
%%% error logged at either end and no reconnect to trigger recovery.
%%%
%%% The fix resolves the recipient on every frame and prefers a
%%% registered name, so a restart is transparent.
-module(macula_peering_recipient_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NAME, macula_peering_recipient_tests_target).

%%====================================================================
%% Unset / absent recipient
%%====================================================================

unset_recipient_falls_back_test() ->
    ?assertEqual(undefined,
                 macula_peering_conn:resolve_recipient(undefined)).

unregistered_name_falls_back_test() ->
    %% A name nobody holds must resolve to `undefined' (the caller then
    %% routes via controlling_pid) rather than crashing the connection.
    ?assertEqual(undefined,
                 macula_peering_conn:resolve_recipient(
                   macula_peering_recipient_tests_never_registered)).

%%====================================================================
%% Live recipients
%%====================================================================

live_pid_resolves_to_itself_test() ->
    Pid = spawn_idle(),
    ?assertEqual(Pid, macula_peering_conn:resolve_recipient(Pid)),
    stop_idle(Pid).

registered_name_resolves_to_holder_test() ->
    Pid = spawn_idle(),
    true = register(?NAME, Pid),
    ?assertEqual(Pid, macula_peering_conn:resolve_recipient(?NAME)),
    unregister(?NAME),
    stop_idle(Pid).

%%====================================================================
%% The regressions
%%====================================================================

dead_pid_does_not_resolve_test() ->
    %% THE defect. Pre-fix the bypass guard was `is_pid(Pid)', which
    %% passes here, and every frame vanished into a dead mailbox.
    Pid = spawn_idle(),
    stop_idle(Pid),
    ?assert(is_pid(Pid)),
    ?assertNot(erlang:is_process_alive(Pid)),
    ?assertEqual(undefined, macula_peering_conn:resolve_recipient(Pid)).

name_follows_recipient_restart_test() ->
    %% What a supervisor restart looks like: same registered name, new
    %% pid. A connection established before the restart must land its
    %% next frame on the NEW pid without being torn down.
    Old = spawn_idle(),
    true = register(?NAME, Old),
    ?assertEqual(Old, macula_peering_conn:resolve_recipient(?NAME)),

    stop_idle(Old),
    unregister_if_present(?NAME),
    New = spawn_idle(),
    true = register(?NAME, New),

    ?assertNotEqual(Old, New),
    ?assertEqual(New, macula_peering_conn:resolve_recipient(?NAME)),

    unregister(?NAME),
    stop_idle(New).

name_resolves_undefined_during_restart_gap_test() ->
    %% Between the crash and the re-register the name holds nothing.
    %% That window must degrade to the legacy path, not crash.
    Pid = spawn_idle(),
    true = register(?NAME, Pid),
    stop_idle(Pid),
    unregister_if_present(?NAME),
    ?assertEqual(undefined, macula_peering_conn:resolve_recipient(?NAME)).

%%====================================================================
%% Helpers
%%====================================================================

spawn_idle() ->
    spawn(fun idle/0).

idle() ->
    receive stop -> ok end.

stop_idle(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! stop,
    receive {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 -> erlang:demonitor(Ref, [flush]), exit({idle_not_stopping, Pid})
    end.

%% A registered name is auto-unregistered when its holder dies, but the
%% unregister is not synchronous with our observation of the exit.
unregister_if_present(Name) ->
    wait_unregistered(Name, 100).

wait_unregistered(Name, 0) ->
    exit({still_registered, Name, whereis(Name)});
wait_unregistered(Name, N) ->
    retry_unregistered(Name, whereis(Name), N).

retry_unregistered(_Name, undefined, _N) ->
    ok;
retry_unregistered(Name, _Pid, N) ->
    timer:sleep(10),
    wait_unregistered(Name, N - 1).
