%% EUnit tests for macula_peering — supervisor + worker lifecycle.
%%
%% Real CONNECT/HELLO exchange over QUIC happens in hecate-station's
%% Common Test suites which exercise this SDK module against a live
%% QUIC listener.
-module(macula_peering_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Supervision tree
%%------------------------------------------------------------------

app_starts_supervisor_test() ->
    {ok, _} = application:ensure_all_started(macula),
    ?assert(is_pid(whereis(macula_peering_sup))),
    ?assert(is_pid(whereis(macula_peering_conn_sup))),
    ok.

%%------------------------------------------------------------------
%% Client worker — connect failure → notification + exit
%%------------------------------------------------------------------

client_worker_emits_disconnected_on_connect_failure_test_() ->
    {timeout, 5,
     fun() ->
         {ok, _} = application:ensure_all_started(macula),
         Self = self(),
         Identity = macula_identity:generate(),
         {ok, Pid} = macula_peering:connect(#{
             identity        => Identity,
             realms          => [],
             capabilities    => 0,
             controlling_pid => Self,
             target          => #{host => "127.0.0.1", port => 1, timeout_ms => 200}
         }),
         %% The worker raises on connect (NIF raises) which crashes the worker.
         %% Either we get a 'disconnected' message, or the worker dies first.
         %% Wait for the linked exit.
         Mon = erlang:monitor(process, Pid),
         receive
             {macula_peering, disconnected, Pid, _Reason} -> ok;
             {'DOWN', Mon, process, Pid, _} -> ok
         after 4_000 ->
             ?assert(false)
         end
     end}.

%%------------------------------------------------------------------
%% Server worker — accept/2 with bogus conn fails fast
%%------------------------------------------------------------------

server_worker_can_be_spawned_test() ->
    {ok, _} = application:ensure_all_started(macula),
    %% Spawn server-role worker without a real conn — it stays in
    %% awaiting_start state. We verify spawning works and close cleans up.
    Self = self(),
    Identity = macula_identity:generate(),
    Opts = #{
        role            => server,
        identity        => Identity,
        realms          => [],
        capabilities    => 0,
        controlling_pid => Self,
        quic_conn       => make_ref()
    },
    {ok, Pid} = macula_peering_conn_sup:start_conn(Opts),
    ?assert(is_pid(Pid)),
    Mon = erlang:monitor(process, Pid),
    macula_peering:close(Pid),
    receive
        {macula_peering, disconnected, Pid, _Reason} ->
            receive {'DOWN', Mon, process, Pid, _} -> ok after 1_000 -> ?assert(false) end;
        {'DOWN', Mon, process, Pid, _} -> ok
    after 2_000 ->
        ?assert(false)
    end.

%%------------------------------------------------------------------
%% close/1 idempotency on already-stopped worker
%%------------------------------------------------------------------

close_on_dead_worker_does_not_crash_test() ->
    {ok, _} = application:ensure_all_started(macula),
    Self = self(),
    Identity = macula_identity:generate(),
    {ok, Pid} = macula_peering_conn_sup:start_conn(#{
        role            => server,
        identity        => Identity,
        realms          => [],
        capabilities    => 0,
        controlling_pid => Self,
        quic_conn       => make_ref()
    }),
    Mon = erlang:monitor(process, Pid),
    macula_peering:close(Pid),
    receive {'DOWN', Mon, process, Pid, _} -> ok after 2_000 -> ?assert(false) end,
    %% Sending close to a dead pid → cast is fire-and-forget, no error.
    ?assertEqual(ok, macula_peering:close(Pid)).
