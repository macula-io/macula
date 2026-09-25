%% A direct-dial link carries its trust options in the dial, not in its
%% seed: `macula:call_station/8' names the station as a URL binary and
%% passes `expected_node_id' as an option, which
%% `macula_station_link:add_tls_opts/2' folds into the seed at start. A
%% configured seed MAP carries its own pin and survives anything; a URL
%% binary does not.
%%
%% The pool kept those options nowhere, so a respawn after a link bounce
%% restarted the seed without them, `seed_checked/4' found no pin, and the
%% link child was refused with `{seed, expected_node_id_required}'.
%%
%% Nothing unpinned is ever dialled: the refusal is the design working. What
%% is lost is the ROUTE, silently, and only after a bounce, which is to say
%% exactly when the fleet is already disturbed.
%%
%% Asserted through the public API alone. `macula_station_link' is on the
%% watched list in `macula_shared_module_mocks_tests', and the pool already
%% reports both things this needs: `links/1' lists only links that have a
%% live pid, and `status/1' counts refusals by reason.
-module(macula_client_respawn_trust_opts_tests).

-include_lib("eunit/include/eunit.hrl").

%% The pool's own configured seed, pinned in the seed map, so the pool starts.
-define(SEED, #{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<1:256>>}).
%% The direct-dial target, as a URL binary: it carries no pin of its own.
-define(STATION, <<"quic://[::1]:1">>).
-define(STATION_PIN, <<2:256>>).
-define(REFUSAL, seed_without_expected_node_id).

a_direct_dial_pin_survives_a_link_respawn_test_() ->
    {timeout, 30, fun a_direct_dial_pin_survives_a_link_respawn/0}.

a_direct_dial_pin_survives_a_link_respawn() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pool} = macula_client:connect([?SEED], #{}),
    try
        OldPid = dialed_link_pid(Pool),
        ?assertEqual(0, refusals(Pool)),

        kill_and_wait(OldPid),
        NewPid = wait_for_respawn(Pool, 60),

        %% The consequence: the station has a live link again. With the pin
        %% dropped the link refuses its own seed, start_link returns an
        %% error, and the station has no link at all.
        ?assertNotEqual(undefined, NewPid),
        ?assertNotEqual(OldPid, NewPid),

        %% The mechanism, by its name rather than by its symptom: the pool
        %% counts a start refused for want of an expected_node_id, and the
        %% respawn must not have produced one.
        ?assertEqual(0, refusals(Pool))
    after
        ok = macula_client:close(Pool)
    end.

%% Direct-dial the station with the pin in the OPTIONS, the shape
%% `macula:call_station/8' uses. The handshake cannot complete against an
%% unreachable host, so the call itself returns an error; the link child it
%% started is what this test is about.
dialed_link_pid(Pool) ->
    _ = macula_client:call_station(Pool, ?STATION, ?STATION_PIN, <<0:256>>, <<"acme/probe">>, #{}, 200, <<>>,
                                   #{expected_node_id => ?STATION_PIN}),
    Pid = link_pid(Pool),
    ?assertNotEqual(undefined, Pid),
    Pid.

%% The pool keys its links map by the seed exactly as it was handed to the
%% dial, and lists only links that have a live pid.
link_pid(Pool) ->
    {ok, Links} = macula_client:links(Pool),
    first_pid([P || #{seed := ?STATION, pid := P} <- Links]).

first_pid([Pid | _]) -> Pid;
first_pid([]) -> undefined.

refusals(Pool) ->
    {ok, #{refused_dials := Counts}} = macula_client:status(Pool),
    maps:get(?REFUSAL, Counts, 0).

kill_and_wait(Pid) ->
    Mon = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Mon, process, Pid, _} -> ok
    after 2_000 -> erlang:error(link_did_not_die)
    end.

%% Past ?LINK_RESPAWN_DELAY_MS (1s), in 100 ms steps.
wait_for_respawn(_Pool, 0) ->
    undefined;
wait_for_respawn(Pool, N) ->
    timer:sleep(100),
    respawned(link_pid(Pool), Pool, N).

respawned(undefined, Pool, N) -> wait_for_respawn(Pool, N - 1);
respawned(Pid, _Pool, _N) -> Pid.
