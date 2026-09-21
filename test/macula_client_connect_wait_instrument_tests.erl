%% The connect-wait instrument emits exactly ONE event per wait, with the
%% reason and the elapsed time.
%%
%% The property worth testing is the ONE. `await_connected/2` polls every 50 ms,
%% and before this instrument existed the recursion ran back through the entry
%% function. Emitting there would produce one event per poll — twenty for a one
%% second wait — which floods the very measurement the instrument exists to
%% take, and nothing about the code's shape would look wrong. The recursion now
%% runs through `poll_connected/2`; this test is what holds it there.
%%
%% Events reach `logger`, so a handler captures them.
-module(macula_client_connect_wait_instrument_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EVENT, <<"_macula.client.connect_wait_ended">>).

%% logger handler callback, defined at the foot of this module.
-export([log/2]).

one_event_per_wait_test_() ->
    {timeout, 30, fun one_event_per_wait/0}.

one_event_per_wait() ->
    {ok, _} = application:ensure_all_started(macula),
    Self = self(),
    HandlerId = ?MODULE,
    ok = logger:add_handler(HandlerId, ?MODULE,
                            #{config => #{to => Self}, level => all}),
    %% The instrument logs at `info'; the primary level may sit above it.
    PrimaryLevel = maps:get(level, logger:get_primary_config()),
    ok = logger:set_primary_config(level, all),
    try
        %% A DEAD pid, deliberately. `safe_is_connected/1' wraps a
        %% gen_server:call, so an ALIVE process that never replies blocks the
        %% probe for its full call timeout and the wait ends after ONE poll —
        %% under which a per-poll emit and a per-wait emit are indistinguishable
        %% and this test proves nothing. A dead pid fails the call instantly, so
        %% the wait really does poll every 50 ms and a per-poll emit would show
        %% up as roughly six events instead of one.
        Never = spawn(fun() -> ok end),
        _ = wait_until_dead(Never, 50),
        Deadline = erlang:monotonic_time(millisecond) + 300,
        Before = erlang:monotonic_time(millisecond),
        ?assertEqual(false, macula_client:await_connected(Never, Deadline)),
        Waited = erlang:monotonic_time(millisecond) - Before,

        Events = drain(200),
        ?assertEqual(1, length(Events)),

        [#{reason := Reason, elapsed_ms := Elapsed, node_id := NodeId}] = Events,
        ?assertEqual(deadline, Reason),
        ?assertEqual(undefined, NodeId),
        %% The instrument's own elapsed must reflect the real wait, not zero
        %% and not the whole test.
        ?assert(Elapsed >= 250),
        ?assert(Elapsed =< Waited + 50),
        %% And the wait really did poll repeatedly rather than ending on one
        %% blocked probe, which is what makes the count of 1 above meaningful.
        ?assert(Waited >= 250),
        ?assert(Waited < 900)
    after
        _ = logger:set_primary_config(level, PrimaryLevel),
        _ = logger:remove_handler(HandlerId)
    end.

wait_until_dead(_Pid, 0) -> timeout;
wait_until_dead(Pid, N) ->
    case is_process_alive(Pid) of
        false -> dead;
        true  -> timer:sleep(10), wait_until_dead(Pid, N - 1)
    end.

drain(TimeoutMs) ->
    receive
        {connect_wait_event, Fields} -> [Fields | drain(TimeoutMs)]
    after TimeoutMs -> []
    end.

%%%===================================================================
%%% logger handler
%%%===================================================================

log(#{msg := {report, #{event := ?EVENT, properties := Props}}}, #{config := #{to := To}}) ->
    To ! {connect_wait_event, Props},
    ok;
log(_LogEvent, _Config) ->
    ok.
