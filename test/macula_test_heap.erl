%% Test support: run a call in a process whose heap is capped, so a call that would build a term without end fails
%% at the cap instead of filling the machine's memory.
-module(macula_test_heap).

-export([capped/1]).

-define(HEAP_CAP_WORDS, 1_000_000).

%% @doc What Fun gives, run in a process whose heap is capped at a million words: {returned, Result}, {raised,
%% Reason} for an error, or {ended, Reason} when the process ends another way, as it does when killed at the cap.
-spec capped(fun(() -> Result)) -> {returned, Result} | {raised, term()} | {ended, term()}.
capped(Fun) when is_function(Fun, 0) ->
    Cap = #{size => ?HEAP_CAP_WORDS, kill => true, error_logger => false},
    {Pid, Ref} = spawn_opt(fun() -> exit(outcome(Fun)) end, [monitor, {max_heap_size, Cap}]),
    receive
        {'DOWN', Ref, process, Pid, {outcome, Outcome}} -> Outcome;
        {'DOWN', Ref, process, Pid, Reason} -> {ended, Reason}
    end.

outcome(Fun) ->
    try {outcome, {returned, Fun()}}
    catch error:Reason -> {outcome, {raised, Reason}}
    end.
