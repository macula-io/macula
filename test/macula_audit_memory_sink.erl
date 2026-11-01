%% A sink for the audit chain's tests: entries in an ordered ETS table, counters for what the chain asked of it, and a
%% refusal a test can arm on either call. It exists so a test can see that a checkpoint synced BEFORE it signed, and so
%% that a sink which cannot sync can be made to say so.
-module(macula_audit_memory_sink).

-behaviour(macula_audit_sink).

-export([new/1, append/2, sync/1, entries/1, calls/1]).

new(Opts) ->
    Tab = ets:new(?MODULE, [ordered_set, public]),
    Counters = counters:new(2, []),
    {ok, #{tab => Tab,
           counters => Counters,
           append => maps:get(append, Opts, ok),
           sync => maps:get(sync, Opts, ok)}}.

append(Bytes, #{tab := Tab, counters := Counters, append := Verdict} = State) ->
    counters:add(Counters, 1, 1),
    case Verdict of
        ok ->
            ets:insert(Tab, {erlang:unique_integer([monotonic]), Bytes}),
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

sync(#{counters := Counters, sync := Verdict}) ->
    counters:add(Counters, 2, 1),
    Verdict.

entries(#{tab := Tab}) ->
    {ok, [Bytes || {_Key, Bytes} <- ets:tab2list(Tab)]}.

calls(#{counters := Counters}) ->
    #{appends => counters:get(Counters, 1), syncs => counters:get(Counters, 2)}.
