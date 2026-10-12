%% @doc What this node's frame reader tolerated from newer peers instead of closing a link (macula#83 decisions 5 and
%% 6): frames of a type it does not know, dropped, and per-hop enum values it does not know, read as the enum's
%% fallback. Each is counted here, node-wide, so an operator sees when the fleet speaks something this node does not.
%%
%% The counters are created when the module loads, so the frame codec counts with no process to start and no table
%% to own, and a count never blocks a reader.
-module(macula_wire_tolerance).

-export([count/1, counters/0]).

-on_load(init/0).

-define(KEY, {?MODULE, counters}).
-define(COUNTERS, [unknown_frame_type, neighbor_priority, delivered_via, swim_state]).

%% unknown_frame_type: a frame of a type this node does not know, dropped. neighbor_priority, delivered_via and
%% swim_state: a value of that per-hop enum this node does not know, read as its fallback (low, direct, the update
%% ignored).
-type counter() :: unknown_frame_type | neighbor_priority | delivered_via | swim_state.
-export_type([counter/0]).

init() ->
    persistent_term:put(?KEY, counters:new(length(?COUNTERS), [write_concurrency])).

%% @doc Count one tolerated read.
-spec count(counter()) -> ok.
count(Name) ->
    counters:add(persistent_term:get(?KEY), index(Name, ?COUNTERS, 1), 1).

%% @doc Every counter by name, since the module loaded.
-spec counters() -> #{counter() => non_neg_integer()}.
counters() ->
    Ref = persistent_term:get(?KEY),
    maps:from_list([{Name, counters:get(Ref, index(Name, ?COUNTERS, 1))} || Name <- ?COUNTERS]).

index(Name, [Name | _], I) -> I;
index(Name, [_ | Rest], I) -> index(Name, Rest, I + 1).
