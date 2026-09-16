%% @doc The bound behind `macula_diagnostics:bounded_event/3': a diagnostics
%% event goes to the log at most once per window, 10 seconds, per event
%% name, node-wide.
%%
%% The first event of a name is logged and opens its window. An event
%% inside the window is counted and becomes the latest, and is not logged.
%% The first event after the window takes the next one and is logged with
%% `suppressed', the number held back since the line before. Once per
%% window this process logs the count a burst left inside a window that no
%% later event has closed, with the latest properties, and that opens the
%% next window.
%%
%% Callers update the counts table themselves with atomic ETS updates, so a
%% burst never queues on this process: it owns the table and handles only
%% its own sweep. Without the table, between an owner's exit and its
%% restart, an event is logged as it is.
-module(macula_diagnostics_bound).
-behaviour(gen_server).

-export([start_link/0, start_link/1, event/4, sweep/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([options/0]).

-define(WINDOW_MS, 10_000).

%% `table' names the counts table, `clock' gives milliseconds, `window_ms'
%% is the window, and `sweep' set to `manual' leaves the sweep to sweep/1.
%% All exist for tests.
-type options() :: #{table => atom(), clock => fun(() -> integer()), window_ms => pos_integer(),
                     sweep => periodic | manual}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Options) when is_map(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc Log an event through the counts table `Table', at most once per
%% window for its name.
-spec event(atom(), macula_diagnostics:level(), binary(), map()) -> ok.
event(Table, Level, Topic, Properties) when is_atom(Level), is_binary(Topic), is_map(Properties) ->
    counted(ets:whereis(Table), Table, Level, Topic, Properties).

%% @doc Run the sweep now, at the table's clock.
-spec sweep(pid()) -> ok.
sweep(Owner) ->
    gen_server:call(Owner, sweep).

init(Options) ->
    Table = ets:new(maps:get(table, Options, ?MODULE), [named_table, public, set, {write_concurrency, true}]),
    Window = maps:get(window_ms, Options, ?WINDOW_MS),
    true = ets:insert(Table, {settings, maps:get(clock, Options, fun monotonic_ms/0), Window}),
    ok = next_sweep(maps:get(sweep, Options, periodic), Window),
    {ok, #{table => Table, sweep => maps:get(sweep, Options, periodic), window_ms => Window}}.

handle_call(sweep, _From, #{table := Table} = State) ->
    {reply, swept(Table), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(sweep, #{table := Table, sweep := Sweep, window_ms := Window} = State) ->
    ok = swept(Table),
    ok = next_sweep(Sweep, Window),
    {noreply, State};
handle_info(_Message, State) ->
    {noreply, State}.

%%------------------------------------------------------------------
%% Counting, in the caller's process
%%------------------------------------------------------------------

counted(undefined, _Table, Level, Topic, Properties) ->
    macula_diagnostics:event(Level, Topic, Properties);
counted(_Tid, Table, Level, Topic, Properties) ->
    [{settings, Clock, Window}] = ets:lookup(Table, settings),
    placed(ets:lookup(Table, {topic, Topic}), Clock(), Window, Table, Level, Topic, Properties).

%% A row is {{topic, Topic}, WindowStart, Suppressed, Level, Properties}.
placed([], Now, Window, Table, Level, Topic, Properties) ->
    opened(ets:insert_new(Table, {{topic, Topic}, Now, 0, Level, Properties}), Now, Window, Table, Level, Topic,
           Properties);
placed([{Key, Start, _Suppressed, _Level, _Latest}], Now, Window, Table, Level, _Topic, Properties)
  when Now - Start < Window ->
    _ = ets:update_counter(Table, Key, {3, 1}),
    _ = ets:update_element(Table, Key, [{4, Level}, {5, Properties}]),
    ok;
placed([{Key, Start, Suppressed, _Level, _Latest}], Now, Window, Table, Level, Topic, Properties) ->
    claimed(ets:select_replace(Table, [replacement(Key, Start, Suppressed, Now, Level, Properties)]), Suppressed, Now,
            Window, Table, Level, Topic, Properties).

opened(true, _Now, _Window, _Table, Level, Topic, Properties) ->
    macula_diagnostics:event(Level, Topic, Properties#{suppressed => 0});
opened(false, Now, Window, Table, Level, Topic, Properties) ->
    placed(ets:lookup(Table, {topic, Topic}), Now, Window, Table, Level, Topic, Properties).

%% Another caller changed the row between the read and the replacement: read it again.
claimed(1, Suppressed, _Now, _Window, _Table, Level, Topic, Properties) ->
    macula_diagnostics:event(Level, Topic, Properties#{suppressed => Suppressed});
claimed(0, _Suppressed, Now, Window, Table, Level, Topic, Properties) ->
    placed(ets:lookup(Table, {topic, Topic}), Now, Window, Table, Level, Topic, Properties).

%% Replace the row only if its window start and count are still the ones read, and open a window at Now.
replacement(Key, Start, Suppressed, Now, Level, Properties) ->
    {{Key, Start, Suppressed, '_', '_'}, [], [{{{const, Key}, Now, 0, Level, {const, Properties}}}]}.

%%------------------------------------------------------------------
%% The sweep, in the owner
%%------------------------------------------------------------------

swept(Table) ->
    [{settings, Clock, Window}] = ets:lookup(Table, settings),
    Now = Clock(),
    Held = ets:select(Table, [{{{topic, '_'}, '$1', '$2', '_', '_'}, [{'>', '$2', 0}, {'>=', {'-', Now, '$1'}, Window}],
                               ['$_']}]),
    lists:foreach(fun(Row) -> flushed(Row, Now, Table) end, Held).

flushed({{topic, Topic} = Key, Start, Suppressed, Level, Latest}, Now, Table) ->
    logged(ets:select_replace(Table, [replacement(Key, Start, Suppressed, Now, Level, Latest)]), Level, Topic,
           Latest#{suppressed => Suppressed}).

logged(1, Level, Topic, Properties) -> macula_diagnostics:event(Level, Topic, Properties);
logged(0, _Level, _Topic, _Properties) -> ok.

next_sweep(periodic, Window) ->
    _ = erlang:send_after(Window, self(), sweep),
    ok;
next_sweep(manual, _Window) ->
    ok.

monotonic_ms() ->
    erlang:monotonic_time(millisecond).
