%% @doc Structured event emission + per-process metric accumulation.
%%
%% Phase 1 implementation: events go through OTP `logger' with a structured
%% report; metrics live in the calling process's dictionary. Phase 7
%% upgrades to Prometheus / OpenTelemetry exporters without changing this
%% module's public surface.
%%
%% Topic namespacing convention:
%% <ul>
%%   <li>`_macula.*' — protocol-layer events (SDK)</li>
%%   <li>`_hecate.*' — station-layer events (Hecate-specific)</li>
%% </ul>
-module(macula_diagnostics).

-export([
    event/2, event/3,
    metric/3,
    snapshot/0,
    reset/0
]).

-export_type([level/0, metric_type/0, sample/0]).

-type level() :: debug | info | notice | warning | error.
-type metric_type() :: counter | gauge.
-type sample() :: {Name :: binary(), metric_type(), number()}.

%%------------------------------------------------------------------
%% Events — go through `logger'
%%------------------------------------------------------------------

%% @doc Emit a structured event at the default `info' level.
-spec event(binary(), map()) -> ok.
event(Topic, Properties) ->
    event(info, Topic, Properties).

%% @doc Emit a structured event at a specific level.
-spec event(level(), binary(), map()) -> ok.
event(Level, Topic, Properties)
  when is_atom(Level), is_binary(Topic), is_map(Properties) ->
    Report = #{event => Topic, properties => Properties},
    Meta   = #{report_cb => fun report_cb/1, domain => [macula]},
    logger:log(Level, Report, Meta).

%% Logger report callback — flat single-line format.
report_cb(#{event := Topic, properties := Props}) ->
    {"~s ~0p", [Topic, Props]}.

%%------------------------------------------------------------------
%% Metrics — process-dictionary backed (Phase 1 only)
%%------------------------------------------------------------------

%% @doc Emit a metric. `counter' increments are summed across calls;
%% `gauge' values overwrite the previous reading.
-spec metric(binary(), metric_type(), number()) -> ok.
metric(Name, counter, N)
  when is_binary(Name), is_integer(N), N > 0 ->
    Key = key(Name, counter),
    Cur = read_or_zero(erlang:get(Key)),
    erlang:put(Key, Cur + N),
    ok;
metric(Name, gauge, V)
  when is_binary(Name), is_number(V) ->
    erlang:put(key(Name, gauge), V),
    ok.

%% @doc Snapshot all metrics held by the calling process.
-spec snapshot() -> [sample()].
snapshot() ->
    [ {Name, Type, V}
      || {{macula_metric, Name, Type}, V} <- erlang:get(),
         is_metric_sample(Name, Type, V) ].

%% @doc Drop all metric entries from the calling process's dictionary.
-spec reset() -> ok.
reset() ->
    [erlang:erase(K) || K <- erlang:get_keys(), is_metric_key(K)],
    ok.

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

key(Name, Type) -> {macula_metric, Name, Type}.

read_or_zero(undefined) -> 0;
read_or_zero(N) when is_integer(N) -> N.

is_metric_key({macula_metric, _Name, _Type}) -> true;
is_metric_key(_) -> false.

is_metric_sample(Name, Type, V)
  when is_binary(Name), (Type =:= counter orelse Type =:= gauge),
       is_number(V) -> true;
is_metric_sample(_, _, _) -> false.
