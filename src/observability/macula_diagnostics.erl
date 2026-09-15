%% @doc Structured event emission + per-process metric accumulation.
%%
%% Phase 1 implementation: events go through OTP `logger' with a structured
%% report; metrics live in the calling process's dictionary. Phase 7
%% upgrades to Prometheus / OpenTelemetry exporters without changing this
%% module's public surface.
%%
%% `bounded_event/3' logs an event at most once per 10 seconds per event
%% name, node-wide, with a count of the rest (`macula_diagnostics_bound').
%%
%% Topic namespacing convention:
%% <ul>
%%   <li>`_macula.*' — protocol-layer events (SDK)</li>
%%   <li>`_hecate.*' — station-layer events (Hecate-specific)</li>
%% </ul>
-module(macula_diagnostics).

-export([
    event/2, event/3,
    bounded_event/3,
    metric/3,
    snapshot/0,
    reset/0,
    install_domain_filter/0
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

%% @doc Emit a structured event at most once per 10 seconds per event name,
%% node-wide. An event inside that window is counted instead, and the next
%% line for the name carries the latest properties with `suppressed', the
%% number held back since the line before. Callers update the counts table
%% themselves, so a burst of events never queues on a process.
-spec bounded_event(level(), binary(), map()) -> ok.
bounded_event(Level, Topic, Properties) when is_atom(Level), is_binary(Topic), is_map(Properties) ->
    macula_diagnostics_bound:event(macula_diagnostics_bound, Level, Topic, Properties).

%% Logger report callback — flat single-line format.
report_cb(#{event := Topic, properties := Props}) ->
    {"~s ~0p", [Topic, Props]}.

%% @doc Allow `domain => [macula]' events through the default logger
%% handler's filter chain.
%%
%% Every event this module emits is stamped `domain => [macula]'
%% (above). On a release that includes `sasl' — true of every
%% consumer's production build — the default handler installs a
%% filter chain with `filter_default => stop' and only two explicit
%% allows: events whose domain is `[otp, sasl]' (or a sub-domain of
%% it) and events with no domain at all. `[macula]' matches neither,
%% so every `event/2,3' call was silently dropped before reaching
%% any handler, in every consumer, always — confirmed live on the
%% macula-station fleet (see CHANGELOG [10.5.5] there) after already
%% being independently rediscovered and worked around three separate
%% times at three separate call sites before anyone traced it to this
%% one line. Call this once, from the `macula' application's own
%% `start/2', so every consumer gets it for free just by depending on
%% `macula' — no per-consumer release config to remember.
-spec install_domain_filter() -> ok.
install_domain_filter() ->
    case logger:add_handler_filter(
            default,
            macula_domain,
            {fun logger_filters:domain/2, {log, equal, [macula]}}) of
        ok                           -> ok;
        {error, {already_exist, _}} -> ok;
        {error, _Reason}             -> ok
    end.

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
