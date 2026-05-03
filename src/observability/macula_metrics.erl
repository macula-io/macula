%%%-------------------------------------------------------------------
%%% @doc Node-wide Prometheus-style metric registry for macula-net.
%%%
%%% Phase 4.1 — see PLAN_MACULA_NET_PHASE4_1_OBSERVABILITY.md.
%%%
%%% Backed by ETS:
%%%
%%% <ul>
%%%   <li>`?META' — `{Name, Type, Labels, Help, Buckets}'</li>
%%%   <li>`?VALS' — `{{counter, Name, LabelVals},   Count}'<br/>
%%%       `{{gauge,   Name, LabelVals},   Value}'<br/>
%%%       `{{hist_bk, Name, LabelVals, I}, Count}' (per bucket)<br/>
%%%       `{{hist_sm, Name, LabelVals},   SumMicros}'<br/>
%%%       `{{hist_ct, Name, LabelVals},   Count}'</li>
%%% </ul>
%%%
%%% Counters and histograms use {@link ets:update_counter/4} which is
%%% lock-free per-key; no gen_server roundtrip on the hot path.
%%%
%%% Telemetry handlers are attached at start. Hot paths emit via
%%% {@link telemetry:execute/3}; the handler maps event_name +
%%% measurements to counter/histogram updates. Grep for
%%% `telemetry:execute' to find every measurement point.
%%%
%%% Gauges are polled (1s) from source-of-truth (ETS sizes).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_metrics).

-behaviour(gen_server).

-export([
    start_link/0, start_link/1,
    stop/0,
    register_counter/3,
    register_gauge/3,
    register_histogram/4,
    inc_counter/3,
    set_gauge/3,
    observe/3,
    gather/0,
    reset_all/0
]).

-export([handle_telemetry_event/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(META, macula_metrics_meta).
-define(VALS, macula_metrics_vals).
-define(GAUGE_POLL_MS, 1000).

-type label_name()  :: atom().
-type label_val()   :: binary() | atom().
-type label_vals()  :: [{label_name(), label_val()}].
-type metric_name() :: binary().
-type metric_type() :: counter | gauge | histogram.

-record(meta, {
    name    :: metric_name(),
    type    :: metric_type(),
    labels  :: [label_name()],
    help    :: binary(),
    buckets :: [number()] | undefined
}).

-record(state, {
    handler_id    :: term() | undefined,
    gauge_pollers :: [fun(() -> ok)]
}).

%% =============================================================================
%% Lifecycle
%% =============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() -> start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% =============================================================================
%% Registration
%% =============================================================================

-spec register_counter(metric_name(), [label_name()], binary()) -> ok.
register_counter(Name, Labels, Help) ->
    register_meta(#meta{name = Name, type = counter,
                        labels = Labels, help = Help, buckets = undefined}).

-spec register_gauge(metric_name(), [label_name()], binary()) -> ok.
register_gauge(Name, Labels, Help) ->
    register_meta(#meta{name = Name, type = gauge,
                        labels = Labels, help = Help, buckets = undefined}).

-spec register_histogram(metric_name(), [label_name()], binary(), [number()]) -> ok.
register_histogram(Name, Labels, Help, Buckets) when is_list(Buckets) ->
    register_meta(#meta{name = Name, type = histogram,
                        labels = Labels, help = Help,
                        buckets = lists:sort(Buckets)}).

%% =============================================================================
%% Hot-path API (no gen_server call)
%% =============================================================================

-spec inc_counter(metric_name(), label_vals(), pos_integer()) -> ok.
inc_counter(Name, LabelVals, N) when is_integer(N), N > 0 ->
    Key = {counter, Name, normalize(LabelVals)},
    _ = ets:update_counter(?VALS, Key, {2, N}, {Key, 0}),
    ok.

-spec set_gauge(metric_name(), label_vals(), number()) -> ok.
set_gauge(Name, LabelVals, V) when is_number(V) ->
    ets:insert(?VALS, {{gauge, Name, normalize(LabelVals)}, V}),
    ok.

-spec observe(metric_name(), label_vals(), number()) -> ok.
observe(Name, LabelVals, V) when is_number(V) ->
    LV = normalize(LabelVals),
    Buckets = lookup_buckets(Name),
    observe_into_buckets(Name, LV, V, Buckets, 0),
    SumKey   = {hist_sm, Name, LV},
    CountKey = {hist_ct, Name, LV},
    SumMicros = round(V * 1000000),
    _ = ets:update_counter(?VALS, SumKey,   {2, SumMicros}, {SumKey,   0}),
    _ = ets:update_counter(?VALS, CountKey, {2, 1},         {CountKey, 0}),
    ok.

observe_into_buckets(_Name, _LV, _V, [], _I) ->
    ok;
observe_into_buckets(Name, LV, V, [B | Rest], I) ->
    on_le_bucket(V =< B, Name, LV, I),
    observe_into_buckets(Name, LV, V, Rest, I + 1).

on_le_bucket(true, Name, LV, I) ->
    Key = {hist_bk, Name, LV, I},
    _ = ets:update_counter(?VALS, Key, {2, 1}, {Key, 0}),
    ok;
on_le_bucket(false, _Name, _LV, _I) ->
    ok.

%% =============================================================================
%% Snapshot for HTTP rendering
%% =============================================================================

%% @doc Returns the full metric snapshot in a render-friendly shape.
-spec gather() -> [map()].
gather() ->
    [render_meta(M) || {_, M} <- ets:tab2list(?META)].

render_meta(#meta{name = N, type = counter, labels = L, help = H}) ->
    #{name => N, type => counter, labels => L, help => H,
      samples => collect_counter_samples(N)};
render_meta(#meta{name = N, type = gauge, labels = L, help = H}) ->
    #{name => N, type => gauge, labels => L, help => H,
      samples => collect_gauge_samples(N)};
render_meta(#meta{name = N, type = histogram, labels = L, help = H, buckets = B}) ->
    #{name => N, type => histogram, labels => L, help => H,
      buckets => B,
      samples => collect_hist_samples(N, B)}.

collect_counter_samples(N) ->
    ets:foldl(fun({{counter, Nm, LV}, V}, Acc) when Nm =:= N ->
                      [#{labels => LV, value => V} | Acc];
                 (_, Acc) -> Acc
              end, [], ?VALS).

collect_gauge_samples(N) ->
    ets:foldl(fun({{gauge, Nm, LV}, V}, Acc) when Nm =:= N ->
                      [#{labels => LV, value => V} | Acc];
                 (_, Acc) -> Acc
              end, [], ?VALS).

%% Histogram sample = one entry per LabelVals, with bucket counts +
%% sum + count. Bucket counts are CUMULATIVE (Prometheus convention).
collect_hist_samples(N, Buckets) ->
    LVs = ets:foldl(fun({{hist_ct, Nm, LV}, _}, Acc) when Nm =:= N ->
                            [LV | Acc];
                       (_, Acc) -> Acc
                    end, [], ?VALS),
    [build_hist_sample(N, LV, Buckets) || LV <- lists:usort(LVs)].

build_hist_sample(N, LV, Buckets) ->
    %% Per-bucket counts are stored as "events ≤ this bucket boundary"
    %% — already cumulative in Prometheus terms, so no further accumulation.
    BucketCounts = bucket_values(N, LV, Buckets, 0, []),
    Sum = lookup_value({hist_sm, N, LV}) / 1000000.0,
    Cnt = lookup_value({hist_ct, N, LV}),
    #{labels => LV, buckets => BucketCounts, sum => Sum, count => Cnt}.

bucket_values(_N, _LV, [], _I, Acc) ->
    lists:reverse(Acc);
bucket_values(N, LV, [B | Rest], I, Acc) ->
    V = lookup_value({hist_bk, N, LV, I}),
    bucket_values(N, LV, Rest, I + 1, [{B, V} | Acc]).

lookup_value(Key) ->
    pick_value(ets:lookup(?VALS, Key)).

pick_value([{_, V}]) -> V;
pick_value([])       -> 0.

%% @doc Wipe every counter / gauge / histogram value. Metadata stays.
-spec reset_all() -> ok.
reset_all() ->
    ets:delete_all_objects(?VALS),
    ok.

%% =============================================================================
%% Telemetry handler — maps macula-net events to metric updates
%% =============================================================================

%% Dispatched by the telemetry library; arity-4 capture installed in init/1.
handle_telemetry_event([macula, net, egress, dispatched], M, Md, _Cfg) ->
    Kind = maps:get(kind, Md, <<"data">>),
    inc_counter(<<"macula_net_envelopes_forwarded_total">>,
                [{kind, Kind}, {direction, <<"egress">>}], 1),
    maybe_observe(<<"macula_net_egress_latency_seconds">>, [], M);
handle_telemetry_event([macula, net, egress, dropped], _M, Md, _Cfg) ->
    Reason = maps:get(reason, Md, <<"unknown">>),
    inc_counter(<<"macula_net_envelopes_dropped_total">>,
                [{reason, Reason}], 1);
handle_telemetry_event([macula, net, ingress, delivered], M, Md, _Cfg) ->
    Kind = maps:get(kind, Md, <<"data">>),
    inc_counter(<<"macula_net_envelopes_forwarded_total">>,
                [{kind, Kind}, {direction, <<"ingress">>}], 1),
    maybe_observe(<<"macula_net_ingress_latency_seconds">>, [], M);
handle_telemetry_event([macula, net, ingress, dropped], _M, Md, _Cfg) ->
    Reason = maps:get(reason, Md, <<"unknown">>),
    inc_counter(<<"macula_net_envelopes_dropped_total">>,
                [{reason, Reason}], 1);
handle_telemetry_event([macula, net, relay, dispatched], _M, Md, _Cfg) ->
    Kind = maps:get(kind, Md, <<"data">>),
    inc_counter(<<"macula_net_envelopes_forwarded_total">>,
                [{kind, Kind}, {direction, <<"relay">>}], 1);
handle_telemetry_event([macula, net, resolve, complete], M, Md, _Cfg) ->
    Outcome = maps:get(outcome, Md, <<"unknown">>),
    inc_counter(<<"macula_net_resolve_total">>, [{outcome, Outcome}], 1),
    maybe_observe(<<"macula_net_resolve_latency_seconds">>, [], M);
handle_telemetry_event([macula, net, attach, attached], _M, _Md, _Cfg) ->
    inc_counter(<<"macula_net_attach_events_total">>,
                [{event, <<"attached">>}], 1);
handle_telemetry_event([macula, net, attach, detached], _M, Md, _Cfg) ->
    Reason = maps:get(reason, Md, <<"normal">>),
    Event  = maps:get(event,  Md, <<"detached">>),
    inc_counter(<<"macula_net_attach_events_total">>,
                [{event, Event}], 1),
    inc_counter(<<"macula_net_attach_detach_reason_total">>,
                [{reason, Reason}], 1);
handle_telemetry_event([macula, net, transport, connect], _M, Md, _Cfg) ->
    Outcome = maps:get(outcome, Md, <<"unknown">>),
    inc_counter(<<"macula_net_transport_connect_total">>,
                [{outcome, Outcome}], 1);
handle_telemetry_event([macula, net, transport, stream_opened], _M, Md, _Cfg) ->
    Direction = maps:get(direction, Md, <<"inbound">>),
    inc_counter(<<"macula_net_transport_stream_total">>,
                [{direction, Direction}, {outcome, <<"opened">>}], 1);
handle_telemetry_event([macula, net, transport, stream_closed], _M, Md, _Cfg) ->
    Direction = maps:get(direction, Md, <<"inbound">>),
    inc_counter(<<"macula_net_transport_stream_total">>,
                [{direction, Direction}, {outcome, <<"closed">>}], 1);
handle_telemetry_event([macula, net, transport, path_mtu], #{bytes := Bytes}, Md, _Cfg) ->
    Peer = maps:get(peer, Md, <<"unknown">>),
    set_gauge(<<"macula_net_transport_path_mtu_bytes">>, [{peer, Peer}], Bytes);
handle_telemetry_event(_Other, _M, _Md, _Cfg) ->
    ok.

maybe_observe(Name, Labels, #{latency_us := Us}) when is_number(Us) ->
    observe(Name, Labels, Us / 1000000.0);
maybe_observe(Name, Labels, #{latency_seconds := Sec}) when is_number(Sec) ->
    observe(Name, Labels, Sec);
maybe_observe(_Name, _Labels, _M) ->
    ok.

%% =============================================================================
%% gen_server
%% =============================================================================

init(Opts) ->
    process_flag(trap_exit, true),
    ensure_tables(),
    register_default_metrics(),
    HandlerId = maybe_attach_telemetry(maps:get(attach_telemetry, Opts, true)),
    Pollers   = maybe_install_pollers(maps:get(install_default_gauges, Opts, true)),
    erlang:send_after(?GAUGE_POLL_MS, self(), poll_gauges),
    {ok, #state{handler_id = HandlerId, gauge_pollers = Pollers}}.

maybe_attach_telemetry(true)  -> attach_telemetry();
maybe_attach_telemetry(false) -> undefined.

maybe_install_pollers(true)  -> install_default_gauge_pollers();
maybe_install_pollers(false) -> [].

handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State)        -> {noreply, State}.

handle_info(poll_gauges, #state{gauge_pollers = Pollers} = State) ->
    lists:foreach(fun(F) -> F() end, Pollers),
    erlang:send_after(?GAUGE_POLL_MS, self(), poll_gauges),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, #state{handler_id = undefined}) -> ok;
terminate(_, #state{handler_id = Id})        ->
    _ = telemetry:detach(Id),
    ok.

%% =============================================================================
%% Internals
%% =============================================================================

register_meta(#meta{name = N} = M) ->
    ensure_tables(),
    ets:insert(?META, {N, M}),
    ok.

lookup_buckets(N) ->
    pick_buckets(ets:lookup(?META, N)).

pick_buckets([{_, #meta{buckets = B}}]) when is_list(B) -> B;
pick_buckets(_)                                          -> [].

normalize(LabelVals) ->
    [{K, to_label_bin(V)} || {K, V} <- lists:keysort(1, LabelVals)].

to_label_bin(V) when is_binary(V) -> V;
to_label_bin(V) when is_atom(V)   -> atom_to_binary(V, utf8);
to_label_bin(V) when is_integer(V) -> integer_to_binary(V);
to_label_bin(V) when is_list(V)   -> iolist_to_binary(V).

ensure_tables() ->
    ensure_table(?META, [named_table, public, set, {read_concurrency, true}]),
    ensure_table(?VALS, [named_table, public, set,
                         {write_concurrency, true},
                         {read_concurrency, true}]).

ensure_table(Name, Opts) ->
    open_table(ets:info(Name), Name, Opts).

open_table(undefined, Name, Opts) -> ets:new(Name, Opts), ok;
open_table(_,         _Name, _Opts) -> ok.

register_default_metrics() ->
    register_counter(<<"macula_net_envelopes_forwarded_total">>,
                     [kind, direction],
                     <<"Total envelopes forwarded by direction.">>),
    register_counter(<<"macula_net_envelopes_dropped_total">>,
                     [reason],
                     <<"Total envelopes dropped, by reason.">>),
    register_counter(<<"macula_net_resolve_total">>,
                     [outcome],
                     <<"Total DHT resolves, by outcome.">>),
    register_counter(<<"macula_net_attach_events_total">>,
                     [event],
                     <<"Total daemon attach lifecycle events.">>),
    register_counter(<<"macula_net_attach_detach_reason_total">>,
                     [reason],
                     <<"Total daemon detachments, by reason.">>),
    register_counter(<<"macula_net_transport_connect_total">>,
                     [outcome],
                     <<"Total QUIC connect attempts, by outcome.">>),
    register_counter(<<"macula_net_transport_stream_total">>,
                     [direction, outcome],
                     <<"Total QUIC stream open/close events.">>),

    register_gauge(<<"macula_net_attach_active">>, [],
                   <<"Currently attached daemons on this station.">>),
    register_gauge(<<"macula_net_route_cache_entries">>, [],
                   <<"Entries in the route cache (DHT mode).">>),
    register_gauge(<<"macula_net_transport_connections">>, [],
                   <<"Active QUIC connections.">>),
    register_gauge(<<"macula_net_transport_path_mtu_bytes">>, [peer],
                   <<"Quinn-tracked path MTU per outbound peer (bytes).">>),

    register_histogram(<<"macula_net_resolve_latency_seconds">>, [],
                       <<"DHT resolve latency.">>,
                       [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0]),
    register_histogram(<<"macula_net_egress_latency_seconds">>, [],
                       <<"Egress dispatch latency (TUN read -> transport send).">>,
                       [0.0001, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5]),
    register_histogram(<<"macula_net_ingress_latency_seconds">>, [],
                       <<"Ingress delivery latency (transport recv -> TUN write).">>,
                       [0.0001, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5]),
    ok.

attach_telemetry() ->
    Id = {?MODULE, telemetry_handler},
    Events = [
        [macula, net, egress, dispatched],
        [macula, net, egress, dropped],
        [macula, net, ingress, delivered],
        [macula, net, ingress, dropped],
        [macula, net, relay, dispatched],
        [macula, net, resolve, complete],
        [macula, net, attach, attached],
        [macula, net, attach, detached],
        [macula, net, transport, connect],
        [macula, net, transport, stream_opened],
        [macula, net, transport, stream_closed],
        [macula, net, transport, path_mtu]
    ],
    _ = telemetry:detach(Id),
    ok = telemetry:attach_many(Id, Events,
                               fun ?MODULE:handle_telemetry_event/4,
                               undefined),
    Id.

install_default_gauge_pollers() ->
    [
        fun poll_attach_active/0,
        fun poll_route_cache/0,
        fun poll_transport_connections/0
    ].

poll_attach_active() ->
    set_gauge(<<"macula_net_attach_active">>, [],
              optional_size(macula_host_identity, count, [])).

poll_route_cache() ->
    set_gauge(<<"macula_net_route_cache_entries">>, [],
              optional_size(macula_cache_route, size, [])).

poll_transport_connections() ->
    set_gauge(<<"macula_net_transport_connections">>, [],
              optional_size(macula_net_transport_quic, connection_count, [])).

%% Polled-only call: if the module exports the function, call it; else 0.
%% Module is one of ours, so if the call raises that's a real bug — let it.
optional_size(Mod, Fun, Args) ->
    invoke_if_exported(erlang:function_exported(Mod, Fun, length(Args)),
                       Mod, Fun, Args).

invoke_if_exported(true,  Mod, Fun, Args) -> erlang:apply(Mod, Fun, Args);
invoke_if_exported(false, _Mod, _Fun, _Args) -> 0.
