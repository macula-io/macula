%%%-------------------------------------------------------------------
%%% @doc Eunit for macula_metrics_http — Phase 4.1.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_metrics_http_tests).

-include_lib("eunit/include/eunit.hrl").

http_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
        fun get_metrics_returns_prom_text/1,
        fun unknown_path_returns_404/1,
        fun counter_appears_in_text/1,
        fun histogram_renders_buckets_and_sum_count/1
     ]}.

setup() ->
    {ok, _} = application:ensure_all_started(telemetry),
    {ok, _} = application:ensure_all_started(inets),
    {ok, MetricsPid} = macula_metrics:start_link(
                        #{install_default_gauges => false}),
    macula_metrics:reset_all(),
    {ok, HttpPid} = macula_metrics_http:start_link(
                      #{port => 0, bind => {127,0,0,1}}),
    Port = macula_metrics_http:port(),
    {MetricsPid, HttpPid, Port}.

teardown({_M, _H, _Port}) ->
    case whereis(macula_metrics_http) of
        undefined -> ok;
        _ -> macula_metrics_http:stop()
    end,
    case whereis(macula_metrics) of
        undefined -> ok;
        _ -> macula_metrics:stop()
    end,
    ok.

get_metrics_returns_prom_text({_M, _H, Port}) ->
    fun() ->
        {ok, Code, Headers, Body} = http_get(Port, "/metrics"),
        ?assertEqual(200, Code),
        CT = proplists:get_value("content-type", Headers, ""),
        ?assertNotEqual(nomatch, string:find(CT, "text/plain")),
        ?assertNotEqual(nomatch, string:find(CT, "version=0.0.4")),
        ?assertNotEqual(nomatch, string:find(Body, "# HELP"))
    end.

unknown_path_returns_404({_M, _H, Port}) ->
    fun() ->
        {ok, Code, _Headers, _Body} = http_get(Port, "/nope"),
        ?assertEqual(404, Code)
    end.

counter_appears_in_text({_M, _H, Port}) ->
    fun() ->
        ok = macula_metrics:inc_counter(
               <<"macula_net_envelopes_forwarded_total">>,
               [{kind, <<"data">>}, {direction, <<"egress">>}], 7),
        {ok, 200, _Headers, Body} = http_get(Port, "/metrics"),
        ?assertNotEqual(
            nomatch,
            string:find(Body,
                        "macula_net_envelopes_forwarded_total{"
                        "direction=\"egress\",kind=\"data\"} 7"))
    end.

histogram_renders_buckets_and_sum_count({_M, _H, Port}) ->
    fun() ->
        ok = macula_metrics:observe(
               <<"macula_net_resolve_latency_seconds">>, [], 0.003),
        {ok, 200, _Headers, Body} = http_get(Port, "/metrics"),
        ?assertNotEqual(nomatch, string:find(Body, "_bucket{le=\"")),
        ?assertNotEqual(nomatch, string:find(Body, "_sum")),
        ?assertNotEqual(nomatch, string:find(Body, "_count")),
        ?assertNotEqual(nomatch, string:find(Body, "le=\"+Inf\""))
    end.

%% --- helpers --------------------------------------------------------

http_get(Port, Path) ->
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ Path,
    case httpc:request(get, {Url, []}, [], [{full_result, true}]) of
        {ok, {{_HttpVer, Code, _Reason}, Headers, Body}} ->
            LowerHeaders = [{string:to_lower(K), V} || {K, V} <- Headers],
            {ok, Code, LowerHeaders, Body};
        Other ->
            Other
    end.
