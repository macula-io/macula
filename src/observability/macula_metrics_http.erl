%%%-------------------------------------------------------------------
%%% @doc Prometheus text-format pull endpoint for {@link macula_metrics}.
%%%
%%% Phase 4.1 — see PLAN_MACULA_NET_PHASE4_1_OBSERVABILITY.md.
%%%
%%% Embeds an `inets' httpd listener that serves a single resource:
%%% `GET /metrics' returns the current snapshot in
%%% Prometheus text-exposition format (v0.0.4 / OpenMetrics-compatible).
%%%
%%% Defaults bind 127.0.0.1; remote scraping requires an explicit
%%% bind address via `start_link/1'.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_metrics_http).

-behaviour(gen_server).

-export([start_link/0, start_link/1, stop/0, port/0, render/0]).

%% inets mod_* callback — exported so httpd can dispatch requests.
-export([do/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DEFAULT_PORT, 9145).
-define(DEFAULT_BIND, {127,0,0,1}).

-record(state, {
    httpd_pid :: pid() | undefined,
    port      :: inet:port_number(),
    bind      :: inet:ip_address()
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
stop() -> gen_server:stop(?MODULE).

%% @doc Returns the actual port the server is listening on (useful when
%% port=0 is passed for tests).
-spec port() -> inet:port_number().
port() ->
    gen_server:call(?MODULE, port).

%% =============================================================================
%% gen_server
%% =============================================================================

init(Opts) ->
    application:ensure_all_started(inets),
    Port = maps:get(port, Opts, ?DEFAULT_PORT),
    Bind = parse_bind(maps:get(bind, Opts, ?DEFAULT_BIND)),
    {ok, Pid} = start_httpd(Port, Bind),
    ActualPort = httpd_port(Pid),
    {ok, #state{httpd_pid = Pid, port = ActualPort, bind = Bind}}.

handle_call(port, _From, #state{port = P} = S) ->
    {reply, P, S};
handle_call(_, _From, S) ->
    {reply, ok, S}.

handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.

terminate(_, #state{httpd_pid = undefined}) -> ok;
terminate(_, #state{httpd_pid = Pid}) ->
    _ = inets:stop(httpd, Pid),
    ok.

%% =============================================================================
%% Inets mod_* callback
%% =============================================================================

-include_lib("inets/include/httpd.hrl").

%% inets calls do/1 with a #mod{} record per HTTP request.
-spec do(#mod{}) -> {proceed, list()}.
do(#mod{request_uri = "/metrics"} = Mod) ->
    serve_metrics(Mod);
do(#mod{request_uri = "/metrics/"} = Mod) ->
    serve_metrics(Mod);
do(#mod{} = _Mod) ->
    {proceed, [{response, {404, "not found"}}]}.

serve_metrics(_Mod) ->
    Body    = iolist_to_binary(render()),
    Length  = integer_to_list(byte_size(Body)),
    Headers = [
        {code, 200},
        {content_type, "text/plain; version=0.0.4; charset=utf-8"},
        {content_length, Length}
    ],
    {proceed, [{response, {response, Headers, [Body]}}]}.

%% @doc Render the current metric snapshot as Prometheus text format.
-spec render() -> iolist().
render() ->
    [render_metric(M) || M <- macula_metrics:gather()].

%% =============================================================================
%% Prometheus text rendering
%% =============================================================================

render_metric(#{name := N, type := counter, help := H, samples := Ss}) ->
    [help_line(N, H), type_line(N, <<"counter">>),
     [counter_line(N, S) || S <- Ss]];
render_metric(#{name := N, type := gauge, help := H, samples := Ss}) ->
    [help_line(N, H), type_line(N, <<"gauge">>),
     [gauge_line(N, S) || S <- Ss]];
render_metric(#{name := N, type := histogram, help := H, samples := Ss}) ->
    [help_line(N, H), type_line(N, <<"histogram">>),
     [hist_lines(N, S) || S <- Ss]].

help_line(N, H)        -> [<<"# HELP ">>, N, $\s, escape_help(H), $\n].
type_line(N, T)        -> [<<"# TYPE ">>, N, $\s, T, $\n].

counter_line(N, #{labels := L, value := V}) ->
    [N, render_labels(L), $\s, num(V), $\n].

gauge_line(N, #{labels := L, value := V}) ->
    [N, render_labels(L), $\s, num(V), $\n].

hist_lines(N, #{labels := L, buckets := Bs, sum := Sum, count := Cnt}) ->
    BucketLines = [hist_bucket_line(N, L, B, C) || {B, C} <- Bs],
    InfLine     = [N, <<"_bucket">>, render_labels_with_le(L, <<"+Inf">>),
                   $\s, num(Cnt), $\n],
    SumLine     = [N, <<"_sum">>,    render_labels(L), $\s, num(Sum), $\n],
    CountLine   = [N, <<"_count">>,  render_labels(L), $\s, num(Cnt), $\n],
    [BucketLines, InfLine, SumLine, CountLine].

hist_bucket_line(N, L, B, C) ->
    [N, <<"_bucket">>, render_labels_with_le(L, num(B)),
     $\s, num(C), $\n].

render_labels([])    -> <<>>;
render_labels(L)     -> [${, lists:join($,, [label_pair(K, V) || {K, V} <- L]), $}].

render_labels_with_le(L, LeBin) ->
    LePair = [<<"le=\"">>, LeBin, $"],
    Rest   = [label_pair(K, V) || {K, V} <- L],
    [${, lists:join($,, [LePair | Rest]), $}].

label_pair(K, V) when is_atom(K) ->
    [atom_to_binary(K, utf8), <<"=\"">>, escape_label(V), $"].

escape_label(V) when is_binary(V) -> escape_label_bin(V, <<>>).

escape_label_bin(<<>>, Acc) -> Acc;
escape_label_bin(<<$\\, Rest/binary>>, Acc) -> escape_label_bin(Rest, <<Acc/binary, "\\\\">>);
escape_label_bin(<<$",  Rest/binary>>, Acc) -> escape_label_bin(Rest, <<Acc/binary, "\\\"">>);
escape_label_bin(<<$\n, Rest/binary>>, Acc) -> escape_label_bin(Rest, <<Acc/binary, "\\n">>);
escape_label_bin(<<C,   Rest/binary>>, Acc) -> escape_label_bin(Rest, <<Acc/binary, C>>).

escape_help(H) when is_binary(H) ->
    %% HELP escapes only \ and \n
    escape_help_bin(H, <<>>).

escape_help_bin(<<>>, Acc) -> Acc;
escape_help_bin(<<$\\, Rest/binary>>, Acc) -> escape_help_bin(Rest, <<Acc/binary, "\\\\">>);
escape_help_bin(<<$\n, Rest/binary>>, Acc) -> escape_help_bin(Rest, <<Acc/binary, "\\n">>);
escape_help_bin(<<C,   Rest/binary>>, Acc) -> escape_help_bin(Rest, <<Acc/binary, C>>).

num(N) when is_integer(N) -> integer_to_binary(N);
num(N) when is_float(N)   -> float_to_binary(N, [{decimals, 6}, compact]).

%% =============================================================================
%% Internals
%% =============================================================================

parse_bind({_,_,_,_} = Ip)         -> Ip;
parse_bind({_,_,_,_,_,_,_,_} = Ip) -> Ip;
parse_bind(Bin) when is_binary(Bin) -> parse_bind(binary_to_list(Bin));
parse_bind(Str) when is_list(Str) ->
    {ok, Ip} = inet:parse_address(Str),
    Ip.

start_httpd(Port, Bind) ->
    BindStr = inet:ntoa(Bind),
    inets:start(httpd, [
        {port, Port},
        {bind_address, BindStr},
        {server_name, "macula_metrics"},
        {server_root, "/tmp"},
        {document_root, "/tmp"},
        {modules, [?MODULE]},
        {mime_types, [{"txt", "text/plain"}]}
    ]).

httpd_port(Pid) ->
    Info = httpd:info(Pid),
    proplists:get_value(port, Info).
