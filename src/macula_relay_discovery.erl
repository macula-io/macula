%%%-------------------------------------------------------------------
%%% @doc
%%% Relay Discovery — geographic-aware relay selection.
%%%
%%% Maintains a ranked list of relay identities sorted by distance
%%% from the node's own location. Updated via three mechanisms:
%%%
%%% 1. Bootstrap: HTTP GET /topology from seed relays on startup
%%% 2. Real-time: _mesh.relay.up/down events via pubsub subscription
%%% 3. Periodic: Reconciliation poll every 5 minutes
%%%
%%% Nodes connect to the nearest available relay. On failover, the
%%% next nearest is selected instantly from the cached ranked list.
%%%
%%% Usage:
%%%   {ok, Pid} = macula_relay_discovery:start_link(Opts).
%%%   {ok, Url} = macula_relay_discovery:nearest().
%%%   {ok, Url} = macula_relay_discovery:nearest_except(FailedHostname).
%%%   Relays = macula_relay_discovery:ranked_relays().
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_discovery).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    nearest/0,
    nearest_except/1,
    ranked_relays/0,
    relay_count/0,
    mark_offline/1
]).

%% Exported for tests
-export([
    haversine_km/4,
    extract_hostname/1,
    build_topology_url/1,
    relay_status/1,
    to_float/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, macula_relay_discovery_cache).
-define(RECONCILE_MS, 300_000).  % 5 minutes
-define(BOOTSTRAP_TIMEOUT_MS, 15_000).

-record(relay_entry, {
    hostname :: binary(),
    url :: binary(),
    lat :: float(),
    lng :: float(),
    status :: online | offline,
    distance_km :: float()
}).

%%====================================================================
%% API
%%====================================================================

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

-spec nearest() -> {ok, binary()} | {error, no_relays}.
nearest() ->
    case ranked_online() of
        [#relay_entry{url = Url} | _] -> {ok, Url};
        [] -> {error, no_relays}
    end.

-spec nearest_except(binary()) -> {ok, binary()} | {error, no_relays}.
nearest_except(FailedHostname) ->
    case ranked_online_except(FailedHostname) of
        [#relay_entry{url = Url} | _] -> {ok, Url};
        [] -> {error, no_relays}
    end.

-spec ranked_relays() -> [#{hostname := binary(), url := binary(), distance_km := float(), status := atom()}].
ranked_relays() ->
    [#{hostname => E#relay_entry.hostname,
       url => E#relay_entry.url,
       distance_km => E#relay_entry.distance_km,
       status => E#relay_entry.status}
     || E <- ranked_all()].

relay_count() ->
    case ets:info(?TABLE, size) of
        undefined -> 0;
        N -> N
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    ets:new(?TABLE, [named_table, set, {keypos, #relay_entry.hostname},
                     public, {read_concurrency, true}]),
    Seeds = maps:get(seeds, Opts, []),
    MyLat = maps:get(lat, Opts, 0.0),
    MyLng = maps:get(lng, Opts, 0.0),
    State = #{seeds => Seeds, lat => MyLat, lng => MyLng, subscribed => false},
    self() ! bootstrap,
    {ok, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

%% Bootstrap — fetch topology from seeds
handle_info(bootstrap, #{seeds := Seeds, lat := Lat, lng := Lng} = State) ->
    bootstrap_from_seeds(Seeds, Lat, Lng),
    schedule_reconcile(),
    {noreply, State};

%% Periodic reconciliation
handle_info(reconcile, #{seeds := Seeds, lat := Lat, lng := Lng} = State) ->
    bootstrap_from_seeds(Seeds, Lat, Lng),
    schedule_reconcile(),
    {noreply, State};

%% Real-time relay events from mesh subscription
handle_info({relay_up, Hostname, RelayInfo}, #{lat := Lat, lng := Lng} = State) ->
    add_relay(Hostname, RelayInfo, Lat, Lng),
    {noreply, State};

handle_info({relay_down, Hostname}, State) ->
    mark_offline(Hostname),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

%%====================================================================
%% Bootstrap — fetch /topology from seed URLs
%%====================================================================

bootstrap_from_seeds(Seeds, Lat, Lng) ->
    Urls = build_topology_urls(Seeds),
    Results = fetch_topologies(Urls),
    process_topology_results(Results, Lat, Lng).

build_topology_urls(Seeds) ->
    [build_topology_url(S) || S <- Seeds].

build_topology_url(Seed) when is_binary(Seed) ->
    Host = extract_hostname(Seed),
    "https://" ++ binary_to_list(Host) ++ "/topology?n=90&s=-90&e=180&w=-180&z=10".

extract_hostname(Url) ->
    Stripped = re:replace(Url, "^https?://", "", [{return, binary}]),
    re:replace(Stripped, ":\\d+.*", "", [{return, binary}]).

fetch_topologies(Urls) ->
    lists:filtermap(fun(Url) ->
        case fetch_topology(Url) of
            {ok, Relays} -> {true, Relays};
            _ -> false
        end
    end, Urls).

fetch_topology(Url) ->
    case httpc:request(get, {Url, []},
                       [{timeout, ?BOOTSTRAP_TIMEOUT_MS},
                        {ssl, [{verify, verify_none}]}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            parse_topology_relays(Body);
        Other ->
            ?LOG_WARNING("[relay_discovery] Topology fetch failed ~s: ~p", [Url, Other]),
            {error, fetch_failed}
    end.

parse_topology_relays(Body) ->
    case json:decode(list_to_binary(Body)) of
        #{<<"relays">> := Relays} when is_list(Relays) ->
            {ok, Relays};
        _ ->
            {error, bad_format}
    end.

process_topology_results([], _Lat, _Lng) ->
    ?LOG_WARNING("[relay_discovery] No topology data from any seed"),
    ok;
process_topology_results(RelayLists, Lat, Lng) ->
    AllRelays = lists:usort(fun(A, B) ->
        maps:get(<<"hostname">>, A, <<>>) =< maps:get(<<"hostname">>, B, <<>>)
    end, lists:append(RelayLists)),
    lists:foreach(fun(R) -> ingest_relay(R, Lat, Lng) end, AllRelays),
    ?LOG_INFO("[relay_discovery] Loaded ~b relay identities", [length(AllRelays)]).

ingest_relay(RelayMap, MyLat, MyLng) ->
    Hostname = maps:get(<<"hostname">>, RelayMap, <<>>),
    RelayLat = to_float(maps:get(<<"lat">>, RelayMap, 0)),
    RelayLng = to_float(maps:get(<<"lng">>, RelayMap, 0)),
    Status = relay_status(maps:get(<<"status">>, RelayMap, <<"unknown">>)),
    Distance = haversine_km(MyLat, MyLng, RelayLat, RelayLng),
    Url = <<"https://", Hostname/binary, ":4433">>,
    Entry = #relay_entry{
        hostname = Hostname,
        url = Url,
        lat = RelayLat,
        lng = RelayLng,
        status = Status,
        distance_km = Distance
    },
    ets:insert(?TABLE, Entry).

%%====================================================================
%% Real-time updates
%%====================================================================

add_relay(Hostname, #{lat := Lat, lng := Lng} = _Info, MyLat, MyLng) ->
    Distance = haversine_km(MyLat, MyLng, Lat, Lng),
    Url = <<"https://", Hostname/binary, ":4433">>,
    Entry = #relay_entry{
        hostname = Hostname, url = Url,
        lat = Lat, lng = Lng,
        status = online, distance_km = Distance
    },
    ets:insert(?TABLE, Entry),
    ?LOG_DEBUG("[relay_discovery] Relay up: ~s (~.0f km)", [Hostname, Distance]).

mark_offline(Hostname) ->
    case ets:lookup(?TABLE, Hostname) of
        [Entry] ->
            ets:insert(?TABLE, Entry#relay_entry{status = offline}),
            ?LOG_DEBUG("[relay_discovery] Relay down: ~s", [Hostname]);
        [] ->
            ok
    end.

%%====================================================================
%% Ranked relay access (reads from ETS, no gen_server call)
%%====================================================================

ranked_all() ->
    Entries = ets:tab2list(?TABLE),
    lists:sort(fun(A, B) -> A#relay_entry.distance_km =< B#relay_entry.distance_km end, Entries).

ranked_online() ->
    [E || E <- ranked_all(), E#relay_entry.status =:= online].

ranked_online_except(FailedHostname) ->
    [E || E <- ranked_online(), E#relay_entry.hostname =/= FailedHostname].

%%====================================================================
%% Helpers
%%====================================================================

schedule_reconcile() ->
    erlang:send_after(?RECONCILE_MS, self(), reconcile).

relay_status(<<"online">>) -> online;
relay_status(_) -> offline.

to_float(V) when is_float(V) -> V;
to_float(V) when is_integer(V) -> float(V);
to_float(_) -> 0.0.

haversine_km(Lat1, Lng1, Lat2, Lng2) ->
    R = 6371.0,
    DLat = deg_to_rad(Lat2 - Lat1),
    DLng = deg_to_rad(Lng2 - Lng1),
    A = math:sin(DLat / 2) * math:sin(DLat / 2) +
        math:cos(deg_to_rad(Lat1)) * math:cos(deg_to_rad(Lat2)) *
        math:sin(DLng / 2) * math:sin(DLng / 2),
    C = 2 * math:atan2(math:sqrt(A), math:sqrt(1 - A)),
    R * C.

deg_to_rad(D) -> D * math:pi() / 180.0.
