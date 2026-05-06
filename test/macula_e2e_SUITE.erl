%%%-------------------------------------------------------------------
%%% @doc End-to-end harness against a real Macula relay fleet.
%%%
%%% Exercises every public V2 surface that consumers actually use:
%%%
%%%   - pool connect + healthy-link verification
%%%   - pubsub roundtrip (two independent pools)
%%%   - realm isolation
%%%   - unary RPC (advertise + call)
%%%   - streaming RPC (advertise_stream + call_stream + chunk drain)
%%%   - DHT put / find
%%%   - the live `_mesh.weather' topic (real stub fleet publishes here
%%%     every 60s under realm `io.macula')
%%%   - pool close → `macula_event_gone' delivery
%%%
%%% Bootstrap URL is configurable via the `MACULA_E2E_BOOTSTRAP' env
%%% var (comma-separated). Defaults to the canonical
%%% `https://boot.macula.io:4433'.
%%%
%%% The whole suite skips if the bootstrap is unreachable — running
%%% offline does not fail the build. Run with:
%%%
%%%   rebar3 ct --suite test/macula_e2e_SUITE
%%%
%%% Per-test timeouts are conservative (5–75s) because individual
%%% relay round-trips can be slow over WAN.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_e2e_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% CT callbacks
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Test cases
-export([
    pool_connect/1,
    pubsub_roundtrip/1,
    realm_isolation/1,
    unary_rpc/1,
    streaming_rpc/1,
    dht_put_find/1,
    weather_subscribe/1,
    pool_close_cleanup/1
]).

-define(DEFAULT_BOOTSTRAP, [<<"https://boot.macula.io:4433">>]).
-define(WEATHER_TOPIC,     <<"_mesh.weather">>).
-define(WAIT_HEALTHY_MS,   30_000).
-define(SUBSCRIBE_SETTLE_MS, 1_500).
-define(ADVERTISE_SETTLE_MS, 1_500).

%%====================================================================
%% CT callbacks
%%====================================================================

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [pool_connect,
     pubsub_roundtrip,
     realm_isolation,
     unary_rpc,
     streaming_rpc,
     dht_put_find,
     weather_subscribe,
     pool_close_cleanup].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(macula),
    Bootstrap = bootstrap_seeds(),
    ct:pal("[e2e] bootstrap = ~p", [Bootstrap]),
    {ok, Pool} = macula:connect(Bootstrap, #{}),
    on_initial_health(wait_for_healthy(Pool, ?WAIT_HEALTHY_MS),
                      Pool, Bootstrap, Config).

on_initial_health(ok, Pool, Bootstrap, Config) ->
    {ok, #{healthy_links := N}} = macula:status(Pool),
    ct:pal("[e2e] pool ready — ~p healthy link(s) of ~p seeds",
           [N, length(Bootstrap)]),
    [{pool, Pool},
     {bootstrap, Bootstrap},
     {test_realm,    macula_realm:id(<<"_test">>)},
     {weather_realm, macula_realm:id(<<"io.macula">>)}
     | Config];
on_initial_health(timeout, Pool, Bootstrap, _Config) ->
    macula:close(Pool),
    {skip, {fleet_not_reachable, Bootstrap}}.

end_per_suite(Config) ->
    on_end(proplists:get_value(pool, Config)).

on_end(undefined) -> ok;
on_end(Pool)      -> macula:close(Pool), ok.

%%====================================================================
%% Test cases
%%====================================================================

pool_connect(Config) ->
    Pool = ?config(pool, Config),
    {ok, #{healthy_links := N}} = macula:status(Pool),
    ?assert(N > 0),
    ct:pal("[e2e:pool_connect] healthy_links=~p", [N]).

pubsub_roundtrip(Config) ->
    %% Open a second pool so subscriber and publisher are independent.
    {Pub, Sub} = open_publisher_subscriber(Config),
    Realm = ?config(test_realm, Config),
    Topic = unique_topic(<<"e2e.pubsub">>),
    {ok, SubRef} = macula:subscribe(Sub, Realm, Topic, self()),
    timer:sleep(?SUBSCRIBE_SETTLE_MS),
    Token = crypto:strong_rand_bytes(16),
    Payload = #{<<"token">> => Token},
    ok = macula:publish(Pub, Realm, Topic, Payload),
    receive_event(SubRef, Topic, Payload, 5_000),
    macula:close(Sub).

realm_isolation(Config) ->
    {Pub, Sub} = open_publisher_subscriber(Config),
    RealmA = macula_realm:id(<<"_test_a">>),
    RealmB = macula_realm:id(<<"_test_b">>),
    Topic = unique_topic(<<"e2e.isolation">>),
    {ok, SubRef} = macula:subscribe(Sub, RealmA, Topic, self()),
    timer:sleep(?SUBSCRIBE_SETTLE_MS),
    %% Publish on B; subscriber on A must not see it.
    ok = macula:publish(Pub, RealmB, Topic, #{<<"secret">> => <<"only_in_B">>}),
    expect_no_event(SubRef, 2_000),
    %% And confirm the A subscriber is wired correctly by sending one on A.
    ok = macula:publish(Pub, RealmA, Topic, #{<<"ok">> => true}),
    receive_event(SubRef, Topic, #{<<"ok">> => true}, 5_000),
    macula:close(Sub).

unary_rpc(Config) ->
    {Server, Caller} = open_publisher_subscriber(Config),
    Realm = ?config(test_realm, Config),
    Procedure = unique_topic(<<"e2e.echo">>),
    Handler = fun(Args) -> {ok, #{<<"got">> => Args}} end,
    ok = macula:advertise(Server, Realm, Procedure, Handler, #{}),
    timer:sleep(?ADVERTISE_SETTLE_MS),
    Args = #{<<"x">> => 42},
    {ok, Reply} = macula:call(Caller, Realm, Procedure, Args, 5_000),
    ?assertEqual(#{<<"got">> => Args}, Reply),
    ok = macula:unadvertise(Server, Realm, Procedure),
    macula:close(Caller).

streaming_rpc(Config) ->
    {Server, Caller} = open_publisher_subscriber(Config),
    Realm = ?config(test_realm, Config),
    Procedure = unique_topic(<<"e2e.count">>),
    Handler = fun(Stream, #{<<"n">> := N}) ->
        lists:foreach(
          fun(I) -> ok = macula:send(Stream, integer_to_binary(I)) end,
          lists:seq(1, N)),
        macula:close_stream(Stream)
    end,
    ok = macula:advertise_stream(Server, Realm, Procedure, server_stream, Handler),
    timer:sleep(?ADVERTISE_SETTLE_MS),
    {ok, Stream} = macula:call_stream(Caller, Realm, Procedure,
                                      #{<<"n">> => 3}, #{}),
    Got = drain_stream(Stream, []),
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>], Got),
    ok = macula:unadvertise_stream(Server, Realm, Procedure),
    macula:close(Caller).

dht_put_find(Config) ->
    Pool = ?config(pool, Config),
    Realm = ?config(test_realm, Config),
    %% Build a fresh-identity node_record so the put doesn't collide
    %% with any existing entry. node_record is the simplest signed
    %% record type; the put round-trips ok+ack through the station's
    %% _dht.put_record handler.
    Identity = macula_identity:generate(),
    NodeId = macula_identity:public(Identity),
    Record = macula_record:node_record(NodeId, [Realm], 0),
    Signed = macula_record:sign(Record, Identity),
    Key = macula_record:storage_key(Signed),
    case macula:put_record(Pool, Signed) of
        ok ->
            timer:sleep(2_000),  %% give DHT replication a beat
            {ok, Found} = macula:find_record(Pool, Key),
            ?assertMatch(#{type := _, payload := _, sig := _}, Found),
            ct:pal("[e2e:dht_put_find] round-tripped record key=~p",
                   [binary:encode_hex(Key)]);
        {error, Reason} ->
            ct:fail({put_record_failed, Reason})
    end.

weather_subscribe(Config) ->
    %% The real stub fleet publishes _mesh.weather every 60s under
    %% realm io.macula. Subscribe and verify at least one event lands
    %% within the 75s window. Dependent on the fleet actually
    %% publishing — surfaces directly any cascading outage like the
    %% one observed at the start of this session.
    Bootstrap = ?config(bootstrap, Config),
    {ok, Sub} = macula:connect(Bootstrap, #{}),
    ok = wait_for_healthy(Sub, 10_000),
    Realm = ?config(weather_realm, Config),
    {ok, SubRef} = macula:subscribe(Sub, Realm, ?WEATHER_TOPIC, self()),
    Got = collect_events(SubRef, 75_000, []),
    macula:close(Sub),
    ct:pal("[e2e:weather_subscribe] received ~p weather event(s) in 75s",
           [length(Got)]),
    ?assert(length(Got) >= 1).

pool_close_cleanup(Config) ->
    Bootstrap = ?config(bootstrap, Config),
    {ok, P} = macula:connect(Bootstrap, #{}),
    ok = wait_for_healthy(P, 10_000),
    Realm = ?config(test_realm, Config),
    Topic = unique_topic(<<"e2e.cleanup">>),
    {ok, SubRef} = macula:subscribe(P, Realm, Topic, self()),
    timer:sleep(500),
    macula:close(P),
    receive
        {macula_event_gone, SubRef, Reason} ->
            ct:pal("[e2e:pool_close_cleanup] reason=~p", [Reason]),
            ok
    after 5_000 ->
        ct:fail(no_event_gone_message)
    end.

%%====================================================================
%% Helpers
%%====================================================================

bootstrap_seeds() ->
    bootstrap_seeds(os:getenv("MACULA_E2E_BOOTSTRAP")).

bootstrap_seeds(false) -> ?DEFAULT_BOOTSTRAP;
bootstrap_seeds(Env) ->
    [list_to_binary(string:trim(U))
     || U <- string:split(Env, ",", all),
        string:trim(U) =/= ""].

wait_for_healthy(_Pool, RemainingMs) when RemainingMs =< 0 ->
    timeout;
wait_for_healthy(Pool, RemainingMs) ->
    on_status(macula:status(Pool), Pool, RemainingMs).

on_status({ok, #{healthy_links := N}}, _Pool, _RemainingMs) when N > 0 ->
    ok;
on_status(_Other, Pool, RemainingMs) ->
    timer:sleep(500),
    wait_for_healthy(Pool, RemainingMs - 500).

%% Open a second independent pool to drive a subscriber / caller side.
%% Returns {Pub, Sub} or {Server, Caller} depending on the test's
%% naming convention; the suite-shared pool plays the first role.
open_publisher_subscriber(Config) ->
    Bootstrap = ?config(bootstrap, Config),
    {ok, Other} = macula:connect(Bootstrap, #{}),
    ok = wait_for_healthy(Other, 10_000),
    {?config(pool, Config), Other}.

unique_topic(Prefix) ->
    Suffix = integer_to_binary(erlang:unique_integer([positive])),
    <<Prefix/binary, ".", Suffix/binary>>.

receive_event(SubRef, Topic, ExpectedPayload, TimeoutMs) ->
    receive
        {macula_event, SubRef, Topic, Got, _Meta} ->
            ?assertEqual(ExpectedPayload, Got)
    after TimeoutMs ->
        ct:fail({no_event, Topic, {expected, ExpectedPayload}})
    end.

expect_no_event(SubRef, TimeoutMs) ->
    receive
        {macula_event, SubRef, Topic, Payload, _Meta} ->
            ct:fail({unexpected_event, Topic, Payload})
    after TimeoutMs ->
        ok
    end.

drain_stream(Stream, Acc) ->
    on_recv(macula:recv(Stream, 5_000), Stream, Acc).

on_recv({chunk, Bin}, Stream, Acc) ->
    drain_stream(Stream, [Bin | Acc]);
on_recv(eof, _Stream, Acc) ->
    lists:reverse(Acc);
on_recv({error, _} = E, _Stream, _Acc) ->
    ct:fail({stream_error, E}).

collect_events(_SubRef, RemainingMs, Acc) when RemainingMs =< 0 ->
    Acc;
collect_events(SubRef, RemainingMs, Acc) ->
    Start = erlang:monotonic_time(millisecond),
    receive
        {macula_event, SubRef, _Topic, Payload, _Meta} ->
            collect_events(SubRef, RemainingMs - elapsed(Start),
                           [Payload | Acc])
    after RemainingMs ->
        Acc
    end.

elapsed(StartMs) ->
    erlang:monotonic_time(millisecond) - StartMs.
