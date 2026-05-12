%% Subscribe to `_mesh.weather' on a single station and count inbound
%% EVENT frames for a fixed window. Used to close Track A: prove that
%% PUBLISH frames from the stub fleet actually route to a subscriber,
%% not just return ok at the sender boundary.
%%
%% Usage from the SDK repo root:
%%
%%   rebar3 shell --apps macula \
%%       --script scripts/subscribe_mesh_weather.erl \
%%       --eval 'subscribe_mesh_weather:run(<<"station-be-brussels.macula.io">>, 60_000).'
%%
%% Or compile + run inline (see runner script).
-module(subscribe_mesh_weather).
-export([run/2]).

run(Host, WindowMs) when is_binary(Host), is_integer(WindowMs) ->
    {ok, _} = application:ensure_all_started(macula),
    Identity = macula_identity:generate(),
    Seed = <<"https://", Host/binary, ":4433">>,
    io:format("[subscribe] connecting to ~s~n", [Seed]),
    {ok, Link} = macula_station_link:start_link(#{seed => Seed, identity => Identity}),
    %% Wait for HELLO. The link gen_server sets peer_node_id once the
    %% handshake completes; subscribe queues if peer_node_id is unset
    %% but for clarity we wait explicitly.
    ok = wait_connected(Link, 10_000),
    io:format("[subscribe] HELLO done; subscribing to _mesh.weather~n"),
    Realm = macula_realm:id(<<"io.macula">>),
    {ok, SubRef} = macula_station_link:subscribe(Link, Realm, <<"_mesh.weather">>,
                                                  self()),
    Deadline = erlang:system_time(millisecond) + WindowMs,
    {EventCount, Publishers} = collect(SubRef, 0, sets:new(), Deadline),
    io:format("[subscribe] DONE: window=~pms events=~p distinct_publishers=~p~n",
              [WindowMs, EventCount, sets:size(Publishers)]),
    macula_station_link:stop(Link),
    {EventCount, sets:size(Publishers)}.

wait_connected(Link, BudgetMs) ->
    Deadline = erlang:system_time(millisecond) + BudgetMs,
    wait_connected_loop(Link, Deadline).

wait_connected_loop(Link, Deadline) ->
    case macula_station_link:is_connected(Link) of
        true -> ok;
        false ->
            case erlang:system_time(millisecond) >= Deadline of
                true -> {error, hello_timeout};
                false ->
                    timer:sleep(200),
                    wait_connected_loop(Link, Deadline)
            end
    end.

collect(SubRef, N, Pubs, Deadline) ->
    Now = erlang:system_time(millisecond),
    Remaining = max(0, Deadline - Now),
    case Remaining of
        0 -> {N, Pubs};
        _ ->
            receive
                {macula_event, SubRef, _Topic, _Payload, Meta} ->
                    Pub = maps:get(publisher, Meta, undefined),
                    collect(SubRef, N + 1, sets:add_element(Pub, Pubs), Deadline);
                {macula_event_gone, SubRef, Reason} ->
                    io:format("[subscribe] event_gone: ~p~n", [Reason]),
                    {N, Pubs}
            after Remaining ->
                {N, Pubs}
            end
    end.
