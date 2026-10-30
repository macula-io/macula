%% A macula_subscriber module with neither notice callback (handle_interrupted/2,
%% handle_gap/2), for macula_client_delivery_gap_tests: its events go to the
%% test process, and the notices must leave it serving.
-module(macula_client_delivery_gap_plain_subscriber).

-behaviour(macula_subscriber).
-export([init/1, handle_event/4]).

init(Test) -> {ok, Test}.

handle_event(Topic, Payload, _Meta, Test) ->
    Test ! {plain_event, {Topic, Payload}},
    {noreply, Test}.
