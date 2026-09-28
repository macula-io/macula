%% A macula_subscriber module with only the required callbacks, for macula_subscriber_tests.
-module(macula_subscriber_plain).

-behaviour(macula_subscriber).
-export([init/1, handle_event/4]).

init(Parent) -> {ok, Parent}.

handle_event(Topic, Payload, Meta, Parent) ->
    Parent ! {seen, Topic, Payload, Meta},
    {noreply, Parent}.
