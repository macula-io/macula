%% Registry node startup script
-module('start-registry').
-export([start/0]).

start() ->
    %% Start macula application
    io:format("==> Starting Macula registry node...~n"),

    case application:ensure_all_started(macula) of
        {ok, _Started} ->
            io:format("==> Registry node started successfully~n"),
            %% Keep node alive
            timer:sleep(infinity);
        {error, Reason} ->
            io:format("==> Failed to start registry: ~p~n", [Reason]),
            halt(1)
    end.
