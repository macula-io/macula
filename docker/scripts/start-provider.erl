%% Provider node startup script
-module('start-provider').
-export([start/3]).

start(ProviderName, RegistryEndpoint, Multiplier) ->
    io:format("==> Starting provider: ~s~n", [ProviderName]),

    %% Start macula application
    case application:ensure_all_started(macula) of
        {ok, _} ->
            io:format("==> Macula started~n"),

            %% Connect to registry
            Opts = #{
                realm => <<"com.test">>,
                node_id => list_to_binary(ProviderName)
            },

            case macula_client:start_link(list_to_binary(RegistryEndpoint), Opts) of
                {ok, Pid} ->
                    io:format("==> Connected to registry: ~s~n", [RegistryEndpoint]),

                    %% Define handler
                    Handler = fun(Args) ->
                        io:format("[~s] Handling call with args: ~p~n", [ProviderName, Args]),
                        X = maps:get(<<"x">>, Args, 1),
                        Result = X * Multiplier,
                        {ok, #{
                            provider => list_to_binary(ProviderName),
                            result => Result,
                            timestamp => erlang:system_time(second)
                        }}
                    end,

                    %% Advertise service
                    case macula_client:advertise(Pid, <<"test.calculator">>, Handler, #{ttl => 300}) of
                        ok ->
                            io:format("[~s] Service advertised successfully~n", [ProviderName]),
                            timer:sleep(infinity);
                        {error, Reason} ->
                            io:format("[~s] Failed to advertise: ~p~n", [ProviderName, Reason]),
                            halt(1)
                    end;
                {error, Reason} ->
                    io:format("==> Connection failed: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("==> Failed to start Macula: ~p~n", [Reason]),
            halt(1)
    end.
