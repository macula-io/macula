#!/usr/bin/env escript
%%% @doc
%%% Macula Decentralized Chat Demo
%%%
%%% This demonstrates two applications chatting through firewalls
%%% without a central server in the data path.
%%%
%%% The gateway is only used for:
%%% 1. Initial bootstrap/connection
%%% 2. Service discovery via DHT
%%% 3. Pub/sub routing (which can be decentralized)
%%%
%%% Usage:
%%%   # Terminal 1 - Start Alice
%%%   ./chat_demo.erl alice
%%%
%%%   # Terminal 2 - Start Bob
%%%   ./chat_demo.erl bob
%%%
%%% Then type messages in either terminal to chat!
%%% @end

-mode(compile).

main([]) ->
    io:format("Usage: ./chat_demo.erl <alice|bob>~n"),
    halt(1);

main([Name]) ->
    NameAtom = list_to_atom(Name),
    io:format("~n=== Macula Decentralized Chat Demo ===~n"),
    io:format("Starting ~s...~n~n", [Name]),

    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    application:ensure_all_started(macula),

    %% Connect to gateway
    io:format("[~s] Connecting to Macula Gateway...~n", [Name]),
    GatewayUrl = <<"https://localhost:9443">>,
    Realm = <<"com.example.chat">>,

    case macula_client:connect(GatewayUrl, #{realm => Realm, node_id => list_to_binary(Name)}) of
        {ok, Client} ->
            io:format("[~s] Connected! Realm: ~s~n", [Name, Realm]),
            io:format("[~s] Subscribing to chat.messages...~n", [Name]),

            %% Subscribe to chat topic
            CallbackFun = fun(Msg) -> handle_message(Name, Msg) end,
            case macula_client:subscribe(Client, <<"chat.messages">>, CallbackFun) of
                {ok, _SubRef} ->
                    io:format("[~s] Ready to chat! Type your messages:~n~n", [Name]),
                    %% Interactive chat loop
                    chat_loop(NameAtom, Client);
                {error, SubError} ->
                    io:format("[~s] Subscribe error: ~p~n", [Name, SubError]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("[~s] Connection error: ~p~n", [Name, Reason]),
            halt(1)
    end.

chat_loop(Name, Client) ->
    %% Read line from stdin
    case io:get_line(atom_to_list(Name) ++ "> ") of
        eof ->
            io:format("~nGoodbye!~n"),
            macula_client:disconnect(Client),
            halt(0);
        {error, _} ->
            chat_loop(Name, Client);
        Line ->
            Message = string:trim(Line),
            case Message of
                "" ->
                    chat_loop(Name, Client);
                "/quit" ->
                    io:format("~nGoodbye!~n"),
                    macula_client:disconnect(Client),
                    halt(0);
                _ ->
                    %% Publish message
                    Event = #{
                        from => atom_to_binary(Name),
                        message => list_to_binary(Message),
                        timestamp => erlang:system_time(second)
                    },
                    case macula_client:publish(Client, <<"chat.messages">>, Event) of
                        ok ->
                            ok;
                        {error, PubError} ->
                            io:format("[~s] Publish error: ~p~n", [Name, PubError])
                    end,
                    chat_loop(Name, Client)
            end
    end.

handle_message(MyName, #{<<"from">> := From, <<"message">> := Msg}) ->
    case binary_to_atom(From) of
        MyName ->
            ok; % Don't show our own messages
        OtherName ->
            io:format("~n[~s]: ~s~n~s> ", [OtherName, Msg, MyName])
    end,
    ok;
handle_message(_MyName, _OtherMsg) ->
    %% Ignore malformed messages
    ok.
