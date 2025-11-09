%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Gateway - HTTP/3 Message Router
%%%
%%% Main API module for the Macula Gateway.
%%% The gateway can be embedded in applications or run standalone.
%%%
%%% Architecture:
%%% - QUIC Listener: Accepts HTTP/3 connections from SDK clients
%%% - Router: Routes pub/sub messages between clients
%%% - RPC: Handles remote procedure calls
%%% - Realm Manager: Manages multiple realms
%%%
%%% Usage (Embedded):
%%% ```
%%% {ok, Pid} = macula_gateway:start_link([
%%%     {port, 9443},
%%%     {realm, <<"be.cortexiq.energy">>}
%%% ]).
%%% '''
%%%
%%% Usage (Standalone):
%%% ```
%%% application:start(macula_gateway).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    get_stats/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    port :: inet:port_number(),
    realm :: binary(),
    listener :: pid() | undefined,
    clients :: #{pid() => client_info()},
    subscriptions :: #{binary() => [pid()]},  % topic => [client_pids]
    registrations :: #{binary() => pid()}     % procedure => client_pid
}).

-type client_info() :: #{
    realm := binary(),
    node_id := binary(),
    capabilities := [atom()]
}.

-define(DEFAULT_PORT, 9443).
-define(DEFAULT_REALM, <<"macula.default">>).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the gateway with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the gateway with custom options.
%% Options:
%%   {port, Port} - Listen port (default: 9443)
%%   {realm, Realm} - Default realm (default: "macula.default")
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the gateway.
-spec stop(pid()) -> ok.
stop(Gateway) ->
    gen_server:stop(Gateway).

%% @doc Get gateway statistics.
-spec get_stats(pid()) -> map().
get_stats(Gateway) ->
    gen_server:call(Gateway, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
    Realm = proplists:get_value(realm, Opts, ?DEFAULT_REALM),

    %% Generate self-signed cert for testing
    {ok, CertFile, KeyFile} = macula_quic_cert:generate_self_signed(),

    %% Start QUIC listener
    ListenOpts = [
        {cert, CertFile},
        {key, KeyFile},
        {alpn, ["macula"]},
        {peer_unidi_stream_count, 3}
    ],

    case macula_quic:listen(Port, ListenOpts) of
        {ok, Listener} ->
            io:format("Macula Gateway listening on port ~p (realm: ~s)~n", [Port, Realm]),

            %% Start accepting connections
            self() ! accept,

            State = #state{
                port = Port,
                realm = Realm,
                listener = Listener,
                clients = #{},
                subscriptions = #{},
                registrations = #{}
            },

            {ok, State};

        {error, Reason} ->
            {stop, {listen_failed, Reason}};

        {error, Type, Details} ->
            {stop, {listen_failed, {Type, Details}}};

        Other ->
            {stop, {listen_failed, Other}}
    end.

handle_call(get_stats, _From, State) ->
    Stats = #{
        port => State#state.port,
        realm => State#state.realm,
        clients => maps:size(State#state.clients),
        subscriptions => maps:size(State#state.subscriptions),
        registrations => maps:size(State#state.registrations)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(accept, #state{listener = Listener} = State) ->
    %% Accept incoming connection
    case macula_quic:accept(Listener, 5000) of
        {ok, Conn} ->
            %% Spawn handler for this connection
            spawn_link(fun() -> handle_client_connection(self(), Conn, State) end),

            %% Continue accepting
            self() ! accept,

            {noreply, State};

        {error, timeout} ->
            %% No connection, keep accepting
            self() ! accept,
            {noreply, State};

        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            {stop, {accept_error, Reason}, State}
    end;

%% Client registered
handle_info({client_connected, ClientPid, ClientInfo}, State) ->
    io:format("Client connected: ~p~n", [ClientInfo]),

    %% Monitor client
    erlang:monitor(process, ClientPid),

    Clients = maps:put(ClientPid, ClientInfo, State#state.clients),
    {noreply, State#state{clients = Clients}};

%% Client disconnected
handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
    io:format("Client disconnected: ~p~n", [ClientPid]),

    %% Remove client from all subscriptions
    Subscriptions = maps:map(fun(_Topic, Subscribers) ->
        lists:delete(ClientPid, Subscribers)
    end, State#state.subscriptions),

    %% Remove client registrations
    Registrations = maps:filter(fun(_Proc, Pid) ->
        Pid =/= ClientPid
    end, State#state.registrations),

    %% Remove client
    Clients = maps:remove(ClientPid, State#state.clients),

    {noreply, State#state{
        clients = Clients,
        subscriptions = Subscriptions,
        registrations = Registrations
    }};

%% Publish message (from client)
handle_info({publish, FromPid, Topic, Payload}, State) ->
    %% Get all subscribers to this topic
    Subscribers = maps:get(Topic, State#state.subscriptions, []),

    %% Send to all subscribers (except sender)
    lists:foreach(fun(SubPid) ->
        if SubPid =/= FromPid ->
            SubPid ! {event, Topic, Payload};
           true ->
            ok
        end
    end, Subscribers),

    {noreply, State};

%% Subscribe request
handle_info({subscribe, ClientPid, Topic}, State) ->
    %% Add client to topic subscribers
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    NewSubscribers = [ClientPid | Subscribers],
    Subscriptions = maps:put(Topic, NewSubscribers, State#state.subscriptions),

    %% Acknowledge subscription
    ClientPid ! {subscribed, Topic},

    {noreply, State#state{subscriptions = Subscriptions}};

%% Unsubscribe request
handle_info({unsubscribe, ClientPid, Topic}, State) ->
    %% Remove client from topic subscribers
    Subscribers = maps:get(Topic, State#state.subscriptions, []),
    NewSubscribers = lists:delete(ClientPid, Subscribers),
    Subscriptions = maps:put(Topic, NewSubscribers, State#state.subscriptions),

    %% Acknowledge unsubscription
    ClientPid ! {unsubscribed, Topic},

    {noreply, State#state{subscriptions = Subscriptions}};

%% RPC Call request
handle_info({call, FromPid, CallId, Procedure, Args}, State) ->
    %% Find registered handler for procedure
    case maps:get(Procedure, State#state.registrations, undefined) of
        undefined ->
            %% No handler registered
            FromPid ! {call_error, CallId, <<"wamp.error.no_such_procedure">>};
        HandlerPid ->
            %% Forward to handler
            HandlerPid ! {invoke, FromPid, CallId, Procedure, Args}
    end,

    {noreply, State};

%% Register procedure
handle_info({register, ClientPid, Procedure}, State) ->
    %% Register procedure handler
    Registrations = maps:put(Procedure, ClientPid, State#state.registrations),

    %% Acknowledge registration
    ClientPid ! {registered, Procedure},

    {noreply, State#state{registrations = Registrations}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener = Listener}) ->
    %% Close listener
    case Listener of
        undefined -> ok;
        _ -> macula_quic:close(Listener)
    end,
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Handle a client connection.
handle_client_connection(GatewayPid, Conn, State) ->
    %% Accept stream
    case macula_quic:accept_stream(Conn, 5000) of
        {ok, Stream} ->
            %% Wait for CONNECT message
            case macula_quic:recv(Stream, 5000) of
                {ok, Data} ->
                    case macula_protocol_decoder:decode(Data) of
                        {ok, {connect, ConnectMsg}} ->
                            %% Validate realm
                            RealmId = maps:get(realm_id, ConnectMsg),
                            case RealmId of
                                StateRealm when StateRealm =:= State#state.realm ->
                                    %% Register client
                                    ClientInfo = #{
                                        realm => RealmId,
                                        node_id => maps:get(node_id, ConnectMsg),
                                        capabilities => maps:get(capabilities, ConnectMsg, [])
                                    },

                                    GatewayPid ! {client_connected, self(), ClientInfo},

                                    %% Handle client messages
                                    client_message_loop(GatewayPid, Stream, Conn);

                                _OtherRealm ->
                                    %% Wrong realm, reject
                                    io:format("Client rejected: wrong realm~n"),
                                    macula_quic:close(Stream),
                                    macula_quic:close(Conn)
                            end;

                        {error, Reason} ->
                            io:format("Decode error: ~p~n", [Reason]),
                            macula_quic:close(Stream),
                            macula_quic:close(Conn)
                    end;

                {error, Reason} ->
                    io:format("Recv error: ~p~n", [Reason]),
                    macula_quic:close(Stream),
                    macula_quic:close(Conn)
            end;

        {error, Reason} ->
            io:format("Stream accept error: ~p~n", [Reason]),
            macula_quic:close(Conn)
    end.

%% @doc Message loop for connected client.
client_message_loop(GatewayPid, Stream, Conn) ->
    receive
        %% Event from gateway (subscribed topic)
        {event, Topic, Payload} ->
            %% Send event to client
            EventMsg = #{
                topic => Topic,
                payload => Payload
            },
            case macula_protocol_encoder:encode(publish, EventMsg) of
                {ok, Data} ->
                    macula_quic:send(Stream, Data);
                {error, Reason} ->
                    io:format("Encode error: ~p~n", [Reason])
            end,
            client_message_loop(GatewayPid, Stream, Conn);

        %% Subscription confirmed
        {subscribed, Topic} ->
            io:format("Subscription confirmed: ~s~n", [Topic]),
            client_message_loop(GatewayPid, Stream, Conn);

        %% Unsubscription confirmed
        {unsubscribed, Topic} ->
            io:format("Unsubscription confirmed: ~s~n", [Topic]),
            client_message_loop(GatewayPid, Stream, Conn);

        %% RPC invocation (this client is the handler)
        {invoke, CallerPid, CallId, Procedure, Args} ->
            %% Send invocation to client
            InvokeMsg = #{
                call_id => CallId,
                procedure => Procedure,
                args => Args
            },
            case macula_protocol_encoder:encode(call, InvokeMsg) of
                {ok, Data} ->
                    macula_quic:send(Stream, Data),
                    %% Wait for result from client
                    %% (Simplified - should track pending invocations)
                    CallerPid ! {call_result, CallId, <<"dummy result">>};
                {error, Reason} ->
                    io:format("Encode error: ~p~n", [Reason]),
                    CallerPid ! {call_error, CallId, <<"encoding_failed">>}
            end,
            client_message_loop(GatewayPid, Stream, Conn);

        %% Call result
        {call_result, _CallId, Result} ->
            %% Forward result (simplified)
            io:format("Call result: ~p~n", [Result]),
            client_message_loop(GatewayPid, Stream, Conn);

        %% Call error
        {call_error, _CallId, Error} ->
            %% Forward error
            io:format("Call error: ~p~n", [Error]),
            client_message_loop(GatewayPid, Stream, Conn)

    after 100 ->
        %% Check for incoming messages from client
        case macula_quic:recv(Stream, 100) of
            {ok, Data} ->
                handle_client_message(GatewayPid, Data),
                client_message_loop(GatewayPid, Stream, Conn);

            {error, timeout} ->
                client_message_loop(GatewayPid, Stream, Conn);

            {error, Reason} ->
                io:format("Client recv error: ~p~n", [Reason]),
                macula_quic:close(Stream),
                macula_quic:close(Conn)
        end
    end.

%% @doc Handle message from client.
handle_client_message(GatewayPid, Data) ->
    case macula_protocol_decoder:decode(Data) of
        {ok, {publish, PubMsg}} ->
            Topic = maps:get(topic, PubMsg),
            Payload = maps:get(payload, PubMsg),
            GatewayPid ! {publish, self(), Topic, Payload};

        {ok, {subscribe, SubMsg}} ->
            Topics = maps:get(topics, SubMsg, []),
            lists:foreach(fun(Topic) ->
                GatewayPid ! {subscribe, self(), Topic}
            end, Topics);

        {ok, {unsubscribe, UnsubMsg}} ->
            Topics = maps:get(topics, UnsubMsg, []),
            lists:foreach(fun(Topic) ->
                GatewayPid ! {unsubscribe, self(), Topic}
            end, Topics);

        {ok, {call, CallMsg}} ->
            CallId = maps:get(call_id, CallMsg),
            Procedure = maps:get(procedure, CallMsg),
            Args = maps:get(args, CallMsg),
            GatewayPid ! {call, self(), CallId, Procedure, Args};

        {error, Reason} ->
            io:format("Decode error: ~p~n", [Reason])
    end.
