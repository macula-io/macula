%%%-------------------------------------------------------------------
%%% @doc
%%% Test helper for macula_gateway - Starts gateway without QUIC listener
%%% for pure logic testing.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_test_helper).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-type client_info() :: #{
    realm := binary(),
    node_id := binary(),
    capabilities := [atom()]
}.

-record(state, {
    port :: inet:port_number(),
    realm :: binary(),
    listener :: undefined,  % No real listener in test mode
    clients :: #{pid() => client_info()},
    subscriptions :: #{binary() => [pid()]},
    registrations :: #{binary() => pid()}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link({Port, Realm}) ->
    gen_server:start_link(?MODULE, {Port, Realm}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Port, Realm}) ->
    State = #state{
        port = Port,
        realm = Realm,
        listener = undefined,  % No QUIC listener in test mode
        clients = #{},
        subscriptions = #{},
        registrations = #{}
    },
    {ok, State}.

%% Same handle_call as macula_gateway
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

%% Same handle_info as macula_gateway (routing logic)

%% Client registered
handle_info({client_connected, ClientPid, ClientInfo}, State) ->
    %% Monitor client
    erlang:monitor(process, ClientPid),
    Clients = maps:put(ClientPid, ClientInfo, State#state.clients),
    {noreply, State#state{clients = Clients}};

%% Client disconnected
handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
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

terminate(_Reason, _State) ->
    ok.
