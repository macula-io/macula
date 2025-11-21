%% @doc Local client for in-VM workloads to connect to macula_gateway
%%
%% This module provides process-to-process communication between workloads
%% running in the same BEAM VM as the Macula platform and the local gateway.
%% Unlike macula_peer which creates QUIC connections, this connects directly
%% to the local macula_gateway process.
%%
%% Architecture:
%%   Phoenix/Elixir App → macula_local_client → macula_gateway
%%                                               ↓ (QUIC)
%%                                          Other Peers
%%
%% @end
-module(macula_local_client).
-behaviour(gen_server).
-behaviour(macula_client_behaviour).

%% API - Connection management
-export([connect/2, connect_local/1, disconnect/1]).
%% API - Pub/Sub
-export([publish/3, publish/4, subscribe/3, unsubscribe/2, discover_subscribers/2]).
%% API - RPC
-export([call/3, call/4, advertise/3, advertise/4, unadvertise/2]).
%% API - Utility
-export([get_node_id/1]).
%% Legacy API (kept for backward compatibility)
-export([start_link/1, stop/1, register_procedure/3, unregister_procedure/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    realm :: binary(),
    gateway_pid :: pid() | undefined,
    subscriptions = #{} :: #{binary() => reference()},
    registrations = #{} :: #{binary() => fun()},
    event_handler :: pid() | undefined
}).

%%==============================================================================
%% API
%%==============================================================================

%%------------------------------------------------------------------------------
%% Connection Management
%%------------------------------------------------------------------------------

%% @doc Connect to remote gateway (not supported for local client)
%% For compatibility with macula_client_behaviour
-spec connect(map(), pid()) -> {error, not_supported}.
connect(_Opts, _EventHandler) ->
    {error, not_supported}.

%% @doc Create a local client connection to the gateway
-spec connect_local(map()) -> {ok, pid()} | {error, term()}.
connect_local(Opts) ->
    start_link(Opts).

%% @doc Disconnect the client
-spec disconnect(pid()) -> ok.
disconnect(Pid) ->
    stop(Pid).

%% @doc Start a local client connection to the gateway (legacy API)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the local client (legacy API)
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%------------------------------------------------------------------------------
%% Pub/Sub Operations
%%------------------------------------------------------------------------------

%% @doc Publish an event to a topic
-spec publish(pid(), binary(), map()) -> ok | {error, term()}.
publish(Pid, Topic, Payload) ->
    publish(Pid, Topic, Payload, #{}).

%% @doc Publish an event to a topic with options
-spec publish(pid(), binary(), map(), map()) -> ok | {error, term()}.
publish(Pid, Topic, Payload, Opts) ->
    gen_server:call(Pid, {publish, Topic, Payload, Opts}).

%% @doc Subscribe to a topic
-spec subscribe(pid(), binary(), pid()) -> {ok, reference()} | {error, term()}.
subscribe(Pid, Topic, HandlerPid) ->
    gen_server:call(Pid, {subscribe, Topic, HandlerPid}).

%% @doc Unsubscribe from a topic
-spec unsubscribe(pid(), reference()) -> ok | {error, term()}.
unsubscribe(Pid, SubRef) ->
    gen_server:call(Pid, {unsubscribe, SubRef}).

%% @doc Discover subscribers of a topic via DHT query
-spec discover_subscribers(pid(), binary()) -> {ok, [binary()]} | {error, term()}.
discover_subscribers(Pid, Topic) ->
    gen_server:call(Pid, {discover_subscribers, Topic}).

%%------------------------------------------------------------------------------
%% RPC Operations
%%------------------------------------------------------------------------------

%% @doc Call an RPC procedure with default options
-spec call(pid(), binary(), list()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args) ->
    call(Pid, Procedure, Args, #{}).

%% @doc Call an RPC procedure
-spec call(pid(), binary(), list(), map()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Opts) ->
    gen_server:call(Pid, {call, Procedure, Args, Opts}, 30000).

%% @doc Advertise an RPC service with default options
-spec advertise(pid(), binary(), fun()) -> {ok, reference()} | {error, term()}.
advertise(Pid, Procedure, Handler) ->
    advertise(Pid, Procedure, Handler, #{}).

%% @doc Advertise an RPC service with options
-spec advertise(pid(), binary(), fun(), map()) -> {ok, reference()} | {error, term()}.
advertise(Pid, Procedure, Handler, Opts) ->
    gen_server:call(Pid, {advertise, Procedure, Handler, Opts}).

%% @doc Unadvertise an RPC service
-spec unadvertise(pid(), binary()) -> ok | {error, term()}.
unadvertise(Pid, Procedure) ->
    gen_server:call(Pid, {unadvertise, Procedure}).

%% @doc Register an RPC procedure (legacy API, use advertise/3 instead)
-spec register_procedure(pid(), binary(), fun()) -> ok | {error, term()}.
register_procedure(Pid, Procedure, Handler) ->
    advertise(Pid, Procedure, Handler, #{}).

%% @doc Unregister an RPC procedure (legacy API, use unadvertise/2 instead)
-spec unregister_procedure(pid(), binary()) -> ok | {error, term()}.
unregister_procedure(Pid, Procedure) ->
    unadvertise(Pid, Procedure).

%%------------------------------------------------------------------------------
%% Utility Operations
%%------------------------------------------------------------------------------

%% @doc Get the node ID of the local gateway
-spec get_node_id(pid()) -> {ok, binary()} | {error, term()}.
get_node_id(Pid) ->
    gen_server:call(Pid, get_node_id).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

init(Opts) ->
    Realm = maps:get(realm, Opts, <<"default">>),
    EventHandler = maps:get(event_handler, Opts, self()),

    io:format("[LocalClient] Initializing local client for realm ~s~n", [Realm]),

    %% Find the local gateway process
    case find_gateway() of
        {ok, GatewayPid} ->
            io:format("[LocalClient] Connected to local gateway: ~p~n", [GatewayPid]),
            monitor(process, GatewayPid),
            State = #state{
                realm = Realm,
                gateway_pid = GatewayPid,
                event_handler = EventHandler
            },
            {ok, State};
        {error, Reason} ->
            io:format("[LocalClient] Failed to find gateway: ~p~n", [Reason]),
            {stop, {gateway_not_found, Reason}}
    end.

handle_call({publish, Topic, Payload, Opts}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm} = State,

    %% Send publish request to gateway via gen_server:call
    %% Note: Opts are currently ignored for local clients
    _ = Opts,
    Result = case gen_server:call(Gateway, {local_publish, Realm, Topic, Payload}) of
        ok -> ok;
        {error, _} = Error -> Error
    end,
    {reply, Result, State};

handle_call({subscribe, Topic, HandlerPid}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm, subscriptions = Subs} = State,

    %% Subscribe via local gateway
    case gen_server:call(Gateway, {local_subscribe, Realm, Topic, HandlerPid}) of
        {ok, SubRef} ->
            NewSubs = maps:put(Topic, SubRef, Subs),
            {reply, {ok, SubRef}, State#state{subscriptions = NewSubs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({unsubscribe, SubRef}, _From, State) ->
    #state{gateway_pid = Gateway, subscriptions = Subs} = State,

    case gen_server:call(Gateway, {local_unsubscribe, SubRef}) of
        ok ->
            %% Remove from local tracking
            NewSubs = maps:filter(fun(_, Ref) -> Ref =/= SubRef end, Subs),
            {reply, ok, State#state{subscriptions = NewSubs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({discover_subscribers, Topic}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm} = State,

    %% Forward DHT query to gateway
    Result = gen_server:call(Gateway, {local_discover_subscribers, Realm, Topic}),
    {reply, Result, State};

handle_call({call, Procedure, Args, Opts}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm} = State,

    %% Route RPC call through local gateway
    Result = gen_server:call(Gateway, {local_rpc_call, Realm, Procedure, Args, Opts}, 30000),
    {reply, Result, State};

handle_call({advertise, Procedure, Handler, Opts}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm, registrations = Regs} = State,

    %% Forward advertise request to gateway
    case gen_server:call(Gateway, {local_advertise, Realm, Procedure, Handler, Opts}) of
        {ok, Ref} ->
            NewRegs = maps:put(Procedure, Handler, Regs),
            {reply, {ok, Ref}, State#state{registrations = NewRegs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({unadvertise, Procedure}, _From, State) ->
    #state{gateway_pid = Gateway, registrations = Regs} = State,

    %% Forward unadvertise request to gateway
    case gen_server:call(Gateway, {local_unadvertise, Procedure}) of
        ok ->
            NewRegs = maps:remove(Procedure, Regs),
            {reply, ok, State#state{registrations = NewRegs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({register, Procedure, Handler}, _From, State) ->
    #state{gateway_pid = Gateway, realm = Realm, registrations = Regs} = State,

    case gen_server:call(Gateway, {local_register_procedure, Realm, Procedure, Handler}) of
        ok ->
            NewRegs = maps:put(Procedure, Handler, Regs),
            {reply, ok, State#state{registrations = NewRegs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({unregister, Procedure}, _From, State) ->
    #state{gateway_pid = Gateway, registrations = Regs} = State,

    case gen_server:call(Gateway, {local_unregister_procedure, Procedure}) of
        ok ->
            NewRegs = maps:remove(Procedure, Regs),
            {reply, ok, State#state{registrations = NewRegs}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(get_node_id, _From, State) ->
    #state{gateway_pid = Gateway} = State,
    Result = gen_server:call(Gateway, local_get_node_id),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, GatewayPid, Reason}, #state{gateway_pid = GatewayPid} = State) ->
    io:format("[LocalClient] Gateway down: ~p. Attempting reconnect...~n", [Reason]),
    %% Try to reconnect to gateway
    case find_gateway() of
        {ok, NewGateway} ->
            io:format("[LocalClient] Reconnected to gateway: ~p~n", [NewGateway]),
            monitor(process, NewGateway),
            {noreply, State#state{gateway_pid = NewGateway}};
        {error, _} ->
            io:format("[LocalClient] Gateway not available, stopping~n"),
            {stop, gateway_unavailable, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{subscriptions = Subs, registrations = Regs, gateway_pid = Gateway}) ->
    %% Clean up subscriptions
    maps:foreach(fun(_, SubRef) ->
        gen_server:call(Gateway, {local_unsubscribe, SubRef})
    end, Subs),

    %% Clean up registrations (unadvertise all procedures)
    maps:foreach(fun(Procedure, _) ->
        gen_server:call(Gateway, {local_unadvertise, Procedure})
    end, Regs),

    ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

%% @doc Find the local gateway process via whereis
-spec find_gateway() -> {ok, pid()} | {error, not_found}.
find_gateway() ->
    case whereis(macula_gateway) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        undefined ->
            {error, not_found}
    end.
