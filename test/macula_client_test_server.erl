%%%-------------------------------------------------------------------
%%% @doc
%%% Test server for macula_client integration tests.
%%% Provides a minimal QUIC server that responds to SDK requests.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_client_test_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    get_port/1
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
    listener :: pid() | undefined,
    port :: inet:port_number(),
    connections :: [pid()],
    opts :: map()
}).

-define(DEFAULT_PORT, 0). % Use 0 for random available port
-define(DEFAULT_REALM, <<"test.realm">>).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the test server with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the test server with custom options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Stop the test server.
-spec stop(pid()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%% @doc Get the port the server is listening on.
-spec get_port(pid()) -> inet:port_number().
get_port(Server) ->
    gen_server:call(Server, get_port).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Port = maps:get(port, Opts, ?DEFAULT_PORT),
    Realm = maps:get(realm, Opts, ?DEFAULT_REALM),

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
            %% Get actual port if we used 0
            ActualPort = case Port of
                0 -> get_listener_port(Listener);
                _ -> Port
            end,

            %% Start accepting connections
            self() ! accept,

            {ok, #state{
                listener = Listener,
                port = ActualPort,
                connections = [],
                opts = Opts#{realm => Realm}
            }};
        {error, Reason} ->
            {stop, {listen_failed, Reason}}
    end.

handle_call(get_port, _From, State) ->
    {reply, State#state.port, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(accept, #state{listener = Listener} = State) ->
    %% Accept incoming connection
    case macula_quic:accept(Listener, 5000) of
        {ok, Conn} ->
            %% Spawn handler for this connection
            Handler = spawn_link(fun() -> handle_connection(Conn, State#state.opts) end),

            %% Continue accepting
            self() ! accept,

            {noreply, State#state{connections = [Handler | State#state.connections]}};
        {error, timeout} ->
            %% No connection, keep accepting
            self() ! accept,
            {noreply, State};
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            {stop, {accept_error, Reason}, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listener = Listener, connections = Conns}) ->
    %% Close all connections
    lists:foreach(fun(Conn) ->
        catch exit(Conn, shutdown)
    end, Conns),

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
handle_connection(Conn, Opts) ->
    Realm = maps:get(realm, Opts, ?DEFAULT_REALM),

    %% Accept stream
    case macula_quic:accept_stream(Conn, 5000) of
        {ok, Stream} ->
            %% Handle messages on this stream
            handle_stream(Stream, Conn, Realm);
        {error, Reason} ->
            io:format("Stream accept error: ~p~n", [Reason]),
            macula_quic:close(Conn)
    end.

%% @doc Handle messages on a stream.
handle_stream(Stream, Conn, Realm) ->
    case macula_quic:recv(Stream, 5000) of
        {ok, Data} ->
            %% Decode message
            case macula_protocol_decoder:decode(Data) of
                {ok, {connect, ConnectMsg}} ->
                    %% Validate realm
                    case maps:get(realm_id, ConnectMsg, undefined) of
                        Realm ->
                            %% Send successful response (for now, just echo)
                            io:format("Client connected to realm: ~p~n", [Realm]),
                            handle_stream(Stream, Conn, Realm);
                        _OtherRealm ->
                            %% Reject connection
                            io:format("Client rejected: wrong realm~n"),
                            macula_quic:close(Stream),
                            macula_quic:close(Conn)
                    end;
                {ok, {publish, PubMsg}} ->
                    %% Echo publish (for testing)
                    io:format("Received publish: ~p~n", [PubMsg]),
                    handle_stream(Stream, Conn, Realm);
                {ok, {call, CallMsg}} ->
                    %% Echo call with dummy reply
                    CallId = maps:get(call_id, CallMsg),
                    ReplyMsg = #{
                        call_id => CallId,
                        result => json:encode(#{status => <<"ok">>})
                    },
                    case macula_protocol_encoder:encode(reply, ReplyMsg) of
                        {ok, ReplyData} ->
                            macula_quic:send(Stream, ReplyData),
                            handle_stream(Stream, Conn, Realm);
                        {error, Reason} ->
                            io:format("Encode error: ~p~n", [Reason]),
                            handle_stream(Stream, Conn, Realm)
                    end;
                {error, Reason} ->
                    io:format("Decode error: ~p~n", [Reason]),
                    macula_quic:close(Stream),
                    macula_quic:close(Conn)
            end;
        {error, timeout} ->
            %% Keep waiting
            handle_stream(Stream, Conn, Realm);
        {error, Reason} ->
            io:format("Recv error: ~p~n", [Reason]),
            macula_quic:close(Stream),
            macula_quic:close(Conn)
    end.

%% @doc Get the port a listener is bound to.
get_listener_port(_Listener) ->
    %% This is a placeholder - quicer might not expose this easily
    %% For now, return a dummy port
    9999.
