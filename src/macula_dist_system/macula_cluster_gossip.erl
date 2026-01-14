%%%-------------------------------------------------------------------
%%% @doc Macula UDP Multicast Gossip Cluster Strategy.
%%%
%%% Zero-config cluster formation using UDP multicast for LAN discovery.
%%% Equivalent to libcluster's Cluster.Strategy.Gossip.
%%%
%%% == How It Works ==
%%%
%%% Each node:
%%% 1. Joins a multicast group (default 230.1.1.251)
%%% 2. Periodically broadcasts its node name
%%% 3. Listens for broadcasts from other nodes
%%% 4. Connects to discovered nodes via Erlang distribution
%%%
%%% == Configuration ==
%%%
%%% Start with options:
%%%
%%% ```
%%% {ok, _Pid} = macula_cluster_gossip:start_link(#{
%%%     multicast_addr => {230, 1, 1, 251},  %% Multicast group
%%%     port => 45892,                        %% UDP port
%%%     broadcast_interval => 1500,           %% ms between broadcasts
%%%     multicast_ttl => 1,                   %% TTL (1 = same subnet)
%%%     secret => <<"my-cluster-secret">>     %% Optional: filter by secret
%%% }).
%%% '''
%%%
%%% Or from environment variables:
%%%
%%% ```
%%% %% MACULA_GOSSIP_ADDR=230.1.1.251
%%% %% MACULA_GOSSIP_PORT=45892
%%% %% MACULA_GOSSIP_SECRET=my-cluster-secret
%%% {ok, _Pid} = macula_cluster_gossip:start_link(#{}).
%%% '''
%%%
%%% == Network Requirements ==
%%%
%%% - Nodes must be on the same multicast-enabled network
%%% - UDP port must be open in firewalls
%%% - Multicast must be enabled on network interfaces
%%% - For Docker, use `network_mode: host` or macvlan
%%%
%%% == Callbacks ==
%%%
%%% Register a callback to receive cluster events:
%%%
%%% ```
%%% {ok, _Pid} = macula_cluster_gossip:start_link(#{
%%%     callback => self()  %% PID or {Module, Function}
%%% }).
%%% %% Receives: {macula_cluster, nodeup, Node}
%%% %% Receives: {macula_cluster, nodedown, Node}
%%% '''
%%%
%%% @copyright 2026 Macula.io Apache-2.0
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_gossip).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0,
    stop/1,
    get_discovered/0,
    get_discovered/1,
    get_connected/0,
    get_connected/1,
    broadcast_now/0,
    broadcast_now/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MULTICAST_ADDR, {230, 1, 1, 251}).
-define(DEFAULT_PORT, 45892).
-define(DEFAULT_BROADCAST_INTERVAL, 1500).
-define(DEFAULT_MULTICAST_TTL, 1).
-define(HEARTBEAT_PREFIX, <<"MACULA_GOSSIP:">>).
-define(HEARTBEAT_PREFIX_LEN, 14).  %% Length of "MACULA_GOSSIP:"

-record(state, {
    %% UDP socket for multicast
    socket :: inet:socket() | undefined,

    %% Multicast address
    multicast_addr :: inet:ip4_address(),

    %% UDP port
    port :: inet:port_number(),

    %% Broadcast interval in ms
    broadcast_interval :: pos_integer(),

    %% Multicast TTL
    multicast_ttl :: non_neg_integer(),

    %% Optional secret for filtering
    secret :: binary() | undefined,

    %% Discovered nodes (may not be connected yet)
    discovered :: sets:set(atom()),

    %% Currently connected nodes
    connected :: sets:set(atom()),

    %% Broadcast timer reference
    broadcast_timer :: reference() | undefined,

    %% Callback for cluster events
    callback :: pid() | {module(), atom()} | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the gossip cluster strategy with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the gossip cluster strategy with options.
%%
%% Options:
%% - multicast_addr: IPv4 multicast address (default {230, 1, 1, 251})
%% - port: UDP port (default 45892)
%% - broadcast_interval: Milliseconds between broadcasts (default 1500)
%% - multicast_ttl: Multicast TTL, 1 = same subnet (default 1)
%% - secret: Optional binary secret for filtering broadcasts
%% - callback: PID or {Module, Function} to receive cluster events
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Stop the gossip cluster strategy.
-spec stop() -> ok.
stop() ->
    stop(?SERVER).

%% @doc Stop a named gossip cluster strategy.
-spec stop(atom() | pid()) -> ok.
stop(NameOrPid) ->
    gen_server:stop(NameOrPid).

%% @doc Get the list of discovered nodes.
-spec get_discovered() -> [atom()].
get_discovered() ->
    get_discovered(?SERVER).

%% @doc Get discovered nodes from a named instance.
-spec get_discovered(atom() | pid()) -> [atom()].
get_discovered(NameOrPid) ->
    gen_server:call(NameOrPid, get_discovered).

%% @doc Get the list of currently connected nodes.
-spec get_connected() -> [atom()].
get_connected() ->
    get_connected(?SERVER).

%% @doc Get connected nodes from a named instance.
-spec get_connected(atom() | pid()) -> [atom()].
get_connected(NameOrPid) ->
    gen_server:call(NameOrPid, get_connected).

%% @doc Force an immediate broadcast.
-spec broadcast_now() -> ok.
broadcast_now() ->
    broadcast_now(?SERVER).

%% @doc Force an immediate broadcast on a named instance.
-spec broadcast_now(atom() | pid()) -> ok.
broadcast_now(NameOrPid) ->
    gen_server:cast(NameOrPid, broadcast_now).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Opts) ->
    process_flag(trap_exit, true),

    %% Resolve configuration
    MulticastAddr = resolve_multicast_addr(Opts),
    Port = resolve_port(Opts),
    BroadcastInterval = maps:get(broadcast_interval, Opts, ?DEFAULT_BROADCAST_INTERVAL),
    MulticastTTL = maps:get(multicast_ttl, Opts, ?DEFAULT_MULTICAST_TTL),
    Secret = resolve_secret(Opts),
    Callback = maps:get(callback, Opts, undefined),

    %% Open UDP socket with multicast
    case open_multicast_socket(MulticastAddr, Port, MulticastTTL) of
        {ok, Socket} ->
            %% Subscribe to node events
            ok = net_kernel:monitor_nodes(true),

            State = #state{
                socket = Socket,
                multicast_addr = MulticastAddr,
                port = Port,
                broadcast_interval = BroadcastInterval,
                multicast_ttl = MulticastTTL,
                secret = Secret,
                discovered = sets:new([{version, 2}]),
                connected = sets:new([{version, 2}]),
                callback = Callback
            },

            ?LOG_INFO("[macula_cluster_gossip] Started on ~p:~p (TTL=~p)",
                      [MulticastAddr, Port, MulticastTTL]),

            %% Start broadcasting immediately
            self() ! broadcast,

            {ok, State};

        {error, Reason} ->
            ?LOG_ERROR("[macula_cluster_gossip] Failed to open socket: ~p", [Reason]),
            {stop, {socket_open_failed, Reason}}
    end.

%% @private
handle_call(get_discovered, _From, State) ->
    {reply, sets:to_list(State#state.discovered), State};

handle_call(get_connected, _From, State) ->
    {reply, sets:to_list(State#state.connected), State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(broadcast_now, State) ->
    do_broadcast(State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% Periodic broadcast
handle_info(broadcast, State) ->
    do_broadcast(State),
    Timer = schedule_broadcast(State#state.broadcast_interval),
    {noreply, State#state{broadcast_timer = Timer}};

%% Incoming UDP packet
handle_info({udp, Socket, _SrcIP, _SrcPort, Packet}, #state{socket = Socket} = State) ->
    NewState = handle_gossip_packet(Packet, State),
    {noreply, NewState};

%% Node joined the cluster
handle_info({nodeup, Node}, State) ->
    case sets:is_element(Node, State#state.discovered) of
        true ->
            ?LOG_INFO("[macula_cluster_gossip] Node ~p connected", [Node]),
            NewConnected = sets:add_element(Node, State#state.connected),
            notify_callback(State#state.callback, nodeup, Node),
            {noreply, State#state{connected = NewConnected}};
        false ->
            %% Not a node we discovered
            {noreply, State}
    end;

%% Node left the cluster
handle_info({nodedown, Node}, State) ->
    case sets:is_element(Node, State#state.connected) of
        true ->
            ?LOG_WARNING("[macula_cluster_gossip] Node ~p disconnected", [Node]),
            NewConnected = sets:del_element(Node, State#state.connected),
            notify_callback(State#state.callback, nodedown, Node),
            {noreply, State#state{connected = NewConnected}};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    cancel_timer(State#state.broadcast_timer),
    catch net_kernel:monitor_nodes(false),
    close_socket(State#state.socket),
    ok.

%%%===================================================================
%%% Internal Functions - Socket Management
%%%===================================================================

%% @private Open multicast UDP socket
-spec open_multicast_socket(inet:ip4_address(), inet:port_number(), non_neg_integer()) ->
    {ok, inet:socket()} | {error, term()}.
open_multicast_socket(MulticastAddr, Port, TTL) ->
    SocketOpts = [
        binary,
        {active, true},
        {reuseaddr, true},
        {multicast_ttl, TTL},
        {multicast_loop, true},  %% Receive own broadcasts (useful for testing)
        {add_membership, {MulticastAddr, {0, 0, 0, 0}}}
    ],
    gen_udp:open(Port, SocketOpts).

%% @private Close socket
-spec close_socket(inet:socket() | undefined) -> ok.
close_socket(undefined) -> ok;
close_socket(Socket) ->
    gen_udp:close(Socket),
    ok.

%%%===================================================================
%%% Internal Functions - Gossip Protocol
%%%===================================================================

%% @private Send a broadcast with our node name
-spec do_broadcast(#state{}) -> ok.
do_broadcast(#state{socket = undefined}) ->
    ok;
do_broadcast(#state{socket = Socket, multicast_addr = Addr, port = Port, secret = Secret}) ->
    NodeName = atom_to_binary(node(), utf8),
    Payload = build_gossip_payload(NodeName, Secret),
    case gen_udp:send(Socket, Addr, Port, Payload) of
        ok ->
            ?LOG_DEBUG("[macula_cluster_gossip] Broadcast sent: ~p", [node()]),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("[macula_cluster_gossip] Broadcast failed: ~p", [Reason]),
            ok
    end.

%% @private Build gossip payload
-spec build_gossip_payload(binary(), binary() | undefined) -> binary().
build_gossip_payload(NodeName, undefined) ->
    <<?HEARTBEAT_PREFIX/binary, NodeName/binary>>;
build_gossip_payload(NodeName, Secret) ->
    %% Include HMAC for authentication when secret is set
    %% Use "|" as delimiter since ":" appears in node names
    Data = <<?HEARTBEAT_PREFIX/binary, NodeName/binary>>,
    HMAC = crypto:mac(hmac, sha256, Secret, Data),
    <<Data/binary, "|", (binary:encode_hex(HMAC))/binary>>.

%% @private Handle incoming gossip packet
-spec handle_gossip_packet(binary(), #state{}) -> #state{}.
handle_gossip_packet(Packet, State) ->
    case parse_gossip_packet(Packet, State#state.secret) of
        {ok, NodeName} ->
            handle_discovered_node(NodeName, State);
        {error, _Reason} ->
            State
    end.

%% @private Parse gossip packet
-spec parse_gossip_packet(binary(), binary() | undefined) -> {ok, atom()} | {error, term()}.
parse_gossip_packet(Packet, undefined) ->
    %% No secret, just extract node name
    Prefix = ?HEARTBEAT_PREFIX,
    PrefixLen = ?HEARTBEAT_PREFIX_LEN,
    case Packet of
        <<Prefix:PrefixLen/binary, Rest/binary>> ->
            %% May have HMAC suffix, strip it
            NodeNameBin = strip_hmac(Rest),
            {ok, binary_to_atom(NodeNameBin, utf8)};
        _ ->
            {error, invalid_prefix}
    end;
parse_gossip_packet(Packet, Secret) ->
    %% Verify HMAC - uses "|" as delimiter since ":" appears in node names
    Prefix = ?HEARTBEAT_PREFIX,
    PrefixLen = ?HEARTBEAT_PREFIX_LEN,
    case binary:split(Packet, <<"|">>) of
        [Data, HexHMAC] ->
            ExpectedHMAC = crypto:mac(hmac, sha256, Secret, Data),
            ReceivedHMAC = binary:decode_hex(HexHMAC),
            case crypto:hash_equals(ExpectedHMAC, ReceivedHMAC) of
                true ->
                    <<Prefix:PrefixLen/binary, NodeNameBin/binary>> = Data,
                    {ok, binary_to_atom(NodeNameBin, utf8)};
                false ->
                    {error, hmac_mismatch}
            end;
        _ ->
            %% No HMAC but we expect one - could be from a node without secret
            %% Depending on policy, either accept or reject
            {error, no_hmac}
    end.

%% @private Strip HMAC suffix if present (uses "|" as delimiter)
-spec strip_hmac(binary()) -> binary().
strip_hmac(Bin) ->
    case binary:split(Bin, <<"|">>) of
        [NodeName, _HMAC] -> NodeName;
        [NodeName] -> NodeName
    end.

%% @private Handle a discovered node
-spec handle_discovered_node(atom(), #state{}) -> #state{}.
handle_discovered_node(Node, State) when Node =:= node() ->
    %% Ignore our own broadcast
    State;
handle_discovered_node(Node, State) ->
    case sets:is_element(Node, State#state.discovered) of
        true ->
            %% Already known, try to connect if not connected
            try_connect(Node, State);
        false ->
            ?LOG_INFO("[macula_cluster_gossip] Discovered new node: ~p", [Node]),
            NewDiscovered = sets:add_element(Node, State#state.discovered),
            try_connect(Node, State#state{discovered = NewDiscovered})
    end.

%% @private Try to connect to a node
-spec try_connect(atom(), #state{}) -> #state{}.
try_connect(Node, State) ->
    case sets:is_element(Node, State#state.connected) of
        true ->
            %% Already connected
            State;
        false ->
            ?LOG_DEBUG("[macula_cluster_gossip] Attempting connection to ~p", [Node]),
            case net_kernel:connect_node(Node) of
                true ->
                    ?LOG_INFO("[macula_cluster_gossip] Connected to ~p", [Node]),
                    NewConnected = sets:add_element(Node, State#state.connected),
                    notify_callback(State#state.callback, nodeup, Node),
                    State#state{connected = NewConnected};
                false ->
                    ?LOG_DEBUG("[macula_cluster_gossip] Failed to connect to ~p", [Node]),
                    State;
                ignored ->
                    ?LOG_WARNING("[macula_cluster_gossip] net_kernel not running"),
                    State
            end
    end.

%%%===================================================================
%%% Internal Functions - Configuration
%%%===================================================================

%% @private Resolve multicast address from options or env
-spec resolve_multicast_addr(map()) -> inet:ip4_address().
resolve_multicast_addr(Opts) ->
    case maps:get(multicast_addr, Opts, undefined) of
        undefined ->
            parse_env_addr();
        Addr when is_tuple(Addr) ->
            Addr
    end.

%% @private Parse multicast address from env var
-spec parse_env_addr() -> inet:ip4_address().
parse_env_addr() ->
    case os:getenv("MACULA_GOSSIP_ADDR") of
        false ->
            ?DEFAULT_MULTICAST_ADDR;
        "" ->
            ?DEFAULT_MULTICAST_ADDR;
        AddrStr ->
            case inet:parse_ipv4_address(AddrStr) of
                {ok, Addr} -> Addr;
                {error, _} -> ?DEFAULT_MULTICAST_ADDR
            end
    end.

%% @private Resolve port from options or env
-spec resolve_port(map()) -> inet:port_number().
resolve_port(Opts) ->
    case maps:get(port, Opts, undefined) of
        undefined ->
            parse_env_port();
        Port when is_integer(Port) ->
            Port
    end.

%% @private Parse port from env var
-spec parse_env_port() -> inet:port_number().
parse_env_port() ->
    case os:getenv("MACULA_GOSSIP_PORT") of
        false ->
            ?DEFAULT_PORT;
        "" ->
            ?DEFAULT_PORT;
        PortStr ->
            try list_to_integer(PortStr)
            catch _:_ -> ?DEFAULT_PORT
            end
    end.

%% @private Resolve secret from options or env
-spec resolve_secret(map()) -> binary() | undefined.
resolve_secret(Opts) ->
    case maps:get(secret, Opts, undefined) of
        undefined ->
            parse_env_secret();
        Secret when is_binary(Secret) ->
            Secret;
        Secret when is_list(Secret) ->
            list_to_binary(Secret)
    end.

%% @private Parse secret from env var
-spec parse_env_secret() -> binary() | undefined.
parse_env_secret() ->
    case os:getenv("MACULA_GOSSIP_SECRET") of
        false -> undefined;
        "" -> undefined;
        Secret -> list_to_binary(Secret)
    end.

%%%===================================================================
%%% Internal Functions - Utilities
%%%===================================================================

%% @private Schedule next broadcast
-spec schedule_broadcast(pos_integer()) -> reference().
schedule_broadcast(Interval) ->
    erlang:send_after(Interval, self(), broadcast).

%% @private Cancel timer if defined
-spec cancel_timer(reference() | undefined) -> ok.
cancel_timer(undefined) -> ok;
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    ok.

%% @private Notify callback of cluster event
-spec notify_callback(pid() | {module(), atom()} | undefined,
                      nodeup | nodedown, atom()) -> ok.
notify_callback(undefined, _Event, _Node) ->
    ok;
notify_callback(Pid, Event, Node) when is_pid(Pid) ->
    Pid ! {macula_cluster, Event, Node},
    ok;
notify_callback({Module, Function}, Event, Node) ->
    try
        Module:Function(Event, Node)
    catch
        _:_ -> ok
    end,
    ok.
