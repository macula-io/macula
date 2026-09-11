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
%%%     secret => &lt;&lt;"at least 32 bytes of shared secret"&gt;&gt;  %% Required
%%% }).
%%% '''
%%%
%%% Or from environment variables:
%%%
%%% ```
%%% %% MACULA_GOSSIP_ADDR=230.1.1.251
%%% %% MACULA_GOSSIP_PORT=45892
%%% %% MACULA_GOSSIP_SECRET=at-least-32-bytes-of-shared-secret
%%% {ok, _Pid} = macula_cluster_gossip:start_link(#{}).
%%% '''
%%%
%%% == Shared secret ==
%%%
%%% Gossip does not start without a shared secret of at least 32 bytes, given
%%% as the `secret' option or in `MACULA_GOSSIP_SECRET': `start_link/1'
%%% returns `{error, secret_required}' or
%%% `{error, {secret_too_short, #{bytes => N, required => 32}}}'. Every packet
%%% carries an HMAC-SHA256 tag over the node name, and a packet whose tag does
%%% not verify is dropped.
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

%% Runs in a dial process of its own, spawned by the gossip server.
-export([dial_node/1]).

-ifdef(TEST).
-export([parse_gossip_packet/2]).
-endif.

-define(SERVER, ?MODULE).
-define(DEFAULT_MULTICAST_ADDR, {230, 1, 1, 251}).
-define(DEFAULT_PORT, 45892).
-define(DEFAULT_BROADCAST_INTERVAL, 1500).
-define(DEFAULT_MULTICAST_TTL, 1).
-define(HEARTBEAT_PREFIX, <<"MACULA_GOSSIP:">>).
%% Bytes of shared secret gossip needs to start.
-define(MIN_SECRET_BYTES, 32).
%% A tag is an HMAC-SHA256 in hex.
-define(TAG_HEX_CHARS, 64).
%% The longest node name, as for any atom.
-define(MAX_NAME_BYTES, 255).
%% The most atoms one gossip server makes for node names.
-define(MAX_NODE_ATOMS, 1024).

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

    %% Shared secret that tags our packets and verifies received ones
    secret :: binary(),

    %% Discovered nodes (may not be connected yet)
    discovered :: sets:set(atom()),

    %% Currently connected nodes
    connected :: sets:set(atom()),

    %% Dials in flight, by the monitor on the process dialling each node
    dialling = #{} :: #{reference() => atom()},

    %% Atoms this server made for node names, capped at MAX_NODE_ATOMS
    atoms_made = 0 :: non_neg_integer(),

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
%% - secret: Shared secret of at least 32 bytes, or MACULA_GOSSIP_SECRET.
%%   Required: without it start_link/1 returns {error, secret_required}, and
%%   with a shorter one {error, {secret_too_short, #{bytes => N, required => 32}}}
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
    start_with_secret(secret_check(resolve_secret(Opts)), Opts).

%% @private Gossip starts only with a shared secret of MIN_SECRET_BYTES or more.
secret_check(undefined) ->
    {error, secret_required};
secret_check(Secret) when byte_size(Secret) < ?MIN_SECRET_BYTES ->
    {error, {secret_too_short, #{bytes => byte_size(Secret), required => ?MIN_SECRET_BYTES}}};
secret_check(Secret) ->
    {ok, Secret}.

%% @private The refusal is an init error, so the caller of start_link/1 gets
%% it back and keeps running.
start_with_secret({error, Reason} = Refused, _Opts) ->
    ?LOG_ERROR("[macula_cluster_gossip] Not started: ~p", [Reason]),
    Refused;
start_with_secret({ok, Secret}, Opts) ->
    %% Resolve configuration
    MulticastAddr = resolve_multicast_addr(Opts),
    Port = resolve_port(Opts),
    BroadcastInterval = maps:get(broadcast_interval, Opts, ?DEFAULT_BROADCAST_INTERVAL),
    MulticastTTL = maps:get(multicast_ttl, Opts, ?DEFAULT_MULTICAST_TTL),
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

%% A dial ended: its process exits with the dial's result
handle_info({'DOWN', Ref, process, _Pid, Reason}, #state{dialling = Dialling} = State)
  when is_map_key(Ref, Dialling) ->
    {Node, Rest} = maps:take(Ref, Dialling),
    {noreply, connect_result(dial_result(Reason), Node, State#state{dialling = Rest})};

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
    try net_kernel:monitor_nodes(false) catch _:_ -> ok end,
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
        %% Bound to the group address, so only packets sent to the group arrive
        {ip, MulticastAddr},
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
-spec build_gossip_payload(binary(), binary()) -> binary().
build_gossip_payload(NodeName, Secret) ->
    %% An HMAC-SHA256 tag in hex authenticates the packet
    %% Use "|" as delimiter since ":" appears in node names
    Data = <<?HEARTBEAT_PREFIX/binary, NodeName/binary>>,
    HMAC = crypto:mac(hmac, sha256, Secret, Data),
    <<Data/binary, "|", (binary:encode_hex(HMAC))/binary>>.

%% @private Handle an incoming gossip packet. Nothing on this path raises: a
%% packet whose tag does not verify, or whose name does not have the shape of
%% a node name, is dropped.
-spec handle_gossip_packet(binary(), #state{}) -> #state{}.
handle_gossip_packet(Packet, State) ->
    handle_parsed(parse_gossip_packet(Packet, State#state.secret), State).

handle_parsed({ok, NodeName}, State) ->
    discover(NodeName, State);
handle_parsed({error, Reason}, State) ->
    ?LOG_DEBUG("[macula_cluster_gossip] Dropped a packet: ~p", [Reason]),
    State.

%% @private The node name a packet announces: the prefix, the name, "|", and
%% a tag of exactly TAG_HEX_CHARS hex characters that verifies against the
%% secret. The name must have the shape of a node name.
-spec parse_gossip_packet(binary(), binary()) -> {ok, binary()} | {error, atom()}.
parse_gossip_packet(Packet, Secret) ->
    verified(binary:split(Packet, <<"|">>), Secret).

verified([Data, Tag], Secret) ->
    announced(tag_matches(Tag, crypto:mac(hmac, sha256, Secret, Data)), Data);
verified(_Parts, _Secret) ->
    {error, malformed}.

%% The tag's characters are checked before it is decoded, and the decoded tag
%% is compared with the expected HMAC in constant time.
tag_matches(Tag, Expected) when byte_size(Tag) =:= ?TAG_HEX_CHARS ->
    all_hex(Tag) andalso crypto:hash_equals(binary:decode_hex(Tag), Expected);
tag_matches(_Tag, _Expected) ->
    false.

all_hex(Bin) ->
    lists:all(fun is_hex/1, binary_to_list(Bin)).

is_hex(C) ->
    (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) orelse (C >= $A andalso C =< $F).

announced(true, <<"MACULA_GOSSIP:", NodeName/binary>>) ->
    shaped(node_name_shape(NodeName), NodeName);
announced(true, _Data) ->
    {error, malformed};
announced(false, _Data) ->
    {error, bad_tag}.

shaped(true, NodeName) -> {ok, NodeName};
shaped(false, _NodeName) -> {error, bad_name}.

%% name@host within MAX_NAME_BYTES: letters, digits, _ and - in the name, and
%% the same, . or : in the host, which may be a hostname, an IPv4 address or
%% an IPv6 literal.
node_name_shape(NodeName) when byte_size(NodeName) =< ?MAX_NAME_BYTES ->
    name_and_host(binary:split(NodeName, <<"@">>, [global]));
node_name_shape(_NodeName) ->
    false.

name_and_host([Name, Host]) when Name =/= <<>>, Host =/= <<>> ->
    only(Name, "_-") andalso only(Host, "_-.:");
name_and_host(_Parts) ->
    false.

only(Bin, Extra) ->
    lists:all(fun(C) -> is_alnum(C) orelse lists:member(C, Extra) end, binary_to_list(Bin)).

is_alnum(C) ->
    (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse (C >= $0 andalso C =< $9).

%% @private A verified node name. A name whose atom exists is discovered as
%% it is; for any other name an atom is made only while this server has made
%% fewer than MAX_NODE_ATOMS.
-spec discover(binary(), #state{}) -> #state{}.
discover(NodeName, State) ->
    discover_node(existing_atom(NodeName), NodeName, State).

existing_atom(NodeName) ->
    try binary_to_existing_atom(NodeName, utf8) of
        Node -> {atom, Node}
    catch
        error:badarg -> no_atom
    end.

discover_node({atom, Node}, _NodeName, State) ->
    handle_discovered_node(Node, State);
discover_node(no_atom, NodeName, #state{atoms_made = Made} = State) ->
    make_node_atom(Made < ?MAX_NODE_ATOMS, NodeName, State).

make_node_atom(true, NodeName, #state{atoms_made = Made} = State) ->
    handle_discovered_node(binary_to_atom(NodeName, utf8), State#state{atoms_made = Made + 1});
make_node_atom(false, NodeName, State) ->
    ?LOG_DEBUG("[macula_cluster_gossip] Made ~b node atoms, ignoring ~ts",
               [?MAX_NODE_ATOMS, NodeName]),
    State.

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

%% @private Try to connect to a node. The dial runs in a monitored process of
%% its own, one per node at a time, so a host that is slow to answer never
%% holds up the gossip server. That process exits with the dial's result.
-spec try_connect(atom(), #state{}) -> #state{}.
try_connect(Node, State) ->
    try_connect_unconnected(sets:is_element(Node, State#state.connected)
                            orelse lists:member(Node, maps:values(State#state.dialling)),
                            Node, State).

try_connect_unconnected(true, _Node, State) ->
    %% Already connected, or being dialled
    State;
try_connect_unconnected(false, Node, #state{dialling = Dialling} = State) ->
    ?LOG_DEBUG("[macula_cluster_gossip] Attempting connection to ~p", [Node]),
    {_Pid, Ref} = spawn_monitor(?MODULE, dial_node, [Node]),
    State#state{dialling = Dialling#{Ref => Node}}.

%% @private Dials Node and exits with the result, for the gossip server that
%% monitors this process.
-spec dial_node(node()) -> no_return().
dial_node(Node) ->
    exit({dialled, net_kernel:connect_node(Node)}).

dial_result({dialled, Result}) -> Result;
dial_result(_Crashed) -> false.

connect_result(true, Node, State) ->
    ?LOG_INFO("[macula_cluster_gossip] Connected to ~p", [Node]),
    NewConnected = sets:add_element(Node, State#state.connected),
    notify_callback(State#state.callback, nodeup, Node),
    State#state{connected = NewConnected};
connect_result(false, Node, State) ->
    ?LOG_DEBUG("[macula_cluster_gossip] Failed to connect to ~p", [Node]),
    State;
connect_result(ignored, _Node, State) ->
    ?LOG_WARNING("[macula_cluster_gossip] net_kernel not running"),
    State.

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
    parse_env_addr(os:getenv("MACULA_GOSSIP_ADDR")).

parse_env_addr(false) ->
    ?DEFAULT_MULTICAST_ADDR;
parse_env_addr("") ->
    ?DEFAULT_MULTICAST_ADDR;
parse_env_addr(AddrStr) ->
    parsed_addr(inet:parse_ipv4_address(AddrStr)).

parsed_addr({ok, Addr}) -> Addr;
parsed_addr({error, _}) -> ?DEFAULT_MULTICAST_ADDR.

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
    parse_port_from_env(os:getenv("MACULA_GOSSIP_PORT")).

%% @private Parse port from env value
parse_port_from_env(false) ->
    ?DEFAULT_PORT;
parse_port_from_env("") ->
    ?DEFAULT_PORT;
parse_port_from_env(PortStr) ->
    handle_port_int_parse(
        try list_to_integer(PortStr) catch _:Reason -> {'EXIT', Reason} end).

%% @private Handle port parse result
handle_port_int_parse({'EXIT', _}) ->
    ?DEFAULT_PORT;
handle_port_int_parse(Port) when is_integer(Port) ->
    Port;
handle_port_int_parse(_) ->
    ?DEFAULT_PORT.

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
    _ = (try Module:Function(Event, Node) catch _:_ -> ok end),
    ok.
