%%%-------------------------------------------------------------------
%%% @doc
%%% Protocol message type definitions and constants for Macula mesh.
%%% Defines all message types that can be sent over QUIC streams.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_protocol_types).

%% Message type constants
-export([
    message_type_id/1,
    message_type_name/1
]).

%% Type exports
-export_type([
    message_type/0,
    message/0,
    connect_msg/0,
    disconnect_msg/0,
    ping_msg/0,
    pong_msg/0,
    publish_msg/0,
    subscribe_msg/0,
    unsubscribe_msg/0,
    call_msg/0,
    reply_msg/0,
    cast_msg/0,
    rpc_route_msg/0,
    pubsub_route_msg/0
]).

%%%===================================================================
%%% Message Type IDs
%%%===================================================================

%% Control messages
-define(MSG_CONNECT,        16#01).
-define(MSG_DISCONNECT,     16#02).
-define(MSG_PING,           16#03).
-define(MSG_PONG,           16#04).

%% Pub/Sub messages
-define(MSG_PUBLISH,        16#10).
-define(MSG_SUBSCRIBE,      16#11).
-define(MSG_UNSUBSCRIBE,    16#12).
-define(MSG_PUBSUB_ROUTE,   16#13).

%% RPC messages
-define(MSG_CALL,           16#20).
-define(MSG_REPLY,          16#21).
-define(MSG_CAST,           16#22).
-define(MSG_RPC_ROUTE,      16#23).
-define(MSG_RPC_REQUEST,    16#24).  % NATS-style async RPC request
-define(MSG_RPC_REPLY,      16#25).  % NATS-style async RPC reply

%% SWIM membership messages
-define(MSG_SWIM_PING,      16#30).
-define(MSG_SWIM_ACK,       16#31).
-define(MSG_SWIM_PING_REQ,  16#32).

%% Kademlia routing messages
-define(MSG_FIND_NODE,      16#40).
-define(MSG_FIND_NODE_REPLY,16#41).
-define(MSG_STORE,          16#42).
-define(MSG_FIND_VALUE,     16#43).
-define(MSG_FIND_VALUE_REPLY,16#44).

%% NAT Traversal messages (0x50-0x5F range)
-define(MSG_NAT_PROBE,          16#50).
-define(MSG_NAT_PROBE_REPLY,    16#51).
-define(MSG_PUNCH_REQUEST,      16#52).
-define(MSG_PUNCH_COORDINATE,   16#53).
-define(MSG_PUNCH_EXECUTE,      16#54).
-define(MSG_PUNCH_RESULT,       16#55).
-define(MSG_RELAY_REQUEST,      16#56).
-define(MSG_RELAY_DATA,         16#57).

%%%===================================================================
%%% Type Definitions
%%%===================================================================

-type message_type() ::
    connect | disconnect | ping | pong |
    publish | subscribe | unsubscribe | pubsub_route |
    call | reply | cast | rpc_route | rpc_request | rpc_reply |
    swim_ping | swim_ack | swim_ping_req |
    find_node | find_node_reply | store | find_value | find_value_reply |
    nat_probe | nat_probe_reply | punch_request | punch_coordinate |
    punch_execute | punch_result | relay_request | relay_data.

-type message() ::
    {connect, connect_msg()} |
    {disconnect, disconnect_msg()} |
    {ping, ping_msg()} |
    {pong, pong_msg()} |
    {publish, publish_msg()} |
    {subscribe, subscribe_msg()} |
    {unsubscribe, unsubscribe_msg()} |
    {pubsub_route, pubsub_route_msg()} |
    {call, call_msg()} |
    {reply, reply_msg()} |
    {cast, cast_msg()} |
    {rpc_route, rpc_route_msg()}.

%% Control Messages

-type connect_msg() :: #{
    version := binary(),           % Protocol version "1.0"
    node_id := binary(),           % 32-byte node ID
    realm_id := binary(),          % 32-byte realm ID
    capabilities := [atom()],      % List of supported features
    endpoint => binary()           % Optional: "https://host:port" for peer connections
}.

-type disconnect_msg() :: #{
    reason := atom(),              % normal | error | timeout
    message := binary()            % Human-readable reason
}.

-type ping_msg() :: #{
    timestamp := integer()         % Monotonic timestamp (milliseconds)
}.

-type pong_msg() :: #{
    timestamp := integer(),        % Original ping timestamp
    server_time := integer()       % Server's monotonic timestamp
}.

%% Pub/Sub Messages

-type publish_msg() :: #{
    topic := binary(),             % Topic name
    payload := binary(),           % Message payload
    qos := 0 | 1 | 2,             % Quality of service
    retain := boolean(),           % Retain flag
    message_id := binary()         % 16-byte unique message ID
}.

-type subscribe_msg() :: #{
    topics := [binary()],          % List of topic patterns
    qos := 0 | 1 | 2              % Requested QoS level
}.

-type unsubscribe_msg() :: #{
    topics := [binary()]           % Topics to unsubscribe from
}.

%% RPC Messages

-type call_msg() :: #{
    procedure := binary(),         % Procedure name (e.g., "my.app.get_user")
    args := binary(),              % JSON-encoded arguments
    call_id := binary(),           % 16-byte unique call ID
    timeout => integer()           % Optional timeout in milliseconds
}.

-type reply_msg() :: #{
    call_id := binary(),           % Matching call_id from call_msg
    result => binary(),            % JSON-encoded result (on success)
    error => #{                    % Error details (on failure)
        code := binary(),          % Error code
        message := binary()        % Error message
    }
}.

-type cast_msg() :: #{
    procedure := binary(),         % Procedure name
    args := binary()               % JSON-encoded arguments (no reply expected)
}.

%% RPC Routing Message (for multi-hop DHT routing)

-type rpc_route_msg() :: #{
    destination_node_id := binary(),  % 32-byte destination node ID
    source_node_id := binary(),       % 32-byte source node ID
    hop_count := non_neg_integer(),   % Current hop count (for debugging/metrics)
    max_hops := pos_integer(),        % Max hops allowed (TTL protection, default: 10)
    payload_type := call | reply,     % Type of wrapped message
    payload := call_msg() | reply_msg()  % The actual RPC message
}.

%% Pub/Sub Routing Message (for multi-hop DHT routing)

-type pubsub_route_msg() :: #{
    destination_node_id := binary(),  % 32-byte subscriber node ID
    source_node_id := binary(),       % 32-byte publisher node ID
    hop_count := non_neg_integer(),   % Current hop count (for debugging/metrics)
    max_hops := pos_integer(),        % Max hops allowed (TTL protection, default: 10)
    topic := binary(),                % Pub/sub topic
    payload := publish_msg()          % The actual PUBLISH message
}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Get numeric ID for a message type.
-spec message_type_id(message_type()) -> byte().
message_type_id(connect) -> ?MSG_CONNECT;
message_type_id(disconnect) -> ?MSG_DISCONNECT;
message_type_id(ping) -> ?MSG_PING;
message_type_id(pong) -> ?MSG_PONG;
message_type_id(publish) -> ?MSG_PUBLISH;
message_type_id(subscribe) -> ?MSG_SUBSCRIBE;
message_type_id(unsubscribe) -> ?MSG_UNSUBSCRIBE;
message_type_id(pubsub_route) -> ?MSG_PUBSUB_ROUTE;
message_type_id(call) -> ?MSG_CALL;
message_type_id(reply) -> ?MSG_REPLY;
message_type_id(cast) -> ?MSG_CAST;
message_type_id(rpc_route) -> ?MSG_RPC_ROUTE;
message_type_id(rpc_request) -> ?MSG_RPC_REQUEST;
message_type_id(rpc_reply) -> ?MSG_RPC_REPLY;
message_type_id(swim_ping) -> ?MSG_SWIM_PING;
message_type_id(swim_ack) -> ?MSG_SWIM_ACK;
message_type_id(swim_ping_req) -> ?MSG_SWIM_PING_REQ;
message_type_id(find_node) -> ?MSG_FIND_NODE;
message_type_id(find_node_reply) -> ?MSG_FIND_NODE_REPLY;
message_type_id(store) -> ?MSG_STORE;
message_type_id(find_value) -> ?MSG_FIND_VALUE;
message_type_id(find_value_reply) -> ?MSG_FIND_VALUE_REPLY;
message_type_id(nat_probe) -> ?MSG_NAT_PROBE;
message_type_id(nat_probe_reply) -> ?MSG_NAT_PROBE_REPLY;
message_type_id(punch_request) -> ?MSG_PUNCH_REQUEST;
message_type_id(punch_coordinate) -> ?MSG_PUNCH_COORDINATE;
message_type_id(punch_execute) -> ?MSG_PUNCH_EXECUTE;
message_type_id(punch_result) -> ?MSG_PUNCH_RESULT;
message_type_id(relay_request) -> ?MSG_RELAY_REQUEST;
message_type_id(relay_data) -> ?MSG_RELAY_DATA.

%% @doc Get message type name from numeric ID.
-spec message_type_name(byte()) -> {ok, message_type()} | {error, unknown_type}.
message_type_name(?MSG_CONNECT) -> {ok, connect};
message_type_name(?MSG_DISCONNECT) -> {ok, disconnect};
message_type_name(?MSG_PING) -> {ok, ping};
message_type_name(?MSG_PONG) -> {ok, pong};
message_type_name(?MSG_PUBLISH) -> {ok, publish};
message_type_name(?MSG_SUBSCRIBE) -> {ok, subscribe};
message_type_name(?MSG_UNSUBSCRIBE) -> {ok, unsubscribe};
message_type_name(?MSG_PUBSUB_ROUTE) -> {ok, pubsub_route};
message_type_name(?MSG_CALL) -> {ok, call};
message_type_name(?MSG_REPLY) -> {ok, reply};
message_type_name(?MSG_CAST) -> {ok, cast};
message_type_name(?MSG_RPC_ROUTE) -> {ok, rpc_route};
message_type_name(?MSG_RPC_REQUEST) -> {ok, rpc_request};
message_type_name(?MSG_RPC_REPLY) -> {ok, rpc_reply};
message_type_name(?MSG_SWIM_PING) -> {ok, swim_ping};
message_type_name(?MSG_SWIM_ACK) -> {ok, swim_ack};
message_type_name(?MSG_SWIM_PING_REQ) -> {ok, swim_ping_req};
message_type_name(?MSG_FIND_NODE) -> {ok, find_node};
message_type_name(?MSG_FIND_NODE_REPLY) -> {ok, find_node_reply};
message_type_name(?MSG_STORE) -> {ok, store};
message_type_name(?MSG_FIND_VALUE) -> {ok, find_value};
message_type_name(?MSG_FIND_VALUE_REPLY) -> {ok, find_value_reply};
message_type_name(?MSG_NAT_PROBE) -> {ok, nat_probe};
message_type_name(?MSG_NAT_PROBE_REPLY) -> {ok, nat_probe_reply};
message_type_name(?MSG_PUNCH_REQUEST) -> {ok, punch_request};
message_type_name(?MSG_PUNCH_COORDINATE) -> {ok, punch_coordinate};
message_type_name(?MSG_PUNCH_EXECUTE) -> {ok, punch_execute};
message_type_name(?MSG_PUNCH_RESULT) -> {ok, punch_result};
message_type_name(?MSG_RELAY_REQUEST) -> {ok, relay_request};
message_type_name(?MSG_RELAY_DATA) -> {ok, relay_data};
message_type_name(_) -> {error, unknown_type}.
