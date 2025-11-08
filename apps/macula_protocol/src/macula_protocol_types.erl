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
    unsubscribe_msg/0
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

%% RPC messages
-define(MSG_CALL,           16#20).
-define(MSG_REPLY,          16#21).
-define(MSG_CAST,           16#22).

%% SWIM membership messages
-define(MSG_SWIM_PING,      16#30).
-define(MSG_SWIM_ACK,       16#31).
-define(MSG_SWIM_PING_REQ,  16#32).

%% Kademlia routing messages
-define(MSG_FIND_NODE,      16#40).
-define(MSG_FIND_NODE_REPLY,16#41).
-define(MSG_STORE,          16#42).
-define(MSG_FIND_VALUE,     16#43).

%%%===================================================================
%%% Type Definitions
%%%===================================================================

-type message_type() ::
    connect | disconnect | ping | pong |
    publish | subscribe | unsubscribe |
    call | reply | cast |
    swim_ping | swim_ack | swim_ping_req |
    find_node | find_node_reply | store | find_value.

-type message() ::
    {connect, connect_msg()} |
    {disconnect, disconnect_msg()} |
    {ping, ping_msg()} |
    {pong, pong_msg()} |
    {publish, publish_msg()} |
    {subscribe, subscribe_msg()} |
    {unsubscribe, unsubscribe_msg()}.

%% Control Messages

-type connect_msg() :: #{
    version := binary(),           % Protocol version "1.0"
    node_id := binary(),           % 32-byte node ID
    realm_id := binary(),          % 32-byte realm ID
    capabilities := [atom()]       % List of supported features
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
message_type_id(call) -> ?MSG_CALL;
message_type_id(reply) -> ?MSG_REPLY;
message_type_id(cast) -> ?MSG_CAST;
message_type_id(swim_ping) -> ?MSG_SWIM_PING;
message_type_id(swim_ack) -> ?MSG_SWIM_ACK;
message_type_id(swim_ping_req) -> ?MSG_SWIM_PING_REQ;
message_type_id(find_node) -> ?MSG_FIND_NODE;
message_type_id(find_node_reply) -> ?MSG_FIND_NODE_REPLY;
message_type_id(store) -> ?MSG_STORE;
message_type_id(find_value) -> ?MSG_FIND_VALUE.

%% @doc Get message type name from numeric ID.
-spec message_type_name(byte()) -> {ok, message_type()} | {error, unknown_type}.
message_type_name(?MSG_CONNECT) -> {ok, connect};
message_type_name(?MSG_DISCONNECT) -> {ok, disconnect};
message_type_name(?MSG_PING) -> {ok, ping};
message_type_name(?MSG_PONG) -> {ok, pong};
message_type_name(?MSG_PUBLISH) -> {ok, publish};
message_type_name(?MSG_SUBSCRIBE) -> {ok, subscribe};
message_type_name(?MSG_UNSUBSCRIBE) -> {ok, unsubscribe};
message_type_name(?MSG_CALL) -> {ok, call};
message_type_name(?MSG_REPLY) -> {ok, reply};
message_type_name(?MSG_CAST) -> {ok, cast};
message_type_name(?MSG_SWIM_PING) -> {ok, swim_ping};
message_type_name(?MSG_SWIM_ACK) -> {ok, swim_ack};
message_type_name(?MSG_SWIM_PING_REQ) -> {ok, swim_ping_req};
message_type_name(?MSG_FIND_NODE) -> {ok, find_node};
message_type_name(?MSG_FIND_NODE_REPLY) -> {ok, find_node_reply};
message_type_name(?MSG_STORE) -> {ok, store};
message_type_name(?MSG_FIND_VALUE) -> {ok, find_value};
message_type_name(_) -> {error, unknown_type}.
