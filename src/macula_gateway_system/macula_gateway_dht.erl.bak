%%%-------------------------------------------------------------------
%%% @doc
%%% DHT Query Handler Module - handles DHT message forwarding to routing server.
%%%
%%% Responsibilities:
%%% - Forward DHT STORE messages to routing server
%%% - Forward DHT FIND_VALUE messages to routing server, send encoded replies
%%% - Forward DHT FIND_NODE messages to routing server, send encoded replies
%%% - Handle DHT queries from process messages
%%% - Encode replies using protocol encoder
%%% - Handle errors gracefully
%%%
%%% Pattern: Stateless delegation module
%%% - No GenServer (no state to manage)
%%% - Pure functions forwarding to routing server
%%% - Consistent error handling ({ok, Result} | {error, Reason})
%%%
%%% Extracted from macula_gateway.erl (Phase 10)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_dht).

%% API
-export([
    handle_store/2,
    handle_find_value/2,
    handle_find_node/2,
    handle_query/3
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Handle DHT STORE message.
%% Forwards to routing server (no reply sent back).
%% Crashes on routing server failures - this exposes DHT issues immediately.
-spec handle_store(pid(), map()) -> ok.
handle_store(_Stream, StoreMsg) ->
    io:format("[DHT] Processing STORE message: ~p~n", [StoreMsg]),
    %% Forward to routing server (let it crash on errors)
    _Reply = macula_routing_server:handle_message(macula_routing_server, StoreMsg),
    io:format("[DHT] STORE processed successfully~n"),
    %% STORE doesn't require a response to be sent back
    ok.

%% @doc Handle DHT FIND_VALUE message.
%% Forwards to routing server and sends encoded reply over stream.
%% Crashes on routing server or encoding failures - exposes DHT/protocol bugs.
-spec handle_find_value(pid(), map()) -> ok.
handle_find_value(Stream, FindValueMsg) ->
    io:format("[DHT] Processing FIND_VALUE message: ~p~n", [FindValueMsg]),
    %% Forward to routing server (let it crash on errors)
    Reply = macula_routing_server:handle_message(macula_routing_server, FindValueMsg),
    io:format("[DHT] FIND_VALUE processed, reply: ~p~n", [Reply]),

    %% Send reply back over stream (let it crash on errors)
    ReplyBinary = macula_protocol_encoder:encode(find_value_reply, Reply),
    macula_quic:send(Stream, ReplyBinary),
    ok.

%% @doc Handle DHT FIND_NODE message.
%% Forwards to routing server and sends encoded reply over stream.
%% Crashes on routing server or encoding failures - exposes DHT/protocol bugs.
-spec handle_find_node(pid(), map()) -> ok.
handle_find_node(Stream, FindNodeMsg) ->
    io:format("[DHT] Processing FIND_NODE message: ~p~n", [FindNodeMsg]),
    %% Forward to routing server (let it crash on errors)
    Reply = macula_routing_server:handle_message(macula_routing_server, FindNodeMsg),
    io:format("[DHT] FIND_NODE processed, reply: ~p~n", [Reply]),

    %% Send reply back over stream (let it crash on errors)
    ReplyBinary = macula_protocol_encoder:encode(find_node_reply, Reply),
    macula_quic:send(Stream, ReplyBinary),
    ok.

%% @doc Handle DHT query from process message.
%% Decodes query, forwards to routing server, encodes reply, sends to requesting process.
%% Crashes on decode or routing failures - exposes protocol/DHT bugs.
-spec handle_query(pid(), atom(), binary()) -> ok.
handle_query(FromPid, _QueryType, QueryData) ->
    io:format("[DHT] Processing query from process ~p~n", [FromPid]),
    %% Decode the query message (let it crash on decode errors)
    {ok, {MessageType, Message}} = macula_protocol_decoder:decode(QueryData),

    %% Forward to DHT routing server (let it crash on errors)
    Reply = macula_routing_server:handle_message(macula_routing_server, Message),

    %% Encode reply based on message type (let it crash on encoding errors)
    ReplyData = encode_reply_by_type(MessageType, Reply),

    %% Send reply back to requesting process
    FromPid ! {dht_reply, ReplyData},
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Encode reply based on message type.
encode_reply_by_type(find_node, Reply) ->
    macula_protocol_encoder:encode(find_node_reply, Reply);
encode_reply_by_type(find_value, Reply) ->
    macula_protocol_encoder:encode(find_value_reply, Reply);
encode_reply_by_type(store, Reply) ->
    macula_protocol_encoder:encode(store, Reply);
encode_reply_by_type(_UnknownType, _Reply) ->
    macula_protocol_encoder:encode(reply, #{error => <<"Unknown DHT message type">>}).
