%%%-------------------------------------------------------------------
%%% @doc Macula Gateway mDNS Discovery.
%%%
%%% Discovers LAN peers advertising the _macula-mesh._udp service via
%%% mDNS and populates the direct routing table with their endpoints.
%%%
%%% This enables LAN pubsub delivery WITHOUT going through the bootstrap
%%% DHT. When the pubsub router checks the direct routing table, it finds
%%% LAN peers there and connects directly.
%%%
%%% The module subscribes to mDNS discovery events via gproc and
%%% processes DNS SRV/TXT records to extract node_id and endpoint.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_mdns_discovery).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    local_node_id :: binary(),
    port :: inet:port_number()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{node_id := NodeId, port := Port, realm := Realm}) ->
    %% Register this gateway for mDNS advertisement
    macula_gateway_mdns_advertiser:register(NodeId, Port, Realm),

    %% Start mDNS advertiser child (if mdns application available)
    start_mdns_advertiser(),

    %% Subscribe to mDNS discovery events for our service type
    subscribe_to_mdns_events(),

    ?LOG_INFO("[mDNS-discovery] Started — advertising on _macula-mesh._udp, port ~p", [Port]),
    {ok, #state{local_node_id = NodeId, port = Port}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle mDNS discovery events.
%% The mdns library sends messages via gproc when services are discovered.
handle_info({mdns, _Type, {_Name, SrvRecord, TxtRecords}}, State) ->
    handle_mdns_record(SrvRecord, TxtRecords, State),
    {noreply, State};

handle_info({mdns, _Type, _Other}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    macula_gateway_mdns_advertiser:unregister(),
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

%% @private Start the mDNS advertiser if the mdns application is available.
start_mdns_advertiser() ->
    case whereis(mdns_advertise_sup) of
        undefined ->
            ?LOG_DEBUG("[mDNS-discovery] mdns application not running, skipping advertiser");
        _Pid ->
            case mdns_advertise_sup:start_child(macula_gateway_mdns_advertiser) of
                {ok, _} ->
                    ?LOG_INFO("[mDNS-discovery] mDNS advertiser started");
                {error, {already_started, _}} ->
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING("[mDNS-discovery] Failed to start mDNS advertiser: ~p", [Reason])
            end
    end.

%% @private Subscribe to mDNS events for _macula-mesh._udp service.
subscribe_to_mdns_events() ->
    try
        mdns:subscribe(discover),
        ?LOG_DEBUG("[mDNS-discovery] Subscribed to mDNS discover events")
    catch
        _:_ ->
            ?LOG_DEBUG("[mDNS-discovery] mdns not available, LAN discovery disabled")
    end.

%% @private Process a discovered mDNS SRV record.
handle_mdns_record(SrvRecord, TxtRecords, #state{local_node_id = LocalNodeId}) ->
    case extract_peer_info(SrvRecord, TxtRecords) of
        {ok, #{node_id := PeerNodeId, endpoint := Endpoint}} when PeerNodeId =/= LocalNodeId ->
            ?LOG_INFO("[mDNS-discovery] Discovered LAN peer: ~s at ~s",
                      [binary:encode_hex(PeerNodeId), Endpoint]),
            %% Store in direct routing table — pubsub router will find it there
            macula_direct_routing:store(PeerNodeId, Endpoint),
            %% Also add to DHT routing table for completeness
            add_to_routing_table(PeerNodeId, Endpoint);
        {ok, _} ->
            %% Our own advertisement, ignore
            ok;
        {error, _Reason} ->
            ok
    end.

%% @private Extract peer info from mDNS records.
extract_peer_info(SrvRecord, TxtRecords) ->
    case SrvRecord of
        {_Priority, _Weight, Port, Hostname} ->
            extract_from_txt(Hostname, Port, TxtRecords);
        _ ->
            {error, invalid_srv}
    end.

%% @private Extract node_id from TXT records and build endpoint.
extract_from_txt(Hostname, Port, TxtRecords) ->
    NodeIdHex = find_txt_value("node_id", TxtRecords),
    build_peer_info(NodeIdHex, Hostname, Port).

build_peer_info(undefined, _Hostname, _Port) ->
    {error, no_node_id};
build_peer_info(NodeIdHex, Hostname, Port) ->
    HostBin = clean_hostname(Hostname),
    Endpoint = iolist_to_binary([
        <<"https://">>, HostBin, <<":">>, integer_to_binary(Port)
    ]),
    {ok, #{
        node_id => binary:decode_hex(list_to_binary(NodeIdHex)),
        endpoint => Endpoint
    }}.

%% @private Find a value in TXT record properties.
find_txt_value(_Key, []) ->
    undefined;
find_txt_value(Key, [Props | Rest]) when is_map(Props) ->
    case maps:get(Key, Props, undefined) of
        undefined -> find_txt_value(Key, Rest);
        Value -> Value
    end;
find_txt_value(Key, [_ | Rest]) ->
    find_txt_value(Key, Rest).

%% @private Remove trailing dot from mDNS hostname.
clean_hostname(Hostname) when is_list(Hostname) ->
    case lists:last(Hostname) of
        $. -> list_to_binary(lists:droplast(Hostname));
        _ -> list_to_binary(Hostname)
    end;
clean_hostname(Hostname) when is_binary(Hostname) ->
    case binary:last(Hostname) of
        $. -> binary:part(Hostname, 0, byte_size(Hostname) - 1);
        _ -> Hostname
    end.

%% @private Add discovered peer to DHT routing table.
add_to_routing_table(PeerNodeId, Endpoint) ->
    case whereis(macula_routing_server) of
        undefined -> ok;
        Pid ->
            NodeInfo = #{
                node_id => PeerNodeId,
                endpoint => Endpoint
            },
            macula_routing_server:add_node(Pid, NodeInfo)
    end.
