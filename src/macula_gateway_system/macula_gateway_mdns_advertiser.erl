%%%-------------------------------------------------------------------
%%% @doc Macula Gateway mDNS Advertiser.
%%%
%%% Advertises this node's QUIC gateway endpoint via mDNS so LAN peers
%%% can discover it directly without going through the bootstrap DHT.
%%%
%%% Service type: _macula-mesh._udp
%%% TXT records: node_id (hex), port, realm
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_mdns_advertiser).

-export([service/0, instances/0]).
-export([register/3, unregister/0]).

-define(INSTANCE_KEY, {?MODULE, instance}).

%%%===================================================================
%%% mdns advertiser callbacks
%%%===================================================================

-spec service() -> string().
service() ->
    "_macula-mesh._udp.".

-spec instances() -> [map()].
instances() ->
    case persistent_term:get(?INSTANCE_KEY, undefined) of
        undefined -> [];
        Instance -> [Instance]
    end.

%%%===================================================================
%%% Registration API
%%%===================================================================

%% @doc Register this gateway for mDNS advertisement.
-spec register(binary(), inet:port_number(), binary()) -> ok.
register(NodeId, Port, Realm) ->
    Hostname = get_hostname(),
    Instance = #{
        instance => binary_to_list(short_node_id(NodeId)) ++ "." ++ service() ++ "local.",
        priority => 0,
        weight => 0,
        port => Port,
        hostname => Hostname ++ ".",
        properties => #{
            "node_id" => binary_to_list(binary:encode_hex(NodeId)),
            "realm" => binary_to_list(Realm),
            "port" => integer_to_list(Port)
        }
    },
    persistent_term:put(?INSTANCE_KEY, Instance),
    ok.

%% @doc Unregister this gateway from mDNS.
-spec unregister() -> ok.
unregister() ->
    persistent_term:erase(?INSTANCE_KEY),
    ok.

%%%===================================================================
%%% Internal
%%%===================================================================

get_hostname() ->
    case net_adm:localhost() of
        Hostname when is_list(Hostname) ->
            case string:split(Hostname, ".") of
                [Short | _] -> Short;
                _ -> Hostname
            end;
        _ ->
            "localhost"
    end.

%% @private First 8 bytes of node ID as hex for mDNS instance name.
short_node_id(NodeId) when byte_size(NodeId) >= 8 ->
    binary:encode_hex(binary:part(NodeId, 0, 8));
short_node_id(NodeId) ->
    binary:encode_hex(NodeId).
