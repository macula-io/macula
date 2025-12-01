%%%-------------------------------------------------------------------
%%% @doc Macula Distribution mDNS Advertiser.
%%%
%%% This module implements the mdns advertiser behaviour from the
%%% shortishly/mdns library. It advertises Macula distribution nodes
%%% via mDNS for automatic discovery on local networks.
%%%
%%% Usage:
%%%   %% Start advertising
%%%   mdns_advertise_sup:start_child(macula_dist_mdns_advertiser).
%%%
%%%   %% Stop advertising
%%%   mdns_advertise:stop(macula_dist_mdns_advertiser).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_mdns_advertiser).

-export([service/0, instances/0]).

%% Registration API (called by macula_dist_discovery)
-export([register/2, unregister/0]).

%% Persistent term key for storing instance info
-define(INSTANCE_KEY, {?MODULE, instance}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Return the mDNS service type.
%% This follows the standard format: _service._protocol.local.
-spec service() -> string().
service() ->
    "_macula-dist._udp.".

%% @doc Return the list of service instances to advertise.
%% Each instance is a map with:
%%   - instance: Full instance name
%%   - priority: SRV record priority (lower = preferred)
%%   - weight: SRV record weight for load balancing
%%   - port: The port the service is running on
%%   - hostname: The hostname without domain
%%   - properties: TXT record key-value pairs
-spec instances() -> [map()].
instances() ->
    case persistent_term:get(?INSTANCE_KEY, undefined) of
        undefined ->
            [];
        Instance ->
            [Instance]
    end.

%%%===================================================================
%%% Registration API (called by macula_dist_discovery)
%%%===================================================================

%% @doc Register this node for mDNS advertisement.
-spec register(atom() | string(), pos_integer()) -> ok.
register(NodeName, Port) ->
    Instance = make_instance(NodeName, Port),
    persistent_term:put(?INSTANCE_KEY, Instance),
    ok.

%% @doc Unregister this node from mDNS advertisement.
-spec unregister() -> ok.
unregister() ->
    persistent_term:erase(?INSTANCE_KEY),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Create instance map for mDNS advertisement.
make_instance(NodeName, Port) ->
    NodeStr = node_to_string(NodeName),
    Hostname = get_hostname(),
    #{
        instance => NodeStr ++ "." ++ service() ++ "local.",
        priority => 0,
        weight => 0,
        port => Port,
        hostname => Hostname ++ ".",
        properties => #{
            "node" => NodeStr,
            "version" => get_version()
        }
    }.

%% @private Convert node name to string.
node_to_string(NodeName) when is_atom(NodeName) ->
    atom_to_list(NodeName);
node_to_string(NodeName) when is_list(NodeName) ->
    NodeName;
node_to_string(NodeName) when is_binary(NodeName) ->
    binary_to_list(NodeName).

%% @private Get the hostname without domain.
get_hostname() ->
    case net_adm:localhost() of
        Hostname when is_list(Hostname) ->
            %% Remove domain suffix if present
            case string:split(Hostname, ".") of
                [Short | _] -> Short;
                _ -> Hostname
            end;
        _ ->
            "localhost"
    end.

%% @private Get macula version.
get_version() ->
    case application:get_key(macula, vsn) of
        {ok, Vsn} -> Vsn;
        undefined -> "unknown"
    end.
