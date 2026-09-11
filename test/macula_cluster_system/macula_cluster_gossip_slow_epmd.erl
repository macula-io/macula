%%%-------------------------------------------------------------------
%%% @doc An epmd module for a test peer node, given with `-epmd_module'.
%%%
%%% The node registers without an epmd daemon and listens on any free port.
%%% Resolving the host `slow.invalid' takes SLOW_MS and then fails; every
%%% other host fails at once. A process that dials a node on `slow.invalid'
%%% is held for SLOW_MS.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cluster_gossip_slow_epmd).

-export([start_link/0,
         register_node/2,
         register_node/3,
         listen_port_please/2,
         address_please/3,
         port_please/2,
         port_please/3,
         names/1]).

-define(SLOW_HOST, "slow.invalid").
-define(SLOW_MS, 3_000).

start_link() ->
    ignore.

register_node(Name, Port) ->
    register_node(Name, Port, inet).

register_node(_Name, _Port, _Family) ->
    {ok, 1}.

listen_port_please(_Name, _Host) ->
    {ok, 0}.

address_please(_Name, ?SLOW_HOST, _Family) ->
    timer:sleep(?SLOW_MS),
    {error, nxdomain};
address_please(_Name, _Host, _Family) ->
    {error, nxdomain}.

port_please(_Name, _Ip) ->
    noport.

port_please(_Name, _Ip, _Timeout) ->
    noport.

names(_Host) ->
    {error, address}.
