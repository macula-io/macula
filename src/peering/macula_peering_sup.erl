%% @doc Top supervisor for macula_peering.
%%
%% Hosts the in-flight reservations of received frame bytes, their keeper
%% first so they outlive a restart of the process that watches them, and then
%% the dynamic conn supervisor under which one `macula_peering_conn'
%% gen_statem is spawned per peer connection, so every connection can reserve
%% from its start.
-module(macula_peering_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    Children = [
        #{
            id       => macula_peering_inflight_keeper,
            start    => {macula_peering_inflight_keeper, start_link, []},
            restart  => permanent,
            shutdown => 5_000,
            type     => worker,
            modules  => [macula_peering_inflight_keeper]
        },
        #{
            id       => macula_peering_inflight,
            start    => {macula_peering_inflight, start_link, []},
            restart  => permanent,
            shutdown => 5_000,
            type     => worker,
            modules  => [macula_peering_inflight]
        },
        #{
            id       => macula_peering_conn_sup,
            start    => {macula_peering_conn_sup, start_link, []},
            restart  => permanent,
            shutdown => 5_000,
            type     => supervisor,
            modules  => [macula_peering_conn_sup]
        }
    ],
    {ok, {SupFlags, Children}}.
