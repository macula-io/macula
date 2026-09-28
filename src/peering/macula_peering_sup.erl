%% @doc Top supervisor for macula_peering.
%%
%% Hosts the dynamic conn supervisor under which one
%% `macula_peering_conn' gen_statem is spawned per peer connection, and,
%% started before it, the owner of the table that says which handshake
%% version each peer is dialled with (`macula_peer_versions').
-module(macula_peering_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    Children = [
        #{
            id       => macula_peer_versions,
            start    => {macula_peer_versions, start_link, []},
            restart  => permanent,
            shutdown => 5_000,
            type     => worker,
            modules  => [macula_peer_versions]
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
