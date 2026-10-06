%% @doc Top supervisor for macula_peering.
%%
%% Hosts the dynamic conn supervisor under which one
%% `macula_peering_conn' gen_statem is spawned per peer connection, and,
%% started before it, the owner of the station's session proof budget
%% (`macula_session_proof_rate'). This process owns the table that says
%% which handshake version each peer is dialled with
%% (`macula_peer_versions'): it lives as long as every connection that
%% reads it, so no child's exit forgets a node seen on v5 (macula#50).
-module(macula_peering_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Peering runs handshake v5 only on a TLS posture that holds it up: it
%% refuses to start otherwise, naming what departed (macula_tls_posture).
init([]) ->
    ok = macula_tls_posture:ensure(),
    ok = macula_peer_versions:new_table(),
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    Children = [
        #{
            id       => macula_session_proof_rate,
            start    => {macula_session_proof_rate, start_link, []},
            restart  => permanent,
            shutdown => 5_000,
            type     => worker,
            modules  => [macula_session_proof_rate]
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
