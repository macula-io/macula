%%%-------------------------------------------------------------------
%% @doc Macula SDK Root Supervisor.
%%
%% Starts the SDK subsystems: MRI (resource identifiers), cert system
%% (identity), and distribution over mesh (optional).
%%
%% Server-side systems (gateway, DHT, RPC routing, PubSub routing,
%% peering, SWIM, etc.) live in macula-relay.
%%
%% @end
%%%-------------------------------------------------------------------
-module(macula_root).

-behaviour(supervisor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Version = get_app_version(),

    ?LOG_INFO(""),
    ?LOG_INFO("═══════════════════════════════════════════════════════════════"),
    ?LOG_INFO("  Macula SDK ~s", [Version]),
    ?LOG_INFO("═══════════════════════════════════════════════════════════════"),
    ?LOG_INFO(""),

    SupFlags = #{strategy => one_for_one, intensity => 10, period => 5},

    ChildSpecs = [
        %% The seq counter every publication a node signs draws from, one per
        %% key. First, so it runs before any pool or pubsub server publishes.
        worker_child(macula_publication_seq),

        %% The counts table behind macula_diagnostics:bounded_event/3, which
        %% callers update themselves. Before any connection logs through it.
        worker_child(macula_diagnostics_bound),

        %% The last record version this node issued, which callers of
        %% macula_record_uuid:v7_monotonic/1 advance themselves. Before any
        %% record is signed.
        worker_child(macula_record_uuid),

        %% The KEM keys this node's advertisements name and its sealed calls
        %% open with, one current key per node identity (E2E design,
        %% Amendment A1). Before any pool advertises or serves.
        worker_child(macula_kem_keyring),

        %% The last reading of each provider's delegation slot, shared by the
        %% streams that follow that provider (macula#120). Before any pool
        %% opens a stream.
        worker_child(macula_provider_standing),

        %% MRI Type Registry (type validation, custom type registration)
        worker_child(macula_mri_registry, [[]]),

        %% MRI ETS Storage (in-memory store/graph adapter)
        worker_child(macula_mri_ets, [[]]),

        %% The content sharers, one per pool (D27): the content a node
        %% shares, kept, served and announced by the node itself.
        supervisor_child(macula_content_sharer_sup),

        %% Local registry + dispatcher for streaming RPC (v1.5.0+).
        %% In-process pairing of client/server stream halves; the QUIC-
        %% backed cross-node path is the station link's.
        worker_child(macula_stream_local),

        %% Holds the table of served stream sessions, so the sessions kept
        %% there outlive a restart of the counter below. Starts first.
        worker_child(macula_stream_sessions_keeper),

        %% The node's count of served stream sessions: each station link
        %% asks it before serving a session, against the caps per verified
        %% caller and node-wide, and a session's place frees when its stream
        %% ends.
        worker_child(macula_stream_sessions),

        %% Distribution-over-mesh bridge supervisor.
        %% Started here (under the application supervisor) so it survives
        %% shell crashes and other transient process deaths in user code.
        supervisor_child(macula_dist_bridge_sup),

        %% The statement issuers of this node's pools, one for each pool,
        %% which every connection a pool's links make draws its CONNECT
        %% material and status statements from. Before peering.
        supervisor_child(macula_statement_issuer_sup),

        %% Peering — per-peer connection state machines (CONNECT/HELLO
        %% handshake + frame send/receive). One macula_peering_conn
        %% gen_statem per peer, supervised by macula_peering_conn_sup
        %% under the macula_peering_sup top supervisor.
        supervisor_child(macula_peering_sup)
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% A permanent worker started as Mod:start_link(Args...).
worker_child(Mod) ->
    worker_child(Mod, []).

worker_child(Mod, Args) ->
    #{id => Mod, start => {Mod, start_link, Args},
      restart => permanent, shutdown => 5000, type => worker}.

%% A permanent supervisor, given all the time it needs to stop its children.
supervisor_child(Mod) ->
    #{id => Mod, start => {Mod, start_link, []},
      restart => permanent, shutdown => infinity, type => supervisor,
      modules => [Mod]}.

%% @private
get_app_version() ->
    case application:get_key(macula, vsn) of
        {ok, Vsn} -> "v" ++ Vsn;
        undefined -> "v0.0.0"
    end.
