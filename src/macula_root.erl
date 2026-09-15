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
        #{
            id => macula_publication_seq,
            start => {macula_publication_seq, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% The counts table behind macula_diagnostics:bounded_event/3, which
        %% callers update themselves. Before any connection logs through it.
        #{
            id => macula_diagnostics_bound,
            start => {macula_diagnostics_bound, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% The last record version this node issued, which callers of
        %% macula_record_uuid:v7_monotonic/1 advance themselves. Before any
        %% record is signed.
        #{
            id => macula_record_uuid,
            start => {macula_record_uuid, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% MRI Type Registry (type validation, custom type registration)
        #{
            id => macula_mri_registry,
            start => {macula_mri_registry, start_link, [[]]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% MRI ETS Storage (in-memory store/graph adapter)
        #{
            id => macula_mri_ets,
            start => {macula_mri_ets, start_link, [[]]},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Correlation-id registry for macula_content_transfer handles
        %% (PLAN_PUSH_UPLOAD.md Phase 1) — a caller that only knows a
        %% transfer's share_id (from a published sharing.*_started_v1
        %% mesh fact) resolves it to a cancellable pid here.
        #{
            id => macula_content_transfer_registry,
            start => {macula_content_transfer_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Local registry + dispatcher for streaming RPC (v1.5.0+).
        %% In-process pairing of client/server stream halves; the QUIC-
        %% backed cross-node path lands in Phase 2 of
        %% PLAN_MACULA_STREAMING.md.
        #{
            id => macula_stream_local,
            start => {macula_stream_local, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Holds the table of served stream sessions, so the sessions kept
        %% there outlive a restart of the counter below. Starts first.
        #{
            id => macula_stream_sessions_keeper,
            start => {macula_stream_sessions_keeper, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% The node's count of served stream sessions: each station link
        %% asks it before serving a session, against the caps per verified
        %% caller and node-wide, and a session's place frees when its stream
        %% ends.
        #{
            id => macula_stream_sessions,
            start => {macula_stream_sessions, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },

        %% Distribution-over-mesh bridge supervisor.
        %% Started here (under the application supervisor) so it survives
        %% shell crashes and other transient process deaths in user code.
        #{
            id => macula_dist_bridge_sup,
            start => {macula_dist_bridge_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [macula_dist_bridge_sup]
        },

        %% The statement issuers of this node's pools, one for each pool,
        %% which every connection a pool's links make draws its CONNECT
        %% material and status statements from. Before peering.
        #{
            id => macula_statement_issuer_sup,
            start => {macula_statement_issuer_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [macula_statement_issuer_sup]
        },

        %% Peering — per-peer connection state machines (CONNECT/HELLO
        %% handshake + frame send/receive). One macula_peering_conn
        %% gen_statem per peer, supervised by macula_peering_conn_sup
        %% under the macula_peering_sup top supervisor.
        #{
            id => macula_peering_sup,
            start => {macula_peering_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [macula_peering_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% @private
get_app_version() ->
    case application:get_key(macula, vsn) of
        {ok, Vsn} -> "v" ++ Vsn;
        undefined -> "v0.0.0"
    end.
