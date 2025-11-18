%%%-------------------------------------------------------------------
%% @doc Macula Peers Supervisor.
%%
%% This supervisor manages dynamic peer connections using a
%% simple_one_for_one strategy. Each peer connection gets its own
%% macula_peer_system supervisor with dedicated handlers.
%%
%% Architecture (v0.8.5):
%% - One macula_peers_sup instance per Macula node
%% - Dynamically adds/removes macula_peer_system children
%% - Each child = one peer connection
%% - simple_one_for_one strategy for efficient scaling
%%
%% Usage:
%% <pre>
%% %% Start new peer connection
%% {ok, PeerPid} = macula_peers_sup:start_peer(Url, Opts).
%%
%% %% List all active peers
%% Peers = macula_peers_sup:list_peers().
%%
%% %% Count active peers
%% Count = macula_peers_sup:count_peers().
%%
%% %% Stop peer connection
%% ok = macula_peers_sup:stop_peer(PeerPid).
%% </pre>
%%
%% @end
%%%-------------------------------------------------------------------

-module(macula_peers_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_peer/2,
    stop_peer/1,
    list_peers/0,
    count_peers/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the peers supervisor.
%%
%% Called by macula_root during application startup.
%% Registers the supervisor with the local name macula_peers_sup.
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @doc Start a new peer connection.
%%
%% Creates a new macula_peer_system supervisor for the peer at the given URL.
%% The peer system will manage the connection lifecycle and all peer-specific
%% handlers (pubsub, rpc, advertisements).
%%
%% @param Url Peer URL (e.g., "https://peer.example.com:4433")
%% @param Opts Connection options (realm, etc.)
%% @returns {ok, PeerPid} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec start_peer(Url :: binary(), Opts :: map()) -> {ok, pid()} | {error, term()}.

start_peer(Url, Opts) when is_binary(Url), is_map(Opts) ->
    supervisor:start_child(?SERVER, [Url, Opts]);
start_peer(Url, Opts) when is_list(Url), is_map(Opts) ->
    start_peer(list_to_binary(Url), Opts).

%%------------------------------------------------------------------------------
%% @doc Stop a peer connection.
%%
%% Gracefully terminates the macula_peer_system supervisor for the given peer.
%% This will clean up all peer handlers and close the connection.
%%
%% @param PeerPid PID of the peer system supervisor to stop
%% @returns ok | {error, not_found}
%% @end
%%------------------------------------------------------------------------------
-spec stop_peer(PeerPid :: pid()) -> ok | {error, term()}.

stop_peer(PeerPid) when is_pid(PeerPid) ->
    case supervisor:terminate_child(?SERVER, PeerPid) of
        ok -> ok;
        {error, not_found} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc List all active peer connections.
%%
%% Returns a list of PIDs for all currently running macula_peer_system
%% supervisors.
%%
%% @returns [pid()]
%% @end
%%------------------------------------------------------------------------------
-spec list_peers() -> [pid()].

list_peers() ->
    Children = supervisor:which_children(?SERVER),
    [Pid || {_Id, Pid, _Type, _Modules} <- Children, is_pid(Pid)].

%%------------------------------------------------------------------------------
%% @doc Count active peer connections.
%%
%% Returns the number of currently running macula_peer_system supervisors.
%%
%% @returns non_neg_integer()
%% @end
%%------------------------------------------------------------------------------
-spec count_peers() -> non_neg_integer().

count_peers() ->
    length(list_peers()).

%%%=============================================================================
%%% Supervisor Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initialize the peers supervisor.
%%
%% Sets up a simple_one_for_one supervisor for macula_peer_system children.
%% Each child will be started with dynamic arguments (Url, Opts).
%%
%% Strategy: simple_one_for_one
%% - Efficient for managing many similar children
%% - Children started/stopped dynamically
%% - All children use same child spec template
%%
%% Restart Strategy: temporary
%% - Peer connections don't auto-restart on crash
%% - Application decides when to reconnect
%% - Prevents reconnection storms
%%
%% @end
%%------------------------------------------------------------------------------
init([]) ->
    io:format("Starting Peers Supervisor (simple_one_for_one)~n"),

    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,  % Max 10 restarts
        period => 60      % In 60 seconds
    },

    %% Child spec template for macula_peer_system
    %% Actual Url and Opts will be passed to start_peer/2
    ChildSpec = #{
        id => macula_peer_system,  % Template ID (not used for simple_one_for_one)
        start => {macula_peer_system, start_link, []},  % Args appended by start_child
        restart => temporary,  % Don't auto-restart failed peers
        shutdown => 5000,      % 5 second shutdown timeout
        type => supervisor,    % macula_peer_system is a supervisor
        modules => [macula_peer_system]
    },

    {ok, {SupFlags, [ChildSpec]}}.
