%%%-------------------------------------------------------------------
%%% @doc Supervisor for distribution relay bridge processes.
%%%
%%% Each relay tunnel gets one `macula_dist_bridge' child (gen_server).
%%% Uses simple_one_for_one — children are started dynamically via
%%% `start_bridge/1' when tunnels are negotiated.
%%%
%%% Children are `temporary' — tunnels are ephemeral and should not
%%% be restarted automatically. If a bridge dies, OTP's dist_util
%%% detects the socket close and marks the node DOWN. The user's
%%% application is responsible for re-pinging if desired.
%%%
%%% Part of the `macula_dist_system' supervision tree. Also started
%%% on demand by `macula_dist_relay:ensure_bridge_sup/0' when the
%%% full dist_system is not running (standalone relay mode).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_bridge_sup).

-behaviour(supervisor).

-export([start_link/0, start_bridge/1]).
-export([init/1]).

%% @doc Start the bridge supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a new bridge child for a tunnel.
-spec start_bridge(map()) -> {ok, pid()} | {error, term()}.
start_bridge(Args) ->
    supervisor:start_child(?MODULE, [Args]).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 30
    },
    ChildSpec = #{
        id => macula_dist_bridge,
        start => {macula_dist_bridge, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [macula_dist_bridge]
    },
    {ok, {SupFlags, [ChildSpec]}}.
