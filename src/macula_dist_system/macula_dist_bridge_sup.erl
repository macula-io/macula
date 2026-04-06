%%%-------------------------------------------------------------------
%%% @doc Supervisor for distribution relay bridge processes.
%%%
%%% Each relay tunnel gets one `macula_dist_bridge' child.
%%% Uses simple_one_for_one — children are started dynamically
%%% when tunnels are created.
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
