%%%-------------------------------------------------------------------
%%% @doc Top-level supervisor for the QUIC distribution test app.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_test_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    Children = [
        #{
            id => macula_dist_test_health,
            start => {macula_dist_test_health, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => macula_dist_test_connector,
            start => {macula_dist_test_connector, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => macula_dist_test_babel,
            start => {macula_dist_test_babel, start_link, []},
            restart => permanent,
            type => worker
        }
    ],

    {ok, {SupFlags, Children}}.
