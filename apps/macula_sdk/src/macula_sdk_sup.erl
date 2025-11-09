%%%-------------------------------------------------------------------
%%% @doc
%%% Macula SDK top-level supervisor.
%%%
%%% Supervises client connection pools and subscription managers.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_sdk_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the supervisor.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        %% Future: Add child workers here
        %% - Connection pool supervisor
        %% - Subscription manager
        %% - Metrics collector
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
