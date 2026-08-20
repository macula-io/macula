%%%-------------------------------------------------------------------
%%% @doc Factory supervisor for `macula_feeder' children.
%%%
%%% Meant to be embedded in the caller's own supervision tree: the
%%% application decides when to start a new feed and when to cancel one
%%% via `supervisor:terminate_child/2' (or `macula_feeder:cancel/1' on
%%% the child pid directly). Children are `temporary' — a finished or
%%% crashed feed is never restarted.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_feeder_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

%% @private
init([]) ->
    ChildSpec = #{
        id => macula_feeder,
        start => {macula_feeder, start_link, []},
        restart => temporary,
        shutdown => 5_000,
        type => worker,
        modules => [macula_feeder]
    },
    {ok, {#{strategy => simple_one_for_one, intensity => 10, period => 10},
          [ChildSpec]}}.
