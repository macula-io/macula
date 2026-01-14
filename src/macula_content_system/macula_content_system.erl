%%%-------------------------------------------------------------------
%%% @doc
%%% Supervisor for Macula content-addressed storage system.
%%%
%%% Manages the lifecycle of content system child processes:
%%% - macula_content_store: Local block and manifest storage
%%% - macula_content_transfer: Want/have/block exchange protocol
%%%
%%% == Supervision Strategy ==
%%% Uses one_for_one strategy - if a child dies, only that child is
%%% restarted. This is appropriate because:
%%% - Store and Transfer can operate independently
%%% - Store failure shouldn't affect in-flight transfers
%%% - Transfer failure shouldn't corrupt storage
%%%
%%% == Integration ==
%%% Add to macula_root.erl supervision tree:
%%% ```
%%% #{id => macula_content_system,
%%%   start => {macula_content_system, start_link, [#{}]},
%%%   type => supervisor}
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_system).
-behaviour(supervisor).

%% API
-export([
    start_link/1,
    child_specs/1,
    strategy/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the content system supervisor.
%% Options:
%% - store_path: Base directory for block storage
%% - node_id: This node's identifier for transfer protocol
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%% @doc Get child specifications for external inspection/testing.
-spec child_specs(map()) -> [supervisor:child_spec()].
child_specs(Opts) ->
    StoreOpts = maps:with([store_path], Opts),
    TransferOpts = maps:with([node_id], Opts),

    [
        #{
            id => macula_content_store,
            start => {macula_content_store, start_link, [StoreOpts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_content_store]
        },
        #{
            id => macula_content_transfer,
            start => {macula_content_transfer, start_link, [TransferOpts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [macula_content_transfer]
        }
    ].

%% @doc Get the supervision strategy.
-spec strategy() -> {ok, supervisor:strategy()}.
strategy() ->
    {ok, one_for_one}.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

%% @private
init(Opts) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = child_specs(Opts),

    {ok, {SupFlags, ChildSpecs}}.
