%%%-------------------------------------------------------------------
%% @doc Simple ra_machine for leader election.
%%
%% This is a minimal state machine used by ra (Raft consensus) to manage
%% leader election. The actual leader election logic is handled by Raft
%% itself - this module just provides the required ra_machine interface.
%%
%% State: Just a counter (not actually used for anything meaningful)
%% Commands: noop (leader election doesn't need commands)
%%
%% @end
%%%-------------------------------------------------------------------

-module(macula_leader_machine).

-behaviour(ra_machine).

%% ra_machine callbacks
-export([init/1, apply/3]).

%%====================================================================
%% ra_machine callbacks
%%====================================================================

%% @doc Initialize the state machine state.
%% Returns empty state (we don't need state for leader election).
init(_Config) ->
    #{}.

%% @doc Apply a command to the state machine.
%% For leader election, we don't actually need to process commands.
%% This is just to satisfy the ra_machine behavior.
apply(_Meta, _Command, State) ->
    {State, ok, []}.
