%%%-------------------------------------------------------------------
%%% @doc A one_for_one supervisor that starts the child specs it is given.
%%%
%%% For tests that need to assert what happens when a supervisor actually
%%% RUNS a child spec, rather than what the spec looks like. A spec whose
%%% start MFA returns `{error, Reason}' makes `supervisor:start_link/2'
%%% return `{error, {shutdown, {failed_to_start_child, Id, Reason}}}',
%%% which is the observable a caller of `macula:child_spec/3' gets.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_test_one_for_one).

-behaviour(supervisor).

-export([init/1]).

init(ChildSpecs) ->
    {ok, {#{strategy => one_for_one, intensity => 0, period => 1}, ChildSpecs}}.
