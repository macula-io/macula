%%%-------------------------------------------------------------------
%%% @doc Tests for macula_stream:stream_io/2, the check of the stream
%%% functions a supervised stream wrapper is given.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_io_tests).

-include_lib("eunit/include/eunit.hrl").

%% Every stream function, by the arity its key takes.
-define(ARITIES, #{call_stream => 5, recv => 2, send => 3, close_send => 1, close => 1,
                   close_stream => 1, abort => 3, set_reply => 2, set_error => 2,
                   await_reply => 1}).

without_given_functions_a_wrapper_runs_on_its_own_test() ->
    Defaults = functions([recv, abort]),
    ?assertEqual(Defaults, macula_stream:stream_io(Defaults, undefined)).

given_functions_with_every_key_the_wrapper_calls_are_used_test() ->
    Given = functions([recv, abort]),
    ?assertEqual(Given, macula_stream:stream_io(functions([abort, recv]), Given)).

a_given_set_may_carry_the_other_stream_functions_test() ->
    Given = functions(maps:keys(?ARITIES)),
    ?assertEqual(Given, macula_stream:stream_io(functions([recv, abort]), Given)).

a_given_set_without_a_function_the_wrapper_calls_is_refused_test() ->
    ?assertError(function_clause,
                 macula_stream:stream_io(functions([recv, abort]), functions([recv]))).

every_stream_function_of_another_arity_is_refused_test() ->
    Every = functions(maps:keys(?ARITIES)),
    [?assertError(function_clause,
                  macula_stream:stream_io(#{}, Every#{Key := function(Arity + 1)}))
     || {Key, Arity} <- maps:to_list(?ARITIES)].

a_key_that_is_not_a_stream_function_is_refused_test() ->
    Given = (functions([recv]))#{receive_chunk => function(2)},
    ?assertError(function_clause, macula_stream:stream_io(functions([recv]), Given)).

a_stream_function_that_is_not_a_fun_is_refused_test() ->
    ?assertError(function_clause,
                 macula_stream:stream_io(functions([recv]), #{recv => macula_stream})).

functions(Keys) ->
    maps:from_list([{Key, function(maps:get(Key, ?ARITIES))} || Key <- Keys]).

function(1) -> fun(_) -> ok end;
function(2) -> fun(_, _) -> ok end;
function(3) -> fun(_, _, _) -> ok end;
function(4) -> fun(_, _, _, _) -> ok end;
function(5) -> fun(_, _, _, _, _) -> ok end;
function(6) -> fun(_, _, _, _, _, _) -> ok end.
