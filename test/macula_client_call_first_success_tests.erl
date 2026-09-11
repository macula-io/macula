%% EUnit tests for how a pool call moves from one link to the next: only a
%% CALL that never went out is tried on another link. The links are plain
%% terms and the call is a fun, so no module is replaced.
-module(macula_client_call_first_success_tests).

-include_lib("eunit/include/eunit.hrl").

first_success_test_() ->
    [fun call_falls_through_to_next_connected_link/0,
     fun a_pool_call_that_timed_out_after_its_write_started_is_not_tried_on_another_link/0].

%% A link whose CALL was never sent passes the call to the next link.
call_falls_through_to_next_connected_link() ->
    {Result, Called} = first_success([dead, live],
                                     #{dead => {error, not_connected},
                                       live => {ok, <<"from live">>}}),
    ?assertEqual({ok, <<"from live">>}, Result),
    ?assertEqual([dead, live], Called).

%% A CALL that may have reached its provider is never sent again, so a
%% provider never runs one call twice.
a_pool_call_that_timed_out_after_its_write_started_is_not_tried_on_another_link() ->
    {Result, Called} = first_success([silent, live],
                                     #{silent => {error, timeout},
                                       live => {ok, <<"from live">>}}),
    ?assertEqual({error, timeout}, Result),
    ?assertEqual([silent], Called).

%% The pool's first-success walk over `Links', every one connected and
%% answering from `Answers': its result, and the links it called, in order.
first_success(Links, Answers) ->
    Self = self(),
    Call = fun(Link) -> Self ! {called, Link}, maps:get(Link, Answers) end,
    Result = macula_client:first_success(Links, fun(_Link) -> true end, Call),
    {Result, called()}.

called() ->
    receive
        {called, Link} -> [Link | called()]
    after 0 -> []
    end.
