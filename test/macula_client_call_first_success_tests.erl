%% EUnit tests for how a pool call moves from one link to the next: only a
%% CALL that never went out is tried on another link. The links are plain
%% terms and the call is a fun, so no module is replaced.
-module(macula_client_call_first_success_tests).

-include_lib("eunit/include/eunit.hrl").

first_success_test_() ->
    [fun call_falls_through_to_next_connected_link/0,
     fun a_pool_call_that_timed_out_after_its_write_started_is_not_tried_on_another_link/0,
     fun a_call_the_wire_cannot_carry_is_refused_once_not_once_per_link/0,
     fun a_call_refused_for_its_procedure_name_is_refused_once_not_once_per_link/0].

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

%% A REQUEST-LEVEL REFUSAL IS THE SAME AT EVERY LINK, SO ASK ONCE.
%%
%% A payload the wire cannot carry is refused by `macula_frame:check_payload/1'
%% against `?MAX_FRAME_BYTES', a compile-time constant. Every link in the pool
%% refuses it identically, so walking the rest is guaranteed waste: it burns the
%% caller's deadline against candidates that will all say the same thing, and
%% hands back the same error it already had.
%%
%% The second assertion is the one that catches the waste. The first alone is
%% satisfied by a pool that tried every link and then returned the last error.
a_call_the_wire_cannot_carry_is_refused_once_not_once_per_link() ->
    TooBig = {error, {refused, {unsupported_payload_type, payload_too_large, []}}},
    {Result, Called} = first_success([a, b, c],
                                     #{a => TooBig,
                                       b => {ok, <<"never reached">>},
                                       c => {ok, <<"never reached">>}}),
    ?assertEqual(TooBig, Result),
    ?assertEqual([a], Called).

%% The same for a procedure name the frame's text bound refuses, against
%% `?MAX_PROCEDURE_BYTES', also a compile-time constant. Two reasons rather than
%% one, because the scope has to hold for every reason that reaches it and not
%% merely for the reason that is easiest to name.
a_call_refused_for_its_procedure_name_is_refused_once_not_once_per_link() ->
    TooLong = {error, {refused, {text_too_long, procedure}}},
    {Result, Called} = first_success([a, b],
                                     #{a => TooLong, b => {ok, <<"never reached">>}}),
    ?assertEqual(TooLong, Result),
    ?assertEqual([a], Called).

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
