%%%-------------------------------------------------------------------
%%% @doc Tests for macula_reason_name: the name a reason leaves this
%%% node as, the text a handler's error crosses as, and how much of a
%%% reason the local log gets.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_reason_name_tests).

-include_lib("eunit/include/eunit.hrl").

%% Data a reason carries that must stay on this node.
-define(MARKER, <<"marker-3f9c-stays-on-this-node">>).
%% The most characters the log prints of a reason and what came with it.
-define(LOGGED_CHARS, 4096).

a_reasons_text_is_the_name_at_its_head_test() ->
    Text = fun macula_reason_name:text/1,
    Stack = [{a_module, a_function, [?MARKER], [{line, 1}]}],
    LongName = list_to_atom(lists:duplicate(65, $a)),
    ?assertEqual(<<"killed">>, Text(killed)),
    ?assertEqual(<<"shutdown">>, Text({shutdown, ?MARKER})),
    ?assertEqual(<<"timeout">>, Text({timeout, {gen_server, call, [pool, ?MARKER]}})),
    ?assertEqual(<<"badmatch">>, Text({{badmatch, ?MARKER}, Stack})),
    ?assertEqual(<<"no_healthy_link">>, Text({error, no_healthy_link})),
    ?assertEqual(<<"crashed">>, Text(?MARKER)),
    ?assertEqual(<<"crashed">>, Text([?MARKER])),
    ?assertEqual(<<"crashed">>, Text(LongName)).

a_handlers_own_text_crosses_whole_when_it_fits_test() ->
    Reply = fun macula_reason_name:reply_text/1,
    Cafe = "no room at the caf" ++ [16#E9],
    ?assertEqual({ok, <<"hold_full">>}, Reply(<<"hold_full">>)),
    ?assertEqual({ok, <<"no room at the caf", 16#E9/utf8>>}, Reply(Cafe)),
    ?assertEqual({ok, <<>>}, Reply("")).

a_handlers_text_that_does_not_fit_is_cut_on_a_character_boundary_test() ->
    Reply = fun macula_reason_name:reply_text/1,
    Euros = binary:copy(<<16#20AC/utf8>>, 100),
    {ok, Cut} = Reply(Euros),
    ?assertEqual(<<(binary:copy(<<16#20AC/utf8>>, 84))/binary, "...">>, Cut),
    ?assertEqual({ok, Cut}, Reply(unicode:characters_to_list(Euros))),
    ?assertEqual({ok, <<(binary:copy(<<"a">>, 253))/binary, "...">>},
                 Reply(lists:duplicate(100000, $a))).

a_long_list_is_looked_at_only_as_far_as_its_text_can_cross_test() ->
    Reply = fun macula_reason_name:reply_text/1,
    Long = lists:duplicate(100000, $a),
    ?assertEqual({ok, <<(binary:copy(<<"a">>, 253))/binary, "...">>},
                 Reply(Long ++ [self()])),
    ?assertEqual(error, Reply([$a, $b | improper])).

a_handlers_text_that_is_not_valid_utf8_crosses_as_its_valid_start_test() ->
    ?assertEqual({ok, <<"refused...">>},
                 macula_reason_name:reply_text(<<"refused", 16#FF, "rest">>)).

a_handler_error_that_is_not_text_crosses_as_its_name_or_not_at_all_test() ->
    Reply = fun macula_reason_name:reply_text/1,
    ?assertEqual({ok, <<"refused">>}, Reply({refused, ?MARKER})),
    ?assertEqual({ok, <<"invalid_token">>}, Reply(invalid_token)),
    ?assertEqual(error, Reply([self(), self()])),
    ?assertEqual(error, Reply([?MARKER])),
    ?assertEqual(error, Reply(42)),
    ?assertEqual(error, Reply(#{why => ?MARKER})).

the_log_gets_a_large_reason_within_bounds_test() ->
    Logged = unicode:characters_to_binary(
               macula_reason_name:logged("~p ends for ~p",
                                         [#{id => 1},
                                          {badmatch, lists:duplicate(10000, ?MARKER)}])),
    ?assert(string:length(Logged) =< ?LOGGED_CHARS),
    ?assertNotEqual(nomatch, binary:match(Logged, <<"badmatch">>)).
