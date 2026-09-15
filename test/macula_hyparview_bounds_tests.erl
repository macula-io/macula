%% EUnit tests for the HyParView frame table rules (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md, DHT and HyParView fields;
%% DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md, 3.1): a peer_sample of at most 7 node_ids, a SHUFFLE or FORWARD_JOIN ttl and a
%% FORWARD_JOIN arwl of at most 8, and a prwl of at most its arwl. A frame outside these is refused as it is decoded, a
%% prwl above its arwl by name, and the constructors refuse to build one. A placement past a neighbour's allowance and an unsolicited SHUFFLE_REPLY
%% are charged refusals.
-module(macula_hyparview_bounds_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(MEMBER, <<1:256>>).

a_peer_sample_of_7_decodes_and_of_8_is_refused_test_() ->
    [?_assertMatch({ok, _, <<>>}, decoded(shuffle_with(#{peer_sample => ids(7)}))),
     ?_assertEqual({error, bad_frame}, decoded(shuffle_with(#{peer_sample => ids(8)}))),
     ?_assertMatch({ok, _, <<>>}, decoded(reply_with(#{peer_sample => ids(7)}))),
     ?_assertEqual({error, bad_frame}, decoded(reply_with(#{peer_sample => ids(8)})))].

a_walk_length_of_8_decodes_and_of_9_is_refused_test_() ->
    [?_assertMatch({ok, _, <<>>}, decoded(shuffle_with(#{ttl => 8}))),
     ?_assertEqual({error, bad_frame}, decoded(shuffle_with(#{ttl => 9}))),
     ?_assertMatch({ok, _, <<>>}, decoded(forward_join_with(#{ttl => 8, arwl => 8, prwl => 8}))),
     ?_assertEqual({error, bad_frame}, decoded(forward_join_with(#{ttl => 9}))),
     ?_assertEqual({error, bad_frame}, decoded(forward_join_with(#{arwl => 9})))].

a_prwl_above_its_arwl_is_refused_test_() ->
    [?_assertMatch({ok, _, <<>>}, decoded(forward_join_with(#{arwl => 4, prwl => 4}))),
     ?_assertEqual({error, {invalid_frame, hyparview_forward_join, prwl}},
                   decoded(forward_join_with(#{arwl => 4, prwl => 5})))].

the_constructors_refuse_frames_outside_the_bounds_test_() ->
    [?_assertError(function_clause, macula_frame:hyparview_shuffle(shuffle_spec(#{ttl => 9}))),
     ?_assertError(function_clause, macula_frame:hyparview_shuffle(shuffle_spec(#{peer_sample => ids(8)}))),
     ?_assertError(function_clause, macula_frame:hyparview_shuffle_reply(#{realm => ?REALM, peer_sample => ids(8)})),
     ?_assertError(function_clause, macula_frame:hyparview_forward_join(forward_join_spec(#{ttl => 9}))),
     ?_assertError(function_clause, macula_frame:hyparview_forward_join(forward_join_spec(#{arwl => 9}))),
     ?_assertError(function_clause, macula_frame:hyparview_forward_join(forward_join_spec(#{arwl => 4, prwl => 5})))].

a_placement_past_the_allowance_and_an_unsolicited_reply_are_charged_test_() ->
    [?_assert(macula_frame:charged_refusal(placement_allowance)),
     ?_assert(macula_frame:charged_refusal(unsolicited_shuffle_reply))].

%%------------------------------------------------------------------
%% Helpers: frames built within the bounds and changed afterwards, so the decoder judges them, not the constructor.
%%------------------------------------------------------------------

shuffle_spec(Changes) ->
    maps:merge(#{realm => ?REALM, origin => ?MEMBER, ttl => 2, peer_sample => []}, Changes).

forward_join_spec(Changes) ->
    maps:merge(#{realm => ?REALM, new_member => ?MEMBER, ttl => 2, arwl => 6, prwl => 3}, Changes).

shuffle_with(Changes) ->
    maps:merge(macula_frame:hyparview_shuffle(shuffle_spec(#{})), Changes).

reply_with(Changes) ->
    maps:merge(macula_frame:hyparview_shuffle_reply(#{realm => ?REALM, peer_sample => []}), Changes).

forward_join_with(Changes) ->
    maps:merge(macula_frame:hyparview_forward_join(forward_join_spec(#{})), Changes).

decoded(Frame) ->
    macula_frame:decode(macula_frame:encode(Frame)).

ids(Count) ->
    [<<N:256>> || N <- lists:seq(100, 99 + Count)].
