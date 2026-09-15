%% EUnit tests for the HyParView placement allowance and SHUFFLE records (DESIGN_PQ_DHT_SLOTS_AND_BUDGET.md, 3.1). A
%% neighbour places at most 20 node_ids per minute in the passive view: a token bucket of 20 with one token back every
%% 3 seconds, counting only node_ids new to the view. A FORWARD_JOIN places its new member only when its ttl equals the
%% receiver's own PRWL. A SHUFFLE_REPLY is merged only while a SHUFFLE this node sent in the last 30 seconds has no
%% reply yet. A frame past the allowance and an unsolicited SHUFFLE_REPLY return a refused action.
-module(macula_hyparview_allowance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SELF, <<1:256>>).
-define(REALM, <<9:256>>).

%%------------------------------------------------------------------
%% FORWARD_JOIN and the receiver's own PRWL
%%------------------------------------------------------------------

%% The frame names prwl 5, and this node's PRWL is 3.
a_forward_join_places_its_member_only_when_ttl_is_the_receivers_own_prwl_test() ->
    Sender = id(2),
    View0 = actives([Sender, id(3)]),
    {AtOwn, _} = macula_hyparview_proto:process(View0, Sender, forward_join(id(40), 3, 6, 5), ctx(0)),
    {AtFrames, _} = macula_hyparview_proto:process(View0, Sender, forward_join(id(41), 5, 6, 5), ctx(0)),
    ?assert(macula_hyparview_view:is_passive(id(40), AtOwn)),
    ?assertNot(macula_hyparview_view:is_passive(id(41), AtFrames)).

%%------------------------------------------------------------------
%% The placement allowance
%%------------------------------------------------------------------

a_neighbour_places_at_most_20_node_ids_and_each_frame_past_that_is_refused_test() ->
    Sender = id(2),
    {View1, Refused1} = shuffled(new_view(), Sender, ids(100, 7), ctx(0)),
    {View2, Refused2} = shuffled(View1, Sender, ids(110, 7), ctx(0)),
    {View3, Refused3} = shuffled(View2, Sender, ids(120, 7), ctx(0)),
    {View4, Refused4} = shuffled(View3, Sender, ids(130, 7), ctx(0)),
    Charged = [{refused, Sender, placement_allowance}],
    ?assertEqual({[], [], Charged, Charged}, {Refused1, Refused2, Refused3, Refused4}),
    ?assertEqual({7, 14, 20, 20}, {passive(View1), passive(View2), passive(View3), passive(View4)}).

the_allowance_gives_one_node_id_back_every_3_seconds_test() ->
    Sender = id(2),
    {View1, _} = shuffled(new_view(), Sender, ids(100, 7), ctx(0)),
    {View2, _} = shuffled(View1, Sender, ids(110, 7), ctx(0)),
    {View3, _} = shuffled(View2, Sender, ids(120, 7), ctx(0)),
    {View4, Refused4} = shuffled(View3, Sender, [id(200)], ctx(2_999)),
    {View5, Refused5} = shuffled(View4, Sender, [id(201)], ctx(3_000)),
    ?assertEqual({[{refused, Sender, placement_allowance}], []}, {Refused4, Refused5}),
    ?assertNot(macula_hyparview_view:is_passive(id(200), View4)),
    ?assert(macula_hyparview_view:is_passive(id(201), View5)).

only_node_ids_new_to_the_view_count_test() ->
    Sender = id(2),
    Known = ids(100, 7),
    {View1, _} = shuffled(new_view(), Sender, Known, ctx(0)),
    {View2, _} = shuffled(View1, Sender, Known, ctx(0)),
    {View3, _} = shuffled(View2, Sender, Known, ctx(0)),
    {View4, Refused} = shuffled(View3, Sender, ids(110, 7), ctx(0)),
    ?assertEqual([], Refused),
    ?assertEqual(14, passive(View4)).

neighbours_have_allowances_of_their_own_test() ->
    {View1, _} = shuffled(new_view(), id(2), ids(100, 7), ctx(0)),
    {View2, _} = shuffled(View1, id(2), ids(110, 7), ctx(0)),
    {View3, _} = shuffled(View2, id(2), ids(120, 7), ctx(0)),
    {View4, Refused} = shuffled(View3, id(3), ids(130, 7), ctx(0)),
    ?assertEqual([], Refused),
    ?assertEqual(27, passive(View4)).

%% Seven node_ids take 21 seconds to come back, and then the neighbour's bucket is full and forgotten.
a_neighbours_allowance_is_forgotten_once_it_is_full_again_test() ->
    {View1, _} = shuffled(new_view(), id(2), ids(100, 7), ctx(0)),
    {View2, _} = shuffled(View1, id(3), [], ctx(20_999)),
    {View3, _} = shuffled(View2, id(3), [], ctx(21_000)),
    ?assertEqual({[id(2)], [id(2)], []},
                 {maps:keys(maps:get(placements, View1)), maps:keys(maps:get(placements, View2)),
                  maps:keys(maps:get(placements, View3))}).

%%------------------------------------------------------------------
%% SHUFFLE records
%%------------------------------------------------------------------

a_shuffle_reply_that_answers_no_shuffle_is_refused_and_not_merged_test() ->
    Sender = id(2),
    {View1, Actions} = macula_hyparview_proto:process(new_view(), Sender, reply(ids(100, 2)), ctx(0)),
    ?assertEqual([{refused, Sender, unsolicited_shuffle_reply}], Actions),
    ?assertEqual(0, passive(View1)).

a_shuffle_reply_within_30_seconds_is_merged_once_test() ->
    Sender = id(2),
    {Sent, [{send, Sender, _Shuffle}]} = macula_hyparview_proto:build_shuffle(actives([Sender]), ctx(0)),
    {View1, Actions1} = macula_hyparview_proto:process(Sent, Sender, reply([id(100)]), ctx(30_000)),
    {View2, Actions2} = macula_hyparview_proto:process(View1, Sender, reply([id(101)]), ctx(30_000)),
    ?assertEqual({[], [{refused, Sender, unsolicited_shuffle_reply}]}, {Actions1, Actions2}),
    ?assert(macula_hyparview_view:is_passive(id(100), View1)),
    ?assertNot(macula_hyparview_view:is_passive(id(101), View2)).

a_shuffle_reply_after_30_seconds_is_refused_test() ->
    Sender = id(2),
    {Sent, _} = macula_hyparview_proto:build_shuffle(actives([Sender]), ctx(0)),
    {View1, Actions} = macula_hyparview_proto:process(Sent, Sender, reply([id(100)]), ctx(30_001)),
    ?assertEqual([{refused, Sender, unsolicited_shuffle_reply}], Actions),
    ?assertEqual([], maps:get(shuffles_sent, View1)).

build_shuffle_sends_a_sample_of_at_most_7_to_an_active_neighbour_and_records_it_test() ->
    Active = [id(N) || N <- lists:seq(2, 6)],
    View0 = lists:foldl(fun(P, V) -> macula_hyparview_view:add_passive(V, P) end, actives(Active), ids(100, 10)),
    Ctx = (ctx(0))#{shuffle_active_size => 5, shuffle_passive_size => 10},
    {View1, [{send, Target, Frame}]} = macula_hyparview_proto:build_shuffle(View0, Ctx),
    ?assert(lists:member(Target, Active)),
    ?assertEqual(hyparview_shuffle, macula_frame:frame_type(Frame)),
    ?assertEqual(7, length(maps:get(peer_sample, Frame))),
    ?assertEqual([0], maps:get(shuffles_sent, View1)).

build_shuffle_sends_nothing_without_an_active_neighbour_test() ->
    {View1, Actions} = macula_hyparview_proto:build_shuffle(new_view(), ctx(0)),
    ?assertEqual({[], []}, {Actions, maps:get(shuffles_sent, View1)}).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

id(N) -> <<N:256>>.

ids(First, Count) -> [id(N) || N <- lists:seq(First, First + Count - 1)].

new_view() ->
    macula_hyparview_view:new(?SELF, #{active_cap => 5, passive_cap => 40}).

actives(Peers) ->
    lists:foldl(fun(P, V) -> macula_hyparview_view:add_active(V, P) end, new_view(), Peers).

ctx(Now) ->
    #{self_id => ?SELF, realm => ?REALM, now => Now}.

passive(View) ->
    macula_hyparview_view:passive_size(View).

%% A SHUFFLE at ttl 0 from From: this node answers it and places its sample, within From's allowance.
shuffled(View, From, Sample, Ctx) ->
    Shuffle = macula_frame:hyparview_shuffle(#{realm => ?REALM, origin => id(99), ttl => 0, peer_sample => Sample}),
    {View1, Actions} = macula_hyparview_proto:process(View, From, Shuffle, Ctx),
    {View1, [Refused || {refused, _, _} = Refused <- Actions]}.

reply(Sample) ->
    macula_frame:hyparview_shuffle_reply(#{realm => ?REALM, peer_sample => Sample}).

forward_join(NewMember, Ttl, Arwl, Prwl) ->
    macula_frame:hyparview_forward_join(#{realm => ?REALM, new_member => NewMember, ttl => Ttl, arwl => Arwl,
                                          prwl => Prwl}).
