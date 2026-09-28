%%%-------------------------------------------------------------------
%%% @doc A sealed group's distributor (plans/DESIGN_E2E_SEALED_PUBSUB.md §3,
%%% §5): the `<org>/group_keys_v1' handler, its epochs per group, who it
%%% admits and how it refuses. The org's UCAN is enforced by the procedure's
%%% advertise policy before the handler runs; here the handler decides from
%%% realm membership and the application's removed set.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_group_keys_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MIN, 60000).
-define(R, (15 * ?MIN)).
-define(ORG, <<"acme">>).
-define(PREFIX, <<"io.macula/acme/chat">>).

distributor_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun a_member_pulls_the_current_epoch/1,
      fun inside_the_ahead_window_the_next_epoch_comes_too/1,
      fun past_publish_until_the_next_epoch_is_current/1,
      fun a_non_member_is_refused/1,
      fun a_failed_membership_lookup_is_membership_unknown/1,
      fun a_removed_member_is_refused_every_epoch/1,
      fun a_past_epoch_is_served_by_id/1,
      fun an_unknown_epoch_id_is_refused/1,
      fun an_epoch_past_its_acceptance_is_refused/1,
      fun another_orgs_group_is_refused/1,
      fun a_prefix_without_an_org_segment_is_refused/1,
      fun a_call_without_a_caller_is_refused/1,
      fun the_policy_rides_every_reply/1]}.

setup() ->
    Now = counters:new(1, []),
    counters:put(Now, 1, 1_000_000),
    Members = ets:new(members, [public, set]),
    Removed = ets:new(removed, [public, set]),
    {ok, Pid} = macula_group_keys:start_link(
                  #{org => ?ORG, policy => required, rotate_after_ms => ?R,
                    now => fun() -> counters:get(Now, 1) end,
                    membership => fun(Caller) -> membership(Members, Caller) end,
                    removed => fun(Caller) -> ets:member(Removed, Caller) end}),
    #{pid => Pid, now => Now, members => Members, removed => Removed, handler => macula_group_keys:handler(Pid)}.

cleanup(#{pid := Pid, members := Members, removed := Removed}) ->
    unlink(Pid),
    exit(Pid, shutdown),
    ets:delete(Members),
    ets:delete(Removed).

membership(Members, Caller) ->
    case ets:lookup(Members, Caller) of
        [{_, Answer}] -> Answer;
        [] -> {error, not_a_member}
    end.

member(#{members := Members}) ->
    Node = crypto:strong_rand_bytes(32),
    true = ets:insert(Members, {Node, ok}),
    Node.

call(#{handler := Handler}, Caller, Epoch) ->
    Handler(#{{text, <<"prefix">>} => {text, ?PREFIX}, {text, <<"epoch">>} => Epoch, caller => Caller}).

advance(#{now := Now}, Ms) -> counters:add(Now, 1, Ms).

epochs(#{epochs := Epochs}) -> Epochs.

a_member_pulls_the_current_epoch(W) ->
    ?_test(begin
        Reply = call(W, member(W), {text, <<"current">>}),
        %% Text on the wire is tagged: a bare binary would reach every other SDK as CBOR bytes.
        ?assertMatch(#{prefix := {text, ?PREFIX}, policy := {text, <<"required">>}, epochs := [_]}, Reply),
        [#{id := Id, key := Key, issued_at := I, publish_until := P, accept_until := A}] = epochs(Reply),
        ?assertEqual({8, 32}, {byte_size(Id), byte_size(Key)}),
        ?assertEqual({1_000_000, 1_000_000 + ?R, 1_000_000 + ?R + 65 * ?MIN}, {I, P, A})
    end).

inside_the_ahead_window_the_next_epoch_comes_too(W) ->
    ?_test(begin
        Node = member(W),
        [Current] = epochs(call(W, Node, {text, <<"current">>})),
        advance(W, ?R - ?R div 3),
        [Same, Next] = epochs(call(W, Node, {text, <<"current">>})),
        ?assertEqual(Current, Same),
        ?assertEqual(maps:get(publish_until, Current), maps:get(issued_at, Next)),
        %% A second pull in the window gets the same next epoch, not a new one.
        ?assertEqual([Same, Next], epochs(call(W, Node, {text, <<"current">>})))
    end).

past_publish_until_the_next_epoch_is_current(W) ->
    ?_test(begin
        Node = member(W),
        %% The group's first epoch starts at its first pull.
        [_First] = epochs(call(W, Node, {text, <<"current">>})),
        advance(W, ?R - ?R div 3),
        [_Current, Next] = epochs(call(W, Node, {text, <<"current">>})),
        advance(W, ?R div 3),
        ?assertEqual(Next, hd(epochs(call(W, Node, {text, <<"current">>}))))
    end).

a_non_member_is_refused(W) ->
    ?_assertEqual({error, not_a_member}, call(W, crypto:strong_rand_bytes(32), {text, <<"current">>})).

a_failed_membership_lookup_is_membership_unknown(#{members := Members} = W) ->
    ?_test(begin
        Node = crypto:strong_rand_bytes(32),
        true = ets:insert(Members, {Node, {error, membership_unknown}}),
        ?assertEqual({error, membership_unknown}, call(W, Node, {text, <<"current">>}))
    end).

a_removed_member_is_refused_every_epoch(#{removed := Removed} = W) ->
    ?_test(begin
        Node = member(W),
        [#{id := Id}] = epochs(call(W, Node, {text, <<"current">>})),
        true = ets:insert(Removed, {Node}),
        ?assertEqual({error, not_a_member}, call(W, Node, {text, <<"current">>})),
        ?assertEqual({error, not_a_member}, call(W, Node, Id))
    end).

a_past_epoch_is_served_by_id(W) ->
    ?_test(begin
        Node = member(W),
        [Old] = epochs(call(W, Node, {text, <<"current">>})),
        advance(W, 2 * ?R),
        ?assertEqual([Old], epochs(call(W, Node, maps:get(id, Old))))
    end).

an_unknown_epoch_id_is_refused(W) ->
    ?_assertEqual({error, unknown_epoch}, call(W, member(W), crypto:strong_rand_bytes(8))).

an_epoch_past_its_acceptance_is_refused(W) ->
    ?_test(begin
        Node = member(W),
        [#{id := Id, accept_until := A}] = epochs(call(W, Node, {text, <<"current">>})),
        advance(W, A - 1_000_000 + 5 * ?MIN + 1),
        ?assertEqual({error, epoch_expired}, call(W, Node, Id))
    end).

another_orgs_group_is_refused(#{handler := Handler} = W) ->
    ?_assertEqual({error, unknown_group},
                  Handler(#{{text, <<"prefix">>} => {text, <<"io.macula/contoso/chat">>},
                            {text, <<"epoch">>} => {text, <<"current">>}, caller => member(W)})).

a_prefix_without_an_org_segment_is_refused(#{handler := Handler} = W) ->
    ?_assertEqual({error, unknown_group},
                  Handler(#{{text, <<"prefix">>} => {text, <<"io.macula">>},
                            {text, <<"epoch">>} => {text, <<"current">>}, caller => member(W)})).

a_call_without_a_caller_is_refused(#{handler := Handler}) ->
    ?_assertEqual({error, not_a_member},
                  Handler(#{{text, <<"prefix">>} => {text, ?PREFIX}, {text, <<"epoch">>} => {text, <<"current">>}})).

the_policy_rides_every_reply(W) ->
    ?_assertMatch(#{policy := {text, <<"required">>}}, call(W, member(W), {text, <<"current">>})).
