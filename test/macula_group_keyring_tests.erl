%%%-------------------------------------------------------------------
%%% @doc A node's keyring for the sealed groups it joined
%%% (plans/DESIGN_E2E_SEALED_PUBSUB.md §4, §6, §7): it pulls a group's epochs
%%% from the org's distributor over a sealed call carrying the org's UCAN,
%%% re-pulls in every ahead window, retries a failed pull with backoff, keeps
%%% the group's policy monotonic, and bounds the pulls unknown epoch ids cost.
%%%
%%% Most cases pull from the real distributor (macula_group_keys) through a
%%% wire that answers as macula:call/6 does: a result with its report, a
%%% handler's refusal as `{error, Detail}' with the detail as text.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_group_keyring_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MIN, 60000).
-define(R, (15 * ?MIN)).
-define(REALM, <<7:256>>).
-define(PREFIX, <<"io.macula/acme/chat">>).
-define(TOPIC, <<"io.macula/acme/chat/room/said_v1">>).
-define(DISTRIBUTOR, <<9:256>>).
-define(ME, <<5:256>>).
-define(TOKEN, <<"org-grant">>).

keyring_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [fun joining_pulls_the_current_epoch_sealed_with_the_org_grant/1,
      fun a_prefix_without_an_org_segment_is_refused_before_any_call/1,
      fun a_second_join_pulls_nothing/1,
      fun a_publisher_seals_under_the_current_epoch_then_the_next/1,
      fun the_re_pull_is_scheduled_inside_the_ahead_window/1,
      fun a_failed_re_pull_backs_off_doubling_up_to_a_third_of_an_epoch/1,
      fun a_publisher_whose_epoch_stopped_and_whose_pull_fails_fails_closed/1,
      fun off_after_required_is_ignored/1,
      fun a_held_epoch_opens/1,
      fun an_epoch_past_its_acceptance_is_reported_expired/1,
      fun an_unknown_id_is_pulled_by_id/1,
      fun an_unknown_epoch_answer_is_remembered/1,
      fun a_publisher_costs_at_most_three_unknown_id_pulls_per_epoch/1,
      fun a_refusal_is_named_by_its_reason/1,
      fun a_distributor_that_cannot_be_reached_is_no_distributor/1,
      fun the_distributor_that_answered_is_kept/1,
      fun a_pinned_distributor_is_the_only_one_asked/1,
      fun a_reply_whose_acceptance_is_not_the_protocols_is_refused/1,
      fun a_reply_for_another_prefix_is_refused/1,
      fun the_policy_covering_a_topic_is_its_longest_joined_prefix/1]}.

%%--------------------------------------------------------------------
%% Fixture: a clock, the real distributor behind a fake wire, and a
%% scheduler that records what the keyring asks to be woken for.
%%--------------------------------------------------------------------

setup() ->
    Now = counters:new(1, []),
    counters:put(Now, 1, 1_000_000),
    Clock = fun() -> counters:get(Now, 1) end,
    Log = ets:new(log, [public, bag]),
    Script = ets:new(script, [public, ordered_set]),
    {ok, Distributor} = macula_group_keys:start_link(
                          #{org => <<"acme">>, policy => required, rotate_after_ms => ?R, now => Clock,
                            membership => fun(_Caller) -> ok end}),
    Handler = macula_group_keys:handler(Distributor),
    {ok, Keyring} = macula_group_keyring:start_link(
                      #{now => Clock,
                        call => fun(Realm, Procedure, Payload, Opts) ->
                                    ets:insert(Log, {call, erlang:unique_integer([monotonic]),
                                                     {Realm, Procedure, Payload, Opts}}),
                                    answer(Script, Handler, Payload)
                                end,
                        schedule => fun(DelayMs, Msg) ->
                                        ets:insert(Log, {schedule, erlang:unique_integer([monotonic]),
                                                         {DelayMs, Msg}})
                                    end,
                        uniform => fun() -> 0.5 end}),
    #{keyring => Keyring, distributor => Distributor, now => Now, log => Log, script => Script}.

cleanup(#{keyring := Keyring, distributor := Distributor, log := Log, script := Script}) ->
    [begin unlink(P), exit(P, shutdown) end || P <- [Keyring, Distributor]],
    ets:delete(Log),
    ets:delete(Script).

%% The next scripted answer if one is queued, else the distributor's, as
%% macula:call/6 with `report => true' returns it.
answer(Script, Handler, Payload) ->
    scripted(ets:first(Script), Script, Handler, Payload).

scripted('$end_of_table', _Script, Handler, Payload) ->
    on_the_wire(Handler(Payload#{caller => ?ME}));
scripted(Key, Script, Handler, Payload) ->
    [{_, Answer}] = ets:lookup(Script, Key),
    ets:delete(Script, Key),
    from_script(Answer, Handler, Payload).

from_script(distributor, Handler, Payload) -> on_the_wire(Handler(Payload#{caller => ?ME}));
from_script({reply, Fun}, Handler, Payload) -> on_the_wire(Fun(Handler(Payload#{caller => ?ME})));
from_script(Answer, _Handler, _Payload) -> Answer.

on_the_wire({error, Reason}) -> {error, atom_to_binary(Reason)};
on_the_wire(Reply) -> {ok, Reply, #{sealed => 1, provider => ?DISTRIBUTOR}}.

queue(#{script := Script}, Answers) ->
    [ets:insert(Script, {erlang:unique_integer([monotonic]), A}) || A <- Answers],
    ok.

calls(#{log := Log}) -> [C || {_, C} <- lists:keysort(1, [{I, C} || {call, I, C} <- ets:tab2list(Log)])].

schedules(#{log := Log}) -> [S || {_, S} <- lists:keysort(1, [{I, S} || {schedule, I, S} <- ets:tab2list(Log)])].

advance(#{now := Now}, Ms) -> counters:add(Now, 1, Ms).

join(#{keyring := K}) -> macula_group_keyring:join(K, ?REALM, ?PREFIX, #{ucan_token => ?TOKEN}).

publish_epoch(#{keyring := K}) -> macula_group_keyring:publish_epoch(K, ?REALM, ?PREFIX).

open_epoch(#{keyring := K}, Id, Publisher) -> macula_group_keyring:open_epoch(K, ?REALM, ?PREFIX, Id, Publisher).

%% Wakes the keyring as its scheduler would have, for the last thing it asked.
wake(#{keyring := K} = W) ->
    {_Delay, Msg} = lists:last(schedules(W)),
    K ! Msg,
    _ = sys:get_state(K),
    ok.

%%--------------------------------------------------------------------
%% Cases
%%--------------------------------------------------------------------

joining_pulls_the_current_epoch_sealed_with_the_org_grant(W) ->
    ?_test(begin
        ?assertEqual({ok, required}, join(W)),
        [{Realm, Procedure, Payload, Opts}] = calls(W),
        ?assertEqual({?REALM, <<"acme/group_keys_v1">>}, {Realm, Procedure}),
        ?assertEqual(#{{text, <<"prefix">>} => {text, ?PREFIX}, {text, <<"epoch">>} => {text, <<"current">>}},
                     Payload),
        ?assertMatch(#{confidential := required, ucan_token := ?TOKEN, report := true}, Opts),
        ?assertNot(maps:is_key(provider, Opts))
    end).

a_prefix_without_an_org_segment_is_refused_before_any_call(#{keyring := K} = W) ->
    ?_test(begin
        ?assertEqual({error, {invalid_option, group}}, macula_group_keyring:join(K, ?REALM, <<"io.macula">>, #{})),
        ?assertEqual({error, {invalid_option, group}},
                     macula_group_keyring:join(K, ?REALM, <<"io.macula//chat">>, #{})),
        ?assertEqual([], calls(W))
    end).

a_second_join_pulls_nothing(W) ->
    ?_test(begin
        {ok, required} = join(W),
        ?assertEqual({ok, required}, join(W)),
        ?assertEqual(1, length(calls(W)))
    end).

a_publisher_seals_under_the_current_epoch_then_the_next(W) ->
    ?_test(begin
        {ok, _} = join(W),
        {ok, #{id := First, publish_until := P}} = publish_epoch(W),
        advance(W, ?R - ?R div 3),
        wake(W),
        {ok, #{id := StillFirst}} = publish_epoch(W),
        ?assertEqual(First, StillFirst),
        advance(W, P - (1_000_000 + ?R - ?R div 3)),
        {ok, #{id := Next, issued_at := P}} = publish_epoch(W),
        ?assertNotEqual(First, Next),
        ?assertEqual(2, length(calls(W)))
    end).

the_re_pull_is_scheduled_inside_the_ahead_window(W) ->
    ?_test(begin
        {ok, _} = join(W),
        [{Delay, _Msg}] = schedules(W),
        %% uniform 0.5: halfway through the last third of the epoch.
        ?assertEqual(?R - ?R div 3 + (?R div 3) div 2, Delay)
    end).

a_failed_re_pull_backs_off_doubling_up_to_a_third_of_an_epoch(W) ->
    ?_test(begin
        {ok, _} = join(W),
        queue(W, lists:duplicate(12, {error, timeout})),
        Delays = [begin wake(W), element(1, lists:last(schedules(W))) end || _ <- lists:seq(1, 12)],
        ?assertEqual([1000, 2000, 4000, 8000, 16000, 32000, 64000, 128000, 256000, ?R div 3, ?R div 3, ?R div 3],
                     Delays)
    end).

a_publisher_whose_epoch_stopped_and_whose_pull_fails_fails_closed(W) ->
    ?_test(begin
        {ok, _} = join(W),
        advance(W, ?R),
        queue(W, [{error, timeout}]),
        ?assertEqual({error, no_distributor}, publish_epoch(W))
    end).

off_after_required_is_ignored(W) ->
    ?_test(begin
        {ok, required} = join(W),
        queue(W, [{reply, fun(Reply) -> Reply#{policy := {text, <<"off">>}} end}]),
        advance(W, ?R - ?R div 3),
        wake(W),
        ?assertEqual({ok, ?PREFIX, required}, macula_group_keyring:policy(maps:get(keyring, W), ?REALM, ?TOPIC))
    end).

a_held_epoch_opens(W) ->
    ?_test(begin
        {ok, _} = join(W),
        {ok, #{id := Id, key := Key}} = publish_epoch(W),
        ?assertMatch({ok, #{id := Id, key := Key}}, open_epoch(W, Id, <<1:256>>)),
        ?assertEqual(1, length(calls(W)))
    end).

an_epoch_past_its_acceptance_is_reported_expired(W) ->
    ?_test(begin
        {ok, _} = join(W),
        {ok, #{id := Id, accept_until := A}} = publish_epoch(W),
        advance(W, A - 1_000_000 + 5 * ?MIN + 1),
        ?assertEqual({error, epoch_expired}, open_epoch(W, Id, <<1:256>>))
    end).

%% A subscriber that joined after an epoch was issued, and missed it, pulls it by id.
an_unknown_id_is_pulled_by_id(#{distributor := D} = W) ->
    ?_test(begin
        Other = macula_group_keys:handler(D),
        Reply = Other(#{{text, <<"prefix">>} => {text, ?PREFIX}, {text, <<"epoch">>} => {text, <<"current">>},
                        caller => <<1:256>>}),
        [#{id := Missed}] = maps:get(epochs, Reply),
        %% The group sits idle a whole rotation, so this node's join gets a fresh epoch.
        advance(W, 2 * ?R),
        {ok, #{id := Current}} = begin {ok, _} = join(W), publish_epoch(W) end,
        ?assertNotEqual(Missed, Current),
        ?assertMatch({ok, #{id := Missed}}, open_epoch(W, Missed, <<1:256>>)),
        {_, _, ByIdPayload, _} = lists:last(calls(W)),
        ?assertEqual(Missed, maps:get({text, <<"epoch">>}, ByIdPayload))
    end).

an_unknown_epoch_answer_is_remembered(W) ->
    ?_test(begin
        {ok, _} = join(W),
        Id = <<1:64>>,
        ?assertEqual({error, unknown_epoch}, open_epoch(W, Id, <<1:256>>)),
        ?assertEqual({error, unknown_epoch}, open_epoch(W, Id, <<2:256>>)),
        ?assertEqual(2, length(calls(W)))
    end).

a_publisher_costs_at_most_three_unknown_id_pulls_per_epoch(W) ->
    ?_test(begin
        {ok, _} = join(W),
        Publisher = <<1:256>>,
        [?assertEqual({error, unknown_epoch}, open_epoch(W, <<N:64>>, Publisher)) || N <- lists:seq(1, 4)],
        ?assertEqual(1 + 3, length(calls(W))),
        advance(W, ?R),
        ?assertEqual({error, unknown_epoch}, open_epoch(W, <<5:64>>, Publisher)),
        ?assertEqual(1 + 4, length(calls(W)))
    end).

a_refusal_is_named_by_its_reason(#{keyring := K} = W) ->
    ?_test(begin
        queue(W, [{error, <<"not_a_member">>}]),
        ?assertEqual({error, not_a_member}, macula_group_keyring:join(K, ?REALM, ?PREFIX, #{})),
        queue(W, [{error, <<"membership_unknown">>}]),
        ?assertEqual({error, membership_unknown}, macula_group_keyring:join(K, ?REALM, ?PREFIX, #{}))
    end).

a_distributor_that_cannot_be_reached_is_no_distributor(#{keyring := K} = W) ->
    ?_test(begin
        queue(W, [{error, {unresolved, no_provider}}, {error, timeout}, {error, {confidentiality, no_kem_key}}]),
        [?assertEqual({error, no_distributor}, macula_group_keyring:join(K, ?REALM, ?PREFIX, #{}))
         || _ <- lists:seq(1, 3)]
    end).

the_distributor_that_answered_is_kept(W) ->
    ?_test(begin
        {ok, _} = join(W),
        advance(W, ?R - ?R div 3),
        wake(W),
        [_, {_, _, _, Opts}] = calls(W),
        ?assertEqual(?DISTRIBUTOR, maps:get(provider, Opts))
    end).

a_pinned_distributor_is_the_only_one_asked(#{keyring := K} = W) ->
    ?_test(begin
        Pinned = <<3:256>>,
        {ok, _} = macula_group_keyring:join(K, ?REALM, ?PREFIX, #{distributor => Pinned}),
        [{_, _, _, Opts}] = calls(W),
        ?assertEqual(Pinned, maps:get(provider, Opts))
    end).

a_reply_whose_acceptance_is_not_the_protocols_is_refused(#{keyring := K} = W) ->
    ?_test(begin
        queue(W, [{reply, fun(#{epochs := [E]} = Reply) ->
                                  Reply#{epochs := [E#{accept_until := maps:get(accept_until, E) + ?MIN}]}
                          end}]),
        ?assertEqual({error, no_distributor}, macula_group_keyring:join(K, ?REALM, ?PREFIX, #{})),
        ?assertEqual(none, macula_group_keyring:policy(K, ?REALM, ?TOPIC))
    end).

a_reply_for_another_prefix_is_refused(#{keyring := K} = W) ->
    ?_test(begin
        queue(W, [{reply, fun(Reply) -> Reply#{prefix := {text, <<"io.macula/acme/other">>}} end}]),
        ?assertEqual({error, no_distributor}, macula_group_keyring:join(K, ?REALM, ?PREFIX, #{}))
    end).

the_policy_covering_a_topic_is_its_longest_joined_prefix(#{keyring := K} = W) ->
    ?_test(begin
        {ok, _} = join(W),
        {ok, _} = macula_group_keyring:join(K, ?REALM, <<?PREFIX/binary, "/room">>, #{}),
        ?assertEqual({ok, <<?PREFIX/binary, "/room">>, required}, macula_group_keyring:policy(K, ?REALM, ?TOPIC)),
        ?assertEqual({ok, ?PREFIX, required},
                     macula_group_keyring:policy(K, ?REALM, <<?PREFIX/binary, "/lobby/said_v1">>)),
        ?assertEqual(none, macula_group_keyring:policy(K, ?REALM, <<"io.macula/acme/chatter/x_v1">>)),
        ?assertEqual(none, macula_group_keyring:policy(K, <<8:256>>, ?TOPIC))
    end).
