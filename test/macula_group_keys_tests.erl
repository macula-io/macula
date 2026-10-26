%%%-------------------------------------------------------------------
%%% @doc A sealed group's distributor (docs/design/DESIGN_E2E_SEALED_PUBSUB.md §3,
%%% §5): the `<org>/group_keys_v1' handler, its epochs per group, who it
%%% admits and how it refuses. A group not listed keeps the org's grant, which
%%% the handler checks on the call's own token (macula_group_keys_access_tests
%%% has the per-group policies); then it decides from realm membership and the
%%% application's removed set.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_group_keys_tests).

-include_lib("eunit/include/eunit.hrl").

%% The logger handler the crash report case captures events with.
-export([log/2]).

-define(MIN, 60000).
-define(R, (15 * ?MIN)).
-define(ORG, <<"acme">>).
-define(PREFIX, <<"io.macula/acme/chat">>).
-define(REALM, crypto:hash(sha256, <<"io.macula">>)).
%% Every token here is valid from the distributor's clock at 1,000 s far past any test's advance.
-define(EXP_S, 100_000).

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
      fun the_policy_rides_every_reply/1,
      fun no_epoch_key_shows_in_its_state_or_status_and_no_other_process_reads_its_table/1]}.

setup() ->
    Now = counters:new(1, []),
    counters:put(Now, 1, 1_000_000),
    Members = ets:new(members, [public, set]),
    Removed = ets:new(removed, [public, set]),
    OrgKey = org_key(),
    {ok, Pid} = macula_group_keys:start_link(
                  (options(OrgKey))#{rotate_after_ms => ?R,
                                     now => fun() -> counters:get(Now, 1) end,
                                     membership => fun(Caller) -> membership(Members, Caller) end,
                                     removed => fun(Caller) -> ets:member(Removed, Caller) end}),
    #{pid => Pid, now => Now, members => Members, removed => Removed, org_key => OrgKey,
      handler => macula_group_keys:handler(Pid)}.

options(OrgKey) ->
    #{org => ?ORG, policy => required, org_key_id => macula_node_keys:key_id(OrgKey), realm => ?REALM,
      profile => pq_pure}.

org_key() ->
    {ok, OrgKey} = macula_node_keys:generate(org, pq_pure),
    OrgKey.

%% The org's grant for Caller, as the link hands the call's token to the handler.
org_grant(OrgKey, Caller) ->
    macula_ucan:create(OrgKey, Caller, [#{with => <<"mri:org:io.macula/acme">>, can => <<"group_keys">>}],
                       #{exp => ?EXP_S}).

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

call(#{handler := Handler, org_key := OrgKey}, Caller, Epoch) ->
    Handler(#{{text, <<"prefix">>} => {text, ?PREFIX}, {text, <<"epoch">>} => Epoch, caller => Caller,
              ucan_token => org_grant(OrgKey, Caller), proofs => []}).

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

%% The epochs live in a private table: the state holds its id only, so neither sys:get_state/1 nor sys:get_status/1
%% shows a key, and the table refuses every other process.
no_epoch_key_shows_in_its_state_or_status_and_no_other_process_reads_its_table(#{pid := Pid} = W) ->
    ?_test(begin
        [#{key := Key} | _] = epochs(call(W, member(W), {text, <<"current">>})),
        ?assertNot(holds(sys:get_state(Pid), Key)),
        ?assertNot(holds(sys:get_status(Pid), Key)),
        Tables = [T || T <- ets:all(), ets:info(T, owner) =:= Pid],
        ?assertEqual([private], lists:usort([ets:info(T, protection) || T <- Tables])),
        ?assertError(badarg, ets:tab2list(hd(Tables)))
    end).

%% A pool named rather than given as a pid is looked up at every read: while no pool holds the name, a pull is
%% membership_unknown and the distributor stays up.
a_named_pool_that_is_not_running_is_membership_unknown_test() ->
    OrgKey = org_key(),
    {ok, Pid} = macula_group_keys:start_link((options(OrgKey))#{pool => macula_group_keys_tests_no_pool,
                                                                realm_key_id => <<2:256>>,
                                                                now => fun() -> 1_000_000 end}),
    Handler = macula_group_keys:handler(Pid),
    Pull = #{{text, <<"prefix">>} => {text, ?PREFIX}, {text, <<"epoch">>} => {text, <<"current">>}, caller => <<3:256>>,
             ucan_token => org_grant(OrgKey, <<3:256>>)},
    ?assertEqual({error, membership_unknown}, Handler(Pull)),
    ?assert(is_process_alive(Pid)),
    unlink(Pid),
    exit(Pid, shutdown).

%% A distributor that crashes after serving a key logs no form of it: not in the report's terms, not in its text.
a_crash_report_carries_no_epoch_key_test() ->
    Trapping = process_flag(trap_exit, true),
    OrgKey = org_key(),
    {ok, Pid} = macula_group_keys:start_link((options(OrgKey))#{membership => fun(_) -> ok end,
                                                                now => fun() -> 1_000_000 end}),
    Handler = macula_group_keys:handler(Pid),
    #{epochs := [#{key := Key} | _]} =
        Handler(#{{text, <<"prefix">>} => {text, ?PREFIX}, {text, <<"epoch">>} => {text, <<"current">>},
                  caller => <<1:256>>, ucan_token => org_grant(OrgKey, <<1:256>>)}),
    Events = captured(fun() ->
                          try gen_server:call(Pid, not_a_pull) catch exit:_ -> ok end,
                          receive {'EXIT', Pid, _} -> ok after 5_000 -> still_running end
                      end),
    _ = process_flag(trap_exit, Trapping),
    ?assertNotEqual([], Events),
    ?assertEqual([], [E || E <- Events, holds(E, Key) orelse holds(formatted(E), printed(Key))]).

holds(Term, Bytes) when is_binary(Term) -> binary:match(Term, Bytes) =/= nomatch;
holds(Term, Bytes) -> holds(term_to_binary(Term), Bytes).

%% A key as a report prints it: its bytes as decimal integers, comma separated.
printed(Key) ->
    iolist_to_binary(lists:join(",", [integer_to_list(Byte) || <<Byte>> <= Key])).

formatted(Event) ->
    Config = #{single_line => true, legacy_header => false, chars_limit => unlimited, depth => unlimited,
               max_size => unlimited},
    unicode:characters_to_binary(logger_formatter:format(Event, Config)).

%% The events every handler receives while Act runs, through a handler added for the run.
captured(Act) ->
    Id = list_to_atom("macula_group_keys_capture_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Id, ?MODULE, #{config => #{test => self()}, level => all, filter_default => log}),
    _ = Act(),
    timer:sleep(500),
    ok = logger:remove_handler(Id),
    drained([]).

log(Event, #{config := #{test := Test}}) ->
    Test ! {captured, Event}.

drained(Events) ->
    receive {captured, Event} -> drained([Event | Events]) after 0 -> lists:reverse(Events) end.

%% A key never travels clear (docs/design/DESIGN_E2E_SEALED_PUBSUB.md §2): the distributor's own advertise options take
%% sealed calls only, AND hand the handler the call's token, which it checks per group (macula#129). With the node's KEM
%% key named they make the spec whose provider link refuses a clear CALL as sealed_required
%% (macula_station_link_kem_advertise_tests: required_refuses_a_clear_call_test_); with it unnamed they refuse to
%% advertise at all, rather than advertise a keyless distributor.
advertise_opts_take_the_calls_token_and_sealed_calls_only_test() ->
    ?assertEqual(#{auth => handler_authorizes, confidential => required}, macula_group_keys:advertise_opts()).

advertise_opts_make_a_sealed_only_spec_or_none_test() ->
    Prev = application:get_env(macula, kem_advertise),
    try
        ok = application:set_env(macula, kem_advertise, enabled),
        ?assertEqual({ok, #{kem => true, confidential => required}},
                     macula:advertise_confidentiality(macula_group_keys:advertise_opts())),
        ok = application:set_env(macula, kem_advertise, disabled),
        ?assertEqual({error, {confidentiality, kem_advertise_disabled}},
                     macula:advertise_confidentiality(macula_group_keys:advertise_opts()))
    after
        restore_env(Prev)
    end.

restore_env(undefined) -> application:unset_env(macula, kem_advertise);
restore_env({ok, Value}) -> application:set_env(macula, kem_advertise, Value).
