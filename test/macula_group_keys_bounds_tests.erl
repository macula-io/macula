%%%-------------------------------------------------------------------
%%% @doc What a pull can cost a sealed group's distributor (macula#130): a slow
%%% membership lookup holds up only its own pull, lookups are bounded in time
%%% and number, and unlisted prefixes cannot grow its table past `max_groups'.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_group_keys_bounds_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MIN, 60000).
-define(R, (15 * ?MIN)).
-define(ORG, <<"acme">>).
-define(PREFIX, <<"io.macula/acme/chat">>).
-define(REALM, crypto:hash(sha256, <<"io.macula">>)).
-define(EXP_S, 100_000).

%% A distributor whose membership answers from `Members': a node mapped to `{wait, Gate}' waits until the gate opens,
%% telling the test its lookup's pid first.
started(Extra) ->
    Now = counters:new(1, []),
    counters:put(Now, 1, 1_000_000),
    Members = ets:new(members, [public, set]),
    OrgKey = org_key(),
    Test = self(),
    {ok, Pid} = macula_group_keys:start_link(
                  maps:merge(#{org => ?ORG, policy => required, org_key_id => macula_node_keys:key_id(OrgKey),
                               realm => ?REALM, profile => pq_pure, rotate_after_ms => ?R,
                               now => fun() -> counters:get(Now, 1) end,
                               membership => fun(Caller) -> membership(Members, Caller, Test) end},
                             Extra)),
    #{pid => Pid, now => Now, members => Members, org_key => OrgKey, handler => macula_group_keys:handler(Pid)}.

stopped(#{pid := Pid, members := Members}) ->
    unlink(Pid),
    exit(Pid, shutdown),
    ets:delete(Members).

membership(Members, Caller, Test) ->
    membership_of(ets:lookup(Members, Caller), Test).

membership_of([{_, {wait, Gate}}], Test) ->
    Test ! {lookup, self()},
    receive {open, Gate, Answer} -> Answer end;
membership_of([{_, crash}], _Test) ->
    exit(lookup_crashed);
membership_of([{_, Answer}], _Test) ->
    Answer;
membership_of([], _Test) ->
    {error, not_a_member}.

org_key() ->
    {ok, OrgKey} = macula_node_keys:generate(org, pq_pure),
    OrgKey.

node_as(#{members := Members}, Answer) ->
    Node = crypto:strong_rand_bytes(32),
    true = ets:insert(Members, {Node, Answer}),
    Node.

pull(W, Caller) -> pull(W, Caller, ?PREFIX).

pull(#{handler := Handler, org_key := OrgKey}, Caller, Prefix) ->
    Grant = macula_ucan:create(OrgKey, Caller, [#{with => <<"mri:org:io.macula/acme">>, can => <<"group_keys">>}],
                               #{exp => ?EXP_S}),
    Handler(#{{text, <<"prefix">>} => {text, Prefix}, {text, <<"epoch">>} => {text, <<"current">>}, caller => Caller,
              ucan_token => Grant, proofs => []}).

%% A pull run by a process of its own, its reply sent to the test.
pull_aside(W, Caller) ->
    Test = self(),
    Ref = make_ref(),
    spawn_link(fun() -> Test ! {pulled, Ref, pull(W, Caller)} end),
    Ref.

awaited(Ref) ->
    receive {pulled, Ref, Reply} -> Reply after 15_000 -> no_reply end.

lookup_started() ->
    receive {lookup, Pid} -> Pid after 5_000 -> no_lookup end.

%% Until N pulls of Caller wait on its lookup, as the distributor's state shows.
waiting_on(W, Caller, N) -> waiting_on(W, Caller, N, 200).

waiting_on(_W, _Caller, _N, 0) ->
    timeout;
waiting_on(#{pid := Pid} = W, Caller, N, Tries) ->
    #{lookups := Lookups} = sys:get_state(Pid),
    waited(length(maps:get(waiting, maps:get(Caller, Lookups, #{}), [])) >= N, W, Caller, N, Tries).

waited(true, _W, _Caller, _N, _Tries) -> ok;
waited(false, W, Caller, N, Tries) -> timer:sleep(10), waiting_on(W, Caller, N, Tries - 1).

table_size(#{pid := Pid}) ->
    [Table] = [T || T <- ets:all(), ets:info(T, owner) =:= Pid],
    ets:info(Table, size).

%% While one member's lookup hangs, another member of the same group is served, and the first is served once its
%% lookup answers.
a_slow_lookup_does_not_delay_another_pull_test() ->
    W = started(#{}),
    Gate = make_ref(),
    Slow = pull_aside(W, node_as(W, {wait, Gate})),
    Lookup = lookup_started(),
    ?assertMatch(#{epochs := [_]}, pull(W, node_as(W, ok))),
    Lookup ! {open, Gate, ok},
    ?assertMatch(#{epochs := [_]}, awaited(Slow)),
    stopped(W).

%% A refusal from a lookup that waited still refuses: the order of checks is kept.
a_slow_lookup_that_refuses_refuses_test() ->
    W = started(#{}),
    Gate = make_ref(),
    Slow = pull_aside(W, node_as(W, {wait, Gate})),
    lookup_started() ! {open, Gate, {error, not_a_member}},
    ?assertEqual({error, not_a_member}, awaited(Slow)),
    stopped(W).

%% A lookup past its wait is ended and its pull answered membership_unknown, so its member retries.
a_lookup_past_its_wait_is_membership_unknown_and_ended_test() ->
    W = started(#{lookup_wait_ms => 200}),
    Slow = pull_aside(W, node_as(W, {wait, make_ref()})),
    Lookup = lookup_started(),
    ?assertEqual({error, membership_unknown}, awaited(Slow)),
    ?assertNot(is_process_alive(Lookup)),
    ?assertMatch(#{epochs := [_]}, pull(W, node_as(W, ok))),
    stopped(W).

%% Past max_lookups at once, a pull is membership_unknown at once; a freed slot serves again.
lookups_past_the_bound_are_membership_unknown_test() ->
    W = started(#{max_lookups => 1}),
    Gate = make_ref(),
    Slow = pull_aside(W, node_as(W, {wait, Gate})),
    Lookup = lookup_started(),
    ?assertEqual({error, membership_unknown}, pull(W, node_as(W, ok))),
    Lookup ! {open, Gate, ok},
    ?assertMatch(#{epochs := [_]}, awaited(Slow)),
    ?assertMatch(#{epochs := [_]}, pull(W, node_as(W, ok))),
    stopped(W).

%% A lookup that crashes, or answers out of its contract, is membership_unknown and the distributor stays up.
a_failing_lookup_is_membership_unknown_test() ->
    W = started(#{}),
    ?assertEqual({error, membership_unknown}, pull(W, node_as(W, not_an_answer))),
    ?assertEqual({error, membership_unknown}, pull(W, node_as(W, crash))),
    ?assertMatch(#{epochs := [_]}, pull(W, node_as(W, ok))),
    stopped(W),
    Pid = dead_pool(),
    OrgKey = org_key(),
    {ok, D} = macula_group_keys:start_link(#{org => ?ORG, policy => required,
                                             org_key_id => macula_node_keys:key_id(OrgKey), realm => ?REALM,
                                             profile => pq_pure, pool => Pid, realm_key_id => <<2:256>>,
                                             now => fun() -> 1_000_000 end}),
    ?assertEqual({error, membership_unknown}, pull(#{handler => macula_group_keys:handler(D), org_key => OrgKey},
                                                   <<3:256>>)),
    ?assert(is_process_alive(D)),
    unlink(D),
    exit(D, shutdown).

dead_pool() ->
    {Pid, Ref} = spawn_monitor(fun() -> ok end),
    receive {'DOWN', Ref, process, Pid, _} -> Pid end.

%% Prefix after prefix: the table stops at max_groups, a new one past it is too_many_groups, and a group already kept
%% and a listed group are still served.
unlisted_prefixes_past_the_bound_are_refused_and_the_table_stays_flat_test() ->
    Listed = <<"io.macula/acme/listed">>,
    W = started(#{max_groups => 3, groups => #{Listed => org_grant}}),
    Node = node_as(W, ok),
    Prefixes = [<<"io.macula/acme/g", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 3)],
    [?assertMatch(#{epochs := [_]}, pull(W, Node, P)) || P <- Prefixes],
    [?assertEqual({error, too_many_groups}, pull(W, Node, <<"io.macula/acme/x", (integer_to_binary(N))/binary>>))
     || N <- lists:seq(1, 50)],
    ?assertEqual(3, table_size(W)),
    ?assertMatch(#{epochs := [_]}, pull(W, Node, hd(Prefixes))),
    ?assertMatch(#{epochs := [_]}, pull(W, Node, Listed)),
    ?assertEqual(4, table_size(W)),
    stopped(W).

%% Groups idle past their keeping give their rows back: a new group is served then, and the table stays bounded.
idle_groups_give_their_rows_back_test() ->
    W = started(#{max_groups => 2}),
    Node = node_as(W, ok),
    [?assertMatch(#{epochs := [_]}, pull(W, Node, P)) || P <- [<<"io.macula/acme/a">>, <<"io.macula/acme/b">>]],
    ?assertEqual({error, too_many_groups}, pull(W, Node, <<"io.macula/acme/c">>)),
    #{now := Now} = W,
    counters:add(Now, 1, 3 * ?R + 70 * ?MIN),
    ?assertMatch(#{epochs := [_]}, pull(W, Node, <<"io.macula/acme/c">>)),
    ?assertEqual(1, table_size(W)),
    stopped(W).

%% One slow caller pulling again and again holds one lookup, not many: its pulls share that lookup's answer, and
%% another member is served meanwhile.
one_slow_caller_holds_one_lookup_test() ->
    W = started(#{max_lookups => 2}),
    Gate = make_ref(),
    Slow = node_as(W, {wait, Gate}),
    First = pull_aside(W, Slow),
    Lookup = lookup_started(),
    Second = pull_aside(W, Slow),
    ok = waiting_on(W, Slow, 2),
    ?assertMatch(#{epochs := [_]}, pull(W, node_as(W, ok))),
    ?assertEqual(no_lookup, receive {lookup, _} -> another_lookup after 100 -> no_lookup end),
    Lookup ! {open, Gate, ok},
    ?assertMatch(#{epochs := [_]}, awaited(First)),
    ?assertMatch(#{epochs := [_]}, awaited(Second)),
    stopped(W).

%% Past 16 pulls waiting on one caller's lookup, its next pull is membership_unknown at once.
a_callers_waiting_pulls_are_bounded_test() ->
    W = started(#{}),
    Gate = make_ref(),
    Slow = node_as(W, {wait, Gate}),
    Waiting = [pull_aside(W, Slow) || _ <- lists:seq(1, 16)],
    Lookup = lookup_started(),
    ok = waiting_on(W, Slow, 16),
    ?assertEqual({error, membership_unknown}, pull(W, Slow)),
    Lookup ! {open, Gate, ok},
    [?assertMatch(#{epochs := [_]}, awaited(Ref)) || Ref <- Waiting],
    stopped(W).

%% A node removed while its lookup ran is refused when the answer comes, a member answer notwithstanding.
a_node_removed_during_its_lookup_is_refused_test() ->
    Removed = ets:new(removed, [public, set]),
    W = started(#{removed => fun(Caller) -> ets:member(Removed, Caller) end}),
    Gate = make_ref(),
    Slow = node_as(W, {wait, Gate}),
    Pending = pull_aside(W, Slow),
    Lookup = lookup_started(),
    true = ets:insert(Removed, {Slow}),
    Lookup ! {open, Gate, ok},
    ?assertEqual({error, not_a_member}, awaited(Pending)),
    stopped(W),
    ets:delete(Removed).

%% The bound counts unlisted groups only: listed groups pulled first leave every unlisted row free.
the_bound_counts_unlisted_groups_only_test() ->
    Listed = [<<"io.macula/acme/l1">>, <<"io.macula/acme/l2">>],
    W = started(#{max_groups => 2, groups => maps:from_keys(Listed, org_grant)}),
    Node = node_as(W, ok),
    Unlisted = [<<"io.macula/acme/u1">>, <<"io.macula/acme/u2">>],
    [?assertMatch(#{epochs := [_]}, pull(W, Node, P)) || P <- Listed ++ Unlisted],
    ?assertEqual({error, too_many_groups}, pull(W, Node, <<"io.macula/acme/u3">>)),
    ?assertEqual(4, table_size(W)),
    stopped(W).

%% An answer from a process other than the caller's lookup in flight answers nothing: the pull still waits on its own.
an_answer_from_another_process_answers_nothing_test() ->
    W = started(#{}),
    Gate = make_ref(),
    Slow = node_as(W, {wait, Gate}),
    Pending = pull_aside(W, Slow),
    Lookup = lookup_started(),
    #{pid := Pid} = W,
    Pid ! {membership, Slow, self(), ok},
    _ = sys:get_state(Pid),
    ?assertEqual(still_waiting, receive {pulled, Pending, _} -> answered after 200 -> still_waiting end),
    Lookup ! {open, Gate, {error, not_a_member}},
    ?assertEqual({error, not_a_member}, awaited(Pending)),
    stopped(W).
