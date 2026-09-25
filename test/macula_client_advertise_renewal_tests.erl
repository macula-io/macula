%% D32 (macula#38): a provider's delegation lives 30 minutes, so the pool renews
%% each advertised chain before it runs out. At a third of the chain's remaining
%% life the pool runs the registration's renew MFA in a worker (a DHT lookup
%% goes through the pool, so it cannot run in the pool), and re-registers the
%% fresh spec on the links it went to. A failure or a crash retries on backoff;
%% a chain past its `not_after' is logged at error level and re-checked slowly,
%% so a re-grant revives the provider without a restart. A renewal answered
%% after an unadvertise or a re-advertise is dropped: it never resurrects a
%% withdrawn procedure. A procedure in the node's own namespace has no chain,
%% and nothing renews it.
%%
%% The renew MFA here is `renewal/3' below: each renewal asks this test process
%% for its answer, so a test decides what every renewal returns and when.
-module(macula_client_advertise_renewal_tests).

-include_lib("eunit/include/eunit.hrl").

-export([renewal/3, log/2]).

-define(REALM, <<7:256>>).
-define(PROCEDURE, <<"acme.count_v1">>).
-define(SEEDS, [#{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<21:256>>}]).
-define(STATION, <<21:256>>).
-define(TWO_SEEDS, [#{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<21:256>>},
                    #{host => <<"127.0.0.1">>, port => 2, expected_node_id => <<22:256>>}]).
-define(OPTS, #{renew_backoff_ms => 200, renew_recheck_ms => 600}).

a_chain_is_renewed_at_a_third_of_its_life_and_the_links_get_it_test_() ->
    {timeout, 10,
     fun() ->
         {Pool, _Link} = connected_pool(),
         Spec = spec(3_000, <<"old delegation">>),
         Advertised = erlang:monotonic_time(millisecond),
         ok = advertise(Pool, Spec),
         [_] = sent_delegations(1),
         {Asked, _} = asked_within(2_500),
         %% Timed from the advertise call, which armed it, not from the first
         %% frame, which a loaded runner delivers late.
         ?assert(erlang:monotonic_time(millisecond) - Advertised >= 900),
         Fresh = spec(3_600_000, <<"fresh delegation">>),
         Asked ! {renewal_answer, {ok, Fresh}},
         ?assertEqual([<<"fresh delegation">>], sent_delegations(1)),
         ?assertEqual(Fresh, registered_ad(Pool)),
         ok = macula_client:close(Pool)
     end}.

a_procedure_in_the_own_namespace_is_not_renewed_test_() ->
    {timeout, 10,
     fun() ->
         {Pool, _Link} = connected_pool(),
         ok = macula_client:advertise(Pool, ?REALM, <<"~own/count_v1">>, fun(_) -> {ok, counted} end,
                                      open, #{}, all, renew_mfa()),
         ?assertEqual(none, asked_within_or_none(1_500)),
         ok = macula_client:close(Pool)
     end}.

%% A late {ok, Spec} after an unadvertise must not put the procedure back.
a_renewal_answered_after_unadvertise_does_not_resurrect_test_() ->
    {timeout, 10,
     fun() ->
         {Pool, _Link} = connected_pool(),
         ok = advertise(Pool, spec(3_000, <<"old delegation">>)),
         [_] = sent_delegations(1),
         {Asked, _} = asked_within(2_500),
         ok = macula_client:unadvertise(Pool, ?REALM, ?PROCEDURE),
         Asked ! {renewal_answer, {ok, spec(3_600_000, <<"fresh delegation">>)}},
         %% The link may still re-sign the old spec on its own schedule before
         %% the unadvertise reaches it; what must never go out is the fresh one.
         ?assertNot(lists:member(<<"fresh delegation">>, delegations_within(1_000))),
         ?assertEqual(none, registered_ad(Pool)),
         ok = macula_client:close(Pool)
     end}.

%% A re-advertise supersedes the renewal in flight: its late answer is dropped.
a_renewal_answered_after_a_readvertise_is_dropped_test_() ->
    {timeout, 10,
     fun() ->
         {Pool, _Link} = connected_pool(),
         ok = advertise(Pool, spec(3_000, <<"old delegation">>)),
         [_] = sent_delegations(1),
         {Asked, _} = asked_within(2_500),
         Readvertised = spec(3_600_000, <<"readvertised delegation">>),
         ok = advertise(Pool, Readvertised),
         [<<"readvertised delegation">>] = sent_delegations(1),
         Asked ! {renewal_answer, {ok, spec(3_600_000, <<"stale delegation">>)}},
         ?assertEqual([], sent_delegations(1)),
         ?assertEqual(Readvertised, registered_ad(Pool)),
         ok = macula_client:close(Pool)
     end}.

%% A refused renewal and a crashed one both come back on the backoff.
a_failed_or_crashed_renewal_is_retried_test_() ->
    {timeout, 10,
     fun() ->
         {Pool, _Link} = connected_pool(),
         ok = advertise(Pool, spec(6_000, <<"old delegation">>)),
         {First, _} = asked_within(3_000),
         First ! {renewal_answer, {error, {provider_authorization, {procedure_delegation, not_found}}}},
         {Second, AfterFailure} = asked_within(1_500),
         ?assert(AfterFailure >= 150),
         Second ! {renewal_answer, crash},
         {Third, _} = asked_within(1_500),
         Third ! {renewal_answer, {ok, spec(3_600_000, <<"fresh delegation">>)}},
         ?assertMatch([<<"old delegation">>, <<"fresh delegation">>], sent_delegations(2)),
         ok = macula_client:close(Pool)
     end}.

%% The realm has not reissued yet: nothing new reaches the links (a link still
%% re-signs the spec it holds, on its own schedule, so frames carrying the old
%% chain may go on), and the pool asks again on the backoff.
an_unchanged_chain_is_not_pushed_again_test_() ->
    {timeout, 10,
     fun() ->
         {Pool, _Link} = connected_pool(),
         Spec = spec(6_000, <<"old delegation">>),
         ok = advertise(Pool, Spec),
         [_] = sent_delegations(1),
         {First, _} = asked_within(3_000),
         First ! {renewal_answer, {ok, Spec}},
         {Second, _} = asked_within(1_500),
         ?assertEqual([], [D || D <- delegations_within(0), D =/= <<"old delegation">>]),
         ?assertEqual(Spec, registered_ad(Pool)),
         Second ! {renewal_answer, {ok, spec(3_600_000, <<"fresh delegation">>)}},
         ?assert(lists:member(<<"fresh delegation">>, delegations_within(1_000))),
         ok = macula_client:close(Pool)
     end}.

%% Past `not_after' with no fresh chain, the pool logs at error level, naming
%% the procedure and the last reason, and keeps checking on the slow interval;
%% a re-grant then revives the provider.
a_chain_past_not_after_is_logged_and_rechecked_slowly_test_() ->
    {timeout, 15,
     fun() ->
         Handler = capture_log(),
         try
             {Pool, _Link} = connected_pool(),
             ok = advertise(Pool, spec(1_500, <<"old delegation">>)),
             Reason = {provider_authorization, {procedure_delegation, not_found}},
             LoggedAtMs = answered_until_logged(?PROCEDURE, Reason, 5_000),
             ?assert(is_integer(LoggedAtMs)),
             {Slow, _} = asked_within(1_500),
             ?assert(erlang:system_time(millisecond) - LoggedAtMs >= 550),
             Slow ! {renewal_answer, {ok, spec(3_600_000, <<"regranted delegation">>)}},
             ?assert(lists:member(<<"regranted delegation">>, delegations_within(1_000))),
             ok = macula_client:close(Pool)
         after
             ok = logger:remove_handler(Handler)
         end
     end}.

%% A renewal of a registration pinned to several stations goes to those that
%% have a link now: one station without a link must not keep the others on a
%% chain that runs out. The station that comes back gets the fresh spec when
%% its link respawns (Fable QA on #38).
a_renewal_reaches_the_linked_stations_while_another_has_none_test_() ->
    {timeout, 10,
     fun() ->
         {Pool, [Link1, Link2]} = connected_pool(?TWO_SEEDS),
         ok = macula_client:advertise(Pool, ?REALM, ?PROCEDURE, fun(_) -> {ok, counted} end, open,
                                      spec(3_000, <<"old delegation">>), [<<21:256>>, <<22:256>>],
                                      renew_mfa()),
         [_, _] = sent_delegations(2),
         {Asked, _} = asked_within(2_500),
         Mon = erlang:monitor(process, Link2),
         exit(Link2, kill),
         receive {'DOWN', Mon, process, Link2, _} -> ok after 1_000 -> error(link_did_not_die) end,
         ok = link_gone(Pool, Link2, 20),
         Asked ! {renewal_answer, {ok, spec(3_600_000, <<"fresh delegation">>)}},
         ?assert(lists:member(<<"fresh delegation">>, delegations_within(500))),
         ?assert(is_process_alive(Link1)),
         ok = macula_client:close(Pool)
     end}.

%% A stream registration is renewed like a procedure and keeps its mode.
a_stream_registration_is_renewed_with_its_mode_test_() ->
    {timeout, 10,
     fun() ->
         {Pool, _Link} = connected_pool(),
         ok = macula_client:advertise_stream(Pool, ?REALM, ?PROCEDURE, bidi, fun(_, _) -> ok end, open,
                                             spec(3_000, <<"old delegation">>), all, renew_mfa()),
         [_] = sent_delegations(1),
         {Asked, _} = asked_within(2_500),
         Asked ! {renewal_answer, {ok, spec(3_600_000, <<"fresh delegation">>)}},
         ?assert(lists:member(<<"fresh delegation">>, delegations_within(500))),
         Streams = element(macula_client:state_field_index(stream_procs), sys:get_state(Pool)),
         ?assertMatch(#{mode := bidi, ad := #{authorization := #{procedure_delegation := <<"fresh delegation">>}}},
                      maps:get({?REALM, ?PROCEDURE}, Streams)),
         ok = macula_client:close(Pool)
     end}.

%%------------------------------------------------------------------
%% The renew MFA
%%------------------------------------------------------------------

%% Called by the pool's renewal worker as `renewal(Pool, Test, Tag)': asks the
%% test for this renewal's answer.
renewal(_Pool, Test, Tag) ->
    Test ! {renewal_asked, Tag, self()},
    receive
        {renewal_answer, crash} -> exit(renewal_crashed);
        {renewal_answer, Answer} -> Answer
    end.

renew_mfa() ->
    {?MODULE, renewal, [self(), renewal]}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

connected_pool() ->
    {Pool, [Link]} = connected_pool(?SEEDS),
    {Pool, Link}.

%% A pool whose links are each connected, with this process as their peer, to
%% the station their seed pins.
connected_pool(Seeds) ->
    flushed(),
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Pool} = macula_client:connect(Seeds, maps:put(node_identity, Key, ?OPTS)),
    {ok, Links} = macula_client:links(Pool),
    Pids = [connected(Pid, maps:get(expected_node_id, Seed))
            || #{pid := Pid, seed := Seed} <- Links],
    {Pool, [P || Seed <- Seeds, {P, S} <- Pids, S =:= maps:get(expected_node_id, Seed)]}.

connected(Link, Station) ->
    Peer = self(),
    _ = sys:replace_state(Link, fun(S) ->
            setelement(macula_station_link:state_field_index(peer_pid), S, Peer)
        end),
    Link ! {macula_peering, connected, Peer, Station},
    Station = element(macula_station_link:state_field_index(peer_node_id), sys:get_state(Link)),
    {Link, Station}.

%% The pool holds no live link `Old' any more, and has not respawned it yet.
link_gone(_Pool, _Old, 0) ->
    error(link_still_listed);
link_gone(Pool, Old, Tries) ->
    {ok, Links} = macula_client:links(Pool),
    still_listed(lists:member(Old, [P || #{pid := P} <- Links]), Pool, Old, Tries).

still_listed(false, _Pool, _Old, _Tries) -> ok;
still_listed(true, Pool, Old, Tries) -> timer:sleep(10), link_gone(Pool, Old, Tries - 1).

advertise(Pool, Spec) ->
    macula_client:advertise(Pool, ?REALM, ?PROCEDURE, fun(_) -> {ok, counted} end, open, Spec, all,
                            renew_mfa()).

%% A chain spec ending `LifeMs' from now, whose delegation's wire form is
%% `Delegation', which the ADVERTISE frames carry as they are.
spec(LifeMs, Delegation) ->
    #{authorization => #{org_directory => <<"org directory wire">>, procedure_delegation => Delegation},
      not_after => erlang:system_time(millisecond) + LifeMs}.

%% The next renewal the pool asks for within `Ms', and how long it took.
asked_within(Ms) ->
    Start = erlang:monotonic_time(millisecond),
    receive
        {renewal_asked, renewal, Worker} -> {Worker, erlang:monotonic_time(millisecond) - Start}
    after Ms -> error(no_renewal_asked)
    end.

asked_within_or_none(Ms) ->
    receive {renewal_asked, renewal, Worker} -> Worker after Ms -> none end.

%% Answer every renewal with `{error, Reason}' until the pool logs the error
%% naming `Procedure' and `Reason'; when it did, in wall-clock milliseconds (the
%% event's own time), or `false' after `Ms'.
answered_until_logged(Procedure, Reason, Ms) ->
    receive
        {renewal_asked, renewal, Worker} ->
            Worker ! {renewal_answer, {error, Reason}},
            answered_until_logged(Procedure, Reason, Ms);
        {logged_error, #{msg := Msg, meta := #{time := TimeUs} = Meta}} ->
            Text = iolist_to_binary(format(Msg, Meta)),
            answered_logged(binary:match(Text, Procedure) =/= nomatch
                            andalso binary:match(Text, iolist_to_binary(io_lib:format("~0p", [Reason]))) =/= nomatch,
                            TimeUs, Procedure, Reason, Ms)
    after Ms -> false
    end.

answered_logged(true, TimeUs, _Procedure, _Reason, _Ms) -> TimeUs div 1000;
answered_logged(false, _TimeUs, Procedure, Reason, Ms) -> answered_until_logged(Procedure, Reason, Ms).

%% The delegations carried by the next `N' ADVERTISE frames sent to this
%% process, each within a second; liveness probes are skipped.
sent_delegations(0) -> [];
sent_delegations(N) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := advertise, advertisement := Encoded}}} ->
            [delegation_of(Encoded) | sent_delegations(N - 1)]
    after 1_000 -> []
    end.

%% A test starts from an empty mailbox: frames an earlier test's links sent
%% here are not this test's.
flushed() ->
    receive _ -> flushed() after 0 -> ok end.

%% The delegations of every ADVERTISE frame sent here so far, and within the
%% next `Ms' of silence, oldest first.
delegations_within(Ms) ->
    delegations_within(Ms, []).

delegations_within(Ms, Acc) ->
    receive
        {'$gen_cast', {send_frame, #{frame_type := advertise, advertisement := Encoded}}} ->
            delegations_within(Ms, [delegation_of(Encoded) | Acc])
    after Ms -> lists:reverse(Acc)
    end.

delegation_of(Encoded) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Record} = macula_record:verify(Encoded, Profile),
    #{authorization := #{procedure_delegation := Delegation}} = macula_record:read_procedure_advertisement(Record),
    Delegation.

%% The spec the pool holds for the procedure, or `none'.
registered_ad(Pool) ->
    Procs = element(macula_client:state_field_index(procs), sys:get_state(Pool)),
    maps:get(ad, maps:get({?REALM, ?PROCEDURE}, Procs, #{}), none).

capture_log() ->
    Handler = list_to_atom("macula_renewal_capture_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Handler, ?MODULE, #{config => #{test => self()}, level => error,
                                               filter_default => log}),
    Handler.

log(#{level := error} = Event, #{config := #{test := Test}}) ->
    Test ! {logged_error, Event};
log(_Event, _Config) ->
    ok.

format({Format, Args}, _Meta) when is_list(Format) -> io_lib:format(Format, Args);
format({string, String}, _Meta) -> String;
format({report, Report}, _Meta) -> io_lib:format("~p", [Report]).
