%% EUnit tests for the pool's keys, identity migration step 3 on the pool side. A pool holds, in the node's crypto
%% profile, one node identity key that every link shares, and a statement issuer of its own under
%% macula_statement_issuer_sup that holds the pool's CONNECT key. Every link holds that key, that profile and that
%% issuer, and no classical identity. The issuer ends with its pool, and a pool whose issuer ends starts a new one and
%% respawns its links with it. The links are real: their state is read by field name, and their seeds name node_ids
%% nothing answers for, so nothing connects.
-module(macula_client_pool_keys_tests).

-include_lib("eunit/include/eunit.hrl").

%% The key loaders a child spec in these tests names, the logger handler callback that captures reports, and the
%% callback of the supervisor a test starts.
-export([child_key/0, raising_loader/1, log/2, init/1]).

%% A pq_hybrid key carries an RSA-4096 half, which takes up to about a second to generate.
-define(EU_TIMEOUT, 120).
%% The primary logger filter the macula application installs to redact keys.
-define(KEY_REDACTION, macula_key_redaction).
%% The issuer restart backoff reaches 5 s, and a link respawns a second after it ends.
-define(RESPAWN_MS, 15_000).

%%------------------------------------------------------------------
%% What every link holds
%%------------------------------------------------------------------

link_keys_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun started_pool/0, fun stop_pool/1, fun link_cases/1}}.

link_cases(#{profile := Profile, pool := Pool}) ->
    [{"every link holds the node's crypto profile",
      ?_assertEqual([Profile, Profile], [held(Link, profile) || Link <- links(Pool)])},
     {"every link holds an identity key in that profile",
      ?_assertMatch([#{purpose := identity, profile := Profile}, #{purpose := identity, profile := Profile}],
                    [held(Link, node_identity) || Link <- links(Pool)])},
     {"every link holds a statement issuer under macula_statement_issuer_sup",
      ?_test([?assert(lists:member(held(Link, issuer), issuers())) || Link <- links(Pool)])},
     {"the links share one identity key and one issuer",
      ?_test(begin
                 [First, Second] = links(Pool),
                 ?assertEqual({held(First, node_identity), held(First, issuer)},
                              {held(Second, node_identity), held(Second, issuer)})
             end)},
     {"the issuer's CONNECT key is in that profile and shares no half with the identity key",
      ?_test(assert_separate_keys(Profile, hd(links(Pool))))},
     {"no link holds a classical identity",
      ?_assertError(function_clause, macula_station_link:state_field_index(identity))},
     {"no value in a link's state, at any depth, is a classical key pair",
      ?_assertEqual([], [Pair || Link <- links(Pool), Pair <- key_pairs(sys:get_state(Link))])}].

assert_separate_keys(Profile, Link) ->
    #{components := IdentityHalves} = held(Link, node_identity),
    #{connect_key := #{purpose := connect, profile := Profile, components := ConnectHalves}} =
        macula_statement_issuer:connect_material(held(Link, issuer)),
    ConnectPublics = [Public || #{public := Public} <- ConnectHalves],
    ?assertEqual([], [Public || #{public := Public} <- IdentityHalves, lists:member(Public, ConnectPublics)]).

%%------------------------------------------------------------------
%% The pool's issuer
%%------------------------------------------------------------------

an_issuer_ends_with_its_pool_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Before = issuers(),
        {ok, Pool} = macula_client:connect([], #{}),
        [Issuer] = issuers() -- Before,
        Ref = erlang:monitor(process, Issuer),
        ok = macula_client:close(Pool),
        ?assertEqual(normal, receive {'DOWN', Ref, process, Issuer, Reason} -> Reason after 5000 -> still_running end)
    end}.

two_pools_have_two_issuers_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Before = issuers(),
        {ok, Pool1} = macula_client:connect([], #{}),
        {ok, Pool2} = macula_client:connect([], #{}),
        New = issuers() -- Before,
        ok = macula_client:close(Pool1),
        ok = macula_client:close(Pool2),
        ?assertEqual(2, length(New))
    end}.

%% The pool starts a new issuer once its issuer ends, and only then its links again: every link that runs
%% afterwards holds the new issuer, and none holds the one that ended.
a_pool_whose_issuer_ends_respawns_its_links_with_a_new_issuer_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        {ok, Pool} = macula_client:connect([seed(1)], #{}),
        [Link] = links(Pool),
        Ended = held(Link, issuer),
        exit(Ended, kill),
        Respawned = respawned(Pool, Link, erlang:monotonic_time(millisecond) + ?RESPAWN_MS),
        New = held(Respawned, issuer),
        Seen = {New =/= Ended, lists:member(New, issuers()), is_process_alive(Ended)},
        ok = macula_client:close(Pool),
        ?assertEqual({true, true, false}, Seen)
    end}.

%% While the pool's issuer cannot start again, a link start waits for the next one and counts once however long it
%% waits, and the link then starts with the issuer that runs.
a_held_link_start_counts_once_and_starts_with_the_next_issuer_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Gate = ets:new(issuer_gate, [public]),
        true = ets:insert(Gate, {open, true}),
        Start = fun(Identity, Owner) -> gated_start(ets:lookup_element(Gate, open, 2), Identity, Owner) end,
        {ok, Pool} = macula_client:connect([seed(1)], #{issuer_start => Start}),
        [Link] = links(Pool),
        true = ets:insert(Gate, {open, false}),
        exit(held(Link, issuer), kill),
        Waited = until(fun() -> waits_for_issuer(Pool) =:= 1 end, ?RESPAWN_MS),
        receive after 2_000 -> ok end,
        Held = {Waited, waits_for_issuer(Pool), links(Pool)},
        true = ets:insert(Gate, {open, true}),
        Respawned = respawned(Pool, Link, erlang:monotonic_time(millisecond) + ?RESPAWN_MS),
        Resumed = {lists:member(held(Respawned, issuer), issuers()), waits_for_issuer(Pool)},
        ok = macula_client:close(Pool),
        ?assertEqual({ok, 1, []}, Held),
        ?assertEqual({true, 1}, Resumed)
    end}.

%% A pool whose issuer ends starts a new one and counts the restart in its status. The issuer_down log line is bounded
%% to one for each window, node-wide, so the count and not the line is what a test reads.
an_issuer_restart_is_counted_in_the_status_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Before = issuers(),
        {ok, Pool} = macula_client:connect([], #{}),
        {ok, #{issuer_restarts := Initially, issuer_losses := InitiallyLost}} = macula_client:status(Pool),
        [Issuer] = issuers() -- Before,
        exit(Issuer, kill),
        Restarted = until(fun() -> issuer_restarts(Pool) =:= 1 end, ?RESPAWN_MS),
        {ok, #{issuer_losses := Lost}} = macula_client:status(Pool),
        Running = issuers() -- Before,
        ok = macula_client:close(Pool),
        ?assertEqual({0, 0, ok, 1}, {Initially, InitiallyLost, Restarted, Lost}),
        ?assertMatch([New] when New =/= Issuer, Running)
    end}.

%% A restart_issuer the pool receives while its issuer runs starts no second issuer.
a_restart_while_the_issuer_runs_starts_no_issuer_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        Before = issuers(),
        {ok, Pool} = macula_client:connect([], #{}),
        Started = issuers() -- Before,
        Pool ! restart_issuer,
        _ = sys:get_state(Pool),
        After = issuers() -- Before,
        ok = macula_client:close(Pool),
        ?assertEqual(Started, After)
    end}.

the_issuer_restart_backoff_doubles_from_100_ms_to_5_s_and_resets_after_a_minute_test() ->
    Backoffs = lists:foldl(fun(_, [Last | _] = Acc) -> [macula_client:next_issuer_backoff(Last) | Acc] end,
                           [100], lists:seq(1, 7)),
    ?assertEqual([100, 200, 400, 800, 1600, 3200, 5000, 5000], lists:reverse(Backoffs)),
    ?assertEqual(5000, macula_client:issuer_restart_delay(59_999, 5000)),
    ?assertEqual(100, macula_client:issuer_restart_delay(60_000, 5000)).

%%------------------------------------------------------------------
%% A child spec loads the key
%%------------------------------------------------------------------

%% A child spec holds how to load the node identity key, never the key: a supervisor keeps the spec for its child's
%% life, and a function holding the key would keep the key there too.
a_child_spec_holds_a_loader_and_not_the_key_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        ok = persistent_term:put({?MODULE, child_key}, Key),
        Spec = macula_client:child_spec(pool, [], #{node_identity => {?MODULE, child_key, []}}),
        #{start := {Module, Function, Args}} = Spec,
        {ok, Pool} = apply(Module, Function, Args),
        {ok, #{self_node_id := NodeId}} = macula_client:status(Pool),
        ok = macula_client:close(Pool),
        _ = persistent_term:erase({?MODULE, child_key}),
        ?assertEqual({ok, NodeId}, macula_node_keys:node_id(Key)),
        ?assertEqual([], [Private || #{private := Private} <- maps:get(components, Key),
                                      binary:match(term_to_binary(Spec), Private) =/= nomatch])
    end}.

%% A child spec given the key, or a function that can hold the key, is refused: only a loader names how to get it.
a_child_spec_given_a_key_or_a_function_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        ?assertError({node_identity, loader_required}, macula_client:child_spec(pool, [], #{node_identity => Key})),
        ?assertError({node_identity, loader_required},
                     macula_client:child_spec(pool, [], #{node_identity => fun() -> Key end}))
    end}.

%% A loader that raises refuses the pool by name. The start leaves no key in its result or in any report, even when
%% the loader's error carries one.
a_loader_that_raises_refuses_the_pool_by_name_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        process_flag(trap_exit, true),
        Test = self(),
        Loader = {erlang, error, [{no_key_here, Key}]},
        Events = captured(fun() -> Test ! {started, catch macula_client:connect([], #{node_identity => Loader})} end),
        Started = receive {started, Result} -> Result after 0 -> no_result end,
        ?assertEqual({error, {node_identity, loader_failed}}, Started),
        ?assertEqual([], [Private || #{private := Private} <- maps:get(components, Key),
                                      binary:match(term_to_binary({Started, Events}), Private) =/= nomatch])
    end}.

%% A supervisor that fails to start a pool whose loader raises logs a start error report. The report names the child
%% spec, whose loader Args say where the key is, and holds no private half of the key, though the loader's error
%% carries it. The capture runs without macula's redaction filter, so only the pool's refusal keeps the key out.
a_supervised_start_whose_loader_raises_logs_no_key_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        ok = persistent_term:put({?MODULE, raising_key}, Key),
        Spec = macula_client:child_spec(pool, [], #{node_identity => {?MODULE, raising_loader, [{?MODULE, raising_key}]}}),
        Trapping = process_flag(trap_exit, true),
        Start = fun() -> supervisor:start_link(?MODULE, {pool, Spec}) end,
        {Redaction, Events} = unredacted(fun() -> captured(Start) end),
        receive {'EXIT', _Supervisor, _Reason} -> ok after 0 -> ok end,
        _ = process_flag(trap_exit, Trapping),
        _ = persistent_term:erase({?MODULE, raising_key}),
        ?assertMatch([_], Redaction),
        ?assertMatch([_ | _], [Report || #{msg := {report, #{label := {supervisor, start_error}} = Report}} <- Events]),
        ?assertEqual([], [Private || #{private := Private} <- maps:get(components, Key),
                                      binary:match(term_to_binary(Events), Private) =/= nomatch])
    end}.

%%------------------------------------------------------------------
%% The pool signs records about itself
%%------------------------------------------------------------------

%% A pool signs a record a node signs about itself with its node identity key, in its own process, and returns only the
%% signed record, stamped now with a new version and the lifetime it was built with. It refuses by name a record that
%% names another node, one that lives past its type's maximum or runs backwards, a type a node does not sign about
%% itself (a station endpoint, realm- and org-signed types, a domain type, a tombstone), a payload past the record
%% bound, one within it whose signed record would pass that bound, and a record it cannot sign, and it keeps
%% answering. A tombstone comes only from withdraw_node_record/3, for a verified record this node signed: another
%% node's record, a realm- or org-signed record, a tombstone, an expired record and a tampered record are each refused
%% by name. No reply, and no log event captured with macula's redaction filter removed, holds a private half.
a_pool_signs_only_records_about_itself_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        {ok, Other} = macula_node_keys:generate(identity, Profile),
        {ok, Realm} = macula_node_keys:generate(realm, Profile),
        {ok, Org} = macula_node_keys:generate(org, Profile),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        Minute = 60_000,
        Hour = 3_600_000,
        Advertisement = fun(Advertiser) ->
                            macula_record:procedure_advertisement(Advertiser, <<1:256>>, <<"acme/echo_v1">>, <<7:256>>)
                        end,
        Endorsement = macula_record:realm_member_endorsement(<<1:256>>, #{realm => <<1:256>>, member_node => NodeId,
                                                                          roles => []}),
        OthersAdvertisement = macula_record:sign(Advertisement(macula_node_keys:key_id(Other)), Other),
        Delegation = macula_record:sign(macula_record:procedure_delegation(macula_node_keys:key_id(Org), NodeId), Org),
        Reversed = (macula_record:node_record(NodeId, [], 0))#{created_at := 2_000, expires_at := 1_000},
        %% Its payload passes payload_bounded/1, and the key and signature take the signed record past 256 KiB.
        NearBound = macula_record:node_record(NodeId, [], 0, #{hostname => binary:copy(<<"h">>, 256 * 1024 - 4096)}),
        Refused = [{key_id_mismatch, macula_record:node_record(<<7:256>>, [], 0)},
                   {lifetime_too_long, macula_record:node_record(NodeId, [], 0, #{ttl_ms => 365 * 24 * Hour})},
                   {lifetime_reversed, Reversed},
                   {not_a_node_signed_type, macula_record:station_endpoint(4433)},
                   {not_a_node_signed_type, Endorsement},
                   {not_a_node_signed_type, macula_record:org_directory(<<1:256>>, <<"acme">>, <<9:256>>)},
                   {not_a_node_signed_type, macula_record:procedure_delegation(<<9:256>>, NodeId)},
                   {not_a_node_signed_type, macula_record:envelope(16#20, #{}, #{})},
                   {not_a_node_signed_type, macula_record:tombstone(OthersAdvertisement, shutdown)},
                   {record_too_large,
                    macula_record:node_record(NodeId, [], 0, #{hostname => binary:copy(<<"h">>, 300 * 1024)})},
                   {record_too_large, NearBound},
                   {malformed_record, #{type => 1, payload => #{{text, <<"node_id">>} => NodeId}}}],
        Old = (macula_record:node_record(NodeId, [], 0))#{created_at := 1_000, expires_at := 1_000 + Hour},
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        Test = self(),
        Run = fun() ->
                  Before = erlang:system_time(millisecond),
                  Signed = macula_client:sign_node_record(Pool, Old),
                  Replies = [{Name, macula_client:sign_node_record(Pool, Unsigned)} || {Name, Unsigned} <- Refused],
                  {ok, Own} = macula_client:sign_node_record(Pool, Advertisement(NodeId)),
                  <<First, Rest/binary>> = maps:get(signature, Own),
                  Tampered = Own#{signature := <<(First bxor 1), Rest/binary>>},
                  {ok, Tombstone} = Withdrawn = macula_client:withdraw_node_record(Pool, Own, shutdown),
                  Expired = macula_record:sign((Advertisement(NodeId))#{created_at := Before - 20 * Minute,
                                                                        expires_at := Before - 15 * Minute}, Key),
                  Withdrawals = [macula_client:withdraw_node_record(Pool, Withdrawable, shutdown)
                                 || Withdrawable <- [OthersAdvertisement, macula_record:sign(Endorsement, Realm),
                                                     Delegation, Tombstone, Expired, Tampered]],
                  Test ! {ran, Before, Signed, Replies, Withdrawn, Withdrawals, macula_client:status(Pool)}
              end,
        {_Redaction, Events} = unredacted(fun() -> captured(Run) end),
        ok = macula_client:close(Pool),
        {Before, Signed, Replies, Withdrawn, Withdrawals, Status} =
            receive {ran, B, S, R, W, Ws, St} -> {B, S, R, W, Ws, St} after 0 -> erlang:error(no_run) end,
        ?assertMatch({ok, #{self_node_id := NodeId}}, Status),
        ?assertMatch({ok, #{key_id := NodeId}}, Signed),
        {ok, Record} = Signed,
        ?assert(macula_record:created_at(Record) >= Before),
        ?assertEqual(Hour, macula_record:expires_at(Record) - macula_record:created_at(Record)),
        ?assertNotEqual(macula_record:version(Old), macula_record:version(Record)),
        ?assertMatch({ok, _}, macula_record:verify(macula_record:encode(Record), Profile)),
        ?assertEqual([{Name, {error, Name}} || {Name, _} <- Refused], Replies),
        ?assertEqual(ok, macula_record:payload_bounded(macula_record:payload(NearBound))),
        ?assertMatch({ok, #{type := 16#0C, key_id := NodeId}}, Withdrawn),
        ?assertEqual([{error, not_this_nodes_record}, {error, not_a_node_signed_type}, {error, not_a_node_signed_type},
                      {error, not_a_node_signed_type}, {error, expired}, {error, signature_invalid}],
                     Withdrawals),
        ?assertEqual([], [Private || #{private := Private} <- maps:get(components, Key),
                                      binary:match(term_to_binary({Signed, Replies, Withdrawn, Withdrawals, Events}),
                                                   Private) =/= nomatch])
    end}.

%% Nothing past the record bound reaches the pool to be encoded there. sign_node_record/2 refuses a record with a
%% subject, which no type a node signs about itself carries, and withdraw_node_record/3 refuses a wire form over
%% 256 KiB, a signed map whose key, tbs and signature pass 256 KiB together, one whose tbs is not a binary or that has
%% no signature, and anything else. Each refusal comes back while the pool is suspended, so it is made before the
%% call, and the pool answers its status after.
nothing_past_the_record_bound_reaches_the_pool_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        Node = macula_record:node_record(NodeId, [], 0),
        #{tbs := Tbs} = Signed = macula_record:sign(Node, Key),
        Over = binary:copy(<<"s">>, 256 * 1024),
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        ok = sys:suspend(Pool),
        Replies = try [macula_client:sign_node_record(Pool, Node#{subject => Over}),
                       macula_client:sign_node_record(Pool, Node#{subject => <<"s">>}),
                       macula_client:withdraw_node_record(Pool, <<(macula_record:encode(Signed))/binary, Over/binary>>,
                                                          shutdown),
                       macula_client:withdraw_node_record(Pool, Signed#{tbs := <<Tbs/binary, Over/binary>>}, shutdown),
                       macula_client:withdraw_node_record(Pool, Signed#{tbs := binary_to_list(Tbs)}, shutdown),
                       macula_client:withdraw_node_record(Pool, maps:remove(signature, Signed), shutdown),
                       macula_client:withdraw_node_record(Pool, not_a_record, shutdown)]
                  after ok = sys:resume(Pool)
                  end,
        Status = macula_client:status(Pool),
        ok = macula_client:close(Pool),
        ?assertEqual([{error, malformed_record}, {error, malformed_record}, {error, record_too_large},
                      {error, record_too_large}, {error, malformed_record}, {error, malformed_record},
                      {error, malformed_record}], Replies),
        ?assertMatch({ok, #{self_node_id := NodeId}}, Status)
    end}.

%% Of a record to sign, the pool is sent only its type, created_at, expires_at and payload, and of a signed record to
%% withdraw only its key, tbs and signature: a receive trace on the pool shows each request as it arrives.
the_pool_is_sent_only_the_fields_it_signs_from_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        Signed = macula_record:sign(macula_record:node_record(NodeId, [], 0), Key),
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        1 = erlang:trace(Pool, true, ['receive']),
        Replies = [macula_client:sign_node_record(Pool, Signed),
                   macula_client:withdraw_node_record(Pool, Signed, shutdown)],
        1 = erlang:trace(Pool, false, ['receive']),
        Delivered = erlang:trace_delivered(Pool),
        receive {trace_delivered, Pool, Delivered} -> ok end,
        Requests = [Request || {trace, _Pid, 'receive', {'$gen_call', _From, Request}} <- traced(Pool, [])],
        ok = macula_client:close(Pool),
        ?assertMatch([{ok, #{key_id := NodeId}}, {ok, #{type := 16#0C}}], Replies),
        ?assertEqual([{sign_node_record, maps:with([type, created_at, expires_at, payload], Signed)},
                      {withdraw_node_record, maps:with([key, tbs, signature], Signed), shutdown}], Requests)
    end}.

%% Every record this node signed that verifies can be withdrawn: one created four minutes ahead, within the clock
%% tolerance, at its type's maximum lifetime gets a tombstone that verifies.
a_record_created_ahead_at_its_maximum_lifetime_is_withdrawn_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        Minute = 60_000,
        Now = erlang:system_time(millisecond),
        Advertisement = macula_record:procedure_advertisement(NodeId, <<1:256>>, <<"acme/echo_v1">>, <<7:256>>),
        Ahead = macula_record:sign(Advertisement#{created_at := Now + 4 * Minute, expires_at := Now + 9 * Minute}, Key),
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        Withdrawn = macula_client:withdraw_node_record(Pool, Ahead, shutdown),
        ok = macula_client:close(Pool),
        ?assertMatch({ok, #{type := 16#0C, key_id := NodeId}}, Withdrawn),
        {ok, Tombstone} = Withdrawn,
        ?assertMatch({ok, #{type := 16#0C}}, macula_record:verify(macula_record:encode(Tombstone), Profile))
    end}.

%% A pool signs a domain record as its node. Each refusal comes back while the pool is suspended, so before the call: a
%% type outside 0x20 to 0xFF, a subject that is not a non-empty binary, a lifetime past the domain maximum of 7 days
%% or running backwards, a payload and subject past 256 KiB together, and a term that is no record. A node record with
%% a subject is refused before the call too, and sign_node_record/2 still refuses a domain type.
a_domain_record_is_refused_before_the_call_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        Day = 86_400_000,
        #{created_at := Created} = Domain = macula_record:envelope(16#20, #{}, #{}),
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        ok = sys:suspend(Pool),
        Replies = try
                      [macula_client:sign_domain_record(Pool, macula_record:node_record(NodeId, [], 0)),
                       macula_client:sign_domain_record(Pool, Domain#{subject => not_a_binary}),
                       macula_client:sign_domain_record(Pool, Domain#{subject => <<>>}),
                       macula_client:sign_domain_record(Pool, Domain#{expires_at := Created + 7 * Day + 1}),
                       macula_client:sign_domain_record(Pool, Domain#{expires_at := Created}),
                       macula_client:sign_domain_record(Pool, Domain#{subject => binary:copy(<<"s">>, 256 * 1024)}),
                       macula_client:sign_domain_record(Pool, not_a_record),
                       macula_client:sign_node_record(Pool, Domain#{subject => <<"s">>})]
                  after ok = sys:resume(Pool)
                  end,
        NotNodeSigned = macula_client:sign_node_record(Pool, Domain),
        Status = macula_client:status(Pool),
        ok = macula_client:close(Pool),
        ?assertEqual([{error, not_a_domain_type}, {error, invalid_subject}, {error, invalid_subject},
                      {error, lifetime_too_long}, {error, lifetime_reversed}, {error, record_too_large},
                      {error, malformed_record}, {error, malformed_record}], Replies),
        ?assertEqual({error, not_a_node_signed_type}, NotNodeSigned),
        ?assertMatch({ok, #{self_node_id := NodeId}}, Status)
    end}.

%% Of a domain record to sign, the pool is sent only its type, created_at, expires_at, payload and subject: a receive
%% trace on the pool shows the request as it arrives.
the_pool_is_sent_only_the_fields_of_a_domain_record_it_signs_from_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        Built = macula_record:envelope(16#20, #{}, #{subject_id => <<"s1">>}),
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        1 = erlang:trace(Pool, true, ['receive']),
        Signed = macula_client:sign_domain_record(Pool, Built#{an_extra_field => 1}),
        1 = erlang:trace(Pool, false, ['receive']),
        Delivered = erlang:trace_delivered(Pool),
        receive {trace_delivered, Pool, Delivered} -> ok end,
        Requests = [Request || {trace, _Pid, 'receive', {'$gen_call', _From, Request}} <- traced(Pool, [])],
        ok = macula_client:close(Pool),
        ?assertMatch({ok, #{type := 16#20, subject := <<"s1">>}}, Signed),
        ?assertEqual([{sign_domain_record, maps:with([type, created_at, expires_at, payload, subject], Built)}],
                     Requests)
    end}.

%% A domain record lives at most its type's maximum of 7 days: one at 7 days signs as this node and verifies, one a
%% millisecond longer is refused and never shortened, and one built with no ttl lives within the maximum. Its key id is
%% the key id of the pool's key as carried, which is not the node_id a node record names.
a_domain_record_lives_within_its_type_maximum_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        Day = 86_400_000,
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        AtMaximum = macula_client:sign_domain_record(Pool, macula_record:envelope(16#20, #{}, #{ttl_ms => 7 * Day})),
        Longer = macula_client:sign_domain_record(Pool, macula_record:envelope(16#20, #{}, #{ttl_ms => 7 * Day + 1})),
        Default = macula_client:sign_domain_record(Pool, macula_record:envelope(16#21, #{}, #{})),
        ok = macula_client:close(Pool),
        {ok, #{key_id := DomainKeyId} = Signed} = AtMaximum,
        ?assertEqual({macula_node_keys:key_id(macula_node_keys:public_key(Key), Profile), true},
                     {DomainKeyId, DomainKeyId =/= NodeId}),
        ?assertEqual(7 * Day, macula_record:expires_at(Signed) - macula_record:created_at(Signed)),
        ?assertMatch({ok, _}, macula_record:verify(macula_record:encode(Signed), Profile)),
        ?assertEqual({error, lifetime_too_long}, Longer),
        {ok, DefaultSigned} = Default,
        ?assert(macula_record:expires_at(DefaultSigned) - macula_record:created_at(DefaultSigned) =< 7 * Day)
    end}.

%% A domain record this node signed is withdrawn through the pool on its own slot: the tombstones of a domain record
%% with a subject and of one without each share their record's storage key, the two records and the node record hold
%% three different slots, and all four records verify. Each tombstone outlives its record by the clock tolerance, within
%% the domain maximum plus twice that tolerance.
a_domain_record_is_withdrawn_on_its_own_slot_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        Day = 86_400_000,
        Minute = 60_000,
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        {ok, WithSubject} = macula_client:sign_domain_record(
                              Pool, macula_record:envelope(16#20, #{}, #{subject_id => <<"s1">>, ttl_ms => 7 * Day})),
        {ok, Bare} = macula_client:sign_domain_record(Pool, macula_record:envelope(16#20, #{}, #{ttl_ms => 7 * Day})),
        {ok, Node} = macula_client:sign_node_record(Pool, macula_record:node_record(NodeId, [], 0)),
        {ok, SubjectTombstone} = macula_client:withdraw_node_record(Pool, WithSubject, shutdown),
        {ok, BareTombstone} = macula_client:withdraw_node_record(Pool, Bare, shutdown),
        ok = macula_client:close(Pool),
        Slot = fun macula_record:storage_key/1,
        ?assertEqual({Slot(WithSubject), Slot(Bare)}, {Slot(SubjectTombstone), Slot(BareTombstone)}),
        ?assertEqual(3, length(lists:usort([Slot(WithSubject), Slot(Bare), Slot(Node)]))),
        ?assertEqual([], [Record || Record <- [WithSubject, Bare, SubjectTombstone, BareTombstone],
                                    element(1, macula_record:verify(macula_record:encode(Record), Profile)) =/= ok]),
        [?assertEqual(macula_record:expires_at(Withdrawn) + 5 * Minute, macula_record:expires_at(Tombstone))
         || {Withdrawn, Tombstone} <- [{WithSubject, SubjectTombstone}, {Bare, BareTombstone}]],
        ?assert(macula_record:expires_at(BareTombstone) - macula_record:created_at(BareTombstone)
                =< 7 * Day + 10 * Minute)
    end}.

%%------------------------------------------------------------------
%% The node identity key
%%------------------------------------------------------------------

a_generated_identity_key_meets_the_puzzle_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, _Profile} = profile(),
        {ok, Pool} = macula_client:connect([], #{}),
        {ok, #{self_node_id := NodeId}} = macula_client:status(Pool),
        ok = macula_client:close(Pool),
        ?assert(macula_node_keys:puzzle_solved(NodeId, macula_node_keys:puzzle_difficulty()))
    end}.

a_supplied_identity_key_is_used_as_it_is_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(identity, Profile),
        {ok, NodeId} = macula_node_keys:node_id(Key),
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        {ok, #{self_node_id := SelfNodeId}} = macula_client:status(Pool),
        ok = macula_client:close(Pool),
        ?assertEqual(NodeId, SelfNodeId)
    end}.

an_identity_key_in_another_profile_is_refused_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        [Other] = macula_crypto_profile:profiles() -- [Profile],
        {ok, Key} = macula_node_keys:generate(identity, Other),
        ?assertEqual({error, {node_identity, {wrong_profile, Other}}},
                     macula_client:connect([], #{node_identity => Key}))
    end}.

a_key_of_another_purpose_is_refused_as_the_identity_key_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        {ok, Profile} = profile(),
        {ok, Key} = macula_node_keys:generate(connect, Profile),
        ?assertEqual({error, {node_identity, not_an_identity_key}},
                     macula_client:connect([], #{node_identity => Key}))
    end}.

%%------------------------------------------------------------------
%% The node's crypto profile
%%------------------------------------------------------------------

a_node_without_a_crypto_profile_starts_no_pool_test_() ->
    {setup, fun profile/0, fun restore_profile/1,
     fun(_) ->
         ?_test(begin
                    ok = application:unset_env(macula, crypto_profile),
                    ?assertEqual({error, crypto_profile_missing}, macula_client:connect([], #{}))
                end)
     end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

profile() ->
    {ok, _} = application:ensure_all_started(macula),
    macula_crypto_profile:configured().

restore_profile({ok, Profile}) ->
    ok = application:set_env(macula, crypto_profile, Profile).

%% The statement issuers running under macula_statement_issuer_sup.
issuers() ->
    [Pid || {_Id, Pid, _Type, _Modules} <- supervisor:which_children(macula_statement_issuer_sup), is_pid(Pid)].

%% A pool of two links whose seeds name node_ids nothing answers for.
started_pool() ->
    {ok, Profile} = profile(),
    {ok, Pool} = macula_client:connect([seed(1), seed(2)], #{}),
    #{profile => Profile, pool => Pool}.

stop_pool(#{pool := Pool}) ->
    ok = macula_client:close(Pool).

seed(Port) ->
    #{host => <<"127.0.0.1">>, port => Port, expected_node_id => crypto:strong_rand_bytes(32)}.

%% The pids of the pool's links that run.
links(Pool) ->
    {ok, Infos} = macula_client:links(Pool),
    [Pid || #{pid := Pid} <- Infos, is_pid(Pid)].

%% A field of a link's state, read by name.
held(Link, Field) ->
    element(macula_station_link:state_field_index(Field), sys:get_state(Link)).

%% The pool's link that runs in place of Ended, once one does, before Deadline.
respawned(Pool, Ended, Deadline) ->
    respawned_in_time(erlang:monotonic_time(millisecond) < Deadline, links(Pool) -- [Ended], Pool, Ended, Deadline).

respawned_in_time(_InTime, [Link], _Pool, _Ended, _Deadline) ->
    Link;
respawned_in_time(true, [], Pool, Ended, Deadline) ->
    receive after 100 -> ok end,
    respawned(Pool, Ended, Deadline);
respawned_in_time(false, Links, _Pool, _Ended, _Deadline) ->
    erlang:error({no_respawned_link, Links}).

%% An issuer start that goes to macula_statement_issuer_sup while the gate is open, and is refused while it is shut.
gated_start(true, Identity, Owner) -> macula_statement_issuer_sup:start_issuer(Identity, Owner);
gated_start(false, _Identity, _Owner) -> {error, gate_shut}.

%% The issuers a pool has started after its first, as its status counts them.
issuer_restarts(Pool) ->
    {ok, #{issuer_restarts := Restarts}} = macula_client:status(Pool),
    Restarts.

%% The link starts of a pool that waited for an issuer, as its status counts them.
waits_for_issuer(Pool) ->
    {ok, #{refused_dials := Refused}} = macula_client:status(Pool),
    maps:get(link_start_waits_for_issuer, Refused, 0).

%% ok once Check holds, checked every 50 ms, or {error, timeout} after TimeoutMs.
until(Check, TimeoutMs) ->
    until_by(Check, erlang:monotonic_time(millisecond) + TimeoutMs).

until_by(Check, Deadline) ->
    checked(Check(), erlang:monotonic_time(millisecond) >= Deadline, Check, Deadline).

checked(true, _Late, _Check, _Deadline) -> ok;
checked(false, true, _Check, _Deadline) -> {error, timeout};
checked(false, false, Check, Deadline) -> receive after 50 -> ok end, until_by(Check, Deadline).

%% Every classical key pair in a term, at any depth: a map of exactly a public and a private half. It walks map values,
%% lists and tuples only, so it misses a pair in a closure's environment or a map key, a {Public, Private} tuple, and
%% bare key bytes.
key_pairs(#{public := _, private := _} = Pair) when map_size(Pair) =:= 2 -> [Pair];
key_pairs(Map) when is_map(Map) -> key_pairs(maps:values(Map));
key_pairs([Head | Tail]) -> key_pairs(Head) ++ key_pairs(Tail);
key_pairs(Tuple) when is_tuple(Tuple) -> key_pairs(tuple_to_list(Tuple));
key_pairs(_Other) -> [].

%% The log events that running Act leaves, caught by a logger handler added for the run.
captured(Act) ->
    Handler = list_to_atom("macula_pool_keys_capture_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Handler, ?MODULE, #{config => #{test => self()}, level => all, filter_default => log}),
    _ = Act(),
    receive after 500 -> ok end,
    ok = logger:remove_handler(Handler),
    drained([]).

log(Event, #{config := #{test := Test}}) ->
    Test ! {captured, Event}.

drained(Events) ->
    receive
        {captured, Event} -> drained([Event | Events])
    after 0 ->
        lists:reverse(Events)
    end.

%% The trace messages about Pid in the mailbox, oldest first.
traced(Pid, Messages) ->
    receive
        {trace, Pid, _Tag, _Detail} = Message -> traced(Pid, [Message | Messages])
    after 0 ->
        lists:reverse(Messages)
    end.

%% The loader the child spec test names: the key it stored.
child_key() ->
    {ok, persistent_term:get({?MODULE, child_key})}.

%% A loader whose Args say where the key is, and whose error carries the key it read.
raising_loader(Name) ->
    erlang:error({key_read, persistent_term:get(Name)}).

%% The supervisor a test starts: the one child spec it is given, never restarted.
init({pool, Spec}) ->
    {ok, {#{strategy => one_for_one, intensity => 0, period => 1}, [Spec]}}.

%% Runs Act without macula's key redaction filter and puts back what it removed. Returns the removed filters and Act's
%% result.
unredacted(Act) ->
    Removed = [Filter || {?KEY_REDACTION, _} = Filter <- maps:get(filters, logger:get_primary_config())],
    _ = logger:remove_primary_filter(?KEY_REDACTION),
    try {Removed, Act()}
    after [ok = logger:add_primary_filter(Id, Filter) || {Id, Filter} <- Removed]
    end.
