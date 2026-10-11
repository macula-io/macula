%% @doc Group distributor scale bench (DESIGN_E2E_SEALED_PUBSUB.md §13 #7):
%% rotation CPU and reply bytes at n members, both crypto profiles, with the
%% REAL default membership path (a realm-signed endorsement per member,
%% verified on every pull through the distributor's injectable lookup seam),
%% plus the publish and open cost per event under a real epoch key.
%%
%% Numbers the register refuses to claim until measured; this prints them.
%% Run through scripts/bench-group-distributor.sh, which compiles this
%% module against a built tree.
-module(bench_group_distributor).

-export([main/1, main/2]).

-define(PREFIX, <<"io.macula/acme/chat">>).
-define(ORG, <<"acme">>).
-define(ROTATE_MS, 15 * 60000).
-define(EVENT_ITERS, 2000).

main(N) ->
    main(N, N).

main(N1, N2) ->
    io:format("otp: ~s~n", [erlang:system_info(otp_release)]),
    io:format("distributor rotation bench: per-pull CPU (membership verify included), reply bytes,~n"
              "per rotation at n members; publish/open per event; both profiles.~n", []),
    [bench(Profile, N) || Profile <- [pq_pure, pq_hybrid], N <- [N1, N2]],
    ok.

bench(Profile, N) ->
    {ok, RealmKey} = macula_node_keys:generate(realm, Profile),
    Realm = crypto:strong_rand_bytes(32),
    RealmKeyId = macula_node_keys:key_id(RealmKey),
    Members = [crypto:strong_rand_bytes(32) || _ <- lists:seq(1, N)],
    %% One realm-signed endorsement per member: the slot the pull's
    %% membership check reads. Setup cost is one-time, reported apart.
    End = ets:new(endorsements, [public, set]),
    SignStart = erlang:monotonic_time(microsecond),
    [ets:insert(End, {macula_record:realm_member_endorsement_key(Realm, M),
                      macula_record:sign(
                        macula_record:realm_member_endorsement(
                          Realm, #{realm => Realm, member_node => M, roles => [<<"peer">>]}),
                        RealmKey)})
     || M <- Members],
    SetupSignMs = (erlang:monotonic_time(microsecond) - SignStart) / 1000,

    Now = counters:new(1, []),
    counters:put(Now, 1, 1_000_000),
    Opts = #{org => ?ORG, policy => required, rotate_after_ms => ?ROTATE_MS,
             now => fun() -> counters:get(Now, 1) end,
             pool => make_ref(),
             lookup => fun(Key) ->
                           case ets:lookup(End, Key) of
                               [{_, Record}] -> {ok, [Record]};
                               [] -> {error, not_found}
                           end
                       end,
             realm => Realm, realm_key_id => RealmKeyId, profile => Profile},
    {ok, Pid} = macula_group_keys:start_link(Opts),
    Handler = macula_group_keys:handler(Pid),
    Pull = fun(M) ->
               Handler(#{{text, <<"prefix">>} => {text, ?PREFIX},
                         {text, <<"epoch">>} => {text, <<"current">>},
                         caller => M})
           end,

    %% Phase A: every member pulls; the first creates the epoch, the rest
    %% read it. Reply carries one epoch.
    {TimesA, BytesA} = timed_pulls(Pull, Members),
    %% Rotation: into the ahead window; the next pull mints the next epoch.
    %% Reply carries two (current and next) until the window closes.
    counters:add(Now, 1, ?ROTATE_MS - ?ROTATE_MS div 3),
    {TimesB, BytesB} = timed_pulls(Pull, Members),

    %% One member's rotation reply, for the publish/open bench's epoch key.
    [{_, ReplyB0} | _] = pull_samples(Pull, Members, 1),
    [Current | _] = maps:get(epochs, ReplyB0),
    {SealUs, OpenUs} = event_cost(Current, Members),

    io:format("~nprofile=~s n=~B~n", [Profile, N]),
    io:format("  setup: sign ~B endorsements in ~.1f ms~n", [N, SetupSignMs]),
    io:format("  pull (current): p50 ~s mean ~s  reply_bytes ~B~n",
              [ms(percentile(TimesA, 50)), ms(mean(TimesA)), round(mean(BytesA))]),
    io:format("  pull (rotation): p50 ~s mean ~s  reply_bytes ~B~n",
              [ms(percentile(TimesB, 50)), ms(mean(TimesB)), round(mean(BytesB))]),
    io:format("  rotation total: ~.1f ms cpu, ~.1f B per member (2-epoch reply)~n",
              [mean(TimesB) * N / 1000, mean(BytesB)]),
    io:format("  publish/open per event (64 B payload): seal p50 ~s, open p50 ~s~n",
              [ms(percentile(SealUs, 50)), ms(percentile(OpenUs, 50))]),

    %% The link-layer costs §6.3 prices a sealed pull at: the signed RESULT
    %% (one ML-DSA sign), the KEM decapsulation of the member's request, and
    %% the UCAN authorize. The handler bench above excludes them by design
    %% (a direct handler call); they are measured here, same machine.
    {ok, ProvKey} = macula_node_keys:generate(identity, Profile),
    ReplyBin = term_to_binary(ReplyB0),
    SignTimes = times(fun() -> macula_node_keys:sign(ReplyBin, ProvKey) end, 1000),
    {PubKem, PrivKem} = crypto:generate_key(mlkem1024, []),
    {_Shared, Ct} = crypto:encapsulate_key(mlkem1024, PubKem),
    DecapTimes = times(fun() -> crypto:decapsulate_key(mlkem1024, PrivKem, Ct) end, 1000),
    {ok, OrgKey} = macula_node_keys:generate(org, Profile),
    UcanRealm = crypto:hash(sha256, <<"test">>),
    MemberNode = hd(Members),
    Token = macula_ucan:create(OrgKey, MemberNode,
                               [#{with => <<"mri:proc:test/acme/x">>, can => <<"group_keys">>}],
                               #{exp => erlang:system_time(second) + 3600}),
    Policy = {realm_member_required, macula_node_keys:key_id(OrgKey), <<"group_keys">>},
    Context = #{caller => MemberNode, profile => Profile, now => erlang:system_time(second),
                realm => UcanRealm, procedure => <<"acme/x">>, proofs => #{}},
    UcanTimes = times(fun() -> macula_ucan:authorize(Token, Policy, Context) end, 1000),
    io:format("  link-layer per pull: reply_sign p50 ~s, kem_decap p50 ~s, ucan_authorize p50 ~s~n",
              [ms(percentile(SignTimes, 50)), ms(percentile(DecapTimes, 50)),
               ms(percentile(UcanTimes, 50))]),
    FullPerPull = mean(TimesB)/1000 + mean(SignTimes)/1000 + mean(DecapTimes)/1000 + mean(UcanTimes)/1000,
    Expected = case {Profile, N} of
                   {pq_pure, 1000} -> 1.3; {pq_pure, 10000} -> 13.0;
                   {pq_hybrid, 1000} -> 5.5; {pq_hybrid, 10000} -> 55.0
               end,
    io:format("  full per-pull ~.3f ms; rotation total ~.1f s cpu (§6.3 estimate ~.1f s)~n",
              [FullPerPull, FullPerPull * N / 1000, Expected]),
    gen_server:stop(Pid),
    ets:delete(End),
    ok.

timed_pulls(Pull, Members) ->
    timed_pulls(Pull, Members, [], []).
timed_pulls(_Pull, [], Times, Bytes) ->
    {lists:reverse(Times), lists:reverse(Bytes)};
timed_pulls(Pull, [M | Rest], Times, Bytes) ->
    Start = erlang:monotonic_time(microsecond),
    Reply = Pull(M),
    Elapsed = erlang:monotonic_time(microsecond) - Start,
    timed_pulls(Pull, Rest, [Elapsed | Times],
                [byte_size(term_to_binary(Reply)) | Bytes]).

%% A single member's pull, as {Reply, ...} samples (for the event bench).
pull_samples(Pull, Members, Count) ->
    [Member | _] = Members,
    [begin
         Reply = Pull(Member),
         {Member, Reply}
     end || _ <- lists:seq(1, Count)].

event_cost(Current, Members) ->
    [Publisher | _] = Members,
    Payload = #{fact => {text, crypto:strong_rand_bytes(64)}},
    Fields = #{publisher => Publisher, realm => crypto:strong_rand_bytes(32),
               topic => <<"t">>, seq => 1,
               published_at => erlang:system_time(millisecond)},
    Sealed = element(2, macula_group_event:seal(Current, Fields, Payload)),
    SealTimes = times(fun() -> macula_group_event:seal(Current, Fields, Payload) end, ?EVENT_ITERS),
    OpenTimes = times(fun() -> macula_group_event:open(Sealed, Current) end, ?EVENT_ITERS),
    {SealTimes, OpenTimes}.

times(Fun, N) ->
    [begin
         Start = erlang:monotonic_time(microsecond),
         Fun(),
         erlang:monotonic_time(microsecond) - Start
     end || _ <- lists:seq(1, N)].

percentile(Times, P) ->
    Sorted = lists:sort(Times),
    lists:nth(max(1, min(length(Sorted), (P * length(Sorted)) div 100)), Sorted).

mean(Times) ->
    lists:sum(Times) / length(Times).

ms(Us) ->
    io_lib:format("~.4f ms", [Us / 1000]).
