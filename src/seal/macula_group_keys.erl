%% @doc A sealed group's distributor (docs/design/DESIGN_E2E_SEALED_PUBSUB.md §3, §5):
%% the handler of `<org>/group_keys_v1' and the epochs of every group it
%% serves.
%%
%% The application advertises the handler as any provider advertises one (its
%% mcl_om capability, or `macula_response:advertise_direct/7' kept alive with
%% `reuse_sup'), with `advertise_opts/0' merged into its options:
%% `confidential => required', so the provider's link answers a clear call
%% `sealed_required' and a key never travels clear, AND `auth =>
%% handler_authorizes', so the link hands the handler the token and proofs the
%% caller signed into the request, and the handler verifies them. The check cannot sit
%% in an advertise gate: the group a pull names travels inside the sealed
%% payload, where no gate sees it (macula#129).
%% With the node's `kem_advertise' switched off those options refuse to
%% advertise at all (`kem_advertise_disabled'), rather than advertise a keyless
%% distributor. The member side refuses a keyless distributor too
%% (`macula_group_keyring' pulls with `confidential => required'): each side
%% enforces it on its own.
%%
%% Each group has one access policy, chosen at start (`groups'):
%% <ul>
%%   <li>`org_grant', the default for a group not listed: the org's
%%   `group_keys' grant, `{realm_member_required, OrgKeyId, <<"group_keys">>}'
%%   on `<org>/group_keys_v1', the check the advertise gate made before;</li>
%%   <li>`{realm_capability, RealmKeyId, Mode}': the realm key's
%%   `pubsub/subscribe', issued to the caller, covering the whole group, its
%%   prefix `mri:topic:P' and everything below it `mri:topic:P/*'
%%   (`macula_ucan:authorize/3' with a `group'). A role the realm grants on
%%   `P/*' alone does not admit: a group key opens `P' too. `Mode' is
%%   `refusing', which admits on the realm capability alone, or `counting',
%%   the default, which counts a pull the realm capability would refuse and
%%   admits it when the same token passes the org grant. Counting is
%%   therefore fail-open by design, the union of two policies, and must show
%%   as counting, never as protected, until its would-be refusals read zero.
%%   A `realm_capability' group needs the distributor's seal policy
%%   `required': its events must never travel clear.</li>
%% </ul>
%% Each listed prefix is its own group: listing `P' restricts the group `P',
%% not a group `P/sub' pulled under its own prefix, which needs its own entry.
%% Either check verifies the token's audience against the caller, its
%% validity window at the time of the pull (a past epoch's pull too), and its
%% chain of proofs up to the key the policy names. A refused pull answers
%% `not_authorized: <can> on <resource>', naming what it lacked. Every pull of
%% a `realm_capability' group is counted per realm, group and outcome
%% (`admitted', `would_refuse', `refused'), read by `counts/1', and a refusal
%% or would-be refusal is logged at most once a minute per group and outcome.
%%
%% The handler then
%% decides from the caller's wire-authenticated node id: a node in the
%% application's removed set is refused every epoch, and so is a node that is
%% not a live realm member, read from the realm's slot for its endorsement
%% (`macula_record:realm_member_endorsement_key/2' and
%% `macula_hyparview_endorsement:slot_endorsement/4', which answers
%% `withdrawn' for the realm's tombstone). Membership is checked at the time of
%% the call, for a past epoch too: a subscriber catching up on one it missed is
%% still a member, and one that is not has no business reading the group.
%%
%% A call names the group (`prefix') and the epoch it wants: `current', or a
%% past one by id. The reply carries the group's policy and the epochs:
%% the current one, and the next one too inside the current's ahead window
%% (`macula_group_epoch'). Epoch keys are kept in memory only. A refusal is
%% `{error, Reason}', which macula answers as a sealed provider error with code
%% `handler_error' and the reason as its detail: `not_authorized: ...' (above),
%% `not_a_member',
%% `membership_unknown' (the lookup failed; retry), `unknown_epoch',
%% `epoch_expired', `unknown_group' (a prefix this distributor's org does not
%% own) or `malformed_pull' (a payload that is not a map).
%%
%% The epochs live in a private table the distributor's process owns, and its
%% state holds only that table's id: no other process can read the table, and
%% neither `sys:get_state/1', `sys:get_status/1' nor a crash report shows a key.
%% format_status/1 redacts every epoch's key as a second line of defence. A
%% crash dump lists a table's size, never its rows; it does copy every process
%% heap, where a reply passes on its way out, so a node holding a group's keys
%% runs with crash dumps off.
-module(macula_group_keys).
-behaviour(gen_server).

-export([start_link/1, handler/1, advertise_opts/0, counts/1]).
-export([init/1, handle_call/3, handle_cast/2, format_status/1]).

-type policy() :: required | preferred | off.
-type mode() :: counting | refusing.
-type access() :: org_grant | {realm_capability, <<_:256>>} | {realm_capability, <<_:256>>, mode()}.
-type outcome() :: admitted | would_refuse | refused.
-type options() :: #{org := binary(), policy := policy(), org_key_id := <<_:256>>, realm := <<_:256>>,
                     profile := macula_crypto_profile:profile(), groups => #{binary() => access()},
                     rotate_after_ms => pos_integer(),
                     now => fun(() -> integer()),
                     membership => fun((<<_:256>>) -> ok | {error, not_a_member | membership_unknown}),
                     removed => fun((<<_:256>>) -> boolean()),
                     pool => pid() | atom(), realm_key_id => <<_:256>>}.
-export_type([options/0, policy/0, access/0, mode/0, outcome/0]).

-define(GROUP_KEYS_CAN, <<"group_keys">>).
-define(SUBSCRIBE_CAN, <<"pubsub/subscribe">>).
%% A refusal and a would-be refusal are logged at most once a window per group and outcome.
-define(REPORT_WINDOW_MS, 60000).

%% How long a group's epochs are kept past their acceptance, so a pull of one
%% is answered `epoch_expired' rather than `unknown_epoch' for that long.
-define(TOLERANCE_MS, 5 * 60000).
%% How long the handler waits for the distributor: past a membership lookup's
%% 5 s and within a member's 15 s pull deadline (macula_group_keyring).
-define(HANDLER_WAIT_MS, 12000).

%% @doc Start a distributor for `org''s groups in `realm' (the realm id),
%% checking tokens in `profile'. `org_key_id' names the org key whose
%% `group_keys' grant an `org_grant' group admits; `groups' maps a group's
%% prefix to its access policy, `{realm_capability, RealmKeyId}' meaning
%% counting mode. A listed prefix the org does not own is refused as
%% `{error, {unknown_group, Prefix}}', an access of another shape as
%% `{error, {invalid_group_access, Prefix}}', and a `realm_capability' group
%% under a seal policy other than `required' as
%% `{error, {restricted_group_not_required, Prefix}}': nothing is started.
%% `membership' defaults to the
%% realm slot read over `pool', which then needs `realm_key_id' too;
%% `removed' defaults to nobody removed. Name the pool: `pool' as
%% its registered name is looked up at every read, so a pool its supervisor
%% restarts is found again, and while no pool holds the name a read is
%% `membership_unknown'. A pid is read as given, and once that pool has died
%% every read exits the distributor. The application keeps its
%% removed set across restarts: a set lost to a restart re-admits every removed
%% member whose grant still verifies.
-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(#{org := Org, policy := Policy, org_key_id := <<_:256>>, realm := <<_:256>>, profile := _} = Opts)
  when is_binary(Org), (Policy =:= required orelse Policy =:= preferred orelse Policy =:= off) ->
    started(checked_groups(maps:to_list(maps:get(groups, Opts, #{})), Org, Policy), Opts).

started({ok, Groups}, Opts) -> gen_server:start_link(?MODULE, Opts#{groups => Groups}, []);
started({error, _} = Refused, _Opts) -> Refused.

%% Every listed group: one the org owns, under an access of a known shape, and a restricted one only under `required'.
checked_groups(Listed, Org, Policy) ->
    checked_groups(Listed, Org, Policy, #{}).

checked_groups([], _Org, _Policy, Groups) ->
    {ok, Groups};
checked_groups([{Prefix, Access} | Rest], Org, Policy, Groups) ->
    next_group(group_access(owned_group(Prefix, Org), access(Access), Policy), Rest, Org, Policy, Groups).

next_group({ok, Prefix, Access}, Rest, Org, Policy, Groups) ->
    checked_groups(Rest, Org, Policy, Groups#{Prefix => Access});
next_group({error, _} = Refused, _Rest, _Org, _Policy, _Groups) ->
    Refused.

owned_group(Prefix, Org) ->
    listed(group_of(Prefix, #{org => Org}), Prefix).

listed({ok, Prefix}, _Listed) -> {ok, Prefix};
listed({error, unknown_group}, Listed) -> {error, {unknown_group, Listed}}.

access(org_grant) -> {ok, org_grant};
access({realm_capability, <<_:256>> = KeyId}) -> {ok, {realm_capability, KeyId, counting}};
access({realm_capability, <<_:256>> = KeyId, Mode}) when Mode =:= counting; Mode =:= refusing ->
    {ok, {realm_capability, KeyId, Mode}};
access(_Other) -> error.

group_access({error, _} = Refused, _Access, _Policy) -> Refused;
group_access({ok, Prefix}, error, _Policy) -> {error, {invalid_group_access, Prefix}};
group_access({ok, Prefix}, {ok, {realm_capability, _, _}}, Policy) when Policy =/= required ->
    {error, {restricted_group_not_required, Prefix}};
group_access({ok, Prefix}, {ok, Access}, _Policy) -> {ok, Prefix, Access}.

%% @doc The `<org>/group_keys_v1' handler for a distributor, to advertise. It
%% waits for the distributor longer than a membership lookup can take (5 s) and
%% less than a member's pull deadline (15 s), so a slow lookup answers
%% `membership_unknown', and no answer is made that no member still waits for.
-spec handler(pid()) -> fun((term()) -> map() | {error, atom() | binary()}).
handler(Pid) ->
    fun(Payload) -> gen_server:call(Pid, {pull, Payload}, ?HANDLER_WAIT_MS) end.

%% @doc The options the distributor's procedure is advertised with, merged into
%% the application's own: the call's own token handed to the handler, which
%% checks it per group, and sealed calls only.
-spec advertise_opts() -> #{auth := handler_authorizes, confidential := required}.
advertise_opts() ->
    #{auth => handler_authorizes, confidential => required}.

%% @doc The pulls of every `realm_capability' group so far, by realm, group and outcome.
-spec counts(pid()) -> #{{<<_:256>>, binary(), outcome()} => pos_integer()}.
counts(Pid) ->
    gen_server:call(Pid, counts).

%% @private
init(#{org := Org, policy := Policy, org_key_id := OrgKeyId, realm := Realm, profile := Profile,
       groups := Groups} = Opts) ->
    {ok, #{org => Org, policy => atom_to_binary(Policy),
           org_key_id => OrgKeyId, realm => Realm, profile => Profile, groups => Groups,
           counts => #{}, report => macula_refusal_report:new(?REPORT_WINDOW_MS),
           rotate_after_ms => maps:get(rotate_after_ms, Opts, macula_group_epoch:default_rotate_after_ms()),
           now => maps:get(now, Opts, fun() -> erlang:system_time(millisecond) end),
           membership => maps:get(membership, Opts, fun(Caller) -> realm_membership(Opts, Caller) end),
           removed => maps:get(removed, Opts, fun(_Caller) -> false end),
           table => ets:new(?MODULE, [private, set])}}.

%% @private
handle_call({pull, Payload}, _From, State) when is_map(Payload) ->
    Now = (maps:get(now, State))(),
    {Reply, NewState} = pulled(group_of(text_field(Payload, <<"prefix">>), State), maps:get(caller, Payload, undefined),
                               macula_record:payload_field(Payload, <<"epoch">>), Now,
                               presented(Payload), State),
    {reply, Reply, NewState};
%% A sealed CALL's payload is any CBOR, and the link merges the caller and the token into a map only: anything else
%% is refused here, never matched on, so no caller can stop the distributor and lose its epochs.
handle_call({pull, _NotAMap}, _From, State) ->
    {reply, {error, malformed_pull}, State};
handle_call(counts, _From, #{counts := Counts} = State) ->
    {reply, Counts, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
%% Status output and crash reports show every epoch with its key redacted.
format_status(Status) ->
    macula_node_keys:redacted(Status).

%% A prefix this distributor's org owns: at least two non-empty `/'-segments,
%% the second of them the org.
group_of(Prefix, #{org := Org}) when is_binary(Prefix) ->
    owned(binary:split(Prefix, <<"/">>, [global]), Org, Prefix);
group_of(_NotAPrefix, _State) ->
    {error, unknown_group}.

owned([Realm, Org | Rest], Org, Prefix) when Realm =/= <<>> ->
    no_empty_segment(lists:member(<<>>, Rest), Prefix);
owned(_Segments, _Org, _Prefix) ->
    {error, unknown_group}.

no_empty_segment(false, Prefix) -> {ok, Prefix};
no_empty_segment(true, _Prefix) -> {error, unknown_group}.

pulled({error, _} = Refused, _Caller, _Epoch, _Now, _Presented, State) ->
    {Refused, State};
pulled({ok, _Prefix}, Caller, _Epoch, _Now, _Presented, State) when not is_binary(Caller) ->
    {{error, not_a_member}, State};
pulled({ok, Prefix}, Caller, Epoch, Now, Presented, State) ->
    {Verdict, Counted} = authorized(access_of(Prefix, State), Prefix, Caller, Presented, Now, State),
    granted(Verdict, Caller, Prefix, Epoch, Now, Counted).

granted(ok, Caller, Prefix, Epoch, Now, State) ->
    admitted(admits(Caller, State), Prefix, Epoch, Now, State);
granted({error, _} = Refused, _Caller, _Prefix, _Epoch, _Now, State) ->
    {Refused, State}.

%% The call's own token and proofs, as the provider link hands them to a `handler_authorizes' handler.
presented(Payload) ->
    #{token => maps:get(ucan_token, Payload, <<>>), proofs => proofs_by_id(maps:get(proofs, Payload, []))}.

proofs_by_id(Proofs) when is_list(Proofs) ->
    maps:from_list([{macula_ucan:proof_id(Proof), Proof} || Proof <- Proofs, is_binary(Proof)]);
proofs_by_id(_NotAList) ->
    #{}.

access_of(Prefix, #{groups := Groups}) ->
    maps:get(Prefix, Groups, org_grant).

%% An `org_grant' group: the check the advertise gate made, uncounted. A `realm_capability' group: the realm's
%% capability on the whole group; refusing stops there, counting lets a would-be refusal through on the org grant.
authorized(org_grant, _Prefix, Caller, Presented, Now, State) ->
    {verdict(org_check(Caller, Presented, Now, State), ?GROUP_KEYS_CAN, org_procedure(State)), State};
authorized({realm_capability, KeyId, Mode}, Prefix, Caller, Presented, Now, State) ->
    realm_checked(realm_check(KeyId, Prefix, Caller, Presented, Now, State), Mode, Prefix, Caller, Presented, Now,
                  State).

realm_checked({ok, _Claims}, _Mode, Prefix, _Caller, _Presented, Now, State) ->
    {ok, counted(Prefix, admitted, Now, State)};
realm_checked({error, _}, refusing, Prefix, _Caller, _Presented, Now, State) ->
    {lacking(?SUBSCRIBE_CAN, topic_resource(Prefix)), counted(Prefix, refused, Now, State)};
realm_checked({error, _}, counting, Prefix, Caller, Presented, Now, State) ->
    org_fallback(org_check(Caller, Presented, Now, State), Prefix, Now, State).

org_fallback({ok, _Claims}, Prefix, Now, State) ->
    {ok, counted(Prefix, would_refuse, Now, State)};
org_fallback({error, _}, Prefix, Now, State) ->
    {lacking(?SUBSCRIBE_CAN, topic_resource(Prefix)), counted(Prefix, refused, Now, State)}.

org_check(Caller, #{token := Token, proofs := Proofs}, Now, #{org_key_id := OrgKeyId, realm := Realm} = State) ->
    macula_ucan:authorize(Token, {realm_member_required, OrgKeyId, ?GROUP_KEYS_CAN},
                          (context(Caller, Proofs, Now, State))#{realm => Realm, procedure => org_procedure(State)}).

realm_check(KeyId, Prefix, Caller, #{token := Token, proofs := Proofs}, Now, #{realm := Realm} = State) ->
    macula_ucan:authorize(Token, {realm_member_required, KeyId, ?SUBSCRIBE_CAN},
                          (context(Caller, Proofs, Now, State))#{realm => Realm, group => Prefix}).

context(Caller, Proofs, Now, #{profile := Profile}) ->
    #{caller => Caller, profile => Profile, now => Now div 1000, proofs => Proofs}.

org_procedure(#{org := Org}) -> <<Org/binary, "/group_keys_v1">>.

verdict({ok, _Claims}, _Can, _Procedure) -> ok;
verdict({error, _}, Can, Procedure) -> lacking(Can, <<"mri:proc:", Procedure/binary>>).

topic_resource(Prefix) -> <<"mri:topic:", Prefix/binary>>.

%% A refusal names the capability and the resource it lacked, as the realm's other checks do. It crosses as the
%% handler's own text.
lacking(Can, Resource) ->
    {error, <<"not_authorized: ", Can/binary, " on ", Resource/binary>>}.

counted(Prefix, Outcome, Now, #{realm := Realm, counts := Counts} = State) ->
    reported(Outcome, Prefix, Now, State#{counts => maps:update_with({Realm, Prefix, Outcome}, fun(N) -> N + 1 end, 1,
                                                                    Counts)}).

reported(admitted, _Prefix, _Now, State) ->
    State;
reported(Outcome, Prefix, Now, #{report := Report} = State) ->
    State#{report => logged(macula_refusal_report:refused(Report, {Prefix, Outcome}, Now), Prefix, Outcome)}.

logged({report, Count, Report}, Prefix, Outcome) ->
    logger:warning("[macula_group_keys] ~b ~ts on group ~ts", [Count, Outcome, Prefix]),
    Report;
logged({quiet, Report}, _Prefix, _Outcome) ->
    Report.

admits(Caller, #{removed := Removed, membership := Membership}) ->
    not_removed(Removed(Caller), Caller, Membership).

not_removed(true, _Caller, _Membership) -> {error, not_a_member};
not_removed(false, Caller, Membership) -> Membership(Caller).

admitted(ok, Prefix, Epoch, Now, State) ->
    served(Epoch, Prefix, Now, State);
admitted({error, _} = Refused, _Prefix, _Epoch, _Now, State) ->
    {Refused, State}.

served(<<"current">>, Prefix, Now, State) ->
    {Epochs, NewState} = current(Prefix, Now, State),
    {reply(Prefix, Epochs, NewState), NewState};
served(<<_:64>> = Id, Prefix, Now, State) ->
    {past(Id, group_epochs(Prefix, Now, State), Prefix, Now, State), State};
served(_Other, _Prefix, _Now, State) ->
    {{error, unknown_epoch}, State}.

%% The group's current epoch, and the next one inside the current's ahead
%% window, made when first asked for and kept so every member gets the same.
current(Prefix, Now, #{rotate_after_ms := R, table := Table} = State) ->
    Epochs = ahead(advanced(group_epochs(Prefix, Now, State), Now, R), Now, R),
    {ok, Current} = macula_group_epoch:for_publish(Epochs, Now),
    true = ets:insert(Table, {Prefix, Epochs}),
    {[Current | [E || #{issued_at := I} = E <- Epochs, I =:= maps:get(publish_until, Current)]], State}.

%% Epochs up to one current at Now: contiguous after the newest, or a fresh one
%% when the group sat idle for a whole rotation.
advanced([], Now, R) ->
    [macula_group_epoch:new(Now, R)];
advanced(Epochs, Now, R) ->
    advanced_from(lists:last(Epochs), Epochs, Now, R).

advanced_from(#{publish_until := P}, Epochs, Now, _R) when Now < P ->
    Epochs;
advanced_from(#{publish_until := P}, Epochs, Now, R) when Now - P >= R ->
    Epochs ++ [macula_group_epoch:new(Now, R)];
advanced_from(Newest, Epochs, Now, R) ->
    advanced(Epochs ++ [macula_group_epoch:next(Newest, R)], Now, R).

ahead(Epochs, Now, R) ->
    Newest = lists:last(Epochs),
    ahead_of(macula_group_epoch:in_ahead_window(Newest, R, Now), Newest, Epochs, R).

ahead_of(true, Newest, Epochs, R) -> Epochs ++ [macula_group_epoch:next(Newest, R)];
ahead_of(false, _Newest, Epochs, _R) -> Epochs.

%% The group's epochs still kept at Now: until acceptance, plus one rotation.
group_epochs(Prefix, Now, #{table := Table, rotate_after_ms := R}) ->
    [E || {_Prefix, Epochs} <- ets:lookup(Table, Prefix), #{accept_until := A} = E <- Epochs,
          Now =< A + ?TOLERANCE_MS + R].

past(Id, Epochs, Prefix, Now, State) ->
    held(lists:search(fun(#{id := EId}) -> EId =:= Id end, Epochs), Prefix, Now, State).

held({value, Epoch}, Prefix, Now, State) ->
    acceptable(macula_group_epoch:acceptable(Epoch, Now), Epoch, Prefix, State);
held(false, _Prefix, _Now, _State) ->
    {error, unknown_epoch}.

acceptable(true, Epoch, Prefix, State) -> reply(Prefix, [Epoch], State);
acceptable(false, _Epoch, _Prefix, _State) -> {error, epoch_expired}.

%% Text travels tagged, so every SDK reads `prefix' and `policy' as text, not bytes.
reply(Prefix, Epochs, #{policy := Policy}) ->
    #{prefix => {text, Prefix}, policy => {text, Policy}, epochs => Epochs}.

text_field(Payload, Name) ->
    macula_record:payload_field(Payload, Name).

%% The default membership read: the realm key's entry in the caller's
%% endorsement slot, verified now; a tombstone answers withdrawn. A lookup that
%% fails is `membership_unknown', never `not_a_member'.
realm_membership(#{pool := Pool, realm := Realm, realm_key_id := RealmKeyId, profile := Profile}, Caller) ->
    slot_read(found(live_pool(Pool), macula_record:realm_member_endorsement_key(Realm, Caller)),
              #{profile => Profile, realm => Realm, realm_key_id => RealmKeyId}, Caller).

live_pool(Pool) when is_pid(Pool) -> Pool;
live_pool(Name) when is_atom(Name) -> whereis(Name).

found(undefined, _Key) -> {error, no_pool};
found(Pool, Key) -> macula:find_records(Pool, Key).

slot_read({ok, Records}, Trust, Caller) ->
    {Outcome, _Stats} = macula_hyparview_endorsement:slot_endorsement([macula_record:encode(R) || R <- Records],
                                                                      Trust, Caller),
    member_of(Outcome);
slot_read({error, _}, _Trust, _Caller) ->
    {error, membership_unknown}.

member_of({ok, _Roles}) -> ok;
member_of({error, _NotAMember}) -> {error, not_a_member}.
