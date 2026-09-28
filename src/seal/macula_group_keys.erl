%% @doc A sealed group's distributor (plans/DESIGN_E2E_SEALED_PUBSUB.md §3, §5):
%% the handler of `<org>/group_keys_v1' and the epochs of every group it
%% serves.
%%
%% The application advertises the handler as any provider advertises one (its
%% mcl_om capability, or `macula_response:advertise_direct/7' kept alive with
%% `reuse_sup'), with `advertise_opts/1' merged into its options: the policy
%% `{realm_member_required, OrgKeyId, <<"group_keys">>}', so the org's
%% `group_keys' UCAN is checked by macula before the handler runs, matched to
%% the org key by its key id, AND `confidential => required', so the provider's
%% link answers a clear call `sealed_required' and a key never travels clear.
%% With the node's `kem_advertise' switched off those options refuse to
%% advertise at all (`kem_advertise_disabled'), rather than advertise a keyless
%% distributor. The member side refuses a keyless distributor too
%% (`macula_group_keyring' pulls with `confidential => required'): each side
%% enforces it on its own. The handler then
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
%% `handler_error' and the reason as its detail: `not_a_member',
%% `membership_unknown' (the lookup failed; retry), `unknown_epoch',
%% `epoch_expired' or `unknown_group' (a prefix this distributor's org does not
%% own).
-module(macula_group_keys).
-behaviour(gen_server).

-export([start_link/1, handler/1, advertise_opts/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-type policy() :: required | preferred | off.
-type options() :: #{org := binary(), policy := policy(), rotate_after_ms => pos_integer(),
                     now => fun(() -> integer()),
                     membership => fun((<<_:256>>) -> ok | {error, not_a_member | membership_unknown}),
                     removed => fun((<<_:256>>) -> boolean()),
                     pool => pid(), realm => <<_:256>>, realm_key_id => <<_:256>>,
                     profile => macula_crypto_profile:profile()}.
-export_type([options/0, policy/0]).

%% How long a group's epochs are kept past their acceptance, so a pull of one
%% is answered `epoch_expired' rather than `unknown_epoch' for that long.
-define(TOLERANCE_MS, 5 * 60000).
%% How long the handler waits for the distributor: past a membership lookup's
%% 5 s and within macula's 30 s handler budget.
-define(HANDLER_WAIT_MS, 25000).

%% @doc Start a distributor for `org''s groups. `membership' defaults to the
%% realm slot read over `pool', which then needs `realm', `realm_key_id' and
%% `profile'; `removed' defaults to nobody removed. The application keeps its
%% removed set across restarts: a set lost to a restart re-admits every removed
%% member whose grant still verifies.
-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(#{org := Org, policy := Policy} = Opts)
  when is_binary(Org), (Policy =:= required orelse Policy =:= preferred orelse Policy =:= off) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc The `<org>/group_keys_v1' handler for a distributor, to advertise. It
%% waits for the distributor as long as a membership lookup can take and less
%% than macula's default handler budget (30 s), so a slow lookup answers
%% `membership_unknown', not a relay failure.
-spec handler(pid()) -> fun((map()) -> map() | {error, atom()}).
handler(Pid) ->
    fun(Payload) -> gen_server:call(Pid, {pull, Payload}, ?HANDLER_WAIT_MS) end.

%% @doc The options the distributor's procedure is advertised with, merged into
%% the application's own: the org's grant, checked before the handler runs, and
%% sealed calls only.
-spec advertise_opts(<<_:256>>) ->
          #{auth := {realm_member_required, <<_:256>>, binary()}, confidential := required}.
advertise_opts(<<_:256>> = OrgKeyId) ->
    #{auth => {realm_member_required, OrgKeyId, <<"group_keys">>}, confidential => required}.

%% @private
init(#{org := Org, policy := Policy} = Opts) ->
    {ok, #{org => Org, policy => atom_to_binary(Policy),
           rotate_after_ms => maps:get(rotate_after_ms, Opts, macula_group_epoch:default_rotate_after_ms()),
           now => maps:get(now, Opts, fun() -> erlang:system_time(millisecond) end),
           membership => maps:get(membership, Opts, fun(Caller) -> realm_membership(Opts, Caller) end),
           removed => maps:get(removed, Opts, fun(_Caller) -> false end),
           groups => #{}}}.

%% @private
handle_call({pull, Payload}, _From, State) ->
    Now = (maps:get(now, State))(),
    {Reply, NewState} = pulled(group_of(text_field(Payload, <<"prefix">>), State), maps:get(caller, Payload, undefined),
                               macula_record:payload_field(Payload, <<"epoch">>), Now, State),
    {reply, Reply, NewState}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

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

pulled({error, _} = Refused, _Caller, _Epoch, _Now, State) ->
    {Refused, State};
pulled({ok, _Prefix}, Caller, _Epoch, _Now, State) when not is_binary(Caller) ->
    {{error, not_a_member}, State};
pulled({ok, Prefix}, Caller, Epoch, Now, State) ->
    admitted(admits(Caller, State), Prefix, Epoch, Now, State).

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
current(Prefix, Now, #{rotate_after_ms := R, groups := Groups} = State) ->
    Epochs = ahead(advanced(group_epochs(Prefix, Now, State), Now, R), Now, R),
    {ok, Current} = macula_group_epoch:for_publish(Epochs, Now),
    {[Current | [E || #{issued_at := I} = E <- Epochs, I =:= maps:get(publish_until, Current)]],
     State#{groups := Groups#{Prefix => Epochs}}}.

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
group_epochs(Prefix, Now, #{groups := Groups, rotate_after_ms := R}) ->
    [E || #{accept_until := A} = E <- maps:get(Prefix, Groups, []), Now =< A + ?TOLERANCE_MS + R].

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
    slot_read(macula:find_records(Pool, macula_record:realm_member_endorsement_key(Realm, Caller)),
              #{profile => Profile, realm => Realm, realm_key_id => RealmKeyId}, Caller).

slot_read({ok, Records}, Trust, Caller) ->
    {Outcome, _Stats} = macula_hyparview_endorsement:slot_endorsement([macula_record:encode(R) || R <- Records],
                                                                      Trust, Caller),
    member_of(Outcome);
slot_read({error, _}, _Trust, _Caller) ->
    {error, membership_unknown}.

member_of({ok, _Roles}) -> ok;
member_of({error, _NotAMember}) -> {error, not_a_member}.
