%% @doc Realm-join handshake helpers (Phase 5.6).
%%
%% Building block for the admission flow of a new station into a
%% realm. The station presents a signed `realm_member_endorsement'
%% record (issued by the realm admin) and the receiving peers
%% verify it against the realm's admin key before admitting the
%% station into the HyParView active/passive view.
%%
%% This module is pure — it does not talk to the network. Callers
%% (typically the per-station dispatcher) feed the decoded endorsement
%% record into `verify_endorsement/3' and act on the outcome.
%%
%% == Acceptance rules ==
%%
%% <ul>
%%   <li>Record type MUST be the endorsement tag
%%       (`#?TYPE_REALM_MEMBER_ENDORSEMENT').</li>
%%   <li>Envelope signature MUST verify against the admin key the
%%       local node trusts for that realm. `verify/1' guards both
%%       signature and expiry already.</li>
%%   <li>Payload `realm' field MUST equal the expected realm id.</li>
%%   <li>Payload `member_node' field MUST equal the candidate node
%%       id claimed by the joining peer — prevents stealing another
%%       member's endorsement.</li>
%%   <li>`valid_from ≤ now ≤ valid_until' — the endorsement must be
%%       currently active.</li>
%% </ul>
%%
%% Reference: plans/PLAN_MACULA_V2_PART6_PROTOCOL.md §9.6.
-module(macula_hyparview_endorsement).

-export([verify_endorsement/3, build_join/4]).

-define(TYPE_REALM_MEMBER_ENDORSEMENT, 16#05).

-type realm()   :: <<_:256>>.
-type node_id() :: <<_:256>>.

-type verify_error() ::
        bad_record
      | signature_invalid
      | expired
      | wrong_type
      | wrong_realm
      | wrong_member
      | not_yet_valid
      | endorsement_expired.

%% @doc Verify an endorsement record authorises `Member' for `Realm'.
%%
%% Returns `{ok, Roles}' with the endorsed role list on success, or
%% `{error, Reason}' otherwise. Callers typically treat any error as
%% a rejection and drop the pending join.
-spec verify_endorsement(macula_record:m_record(), realm(), node_id()) ->
        {ok, [binary()]} | {error, verify_error()}.
verify_endorsement(Record, Realm, Member)
  when is_binary(Realm),  byte_size(Realm)  =:= 32,
       is_binary(Member), byte_size(Member) =:= 32 ->
    verify_shape_then_payload(macula_record:verify(Record), Realm, Member).

verify_shape_then_payload({error, Reason}, _Realm, _Member) ->
    {error, Reason};
verify_shape_then_payload({ok, Record}, Realm, Member) ->
    check_type(Record, Realm, Member).

check_type(#{type := ?TYPE_REALM_MEMBER_ENDORSEMENT} = R, Realm, Member) ->
    check_realm(R, Realm, Member);
check_type(_R, _Realm, _Member) ->
    {error, wrong_type}.

check_realm(#{key := Realm, payload := P} = R, Realm, Member) ->
    case maps:get({text, <<"realm">>}, P, undefined) of
        Realm -> check_member(R, Member);
        _     -> {error, wrong_realm}
    end;
check_realm(_R, _Realm, _Member) ->
    {error, wrong_realm}.

check_member(#{payload := P} = R, Member) ->
    case maps:get({text, <<"member_node">>}, P, undefined) of
        Member -> check_window(R);
        _      -> {error, wrong_member}
    end.

check_window(#{payload := P}) ->
    NowMs = erlang:system_time(millisecond),
    From  = maps:get({text, <<"valid_from">>}, P, 0),
    Until = maps:get({text, <<"valid_until">>}, P, 0),
    evaluate_window(NowMs, From, Until, P).

evaluate_window(Now, From, _Until, _P) when Now < From ->
    {error, not_yet_valid};
evaluate_window(Now, _From, Until, _P) when Now > Until ->
    {error, endorsement_expired};
evaluate_window(_Now, _From, _Until, P) ->
    {ok, extract_roles(P)}.

extract_roles(P) ->
    Raw = maps:get({text, <<"roles">>}, P, []),
    [role_to_binary(R) || R <- Raw].

role_to_binary({text, R}) when is_binary(R) -> R;
role_to_binary(R)        when is_binary(R) -> R.

%% @doc Build the initial JOIN frame the joining station sends to
%% one of the realm's known stations.
%%
%% The joining station signs the frame with `Identity' so the
%% receiver can bind the join attempt to the candidate member key
%% (the endorsement binds it to the realm).
-spec build_join(realm(), node_id(), macula_record:m_record(),
                 macula_identity:key_pair()) -> macula_frame:frame().
build_join(Realm, NewMember, Endorsement, Identity)
  when is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(NewMember), byte_size(NewMember) =:= 32,
       is_map(Endorsement) ->
    macula_frame:sign(macula_frame:hyparview_join(#{
        realm      => Realm,
        new_member => NewMember,
        record     => Endorsement
    }), Identity).
