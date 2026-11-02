%% @doc The realm's RBAC checks at the points that enforce them (macula#128, WP3 of the realm RBAC plan; threat model:
%% macula-realm#97, "Threat model: RBAC for call, serve, publish and subscribe (WP0)").
%%
%% A member presents its realm membership token once per link to its own station (`admit/2'): the station checks its
%% size before verifying anything, verifies it under the realm's key with the link peer as its audience, and keeps the
%% capabilities it grants until it stops authorizing. Each SUBSCRIBE and PUBLISH is then answered from those
%% capabilities (`subscribe/4', `publish/4'), with nothing verified again: one verification per token per link (T15).
%% A caller checks that a provider's delegation names the procedure it serves (`serve/4', T7).
%%
%% == Counting first ==
%%
%% Every check runs in one of four modes per realm (`mode/1'): `off' checks nothing, `counting' counts what it would
%% refuse and lets it through, `members' asks only whether the link holds a live membership of the realm, and
%% `refusing' refuses what the capability check would. The default is `counting', so a realm refuses only once its
%% operator has seen its counters read zero for authorised traffic and set it to `refusing'. A realm in counting mode
%% is open by design and must be shown as counting, never as protected (threat model 6.7).
%%
%% Members mode is how a realm is closed on the wire (macula-realm#175, D1): a realm mints no `pubsub/publish'
%% capability by design (macula#147), so `refusing' would refuse every legitimate PUBLISH and cannot close anything.
%% `membership/3' is a decision of its own, askable in every mode from `counting' up: in `counting' and `refusing' it
%% allows and only counts what members mode would have done, under the `membership' check and by reason, so the
%% close is measured before it is switched on (threat 6). `subscribe/4', `publish/4' and `serve/5' defer to it in
%% members mode.
%%
%% Every decision in counting or refusing mode is counted by realm, check and outcome (`counters/0'): `authorised',
%% `would_refuse' (counting), `refused' (refusing) and `exempt' (a system topic, which names no realm and carries the
%% mesh's own traffic, `macula_topic:is_system_topic/1'). A refusal names the capability and the resource it lacked.
%%
%% == What this does not promise ==
%%
%% Publish is checked at the publisher's ingress station only: a station that colludes with a member can still inject
%% readable events on a clear topic (threat model 4.1). Subscribe control on a clear topic is delivery control, not
%% confidentiality (4.2): only a sealed group keeps a topic's content from those who may not read it.
%%
%% A members-mode realm couples its own availability to its members' (macula-realm#175, threat 5, D4, accepted): if
%% the realm cannot mint, or the trust list stops naming its key, every member of that realm is refused at every
%% station within the token life, admins included, and the way back is an operator's mode change on the station, not
%% anything the realm can do. Membership is also closed, not secret: a member's node id is readable from its
%% endorsement records in the DHT.
%%
%% SUBSCRIBE and PUBLISH are counted under `membership' only in `members' mode, where that is the whole decision: in
%% `counting' and `refusing' they are counted under `subscribe' and `publish' as they always were, and a `membership'
%% row moves only where the caller asks `membership/3' itself — which is exactly what a station watching a realm
%% before it closes it does, and why the two kinds of row must be read apart.
%%
%% The counter table belongs to macula_peering_sup, which creates it with new_table/0, as macula_wire_tolerance's is.
-module(macula_realm_rights).

-export([new_table/0, admit/2, subscribe/4, publish/4, serve/4, serve/5, membership/3, membership_reason/1,
         mode/1, counters/0,
         max_token_bytes/0, max_proofs/0, max_presented_bytes/0, within_size/1, carrying/1, presented/1,
         extension_name/0]).

-export_type([rights/0, presented/0, mode/0, check/0, outcome/0, refusal/0, membership/0, membership_reason/0]).

-define(TABLE, ?MODULE).
-define(SUBSCRIBE_CAN, <<"pubsub/subscribe">>).
-define(PUBLISH_CAN, <<"pubsub/publish">>).
-define(SERVE_CAN, <<"rpc/serve">>).
%% The capability a membership refusal names (macula-realm#175, D1): members mode reads no capability, so a refusal
%% says the frame needed a membership in the realm and did not have a live one.
-define(MEMBER_CAN, <<"realm/member">>).
%% A membership token carries at most 1 + 8 x 16 capabilities (macula-realm#97 requirement 5): about 12 KB of
%% capabilities, base64url, plus a did:key and an ML-DSA-87 signature, about 26 KB in all. 48 KiB leaves room for
%% the composite profile's larger key and signature and still refuses a flood of large tokens before any verify.
-define(MAX_TOKEN_BYTES, 49152).
%% A delegated client's chain is the realm's token and a hop or two; four proofs is more than any chain the realm
%% issues today.
-define(MAX_PROOFS, 4).
-define(MAX_PRESENTED_BYTES, 131072).
%% The per-hop extension a SUBSCRIBE or PUBLISH presents a membership in (docs/design/WIRE_1_0.md section 3.6).
-define(MEMBERSHIP_EXTENSION, <<"realm.membership">>).
%% A realm id comes from a peer's frame: past 256 realms with a row, a new one counts under the label other.
-define(MAX_REALM_LABELS, 256).
-define(OTHER, other).

-type mode() :: off | counting | members | refusing.
-type check() :: subscribe | publish | serve | membership.
-type outcome() :: authorised | would_refuse | refused | exempt.
%% What a members-mode decision counted when it was only counting (macula-realm#175, threat 6): a membership that
%% would have been admitted, or would have been refused, counted apart from the capability decision's own
%% `no_token' / `not_granted', so an operator can see which members a close would have turned away.
-type member_outcome() :: member | would_refuse.
%% What a member presented on its link: its membership token and the proofs of its chain, each as it travelled.
-type presented() :: #{token := binary(), proofs := [binary()]}.
%% What admit/2 keeps for a link: the capabilities the token grants, the second it stops authorizing, and the member
%% the realm endorsed at its chain's root.
-type rights() :: #{caps := [map()], until := integer(), member := <<_:256>>}.
%% Why a frame did not come from a live member of the realm (macula-realm#175, threat 6): it presented no token, its
%% token had expired, its token was minted for another node's link, or for another realm, or it did not verify at all
%% (`invalid', which keeps the refusal's own atom in the refusal's log line).
-type membership_reason() :: no_token | expired | not_the_audience | wrong_realm | invalid.
%% The membership verdict a station keeps per link and realm, apart from the capabilities the token grants: the two
%% are different questions, and in members mode only this one is asked.
-type membership() :: {member, rights()} | {not_member, membership_reason()}.
%% A refusal names the check's capability, the resource it was asked for, and why.
-type refusal() :: #{can := binary(), resource := binary(), reason := atom()}.

%% @doc Create the counter table, owned by the calling process: macula_peering_sup, whose life is the run's.
-spec new_table() -> ok.
new_table() ->
    ?TABLE = ets:new(?TABLE, [named_table, public, set, {write_concurrency, true}]),
    ok.

%% @doc The largest membership token admit/2 verifies, in bytes.
-spec max_token_bytes() -> pos_integer().
max_token_bytes() -> ?MAX_TOKEN_BYTES.

%% @doc The most proofs admit/2 takes with a token.
-spec max_proofs() -> pos_integer().
max_proofs() -> ?MAX_PROOFS.

%% @doc The most bytes a token and its proofs together may come to.
-spec max_presented_bytes() -> pos_integer().
max_presented_bytes() -> ?MAX_PRESENTED_BYTES.

%% @doc The name of the per-hop extension that carries a presented membership.
-spec extension_name() -> binary().
extension_name() -> ?MEMBERSHIP_EXTENSION.

%% @doc The extension map a SUBSCRIBE or PUBLISH carries to present Presented to the link's station: one entry,
%% not critical, so a station that predates it ignores it and the frame is handled as before.
-spec carrying(presented()) -> #{binary() => #{critical := 0, value := map()}}.
carrying(#{token := Token, proofs := Proofs}) when is_binary(Token), is_list(Proofs) ->
    #{?MEMBERSHIP_EXTENSION => #{critical => 0, value => #{<<"token">> => Token, <<"proofs">> => Proofs}}}.

%% @doc The membership a received SUBSCRIBE or PUBLISH presents, read from its per-hop extension map as the frame
%% decoder keeps it: `none' when it carries none, malformed when the entry is not a token and a list of proofs, each
%% bytes. Nothing is verified here; admit/2 checks the size and verifies.
-spec presented(macula_frame:frame()) -> {ok, presented()} | none | {error, malformed}.
presented(#{ext := #{?MEMBERSHIP_EXTENSION := #{value := Value}}}) ->
    presented_value(Value);
presented(_Frame) ->
    none.

presented_value(#{{text, <<"token">>} := Token, {text, <<"proofs">>} := Proofs} = Value)
  when map_size(Value) =:= 2, is_binary(Token), is_list(Proofs) ->
    presented_proofs(lists:all(fun is_binary/1, Proofs), Token, Proofs);
presented_value(_Value) ->
    {error, malformed}.

presented_proofs(true, Token, Proofs) -> {ok, #{token => Token, proofs => Proofs}};
presented_proofs(false, _Token, _Proofs) -> {error, malformed}.

%% @doc Verify a membership token a member presented on its link: checked for size before anything is verified, then
%% under the realm key RealmKeyId with the link peer as its audience (threat model 6.1), so a token another node
%% presents is refused not_the_audience. The rights it grants are kept until `until', the earliest exp of the token
%% and its proofs; the link's holder drops what it admitted on them then, unless a fresh token arrived (6.2).
-spec admit(presented(), #{peer := <<_:256>>, realm_key := <<_:256>>, profile := macula_crypto_profile:profile(),
                           now := integer()}) ->
        {ok, rights()} | {error, too_large | macula_ucan:refusal()}.
admit(#{token := Token, proofs := Proofs} = Presented,
      #{peer := <<_:256>> = Peer, realm_key := <<_:256>> = RealmKey, profile := Profile, now := Now})
  when is_binary(Token), is_list(Proofs), is_integer(Now) ->
    sized(within_size(Presented), Token, Proofs, #{caller => Peer, profile => Profile, now => Now},
          {realm_member_required, RealmKey, any});
admit(_Presented, _Context) ->
    {error, malformed}.

%% @doc Whether a token and its proofs fit the sizes admit/2 verifies: the token and each proof within
%% max_token_bytes/0, at most max_proofs/0 proofs, and all within max_presented_bytes/0.
-spec within_size(presented()) -> boolean().
within_size(#{token := Token, proofs := Proofs}) ->
    byte_size(Token) =< ?MAX_TOKEN_BYTES andalso length(Proofs) =< ?MAX_PROOFS
        andalso lists:all(fun(P) -> is_binary(P) andalso byte_size(P) =< ?MAX_TOKEN_BYTES end, Proofs)
        andalso byte_size(Token) + lists:sum([byte_size(P) || P <- Proofs]) =< ?MAX_PRESENTED_BYTES.

sized(false, _Token, _Proofs, _Context, _Policy) ->
    {error, too_large};
sized(true, Token, Proofs, Context, Policy) ->
    ByProofId = maps:from_list([{macula_ucan:proof_id(P), P} || P <- Proofs]),
    admitted(macula_ucan:authorize(Token, Policy, Context#{proofs => ByProofId}), Token, Proofs).

admitted({ok, #{<<"cap">> := Caps}}, Token, Proofs) ->
    {ok, #{caps => Caps, until => macula_ucan:authorized_until(Token, Proofs),
           member => macula_ucan:root_audience(Token, Proofs)}};
admitted({error, _} = Refusal, _Token, _Proofs) ->
    Refusal.

%% @doc Whether the link may subscribe to Pattern in Realm, in Mode, on the rights admit/2 kept for it (`none' when
%% it presented no token, or its token stopped authorizing). A grant must cover every topic the pattern can match
%% (threat model 4.4, macula_ucan:grants/4). `allow' lets the SUBSCRIBE through; `{refuse, Refusal}' refuses it
%% not_authorized, naming the capability and the pattern.
%%
%% In `members' mode the rights map is read as a live membership and its `until' is NOT re-checked here: the caller
%% keeps what admit/2 kept and drops it at `until', as it already does on the capability path, and members mode
%% relies on that. A caller that holds the map past `until' admits a member whose token ended — which is why the
%% station, not this function, is the one that knows why a token ended (membership/3).
-spec subscribe(mode(), <<_:256>>, rights() | none, binary()) -> allow | {refuse, refusal()}.
subscribe(Mode, Realm, Rights, Pattern) when is_binary(Pattern) ->
    topic_decision(subscribe, ?SUBSCRIBE_CAN, Mode, Realm, Rights, Pattern, {subscription, Pattern}).

%% @doc Whether the link may publish on Topic in Realm, in Mode, on the rights admit/2 kept for it: the exact bytes
%% the station routes on, never a pattern.
-spec publish(mode(), <<_:256>>, rights() | none, binary()) -> allow | {refuse, refusal()}.
publish(Mode, Realm, Rights, Topic) when is_binary(Topic) ->
    topic_decision(publish, ?PUBLISH_CAN, Mode, Realm, Rights, Topic, {topic, Topic}).

topic_decision(_Check, _Can, off, _Realm, _Rights, _Topic, _Request) ->
    allow;
topic_decision(Check, Can, Mode, Realm, Rights, Topic, Request) ->
    topic_verdict(macula_topic:is_system_topic(Topic), Check, Can, Mode, Realm, Rights, Topic, Request).

topic_verdict(true, Check, _Can, _Mode, Realm, _Rights, _Topic, _Request) ->
    count(Realm, Check, exempt),
    allow;
%% Members mode reads no capability at all (macula-realm#175, D1): a live, unexpired membership is the whole
%% decision, and there is no `pubsub/publish' grant to read by design (macula#147). A caller that still hands the
%% old function a link's rights gets the same answer the station gets from membership/3, with `no_token' when it
%% passed none — but only the station knows why a token ended, so it asks membership/3 itself.
topic_verdict(false, _Check, _Can, members, Realm, Rights, _Topic, _Request) ->
    membership(members, Realm, held_membership(Rights));
topic_verdict(false, Check, Can, Mode, Realm, Rights, Topic, Request) ->
    decided(Mode, Check, Realm, #{can => Can, resource => Topic}, held(Rights, Can, Realm, Request)).

held(none, _Can, _Realm, _Request) -> {error, no_token};
held(#{caps := Caps}, Can, Realm, Request) -> macula_ucan:grants(Caps, Can, Realm, Request).

%% The membership a caller of subscribe/4 or publish/4 has to hand: the rights a link's token was admitted with, or
%% nothing presented. `no_token' is the only reason these two functions can see; a token that ended is the station's
%% to report through membership/3, with `expired'.
held_membership(none) -> {not_member, no_token};
held_membership(#{caps := _} = Rights) -> {member, Rights}.

%% @doc Whether a provider may serve Procedure in Realm, in Mode, given what the caller or the station read from its
%% delegation (macula_record:delegated_procedure/3): `ok' when the delegation names it, `{error, Reason}' when it does
%% not or names no procedures at all. Counted like the other checks.
-spec serve(mode(), <<_:256>>, binary(), ok | {error, atom()}) -> allow | {refuse, refusal()}.
serve(off, _Realm, _Procedure, _Verdict) ->
    allow;
serve(Mode, Realm, Procedure, Verdict) when is_binary(Procedure) ->
    decided(Mode, serve, Realm, #{can => ?SERVE_CAN, resource => Procedure}, Verdict).

%% @doc Whether a provider may serve Procedure in Realm, in Mode, given both what a caller or the station read from
%% its delegation and the advertiser's membership of the realm (macula-realm#175, D3): in `members' an ADVERTISE
%% needs both, so a member without the delegation is refused `rpc/serve' and a delegation without a live membership
%% is refused `realm/member'. In every other mode the delegation decides as `serve/4' does, and the membership is
%% only counted (which is all `counting' and `refusing' do with it). Both decisions are counted, under `serve' and
%% under `membership'.
%%
%% Which refusal comes back when both fail: the delegation's. A provider with neither is told what it can fix in its
%% own record first (`rpc/serve'), and what it lacks in the realm — its membership — is what it then finds on the
%% next attempt. The membership decision is still taken and counted in that case, so the counters do not depend on
%% which one was reported.
-spec serve(mode(), <<_:256>>, binary(), ok | {error, atom()}, membership()) -> allow | {refuse, refusal()}.
serve(off, _Realm, _Procedure, _Verdict, _Membership) ->
    allow;
serve(members, Realm, Procedure, Verdict, Membership) when is_binary(Procedure) ->
    both(decided(members, serve, Realm, #{can => ?SERVE_CAN, resource => Procedure}, Verdict),
         membership(members, Realm, Membership));
serve(Mode, Realm, Procedure, Verdict, Membership) when is_binary(Procedure) ->
    Delegation = decided(Mode, serve, Realm, #{can => ?SERVE_CAN, resource => Procedure}, Verdict),
    _ = membership(Mode, Realm, Membership),
    Delegation.

both(allow, allow) -> allow;
both({refuse, _} = Refused, _Allowed) -> Refused;
both(allow, {refuse, _} = Refused) -> Refused.

%% @doc Whether a frame from a link holding Membership may act in Realm, in Mode (macula-realm#175, threat 1): the
%% check a realm closes on. In `members' a live membership is the whole decision and no capability is read: a member
%% is allowed, and anything else is refused naming `realm/member' and the reason, without reading a grant.
%%
%% In `counting' and `refusing' it always allows, and counts what members mode would have decided as
%% `{Realm, membership, member | would_refuse}' and, when it would have refused, the reason as its own outcome — the
%% counter threat 6 is read on before a realm is closed, kept apart from the capability decision's `no_token' and
%% `not_granted'. `off' does nothing.
%%
%% Only the five reasons in `membership_reason/0' are accepted in `members': any other atom in `not_member' raises,
%% rather than being put on the wire as a refusal whose reason a peer cannot name. A term that is neither `{member,
%% Rights}' nor `{not_member, Reason}' does not match any clause, so it raises in every mode; in `counting' that is
%% the first thing that happens, to the caller, rather than a counter nobody can explain.
-spec membership(mode(), <<_:256>>, membership()) -> allow | {refuse, refusal()}.
membership(off, _Realm, _Membership) ->
    allow;
membership(members, Realm, {member, _Rights}) ->
    count(Realm, membership, authorised),
    allow;
membership(members, Realm, {not_member, Reason})
  when Reason =:= no_token; Reason =:= expired; Reason =:= not_the_audience;
       Reason =:= wrong_realm; Reason =:= invalid ->
    count(Realm, membership, refused),
    count(Realm, membership, Reason),
    {refuse, #{can => ?MEMBER_CAN, resource => Realm, reason => Reason}};
membership(Mode, Realm, Membership) when Mode =:= counting; Mode =:= refusing ->
    count(Realm, membership, would_be_member(Membership)),
    count_membership_reason(Realm, Membership),
    allow.

would_be_member({member, _Rights}) -> member;
would_be_member({not_member, _Reason}) -> would_refuse.

count_membership_reason(Realm, {not_member, Reason}) -> count(Realm, membership, Reason);
count_membership_reason(_Realm, {member, _Rights}) -> ok.

%% @doc Why a frame from a link is not from a live member, from what admit/2 answered (macula-realm#175, WP3): a
%% token another realm minted is `wrong_realm' — its issuer is not this realm's key (`not_the_issuer'), or a
%% capability it grants names another realm (`wrong_realm') — one minted for another node's link is
%% `not_the_audience', one past its exp is `expired', and every other refusal is `invalid': `malformed',
%% `signature_invalid', `not_yet_valid', `realm_name_not_canonical' (a malformed realm NAME in a capability says
%% nothing about which realm the token belongs to) and the capability-side refusals. The refusal's own atom stays in
%% the refusal's reason, and in the log line of whoever reported it.
-spec membership_reason(atom()) -> membership_reason().
membership_reason(not_the_audience) -> not_the_audience;
membership_reason(expired) -> expired;
membership_reason(not_the_issuer) -> wrong_realm;
membership_reason(wrong_realm) -> wrong_realm;
membership_reason(_Other) -> invalid.

decided(_Mode, Check, Realm, _Asked, ok) ->
    count(Realm, Check, authorised),
    allow;
decided(counting, Check, Realm, _Asked, {error, _Reason}) ->
    count(Realm, Check, would_refuse),
    allow;
decided(Mode, Check, Realm, Asked, {error, Reason}) when Mode =:= refusing; Mode =:= members ->
    count(Realm, Check, refused),
    {refuse, Asked#{reason => Reason}}.

%% @doc The mode Realm runs its checks in: the `realm_rights' application environment of macula, a map from realm id
%% to mode with an optional `default', or `counting' when it names neither.
-spec mode(<<_:256>>) -> mode().
mode(<<_:256>> = Realm) ->
    Modes = application:get_env(macula, realm_rights, #{}),
    known_mode(maps:get(Realm, Modes, maps:get(default, Modes, counting))).

known_mode(Mode) when Mode =:= off; Mode =:= counting; Mode =:= members; Mode =:= refusing -> Mode.

%% @doc Every decision counted since the table was created, keyed by realm (or `other'), check and outcome. A
%% `membership' row also carries the outcome members mode only counts while it is counting (`member', `would_refuse')
%% and the reason a frame would have been refused for (`no_token', `expired', `not_the_audience', `wrong_realm',
%% `invalid'). Empty when there is no table to read, which is what a caller finds while the peering run that owns it
%% is not up (macula#186).
-spec counters() -> #{{<<_:256>> | other, check(), outcome() | member_outcome() | membership_reason()} =>
                          non_neg_integer()}.
counters() ->
    counters_in(ets:whereis(?TABLE)).

%% The counters belong to the peering run: `macula_peering_sup' creates the table and its life is the run's. A caller
%% can outlive it -- the station's peer observer is in the station's own tree, not in macula's, and is still handed
%% events while a station shuts down -- and then there is no table and no decision in it. That is not an error to
%% fail the caller with: nothing reads a counter back on the decision's path, so bookkeeping that cannot be written
%% must not take the caller down (macula#186). `ets:whereis/1' is the same reading `macula_provider_standing' and
%% `macula_diagnostics_bound' make of a table that may not be there.
count(Realm, Check, Outcome) ->
    count_in(ets:whereis(?TABLE), Realm, Check, Outcome).

counters_in(undefined) -> #{};
counters_in(Table) ->
    maps:from_list([Row || {{_Realm, _Check, _Outcome}, _} = Row <- ets:tab2list(Table)]).

count_in(undefined, _Realm, _Check, _Outcome) ->
    ok;
count_in(Table, Realm, Check, Outcome) ->
    Key = {realm_label(Table, ets:member(Table, {realm, Realm}), Realm), Check, Outcome},
    _ = ets:update_counter(Table, Key, 1, {Key, 0}),
    ok.

%% A realm already counted keeps its label; a new one takes one while fewer than 256 realms have one.
realm_label(_Table, true, Realm) -> Realm;
realm_label(Table, false, Realm) ->
    labelled(Table, ets:update_counter(Table, realm_labels, 1, {realm_labels, 0}), Realm).

labelled(Table, N, Realm) when N =< ?MAX_REALM_LABELS ->
    true = ets:insert(Table, {{realm, Realm}, labelled}),
    Realm;
labelled(_Table, _N, _Realm) ->
    ?OTHER.
