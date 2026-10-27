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
%% Every check runs in one of three modes per realm (`mode/1'): `off' checks nothing, `counting' counts what it would
%% refuse and lets it through, and `refusing' refuses it. The default is `counting', so a realm refuses only once its
%% operator has seen its counters read zero for authorised traffic and set it to `refusing'. A realm in counting mode
%% is open by design and must be shown as counting, never as protected (threat model 6.7).
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
%% The counter table belongs to macula_peering_sup, which creates it with new_table/0, as macula_wire_tolerance's is.
-module(macula_realm_rights).

-export([new_table/0, admit/2, subscribe/4, publish/4, serve/4, mode/1, counters/0,
         max_token_bytes/0, max_proofs/0, max_presented_bytes/0]).

-export_type([rights/0, presented/0, mode/0, check/0, outcome/0, refusal/0]).

-define(TABLE, ?MODULE).
-define(SUBSCRIBE_CAN, <<"pubsub/subscribe">>).
-define(PUBLISH_CAN, <<"pubsub/publish">>).
-define(SERVE_CAN, <<"rpc/serve">>).
%% A membership token carries at most 1 + 8 x 16 capabilities (macula-realm#97 requirement 5): about 12 KB of
%% capabilities, base64url, plus a did:key and an ML-DSA-87 signature, about 26 KB in all. 48 KiB leaves room for
%% the composite profile's larger key and signature and still refuses a flood of large tokens before any verify.
-define(MAX_TOKEN_BYTES, 49152).
%% A delegated client's chain is the realm's token and a hop or two; four proofs is more than any chain the realm
%% issues today.
-define(MAX_PROOFS, 4).
-define(MAX_PRESENTED_BYTES, 131072).
%% A realm id comes from a peer's frame: past 256 realms with a row, a new one counts under the label other.
-define(MAX_REALM_LABELS, 256).
-define(OTHER, other).

-type mode() :: off | counting | refusing.
-type check() :: subscribe | publish | serve.
-type outcome() :: authorised | would_refuse | refused | exempt.
%% What a member presented on its link: its membership token and the proofs of its chain, each as it travelled.
-type presented() :: #{token := binary(), proofs := [binary()]}.
%% What admit/2 keeps for a link: the capabilities the token grants, the second it stops authorizing, and the member
%% the realm endorsed at its chain's root.
-type rights() :: #{caps := [map()], until := integer(), member := <<_:256>>}.
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
    sized(within_size(Presented), Token, Proofs,
          #{caller => Peer, profile => Profile, now => Now,
            proofs => maps:from_list([{macula_ucan:proof_id(P), P} || P <- Proofs])},
          {realm_member_required, RealmKey, any});
admit(_Presented, _Context) ->
    {error, malformed}.

within_size(#{token := Token, proofs := Proofs}) ->
    byte_size(Token) =< ?MAX_TOKEN_BYTES andalso length(Proofs) =< ?MAX_PROOFS
        andalso lists:all(fun(P) -> is_binary(P) andalso byte_size(P) =< ?MAX_TOKEN_BYTES end, Proofs)
        andalso byte_size(Token) + lists:sum([byte_size(P) || P <- Proofs]) =< ?MAX_PRESENTED_BYTES.

sized(false, _Token, _Proofs, _Context, _Policy) ->
    {error, too_large};
sized(true, Token, Proofs, Context, Policy) ->
    admitted(macula_ucan:authorize(Token, Policy, Context), Token, Proofs).

admitted({ok, #{<<"cap">> := Caps}}, Token, Proofs) ->
    {ok, #{caps => Caps, until => macula_ucan:authorized_until(Token, Proofs),
           member => macula_ucan:root_audience(Token, Proofs)}};
admitted({error, _} = Refusal, _Token, _Proofs) ->
    Refusal.

%% @doc Whether the link may subscribe to Pattern in Realm, in Mode, on the rights admit/2 kept for it (`none' when
%% it presented no token, or its token stopped authorizing). A grant must cover every topic the pattern can match
%% (threat model 4.4, macula_ucan:grants/4). `allow' lets the SUBSCRIBE through; `{refuse, Refusal}' refuses it
%% not_authorized, naming the capability and the pattern.
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
topic_verdict(false, Check, Can, Mode, Realm, Rights, Topic, Request) ->
    decided(Mode, Check, Realm, #{can => Can, resource => Topic}, held(Rights, Can, Realm, Request)).

held(none, _Can, _Realm, _Request) -> {error, no_token};
held(#{caps := Caps}, Can, Realm, Request) -> macula_ucan:grants(Caps, Can, Realm, Request).

%% @doc Whether a provider may serve Procedure in Realm, in Mode, given what the caller or the station read from its
%% delegation (macula_record:delegated_procedure/3): `ok' when the delegation names it, `{error, Reason}' when it does
%% not or names no procedures at all. Counted like the other checks.
-spec serve(mode(), <<_:256>>, binary(), ok | {error, atom()}) -> allow | {refuse, refusal()}.
serve(off, _Realm, _Procedure, _Verdict) ->
    allow;
serve(Mode, Realm, Procedure, Verdict) when is_binary(Procedure) ->
    decided(Mode, serve, Realm, #{can => ?SERVE_CAN, resource => Procedure}, Verdict).

decided(_Mode, Check, Realm, _Asked, ok) ->
    count(Realm, Check, authorised),
    allow;
decided(counting, Check, Realm, _Asked, {error, _Reason}) ->
    count(Realm, Check, would_refuse),
    allow;
decided(refusing, Check, Realm, Asked, {error, Reason}) ->
    count(Realm, Check, refused),
    {refuse, Asked#{reason => Reason}}.

%% @doc The mode Realm runs its checks in: the `realm_rights' application environment of macula, a map from realm id
%% to mode with an optional `default', or `counting' when it names neither.
-spec mode(<<_:256>>) -> mode().
mode(<<_:256>> = Realm) ->
    Modes = application:get_env(macula, realm_rights, #{}),
    known_mode(maps:get(Realm, Modes, maps:get(default, Modes, counting))).

known_mode(Mode) when Mode =:= off; Mode =:= counting; Mode =:= refusing -> Mode.

%% @doc Every decision counted since the table was created, keyed by realm (or `other'), check and outcome.
-spec counters() -> #{{<<_:256>> | other, check(), outcome()} => non_neg_integer()}.
counters() ->
    maps:from_list([Row || {{_Realm, _Check, _Outcome}, _} = Row <- ets:tab2list(?TABLE)]).

count(Realm, Check, Outcome) ->
    Key = {realm_label(ets:member(?TABLE, {realm, Realm}), Realm), Check, Outcome},
    _ = ets:update_counter(?TABLE, Key, 1, {Key, 0}),
    ok.

%% A realm already counted keeps its label; a new one takes one while fewer than 256 realms have one.
realm_label(true, Realm) -> Realm;
realm_label(false, Realm) ->
    labelled(ets:update_counter(?TABLE, realm_labels, 1, {realm_labels, 0}), Realm).

labelled(N, Realm) when N =< ?MAX_REALM_LABELS ->
    true = ets:insert(?TABLE, {{realm, Realm}, labelled}),
    Realm;
labelled(_N, _Realm) ->
    ?OTHER.
