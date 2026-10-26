%% @doc Read control on a restricted sealed group, end to end (macula#129): a member's keyring pulls the group's keys
%% through a real provider link, which hands the distributor's handler the call's own token (`handler_authorizes'),
%% and the distributor admits the realm's `pubsub/subscribe' on the whole group. A member with the role joins and
%% opens the group's events; one without it, holding only the org's grant, is answered `not_authorized' and opens
%% nothing. No station sits between them: the link is fed the member's sealed CALL frames as a station relays them.
-module(macula_group_read_control_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([a_member_with_the_role_opens_the_groups_events_and_one_without_opens_nothing/1]).

-define(REALM_NAME, <<"io.example">>).
-define(GROUP, <<"io.example/acme/orders">>).
-define(TOPIC, <<"io.example/acme/orders/placed_v1">>).
-define(PROCEDURE, <<"acme/group_keys_v1">>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).

all() ->
    [a_member_with_the_role_opens_the_groups_events_and_one_without_opens_nothing].

init_per_suite(Cfg) ->
    {ok, _} = application:ensure_all_started(macula),
    Cfg.

end_per_suite(_Cfg) -> ok.

a_member_with_the_role_opens_the_groups_events_and_one_without_opens_nothing(_Cfg) ->
    Realm = crypto:hash(sha256, ?REALM_NAME),
    {ok, RealmKey} = macula_node_keys:generate(realm, profile()),
    {ok, OrgKey} = macula_node_keys:generate(org, profile()),
    {ok, Distributor} = macula_group_keys:start_link(
                          #{org => <<"acme">>, policy => required, org_key_id => macula_node_keys:key_id(OrgKey),
                            realm => Realm, profile => profile(),
                            groups => #{?GROUP => {realm_capability, macula_node_keys:key_id(RealmKey), refusing}},
                            membership => fun(_Caller) -> ok end}),
    {Link, Relay} = provider_link(Realm, macula_group_keys:handler(Distributor)),
    {ok, Reader} = macula_node_keys:generate(identity, profile()),
    {ok, Outsider} = macula_node_keys:generate(identity, profile()),
    ReaderRing = keyring(Link, Relay, Reader),
    OutsiderRing = keyring(Link, Relay, Outsider),
    Role = grant(RealmKey, Reader, <<"pubsub/subscribe">>, [<<"mri:topic:", ?GROUP/binary>>,
                                                            <<"mri:topic:", ?GROUP/binary, "/*">>]),
    OrgGrant = grant(OrgKey, Outsider, <<"group_keys">>, [<<"mri:org:io.example/acme">>]),

    %% The member with the role joins, and an event sealed under the group's current epoch opens for it.
    {ok, required} = macula_group_keyring:join(ReaderRing, Realm, ?GROUP, #{ucan_token => Role}),
    {ok, #{id := Id} = Epoch} = macula_group_keyring:publish_epoch(ReaderRing, Realm, ?GROUP),
    Publisher = macula_node_keys:key_id(Reader),
    Fields = #{realm => Realm, topic => ?TOPIC, publisher => Publisher, seq => 1,
               published_at => erlang:system_time(millisecond)},
    {ok, Sealed} = macula_group_event:seal(Epoch, Fields, {text, <<"order 42">>}),
    {ok, Opener} = macula_group_keyring:open_epoch(ReaderRing, Realm, ?GROUP, Id, Publisher),
    {ok, {text, <<"order 42">>}} = macula_group_event:open(Opener, Fields#{sealed => Sealed}),

    %% The org's grant alone is not the role: the join is refused, so the outsider holds no key to open the event with.
    {error, not_authorized} = macula_group_keyring:join(OutsiderRing, Realm, ?GROUP, #{ucan_token => OrgGrant}),
    {error, not_joined} = macula_group_keyring:open_epoch(OutsiderRing, Realm, ?GROUP, Id, Publisher),
    none = macula_group_keyring:policy(OutsiderRing, Realm, ?TOPIC),

    macula_station_link:stop(Link),
    unlink(Relay),
    exit(Relay, shutdown),
    [begin unlink(P), exit(P, shutdown) end || P <- [Distributor, macula_group_keyring:pid(ReaderRing),
                                                     macula_group_keyring:pid(OutsiderRing)]],
    ok.

%%---------------------------------------------------------------------
%% The provider side: a real link serving the distributor
%%---------------------------------------------------------------------

%% A link serving the distributor's handler under the distributor's own advertise policy, whose peer is a relay
%% process standing in for the station: it hands the link each member's CALL and the member the link's reply.
provider_link(Realm, Handler) ->
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                                connect_timeout_ms => 2000})),
    {ok, PeerKey} = macula_node_keys:generate(identity, profile()),
    {ok, PeerNodeId} = macula_node_keys:node_id(PeerKey),
    Relay = spawn_link(fun() -> relay(Pid, #{}) end),
    _ = sys:replace_state(Pid, fun(S) ->
        setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Relay), PeerNodeId)
    end),
    #{auth := Policy} = macula_group_keys:advertise_opts(),
    ok = macula_station_link:advertise(Pid, Realm, ?PROCEDURE, Handler, Policy),
    ok = macula_kem_keyring:ensure(link_node_id(Pid), profile()),
    {Pid, Relay}.

relay(Link, Waiting) ->
    receive
        {call, From, Frame, #{request_id := RequestId} = Request} ->
            Link ! {macula_peering, frame, self(), Frame},
            relay(Link, Waiting#{RequestId => {From, Request}});
        {'$gen_cast', {send_frame, _, #{frame_type := Type} = Reply}} when Type =:= result; Type =:= error ->
            {ok, #{request_id := RequestId}} = macula_frame:claimed_reply_ids(Reply),
            {{From, Request}, Rest} = maps:take(RequestId, Waiting),
            {ok, Fields} = macula_frame:verify_reply(Reply, Request, profile()),
            From ! {relayed, {Type, Fields}},
            relay(Link, Rest);
        _Other ->
            relay(Link, Waiting)
    end.

with_link_keys(Opts) ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
          share => {seed, {<<"127.0.0.1">>, 1}}, expected_node_id => <<1:256>>}.

link_node_id(Pid) ->
    macula_node_keys:key_id(element(macula_station_link:state_field_index(node_identity), sys:get_state(Pid))).

%%---------------------------------------------------------------------
%% The member side: a real keyring whose pulls reach the link as sealed CALLs
%%---------------------------------------------------------------------

keyring(Link, Relay, MemberKey) ->
    Call = fun(Realm, Procedure, Payload, Opts) -> relayed(Relay, Link, MemberKey, Realm, Procedure, Payload, Opts) end,
    {ok, Pid} = macula_group_keyring:start_link(#{call => Call}),
    macula_group_keyring:handle(Pid).

%% The keyring's process makes the call through the relay and waits for the reply it relays back.
relayed(Relay, Link, MemberKey, Realm, Procedure, Payload, Opts) ->
    {Frame, #{verified := Request} = Call} =
        sealed_call(Link, MemberKey, Realm, Procedure, Payload, maps:get(ucan_token, Opts, <<>>)),
    Relay ! {call, self(), Frame, Request},
    receive {relayed, Reply} -> answered(Reply, Call) after 10_000 -> {error, timeout} end.

%% As macula:call/6 with `report' answers: the payload and that it came sealed from the distributor, or the
%% handler's own error text.
answered({result, Fields}, Call) ->
    {ok, Payload} = opened(result, Fields, Call),
    {ok, Payload, #{sealed => 1, provider => maps:get(responded_by, Fields)}};
answered({error, Fields}, Call) ->
    {ok, #{code := <<"handler_error">>, detail := Detail}} = opened(error, Fields, Call),
    {error, Detail}.

sealed_call(Link, CallerKey, Realm, Procedure, Payload, Token) ->
    {ok, #{key := Carried}} = macula_kem_keyring:current(link_node_id(Link)),
    {ok, _Profile, Public} = macula_seal:public_key(Carried),
    Spec = #{request_id => crypto:strong_rand_bytes(16), realm => Realm, procedure => Procedure,
             target => link_node_id(Link), deadline => erlang:system_time(millisecond) + 10_000, token => Token},
    SealRequest = #{frame_type => <<"call">>, realm => Realm, procedure => Procedure,
                    caller => macula_node_keys:key_id(CallerKey), target => link_node_id(Link),
                    request_id => maps:get(request_id, Spec), deadline => maps:get(deadline, Spec)},
    {ok, Plain} = macula_frame:payload_plain(Payload),
    {Sealed, Keys} = macula_sealed_call:seal_request(profile(), Public, SealRequest, Plain),
    Frame = macula_frame:call(Spec#{sealed => Sealed}, CallerKey),
    {ok, Request} = macula_frame:verify_request(Frame, profile()),
    {Frame, #{keys => Keys, request => SealRequest, verified => Request,
              request_hash => maps:get(request_hash, Request)}}.

opened(Type, #{sealed := Sealed, responded_by := RespondedBy},
       #{keys := Keys, request := SealRequest, request_hash := RequestHash}) ->
    Reply = #{frame_type => atom_to_binary(Type), request_hash => RequestHash, responded_by => RespondedBy},
    {ok, Plain} = macula_sealed_call:open_reply(Keys, SealRequest, Reply, Sealed),
    plain(Type, Plain).

plain(result, Plain) -> macula_frame:plain_payload(Plain);
plain(error, Plain) -> macula_frame:plain_error(Plain).

%%---------------------------------------------------------------------
%% The world under test
%%---------------------------------------------------------------------

grant(IssuerKey, MemberKey, Can, Withs) ->
    macula_ucan:create(IssuerKey, macula_node_keys:key_id(MemberKey), [#{with => W, can => Can} || W <- Withs],
                       #{exp => erlang:system_time(second) + 3600}).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
