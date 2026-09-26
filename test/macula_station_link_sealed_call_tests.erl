%% A provider serves a sealed CALL (E2E design §5.1, Amendment A1): the link
%% opens it with its node's KEM keyring, runs the handler on the plaintext,
%% and seals whatever it answers (a RESULT, a handler's error, an unknown
%% procedure, an unauthorized caller) under the call's reply key. Only the
%% closed set of admission refusals goes out in the clear, and
%% `sealed_refused' names the key the node holds now.
-module(macula_station_link_sealed_call_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"test">>)).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).

%% The handler gets the plaintext payload with the verified caller, and its
%% answer comes back sealed: the RESULT carries no payload a station reads.
a_sealed_call_is_opened_served_and_answered_sealed_test_() ->
    {timeout, 10, fun() ->
        Echo = fun(Args) -> #{city => macula:field(city, Args), caller => macula:field(caller, Args)} end,
        {Pid, CallerKey} = fixture([{<<"_test.echo">>, Echo}]),
        {Frame, Call} = sealed_call(Pid, CallerKey, <<"_test.echo">>, #{city => {text, <<"Tienen">>}}),
        Pid ! {macula_peering, frame, self(), Frame},
        {result, Fields} = await_reply(Frame),
        ?assertNot(is_map_key(payload, Fields)),
        {ok, Payload} = opened(result, Fields, Call),
        ?assertEqual({text, <<"Tienen">>}, maps:get({text, <<"city">>}, Payload)),
        ?assertEqual(macula_node_keys:key_id(CallerKey), maps:get({text, <<"caller">>}, Payload)),
        macula_station_link:stop(Pid)
    end}.

%% A handler's error is sealed too: a station sees that a reply is an error,
%% never its code or detail.
a_handlers_error_is_sealed_test_() ->
    {timeout, 10, fun() ->
        {Pid, CallerKey} = fixture([{<<"_test.fail">>, fun(_) -> {error, no_such_city} end}]),
        {Frame, Call} = sealed_call(Pid, CallerKey, <<"_test.fail">>, #{}),
        Pid ! {macula_peering, frame, self(), Frame},
        {error, Fields} = await_reply(Frame),
        ?assertNot(is_map_key(code, Fields)),
        ?assertMatch({ok, #{code := <<"handler_error">>}}, opened(error, Fields, Call)),
        macula_station_link:stop(Pid)
    end}.

%% A sealed call to a procedure this node does not serve is answered sealed:
%% `unknown_next_peer' is not an admission refusal.
an_unknown_procedure_is_answered_sealed_test_() ->
    {timeout, 10, fun() ->
        {Pid, CallerKey} = fixture([]),
        {Frame, Call} = sealed_call(Pid, CallerKey, <<"_test.nobody">>, #{}),
        Pid ! {macula_peering, frame, self(), Frame},
        {error, Fields} = await_reply(Frame),
        ?assertMatch({ok, #{code := <<"unknown_next_peer">>}}, opened(error, Fields, Call)),
        macula_station_link:stop(Pid)
    end}.

%% A call sealed to a key the node no longer holds is refused in the clear,
%% naming the node's current key id, and its handler never runs.
a_call_to_another_key_is_refused_naming_the_current_key_test_() ->
    {timeout, 10, fun() ->
        Test = self(),
        {Pid, CallerKey} = fixture([{<<"_test.echo">>, fun(_) -> Test ! handler_ran, #{} end}]),
        {Frame, _Call} = sealed_call(Pid, CallerKey, <<"_test.echo">>, #{}, macula_seal:generate_key(profile())),
        Pid ! {macula_peering, frame, self(), Frame},
        {ok, #{key_id := Current}} = macula_kem_keyring:current(link_node_id(Pid)),
        {error, #{code := Code, detail := Detail}} = await_reply(Frame),
        ?assertEqual(<<"sealed_refused">>, Code),
        ?assertEqual(binary:encode_hex(Current, lowercase), Detail),
        ?assertEqual(none, receive handler_ran -> ran after 200 -> none end),
        macula_station_link:stop(Pid)
    end}.

%% A node that holds no KEM key refuses a sealed call in the clear, as
%% before, and runs nothing.
a_node_without_a_key_refuses_a_sealed_call_test_() ->
    {timeout, 10, fun() ->
        Test = self(),
        {Pid, CallerKey} = fixture([{<<"_test.echo">>, fun(_) -> Test ! handler_ran, #{} end}], no_key),
        {Frame, _Call} = sealed_call(Pid, CallerKey, <<"_test.echo">>, #{}, macula_seal:generate_key(profile())),
        Pid ! {macula_peering, frame, self(), Frame},
        ?assertMatch({error, #{code := <<"sealed_refused">>}}, await_reply(Frame)),
        ?assertEqual(none, receive handler_ran -> ran after 200 -> none end),
        macula_station_link:stop(Pid)
    end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

fixture(Handlers) ->
    fixture(Handlers, with_key).

fixture(Handlers, KeyOrNot) ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                                connect_timeout_ms => 2000})),
    {ok, PeerKey} = macula_node_keys:generate(identity, profile()),
    {ok, PeerNodeId} = macula_node_keys:node_id(PeerKey),
    Self = self(),
    _ = sys:replace_state(Pid, fun(S) ->
        setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Self), PeerNodeId)
    end),
    [ok = macula_station_link:advertise(Pid, ?REALM, Proc, Fun, open) || {Proc, Fun} <- Handlers],
    ok = keyed(KeyOrNot, link_node_id(Pid)),
    {Pid, PeerKey}.

keyed(with_key, NodeId) -> macula_kem_keyring:ensure(NodeId, profile());
keyed(no_key, _NodeId) -> ok.

with_link_keys(Opts) ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
          share => {seed, {<<"127.0.0.1">>, 1}}, expected_node_id => <<1:256>>}.

link_node_id(Pid) ->
    Key = element(macula_station_link:state_field_index(node_identity), sys:get_state(Pid)),
    macula_node_keys:key_id(Key).

%% A CALL sealed to the link node's current key, and what the caller keeps to
%% open the reply.
sealed_call(Pid, CallerKey, Procedure, Payload) ->
    {ok, #{key := Carried}} = macula_kem_keyring:current(link_node_id(Pid)),
    {ok, _Profile, Public} = macula_seal:public_key(Carried),
    sealed_call(Pid, CallerKey, Procedure, Payload, {Public, unused}).

sealed_call(Pid, CallerKey, Procedure, Payload, {Public, _Private}) ->
    Spec = #{request_id => crypto:strong_rand_bytes(16), realm => ?REALM, procedure => Procedure,
             target => link_node_id(Pid), deadline => erlang:system_time(millisecond) + 5_000},
    SealRequest = #{frame_type => <<"call">>, realm => ?REALM, procedure => Procedure,
                    caller => macula_node_keys:key_id(CallerKey), target => link_node_id(Pid),
                    request_id => maps:get(request_id, Spec), deadline => maps:get(deadline, Spec)},
    {ok, Plain} = macula_frame:payload_plain(Payload),
    {Sealed, Keys} = macula_sealed_call:seal_request(profile(), Public, SealRequest, Plain),
    Frame = macula_frame:call(Spec#{sealed => Sealed}, CallerKey),
    {ok, #{request_hash := RequestHash}} = macula_frame:verify_request(Frame, profile()),
    {Frame, #{keys => Keys, request => SealRequest, request_hash => RequestHash}}.

%% The reply to CallFrame, verified against its request.
await_reply(CallFrame) ->
    {ok, Request} = macula_frame:verify_request(CallFrame, profile()),
    RequestId = maps:get(request_id, Request),
    receive
        {'$gen_cast', {send_frame, _, #{frame_type := Type} = Reply}} when Type =:= result; Type =:= error ->
            {ok, #{request_id := RequestId}} = macula_frame:claimed_reply_ids(Reply),
            {ok, Fields} = macula_frame:verify_reply(Reply, Request, profile()),
            {Type, Fields}
    after 3_000 -> error(no_reply)
    end.

%% A sealed reply's plaintext, opened as the caller does.
opened(Type, #{sealed := Sealed, responded_by := RespondedBy},
       #{keys := Keys, request := SealRequest, request_hash := RequestHash}) ->
    Reply = #{frame_type => atom_to_binary(Type), request_hash => RequestHash, responded_by => RespondedBy},
    {ok, Plain} = macula_sealed_call:open_reply(Keys, SealRequest, Reply, Sealed),
    plain(Type, Plain).

plain(result, Plain) -> macula_frame:plain_payload(Plain);
plain(error, Plain) -> macula_frame:plain_error(Plain).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
