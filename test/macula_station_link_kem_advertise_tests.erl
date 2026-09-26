%% A provider's link names its node's KEM key in every advertisement a spec
%% marks `kem', reading the keyring's current key each time it signs, so a
%% rotated key reaches the next renewal (E2E design, Amendment A1). And it
%% refuses a clear CALL, in the clear, as `sealed_required', to a procedure
%% whose spec says `required', and to one that names a key once its last
%% keyless advertisement can no longer be served (design §8.1, "Opting in").
-module(macula_station_link_kem_advertise_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"test">>)).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).

%%------------------------------------------------------------------
%% What an advertisement is signed with
%%------------------------------------------------------------------

%% A spec marked `kem' signs with the keyring's current key, and a rotation
%% reaches the next signing.
a_kem_spec_signs_with_the_current_key_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    NodeId = macula_node_keys:key_id(Key),
    #{kem_key := First} = macula_station_link:advertisement_opts(#{kem => true, ttl_ms => 60_000}, Key, profile()),
    ?assertEqual({ok, #{key => First, key_id => macula_seal:key_id(First)}}, macula_kem_keyring:current(NodeId)),
    ok = macula_kem_keyring:rotate(NodeId),
    #{kem_key := Second, ttl_ms := 60_000} =
        macula_station_link:advertisement_opts(#{kem => true, ttl_ms => 60_000}, Key, profile()),
    ?assertNotEqual(First, Second).

%% A spec not marked `kem' signs with no key, and carries only what the
%% record builder takes.
a_keyless_spec_names_no_key_test() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    ?assertEqual(#{ttl_ms => 60_000},
                 macula_station_link:advertisement_opts(#{ttl_ms => 60_000, confidential => required}, Key, profile())).

%%------------------------------------------------------------------
%% Clear calls
%%------------------------------------------------------------------

%% `required' refuses a clear call in the clear, and runs nothing.
required_refuses_a_clear_call_test_() ->
    {timeout, 10, fun() ->
        Test = self(),
        {Pid, CallerKey, Proc} = fixture(#{kem => true, confidential => required}, fun(_) -> Test ! ran, #{} end),
        Frame = clear_call(Pid, CallerKey, Proc),
        ?assertMatch({error, #{code := <<"sealed_required">>}}, await_reply(Frame)),
        ?assertEqual(none, receive ran -> ran after 200 -> none end),
        macula_station_link:stop(Pid)
    end}.

%% A procedure that names a key still serves a clear call while its last
%% keyless advertisement can be served ...
a_newly_keyed_procedure_still_serves_clear_calls_test_() ->
    {timeout, 10, fun() ->
        {Pid, CallerKey, Proc} = fixture(#{kem => true}, fun(_) -> #{ok => 1} end),
        ?assertMatch({ok, _}, await_reply(clear_call(Pid, CallerKey, Proc))),
        macula_station_link:stop(Pid)
    end}.

%% ... and refuses one once it cannot.
a_keyed_procedure_refuses_clear_calls_after_the_window_test_() ->
    {timeout, 10, fun() ->
        {Pid, CallerKey, Proc} = fixture(#{kem => true}, fun(_) -> #{ok => 1} end),
        Window = macula_record:procedure_advertisement_max_lifetime_ms() + macula_record:clock_tolerance_ms(),
        Index = macula_station_link:state_field_index(keyed_since),
        _ = sys:replace_state(Pid, fun(S) ->
                Since = element(Index, S),
                setelement(Index, S, maps:map(fun(_Key, At) -> At - Window - 1 end, Since))
            end),
        ?assertMatch({error, #{code := <<"sealed_required">>}}, await_reply(clear_call(Pid, CallerKey, Proc))),
        macula_station_link:stop(Pid)
    end}.

%% A keyless procedure serves clear calls as ever.
a_keyless_procedure_serves_clear_calls_test_() ->
    {timeout, 10, fun() ->
        {Pid, CallerKey, Proc} = fixture(#{}, fun(_) -> #{ok => 1} end),
        ?assertMatch({ok, _}, await_reply(clear_call(Pid, CallerKey, Proc))),
        macula_station_link:stop(Pid)
    end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A link advertising one own-namespace procedure with Spec, its caller's key
%% and the procedure.
fixture(Spec, Handler) ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                                connect_timeout_ms => 2000})),
    {ok, PeerKey} = macula_node_keys:generate(identity, profile()),
    {ok, PeerNodeId} = macula_node_keys:node_id(PeerKey),
    Self = self(),
    _ = sys:replace_state(Pid, fun(S) ->
        setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Self), PeerNodeId)
    end),
    Proc = <<"~", (binary:encode_hex(link_node_id(Pid), lowercase))/binary, "/ring">>,
    ok = macula_station_link:advertise(Pid, ?REALM, Proc, Handler, open, Spec),
    {Pid, PeerKey, Proc}.

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

clear_call(Pid, CallerKey, Proc) ->
    Frame = macula_frame:call(#{request_id => crypto:strong_rand_bytes(16), realm => ?REALM, procedure => Proc,
                                target => link_node_id(Pid), deadline => erlang:system_time(millisecond) + 5_000,
                                payload => #{}}, CallerKey),
    Pid ! {macula_peering, frame, self(), Frame},
    Frame.

await_reply(CallFrame) ->
    {ok, Request} = macula_frame:verify_request(CallFrame, profile()),
    RequestId = maps:get(request_id, Request),
    receive
        {'$gen_cast', {send_frame, _, #{frame_type := Type} = Reply}} when Type =:= result; Type =:= error ->
            {ok, #{request_id := RequestId}} = macula_frame:claimed_reply_ids(Reply),
            {ok, Fields} = macula_frame:verify_reply(Reply, Request, profile()),
            replied(Type, Fields)
    after 3_000 -> error(no_reply)
    end.

replied(result, #{payload := Payload}) -> {ok, Payload};
replied(error, Fields) -> {error, Fields}.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
