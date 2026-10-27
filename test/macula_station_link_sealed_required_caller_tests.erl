%% A caller whose CLEAR CALL a keyed provider refuses `sealed_required' (DESIGN_E2E_SEALED_BY_DEFAULT §3, release A0).
%% The refusal is signed by the provider for this very request and bound to it, and its non-critical extension
%% `seal.kem_key' names the KEM key the provider holds now: the link seals the same payload to that key under a new
%% request id and sends it once more, at most once per call, reports the key, and tells the pool. A refusal that names
%% no key, or one of another profile's size, reaches the caller as the provider's own error.
-module(macula_station_link_sealed_required_caller_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"test">>)).
-define(PROCEDURE, <<"acme/echo_v1">>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(POOL_INDEX, macula_station_link:state_field_index(pool)).
-define(PENDING_INDEX, macula_station_link:state_field_index(pending)).

%% The provider refuses in the clear, naming the key it holds now; the link seals the same payload to it under a new
%% request id, and the sealed answer is the call's.
a_clear_call_refused_with_the_key_is_resealed_and_answered_test_() ->
    {timeout, 15, fun() ->
        #{link := Link} = F = fixture(),
        Caller = call_async(F, #{city => <<"Tienen">>}, clear, false),
        First = received(#{frame_type => call}),
        answer(F, First, refusal(F, First, maps:get(kem_key, F))),
        Second = received(#{frame_type => call}),
        ?assertNotEqual(request_id(First), request_id(Second)),
        ?assertEqual({ok, #{{text, <<"city">>} => <<"Tienen">>}}, macula_frame:plain_payload(plain(F, Second))),
        ?assertNot(is_map_key(payload, request(Second))),
        answer(F, Second, sealed_result(F, Second)),
        ?assertEqual({ok, #{{text, <<"temp">>} => 21}}, result(Caller)),
        macula_station_link:stop(Link)
    end}.

%% Asked for, the report names the reseal's key: it describes the exchange that answered, not the one refused.
a_resealed_call_reports_the_key_it_sealed_to_test_() ->
    {timeout, 15, fun() ->
        #{link := Link, provider_id := Target, kem_key := KemKey} = F = fixture(),
        Caller = call_async(F, #{}, clear, true),
        First = received(#{frame_type => call}),
        answer(F, First, refusal(F, First, KemKey)),
        Second = received(#{frame_type => call}),
        answer(F, Second, sealed_result(F, Second)),
        ?assertEqual({ok, #{{text, <<"temp">>} => 21},
                      #{sealed => 1, provider => Target, seal_key_id => macula_seal:key_id(KemKey)}},
                     result(Caller)),
        macula_station_link:stop(Link)
    end}.

%% At most once per call: the resealed call is sealed, and its own refusal (`sealed_refused', as a sealed call's is)
%% is the provider's answer. Nothing goes out a third time.
a_call_is_resealed_at_most_once_test_() ->
    {timeout, 15, fun() ->
        #{link := Link} = F = fixture(),
        Caller = call_async(F, #{}, clear, false),
        First = received(#{frame_type => call}),
        answer(F, First, refusal(F, First, maps:get(kem_key, F))),
        Second = received(#{frame_type => call}),
        answer(F, Second, clear_error(F, Second, <<"sealed_refused">>, binary:encode_hex(<<7:64>>, lowercase))),
        ?assertEqual({error, {sealed_refused, <<7:64>>}}, result(Caller)),
        ?assertEqual(none, next_call(300)),
        macula_station_link:stop(Link)
    end}.

%% At most once per call whatever the provider replays: the refused request is done with the moment the reseal is
%% drawn, so a second copy of the same signed refusal — signed, still, for the first request id — finds nothing
%% pending and no further CALL goes out. The entry that held the payload is gone with it, so the link keeps only the
%% call that is running.
a_replayed_refusal_does_not_reseal_again_test_() ->
    {timeout, 15, fun() ->
        #{link := Link} = F = fixture(),
        Caller = call_async(F, #{}, clear, false),
        First = received(#{frame_type => call}),
        Refusal = refusal(F, First, maps:get(kem_key, F)),
        answer(F, First, Refusal),
        Second = received(#{frame_type => call}),
        answer(F, First, Refusal),
        ?assertEqual(none, next_call(500)),
        ?assertEqual(1, map_size(element(?PENDING_INDEX, sys:get_state(Link)))),
        answer(F, Second, sealed_result(F, Second)),
        ?assertMatch({ok, _}, result(Caller)),
        ?assertEqual(0, map_size(element(?PENDING_INDEX, sys:get_state(Link)))),
        macula_station_link:stop(Link)
    end}.

%% No extension: the provider's own error reaches the caller, unchanged, and nothing is sent again.
a_refusal_without_the_key_is_the_providers_own_test_() ->
    {timeout, 15, fun() ->
        #{link := Link, provider := Provider} = F = fixture(),
        Caller = call_async(F, #{}, clear, false),
        First = received(#{frame_type => call}),
        answer(F, First, macula_frame:provider_error(#{request => request(First), code => <<"sealed_required">>,
                                                       detail => <<"this procedure takes sealed calls only">>},
                                                     Provider)),
        ?assertEqual({error, {call_error, <<"sealed_required">>, <<"this procedure takes sealed calls only">>}},
                     result(Caller)),
        ?assertEqual(none, next_call(300)),
        macula_station_link:stop(Link)
    end}.

%% A key of another profile's size seals nothing here: the refusal reaches the caller as the provider sent it.
a_key_of_another_profile_does_not_reseal_test_() ->
    {timeout, 15, fun() ->
        #{link := Link} = F = fixture(),
        {Public, _} = macula_seal:generate_key(other_profile(profile())),
        Caller = call_async(F, #{}, clear, false),
        First = received(#{frame_type => call}),
        answer(F, First, refusal(F, First, macula_seal:key_as_carried(Public))),
        ?assertEqual({error, {call_error, <<"sealed_required">>, undefined}}, result(Caller)),
        ?assertEqual(none, next_call(300)),
        macula_station_link:stop(Link)
    end}.

%% The pool is told the key for the provider it called, so its next call seals from the start.
a_reseal_teaches_the_pool_the_key_test_() ->
    {timeout, 15, fun() ->
        #{link := Link, provider_id := Target, kem_key := KemKey} = F = fixture(),
        Self = self(),
        _ = sys:replace_state(Link, fun(S) -> setelement(?POOL_INDEX, S, Self) end),
        ?assertEqual(Self, element(?POOL_INDEX, sys:get_state(Link))),
        Caller = call_async(F, #{}, clear, false),
        First = received(#{frame_type => call}),
        answer(F, First, refusal(F, First, KemKey)),
        ?assertEqual({learn_kem_key, ?REALM, ?PROCEDURE, Target, KemKey},
                     receive {'$gen_cast', {learn_kem_key, _, _, _, _} = Learned} -> Learned after 300 -> none end),
        Second = received(#{frame_type => call}),
        answer(F, Second, sealed_result(F, Second)),
        ?assertMatch({ok, _}, result(Caller)),
        macula_station_link:stop(Link)
    end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A link whose peer is this process, and a provider key, a KEM keypair and the KEM key as carried.
fixture() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Link} = macula_station_link:start_link(with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                                 connect_timeout_ms => 2000})),
    {ok, StationKey} = macula_node_keys:generate(identity, profile()),
    {ok, StationNodeId} = macula_node_keys:node_id(StationKey),
    Self = self(),
    _ = sys:replace_state(Link, fun(S) ->
        setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Self), StationNodeId)
    end),
    {ok, Provider} = macula_node_keys:generate(identity, profile()),
    {Public, Private} = macula_seal:generate_key(profile()),
    Carried = macula_seal:key_as_carried(Public),
    #{link => Link, provider => Provider, provider_id => macula_node_keys:key_id(Provider), kem_key => Carried,
      holder => #{current_key_id => macula_seal:key_id(Carried),
                  lookup => fun(_KeyId) -> {ok, Private, Carried} end}}.

with_link_keys(Opts) ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
          share => {seed, {<<"127.0.0.1">>, 1}}, expected_node_id => <<1:256>>}.

call_async(#{link := Link, provider_id := Target}, Payload, Seal, Report) ->
    Self = self(),
    spawn(fun() ->
              Self ! {call_result, self(),
                      macula_station_link:call(Link, Target, ?REALM, ?PROCEDURE, Payload, 5_000, <<>>, Seal, Report)}
          end).

result(Caller) ->
    receive {call_result, Caller, Result} -> Result after 5_000 -> error(no_result) end.

%% The next frame the link sent that the filter matches.
received(Filter) ->
    receive
        {'$gen_cast', {send_frame, _, Frame}} ->
            case lists:all(fun({Key, Value}) -> maps:get(Key, Frame, undefined) =:= Value end, maps:to_list(Filter)) of
                true -> Frame;
                false -> received(Filter)
            end
    after 3_000 -> error(no_frame_sent)
    end.

%% Whether the link sends another CALL within `Ms'.
next_call(Ms) ->
    receive {'$gen_cast', {send_frame, _, #{frame_type := call}}} -> sent after Ms -> none end.

request(Frame) ->
    {ok, Request} = macula_frame:verify_request(Frame, profile()),
    Request.

request_id(Frame) ->
    maps:get(request_id, request(Frame)).

%% A CLEAR `sealed_required' refusal of the CALL in `Frame', signed by the provider, carrying `KemKey'.
refusal(#{provider := Provider}, Frame, KemKey) ->
    macula_frame:provider_error(#{request => request(Frame), code => <<"sealed_required">>,
                                  ext => #{<<"seal.kem_key">> => #{critical => 0, value => KemKey}}}, Provider).

answer(#{link := Link}, _Frame, Reply) ->
    Link ! {macula_peering, frame, self(), Reply}.

%% A clear refusal the closed set admits, or `sealed_refused' naming the key the provider holds now.
clear_error(#{provider := Provider}, Frame, Code, Detail) ->
    macula_frame:provider_error(#{request => request(Frame), code => Code, detail => Detail}, Provider).

%% The plaintext of a sealed CALL the link sent, opened as the provider opens it.
plain(#{holder := Holder}, Frame) ->
    Request = request(Frame),
    {ok, Plain, _Keys} = macula_sealed_call:open_request(profile(), Holder, seal_request(Request),
                                                         maps:get(sealed, Request)),
    Plain.

sealed_result(#{provider := Provider, holder := Holder}, Frame) ->
    Request = request(Frame),
    {ok, _Plain, Keys} = macula_sealed_call:open_request(profile(), Holder, seal_request(Request),
                                                         maps:get(sealed, Request)),
    {ok, Answer} = macula_frame:payload_plain(#{temp => 21}),
    Sealed = macula_sealed_call:seal_reply(Keys, seal_request(Request), reply(Request), Answer),
    macula_frame:result(#{request => Request, sealed => Sealed}, Provider).

seal_request(#{frame_type := Type, realm := Realm, procedure := Procedure, caller := Caller, target := Target,
               request_id := RequestId, deadline := Deadline}) ->
    #{frame_type => atom_to_binary(Type), realm => Realm, procedure => Procedure, caller => Caller,
      target => Target, request_id => RequestId, deadline => Deadline}.

reply(#{request_hash := RequestHash, target := Provider}) ->
    #{frame_type => <<"result">>, request_hash => RequestHash, responded_by => Provider}.

other_profile(pq_pure) -> pq_hybrid;
other_profile(pq_hybrid) -> pq_pure.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
