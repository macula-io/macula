%% A caller's link seals a CALL to the provider's KEM key and opens the sealed
%% reply (E2E design §5.1). The provider is played here by this test process:
%% it opens what the link sent and answers sealed, or in the clear.
%%
%% Against a sealed request, a clear provider error is accepted only from the
%% closed set of admission refusals; `sealed_refused' comes back as
%% `{sealed_refused, KeyId}', naming the key the provider holds now. Any other
%% clear answer is refused as the design says, and the call runs to its
%% deadline.
-module(macula_station_link_sealed_caller_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"test">>)).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).

%% The CALL carries no payload a station reads, and the sealed RESULT opens
%% to the provider's answer.
a_sealed_call_round_trips_test_() ->
    {timeout, 10, fun() ->
        #{link := Pid} = F = fixture(),
        Caller = call_async(F, #{city => {text, <<"Tienen">>}}),
        {Frame, Opened, Keys, SealRequest} = received_call(F),
        ?assertNot(is_map_key(payload, maps:get(request, Opened))),
        ?assertEqual({ok, #{{text, <<"city">>} => {text, <<"Tienen">>}}}, maps:get(plain, Opened)),
        answer(F, Frame, sealed_result(F, Frame, Keys, SealRequest, #{temp => 21})),
        ?assertEqual({ok, #{{text, <<"temp">>} => 21}}, result(Caller)),
        macula_station_link:stop(Pid)
    end}.

%% A sealed handler error opens to the caller as a clear one reads.
a_sealed_error_opens_as_a_clear_one_reads_test_() ->
    {timeout, 10, fun() ->
        #{link := Pid} = F = fixture(),
        Caller = call_async(F, #{}),
        {Frame, _Opened, Keys, SealRequest} = received_call(F),
        answer(F, Frame, sealed_error(F, Frame, Keys, SealRequest, #{code => <<"handler_error">>,
                                                                      detail => <<"no such city">>})),
        ?assertEqual({error, <<"no such city">>}, result(Caller)),
        macula_station_link:stop(Pid)
    end}.

%% `sealed_refused' in the clear names the provider's current key.
sealed_refused_names_the_providers_key_test_() ->
    {timeout, 10, fun() ->
        #{link := Pid} = F = fixture(),
        Caller = call_async(F, #{}),
        {Frame, _Opened, _Keys, _SealRequest} = received_call(F),
        answer(F, Frame, clear_error(F, Frame, <<"sealed_refused">>, binary:encode_hex(<<7:64>>, lowercase))),
        ?assertEqual({error, {sealed_refused, <<7:64>>}}, result(Caller)),
        macula_station_link:stop(Pid)
    end}.

%% An admission refusal from the closed set comes back in the clear, as ever.
a_clear_admission_refusal_is_accepted_test_() ->
    {timeout, 10, fun() ->
        #{link := Pid} = F = fixture(),
        Caller = call_async(F, #{}),
        {Frame, _Opened, _Keys, _SealRequest} = received_call(F),
        answer(F, Frame, clear_error(F, Frame, <<"caller_quota">>, undefined)),
        ?assertEqual({error, {call_error, <<"caller_quota">>, undefined}}, result(Caller)),
        macula_station_link:stop(Pid)
    end}.

%% A clear RESULT, or a clear error outside the closed set, does not answer a
%% sealed request: it is refused, and the call runs to its deadline.
a_clear_answer_outside_the_set_is_refused_test_() ->
    {timeout, 10, fun() ->
        #{link := Pid} = F = fixture(),
        Caller = call_async(F, #{}, 1_500),
        {Frame, _Opened, _Keys, _SealRequest} = received_call(F),
        answer(F, Frame, clear_error(F, Frame, <<"handler_error">>, <<"leaked">>)),
        answer(F, Frame, clear_result(F, Frame, #{leaked => 1})),
        ?assertEqual({error, timeout}, result(Caller)),
        macula_station_link:stop(Pid)
    end}.

%% A key of another profile's size seals nothing: the call fails before
%% anything is sent.
a_key_of_another_profile_is_refused_test_() ->
    {timeout, 10, fun() ->
        #{link := Pid} = F = fixture(),
        Other = other_profile(profile()),
        {Public, _} = macula_seal:generate_key(Other),
        ?assertEqual({error, {confidentiality, no_kem_key}},
                     macula_station_link:call(Pid, maps:get(provider_id, F), ?REALM, <<"acme/echo_v1">>, #{}, 1_000,
                                              <<>>, {sealed_to, macula_seal:key_as_carried(Public)})),
        macula_station_link:stop(Pid)
    end}.

%% A pending sealed call's keys never show in the link's status or crash
%% report: its reply key would open its reply.
a_pending_calls_keys_stay_out_of_the_status_test_() ->
    {timeout, 10, fun() ->
        #{link := Pid} = F = fixture(),
        _Caller = call_async(F, #{}),
        {_Frame, _Opened, #{k_rep := KRep, k_req := KReq}, _SealRequest} = received_call(F),
        Status = term_to_binary(sys:get_status(Pid)),
        ?assertEqual(nomatch, binary:match(Status, KRep)),
        ?assertEqual(nomatch, binary:match(Status, KReq)),
        macula_station_link:stop(Pid)
    end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A link whose peer is this process, and a provider key and KEM keypair
%% this process answers with.
fixture() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{seed => #{host => <<"127.0.0.1">>, port => 1},
                                                                connect_timeout_ms => 2000})),
    {ok, StationKey} = macula_node_keys:generate(identity, profile()),
    {ok, StationNodeId} = macula_node_keys:node_id(StationKey),
    Self = self(),
    _ = sys:replace_state(Pid, fun(S) ->
        setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Self), StationNodeId)
    end),
    {ok, ProviderKey} = macula_node_keys:generate(identity, profile()),
    {Public, Private} = macula_seal:generate_key(profile()),
    Carried = macula_seal:key_as_carried(Public),
    #{link => Pid, provider => ProviderKey, provider_id => macula_node_keys:key_id(ProviderKey),
      kem_key => Carried,
      holder => #{current_key_id => macula_seal:key_id(Carried),
                  lookup => fun(_KeyId) -> {ok, Private, Carried} end}}.

with_link_keys(Opts) ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
          share => {seed, {<<"127.0.0.1">>, 1}}, expected_node_id => <<1:256>>}.

call_async(F, Payload) ->
    call_async(F, Payload, 3_000).

call_async(#{link := Pid, provider_id := Target, kem_key := KemKey}, Payload, TimeoutMs) ->
    Self = self(),
    spawn(fun() ->
              Self ! {call_result, self(),
                      macula_station_link:call(Pid, Target, ?REALM, <<"acme/echo_v1">>, Payload, TimeoutMs, <<>>,
                                               {sealed_to, KemKey})}
          end).

result(Caller) ->
    receive {call_result, Caller, Result} -> Result after 5_000 -> error(no_result) end.

%% The CALL the link sent, opened as the provider opens it.
received_call(#{holder := Holder}) ->
    receive
        {'$gen_cast', {send_frame, _, #{frame_type := call} = Frame}} ->
            {ok, Request} = macula_frame:verify_request(Frame, profile()),
            SealRequest = seal_request(Request),
            {ok, Plain, Keys} = macula_sealed_call:open_request(profile(), Holder, SealRequest,
                                                                maps:get(sealed, Request)),
            {Frame, #{request => Request, plain => macula_frame:plain_payload(Plain)}, Keys, SealRequest}
    after 3_000 -> error(no_call_sent)
    end.

seal_request(#{frame_type := Type, realm := Realm, procedure := Procedure, caller := Caller, target := Target,
               request_id := RequestId, deadline := Deadline}) ->
    #{frame_type => atom_to_binary(Type), realm => Realm, procedure => Procedure, caller => Caller,
      target => Target, request_id => RequestId, deadline => Deadline}.

answer(#{link := Pid}, _CallFrame, Reply) ->
    Pid ! {macula_peering, frame, self(), Reply}.

sealed_result(#{provider := Provider}, Frame, Keys, SealRequest, Payload) ->
    {ok, Request} = macula_frame:verify_request(Frame, profile()),
    {ok, Plain} = macula_frame:payload_plain(Payload),
    Sealed = macula_sealed_call:seal_reply(Keys, SealRequest, reply(<<"result">>, Request, Provider), Plain),
    macula_frame:result(#{request => Request, sealed => Sealed}, Provider).

sealed_error(#{provider := Provider}, Frame, Keys, SealRequest, Error) ->
    {ok, Request} = macula_frame:verify_request(Frame, profile()),
    {ok, Plain} = macula_frame:error_plain(Error),
    Sealed = macula_sealed_call:seal_reply(Keys, SealRequest, reply(<<"error">>, Request, Provider), Plain),
    macula_frame:provider_error(#{request => Request, sealed => Sealed}, Provider).

clear_error(#{provider := Provider}, Frame, Code, Detail) ->
    {ok, Request} = macula_frame:verify_request(Frame, profile()),
    macula_frame:provider_error(with_detail(Detail, #{request => Request, code => Code}), Provider).

clear_result(#{provider := Provider}, Frame, Payload) ->
    {ok, Request} = macula_frame:verify_request(Frame, profile()),
    macula_frame:result(#{request => Request, payload => Payload}, Provider).

with_detail(undefined, Spec) -> Spec;
with_detail(Detail, Spec) -> Spec#{detail => Detail}.

reply(Type, #{request_hash := RequestHash}, Provider) ->
    #{frame_type => Type, request_hash => RequestHash, responded_by => macula_node_keys:key_id(Provider)}.

other_profile(pq_pure) -> pq_hybrid;
other_profile(pq_hybrid) -> pq_pure.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
