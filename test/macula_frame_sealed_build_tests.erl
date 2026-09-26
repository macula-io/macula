%% The frame builders write `sealed' in place of a payload (E2E design §3.1,
%% §5.1): a CALL, a STREAM_OPEN, a RESULT and a provider's ERROR, through the
%% direct builders and through stream_bytes/2 alike. A build carries a payload
%% or a sealed payload, never both. What a frame seals is the CBOR of its
%% payload, which payload_plain/1 writes and plain_payload/1 reads back in the
%% shape a clear payload arrives in; a sealed ERROR seals its code and detail.
-module(macula_frame_sealed_build_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<1:256>>).
-define(DEADLINE, 1789000600000).

sealed_build_test_() ->
    {setup, fun keys/0, fun(Keys) -> [{Name, fun() -> Case(Keys) end} || {Name, Case} <- cases()] end}.

cases() ->
    [{"a sealed CALL builds and verifies with its sealed and no payload", fun a_sealed_call_builds/1},
     {"a sealed STREAM_OPEN builds and verifies", fun a_sealed_stream_open_builds/1},
     {"a CALL build with both payload and sealed raises", fun a_call_with_both_raises/1},
     {"a sealed RESULT and a sealed ERROR build and verify", fun sealed_replies_build/1},
     {"stream_bytes builds sealed requests and replies", fun stream_bytes_builds_sealed/1},
     {"stream_bytes refuses a build with both", fun stream_bytes_refuses_both/1},
     {"sealed provider stream frames build and verify", fun sealed_provider_stream_frames_build/1},
     {"sealed caller stream frames build and verify", fun sealed_caller_stream_frames_build/1},
     {"stream_bytes builds sealed stream frames", fun stream_bytes_builds_sealed_stream_frames/1},
     {"a stream frame build with both clear and sealed raises", fun a_stream_frame_with_both_raises/1},
     {"a stream frame sealed against its side's nonce rule raises", fun a_stream_frame_against_its_nonce_rule_raises/1},
     {"a STREAM_END has nothing to seal", fun a_stream_end_has_nothing_to_seal/1}].

a_sealed_call_builds(#{caller := Caller} = Keys) ->
    {ok, Verified} = macula_frame:verify_request(wire(macula_frame:call(sealed_spec(Keys), Caller)), pq_pure),
    ?assertEqual(request_sealed(), maps:get(sealed, Verified)),
    ?assertNot(is_map_key(payload, Verified)).

a_sealed_stream_open_builds(#{caller := Caller} = Keys) ->
    ?assertMatch({ok, #{sealed := #{kem_ct := _}, mode := bidi}},
                 macula_frame:verify_request(wire(macula_frame:stream_open((sealed_spec(Keys))#{mode => bidi}, Caller)),
                                             pq_pure)).

a_call_with_both_raises(#{caller := Caller} = Keys) ->
    ?assertError(function_clause, macula_frame:call((sealed_spec(Keys))#{payload => #{}}, Caller)).

sealed_replies_build(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    {ok, Result} = macula_frame:verify_reply(wire(macula_frame:result(#{request => Request, sealed => reply_sealed()},
                                                                      Provider)), Request, pq_pure),
    ?assertEqual(reply_sealed(), maps:get(sealed, Result)),
    ?assertNot(is_map_key(payload, Result)),
    {ok, Error} = macula_frame:verify_reply(wire(macula_frame:provider_error(#{request => Request,
                                                                               sealed => reply_sealed()}, Provider)),
                                            Request, pq_pure),
    ?assertEqual(reply_sealed(), maps:get(sealed, Error)),
    ?assertNot(is_map_key(code, Error)).

stream_bytes_builds_sealed(#{caller := Caller, provider := Provider} = Keys) ->
    {ok, CallBytes} = macula_frame:stream_bytes({call, sealed_spec(Keys)}, Caller),
    ?assertMatch({ok, #{sealed := #{kem_ct := _}}}, macula_frame:verify_request(decoded(CallBytes), pq_pure)),
    Request = verified_call(Keys),
    [?assertMatch({ok, #{sealed := #{nonce := _}}},
                  macula_frame:verify_reply(decoded(Bytes), Request, pq_pure))
     || {ok, Bytes} <- [macula_frame:stream_bytes({result, #{request => Request, sealed => reply_sealed()}}, Provider),
                        macula_frame:stream_bytes({provider_error, #{request => Request, sealed => reply_sealed()}},
                                                  Provider)]].

stream_bytes_refuses_both(#{caller := Caller} = Keys) ->
    ?assertError(function_clause, macula_frame:stream_bytes({call, (sealed_spec(Keys))#{payload => #{}}}, Caller)).

%% A provider's stream frames seal their body, reply payload or error under a
%% random nonce they carry (§5.2): a sealed STREAM_DATA keeps its encoding in
%% the clear, and none of them carries its clear field too.
sealed_provider_stream_frames_build(#{provider := Provider} = Keys) ->
    Open = verified_bidi_open(Keys),
    Data = macula_frame:provider_stream(#{frame_type => stream_data, seq => 0, encoding => raw,
                                          sealed => provider_sealed()}, Provider, Open),
    {ok, DataFields, Next} = macula_frame:verify_provider_stream(wire(Data), macula_frame:open_stream(Open), pq_pure),
    ?assertMatch(#{frame_type := stream_data, encoding := raw, sealed := #{nonce := _}}, DataFields),
    ?assertNot(is_map_key(body, DataFields)),
    Reply = macula_frame:provider_stream(#{frame_type => stream_reply, seq => 1, sealed => provider_sealed()},
                                         Provider, Open),
    {ok, ReplyFields, _} = macula_frame:verify_provider_stream(wire(Reply), Next, pq_pure),
    ?assertMatch(#{frame_type := stream_reply, sealed := _}, ReplyFields),
    ?assertNot(is_map_key(payload, ReplyFields)),
    Error = macula_frame:provider_stream(#{frame_type => stream_error, seq => 0, sealed => provider_sealed()},
                                         Provider, Open),
    {ok, ErrorFields, _} = macula_frame:verify_provider_stream(wire(Error), macula_frame:open_stream(Open), pq_pure),
    ?assertMatch(#{frame_type := stream_error, sealed := _}, ErrorFields),
    ?assertNot(is_map_key(code, ErrorFields)).

%% A caller's stream frames seal under a nonce derived from their signed seq,
%% so their sealed carries none.
sealed_caller_stream_frames_build(#{caller := Caller} = Keys) ->
    Open = verified_bidi_open(Keys),
    Data = macula_frame:caller_stream(#{frame_type => stream_data, seq => 0, encoding => msgpack,
                                        sealed => caller_sealed()}, Caller, Open),
    {ok, DataFields, Next} = macula_frame:verify_caller_stream(wire(Data), macula_frame:open_stream(Open), pq_pure),
    ?assertMatch(#{frame_type := stream_data, encoding := msgpack, sealed := _}, DataFields),
    ?assertNot(is_map_key(nonce, maps:get(sealed, DataFields))),
    Error = macula_frame:caller_stream(#{frame_type => stream_error, seq => 1, sealed => caller_sealed()}, Caller, Open),
    ?assertMatch({ok, #{frame_type := stream_error, sealed := _}, _},
                 macula_frame:verify_caller_stream(wire(Error), Next, pq_pure)).

stream_bytes_builds_sealed_stream_frames(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_bidi_open(Keys),
    {ok, ProviderBytes} = macula_frame:stream_bytes({provider_stream, #{frame_type => stream_data, seq => 0,
                                                                        encoding => raw, sealed => provider_sealed()},
                                                     Open}, Provider),
    ?assertMatch({ok, #{sealed := _}, _},
                 macula_frame:verify_provider_stream(decoded(ProviderBytes), macula_frame:open_stream(Open), pq_pure)),
    {ok, CallerBytes} = macula_frame:stream_bytes({caller_stream, #{frame_type => stream_error, seq => 0,
                                                                    sealed => caller_sealed()}, Open}, Caller),
    ?assertMatch({ok, #{sealed := _}, _},
                 macula_frame:verify_caller_stream(decoded(CallerBytes), macula_frame:open_stream(Open), pq_pure)).

a_stream_frame_with_both_raises(#{provider := Provider} = Keys) ->
    Open = verified_bidi_open(Keys),
    [?assertError(function_clause, macula_frame:provider_stream(Spec, Provider, Open))
     || Spec <- [#{frame_type => stream_data, seq => 0, encoding => raw, body => <<"x">>, sealed => provider_sealed()},
                 #{frame_type => stream_reply, seq => 0, payload => 1, sealed => provider_sealed()},
                 #{frame_type => stream_error, seq => 0, code => <<"c">>, message => <<"m">>,
                   sealed => provider_sealed()}]].

%% What the verifier refuses is never built: a provider's sealed without its
%% nonce, a caller's with one.
a_stream_frame_against_its_nonce_rule_raises(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_bidi_open(Keys),
    ?assertError(function_clause,
                 macula_frame:provider_stream(#{frame_type => stream_reply, seq => 0, sealed => caller_sealed()},
                                              Provider, Open)),
    ?assertError(function_clause,
                 macula_frame:caller_stream(#{frame_type => stream_data, seq => 0, encoding => raw,
                                              sealed => provider_sealed()}, Caller, Open)).

a_stream_end_has_nothing_to_seal(#{provider := Provider} = Keys) ->
    Open = verified_bidi_open(Keys),
    ?assertError(function_clause,
                 macula_frame:provider_stream(#{frame_type => stream_end, seq => 0, role => both,
                                                sealed => provider_sealed()}, Provider, Open)),
    ?assertEqual({error, {unknown_build_key, sealed}},
                 macula_frame:stream_bytes({provider_stream, #{frame_type => stream_end, seq => 0, role => both,
                                                               sealed => provider_sealed()}, Open}, Provider)).

%%------------------------------------------------------------------
%% Plaintext
%%------------------------------------------------------------------

%% A payload's plaintext reads back as the same payload arrives in the clear.
a_payload_round_trips_through_its_plaintext_test() ->
    Payload = #{city => {text, <<"Tienen">>}, days => [1, 2, 3], raw => <<1, 2>>, nested => #{deep => true}},
    {ok, Plain} = macula_frame:payload_plain(Payload),
    ?assertEqual({ok, #{{text, <<"city">>} => {text, <<"Tienen">>}, {text, <<"days">>} => [1, 2, 3],
                        {text, <<"raw">>} => <<1, 2>>, {text, <<"nested">>} => #{{text, <<"deep">>} => {text, <<"true">>}}}},
                 macula_frame:plain_payload(Plain)).

%% A payload the wire cannot carry is refused before it is sealed, as it is
%% before it is sent in the clear.
an_unsendable_payload_has_no_plaintext_test() ->
    ?assertMatch({error, {unsupported_payload_type, _, _}}, macula_frame:payload_plain(#{bad => {1, 2}})).

%% Opened bytes that are not one CBOR value under the decoding rule are
%% refused as sealed_refused: an opened payload is the peer's bytes.
opened_bytes_that_are_not_cbor_are_refused_test_() ->
    [?_assertEqual({error, sealed_refused}, macula_frame:plain_payload(Bytes))
     || Bytes <- [<<>>, <<16#ff>>, <<16#a1, 16#61, $a>>, <<1, 2>>]].

%% A sealed ERROR seals its code and its detail, and reads them back.
an_error_round_trips_through_its_plaintext_test_() ->
    [?_assertEqual({ok, Error}, macula_frame:plain_error(element(2, macula_frame:error_plain(Error))))
     || Error <- [#{code => <<"not_found">>}, #{code => <<"bad_city">>, detail => <<"no such city">>}]].

%% The error plaintext is the CBOR array [code, detail], detail empty text when there is none.
an_error_plaintext_is_code_and_detail_test() ->
    ?assertEqual({ok, macula_record_cbor:encode([{text, <<"x">>}, {text, <<>>}])},
                 macula_frame:error_plain(#{code => <<"x">>})).

%% A code or detail over its bound, or not UTF-8, is refused, as it is in a
%% clear ERROR.
an_error_of_unbounded_text_has_no_plaintext_test_() ->
    [?_assertMatch({error, _}, macula_frame:error_plain(Error))
     || Error <- [#{code => binary:copy(<<"c">>, 65)}, #{code => <<"c">>, detail => binary:copy(<<"d">>, 257)},
                  #{code => <<255>>}]].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    _ = application:load(macula),
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key end,
    #{caller => Generate(), provider => Generate()}.

target(#{provider := Provider}) ->
    macula_node_keys:key_id(Provider).

call_spec(Keys) ->
    #{request_id => <<7:128>>, realm => ?REALM, procedure => <<"acme/get_forecast_v1">>, target => target(Keys),
      deadline => ?DEADLINE, payload => #{city => {text, <<"Tienen">>}}}.

sealed_spec(Keys) ->
    maps:remove(payload, (call_spec(Keys))#{sealed => request_sealed()}).

request_sealed() ->
    #{scheme => 1, key_id => <<1:64>>, kem_ct => <<2:12544>>, ct => <<"ciphertext">>}.

reply_sealed() ->
    #{scheme => 1, key_id => <<1:64>>, nonce => <<3:96>>, ct => <<"ciphertext">>}.

provider_sealed() ->
    #{scheme => 1, key_id => <<1:64>>, nonce => <<4:96>>, ct => <<"stream ciphertext">>}.

caller_sealed() ->
    #{scheme => 1, key_id => <<1:64>>, ct => <<"stream ciphertext">>}.

verified_bidi_open(#{caller := Caller} = Keys) ->
    {ok, Open} = macula_frame:verify_request(wire(macula_frame:stream_open((sealed_spec(Keys))#{mode => bidi}, Caller)),
                                             pq_pure),
    Open.

verified_call(#{caller := Caller} = Keys) ->
    {ok, Request} = macula_frame:verify_request(wire(macula_frame:call(call_spec(Keys), Caller)), pq_pure),
    Request.

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

decoded(StreamBytes) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:written_bytes(StreamBytes)),
    Decoded.
