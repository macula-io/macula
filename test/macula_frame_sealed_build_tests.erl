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
     {"stream_bytes refuses a build with both", fun stream_bytes_refuses_both/1}].

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

verified_call(#{caller := Caller} = Keys) ->
    {ok, Request} = macula_frame:verify_request(wire(macula_frame:call(call_spec(Keys), Caller)), pq_pure),
    Request.

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

decoded(StreamBytes) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:written_bytes(StreamBytes)),
    Decoded.
