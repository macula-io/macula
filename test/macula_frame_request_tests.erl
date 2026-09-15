%% EUnit tests for requests, replies and relay errors (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md: Requests, Replies and
%% Relay errors; D25). CALL and STREAM_OPEN carry request, a {key, tbs, signature} under MACULA-PQ-REQUEST-V1 signed by
%% the caller; RESULT and ERROR from a provider carry reply under MACULA-PQ-REPLY-V1; ERROR and STREAM_ERROR from a
%% station carry relay_error under MACULA-PQ-RELAY-ERROR-V1. caller, responded_by and reported_by are key ids.
-module(macula_frame_request_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REQUEST_LABEL, <<"MACULA-PQ-REQUEST-V1">>).
-define(REPLY_LABEL, <<"MACULA-PQ-REPLY-V1">>).
-define(RELAY_ERROR_LABEL, <<"MACULA-PQ-RELAY-ERROR-V1">>).
-define(REALM, <<1:256>>).
-define(PROCEDURE, <<"acme/get_forecast_v1">>).
-define(DEADLINE, 1789000600000).

requests_replies_and_relay_errors_test_() ->
    {setup, fun keys/0, fun cases/1}.

%% Every case signs inside its own test, so each one passes or fails on its own.
cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_signed_call_verifies_with_its_caller_key_and_request_hash/1,
                 fun a_call_frame_carries_version_frame_type_request_and_its_routing_fields/1,
                 fun a_stream_open_carries_its_mode/1,
                 fun mode_belongs_to_stream_open_only/1,
                 fun a_token_is_optional_bytes/1,
                 fun a_caller_that_is_not_the_key_id_of_key_is_refused/1,
                 fun a_tampered_request_is_refused/1,
                 fun a_request_under_another_label_is_refused/1,
                 fun request_fields_the_design_does_not_allow_are_malformed/1,
                 fun a_request_under_the_other_profile_is_malformed/1,
                 fun two_callers_never_share_a_request_hash/1,
                 fun a_result_from_the_target_verifies_for_its_request/1,
                 fun a_provider_error_carries_its_code_and_detail/1,
                 fun an_error_code_and_detail_are_read_within_their_bounds/1,
                 fun a_reply_from_a_node_other_than_the_target_is_refused/1,
                 fun a_reply_for_another_request_is_refused/1,
                 fun a_responded_by_that_is_not_the_key_id_of_key_is_refused/1,
                 fun a_tampered_reply_is_refused/1,
                 fun reply_fields_the_design_does_not_allow_are_malformed/1,
                 fun a_relay_error_verifies_for_its_request/1,
                 fun a_stream_error_from_a_station_is_a_relay_error/1,
                 fun a_relay_code_outside_the_closed_set_is_refused/1,
                 fun a_relay_error_for_another_request_is_refused/1,
                 fun a_reported_by_that_is_not_the_key_id_of_key_is_refused/1,
                 fun a_relay_error_from_a_station_other_than_the_connection_is_refused/1,
                 fun an_error_frame_is_either_a_reply_or_a_relay_error/1,
                 fun claimed_ids_come_from_a_result_an_error_and_a_relay_error/1,
                 fun claimed_ids_are_malformed_for_anything_else/1,
                 fun claimed_ids_are_only_a_lookup_key/1,
                 fun claimed_ids_refuse_a_short_id_and_a_malformed_relay_error/1]].

%%------------------------------------------------------------------
%% Requests: CALL and STREAM_OPEN
%%------------------------------------------------------------------

a_signed_call_verifies_with_its_caller_key_and_request_hash(#{caller := Caller} = Keys) ->
    Frame = wire(macula_frame:call(call_spec(Keys), Caller)),
    #{request := #{key := Key, tbs := Tbs}} = Frame,
    {ok, Request} = macula_frame:verify_request(Frame, pq_pure),
    ?assertMatch(#{frame_type := call, request_id := <<7:128>>, realm := ?REALM, procedure := ?PROCEDURE,
                   deadline := ?DEADLINE}, Request),
    ?assertEqual(macula_node_keys:public_key(Caller), Key),
    ?assertEqual(Key, maps:get(key, Request)),
    ?assertEqual(macula_node_keys:key_id(Caller), maps:get(caller, Request)),
    ?assertEqual(target(Keys), maps:get(target, Request)),
    ?assertEqual(crypto:hash(sha384, Tbs), maps:get(request_hash, Request)),
    ?assertEqual(#{{text, <<"city">>} => {text, <<"Tienen">>}}, maps:get(payload, Request)).

a_call_frame_carries_version_frame_type_request_and_its_routing_fields(#{caller := Caller} = Keys) ->
    Routed = wire(macula_frame:call((call_spec(Keys))#{source_route => <<1, 2>>, retry_budget => 3}, Caller)),
    ?assertEqual([frame_type, request, retry_budget, source_route, version], lists:sort(maps:keys(Routed))),
    ?assertEqual([key, signature, tbs], lists:sort(maps:keys(maps:get(request, Routed)))),
    Plain = wire(macula_frame:call(call_spec(Keys), Caller)),
    ?assertEqual([frame_type, request, version], lists:sort(maps:keys(Plain))).

a_stream_open_carries_its_mode(Keys) ->
    ?assertMatch(#{frame_type := stream_open, mode := server_stream}, verified_stream_open(Keys)).

mode_belongs_to_stream_open_only(#{caller := Caller} = Keys) ->
    ?assertError(function_clause, macula_frame:call((call_spec(Keys))#{mode => bidi}, Caller)),
    ?assertError(function_clause, macula_frame:stream_open(call_spec(Keys), Caller)),
    WithMode = (request_tbs(Keys))#{{text, <<"mode">>} => {text, <<"bidi">>}},
    ?assertEqual({error, malformed_frame}, verify_crafted_request(call, WithMode, Caller)),
    WithoutMode = (request_tbs(Keys))#{{text, <<"frame_type">>} := {text, <<"stream_open">>}},
    ?assertEqual({error, malformed_frame}, verify_crafted_request(stream_open, WithoutMode, Caller)).

a_token_is_optional_bytes(#{caller := Caller} = Keys) ->
    Frame = wire(macula_frame:call((call_spec(Keys))#{token => <<"a token">>}, Caller)),
    ?assertMatch({ok, #{token := <<"a token">>}}, macula_frame:verify_request(Frame, pq_pure)),
    Text = (request_tbs(Keys))#{{text, <<"token">>} => {text, <<"a token">>}},
    ?assertEqual({error, malformed_frame}, verify_crafted_request(call, Text, Caller)).

a_caller_that_is_not_the_key_id_of_key_is_refused(#{caller := Caller, other := Other} = Keys) ->
    Tbs = (request_tbs(Keys))#{{text, <<"caller">>} := macula_node_keys:key_id(Other)},
    ?assertEqual({error, key_id_mismatch}, verify_crafted_request(call, Tbs, Caller)).

a_tampered_request_is_refused(#{caller := Caller} = Keys) ->
    #{request := Signed} = Frame = macula_frame:call(call_spec(Keys), Caller),
    Tampered = Frame#{request := Signed#{tbs := flip(maps:get(tbs, Signed))}},
    ?assertEqual({error, signature_invalid}, macula_frame:verify_request(wire(Tampered), pq_pure)).

a_request_under_another_label_is_refused(#{caller := Caller} = Keys) ->
    Frame = crafted(call, request, macula_signed_object:sign(?REPLY_LABEL, request_tbs(Keys), Caller)),
    ?assertEqual({error, signature_invalid}, macula_frame:verify_request(wire(Frame), pq_pure)).

request_fields_the_design_does_not_allow_are_malformed(#{caller := Caller} = Keys) ->
    Base = request_tbs(Keys),
    ?assertMatch({ok, _}, verify_crafted_request(call, Base, Caller)),
    [?assertEqual({error, malformed_frame}, verify_crafted_request(call, Tbs, Caller))
     || Tbs <- [Base#{{text, <<"extra">>} => 1},
                maps:remove({text, <<"target">>}, Base),
                Base#{{text, <<"request_id">>} := <<7:120>>},
                Base#{{text, <<"target">>} := <<1:248>>},
                Base#{{text, <<"deadline">>} := 1 bsl 53},
                Base#{{text, <<"deadline">>} := {text, <<"soon">>}},
                Base#{{text, <<"procedure">>} := ?PROCEDURE},
                Base#{{text, <<"frame_type">>} := {text, <<"result">>}}]].

a_request_under_the_other_profile_is_malformed(#{caller := Caller} = Keys) ->
    Frame = wire(macula_frame:call(call_spec(Keys), Caller)),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_request(Frame, pq_hybrid)).

two_callers_never_share_a_request_hash(#{caller := Caller, other := Other} = Keys) ->
    #{request_hash := One} = verified_request(Caller, call_spec(Keys)),
    #{request_hash := Two} = verified_request(Other, call_spec(Keys)),
    ?assertNotEqual(One, Two).

%%------------------------------------------------------------------
%% Replies: RESULT and ERROR from a provider
%%------------------------------------------------------------------

a_result_from_the_target_verifies_for_its_request(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    Result = wire(macula_frame:result(#{request => Request, payload => #{temp => 21}}, Provider)),
    ?assertEqual([frame_type, reply, version], lists:sort(maps:keys(Result))),
    ?assertEqual({ok, #{frame_type => result, responded_by => target(Keys), payload => #{{text, <<"temp">>} => 21}}},
                 macula_frame:verify_reply(Result, Request, pq_pure)).

a_provider_error_carries_its_code_and_detail(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    Detailed = macula_frame:provider_error(#{request => Request, code => <<"closed">>, detail => <<"after six">>},
                                           Provider),
    ?assertEqual({ok, #{frame_type => error, responded_by => target(Keys), code => <<"closed">>,
                        detail => <<"after six">>}},
                 macula_frame:verify_reply(wire(Detailed), Request, pq_pure)),
    Bare = macula_frame:provider_error(#{request => Request, code => <<"closed">>}, Provider),
    ?assertEqual({ok, #{frame_type => error, responded_by => target(Keys), code => <<"closed">>}},
                 macula_frame:verify_reply(wire(Bare), Request, pq_pure)).

%% A provider error's code is text of at most 64 bytes and its detail text of at most 256: at the bound it verifies,
%% with the text as a binary, and one byte over is malformed.
an_error_code_and_detail_are_read_within_their_bounds(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    #{reply := #{tbs := Built}} =
        macula_frame:provider_error(#{request => Request, code => <<"c">>, detail => <<"d">>}, Provider),
    {ok, Tbs} = macula_record_cbor:decode_strict(Built),
    Verify = fun(Field, Bytes) ->
        Resized = Tbs#{{text, Field} := {text, binary:copy(<<"a">>, Bytes)}},
        Signed = macula_signed_object:sign(?REPLY_LABEL, Resized, Provider),
        macula_frame:verify_reply(wire(crafted(error, reply, Signed)), Request, pq_pure)
    end,
    ?assertMatch({ok, #{code := <<_:512>>}}, Verify(<<"code">>, 64)),
    ?assertMatch({ok, #{detail := <<_:2048>>}}, Verify(<<"detail">>, 256)),
    ?assertEqual({error, malformed_frame}, Verify(<<"code">>, 65)),
    ?assertEqual({error, malformed_frame}, Verify(<<"detail">>, 257)).

a_reply_from_a_node_other_than_the_target_is_refused(#{other := Other} = Keys) ->
    Request = verified_call(Keys),
    Result = wire(macula_frame:result(#{request => Request, payload => 1}, Other)),
    ?assertEqual({error, not_the_target}, macula_frame:verify_reply(Result, Request, pq_pure)).

a_reply_for_another_request_is_refused(#{caller := Caller, provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    Result = wire(macula_frame:result(#{request => Request, payload => 1}, Provider)),
    Another = verified_request(Caller, (call_spec(Keys))#{request_id => <<8:128>>}),
    ?assertEqual({error, request_mismatch}, macula_frame:verify_reply(Result, Another, pq_pure)),
    OtherHash = Request#{request_hash := crypto:hash(sha384, <<"another request">>)},
    ?assertEqual({error, request_mismatch}, macula_frame:verify_reply(Result, OtherHash, pq_pure)).

a_responded_by_that_is_not_the_key_id_of_key_is_refused(#{provider := Provider, other := Other} = Keys) ->
    Request = verified_call(Keys),
    Tbs = (reply_tbs(Request, result))#{{text, <<"responded_by">>} := macula_node_keys:key_id(Other)},
    Frame = crafted(result, reply, macula_signed_object:sign(?REPLY_LABEL, Tbs, Provider)),
    ?assertEqual({error, key_id_mismatch}, macula_frame:verify_reply(wire(Frame), Request, pq_pure)).

a_tampered_reply_is_refused(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    #{reply := Signed} = Frame = macula_frame:result(#{request => Request, payload => 1}, Provider),
    Tampered = Frame#{reply := Signed#{tbs := flip(maps:get(tbs, Signed))}},
    ?assertEqual({error, signature_invalid}, macula_frame:verify_reply(wire(Tampered), Request, pq_pure)).

reply_fields_the_design_does_not_allow_are_malformed(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    Verify = fun(FrameType, Tbs) ->
        Frame = crafted(FrameType, reply, macula_signed_object:sign(?REPLY_LABEL, Tbs, Provider)),
        macula_frame:verify_reply(wire(Frame), Request, pq_pure)
    end,
    Result = reply_tbs(Request, result),
    Error = reply_tbs(Request, error),
    ?assertMatch({ok, _}, Verify(result, Result)),
    ?assertMatch({ok, _}, Verify(error, Error)),
    [?assertEqual({error, malformed_frame}, Verify(FrameType, Tbs))
     || {FrameType, Tbs} <- [{result, Result#{{text, <<"code">>} => {text, <<"closed">>}}},
                             {result, maps:remove({text, <<"payload">>}, Result)},
                             {error, Error#{{text, <<"payload">>} => 1}},
                             {error, maps:remove({text, <<"code">>}, Error)},
                             {result, Result#{{text, <<"request_hash">>} := <<0:376>>}},
                             {result, Result#{{text, <<"extra">>} => 1}},
                             {error, Result}]].

%%------------------------------------------------------------------
%% Relay errors: ERROR and STREAM_ERROR from a station
%%------------------------------------------------------------------

a_relay_error_verifies_for_its_request(#{station := Station} = Keys) ->
    Request = verified_call(Keys),
    Spec = #{frame_type => error, request => Request, code => unknown_next_peer, detail => <<"no route">>,
             offending_hop => <<9:256>>},
    Relay = wire(macula_frame:relay_error(Spec, Station)),
    ?assertEqual([frame_type, relay_error, version], lists:sort(maps:keys(Relay))),
    ?assertEqual({ok, #{frame_type => error, reported_by => macula_node_keys:key_id(Station), code => unknown_next_peer,
                        detail => <<"no route">>, offending_hop => <<9:256>>}},
                 macula_frame:verify_relay_error(Relay, Request, pq_pure, macula_node_keys:key_id(Station))).

a_stream_error_from_a_station_is_a_relay_error(#{station := Station} = Keys) ->
    Request = verified_stream_open(Keys),
    Spec = #{frame_type => stream_error, request => Request, code => unknown_next_peer},
    Relay = wire(macula_frame:relay_error(Spec, Station)),
    ?assertEqual({ok, #{frame_type => stream_error, reported_by => macula_node_keys:key_id(Station),
                        code => unknown_next_peer}},
                 macula_frame:verify_relay_error(Relay, Request, pq_pure, macula_node_keys:key_id(Station))).

a_relay_code_outside_the_closed_set_is_refused(#{station := Station} = Keys) ->
    Request = verified_call(Keys),
    ?assertError(function_clause,
                 macula_frame:relay_error(#{frame_type => error, request => Request, code => no_route}, Station)),
    Tbs = (relay_tbs(Request, Station))#{{text, <<"code">>} := {text, <<"no_route">>}},
    Frame = crafted(error, relay_error, macula_signed_object:sign(?RELAY_ERROR_LABEL, Tbs, Station)),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_relay_error(wire(Frame), Request, pq_pure, macula_node_keys:key_id(Station))).

a_relay_error_for_another_request_is_refused(#{caller := Caller, station := Station} = Keys) ->
    Request = verified_call(Keys),
    Spec = #{frame_type => error, request => Request, code => unknown_next_peer},
    Relay = wire(macula_frame:relay_error(Spec, Station)),
    Another = verified_request(Caller, (call_spec(Keys))#{request_id => <<8:128>>}),
    ?assertEqual({error, request_mismatch},
                 macula_frame:verify_relay_error(Relay, Another, pq_pure, macula_node_keys:key_id(Station))).

a_reported_by_that_is_not_the_key_id_of_key_is_refused(#{station := Station, other := Other} = Keys) ->
    Request = verified_call(Keys),
    Tbs = (relay_tbs(Request, Station))#{{text, <<"reported_by">>} := macula_node_keys:key_id(Other)},
    Frame = crafted(error, relay_error, macula_signed_object:sign(?RELAY_ERROR_LABEL, Tbs, Station)),
    ?assertEqual({error, key_id_mismatch},
                 macula_frame:verify_relay_error(wire(Frame), Request, pq_pure, macula_node_keys:key_id(Station))).

an_error_frame_is_either_a_reply_or_a_relay_error(#{provider := Provider, station := Station} = Keys) ->
    Request = verified_call(Keys),
    #{reply := Reply} = ProviderError =
        macula_frame:provider_error(#{request => Request, code => <<"closed">>}, Provider),
    #{relay_error := RelayError} = Relay =
        macula_frame:relay_error(#{frame_type => error, request => Request, code => unknown_next_peer}, Station),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_reply(wire(Relay), Request, pq_pure)),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_relay_error(wire(ProviderError), Request, pq_pure, macula_node_keys:key_id(Station))),
    %% A frame that holds both objects no longer decodes: decode/1 refuses it by name, and the verifiers still refuse
    %% it as built.
    Both = ProviderError#{relay_error => RelayError},
    ?assertEqual({error, {invalid_frame, error, relay_error}}, macula_frame:decode(macula_frame:encode(Both))),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_reply(Both, Request, pq_pure)),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_relay_error(Relay#{reply => Reply}, Request, pq_pure,
                                                 macula_node_keys:key_id(Station))).

%% A relay error counts only from the station the connection authenticated: one that another station reports is
%% refused as not_the_connection, once its signature and request checks pass.
a_relay_error_from_a_station_other_than_the_connection_is_refused(#{station := Station, other := Other} = Keys) ->
    Request = verified_call(Keys),
    Relay = wire(macula_frame:relay_error(#{frame_type => error, request => Request, code => unknown_next_peer},
                                          Other)),
    ?assertEqual({error, not_the_connection},
                 macula_frame:verify_relay_error(Relay, Request, pq_pure, macula_node_keys:key_id(Station))),
    ?assertMatch({ok, #{frame_type := error, code := unknown_next_peer}},
                 macula_frame:verify_relay_error(Relay, Request, pq_pure, macula_node_keys:key_id(Other))).

%%------------------------------------------------------------------
%% The ids a reply names, before it is verified
%%------------------------------------------------------------------

claimed_ids_come_from_a_result_an_error_and_a_relay_error(#{provider := Provider, station := Station} = Keys) ->
    #{request_id := RequestId, request_hash := RequestHash} = Request = verified_call(Keys),
    #{request_id := OpenId, request_hash := OpenHash} = Open = verified_stream_open(Keys),
    Replies = [macula_frame:result(#{request => Request, payload => 1}, Provider),
               macula_frame:provider_error(#{request => Request, code => <<"closed">>}, Provider),
               macula_frame:relay_error(#{frame_type => error, request => Request, code => unknown_next_peer},
                                        Station)],
    StreamRelay = macula_frame:relay_error(#{frame_type => stream_error, request => Open, code => unknown_next_peer},
                                           Station),
    Named = {ok, #{request_id => RequestId, request_hash => RequestHash}},
    ?assertEqual([Named, Named, Named], [macula_frame:claimed_reply_ids(wire(Frame)) || Frame <- Replies]),
    ?assertEqual({ok, #{request_id => OpenId, request_hash => OpenHash}},
                 macula_frame:claimed_reply_ids(wire(StreamRelay))).

%% A frame of another type or with a field a reply does not have, a signed object without its key, a tbs that is not
%% CBOR, and a tbs whose ids are missing or of another length name no request.
claimed_ids_are_malformed_for_anything_else(#{caller := Caller, provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    #{reply := Signed} = Result = macula_frame:result(#{request => Request, payload => 1}, Provider),
    Tbs = reply_tbs(Request, result),
    Crafted = fun(Fields) -> crafted(result, reply, macula_signed_object:sign(?REPLY_LABEL, Fields, Provider)) end,
    Refused = [macula_frame:call(call_spec(Keys), Caller),
               Result#{source_route => <<"a route">>},
               Result#{reply := maps:remove(key, Signed)},
               Result#{reply := Signed#{tbs := <<"not cbor">>}},
               Crafted(maps:remove({text, <<"request_id">>}, Tbs)),
               Crafted(Tbs#{{text, <<"request_hash">>} := <<0:376>>})],
    ?assertEqual([{error, malformed_frame} || _ <- Refused],
                 [macula_frame:claimed_reply_ids(Frame) || Frame <- Refused]).

%% Ids that name a pending request decide nothing: a reply from a node other than the target, and one whose signature
%% does not verify, both name that request and still fail verify_reply/3.
claimed_ids_are_only_a_lookup_key(#{provider := Provider, other := Other} = Keys) ->
    #{request_id := RequestId, request_hash := RequestHash} = Request = verified_call(Keys),
    FromOther = wire(macula_frame:result(#{request => Request, payload => 1}, Other)),
    #{reply := #{signature := Signature} = Signed} = Result =
        macula_frame:result(#{request => Request, payload => 1}, Provider),
    Unsigned = wire(Result#{reply := Signed#{signature := binary:copy(<<0>>, byte_size(Signature))}}),
    Named = {ok, #{request_id => RequestId, request_hash => RequestHash}},
    ?assertEqual([Named, Named], [macula_frame:claimed_reply_ids(Frame) || Frame <- [FromOther, Unsigned]]),
    ?assertEqual([{error, not_the_target}, {error, signature_invalid}],
                 [macula_frame:verify_reply(Frame, Request, pq_pure) || Frame <- [FromOther, Unsigned]]).

%% A request_id of another length is refused as a request_hash of another length is, and the relay_error branch
%% refuses a signed object without its key and a tbs that is not CBOR, as the reply branch does.
claimed_ids_refuse_a_short_id_and_a_malformed_relay_error(#{provider := Provider, station := Station} = Keys) ->
    Request = verified_call(Keys),
    ShortId = (reply_tbs(Request, result))#{{text, <<"request_id">>} := <<0:120>>},
    Short = crafted(result, reply, macula_signed_object:sign(?REPLY_LABEL, ShortId, Provider)),
    #{relay_error := StreamSigned} = StreamRelay =
        macula_frame:relay_error(#{frame_type => stream_error, request => verified_stream_open(Keys),
                                   code => unknown_next_peer}, Station),
    #{relay_error := ErrorSigned} = ErrorRelay =
        macula_frame:relay_error(#{frame_type => error, request => Request, code => unknown_next_peer}, Station),
    Refused = [Short,
               StreamRelay#{relay_error := maps:remove(key, StreamSigned)},
               ErrorRelay#{relay_error := ErrorSigned#{tbs := <<"not cbor">>}}],
    ?assertEqual([{error, malformed_frame} || _ <- Refused],
                 [macula_frame:claimed_reply_ids(Frame) || Frame <- Refused]).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key end,
    #{caller => Generate(), provider => Generate(), station => Generate(), other => Generate()}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

target(#{provider := Provider}) ->
    macula_node_keys:key_id(Provider).

call_spec(Keys) ->
    #{request_id => <<7:128>>, realm => ?REALM, procedure => ?PROCEDURE, target => target(Keys),
      deadline => ?DEADLINE, payload => #{city => {text, <<"Tienen">>}}}.

verified_call(#{caller := Caller} = Keys) ->
    verified_request(Caller, call_spec(Keys)).

verified_request(Caller, Spec) ->
    {ok, Request} = macula_frame:verify_request(wire(macula_frame:call(Spec, Caller)), pq_pure),
    Request.

verified_stream_open(#{caller := Caller} = Keys) ->
    Frame = macula_frame:stream_open((call_spec(Keys))#{mode => server_stream}, Caller),
    {ok, Request} = macula_frame:verify_request(wire(Frame), pq_pure),
    Request.

%% The fields of a CALL request as its signer puts them in tbs; signing adds alg.
request_tbs(#{caller := Caller} = Keys) ->
    #{{text, <<"frame_type">>} => {text, <<"call">>}, {text, <<"caller">>} => macula_node_keys:key_id(Caller),
      {text, <<"request_id">>} => <<7:128>>, {text, <<"realm">>} => ?REALM,
      {text, <<"procedure">>} => {text, ?PROCEDURE}, {text, <<"target">>} => target(Keys),
      {text, <<"deadline">>} => ?DEADLINE, {text, <<"payload">>} => 1}.

reply_tbs(#{request_id := RequestId, request_hash := RequestHash, target := Target}, result) ->
    #{{text, <<"frame_type">>} => {text, <<"result">>}, {text, <<"request_id">>} => RequestId,
      {text, <<"request_hash">>} => RequestHash, {text, <<"responded_by">>} => Target, {text, <<"payload">>} => 1};
reply_tbs(#{request_id := RequestId, request_hash := RequestHash, target := Target}, error) ->
    #{{text, <<"frame_type">>} => {text, <<"error">>}, {text, <<"request_id">>} => RequestId,
      {text, <<"request_hash">>} => RequestHash, {text, <<"responded_by">>} => Target,
      {text, <<"code">>} => {text, <<"closed">>}}.

relay_tbs(#{request_id := RequestId, request_hash := RequestHash}, Station) ->
    #{{text, <<"frame_type">>} => {text, <<"error">>}, {text, <<"request_id">>} => RequestId,
      {text, <<"request_hash">>} => RequestHash, {text, <<"reported_by">>} => macula_node_keys:key_id(Station),
      {text, <<"code">>} => {text, <<"unknown_next_peer">>}}.

verify_crafted_request(FrameType, Tbs, Key) ->
    Frame = crafted(FrameType, request, macula_signed_object:sign(?REQUEST_LABEL, Tbs, Key)),
    macula_frame:verify_request(wire(Frame), pq_pure).

%% A frame around one signed object, with the version the codec writes.
crafted(FrameType, Field, Signed) ->
    Version = macula_frame:version(macula_frame:ping(#{nonce => <<0:128>>})),
    #{version => Version, frame_type => FrameType, Field => Signed}.

%% A frame as a peer receives it: encoded and decoded.
wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

flip(<<Head:20/binary, Byte, Tail/binary>>) ->
    <<Head/binary, (Byte bxor 1), Tail/binary>>.
