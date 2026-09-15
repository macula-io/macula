%% EUnit tests for macula_frame:stream_bytes/2, the one step every frame built locally for a dedicated stream passes
%% through before its bytes are written: it signs and encodes the frame under the key's profile, or returns an error with
%% nothing to write. The bytes of each build kind decode and verify. An unsendable payload, body or reply, and a frame
%% without an identity key or a verified STREAM_OPEN, give an error, never a raise. An error frame carries only the code
%% it was given. The result is tagged, and no module other than macula_frame builds or reads that tag.
-module(macula_frame_stream_bytes_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<1:256>>).
-define(DEADLINE, 1789000600000).
-define(UNSENDABLE, #{<<"unsendable">> => {1, 2}}).

stream_bytes_test_() ->
    {setup, fun keys/0, fun cases/1}.

%% Every case builds inside its own test, so each one passes or fails on its own.
cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_call_verifies_as_its_callers_request/1,
                 fun a_stream_open_verifies_with_its_mode/1,
                 fun a_provider_stream_frame_verifies_against_its_open/1,
                 fun a_caller_stream_frame_verifies_against_its_open/1,
                 fun a_result_verifies_as_the_targets_reply/1,
                 fun a_provider_error_carries_only_the_code_it_was_given/1,
                 fun a_relay_error_carries_only_its_fixed_code/1,
                 fun an_unsendable_payload_is_an_error_with_nothing_to_write/1,
                 fun an_unsendable_stream_body_or_reply_is_an_error/1,
                 fun a_frame_without_an_identity_key_or_open_is_unsignable/1,
                 fun a_frame_over_the_frame_cap_is_an_error_not_a_raise/1,
                 fun a_key_that_is_not_the_verified_sender_is_unsignable/1,
                 fun a_key_of_the_other_profile_is_unsignable/1,
                 fun text_that_is_not_utf8_is_a_named_error/1,
                 fun a_provider_detail_over_256_bytes_is_refused/1,
                 fun a_relay_error_takes_no_free_detail_and_no_code_outside_its_set/1,
                 fun a_build_key_the_frame_does_not_have_is_refused/1,
                 fun a_later_provider_frame_verifies_with_the_held_key/1,
                 fun a_stream_end_error_and_reply_verify/1,
                 fun a_stream_frame_for_another_open_is_refused/1,
                 fun a_local_error_term_as_provider_code_or_detail_writes_nothing/1,
                 fun a_provider_code_over_64_bytes_is_refused/1,
                 fun a_stream_error_code_or_message_over_its_bound_is_refused/1,
                 fun a_caller_frame_its_side_may_not_send_is_a_named_error/1,
                 fun no_other_module_builds_or_reads_the_stream_bytes_tag/1]].

a_call_verifies_as_its_callers_request(#{caller := Caller} = Keys) ->
    {ok, Bytes} = macula_frame:stream_bytes({call, call_spec(Keys)}, Caller),
    CallerId = macula_node_keys:key_id(Caller),
    ?assertMatch({ok, #{frame_type := call, caller := CallerId, procedure := <<"acme/get_forecast_v1">>}},
                 macula_frame:verify_request(decoded(Bytes), pq_pure)).

a_stream_open_verifies_with_its_mode(#{caller := Caller} = Keys) ->
    {ok, Bytes} = macula_frame:stream_bytes({stream_open, (call_spec(Keys))#{mode => bidi}}, Caller),
    ?assertMatch({ok, #{frame_type := stream_open, mode := bidi}},
                 macula_frame:verify_request(decoded(Bytes), pq_pure)).

a_provider_stream_frame_verifies_against_its_open(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys),
    {ok, Bytes} = macula_frame:stream_bytes({provider_stream, chunk(0), Open}, Provider),
    ?assertMatch({ok, #{frame_type := stream_data, seq := 0, encoding := raw, body := <<"chunk">>}, _},
                 macula_frame:verify_provider_stream(decoded(Bytes), macula_frame:open_stream(Open), pq_pure)).

a_caller_stream_frame_verifies_against_its_open(#{caller := Caller} = Keys) ->
    Open = verified_open(Keys),
    {ok, Bytes} = macula_frame:stream_bytes({caller_stream, chunk(0), Open}, Caller),
    ?assertMatch({ok, #{frame_type := stream_data, seq := 0, encoding := raw, body := <<"chunk">>}, _},
                 macula_frame:verify_caller_stream(decoded(Bytes), macula_frame:open_stream(Open), pq_pure)).

a_result_verifies_as_the_targets_reply(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    {ok, Bytes} = macula_frame:stream_bytes({result, #{request => Request, payload => #{temp => 21}}}, Provider),
    ?assertEqual({ok, #{frame_type => result, responded_by => target(Keys), payload => #{{text, <<"temp">>} => 21}}},
                 macula_frame:verify_reply(decoded(Bytes), Request, pq_pure)).

a_provider_error_carries_only_the_code_it_was_given(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    {ok, Bytes} = macula_frame:stream_bytes({provider_error, #{request => Request, code => <<"unavailable">>}},
                                            Provider),
    ?assertEqual({ok, #{frame_type => error, responded_by => target(Keys), code => <<"unavailable">>}},
                 macula_frame:verify_reply(decoded(Bytes), Request, pq_pure)).

a_relay_error_carries_only_its_fixed_code(#{station := Station} = Keys) ->
    Request = verified_call(Keys),
    {ok, Bytes} = macula_frame:stream_bytes({relay_error, #{frame_type => stream_error, request => Request,
                                                            code => unknown_next_peer}}, Station),
    ?assertEqual({ok, #{frame_type => stream_error, reported_by => macula_node_keys:key_id(Station),
                        code => unknown_next_peer}},
                 macula_frame:verify_relay_error(decoded(Bytes), Request, pq_pure, macula_node_keys:key_id(Station))).

%% A payload the wire cannot carry is the local caller's error: nothing is built, signed or returned to write.
an_unsendable_payload_is_an_error_with_nothing_to_write(#{caller := Caller, provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    ?assertMatch({error, {unsupported_payload_type, _, _}},
                 macula_frame:stream_bytes({call, (call_spec(Keys))#{payload => ?UNSENDABLE}}, Caller)),
    ?assertMatch({error, {unsupported_payload_type, _, _}},
                 macula_frame:stream_bytes({stream_open, (call_spec(Keys))#{mode => bidi, payload => ?UNSENDABLE}},
                                           Caller)),
    ?assertMatch({error, {unsupported_payload_type, _, _}},
                 macula_frame:stream_bytes({result, #{request => Request, payload => ?UNSENDABLE}}, Provider)).

an_unsendable_stream_body_or_reply_is_an_error(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys),
    Body = #{frame_type => stream_data, seq => 0, encoding => msgpack, body => ?UNSENDABLE},
    Reply = #{frame_type => stream_reply, seq => 0, payload => ?UNSENDABLE},
    ?assertMatch({error, {unsupported_payload_type, _, _}},
                 macula_frame:stream_bytes({provider_stream, Body, Open}, Provider)),
    ?assertMatch({error, {unsupported_payload_type, _, _}},
                 macula_frame:stream_bytes({caller_stream, Body, Open}, Caller)),
    ?assertMatch({error, {unsupported_payload_type, _, _}},
                 macula_frame:stream_bytes({provider_stream, Reply, Open}, Provider)).

%% A frame needs an identity key to sign it, and a stream frame the verified STREAM_OPEN it belongs to.
a_frame_without_an_identity_key_or_open_is_unsignable(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys),
    ?assertEqual({error, unsignable}, macula_frame:stream_bytes({call, call_spec(Keys)}, undefined)),
    ?assertEqual({error, unsignable}, macula_frame:stream_bytes({provider_stream, chunk(0), Open}, undefined)),
    ?assertEqual({error, unsignable}, macula_frame:stream_bytes({provider_stream, chunk(0), undefined}, Provider)).

%% A payload whose byte floor passes the payload check can still make a frame over the 16 MiB frame cap once it is
%% signed and encoded: that is an error with nothing to write, never a raise.
a_frame_over_the_frame_cap_is_an_error_not_a_raise(#{caller := Caller} = Keys) ->
    Payload = binary:copy(<<0>>, 16#FFFFFF - 16),
    ?assertEqual(ok, macula_frame:check_payload(Payload)),
    ?assertEqual({error, frame_too_large},
                 macula_frame:stream_bytes({call, (call_spec(Keys))#{payload => Payload}}, Caller)).

%% Bytes are built only for the sender its receiver verifies: the STREAM_OPEN's target for a provider's stream frame, its
%% caller for a caller's, the request's target for a reply. Any other identity key is unsignable, so stream_bytes/2
%% never returns bytes the receiver refuses.
a_key_that_is_not_the_verified_sender_is_unsignable(#{caller := Caller, provider := Provider, station := Station} = Keys) ->
    Open = verified_open(Keys),
    Request = verified_call(Keys),
    ?assertEqual({error, unsignable}, macula_frame:stream_bytes({provider_stream, chunk(0), Open}, Caller)),
    ?assertEqual({error, unsignable}, macula_frame:stream_bytes({caller_stream, chunk(0), Open}, Provider)),
    ?assertEqual({error, unsignable},
                 macula_frame:stream_bytes({result, #{request => Request, payload => 1}}, Caller)),
    ?assertEqual({error, unsignable},
                 macula_frame:stream_bytes({provider_error, #{request => Request, code => <<"closed">>}}, Station)).

%% A key id holds its profile, so a key of the other profile never passes for the sender.
a_key_of_the_other_profile_is_unsignable(Keys) ->
    {ok, Hybrid} = macula_node_keys:generate(identity, pq_hybrid),
    ?assertEqual({error, unsignable},
                 macula_frame:stream_bytes({provider_stream, chunk(0), verified_open(Keys)}, Hybrid)).

%% Text a receiver reads as text must be valid UTF-8; otherwise the build is a named error and nothing is written.
text_that_is_not_utf8_is_a_named_error(#{caller := Caller, provider := Provider} = Keys) ->
    Bad = <<16#ff, 16#fe>>,
    Request = verified_call(Keys),
    Open = verified_open(Keys),
    Error = #{frame_type => stream_error, seq => 0, code => <<"c">>, message => <<"m">>},
    ?assertEqual({error, {invalid_text, procedure}},
                 macula_frame:stream_bytes({call, (call_spec(Keys))#{procedure => Bad}}, Caller)),
    ?assertEqual({error, {invalid_text, code}},
                 macula_frame:stream_bytes({provider_error, #{request => Request, code => Bad}}, Provider)),
    ?assertEqual({error, {invalid_text, detail}},
                 macula_frame:stream_bytes({provider_error, #{request => Request, code => <<"c">>, detail => Bad}},
                                           Provider)),
    ?assertEqual({error, {invalid_text, code}},
                 macula_frame:stream_bytes({provider_stream, Error#{code => Bad}, Open}, Provider)),
    ?assertEqual({error, {invalid_text, message}},
                 macula_frame:stream_bytes({provider_stream, Error#{message => Bad}, Open}, Provider)).

a_provider_detail_over_256_bytes_is_refused(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    Error = #{request => Request, code => <<"closed">>},
    ?assertEqual({error, {text_too_long, detail}},
                 macula_frame:stream_bytes({provider_error, Error#{detail => binary:copy(<<"a">>, 257)}}, Provider)),
    ?assertMatch({ok, _},
                 macula_frame:stream_bytes({provider_error, Error#{detail => binary:copy(<<"a">>, 256)}}, Provider)).

%% A relay error carries a code from its closed set and no free text.
a_relay_error_takes_no_free_detail_and_no_code_outside_its_set(#{station := Station} = Keys) ->
    Relay = #{frame_type => error, request => verified_call(Keys), code => unknown_next_peer},
    ?assertEqual({error, {unknown_build_key, detail}},
                 macula_frame:stream_bytes({relay_error, Relay#{detail => <<"no route">>}}, Station)),
    ?assertEqual({error, relay_code_outside_its_set},
                 macula_frame:stream_bytes({relay_error, Relay#{code => made_up}}, Station)).

%% A build names only the fields its frame has.
a_build_key_the_frame_does_not_have_is_refused(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys),
    ?assertEqual({error, {unknown_build_key, surprise}},
                 macula_frame:stream_bytes({call, (call_spec(Keys))#{surprise => 1}}, Caller)),
    ?assertEqual({error, {unknown_build_key, mode}},
                 macula_frame:stream_bytes({call, (call_spec(Keys))#{mode => bidi}}, Caller)),
    ?assertEqual({error, {unknown_build_key, payload}},
                 macula_frame:stream_bytes({provider_stream, (chunk(0))#{payload => 1}, Open}, Provider)).

a_later_provider_frame_verifies_with_the_held_key(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys),
    {ok, First} = macula_frame:stream_bytes({provider_stream, chunk(0), Open}, Provider),
    {ok, Later} = macula_frame:stream_bytes({provider_stream, chunk(1), Open}, Provider),
    {ok, _Fields, Held} = macula_frame:verify_provider_stream(decoded(First), macula_frame:open_stream(Open), pq_pure),
    ?assertMatch({ok, #{seq := 1, body := <<"chunk">>}, _}, macula_frame:verify_provider_stream(decoded(Later), Held, pq_pure)).

a_stream_end_error_and_reply_verify(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys),
    Specs = [#{frame_type => stream_end, seq => 0, role => both},
             #{frame_type => stream_error, seq => 0, code => <<"c">>, message => <<"m">>},
             #{frame_type => stream_reply, seq => 0, payload => #{n => 1}}],
    ?assertEqual([stream_end, stream_error, stream_reply],
                 [begin
                      {ok, Bytes} = macula_frame:stream_bytes({provider_stream, Spec, Open}, Provider),
                      {ok, #{frame_type := Type}, _} =
                          macula_frame:verify_provider_stream(decoded(Bytes), macula_frame:open_stream(Open), pq_pure),
                      Type
                  end || Spec <- Specs]).

a_stream_frame_for_another_open_is_refused(#{provider := Provider} = Keys) ->
    {ok, Bytes} = macula_frame:stream_bytes({provider_stream, chunk(0), verified_open(Keys)}, Provider),
    Other = verified_open(Keys, <<8:128>>),
    ?assertEqual({error, request_mismatch},
                 macula_frame:verify_provider_stream(decoded(Bytes), macula_frame:open_stream(Other), pq_pure)).

%% A local error, such as the refusal stream_bytes/2 itself returns or the Path inside it, is never error text for a
%% peer: as a provider's code or detail it is refused, and nothing is written.
a_local_error_term_as_provider_code_or_detail_writes_nothing(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    Local = {unsupported_payload_type, tuple, [<<"payload">>, 3]},
    ?assertEqual({error, {invalid_text, code}},
                 macula_frame:stream_bytes({provider_error, #{request => Request, code => Local}}, Provider)),
    ?assertEqual({error, {invalid_text, detail}},
                 macula_frame:stream_bytes({provider_error, #{request => Request, code => <<"c">>,
                                                             detail => [<<"payload">>, 3]}}, Provider)).

a_provider_code_over_64_bytes_is_refused(#{provider := Provider} = Keys) ->
    Request = verified_call(Keys),
    ?assertEqual({error, {text_too_long, code}},
                 macula_frame:stream_bytes({provider_error, #{request => Request, code => binary:copy(<<"c">>, 65)}},
                                           Provider)),
    ?assertMatch({ok, _},
                 macula_frame:stream_bytes({provider_error, #{request => Request, code => binary:copy(<<"c">>, 64)}},
                                           Provider)).

%% A stream error's code is at most 64 bytes, as a provider error's is, and its message at most 256 bytes.
a_stream_error_code_or_message_over_its_bound_is_refused(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys),
    StreamError = fun(Code, Message) ->
                      macula_frame:stream_bytes({provider_stream, #{frame_type => stream_error, seq => 0, code => Code,
                                                                    message => Message}, Open}, Provider)
                  end,
    ?assertEqual({error, {text_too_long, code}}, StreamError(binary:copy(<<"c">>, 65), <<>>)),
    ?assertEqual({error, {text_too_long, message}}, StreamError(<<"c">>, binary:copy(<<"m">>, 257))),
    ?assertMatch({ok, _}, StreamError(binary:copy(<<"c">>, 64), binary:copy(<<"m">>, 256))).

%% A caller sends no STREAM_REPLY, and no STREAM_DATA in a server_stream: asking for one, which a caller's own stream
%% state can do, is a named error, not a raise.
a_caller_frame_its_side_may_not_send_is_a_named_error(#{caller := Caller} = Keys) ->
    Bidi = verified_open(Keys),
    ServerStream = verified_open(Keys, <<9:128>>, server_stream),
    ?assertEqual({error, {not_allowed, stream_reply}},
                 macula_frame:stream_bytes({caller_stream, #{frame_type => stream_reply, seq => 0, payload => 1}, Bidi},
                                           Caller)),
    ?assertEqual({error, {not_allowed, stream_data}},
                 macula_frame:stream_bytes({caller_stream, chunk(0), ServerStream}, Caller)).

%% The tag that marks bytes built here appears in no other module, in a construction or a match, so no other module can
%% write bytes onto a stream as if stream_bytes/2 had built them. The search sees literal tuples only: a record of that
%% name, list_to_tuple/1 or an untagged element/2 read would pass it.
no_other_module_builds_or_reads_the_stream_bytes_tag(_Keys) ->
    {ok, Modules} = application:get_key(macula, modules),
    %% The search must find the tag where it is, or finding it nowhere else proves nothing.
    ?assert(mentions_tag(macula_frame, macula_stream_bytes)),
    ?assertEqual([], [Module || Module <- Modules, Module =/= macula_frame,
                                mentions_tag(Module, macula_stream_bytes)]).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    _ = application:load(macula),
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key end,
    #{caller => Generate(), provider => Generate(), station => Generate()}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

target(#{provider := Provider}) ->
    macula_node_keys:key_id(Provider).

call_spec(Keys) ->
    #{request_id => <<7:128>>, realm => ?REALM, procedure => <<"acme/get_forecast_v1">>, target => target(Keys),
      deadline => ?DEADLINE, payload => #{city => {text, <<"Tienen">>}}}.

chunk(Seq) ->
    #{frame_type => stream_data, seq => Seq, encoding => raw, body => <<"chunk">>}.

verified_call(#{caller := Caller} = Keys) ->
    {ok, Request} = macula_frame:verify_request(wire(macula_frame:call(call_spec(Keys), Caller)), pq_pure),
    Request.

verified_open(Keys) ->
    verified_open(Keys, <<7:128>>).

verified_open(Keys, RequestId) ->
    verified_open(Keys, RequestId, bidi).

verified_open(#{caller := Caller} = Keys, RequestId, Mode) ->
    Frame = macula_frame:stream_open((call_spec(Keys))#{mode => Mode, request_id => RequestId}, Caller),
    {ok, Request} = macula_frame:verify_request(wire(Frame), pq_pure),
    Request.

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

decoded(StreamBytes) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:written_bytes(StreamBytes)),
    Decoded.

%% Whether a module's abstract code holds a tuple whose first element is Tag, anywhere. The beam is read from the
%% application's ebin, since a cover-compiled module has no beam file of its own.
mentions_tag(Module, Tag) ->
    Beam = filename:join([code:lib_dir(macula), "ebin", atom_to_list(Module) ++ ".beam"]),
    {ok, {Module, [{debug_info, {debug_info_v1, Backend, Data}}]}} = beam_lib:chunks(Beam, [debug_info]),
    {ok, Forms} = Backend:debug_info(erlang_v1, Module, Data, []),
    holds_tag_tuple(Forms, Tag).

holds_tag_tuple({tuple, _Anno, [{atom, _, Tag} | _]}, Tag) -> true;
holds_tag_tuple(Term, Tag) when is_tuple(Term)            -> holds_tag_tuple(tuple_to_list(Term), Tag);
holds_tag_tuple([Head | Tail], Tag)                        -> holds_tag_tuple(Head, Tag) orelse holds_tag_tuple(Tail, Tag);
holds_tag_tuple(_Leaf, _Tag)                               -> false.
