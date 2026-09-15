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
                 macula_frame:verify_relay_error(decoded(Bytes), Request, pq_pure)).

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

%% The tag that marks bytes built here appears in no other module, in a construction or a match, so no other module can
%% write bytes onto a stream as if stream_bytes/2 had built them.
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

verified_open(#{caller := Caller} = Keys) ->
    Frame = macula_frame:stream_open((call_spec(Keys))#{mode => bidi}, Caller),
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
