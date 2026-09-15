%% EUnit tests for stream frames (DESIGN_PQ_SIGNED_FRAMES_AND_RECORDS.md: Provider stream frames and Caller stream
%% frames; D25 item 5, D17 as revised). A provider's STREAM_DATA, STREAM_END, STREAM_ERROR and STREAM_REPLY carry stream
%% under MACULA-PQ-STREAM-V1: {key, tbs, signature} on its first frame of a stream, {tbs, signature} after. A caller's
%% STREAM_DATA, STREAM_END and STREAM_ERROR carry caller_stream, {tbs, signature} under MACULA-PQ-CALLER-STREAM-V1,
%% verified with the caller key of the STREAM_OPEN. Each side numbers its own frames from 0, and STREAM_END is its last.
-module(macula_frame_stream_tests).

-include_lib("eunit/include/eunit.hrl").

-define(STREAM_LABEL, <<"MACULA-PQ-STREAM-V1">>).
-define(CALLER_STREAM_LABEL, <<"MACULA-PQ-CALLER-STREAM-V1">>).

stream_frames_test_() ->
    {setup, fun keys/0, fun cases/1}.

%% Every case signs inside its own test, so each one passes or fails on its own.
cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_provider_first_frame_carries_its_key_and_verifies_against_the_open/1,
                 fun later_provider_frames_verify_with_the_held_key/1,
                 fun a_provider_frame_out_of_order_is_refused/1,
                 fun a_provider_first_frame_from_a_node_other_than_the_target_is_refused/1,
                 fun a_provider_first_frame_whose_signer_is_not_its_key_is_refused/1,
                 fun a_provider_frame_for_another_stream_is_refused/1,
                 fun nothing_follows_a_provider_stream_end/1,
                 fun a_tampered_provider_frame_is_refused/1,
                 fun provider_frame_fields_the_design_does_not_allow_are_malformed/1,
                 fun a_first_frame_carries_the_provider_key_and_a_later_one_does_not/1,
                 fun a_provider_stream_error_and_stream_reply_carry_their_fields/1,
                 fun a_caller_frame_verifies_with_the_caller_key_from_the_open/1,
                 fun caller_and_provider_count_their_frames_apart/1,
                 fun a_caller_frame_signed_by_another_key_is_refused/1,
                 fun a_caller_frame_whose_signer_is_not_the_caller_is_refused/1,
                 fun caller_stream_data_in_a_server_stream_is_refused/1,
                 fun a_caller_frame_out_of_order_is_refused/1,
                 fun nothing_follows_a_caller_stream_end/1,
                 fun a_caller_has_no_stream_reply/1,
                 fun a_stream_error_frame_carries_exactly_one_signed_object/1]].

%%------------------------------------------------------------------
%% Provider stream frames
%%------------------------------------------------------------------

a_provider_first_frame_carries_its_key_and_verifies_against_the_open(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    First = wire(macula_frame:provider_stream(chunk(0), Provider, Open)),
    ?assertEqual([frame_type, stream, version], lists:sort(maps:keys(First))),
    ?assertEqual([key, signature, tbs], lists:sort(maps:keys(maps:get(stream, First)))),
    ?assertMatch({ok, #{frame_type := stream_data, seq := 0, encoding := raw, body := <<"chunk">>}, _},
                 macula_frame:verify_provider_stream(First, macula_frame:open_stream(Open), pq_pure)),
    {ok, Fields, _State} = macula_frame:verify_provider_stream(First, macula_frame:open_stream(Open), pq_pure),
    ?assertEqual(macula_node_keys:key_id(Provider), maps:get(signer, Fields)).

later_provider_frames_verify_with_the_held_key(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    State = provider_verified(Keys, Open, [chunk(0)]),
    Later = wire(macula_frame:provider_stream(chunk(1), Provider, Open)),
    ?assertEqual([signature, tbs], lists:sort(maps:keys(maps:get(stream, Later)))),
    ?assertMatch({ok, #{seq := 1}, _}, macula_frame:verify_provider_stream(Later, State, pq_pure)).

a_provider_frame_out_of_order_is_refused(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Fresh = macula_frame:open_stream(Open),
    State = provider_verified(Keys, Open, [chunk(0)]),
    Frame = fun(Seq) -> wire(macula_frame:provider_stream(chunk(Seq), Provider, Open)) end,
    ?assertEqual({error, seq_mismatch}, macula_frame:verify_provider_stream(Frame(1), Fresh, pq_pure)),
    ?assertEqual({error, seq_mismatch}, macula_frame:verify_provider_stream(Frame(0), State, pq_pure)),
    ?assertEqual({error, seq_mismatch}, macula_frame:verify_provider_stream(Frame(2), State, pq_pure)).

a_provider_first_frame_from_a_node_other_than_the_target_is_refused(#{other := Other} = Keys) ->
    Open = verified_open(Keys, bidi),
    First = wire(macula_frame:provider_stream(chunk(0), Other, Open)),
    ?assertEqual({error, not_the_target},
                 macula_frame:verify_provider_stream(First, macula_frame:open_stream(Open), pq_pure)).

a_provider_first_frame_whose_signer_is_not_its_key_is_refused(#{provider := Provider, other := Other} = Keys) ->
    Open = verified_open(Keys, bidi),
    Tbs = chunk_tbs(Open, macula_node_keys:key_id(Other), 0),
    Frame = crafted(stream_data, stream, macula_signed_object:sign(?STREAM_LABEL, Tbs, Provider)),
    ?assertEqual({error, key_id_mismatch},
                 macula_frame:verify_provider_stream(wire(Frame), macula_frame:open_stream(Open), pq_pure)).

a_provider_frame_for_another_stream_is_refused(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Another = verified_request(Caller, (open_spec(Keys, bidi))#{request_id => <<8:128>>}),
    First = wire(macula_frame:provider_stream(chunk(0), Provider, Open)),
    ?assertEqual({error, request_mismatch},
                 macula_frame:verify_provider_stream(First, macula_frame:open_stream(Another), pq_pure)).

nothing_follows_a_provider_stream_end(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    State = provider_verified(Keys, Open, [chunk(0), #{frame_type => stream_end, seq => 1, role => both}]),
    After = wire(macula_frame:provider_stream(chunk(2), Provider, Open)),
    ?assertEqual({error, stream_ended}, macula_frame:verify_provider_stream(After, State, pq_pure)).

a_tampered_provider_frame_is_refused(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    #{stream := Signed} = First = macula_frame:provider_stream(chunk(0), Provider, Open),
    Tampered = First#{stream := Signed#{tbs := flip(maps:get(tbs, Signed))}},
    ?assertEqual({error, signature_invalid},
                 macula_frame:verify_provider_stream(wire(Tampered), macula_frame:open_stream(Open), pq_pure)).

provider_frame_fields_the_design_does_not_allow_are_malformed(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Signer = macula_node_keys:key_id(Provider),
    Verify = fun(FrameType, Tbs) ->
        Frame = crafted(FrameType, stream, macula_signed_object:sign(?STREAM_LABEL, Tbs, Provider)),
        macula_frame:verify_provider_stream(wire(Frame), macula_frame:open_stream(Open), pq_pure)
    end,
    Chunk = chunk_tbs(Open, Signer, 0),
    ?assertMatch({ok, _, _}, Verify(stream_data, Chunk)),
    [?assertEqual({error, malformed_frame}, Verify(FrameType, Tbs))
     || {FrameType, Tbs} <- [{stream_data, maps:remove({text, <<"body">>}, Chunk)},
                             {stream_data, Chunk#{{text, <<"encoding">>} := {text, <<"json">>}}},
                             {stream_data, Chunk#{{text, <<"body">>} := {text, <<"chunk">>}}},
                             {stream_data, Chunk#{{text, <<"seq">>} := 1 bsl 53}},
                             {stream_data, Chunk#{{text, <<"role">>} => {text, <<"both">>}}},
                             {stream_data, Chunk#{{text, <<"extra">>} => 1}},
                             {stream_end, end_tbs(Open, Signer, 0, <<"recv">>)},
                             {stream_end, Chunk},
                             {stream_data, Chunk#{{text, <<"frame_type">>} := {text, <<"call">>}}}]].

%% Before the first provider frame the verifier holds no key, so a frame without one can only be a later frame, out of
%% order. After it, a later frame that carries a key has the wrong shape.
a_first_frame_carries_the_provider_key_and_a_later_one_does_not(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Signer = macula_node_keys:key_id(Provider),
    Held = crafted(stream_data, stream,
                   macula_signed_object:sign_held(?STREAM_LABEL, chunk_tbs(Open, Signer, 0), Provider)),
    ?assertEqual({error, seq_mismatch},
                 macula_frame:verify_provider_stream(wire(Held), macula_frame:open_stream(Open), pq_pure)),
    State = provider_verified(Keys, Open, [chunk(0)]),
    Carried = crafted(stream_data, stream,
                      macula_signed_object:sign(?STREAM_LABEL, chunk_tbs(Open, Signer, 1), Provider)),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_provider_stream(wire(Carried), State, pq_pure)).

a_provider_stream_error_and_stream_reply_carry_their_fields(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, client_stream),
    State = provider_verified(Keys, Open, [chunk(0)]),
    Reply = wire(macula_frame:provider_stream(#{frame_type => stream_reply, seq => 1, payload => #{total => 3}},
                                              Provider, Open)),
    {ok, ReplyFields, Replied} = macula_frame:verify_provider_stream(Reply, State, pq_pure),
    ?assertEqual(#{{text, <<"total">>} => 3}, maps:get(payload, ReplyFields)),
    Error = wire(macula_frame:provider_stream(#{frame_type => stream_error, seq => 2, code => <<"overflow">>,
                                                message => <<"too many rows">>}, Provider, Open)),
    ?assertMatch({ok, #{frame_type := stream_error, code := <<"overflow">>, message := <<"too many rows">>}, _},
                 macula_frame:verify_provider_stream(Error, Replied, pq_pure)).

%%------------------------------------------------------------------
%% Caller stream frames
%%------------------------------------------------------------------

a_caller_frame_verifies_with_the_caller_key_from_the_open(#{caller := Caller} = Keys) ->
    Open = verified_open(Keys, bidi),
    Data = #{frame_type => stream_data, seq => 0, encoding => msgpack, body => #{n => 1}},
    Frame = wire(macula_frame:caller_stream(Data, Caller, Open)),
    ?assertEqual([caller_stream, frame_type, version], lists:sort(maps:keys(Frame))),
    ?assertEqual([signature, tbs], lists:sort(maps:keys(maps:get(caller_stream, Frame)))),
    {ok, Fields, _State} = macula_frame:verify_caller_stream(Frame, macula_frame:open_stream(Open), pq_pure),
    ?assertEqual(#{frame_type => stream_data, signer => macula_node_keys:key_id(Caller), seq => 0, encoding => msgpack,
                   body => #{{text, <<"n">>} => 1}},
                 Fields).

caller_and_provider_count_their_frames_apart(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    {ok, _, Provided} = macula_frame:verify_provider_stream(
                          wire(macula_frame:provider_stream(chunk(0), Provider, Open)),
                          macula_frame:open_stream(Open), pq_pure),
    ?assertMatch({ok, #{seq := 0}, _},
                 macula_frame:verify_caller_stream(wire(macula_frame:caller_stream(chunk(0), Caller, Open)),
                                                   Provided, pq_pure)).

a_caller_frame_signed_by_another_key_is_refused(#{other := Other} = Keys) ->
    Open = verified_open(Keys, bidi),
    Frame = wire(macula_frame:caller_stream(chunk(0), Other, Open)),
    ?assertEqual({error, signature_invalid},
                 macula_frame:verify_caller_stream(Frame, macula_frame:open_stream(Open), pq_pure)).

a_caller_frame_whose_signer_is_not_the_caller_is_refused(#{caller := Caller, other := Other} = Keys) ->
    Open = verified_open(Keys, bidi),
    Tbs = chunk_tbs(Open, macula_node_keys:key_id(Other), 0),
    Frame = crafted(stream_data, caller_stream, macula_signed_object:sign_held(?CALLER_STREAM_LABEL, Tbs, Caller)),
    ?assertEqual({error, key_id_mismatch},
                 macula_frame:verify_caller_stream(wire(Frame), macula_frame:open_stream(Open), pq_pure)).

caller_stream_data_in_a_server_stream_is_refused(#{caller := Caller} = Keys) ->
    Open = verified_open(Keys, server_stream),
    ?assertError(function_clause, macula_frame:caller_stream(chunk(0), Caller, Open)),
    Tbs = chunk_tbs(Open, macula_node_keys:key_id(Caller), 0),
    Frame = crafted(stream_data, caller_stream, macula_signed_object:sign_held(?CALLER_STREAM_LABEL, Tbs, Caller)),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_caller_stream(wire(Frame), macula_frame:open_stream(Open), pq_pure)),
    End = wire(macula_frame:caller_stream(#{frame_type => stream_end, seq => 0, role => both}, Caller, Open)),
    ?assertMatch({ok, #{frame_type := stream_end}, _},
                 macula_frame:verify_caller_stream(End, macula_frame:open_stream(Open), pq_pure)).

a_caller_frame_out_of_order_is_refused(#{caller := Caller} = Keys) ->
    Open = verified_open(Keys, bidi),
    Skipped = wire(macula_frame:caller_stream(chunk(1), Caller, Open)),
    ?assertEqual({error, seq_mismatch},
                 macula_frame:verify_caller_stream(Skipped, macula_frame:open_stream(Open), pq_pure)).

nothing_follows_a_caller_stream_end(#{caller := Caller} = Keys) ->
    Open = verified_open(Keys, bidi),
    End = wire(macula_frame:caller_stream(#{frame_type => stream_end, seq => 0, role => send}, Caller, Open)),
    {ok, _, Ended} = macula_frame:verify_caller_stream(End, macula_frame:open_stream(Open), pq_pure),
    After = wire(macula_frame:caller_stream(chunk(1), Caller, Open)),
    ?assertEqual({error, stream_ended}, macula_frame:verify_caller_stream(After, Ended, pq_pure)).

%% STREAM_REPLY has no caller_stream field: a peer's frame carrying one is refused as it is decoded, and the same frame
%% built in process is refused by the verifier.
a_caller_has_no_stream_reply(#{caller := Caller} = Keys) ->
    Open = verified_open(Keys, bidi),
    ?assertError(function_clause,
                 macula_frame:caller_stream(#{frame_type => stream_reply, seq => 0, payload => 1}, Caller, Open)),
    Chunk = chunk_tbs(Open, macula_node_keys:key_id(Caller), 0),
    Tbs = Chunk#{{text, <<"frame_type">>} := {text, <<"stream_reply">>}},
    Frame = crafted(stream_reply, caller_stream, macula_signed_object:sign_held(?CALLER_STREAM_LABEL, Tbs, Caller)),
    ?assertEqual({error, bad_frame}, macula_frame:decode(macula_frame:encode(Frame))),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_caller_stream(Frame, macula_frame:open_stream(Open), pq_pure)).

a_stream_error_frame_carries_exactly_one_signed_object(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Fresh = macula_frame:open_stream(Open),
    ErrorSpec = #{frame_type => stream_error, seq => 0, code => <<"gone">>, message => <<"gone">>},
    #{stream := Stream} = ProviderError = macula_frame:provider_stream(ErrorSpec, Provider, Open),
    #{caller_stream := CallerStream} = CallerError = macula_frame:caller_stream(ErrorSpec, Caller, Open),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_provider_stream(wire(CallerError), Fresh, pq_pure)),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_caller_stream(wire(ProviderError), Fresh, pq_pure)),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_provider_stream(wire(ProviderError#{caller_stream => CallerStream}), Fresh,
                                                     pq_pure)),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_caller_stream(wire(CallerError#{stream => Stream}), Fresh, pq_pure)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys() ->
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key end,
    #{caller => Generate(), provider => Generate(), other => Generate()}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

chunk(Seq) ->
    #{frame_type => stream_data, seq => Seq, encoding => raw, body => <<"chunk">>}.

open_spec(#{provider := Provider}, Mode) ->
    #{request_id => <<7:128>>, realm => <<1:256>>, procedure => <<"acme/count_v1">>,
      target => macula_node_keys:key_id(Provider), deadline => 1789000600000, payload => #{}, mode => Mode}.

verified_open(#{caller := Caller} = Keys, Mode) ->
    verified_request(Caller, open_spec(Keys, Mode)).

verified_request(Caller, Spec) ->
    {ok, Request} = macula_frame:verify_request(wire(macula_frame:stream_open(Spec, Caller)), pq_pure),
    Request.

%% The stream state after the provider frames given, each verified in turn.
provider_verified(#{provider := Provider}, Open, Specs) ->
    lists:foldl(fun(Spec, State) ->
                    Frame = wire(macula_frame:provider_stream(Spec, Provider, Open)),
                    {ok, _Fields, Next} = macula_frame:verify_provider_stream(Frame, State, pq_pure),
                    Next
                end, macula_frame:open_stream(Open), Specs).

chunk_tbs(#{request_id := RequestId, request_hash := RequestHash}, Signer, Seq) ->
    #{{text, <<"frame_type">>} => {text, <<"stream_data">>}, {text, <<"request_id">>} => RequestId,
      {text, <<"request_hash">>} => RequestHash, {text, <<"signer">>} => Signer, {text, <<"seq">>} => Seq,
      {text, <<"encoding">>} => {text, <<"raw">>}, {text, <<"body">>} => <<"chunk">>}.

end_tbs(#{request_id := RequestId, request_hash := RequestHash}, Signer, Seq, Role) ->
    #{{text, <<"frame_type">>} => {text, <<"stream_end">>}, {text, <<"request_id">>} => RequestId,
      {text, <<"request_hash">>} => RequestHash, {text, <<"signer">>} => Signer, {text, <<"seq">>} => Seq,
      {text, <<"role">>} => {text, Role}}.

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
