%% A link-carried macula_stream started with the stream keys its STREAM_OPEN
%% agreed (E2E design §5.2) seals what it sends and opens what it receives:
%% a caller's frames under k_c2p with the nonce its signed seq gives, a
%% provider's under k_p2c with a random nonce each frame carries, every frame's
%% AAD naming its type, request, seq and direction. What a reader gets is the
%% plaintext, as a clear stream delivers it. A sealed frame that does not open,
%% and a clear frame where a sealed one belongs, end the session; a provider's
%% clear STREAM_ERROR is taken only from the closed set of admission refusals.
%% The test process stands in for the stream's link and connection.
-module(macula_stream_sealed_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SID, <<9:128>>).
-define(KEY_ID, <<1:64>>).

sealed_stream_test_() ->
    {setup, fun keys/0, fun cases/1}.

cases(Keys) ->
    [{case_name(Case), {spawn, fun() -> process_flag(trap_exit, true), Case(Keys) end}}
     || Case <- [fun a_callers_chunks_seal_as_the_vectors_do/1,
                 fun a_providers_chunk_from_the_vectors_opens_for_the_reader/1,
                 fun a_providers_frames_seal_under_their_own_random_nonces/1,
                 fun a_callers_sealed_chunks_reach_the_providers_reader/1,
                 fun a_sealed_reply_and_error_reach_the_caller_as_themselves/1,
                 fun a_sealed_frame_that_does_not_open_ends_the_session/1,
                 fun a_sealed_frame_under_another_key_id_ends_the_session/1,
                 fun a_clear_chunk_on_a_sealed_stream_ends_the_session/1,
                 fun a_clear_admission_refusal_reaches_the_reader_as_itself/1,
                 fun a_clear_error_outside_the_closed_set_ends_the_session/1,
                 fun a_stream_end_stays_clear/1,
                 fun a_refused_open_reopens_once_under_the_resealed_key/1,
                 fun a_refused_open_after_a_send_ends_naming_the_key/1,
                 fun a_reseal_that_fails_ends_with_its_reason/1,
                 fun a_second_refusal_ends_the_stream/1,
                 fun a_refusal_without_a_reseal_ends_naming_the_key/1]].

%% The caller's nonce is its seq, so its ciphertext is the vectors' byte for
%% byte (E2E_SEAL_V1.md, the stream_open entry's frames in direction 0).
a_callers_chunks_seal_as_the_vectors_do(#{caller := Caller} = Keys) ->
    V = vector(),
    Open = verified_open(Keys, bidi, x(V, <<"request_id">>)),
    Stream = stream(client, Caller, Open, vector_seal(V)),
    [ok = macula_stream:send(Stream, x(F, <<"plain">>)) || F <- vector_frames(V, 0)],
    Sent = [sent() || _ <- vector_frames(V, 0)],
    {Read, _} = lists:mapfoldl(fun({Frame, false}, St) ->
                                   {ok, #{sealed := Sealed, encoding := raw} = Fields, Next} =
                                       macula_frame:verify_caller_stream(Frame, St, pq_pure),
                                   ?assertNot(is_map_key(body, Fields)),
                                   ?assertNot(is_map_key(nonce, Sealed)),
                                   {maps:get(ct, Sealed), Next}
                               end, macula_frame:open_stream(Open), Sent),
    ?assertEqual([x(F, <<"ct">>) || F <- vector_frames(V, 0)], Read),
    gen_server:stop(Stream).

%% A provider's frame carries its nonce: the vectors' first provider chunk,
%% delivered to the caller, reaches its reader as the vector's plaintext.
a_providers_chunk_from_the_vectors_opens_for_the_reader(#{caller := Caller, provider := Provider} = Keys) ->
    V = vector(),
    Open = verified_open(Keys, bidi, x(V, <<"request_id">>)),
    Stream = stream(client, Caller, Open, vector_seal(V)),
    [F | _] = vector_frames(V, 1),
    Sealed = #{scheme => 1, key_id => ?KEY_ID, nonce => x(F, <<"nonce">>), ct => x(F, <<"ct">>)},
    ok = macula_stream:deliver_frame(Stream, provider_frame(#{frame_type => stream_data, seq => 0, encoding => raw,
                                                              sealed => Sealed}, Provider, Open)),
    ?assertEqual({chunk, x(F, <<"plain">>)}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

%% A provider's chunk, reply and error each seal under k_p2c with a random nonce
%% of their own, in direction 1, and open to what was sent.
a_providers_frames_seal_under_their_own_random_nonces(#{provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    #{k_p2c := KP2C} = Seal = seal(),
    Stream = stream(server, Provider, Open, Seal),
    ok = macula_stream:send(Stream, <<"one">>),
    ok = macula_stream:send(Stream, #{n => 2}, msgpack),
    ok = macula_stream:set_reply(Stream, #{done => 1}),
    Frames = [sent() || _ <- [1, 2, 3]],
    {Opened, _} = lists:mapfoldl(fun({Frame, _Last}, St) ->
                                     {ok, #{frame_type := Type, seq := Seq, sealed := #{nonce := N, ct := Ct}}, Next} =
                                         macula_frame:verify_provider_stream(Frame, St, pq_pure),
                                     Aad = macula_seal:stream_aad(atom_to_binary(Type), request_id(Open), Seq, 1),
                                     {ok, Plain} = macula_seal:open(KP2C, N, Aad, Ct),
                                     {{Type, N, Plain}, Next}
                                 end, macula_frame:open_stream(Open), Frames),
    [{stream_data, N0, P0}, {stream_data, N1, P1}, {stream_reply, N2, P2}] = Opened,
    ?assertEqual(<<"one">>, P0),
    ?assertEqual({ok, #{{text, <<"n">>} => 2}}, macula_frame:plain_payload(P1)),
    ?assertEqual({ok, #{{text, <<"done">>} => 1}}, macula_frame:plain_payload(P2)),
    ?assertEqual(3, length(lists:usort([N0, N1, N2]))),
    Error = stream(server, Provider, Open, Seal),
    ok = macula_stream:abort(Error, <<"bad_city">>, <<"no such city">>),
    {ErrorFrame, true} = sent(),
    {ok, #{sealed := #{nonce := EN, ct := ECt}}, _} =
        macula_frame:verify_provider_stream(ErrorFrame, macula_frame:open_stream(Open), pq_pure),
    {ok, EPlain} = macula_seal:open(KP2C, EN, macula_seal:stream_aad(<<"stream_error">>, request_id(Open), 0, 1), ECt),
    ?assertEqual({ok, #{code => <<"bad_city">>, detail => <<"no such city">>}}, macula_frame:plain_error(EPlain)),
    gen_server:stop(Stream).

%% The provider's side opens the caller's sealed chunks, raw and msgpack, for
%% its reader.
a_callers_sealed_chunks_reach_the_providers_reader(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    #{k_c2p := KC2P} = Seal = seal(),
    Stream = stream(server, Provider, Open, Seal),
    {ok, TermPlain} = macula_frame:payload_plain(#{city => {text, <<"Tienen">>}}),
    ok = macula_stream:deliver_frame(Stream, caller_frame(0, raw, <<"raw bytes">>, KC2P, Caller, Open)),
    ok = macula_stream:deliver_frame(Stream, caller_frame(1, msgpack, TermPlain, KC2P, Caller, Open)),
    ?assertEqual({chunk, <<"raw bytes">>}, macula_stream:recv(Stream, 1000)),
    ?assertEqual({data, #{{text, <<"city">>} => {text, <<"Tienen">>}}}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

%% A sealed STREAM_REPLY is the caller's reply, and a sealed STREAM_ERROR its
%% error, as they are in the clear.
a_sealed_reply_and_error_reach_the_caller_as_themselves(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    #{k_p2c := KP2C} = Seal = seal(),
    Stream = stream(client, Caller, Open, Seal),
    {ok, ReplyPlain} = macula_frame:payload_plain(#{total => 3}),
    ok = macula_stream:deliver_frame(Stream, provider_sealed(stream_reply, 0, #{}, ReplyPlain, KP2C, Provider, Open)),
    ?assertEqual({ok, #{{text, <<"total">>} => 3}}, macula_stream:await_reply(Stream, 1000)),
    gen_server:stop(Stream),
    Erring = stream(client, Caller, Open, Seal),
    {ok, ErrorPlain} = macula_frame:error_plain(#{code => <<"bad_city">>, detail => <<"no such city">>}),
    ok = macula_stream:deliver_frame(Erring, provider_sealed(stream_error, 0, #{}, ErrorPlain, KP2C, Provider, Open)),
    ?assertEqual({error, {<<"bad_city">>, <<"no such city">>}}, macula_stream:recv(Erring, 1000)),
    gen_server:stop(Erring).

%% A sealed frame that does not open under its direction's key ends the
%% session as malformed_frame, and nothing of it reaches the reader.
a_sealed_frame_that_does_not_open_ends_the_session(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    #{k_c2p := KC2P} = Seal = seal(),
    Stream = stream(client, Caller, Open, Seal),
    ok = macula_stream:deliver_frame(Stream, provider_sealed(stream_data, 0, #{encoding => raw}, <<"x">>, KC2P,
                                                             Provider, Open)),
    ?assertMatch({error, {<<"malformed_frame">>, _}}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

a_sealed_frame_under_another_key_id_ends_the_session(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    #{k_p2c := KP2C} = Seal = seal(),
    Stream = stream(client, Caller, Open, Seal),
    Aad = macula_seal:stream_aad(<<"stream_data">>, request_id(Open), 0, 1),
    Nonce = macula_seal:random_nonce(),
    Sealed = #{scheme => 1, key_id => <<2:64>>, nonce => Nonce, ct => macula_seal:seal(KP2C, Nonce, Aad, <<"x">>)},
    ok = macula_stream:deliver_frame(Stream, provider_frame(#{frame_type => stream_data, seq => 0, encoding => raw,
                                                              sealed => Sealed}, Provider, Open)),
    ?assertMatch({error, {<<"malformed_frame">>, _}}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

%% A sealed stream carries no clear chunk: one is a downgrade, and ends the
%% session rather than reaching the reader.
a_clear_chunk_on_a_sealed_stream_ends_the_session(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open, seal()),
    ok = macula_stream:deliver_frame(Stream, provider_frame(#{frame_type => stream_data, seq => 0, encoding => raw,
                                                              body => <<"clear">>}, Provider, Open)),
    ?assertMatch({error, {<<"malformed_frame">>, _}}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

%% A provider refuses a sealed open in the clear for admission, from the
%% closed set: the reader gets the refusal as it came. (A clear
%% `sealed_refused' is the reseal's, below.)
a_clear_admission_refusal_reaches_the_reader_as_itself(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    [begin
         Stream = stream(client, Caller, Open, seal()),
         ok = macula_stream:deliver_frame(Stream, provider_frame(#{frame_type => stream_error, seq => 0, code => Code,
                                                                   message => Message}, Provider, Open)),
         ?assertEqual({error, {Code, Message}}, macula_stream:recv(Stream, 1000)),
         gen_server:stop(Stream)
     end || {Code, Message} <- [{<<"too_many_sessions">>, <<"no more sessions are served now">>},
                                {<<"unavailable">>, <<"sessions are not being admitted now">>}]].

a_clear_error_outside_the_closed_set_ends_the_session(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open, seal()),
    ok = macula_stream:deliver_frame(Stream, provider_frame(#{frame_type => stream_error, seq => 0,
                                                              code => <<"unauthorized">>, message => <<"no">>},
                                                            Provider, Open)),
    ?assertMatch({error, {<<"malformed_frame">>, _}}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

%% STREAM_END carries nothing to seal (§5.2).
a_stream_end_stays_clear(#{caller := Caller} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open, seal()),
    ok = macula_stream:close_send(Stream),
    {Frame, false} = sent(),
    ?assertMatch({ok, #{frame_type := stream_end, role := send}, _},
                 macula_frame:verify_caller_stream(Frame, macula_frame:open_stream(Open), pq_pure)),
    gen_server:stop(Stream).

%% A provider that no longer holds the key the open was sealed to refuses it
%% in the clear, naming the key it holds now. While the caller has sent
%% nothing, its stream reseals ONCE: it asks its reseal for the named key and
%% its link to reopen under it, keeps its pid, and reads on under the new
%% open's keys (Amendment A1, E2E design §5.2).
a_refused_open_reopens_once_under_the_resealed_key(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Test = self(),
    Reopen = #{args => #{n => 1}, reseal => fun(Named) -> Test ! {resealing, Named}, {ok, <<"new kem key">>} end},
    Stream = stream(client, Caller, Open, seal(), Reopen),
    ok = macula_stream:deliver_frame(Stream, refusal(<<2:64>>, Provider, Open)),
    ?assertEqual({resealing, <<2:64>>}, receive {resealing, _} = R -> R after 1000 -> none end),
    NewOpen = verified_open(Keys, bidi, <<8:128>>),
    #{k_p2c := KP2C} = NewSeal = seal(),
    ?assertEqual({?SID, Open, #{n => 1}, <<"new kem key">>}, reopen_asked(#{open => NewOpen, seal => NewSeal,
                                                                            sid => <<8:128>>})),
    ok = macula_stream:deliver_frame(Stream, provider_sealed(stream_data, 0, #{encoding => raw}, <<"after">>, KP2C,
                                                             Provider, NewOpen)),
    ?assertEqual({chunk, <<"after">>}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

%% Once the caller has sent, its frames are sealed under the refused keys and
%% cannot be taken back: the stream ends naming the key, and asks no reopen.
a_refused_open_after_a_send_ends_naming_the_key(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open, seal(), reopen_by(fun(_) -> {ok, <<"new">>} end)),
    ok = macula_stream:send(Stream, <<"already sent">>),
    _ = sent(),
    ok = macula_stream:deliver_frame(Stream, refusal(<<2:64>>, Provider, Open)),
    ?assertEqual({error, {sealed_refused, <<2:64>>}}, macula_stream:recv(Stream, 1000)),
    ?assertEqual(none, no_reopen()),
    gen_server:stop(Stream).

a_reseal_that_fails_ends_with_its_reason(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Mismatch = {confidentiality, {key_mismatch, <<2:64>>, <<3:64>>}},
    Stream = stream(client, Caller, Open, seal(), reopen_by(fun(_) -> {error, Mismatch} end)),
    ok = macula_stream:deliver_frame(Stream, refusal(<<2:64>>, Provider, Open)),
    ?assertEqual({error, Mismatch}, macula_stream:recv(Stream, 1000)),
    ?assertEqual(none, no_reopen()),
    gen_server:stop(Stream).

%% A second refusal is the answer: no third open.
a_second_refusal_ends_the_stream(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open, seal(), reopen_by(fun(_) -> {ok, <<"new">>} end)),
    ok = macula_stream:deliver_frame(Stream, refusal(<<2:64>>, Provider, Open)),
    NewOpen = verified_open(Keys, bidi, <<8:128>>),
    _ = reopen_asked(#{open => NewOpen, seal => seal(), sid => <<8:128>>}),
    ok = macula_stream:deliver_frame(Stream, refusal(<<4:64>>, Provider, NewOpen)),
    ?assertEqual({error, {sealed_refused, <<4:64>>}}, macula_stream:recv(Stream, 1000)),
    ?assertEqual(none, no_reopen()),
    gen_server:stop(Stream).

a_refusal_without_a_reseal_ends_naming_the_key(#{caller := Caller, provider := Provider} = Keys) ->
    Open = verified_open(Keys, bidi),
    Stream = stream(client, Caller, Open, seal()),
    ok = macula_stream:deliver_frame(Stream, refusal(<<2:64>>, Provider, Open)),
    ?assertEqual({error, {sealed_refused, <<2:64>>}}, macula_stream:recv(Stream, 1000)),
    gen_server:stop(Stream).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

reopen_by(Reseal) ->
    #{args => #{}, reseal => Reseal}.

%% The provider's clear refusal of the open, naming the key it holds now.
refusal(KeyId, Provider, Open) ->
    provider_frame(#{frame_type => stream_error, seq => 0, code => <<"sealed_refused">>,
                     message => binary:encode_hex(KeyId, lowercase)}, Provider, Open).

%% The stream's request to its link (this process) to reopen, answered with
%% Reopened: what it asked with.
reopen_asked(Reopened) ->
    receive
        {'$gen_call', From, {reopen_stream, _Stream, Sid, Open, Args, KemKey}} ->
            gen_server:reply(From, {ok, Reopened}),
            {Sid, Open, Args, KemKey}
    after 1000 ->
        error(no_reopen_asked)
    end.

no_reopen() ->
    receive {'$gen_call', _From, {reopen_stream, _, _, _, _, _}} -> asked after 200 -> none end.

keys() ->
    {ok, _} = application:ensure_all_started(macula),
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, pq_pure), Key end,
    #{caller => Generate(), provider => Generate()}.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

seal() ->
    #{k_req => crypto:strong_rand_bytes(32), k_c2p => crypto:strong_rand_bytes(32),
      k_p2c => crypto:strong_rand_bytes(32), key_id => ?KEY_ID}.

verified_open(Keys, Mode) ->
    verified_open(Keys, Mode, <<7:128>>).

verified_open(#{caller := Caller, provider := Provider}, Mode, RequestId) ->
    Spec = #{request_id => RequestId, realm => <<1:256>>, procedure => <<"acme/count_v1">>,
             target => macula_node_keys:key_id(Provider), deadline => 1789000600000, mode => Mode,
             sealed => #{scheme => 1, key_id => ?KEY_ID, kem_ct => <<2:12544>>, ct => <<"sealed open">>}},
    {ok, Open} = macula_frame:verify_request(wire(macula_frame:stream_open(Spec, Caller)), pq_pure),
    Open.

request_id(#{request_id := RequestId}) -> RequestId.

%% A served stream keeps what it has not read on its caller's budget, so it is
%% admitted first, as the link admits it.
stream(Role, Key, Open, Seal) ->
    stream(Role, Key, Open, Seal, #{}).

stream(Role, Key, #{mode := Mode, caller := Caller} = Open, Seal, Reopen) ->
    {ok, Pid} = macula_stream:start_link(reopening(Reopen, #{id => ?SID, role => Role, mode => Mode, owner => self(),
                                                              key => fun() -> Key end, open => Open, conn => self(),
                                                              profile => pq_pure, station => <<5:256>>,
                                                              seal => Seal})),
    ok = admitted(Role, Caller, Pid),
    ok = macula_stream:attach_to_link(Pid, self(), ?SID),
    Pid.

reopening(Reopen, Opts) when map_size(Reopen) =:= 0 -> Opts;
reopening(Reopen, Opts) -> Opts#{reopen => Reopen}.

admitted(server, Caller, Pid) -> macula_stream_sessions:admit(Caller, Pid);
admitted(client, _Caller, _Pid) -> ok.

provider_frame(Spec, Provider, Open) ->
    wire(macula_frame:provider_stream(Spec, Provider, Open)).

%% A provider frame of Type at Seq sealing Plain under Key in direction 1.
provider_sealed(Type, Seq, Extra, Plain, Key, Provider, Open) ->
    Nonce = macula_seal:random_nonce(),
    Aad = macula_seal:stream_aad(atom_to_binary(Type), request_id(Open), Seq, 1),
    Sealed = #{scheme => 1, key_id => ?KEY_ID, nonce => Nonce, ct => macula_seal:seal(Key, Nonce, Aad, Plain)},
    provider_frame(Extra#{frame_type => Type, seq => Seq, sealed => Sealed}, Provider, Open).

%% A caller STREAM_DATA at Seq sealing Plain under Key with the seq's nonce.
caller_frame(Seq, Encoding, Plain, Key, Caller, Open) ->
    Aad = macula_seal:stream_aad(<<"stream_data">>, request_id(Open), Seq, 0),
    Sealed = #{scheme => 1, key_id => ?KEY_ID,
               ct => macula_seal:seal(Key, macula_seal:stream_nonce(Seq), Aad, Plain)},
    wire(macula_frame:caller_stream(#{frame_type => stream_data, seq => Seq, encoding => Encoding, sealed => Sealed},
                                    Caller, Open)).

%% The next frame the stream handed to its link, decoded, and whether it was its last.
sent() ->
    receive
        {'$gen_cast', {send_stream_bytes, ?SID, Bytes, Last}} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            {Frame, Last}
    after 1000 ->
        erlang:error(no_frame_sent)
    end.

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

%% The stream_open vector and its frames in one direction.
vector() ->
    {ok, Bytes} = file:read_file(vector_file()),
    [V | _] = [C || C <- maps:get(<<"calls">>, json:decode(Bytes)), maps:get(<<"frame_type">>, C) =:= <<"stream_open">>],
    V.

vector_frames(V, Direction) ->
    [F || F <- maps:get(<<"frames">>, V), maps:get(<<"direction">>, F) =:= Direction,
          maps:get(<<"frame_type">>, F) =:= <<"stream_data">>].

vector_seal(V) ->
    #{k_req => x(V, <<"k_req">>), k_c2p => x(V, <<"k_c2p">>), k_p2c => x(V, <<"k_p2c">>), key_id => ?KEY_ID}.

%% The source tree's vector file, found as macula_seal_vectors_tests finds it.
vector_file() ->
    first_existing(["test/vectors/e2e_seal_v1.json", "../../test/vectors/e2e_seal_v1.json"]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", "e2e_seal_v1.json"])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    case filelib:is_regular(F) of
        true -> F;
        false -> first_existing(Rest)
    end.

x(Map, Key) -> binary:decode_hex(maps:get(Key, Map)).
