%% A CLEAR caller stream whose provider turns out to be keyed (DESIGN_E2E_SEALED_BY_DEFAULT §3, release A0). The stream
%% sends every owner action at once, as a clear stream always has, and keeps an ordered copy until the provider's first
%% frame. When that first frame is a `sealed_required' carrying `seal.kem_key', the stream reopens sealed to that key
%% under a new request id on a new session, keeping its pid, and replays the copy - its chunks and the owner's
%% STREAM_END - then whatever the owner wrote while the reopen was in flight, all sealed and in order, numbered from 0
%% on the new session. Any other first frame ends the copy for good. The provider and its dedicated streams are played
%% here by this test process.
-module(macula_station_link_clear_refusal_replay_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"test">>)).
-define(PROCEDURE, <<"acme/count_v1">>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(EVENT_MS, 3_000).

clear_refusal_replay_test_() ->
    [{Name, {timeout, 20, fun() -> W = world(), try Case(W) after stop(W) end end}}
     || {Name, Case} <- [{"a clear stream that wrote reopens sealed and replays its chunk",
                          fun a_clear_stream_that_wrote_reopens_sealed_and_replays_its_chunk/1},
                         {"the replay carries the owner's STREAM_END after its chunk",
                          fun the_replay_carries_the_owners_end_after_its_chunk/1},
                         {"what the owner wrote during the reopen goes after the copy",
                          fun what_the_owner_wrote_during_the_reopen_goes_after_the_copy/1},
                         {"a first frame that is not a refusal ends the copy",
                          fun a_first_frame_that_is_not_a_refusal_ends_the_copy/1},
                         {"a refusal without the key leaves the copy and surfaces",
                          fun a_refusal_without_the_key_surfaces_as_sealed_required/1}]].

%% The chunk sent on the clear session goes out again, sealed to the key the refusal named, as the new session's first
%% frame; the answer under the new key is the owner's, and the report names the key the stream ended up sealed to.
a_clear_stream_that_wrote_reopens_sealed_and_replays_its_chunk(#{link := Link, provider := Provider,
                                                                provider_id := Target} = W) ->
    #{stream := Stream, quic := Quic, open := Open} = clear_open(W),
    ok = macula_stream:send(Stream, <<"first">>),
    _Clear = written(Quic),
    New = new_key(W),
    refuse(Link, Quic, Provider, Open, New),
    #{quic := NewQuic, open := NewOpen, keys := #{k_c2p := KC2P, key_id := KeyId} = Keys} = reopened(New, W),
    ?assertNotEqual(maps:get(request_id, Open), maps:get(request_id, NewOpen)),
    [Chunk] = replayed(NewQuic, NewOpen, 1),
    ?assertEqual({ok, <<"first">>}, opened_chunk(Chunk, NewOpen, KC2P)),
    answer(Link, NewQuic, sealed_chunk(Provider, NewOpen, Keys, <<"back">>)),
    ?assertEqual({chunk, <<"back">>}, macula_stream:recv(Stream, ?EVENT_MS)),
    ?assertEqual({ok, #{sealed => 1, provider => Target, seal_key_id => KeyId}}, macula:stream_report(Stream)).

%% The owner ended its side on the clear session: the copy holds that too, and the replay sends the chunk and then the
%% END, in order, at the new session's own numbers.
the_replay_carries_the_owners_end_after_its_chunk(#{link := Link, provider := Provider} = W) ->
    #{stream := Stream, quic := Quic, open := Open} = clear_open(W),
    ok = macula_stream:send(Stream, <<"first">>),
    _Clear = written(Quic),
    ok = macula_stream:close_send(Stream),
    _ClearEnd = written(Quic),
    New = new_key(W),
    refuse(Link, Quic, Provider, Open, New),
    #{quic := NewQuic, open := NewOpen, keys := #{k_c2p := KC2P}} = reopened(New, W),
    [Chunk, End] = replayed(NewQuic, NewOpen, 2),
    ?assertEqual({ok, <<"first">>}, opened_chunk(Chunk, NewOpen, KC2P)),
    ?assertMatch(#{frame_type := stream_end, seq := 1, role := send}, End).

%% A write the owner makes while the reopen is in flight waits in order and goes out after the copy, not before it.
what_the_owner_wrote_during_the_reopen_goes_after_the_copy(#{link := Link, provider := Provider} = W) ->
    #{stream := Stream, quic := Quic, open := Open} = clear_open(W),
    ok = macula_stream:send(Stream, <<"first">>),
    _Clear = written(Quic),
    New = new_key(W),
    refuse(Link, Quic, Provider, Open, New),
    ok = macula_stream:send(Stream, <<"during">>),
    #{quic := NewQuic, open := NewOpen, keys := #{k_c2p := KC2P}} = reopened(New, W),
    [First, During] = replayed(NewQuic, NewOpen, 2),
    ?assertEqual({ok, <<"first">>}, opened_chunk(First, NewOpen, KC2P)),
    ?assertEqual({ok, <<"during">>}, opened_chunk(During, NewOpen, KC2P)).

%% The provider's first frame settles it: a clear chunk ends the copy, so a refusal arriving after it is the session's
%% answer, and no reopen is made.
a_first_frame_that_is_not_a_refusal_ends_the_copy(#{link := Link, provider := Provider} = W) ->
    #{stream := Stream, quic := Quic, open := Open} = clear_open(W),
    ok = macula_stream:send(Stream, <<"first">>),
    _Clear = written(Quic),
    answer(Link, Quic, macula_frame:provider_stream(#{frame_type => stream_data, seq => 0, encoding => raw,
                                                       body => <<"back">>}, Provider, Open)),
    ?assertEqual({chunk, <<"back">>}, macula_stream:recv(Stream, ?EVENT_MS)),
    New = new_key(W),
    refuse(Link, Quic, Provider, Open, New, 1),
    ?assertEqual({error, {<<"sealed_required">>, <<>>}}, macula_stream:recv(Stream, ?EVENT_MS)),
    ?assertEqual(none, receive {opened, _} -> reopened after 200 -> none end).

%% A refusal that names no key is the session's answer as the provider sent it, and nothing is reopened for it.
a_refusal_without_the_key_surfaces_as_sealed_required(#{link := Link, provider := Provider} = W) ->
    #{stream := Stream, quic := Quic, open := Open} = clear_open(W),
    ok = macula_stream:send(Stream, <<"first">>),
    _Clear = written(Quic),
    answer(Link, Quic, macula_frame:provider_stream(#{frame_type => stream_error, seq => 0,
                                                       code => <<"sealed_required">>, message => <<>>},
                                                    Provider, Open)),
    ?assertEqual({error, {<<"sealed_required">>, <<>>}}, macula_stream:recv(Stream, ?EVENT_MS)),
    ?assertEqual(none, receive {opened, _} -> reopened after 200 -> none end).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

profile() -> pq_pure.

%% A link that believes it is connected, with this process as its peering connection and its dedicated streams, and a
%% provider with a KEM key the refusals will name.
world() ->
    {ok, _} = application:ensure_all_started(macula),
    Test = self(),
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    {ok, Link} = macula_station_link:start_link(
                   #{seed => #{host => <<"127.0.0.1">>, port => 1}, expected_node_id => <<1:256>>,
                     node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
                     share => {seed, {<<"127.0.0.1">>, 1}},
                     connect => fun(_PeeringOpts) -> {error, not_dialed_here} end,
                     open_stream => fun(_Conn) -> Opened = make_ref(), Test ! {opened, Opened}, {ok, Opened} end,
                     send_on_stream => fun(Stream, Bytes) -> Test ! {written, Stream, Bytes}, ok end,
                     close_stream => fun(_Stream) -> ok end}),
    _ = sys:replace_state(Link, fun(S) -> setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Test),
                                                     <<2:256>>) end),
    {ok, Provider} = macula_node_keys:generate(identity, profile()),
    #{link => Link, provider => Provider, provider_id => macula_node_keys:key_id(Provider)}.

stop(#{link := Link}) ->
    catch macula_station_link:stop(Link),
    ok.

%% A bidi stream the link opens in the clear, and its verified open.
clear_open(#{link := Link, provider_id := Target}) ->
    {ok, Stream} = macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, #{n => 1},
                                                   #{mode => bidi, seal => clear}),
    Quic = receive {opened, Opened} -> Opened after ?EVENT_MS -> error(no_stream_opened) end,
    {ok, Open} = macula_frame:verify_request(written(Quic), profile()),
    #{stream => Stream, quic => Quic, open => Open}.

%% A fresh KEM key the refusal names, and the holder that opens what is sealed to it.
new_key(_W) ->
    {Public, Private} = macula_seal:generate_key(profile()),
    Carried = macula_seal:key_as_carried(Public),
    #{kem_key => Carried, kem_key_id => macula_seal:key_id(Carried),
      holder => #{current_key_id => macula_seal:key_id(Carried),
                  lookup => fun(_KeyId) -> {ok, Private, Carried} end}}.

%% The provider refuses the CLEAR open as a keyed one does, with the key it holds.
refuse(Link, Quic, Provider, Open, #{kem_key := KemKey}) ->
    refuse(Link, Quic, Provider, Open, #{kem_key => KemKey}, 0).

refuse(Link, Quic, Provider, Open, #{kem_key := KemKey}, Seq) ->
    answer(Link, Quic, macula_frame:provider_stream(#{frame_type => stream_error, seq => Seq,
                                                       code => <<"sealed_required">>, message => <<>>,
                                                       ext => #{<<"seal.kem_key">> =>
                                                                    #{critical => 0, value => KemKey}}},
                                                    Provider, Open)).

%% The reopen the link made: its new dedicated stream and the STREAM_OPEN it wrote there, with the keys it agreed.
reopened(#{holder := Holder}, _W) ->
    NewQuic = receive {opened, Opened} -> Opened after ?EVENT_MS -> error(no_reopen) end,
    {ok, NewOpen} = macula_frame:verify_request(written(NewQuic), profile()),
    {ok, Plain, Keys} = macula_sealed_call:open_request(profile(), Holder, seal_request(NewOpen),
                                                        maps:get(sealed, NewOpen)),
    ?assertEqual({ok, #{{text, <<"n">>} => 1}}, macula_frame:plain_payload(Plain)),
    #{quic => NewQuic, open => NewOpen, keys => Keys}.

%% The first `Count' frames the reopened session wrote, each verified as the provider verifies it, in turn.
replayed(NewQuic, NewOpen, Count) ->
    replayed(Count, macula_frame:open_stream(NewOpen), NewQuic, []).

replayed(0, _Verifier, _NewQuic, Verified) ->
    lists:reverse(Verified);
replayed(Count, Verifier, NewQuic, Verified) ->
    {ok, Fields, Next} = macula_frame:verify_caller_stream(written(NewQuic), Verifier, profile()),
    replayed(Count - 1, Next, NewQuic, [Fields | Verified]).

%% The plaintext of a verified caller chunk, opened under the session's `k_c2p'.
opened_chunk(#{frame_type := stream_data, seq := Seq, sealed := #{ct := Ct}}, NewOpen, KC2P) ->
    Aad = macula_seal:stream_aad(<<"stream_data">>, maps:get(request_id, NewOpen), Seq, 0),
    macula_seal:open(KC2P, macula_seal:stream_nonce(Seq), Aad, Ct).

%% The provider answers under the new session's keys, which is what settles the caller's report.
sealed_chunk(Provider, NewOpen, #{k_p2c := KP2C, key_id := KeyId}, Body) ->
    Nonce = macula_seal:random_nonce(),
    Aad = macula_seal:stream_aad(<<"stream_data">>, maps:get(request_id, NewOpen), 0, 1),
    macula_frame:provider_stream(#{frame_type => stream_data, seq => 0, encoding => raw,
                                   sealed => #{scheme => 1, key_id => KeyId, nonce => Nonce,
                                               ct => macula_seal:seal(KP2C, Nonce, Aad, Body)}},
                                 Provider, NewOpen).

answer(Link, Quic, Frame) when is_pid(Link) ->
    Link ! {quic, macula_frame:encode(Frame), Quic, undefined}.

%% The next frame the link wrote on Quic, decoded.
written(Quic) ->
    receive
        {written, Quic, Bytes} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            Frame
    after ?EVENT_MS -> error(nothing_written)
    end.

seal_request(#{frame_type := Type, realm := Realm, procedure := Procedure, caller := Caller, target := Target,
               request_id := RequestId, deadline := Deadline}) ->
    #{frame_type => atom_to_binary(Type), realm => Realm, procedure => Procedure, caller => Caller,
      target => Target, request_id => RequestId, deadline => Deadline}.
