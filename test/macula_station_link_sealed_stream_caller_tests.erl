%% A caller's link seals a STREAM_OPEN to the provider's KEM key (E2E design
%% §5.2), exactly as it seals a CALL: the open carries `sealed' and no
%% payload a station reads, and its encapsulation agrees the stream keys the
%% stream then seals and opens under. The provider is played by this test
%% process: it opens what the link wrote, as a provider does.
-module(macula_station_link_sealed_stream_caller_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"test">>)).
-define(PROCEDURE, <<"acme/count_v1">>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(EVENT_MS, 3_000).

%% Each case builds its link in its own process, the one the link's
%% dedicated streams report to.
sealed_stream_caller_test_() ->
    [{Name, fun() -> W = world(), try Case(W) after stop(W) end end}
      || {Name, Case} <- [{"a sealed open carries no payload and opens to its args at the provider",
                           fun a_sealed_open_opens_at_the_provider/1},
                          {"the stream seals its chunks under the keys its open agreed",
                           fun the_stream_seals_under_the_keys_its_open_agreed/1},
                          {"a provider's sealed chunk under those keys reaches the reader",
                           fun a_providers_sealed_chunk_reaches_the_reader/1},
                          {"a key of another profile seals nothing and opens no stream",
                           fun a_key_of_another_profile_opens_no_stream/1},
                          {"a clear open stays clear", fun a_clear_open_stays_clear/1},
                          {"an open that names no seal is refused where it is made",
                           fun an_open_naming_no_seal_is_refused/1},
                          {"a refused open reopens under the resealed key on a new stream, same stream pid",
                           fun a_refused_open_reopens_under_the_resealed_key/1},
                          {"a sealed stream's report settles on the provider's first opened chunk",
                           fun a_sealed_streams_report_settles_on_an_opened_chunk/1},
                          {"a sealed stream the provider ends before any data has no report",
                           fun a_sealed_stream_ended_before_data_has_no_report/1},
                          {"a clear stream's report settles on the provider's first chunk, sealed 0",
                           fun a_clear_streams_report_settles_sealed_0/1},
                          {"a clear stream refused at seq 0 has no report",
                           fun a_clear_stream_refused_at_seq_0_has_no_report/1},
                          {"a served stream has no report: it is not a caller's",
                           fun a_served_stream_has_no_report/1},
                          {"a provider that answers under the key it refused ends the stream; the report keeps that key",
                           fun a_provider_answering_under_the_refused_key_ends_the_stream/1}]].

a_sealed_open_opens_at_the_provider(W) ->
    #{open := Open, plain := Plain} = sealed_open(W, #{city => {text, <<"Tienen">>}}),
    ?assertNot(is_map_key(payload, Open)),
    ?assertMatch(#{kem_ct := _}, maps:get(sealed, Open)),
    ?assertEqual({ok, #{{text, <<"city">>} => {text, <<"Tienen">>}}}, macula_frame:plain_payload(Plain)).

the_stream_seals_under_the_keys_its_open_agreed(#{provider := _} = W) ->
    #{stream := Stream, quic := Quic, open := Open, keys := #{k_c2p := KC2P}} = sealed_open(W, #{}),
    ok = macula_stream:send(Stream, <<"to the provider">>),
    Frame = written(Quic),
    {ok, #{sealed := #{ct := Ct}, seq := 0}, _} =
        macula_frame:verify_caller_stream(Frame, macula_frame:open_stream(Open), profile()),
    Aad = macula_seal:stream_aad(<<"stream_data">>, maps:get(request_id, Open), 0, 0),
    ?assertEqual({ok, <<"to the provider">>}, macula_seal:open(KC2P, macula_seal:stream_nonce(0), Aad, Ct)).

a_providers_sealed_chunk_reaches_the_reader(#{link := Link, provider := Provider} = W) ->
    #{stream := Stream, quic := Quic, open := Open, keys := #{k_p2c := KP2C, key_id := KeyId}} = sealed_open(W, #{}),
    Nonce = macula_seal:random_nonce(),
    Aad = macula_seal:stream_aad(<<"stream_data">>, maps:get(request_id, Open), 0, 1),
    Sealed = #{scheme => 1, key_id => KeyId, nonce => Nonce, ct => macula_seal:seal(KP2C, Nonce, Aad, <<"back">>)},
    Frame = macula_frame:provider_stream(#{frame_type => stream_data, seq => 0, encoding => raw, sealed => Sealed},
                                         Provider, Open),
    Link ! {quic, macula_frame:encode(Frame), Quic, undefined},
    ?assertEqual({chunk, <<"back">>}, macula_stream:recv(Stream, ?EVENT_MS)).

a_key_of_another_profile_opens_no_stream(#{link := Link, provider_id := Target}) ->
    {Public, _Private} = macula_seal:generate_key(pq_hybrid),
    ?assertEqual({error, {confidentiality, no_kem_key}},
                 macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, #{},
                                                 #{mode => bidi, seal => {sealed_to, macula_seal:key_as_carried(Public)}})),
    ?assertEqual(none, receive {opened, _} -> opened after 200 -> none end).

a_clear_open_stays_clear(#{link := Link, provider_id := Target}) ->
    {ok, _Stream} = macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, #{n => 1},
                                                    #{mode => bidi, seal => clear}),
    Quic = receive {opened, Opened} -> Opened after ?EVENT_MS -> error(no_stream_opened) end,
    {ok, Open} = macula_frame:verify_request(written(Quic), profile()),
    ?assertNot(is_map_key(sealed, Open)),
    ?assertEqual(#{{text, <<"n">>} => 1}, maps:get(payload, Open)).

%% How a stream goes is the caller's explicit decision, never a default: an
%% open that names no seal raises in the calling process and opens nothing.
an_open_naming_no_seal_is_refused(#{link := Link, provider_id := Target}) ->
    ?assertError(function_clause, macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, #{},
                                                                  #{mode => bidi})),
    ?assertEqual(none, receive {opened, _} -> opened after 200 -> none end).

%% The provider refuses the open naming the key it holds now; the stream's
%% reseal resolves that key, and the link reopens on a new dedicated stream,
%% under a new request, sealed to it. The same stream reads on.
a_refused_open_reopens_under_the_resealed_key(#{link := Link, provider := Provider, provider_id := Target,
                                                 kem_key := OldKemKey}) ->
    Test = self(),
    {Public, Private} = macula_seal:generate_key(profile()),
    NewKemKey = macula_seal:key_as_carried(Public),
    NewId = macula_seal:key_id(NewKemKey),
    NewHolder = #{current_key_id => NewId, lookup => fun(_KeyId) -> {ok, Private, NewKemKey} end},
    {ok, Stream} = macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, #{n => 1},
                                                   #{mode => bidi, seal => {sealed_to, OldKemKey},
                                                     reseal => fun(Named) -> Test ! {reseal, Named}, {ok, NewKemKey}
                                                               end}),
    OldQuic = receive {opened, Q1} -> Q1 after ?EVENT_MS -> error(no_stream_opened) end,
    {ok, OldOpen} = macula_frame:verify_request(written(OldQuic), profile()),
    Refusal = macula_frame:provider_stream(#{frame_type => stream_error, seq => 0, code => <<"sealed_refused">>,
                                             message => binary:encode_hex(NewId, lowercase)}, Provider, OldOpen),
    Link ! {quic, macula_frame:encode(Refusal), OldQuic, undefined},
    ?assertEqual({reseal, NewId}, receive {reseal, _} = R -> R after ?EVENT_MS -> none end),
    NewQuic = receive {opened, Q2} -> Q2 after ?EVENT_MS -> error(no_reopen) end,
    {ok, NewOpen} = macula_frame:verify_request(written(NewQuic), profile()),
    ?assertNotEqual(maps:get(request_id, OldOpen), maps:get(request_id, NewOpen)),
    {ok, Plain, #{k_p2c := KP2C, key_id := KeyId}} =
        macula_sealed_call:open_request(profile(), NewHolder, seal_request(NewOpen), maps:get(sealed, NewOpen)),
    ?assertEqual({ok, #{{text, <<"n">>} => 1}}, macula_frame:plain_payload(Plain)),
    Nonce = macula_seal:random_nonce(),
    Aad = macula_seal:stream_aad(<<"stream_data">>, maps:get(request_id, NewOpen), 0, 1),
    Chunk = macula_frame:provider_stream(#{frame_type => stream_data, seq => 0, encoding => raw,
                                           sealed => #{scheme => 1, key_id => KeyId, nonce => Nonce,
                                                       ct => macula_seal:seal(KP2C, Nonce, Aad, <<"reopened">>)}},
                                         Provider, NewOpen),
    Link ! {quic, macula_frame:encode(Chunk), NewQuic, undefined},
    ?assertEqual({chunk, <<"reopened">>}, macula_stream:recv(Stream, ?EVENT_MS)),
    %% The report describes the exchange that produced the chunk: the reseal's key, never the first (§3).
    ?assertEqual({ok, #{sealed => 1, provider => Target, seal_key_id => NewId}}, macula:stream_report(Stream)).

%% DESIGN_E2E_SEAL_REPORT §3: a sealed stream's report settles on the
%% provider's first data or reply opened under the stream's key, and names that
%% key; before it, it is `not_settled', never guessed from the open.
a_sealed_streams_report_settles_on_an_opened_chunk(#{link := Link, provider := Provider, provider_id := Target} = W) ->
    #{stream := Stream, quic := Quic, open := Open, keys := #{k_p2c := KP2C, key_id := KeyId}} = sealed_open(W, #{}),
    ?assertEqual({error, not_settled}, macula:stream_report(Stream)),
    Nonce = macula_seal:random_nonce(),
    Aad = macula_seal:stream_aad(<<"stream_data">>, maps:get(request_id, Open), 0, 1),
    Sealed = #{scheme => 1, key_id => KeyId, nonce => Nonce, ct => macula_seal:seal(KP2C, Nonce, Aad, <<"back">>)},
    Frame = macula_frame:provider_stream(#{frame_type => stream_data, seq => 0, encoding => raw, sealed => Sealed},
                                         Provider, Open),
    Link ! {quic, macula_frame:encode(Frame), Quic, undefined},
    ?assertEqual({chunk, <<"back">>}, macula_stream:recv(Stream, ?EVENT_MS)),
    ?assertEqual({ok, #{sealed => 1, provider => Target, seal_key_id => KeyId}}, macula:stream_report(Stream)).

%% A STREAM_END travels clear on a sealed stream: nothing about it is opened,
%% so it settles nothing (§3).
a_sealed_stream_ended_before_data_has_no_report(#{link := Link, provider := Provider} = W) ->
    #{stream := Stream, quic := Quic, open := Open} = sealed_open(W, #{}),
    End = macula_frame:provider_stream(#{frame_type => stream_end, seq => 0, role => both}, Provider, Open),
    Link ! {quic, macula_frame:encode(End), Quic, undefined},
    ?assertEqual(eof, macula_stream:recv(Stream, ?EVENT_MS)),
    ?assertEqual({error, not_settled}, macula:stream_report(Stream)).

a_clear_streams_report_settles_sealed_0(#{link := Link, provider := Provider, provider_id := Target}) ->
    #{stream := Stream, quic := Quic, open := Open} = clear_open(Link, Target),
    ?assertEqual({error, not_settled}, macula:stream_report(Stream)),
    Chunk = macula_frame:provider_stream(#{frame_type => stream_data, seq => 0, encoding => raw, body => <<"back">>},
                                         Provider, Open),
    Link ! {quic, macula_frame:encode(Chunk), Quic, undefined},
    ?assertEqual({chunk, <<"back">>}, macula_stream:recv(Stream, ?EVENT_MS)),
    ?assertEqual({ok, #{sealed => 0, provider => Target}}, macula:stream_report(Stream)).

%% An error never settles a stream, clear or sealed, as a call's error carries
%% no report.
a_clear_stream_refused_at_seq_0_has_no_report(#{link := Link, provider := Provider, provider_id := Target}) ->
    #{stream := Stream, quic := Quic, open := Open} = clear_open(Link, Target),
    Refusal = macula_frame:provider_stream(#{frame_type => stream_error, seq => 0, code => <<"caller_quota">>,
                                             message => <<>>}, Provider, Open),
    Link ! {quic, macula_frame:encode(Refusal), Quic, undefined},
    ?assertMatch({error, _}, macula_stream:recv(Stream, ?EVENT_MS)),
    ?assertEqual({error, not_settled}, macula:stream_report(Stream)).

%% The report is the caller's evidence; the provider side of a stream has
%% none.
a_served_stream_has_no_report(_W) ->
    {ok, Served} = macula_stream:start_link(#{id => crypto:strong_rand_bytes(16), role => server, mode => bidi,
                                              owner => self()}),
    ?assertEqual({error, not_a_caller}, macula:stream_report(Served)).

%% A provider refuses the open's key (sealed_refused naming another) and then,
%% before the reopen lands, answers under the very key it refused. The chunk
%% opened under the first key and settled the report on it; a reopen landing
%% after that must not swap the report to the second key. The provider is
%% incoherent, so the session ends, and the report still names the key the chunk
%% opened under (Fable round 1, 13.1.0).
a_provider_answering_under_the_refused_key_ends_the_stream(#{link := Link, provider := Provider,
                                                             provider_id := Target, kem_key := OldKemKey,
                                                             holder := Holder}) ->
    Test = self(),
    {Public, _Private} = macula_seal:generate_key(profile()),
    NewKemKey = macula_seal:key_as_carried(Public),
    NewId = macula_seal:key_id(NewKemKey),
    {ok, Stream} = macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, #{},
                                                   #{mode => bidi, seal => {sealed_to, OldKemKey},
                                                     reseal => fun(_Named) ->
                                                                   Test ! {resealing, self()},
                                                                   receive go -> {ok, NewKemKey} end
                                                               end}),
    OldQuic = receive {opened, Q1} -> Q1 after ?EVENT_MS -> error(no_stream_opened) end,
    {ok, OldOpen} = macula_frame:verify_request(written(OldQuic), profile()),
    {ok, _Plain, #{k_p2c := KP2C, key_id := OldId}} =
        macula_sealed_call:open_request(profile(), Holder, seal_request(OldOpen), maps:get(sealed, OldOpen)),
    Refusal = macula_frame:provider_stream(#{frame_type => stream_error, seq => 0, code => <<"sealed_refused">>,
                                             message => binary:encode_hex(NewId, lowercase)}, Provider, OldOpen),
    Link ! {quic, macula_frame:encode(Refusal), OldQuic, undefined},
    Resealer = receive {resealing, R} -> R after ?EVENT_MS -> error(no_reseal) end,
    Nonce = macula_seal:random_nonce(),
    Aad = macula_seal:stream_aad(<<"stream_data">>, maps:get(request_id, OldOpen), 1, 1),
    Chunk = macula_frame:provider_stream(#{frame_type => stream_data, seq => 1, encoding => raw,
                                           sealed => #{scheme => 1, key_id => OldId, nonce => Nonce,
                                                       ct => macula_seal:seal(KP2C, Nonce, Aad, <<"under the old">>)}},
                                         Provider, OldOpen),
    Link ! {quic, macula_frame:encode(Chunk), OldQuic, undefined},
    ?assertEqual({chunk, <<"under the old">>}, macula_stream:recv(Stream, ?EVENT_MS)),
    Resealer ! go,
    ?assertMatch({error, {<<"malformed_frame">>, _}}, macula_stream:recv(Stream, ?EVENT_MS)),
    ?assertEqual({ok, #{sealed => 1, provider => Target, seal_key_id => OldId}}, macula:stream_report(Stream)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

profile() -> pq_pure.

%% A link that believes it is connected, with this process as its peering
%% connection and its dedicated streams, and a provider with a KEM key.
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
    {Public, Private} = macula_seal:generate_key(profile()),
    Carried = macula_seal:key_as_carried(Public),
    #{link => Link, provider => Provider, provider_id => macula_node_keys:key_id(Provider), kem_key => Carried,
      holder => #{current_key_id => macula_seal:key_id(Carried), lookup => fun(_KeyId) -> {ok, Private, Carried} end}}.

stop(#{link := Link}) ->
    catch macula_station_link:stop(Link),
    ok.

%% A bidi stream the link opens sealed to the provider's KEM key, and what the
%% provider reads of its open: the verified open, its opened plaintext and the
%% keys it agreed.
sealed_open(#{link := Link, provider_id := Target, kem_key := KemKey, holder := Holder}, Args) ->
    {ok, Stream} = macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, Args,
                                                   #{mode => bidi, seal => {sealed_to, KemKey}}),
    Quic = receive {opened, Opened} -> Opened after ?EVENT_MS -> error(no_stream_opened) end,
    {ok, Open} = macula_frame:verify_request(written(Quic), profile()),
    {ok, Plain, Keys} = macula_sealed_call:open_request(profile(), Holder, seal_request(Open), maps:get(sealed, Open)),
    #{stream => Stream, quic => Quic, open => Open, plain => Plain, keys => Keys}.

seal_request(#{frame_type := Type, realm := Realm, procedure := Procedure, caller := Caller, target := Target,
               request_id := RequestId, deadline := Deadline}) ->
    #{frame_type => atom_to_binary(Type), realm => Realm, procedure => Procedure, caller => Caller,
      target => Target, request_id => RequestId, deadline => Deadline}.

%% A bidi stream the link opens in the clear, and its verified open.
clear_open(Link, Target) ->
    {ok, Stream} = macula_station_link:call_stream(Link, Target, ?REALM, ?PROCEDURE, #{}, #{mode => bidi, seal => clear}),
    Quic = receive {opened, Opened} -> Opened after ?EVENT_MS -> error(no_stream_opened) end,
    {ok, Open} = macula_frame:verify_request(written(Quic), profile()),
    #{stream => Stream, quic => Quic, open => Open}.

%% The next frame the link wrote on Quic, decoded.
written(Quic) ->
    receive
        {written, Quic, Bytes} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            Frame
    after ?EVENT_MS ->
        error(nothing_written)
    end.
