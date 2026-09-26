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
                           fun an_open_naming_no_seal_is_refused/1}]].

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

%% The next frame the link wrote on Quic, decoded.
written(Quic) ->
    receive
        {written, Quic, Bytes} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            Frame
    after ?EVENT_MS ->
        error(nothing_written)
    end.
