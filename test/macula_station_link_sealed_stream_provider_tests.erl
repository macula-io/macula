%% A provider's link opens a sealed STREAM_OPEN with its KEM keyring (E2E
%% design §5.2), as it opens a sealed CALL, and serves it with the keys the
%% open agreed: the handler gets the opened args, and its stream seals and
%% opens under k_p2c and k_c2p.
%%
%% A refusal decided before anything is opened (admission, session caps) goes
%% in the clear, from the closed set; `sealed_refused' names the key this node
%% holds now. Anything decided after the open (the procedure's policy, an
%% unknown procedure, a mode mismatch) goes sealed, since it is about the
%% procedure. A clear open to a procedure that takes only sealed ones is
%% refused `sealed_required'. The caller is played by this test process.
-module(macula_station_link_sealed_stream_provider_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"test">>)).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(ADVERTISEMENTS_INDEX, macula_station_link:state_field_index(advertisements)).
-define(EVENT_MS, 3_000).

sealed_stream_provider_test_() ->
    [{Name, {timeout, 30, fun() -> W = world(), try Case(W) after stop(W) end end}}
     || {Name, Case} <- [{"a sealed open is served with its opened args, sealed both ways",
                          fun a_sealed_open_is_served_sealed_both_ways/1},
                         {"an open sealed to a key this node does not hold is refused naming its key",
                          fun an_open_to_another_key_is_refused_naming_the_current_one/1},
                         {"a policy refusal of a sealed open is sealed", fun a_policy_refusal_is_sealed/1},
                         {"an unknown procedure is answered sealed", fun an_unknown_procedure_is_answered_sealed/1},
                         {"a mode mismatch is answered sealed", fun a_mode_mismatch_is_answered_sealed/1},
                         {"a session cap refusal stays clear", fun a_session_cap_refusal_stays_clear/1},
                         {"a caller at its session cap is refused before its open is opened",
                          fun a_caller_at_its_cap_is_refused_before_decapsulation/1},
                         {"a clear open to a procedure that takes only sealed ones is refused",
                          fun a_clear_open_to_a_required_procedure_is_refused/1},
                         {"a clear open's refusal names the key when the spec asks it to",
                          fun a_clear_open_refusal_names_the_key_when_the_spec_asks/1},
                         {"a rotated KEM key does not break a stream already open",
                          fun a_rotated_key_does_not_break_an_open_stream/1},
                         {"a sealed_required refusal drains the caller's writes before it closes",
                          fun a_sealed_required_refusal_drains_before_it_closes/1},
                         {"a drain past the link's cap closes at once, and one ending frees its place",
                          fun a_drain_past_the_links_cap_closes_at_once/1},
                         {"a caller's flood of clear opens is refused at its own cap",
                          fun a_callers_flood_is_refused_at_its_own_cap/1},
                         {"a refusal for another reason names no key",
                          fun a_refusal_for_another_reason_names_no_key/1}]].

a_sealed_open_is_served_sealed_both_ways(#{link := Link} = W) ->
    Test = self(),
    ok = advertise(W, bidi, fun(Stream, Args) ->
                                Test ! {served, Args, self()},
                                {chunk, In} = macula_stream:recv(Stream, ?EVENT_MS),
                                ok = macula_stream:send(Stream, <<"got ", In/binary>>),
                                receive stop -> ok end
                            end),
    #{quic := Quic, open := Open, keys := Keys, caller := Caller} = sealed_open(W, bidi, #{city => {text, <<"Tienen">>}}),
    {Args, Handler} = receive {served, A, H} -> {A, H} after ?EVENT_MS -> error(not_served) end,
    ?assertEqual({text, <<"Tienen">>}, macula:field(city, Args)),
    ?assertEqual(macula_node_keys:key_id(Caller), maps:get(caller, Args)),
    on_stream(Link, Quic, caller_sealed(0, <<"ping">>, Keys, Caller, Open)),
    ?assertEqual({ok, <<"got ping">>}, provider_opened(written(Quic), 0, Keys, Open)),
    ended(Handler).

an_open_to_another_key_is_refused_naming_the_current_one(W) ->
    ok = advertise(W, bidi, fun(_Stream, _Args) -> error(must_not_run) end),
    {Other, _Private} = macula_seal:generate_key(profile()),
    #{quic := Quic, open := Open} = sealed_open(W, bidi, #{}, macula_seal:key_as_carried(Other)),
    {ok, #{key_id := Current}} = macula_kem_keyring:current(node_id(W)),
    ?assertEqual({ok, #{code => <<"sealed_refused">>, message => binary:encode_hex(Current, lowercase)}},
                 clear_error(written(Quic), Open)).

a_policy_refusal_is_sealed(#{link := Link} = W) ->
    ok = macula_station_link:advertise_stream(Link, ?REALM, procedure(), bidi, fun(_S, _A) -> error(must_not_run) end,
                                              {ucan_required, <<9:256>>}),
    #{quic := Quic, open := Open, keys := Keys} = sealed_open(W, bidi, #{}),
    ?assertMatch({ok, #{code := <<"unauthorized">>}}, sealed_error(written(Quic), Keys, Open)).

an_unknown_procedure_is_answered_sealed(W) ->
    #{quic := Quic, open := Open, keys := Keys} = sealed_open(W, bidi, #{}),
    ?assertMatch({ok, #{code := <<"not_found">>}}, sealed_error(written(Quic), Keys, Open)).

a_mode_mismatch_is_answered_sealed(W) ->
    ok = advertise(W, server_stream, fun(_S, _A) -> error(must_not_run) end),
    #{quic := Quic, open := Open, keys := Keys} = sealed_open(W, bidi, #{}),
    ?assertMatch({ok, #{code := <<"mode_mismatch">>}}, sealed_error(written(Quic), Keys, Open)).

a_session_cap_refusal_stays_clear(W) ->
    ok = advertise(W, bidi, fun(_S, _A) -> error(must_not_run) end),
    with_env(max_served_sessions_per_caller, 0,
             fun() ->
                 #{quic := Quic, open := Open} = sealed_open(W, bidi, #{}),
                 ?assertMatch({ok, #{code := <<"too_many_sessions">>}}, clear_error(written(Quic), Open))
             end).

%% A caller that holds all its sessions costs the node no decapsulation: the
%% cap is read first, so even an open this node could not open is refused
%% `too_many_sessions', not `sealed_refused'.
a_caller_at_its_cap_is_refused_before_decapsulation(W) ->
    ok = advertise(W, bidi, fun(_S, _A) -> error(must_not_run) end),
    {Other, _Private} = macula_seal:generate_key(profile()),
    with_env(max_served_sessions_per_caller, 0,
             fun() ->
                 #{quic := Quic, open := Open} = sealed_open(W, bidi, #{}, macula_seal:key_as_carried(Other)),
                 ?assertMatch({ok, #{code := <<"too_many_sessions">>}}, clear_error(written(Quic), Open))
             end).

a_clear_open_to_a_required_procedure_is_refused(#{link := Link} = W) ->
    ok = advertise(W, bidi, fun(_S, _A) -> error(must_not_run) end),
    _ = sys:replace_state(Link, fun(S) ->
                                    Ads = element(?ADVERTISEMENTS_INDEX, S),
                                    setelement(?ADVERTISEMENTS_INDEX, S,
                                               Ads#{{?REALM, procedure()} => #{kem => true, confidential => required}})
                                end),
    #{quic := Quic, open := Open} = clear_open(W, bidi, #{}),
    ?assertMatch({ok, #{code := <<"sealed_required">>}}, clear_error(written(Quic), Open)).

%% A spec that asks for it names the key this node holds now in the refusal, as
%% the CALL path's does, so a caller that does not know the key seals again to
%% it (release A0, DESIGN_E2E_SEALED_BY_DEFAULT §3).
a_clear_open_refusal_names_the_key_when_the_spec_asks(#{link := Link} = W) ->
    ok = advertise(W, bidi, fun(_S, _A) -> error(must_not_run) end),
    _ = sys:replace_state(Link, fun(S) ->
                                    Ads = element(?ADVERTISEMENTS_INDEX, S),
                                    setelement(?ADVERTISEMENTS_INDEX, S,
                                               Ads#{{?REALM, procedure()} => #{kem => true,
                                                                               confidential => required,
                                                                               refusal_names_key => true}})
                                end),
    #{quic := Quic, open := Open} = clear_open(W, bidi, #{}),
    Fields = clear_refusal(written(Quic), Open),
    ?assertMatch(#{code := <<"sealed_required">>}, Fields),
    {ok, #{key := Carried}} = macula_kem_keyring:current(node_id(W)),
    ?assertEqual({ok, Carried}, macula_frame:refused_kem_key(Fields)).

%% After decapsulation the stream keys do not depend on the keyring: a key
%% rotated mid-stream leaves the stream sealing and opening as before.
a_rotated_key_does_not_break_an_open_stream(#{link := Link} = W) ->
    Test = self(),
    ok = advertise(W, bidi, fun(Stream, _Args) ->
                                Test ! {served, self()},
                                {chunk, In} = macula_stream:recv(Stream, ?EVENT_MS),
                                ok = macula_stream:send(Stream, <<"still ", In/binary>>),
                                receive stop -> ok end
                            end),
    #{quic := Quic, open := Open, keys := Keys, caller := Caller} = sealed_open(W, bidi, #{}),
    Handler = receive {served, H} -> H after ?EVENT_MS -> error(not_served) end,
    {ok, #{key_id := Before}} = macula_kem_keyring:current(node_id(W)),
    ok = rotated(macula_kem_keyring:rotate(node_id(W))),
    {ok, #{key_id := After}} = macula_kem_keyring:current(node_id(W)),
    ?assertNotEqual(Before, After),
    on_stream(Link, Quic, caller_sealed(0, <<"here">>, Keys, Caller, Open)),
    ?assertEqual({ok, <<"still here">>}, provider_opened(written(Quic), 0, Keys, Open)),
    ended(Handler).

%% A clear open to a sealed-only procedure: the caller wrote on the stream before the refusal could reach it, so the
%% link drains its remaining writes instead of closing at once — closing stops the receive side, and QUIC turns an
%% unread close into a reset the station passes on, losing the refusal (§3, release A0).
a_sealed_required_refusal_drains_before_it_closes(#{link := Link} = W) ->
    ok = sealed_opens_only(W),
    #{quic := Quic, open := Open} = clear_open(W, bidi, #{}),
    ?assertMatch(#{code := <<"sealed_required">>}, clear_refusal(written(Quic), Open)),
    ?assertEqual(none, closed_within(Quic, 100)),
    %% The caller's writes are discarded, never served, and its own end closes the stream.
    Link ! {quic, <<"writes in flight">>, Quic, undefined},
    ?assertEqual(none, closed_within(Quic, 100)),
    Link ! {quic, peer_send_shutdown, Quic, undefined},
    ?assertEqual(closed, closed_within(Quic, ?EVENT_MS)).

%% A link drains at most `?DRAIN_MAX_STREAMS' refused opens at once: past that the refusal closes at once, still
%% writing its STREAM_ERROR first, and a drain that ends frees its place for the next one.
a_drain_past_the_links_cap_closes_at_once(#{link := Link} = W) ->
    ok = sealed_opens_only(W),
    Full = [begin
                #{quic := Q, open := O} = clear_open(W, bidi, #{}),
                ?assertMatch(#{code := <<"sealed_required">>}, clear_refusal(written(Q), O)),
                Q
            end || _ <- lists:seq(1, 16)],
    ?assertEqual(none, closed_within(hd(Full), 100)),
    #{quic := Seventeenth, open := SeventeenthOpen} = clear_open(W, bidi, #{}),
    ?assertMatch(#{code := <<"sealed_required">>}, clear_refusal(written(Seventeenth), SeventeenthOpen)),
    ?assertEqual(closed, closed_within(Seventeenth, ?EVENT_MS)),
    %% One drain ends, and its place is free for the next refusal to drain again.
    Link ! {quic, peer_send_shutdown, hd(Full), undefined},
    ?assertEqual(closed, closed_within(hd(Full), ?EVENT_MS)),
    #{quic := Next, open := NextOpen} = clear_open(W, bidi, #{}),
    ?assertMatch(#{code := <<"sealed_required">>}, clear_refusal(written(Next), NextOpen)),
    ?assertEqual(none, closed_within(Next, 100)).

%% A refused open that drains keeps its place in its caller's admission, so one caller cannot hold the link's
%% streams open: past its own quota a further open is refused `caller_quota' and closed at once, while another
%% caller is served. Its place goes when the drain ends, and a later open from that caller is admitted again.
a_callers_flood_is_refused_at_its_own_cap(_W) ->
    Test = self(),
    Quota = world(1),
    try
        #{link := Link} = Quota,
        ok = sealed_opens_only(Quota),
        ok = advertise(Quota, bidi, fun(_S, _A) -> Test ! served, {reply, #{ok => 1}} end),
        {ok, Caller} = macula_node_keys:generate(identity, profile()),
        #{quic := First, open := FirstOpen} = clear_open(Quota, bidi, #{}, Caller),
        ?assertMatch(#{code := <<"sealed_required">>}, clear_refusal(written(First), FirstOpen)),
        #{quic := Second, open := SecondOpen} = clear_open(Quota, bidi, #{}, Caller),
        ?assertMatch(#{code := <<"caller_quota">>}, clear_refusal(written(Second), SecondOpen)),
        ?assertEqual(closed, closed_within(Second, ?EVENT_MS)),
        %% Another caller's sealed open on the same link is still served.
        _ = sealed_open(Quota, bidi, #{}),
        ?assertEqual(served, receive served -> served after ?EVENT_MS -> none end),
        %% The drain ends, its place goes, and the flood's caller is admitted again.
        Link ! {quic, peer_send_shutdown, First, undefined},
        ?assertEqual(closed, closed_within(First, ?EVENT_MS)),
        #{quic := Third, open := ThirdOpen} = clear_open(Quota, bidi, #{}, Caller),
        ?assertMatch(#{code := <<"sealed_required">>}, clear_refusal(written(Third), ThirdOpen)),
        ?assertEqual(none, closed_within(Third, 100))
    after
        stop(Quota)
    end.

%% The key is named only by a `sealed_required' refusal: another code's refusal carries no extension.
a_refusal_for_another_reason_names_no_key(#{link := Link} = W) ->
    ok = sealed_opens_only(W),
    _ = sys:replace_state(Link, fun(S) ->
                                    Ads = element(?ADVERTISEMENTS_INDEX, S),
                                    setelement(?ADVERTISEMENTS_INDEX, S,
                                               Ads#{{?REALM, procedure()} => #{kem => true, confidential => required,
                                                                               refusal_names_key => true}})
                                end),
    with_env(max_served_sessions_per_caller, 0,
             fun() ->
                 #{quic := Quic, open := Open} = sealed_open(W, bidi, #{}),
                 Fields = clear_refusal(written(Quic), Open),
                 ?assertMatch(#{code := <<"too_many_sessions">>}, Fields),
                 ?assertEqual(none, macula_frame:refused_kem_key(Fields))
             end).

%% A procedure that takes sealed opens only, and a way to say whether a stream closed and when.
sealed_opens_only(#{link := Link}) ->
    _ = sys:replace_state(Link, fun(S) ->
                                    Ads = element(?ADVERTISEMENTS_INDEX, S),
                                    setelement(?ADVERTISEMENTS_INDEX, S,
                                               Ads#{{?REALM, procedure()} => #{kem => true, confidential => required}})
                                end),
    ok.

%% Whether THIS stream closed within Ms: a link closes other streams as sessions end, so the ref is what is watched.
closed_within(Stream, Ms) ->
    receive {closed, Stream} -> closed after Ms -> none end.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A served session's handler told to end, and seen gone, so its session is
%% released before the next test counts sessions.
ended(Handler) ->
    Ref = erlang:monitor(process, Handler),
    Handler ! stop,
    receive {'DOWN', Ref, process, Handler, _} -> ok after ?EVENT_MS -> error(handler_never_ended) end,
    timer:sleep(50).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

procedure() -> <<"acme/count_v1">>.

rotated(ok) -> ok;
rotated({ok, _}) -> ok.

%% A link that believes it is connected, with this process as its peering
%% connection and its dedicated streams, holding a KEM key.
world() -> world(256).

world(CallerQuota) ->
    {ok, _} = application:ensure_all_started(macula),
    Test = self(),
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => CallerQuota, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    {ok, Link} = macula_station_link:start_link(
                   #{seed => #{host => <<"127.0.0.1">>, port => 1}, expected_node_id => <<1:256>>,
                     node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
                     share => {seed, {<<"127.0.0.1">>, 1}},
                     connect => fun(_PeeringOpts) -> {error, not_dialed_here} end,
                     open_stream => fun(_Conn) -> Opened = make_ref(), Test ! {opened, Opened}, {ok, Opened} end,
                     send_on_stream => fun(Stream, Bytes) -> Test ! {written, Stream, Bytes}, ok end,
                     close_stream => fun(Stream) -> Test ! {closed, Stream}, ok end}),
    _ = sys:replace_state(Link, fun(S) -> setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Test),
                                                     <<2:256>>) end),
    NodeId = macula_node_keys:key_id(Key),
    _ = macula_kem_keyring:ensure(NodeId, profile()),
    #{link => Link, key => Key}.

stop(#{link := Link}) ->
    catch macula_station_link:stop(Link),
    ok.

node_id(#{key := Key}) -> macula_node_keys:key_id(Key).

advertise(#{link := Link}, Mode, Handler) ->
    macula_station_link:advertise_stream(Link, ?REALM, procedure(), Mode, Handler, open).

%% A STREAM_OPEN the caller seals to the node's current KEM key (or to
%% KemKey), written on a dedicated stream the peer opens: the stream, the
%% verified open, the keys it agreed and the caller.
sealed_open(W, Mode, Args) ->
    {ok, #{key := Carried}} = macula_kem_keyring:current(node_id(W)),
    sealed_open(W, Mode, Args, Carried).

sealed_open(#{link := Link} = W, Mode, Args, KemKey) ->
    {ok, Caller} = macula_node_keys:generate(identity, profile()),
    RequestId = crypto:strong_rand_bytes(16),
    Deadline = erlang:system_time(millisecond) + 30_000,
    SealRequest = #{frame_type => <<"stream_open">>, realm => ?REALM, procedure => procedure(),
                    caller => macula_node_keys:key_id(Caller), target => node_id(W), request_id => RequestId,
                    deadline => Deadline},
    {ok, Public} = public(macula_seal:public_key(KemKey)),
    {ok, Plain} = macula_frame:payload_plain(Args),
    {Sealed, Keys} = macula_sealed_call:seal_request(profile(), Public, SealRequest, Plain),
    Frame = wire(macula_frame:stream_open(#{request_id => RequestId, realm => ?REALM, procedure => procedure(),
                                            target => node_id(W), deadline => Deadline, mode => Mode,
                                            sealed => Sealed}, Caller)),
    Quic = make_ref(),
    Link ! {macula_peering, new_dedicated_stream, self(), Quic},
    Link ! {quic, macula_frame:encode(Frame), Quic, undefined},
    {ok, Open} = macula_frame:verify_request(Frame, profile()),
    #{quic => Quic, open => Open, keys => Keys, caller => Caller}.

public({ok, _Profile, Public}) -> {ok, Public}.

clear_open(W, Mode, Args) ->
    {ok, Caller} = macula_node_keys:generate(identity, profile()),
    clear_open(W, Mode, Args, Caller).

clear_open(#{link := Link} = W, Mode, Args, Caller) ->
    Frame = wire(macula_frame:stream_open(#{request_id => crypto:strong_rand_bytes(16), realm => ?REALM,
                                            procedure => procedure(), target => node_id(W),
                                            deadline => erlang:system_time(millisecond) + 30_000, mode => Mode,
                                            payload => Args}, Caller)),
    Quic = make_ref(),
    Link ! {macula_peering, new_dedicated_stream, self(), Quic},
    Link ! {quic, macula_frame:encode(Frame), Quic, undefined},
    {ok, Open} = macula_frame:verify_request(Frame, profile()),
    #{quic => Quic, open => Open}.

%% A caller STREAM_DATA at Seq, sealed under the open's k_c2p.
caller_sealed(Seq, Plain, #{k_c2p := KC2P, key_id := KeyId}, Caller, #{request_id := RequestId} = Open) ->
    Aad = macula_seal:stream_aad(<<"stream_data">>, RequestId, Seq, 0),
    Sealed = #{scheme => 1, key_id => KeyId, ct => macula_seal:seal(KC2P, macula_seal:stream_nonce(Seq), Aad, Plain)},
    wire(macula_frame:caller_stream(#{frame_type => stream_data, seq => Seq, encoding => raw, sealed => Sealed},
                                    Caller, Open)).

%% The plaintext of the provider's first frame, a sealed STREAM_DATA.
provider_opened(Frame, Seq, #{k_p2c := KP2C}, #{request_id := RequestId} = Open) ->
    {ok, #{frame_type := stream_data, seq := Seq, sealed := #{nonce := N, ct := Ct}}, _} =
        macula_frame:verify_provider_stream(Frame, macula_frame:open_stream(Open), profile()),
    macula_seal:open(KP2C, N, macula_seal:stream_aad(<<"stream_data">>, RequestId, Seq, 1), Ct).

%% The provider's first frame as a clear STREAM_ERROR.
clear_error(Frame, Open) ->
    case macula_frame:verify_provider_stream(Frame, macula_frame:open_stream(Open), profile()) of
        {ok, #{frame_type := stream_error, code := Code, message := Message}, _} -> {ok, #{code => Code, message => Message}};
        Other -> {not_a_clear_error, Other}
    end.

%% A refusal's verified fields, extension included: what a caller reads the keyed
%% one from.
clear_refusal(Frame, Open) ->
    {ok, Fields, _Verifier} = macula_frame:verify_provider_stream(Frame, macula_frame:open_stream(Open), profile()),
    Fields.

%% The provider's first frame as a STREAM_ERROR sealed under k_p2c, opened.
sealed_error(Frame, #{k_p2c := KP2C}, #{request_id := RequestId} = Open) ->
    {ok, #{frame_type := stream_error, sealed := #{nonce := N, ct := Ct}}, _} =
        macula_frame:verify_provider_stream(Frame, macula_frame:open_stream(Open), profile()),
    {ok, Plain} = macula_seal:open(KP2C, N, macula_seal:stream_aad(<<"stream_error">>, RequestId, 0, 1), Ct),
    macula_frame:plain_error(Plain).

on_stream(Link, Quic, Frame) ->
    Link ! {quic, macula_frame:encode(Frame), Quic, undefined}.

written(Quic) ->
    receive
        {written, Quic, Bytes} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            Frame
    after ?EVENT_MS ->
        error(nothing_written)
    end.

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

with_env(Key, Value, Fun) ->
    Old = application:get_env(macula, Key),
    ok = application:set_env(macula, Key, Value),
    try Fun() after restore_env(Key, Old) end.

restore_env(Key, undefined) -> application:unset_env(macula, Key);
restore_env(Key, {ok, Value}) -> application:set_env(macula, Key, Value).
