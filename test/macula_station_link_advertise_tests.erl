%% EUnit tests for advertising on `macula_station_link'. A link registers a procedure's handler, unary or streaming,
%% so it can dispatch a CALL or STREAM_OPEN that its station delivers to it; given the pool-signed provider
%% advertisement (the resolved D25 authorization) it sends the ADVERTISE frame at once when connected, drains a
%% register that arrived before the handshake on `connected', and sends the UNADVERTISE withdrawal on unadvertise.
%% Without the advertisement bytes (a legacy pool-level register) it sends nothing, before or after connecting.
%%
%% The test process stands in for the peering connection, and each test runs in a process of its own, so no frame an
%% earlier test's link sent is read as this test's.
-module(macula_station_link_advertise_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(PROCEDURE, <<"acme.count_v1">>).
-define(ENCODED_AD, <<1, 2, 3>>).

%% The link's state fields these tests read or set, looked up by name in the state record.
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).

%% A connected link with a resolved advertisement sends the ADVERTISE
%% frame carrying its bytes.
advertising_with_a_resolved_advertisement_sends_the_advertise_frame_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_connected_link(),
         ok = macula_station_link:advertise(Pid, ?REALM, ?PROCEDURE,
                                            fun unary_handler/1, open,
                                            ?ENCODED_AD),
         ?assertMatch({sent, #{frame_type := advertise,
                               advertisement := ?ENCODED_AD}},
                      sent_frame_within(200)),
         ?assert(maps:is_key({?REALM, ?PROCEDURE}, registered(procedures, Pid))),
         macula_station_link:stop(Pid)
     end}}.

%% A legacy register without the resolved advertisement sends nothing.
advertising_without_a_resolved_advertisement_sends_no_frame_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_connected_link(),
         ok = macula_station_link:advertise(Pid, ?REALM, ?PROCEDURE,
                                            fun unary_handler/1),
         ?assertEqual(none, sent_frame_within(200)),
         ?assert(maps:is_key({?REALM, ?PROCEDURE}, registered(procedures, Pid))),
         macula_station_link:stop(Pid)
     end}}.

%% An advertisement registered before the handshake is drained onto the
%% wire once `connected' fires.
an_advertisement_made_before_connect_is_sent_on_connect_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_link_with_peer(),
         ok = macula_station_link:advertise(Pid, ?REALM, ?PROCEDURE,
                                            fun unary_handler/1, open,
                                            ?ENCODED_AD),
         ?assertEqual(none, sent_frame_within(200)),
         Pid ! {macula_peering, connected, self(), <<9:256>>},
         %% The link has handled the connected message before its mailbox is read.
         ?assertEqual(<<9:256>>, element(?PEER_NODE_ID_INDEX, sys:get_state(Pid))),
         ?assertMatch({sent, #{frame_type := advertise,
                               advertisement := ?ENCODED_AD}},
                      sent_frame_within(300)),
         ?assert(maps:is_key({?REALM, ?PROCEDURE}, registered(procedures, Pid))),
         macula_station_link:stop(Pid)
     end}}.

%% Unadvertising with a withdrawal sends the UNADVERTISE frame and
%% drops the stored advertisement.
unadvertising_with_a_withdrawal_sends_the_unadvertise_frame_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_connected_link(),
         ok = macula_station_link:advertise(Pid, ?REALM, ?PROCEDURE,
                                            fun unary_handler/1, open,
                                            ?ENCODED_AD),
         _ = sent_frame_within(200),
         ok = macula_station_link:unadvertise(Pid, ?REALM, ?PROCEDURE,
                                              <<4, 5, 6>>),
         ?assertMatch({sent, #{frame_type := unadvertise,
                               withdrawal := <<4, 5, 6>>}},
                      sent_frame_within(200)),
         ?assertNot(maps:is_key({?REALM, ?PROCEDURE}, registered(procedures, Pid))),
         macula_station_link:stop(Pid)
     end}}.

%% Unadvertising without a withdrawal sends nothing.
unadvertising_without_a_withdrawal_sends_no_frame_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_connected_link(),
         ok = macula_station_link:advertise(Pid, ?REALM, ?PROCEDURE,
                                            fun unary_handler/1, open,
                                            ?ENCODED_AD),
         _ = sent_frame_within(200),
         ok = macula_station_link:unadvertise(Pid, ?REALM, ?PROCEDURE),
         ?assertEqual(none, sent_frame_within(200)),
         ?assertNot(maps:is_key({?REALM, ?PROCEDURE}, registered(procedures, Pid))),
         macula_station_link:stop(Pid)
     end}}.

%% A streaming advertisement shares the same wire behaviour: the frame
%% does not distinguish streaming from unary procedures.
a_stream_advertisement_sends_the_same_advertise_frame_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_connected_link(),
         ok = macula_station_link:advertise_stream(Pid, ?REALM, ?PROCEDURE,
                                                   bidi, fun stream_handler/2,
                                                   open, ?ENCODED_AD),
         ?assertMatch({sent, #{frame_type := advertise,
                               advertisement := ?ENCODED_AD}},
                      sent_frame_within(200)),
         ?assertMatch(#{{?REALM, ?PROCEDURE} := {bidi, _}},
                      registered(stream_procedures, Pid)),
         ok = macula_station_link:unadvertise_stream(Pid, ?REALM, ?PROCEDURE,
                                                     <<7, 8, 9>>),
         ?assertMatch({sent, #{frame_type := unadvertise,
                               withdrawal := <<7, 8, 9>>}},
                      sent_frame_within(200)),
         ?assertNot(maps:is_key({?REALM, ?PROCEDURE},
                                registered(stream_procedures, Pid))),
         macula_station_link:stop(Pid)
     end}}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

unary_handler(_Payload) -> {ok, counted}.

stream_handler(_StreamPid, _Args) -> ok.

%% Start options with the keys a link starts with: a node identity key in the node's profile, an issuer of its own for
%% that key, owned by the calling process, and the node_id its seed expects.
with_link_keys(Opts) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    %% A link also starts with a request admission and its share in it.
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    Opts#{node_identity => fun() -> Key end, issuer => Issuer, admission => Admission,
          share => {seed, {<<"127.0.0.1">>, 1}}, expected_node_id => <<1:256>>}.

%% A link whose peering connection is this process, not yet connected.
start_link_with_peer() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Pid} = macula_station_link:start_link(with_link_keys(#{
        seed     => #{host => <<"127.0.0.1">>, port => 1},
        connect_timeout_ms => 2000
    })),
    FakePeer = self(),
    _ = sys:replace_state(Pid, fun(S) -> setelement(?PEER_PID_INDEX, S, FakePeer) end),
    Pid.

%% A link that believes it is connected, with this process as its peer.
start_connected_link() ->
    Pid = start_link_with_peer(),
    _ = sys:replace_state(Pid, fun(S) -> setelement(?PEER_NODE_ID_INDEX, S, <<9:256>>) end),
    Pid.

%% The first frame the link sent to its peer within `Ms', or none.
sent_frame_within(Ms) ->
    receive
        {'$gen_cast', {send_frame, Frame}} -> {sent, Frame}
    after Ms ->
        none
    end.

%% A registry of the link's state: `procedures' or `stream_procedures'.
registered(Field, Pid) ->
    element(macula_station_link:state_field_index(Field), sys:get_state(Pid)).
