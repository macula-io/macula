%% EUnit tests for advertising on `macula_station_link'. A link only registers a procedure's handler, unary or streaming,
%% so it can dispatch a CALL or STREAM_OPEN that its station delivers to it by target; it sends no ADVERTISE or
%% UNADVERTISE frame, before or after connecting.
%%
%% The test process stands in for the peering connection, and each test runs in a process of its own, so no frame an
%% earlier test's link sent is read as this test's.
-module(macula_station_link_advertise_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).
-define(PROCEDURE, <<"acme.count_v1">>).

%% The link's state fields these tests read or set, looked up by name in the state record.
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).

%% Advertising on a connected link registers the handler and sends nothing.
advertising_registers_the_handler_and_sends_no_frame_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_connected_link(),
         ok = macula_station_link:advertise(Pid, ?REALM, ?PROCEDURE, fun unary_handler/1),
         ?assertEqual(none, sent_frame_within(200)),
         ?assert(maps:is_key({?REALM, ?PROCEDURE}, registered(procedures, Pid))),
         macula_station_link:stop(Pid)
     end}}.

%% A handler advertised before the link connects stays registered, and connecting sends nothing for it.
an_advertisement_made_before_connect_sends_no_frame_on_connect_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_link_with_peer(),
         ok = macula_station_link:advertise(Pid, ?REALM, ?PROCEDURE, fun unary_handler/1),
         Pid ! {macula_peering, connected, self(), <<9:256>>},
         %% The link has handled the connected message before its mailbox is read.
         ?assertEqual(<<9:256>>, element(?PEER_NODE_ID_INDEX, sys:get_state(Pid))),
         ?assertEqual(none, sent_frame_within(300)),
         ?assert(maps:is_key({?REALM, ?PROCEDURE}, registered(procedures, Pid))),
         macula_station_link:stop(Pid)
     end}}.

%% Unadvertising removes the handler and sends nothing.
unadvertising_unregisters_the_handler_and_sends_no_frame_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_connected_link(),
         ok = macula_station_link:advertise(Pid, ?REALM, ?PROCEDURE, fun unary_handler/1),
         ok = macula_station_link:unadvertise(Pid, ?REALM, ?PROCEDURE),
         ?assertEqual(none, sent_frame_within(200)),
         ?assertNot(maps:is_key({?REALM, ?PROCEDURE}, registered(procedures, Pid))),
         macula_station_link:stop(Pid)
     end}}.

%% A streaming handler is registered and removed the same way, with nothing sent either time.
a_stream_advertisement_registers_and_unregisters_with_no_frame_test_() ->
    {spawn, {timeout, 5,
     fun() ->
         Pid = start_connected_link(),
         ok = macula_station_link:advertise_stream(Pid, ?REALM, ?PROCEDURE, bidi, fun stream_handler/2),
         ?assertEqual(none, sent_frame_within(200)),
         ?assertMatch(#{{?REALM, ?PROCEDURE} := {bidi, _}}, registered(stream_procedures, Pid)),
         ok = macula_station_link:unadvertise_stream(Pid, ?REALM, ?PROCEDURE),
         ?assertEqual(none, sent_frame_within(200)),
         ?assertNot(maps:is_key({?REALM, ?PROCEDURE}, registered(stream_procedures, Pid))),
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
