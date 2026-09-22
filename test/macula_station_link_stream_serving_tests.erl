%% EUnit tests for the sessions a station link serves. A served session's handler runs once its open is admitted and
%% its policy passes: a policy that refuses answers with a verified provider STREAM_ERROR and runs no handler, as does a
%% caller past its session cap or a node whose session counter is down. However a served session ends, the processes
%% that served it end with it: a handler that aborts, and a caller that sends more than its inbox budget, end the
%% session with a verified STREAM_ERROR and crash nothing, and a handler that crashes tells its caller the crash's name
%% only. The test process is the link's peering connection, and the link's open_stream, send_on_stream and close_stream
%% options tell it what the link does to dedicated streams, so no shared module is replaced.
-module(macula_station_link_stream_serving_tests).

-include_lib("eunit/include/eunit.hrl").

%% macula_upload and macula_streamer callbacks, for the upload and streamer cases.
-export([init/1, handle_uploaded/2, handle_open/2]).

%% A realm id is SHA-256 over its name (D7), so a capability naming `mri:realm:test' is one in THIS realm.
-define(REALM_NAME, <<"test">>).
-define(REALM, crypto:hash(sha256, ?REALM_NAME)).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).
-define(MARKER, <<"a term of the crash that must not reach the caller">>).
-define(LOGGED_BYTES, 8192).
-define(SESSIONS, 3).
-define(EVENT_MS, 1_000).

stream_serving_test_() ->
    [{case_name(Case), {timeout, 15, {spawn, fun() -> process_flag(trap_exit, true), Case() end}}}
     || Case <- [fun a_stream_handler_crash_tells_the_caller_its_name_only/0,
                 fun a_served_session_its_handler_aborts_sends_its_stream_error/0,
                 fun a_served_session_past_its_inbox_budget_sends_its_stream_error/0,
                 fun a_stream_procedures_policy_is_enforced_before_its_handler/0,
                 fun a_stream_procedures_ucan_policy_binds_its_token_to_the_caller/0,
                 fun an_upload_advertised_with_a_policy_refuses_a_caller_without_a_token/0,
                 fun a_stream_open_past_the_callers_session_cap_is_refused/0,
                 fun a_stream_open_without_the_session_counter_is_refused/0,
                 fun a_refused_stream_open_starts_no_process/0,
                 fun a_streamer_served_session_ends_with_its_caller/0]]
    ++ [{"a served session leaves no process behind: " ++ Name,
         {timeout, 15, {spawn, fun() -> process_flag(trap_exit, true), served_sessions_end(Handler, Ending) end}}}
        || {Name, Handler, Ending} <- [{"the handler closes and returns", fun close_and_return/2, none},
                                        {"the handler aborts", fun abort_and_return/2, none},
                                        {"the handler crashes", fun crash_serving/2, none},
                                        {"the caller ends the stream", fun read_to_the_end/2, caller_end},
                                        {"the caller sends an error", fun read_to_the_end/2, caller_error},
                                        {"the link is lost", fun read_to_the_end/2, link_lost}]].

%%------------------------------------------------------------------
%% Cases
%%------------------------------------------------------------------

%% A handler that crashes answers its caller with a verified provider STREAM_ERROR whose code is the crash class and
%% whose message is the reason's name, with none of the crash's terms. The node's log gets the crash, within bounds.
a_stream_handler_crash_tells_the_caller_its_name_only() ->
    Log = macula_test_log:capture(),
    try
        #{link := Link} = World = linked(),
        Procedure = <<"foo.crashing">>,
        Crash = fun(_Stream, _Args) -> error({boom, lists:duplicate(10_000, ?MARKER)}) end,
        ok = macula_station_link:advertise_stream(Link, ?REALM, Procedure, server_stream, Crash),
        #{quic := Quic, frame := Open} = opened_by_peer(World, key(), Procedure, #{}),
        {ok, Refusal} = written_within(Quic, ?EVENT_MS),
        ?assertMatch({ok, #{frame_type := stream_error, code := <<"error">>, message := <<"boom">>}},
                     provider_fields(Open, Refusal)),
        ?assertEqual(nomatch, binary:match(term_to_binary(Refusal), ?MARKER)),
        Logged = macula_test_log:wait_text(Procedure, ?EVENT_MS),
        ?assert(byte_size(Logged) < ?LOGGED_BYTES),
        stop(Link)
    after
        macula_test_log:release(Log)
    end.

%% A handler that aborts its link-started session sends the verified provider STREAM_ERROR with its code and message on
%% the session's own stream. The session's processes end and the link goes on.
a_served_session_its_handler_aborts_sends_its_stream_error() ->
    #{link := Link} = World = linked(),
    Before = macula_test_sessions:serving(),
    Procedure = <<"foo.aborted">>,
    Abort = fun(Stream, _Args) -> macula_stream:abort(Stream, <<"stop">>, <<"why">>) end,
    ok = macula_station_link:advertise_stream(Link, ?REALM, Procedure, server_stream, Abort),
    #{quic := Quic, frame := Open} = opened_by_peer(World, key(), Procedure, #{}),
    {ok, Written} = written_within(Quic, ?EVENT_MS),
    ?assertMatch({ok, #{frame_type := stream_error, code := <<"stop">>, message := <<"why">>}},
                 provider_fields(Open, Written)),
    ?assertEqual([], macula_test_sessions:await_none_new(Before)),
    ?assert(is_process_alive(Link)),
    stop(Link).

%% A caller whose chunks a link-started session keeps unread past max_served_inbox_bytes_per_caller ends that session
%% with a verified provider STREAM_ERROR resource_exhausted on its own stream. The handler is told, the session's
%% processes end, and the link goes on.
a_served_session_past_its_inbox_budget_sends_its_stream_error() ->
    with_env(max_served_inbox_bytes_per_caller, 1_024, fun() ->
        #{link := Link} = World = linked(),
        Before = macula_test_sessions:serving(),
        Procedure = <<"foo.unread">>,
        ok = macula_station_link:advertise_stream(Link, ?REALM, Procedure, bidi, telling_how_it_ended(self())),
        Caller = key(),
        #{quic := Quic, frame := Open} = served(World, Caller, Procedure, bidi),
        on_stream(Link, Quic, caller_frame(#{frame_type => stream_data, seq => 0, encoding => raw,
                                             body => binary:copy(<<1>>, 2_048)}, Caller, Open)),
        {ok, Written} = written_within(Quic, ?EVENT_MS),
        ?assertMatch({ok, #{frame_type := stream_error, code := <<"resource_exhausted">>}},
                     provider_fields(Open, Written)),
        ?assertMatch({ended, {error, {<<"resource_exhausted">>, _}}}, how_it_ended(?EVENT_MS)),
        ?assertEqual([], macula_test_sessions:await_none_new(Before)),
        ?assert(is_process_alive(Link)),
        stop(Link)
    end).

%% advertise_stream/6 gates a streaming procedure as advertise/5 gates a unary one. An open the policy refuses gets a
%% verified provider STREAM_ERROR unauthorized on its own stream and runs no handler; an open whose token is a
%% membership UCAN for its caller is served. A token names its audience as the caller's key id, hex-encoded in
%% lowercase.
a_stream_procedures_policy_is_enforced_before_its_handler() ->
    #{link := Link} = World = linked(),
    Test = self(),
    Procedure = <<"acme/foo.gated">>,
    {ok, RealmIdentity} = macula_node_keys:generate(realm, pq_pure),
    Policy = {realm_member_required, macula_node_keys:key_id(RealmIdentity), <<"member/email-verified">>},
    ok = macula_station_link:advertise_stream(Link, ?REALM, Procedure, server_stream,
                                              fun(_Stream, Args) -> Test ! {handler_ran, macula:field(tag, Args)}, ok end,
                                              Policy),
    [NoTokenTag, MemberTag] = [1, 2],
    Caller = key(),
    #{quic := NoToken, frame := NoTokenOpen} = opened_by_peer(World, Caller, Procedure, #{tag => NoTokenTag}),
    Token = mint_ucan(RealmIdentity, macula_node_keys:key_id(Caller), <<"member/email-verified">>),
    _ = opened_by_peer(World, Caller, Procedure, #{tag => MemberTag}, #{token => Token}),
    {ok, Refusal} = written_within(NoToken, ?EVENT_MS),
    ?assertMatch({ok, #{code := <<"unauthorized">>}}, provider_fields(NoTokenOpen, Refusal)),
    ?assertEqual({handler_ran, MemberTag}, receive {handler_ran, MemberTag} = Ran -> Ran after ?EVENT_MS -> none end),
    ?assertEqual(none, receive {handler_ran, NoTokenTag} = Unauthorized -> Unauthorized after 300 -> none end),
    stop(Link).

%% A ucan_required stream procedure binds a token's audience to its caller. An open carrying a genuine token the issuer
%% granted another identity gets a verified provider STREAM_ERROR unauthorized and runs no handler; an open carrying
%% the caller's own token is served.
a_stream_procedures_ucan_policy_binds_its_token_to_the_caller() ->
    #{link := Link} = World = linked(),
    Test = self(),
    Procedure = <<"acme/foo.issuer_gated">>,
    Issuer = macula_test_identity:key(),
    {ok, IssuerNodeId} = macula_node_keys:node_id(Issuer),
    ok = macula_station_link:advertise_stream(Link, ?REALM, Procedure, server_stream,
                                              fun(_Stream, Args) -> Test ! {handler_ran, macula:field(tag, Args)}, ok end,
                                              {ucan_required, IssuerNodeId}),
    [ForSomeoneTag, OwnTag] = [1, 2],
    Caller = key(),
    ForSomeone = mint_ucan(Issuer, macula_node_keys:key_id(key()), <<"call">>),
    #{quic := ForSomeoneQuic, frame := ForSomeoneOpen} =
        opened_by_peer(World, Caller, Procedure, #{tag => ForSomeoneTag}, #{token => ForSomeone}),
    Own = mint_ucan(Issuer, macula_node_keys:key_id(Caller), <<"call">>),
    _ = opened_by_peer(World, Caller, Procedure, #{tag => OwnTag}, #{token => Own}),
    {ok, Refusal} = written_within(ForSomeoneQuic, ?EVENT_MS),
    ?assertMatch({ok, #{code := <<"unauthorized">>}}, provider_fields(ForSomeoneOpen, Refusal)),
    ?assertEqual({handler_ran, OwnTag}, receive {handler_ran, OwnTag} = Ran -> Ran after ?EVENT_MS -> none end),
    ?assertEqual(none, receive {handler_ran, ForSomeoneTag} = Unauthorized -> Unauthorized after 300 -> none end),
    stop(Link).

%% A gated upload advertised through macula_upload:advertise/6 answers an open that carries no token with a verified
%% STREAM_ERROR unauthorized on its own stream, and starts no receiver for it.
an_upload_advertised_with_a_policy_refuses_a_caller_without_a_token() ->
    #{link := Link} = World = linked(),
    Procedure = <<"acme/bulk.gated_ingest">>,
    Policy = {realm_member_required, macula_test_identity:node_id(), <<"member/email-verified">>},
    AdvertiseOnLink = fun(_Pool, Realm, Proc, Mode, Handler, Opts) ->
                          Auth = maps:get(auth, Opts, open),
                          macula_station_link:advertise_stream(Link, Realm, Proc, Mode, Handler, Auth)
                      end,
    {ok, Sup} = macula_upload:advertise(pool, ?REALM, Procedure, ?MODULE, self(),
                                        #{auth => Policy, advertise_stream => AdvertiseOnLink}),
    #{quic := Quic, frame := Open} = opened_by_peer(World, key(), Procedure, #{}, #{mode => client_stream}),
    {ok, Refusal} = written_within(Quic, ?EVENT_MS),
    ?assertMatch({ok, #{code := <<"unauthorized">>}}, provider_fields(Open, Refusal)),
    timer:sleep(100),
    ?assertEqual([], supervisor:which_children(Sup)),
    stop(Link).

%% A verified caller is served at most max_served_sessions_per_caller sessions at once. Its open past the cap is refused
%% with a verified too_many_sessions and runs no handler, while another caller is still served; once the first caller's
%% session ends, its place is given back and that caller is served again.
a_stream_open_past_the_callers_session_cap_is_refused() ->
    with_env(max_served_sessions_per_caller, 1, fun() ->
        #{link := Link} = World = linked(),
        Procedure = <<"foo.capped">>,
        ok = macula_station_link:advertise_stream(Link, ?REALM, Procedure, server_stream, telling_how_it_ended(self())),
        [Caller, Other] = [key(), key()],
        #{quic := FirstQuic, frame := FirstOpen} = served(World, Caller, Procedure),
        #{quic := SecondQuic, frame := SecondOpen} = opened_by_peer(World, Caller, Procedure, #{}),
        {ok, Refusal} = written_within(SecondQuic, ?EVENT_MS),
        ?assertMatch({ok, #{code := <<"too_many_sessions">>}}, provider_fields(SecondOpen, Refusal)),
        ?assertEqual(not_served, served_within(200)),
        ?assertMatch(#{}, served(World, Other, Procedure)),
        on_stream(Link, FirstQuic, caller_stream_end(Caller, FirstOpen)),
        ?assertEqual({ended, closed}, how_it_ended(?EVENT_MS)),
        %% The place is given back to THIS caller, and the other caller's
        %% session is untouched. Counted per caller, not as a share of the
        %% node's total: that total is global to the VM, so a session another
        %% test left behind and drains during this window would move it under
        %% the test's feet.
        ?assertEqual(0, caller_sessions_back_to(macula_node_keys:key_id(Caller), 0, ?EVENT_MS)),
        ?assertEqual(1, macula_stream_sessions:sessions(macula_node_keys:key_id(Other))),
        ?assertMatch(#{}, served(World, Caller, Procedure)),
        stop(Link)
    end).

%% Admission to a served session fails closed without holding up the link: with the session counter stopped, an open is
%% refused with a verified unavailable and its stream closes, the link stays up, and a session it already serves goes
%% on.
a_stream_open_without_the_session_counter_is_refused() ->
    #{link := Link} = World = linked(),
    Procedure = <<"foo.without_counter">>,
    ok = macula_station_link:advertise_stream(Link, ?REALM, Procedure, server_stream, telling_how_it_ended(self())),
    Caller = key(),
    #{quic := ServedQuic, frame := ServedOpen} = served(World, Caller, Procedure),
    ok = supervisor:terminate_child(macula_root, macula_stream_sessions),
    _ = restart_the_counter_later(),
    try
        #{quic := Quic, frame := Open} = opened_by_peer(World, Caller, Procedure, #{}),
        {ok, Refusal} = written_within(Quic, 2_000),
        ?assertMatch({ok, #{code := <<"unavailable">>}}, provider_fields(Open, Refusal)),
        ?assertEqual(closed, closed_within(Quic, ?EVENT_MS)),
        ?assert(is_process_alive(Link)),
        on_stream(Link, ServedQuic, caller_stream_end(Caller, ServedOpen)),
        ?assertEqual({ended, closed}, how_it_ended(?EVENT_MS))
    after
        _ = supervisor:restart_child(macula_root, macula_stream_sessions)
    end,
    stop(Link).

%% An open refused before any handler runs starts no process at all: each verified open for a procedure the link does
%% not advertise gets its verified not_found, and none of them leaves a process behind.
a_refused_stream_open_starts_no_process() ->
    #{link := Link} = World = linked(),
    Before = macula_test_sessions:serving(),
    Caller = key(),
    Opened = [opened_by_peer(World, Caller, <<"foo.nobody">>, #{}) || _ <- lists:seq(1, ?SESSIONS)],
    Codes = [Code || #{quic := Quic, frame := Open} <- Opened,
                     {ok, Refusal} <- [written_within(Quic, ?EVENT_MS)],
                     {ok, #{code := Code}} <- [provider_fields(Open, Refusal)]],
    ?assertEqual(lists:duplicate(?SESSIONS, <<"not_found">>), Codes),
    ?assertEqual([], macula_test_sessions:await_none_new(Before)),
    stop(Link).

%% A session served by a macula_streamer whose module never stops by itself ends with its caller: the streamer and its
%% stream process end too.
a_streamer_served_session_ends_with_its_caller() ->
    #{link := Link} = World = linked(),
    Before = macula_test_sessions:serving(),
    Procedure = <<"foo.streamed">>,
    AdvertiseOnLink = fun(_Pool, Realm, Proc, Mode, Handler, _Opts) ->
                          macula_station_link:advertise_stream(Link, Realm, Proc, Mode, Handler)
                      end,
    {ok, _Sup} = macula_streamer:advertise(pool, ?REALM, Procedure, ?MODULE, self(),
                                           #{announce => false, advertise_stream => AdvertiseOnLink,
                                             fact_publish => fun(_Pool, _Realm, _Topic, _Payload) -> ok end}),
    Caller = key(),
    Opened = [streamer_session(World, Caller, Procedure) || _ <- lists:seq(1, ?SESSIONS)],
    timer:sleep(200),
    ?assertEqual([], [Streamer || {_Session, Streamer} <- Opened, not is_process_alive(Streamer)]),
    ok = end_served_sessions(caller_end, World, Caller, [Session || {Session, _Streamer} <- Opened]),
    ?assertEqual([], macula_test_sessions:await_none_new(Before)),
    stop(Link).

%% However a served session ends, the processes that served it end with it: no stream process and no host process stays
%% behind. Each ending runs three sessions on one link.
served_sessions_end(Handler, Ending) ->
    Log = macula_test_log:capture(),
    try
        #{link := Link} = World = linked(),
        Before = macula_test_sessions:serving(),
        Procedure = <<"foo.sessions">>,
        Test = self(),
        ok = macula_station_link:advertise_stream(Link, ?REALM, Procedure, server_stream,
                                                  fun(Stream, Args) -> Test ! {session_served, Stream},
                                                                       Handler(Stream, Args) end),
        Caller = key(),
        Served = [served(World, Caller, Procedure) || _ <- lists:seq(1, ?SESSIONS)],
        ok = end_served_sessions(Ending, World, Caller, Served),
        ?assertEqual([], macula_test_sessions:await_none_new(Before)),
        stop(Link)
    after
        macula_test_log:release(Log)
    end.

end_served_sessions(none, _World, _Caller, _Served) ->
    ok;
end_served_sessions(caller_end, #{link := Link}, Caller, Served) ->
    _ = [on_stream(Link, Quic, caller_stream_end(Caller, Open)) || #{quic := Quic, frame := Open} <- Served],
    ok;
end_served_sessions(caller_error, #{link := Link}, Caller, Served) ->
    _ = [on_stream(Link, Quic, caller_frame(#{frame_type => stream_error, seq => 0, code => <<"error">>,
                                              message => <<"stop">>}, Caller, Open))
         || #{quic := Quic, frame := Open} <- Served],
    ok;
end_served_sessions(link_lost, #{link := Link}, _Caller, _Served) ->
    Link ! {macula_peering, disconnected, self(), peer_gone},
    ok.

close_and_return(Stream, _Args) -> macula_stream:close(Stream).

abort_and_return(Stream, _Args) -> macula_stream:abort(Stream, <<"stop">>, <<"stop">>).

crash_serving(_Stream, _Args) -> error(deliberate).

read_to_the_end(Stream, _Args) -> read_until_ended(macula_stream:recv(Stream, 5_000), Stream).

read_until_ended({chunk, _Chunk}, Stream) -> read_until_ended(macula_stream:recv(Stream, 5_000), Stream);
read_until_ended({data, _Data}, Stream) -> read_until_ended(macula_stream:recv(Stream, 5_000), Stream);
read_until_ended(_EndOrError, _Stream) -> ok.

%%------------------------------------------------------------------
%% macula_upload and macula_streamer callbacks
%%------------------------------------------------------------------

init(Parent) -> {ok, Parent}.

handle_uploaded(_Result, _Parent) -> ok.

%% A streamer module that never stops by itself.
handle_open(_StreamArgs, Parent) ->
    Parent ! {streamer_opened, self()},
    {ok, Parent}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

key() ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    Key.

%% A link that believes it is connected, with this process as its peering connection, and dedicated-stream functions
%% that tell this process what the link opens, writes and closes.
linked() ->
    {ok, _} = application:ensure_all_started(macula),
    Test = self(),
    Key = key(),
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
                     close_stream => fun(Stream) -> Test ! {closed, Stream}, ok end}),
    _ = sys:replace_state(Link, fun(S) -> setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Test),
                                                     <<2:256>>) end),
    #{link => Link, key => Key}.

stop(Link) ->
    catch macula_station_link:stop(Link),
    ok.

node_id(#{key := Key}) ->
    macula_node_keys:key_id(Key).

with_env(Key, Value, Fun) ->
    Old = application:get_env(macula, Key),
    ok = application:set_env(macula, Key, Value),
    try Fun() after restore_env(Key, Old) end.

restore_env(Key, undefined) -> application:unset_env(macula, Key);
restore_env(Key, {ok, Value}) -> application:set_env(macula, Key, Value).

%% A UCAN the issuer's key grants an audience, by node_id, for one ability, valid for an hour.
mint_ucan(IssuerKey, Audience, Can) ->
    macula_ucan:create(IssuerKey, Audience, [#{with => <<"mri:realm:test">>, can => Can}],
                       #{exp => erlang:system_time(second) + 3_600}).

%% Restarts the session counter once this test process ends or three seconds have passed, whichever comes first, so a
%% case cut off with the counter stopped does not leave it stopped for the cases after it.
restart_the_counter_later() ->
    Test = self(),
    spawn(fun() -> restart_the_counter_after(erlang:monitor(process, Test)) end).

restart_the_counter_after(Ref) ->
    receive
        {'DOWN', Ref, process, _Test, _Why} -> ok
    after 3_000 ->
        ok
    end,
    supervisor:restart_child(macula_root, macula_stream_sessions).

%% A session ends when its stream process does, which this test observes
%% rather than drives, so the count is waited on. Per caller, so nothing
%% another test leaves behind can be mistaken for this one's.
caller_sessions_back_to(Caller, Want, Ms) ->
    caller_count_back_to(macula_stream_sessions:sessions(Caller), Caller, Want, Ms).

caller_count_back_to(Want, _Caller, Want, _Ms) -> Want;
caller_count_back_to(Count, _Caller, _Want, Ms) when Ms =< 0 -> Count;
caller_count_back_to(_Count, Caller, Want, Ms) ->
    timer:sleep(20),
    caller_sessions_back_to(Caller, Want, Ms - 20).

%% A dedicated stream the peer opens with a verified STREAM_OPEN from Caller for Procedure, addressed to the link's
%% node: the stream and the open frame as sent.
opened_by_peer(World, Caller, Procedure, Payload) ->
    opened_by_peer(World, Caller, Procedure, Payload, #{}).

opened_by_peer(#{link := Link} = World, Caller, Procedure, Payload, Overrides) ->
    Spec = maps:merge(#{request_id => crypto:strong_rand_bytes(16), realm => ?REALM, procedure => Procedure,
                        target => node_id(World), deadline => erlang:system_time(millisecond) + 30_000,
                        payload => Payload, mode => server_stream},
                      Overrides),
    Frame = wire(macula_frame:stream_open(Spec, Caller)),
    Quic = make_ref(),
    Link ! {macula_peering, new_dedicated_stream, self(), Quic},
    Link ! {quic, macula_frame:encode(Frame), Quic, undefined},
    #{quic => Quic, frame => Frame}.

%% A session the link serves for Caller, once its handler has said so.
served(World, Caller, Procedure) ->
    served(World, Caller, Procedure, server_stream).

served(World, Caller, Procedure, Mode) ->
    Session = opened_by_peer(World, Caller, Procedure, #{}, #{mode => Mode}),
    receive {session_served, _} -> Session after ?EVENT_MS -> erlang:error(session_not_served) end.

served_within(Ms) ->
    receive {session_served, _} -> served after Ms -> not_served end.

streamer_session(World, Caller, Procedure) ->
    Session = opened_by_peer(World, Caller, Procedure, #{}),
    receive {streamer_opened, Streamer} -> {Session, Streamer} after ?EVENT_MS -> erlang:error(streamer_not_opened) end.

%% A handler that says it serves, and then how its session ended.
telling_how_it_ended(Test) ->
    fun(Stream, _Args) ->
        Test ! {session_served, Stream},
        receive {macula_stream, ended, Stream, How} -> Test ! {session_ended, How} end
    end.

how_it_ended(Ms) ->
    receive {session_ended, How} -> {ended, How} after Ms -> not_ended end.

verified(Frame) ->
    {ok, Open} = macula_frame:verify_request(Frame, profile()),
    Open.

caller_frame(Spec, Caller, OpenFrame) ->
    wire(macula_frame:caller_stream(Spec, Caller, verified(OpenFrame))).

%% The caller's STREAM_END, as its first frame on the session.
caller_stream_end(Caller, OpenFrame) ->
    caller_frame(#{frame_type => stream_end, seq => 0, role => both}, Caller, OpenFrame).

%% The fields of a provider frame the link wrote, verified as the provider's first frame under the open it answers.
provider_fields(OpenFrame, Written) ->
    fields(macula_frame:verify_provider_stream(Written, macula_frame:open_stream(verified(OpenFrame)), profile())).

fields({ok, Fields, _State}) -> {ok, Fields};
fields(Refused) -> Refused.

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

on_stream(Link, Stream, Frame) ->
    Link ! {quic, macula_frame:encode(Frame), Stream, undefined}.

written_within(Stream, Ms) ->
    receive
        {written, Stream, Bytes} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            {ok, Frame}
    after Ms ->
        none
    end.

closed_within(Stream, Ms) ->
    receive {closed, Stream} -> closed after Ms -> open end.
