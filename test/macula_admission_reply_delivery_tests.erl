%% EUnit tests for what a provider's request admission does with a reply that is past its per-caller byte budget
%% (macula-portal#89: a portal fetching eight thumbnails at once is served one to three and times out on the rest).
%%
%% The budget bounds what is KEPT, not what is SENT. `macula_request_admission' refuses to store a reply past
%% `reply_bytes' for its caller, and `macula_station_link:replied/6' sends that reply anyway: the caller that asked
%% for it gets it. What the budget costs is the COPY: a repeated request whose reply was not kept is refused
%% `reply_not_kept' at once, by name, rather than served again.
%%
%% Both halves are asserted here against the production limits, since the budget only binds under them: eight
%% 64 KiB replies from one caller are 512 KiB in one 5 s window, against a default `reply_bytes' of 256 KiB. Four of
%% them fit and are kept; the rest are answered and not kept.
-module(macula_admission_reply_delivery_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"macula-portal#89">>)).
-define(PROCEDURE, <<"acme/thumbnail_v1">>).
-define(REPLY_BYTES, 65536).
-define(BURST, 8).
-define(EVENT_MS, 5_000).

-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).

%% The production limits a provider runs with when none are configured (macula_client:?DEFAULT_ADMISSION_LIMITS), and
%% the cap a pool derives for them: the share times the sum of the pool's link limits, which the admission accepts.
-define(PRODUCTION_LIMITS, #{caller_quota => 256, share => 1024, cap => 46080,
                             reply_bytes => 262144, reply_bytes_total => 16777216}).

reply_past_the_budget_is_still_delivered_test_() ->
    {timeout, 60, fun replies_past_the_budget/0}.

%%------------------------------------------------------------------
%% A reply the byte budget does not keep is still sent to the caller that asked
%%------------------------------------------------------------------

replies_past_the_budget() ->
    W = world(),
    try
        Burst = [call_frame(W, RequestId) || RequestId <- request_ids(?BURST)],
        [deliver(W, Frame) || {_RequestId, Frame} <- Burst],
        %% Every caller's own request is answered with the bytes it asked for, whether or not the budget kept a copy
        %% of that reply: 4 x 64 KiB fit the 256 KiB a caller may have kept, and the four after them do not. The ids
        %% are compared as a SET against the burst's own, so an answer that reached the wrong caller -- or two answers
        %% to one request and none to another -- fails here rather than passing as "eight replies arrived".
        Answers = answers(W, Burst),
        ?assertEqual(lists:sort([RequestId || {RequestId, _Frame} <- Burst]),
                     lists:sort([RequestId || {RequestId, _Answer} <- Answers])),
        ?assertEqual(lists:duplicate(?BURST, {result, ?REPLY_BYTES}), [Answer || {_RequestId, Answer} <- Answers]),
        %% What the budget does with them: some fit and are kept, the rest are answered and not kept. How many fit is
        %% the encoded size of a reply, not the bytes in it, so only "not all, and not none" is asserted.
        BurstNotStored = maps:get(reply_not_stored, refusals(W), 0),
        ?assert(BurstNotStored >= 1 andalso BurstNotStored =< ?BURST - 1),
        ?assertEqual(0, maps:get(reply_not_kept, refusals(W), 0)),
        %% A request made while the budget is full is answered the same way, and its reply is not kept either.
        Over = call_frame(W, crypto:strong_rand_bytes(16)),
        deliver(W, element(2, Over)),
        ?assertEqual([{element(1, Over), {result, ?REPLY_BYTES}}], answers(W, [Over])),
        ?assertEqual(BurstNotStored + 1, maps:get(reply_not_stored, refusals(W), 0)),
        %% A copy of that request is refused at once, by name: the one thing the budget costs a caller, and never a
        %% deadline spent waiting for a frame that was not going to come.
        deliver(W, element(2, Over)),
        ?assertEqual([{element(1, Over), {error, <<"reply_not_kept">>}}], answers(W, [Over]))
    after
        stop(W)
    end.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% A provider link with the production admission limits, its peer played by this process, serving a procedure that
%% answers with `?REPLY_BYTES' of its own.
world() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, ProviderKey} = macula_node_keys:generate(identity, Profile),
    {ok, CallerKey} = macula_node_keys:generate(identity, Profile),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> ProviderKey end, self()),
    {ok, Admission} = macula_request_admission:start_link(?PRODUCTION_LIMITS),
    {ok, Link} = macula_station_link:start_link(
                   #{seed => #{host => <<"127.0.0.1">>, port => 1, expected_node_id => <<1:256>>},
                     node_identity => fun() -> ProviderKey end, issuer => Issuer, admission => Admission,
                     share => {seed, {<<"127.0.0.1">>, 1}},
                     connect => fun(_PeeringOpts) -> {error, not_dialed_here} end}),
    Test = self(),
    _ = sys:replace_state(Link, fun(S) ->
                                        setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Test), <<1:256>>)
                                end),
    ok = macula_station_link:advertise(Link, ?REALM, ?PROCEDURE,
                                       fun(_Payload) -> #{bytes => binary:copy(<<0>>, ?REPLY_BYTES)} end, open),
    #{link => Link, admission => Admission, profile => Profile, caller_key => CallerKey,
      target => macula_node_keys:key_id(ProviderKey), issuer => Issuer}.

stop(#{link := Link, issuer := Issuer}) ->
    macula_station_link:stop(Link),
    Issuer ! stop,
    ok.

refusals(#{admission := Admission}) -> macula_request_admission:refusals(Admission).

request_ids(N) ->
    [crypto:strong_rand_bytes(16) || _ <- lists:seq(1, N)].

%% One CALL from the portal's key: the same caller for every call, which is what the per-caller byte budget counts.
call_frame(#{caller_key := CallerKey, target := Target}, RequestId) ->
    {RequestId, macula_frame:call(#{request_id => RequestId, realm => ?REALM, procedure => ?PROCEDURE,
                                    target => Target,
                                    deadline => erlang:system_time(millisecond) + ?EVENT_MS,
                                    payload => #{}}, CallerKey)}.

%% A frame reaching the link the way its peering connection delivers one.
deliver(#{link := Link}, Frame) ->
    Link ! {macula_peering, frame, self(), received(Frame)}.

received(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.

%% The answers the provider sends its peer, each with the request id it claims and verified against that request: a
%% RESULT carrying the bytes asked for, or an ERROR carrying its code. Returned with the id so a test can say WHICH
%% request each answer belongs to, not only how many arrived.
answers(#{profile := Profile}, Requests) ->
    Verified = maps:from_list([{RequestId, verified_request(Frame, Profile)} || {RequestId, Frame} <- Requests]),
    [reply(sent_frame(), Verified, Profile) || _ <- Requests].

verified_request(Frame, Profile) ->
    {ok, Verified} = macula_frame:verify_request(received(Frame), Profile),
    Verified.

sent_frame() ->
    receive
        {'$gen_cast', {send_frame, _At, Frame}} -> Frame
    after ?EVENT_MS -> erlang:error(no_frame_sent)
    end.

reply(Frame, Verified, Profile) ->
    {ok, #{request_id := RequestId}} = macula_frame:claimed_reply_ids(Frame),
    {RequestId, read_reply(macula_frame:verify_reply(Frame, maps:get(RequestId, Verified), Profile))}.

read_reply({ok, #{payload := Payload}}) ->
    {result, byte_size(bytes_of(Payload))};
read_reply({ok, #{code := Code}}) ->
    {error, Code};
read_reply({error, Reason}) ->
    erlang:error({reply_did_not_verify, Reason}).

%% A decoded payload's key is the atom this VM knows, or the text it does not.
bytes_of(#{bytes := Bytes}) -> Bytes;
bytes_of(#{{text, <<"bytes">>} := Bytes}) -> Bytes.
