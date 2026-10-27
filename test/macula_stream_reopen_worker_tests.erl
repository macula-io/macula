%% The reopen a refused clear stream starts runs in a worker, because it calls the link and the link may be calling
%% this stream at the same moment. The worker always answers: a link that is gone — and, for the same reason, one that
%% is busy past the call's timeout or that crashes — ends the session with an error, instead of leaving the stream in
%% `reopening' with every owner write queued behind a session that never opens and no deadline to end it (release A0,
%% review round 1: A1's reseal and the clear reopen's carried the same hole).
-module(macula_stream_reopen_worker_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SID, <<9:128>>).
-define(REOPENING_INDEX, macula_stream:state_field_index(reopening)).

a_reopen_whose_link_never_answers_ends_the_stream_with_an_error_test_() ->
    {timeout, 30, fun() ->
        #{stream := Stream, open := Open} = W = stream(),
        ok = macula_stream:attach_to_link(Stream, busy_link(), ?SID),
        ok = macula_stream:send(Stream, <<"abc">>),
        ok = macula_stream:deliver_frame(Stream, refuse(W, Open)),
        ?assertMatch({error, {reopen, exit, {timeout, _}}}, macula_stream:recv(Stream, 10_000)),
        ?assertEqual(undefined, element(?REOPENING_INDEX, sys:get_state(Stream))),
        gen_server:stop(Stream)
    end}.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

profile() -> pq_pure.

%% A link that is alive and never answers: it swallows the call, the way a link held inside another callback past the
%% call's timeout does. A link that is GONE is not this case — the stream monitors it and ends the session on its
%% DOWN — so the hole is the live link that never comes back.
busy_link() ->
    spawn(fun Loop() -> receive _ -> Loop() end end).

%% A clear caller stream that has not attached to its link yet, and a provider whose key a refusal names.
stream() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Caller} = macula_node_keys:generate(identity, profile()),
    {ok, Provider} = macula_node_keys:generate(identity, profile()),
    {ok, ProviderId} = macula_node_keys:node_id(Provider),
    {ok, OpenBytes} = macula_frame:stream_bytes({stream_open, #{request_id => crypto:strong_rand_bytes(16),
                                                                realm => <<1:256>>,
                                                                procedure => <<"acme/count_v1">>,
                                                                target => ProviderId,
                                                                deadline => erlang:system_time(millisecond) + 30_000,
                                                                payload => #{}, mode => bidi}}, Caller),
    {ok, OpenFrame, <<>>} = macula_frame:decode(macula_frame:written_bytes(OpenBytes)),
    {ok, Open} = macula_frame:verify_request(OpenFrame, profile()),
    {ok, Stream} = macula_stream:start_link(#{id => ?SID, role => client, mode => bidi, owner => self(),
                                              key => fun() -> Caller end, open => Open, conn => self(),
                                              profile => profile(), max_inbox_bytes => 4096}),
    {Public, _Private} = macula_seal:generate_key(profile()),
    #{stream => Stream, open => Open, provider => Provider, kem_key => macula_seal:key_as_carried(Public)}.

%% The provider refuses the open as a keyed one does, with the key it holds.
refuse(#{provider := Provider, kem_key := KemKey}, Open) ->
    macula_frame:provider_stream(#{frame_type => stream_error, seq => 0, code => <<"sealed_required">>,
                                   message => <<>>,
                                   ext => #{<<"seal.kem_key">> => #{critical => 0, value => KemKey}}},
                                 Provider, Open).
