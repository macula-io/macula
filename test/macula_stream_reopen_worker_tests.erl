%% The edges of a stream's reopen: what happens when the link cannot answer it, and when the provider's first frame on
%% the new session lands before the worker's report that the session opened.
%%
%% The reopen runs in a worker because it calls the link, and the link may be calling this stream at that moment. That
%% worker always answers, so a link that is gone — and one held past the call's timeout, or crashing — ends the session
%% with an error instead of leaving the stream in `reopening' with every owner write queued behind a session that never
%% opens (release A0, review round 1). The link writes the new STREAM_OPEN before the worker reports back, and those
%% are two different senders, so a frame can land ahead of the report: it is held and read under the session that is
%% opening, rather than verified against the one the provider just refused and dropped (review round 2).
-module(macula_stream_reopen_worker_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SID, <<9:128>>).
-define(NEW_SID, <<8:128>>).
-define(KEY_ID, <<3:64>>).
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

%% The provider's first frame on the reopened session, delivered while the reopen is still in flight: it is read under
%% that session, so the owner receives it and the session's seqs start from it.
a_frame_that_lands_before_the_clear_reopen_is_read_under_it_test_() ->
    {timeout, 20, fun() ->
        #{stream := Stream, open := Open, provider := Provider} = W = stream(),
        ok = macula_stream:attach_to_link(Stream, self(), ?SID),
        ok = macula_stream:send(Stream, <<"abc">>),
        ok = macula_stream:deliver_frame(Stream, refuse(W, Open)),
        Answer = reopen_held(),
        NewOpen = verified_open(W, ?NEW_SID),
        #{k_p2c := KP2C} = NewSeal = seal(),
        ok = macula_stream:deliver_frame(Stream, ?NEW_SID,
                                         provider_sealed(stream_data, 0, #{encoding => raw}, <<"after">>,
                                                         KP2C, Provider, NewOpen)),
        Answer(#{open => NewOpen, seal => NewSeal, sid => ?NEW_SID}),
        ?assertEqual({chunk, <<"after">>}, macula_stream:recv(Stream, 2_000)),
        gen_server:stop(Stream)
    end}.

%% A frame on the session the stream still holds is not held: the old session stays open until the reopen lands, and
%% the provider answering on it is read where it lands. Holding every frame for the reopen's sake would blind the
%% stream to the session it already has, and a chunk that settles the report settles it there.
a_frame_on_the_session_the_stream_holds_is_read_while_the_reopen_is_in_flight_test_() ->
    {timeout, 20, fun() ->
        #{stream := Stream, open := Open, provider := Provider} = W = stream(),
        ok = macula_stream:attach_to_link(Stream, self(), ?SID),
        ok = macula_stream:send(Stream, <<"abc">>),
        ok = macula_stream:deliver_frame(Stream, refuse(W, Open)),
        Answer = reopen_held(),
        ok = macula_stream:deliver_frame(Stream, ?SID,
                                         wire(macula_frame:provider_stream(#{frame_type => stream_data, seq => 1,
                                                                             encoding => raw,
                                                                             body => <<"on the old">>},
                                                                           Provider, Open))),
        ?assertEqual({chunk, <<"on the old">>}, macula_stream:recv(Stream, 2_000)),
        NewOpen = verified_open(W, ?NEW_SID),
        NewSeal = seal(),
        Answer(#{open => NewOpen, seal => NewSeal, sid => ?NEW_SID}),
        %% The report settled on the old session's key, so the provider answered under a key it refused: the stream
        %% ends rather than swapping the report to the reopen's.
        ?assertMatch({error, {<<"malformed_frame">>, _}}, macula_stream:recv(Stream, 2_000)),
        gen_server:stop(Stream)
    end}.

%% The link can go while a reopen is in flight: a write on the old session fails, so the stream's peer is gone by the
%% time the report lands. There is nothing to adopt — adopting would crash on `peer = undefined' and kill the pid —
%% so the session the link opened is dropped there instead, and the stream ends with the transport failure it already
%% had (release A0, review round 2).
a_reopen_that_lands_after_the_link_went_drops_the_new_session_test_() ->
    {timeout, 20, fun() ->
        #{stream := Stream, open := Open} = W = stream(),
        ok = macula_stream:attach_to_link(Stream, self(), ?SID),
        ok = macula_stream:send(Stream, <<"abc">>),
        ok = macula_stream:deliver_frame(Stream, refuse(W, Open)),
        Answer = reopen_held(),
        %% A write on the old session fails: the stream's peer goes.
        Stream ! {stream_write_failed, ?SID, gone},
        NewOpen = verified_open(W, ?NEW_SID),
        NewSeal = seal(),
        Answer(#{open => NewOpen, seal => NewSeal, sid => ?NEW_SID}),
        receive {session_dropped, Dropped} -> ?assertEqual(?NEW_SID, Dropped)
        after 2_000 -> error(not_dropped)
        end,
        %% The stream is alive, and ended with the transport failure.
        ?assertMatch({error, _}, macula_stream:recv(Stream, 2_000)),
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

%% The reopen request, held until this process answers it: a link that has already dispatched a frame to the stream and
%% not yet let the worker's report through.
reopen_held() ->
    receive
        {'$gen_call', From, {reopen_stream, _Stream, _Sid, _Open, _Args, _KemKey}} ->
            fun(Reopened) -> gen_server:reply(From, {ok, Reopened}) end
    after 2_000 ->
        error(no_reopen_asked)
    end.

seal() ->
    #{k_req => crypto:strong_rand_bytes(32), k_c2p => crypto:strong_rand_bytes(32),
      k_p2c => crypto:strong_rand_bytes(32), key_id => ?KEY_ID}.

%% A clear caller stream that has not attached to its link yet, and a provider whose key a refusal names.
stream() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Caller} = macula_node_keys:generate(identity, profile()),
    {ok, Provider} = macula_node_keys:generate(identity, profile()),
    W = #{caller => Caller, provider => Provider},
    Open = verified_open(W, crypto:strong_rand_bytes(16)),
    {ok, Stream} = macula_stream:start_link(#{id => ?SID, role => client, mode => bidi, owner => self(),
                                              key => fun() -> Caller end, open => Open, conn => self(),
                                              profile => profile(), max_inbox_bytes => 4096}),
    {Public, _Private} = macula_seal:generate_key(profile()),
    W#{stream => Stream, open => Open, kem_key => macula_seal:key_as_carried(Public)}.

verified_open(#{caller := Caller, provider := Provider}, RequestId) ->
    {ok, ProviderId} = macula_node_keys:node_id(Provider),
    {ok, OpenBytes} = macula_frame:stream_bytes({stream_open, #{request_id => RequestId, realm => <<1:256>>,
                                                                procedure => <<"acme/count_v1">>,
                                                                target => ProviderId,
                                                                deadline => erlang:system_time(millisecond) + 30_000,
                                                                payload => #{}, mode => bidi}}, Caller),
    {ok, OpenFrame, <<>>} = macula_frame:decode(macula_frame:written_bytes(OpenBytes)),
    {ok, Open} = macula_frame:verify_request(OpenFrame, profile()),
    Open.

%% The provider refuses the open as a keyed one does, with the key it holds.
refuse(#{provider := Provider, kem_key := KemKey}, Open) ->
    macula_frame:provider_stream(#{frame_type => stream_error, seq => 0, code => <<"sealed_required">>,
                                   message => <<>>,
                                   ext => #{<<"seal.kem_key">> => #{critical => 0, value => KemKey}}},
                                 Provider, Open).

%% A provider frame of Type at Seq sealing Plain under Key in direction 1, on the session Open.
provider_sealed(Type, Seq, Extra, Plain, Key, Provider, Open) ->
    Nonce = macula_seal:random_nonce(),
    Aad = macula_seal:stream_aad(atom_to_binary(Type), maps:get(request_id, Open), Seq, 1),
    Sealed = #{scheme => 1, key_id => ?KEY_ID, nonce => Nonce, ct => macula_seal:seal(Key, Nonce, Aad, Plain)},
    wire(macula_frame:provider_stream(Extra#{frame_type => Type, seq => Seq, sealed => Sealed}, Provider, Open)).

wire(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.
