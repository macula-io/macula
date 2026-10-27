%% A clear caller stream's copy is bounded by the same number as its inbox (`max_inbox_bytes`), and it is the copy's
%% own count: DESIGN_E2E_SEALED_BY_DEFAULT §3, release A0. Past that bound retention stops and the copy is dropped, so a
%% later `sealed_required' reaches the owner as the provider sent it - the provider before release A, or one this node
%% cannot seal to - instead of reopening a session whose first frames it no longer holds. The link, the connection and
%% the provider are played here by this test process.
-module(macula_stream_clear_copy_bound_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SID, <<9:128>>).
-define(REPLAY_INDEX, macula_stream:state_field_index(replay)).

%% Three bytes fit a four-byte bound and are kept; three more do not fit it, and drop the copy.
the_copy_is_bounded_by_the_streams_inbox_bound_test() ->
    #{stream := Stream} = stream(4),
    ok = macula_stream:send(Stream, <<"abc">>),
    ?assertEqual(#{bytes => 3, sent => [{chunk, raw, <<"abc">>}]}, replay_of(Stream)),
    ok = macula_stream:send(Stream, <<"def">>),
    ?assertEqual(undefined, replay_of(Stream)),
    gen_server:stop(Stream).

%% With no copy left, the refusal is the owner's answer at once and nothing is reopened.
a_refusal_past_the_bound_surfaces_as_sealed_required_test() ->
    #{stream := Stream, open := Open} = W = stream(4),
    ok = macula_stream:send(Stream, <<"abc">>),
    ok = macula_stream:send(Stream, <<"def">>),
    ok = macula_stream:deliver_frame(Stream, refuse(W, Open)),
    ?assertEqual({error, {<<"sealed_required">>, <<>>}}, macula_stream:recv(Stream, 1_000)),
    ?assert(is_process_alive(Stream)),
    gen_server:stop(Stream).

%% The owner's STREAM_END is copied too, and costs the copy nothing: it carries no bytes.
the_owners_end_is_copied_at_no_cost_test() ->
    #{stream := Stream} = stream(8),
    ok = macula_stream:send(Stream, <<"abc">>),
    ok = macula_stream:close_send(Stream),
    ?assertEqual(#{bytes => 3, sent => [{end_stream, send}, {chunk, raw, <<"abc">>}]}, replay_of(Stream)),
    gen_server:stop(Stream).

%% What is not the owner's own data is not copied: an abort ends the session, and its STREAM_ERROR is no frame to
%% replay.
an_abort_is_not_copied_test() ->
    #{stream := Stream} = stream(8),
    ok = macula_stream:send(Stream, <<"abc">>),
    ok = macula_stream:abort(Stream, <<"aborted">>, <<"why">>),
    ?assertEqual([{chunk, raw, <<"abc">>}], sent_of(Stream)),
    gen_server:stop(Stream).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

profile() -> pq_pure.

%% A clear caller stream attached to this process as its link, with `Bound' bytes for the copy and for its inbox, and
%% a provider whose key a refusal names.
stream(Bound) ->
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
                                              profile => profile(), max_inbox_bytes => Bound}),
    ok = macula_stream:attach_to_link(Stream, self(), ?SID),
    {Public, _Private} = macula_seal:generate_key(profile()),
    #{stream => Stream, open => Open, provider => Provider,
      kem_key => macula_seal:key_as_carried(Public)}.

%% The provider refuses the open as a keyed one does, with the key it holds.
refuse(#{provider := Provider, kem_key := KemKey}, Open) ->
    macula_frame:provider_stream(#{frame_type => stream_error, seq => 0, code => <<"sealed_required">>,
                                   message => <<>>,
                                   ext => #{<<"seal.kem_key">> => #{critical => 0, value => KemKey}}},
                                 Provider, Open).

replay_of(Stream) ->
    element(?REPLAY_INDEX, sys:get_state(Stream)).

sent_of(Stream) ->
    maps:get(sent, replay_of(Stream)).
