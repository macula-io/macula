%% EUnit tests for how a caller's link keeps its gated streams authorized past the token they were opened with
%% (macula#120). A stream opened with a token and a `ucan_renewal' source is renewed by a signed CALL to the provider's
%% own `~<provider>/stream_renew', at a third of the token's remaining life, one CALL per provider and source naming
%% every stream still open, carrying the token the source hands out then. A renewal the provider answers starts the
%% next third from the new token; a provider-signed `unknown_next_peer' (an SDK without stream_renew) ends the
%% renewing; anything else, a station's relay error or a provider's refusal included, is tried again, never past the
%% current end. The test process is the link's peering connection and its dedicated streams, and plays the provider.
-module(macula_station_link_stream_renewal_caller_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, crypto:hash(sha256, <<"test">>)).
-define(PROCEDURE, <<"acme/count_v1">>).
-define(PEER_PID_INDEX, macula_station_link:state_field_index(peer_pid)).
-define(PEER_NODE_ID_INDEX, macula_station_link:state_field_index(peer_node_id)).

stream_renewal_caller_test_() ->
    [{Name, {timeout, 20, fun() -> W = world(), try Case(W) after stop(W) end end}}
      || {Name, Case} <- [{"one renewal CALL per provider names its open streams at a third of the token's life",
                           fun one_call_per_provider_names_its_open_streams/1},
                          {"a renewal the provider answers starts the next third from the new token",
                           fun an_answered_renewal_starts_the_next_third/1},
                          {"a provider-signed unknown_next_peer ends the renewing",
                           fun a_provider_without_stream_renew_is_not_asked_again/1},
                          {"a station's relay error is tried again before the end",
                           fun a_relay_error_is_tried_again/1},
                          {"a refused renewal is tried again with the next token",
                           fun a_refused_renewal_is_tried_with_the_next_token/1},
                          {"a stream that ended is not named, and none left asks nothing",
                           fun an_ended_stream_is_not_named/1},
                          {"a stream without a source, or to the station, is not renewed",
                           fun a_stream_without_a_source_is_not_renewed/1},
                          {"a source that is not a function of no arguments is refused where the stream is opened",
                           fun a_source_of_another_shape_is_refused/1}]].

one_call_per_provider_names_its_open_streams(#{link := Link, provider_id := Provider} = W) ->
    Source = tokens(W, [3, 60]),
    A = open(Link, Provider, token(W, 3), Source),
    B = open(Link, Provider, token(W, 3), Source),
    Opened = erlang:system_time(millisecond),
    Request = renewal_call(3_000),
    Waited = erlang:system_time(millisecond) - Opened,
    ?assert(Waited >= 400 andalso Waited =< 1_500),
    ?assertMatch(#{target := Provider, realm := _}, Request),
    ?assertEqual(macula_station_link:stream_renewal_procedure(Provider), maps:get(procedure, Request)),
    ?assertEqual(lists:sort([A, B]), lists:sort(maps:get({text, <<"streams">>}, maps:get(payload, Request)))),
    ?assertEqual(source_token(1), maps:get(token, Request)),
    ?assertEqual(none, renewal_call_within(300)).

an_answered_renewal_starts_the_next_third(#{link := Link, provider_id := Provider} = W) ->
    A = open(Link, Provider, token(W, 3), tokens(W, [6, 60])),
    First = renewal_call(3_000),
    Answered = erlang:system_time(millisecond),
    answer(W, First, #{renewed => [A], refused => []}),
    ?assertEqual(none, renewal_call_within(900)),
    Second = renewal_call(3_000),
    ?assert(erlang:system_time(millisecond) - Answered >= 1_000),
    ?assertEqual(source_token(2), maps:get(token, Second)).

a_provider_without_stream_renew_is_not_asked_again(#{link := Link, provider_id := Provider,
                                                     provider := ProviderKey} = W) ->
    _ = open(Link, Provider, token(W, 4), tokens(W, [4, 4, 4])),
    First = renewal_call(3_000),
    deliver(Link, macula_frame:provider_error(#{request => First, code => <<"unknown_next_peer">>}, ProviderKey)),
    ?assertEqual(none, renewal_call_within(2_500)).

a_relay_error_is_tried_again(#{link := Link, provider_id := Provider, station := StationKey} = W) ->
    _ = open(Link, Provider, token(W, 5), tokens(W, [5, 5, 5])),
    First = renewal_call(3_000),
    deliver(Link, macula_frame:relay_error(#{frame_type => error, request => First, code => unknown_next_peer},
                                           StationKey)),
    ?assertMatch(#{procedure := _}, renewal_call(2_500)).

a_refused_renewal_is_tried_with_the_next_token(#{link := Link, provider_id := Provider} = W) ->
    A = open(Link, Provider, token(W, 5), tokens(W, [5, 5, 5])),
    First = renewal_call(3_000),
    answer(W, First, #{renewed => [], refused => [A]}),
    Second = renewal_call(2_500),
    ?assertEqual(source_token(2), maps:get(token, Second)),
    ?assertNotEqual(maps:get(token, First), maps:get(token, Second)).

an_ended_stream_is_not_named(#{link := Link, provider_id := Provider} = W) ->
    Source = tokens(W, [3, 3, 3]),
    {A, StreamA} = open_stream(Link, Provider, token(W, 3), Source),
    {B, StreamB} = open_stream(Link, Provider, token(W, 3), Source),
    ok = ended(StreamA),
    ?assertEqual([B], maps:get({text, <<"streams">>}, maps:get(payload, renewal_call(3_000)))),
    ?assertNotEqual(A, B),
    ok = ended(StreamB),
    ?assertEqual(none, renewal_call_within(2_000)).

a_stream_without_a_source_is_not_renewed(#{link := Link, provider_id := Provider} = W) ->
    {ok, _} = macula_station_link:call_stream(Link, Provider, ?REALM, ?PROCEDURE, #{},
                                              #{mode => bidi, seal => clear, ucan_token => token(W, 3)}),
    {ok, _} = macula_station_link:call_stream(Link, station, ?REALM, ?PROCEDURE, #{},
                                              #{mode => bidi, seal => clear, ucan_token => token(W, 3),
                                                ucan_renewal => tokens(W, [60])}),
    ?assertEqual(none, renewal_call_within(2_000)).

a_source_of_another_shape_is_refused(#{link := Link, provider_id := Provider} = W) ->
    ?assertError(function_clause,
                 macula_station_link:call_stream(Link, Provider, ?REALM, ?PROCEDURE, #{},
                                                 #{mode => bidi, seal => clear, ucan_token => token(W, 3),
                                                   ucan_renewal => fun(_) -> {ok, <<>>} end})).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

profile() -> pq_pure.

%% A link that believes it is connected to a station, with this process as its peering connection and its dedicated
%% streams, the station's key, a provider's key, and an issuer of tokens for the link's own node.
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
    {ok, Station} = macula_node_keys:generate(identity, profile()),
    StationId = macula_node_keys:key_id(Station),
    _ = sys:replace_state(Link, fun(S) -> setelement(?PEER_NODE_ID_INDEX, setelement(?PEER_PID_INDEX, S, Test),
                                                     StationId) end),
    {ok, Provider} = macula_node_keys:generate(identity, profile()),
    {ok, TokenIssuer} = macula_node_keys:generate(identity, profile()),
    #{link => Link, node => macula_node_keys:key_id(Key), station => Station, provider => Provider,
      provider_id => macula_node_keys:key_id(Provider), issuer => TokenIssuer}.

stop(#{link := Link}) ->
    catch macula_station_link:stop(Link),
    ok.

%% A token for the link's node, ending Seconds from now.
token(#{issuer := Issuer, node := Node}, Seconds) ->
    macula_ucan:create(Issuer, Node, [#{with => <<"mri:realm:test">>, can => <<"call">>}],
                       #{exp => erlang:system_time(second) + Seconds}).

%% A source handing out a fresh token on each ask, the Nth ending the Nth of Lives seconds from then; each token is
%% remembered under source_token(N), so a test can tell which one a CALL carried.
tokens(W, Lives) ->
    Counter = counters:new(1, []),
    fun() ->
        ok = counters:add(Counter, 1, 1),
        N = counters:get(Counter, 1),
        Token = token(W, lists:nth(min(N, length(Lives)), Lives)),
        persistent_term:put({?MODULE, N}, Token),
        {ok, Token}
    end.

source_token(N) ->
    persistent_term:get({?MODULE, N}).

%% A bidi stream to Provider opened with Token and the renewal Source, by its request id.
open(Link, Provider, Token, Source) ->
    {RequestId, _Stream} = open_stream(Link, Provider, Token, Source),
    RequestId.

open_stream(Link, Provider, Token, Source) ->
    {ok, Stream} = macula_station_link:call_stream(Link, Provider, ?REALM, ?PROCEDURE, #{},
                                                   #{mode => bidi, seal => clear, ucan_token => Token,
                                                     ucan_renewal => Source}),
    Quic = receive {opened, Opened} -> Opened after 3_000 -> error(no_stream_opened) end,
    {ok, #{request_id := RequestId}} = macula_frame:verify_request(written(Quic), profile()),
    {RequestId, Stream}.

%% Ends a caller's stream and waits until it is gone.
ended(Stream) ->
    Mon = erlang:monitor(process, Stream),
    unlink(Stream),
    exit(Stream, kill),
    receive {'DOWN', Mon, process, Stream, _} -> ok after 3_000 -> error(stream_not_ended) end.

written(Quic) ->
    receive
        {written, Quic, Bytes} ->
            {ok, Frame, <<>>} = macula_frame:decode(Bytes),
            Frame
    after 3_000 ->
        error(nothing_written)
    end.

%% The next renewal CALL the link sends within Ms, verified, or an error.
renewal_call(Ms) ->
    case renewal_call_within(Ms) of
        none -> error(no_renewal_call);
        Request -> Request
    end.

renewal_call_within(Ms) ->
    receive
        {'$gen_cast', {send_frame, _, #{frame_type := call} = Frame}} ->
            {ok, Request} = macula_frame:verify_request(received(Frame), profile()),
            Request
    after Ms ->
        none
    end.

%% The provider's RESULT to a renewal CALL.
answer(#{link := Link, provider := ProviderKey}, Request, Payload) ->
    deliver(Link, macula_frame:result(#{request => Request, payload => Payload}, ProviderKey)).

deliver(Link, Frame) ->
    Link ! {macula_peering, frame, self(), received(Frame)}.

received(Frame) ->
    {ok, Decoded, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    Decoded.
