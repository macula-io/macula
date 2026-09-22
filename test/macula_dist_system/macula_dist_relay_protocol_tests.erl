%% EUnit tests for macula_dist_relay_protocol: what the relay's control channel accepts.
%%
%% The control channel carries the frames that set a tunnel up: identify, tunnel_request, tunnel_ok, tunnel_notify,
%% tunnel_close. Each is a length and then that many bytes. Whatever is at the far end of it is a relay, which is a
%% forwarder and not a thing to be trusted, so a reader here has to hold two lines that a reader of a trusted
%% channel can be lazy about.
%%
%% ⚠ A LENGTH IS A PROMISE ABOUT BYTES THAT HAVE NOT ARRIVED. Read without a cap, a length of 4 GiB is a reader
%% that waits, holding everything that arrives in the meantime, and grows until the node dies. A control frame here
%% is tiny: the largest carries a node name.
%%
%% ⚠ A FRAME THAT DOES NOT DECODE MEANS THE READER AND THE WRITER NO LONGER AGREE WHERE FRAMES BEGIN. Skipping it
%% and reading on treats the next bytes as a length when they are the middle of something else, so one bad frame
%% becomes an endless run of them, and the channel looks alive throughout. The connection ends instead.
-module(macula_dist_relay_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

%% What the relay client would have done with a 4 GiB length before the cap: wait for it.
-define(ABSURD_LENGTH, 16#FFFFFFFF).

%%------------------------------------------------------------------
%% What a reader must refuse
%%------------------------------------------------------------------

a_length_above_the_cap_is_refused_test() ->
    ?assertEqual({error, frame_too_large},
                 macula_dist_relay_protocol:decode_buffer(<<?ABSURD_LENGTH:32/big, "whatever follows">>)).

%% Refused from the header alone: the bytes it promises are not waited for, and nothing is held while they do not
%% arrive.
a_length_above_the_cap_is_refused_before_its_bytes_test() ->
    ?assertEqual({error, frame_too_large},
                 macula_dist_relay_protocol:decode_buffer(<<?ABSURD_LENGTH:32/big>>)).

a_frame_that_does_not_decode_is_refused_test() ->
    ?assertMatch({error, _}, macula_dist_relay_protocol:decode_buffer(framed(<<"not CBOR at all">>))).

%% A frame that decodes but says nothing this protocol knows is refused the same way: the two ends disagree about
%% what is being said, which is not a thing to carry on through.
a_frame_of_an_unknown_type_is_refused_test() ->
    Unknown = macula_cbor_nif:pack(#{<<"t">> => <<"no_such_frame">>}),
    ?assertMatch({error, _}, macula_dist_relay_protocol:decode_buffer(framed(Unknown))).

%% A bad frame ends the channel rather than being skipped, so what followed it is never read as though the reader
%% were still in step.
a_refusal_does_not_deliver_what_followed_it_test() ->
    Good = macula_dist_relay_protocol:encode(#{type => identify, node_name => <<"node@host">>}),
    ?assertMatch({error, _}, macula_dist_relay_protocol:decode_buffer(<<(framed(<<"not CBOR">>))/binary, Good/binary>>)).

%%------------------------------------------------------------------
%% What a reader must still do
%%------------------------------------------------------------------

frames_decode_in_order_test() ->
    Identify = macula_dist_relay_protocol:encode(#{type => identify, node_name => <<"node@host">>}),
    Request = macula_dist_relay_protocol:encode(#{type => tunnel_request, target => <<"other@host">>}),
    ?assertEqual({ok, [#{type => identify, node_name => <<"node@host">>},
                       #{type => tunnel_request, target => <<"other@host">>}],
                  <<>>},
                 macula_dist_relay_protocol:decode_buffer(<<Identify/binary, Request/binary>>)).

%% A frame that has not all arrived is not an error: it is the ordinary case on a stream, and what has arrived
%% stays for the next read.
a_partial_frame_waits_test() ->
    Identify = macula_dist_relay_protocol:encode(#{type => identify, node_name => <<"node@host">>}),
    Head = binary:part(Identify, 0, byte_size(Identify) - 3),
    ?assertEqual({ok, [], Head}, macula_dist_relay_protocol:decode_buffer(Head)).

a_large_frame_still_decodes_test() ->
    Name = binary:copy(<<"n">>, 4_000),
    Encoded = macula_dist_relay_protocol:encode(#{type => identify, node_name => Name}),
    ?assertEqual({ok, [#{type => identify, node_name => Name}], <<>>},
                 macula_dist_relay_protocol:decode_buffer(Encoded)).

%% The boundary itself, read off the header with no body at all, so it pins where the cap is rather than that
%% there is one somewhere: the largest allowed length waits for its bytes, one more than that is refused.
the_cap_is_where_it_says_it_is_test() ->
    Max = macula_dist_relay_protocol:max_frame_bytes(),
    ?assertEqual({ok, [], <<Max:32/big>>}, macula_dist_relay_protocol:decode_buffer(<<Max:32/big>>)),
    ?assertEqual({error, frame_too_large}, macula_dist_relay_protocol:decode_buffer(<<(Max + 1):32/big>>)).

framed(Payload) ->
    <<(byte_size(Payload)):32/big, Payload/binary>>.
