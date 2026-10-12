%%%-------------------------------------------------------------------
%%% @doc One valid frame per wire 1.0 frame type, for the conformance
%%% vectors. Every frame type macula_frame reads is here except connect
%%% and hello, which wire 1.0 retires after HELLO. Each frame is built by
%%% macula_frame's own constructors from deterministic synthetic values,
%%% and signed frames are signed with the pq_pure keys the caller passes:
%%% the client_identity key for a caller or publisher, the
%%% station_identity key for a provider or station.
%%% @end
%%%-------------------------------------------------------------------
-module(wire_1_0_frame_specs).

-export([frames/1]).

-define(PROFILE, pq_pure).
-define(NOW_MS, 1791800000000).

%% frames(Keys) -> [{FrameTypeName :: binary(), Frame :: map()}]
-spec frames(#{{atom(), atom()} => macula_node_keys:node_key()}) -> [{binary(), map()}].
frames(Keys) ->
    Client = maps:get({pq_pure, client_identity}, Keys),
    Station = maps:get({pq_pure, station_identity}, Keys),
    {ok, StationId} = macula_node_keys:node_id(Station),
    Record = record_bytes(Station, StationId),
    Request = verified(macula_frame:call(request_spec(StationId, 1), Client)),
    Open = verified(macula_frame:stream_open((request_spec(StationId, 2))#{mode => bidi}, Client)),
    Publish = macula_frame:publish(publish_spec(), Client),
    #{publication := Publication} = Publish,
    [{<<"goodbye">>, macula_frame:goodbye(normal, <<"bye">>)},
     {<<"swim_ping">>, macula_frame:swim_ping(#{round => 1, incarnation => 0, piggyback => [swim_update()]})},
     {<<"swim_ack">>, macula_frame:swim_ack(#{round => 1, responder => id256(2), incarnation => 0})},
     {<<"swim_suspect">>, macula_frame:swim_suspect(suspect_spec())},
     {<<"swim_confirm">>, macula_frame:swim_confirm(suspect_spec())},
     {<<"swim_ping_req">>, macula_frame:swim_ping_req(#{round => 1, target => id256(3)})},
     {<<"ping">>, macula_frame:ping(#{nonce => id128(1)})},
     {<<"pong">>, macula_frame:pong(#{nonce => id128(1)})},
     {<<"liveness_ping">>, macula_frame:liveness_ping(#{nonce => id128(2)})},
     {<<"liveness_pong">>, macula_frame:liveness_pong(#{nonce => id128(2)})},
     {<<"find_node">>, macula_frame:find_node(#{key => id256(4), origin => id256(1), depth => 0})},
     {<<"nodes">>, macula_frame:nodes(#{key => id256(4), nodes => [station_ref()]})},
     {<<"find_value">>, macula_frame:find_value(#{key => id256(4), origin => id256(1)})},
     {<<"value">>, macula_frame:value(#{key => id256(4), records => [Record]})},
     {<<"store">>, macula_frame:store(#{record => Record})},
     {<<"store_ack">>, macula_frame:store_ack(#{key => id256(4), signer => StationId, record_version => id128(3),
                                                stored => true})},
     {<<"call">>, macula_frame:call(request_spec(StationId, 1), Client)},
     {<<"result">>, macula_frame:result(#{request => Request, payload => #{<<"n">> => 1}}, Station)},
     {<<"error">>, macula_frame:provider_error(#{request => Request, code => <<"closed">>,
                                                 detail => <<"example detail">>}, Station)},
     {<<"hyparview_join">>, macula_frame:hyparview_join(#{realm => realm(), new_member => id256(5)})},
     {<<"hyparview_forward_join">>,
      macula_frame:hyparview_forward_join(#{realm => realm(), new_member => id256(5), ttl => 3, arwl => 3,
                                            prwl => 2})},
     {<<"hyparview_neighbor">>, macula_frame:hyparview_neighbor(#{realm => realm(), priority => high})},
     {<<"hyparview_disconnect">>, macula_frame:hyparview_disconnect(#{realm => realm()})},
     {<<"hyparview_shuffle">>,
      macula_frame:hyparview_shuffle(#{realm => realm(), origin => id256(1), ttl => 2,
                                       peer_sample => [id256(6), id256(7)]})},
     {<<"hyparview_shuffle_reply">>,
      macula_frame:hyparview_shuffle_reply(#{realm => realm(), peer_sample => [id256(8)]})},
     {<<"plumtree_gossip">>, macula_frame:plumtree_gossip(#{publication => Publication, round => 0})},
     {<<"plumtree_ihave">>, macula_frame:plumtree_ihave(#{realm => realm(), msg_id => msg_id(), round => 0})},
     {<<"plumtree_graft">>, macula_frame:plumtree_graft(#{realm => realm(), msg_id => msg_id(), round => 0})},
     {<<"plumtree_prune">>, macula_frame:plumtree_prune(#{realm => realm()})},
     {<<"overlay_relay">>,
      macula_frame:overlay_relay(#{peer => id256(9),
                                   payload => macula_frame:encode(macula_frame:plumtree_prune(#{realm => realm()}))})},
     {<<"publish">>, Publish},
     {<<"subscribe">>, macula_frame:subscribe(#{topic => <<"example.topic">>, realm => realm(),
                                                subscriber => id256(1)})},
     {<<"unsubscribe">>, macula_frame:unsubscribe(#{topic => <<"example.topic">>, realm => realm(),
                                                    subscriber => id256(1)})},
     {<<"event">>, macula_frame:event(#{publication => Publication, delivered_via => direct})},
     {<<"advertise">>, macula_frame:advertise(#{advertisement => Record})},
     {<<"unadvertise">>, macula_frame:unadvertise(#{withdrawal => Record})},
     {<<"stream_open">>, macula_frame:stream_open((request_spec(StationId, 2))#{mode => bidi}, Client)},
     {<<"stream_data">>,
      macula_frame:provider_stream(#{frame_type => stream_data, seq => 0, encoding => raw, body => <<"chunk">>},
                                   Station, Open)},
     {<<"stream_end">>, macula_frame:caller_stream(#{frame_type => stream_end, seq => 0, role => send}, Client, Open)},
     {<<"stream_error">>,
      macula_frame:provider_stream(#{frame_type => stream_error, seq => 1, code => <<"stopped">>,
                                     message => <<"no more">>}, Station, Open)},
     {<<"stream_reply">>,
      macula_frame:provider_stream(#{frame_type => stream_reply, seq => 2, payload => #{<<"n">> => 1}}, Station, Open)},
     {<<"want">>, macula_frame:want(#{blocks => [#{mcid => mcid(), priority => 1}]})},
     {<<"have">>, macula_frame:have(#{blocks => [#{mcid => mcid(), size => 5}]})},
     {<<"block">>, macula_frame:block(#{mcid => mcid(), payload => <<"block">>})},
     {<<"manifest_req">>, macula_frame:manifest_req(#{mcid => mcid()})},
     {<<"manifest_res">>, macula_frame:manifest_res(#{mcid => mcid(), manifest => #{<<"size">> => 5}})},
     {<<"cancel">>, macula_frame:cancel(#{blocks => [mcid()]})}].

%% The verified request of a CALL or STREAM_OPEN as a provider holds it: the frame over the wire, then verified.
verified(Frame) ->
    {ok, Received, <<>>} = macula_frame:decode(macula_frame:encode(Frame)),
    {ok, Request} = macula_frame:verify_request(Received, ?PROFILE),
    Request.

request_spec(Target, N) ->
    #{request_id => id128(16#10 + N), realm => realm(), procedure => <<"example.procedure">>, target => Target,
      deadline => ?NOW_MS + 60000, payload => #{<<"n">> => 1}}.

publish_spec() ->
    #{realm => realm(), topic => <<"example.topic">>, seq => 0, published_at => ?NOW_MS,
      payload => #{<<"n">> => 1}}.

record_bytes(Station, StationId) ->
    macula_record:encode(macula_record:sign(macula_record:node_record(StationId, [], 0), Station)).

swim_update() ->
    macula_frame:swim_update(#{target => id256(3), state => alive, incarnation => 0, observed_at => ?NOW_MS,
                               by => id256(1)}).

suspect_spec() ->
    #{target => id256(3), target_incarnation => 0, suspected_by => id256(1), ttl => 3}.

station_ref() ->
    macula_frame:station_ref(#{node_id => id256(10), station_id => id256(11), tier => 2, country => <<"BE">>,
                               last_seen_at => ?NOW_MS,
                               addresses => [#{host => <<"2001:db8::1">>, port => 4433, transport => quic}]}).

realm() -> <<16#AA:256>>.

id256(N) -> <<N:256>>.

id128(N) -> <<N:128>>.

%% A plumtree message id: 48 bytes.
msg_id() -> <<1:384>>.

%% An MCID: tag 2 (SHA-384), codec 0x55 (raw), a 48-byte hash.
mcid() -> <<2, 16#55, 1:384>>.
