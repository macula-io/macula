%% A signed tbs may carry `sealed' in place of its payload field (E2E seal
%% scheme 1, test/vectors/E2E_SEAL_V1.md): a request, a reply, a stream frame
%% and a publication verify with it, so a station verifies and routes a sealed
%% frame as it does any other and never charges it. `sealed' is never beside the
%% payload it replaces, and its shape is the one its frame allows: a request's
%% carries a `kem_ct' and no nonce; a reply's and an event's a nonce and no
%% `kem_ct'; a stream frame's no `kem_ct'.
-module(macula_frame_sealed_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<7:256>>).

sealed_test_() ->
    {setup, fun keys/0, fun(Keys) -> [{Name, fun() -> Case(Keys) end} || {Name, Case} <- cases()] end}.

cases() ->
    [{"a sealed CALL verifies, and names what it seals", fun a_sealed_call_verifies/1},
     {"a sealed STREAM_OPEN verifies", fun a_sealed_stream_open_verifies/1},
     {"a request with both payload and sealed is malformed", fun a_request_with_both_is_malformed/1},
     {"a request's sealed carries a kem_ct and no nonce", fun a_request_sealed_has_its_shape/1},
     {"a sealed RESULT and a sealed ERROR verify", fun sealed_replies_verify/1},
     {"a reply with both, or a sealed without its nonce, is malformed", fun a_reply_of_another_shape_is_malformed/1},
     {"a sealed publication verifies as PUBLISH and as EVENT", fun a_sealed_publication_verifies/1},
     {"a publication with both is malformed", fun a_publication_with_both_is_malformed/1},
     {"sealed provider stream frames verify, a sealed STREAM_END does not", fun sealed_stream_frames_verify/1},
     {"a sealed of another scheme or shape is malformed", fun a_sealed_of_another_scheme_is_malformed/1},
     {"a caller's sealed stream frame derives its nonce, a provider's carries one",
      fun a_stream_frames_sealed_nonce_follows_its_side/1},
     {"a sealed publication verifies as GOSSIP", fun a_sealed_publication_verifies_as_gossip/1}].

a_sealed_call_verifies(#{caller := Caller} = Keys) ->
    {ok, Verified} = macula_frame:verify_request(request(call, Keys, #{<<"sealed">> => request_sealed()}), profile()),
    ?assertEqual(#{scheme => 1, key_id => <<1:64>>, kem_ct => <<2:12544>>, ct => <<"ciphertext">>},
                 maps:get(sealed, Verified)),
    ?assertNot(is_map_key(payload, Verified)),
    ?assertEqual(macula_node_keys:key_id(Caller), maps:get(caller, Verified)).

a_sealed_stream_open_verifies(Keys) ->
    Fields = #{<<"sealed">> => request_sealed(), <<"mode">> => {text, <<"server_stream">>}},
    ?assertMatch({ok, #{sealed := #{kem_ct := _}, mode := server_stream}},
                 macula_frame:verify_request(request(stream_open, Keys, Fields), profile())).

a_request_with_both_is_malformed(Keys) ->
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_request(request(call, Keys, #{<<"sealed">> => request_sealed(),
                                                                    <<"payload">> => #{}}), profile())),
    ?assertEqual({error, malformed_frame}, macula_frame:verify_request(request(call, Keys, #{}), profile())).

a_request_sealed_has_its_shape(Keys) ->
    [?assertEqual({error, malformed_frame},
                  macula_frame:verify_request(request(call, Keys, #{<<"sealed">> => Sealed}), profile()))
     || Sealed <- [maps:remove({text, <<"kem_ct">>}, request_sealed()),
                   (request_sealed())#{{text, <<"nonce">>} => <<0:96>>}]].

sealed_replies_verify(Keys) ->
    Request = verified_request(Keys),
    [?assertMatch({ok, #{sealed := #{nonce := <<3:96>>}}},
                  macula_frame:verify_reply(reply(Type, Keys, Request, #{<<"sealed">> => nonce_sealed()}), Request,
                                            profile()))
     || Type <- [result, error]].

a_reply_of_another_shape_is_malformed(Keys) ->
    Request = verified_request(Keys),
    Bad = [{result, #{<<"sealed">> => nonce_sealed(), <<"payload">> => #{}}},
           {error, #{<<"sealed">> => nonce_sealed(), <<"code">> => {text, <<"oops">>}}},
           {result, #{<<"sealed">> => maps:remove({text, <<"nonce">>}, nonce_sealed())}},
           {result, #{<<"sealed">> => (nonce_sealed())#{{text, <<"kem_ct">>} => <<2:12544>>}}}],
    [?assertEqual({error, malformed_frame},
                  macula_frame:verify_reply(reply(Type, Keys, Request, Fields), Request, profile()))
     || {Type, Fields} <- Bad].

a_sealed_publication_verifies(Keys) ->
    Publish = publication(Keys, #{<<"sealed">> => nonce_sealed()}),
    {ok, Verified} = macula_frame:verify_publication(Publish, profile(), now_ms()),
    ?assertEqual(<<3:96>>, maps:get(nonce, maps:get(sealed, Verified))),
    Event = macula_frame:event(#{publication => maps:get(publication, Publish), delivered_via => direct}),
    ?assertMatch({ok, #{sealed := _}}, macula_frame:verify_publication(Event, profile(), now_ms())).

a_publication_with_both_is_malformed(Keys) ->
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_publication(publication(Keys, #{<<"sealed">> => nonce_sealed(),
                                                                     <<"payload">> => #{}}), profile(), now_ms())).

sealed_stream_frames_verify(#{provider := Provider} = Keys) ->
    Open = verified_request(Keys, stream_open),
    Sealed = #{<<"sealed">> => nonce_sealed()},
    Frames = [stream_frame(stream_data, 0, Sealed#{<<"encoding">> => {text, <<"raw">>}}, Provider, Open),
              stream_frame(stream_reply, 1, Sealed, Provider, Open)],
    {Read, _State} = lists:mapfoldl(fun(F, St) ->
                                        {ok, Fields, Next} = macula_frame:verify_provider_stream(F, St, profile()),
                                        {maps:get(frame_type, Fields), Next}
                                    end, macula_frame:open_stream(Open), Frames),
    ?assertEqual([stream_data, stream_reply], Read),
    ErrorFrame = stream_frame(stream_error, 0, Sealed, Provider, Open),
    ?assertMatch({ok, #{sealed := _}, _},
                 macula_frame:verify_provider_stream(ErrorFrame, macula_frame:open_stream(Open), profile())),
    EndFrame = stream_frame(stream_end, 0, Sealed#{<<"role">> => {text, <<"both">>}}, Provider, Open),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_provider_stream(EndFrame, macula_frame:open_stream(Open), profile())).

a_sealed_of_another_scheme_is_malformed(Keys) ->
    [?assertEqual({error, malformed_frame},
                  macula_frame:verify_request(request(call, Keys, #{<<"sealed">> => Sealed}), profile()))
     || Sealed <- [(request_sealed())#{{text, <<"scheme">>} => 2},
                   (request_sealed())#{{text, <<"kem_ct">>} => <<2:8>>},
                   (request_sealed())#{{text, <<"key_id">>} => <<1:56>>},
                   maps:remove({text, <<"ct">>}, request_sealed()),
                   (request_sealed())#{{text, <<"extra">>} => 1},
                   <<"not a map">>]].

%% A caller's frame derives its nonce from its seq and carries none; a
%% provider's carries one (E2E_SEAL_V1.md). Each side's verifier holds it so.
a_stream_frames_sealed_nonce_follows_its_side(#{caller := Caller, provider := Provider} = Keys) ->
    {Open, OpenFrame} = bidi_open(Keys),
    CallerFrame = fun(Sealed) -> caller_frame(Caller, Open, Sealed) end,
    ?assertMatch({ok, #{sealed := _}, _},
                 macula_frame:verify_caller_stream(CallerFrame(maps:remove({text, <<"nonce">>}, nonce_sealed())),
                                                   macula_frame:open_stream(Open), profile())),
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_caller_stream(CallerFrame(nonce_sealed()), macula_frame:open_stream(Open),
                                                   profile())),
    NoNonce = #{<<"sealed">> => maps:remove({text, <<"nonce">>}, nonce_sealed()),
                <<"encoding">> => {text, <<"raw">>}},
    ?assertEqual({error, malformed_frame},
                 macula_frame:verify_provider_stream(stream_frame(stream_data, 0, NoNonce, Provider, Open),
                                                     macula_frame:open_stream(Open), profile())),
    _ = OpenFrame.

a_sealed_publication_verifies_as_gossip(Keys) ->
    #{publication := Publication} = publication(Keys, #{<<"sealed">> => nonce_sealed()}),
    Gossip = #{version => 2, frame_type => plumtree_gossip, publication => Publication, round => 0},
    ?assertMatch({ok, #{sealed := _}}, macula_frame:verify_publication(Gossip, profile(), now_ms())).

bidi_open(Keys) ->
    Fields = #{<<"sealed">> => request_sealed(), <<"mode">> => {text, <<"bidi">>}},
    Frame = request(stream_open, Keys, Fields),
    {ok, Open} = macula_frame:verify_request(Frame, profile()),
    {Open, Frame}.

caller_frame(Caller, #{request_id := RequestId, request_hash := RequestHash}, Sealed) ->
    Tbs = wire(#{<<"frame_type">> => {text, <<"stream_data">>}, <<"request_id">> => RequestId,
                 <<"request_hash">> => RequestHash, <<"signer">> => macula_node_keys:key_id(Caller), <<"seq">> => 0,
                 <<"encoding">> => {text, <<"raw">>}, <<"sealed">> => Sealed}),
    #{version => 2, frame_type => stream_data,
      caller_stream => macula_signed_object:sign_held(<<"MACULA-PQ-CALLER-STREAM-V1">>, Tbs, Caller)}.

%%%===================================================================
%%% Helpers: tbs built on the wire and signed as the builders sign them
%%%===================================================================

keys() ->
    Generate = fun() -> {ok, Key} = macula_node_keys:generate(identity, profile()), Key end,
    #{caller => Generate(), provider => Generate()}.

profile() -> pq_pure.

now_ms() -> erlang:system_time(millisecond).

request_sealed() ->
    #{{text, <<"scheme">>} => 1, {text, <<"key_id">>} => <<1:64>>, {text, <<"kem_ct">>} => <<2:12544>>,
      {text, <<"ct">>} => <<"ciphertext">>}.

nonce_sealed() ->
    #{{text, <<"scheme">>} => 1, {text, <<"key_id">>} => <<1:64>>, {text, <<"nonce">>} => <<3:96>>,
      {text, <<"ct">>} => <<"ciphertext">>}.

wire(Fields) ->
    maps:fold(fun(K, V, Acc) -> Acc#{{text, K} => V} end, #{}, Fields).

request(Type, #{caller := Caller, provider := Provider}, Extra) ->
    Tbs = wire(maps:merge(#{<<"frame_type">> => {text, atom_to_binary(Type)},
                            <<"caller">> => macula_node_keys:key_id(Caller),
                            <<"request_id">> => <<9:128>>, <<"realm">> => ?REALM,
                            <<"procedure">> => {text, <<"acme/count_v1">>},
                            <<"target">> => macula_node_keys:key_id(Provider),
                            <<"deadline">> => now_ms() + 60_000}, Extra)),
    #{version => 2, frame_type => Type,
      request => macula_signed_object:sign(<<"MACULA-PQ-REQUEST-V1">>, Tbs, Caller)}.

verified_request(Keys) -> verified_request(Keys, call).

verified_request(Keys, call) ->
    {ok, R} = macula_frame:verify_request(request(call, Keys, #{<<"sealed">> => request_sealed()}), profile()),
    R;
verified_request(Keys, stream_open) ->
    Fields = #{<<"sealed">> => request_sealed(), <<"mode">> => {text, <<"server_stream">>}},
    {ok, R} = macula_frame:verify_request(request(stream_open, Keys, Fields), profile()),
    R.

reply(Type, #{provider := Provider}, #{request_id := RequestId, request_hash := RequestHash}, Extra) ->
    Tbs = wire(maps:merge(#{<<"frame_type">> => {text, atom_to_binary(Type)}, <<"request_id">> => RequestId,
                            <<"request_hash">> => RequestHash,
                            <<"responded_by">> => macula_node_keys:key_id(Provider)}, Extra)),
    #{version => 2, frame_type => Type,
      reply => macula_signed_object:sign(<<"MACULA-PQ-REPLY-V1">>, Tbs, Provider)}.

publication(#{caller := Publisher}, Extra) ->
    Tbs = wire(maps:merge(#{<<"publisher">> => macula_node_keys:key_id(Publisher), <<"realm">> => ?REALM,
                            <<"topic">> => {text, <<"acme/news">>}, <<"seq">> => 1,
                            <<"published_at">> => now_ms()}, Extra)),
    #{version => 2, frame_type => publish,
      publication => macula_signed_object:sign(<<"MACULA-PQ-PUBLICATION-V1">>, Tbs, Publisher)}.

stream_frame(Type, Seq, Extra, Provider, #{request_id := RequestId, request_hash := RequestHash}) ->
    Tbs = wire(maps:merge(#{<<"frame_type">> => {text, atom_to_binary(Type)}, <<"request_id">> => RequestId,
                            <<"request_hash">> => RequestHash, <<"signer">> => macula_node_keys:key_id(Provider),
                            <<"seq">> => Seq}, Extra)),
    Signed = case Seq of
                 0 -> macula_signed_object:sign(<<"MACULA-PQ-STREAM-V1">>, Tbs, Provider);
                 _ -> macula_signed_object:sign_held(<<"MACULA-PQ-STREAM-V1">>, Tbs, Provider)
             end,
    #{version => 2, frame_type => Type, stream => Signed}.
