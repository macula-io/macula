%% A sealed call, one level above macula_seal: the caller seals a request to
%% the provider's KEM key and opens the reply, and the provider opens the
%% request and seals the reply (design §5.1). The recipient's side is checked
%% against the E2E seal scheme 1 vectors byte for byte; the sender's side,
%% whose encapsulation is random, by a round trip.
-module(macula_sealed_call_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Against the vectors
%%------------------------------------------------------------------

%% The provider opens each vector's request with the recipient's private key,
%% and gets its plaintext and the call's keys.
the_provider_opens_the_vector_request_test_() ->
    [{name(C), fun() ->
          Profile = profile(C),
          Holder = holder(Profile),
          {ok, Plain, Keys} = macula_sealed_call:open_request(Profile, Holder, request(C), request_sealed(C)),
          ?assertEqual(x(maps:get(<<"request">>, C), <<"plain">>), Plain),
          ?assertEqual(expected_keys(C), Keys)
      end} || C <- calls()].

%% A STREAM_OPEN's keys are its request key and the two stream keys, and no
%% reply key: a stream's later frames seal under the key for their
%% direction, so a stream's keys cannot seal a reply.
a_stream_opens_keys_cannot_seal_a_reply_test() ->
    Request = (base_request(pq_pure))#{frame_type := <<"stream_open">>},
    {Sealed, Keys} = macula_sealed_call:seal_request(pq_pure, public(pq_pure), Request, <<"open">>),
    ?assertEqual([k_c2p, k_p2c, k_req, key_id], lists:sort(maps:keys(Keys))),
    ?assertMatch({ok, <<"open">>, Keys}, macula_sealed_call:open_request(pq_pure, holder(pq_pure), Request, Sealed)),
    Reply = #{frame_type => <<"result">>, request_hash => <<7:384>>, responded_by => maps:get(target, Request)},
    ?assertError(function_clause, macula_sealed_call:seal_reply(Keys, Request, Reply, <<"no">>)).

expected_keys(#{<<"frame_type">> := <<"stream_open">>} = C) ->
    #{k_req => x(C, <<"k_req">>), k_c2p => x(C, <<"k_c2p">>), k_p2c => x(C, <<"k_p2c">>), key_id => x(C, <<"key_id">>)};
expected_keys(C) ->
    #{k_req => x(C, <<"k_req">>), k_rep => x(C, <<"k_rep">>), key_id => x(C, <<"key_id">>)}.

%% The caller opens each vector's reply with the call's reply key. A
%% stream_open's vector has stream frames instead, which the stream package
%% checks.
the_caller_opens_the_vector_reply_test_() ->
    [{name(C), fun() ->
          Reply = maps:get(<<"reply">>, C),
          ?assertEqual({ok, x(Reply, <<"plain">>)},
                       macula_sealed_call:open_reply(#{k_req => x(C, <<"k_req">>), k_rep => x(C, <<"k_rep">>),
                                                       key_id => x(C, <<"key_id">>)}, request(C), reply(Reply),
                                                     reply_sealed(C)))
      end} || C <- calls(), is_map_key(<<"reply">>, C)].

%% The caller opens each call vector's sealed ERROR, and its plaintext reads
%% back as the code and detail beside it.
the_caller_opens_the_vector_error_test_() ->
    [{name(C), fun() ->
          Error = maps:get(<<"error_reply">>, C),
          {ok, Plain} = macula_sealed_call:open_reply(#{k_req => x(C, <<"k_req">>), k_rep => x(C, <<"k_rep">>),
                                                        key_id => x(C, <<"key_id">>)}, request(C), reply(Error),
                                                      #{scheme => 1, key_id => x(C, <<"key_id">>),
                                                        nonce => x(Error, <<"nonce">>), ct => x(Error, <<"ct">>)}),
          ?assertEqual(x(Error, <<"plain">>), Plain),
          ?assertEqual({ok, #{code => maps:get(<<"code">>, Error), detail => maps:get(<<"detail">>, Error)}},
                       macula_frame:plain_error(Plain)),
          ?assertEqual({ok, Plain}, macula_frame:error_plain(#{code => maps:get(<<"code">>, Error),
                                                                detail => maps:get(<<"detail">>, Error)}))
      end} || C <- calls(), is_map_key(<<"error_reply">>, C)].

%%------------------------------------------------------------------
%% Round trips, both profiles
%%------------------------------------------------------------------

a_sealed_call_round_trips_test_() ->
    [{atom_to_list(Profile), fun() ->
          Request = base_request(Profile),
          {Sealed, CallerKeys} = macula_sealed_call:seal_request(Profile, public(Profile), Request, <<"ping">>),
          ?assertEqual(key_id(Profile), maps:get(key_id, Sealed)),
          ?assertEqual(1, maps:get(scheme, Sealed)),
          ?assertNot(is_map_key(nonce, Sealed)),
          {ok, <<"ping">>, ProviderKeys} = macula_sealed_call:open_request(Profile, holder(Profile), Request, Sealed),
          ?assertEqual(CallerKeys, ProviderKeys),

          Reply = #{frame_type => <<"result">>, request_hash => <<7:384>>, responded_by => maps:get(target, Request)},
          SealedReply = macula_sealed_call:seal_reply(ProviderKeys, Request, Reply, <<"pong">>),
          ?assertEqual(12, byte_size(maps:get(nonce, SealedReply))),
          ?assertEqual({ok, <<"pong">>}, macula_sealed_call:open_reply(CallerKeys, Request, Reply, SealedReply))
      end} || Profile <- [pq_pure, pq_hybrid]].

%% Two replies under one reply key never share a nonce.
each_reply_has_a_fresh_nonce_test() ->
    Request = base_request(pq_pure),
    {Sealed, _} = macula_sealed_call:seal_request(pq_pure, public(pq_pure), Request, <<"ping">>),
    {ok, _, Keys} = macula_sealed_call:open_request(pq_pure, holder(pq_pure), Request, Sealed),
    Reply = #{frame_type => <<"result">>, request_hash => <<7:384>>, responded_by => maps:get(target, Request)},
    #{nonce := N1} = macula_sealed_call:seal_reply(Keys, Request, Reply, <<"a">>),
    #{nonce := N2} = macula_sealed_call:seal_reply(Keys, Request, Reply, <<"a">>),
    ?assertNotEqual(N1, N2).

%%------------------------------------------------------------------
%% Refusals
%%------------------------------------------------------------------

%% A request sealed to a key the provider does not hold is refused, naming
%% the key the provider holds now, so the caller can seal again to it.
a_key_the_provider_does_not_hold_is_refused_naming_its_current_key_test() ->
    Request = base_request(pq_pure),
    {Sealed, _} = macula_sealed_call:seal_request(pq_pure, public(pq_pure), Request, <<"ping">>),
    Rotated = fun(_KeyId) -> error end,
    Holder = #{lookup => Rotated, current_key_id => <<9:64>>},
    ?assertEqual({error, {sealed_refused, <<9:64>>}},
                 macula_sealed_call:open_request(pq_pure, Holder, Request, Sealed)).

%% A sealed payload copied into another caller's request does not open: the
%% AAD binds it to the caller, the request_id and every routing field (T3).
a_sealed_payload_in_another_request_is_refused_test_() ->
    Request = base_request(pq_pure),
    {Sealed, _} = macula_sealed_call:seal_request(pq_pure, public(pq_pure), Request, <<"ping">>),
    Current = key_id(pq_pure),
    [{atom_to_list(Field), ?_assertEqual({error, {sealed_refused, Current}},
                                         macula_sealed_call:open_request(pq_pure, holder(pq_pure),
                                                                         Request#{Field := Other}, Sealed))}
     || {Field, Other} <- [{caller, <<1:256>>}, {request_id, <<2:128>>}, {procedure, <<"acme/other_v1">>},
                           {deadline, 1}, {realm, <<3:256>>}, {frame_type, <<"stream_open">>}]].

%% A RESULT's sealed payload does not open as an ERROR's, nor under another
%% request hash or provider.
a_sealed_reply_opens_only_as_itself_test_() ->
    Request = base_request(pq_pure),
    {Sealed, CallerKeys} = macula_sealed_call:seal_request(pq_pure, public(pq_pure), Request, <<"ping">>),
    {ok, _, Keys} = macula_sealed_call:open_request(pq_pure, holder(pq_pure), Request, Sealed),
    Reply = #{frame_type => <<"result">>, request_hash => <<7:384>>, responded_by => maps:get(target, Request)},
    SealedReply = macula_sealed_call:seal_reply(Keys, Request, Reply, <<"pong">>),
    [{atom_to_list(Field), ?_assertEqual({error, sealed_refused},
                                         macula_sealed_call:open_reply(CallerKeys, Request, Reply#{Field := Other},
                                                                       SealedReply))}
     || {Field, Other} <- [{frame_type, <<"error">>}, {request_hash, <<8:384>>}, {responded_by, <<4:256>>}]]
    ++ [{"another key id", ?_assertEqual({error, sealed_refused},
                                         macula_sealed_call:open_reply(CallerKeys, Request, Reply,
                                                                       SealedReply#{key_id := <<1:64>>}))}].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

base_request(_Profile) ->
    #{frame_type => <<"call">>, realm => <<5:256>>, procedure => <<"acme/count_v1">>, caller => <<6:256>>,
      target => <<7:256>>, request_id => <<8:128>>, deadline => 1790000000000}.

%% What a provider holding the vectors' recipient key answers a lookup with.
holder(Profile) ->
    R = recipient(Profile),
    Carried = x(R, <<"key_as_carried">>),
    KeyId = x(R, <<"key_id">>),
    Private = private(Profile, R),
    #{lookup => fun(Id) when Id =:= KeyId -> {ok, Private, Carried};
                   (_Other) -> error
                end,
      current_key_id => KeyId}.

public(pq_pure) -> #{mlkem_ek => x(recipient(pq_pure), <<"mlkem_ek">>)};
public(pq_hybrid) -> R = recipient(pq_hybrid), #{mlkem_ek => x(R, <<"mlkem_ek">>), p384_pub => x(R, <<"p384_pub">>)}.

private(pq_pure, R) -> #{mlkem_dk => x(R, <<"mlkem_dk">>)};
private(pq_hybrid, R) -> #{mlkem_dk => x(R, <<"mlkem_dk">>), p384_priv => x(R, <<"p384_priv">>)}.

key_id(Profile) -> x(recipient(Profile), <<"key_id">>).

request(C) ->
    #{frame_type => maps:get(<<"frame_type">>, C), realm => x(C, <<"realm">>), procedure => maps:get(<<"procedure">>, C),
      caller => x(C, <<"caller">>), target => x(C, <<"target">>), request_id => x(C, <<"request_id">>),
      deadline => maps:get(<<"deadline">>, C)}.

request_sealed(C) ->
    #{scheme => 1, key_id => x(C, <<"key_id">>), kem_ct => x(C, <<"kem_ct">>),
      ct => x(maps:get(<<"request">>, C), <<"ct">>)}.

reply(Reply) ->
    #{frame_type => maps:get(<<"frame_type">>, Reply), request_hash => x(Reply, <<"request_hash">>),
      responded_by => x(Reply, <<"responded_by">>)}.

reply_sealed(C) ->
    Reply = maps:get(<<"reply">>, C),
    #{scheme => 1, key_id => x(C, <<"key_id">>), nonce => x(Reply, <<"nonce">>), ct => x(Reply, <<"ct">>)}.

calls() -> maps:get(<<"calls">>, vectors()).

recipient(Profile) -> maps:get(atom_to_binary(Profile), maps:get(<<"recipients">>, vectors())).

profile(C) -> binary_to_existing_atom(maps:get(<<"profile">>, C)).

name(C) -> binary_to_list(<<(maps:get(<<"frame_type">>, C))/binary, " ", (maps:get(<<"profile">>, C))/binary>>).

x(Map, Key) -> binary:decode_hex(maps:get(Key, Map)).

vectors() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or
%% from the build tree's copy of the application.
vector_file() ->
    hd([F || F <- ["test/vectors/e2e_seal_v1.json", "../../test/vectors/e2e_seal_v1.json"]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", "e2e_seal_v1.json"])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)],
             filelib:is_regular(F)]).
