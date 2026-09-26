%% Frames whose payload is sealed (E2E seal scheme 1), signed exactly as
%% macula_frame's builders sign their clear counterparts. No builder writes
%% `sealed' yet, so a test that needs one to arrive builds it here.
-module(macula_sealed_frames).

-export([request/3, result/2, publication/4, provider_stream/4, request_sealed/0, nonce_sealed/0]).

%% A sealed CALL or STREAM_OPEN from `Caller', as `Spec' (request_id, realm,
%% procedure, target, deadline, and mode for a STREAM_OPEN) describes it.
request(Type, #{request_id := RequestId, realm := Realm, procedure := Procedure, target := Target,
                deadline := Deadline} = Spec, Caller) ->
    Mode = [{<<"mode">>, {text, atom_to_binary(M)}} || M <- [maps:get(mode, Spec, none)], M =/= none],
    Tbs = wire([{<<"frame_type">>, {text, atom_to_binary(Type)}}, {<<"caller">>, macula_node_keys:key_id(Caller)},
                {<<"request_id">>, RequestId}, {<<"realm">>, Realm}, {<<"procedure">>, {text, Procedure}},
                {<<"target">>, Target}, {<<"deadline">>, Deadline}, {<<"sealed">>, request_sealed()} | Mode]),
    #{version => 2, frame_type => Type,
      request => macula_signed_object:sign(<<"MACULA-PQ-REQUEST-V1">>, Tbs, Caller)}.

%% A sealed RESULT for a verified request, from `Provider'.
result(#{request_id := RequestId, request_hash := RequestHash}, Provider) ->
    Tbs = wire([{<<"frame_type">>, {text, <<"result">>}}, {<<"request_id">>, RequestId},
                {<<"request_hash">>, RequestHash}, {<<"responded_by">>, macula_node_keys:key_id(Provider)},
                {<<"sealed">>, nonce_sealed()}]),
    #{version => 2, frame_type => result, reply => macula_signed_object:sign(<<"MACULA-PQ-REPLY-V1">>, Tbs, Provider)}.

%% A sealed publication from `Publisher', as the EVENT that carries it.
publication(Publisher, Realm, Topic, Seq) ->
    Tbs = wire([{<<"publisher">>, macula_node_keys:key_id(Publisher)}, {<<"realm">>, Realm},
                {<<"topic">>, {text, Topic}}, {<<"seq">>, Seq},
                {<<"published_at">>, erlang:system_time(millisecond)}, {<<"sealed">>, nonce_sealed()}]),
    Publication = macula_signed_object:sign(<<"MACULA-PQ-PUBLICATION-V1">>, Tbs, Publisher),
    macula_frame:event(#{publication => Publication, delivered_via => direct}).

%% A provider's first stream frame, sealed, answering a verified STREAM_OPEN.
provider_stream(Type, Provider, #{request_id := RequestId, request_hash := RequestHash}, Extra) ->
    Tbs = wire([{<<"frame_type">>, {text, atom_to_binary(Type)}}, {<<"request_id">>, RequestId},
                {<<"request_hash">>, RequestHash}, {<<"signer">>, macula_node_keys:key_id(Provider)},
                {<<"seq">>, 0}, {<<"sealed">>, nonce_sealed()} | Extra]),
    #{version => 2, frame_type => Type, stream => macula_signed_object:sign(<<"MACULA-PQ-STREAM-V1">>, Tbs, Provider)}.

request_sealed() ->
    #{{text, <<"scheme">>} => 1, {text, <<"key_id">>} => <<1:64>>, {text, <<"kem_ct">>} => <<2:12544>>,
      {text, <<"ct">>} => <<"ciphertext">>}.

nonce_sealed() ->
    #{{text, <<"scheme">>} => 1, {text, <<"key_id">>} => <<1:64>>, {text, <<"nonce">>} => <<3:96>>,
      {text, <<"ct">>} => <<"ciphertext">>}.

wire(Fields) ->
    maps:from_list([{{text, K}, V} || {K, V} <- Fields]).
