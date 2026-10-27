%% The refusal vectors (test/vectors/E2E_SEAL_V1.md, release A0): a keyed provider's `sealed_required' refusal of a
%% CALL and of a STREAM_OPEN, in both crypto profiles, each carrying the key the provider holds as the non-critical
%% signed extension `seal.kem_key'. Every frame in the file is signed for the request beside it and carries its
%% signer's key, so this module reads each refusal the way any SDK must - verify the request, verify the refusal
%% against it, read the key - and pins the key it names. The file is committed as generated, since signing is
%% randomized; re-deriving every key from it on each run is what keeps it from drifting from macula_frame.
-module(macula_seal_refusal_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

vectors_test_() ->
    Doc = vectors(),
    Profiles = maps:get(<<"profiles">>, Doc),
    [{"both profiles", ?_assertEqual([<<"pq_hybrid">>, <<"pq_pure">>], lists:sort(maps:keys(Profiles)))}]
    ++ [{binary_to_list(Name) ++ " refusal of a CALL",
         fun() -> call_checked(Doc, profile(Name), P) end}
        || Name := P <- Profiles]
    ++ [{binary_to_list(Name) ++ " refusal of a STREAM_OPEN",
         fun() -> stream_checked(Doc, profile(Name), P) end}
        || Name := P <- Profiles].

%% A profile by its name in the file, never through binary_to_existing_atom: whether that atom exists yet depends on
%% which modules happen to have loaded.
profile(<<"pq_pure">>) -> pq_pure;
profile(<<"pq_hybrid">>) -> pq_hybrid.

%% The CALL verifies, its refusal verifies against it, and the key read off the refusal is the one the file pins, of
%% that profile's size and with that id.
call_checked(_Doc, Profile, #{<<"call">> := Call, <<"refusal">> := Refusal} = P) ->
    Request = verified_request(Call, Profile),
    {ok, Fields} = macula_frame:verify_reply(decode(Refusal), Request, Profile),
    key_checked(Fields, Profile, P).

%% A STREAM_OPEN and the seq-0 STREAM_ERROR answering it, read as the open's caller reads them.
stream_checked(_Doc, Profile, #{<<"stream_open">> := Open, <<"stream_refusal">> := Refusal} = P) ->
    OpenFields = verified_request(Open, Profile),
    ?assertEqual(stream_open, maps:get(frame_type, OpenFields)),
    {ok, Fields, _Verifier} =
        macula_frame:verify_provider_stream(decode(Refusal), macula_frame:open_stream(OpenFields), Profile),
    ?assertEqual(0, maps:get(seq, Fields)),
    key_checked(Fields, Profile, P).

%% The refusal names the key the file pins: as carried, of that profile's size, with that id, signed by the provider
%% the file names.
key_checked(Fields, Profile, #{<<"kem_key">> := KeyHex, <<"kem_key_size">> := Size, <<"kem_key_id">> := KeyId} = P) ->
    ?assertEqual(<<"sealed_required">>, maps:get(code, Fields)),
    KemKey = binary:decode_hex(KeyHex),
    ?assertEqual({ok, KemKey}, macula_frame:refused_kem_key(Fields)),
    ?assertEqual(Size, byte_size(KemKey)),
    ?assertEqual(KeyId, hex(macula_seal:key_id(KemKey))),
    ?assertMatch({ok, Profile, _Public}, macula_seal:public_key(KemKey)),
    ?assertEqual(maps:get(<<"provider_node_id">>, P), hex(signed_by(Fields))).

%% A reply names the node that signed it `responded_by', a stream frame `signer'.
signed_by(#{responded_by := By}) -> By;
signed_by(#{signer := By}) -> By.

verified_request(HexFrame, Profile) ->
    {ok, Request} = macula_frame:verify_request(decode(HexFrame), Profile),
    Request.

decode(HexFrame) ->
    {ok, Frame, <<>>} = macula_frame:decode(binary:decode_hex(HexFrame)),
    Frame.

hex(Bin) -> binary:encode_hex(Bin, lowercase).

vectors() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file() ->
    Name = "e2e_seal_v1_refusals.json",
    first_existing([filename:join("test/vectors", Name), filename:join("../../test/vectors", Name)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Name])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    case filelib:is_regular(F) of
        true -> F;
        false -> first_existing(Rest)
    end;
first_existing([]) ->
    error(no_vector_file).
