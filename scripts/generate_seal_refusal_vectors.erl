%% Writes test/vectors/e2e_seal_v1_refusals.json: a keyed provider's `sealed_required' refusal of a CALL and of a
%% STREAM_OPEN, in both crypto profiles, each carrying the KEM key the provider holds as the non-critical signed
%% extension `seal.kem_key' (DESIGN_E2E_SEALED_BY_DEFAULT.md section 3, release A0). Every frame is signed for the very
%% request beside it and carries its signer's key, so a reader needs nothing beyond the bytes and its own profile: it
%% verifies the request, verifies the refusal against it, and reads the key. test/vectors/E2E_SEAL_V1.md says what an
%% SDK must do with them. Run through scripts/generate-seal-refusal-vectors.sh with the compiled tree on the code path.
%%
%% ML-DSA-87 signing is hedged and pq_hybrid's RSA-PSS half is randomized, so a second run signs other bytes with the
%% same shape: the committed file is the vector. macula_seal_refusal_vectors_tests re-reads every key from it on each
%% run.
{ok, _} = application:ensure_all_started(crypto),
Hex = fun(Bin) -> binary:encode_hex(Bin, lowercase) end,
Realm = crypto:hash(sha256, <<"io.macula">>),
Procedure = <<"acme/sealed_echo_v1">>,
Now = erlang:system_time(millisecond),
%% One profile's four frames: the CALL and its refusal, the STREAM_OPEN and its refusal, and the key each refusal
%% carries.
Frames = fun(Profile) ->
    {ok, Caller} = macula_node_keys:generate(identity, Profile),
    {ok, Provider} = macula_node_keys:generate(identity, Profile),
    {ok, ProviderId} = macula_node_keys:node_id(Provider),
    {KemPublic, _KemPrivate} = macula_seal:generate_key(Profile),
    KemKey = macula_seal:key_as_carried(KemPublic),
    Spec = #{request_id => crypto:strong_rand_bytes(16), realm => Realm, procedure => Procedure,
             target => ProviderId, deadline => Now + 60_000, payload => #{<<"n">> => 1}},
    Call = macula_frame:call(Spec, Caller),
    {ok, Request} = macula_frame:verify_request(Call, Profile),
    Error = macula_frame:provider_error(#{request => Request, code => <<"sealed_required">>,
                                          ext => #{<<"seal.kem_key">> => #{critical => 0, value => KemKey}}},
                                        Provider),
    OpenSpec = Spec#{mode => bidi},
    {ok, OpenBytes} = macula_frame:stream_bytes({stream_open, OpenSpec}, Caller),
    {ok, OpenFrame, <<>>} = macula_frame:decode(macula_frame:written_bytes(OpenBytes)),
    {ok, Open} = macula_frame:verify_request(OpenFrame, Profile),
    StreamError = macula_frame:provider_stream(#{frame_type => stream_error, seq => 0, code => <<"sealed_required">>,
                                                 message => <<>>,
                                                 ext => #{<<"seal.kem_key">> => #{critical => 0, value => KemKey}}},
                                               Provider, Open),
    #{<<"call">> => Hex(macula_frame:encode(Call)),
      <<"refusal">> => Hex(macula_frame:encode(Error)),
      <<"stream_open">> => Hex(macula_frame:written_bytes(OpenBytes)),
      <<"stream_refusal">> => Hex(macula_frame:encode(StreamError)),
      <<"kem_key">> => Hex(KemKey),
      <<"kem_key_size">> => byte_size(KemKey),
      <<"kem_key_id">> => Hex(macula_seal:key_id(KemKey)),
      <<"provider_node_id">> => Hex(ProviderId),
      <<"extension">> => macula_frame:kem_key_extension()}
end,
Doc = #{<<"design">> => <<"docs/design/DESIGN_E2E_SEALED_BY_DEFAULT.md, section 3, release A0">>,
        <<"generator">> => <<"scripts/generate-seal-refusal-vectors.sh">>,
        <<"realm">> => Hex(Realm),
        <<"procedure">> => Procedure,
        <<"deadline_ms">> => Now + 60_000,
        <<"profiles">> => #{<<"pq_pure">> => Frames(pq_pure), <<"pq_hybrid">> => Frames(pq_hybrid)}},
Path = "test/vectors/e2e_seal_v1_refusals.json",
ok = file:write_file(Path, [json:encode(Doc), $\n]),
io:format("wrote ~s~n", [Path]),
halt(0).
