#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%% Generate TLS test certificates using Erlang's public_key application
%% This creates properly formatted X509 certificates for QUIC/TLS testing

-mode(compile).

main([OutputDir]) ->
    io:format("Generating TLS test certificates in: ~s~n", [OutputDir]),

    %% Ensure output directory exists
    filelib:ensure_dir(filename:join(OutputDir, "dummy")),

    %% Define certificate configuration for server
    ServerConfig = #{
        key => {rsa, 4096},
        digest => sha256,
        validity => {-1, 365}, %% Valid from yesterday to 1 year from now
        extensions => [
            #{ ?'id-ce-keyUsage' => [digitalSignature, keyEncipherment] },
            #{ ?'id-ce-extKeyUsage' => [?'id-kp-serverAuth', ?'id-kp-clientAuth'] },
            #{ ?'id-ce-subjectAltName' =>
               [{dNSName, "*.arcade.local"},
                {dNSName, "arcade-gateway"},
                {dNSName, "arcade-peer1"},
                {dNSName, "arcade-peer2"},
                {dNSName, "arcade-peer3"},
                {dNSName, "localhost"}]
            }
        ]
    },

    %% Generate test data (creates CA + server cert chain)
    #{server := ServerData} = public_key:pkix_test_data(#{server_chain => ServerConfig}),

    %% Extract certificate and private key
    #{cert := ServerCert, key := ServerKey} = ServerData,

    %% Encode to PEM format
    CertPEM = public_key:pem_encode([{'Certificate', ServerCert, not_encrypted}]),
    KeyPEM = public_key:pem_encode([encode_key(ServerKey)]),

    %% Write to files
    CertFile = filename:join(OutputDir, "cert.pem"),
    KeyFile = filename:join(OutputDir, "key.pem"),

    ok = file:write_file(CertFile, CertPEM),
    ok = file:write_file(KeyFile, KeyPEM),

    io:format("✅ Certificates generated successfully:~n"),
    io:format("   Certificate: ~s~n", [CertFile]),
    io:format("   Private Key: ~s~n", [KeyFile]),

    %% Verify certificate info
    print_cert_info(ServerCert),

    halt(0);

main(_) ->
    io:format("Usage: generate_test_certs.erl <output_directory>~n"),
    io:format("Example: ./scripts/generate_test_certs.erl ./certs~n"),
    halt(1).

%% Helper to encode private key
encode_key({Type, KeyData}) when Type == rsa; Type == dsa; Type == 'ECPrivateKey' ->
    {atom_to_list(Type) ++ "PrivateKey", KeyData, not_encrypted};
encode_key({ed_pri, Algo, _Pub, Priv}) ->
    {'PrivateKeyInfo', #'PrivateKeyInfo'{
        version = 0,
        privateKeyAlgorithm = #'PrivateKeyInfo_privateKeyAlgorithm'{
            algorithm = pubkey_cert_records:supportedPublicKeyAlgorithms(Algo)
        },
        privateKey = Priv
    }, not_encrypted}.

%% Print certificate information for verification
print_cert_info(CertDER) ->
    Cert = public_key:pkix_decode_cert(CertDER, otp),
    TBSCert = Cert#'OTPCertificate'.tbsCertificate,
    Subject = TBSCert#'OTPTBSCertificate'.subject,
    Validity = TBSCert#'OTPTBSCertificate'.validity,
    Extensions = TBSCert#'OTPTBSCertificate'.extensions,

    io:format("~nCertificate Details:~n"),
    io:format("  Subject: ~p~n", [Subject]),
    io:format("  Validity: ~p~n", [Validity]),
    io:format("  Extensions: ~p~n", [length(Extensions)]),

    %% Find SubjectAltName extension
    case lists:keyfind(?'id-ce-subjectAltName', #'Extension'.extnID, Extensions) of
        #'Extension'{extnValue = SANValue} ->
            io:format("  SubjectAltName: ~p~n", [SANValue]);
        false ->
            io:format("  ⚠️  WARNING: No SubjectAltName extension found!~n")
    end,
    ok.
