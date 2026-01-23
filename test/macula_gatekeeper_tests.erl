%% @doc Unit tests for macula_gatekeeper module.
%%
%% Tests cover:
%% - Certificate validation
%% - Operation validation (capability checks)
%% - Callback verification
%% - Fingerprint calculation
-module(macula_gatekeeper_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% A valid X.509 certificate from docker/certs/cert.pem
test_certificate() ->
    <<"-----BEGIN CERTIFICATE-----
MIIGPDCCBCSgAwIBAgIUdk2k1Y07AZ3iniPJ6T5gCkiRwHswDQYJKoZIhvcNAQEL
BQAwZDELMAkGA1UEBhMCVVMxDjAMBgNVBAgMBVN0YXRlMQ0wCwYDVQQHDARDaXR5
MQ8wDQYDVQQKDAZNYWN1bGExDTALBgNVBAsMBFRlc3QxFjAUBgNVBAMMDSoubWFj
dWxhLnRlc3QwHhcNMjUxMTE2MTgzMTE1WhcNMjYxMTE2MTgzMTE1WjBkMQswCQYD
VQQGEwJVUzEOMAwGA1UECAwFU3RhdGUxDTALBgNVBAcMBENpdHkxDzANBgNVBAoM
Bk1hY3VsYTENMAsGA1UECwwEVGVzdDEWMBQGA1UEAwwNKi5tYWN1bGEudGVzdDCC
AiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoCggIBAMcINMBcfASffzCehQWhO3my
CPoKHBZpibhzUJ9sW8Ya85By0t4Q2t5GDjpgjwrPdu4KWZPQT8Jgu8cuzuookM9J
VpYlJ4iiZ53iNAJLzuM+ZT/lK2sJaZkGZyTGBCn7KNgIRUUFTDHHBKycoU8E8jn2
oXy0bd5BHPw2y8aqmZ1+ISqcSWRph2LYaQMGQzwdMyvvxsOlyXFU6nEe23wNZUT3
NzP82bqt3TXHoe9GDxnlGFdakJUw1uyxcXEXydbxuHWNDsAwF45RwgnkK4ckbAZ7
fE1F7hESoS8Y7Sr0GceU07YuKPDT3dIPTSfI904AHNTC4nd7W1qEeOEKzyfyBRyw
XyArb/Zdmt40B0I8LUWWCqNq115YJUWNq4SNvt/gREIH+tITukDW3vbpZ0LmCDwo
7iJpPcHkKX0C6kX1s8YqHR7Zoi4AptxDlpSIb6pipsbNRLaY3v4TKDCaWtzjF9mS
vvmUif05tBwUcPBzu3oAWCfl0iZje/lc4fRivLDXG6lcoUpsPLuhv2DAeQWZVGW2
5mEwtK+7JavUuFuK7yMZaqhomNU3VXYMowRM+dV7LRYFHW6eP9+/CYUOL73p8m9+
ZsoeaJXAl2ioC4vHm7t9ka9IrOSI8zQ/dX/TdJXciextysP+nKmievQI+Q5laRC3
ofx29CLA4jctukUXAa0tAgMBAAGjgeUwgeIwDAYDVR0TBAUwAwEB/zAOBgNVHQ8B
Af8EBAMCAqQwHQYDVR0lBBYwFAYIKwYBBQUHAwEGCCsGAQUFBwMCMIGDBgNVHREE
fDB6gg0qLm1hY3VsYS50ZXN0gg4qLmFyY2FkZS5sb2NhbIIOYXJjYWRlLWdhdGV3
YXmCDGFyY2FkZS1wZWVyMYIMYXJjYWRlLXBlZXIyggxhcmNhZGUtcGVlcjOCFHJl
Z2lzdHJ5Lm1hY3VsYS50ZXN0gglsb2NhbGhvc3QwHQYDVR0OBBYEFLz7Ciw9Y8JY
EXF5Ka8diVya0lkDMA0GCSqGSIb3DQEBCwUAA4ICAQB1qrXoGVZPAsHOHSabBJdO
3b+2Qks6APiaHSRtP6NM/ydw/M0FrhMY855zhbsbgffMJjAR4aeCW2YJJfFEzl6B
0DrFPJbr9FkvPmo42o3LsxCEA3Tpfn2B3Gt/YxwKSePifJew/CKq9dmZ/fy9LpUr
Gu56kP9HS+ZqqV1ZpLAUYKclSK6NGhESJE/+uyuAoeCGxseYO0I5TXZokO0sR11i
grUVo+Yx2lssUFWh2FMlndrQZsF7sLKDFd+pAyfoMZt/CD4SCJWJFyg5t+teRs5d
OTeCaARVjKurji8dZqFMIQpLBbUfMNszcAg8RO/hwLAHXyFamSngyEKx0CkGVTRD
tEEjR78qkIKfvYg5HswHDMcip0VEdaoBnQ8KMKN5GOE/gvZpWCWwyKjWF1hFe7Yu
lQkpPxBuey33/rE13IET1L15kzu5IyiXngY0/8190i3gzP1FUt7TPXqruqT9GX6z
UKO6f8GEwQSFpzuEkwD7MsGAfPZtQ25tr7Ssei0VocY55GD4pLgpEUv0o7qQXGAh
vVzl4/p8dxwtMs1NuYjAB4kl52UpYsmrVvzZGkZTFDb0tDUKSVpzJQEy49xCITFb
6yIpy//2C2Vy/kH89nP3wN9dj01iqY4y5spcNcQp0qN2EgYThPT325fy+l75nTLf
HfxL42IlPllSoSjdKBGH1A==
-----END CERTIFICATE-----">>.

%%====================================================================
%% Certificate Validation Tests
%%====================================================================

verify_certificate_valid_pem_test() ->
    CertPem = test_certificate(),
    Identity = <<"io.macula.test.app">>,
    %% Should return fingerprint (cert parsing works)
    Result = macula_gatekeeper:verify_certificate(CertPem, Identity),
    ?assertMatch({ok, _Fingerprint}, Result).

verify_certificate_returns_fingerprint_test() ->
    CertPem = test_certificate(),
    Identity = <<"io.macula.test.app">>,
    {ok, Fingerprint} = macula_gatekeeper:verify_certificate(CertPem, Identity),
    %% Fingerprint should be 64 hex chars (SHA-256 = 32 bytes = 64 hex)
    ?assertEqual(64, byte_size(Fingerprint)).

verify_certificate_fingerprint_is_deterministic_test() ->
    CertPem = test_certificate(),
    Identity = <<"io.macula.test.app">>,
    {ok, Fingerprint1} = macula_gatekeeper:verify_certificate(CertPem, Identity),
    {ok, Fingerprint2} = macula_gatekeeper:verify_certificate(CertPem, Identity),
    ?assertEqual(Fingerprint1, Fingerprint2).

verify_certificate_invalid_pem_test() ->
    InvalidCert = <<"not a valid certificate">>,
    Identity = <<"io.macula.test.app">>,
    Result = macula_gatekeeper:verify_certificate(InvalidCert, Identity),
    ?assertMatch({error, {certificate_invalid, _}}, Result).

verify_certificate_empty_pem_test() ->
    EmptyCert = <<"-----BEGIN CERTIFICATE-----
-----END CERTIFICATE-----">>,
    Identity = <<"io.macula.test.app">>,
    Result = macula_gatekeeper:verify_certificate(EmptyCert, Identity),
    ?assertMatch({error, {certificate_invalid, _}}, Result).

verify_certificate_not_binary_test() ->
    Result = macula_gatekeeper:verify_certificate("not binary", <<"id">>),
    ?assertEqual({error, {certificate_invalid, not_binary}}, Result).

%%====================================================================
%% Operation Validation Tests
%%====================================================================

validate_operation_publish_allowed_test() ->
    Manifest = #{capabilities => [publish, subscribe]},
    ?assertEqual(ok,
                 macula_gatekeeper:validate_operation(Manifest, publish, <<"topic">>)).

validate_operation_subscribe_allowed_test() ->
    Manifest = #{capabilities => [publish, subscribe]},
    ?assertEqual(ok,
                 macula_gatekeeper:validate_operation(Manifest, subscribe, <<"topic">>)).

validate_operation_call_allowed_test() ->
    Manifest = #{capabilities => [call]},
    ?assertEqual(ok,
                 macula_gatekeeper:validate_operation(Manifest, call, <<"proc">>)).

validate_operation_register_allowed_test() ->
    Manifest = #{capabilities => [register]},
    ?assertEqual(ok,
                 macula_gatekeeper:validate_operation(Manifest, register, <<"proc">>)).

validate_operation_provide_content_allowed_test() ->
    Manifest = #{capabilities => [provide_content]},
    ?assertEqual(ok,
                 macula_gatekeeper:validate_operation(Manifest, provide_content, <<"mcid">>)).

validate_operation_consume_content_allowed_test() ->
    Manifest = #{capabilities => [consume_content]},
    ?assertEqual(ok,
                 macula_gatekeeper:validate_operation(Manifest, consume_content, <<"mcid">>)).

validate_operation_publish_not_allowed_test() ->
    Manifest = #{capabilities => [subscribe]},  % No publish
    ?assertEqual({error, {capability_not_declared, publish}},
                 macula_gatekeeper:validate_operation(Manifest, publish, <<"topic">>)).

validate_operation_call_not_allowed_test() ->
    Manifest = #{capabilities => [publish, subscribe]},  % No call
    ?assertEqual({error, {capability_not_declared, call}},
                 macula_gatekeeper:validate_operation(Manifest, call, <<"proc">>)).

validate_operation_unknown_operation_test() ->
    Manifest = #{capabilities => [publish]},
    ?assertEqual({error, {unknown_operation, delete}},
                 macula_gatekeeper:validate_operation(Manifest, delete, <<"resource">>)).

validate_operation_empty_capabilities_test() ->
    Manifest = #{capabilities => []},
    ?assertEqual({error, {capability_not_declared, publish}},
                 macula_gatekeeper:validate_operation(Manifest, publish, <<"topic">>)).

%%====================================================================
%% Callback Verification Tests
%%====================================================================

verify_callbacks_missing_all_test() ->
    %% A module that exists but doesn't implement macula_protocol
    %% Using 'lists' as example - it exists but doesn't have our callbacks
    Result = macula_gatekeeper:verify_callbacks(lists),
    ?assertMatch({error, {missing_callbacks, _}}, Result).

verify_callbacks_checks_all_required_test() ->
    %% Verify the error lists all missing callbacks
    {error, {missing_callbacks, Missing}} = macula_gatekeeper:verify_callbacks(lists),
    RequiredCallbacks = [
        {mesh_identity, 0},
        {mesh_capabilities, 0},
        {mesh_api, 0},
        {handle_mesh_event, 2},
        {handle_rpc_call, 2},
        {provide_content, 1},
        {content_received, 2},
        {mesh_health, 0}
    ],
    %% All required callbacks should be in the missing list
    ?assertEqual(RequiredCallbacks, Missing).

%%====================================================================
%% Health Check Tests
%%====================================================================

check_health_returns_ok_test() ->
    Manifest = #{identity => <<"io.macula.test.app">>},
    ?assertEqual(ok, macula_gatekeeper:check_health(Manifest)).

%%====================================================================
%% External App Verification Tests
%%====================================================================

verify_external_app_not_implemented_test() ->
    Result = macula_gatekeeper:verify_external_app("http://localhost:8080", <<"cert">>),
    ?assertMatch({error, {not_implemented, external_verification}}, Result).
