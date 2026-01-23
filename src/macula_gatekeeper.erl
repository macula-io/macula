%% @doc Gatekeeper module for validating mesh application admissions.
%%
%% == Overview ==
%%
%% The gatekeeper validates that applications are "mesh-worthy" before allowing
%% them to participate in the Macula mesh network. Validation happens at:
%% - Session establishment (initial admission)
%% - Periodically during session (health checks)
%% - On each operation (capability enforcement)
%%
%% == Validation Layers ==
%%
%% 1. Protocol Compliance: App implements `macula_protocol' behaviour
%% 2. Identity Verification: Identity matches presented certificate
%% 3. Certificate Validation: Certificate is valid, not expired, not revoked
%% 4. Capability Declaration: App declares its required capabilities
%% 5. Health Status: App responds to health checks
%%
%% == BEAM vs Non-BEAM Apps ==
%%
%% For BEAM apps (Erlang/Elixir):
%% - Use `verify_beam_app/2' which checks `code:ensure_loaded/1'
%% - Validates behaviour callbacks via module introspection
%%
%% For non-BEAM apps (via sidecar or gRPC):
%% - Use `verify_external_app/2' which probes HTTP/gRPC endpoints
%% - Requires macula sidecar or compatible protocol implementation
%%
%% @see macula_protocol
%% @see macula_authorization
%% @author Macula Team
%% @end
-module(macula_gatekeeper).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type identity() :: binary().
-type certificate() :: binary().  % PEM-encoded
-type validation_result() :: {ok, app_manifest()} | {error, validation_error()}.

-type app_manifest() :: #{
    identity := identity(),
    capabilities := [macula_protocol:capability()],
    api := macula_protocol:api_spec(),
    certificate_fingerprint := binary(),
    verified_at := calendar:datetime()
}.

-type validation_error() ::
    no_macula_sdk |
    not_macula_app |
    behaviour_not_implemented |
    identity_mismatch |
    certificate_invalid |
    certificate_expired |
    certificate_revoked |
    health_check_failed |
    {validation_exception, term()}.

-export_type([app_manifest/0, validation_result/0, validation_error/0]).

%%====================================================================
%% API
%%====================================================================

-export([
    verify_beam_app/2,
    verify_beam_app/3,
    verify_external_app/2,
    verify_certificate/2,
    check_health/1,
    validate_operation/3,
    %% Exported for RPC calls from remote nodes
    verify_callbacks/1
]).

%%====================================================================
%% BEAM App Verification
%%====================================================================

%% @doc Verifies a BEAM app is mesh-worthy.
-spec verify_beam_app(module(), certificate()) -> validation_result().
verify_beam_app(Module, CertPem) ->
    verify_beam_app(Module, CertPem, node()).

-spec verify_beam_app(module(), certificate(), node()) -> validation_result().
verify_beam_app(Module, CertPem, Node) ->
    ?LOG_DEBUG("Gatekeeper: verifying BEAM app ~p on node ~p", [Module, Node]),
    Result = run_verification_pipeline(Module, CertPem, Node),
    audit_verification(Module, Node, Result),
    Result.

%% Pipeline approach - each step returns ok or error, chain with andalso-like logic
run_verification_pipeline(Module, CertPem, Node) ->
    with_step(ensure_protocol_loaded(Node), fun() ->
    with_step(check_behaviour_implementation(Module, Node), fun() ->
    with_step(get_and_validate_identity(Module, CertPem, Node), fun({Identity, Fingerprint}) ->
    with_step(get_capabilities_and_api(Module, Node), fun({Caps, Api}) ->
    with_step(check_app_health(Module, Node), fun() ->
        {ok, #{
            identity => Identity,
            capabilities => Caps,
            api => Api,
            certificate_fingerprint => Fingerprint,
            verified_at => calendar:universal_time()
        }}
    end)end)end)end)end).

%% Helper for pipeline - continues on ok, stops on error
with_step({ok, Value}, Next) -> Next(Value);
with_step(ok, Next) -> Next();
with_step({error, _} = Err, _Next) -> Err.

%%====================================================================
%% External App Verification
%%====================================================================

%% @doc Verifies a non-BEAM app via HTTP/gRPC probes.
-spec verify_external_app(uri_string:uri_string(), certificate()) -> validation_result().
verify_external_app(Endpoint, _CertPem) ->
    ?LOG_WARNING("External app verification not yet implemented for ~s", [Endpoint]),
    {error, {not_implemented, external_verification}}.

%%====================================================================
%% Certificate Verification
%%====================================================================

%% @doc Verifies a certificate and extracts identity.
-spec verify_certificate(certificate(), identity()) ->
    {ok, binary()} | {error, validation_error()}.
verify_certificate(CertPem, ExpectedIdentity) when is_binary(CertPem) ->
    case decode_certificate(CertPem) of
        {ok, Cert} ->
            verify_certificate_chain(Cert, CertPem, ExpectedIdentity);
        {error, Reason} ->
            {error, Reason}
    end;
verify_certificate(_, _) ->
    {error, {certificate_invalid, not_binary}}.

verify_certificate_chain(Cert, CertPem, ExpectedIdentity) ->
    case check_certificate_expiry(Cert) of
        ok ->
            verify_certificate_identity(Cert, CertPem, ExpectedIdentity);
        {error, _} = Err ->
            Err
    end.

verify_certificate_identity(_Cert, CertPem, _ExpectedIdentity) ->
    %% For now, skip identity extraction from cert (complex ASN.1 parsing)
    %% In production, extract CN and compare to ExpectedIdentity
    Fingerprint = calculate_fingerprint(CertPem),
    {ok, Fingerprint}.

%%====================================================================
%% Health Checks
%%====================================================================

%% @doc Performs a health check on a verified app.
-spec check_health(app_manifest()) -> ok | {error, term()}.
check_health(#{identity := Identity}) ->
    ?LOG_DEBUG("Health check for ~s", [Identity]),
    ok.

%%====================================================================
%% Operation Validation
%%====================================================================

%% @doc Validates that an app can perform an operation.
-spec validate_operation(app_manifest(), atom(), binary()) -> ok | {error, term()}.
validate_operation(#{capabilities := Caps}, Operation, _Resource) ->
    validate_capability(Caps, Operation).

validate_capability(Caps, Operation) when
    Operation =:= publish;
    Operation =:= subscribe;
    Operation =:= call;
    Operation =:= register;
    Operation =:= provide_content;
    Operation =:= consume_content ->
    case lists:member(Operation, Caps) of
        true -> ok;
        false -> {error, {capability_not_declared, Operation}}
    end;
validate_capability(_, Operation) ->
    {error, {unknown_operation, Operation}}.

%%====================================================================
%% Internal: Protocol Loading
%%====================================================================

ensure_protocol_loaded(Node) when Node =:= node() ->
    ensure_module_loaded(macula_protocol);
ensure_protocol_loaded(Node) ->
    rpc_ensure_loaded(Node, macula_protocol).

ensure_module_loaded(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} -> ok;
        _ -> {error, no_macula_sdk}
    end.

rpc_ensure_loaded(Node, Module) ->
    case rpc:call(Node, code, ensure_loaded, [Module]) of
        {module, Module} -> ok;
        {badrpc, Reason} -> {error, {rpc_failed, Reason}};
        _ -> {error, no_macula_sdk}
    end.

%%====================================================================
%% Internal: Behaviour Checking
%%====================================================================

check_behaviour_implementation(Module, Node) when Node =:= node() ->
    case code:ensure_loaded(Module) of
        {module, Module} -> verify_callbacks(Module);
        _ -> {error, {module_not_found, Module}}
    end;
check_behaviour_implementation(Module, Node) ->
    case rpc:call(Node, code, ensure_loaded, [Module]) of
        {module, Module} -> rpc_verify_callbacks(Node, Module);
        {badrpc, Reason} -> {error, {rpc_failed, Reason}};
        _ -> {error, {module_not_found, Module}}
    end.

rpc_verify_callbacks(Node, Module) ->
    case rpc:call(Node, ?MODULE, verify_callbacks, [Module]) of
        ok -> ok;
        {error, _} = Err -> Err;
        {badrpc, Reason} -> {error, {rpc_failed, Reason}}
    end.

-spec verify_callbacks(module()) -> ok | {error, term()}.
verify_callbacks(Module) ->
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
    Exports = Module:module_info(exports),
    Missing = [CB || CB <- RequiredCallbacks, not lists:member(CB, Exports)],
    case Missing of
        [] -> ok;
        _ -> {error, {missing_callbacks, Missing}}
    end.

%%====================================================================
%% Internal: Identity Validation
%%====================================================================

get_and_validate_identity(Module, CertPem, Node) when Node =:= node() ->
    Identity = Module:mesh_identity(),
    validate_identity_with_cert(Identity, CertPem);
get_and_validate_identity(Module, CertPem, Node) ->
    case rpc:call(Node, Module, mesh_identity, []) of
        Identity when is_binary(Identity) ->
            validate_identity_with_cert(Identity, CertPem);
        {badrpc, Reason} ->
            {error, {rpc_failed, Reason}}
    end.

validate_identity_with_cert(Identity, CertPem) ->
    case verify_certificate(CertPem, Identity) of
        {ok, Fingerprint} -> {ok, {Identity, Fingerprint}};
        {error, _} = Err -> Err
    end.

%%====================================================================
%% Internal: Capabilities and API
%%====================================================================

get_capabilities_and_api(Module, Node) when Node =:= node() ->
    Caps = Module:mesh_capabilities(),
    Api = Module:mesh_api(),
    validate_caps_and_api(Caps, Api);
get_capabilities_and_api(Module, Node) ->
    get_caps_and_api_remote(Module, Node).

get_caps_and_api_remote(Module, Node) ->
    case rpc:call(Node, Module, mesh_capabilities, []) of
        Caps when is_list(Caps) ->
            case rpc:call(Node, Module, mesh_api, []) of
                Api when is_map(Api) ->
                    validate_caps_and_api(Caps, Api);
                {badrpc, Reason} ->
                    {error, {rpc_failed, Reason}}
            end;
        {badrpc, Reason} ->
            {error, {rpc_failed, Reason}}
    end.

validate_caps_and_api(Caps, Api) ->
    case macula_protocol:validate_capabilities(Caps) of
        ok ->
            case macula_protocol:validate_api_spec(Api) of
                ok -> {ok, {Caps, Api}};
                {error, _} = Err -> Err
            end;
        {error, _} = Err ->
            Err
    end.

%%====================================================================
%% Internal: Health Check
%%====================================================================

check_app_health(Module, Node) when Node =:= node() ->
    Module:mesh_health();
check_app_health(Module, Node) ->
    case rpc:call(Node, Module, mesh_health, []) of
        ok -> ok;
        {error, _} = Err -> Err;
        {badrpc, Reason} -> {error, {rpc_failed, Reason}}
    end.

%%====================================================================
%% Internal: Certificate Utilities
%%====================================================================

decode_certificate(CertPem) ->
    case public_key:pem_decode(CertPem) of
        [{'Certificate', DerCert, _}] ->
            decode_der_certificate(DerCert);
        [] ->
            {error, {certificate_invalid, empty_pem}};
        _ ->
            {error, {certificate_invalid, decode_failed}}
    end.

decode_der_certificate(<<>>) ->
    {error, {certificate_invalid, empty_der}};
decode_der_certificate(DerCert) when byte_size(DerCert) < 50 ->
    %% Valid X.509 certs are at least ~200 bytes; 50 is generous minimum
    {error, {certificate_invalid, invalid_der}};
decode_der_certificate(<<16#30, _/binary>> = DerCert) ->
    %% Starts with SEQUENCE tag (0x30) - valid DER structure
    {ok, public_key:pkix_decode_cert(DerCert, otp)};
decode_der_certificate(_DerCert) ->
    %% Does not start with SEQUENCE tag - not a valid certificate
    {error, {certificate_invalid, invalid_der_structure}}.

check_certificate_expiry(_Cert) ->
    %% TODO: Extract validity dates and check against current time
    ok.

calculate_fingerprint(CertPem) ->
    Hash = crypto:hash(sha256, CertPem),
    list_to_binary([io_lib:format("~2.16.0B", [B]) || <<B>> <= Hash]).

%%====================================================================
%% Internal: Audit Logging
%%====================================================================

audit_verification(Module, Node, {ok, Manifest}) ->
    ?LOG_INFO("Gatekeeper: ADMITTED app=~p node=~p identity=~s caps=~p",
              [Module, Node, maps:get(identity, Manifest), maps:get(capabilities, Manifest)]);
audit_verification(Module, Node, {error, Reason}) ->
    ?LOG_WARNING("Gatekeeper: REJECTED app=~p node=~p reason=~p",
                 [Module, Node, Reason]).
