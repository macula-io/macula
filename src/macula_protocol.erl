%% @doc Behaviour defining the protocol that Macula mesh applications must implement.
%%
%% == Overview ==
%%
%% This behaviour defines the contract that applications must implement to be
%% considered "mesh-worthy" and allowed to participate in the Macula mesh network.
%% The gatekeeper validates that apps implement this protocol before granting
%% mesh access.
%%
%% == Why This Matters ==
%%
%% Not every container or process should be allowed to join the mesh. This protocol
%% ensures that:
%% - Apps have proper identity (backed by certificates)
%% - Apps declare their capabilities upfront
%% - Apps can receive mesh events
%% - Apps can be health-checked during sessions
%%
%% == Implementation ==
%%
%% BEAM apps implement this as a behaviour. The implementation must export:
%% - mesh_identity/0 - Returns the app's identity binary
%% - mesh_capabilities/0 - Returns list of capabilities
%% - mesh_api/0 - Returns map of topics, procedures, content_types
%% - handle_mesh_event/2 - Handles incoming events
%% - mesh_health/0 - Returns ok or error tuple
%%
%% Non-BEAM apps implement equivalent gRPC or HTTP endpoints that the gatekeeper
%% can probe.
%%
%% == Security Model ==
%%
%% 1. Identity must match the certificate's subject
%% 2. Capabilities are validated against licenses at operation time
%% 3. Health checks run periodically during sessions
%% 4. Failed health checks may result in session termination
%%
%% @see macula_gatekeeper
%% @author Macula Team
%% @end
-module(macula_protocol).

%%====================================================================
%% Types
%%====================================================================

-type identity() :: binary().
%% App identity, e.g., "io.macula.rgfaber.my-app"

-type capability() ::
    publish |           % Can publish messages to topics
    subscribe |         % Can subscribe to topics
    call |              % Can make RPC calls
    register |          % Can register RPC handlers
    provide_content |   % Can provide content (files/artifacts)
    consume_content.    % Can consume content (files/artifacts)
%% What the app can do on the mesh

-type api_spec() :: #{
    topics => [binary()],          % PubSub topics provided
    procedures => [binary()],      % RPC procedures provided
    content_types => [binary()]    % Content types provided (e.g., "application/wasm")
}.
%% API declaration: topics, procedures, and content types this app provides

-type health_status() :: ok | {error, term()}.
%% Health check result

%%====================================================================
%% Behaviour Callbacks
%%====================================================================

%% Returns the app's mesh identity.
%% Must match the certificate's subject: {realm}.{organization}.{app-name}
-callback mesh_identity() -> identity().

%% Returns the capabilities this app requires.
%% Capabilities: publish, subscribe, call, register, provide_content, consume_content
-callback mesh_capabilities() -> [capability()].

%% Returns the API specification (topics, procedures, content_types).
-callback mesh_api() -> api_spec().

%% Handles incoming pub/sub events.
-callback handle_mesh_event(Topic :: binary(), Payload :: term()) -> ok | {error, term()}.

%% Handles incoming RPC calls.
-callback handle_rpc_call(Procedure :: binary(), Args :: list()) ->
    {ok, term()} | {error, term()}.

%% Provides content by content ID.
-callback provide_content(ContentId :: binary()) ->
    {ok, binary()} | {error, term()}.

%% Called when content has been received.
-callback content_received(ContentId :: binary(), Content :: binary()) ->
    ok | {error, term()}.

%% Health check callback. Return ok or {error, Reason}.
-callback mesh_health() -> health_status().

%%====================================================================
%% Exports
%%====================================================================

-export_type([identity/0, capability/0, api_spec/0, health_status/0]).

%% Utility functions for protocol validation
-export([
    validate_identity/1,
    validate_capabilities/1,
    validate_api_spec/1,
    is_valid_capability/1
]).

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Validates that an identity is well-formed.
%%
%% Valid identities:
%% - Are non-empty binaries
%% - Contain at least 3 dot-separated segments
%% - Each segment contains only alphanumeric characters and hyphens
-spec validate_identity(identity()) -> ok | {error, term()}.
validate_identity(Identity) when is_binary(Identity), byte_size(Identity) > 0 ->
    Parts = binary:split(Identity, <<".">>, [global]),
    case length(Parts) >= 3 of
        true ->
            case lists:all(fun is_valid_segment/1, Parts) of
                true -> ok;
                false -> {error, invalid_segment}
            end;
        false ->
            {error, insufficient_segments}
    end;
validate_identity(_) ->
    {error, invalid_identity}.

%% @doc Validates that capabilities list is valid.
-spec validate_capabilities([capability()]) -> ok | {error, term()}.
validate_capabilities(Caps) when is_list(Caps) ->
    case lists:all(fun is_valid_capability/1, Caps) of
        true -> ok;
        false -> {error, invalid_capability}
    end;
validate_capabilities(_) ->
    {error, invalid_capabilities_format}.

%% @doc Validates that an API spec is well-formed.
-spec validate_api_spec(api_spec()) -> ok | {error, term()}.
validate_api_spec(Spec) when is_map(Spec) ->
    Topics = maps:get(topics, Spec, []),
    Procs = maps:get(procedures, Spec, []),
    ContentTypes = maps:get(content_types, Spec, []),
    TopicsValid = is_list(Topics) andalso lists:all(fun is_binary/1, Topics),
    ProcsValid = is_list(Procs) andalso lists:all(fun is_binary/1, Procs),
    ContentTypesValid = is_list(ContentTypes) andalso lists:all(fun is_binary/1, ContentTypes),
    case TopicsValid andalso ProcsValid andalso ContentTypesValid of
        true -> ok;
        false -> {error, invalid_api_entries}
    end;
validate_api_spec(_) ->
    {error, invalid_api_spec_format}.

%% @doc Checks if a capability is valid.
-spec is_valid_capability(term()) -> boolean().
is_valid_capability(publish) -> true;
is_valid_capability(subscribe) -> true;
is_valid_capability(call) -> true;
is_valid_capability(register) -> true;
is_valid_capability(provide_content) -> true;
is_valid_capability(consume_content) -> true;
is_valid_capability(_) -> false.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Validates a single segment of an identity
-spec is_valid_segment(binary()) -> boolean().
is_valid_segment(<<>>) ->
    false;
is_valid_segment(Segment) ->
    lists:all(
        fun(Char) ->
            (Char >= $a andalso Char =< $z) orelse
            (Char >= $A andalso Char =< $Z) orelse
            (Char >= $0 andalso Char =< $9) orelse
            Char =:= $-
        end,
        binary_to_list(Segment)
    ).
