%% @doc Unit tests for macula_authorization module.
%%
%% Tests cover:
%% - Namespace extraction from topics/procedures
%% - Namespace ownership checks (owner, ancestor, not_owner)
%% - Public topic detection
%% - RPC call authorization
%% - Publish authorization
%% - Subscribe authorization
%% - Announce authorization
%% - UCAN capability matching
%% - Wildcard patterns
-module(macula_authorization_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Namespace Extraction Tests
%%====================================================================

extract_namespace_four_segments_test() ->
    ?assertEqual(<<"io.macula.rgfaber">>,
                 macula_authorization:extract_namespace(<<"io.macula.rgfaber.place_order">>)).

extract_namespace_five_segments_test() ->
    ?assertEqual(<<"io.macula.rgfaber">>,
                 macula_authorization:extract_namespace(<<"io.macula.rgfaber.orders.placed">>)).

extract_namespace_three_segments_test() ->
    ?assertEqual(<<"io.macula.rgfaber">>,
                 macula_authorization:extract_namespace(<<"io.macula.rgfaber">>)).

extract_namespace_two_segments_test() ->
    %% Short topic returns itself
    ?assertEqual(<<"io.macula">>,
                 macula_authorization:extract_namespace(<<"io.macula">>)).

extract_namespace_one_segment_test() ->
    ?assertEqual(<<"test">>,
                 macula_authorization:extract_namespace(<<"test">>)).

extract_namespace_public_topic_test() ->
    ?assertEqual(<<"io.macula.public">>,
                 macula_authorization:extract_namespace(<<"io.macula.public.announcements">>)).

%%====================================================================
%% Namespace Ownership Tests
%%====================================================================

check_ownership_exact_match_test() ->
    DID = <<"did:macula:io.macula.rgfaber">>,
    Namespace = <<"io.macula.rgfaber">>,
    ?assertEqual({ok, owner},
                 macula_authorization:check_namespace_ownership(DID, Namespace)).

check_ownership_ancestor_test() ->
    DID = <<"did:macula:io.macula">>,
    Namespace = <<"io.macula.rgfaber">>,
    ?assertEqual({ok, ancestor},
                 macula_authorization:check_namespace_ownership(DID, Namespace)).

check_ownership_not_owner_test() ->
    DID = <<"did:macula:io.macula.other">>,
    Namespace = <<"io.macula.rgfaber">>,
    ?assertEqual({error, not_owner},
                 macula_authorization:check_namespace_ownership(DID, Namespace)).

check_ownership_sibling_not_owner_test() ->
    DID = <<"did:macula:io.macula.ibm">>,
    Namespace = <<"io.macula.rgfaber">>,
    ?assertEqual({error, not_owner},
                 macula_authorization:check_namespace_ownership(DID, Namespace)).

check_ownership_child_cannot_access_parent_test() ->
    DID = <<"did:macula:io.macula.rgfaber.app">>,
    Namespace = <<"io.macula.rgfaber">>,
    ?assertEqual({error, not_owner},
                 macula_authorization:check_namespace_ownership(DID, Namespace)).

check_ownership_invalid_did_test() ->
    DID = <<"invalid:did">>,
    Namespace = <<"io.macula.rgfaber">>,
    ?assertEqual({error, not_owner},
                 macula_authorization:check_namespace_ownership(DID, Namespace)).

%%====================================================================
%% Ancestor Namespace Tests
%%====================================================================

is_ancestor_parent_of_child_test() ->
    ?assertEqual(true,
                 macula_authorization:is_ancestor_namespace(<<"io.macula">>, <<"io.macula.rgfaber">>)).

is_ancestor_grandparent_test() ->
    ?assertEqual(true,
                 macula_authorization:is_ancestor_namespace(<<"io.macula">>, <<"io.macula.rgfaber.app">>)).

is_ancestor_same_namespace_test() ->
    %% Same namespace is NOT ancestor
    ?assertEqual(false,
                 macula_authorization:is_ancestor_namespace(<<"io.macula.rgfaber">>, <<"io.macula.rgfaber">>)).

is_ancestor_sibling_test() ->
    ?assertEqual(false,
                 macula_authorization:is_ancestor_namespace(<<"io.macula.ibm">>, <<"io.macula.rgfaber">>)).

is_ancestor_child_of_parent_test() ->
    %% Child is NOT ancestor of parent
    ?assertEqual(false,
                 macula_authorization:is_ancestor_namespace(<<"io.macula.rgfaber">>, <<"io.macula">>)).

%%====================================================================
%% Public Topic Tests
%%====================================================================

is_public_topic_middle_test() ->
    ?assertEqual(true,
                 macula_authorization:is_public_topic(<<"io.macula.rgfaber.public.events">>)).

is_public_topic_start_test() ->
    ?assertEqual(true,
                 macula_authorization:is_public_topic(<<"public.announcements">>)).

is_public_topic_not_public_test() ->
    ?assertEqual(false,
                 macula_authorization:is_public_topic(<<"io.macula.rgfaber.private.events">>)).

is_public_topic_partial_match_test() ->
    %% "publicdata" is NOT public (no dot after)
    ?assertEqual(false,
                 macula_authorization:is_public_topic(<<"io.macula.rgfaber.publicdata">>)).

%%====================================================================
%% RPC Call Authorization Tests
%%====================================================================

check_rpc_call_owner_authorized_test() ->
    CallerDID = <<"did:macula:io.macula.rgfaber">>,
    Procedure = <<"io.macula.rgfaber.place_order">>,
    ?assertEqual({ok, authorized},
                 macula_authorization:check_rpc_call(CallerDID, Procedure, undefined, #{})).

check_rpc_call_ancestor_authorized_test() ->
    CallerDID = <<"did:macula:io.macula">>,
    Procedure = <<"io.macula.rgfaber.place_order">>,
    ?assertEqual({ok, authorized},
                 macula_authorization:check_rpc_call(CallerDID, Procedure, undefined, #{})).

check_rpc_call_non_owner_no_ucan_test() ->
    CallerDID = <<"did:macula:io.macula.other">>,
    Procedure = <<"io.macula.rgfaber.place_order">>,
    ?assertEqual({error, unauthorized},
                 macula_authorization:check_rpc_call(CallerDID, Procedure, undefined, #{})).

%%====================================================================
%% Publish Authorization Tests
%%====================================================================

check_publish_owner_authorized_test() ->
    CallerDID = <<"did:macula:io.macula.rgfaber">>,
    Topic = <<"io.macula.rgfaber.order_placed">>,
    ?assertEqual({ok, authorized},
                 macula_authorization:check_publish(CallerDID, Topic, undefined, #{})).

check_publish_non_owner_denied_test() ->
    CallerDID = <<"did:macula:io.macula.other">>,
    Topic = <<"io.macula.rgfaber.order_placed">>,
    ?assertEqual({error, unauthorized},
                 macula_authorization:check_publish(CallerDID, Topic, undefined, #{})).

check_publish_public_topic_still_requires_ownership_test() ->
    %% Public topics allow subscription, NOT publish
    CallerDID = <<"did:macula:io.macula.other">>,
    Topic = <<"io.macula.rgfaber.public.events">>,
    ?assertEqual({error, unauthorized},
                 macula_authorization:check_publish(CallerDID, Topic, undefined, #{})).

%%====================================================================
%% Subscribe Authorization Tests
%%====================================================================

check_subscribe_public_topic_authorized_test() ->
    CallerDID = <<"did:macula:io.macula.other">>,
    Topic = <<"io.macula.rgfaber.public.announcements">>,
    ?assertEqual({ok, authorized},
                 macula_authorization:check_subscribe(CallerDID, Topic, #{})).

check_subscribe_private_topic_denied_test() ->
    CallerDID = <<"did:macula:io.macula.other">>,
    Topic = <<"io.macula.rgfaber.private.events">>,
    ?assertEqual({error, unauthorized},
                 macula_authorization:check_subscribe(CallerDID, Topic, #{})).

check_subscribe_owner_authorized_test() ->
    CallerDID = <<"did:macula:io.macula.rgfaber">>,
    Topic = <<"io.macula.rgfaber.private.events">>,
    %% Use 4-arg version with undefined UCAN
    ?assertEqual({ok, authorized},
                 macula_authorization:check_subscribe(CallerDID, Topic, undefined, #{})).

%%====================================================================
%% Announce Authorization Tests
%%====================================================================

check_announce_owner_authorized_test() ->
    CallerDID = <<"did:macula:io.macula.rgfaber">>,
    Procedure = <<"io.macula.rgfaber.place_order">>,
    ?assertEqual({ok, authorized},
                 macula_authorization:check_announce(CallerDID, Procedure, #{})).

check_announce_ancestor_authorized_test() ->
    CallerDID = <<"did:macula:io.macula">>,
    Procedure = <<"io.macula.rgfaber.place_order">>,
    ?assertEqual({ok, authorized},
                 macula_authorization:check_announce(CallerDID, Procedure, #{})).

check_announce_non_owner_denied_test() ->
    %% Announce NEVER allowed for non-owners (even with UCAN)
    CallerDID = <<"did:macula:io.macula.other">>,
    Procedure = <<"io.macula.rgfaber.place_order">>,
    ?assertEqual({error, unauthorized},
                 macula_authorization:check_announce(CallerDID, Procedure, #{})).

%%====================================================================
%% Capability Matching Tests
%%====================================================================

check_capability_exact_match_test() ->
    Cap = #{<<"with">> => <<"io.macula.rgfaber.place_order">>,
            <<"can">> => <<"mesh:call">>},
    Resource = <<"io.macula.rgfaber.place_order">>,
    Operation = <<"mesh:call">>,
    ?assertEqual(true,
                 macula_authorization:check_capability_match(Cap, Resource, Operation)).

check_capability_wildcard_resource_test() ->
    Cap = #{<<"with">> => <<"io.macula.rgfaber.*">>,
            <<"can">> => <<"mesh:call">>},
    Resource = <<"io.macula.rgfaber.place_order">>,
    Operation = <<"mesh:call">>,
    ?assertEqual(true,
                 macula_authorization:check_capability_match(Cap, Resource, Operation)).

check_capability_wildcard_operation_test() ->
    Cap = #{<<"with">> => <<"io.macula.rgfaber.place_order">>,
            <<"can">> => <<"mesh:*">>},
    Resource = <<"io.macula.rgfaber.place_order">>,
    Operation = <<"mesh:call">>,
    ?assertEqual(true,
                 macula_authorization:check_capability_match(Cap, Resource, Operation)).

check_capability_double_wildcard_test() ->
    Cap = #{<<"with">> => <<"io.macula.rgfaber.*">>,
            <<"can">> => <<"mesh:*">>},
    Resource = <<"io.macula.rgfaber.any_procedure">>,
    Operation = <<"mesh:subscribe">>,
    ?assertEqual(true,
                 macula_authorization:check_capability_match(Cap, Resource, Operation)).

check_capability_no_match_test() ->
    Cap = #{<<"with">> => <<"io.macula.other.procedure">>,
            <<"can">> => <<"mesh:call">>},
    Resource = <<"io.macula.rgfaber.place_order">>,
    Operation = <<"mesh:call">>,
    ?assertEqual(false,
                 macula_authorization:check_capability_match(Cap, Resource, Operation)).

check_capability_wrong_operation_test() ->
    Cap = #{<<"with">> => <<"io.macula.rgfaber.place_order">>,
            <<"can">> => <<"mesh:publish">>},
    Resource = <<"io.macula.rgfaber.place_order">>,
    Operation = <<"mesh:call">>,
    ?assertEqual(false,
                 macula_authorization:check_capability_match(Cap, Resource, Operation)).

check_capability_invalid_format_test() ->
    ?assertEqual(false,
                 macula_authorization:check_capability_match(not_a_map, <<"resource">>, <<"op">>)).

%%====================================================================
%% DID Resolution Tests
%%====================================================================

resolve_caller_did_valid_test() ->
    DID = <<"did:macula:io.macula.rgfaber">>,
    {ok, Components} = macula_authorization:resolve_caller_did(DID),
    ?assertEqual(<<"macula">>, maps:get(<<"method">>, Components)),
    ?assertEqual(<<"io.macula.rgfaber">>, maps:get(<<"identity">>, Components)).

resolve_caller_did_invalid_test() ->
    ?assertEqual({error, invalid_did},
                 macula_authorization:resolve_caller_did(<<"invalid:did">>)).

resolve_caller_did_non_binary_test() ->
    ?assertEqual({error, invalid_did},
                 macula_authorization:resolve_caller_did(not_a_binary)).

extract_identity_valid_test() ->
    DID = <<"did:macula:io.macula.rgfaber">>,
    ?assertEqual({ok, <<"io.macula.rgfaber">>},
                 macula_authorization:extract_identity_from_did(DID)).

extract_identity_invalid_test() ->
    ?assertEqual({error, invalid_did},
                 macula_authorization:extract_identity_from_did(<<"invalid">>)).

%%====================================================================
%% UCAN Validation Tests (without actual tokens - edge cases)
%%====================================================================

validate_ucan_undefined_test() ->
    ?assertEqual({error, unauthorized},
                 macula_authorization:validate_ucan_for_operation(
                     undefined, <<"did:macula:io.macula.other">>,
                     <<"io.macula.rgfaber.procedure">>, <<"mesh:call">>)).

validate_ucan_invalid_token_test() ->
    %% Invalid JWT format
    ?assertEqual({error, invalid_ucan},
                 macula_authorization:validate_ucan_for_operation(
                     <<"not.a.valid.jwt">>, <<"did:macula:io.macula.other">>,
                     <<"io.macula.rgfaber.procedure">>, <<"mesh:call">>)).
