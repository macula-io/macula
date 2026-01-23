%%%-------------------------------------------------------------------
%%% @doc
%%% Subscription management for pub/sub.
%%%
%%% Responsibilities:
%%% - Store and retrieve subscriptions
%%% - Pattern matching with wildcards (*, **)
%%% - Find matching subscriptions for a topic
%%% - Invoke subscriber callbacks
%%%
%%% Extracted from macula_pubsub_handler.erl (Phase 4)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_subscription).

-include_lib("kernel/include/logger.hrl").

%% API
-export([add_subscription/4, remove_subscription/2,
         find_matches/3, invoke_callbacks/4]).

-type topic() :: binary().
-type callback() :: fun((map()) -> ok).
-type subscription_ref() :: reference().
-type node_id() :: binary().
-type payload() :: binary().

-type subscriptions() :: #{subscription_ref() => {topic(), callback()}}.

-export_type([subscriptions/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Add a subscription.
%% Returns {ok, UpdatedSubscriptions, SubRef}.
-spec add_subscription(topic(), callback(), subscriptions(), subscription_ref()) ->
    {ok, subscriptions(), subscription_ref()}.
add_subscription(Topic, Callback, Subscriptions, SubRef) ->
    UpdatedSubscriptions = Subscriptions#{SubRef => {Topic, Callback}},
    {ok, UpdatedSubscriptions, SubRef}.

%% @doc Remove a subscription.
%% Returns {ok, UpdatedSubscriptions, Topic} | {error, not_found}.
-spec remove_subscription(subscription_ref(), subscriptions()) ->
    {ok, subscriptions(), topic()} | {error, not_found}.
remove_subscription(SubRef, Subscriptions) ->
    SubInfo = maps:get(SubRef, Subscriptions, undefined),
    do_remove_subscription(SubInfo, SubRef, Subscriptions).

%% @private Subscription not found
do_remove_subscription(undefined, _SubRef, _Subscriptions) ->
    {error, not_found};
%% @private Remove subscription
do_remove_subscription({Topic, _Callback}, SubRef, Subscriptions) ->
    UpdatedSubscriptions = maps:remove(SubRef, Subscriptions),
    {ok, UpdatedSubscriptions, Topic}.

%% @doc Find matching subscriptions for a topic.
%% Returns list of {SubRef, {Pattern, Callback}} tuples.
-spec find_matches(topic(), subscriptions(), #{atom() => binary()}) ->
    [{subscription_ref(), {topic(), callback()}}].
find_matches(Topic, Subscriptions, Config) ->
    Separator = maps:get(topic_separator, Config),
    WildcardSingle = maps:get(topic_wildcard_single, Config),
    WildcardMulti = maps:get(topic_wildcard_multi, Config),

    lists:filter(
        fun({_SubRef, {Pattern, _Callback}}) ->
            macula_utils:topic_matches(Pattern, Topic, Separator, WildcardSingle, WildcardMulti)
        end,
        maps:to_list(Subscriptions)
    ).

%% @doc Invoke callbacks for matching subscriptions.
%% Spawns async tasks to invoke each callback.
-spec invoke_callbacks([{subscription_ref(), {topic(), callback()}}], topic(), payload(), node_id()) -> ok.
invoke_callbacks([], _Topic, _Payload, _NodeId) ->
    ok;
invoke_callbacks(Matches, Topic, Payload, NodeId) ->
    ?LOG_INFO("[~s] Found ~p subscription(s) for topic: ~s",
             [NodeId, length(Matches), Topic]),

    %% Invoke all matching callbacks asynchronously
    lists:foreach(
        fun({_SubRef, {SubTopic, Callback}}) ->
            spawn(fun() ->
                invoke_single_callback(Callback, Topic, SubTopic, Payload, NodeId)
            end)
        end,
        Matches
    ),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Invoke a single callback with payload decoding
invoke_single_callback(Callback, Topic, SubTopic, Payload, NodeId) ->
    DecodedPayload = safe_decode_json(Payload),
    PublishData = #{
        topic => Topic,
        matched_pattern => SubTopic,
        payload => DecodedPayload
    },
    handle_callback_invocation(catch Callback(PublishData), Topic, NodeId).

%% @private Safely decode JSON payload
safe_decode_json(Payload) ->
    handle_decode_result(catch macula_utils:decode_json(Payload), Payload).

%% @private Handle JSON decode result
handle_decode_result({'EXIT', _}, Original) ->
    Original;
handle_decode_result(Decoded, _Original) ->
    Decoded.

%% @private Handle callback invocation result
handle_callback_invocation({'EXIT', {Reason, Stacktrace}}, Topic, NodeId) ->
    ?LOG_ERROR("[~s] Callback error for topic ~s: ~p~nStack: ~p",
               [NodeId, Topic, Reason, Stacktrace]);
handle_callback_invocation({'EXIT', Reason}, Topic, NodeId) ->
    ?LOG_ERROR("[~s] Callback error for topic ~s: ~p", [NodeId, Topic, Reason]);
handle_callback_invocation(_Result, Topic, NodeId) ->
    ?LOG_DEBUG("[~s] Invoked callback for topic ~s", [NodeId, Topic]).

