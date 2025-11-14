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
    case maps:get(SubRef, Subscriptions, undefined) of
        undefined ->
            {error, not_found};
        {Topic, _Callback} ->
            UpdatedSubscriptions = maps:remove(SubRef, Subscriptions),
            {ok, UpdatedSubscriptions, Topic}
    end.

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
                try
                    %% Decode payload if it's JSON
                    DecodedPayload = try
                        macula_utils:decode_json(Payload)
                    catch
                        _:_ -> Payload
                    end,

                    PublishData = #{
                        topic => Topic,
                        matched_pattern => SubTopic,
                        payload => DecodedPayload
                    },
                    Callback(PublishData),
                    ?LOG_DEBUG("[~s] Invoked callback for topic ~s", [NodeId, Topic])
                catch
                    Error:Reason:Stack ->
                        ?LOG_ERROR("[~s] Callback error for topic ~s: ~p:~p~nStack: ~p",
                                  [NodeId, Topic, Error, Reason, Stack])
                end
            end)
        end,
        Matches
    ),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% TODO: Add internal helper functions as needed
