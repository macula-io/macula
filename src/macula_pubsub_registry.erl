%%%-------------------------------------------------------------------
%%% @doc
%%% Local subscription registry for pub/sub.
%%% Maps topic patterns to local subscribers (callback PIDs).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_registry).

%% API
-export([
    new/0,
    subscribe/4,
    unsubscribe/3,
    match/2,
    list_patterns/1,
    get_subscription/3,
    size/1
]).

%% Types
-type subscription() :: #{
    subscriber_id := binary(),
    pattern := binary(),
    callback := pid()
}.

-type registry() :: #{
    subscriptions := [subscription()],
    pattern_index := #{binary() => [subscription()]}  % Pattern -> [Subscriptions]
}.

-export_type([subscription/0, registry/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create new empty registry.
-spec new() -> registry().
new() ->
    #{
        subscriptions => [],
        pattern_index => #{}
    }.

%% @doc Subscribe to a pattern.
%% If subscription already exists (same subscriber_id + pattern), updates callback.
-spec subscribe(registry(), binary(), binary(), pid()) -> registry().
subscribe(#{subscriptions := Subs, pattern_index := Index} = Registry, SubscriberId, Pattern, Callback) ->
    %% Create subscription
    Subscription = #{
        subscriber_id => SubscriberId,
        pattern => Pattern,
        callback => Callback
    },

    %% Check if subscription already exists
    case find_subscription(Subs, SubscriberId, Pattern) of
        {found, _OldSub} ->
            %% Update existing subscription
            NewSubs = update_subscription(Subs, Subscription),
            NewIndex = update_pattern_index(Index, Pattern, NewSubs),
            Registry#{subscriptions => NewSubs, pattern_index => NewIndex};

        not_found ->
            %% Add new subscription
            NewSubs = [Subscription | Subs],
            NewIndex = add_to_pattern_index(Index, Pattern, Subscription),
            Registry#{subscriptions => NewSubs, pattern_index => NewIndex}
    end.

%% @doc Unsubscribe from a pattern.
-spec unsubscribe(registry(), binary(), binary()) -> registry().
unsubscribe(#{subscriptions := Subs, pattern_index := Index} = Registry, SubscriberId, Pattern) ->
    case find_subscription(Subs, SubscriberId, Pattern) of
        {found, Sub} ->
            %% Remove subscription
            NewSubs = lists:filter(
                fun(S) ->
                    not (maps:get(subscriber_id, S) =:= SubscriberId andalso
                         maps:get(pattern, S) =:= Pattern)
                end,
                Subs
            ),
            NewIndex = remove_from_pattern_index(Index, Pattern, Sub),
            Registry#{subscriptions => NewSubs, pattern_index => NewIndex};

        not_found ->
            Registry  % No change
    end.

%% @doc Find subscriptions matching a topic.
-spec match(registry(), binary()) -> [subscription()].
match(#{subscriptions := Subs}, Topic) ->
    lists:filter(
        fun(Sub) ->
            Pattern = maps:get(pattern, Sub),
            macula_pubsub_topic:matches(Topic, Pattern)
        end,
        Subs
    ).

%% @doc List all unique patterns.
-spec list_patterns(registry()) -> [binary()].
list_patterns(#{pattern_index := Index}) ->
    maps:keys(Index).

%% @doc Get specific subscription.
-spec get_subscription(registry(), binary(), binary()) -> {ok, subscription()} | not_found.
get_subscription(#{subscriptions := Subs}, SubscriberId, Pattern) ->
    case find_subscription(Subs, SubscriberId, Pattern) of
        {found, Sub} -> {ok, Sub};
        not_found -> not_found
    end.

%% @doc Get number of subscriptions.
-spec size(registry()) -> non_neg_integer().
size(#{subscriptions := Subs}) ->
    length(Subs).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Find subscription by subscriber_id and pattern.
-spec find_subscription([subscription()], binary(), binary()) -> {found, subscription()} | not_found.
find_subscription([], _SubscriberId, _Pattern) ->
    not_found;
find_subscription([Sub | Rest], SubscriberId, Pattern) ->
    case maps:get(subscriber_id, Sub) =:= SubscriberId andalso
         maps:get(pattern, Sub) =:= Pattern of
        true -> {found, Sub};
        false -> find_subscription(Rest, SubscriberId, Pattern)
    end.

%% @doc Update existing subscription in list.
-spec update_subscription([subscription()], subscription()) -> [subscription()].
update_subscription(Subs, NewSub) ->
    SubscriberId = maps:get(subscriber_id, NewSub),
    Pattern = maps:get(pattern, NewSub),

    lists:map(
        fun(Sub) ->
            case maps:get(subscriber_id, Sub) =:= SubscriberId andalso
                 maps:get(pattern, Sub) =:= Pattern of
                true -> NewSub;
                false -> Sub
            end
        end,
        Subs
    ).

%% @doc Add subscription to pattern index.
-spec add_to_pattern_index(#{binary() => [subscription()]}, binary(), subscription()) ->
    #{binary() => [subscription()]}.
add_to_pattern_index(Index, Pattern, Subscription) ->
    Existing = maps:get(Pattern, Index, []),
    Index#{Pattern => [Subscription | Existing]}.

%% @doc Update pattern index after subscription update.
-spec update_pattern_index(#{binary() => [subscription()]}, binary(), [subscription()]) ->
    #{binary() => [subscription()]}.
update_pattern_index(Index, Pattern, AllSubs) ->
    %% Rebuild index entry for this pattern
    PatternSubs = lists:filter(
        fun(Sub) -> maps:get(pattern, Sub) =:= Pattern end,
        AllSubs
    ),
    Index#{Pattern => PatternSubs}.

%% @doc Remove subscription from pattern index.
-spec remove_from_pattern_index(#{binary() => [subscription()]}, binary(), subscription()) ->
    #{binary() => [subscription()]}.
remove_from_pattern_index(Index, Pattern, Subscription) ->
    Existing = maps:get(Pattern, Index, []),
    SubscriberId = maps:get(subscriber_id, Subscription),

    NewList = lists:filter(
        fun(Sub) -> maps:get(subscriber_id, Sub) =/= SubscriberId end,
        Existing
    ),

    case NewList of
        [] ->
            %% No more subscriptions for this pattern, remove key
            maps:remove(Pattern, Index);
        _ ->
            Index#{Pattern => NewList}
    end.
