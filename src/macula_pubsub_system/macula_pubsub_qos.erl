%%%-------------------------------------------------------------------
%%% @doc
%%% QoS (Quality of Service) manager for pub/sub.
%%%
%%% Handles QoS 1 (at-least-once delivery) logic:
%%% - Message tracking with timeout timers
%%% - Automatic retry on timeout (up to max retries)
%%% - Acknowledgment handling
%%%
%%% Extracted from macula_pubsub_handler.erl (Phase 2)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_qos).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%% API
-export([track_message/5, handle_timeout/3, handle_ack/2, get_pending/1]).

-type message_id() :: binary().
-type topic() :: binary().
-type payload() :: binary().
-type qos() :: 0 | 1.
-type retry_count() :: non_neg_integer().
-type timer_ref() :: reference().
-type connection_manager_pid() :: pid().

-type pending_pubacks() :: #{message_id() => {topic(), payload(), qos(), retry_count(), timer_ref()}}.

-export_type([pending_pubacks/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Track a message for QoS 1 acknowledgment.
%% Starts a timeout timer and stores message in pending map.
%% Returns updated pending_pubacks map.
-spec track_message(message_id(), topic(), payload(), qos(), pending_pubacks()) ->
    {ok, pending_pubacks()} | {error, term()}.
track_message(_MsgId, _Topic, _Payload, Qos, PendingPubacks) when Qos =/= 1 ->
    %% Only track QoS 1 messages
    {ok, PendingPubacks};
track_message(MsgId, Topic, Payload, Qos, PendingPubacks) when Qos =:= 1 ->
    %% Start timeout timer
    TimerRef = erlang:send_after(?PUBACK_TIMEOUT, self(), {puback_timeout, MsgId}),
    %% Store pending acknowledgment with retry count 0
    UpdatedPending = PendingPubacks#{MsgId => {Topic, Payload, Qos, 0, TimerRef}},
    {ok, UpdatedPending}.

%% @doc Handle timeout for a pending message.
%% Retries sending if under max retries, otherwise gives up.
%% Returns {retry, UpdatedPending, PublishMsg} | {give_up, UpdatedPending}.
-spec handle_timeout(message_id(), connection_manager_pid(), pending_pubacks()) ->
    {retry, pending_pubacks(), map()} | {give_up, pending_pubacks()} | {not_found, pending_pubacks()}.
handle_timeout(MsgId, _ConnMgrPid, PendingPubacks) ->
    PendingInfo = maps:get(MsgId, PendingPubacks, undefined),
    do_handle_timeout(PendingInfo, MsgId, PendingPubacks).

%% @private Message not found (already acknowledged or removed)
do_handle_timeout(undefined, _MsgId, PendingPubacks) ->
    {not_found, PendingPubacks};
%% @private Message found - check retry count
do_handle_timeout({Topic, Payload, Qos, RetryCount, _OldTimerRef}, MsgId, PendingPubacks) ->
    NewRetryCount = RetryCount + 1,
    MaxRetriesReached = NewRetryCount >= ?PUBACK_MAX_RETRIES,
    handle_timeout_retry(MaxRetriesReached, MsgId, Topic, Payload, Qos, NewRetryCount, PendingPubacks).

%% @private Max retries reached, give up
handle_timeout_retry(true, MsgId, Topic, _Payload, _Qos, NewRetryCount, PendingPubacks) ->
    ?LOG_ERROR("[QoS] PUBACK timeout for message ~p (topic: ~s) after ~p retries - giving up",
              [MsgId, Topic, NewRetryCount]),
    {give_up, maps:remove(MsgId, PendingPubacks)};
%% @private Retry sending the message
handle_timeout_retry(false, MsgId, Topic, Payload, Qos, NewRetryCount, PendingPubacks) ->
    ?LOG_WARNING("[QoS] PUBACK timeout for message ~p (topic: ~s) - retry ~p/~p",
                [MsgId, Topic, NewRetryCount, ?PUBACK_MAX_RETRIES]),

    PublishMsg = #{
        topic => Topic,
        payload => Payload,
        qos => Qos,
        retain => false,
        message_id => MsgId
    },

    NewTimerRef = erlang:send_after(?PUBACK_TIMEOUT, self(), {puback_timeout, MsgId}),

    UpdatedPending = PendingPubacks#{
        MsgId => {Topic, Payload, Qos, NewRetryCount, NewTimerRef}
    },

    {retry, UpdatedPending, PublishMsg}.

%% @doc Handle acknowledgment for a message.
%% Cancels timer and removes message from pending map.
%% Returns updated pending_pubacks map.
-spec handle_ack(message_id(), pending_pubacks()) -> pending_pubacks().
handle_ack(MsgId, PendingPubacks) ->
    PendingInfo = maps:get(MsgId, PendingPubacks, undefined),
    do_handle_ack(PendingInfo, MsgId, PendingPubacks).

%% @private Message not found, return unchanged
do_handle_ack(undefined, _MsgId, PendingPubacks) ->
    PendingPubacks;
%% @private Cancel timer and remove message
do_handle_ack({_Topic, _Payload, _Qos, _RetryCount, TimerRef}, MsgId, PendingPubacks) ->
    erlang:cancel_timer(TimerRef),
    maps:remove(MsgId, PendingPubacks).

%% @doc Get list of pending message IDs (for testing/debugging).
-spec get_pending(pending_pubacks()) -> [message_id()].
get_pending(PendingPubacks) ->
    maps:keys(PendingPubacks).

%%%===================================================================
%%% Internal functions
%%%===================================================================

