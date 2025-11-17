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
    case maps:get(MsgId, PendingPubacks, undefined) of
        undefined ->
            %% Message not found (already acknowledged or removed)
            {not_found, PendingPubacks};
        {Topic, Payload, Qos, RetryCount, _OldTimerRef} ->
            NewRetryCount = RetryCount + 1,
            case NewRetryCount >= ?PUBACK_MAX_RETRIES of
                true ->
                    %% Max retries reached, give up
                    ?LOG_ERROR("[QoS] PUBACK timeout for message ~p (topic: ~s) after ~p retries - giving up",
                              [MsgId, Topic, NewRetryCount]),
                    {give_up, maps:remove(MsgId, PendingPubacks)};
                false ->
                    %% Retry sending the message
                    ?LOG_WARNING("[QoS] PUBACK timeout for message ~p (topic: ~s) - retry ~p/~p",
                                [MsgId, Topic, NewRetryCount, ?PUBACK_MAX_RETRIES]),

                    %% Build publish message for retry
                    PublishMsg = #{
                        topic => Topic,
                        payload => Payload,
                        qos => Qos,
                        retain => false,
                        message_id => MsgId
                    },

                    %% Start new timer
                    NewTimerRef = erlang:send_after(?PUBACK_TIMEOUT, self(), {puback_timeout, MsgId}),

                    %% Update pending with new retry count and timer
                    UpdatedPending = PendingPubacks#{
                        MsgId => {Topic, Payload, Qos, NewRetryCount, NewTimerRef}
                    },

                    {retry, UpdatedPending, PublishMsg}
            end
    end.

%% @doc Handle acknowledgment for a message.
%% Cancels timer and removes message from pending map.
%% Returns updated pending_pubacks map.
-spec handle_ack(message_id(), pending_pubacks()) -> pending_pubacks().
handle_ack(MsgId, PendingPubacks) ->
    case maps:get(MsgId, PendingPubacks, undefined) of
        undefined ->
            %% Message not found, return unchanged
            PendingPubacks;
        {_Topic, _Payload, _Qos, _RetryCount, TimerRef} ->
            %% Cancel timer and remove message
            erlang:cancel_timer(TimerRef),
            maps:remove(MsgId, PendingPubacks)
    end.

%% @doc Get list of pending message IDs (for testing/debugging).
-spec get_pending(pending_pubacks()) -> [message_id()].
get_pending(PendingPubacks) ->
    maps:keys(PendingPubacks).

%%%===================================================================
%%% Internal functions
%%%===================================================================

