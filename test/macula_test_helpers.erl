-module(macula_test_helpers).
-behaviour(gen_server).
-export([start_mock_connection_manager/0, start_mock_connection_manager/1,
         stop_mock_connection_manager/1, get_mock_messages/1,
         set_mock_status/2, clear_mock_messages/1,
         default_pubsub_opts/0, default_pubsub_opts/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(mock_state, {status = connected :: atom(), messages = [] :: list()}).

start_mock_connection_manager() -> start_mock_connection_manager(connected).
start_mock_connection_manager(Status) when is_atom(Status) ->
    gen_server:start_link(?MODULE, #{status => Status}, []).
stop_mock_connection_manager(Pid) -> gen_server:stop(Pid).
get_mock_messages(Pid) -> gen_server:call(Pid, get_messages).
set_mock_status(Pid, Status) -> gen_server:call(Pid, {set_status, Status}).
clear_mock_messages(Pid) -> gen_server:call(Pid, clear_messages).

default_pubsub_opts() -> default_pubsub_opts(#{}).
default_pubsub_opts(Overrides) when is_map(Overrides) ->
    UniqueId = integer_to_binary(erlang:unique_integer([positive])),
    NodeId = <<"test_node_", UniqueId/binary>>,
    Sep = <<".">>,
    Star = <<"*">>,
    StarStar = <<"**">>,
    Defaults = #{node_id => NodeId,
                 realm => <<"test.realm">>,
                 url => <<"http://localhost:4000">>,
                 topic_separator => Sep,
                 topic_wildcard_single => Star,
                 topic_wildcard_multi => StarStar},
    maps:merge(Defaults, Overrides).

init(#{status := Status}) -> {ok, #mock_state{status = Status}}.
handle_call({send_message, Type, Msg}, _From, State) ->
    Message = #{type => Type, msg => Msg, timestamp => erlang:system_time(millisecond)},
    Messages = [Message | State#mock_state.messages],
    {reply, ok, State#mock_state{messages = Messages}};
handle_call(get_status, _From, State) ->
    {reply, State#mock_state.status, State};
handle_call(get_messages, _From, State) ->
    {reply, lists:reverse(State#mock_state.messages), State};
handle_call({set_status, Status}, _From, State) ->
    {reply, ok, State#mock_state{status = Status}};
handle_call(clear_messages, _From, State) ->
    {reply, ok, State#mock_state{messages = []}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
