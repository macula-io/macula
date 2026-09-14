%% Test support for stream session lifetimes: the processes serving a stream session on this node, and a bounded wait
%% until none is left beyond a set taken before the sessions started.
-module(macula_test_sessions).

-export([serving/0, await_none_new/1]).

-define(POLL_MS, 20).
-define(POLLS, 50).

%% @doc Every process on this node that serves a stream session: a macula_stream, a macula_streamer, or a process
%% parked in a stream host loop.
-spec serving() -> [pid()].
serving() ->
    [Pid || Pid <- erlang:processes(), serves_a_session(erlang:process_info(Pid, [current_function, dictionary]))].

%% @doc The session processes still alive that are not in Before, after waiting up to a second for them to end: []
%% once every one has ended.
-spec await_none_new([pid()]) -> [pid()].
await_none_new(Before) when is_list(Before) ->
    await_none_new(Before, ?POLLS).

await_none_new(Before, 0) ->
    serving() -- Before;
await_none_new(Before, Polls) ->
    none_new(serving() -- Before, Before, Polls).

none_new([], _Before, _Polls) ->
    [];
none_new(_New, Before, Polls) ->
    timer:sleep(?POLL_MS),
    await_none_new(Before, Polls - 1).

serves_a_session([{current_function, {macula_station_link, stream_host_loop, 0}} | _]) ->
    true;
serves_a_session([{current_function, {macula_stream_local, host_loop, 0}} | _]) ->
    true;
serves_a_session([{current_function, _}, {dictionary, Dictionary}]) ->
    lists:member(proplists:get_value('$initial_call', Dictionary),
                 [{macula_stream, init, 1}, {macula_streamer, init, 1}]);
serves_a_session(undefined) ->
    false.
